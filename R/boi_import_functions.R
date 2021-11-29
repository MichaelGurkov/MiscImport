utils::globalVariables("where")

#' @title Import Oracle format financial report data
#'
#' @description  This function imports financial report data from Oracle format
#'
#' @param filepath the path to financial report data (in rds format)
#'
#' @param data_frequency filtering parameter. can be one of:
#' \itemize{
#' \item {annual}
#' \item {semiannual}
#' \item {quarterly}
#' }
#' The default
#' @importFrom readr read_rds
#'
#' @import dplyr
#'
#' @importFrom zoo as.yearqtr
#'
#' @import stringr
#'
#' @export

import_boi_oracle_finrep_data = function(filepath = NULL,
                                         data_frequency){

  if(missing(data_frequency)){stop("data frequency argument is required")}


  if(is.null(filepath)){filepath = paste0(file.path(
    Sys.getenv("USERPROFILE"),fsep="\\"),
    "\\OneDrive - Bank Of Israel\\Data\\",
    "TASE liquidity\\Rdata files\\finrep_oracle_data.rds")}

  temp_df = read_rds(filepath)

  df = temp_df %>%
    rename_all(tolower) %>%
    rename_all(~str_replace_all(.,"__","_"))

  df = df %>%
    mutate(fsd_period = nchar(.data$fsd_period)) %>%
    mutate(fsd_period = recode(.data$fsd_period,
                               `4` = "annual",
                               `6` = "quarterly",
                               `8` = "semiannual")) %>%
    filter(.data$fsd_period == data_frequency)

  df = df  %>%
    mutate(date_yearqtr = as.yearqtr(.data$date_fsd,
                                     format = "%Y%q"))%>%
    rename(total_assets = .data$total_balance,
           tase_id = .data$tase_issuer_id)

  df = df %>%
    mutate(across(!c("tase_id","date_yearqtr") & where(~!is.numeric(.)),
                  as.numeric))

  return(df)

}


#' This function downloads data files from BOI website
#' to OneDrive directory
#'
#' @import purrr
#'
#' @export
#'

download_boi_credit_data = function(credit_target_path = NULL,
                                debt_target_path = NULL,
                                debt_by_sectors_target_path = NULL){

  files_path = list(credit = list(
    source_path = paste0(
      "https://www.boi.org.il/he/DataAndStatistics",
      "/Lists/BoiTablesAndGraphs/itrashrh.xlsx"
    )
  ),

  debt = list(
    source_path = paste0(
      "https://www.boi.org.il/he/DataAndStatistics",
      "/Lists/BoiTablesAndGraphs/itrchovh.xlsx"
    )
  ),

  debt_by_sectors = list(
    source_path = paste0(
      "https://www.boi.org.il/he/DataAndStatistics",
      "/Lists/BoiTablesAndGraphs/itra_hov_bs.xlsx"
    )
  ))




  if(is.null(credit_target_path)){

    files_path$credit$target_path = paste0(
      file.path(Sys.getenv("USERPROFILE")),
      "\\OneDrive - Bank Of Israel\\Data\\BoI\\Credit",
      "\\itrashrh.xlsx")

  } else

  {files_path$credit$target_path = credit_target_path}


  if(is.null(debt_target_path)){

    files_path$debt$target_path = paste0(
      file.path(Sys.getenv("USERPROFILE")),
      "\\OneDrive - Bank Of Israel\\Data\\BoI\\Credit",
      "\\itrchovh.xlsx")

  } else

  {files_path$debt$target_path = debt_target_path}


  if(is.null(debt_by_sectors_target_path)){

    files_path$debt_by_sectors$target_path = paste0(
      file.path(Sys.getenv("USERPROFILE")),
      "\\OneDrive - Bank Of Israel\\Data\\BoI\\Credit",
      "\\itra_hov_bs.xlsx")

  } else

  {files_path$debt$target_path = debt_target_path}


  purrr::walk(files_path, ~download.file(url = .[["source_path"]],
                                         destfile = .[["target_path"]],
                                         quiet = TRUE, mode = "wb"))


}


#' This function imports credit data from BOI format file
#'
#' @import readxl
#'
#' @import tidyr
#'
#' @import dplyr
#'
#' @import lubridate
#'
#' @export
#'
import_boi_credit_df = function(credit_df_filepath = NULL,
                            data_frequency = "quarter"){

  if(is.null(credit_df_filepath)){

    credit_df_filepath = paste0(
      file.path(Sys.getenv("USERPROFILE")),
      "\\OneDrive - Bank Of Israel\\Data\\",
      "BoI\\Credit\\itrashrh.xlsx")
  }

  sheets_names = excel_sheets(credit_df_filepath)

  data_format_filepath = paste0(
    file.path(Sys.getenv("USERPROFILE")),
    "\\Documents\\BoICredit\\analysis\\BOI_data_format.csv")

  data_format = readr::read_csv(data_format_filepath)

  if(data_frequency == "quarter"){

    quarterly_df = read_xlsx(credit_df_filepath,sheet = sheets_names[1],
                             range = cell_limits(ul = c(8,1),
                                                 lr = c(77,NA_integer_)))

    df = quarterly_df %>%
      slice(data_format$row_num) %>%
      select(-1) %>%
      cbind.data.frame(select(data_format, -row_num)) %>%
      pivot_longer(-c("lender","borrower","instrument","category"),
                   names_to = "date", values_to = "value") %>%
      mutate(date = zoo::as.yearqtr(date, format = "%m-%y"))

  }

  if(data_frequency == "month"){

    monthly_df = read_xlsx(credit_df_filepath,sheet = sheets_names[2],
                           range = cell_limits(ul = c(8,1),
                                               lr = c(77,NA_integer_)))

    df = monthly_df %>%
      slice(data_format$row_num) %>%
      select(-1) %>%
      cbind.data.frame(select(data_format, -row_num)) %>%
      pivot_longer(-c("lender","borrower","instrument","category"),
                   names_to = "date", values_to = "value") %>%
      mutate(date = zoo::as.yearmon(date, format = "%m-%y"))

  }

  return(df)



}


#' This function imports debt by sectors data from BOI format file
#'
#' @import readxl
#'
#' @import tidyr
#'
#' @import dplyr
#'
#' @importFrom  zoo as.yearqtr
#'
#' @importFrom stringr str_replace_all str_remove_all
#'
#' @export
#'
import_boi_debt_by_sectors_df = function(debt_sectors_df_filepath = NULL){

  if(is.null(debt_sectors_df_filepath)){

    debt_sectors_df_filepath = paste0(
      file.path(Sys.getenv("USERPROFILE")),
      "\\OneDrive - Bank Of Israel\\Data\\",
      "BoI\\Credit\\itra_hov_bs.xlsx")
  }


  df = read_xlsx(debt_sectors_df_filepath, sheet = 1) %>%
    select(matches("[A-Z]")) %>%
    rename_all(tolower) %>%
    rename_all(~str_replace_all(.,"\\s","_")) %>%
    rename_all(~str_remove_all(., "_\\([a-z_]+\\)$")) %>%
    mutate(date = as.yearqtr(date))



  return(df)



}


#' This function imports credit cards categories
#'
#' @import tidyr
#'
#' @import dplyr
#'
#' @import readr
#'
#' @import lubridate
#'
#' @export
#'
import_boi_credit_cards_debt = function(df_filepath = NULL){

  if(is.null(df_filepath)){

    df_filepath = paste0(
      file.path(Sys.getenv("USERPROFILE")),
      "\\OneDrive - Bank Of Israel\\Data\\",
      "BoI\\Credit\\CreditCard-Sectors.csv")
  }

  raw_df = read_csv(df_filepath, skip = 1) %>%
    rename(date = 1) %>%
    slice(-c(1:14)) %>%
    mutate(date = dmy(date)) %>%
    mutate(across(-date, as.numeric))

  data_colnames_table = read_csv(paste0(
    file.path(Sys.getenv("USERPROFILE")),
    "\\Documents\\BoICredit\\analysis\\",
    "credit_cards_debt_categories.csv"))


  df = raw_df %>%
    pivot_longer(-date, names_to = "heb_name") %>%
    inner_join(select(data_colnames_table, - table_name),
               by = "heb_name") %>%
    select(-heb_name)


  return(df)

}


#' This function imports bond_market_players share
#'
#' @import tydir
#'
#' @import readr
#'
#' @import dplyr
#'
#' @import lubridate
#'
import_boi_corp_bond_players = function(file_path = NULL){

  if(is.null(file_path)){

    file_path = paste0(
      file.path(Sys.getenv("USERPROFILE")),
      "\\OneDrive - Bank Of Israel\\Data\\BoI\\Misc",
      "\\Corp_bond_market_players.csv")


  }


  df = read_csv(file_path)

  df = df %>%
    mutate(date = mdy(date))


  return(df)





}


#' This function imports corp_bond_market_data
#'
#' @import tydir
#'
#' @import readr
#'
#' @import dplyr
#'
#' @import lubridate
#'
import_boi_corp_bond_market_data = function(file_path = NULL){

  if(is.null(file_path)){

    file_path = paste0(
      file.path(Sys.getenv("USERPROFILE")),
      "\\OneDrive - Bank Of Israel\\Data",
      "\\BoI\\Corp Bond\\CorpBondData.Rds")


  }


  df = read_rds(file_path)

  df = df %>%
    rename_all(tolower) %>%
    rename(date = date_value,
           sec_id = security_ident_num_tase,
           yield_close = yld_close_bruto)

  df = df %>%
    mutate(across(c(date, maturity_date), ymd)) %>%
    mutate(across(c(market_value, turnover), ~ . * 10 ^ (-6)))


  return(df)





}

