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
import_boi_debt_df = function(debt_df_filepath = NULL,
                                data_frequency = "month"){

  if(is.null(debt_df_filepath)){

    debt_df_filepath = paste0(
      file.path(Sys.getenv("USERPROFILE")),
      "\\OneDrive - Bank Of Israel\\Data\\",
      "BoI\\Credit\\itrchovh.xlsx")
  }

  sheets_names = excel_sheets(debt_df_filepath)


  data_format = return_datafile_format(data_freq = "month",
                                       data_type = "debt")


  if(data_frequency == "month"){

    monthly_df = read_xlsx(debt_df_filepath,sheet = sheets_names[2],
                           range = cell_limits(ul = c(8,1),
                                               lr = c(77,NA_integer_)))

    df = monthly_df %>%
      slice(data_format$row_num) %>%
      select(-1) %>%
      bind_cols(select(data_format, -row_num)) %>%
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
#' @import tidyr
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
#' @import tidyr
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


#' @import readxl
#'
#' @import tidyr
#'
#' @import dplyr
#'
#' @import lubridate
#'
#' @export

import_boi_public_assets_by_asset_class = function(file_path = NULL,
                                                   download_file = FALSE,
                                                   pivot_to_long = TRUE){

  names_vec = c(
    "date",
    "total_assets",
    "cash_and_deposits",
    "gov_bond_traded",
    "gov_bond_not_traded",
    "corp_bond_traded",
    "corp_bond_not_traded",
    "makam",
    "stocks_domestics",
    "deposits_foreign",
    "bonds_foreign",
    "stocks_foreign",
    "other_foreign"
  )

  if(is.null(file_path)){

    file_path = paste0(Sys.getenv("USERPROFILE"),
                       "\\OneDrive - Bank Of Israel\\Data",
                       "\\BoI\\public_assets\\tnc04_h.xls")


  }


  if(download_file){

    target_link = paste0("https://www.boi.org.il/he/",
                  "DataAndStatistics/Lists/BoiTablesAndGraphs/tnc04_h.xls")

    download.file(url = target_link,destfile = file_path,mode = "wb")


  }


  if(is.null(file_path)){

    file_path = paste0(Sys.getenv("USERPROFILE"),
                                  "\\OneDrive - Bank Of Israel\\Data",
                                  "\\BoI\\public_assets\\tnc04_h.xls")


  }

  raw_df = read_xlsx(file_path,sheet = 2,range = cell_limits(c(11, 2), c(NA, NA)))

  df = raw_df %>%
    select(-2,-4) %>%
    set_names(names_vec) %>%
    mutate(date = as.Date(as.numeric(date), origin = "1899-12-30")) %>%
    filter(!is.na(date)) %>%
    mutate(across(-c(date, total_assets), ~ . * total_assets / 100))

  if(pivot_to_long){

    df = df %>%
      pivot_longer(-date,names_to = "asset_category")

  }


  return(df)

}


#' @import readxl
#'
#' @import tidyr
#'
#' @import dplyr
#'
#' @import lubridate
#'
#' @export

import_boi_public_assets_by_investment_vehicle = function(file_path = NULL,
                                                   download_file = FALSE,
                                                   pivot_to_long = TRUE){

  names_vec = c(
    "date",
    "total_assets-total",
    "gemel-institutional_holdings",
    "hishtalmut-institutional_holdings",
    "pensia_vatikot-institutional_holdings",
    "pensia_hadashot-institutional_holdings",
    "bituah_mavtihot_tsua-institutional_holdings",
    "bituah_mishtatfot_bereavihim-institutional_holdings",
    "nemanut-institutional_holdings",
    "cash_and_deposits-direct_holdings",
    "gov_bond_traded-direct_holdings",
    "corp_bond_traded-direct_holdings",
    "stocks_domestics-direct_holdings",
    "foreign_investments-direct_holdings",
    "other_assets-direct_holdings"

  )

  if(is.null(file_path)){

    file_path = paste0(Sys.getenv("USERPROFILE"),
                       "\\OneDrive - Bank Of Israel\\Data",
                       "\\BoI\\public_assets\\tnc07_h.xls")


  }


  if(download_file){

    target_link = paste0("https://www.boi.org.il/he/",
                         "DataAndStatistics/Lists/BoiTablesAndGraphs/tnc04_h.xls")

    download.file(url = target_link,destfile = file_path,mode = "wb")


  }


  raw_df = read_xlsx(file_path,sheet = 2,range = cell_limits(c(11, 2), c(NA, NA)))

  df = raw_df %>%
    select(-2,-4,-12,-13,-14) %>%
    set_names(names_vec) %>%
    mutate(date = as.Date(as.numeric(date), origin = "1899-12-30")) %>%
    filter(!is.na(date)) %>%
    mutate(across(-c(date, `total_assets-total`), ~ . * `total_assets-total` / 100))

  if(pivot_to_long){

    df = df %>%
      pivot_longer(-date,names_to = "asset_category") %>%
      separate(col = asset_category,into = c("asset_category","investment_vehicle"),
               sep = "-")

  }


  return(df)

}



# This function returns data format

return_datafile_format = function(data_freq = "month",
                                  data_type = "debt"){

  month_debt_format = tibble::tribble(
    ~row_num,~lender,~borrower,~instrument,~category,
    "2","banks","business_sector","all_instruments","na",
    "3","banks","business_sector","loans","na",
    "4","banks","business_sector","traded_bonds","na",
    "5","institutional","business_sector","all_instruments","na",
    "6","institutional","business_sector","loans","na",
    "7","institutional","business_sector","traded_bonds","na",
    "8","institutional","business_sector","non_traded_bonds","na",
    "9","credit_card","business_sector","all_instruments","na",
    "10","credit_card","business_sector","loans","na",
    "12","foreign","business_sector","all_instruments","na",
    "13","foreign","business_sector","loans","na",
    "14","foreign","business_sector","traded_bonds","na",
    "15","foreign","business_sector","non_traded_bonds","na",
    "17","gov_sector","business_sector","all_instruments","na",
    "18","gov_sector","business_sector","allocated_credit","na",
    "19","households","business_sector","all_instruments","na",
    "20","households","business_sector","traded_bonds","na",
    "29","banks","households_sector","na","total",
    "30","banks","households_sector","na","non_residental",
    "32","banks","households_sector","na","residental",
    "33","institutional","households_sector","na","total",
    "34","institutional","households_sector","na","non_residental",
    "35","institutional","households_sector","na","residental",
    "36","credit_card","households_sector","na","total",
    "37","credit_card","households_sector","na","non_residental",
    "39","foreign","households_sector","na","total",
    "40","foreign","households_sector","na","non_residental",
    "55","banks","gov_sector","all_instruments","na",
    "56","banks","gov_sector","loans","na",
    "57","banks","gov_sector","traded_bonds","na",
    "58","institutional","gov_sector","all_instruments","na",
    "59","institutional","gov_sector","loans","na",
    "60","institutional","gov_sector","traded_bonds","na",
    "61","institutional","gov_sector","non_traded_bonds","na"
  ) %>%
    mutate(across(everything(), ~na_if(.,"na"))) %>%
    mutate(row_num = as.numeric(row_num))



  if(data_freq == "month" & data_type == "debt"){


    data_file_format = month_debt_format

  }

return(data_file_format)



}
