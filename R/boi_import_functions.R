utils::globalVariables("where")


#' This function imports issuance data from cognus
#'
#' @import readxl
#'
#' @import dplyr
#'
#' @import tidyr
#'
#' @importFrom magrittr set_names
#'
#' @importFrom zoo as.yearmon
#'
#' @param sec_type string that specifies the type of securites (stock or bond)
#'
#' @export
#'
import_boi_cognus_issuance_data = function(file_path = NULL,
                                           sec_type = "stock"){

  if(is.null(file_path)){

    file_path = paste0(Sys.getenv("USERPROFILE"),
                       "\\OneDrive - Bank Of Israel\\Data",
                       "\\BoI\\securities_issuance\\cognus_issuance_data.xlsx")

  }

  if(sec_type == "stock"){

   stocks_names = c("month","year","tase_num","issue_israel",
                    "issue_foreign")


   stocks_data = read_xlsx(file_path, sheet = 2)

   stocks_data_clean = stocks_data %>%
      select(c(1,2,4,12,13)) %>%
      purrr::set_names(stocks_names) %>%
      unite(date, c("month","year"), sep = "-") %>%
      mutate(date = as.yearmon(date, format = "%m-%Y"))


  return(stocks_data_clean)




  }


  if(sec_type == "bond"){

    bonds_names = c("month","year","tase_num","issue_israel",
                    "issue_foreign", "sec_num")


    bonds_data = read_xlsx(file_path, sheet = 1)

    bonds_data_clean = bonds_data %>%
      select(c(1:3,16:18)) %>%
      purrr::set_names(bonds_names) %>%
      unite(date, c("month","year"), sep = "-") %>%
      mutate(date = as.yearmon(date, format = "%m-%Y"))

    return(bonds_data_clean)



  }

}


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


