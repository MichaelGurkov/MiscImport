utils::globalVariables("where")



#' @title Import institutional investor portfolio by asset class
#'
#' @description This function imports institutional investor
#' investor portfolio by asset class (in millions USD)
#'
#' @import readxl
#'
#' @importFrom zoo as.yearmon
#'
#' @import dplyr
#'
#' @import tidyr
#'

import_boi_institutional_portolio_asset_class = function(file_path = NULL,
                                                         download_file = FALSE,
                                                         pivot_to_long = TRUE){

  import_temp_sheet = function(temp_file_path,temp_sheet_name,
                               temp_cell_range = NULL){

    col_names_vec = c(
      "total_assets",
      "gov_bond-traded",
      "gov_bond-non_traded",
      "corp_bond-traded",
      "corp_bond-non_traded",
      "stocks-traded",
      "stocks-non_traded",
      "stocks-etf",
      "bond-etf",
      "foreign",
      "cash_and_deposits-linked",
      "cash_and_deposits-nominal",
      "makam",
      "other_assets"
    )

    investor_types_vec = c(
      "gemel",
      "hishtalmut",
      "pensia_vatikot",
      "pensia_claliot_hadashot",
      "pensia_mekifot_hadashot",
      "nemanut",
      "bituah_mishtatfot_berevahim",
      "bituah_mavtihot_tsua",
      "total"
    )

    empty_cols = c(5,8,11) - 1 # offset to start at column 2

    if(is.null(temp_cell_range)){

      temp_cell_range = cell_limits(ul = c(9,2),
                                    lr = c(NA_integer_,NA_integer_))

      }

    raw_df = suppressMessages(read_xls(temp_file_path,sheet = temp_sheet_name,
                                range = temp_cell_range))

    temp_df = raw_df %>%
      select(-empty_cols) %>%
      filter(complete.cases(.)) %>%
      set_names(col_names_vec) %>%
      mutate(across(-total_assets, ~ . / 100 * total_assets)) %>%
      select(-total_assets)

    if(temp_sheet_name == "2001"){

      investor_types_column = rep(str_subset(investor_types_vec,"bituah",
                                             negate = TRUE),11)
      investor_types_column = c(investor_types_column, investor_types_vec)


      months_column = c(rep(month.abb[-12],each = 7),rep("Dec",9))

    } else {

      investor_types_column = rep(investor_types_vec,
                                  (nrow(temp_df) / length(investor_types_vec)))

      months_column = rep(month.abb,
                          each = length(investor_types_vec))[1:nrow(temp_df)]


    }



    temp_df = temp_df %>%
      mutate(investor_type = investor_types_column) %>%
      mutate(date = months_column) %>%
      mutate(date = as.yearmon(paste(date, temp_sheet_name))) %>%
      relocate(date, investor_type)

    return(temp_df)


    return(temp_df)


  }

  file_name = "shce28_h.xls"

  source_link = paste0(
    "https://www.boi.org.il/he/",
    "DataAndStatistics/Lists/BoiTablesAndGraphs/",
    file_name)


  if (is.null(file_path)) {

    file_path = paste0(
      Sys.getenv("USERPROFILE"),
      "\\OneDrive - Bank Of Israel\\Data",
      "\\BoI\\institutional_investors\\",file_name)

  }

  if (download_file) {
    download.file(url = source_link,
                  destfile = file_path,
                  mode = "wb")

  }


  df = map_dfr(excel_sheets(file_path),
           import_temp_sheet,temp_file_path = file_path)


  if(pivot_to_long){

    df = df %>%
      pivot_longer(-c(date, investor_type),names_to = "asset_class")

  }


  return(df)





}



#' @title Import institutional investor foreign assets exposure balance
#'
#' @description This function imports institutional investor
#' foreign assets exposure balance (in millions USD)
#'
#' @import readxl
#'
#' @importFrom zoo as.yearmon
#'
#' @import dplyr
#'
#' @import tidyr
#'
#' @param report_category a string. Either ""balance" or "flows"

import_boi_institutional_foreign_assets_exposure = function(file_path = NULL,
                                                     download_file = FALSE,
                                                     pivot_to_long = TRUE) {

  file_name = "mosadiyim_l2h.xlsx"

  source_link = paste0(
    "https://www.boi.org.il/he",
    "/DataAndStatistics/Lists",
    "/BoiTablesAndGraphs/", file_name)

  row_indices_list = list(17:21, 23:27, 29:33, 35:39, 41:45) %>%
    map( ~ . - 2) # offset to start at row 3

  names(row_indices_list) = c(
    "gemel_hishtalmut",
    "pensia_hadashot",
    "pensia_vatikot",
    "bituah_mishtatfot_berevahim",
    "bituah_mavtihot_tsua"
  )

  cell_limits = cell_limits(ul = c(2, 2),
                            lr = c(NA_integer_,
                                   NA_integer_))

  categories = c(
    "balance_assets",
    "derivative_assets",
    "exposure",
    "total_assets",
    "exposure_rate"
  )

  if (is.null(file_path)) {
    file_path = paste0(
      Sys.getenv("USERPROFILE"),
      "\\OneDrive - Bank Of Israel\\Data",
      "\\BoI\\institutional_investors\\",file_name)

  }


  if (download_file) {
    download.file(url = source_link,
                  destfile = file_path,
                  mode = "wb")

  }


  raw_df = read_xlsx(file_path, range = cell_limits)

  df = map_dfr(row_indices_list, function(temp_ind) {
    temp_df = raw_df %>%
      slice(c(1, temp_ind)) %>%
      t() %>%
      as_tibble(.name_repair = "minimal") %>%
      set_names(c("date", categories))

  },
  .id = "investor_type")


  df = df %>%
    mutate(date = as.yearmon(date, format = "%m-%y")) %>%
    mutate(across(-c("investor_type", "date"), as.numeric))

  if (pivot_to_long) {
    df = df %>%
      pivot_longer(-c("investor_type", "date"),
                   names_to = "category")

  }

  return(df)


}


#' @title Import institutional investor FX exposure
#'
#' @description This function imports institutional investor
#' FX exposure
#'
#' @import readxl
#'
#' @importFrom zoo as.yearmon
#'
#' @import dplyr
#'
#' @import tidyr
#'
#' @param report_category a string. Either ""balance" or "flows"

import_boi_institutional_FX_exposure = function(file_path = NULL,
                                                     download_file = FALSE,
                                                     pivot_to_long = TRUE,
                                                     report_category) {

  file_name = "mosadiyim_l1h.xlsx"

  cell_limits = cell_limits(ul = c(2, 2),
                            lr = c(NA_integer_,
                                   NA_integer_))

  source_link = paste0(
    "https://www.boi.org.il/he",
    "/DataAndStatistics/Lists",
    "/BoiTablesAndGraphs/", file_name)

  if (is.null(file_path)) {
    file_path = paste0(
      Sys.getenv("USERPROFILE"),
      "\\OneDrive - Bank Of Israel\\Data",
      "\\BoI\\institutional_investors\\",file_name)

  }

  if (download_file) {
    download.file(url = source_link,
                  destfile = file_path,
                  mode = "wb")

  }

  if(report_category == "balance"){

   sheet_ind = 1

    row_indices_list = list(19:24, 26:31, 33:38, 40:45, 47:52) %>%
      map( ~ . - 2) # offset to start at row 3

    categories = c(
      "balance_assets",
      "derivative_assets",
      "exposure",
      "total_assets",
      "balance_exposure_rate",
      "total_exposure_rate"
    )


  }

  if(report_category == "flows"){

    sheet_ind = 2

    row_indices_list = list(15:18, 20:23, 25:28, 30:33, 35:38) %>%
      map( ~ . - 2) # offset to start at row 3


    categories = c(
      "balance_assets",
      "derivative_assets",
      "exposure",
      "total_assets"
    )


  }


  names(row_indices_list) = c(
    "gemel_hishtalmut",
    "pensia_hadashot",
    "pensia_vatikot",
    "bituah_mishtatfot_berevahim",
    "bituah_mavtihot_tsua"
  )


  raw_df = read_xlsx(file_path, range = cell_limits, sheet = sheet_ind)

  df = map_dfr(row_indices_list, function(temp_ind) {
    temp_df = raw_df %>%
      slice(c(1, temp_ind)) %>%
      t() %>%
      as_tibble(.name_repair = "minimal") %>%
      set_names(c("date", categories))

  },
  .id = "investor_type")


  df = df %>%
    mutate(date = as.yearmon(date, format = "%m-%y")) %>%
    mutate(across(-c("investor_type", "date"), as.numeric))

  if (pivot_to_long) {
    df = df %>%
      pivot_longer(-c("investor_type", "date"),
                   names_to = "category")

  }

  return(df)



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


#' @title  This function returns public assets data categorized by asset class
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

    source_link = paste0("https://www.boi.org.il/he/",
                  "DataAndStatistics/Lists/BoiTablesAndGraphs/tnc04_h.xls")

    download.file(url = source_link,destfile = file_path,mode = "wb")


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


  return(df)

}


#' @title  This function returns public assets data categorized by investment vehicle
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

import_boi_public_assets_by_institution_type = function(file_path = NULL,
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
    "bituah_mishtatfot_berevahim-institutional_holdings",
    "nemanut-institutional_holdings",
    "cash_and_deposits-direct_holdings",
    "gov_bond_traded-direct_holdings",
    "corp_bond_traded-direct_holdings",
    "stocks_domestics-direct_holdings",
    "foreign_investments-direct_holdings",
    "other_assets-direct_holdings"

  )

  file_name = "tnc07_h.xls"

  if(is.null(file_path)){

    file_path = paste0(Sys.getenv("USERPROFILE"),
                       "\\OneDrive - Bank Of Israel\\Data",
                       "\\BoI\\public_assets\\", file_name)


  }


  if(download_file){

    source_link = paste0("https://www.boi.org.il/he/",
                         "DataAndStatistics/Lists/BoiTablesAndGraphs/",
                         file_name)

    download.file(url = source_link,destfile = file_path,mode = "wb")


  }


  raw_df = read_xlsx(file_path,sheet = 2,range = cell_limits(c(11, 2), c(NA, NA)))

  df = raw_df %>%
    select(-2,-4,-12,-13,-14) %>%
    set_names(names_vec) %>%
    mutate(date = as.Date(as.numeric(date), origin = "1899-12-30")) %>%
    filter(!is.na(date)) %>%
    mutate(across(-c(date, `total_assets-total`),
                  ~ . * `total_assets-total` / 100))

  if(pivot_to_long){

    df = df %>%
      pivot_longer(-date,names_to = "asset_class") %>%
      separate(col = asset_class,into = c("asset_class","investment_vehicle"),
               sep = "-")

  }


  return(df)

}


#' @title  This is an auxiliary function that returns generic pension funds flows
#' accounting
#'
#' @import readxl
#'
#' @import purrr
#'
#' @import stringr
#'
#' @import tidyr
#'
#' @import dplyr
#'
#' @import lubridate
#'

import_boi_pension_generic_flows = function(file_path = NULL,
                                                  source_link = NULL,
                                                  generic_data_type){

  names_vec = c(
    "date",
    "deposits",
    "withdrawals",
    "accumulated_savings",
    "gov_bond-traded",
    "gov_bond-earmarked",
    "corp_bond-traded",
    "corp_bond-non_traded",
    "stocks-traded",
    "stocks-non_traded",
    "stocks-etf",
    "bond-etf",
    "foreign",
    "cash_and_deposits-linked",
    "cash_and_deposits-nominal",
    "makam",
    "other_payments"
  )

  if(!is.null(source_link)){

    download.file(url = source_link,destfile = file_path,mode = "wb")


  }


  raw_df = read_xls(file_path,sheet = 1,range = cell_limits(c(8, 1), c(NA, NA)))

  empty_cols = c(5,8,11,14)

  df = raw_df %>%
    select(-empty_cols) %>%
    set_names(names_vec) %>%
    mutate(date = as.Date(as.numeric(date), origin = "1899-12-30")) %>%
    filter(!is.na(date))

  if(pivot_to_long & data_type == "assets_composition"){

    df = df %>%
      select(-c("deposits","withdrawals","accumulated_savings")) %>%
      pivot_longer(-date,names_to = "asset_category") %>%
      separate(col = asset_category,into = c("asset_category","asset_characteristic"),
               sep = "-")

  }

  if(pivot_to_long & data_type == "total_flows"){

    df = df %>%
      select(c("date","deposits","withdrawals","accumulated_savings")) %>%
      pivot_longer(-date,names_to = "flow_category")

  }


  return(df)

}


#' @title  This is an auxiliary function that returns generic institutional
#' investors flows accounting
#'
#' @import readxl
#'
#' @import stringr
#'
#' @import tidyr
#'
#' @import dplyr
#'
#' @import lubridate
#'
#'

import_boi_pension_generic_balance = function(file_path = NULL,
                                                  source_link = NULL,
                                                  generic_data_type,
                                                  pivot_to_long = TRUE){

  names_vec = c(
    "date",
    "total_assets",
    "gov_bond-traded",
    "gov_bond-earmarked",
    "corp_bond-traded",
    "corp_bond-not_traded",
    "stocks-traded",
    "stocks-not_traded",
    "stocks-etf",
    "bond-etf",
    "foreign",
    "cash_and_deposits-linked",
    "cash_and_deposits-nominal",
    "makam",
    "other_payments"
  )


  if(!is.null(source_link)){

    download.file(url = source_link,destfile = file_path,mode = "wb")


  }


  raw_df = read_xls(file_path,sheet = 1,range = cell_limits(c(8, 1), c(NA, NA)))

  empty_cols = c(5,8,11)

  df = raw_df %>%
    select(-empty_cols) %>%
    set_names(names_vec) %>%
    mutate(date = as.Date(as.numeric(date), origin = "1899-12-30")) %>%
    filter(!is.na(date)) %>%
    mutate(across(-c(date, `total_assets`), ~ . * `total_assets` / 100))

  if(pivot_to_long){

    df = df %>%
      pivot_longer(-date,names_to = "asset_class") %>%
      separate(col = asset_class,into = c("asset_class","asset_category"),
               sep = "-")

  }



  return(df)

}

#' @title  This function returns institutional investors flows accounting
#'
#' @import readxl
#'
#' @import stringr
#'
#' @import tidyr
#'
#' @import dplyr
#'
#' @import lubridate
#'
#' @export
#'


import_boi_pension_funds_flows = function(download_file = FALSE,
                                                    data_type = "assets_composition",
                                                    pivot_to_long = TRUE){

  files_table = tribble(
    ~ category,
    ~ temp_source_link,
    "pensia_vatikot",
    "https://www.boi.org.il/he/DataAndStatistics/Lists/BoiTablesAndGraphs/shce19_h.xls",
    "pensia_mekifot_hadashot",
    "https://www.boi.org.il/he/DataAndStatistics/Lists/BoiTablesAndGraphs/shce21_h.xls",
    "pensia_claliot_hadashot",
    "https://www.boi.org.il/he/DataAndStatistics/Lists/BoiTablesAndGraphs/shce23_h.xls"
  )




  files_table = files_table %>%
    mutate(temp_file_path = map_chr(temp_source_link,
                                    ~str_extract(.,pattern = "shce.*$"))) %>%
    mutate(temp_file_path = paste0(Sys.getenv("USERPROFILE"),
                                   "\\OneDrive - Bank Of Israel\\Data",
                                   "\\BoI\\institutional_investors\\",
                                   temp_file_path))


  if(download_file){


    df =  files_table %>%
      pmap_dfr(function(category,temp_source_link,temp_file_path){

        temp_df = import_boi_pension_generic_flows(
          file_path = temp_file_path,
          source_link = temp_source_link,
          generic_data_type = data_type) %>%
          mutate(investor_type = category)


      })


  }
  else {


    df =  files_table %>%
      pmap_dfr(function(category,temp_source_link,temp_file_path){

        temp_df = import_boi_pension_generic_flows(
          temp_file_path,
          generic_data_type = data_type) %>%
          mutate(investor_type = category)


      })

  }


  return(df)



}


#' @title  This function returns institutional investors balance accounting
#'
#' @import readxl
#'
#' @import purrr
#'
#' @import stringr
#'
#' @import tidyr
#'
#' @import dplyr
#'
#' @import lubridate
#'
#' @export
#'

import_boi_pension_funds_balance = function(download_file = FALSE,
                                                    pivot_to_long = TRUE){

  files_table = tribble(
    ~ category,
    ~ temp_source_link,
    "pensia_vatikot",
    "https://www.boi.org.il/he/DataAndStatistics/Lists/BoiTablesAndGraphs/shce16_h.xls",
    "pensia_mekifot_hadashot",
    "https://www.boi.org.il/he/DataAndStatistics/Lists/BoiTablesAndGraphs/shce20_h.xls",
    "pensia_claliot_hadashot",
    "https://www.boi.org.il/he/DataAndStatistics/Lists/BoiTablesAndGraphs/shce22_h.xls"
  )




  files_table = files_table %>%
    mutate(temp_file_path = map_chr(temp_source_link,
                                    ~str_extract(.,pattern = "shce.*$"))) %>%
    mutate(temp_file_path = paste0(Sys.getenv("USERPROFILE"),
                                   "\\OneDrive - Bank Of Israel\\Data",
                                   "\\BoI\\institutional_investors\\",
                                   temp_file_path))


  if(download_file){


    df =  files_table %>%
      pmap_dfr(function(category,temp_source_link,temp_file_path){

        temp_df = import_boi_pension_generic_balance(
          file_path = temp_file_path,
          source_link = temp_source_link) %>%
          mutate(investor_type = category)


      })


  }
  else {


    df =  files_table %>%
      pmap_dfr(function(category,temp_source_link,temp_file_path){

        temp_df = import_boi_pension_generic_balance(temp_file_path) %>%
          mutate(investor_type = category)


      })

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
