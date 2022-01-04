utils::globalVariables("where")


#' @title Import traded corporate bonds holdings by investor type
#'
#' @description This function imports traded corporate bonds holdings
#'  by investor type
#'
#' @import readxl
#'
#' @importFrom zoo as.yearmon
#'
#' @import dplyr
#'
#' @import tidyr
#'
#' @export
#'

import_boi_corporate_bonds_holdings = function(file_path = NULL,
                                               download_file = FALSE,
                                               pivot_to_long = TRUE){

  file_name = "tnc12_h.xls"

  source_link = paste0(
    "https://www.boi.org.il/he/DataAndStatistics",
    "/Lists/BoiTablesAndGraphs/",file_name)

  empty_cols = c(2,4)

  col_names = c("date",
                "market_cap",
                "other",
                "nemanut",
                "gemel_hishtalmut",
                "pensia",
                "banks",
                "foreign_investors",
                "insurance",
                "bank_of_israel")


  if (is.null(file_path)) {

    file_path = paste0(
      Sys.getenv("USERPROFILE"),
      "\\OneDrive - Bank Of Israel\\Data",
      "\\BoI\\corp_bonds\\",file_name)

  }

  if (download_file) {

    download.file(url = source_link,
                  destfile = file_path,
                  mode = "wb")

  }


  raw_df = suppressMessages(
    read_xlsx(file_path, sheet = 2,
              range = cell_limits(ul = c(11,2),
                                  lr = c(NA_integer_, NA_integer_))))

  df = raw_df %>%
    select(-all_of(empty_cols)) %>%
    set_names(col_names) %>%
    filter(complete.cases(.)) %>%
    mutate(date = as.Date(as.numeric(date), origin = "1899-12-30")) %>%
    mutate(date = as.yearmon(date)) %>%
    mutate(across(-c(date, market_cap), ~ . / 100 * market_cap)) %>%
    select(-market_cap) %>%
    pivot_longer(-date,names_to = "investor_type")



  return(df)





}

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
#' @export
#'

import_boi_institutional_portolio_asset_class = function(file_path = NULL,
                                                         download_file = FALSE,
                                                         pivot_to_long = TRUE){

  import_temp_sheet = function(temp_file_path,temp_sheet_name,
                               temp_cell_range = NULL){

    col_names_vec = c(
      "total_assets",
      "gov_bond-traded",
      "gov_bond-not_traded",
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
      "other_assets"
    )

    investor_types_vec = c(
      "gemel",
      "hishtalmut",
      "pensia_vatikot",
      "pensia_claliot_hadashot",
      "pensia_mekifot_hadashot",
      "nemanut",
      "bituah_mavtihot_tsua",
      "bituah_mishtatfot_berevahim",
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
      relocate(date, investor_type) %>%
      filter(!investor_type == "total")

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


#' This function imports credit data from BOI website
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
import_boi_credit_df = function(file_path = NULL,
                                download_file = FALSE,
                                data_frequency = "quarter"){

  file_name = "itrashrh.xlsx"

  source_link = paste0(
    "https://www.boi.org.il/he/DataAndStatistics",
    "/Lists/BoiTablesAndGraphs/", file_path)

  if(is.null(file_path)){

    file_path = paste0(
      file.path(Sys.getenv("USERPROFILE")),
      "\\OneDrive - Bank Of Israel\\Data\\",
      "BoI\\Credit\\",file_name)
  }

  if(download_file){

    download.file(source_link, file_path,mode = "wb")

  }

  sheets_names = excel_sheets(file_path)

  data_format_filepath = paste0(
    file.path(Sys.getenv("USERPROFILE")),
    "\\Documents\\BoICredit\\analysis\\BOI_data_format.csv")

  data_format = readr::read_csv(data_format_filepath)

  if(data_frequency == "quarter"){

    quarterly_df = read_xlsx(file_path,sheet = sheets_names[1],
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

    monthly_df = read_xlsx(file_path,sheet = sheets_names[2],
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

#' This function imports debt data from BOI website
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
import_boi_debt_df = function(file_path = NULL,
                              download_file = FALSE,
                              data_frequency = "month"){

  file_name = "itrchovh.xlsx"

  source_link = paste0(
    "https://www.boi.org.il/he/DataAndStatistics",
    "/Lists/BoiTablesAndGraphs/", file_name)

  if(is.null(file_path)){

    file_path = paste0(
      Sys.getenv("USERPROFILE"),
      "\\OneDrive - Bank Of Israel\\Data\\",
      "BoI\\Credit\\", file_name)
  }

  if(download_file){

    download.file(source_link, file_path,mode = "wb")

  }


  sheets_names = excel_sheets(file_path)


  data_format = return_datafile_format(data_freq = "month",
                                       data_type = "debt")


  if(data_frequency == "month"){

    monthly_df = read_xlsx(file_path,sheet = sheets_names[2],
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
import_boi_debt_by_sectors_df = function(file_path = NULL,
                                         download_file = FALSE){

  source_link = paste0("https://www.boi.org.il/he/DataAndStatistics",
                       "/Lists/BoiTablesAndGraphs/itra_hov_bs.xlsx")

  if(is.null(file_path)){

    file_path = paste0(
      file.path(Sys.getenv("USERPROFILE")),
      "\\OneDrive - Bank Of Israel\\Data\\",
      "BoI\\Credit\\itra_hov_bs.xlsx")
  }


  if(download_file){

    download.file(url = source_link,destfile =  file_path,mode = "wb")


  }


  df = read_xlsx(file_path, sheet = 1) %>%
    select(matches("[A-Z]")) %>%
    rename_all(tolower) %>%
    rename_all(~str_replace_all(.,"\\s","_")) %>%
    rename_all(~str_remove_all(., "_\\([a-z_]+\\)$")) %>%
    mutate(date = as.yearqtr(date))



  return(df)



}


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


#' @title  This is an auxiliary function that returns generic flows
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

import_boi_generic_flows = function(file_path = NULL,
                                            source_link = NULL,
                                            start_row = 8,
                                            generic_pivot_to_long,
                                            generic_data_type){

  if(!generic_data_type %in% c("assets_composition","total_flows")){

    stop("data_type argument must be either assets_composition or total_flows")
  }


  names_vec = c(
    "date",
    "deposits",
    "withdrawals",
    "accumulated_savings",
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


  raw_df = suppressMessages(read_xls(file_path,sheet = 1,
                                     range = cell_limits(c(start_row, 1),
                                                         c(NA, NA))))

  empty_cols = c(5,8,11,14)

  df = raw_df %>%
    select(-empty_cols) %>%
    set_names(names_vec) %>%
    mutate(date = as.Date(as.numeric(date), origin = "1899-12-30")) %>%
    filter(!is.na(date))

  if(generic_pivot_to_long & generic_data_type == "assets_composition"){

    df = df %>%
      select(-c("deposits", "withdrawals", "accumulated_savings")) %>%
      pivot_longer(-date, names_to = "asset_class")

  }

  if(generic_pivot_to_long & generic_data_type == "total_flows"){

    df = df %>%
      select(c("date","deposits","withdrawals","accumulated_savings")) %>%
      pivot_longer(-date,names_to = "flow_category")

  }


  return(df)

}


#' @title  This is an auxiliary function that returns generic pension balance
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


  raw_df = suppressMessages(read_xls(file_path,sheet = 1,
                                     range = cell_limits(c(8, 1), c(NA, NA))))

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
      filter(!asset_class == "total_assets")

  }



  return(df)

}


#' @title  This is an auxiliary function that returns generic pension balance
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

import_boi_insurance_generic_balance = function(file_path = NULL,
                                              source_link = NULL,
                                              pivot_to_long = TRUE){

  names_vec = c(
    "date",
    "total_assets",
    "gov_bond-traded",
    "gov_bond-earmarked",
    "corp_bond-traded",
    "corp_bond-not_traded",
    "makam",
    "stocks",
    "stocks-etf",
    "bond-etf",
    "mutual_fund_shares",
    "cash_and_deposits",
    "loans",
    "real_estate",
    "foreign",
    "other_payments"
  )


  if(!is.null(source_link)){

    download.file(url = source_link,destfile = file_path,mode = "wb")


  }


  raw_df = suppressMessages(read_xls(file_path,sheet = 1,
                                     range = cell_limits(c(8, 1), c(NA, NA))))

  empty_cols = c(5,8,14)

  df = raw_df %>%
    select(-empty_cols) %>%
    set_names(names_vec) %>%
    mutate(date = as.Date(as.numeric(date), origin = "1899-12-30")) %>%
    filter(!is.na(date)) %>%
    mutate(across(-c(date, `total_assets`), ~ . * `total_assets` / 100))

  if(pivot_to_long){

    df = df %>%
      pivot_longer(-date,names_to = "asset_class")

  }



  return(df)

}

#' @title  This function returns institutional funds flows data
#'
#' @description The function returns two types of data
#'
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
#' @param data_type a string.
#'
#' \itemize{
#'  \item{"assets_composition"}{The default. Returns a time series of
#'   pension funds net inflows by asset class}
#'  \item{"total_flows"}{Returns a time series of
#'   pension funds deposits, withdrawals and accumulated_savings}
#' }
#'
#' @export
#'


import_boi_institutional_funds_flows = function(download_file = FALSE,
                                          data_type = "assets_composition",
                                          pivot_to_long = TRUE){

  files_table = tribble(
    ~ category,
    ~ temp_source_link,
    ~ temp_start_row,
    "pensia_vatikot",
    paste0("https://www.boi.org.il/he/DataAndStatistics",
           "/Lists/BoiTablesAndGraphs/shce19_h.xls"),
    8,
    "pensia_mekifot_hadashot",
    paste0("https://www.boi.org.il/he/DataAndStatistics",
           "/Lists/BoiTablesAndGraphs/shce21_h.xls"),
    8,
    "pensia_claliot_hadashot",
    paste0("https://www.boi.org.il/he/DataAndStatistics",
           "/Lists/BoiTablesAndGraphs/shce23_h.xls"),
    8,
    "gemel",
    paste0("https://www.boi.org.il/he/DataAndStatistics",
           "/Lists/BoiTablesAndGraphs/shce17_h.xls"),
    9,
    "hishtalmut",
    paste0("https://www.boi.org.il/he/DataAndStatistics",
           "/Lists/BoiTablesAndGraphs/shce18_h.xls"),
    8

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
      pmap_dfr(function(category,temp_source_link,temp_file_path,
                        temp_start_row){

        temp_df = import_boi_generic_flows(
          file_path = temp_file_path,
          source_link = temp_source_link,
          start_row = temp_start_row,
          generic_pivot_to_long = pivot_to_long,
          generic_data_type = data_type) %>%
          mutate(investor_type = category)


      })


  }
  else {


    df =  files_table %>%
      pmap_dfr(function(category,temp_source_link,temp_file_path,
                        temp_start_row){

        temp_df = import_boi_generic_flows(
          file_path = temp_file_path,
          start_row = temp_start_row,
          generic_pivot_to_long = pivot_to_long,
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
    paste0("https://www.boi.org.il/he/DataAndStatistics",
           "/Lists/BoiTablesAndGraphs/shce16_h.xls"),
    "pensia_mekifot_hadashot",
    paste0("https://www.boi.org.il/he/DataAndStatistics",
           "/Lists/BoiTablesAndGraphs/shce20_h.xls"),
    "pensia_claliot_hadashot",
    paste0("https://www.boi.org.il/he/DataAndStatistics",
           "/Lists/BoiTablesAndGraphs/shce22_h.xls")
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


import_boi_pension_funds_flows = function(download_file = FALSE,
                                          data_type = "assets_composition",
                                          pivot_to_long = TRUE){

  files_table = tribble(
    ~ category,
    ~ temp_source_link,
    "pensia_vatikot",
    paste0("https://www.boi.org.il/he/DataAndStatistics",
           "/Lists/BoiTablesAndGraphs/shce19_h.xls"),
    "pensia_mekifot_hadashot",
    paste0("https://www.boi.org.il/he/DataAndStatistics",
           "/Lists/BoiTablesAndGraphs/shce21_h.xls"),
    "pensia_claliot_hadashot",
    paste0("https://www.boi.org.il/he/DataAndStatistics",
           "/Lists/BoiTablesAndGraphs/shce23_h.xls")
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

        temp_df = import_boi_generic_flows(
          file_path = temp_file_path,
          source_link = temp_source_link,
          generic_pivot_to_long = pivot_to_long,
          generic_data_type = data_type) %>%
          mutate(investor_type = category)


      })


  }
  else {


    df =  files_table %>%
      pmap_dfr(function(category,temp_source_link,temp_file_path){

        temp_df = import_boi_generic_flows(
          temp_file_path,
          generic_pivot_to_long = pivot_to_long,
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

import_boi_insurance_balance = function(download_file = FALSE,
                                            pivot_to_long = TRUE){

  files_table = tribble(
    ~ category,
    ~ temp_source_link,
    "bituah_mavtihot_tsua",
    paste0("https://www.boi.org.il/he/DataAndStatistics",
           "/Lists/BoiTablesAndGraphs/shce24_h.xls"),
    "bituah_mishtatfot_berevahim",
    paste0("https://www.boi.org.il/he/DataAndStatistics",
           "/Lists/BoiTablesAndGraphs/shce25_h.xls")
  )




  files_table = files_table %>%
    mutate(temp_file_path = map_chr(temp_source_link,
                                    ~str_extract(.,pattern = "shce.*$"))) %>%
    mutate(temp_file_path = paste0(Sys.getenv("USERPROFILE"),
                                   "\\OneDrive - Bank Of Israel\\Data",
                                   "\\BoI\\institutional_investors\\insurance\\",
                                   temp_file_path))


  if(download_file){


    df =  files_table %>%
      pmap_dfr(function(category,temp_source_link,temp_file_path){

        temp_df = import_boi_insurance_generic_balance(
          file_path = temp_file_path,
          source_link = temp_source_link) %>%
          mutate(investor_type = category)


      })


  }
  else {


    df =  files_table %>%
      pmap_dfr(function(category,temp_source_link,temp_file_path){

        temp_df = import_boi_insurance_generic_balance(temp_file_path) %>%
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
    "8","institutional","business_sector","not_traded_bonds","na",
    "9","credit_card","business_sector","all_instruments","na",
    "10","credit_card","business_sector","loans","na",
    "12","foreign","business_sector","all_instruments","na",
    "13","foreign","business_sector","loans","na",
    "14","foreign","business_sector","traded_bonds","na",
    "15","foreign","business_sector","not_traded_bonds","na",
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
    "61","institutional","gov_sector","not_traded_bonds","na"
  ) %>%
    mutate(across(everything(), ~na_if(.,"na"))) %>%
    mutate(row_num = as.numeric(row_num))



  if(data_freq == "month" & data_type == "debt"){


    data_file_format = month_debt_format

  }

  return(data_file_format)



}
