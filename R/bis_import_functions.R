#' This function imports policy rates from BIS format
#'
#' @importFrom  readr read_csv cols
#'
#' @importFrom zoo as.yearmon
#'
#' @importFrom stats complete.cases
#'
#' @importFrom rlang .data
#'
#' @importFrom  tidyr pivot_longer
#'
#' @import magrittr
#'
#' @importFrom stringr str_replace_all
#'
#' @import dplyr
#'
#' @param file_path string a file path to the source data file
#'
#' @export
#'
import_bis_policy_rates = function(file_path){

  . = NULL

  filtered_df = read_csv(file_path,col_types = cols()) %>%
    select(-.data$`Time Period`) %>%
    select(-.data$`Frequency`) %>%
    select(-matches("^[A-Z_]+$", ignore.case = FALSE)) %>%
    rename_all( ~ str_replace_all(., " ", "_")) %>%
    rename_all( ~ str_replace_all(., "_-_", "_")) %>%
    rename_all(~ str_remove_all(., "_\\(.*\\)$")) %>%
    rename_with(tolower, matches("^[A-Za-z]")) %>%
    rename(country = "reference_area") %>%
    mutate(country = str_replace_all(.data$country, "\\s", "_")) %>%
    pivot_longer(-country,names_to = "date",values_to = "policy_rate") %>%
    mutate(date = as.yearmon(date))

  return(filtered_df)

}


#' This function imports credit to GDP ratios from BIS format
#'
#' @importFrom  readr read_csv cols
#'
#' @importFrom zoo as.yearqtr
#'
#' @importFrom stats complete.cases
#'
#' @importFrom rlang .data
#'
#' @importFrom  tidyr pivot_longer
#'
#' @importFrom stringr str_replace_all str_remove_all
#'
#' @import magrittr
#'
#' @import dplyr
#'
#' @param file_path string a file path to the source data file
#'
#' @param my_frequency time frequency of the data (default is Quarterly),
#' available options are:
#' \itemize{
#'  \item Quarterly
#' }
#'
#' @param my_borrowing_sector (default is Private non-financial sector),
#' available options are:
#' \itemize{
#'  \item Private non-financial sector
#' }
#'
#' @param my_lending_sector (default is All sectors),
#' available options are:
#' \itemize{
#'  \item All sectors
#' }
#'
#' @export
#'
import_bis_credit_to_gdp_ratios = function(file_path,
                                           my_frequency = "Quarterly",
                                           my_borrowing_sector = "Private non-financial sector",
                                           my_lending_sector = "All sectors") {
  . = NULL

  raw_df = read_csv(file_path,col_types = cols())

  named_df = raw_df %>%
    select(
      -.data$FREQ,
      -.data$TC_BORROWERS,
      -.data$TC_LENDERS,
      -.data$`Time Period`,
      -.data$CG_DTYPE
    ) %>%
    rename_all(~ str_replace_all(., " ", "_")) %>%
    rename_all(~ str_remove_all(., "'")) %>%
    rename_with(tolower, matches("^[A-Za-z]")) %>%
    rename(country = .data$borrowers_country) %>%
    rename(country_code = .data$borrowers_cty)

  filtered_df = named_df %>%
    filter(.data$frequency == my_frequency) %>%
    select(-.data$frequency) %>%
    filter(.data$borrowing_sector == my_borrowing_sector) %>%
    select(-.data$borrowing_sector) %>%
    filter(.data$lending_sector == my_lending_sector) %>%
    select(-.data$lending_sector)

  clean_df = filtered_df %>%
    mutate(credit_gap_data_type = str_remove_all(.data$credit_gap_data_type,"\\(.*\\)")) %>%
    mutate(credit_gap_data_type = str_remove_all(.data$credit_gap_data_type,"Credit-to-GDP")) %>%
    mutate(credit_gap_data_type = str_remove_all(.data$credit_gap_data_type,"\\s")) %>%
    pivot_longer(-c(.data$country,
                    .data$country_code,
                    .data$credit_gap_data_type), names_to = "date",
                 values_to = "credit_gdp") %>%
    filter(complete.cases(.)) %>%
    mutate(date = as.yearqtr(.data$date, format = "%Y-Q%q")) %>%
    mutate(country = str_replace_all(.data$country, "\\s", "_"))

  return(clean_df)


}



#' This function imports total credit to private sector from BIS format
#'
#' @importFrom  readr read_csv cols
#'
#' @importFrom zoo as.yearqtr
#'
#' @importFrom stats complete.cases
#'
#' @importFrom rlang .data
#'
#' @importFrom  tidyr pivot_longer
#'
#' @importFrom stringr str_replace_all str_remove_all
#'
#' @import magrittr
#'
#' @import dplyr
#'
#' @param file_path string a file path to the source data file
#'
#' @param my_frequency time frequency of the data (default is NULL),
#' available options are:
#' \itemize{
#'  \item Quarterly
#' }
#'
#' @param my_borrowing_sector (default is NULL),
#' available options are:
#' \itemize{
#'  \item Non financial sector
#'  \item General government
#'  \item Households & NPISHs
#'  \item Non-financial corporations
#'  \item Private non-financial sector
#' }
#'
#' @param my_lending_sector (default is NULL),
#' available options are:
#' \itemize{
#'  \item All sectors
#'  \item Banks, domestic
#' }
#'
#' @param my_valuation (default is NULL), toggles the
#' difference between credit and debt. available options are:
#' \itemize{
#'  \item Nominal value
#'  \item Market value
#' }
#'
#' @param my_unit_type (default is NULL),
#' available options are:
#' \itemize{
#'  \item US Dollar
#'  \item Percentage of GDP
#'  \item Percentage of GDP-PPP
#'  \item Domestic currency
#' }
#'
#'
#' @param my_type_of_adjustment (default is NULL),
#' available options are:
#' \itemize{
#'  \item Adjusted for breaks
#'  \item Unadjusted for breaks
#' }
#'
#' @param pivot_to_long reshape the data to long format? Default is FALSE
#'
#' @export
#'
import_bis_total_credit = function(file_path,
                                   my_frequency = NULL,
                                   my_borrowing_sector = NULL,
                                   my_lending_sector = NULL,
                                   my_valuation = NULL,
                                   my_unit_type = NULL,
                                   my_type_of_adjustment = NULL,
                                   pivot_to_long = FALSE) {

  . = NULL

  filtered_df = read_csv(file_path, col_types = cols()) %>%
    select(-.data$`Time Period`) %>%
    select(-matches("^[A-Z_]+$", ignore.case = FALSE)) %>%
    rename_all( ~ str_replace_all(., " ", "_")) %>%
    rename_all( ~ str_replace_all(., "_-_", "_")) %>%
    rename_all(~ str_remove_all(., "_\\(.*\\)$")) %>%
    rename_with(tolower, matches("^[A-Za-z]")) %>%
    rename(country = "borrowers'_country") %>%
    mutate(country = str_replace_all(.data$country, "\\s", "_"))


  for (filter_name in c(
    "my_frequency",
    "my_borrowing_sector",
    "my_lending_sector",
    "my_valuation",
    "my_unit_type",
    "my_type_of_adjustment"
  )) {


    filter_val = get(filter_name)

    if(!is.null(filter_val)){

      filter_name = filter_name %>%
        str_remove("my_")

      filtered_df = filtered_df %>%
        filter(!!sym(filter_name) %in% filter_val)

      if(length(filter_val) == 1){

        filtered_df = filtered_df %>%
          select(-!!sym(filter_name))

      }


    }




  }



  if(pivot_to_long){

    filtered_df = filtered_df %>%
      pivot_longer(matches("^[0-9]"),
                   names_to = "date",
                   values_to = "total_credit"
      )



  }


  return(filtered_df)


}


#' This function imports debt securities from BIS format
#'
#' @importFrom  readr read_csv cols
#'
#' @importFrom zoo as.yearqtr
#'
#' @importFrom stats complete.cases
#'
#' @importFrom rlang .data
#'
#' @importFrom  tidyr pivot_longer
#'
#' @importFrom stringr str_replace_all str_remove_all
#'
#' @import magrittr
#'
#' @import dplyr
#'
#' @param file_path string a file path to the source data file
#'
#' @param my_frequency time frequency of the data (default is NULL),
#' available options are:
#' \itemize{
#'  \item Quarterly
#' }
#'
#' @param my_measure (default is NULL),
#' available options are:
#' \itemize{
#'  \item Amounts outstanding
#'  \item Net issues
#'  \item Gross issues
#' }
#'
#' @param my_issuer_residence (default is NULL)
#' The residence of the issuer is the country where the issuer is incorporated.
#'
#' @param my_issuer_nationality (default is NULL)
#' The nationality of the issuer is the country where the issuer's parent is headquartered
#'
#' @param my_issuer_sector_immediate_borrower (default is NULL),
#' available options are:
#' \itemize{
#'  \item All issuers
#'  \item Financial corporations
#'  \item Private banks
#'  \item International institutions
#'  \item Public banks
#'  \item General government
#'  \item Non-financial corporations
#'  \item Private other financial institutions
#'  \item Central bank
#'  \item Public other financial institutions
#' }
#'
#' @param my_issuer_sector_ultimate_borrower (default is NULL),
#' available options are:
#' \itemize{
#'  \item All issuers
#'  \item Financial corporations
#'  \item Private banks
#'  \item International institutions
#'  \item Public banks
#'  \item General government
#'  \item Non-financial corporations
#'  \item Private other financial institutions
#'  \item Central bank
#'  \item Public other financial institutions
#' }
#'
#' @param my_issue_market (default is NULL),
#' available options are:
#' \itemize{
#'  \item All markets
#'  \item Domestic market
#'  \item International markets
#' }
#'
#' @param my_issue_type (default is NULL),
#' available options are:
#' \itemize{
#'  \item All issue types
#' }
#'
#'
#' @param my_issue_currency_group (default is NULL),
#' available options are:
#' \itemize{
#'  \item All currencies
#'  \item Foreign currencies
#'  \item Domestic currency
#' }

#' @param my_issue_currency (default is NULL),
#' There are 65 available options, the special are:
#' \itemize{
#'  \item Total all currencies
#'  \item Sum of ECU, Euro and legacy currencies now included in the Euro
#' }
#'
#' @param pivot_to_long reshape the data to long format? Default is FALSE
#'
#'
#' @export
#'
import_bis_debt_sec = function(file_path,
                               my_frequency = NULL,
                               my_measure = NULL,
                               my_issuer_residence = NULL,
                               my_issuer_nationality = NULL,
                               my_issuer_sector_immediate_borrower = NULL,
                               my_issuer_sector_ultimate_borrower = NULL,
                               my_issue_market = NULL,
                               my_issue_type = NULL,
                               my_issue_currency_group = NULL,
                               my_issue_currency = NULL,
                               my_original_maturity = NULL,
                               my_remaining_maturity = NULL,
                               my_rate_type = NULL,
                               my_default_risk = NULL,
                               my_collateral_type = NULL,
                               pivot_to_long = FALSE) {
  . = NULL

  filtered_df = read_csv(file_path, col_types = cols()) %>%
    select(-matches("^[A-Z_]+$", ignore.case = FALSE)) %>%
    select(-.data$`Time Period`) %>%
    rename_all( ~ str_replace_all(., " ", "_")) %>%
    rename_all( ~ str_replace_all(., "_-_", "_")) %>%
    rename_all(~ str_remove_all(., "_\\(.*\\)$")) %>%
    rename_with(tolower, matches("^[A-Za-z]")) %>%
    mutate(issuer_residence = str_replace_all(.data$issuer_residence,"\\s","_"))


  for (filter_val in c(
    "my_frequency",
    "my_measure",
    "my_issuer_residence",
    "my_issuer_nationality",
    "my_issuer_sector_immediate_borrower",
    "my_issuer_sector_ultimate_borrower",
    "my_issue_market",
    "my_issue_type",
    "my_issue_currency_group",
    "my_issue_currency",
    "my_original_maturity",
    "my_remaining_maturity",
    "my_rate_type",
    "my_default_risk",
    "my_collateral_type"
  )) {


    if(!is.null(get(filter_val))){

      filter_name = filter_val %>%
        str_remove("my_")

      filtered_df = filtered_df %>%
        filter(!!sym(filter_name) %in% !!sym(filter_val))


    }




  }


  if(pivot_to_long){

    filtered_df = filtered_df %>%
      pivot_longer(matches("^[0-9]"),
                   names_to = "date",
                   values_to = "debt_credit"
      ) %>%
      mutate(date = as.yearqtr(.data$date, format = "%Y-Q%q"))



  }


  return(filtered_df)


}




#' This function imports locational banking statistics from BIS format
#'
#' @importFrom  readr read_csv cols
#'
#' @importFrom zoo as.yearqtr
#'
#' @importFrom stats complete.cases
#'
#' @importFrom rlang .data
#'
#' @importFrom  tidyr pivot_longer
#'
#' @importFrom stringr str_replace_all str_remove_all
#'
#' @import magrittr
#'
#' @import dplyr
#'
#' @param file_path string a file path to the source data file
#'
#' @param my_frequency time frequency of the data (default is NULL),
#' available options are:
#' \itemize{
#'  \item Quarterly
#' }
#'
#' @param my_measure (default is NULL),
#' available options are:
#' \itemize{
#'  \item FX and break adjusted change (BIS calculated)
#'  \item Amounts outstanding / Stocks
#' }
#'
#' @param my_balance_sheet_position (default is NULL)
#' available options are:
#' \itemize{
#'  \item Total claims
#'  \item Total liabilities
#' }
#'
#' @param my_type_of_instruments (default is NULL)
#' available options are:
#' \itemize{
#'  \item All instruments
#'  \item Debt securities
#'  \item Loans and deposits
#'  \item Other instruments
#'  \item Unallocated by instrument
#'  \item Debt securities, short-term
#' }
#'
#' @param my_currency_denomination (default is NULL),
#' available options are:
#' \itemize{
#'  \item Swiss Franc
#'  \item Euro
#'  \item Pound Sterling
#'  \item Japanese Yen
#'  \item US Dollar
#'  \item All currencies
#'  \item All currencies excluding USD, EUR, JPY, CHF and GBP
#'  \item Unallocated currencies
#' }
#'
#' @param my_currency_type_of_reporting_country (default is NULL),
#' available options are:
#' \itemize{
#'  \item All currencies (=D+F+U)
#'  \item Foreign currency (ie currencies foreign to bank location country)
#'  \item Domestic currency (ie currency of bank location country)
#'  \item Unclassified currency
#' }
#'
#' @param my_parent_country string
#'
#' @param my_reporting_country string
#'
#' @param my_type_of_reporting_institutions (default is NULL),
#' available options are:
#' \itemize{
#'  \item All reporting banks/institutions (domestic, foreign, consortium and unclassified)
#'  \item Foreign branches
#'  \item Domestic banks
#'  \item Foreign subsidiaries
#' }
#'
#' @param my_counterparty_sector (default is NULL),
#' available options are:
#' \itemize{
#'  \item All sectors
#'  \item Banks, total
#'  \item Non-financial corporations
#'  \item Non-bank financial institutions
#'  \item General government
#'  \item Households and NPISHs
#'  \item Banks, related offices
#'  \item Banks, central banks
#'  \item Non-banks, total
#'  \item Non-financial sectors
#'  \item Unallocated by sector
#'
#' }
#'
#' @param counterparty_country string
#'
#'
#' @param my_position_type (default is NULL),
#' available options are:
#' \itemize{
#'  \item All
#'  \item Cross-border
#'  \item Local
#'  \item Unallocated
#' }
#'
#'
#' @param pivot_to_long reshape the data to long format? Default is FALSE
#'
#'
#' @export
#'
import_bis_lbs = function(file_path,
                          my_frequency = NULL,
                          my_measure = NULL,
                          my_balance_sheet_position = NULL,
                          my_type_of_instruments = NULL,
                          my_currency_denomination = NULL,
                          my_currency_type_of_reporting_country = NULL,
                          my_parent_country = NULL,
                          my_type_of_reporting_institutions = NULL,
                          my_reporting_country = NULL,
                          my_counterparty_sector = NULL,
                          my_counterparty_country = NULL,
                          my_position_type = NULL,
                          pivot_to_long = FALSE) {
  . = NULL

  filtered_df = read_csv(file_path, col_types = cols()) %>%
    select(-.data$`Time Period`) %>%
    select(-matches("^[A-Z_]+$", ignore.case = FALSE)) %>%
    rename_all( ~ str_replace_all(., " ", "_")) %>%
    rename_all( ~ str_replace_all(., "_-_", "_")) %>%
    rename_all(~ str_remove_all(., "_\\(.*\\)$")) %>%
    rename_with(tolower, matches("^[A-Za-z]"))


  for (filter_name in c(
    "my_frequency",
    "my_measure",
    "my_balance_sheet_position",
    "my_type_of_instruments",
    "my_currency_denomination",
    "my_currency_type_of_reporting_country",
    "my_parent_country",
    "my_type_of_reporting_institutions",
    "my_reporting_country",
    "my_counterparty_sector",
    "my_counterparty_country",
    "my_position_type"
  )) {


    filter_val = get(filter_name)

    if(!is.null(filter_val)){

      filter_name = filter_name %>%
        str_remove("my_")

      filtered_df = filtered_df %>%
        filter(!!sym(filter_name) %in% filter_val)

      if(length(filter_val) == 1){

        filtered_df = filtered_df %>%
          select(-!!sym(filter_name))

      }


    }




  }


  if(pivot_to_long){

    filtered_df = filtered_df %>%
      pivot_longer(matches("^[0-9]"),
                   names_to = "date",
                   values_to = "balance"
      ) %>%
      mutate(date = as.yearqtr(.data$date, format = "%Y-Q%q"))



  }


  return(filtered_df)


}



#' @title  This function imports BIS residential property prices
#'
#' @importFrom  readr read_csv cols
#'
#' @importFrom zoo as.yearqtr
#'
#' @importFrom stats complete.cases
#'
#' @importFrom rlang .data
#'
#' @importFrom  tidyr pivot_longer
#'
#' @importFrom stringr str_replace_all str_remove_all
#'
#' @import magrittr
#'
#' @import dplyr
#'
#' @param file_path string a file path to the source data file
#'
#'
#' @param my_frequency time frequency of the data (default is NULL),
#' available options are:
#' \itemize{
#'  \item Quarterly
#' }
#'
#' @param my_unit_of_measure (default is NULL),
#' available options are:
#' \itemize{
#'  \item Index, 2010 = 100
#'  \item Year-on-year changes, in per cent
#' }
#'
#'
#' @param my_value (default is NULL),
#' available options are:
#' \itemize{
#'  \item Real
#'  \item Nominal
#' }
#'
#' @param pivot_to_long reshape the data to long format? Default is FALSE
#'
#' @export
#'
#'
import_bis_selected_property_prices = function(file_path,
                                               my_frequency = NULL,
                                               my_unit_of_measure = NULL,
                                               my_value = NULL,
                                               pivot_to_long = FALSE) {


  . = NULL

  filtered_df = read_csv(file_path, col_types = cols()) %>%
    select(-.data$`Time Period`) %>%
    select(-matches("^[A-Z_]+$", ignore.case = FALSE)) %>%
    rename_all( ~ str_replace_all(., " ", "_")) %>%
    rename_all( ~ str_replace_all(., "_-_", "_")) %>%
    rename_all(~ str_remove_all(., "_\\(.*\\)$")) %>%
    rename_with(tolower, matches("^[A-Za-z]")) %>%
    rename(country = reference_area) %>%
    mutate(country = str_replace_all(.data$country, "\\s", "_"))


  for (filter_name in c(
    "my_frequency",
    "my_unit_of_measure",
    "my_value"
  )) {


    filter_val = get(filter_name)

    if(!is.null(filter_val)){

      filter_name = filter_name %>%
        str_remove("my_")

      filtered_df = filtered_df %>%
        filter(!!sym(filter_name) %in% filter_val)

      if(length(filter_val) == 1){

        filtered_df = filtered_df %>%
          select(-!!sym(filter_name))

      }


    }




  }



  if(pivot_to_long){

    filtered_df = filtered_df %>%
      pivot_longer(matches("^[0-9]"),
                   names_to = "date",
                   values_to = "property_price"
      ) %>%
      mutate(date = as.yearqtr(.data$date, format = "%Y-Q%q"))



  }


  return(filtered_df)

}


#' @title  This function imports BIS FX rates
#'
#' @importFrom  readr read_csv cols
#'
#' @importFrom zoo as.yearqtr
#'
#' @importFrom stats complete.cases
#'
#' @importFrom rlang .data
#'
#' @importFrom  tidyr pivot_longer
#'
#' @importFrom stringr str_replace_all str_remove_all
#'
#' @import magrittr
#'
#' @import dplyr
#'
#' @param file_path string a file path to the source data file
#'
#'
#' @param my_frequency time frequency of the data (default is NULL),
#' available options are:
#' \itemize{
#'  \item Quarterly
#'  \item Annual
#' }
#'
#' @param my_currency string specifies the counter currency in USD/XXX pair
#'
#'
#' @param my_collection (default is NULL),
#' available options are:
#' \itemize{
#'  \item Average of observations through period
#'  \item End of period
#' }
#'
#' @param pivot_to_long reshape the data to long format? Default is FALSE
#'
#' @export
#'
#'
import_bis_fx_rates = function(file_path,
                               my_frequency = NULL,
                               my_currency = NULL,
                               my_collection = NULL,
                               pivot_to_long = FALSE) {


  . = NULL

  filtered_df = read_csv(file_path, col_types = cols()) %>%
    select(-.data$`Time Period`) %>%
    select(-matches("^[A-Z_]+$", ignore.case = FALSE)) %>%
    rename_all( ~ str_replace_all(., " ", "_")) %>%
    rename_all( ~ str_replace_all(., "_-_", "_")) %>%
    rename_all(~ str_remove_all(., "_\\(.*\\)$")) %>%
    rename_with(tolower, matches("^[A-Za-z]")) %>%
    rename(country = reference_area) %>%
    mutate(country = str_replace_all(.data$country, "\\s", "_"))


  for (filter_name in c(
    "my_frequency",
    "my_currency",
    "my_collection"
  )) {


    filter_val = get(filter_name)

    if(!is.null(filter_val)){

      filter_name = filter_name %>%
        str_remove("my_")

      filtered_df = filtered_df %>%
        filter(!!sym(filter_name) %in% filter_val)

      if(length(filter_val) == 1){

        filtered_df = filtered_df %>%
          select(-!!sym(filter_name))

      }


    }




  }



  if(pivot_to_long){

    filtered_df = filtered_df %>%
      pivot_longer(matches("^[0-9]"),
                   names_to = "date",
                   values_to = "fx_rate"
      ) %>%
      mutate(date = as.yearqtr(.data$date, format = "%Y-Q%q"))



  }


  return(filtered_df)

}




#' @title  This function imports BIS CPI index
#'
#' @importFrom  readr read_csv cols
#'
#' @importFrom zoo as.yearqtr
#'
#' @importFrom stats complete.cases
#'
#' @importFrom rlang .data
#'
#' @importFrom  tidyr pivot_longer
#'
#' @importFrom stringr str_replace_all str_remove_all
#'
#' @import magrittr
#'
#' @import dplyr
#'
#' @param file_path string a file path to the source data file
#'
#'
#' @param my_frequency time frequency of the data (default is NULL),
#' available options are:
#' \itemize{
#'  \item Monthly
#'  \item Annual
#' }
#'
#' @param unit_of_measure (default is NULL),
#' available options are:
#' \itemize{
#'  \item Index, 2010 = 100
#'  \item Year-on-year changes, in per cent
#' }
#'
#' @param pivot_to_long reshape the data to long format? Default is FALSE
#'
#' @export
#'
#'
import_bis_cpi_index = function(file_path,
                               my_frequency = NULL,
                               my_unit_of_measure = NULL,
                               pivot_to_long = FALSE) {


  . = NULL

  filtered_df = read_csv(file_path, col_types = cols()) %>%
    select(-.data$`Time Period`) %>%
    select(-matches("^[A-Z_]+$", ignore.case = FALSE)) %>%
    rename_all( ~ str_replace_all(., " ", "_")) %>%
    rename_all( ~ str_replace_all(., "_-_", "_")) %>%
    rename_all(~ str_remove_all(., "_\\(.*\\)$")) %>%
    rename_with(tolower, matches("^[A-Za-z]")) %>%
    rename(country = reference_area) %>%
    mutate(country = str_replace_all(.data$country, "\\s", "_"))


  for (filter_name in c(
    "my_frequency",
    "my_unit_of_measure"
  )) {


    filter_val = get(filter_name)

    if(!is.null(filter_val)){

      filter_name = filter_name %>%
        str_remove("my_")

      filtered_df = filtered_df %>%
        filter(!!sym(filter_name) %in% filter_val)

      if(length(filter_val) == 1){

        filtered_df = filtered_df %>%
          select(-!!sym(filter_name))

      }


    }




  }



  if(pivot_to_long){

    filtered_df = filtered_df %>%
      pivot_longer(matches("^[0-9]"),
                   names_to = "date",
                   values_to = "cpi"
      )



  }


  return(filtered_df)

}
