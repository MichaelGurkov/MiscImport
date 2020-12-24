#' This function imports policy rates from BIS format
#'
#' @importFrom  readr read_csv
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

  raw_df = read_csv(file_path)

  clean_df = raw_df %>%
    select(-.data$FREQ,
           -.data$REF_AREA,
           -.data$`Time Period`,
           -.data$Frequency) %>%
    rename(country = .data$`Reference area`) %>%
    pivot_longer(-.data$country, names_to = "date",
                 values_to = "policy_rate") %>%
    filter(complete.cases(.)) %>%
    mutate(date = as.yearmon(.data$date)) %>%
    mutate(country = str_replace_all(.data$country, "\\s", "_"))


}


#' This function imports credit to GDP ratios from BIS format
#'
#' @importFrom  readr read_csv
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
    rename(country = borrowers_country) %>%
    rename(country_code = borrowers_cty)

  filtered_df = named_df %>%
    filter(.data$frequency == my_frequency) %>%
    select(-.data$frequency) %>%
    filter(.data$borrowing_sector == my_borrowing_sector) %>%
    select(-.data$borrowing_sector) %>%
    filter(.data$lending_sector == my_lending_sector) %>%
    select(-.data$lending_sector)

  clean_df = filtered_df %>%
    mutate(credit_gap_data_type = str_remove_all(credit_gap_data_type,"\\(.*\\)")) %>%
    mutate(credit_gap_data_type = str_remove_all(credit_gap_data_type,"Credit-to-GDP")) %>%
    mutate(credit_gap_data_type = str_remove_all(credit_gap_data_type,"\\s")) %>%
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
#' @importFrom  readr read_csv
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
#'  \item Non financial sector
#'  \item General government
#'  \item Households & NPISHs
#'  \item Non-financial corporations
#'  \item Private non-financial sector
#' }
#'
#' @param my_lending_sector (default is All sectors),
#' available options are:
#' \itemize{
#'  \item All sectors
#'  \item Banks, domestic
#' }
#'
#' @param my_valuation (default is Market value), toggles the
#' difference between credit and debt. available options are:
#' \itemize{
#'  \item Nominal value
#'  \item Market value
#' }
#'
#' @param my_unit_type (default is US Dollar),
#' available options are:
#' \itemize{
#'  \item US Dollar
#'  \item Percentage of GDP
#'  \item Percentage of GDP-PPP
#'  \item Domestic currency
#' }
#'
#'
#' @param my_type_of_adjustment (default is Unadjusted for breaks),
#' available options are:
#' \itemize{
#'  \item Adjusted for breaks
#'  \item Unadjusted for breaks
#' }
#' @export
#'
import_bis_total_credit = function(file_path,
                                   my_frequency = "Quarterly",
                                   my_borrowing_sector = "Private non-financial sector",
                                   my_lending_sector = "All sectors",
                                   my_valuation = "Market value",
                                   my_unit_type = "US Dollar",
                                   my_type_of_adjustment = "Unadjusted for breaks") {
  . = NULL

  raw_df = read_csv(file_path,col_types = cols())

  named_df = raw_df %>%
    select(
      -.data$FREQ,
      -.data$TC_BORROWERS,
      -.data$TC_LENDERS,
      -.data$`Time Period`,
      -.data$TC_ADJUST,
      -.data$UNIT_TYPE,
      -.data$VALUATION
    ) %>%
    rename_all(~ str_replace_all(., " ", "_")) %>%
    rename_all(~ str_remove_all(., "'")) %>%
    rename_with(tolower, matches("^[A-Za-z]")) %>%
    rename(country = borrowers_country) %>%
    rename(country_code = borrowers_cty) %>%
    mutate(country = str_replace_all(.data$country, "\\s", "_"))

  filtered_df = named_df %>%
    filter(.data$frequency == my_frequency) %>%
    select(-.data$frequency) %>%
    filter(.data$type_of_adjustment == my_type_of_adjustment) %>%
    select(-.data$type_of_adjustment) %>%
    filter(.data$valuation %in% my_valuation) %>%
    filter(.data$borrowing_sector %in% my_borrowing_sector) %>%
    filter(.data$lending_sector %in% my_lending_sector) %>%
    mutate(unit_type = str_replace_all(.data$unit_type,
                                       " \\(using PPP exchange rates\\)",
                                       "-PPP")) %>%
    mutate(unit_type = str_remove_all(
      .data$unit_type,
      " \\(incl. conversion to current currency made using a fix parity\\)"
    )) %>%
    filter(.data$unit_type %in% unit_type)


  clean_df = filtered_df %>%
    pivot_longer(-c(.data$country,
                    .data$country_code,
                    .data$borrowing_sector,
                    .data$lending_sector,
                    .data$unit_type,
                    .data$valuation), names_to = "date",
                 values_to = "total_credit") %>%
    filter(complete.cases(.)) %>%
    mutate(date = as.yearqtr(.data$date, format = "%Y-Q%q"))

  return(clean_df)


}


