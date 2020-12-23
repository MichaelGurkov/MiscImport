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
#' @param frequency time frequency of the data (default is Quarterly),
#' available options are:
#' \itemize{
#'  \item Quarterly
#' }
#'
#' @param borrowing_sector (default is Private non-financial sector),
#' available options are:
#' \itemize{
#'  \item Private non-financial sector
#' }
#'
#' @param lending_sector (default is All sectors),
#' available options are:
#' \itemize{
#'  \item All sectors
#' }
#'
#' @export
#'
import_bis_credit_to_gdp_ratios = function(file_path,
                                           frequency = "Quarterly",
                                           borrowing_sector = "Private non-financial sector",
                                           lending_sector = "All sectors") {
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
    filter(.data$frequency == frequency) %>%
    select(-.data$frequency) %>%
    filter(.data$borrowing_sector == borrowing_sector) %>%
    select(-.data$borrowing_sector) %>%
    filter(.data$lending_sector == lending_sector) %>%
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
    mutate(date = as.yearqtr(.data$date,, format = "%Y-Q%q")) %>%
    mutate(country = str_replace_all(.data$country, "\\s", "_"))

  return(clean_df)


}
