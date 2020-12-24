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
#' @import stringr
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



