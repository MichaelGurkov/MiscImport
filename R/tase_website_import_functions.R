#' This functions downloads market capitalization data
#'
#' @import readxl
#'
#' @importFrom zoo as.yearmon
#'
#' @import dplyr
#'
#' @import tidyr
#'
#' @import lubridate
#'
#' @export
#'
import_tase_market_cap = function(file_path = NULL,
                                  download_file = FALSE,
                                  pivot_to_long = TRUE){


  col_names = c("year",
                "stock",
                "stock-etf",
                "stock-etf_free_float",
                "gov_bond",
                "corp_bond",
                "corp_bond_tase_up",
                "bond_index_etf",
                "bond_index_etf_free_float",
                "bond_structured",
                "bond_structured_free_float",
                "makam")

  # temp_year = year(now())
  #
  # source_url = paste0("https://info.tase.co.il/Heb/Statistics/StatRes/",
  #                     temp_year,"/Stat_281_l15_",temp_year,".xlsx")

  if(is.null(file_path)){

    file_path = paste0(Sys.getenv("USERPROFILE"),
                       "\\OneDrive - Bank Of Israel",
                       "\\Data\\TASE\\stats\\market_cap_stats.xlsx")

  }


  cell_range = cell_limits(ul = c(4,1),lr = c(NA_integer_,12))

  raw_df = read_xlsx(file_path,range = cell_range)

  df = raw_df %>%
    purrr::set_names(col_names) %>%
    mutate(across(everything(), as.numeric)) %>%
    filter(!is.na(year))


  if(pivot_to_long){

    df = df %>%
      select(-contains("total")) %>%
      pivot_longer(-year,names_to = "asset_class", values_to = "market_cap")


  }


  return(df)





}


#' This functions downloads capital issuance data
#'
#' @import readxl
#'
#' @importFrom zoo as.yearmon
#'
#' @import dplyr
#'
#' @import tidyr
#'
#' @import lubridate
#'
#' @param linkage_category string nominal or real
#'
#' @export
#'
import_tase_capital_issuance = function(file_path = NULL,
                                  download_file = FALSE,
                                  linkage_category,
                                  pivot_to_long = TRUE){


  col_names_nominal = c("year",
                "stock_options",
                "stock_convertible_bond",
                "stock_issuance_total",
                "stock_option_exercise",
                "stock_total",
                "gov_bond_linked_fx",
                "gov_bond_linked_cpi",
                "gov_bond",
                "gov_bond_total",
                "gov_bond_redemptions",
                "gov_bond_net_issuance",
                "corp_bond",
                "corp_bond_option_exercise",
                "corp_bond_tase_up",
                "corp_bond_total",
                "financial_instruments")


  col_names_real = c("year",
                     "stock_convertibles_issuance",
                     "stock_convertibles_options_exercise",
                     "stock_total",
                     "gov_bond_issuance",
                     "corp_bond_issuance",
                     "corp_bond_option_exercise",
                     "corp_bond_tase_up",
                     "corp_bond_total",
                     "financial_instruments")


  if(linkage_category == "nominal"){

    cell_range = cell_limits(ul = c(4,1),lr = c(NA_integer_,18))

    col_names = col_names_nominal

  }

  if(linkage_category == "real"){

    cell_range = cell_limits(ul = c(4,1),lr = c(NA_integer_,10))

    col_names = col_names_real

  }






  raw_df = read_xlsx(file_path,range = cell_range)

  df = raw_df %>%
    purrr::set_names(col_names) %>%
    mutate(across(everything(), as.numeric)) %>%
    filter(!is.na(year))


  if(pivot_to_long){

    df = df %>%
      select(-contains("total")) %>%
      pivot_longer(-year,names_to = "asset_class",
                   values_to = "issuance_amount")


  }


  return(df)





}

#' This functions downloads trading volumes data
#'
#' @import readxl
#'
#' @importFrom zoo as.yearmon
#'
#' @import dplyr
#'
#' @import tidyr
#'
#' @import lubridate
#'
#' @export
#'
import_tase_trading_volume = function(file_path,
                                      download_file = FALSE,
                                      pivot_to_long = TRUE){


  col_names = c("year",
                "stock_and_convertibles",
                "gov_bond",
                "non_gov_bond",
                "total_bond",
                "makam",
                "total_trading_volume")


  cell_range = cell_limits(ul = c(4,1),lr = c(NA_integer_,7))

  raw_df = read_xlsx(file_path,range = cell_range)

  df = raw_df %>%
    purrr::set_names(col_names) %>%
    mutate(across(everything(), as.numeric)) %>%
    filter(!is.na(year))


  if(pivot_to_long){

    df = df %>%
      select(-contains("total")) %>%
      pivot_longer(-year,names_to = "asset_class", values_to = "market_cap")


  }


  return(df)





}
