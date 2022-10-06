#' @title Import trading status data
#'
#' @description This function imports data set about listing status of TASE companies
#'
#' @param filepath the path to financial report data (in xlsx format)
#'
#' @import readxl
#'
#' @import dplyr
#'
#' @import xts
#'
#' @export

import_tase_comps_status = function(filepath = NULL){

  if(is.null(filepath)){
    filepath = paste0(Sys.getenv("USERPROFILE"),
                      "\\OneDrive - Bank Of Israel",
                      "\\Data\\TASE liquidity",
                      "\\Trading_Companies_Status.xlsx")}

  sheets_names = excel_sheets(filepath)

  sheet_list = lapply(sheets_names,function(temp_name){

    temp_sheet = read_xlsx(filepath,sheet = temp_name) %>%
      mutate(year = temp_name) %>%
      rename_all(tolower) %>%
      rename(start_year = start, end_year = end,
             issued_comps = issued,
             delisted_comps = delisted) %>%
      select(year, sector, everything())

  })

  status_df = bind_rows(sheet_list)

  status_df = status_df %>%
    mutate(across(where(is.numeric), ~replace_na(.,0)))


  return(status_df)

}
