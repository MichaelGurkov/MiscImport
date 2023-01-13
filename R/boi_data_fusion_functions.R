
#' @title This functions imports data in query format
#'
import_boi_query = function(file_path){

  raw_df = read_csv(file_path)

  df = raw_df %>%
    rename_with(~str_replace_all(.,pattern = " ",replacement = "_")) %>%
    select(matches("[a-z]+"))



}
