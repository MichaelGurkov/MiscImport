


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
