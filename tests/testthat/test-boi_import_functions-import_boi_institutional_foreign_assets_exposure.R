test_that("numeric stability check", {
  expect_equal(import_boi_institutional_portolio_asset_class() %>%
                 filter(asset_class == "makam") %>%
                 filter(investor_type == "pensia_mekifot_hadashot") %>%
                 filter(month(date) == 2) %>%
                 filter(year(date) %in% c(2001,2007,2015,2021)) %>%
                 pull(value),c(1577.169,3003.065,57.020,142.452))
})
