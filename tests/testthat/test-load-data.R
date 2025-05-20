# tests/testthat/test-load-data.R

test_that("load_and_prep_data retorna lista con los cuatro componentes", {
  data_list <- load_and_prep_data()

  expect_type(data_list, "list")
  expect_named(data_list, c("diatom", "palyno", "phyto", "zone_data"))

  expect_s3_class(data_list$diatom,    "tbl_df")
  expect_s3_class(data_list$palyno,    "tbl_df")
  expect_s3_class(data_list$phyto,     "tbl_df")
  expect_s3_class(data_list$zone_data, "tbl_df")

  expect_true(all(c("depth", "abn_p")  %in% names(data_list$diatom)))
  expect_true(all(c("depth", "conc_p") %in% names(data_list$palyno)))
  expect_true(all(c("depth", "abn_p")  %in% names(data_list$phyto)))
  expect_true(all(c("ymin","ymax")     %in% names(data_list$zone_data)))
})
