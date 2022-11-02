test_that("indicator returns a named numeric scalar", {
  result <- no2_seq(city_example)
  expect_length(result, 1)
  expect_type(result, "double")
  expect_type(names(result), "character")
})

test_that("city_functions is not used when green_df is not NULL", {
  no2_seq(city_example, green_df = tibble(a = 1, b=2)) %>%
    expect_error()
})

