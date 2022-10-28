test_that("Green per capita is lower when private is FALSE", {
  private_true <- green_capita(city_example, inhabitants = 6000, private = TRUE)
  private_false <- green_capita(city_example, inhabitants = 6000, private = FALSE)

  expect_gt(private_true, private_false)
})

test_that("Verbose and neighbourhoods return a tibble", {
  green_capita(city_example,
               neighbourhoods = neighbourhoods,
               inh_col = "inhabitants",
               name_col = "name",
               verbose = TRUE) %>%
    expect_s3_class("tbl")
})

test_that("min_inh is working", {
    green_capita(city_example,
                 neighbourhoods = neighbourhoods,
                 inh_col = "inhabitants",
                 name_col = "name",
                 min_inh = min(neighbourhoods$inhabitants),
                 verbose = TRUE) %>%
      nrow() %>%
      expect_equal(nrow(neighbourhoods)-1)
})

