test_that("Previous edible areas are respected", {
  test <- set_scenario(city_example,
                       pGardens = 0.2,
                       pVacant = 0,
                       pRooftop = 0.3)

  after_area <- test$edible_area[test$Function == "Community garden"]
  before_area <- city_example$edible_area[city_example$Function == "Community garden"]
  expect_equal(after_area, before_area)
})

test_that("Raise error if x is not an sf object", {
  expect_error(
    city_functions %>%
      set_scenario()
  )
})

test_that("Raise error if Function col does not exist", {
  expect_error(
    city_example %>%
      select(-Function) %>%
      set_scenario()
  )
})

test_that("Commercial are larger than community gardens", {
  scenario <- set_scenario(city_example, pGardens = 0, pVacant = 0.5, pRooftop = 0.5,
                           vacant_from = c("Vacant", "Streets"), pCommercial = 0.5)
  community_surface <- scenario %>%
    filter(Function %in% city_functions$functions[city_functions$volunteers]) %>%
    summarise(area = sum(area)) %>% pull(area)

  commercial_surface <- scenario %>%
    filter(Function %in% city_functions$functions[city_functions$jobs]) %>%
    summarise(area = sum(area)) %>% pull(area)

  expect_gt(commercial_surface, community_surface)
})
