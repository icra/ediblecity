test_that("Previous edible areas are respected", {
  test <- set_scenario(city_example,
                       pGardens = 0.2,
                       pVacant = 0,
                       pRooftop = 0.3)

  after_area <- test$edible_area[test$Function == "Community garden"]
  before_area <- city_example$edible_area[city_example$Function == "Community garden"]
  expect_equal(after_area, before_area)
})
