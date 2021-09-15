## code to prepare `NO2_seq` dataset goes here

NO2_seq <- data.frame(
  "Function" = c("Trees", "Schrubs", "Grass"),
  "Sequestration" = c(0.11, 0.09, 0.07)
)

usethis::use_data(NO2_seq, overwrite = TRUE)
