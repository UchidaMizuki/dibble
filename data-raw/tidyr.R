library(magrittr)

# population --------------------------------------------------------------

population_dibble <- tidyr::population %>%
  dibble_by(country, year)

usethis::use_data(population_dibble,
                  overwrite = TRUE)

# table1 ------------------------------------------------------------------

table1_dibble <- tidyr::table1 %>%
  dibble_by(country, year)

usethis::use_data(table1_dibble,
                  overwrite = TRUE)

# us_rent_income ----------------------------------------------------------

us_rent_income_dibble <- tidyr::us_rent_income %>%
  tidyr::unite("GEOID_NAME", GEOID:NAME) %>%
  dibble_by(GEOID_NAME, variable)

usethis::use_data(us_rent_income_dibble,
                  overwrite = TRUE)

# fish_encounters ---------------------------------------------------------

fish_encounters_dibble <- tidyr::fish_encounters %>%
  dibble_by(fish, station)

usethis::use_data(fish_encounters_dibble,
                  overwrite = TRUE)

