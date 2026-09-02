test_that("add_lag_age_covariate attaches the previous year's next-younger-age observation (raw scale)", {
  d <- data.frame(
    population = rep(c("A", "B"), each = 9),
    age = rep(rep(3:5, each = 3), 2),
    year = rep(2018:2020, 6),
    abundance = c(100, 110, 120, 200, 210, 220, 300, 310, 320,
                  150, 160, 170, 250, 260, 270, 350, 360, 370)
  )

  out <- add_lag_age_covariate(d, log_transform = FALSE, standardize = FALSE)

  # youngest age (3) has no younger age below it -> always NA
  expect_true(all(is.na(out$lag1_age_minus1[out$age == 3])))

  # age 4 in year Y should equal age 3's abundance in year Y-1, within the same population
  row <- out[out$population == "A" & out$age == 4 & out$year == 2019, ]
  expected <- out$abundance[out$population == "A" & out$age == 3 & out$year == 2018]
  expect_equal(row$lag1_age_minus1, expected)

  # populations must not leak into each other
  row_b <- out[out$population == "B" & out$age == 5 & out$year == 2020, ]
  expected_b <- out$abundance[out$population == "B" & out$age == 4 & out$year == 2019]
  expect_equal(row_b$lag1_age_minus1, expected_b)

  # first year of data has no prior-year observation -> NA even for older ages
  first_yr <- out[out$year == 2018, ]
  expect_true(all(is.na(first_yr$lag1_age_minus1)))
})

test_that("add_lag_age_covariate log-transforms and standardizes within (population, age) by default", {
  d <- data.frame(
    population = rep(c("A", "B"), each = 9),
    age = rep(rep(3:5, each = 3), 2),
    year = rep(2018:2020, 6),
    abundance = c(100, 110, 120, 200, 210, 220, 300, 310, 320,
                  150, 160, 170, 250, 260, 270, 350, 360, 370)
  )

  out <- add_lag_age_covariate(d)

  # default column name reflects log-transform
  expect_true("lag1_log_age_minus1" %in% names(out))
  expect_false("lag1_age_minus1" %in% names(out))

  # value for age 4/2019 should be the standardized log of age 3's 2018 abundance,
  # standardized against age 4's own (population A) lag-covariate series
  raw <- add_lag_age_covariate(d, log_transform = TRUE, standardize = FALSE,
                               lag_name = "raw_log_lag")
  age4_A <- raw[raw$population == "A" & raw$age == 4, ]
  expected_scaled <- as.numeric(scale(age4_A$raw_log_lag))

  actual <- out[out$population == "A" & out$age == 4, ] |>
    (\(df) df[order(df$year), ])()
  age4_A <- age4_A[order(age4_A$year), ]

  expect_equal(actual$lag1_log_age_minus1, expected_scaled)

  # standardizing is done separately per (population, age) group (not globally); confirm
  # by checking the age-4 series for population B is centered/scaled on its own values,
  # not shared with population A's
  raw_B <- raw[raw$population == "B" & raw$age == 4, ]
  raw_B <- raw_B[order(raw_B$year), ]
  expected_scaled_B <- as.numeric(scale(raw_B$raw_log_lag))
  actual_B <- out[out$population == "B" & out$age == 4, ]
  actual_B <- actual_B[order(actual_B$year), ]
  expect_equal(actual_B$lag1_log_age_minus1, expected_scaled_B)
  expect_false(isTRUE(all.equal(mean(raw$raw_log_lag[raw$population == "A" & raw$age == 4], na.rm = TRUE),
                                mean(raw$raw_log_lag[raw$population == "B" & raw$age == 4], na.rm = TRUE))))
})

test_that("add_lag_age_covariate errors on missing columns, non-numeric age, or name collision", {
  d <- data.frame(population = "A", age = 3, year = 2020, abundance = 100)

  expect_error(add_lag_age_covariate(d[, c("age", "year", "abundance")]), "requires columns")

  d_char_age <- d
  d_char_age$age <- "three"
  expect_error(add_lag_age_covariate(d_char_age), "must be numeric")

  d_existing <- d
  d_existing$lag1_log_age_minus1 <- 1
  expect_error(add_lag_age_covariate(d_existing), "already exists")

  d_existing_raw <- d
  d_existing_raw$lag1_age_minus1 <- 1
  expect_error(add_lag_age_covariate(d_existing_raw, log_transform = FALSE), "already exists")
})
