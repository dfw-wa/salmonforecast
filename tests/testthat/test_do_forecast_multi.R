mock_do_forecast <- function(dat, covariates, ...) {
  pop <- unique(dat$population)
  age <- unique(dat$age)
  stopifnot(length(pop) == 1, length(age) == 1)

  years <- sort(unique(dat$year))
  obs <- dat$abundance[match(years, dat$year)]
  pred <- obs * 0.95

  ens <- data.frame(year = years, model = "MAPE_weighted", predicted_abundance = pred)
  fs <- data.frame(model = c("m1", "MAPE_weighted"), MAPE = c(20, 10), RMSE = c(5, 3), MSA = c(1, 1))
  all_mods <- data.frame(year = years, abundance = obs)

  list(
    ens = list(ensembles = ens, forecast_skill = fs),
    rp = list(all_mods = all_mods),
    used_covariates = covariates
  )
}

make_multi_age_pop_data <- function() {
  set.seed(2)
  do.call(rbind, lapply(c("A", "B"), function(p) {
    do.call(rbind, lapply(3:5, function(a) {
      data.frame(
        population = p, age = a, year = 2015:2020,
        abundance = round(runif(6, 80, 400) / a * 3, 1),
        lag1_PDO = rnorm(6)
      )
    }))
  }))
}

test_that("do_forecast_multi splits by population and age and auto-injects the lag covariate for older ages only", {
  testthat::local_mocked_bindings(do_forecast = mock_do_forecast)

  d <- make_multi_age_pop_data()

  res <- do_forecast_multi(
    dat = d,
    covariates = c("lag1_PDO"),
    forecast_ages = c(3, 4, 5),
    total_ages = c(4, 5),
    add_lag_age_covariate = TRUE,
    aggregate_by = "age"
  )

  expect_equal(nrow(res$group_keys), 6)

  used <- vapply(res$by_group, function(x) paste(x$used_covariates, collapse = ","), character(1))
  expect_false(grepl("lag1_log_age_minus1", used[["A.3"]]))
  expect_false(grepl("lag1_log_age_minus1", used[["B.3"]]))
  expect_true(grepl("lag1_log_age_minus1", used[["A.4"]]))
  expect_true(grepl("lag1_log_age_minus1", used[["A.5"]]))
  expect_true(grepl("lag1_log_age_minus1", used[["B.4"]]))
  expect_true(grepl("lag1_log_age_minus1", used[["B.5"]]))

  # age 3 excluded from the summed total (age is summed away, so the column is dropped),
  # but still present in the un-aggregated combined outputs
  expect_true(3 %in% res$combined_forecasts$age)
  expect_false("age" %in% names(res$aggregated_performance$series))
  expect_setequal(unique(res$aggregated_performance$series$population), c("A", "B"))
  # only ages 4 and 5 contributed to each summed row
  expect_true(all(res$aggregated_performance$series$n_groups_summed == 2))
})

test_that("do_forecast_multi's forecast_ages drops an age entirely before modeling", {
  testthat::local_mocked_bindings(do_forecast = mock_do_forecast)

  d <- make_multi_age_pop_data()

  res <- do_forecast_multi(
    dat = d,
    covariates = c("lag1_PDO"),
    forecast_ages = c(4, 5),
    add_lag_age_covariate = TRUE,
    aggregate_by = "age"
  )

  expect_false(any(res$group_keys$age == 3))
  expect_equal(nrow(res$group_keys), 4)
})

test_that("do_forecast_multi's total_ages must be a subset of forecast_ages", {
  testthat::local_mocked_bindings(do_forecast = mock_do_forecast)

  d <- make_multi_age_pop_data()

  expect_error(
    do_forecast_multi(
      dat = d,
      covariates = c("lag1_PDO"),
      forecast_ages = c(4, 5),
      total_ages = c(3, 4, 5)
    ),
    "must be a subset"
  )
})

test_that("do_forecast_multi works with population-only grouping (no age column)", {
  testthat::local_mocked_bindings(do_forecast = mock_do_forecast)

  d <- data.frame(
    population = rep(c("A", "B"), each = 6),
    age = 4, # constant age so the mock's stopifnot(length(age)==1) still holds
    year = rep(2015:2020, 2),
    abundance = c(100, 110, 120, 130, 140, 150, 200, 210, 220, 230, 240, 250),
    lag1_PDO = rnorm(12)
  )

  res <- do_forecast_multi(
    dat = d,
    group_vars = "population",
    covariates = c("lag1_PDO"),
    aggregate_by = "population"
  )

  expect_equal(nrow(res$group_keys), 2)
  expect_equal(nrow(res$aggregated_performance$series), 6) # one row per year
})
