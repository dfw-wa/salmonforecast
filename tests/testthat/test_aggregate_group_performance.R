make_mock_result <- function(years, pred, obs) {
  ens <- data.frame(year = years, model = "MAPE_weighted", predicted_abundance = pred)
  fs <- data.frame(model = c("m1", "MAPE_weighted"), MAPE = c(20, 10), RMSE = c(5, 3), MSA = c(1, 1))
  all_mods <- data.frame(year = years, abundance = obs)
  list(ens = list(ensembles = ens, forecast_skill = fs), rp = list(all_mods = all_mods))
}

test_that("aggregate_group_performance sums across ages, excluding a dropped age, per population", {
  results_list <- list(
    A.3 = make_mock_result(2018:2020, c(95, 105, 115), c(100, 110, 120)),
    A.4 = make_mock_result(2018:2020, c(195, 205, 215), c(200, 210, 220)),
    A.5 = make_mock_result(2018:2020, c(295, 305, 315), c(300, 310, 320)),
    B.3 = make_mock_result(2018:2020, c(145, 155, 165), c(150, 160, 170)),
    B.4 = make_mock_result(2018:2020, c(245, 255, 265), c(250, 260, 270)),
    B.5 = make_mock_result(2018:2020, c(345, 355, 365), c(350, 360, 370))
  )
  group_keys <- expand.grid(population = c("A", "B"), age = c(3, 4, 5))
  group_keys <- group_keys[order(group_keys$population, group_keys$age), ]
  rownames(group_keys) <- NULL

  agg <- aggregate_group_performance(
    results_list = results_list,
    group_keys = group_keys,
    group_vars = c("population", "age"),
    aggregate_by = "age",
    total_ages = c(4, 5)
  )

  # age 3 excluded from the total: only ages 4 + 5 summed
  row_a_2018 <- agg$series[agg$series$population == "A" & agg$series$year == 2018, ]
  expect_equal(row_a_2018$predicted_abundance, 195 + 295)
  expect_equal(row_a_2018$abundance, 200 + 300)
  expect_equal(row_a_2018$n_groups_summed, 2)

  expect_equal(nrow(agg$performance), 2) # one row per population
  expect_true(all(c("MAPE", "RMSE", "MSA") %in% names(agg$performance)))
})

test_that("aggregate_group_performance can sum across both population and age (grand total)", {
  results_list <- list(
    A.3 = make_mock_result(2018:2019, c(95, 105), c(100, 110)),
    A.4 = make_mock_result(2018:2019, c(195, 205), c(200, 210)),
    B.3 = make_mock_result(2018:2019, c(145, 155), c(150, 160)),
    B.4 = make_mock_result(2018:2019, c(245, 255), c(250, 260))
  )
  group_keys <- expand.grid(population = c("A", "B"), age = c(3, 4))
  group_keys <- group_keys[order(group_keys$population, group_keys$age), ]
  rownames(group_keys) <- NULL

  agg <- aggregate_group_performance(
    results_list = results_list,
    group_keys = group_keys,
    group_vars = c("population", "age"),
    aggregate_by = c("population", "age")
  )

  expect_equal(nrow(agg$series), 2) # one row per year, nothing left to group by
  row_2018 <- agg$series[agg$series$year == 2018, ]
  expect_equal(row_2018$predicted_abundance, 95 + 195 + 145 + 245)
  expect_equal(row_2018$abundance, 100 + 200 + 150 + 250)
  expect_equal(nrow(agg$performance), 1)
})

test_that("aggregate_group_performance rejects aggregate_by outside group_vars", {
  results_list <- list(A.3 = make_mock_result(2020, 95, 100))
  group_keys <- data.frame(population = "A", age = 3)
  expect_error(
    aggregate_group_performance(results_list, group_keys, group_vars = "population",
                                aggregate_by = "age"),
    "must be a subset"
  )
})
