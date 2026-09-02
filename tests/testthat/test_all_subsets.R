# Test all_subsets function
test_that("Test all_subsets function", {
  load_or_generate_data <- function(use_rda = TRUE, excel_path = NULL) {
    if (use_rda) {
      # Load data from .rda file in the "data" folder
      dat <- dat
    } else {
      csv_path <- system.file("data", "up_sum_chk.csv", package = "SalmonForecasting")

      if (file.exists(csv_path)) {
        # Read data from CSV if it exists
        up_sum_chk <- read.csv(csv_path)
      } else {
        # Generate data using make_dat_from_excel
        up_sum_chk <- readxl::read_xlsx(excel_path, sheet = 1) %>%
          brood_to_return() %>%
          mutate(
            abundance = Age4 + Age5 + Age6
          ) %>%
          dplyr::select(year = ReturnYear, abundance, Age4, Jack = Age3) %>%
          arrange(year)

        #Save up_sum_chk as CSV
        write.csv(up_sum_chk, csv_path, row.names = FALSE)
      }

      dat <- make_dat(dat1 = up_sum_chk)
    }

    return(dat)
  }
  # Set use_rda to TRUE to load from .rda file or FALSE to generate from Excel
  use_rda <- TRUE
  excel_path <- system.file("data", "SummerChinook.xlsx", package = "SalmonForecasting")

  # Call the function
  dat <- load_or_generate_data(use_rda = use_rda, excel_path = excel_path)

  # Conditional assignment of covariates
  if (use_rda) {
    covariates <- c(
      "lag1_log_JackOPI",
      "lag1_log_SmAdj",
      "lag1_NPGO",
      "lag1_PDO",
      "WSST_A",
      "PDO.MJJ",
      "MEI.OND",
      "UWI.JAS",
      "SST.AMJ",
      "SSH.AMJ",
      "UWI.SON"
    )
  } else {
    covariates <- c(
      "lag1_log_Jack",
      "lag4_log_adults",
      "lag5_log_adults",
      "lag1_log_SAR",
      "lag2_log_SAR",
      "lag1_NPGO",
      "lag1_PDO",
      "lag2_NPGO",
      "lag2_PDO",
      "lag2_PC1",
      "lag2_PC2",
      "lag2_sp_phys_trans",
      "pink_ind",
      "lag1_log_socksmolt"
    )
  }


  # Run the function using the loaded data
  results <- all_subsets(



    series = dat,
    covariates=covariates,
    min<-0,
    max<-1,
    type = "preseason",
    fit = TRUE
  )

  # Add test assertions
  expect_equal(length(results), 2)

})

test_that("all_subsets includes the null (no-covariate) model in `vars` when min = 0, regardless of `fit`", {
  set.seed(42)
  d <- data.frame(
    year = 2000:2015, species = "x", period = 1,
    abundance = runif(16, 50, 150),
    cov1 = rnorm(16), cov2 = rnorm(16)
  )

  # fit = FALSE is the path used by do_forecast(); the null model must still show up in
  # `vars` (the first list element) since that's what's passed to one_step_ahead().
  res_no_fit <- all_subsets(series = d, covariates = c("cov1", "cov2"), min = 0, max = 1,
                            type = "preseason", fit = FALSE)
  vars_no_fit <- res_no_fit[[1]]
  expect_equal(length(vars_no_fit), 3) # cov1, cov2, and the null model
  expect_true(any(sapply(vars_no_fit, length) == 0))
  expect_null(res_no_fit[[2]])

  # fit = TRUE should fit the null model too, and it should appear in the AICc table
  # with the "abundance ~ 1" formula.
  res_fit <- all_subsets(series = d, covariates = c("cov1", "cov2"), min = 0, max = 1,
                         type = "preseason", fit = TRUE)
  vars_fit <- res_fit[[1]]
  table_fit <- res_fit[[2]]
  expect_equal(length(vars_fit), 3)
  expect_true(any(sapply(vars_fit, length) == 0))
  expect_true("abundance ~ 1" %in% table_fit$formula)
  expect_equal(nrow(table_fit), 3)
})
