#' @name all_subsets
#' @title all_subsets Function
#'
#' @description A comprehensive R package for time series analysis, forecasting, and regression modeling.
#'
#' @param series Data with annual values of response (abundance) and predictor covariates.
#' @param covariates Vector of covariates to include in models.
#' @param min Minimum number of covariates within individual model.
#' @param max Maximum number of covariates within individual model.
#' @param type Specifies whether an in-season or pre-season forecast.
#' @param fit Flag for whether to fit models.
#'
#' @return A list containing ...
#'
#' @importFrom dplyr all_of filter_all select ungroup all_vars %>%
#' @importFrom forecast auto.arima
#' @importFrom stats ts
#' @importFrom utils combn
#' @export

all_subsets <- function(series, covariates, min, max, type, fit = TRUE) {
  abundance <- series$abundance  # Define abundance variable
  .=NULL

  # Check if 'dplyr' package is available
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is not available.")
  }

  # Check if 'forecast' package is available
  if (!requireNamespace("forecast", quietly = TRUE)) {
    stop("Package 'forecast' is not available.")
  }

  freq <- ifelse(type == "inseason", 2, 1)
  seasonal <- ifelse(type == "inseason", T, F)

  # Create a character vector of column names to select from the data frame
  covariates_select <- c("year", "species", "period", "abundance", covariates)

  series <- dplyr::ungroup(series) %>%
    dplyr::select(dplyr::all_of(covariates_select)) %>%
    dplyr::filter_all(all_vars(!is.na(.)))

  vars <- list()
  AICc <- c()
  formula <- c()
  model_num <- c()
  tmin <- ifelse(min == 0, 1, min)

  for (i in tmin:max) { # loop over the number of covariates per model
    temp <- utils::combn(covariates, i, simplify = FALSE) # create all unique combinations of length i
    for (j in 1:length(temp)) {
      vars[[length(vars) + 1]] <- temp[[j]]
    }
  }

  if (min == 0) {
    # Add the null (intercept-only, no covariates) model to `vars` itself -- not just to
    # the AICc ranking table -- so that it actually gets fit downstream (e.g. in
    # one_step_ahead()), regardless of whether `fit` is TRUE or FALSE here.
    vars[[length(vars) + 1]] <- character(0)
  }

  total <- length(vars)
  print(paste0("There are ", total, " models to fit! Fitting model number:"))

  if (fit) {
    for (i in 1:length(vars)) {
      print(paste0(i, " out of ", total))

      if (length(vars[[i]]) == 0) {
        xreg <- NULL
      } else {
        xreg <- dplyr::ungroup(series) %>%
          dplyr::select(dplyr::all_of(vars[[i]])) %>%
          as.matrix()
      }

      m1 <- dplyr::ungroup(series) %>%
        dplyr::select(abundance) %>%
        unlist() %>%
        stats::ts(frequency = freq) %>%
        forecast::auto.arima(lambda = 0, seasonal = seasonal, xreg = xreg)
      AICc[i] <- m1$aicc
      formula[i] <- if (length(vars[[i]]) == 0) "abundance ~ 1" else paste0("abundance ~ ", paste(vars[[i]], collapse = " + "))
      model_num[i] <- i
    }
    table <- dplyr::as_tibble(data.frame(model_num, AICc, formula)) %>%
      dplyr::arrange(AICc)
  } else {
    table <- NULL
  }
  results <- list(vars, table)
  return(results)
}
