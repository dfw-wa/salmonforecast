#' @name make_dat
#' @description Scrape covariates from the web and add covariates to return table
#' @title Make data
#' @param dat1 Input file with fields called "year", "abundance", and "Jack"
#' @param covariates Vector of covariate names to include in the output. Options are:"lag1_log_Jack", "lag4_log_adults", "lag5_log_adults", "lag1_log_SAR","lag2_log_SAR", "lag1_NPGO", "lag1_PDO", "lag2_NPGO", "lag2_PDO", "lag2_PC1","lag2_PC2", "lag2_sp_phys_trans", "pink_ind", and "lag1_log_socksmolt", where the "SAR" covariates are sockeye SARs from PIT tags
#' @return A data frame with the processed data.
#' @export
#' @importFrom dplyr right_join add_tally across
#' @importFrom readxl read_xlsx
#' @import mgcv
#' @importFrom magrittr %<>%
make_dat <- function(dat1,
                     covariates = c(
                       #
                       "lag1_log_Jack",
                       "lag4_log_adults",
                       "lag5_log_adults",
                       "lag1_log_SAR" # snake river sockeye PIT SAR
                       , "lag2_log_SAR" # snake river sockeye PIT SAR
                       # ,"lag1_NPGO"
                       , "lag1_PDO"
                       # ,"lag2_NPGO"
                       , "lag2_PDO"
                       # ,"WSST_A"
                       # NOAA Ocean Indicators
                       , "lag2_PC1",
                       "lag2_PC2",
                       "lag2_sp_phys_trans",
                       "pink_ind",
                       "lag1_log_socksmolt" # sockeye smolts
                     )) {
  # scrape covariates and munge data
  # add any missing years in the input to the output (with NAs in the values) because ARIMA models assume that the years are consecutive.

  Yrlist <- data.frame(year = c(min(dat1$year):(max(dat1$year) + 1)))
  yr_end <- tail(Yrlist$year, 1)

  dat1 %<>%
    dplyr::right_join(Yrlist) %>%
    arrange(year) %>%
    # add some columns
    mutate(
      species = "fish", # isn't really used
      period = 1, # isng't raelly used
      ## covariates based on input data
      lag1_log_Jack = lag(log(Jack)),
      # lag1_log_age4=lag(log(Age4)),
      lag4_log_adults = lag(log(abundance), 4),
      lag5_log_adults = lag(log(abundance), 5)
    )

  #----------------------------------------------------------------
  # scrape covariate data from web

  # =========================================================
  # get PDO data
  # =========================================================
  PDO <- read_table("https://psl.noaa.gov/pdo/data/pdo.timeseries.ersstv5.csv", skip = 1, col_names = F, comment = "#") %>%
    dplyr::rename(Date = X1, PDO = X2) %>%
    filter(!PDO < -99) %>%
    mutate(Date = as.Date(Date), Month = month(Date), Year = as.integer(year(Date))) %>%
    group_by(Year) %>%
    dplyr::add_tally() %>%
    # filter(!Month>6)%>% #use only spring (Jan-June) NPGO
    # filter(!n < 12)%>% #use only complete years
    group_by(Year) %>%
    dplyr::rename(year = Year) %>%
    dplyr::summarise(PDO = mean(PDO)) %>%
    dplyr::right_join(Yrlist) %>%
    mutate(
      lag1_PDO = lag(PDO, 1),
      lag2_PDO = lag(PDO, 2)
    ) %>%
    dplyr::select(year, lag1_PDO, lag2_PDO) %>%
    filter(!is.na(lag1_PDO))

  # =========================================================
  # get NPGO data
  # =========================================================
  NPGO <- read_table("https://www.o3d.org/npgo/data/NPGO.txt", skip = 29, col_names = F, comment = "#") %>%
    filter(!is.na(X2)) %>%
    dplyr::rename(Year = X1, Month = X2, NPGO = X3) %>%
    mutate(Year = as.integer(as.numeric(Year))) %>%
    group_by(Year) %>%
    add_tally() %>%
    filter(between(Month, 4, 5)) %>% # use only spring (Jan-June) NPGO
    # filter(!n < 12)%>% #use only complete years
    group_by(Year) %>%
    dplyr::summarise(NPGO = mean(NPGO)) %>%
    dplyr::rename(year = Year) %>%
    right_join(Yrlist) %>%
    mutate(
      lag1_NPGO = lag(NPGO),
      lag2_NPGO = lag(NPGO, 2)
    ) %>%
    arrange(year) %>%
    dplyr::select(year, lag1_NPGO, lag2_NPGO) # %>%
  # filter(!is.na(lag1_NPGO))
  # =========================================================
  # get NOAA indicator data, wrangle into usable format
  # =========================================================
  indicators <- read_csv("https://www.fisheries.noaa.gov/s3//2022-12/OEI-spotlight-cvs-2022-NWFSC.csv", skip = 1) %>%
    filter(!is.na(`Ecosystem Indicators`)) %>%
    pivot_longer(
      names_to = "Year",
      cols = c(starts_with("1"), starts_with("2")),
      values_to = "value"
    ) %>%
    pivot_wider(names_from = `Ecosystem Indicators`, values_from = value) %>%
    mutate(year = as.integer(Year)) %>%
    dplyr::select(-Year) %>%
    mutate(
      lag2_sp_phys_trans = lag(`Physical Spring Trans.\nUI based (day of year)`, 2),
      lag2_PC1 = lag(scale(`Principal Component scores (PC1)`), 2),
      lag2_PC2 = lag(scale(`Principal Component scores (PC2)`), 2)
    )

  # =========================================
  # Get ENSO index from PSL CSV endpoint
  # =========================================
  enso_raw <- readr::read_csv(
    "https://psl.noaa.gov/data/timeseries/month/data/nino34.long.anom.csv",
    show_col_types = FALSE
  )
  names(enso_raw)[1:2] <- c("Date", "Nino3.4")

  enso <- enso_raw %>%
    dplyr::mutate(
      Date = as.Date(Date),
      year = lubridate::year(Date),
      month = lubridate::month(Date),
      Nino3.4 = as.numeric(Nino3.4)
    ) %>%
    dplyr::filter(!is.na(Nino3.4), Nino3.4 > -99) %>%
    dplyr::filter(month %in% c(10, 11, 12)) %>%
    dplyr::group_by(year) %>%
    dplyr::summarise(fall_Nino3.4 = mean(Nino3.4), .groups = "drop") %>%
    dplyr::right_join(Yrlist, by = "year") %>%
    dplyr::mutate(lag2_fall_Nino3.4 = dplyr::lag(fall_Nino3.4, 2)) %>%
    dplyr::select(year, lag2_fall_Nino3.4)

  # ===========================================================================================================
  # Get ERSST data: get_ersst_v5_data takes A LONG TIME (1 hr) vs. get_ersst_v5_data_V2 which is much quicker!
  # ==========================================================================================================
  # sstdat<-get_ersst_v5_data_V2(years=c(min(Yrlist$year):max(Yrlist$year)),
  #                              data.dir=#"https://stateofwa.sharepoint.com/:u:/r/sites/DFW-TeamWDFWOPIModelReview/Shared%20Documents/General/sst.mnmean.nc?csf=1&web=1&e=mhx1bc",
  #                                "C:\\Users\\sorelmhs\\Washington State Executive Branch Agencies\\DFW-Team WDFW OPI Model Review - General" ,
  #                              ncfilename="sst.mnmean.nc",
  #                              latrange=c(44,50),
  #                              lonrange=c(-125,-120)
  # )
  #
  # ssta<-sstdat%>%
  #   dplyr::select(year,month,resid)%>%
  #   mutate(month=as.numeric(month))%>%
  #   mutate(year=ifelse(month>9,year+1,year))%>%
  #   group_by(year)%>%
  #   summarise(WSST_A = mean(resid[month>9|month<11]),
  #             SSST_A = mean(resid[month<=9|month>=4]),
  #   )


  # =========================================
  # Get Snake River Sockeye PIT tag-based SAR
  # =========================================
  if ("lag1_log_SAR" %in% covariates |
    "lag2_log_SAR" %in% covariates) {
    PIT <- read_csv("https://www.cbr.washington.edu/dart/cs/php/rpt/pit_sar_esu.php?queryType=year&proj=BON&esu_type=SR_Sock&rt=A&age=all&grouptype=basin&csvOnly=1") %>%
      mutate(across(year, as.numeric)) %>%
      filter(year != year(Sys.Date())) %>%
      mutate(OutmigrationYear = year, Year = OutmigrationYear + 2)

    PIT <- PIT %>%
      bind_cols(data.frame(SAR1 = mgcv::gam(cbind(ocean1Count, juvCount - ocean1Count) ~ s(OutmigrationYear, k = (dim(PIT)[1]), m = 1, bs = "ps"), family = binomial, data = PIT)$fitted)) %>%
      bind_cols(data.frame(SAR2 = c(mgcv::gam(cbind(ocean2Count, juvCount - ocean2Count) ~ s(OutmigrationYear, k = (dim(PIT)[1] - 1), m = 1, bs = "ps"), family = binomial, data = PIT)$fitted, NA))) %>%
      mutate(lag1_log_SAR = log(SAR1), lag2_log_SAR = lag(log(SAR2), 1)) %>%
      dplyr::select(year = Year, lag1_log_SAR, lag2_log_SAR)
  }

  # =========================================================
  # get sockeye smolt index at Bonneville
  # =========================================================
  if ("lag1_log_socksmolt" %in% covariates) {
    smolt_sock <- read_csv("https://www.cbr.washington.edu/dart/cs/php/rpt/mg.php?sc=1&mgconfig=smolt&outputFormat=csvSingle&year%5B%5D=2023&year%5B%5D=2022&year%5B%5D=2021&year%5B%5D=2020&year%5B%5D=2019&year%5B%5D=2018&year%5B%5D=2017&year%5B%5D=2016&year%5B%5D=2015&year%5B%5D=2014&year%5B%5D=2013&year%5B%5D=2012&year%5B%5D=2011&year%5B%5D=2010&year%5B%5D=2009&year%5B%5D=2008&year%5B%5D=2007&year%5B%5D=2006&year%5B%5D=2005&year%5B%5D=2004&year%5B%5D=2003&year%5B%5D=2002&year%5B%5D=2001&year%5B%5D=2000&year%5B%5D=1999&year%5B%5D=1998&year%5B%5D=1997&year%5B%5D=1996&year%5B%5D=1995&year%5B%5D=1994&year%5B%5D=1993&year%5B%5D=1992&year%5B%5D=1991&year%5B%5D=1990&year%5B%5D=1989&year%5B%5D=1988&year%5B%5D=1987&year%5B%5D=1986&year%5B%5D=1985&loc%5B%5D=BON&ftype%5B%5D=Sock&data%5B%5D=&startdate=4%2F1&enddate=6%2F30&sumAttribute=none&cumAttribute%5B%5D=Smolt+Index&consolidate=1&zeros=1&grid=1&y1min=0&y1max=&y2min=&y2max=&size=large") %>%
      filter(`mm-dd` == "6-30") %>%
      dplyr::select(year, smolt_sock = "value") %>%
      mutate(across(year, as.numeric), lag1_log_socksmolt = lag(log(smolt_sock)))
  }
  # end of scraping covariate data from web


  # ================================================================
  #   Joing fish data "dat1" and covariate data
  #

  dat <- dat1 %>%
    ungroup() %>%
    # left_join(Yrlist)%>%
    left_join(PDO) %>%
    # left_join(NPGO)%>%
    left_join(indicators) %>%
    left_join(enso) # %>%
  # left_join(ssta)%>%
  # left_join(OCN)


  if ("lag1_log_SAR" %in% covariates |
    "lag2_log_SAR" %in% covariates) {
    dat <- dat %>% left_join(PIT)
  }
  if ("lag1_log_socksmolt" %in% covariates) {
    dat <- dat %>% left_join(smolt_sock)
  }
  # ================================================================
  #
  # add pink salmon indicator variables
  dat <- dat %>%
    mutate(pink_ind = ifelse( # year>1999 &
      year %% 2 == 0, 0, 1
    )) %>%
    ungroup() %>%
    # selecting covariates we want
    dplyr::select(year, species, period, abundance, all_of(covariates)) %>%
    mutate(
      across(all_of(unique(unlist(covariates))), \(x) c(scale(x))), # mean of 0 SD of 1 for covariates
      across(all_of(unique(unlist(covariates))), \(x) replace_na(x, 0))
    ) %>% # replace NAs with 0s

    filter(
      !is.na(abundance) | year == yr_end
      # across(
      #   .cols = everything(),
      #   .fns = ~ !is.na(.x)
      # )
    ) # %>%mutate(across(!c(year,species,period,abundance),boxcox_scale(.)))


  return(dat)
}




#' @name brood_to_return
#' @title Convert a brood table to a return table
#'
#' @param bt Brood table data frame.
#' @description Useful if you have data in brood table format because it needs to be in return table format for the "make_dat_function.
#' @return Returns a "return table" data frame.
#' @export
brood_to_return <- function(bt) {
  bt %>%
    group_by(Stock) %>%
    pivot_longer(cols = contains("Age"), names_to = "AgeNames", values_to = "Return") %>%
    mutate(
      Age = parse_number(AgeNames),
      ReturnYear = BroodYear + Age
    ) %>%
    filter(!is.na(Return)) %>%
    dplyr::select(Stock, ReturnYear, AgeNames, Return) %>%
    pivot_wider(names_from = AgeNames, values_from = Return)
}

