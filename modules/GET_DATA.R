#set global options and bbg query options
opt <- c(
  "periodicitySelection" = "DAILY",
  "nonTradingDayFillOption" = "NON_TRADING_WEEKDAYS",
  "nonTradingDayFillMethod" = "PREVIOUS_VALUE"
)


# get holdings data using OL API query 
holdings <- get_holdings_data()

# static bbg fields
fields_static <- c(
  "ID_BB_SEC_NUM_DES", 
  "TICKER",
  "CPN",
  "CRNCY", 
  "ISSUE_DT",
  "MATURITY",
  "YLD_YTM_MID",
  "DUR_ADJ_MID",
  "YAS_ISPREAD_TO_GOVT",
  "YAS_ZSPREAD", 
  "YAS_BOND_YLD", 
  "ASSET_SWAP_SPD_MID",
  "YAS_ASW_SPREAD", 
  "YAS_O_SPREAD", 
  "ID_ISIN", 
  "YRS_TO_MTY_ISSUE", 
  "BB_SPRD_TO_SPLINE_EXPONENTIAL_RT", 
  "BB_3M_ZSCORE_SP_TO_SPLINE_EXP_RT", 
  "ID_JAPAN_SHORT_CODE"
)

# set up database connection and pull securities ----------------------------------

securities <-
  lapply(
    bond_lists,
    read_securities,
    database_filename = database_filename
  )

securities_list <- data.table::rbindlist(securities) %>%
  dplyr::select(Security, TICKER) %>%
  unique()

# pull static data for all securities 
all_securities <-
  bloomberg_query_static(
    securities = securities_list$Security,
    fields = c(fields_static),
    tidy_data = FALSE
  ) 

# read in benchmark securities for 1 - 10yr issues 
securities <-
  lapply(
    currencies,
    read_benchmarks,
    ticker_groups = unique(securities_list$TICKER),
    benchmark_years = c(1:10),
    database_filename = database_filename
  ) %>%
  data.table::rbindlist() %>%
  dplyr::select(Security, TICKER, Bmk, currency)

securities <-
  bloomberg_query_static(
    securities = securities$Security,
    fields = c(fields_static),
    tidy_data = FALSE
  ) %>% 
  dplyr::left_join(securities, by = c("Security", "TICKER")) %>%
  dplyr::mutate(TICKER = case_when(TICKER == "BKO" | TICKER == "OBL" | TICKER == "DBR" ~ "DE", TRUE ~ TICKER))


# clean data for outputs ----------------------------------------------------------------------------

# remove yield outliers 2.5% above second biggest 

remove_outlier_filter <- all_securities %>% 
  dplyr::group_by(TICKER, CRNCY) %>% 
  dplyr::mutate(rank = rank(YLD_YTM_MID), YLD_YTM_MID = YLD_YTM_MID  + 2.5) %>% 
  dplyr::filter(rank == 2) %>% 
  dplyr::select(TICKER, CRNCY, YLD_YTM_MID) %>% 
  dplyr::rename("MAX_YIELD" = "YLD_YTM_MID")

# format DF and join holdings 

all_securities <- all_securities %>%
  dplyr::left_join(remove_outlier_filter, by = c("TICKER", "CRNCY")) %>%
  dplyr::filter(YLD_YTM_MID < MAX_YIELD) %>%
  dplyr::mutate(MATURITY_YEARS = as.numeric(MATURITY - Sys.Date()) / 365) %>%
  dplyr::rename("DURATION" = "DUR_ADJ_MID") %>%
  dplyr::mutate(
    TICKER = case_when(TICKER == "BKO" | TICKER == "OBL" | TICKER == "DBR" ~ "DE", TRUE ~ TICKER),
    ID_BB_SEC_NUM_DES = dplyr::case_when(CRNCY == "JPY" ~ str_replace(ID_BB_SEC_NUM_DES, "\\s[^ ]+$", ""),  TRUE ~ ID_BB_SEC_NUM_DES), 
    ID_BB_SEC_NUM_DES = str_replace(ID_BB_SEC_NUM_DES, "JGB", ID_JAPAN_SHORT_CODE)
  ) %>%
  dplyr::left_join(holdings, by = c("ID_BB_SEC_NUM_DES" = "security")) %>%
  dplyr::arrange(BB_SPRD_TO_SPLINE_EXPONENTIAL_RT) %>%
  dplyr::mutate(YRS_TO_MTY_ISSUE = as.character(paste0(round(YRS_TO_MTY_ISSUE, 0), "_year")), 
                YRS_TO_MTY_ISSUE = if_else(YRS_TO_MTY_ISSUE == "0_year", "bills", YRS_TO_MTY_ISSUE),
                CARRY12M = YLD_YTM_MID / DURATION, 
                YAS_O_SPREAD = YAS_O_SPREAD - 8.5)

# select govvies only 

government_bonds <- all_securities %>%
  dplyr::filter(
    TICKER %in% govie_tickers
  ) 

# formatted data frame for carry/rolls calcs 
carry_roll_df <- government_bonds %>% 
  dplyr::filter(
    !grepl(pattern = "TB", x = TICKER, ignore.case = T)
  ) %>%
  dplyr::mutate(
    MATURITY = as.numeric(MATURITY - Sys.Date()) / 365
  ) %>%
  tidyr::drop_na(YLD_YTM_MID) 




# fetch SERIES and series data from BBG ---------------------------------------------

# data_series <-
#   bloomberg_query(
#     securities = unique(securities$Security),
#     fields = c("BLOOMBERG_MID_G_SPREAD"),
#     from_date  = start_date,
#     to_date = end_date,
#     options = opt
#   )
# 
# all_dates <- sort(unique(data_series$Date), decreasing = TRUE)
# valid_dates <- all_dates[all_dates <= end_date]
# start <- valid_dates[1 + 5]
# 
# spreads_change <- data_series %>%
#   dplyr::left_join(securities, by = "Security") %>%
#   dplyr::inner_join(
#     data_series %>% dplyr::filter(Date == as.Date(start)),
#     by = c("Security"),
#     suffix = c("", "_start")
#   ) %>%
#   dplyr::mutate(
#     Change_SPREAD = Value - Value_start,
#     PercentChange_SPREAD = 100 * Change_SPREAD / Value,
#     Period = 5
#   ) %>%
#   dplyr::group_by(currency) %>%
#   dplyr::filter(Date == min(as.Date(end_date) , max(Date))) %>%
#   dplyr::ungroup()


