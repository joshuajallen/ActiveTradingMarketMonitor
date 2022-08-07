#-----------------------------------------------------------------------
## FILE        : UI.R - USER INTERFACE             DATE : 19/08/2020
##
## CREATED BY  : Joshua Allen
##
## DESCRIPTION : Market monitor for US, CAD, EUR, YEN, AUS portfolios, including: 
##               yield and asset swap curves, carry/roll profiles of 
##               and more details view of the investment opportunity set 
##
## INPUTS      : start_date, end_date, REPO rates, MATURITY buckets 

# set config  -------------------------------------------------------

# source environment files
source("\\\\istdba/BuildArchive/ShinyApps/EnvironmentScript/EnvironmentScript_Latest/LOCAL/Artifactory/environment.R")

#set base directories
database_filename <- file.path("N:/Offdata/RM/_Data/ListManagement/list_membership.db")

# boe checkpoint
checkpoint_date <- "2022-01-01"
message(paste0("checkpoint_date: ", checkpoint_date))
boeCheckpoint(checkpoint_date, scanForPackages = FALSE)

# load packages -----------------------------------------------------

library(data.table)
library(DT)
library(lubridate)
library(tidyverse)
library(readr)
library(RSQLite)
library(jrvFinance)
library(sparkline)
library(formattable)
library(Rblpapi)
library(httr)
library(zoo)
library(flexdashboard)
library(ggplot2)
library(janitor)
library(plotly)
library(magrittr)
library(timeDate)

options(digits = 2)
options(DT.fillContainer = T)
options(scipen = 999)

# Load a library so that boeCheckpoint won't complain about it being missing
FIRVr_str <- "library(FIRVr, lib.loc = 'N:/Offdata/RM/_R code repository/RMPackages/_R4.0')"
eval(parse(text=FIRVr_str))


# souce functions ---------------------------------------------------------

source(file.path("./functions/functions.R"))
source(file.path("./config/config.R"))

# set paramters -------------------------------------------------------

# 1. set start/end dates in the format YYYY-MM-DD
start_date =  Sys.Date() - 1.5*360 # start date - default today - 270 days, need this for vol adjusted C+R (times series calc with 2m look back period)
end_date = Sys.Date() # end date - default today
DATE_START <- as.Date(start_date, "%d-%m-%Y")
DATE_END <-  as.Date(end_date, "%d-%m-%Y")

#2. set MATURITY buckets
Buckets = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 15, 20, 25, 30, 40)

# choose benchmark securities for each ortfolio, for carry/roll calculations
BENCHMARK_SECURITY_CAD = "CAN"
BENCHMARK_SECURITY_USD = "T"
BENCHMARK_SECURITY_EUR = "DE"
BENCHMARK_SECURITY_AUD = "ACGB"
BENCHMARK_SECURITY_JPY = "JGB"

# read in config file with defined lists 

config <- readr::read_csv(file = "./data/bond_lists.csv", show_col_types = FALSE)
provie_tickers <- config$PROVIES[!is.na(config$PROVIES)]  
aussie_tickers <- config$AUSSIE_STATES[!is.na(config$AUSSIE_STATES)] 
govie_tickers <- config$GOVIE_TICKERS[!is.na(config$GOVIE_TICKERS)]  
currencies <- config$CURRENCIES[!is.na(config$CURRENCIES)]  
bond_lists <- config$BOND_LISTS[!is.na(config$BOND_LISTS)]  

# source modules for report ----------------------------------------------
source("../modules/GET_DATA.R")
source("../modules/CARRY_ROLL.R")



# portfolio positions -----------------------------------------------------

# set parameters -----------------------------------------------------
#source(file.path(baseDir, "functions/functions.R"))
DASHBOARD_ROOT <- readLines("N:/Offdata/RM/DASHBOARD_ROOT")
config_location <- paste0(DASHBOARD_ROOT, "/../P&L Config/config") 
pnl_location <- paste0(DASHBOARD_ROOT, "/../P&L Config/historical/")

opt <- c(
  "periodicitySelection" = "DAILY",
  "nonTradingDayFillOption" = "NON_TRADING_WEEKDAYS",
  "nonTradingDayFillMethod" = "PREVIOUS_VALUE"
)
# functions

strategies <-
  read.csv(file.path(config_location, "strategy.csv")) %>%
  dplyr::filter(
    Closed == FALSE,
    Level0 %in% c("Active Management"),!Level1 %in% c("Active Residual")
  ) %>%
  dplyr::select(StrategyID, Strategy, Level1, Portfolio, CIX) %>%
  tidyr::drop_na()

mapping <- read.csv(file.path(config_location , "strategy_trade_mapping.csv")) %>% 
  dplyr::select(StrategyID, Deal.No, Portfolio)

config <-
  dplyr::left_join(strategies, mapping, by = c("StrategyID", "Portfolio")) %>%
  dplyr::rename("DEAL_TRACKING_NUM" = "Deal.No") %>% 
  dplyr::distinct()

results <-
  openlink_deal_info(OL_deal_number = unique(config$DEAL_TRACKING_NUM)) %>%
  dplyr::select(DEAL_TRACKING_NUM, TICKER, BUY_SELL) %>%
  dplyr::left_join(config, by = c("DEAL_TRACKING_NUM")) %>%
  tidyr::drop_na(TICKER) %>%
  dplyr::mutate(
    instruments =  gsub(pattern = "_", " ", TICKER),
    issuer_coupon = gsub('.{6}$', '', instruments),
    date = stringr::str_sub(instruments, -6, -1),
    day = stringr::str_sub(date, 1, 2),
    month = stringr::str_sub(date, 3, 4),
    year = stringr::str_sub(date, 5, 6),
    date = paste0(month, "/", day, "/", year),
    TICKER = paste0(issuer_coupon, " ", date, " Govt")
  ) %>%
  dplyr::select(
    Portfolio,
    Strategy,
    StrategyID,
    CIX,
    Level1,
    DEAL_TRACKING_NUM,
    TICKER,
    BUY_SELL,
    TICKER
  )

fields_static <- c("DUR_ADJ_MID")

duration <-
  bloomberg_query_static(
    securities = unique(results$TICKER),
    fields = fields_static,
    tidy_data = FALSE
  ) %>%
  tidyr::drop_na() 


portfolio_results <-
  dplyr::left_join(results, duration, by = c("TICKER" = "Security")) %>%
  tidyr::drop_na(DUR_ADJ_MID) %>%
  dplyr::group_by(CIX) %>%
  dplyr::arrange(CIX, BUY_SELL) %>%
  dplyr::filter(DUR_ADJ_MID == max(DUR_ADJ_MID, na.rm = T)) %>%
  dplyr::slice(1) %>%
  dplyr::select(Portfolio, Strategy, StrategyID, CIX, Level1, DUR_ADJ_MID) %>%
  dplyr::mutate(CIX = as.character(CIX))

# get PnL figures ---------------------------------------------------------

eom_date <- Sys.Date() - 2

if(timeDate:: isWeekend(eom_date)){ eom_date <-  eom_date <- Sys.Date() - 4}
if(timeDate:: isWeekend(eom_date)){ eom_date <-  eom_date <- Sys.Date() -5}


eom_pl <- tibble()

eom_pl  <-
  readr:: read_csv(file = file.path(pnl_location,
                                    paste0("PL_", eom_date, ".csv")),
                   col_types = if_else(eom_date > "2020-01-31",
                                       "Dcccdcddd", # Read PV01 column too
                                       "Dcccdcdd")  # PV01 not in old files
  ) %>%
  left_join(
    read_csv(
      file = file.path(pnl_location,
                       paste0("Strategy_", eom_date, ".csv")),
      col_types = cols(
        StrategyID = col_double(),
        Level2 = col_character(),
        Level3 = col_character(), 
        CIX = col_character()
      )
    ) %>%
      select(c(StrategyID, Level2, Level3, CIX)),
    by = c("StrategyID")
  ) 

profit_loss <- eom_pl %>% 
  dplyr:: select(StrategyID, Portfolio, `P&L ($s)`, `PV01 ($s)`)


# get CIX data ------------------------------------------------------------

cix <- bloomberg_query_static(
  securities = unique(portfolio_results$CIX) %>% na.omit(),
  fields = c("PX_LAST"),
  tidy_data = FALSE
) %>%
  dplyr::rename("Value" = "PX_LAST") %>%
  dplyr::select(Security, Value)

ytd_daycount <-
  sum(!weekdays(seq(
    as.Date(paste0(lubridate::year(Sys.Date(
    )) - 1, "-12-31")), lubridate::today(), "days"
  )) %in% c("Saturday", "Sunday"))

cix_series <- bloomberg_query(
  securities = unique(portfolio_results$CIX),
  fields = c("PX_LAST"),
  from_date  = start_date,
  to_date = end_date,
  options = opt
) %>%
  dplyr::select(Security, Date, Value) %>%
  dplyr::arrange(Security, Date) %>%
  dplyr::group_by(Security) %>%
  dplyr::distinct() %>%
  dplyr::mutate(
    Stdev = sd(Value - lag(Value, 1), na.rm = TRUE),
    Value1D = (Value - lag(Value)),
    Value7D = (Value - lag(Value, 7)),
    Value30D = (Value - lag(Value, 30)),
    ValueYTD = (Value - lag(Value, ytd_daycount))
  )

# bring data together -----------------------------------------------------

portfolio_table <- portfolio_results %>%
  dplyr::left_join(profit_loss, by = c("StrategyID", "Portfolio")) %>%
  dplyr::left_join(cix, by = c("CIX" = "Security")) %>%
  dplyr::left_join(
    dplyr::select(
      cix_series,
      Date,
      Security,
      Stdev,
      Value1D,
      Value7D,
      Value30D,
      ValueYTD
    ) %>% unique(),
    by = c("CIX" = "Security")
  ) %>%
  dplyr::mutate(
    CARRY_1Y = Value / DUR_ADJ_MID,
    CARRY_1M = CARRY_1Y / 12,
    `12M_PnL` = CARRY_1Y * `PV01 ($s)`,
    PnL_Maturity = Value * `PV01 ($s)`,
    `CARRY_VOL_ADJ` = CARRY_1M / (Stdev)
  )

# add spark lines ---------------------------------------------------------

spark <- add_sparklines(cix_series)
result <- cix_series %>%
  dplyr::group_by(Security) %>%
  dplyr::filter(Date == max(Date, na.rm = T)) %>% 
  dplyr::select(Security, Value7D)

x <-  cix %>%
  dplyr:: select(Security, Value) %>% 
  dplyr:: left_join(
    spark,
    by = c("Security")
  ) %>% 
  dplyr:: left_join(
    result,
    by = c("Security")
  ) %>% 
  dplyr:: select(Security, everything()) %>% 
  dplyr:: mutate_if(is.numeric, round, 3)

x <- dplyr:: select(x, Security, Timeseries,`Box plot`)
portfolio_table<- dplyr:: left_join(portfolio_table, x, by = c("CIX" = "Security")) 

rm(cix_series, cix, x)

# final table --------------------------------------------------------------

suppressWarnings(
  portfolio_table <- portfolio_table %>%
    dplyr::filter(Date == max(Date, na.rm = T)) %>%
    dplyr::rename('Latest Value (Bps)' = Value,
                  '1-Day Change (Bps)' = Value1D,
                  '1-Week Change (Bps)' = Value7D,
                  '30-Day Change (Bps)' = Value30D,
                  'YTD Change (Bps)' = ValueYTD
    ) %>% 
    dplyr::arrange(Portfolio) %>% 
    dplyr::select(-Date)
)

df <- dplyr::filter(portfolio_table) %>%
  dplyr::mutate(PF = Portfolio, 
                Level1 = if_else(Level1 %in% c("Relative Value"), "RV", Level1), 
                Level1 = if_else(Level1 %in% c("Swap Spread"), "Swaps", Level1), 
                `CARRY VOL ADJ` = CARRY_VOL_ADJ, 
                DURATION = DUR_ADJ_MID) %>% 
  dplyr::select(
    Portfolio,
    PF, 
    Level1,
    Strategy,
    CIX,
    contains("Value"),
    contains("Change"),
    Stdev,
    DURATION,
    CARRY_1Y,
    `CARRY VOL ADJ`,
    `12M_PnL`,
    `P&L ($s)`,
    `PV01 ($s)`,
    Timeseries,
    `Box plot`
  ) %>%
  dplyr::arrange(Portfolio, Level1)

ism <- df %>% lapply(class) %>% unlist == "numeric"


DT::datatable(
  df, 
  extensions = c("RowGroup", "Buttons", "Scroller"),
  filter = "top",
  rownames = FALSE,
  escape = FALSE,
  selection = "none",
  options = list(
    scrollX = T, 
    scrollY = "500px", 
    dom = 't',
    ordering = TRUE,
    dom = "",
    paging = FALSE,
    rowGroup = 'Portfolio',
    columnDefs = list(list(
      visible = FALSE, targets = c(0)
    )), 
    fnDrawCallback = htmlwidgets::JS(
      "function() { HTMLWidgets.staticRender(); }"
    )
  )
  ) %>%
  formatRound('Latest Value (Bps)', 2) %>% 
  formatRound(ism, digits = 0) %>% 
  formatStyle('1-Day Change (Bps)',
              color = styleInterval(0, c(
                app_config$boe_palette[2],
                app_config$boe_palette[5]
              )),) %>%
  formatStyle('1-Week Change (Bps)',
              color = styleInterval(0, c(
                app_config$boe_palette[2],
                app_config$boe_palette[5]
              )),) %>%
  formatStyle('YTD Change (Bps)',
              color = styleInterval(0, c(
                app_config$boe_palette[2],
                app_config$boe_palette[5]
              ))) %>%
  formatStyle(0, target = 'row', lineHeight = '75%') %>%
  formatRound(
    c(
      '1-Day Change (Bps)',
      '1-Week Change (Bps)',
      '30-Day Change (Bps)',
      'YTD Change (Bps)', 
      'CARRY VOL ADJ', 
      'CARRY_1Y', 
      "DURATION"
    ),
    1
  ) %>%
  formatStyle(ism, color = styleInterval(0, c("red", "black"))) %>%
  formatStyle('CARRY_1Y',
              backgroundColor = styleInterval(0, c('lightpink', 'lightgreen'))
  ) %>%
  formatStyle('P&L ($s)',
              background = styleColorBar(df$`P&L ($s)`, 'lightblue'),
              backgroundSize = '95% 30%',
              backgroundRepeat = 'no-repeat',
              backgroundPosition = 'left')  %>%
  formatStyle('PV01 ($s)',
              background = styleColorBar(df$`PV01 ($s)`, 'lightblue'),
              backgroundSize = '95% 30%',
              backgroundRepeat = 'no-repeat',
              backgroundPosition = 'left') %>%
  formatStyle('12M_PnL',
              background = styleColorBar(df$`12M_PnL`, "#CAC0B6"),
              backgroundSize = '95% 30%',
              backgroundRepeat = 'no-repeat',
              backgroundPosition = 'left') %>%
  # formatStyle('PnL_Maturity',
  #             background = styleColorBar(df$`PnL_Maturity`, "#CAC0B6"),
  #             backgroundSize = '95% 30%',
  #             backgroundRepeat = 'no-repeat',
  #             backgroundPosition = 'left') %>%
  sparkline:: spk_add_deps() 




















