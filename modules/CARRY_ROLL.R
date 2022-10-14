eur_carry_roll <-
  calculate_carry(
    data_raw = carry_roll_df %>% dplyr::filter(!TICKER %in% provie_tickers),
    currency = "EUR",
    BENCHMARK_SECURITY = BENCHMARK_SECURITY_EUR,
    repo = 0
  ) %>%
  dplyr::filter(
    CRNCY %in% c("EUR"), 
    TICKER != BENCHMARK_SECURITY_EUR,
    MATURITY > 1.5,
    MATURITY < 15,
    abs(CARRY_ROLL_12M) != Inf, 
    abs(CARRY_ROLL_12M) < 3
  )

cad_carry_roll <-
  calculate_carry(
    data_raw = carry_roll_df,
    currency = "CAD",
    BENCHMARK_SECURITY = BENCHMARK_SECURITY_CAD,
    repo = 0
  ) %>%
  dplyr::filter(
    CRNCY %in% c("CAD"), 
    TICKER != BENCHMARK_SECURITY_CAD,
    MATURITY > 1.5,
    MATURITY < 12,
    abs(CARRY_ROLL_12M) != Inf, 
    abs(CARRY_ROLL_12M) < 3
  )

aud_carry_roll <-
  calculate_carry(
    data_raw = carry_roll_df %>% dplyr::filter(!TICKER %in% c(provie_tickers)),
    currency = "AUD",
    BENCHMARK_SECURITY = BENCHMARK_SECURITY_AUD,
    repo = 0
  ) %>%
  dplyr::filter(
    CRNCY %in% c("AUD"), 
    TICKER != BENCHMARK_SECURITY_AUD,
    MATURITY > 1.5,
    MATURITY < 15,
    abs(CARRY_ROLL_12M) != Inf, 
    abs(CARRY_ROLL_12M) < 3
  )


usa_carry_roll <-
    calculate_carry(
      data_raw = all_securities %>%
        dplyr::filter(!TICKER %in% c(provie_tickers, "B", "CMB"), 
                      CRNCY %in% "USD") %>%
        dplyr::mutate(MATURITY = as.numeric(MATURITY - Sys.Date()) / 365) %>%
        tidyr::drop_na(YLD_YTM_MID),
      currency = "USD",
      BENCHMARK_SECURITY = BENCHMARK_SECURITY_USD,
      repo = 0
    ) %>%
  dplyr::filter(
    CRNCY %in% c("USD"),
    TICKER != BENCHMARK_SECURITY_USD,
    MATURITY > 1.5,
    MATURITY < 15,
    abs(CARRY_ROLL_12M) != Inf,
    abs(CARRY_ROLL_12M) < 3
  )

jpy_carry_roll <-
  calculate_carry_jpy(
    data_raw = carry_roll_df %>% dplyr::filter(TICKER != "JTDB"),
    currency = "JPY",
    BENCHMARK_SECURITY = BENCHMARK_SECURITY_JPY,
    repo = 0
  )


