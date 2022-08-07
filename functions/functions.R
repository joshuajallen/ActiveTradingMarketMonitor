boe_palette <- rep(c(
  "#4a7e8f", # teal
  "#cf395c", # bright pink
  "#a9c9d3", # light blue
  "#b25395", # pink
  "#3b855f", # green
  "#2f4f5b", # very dark teal
  "#b65e19", # orange
  "#0f7cbf", # blue
  "#50882E", # dark green
  "#555555", # dark grey
  "#752864", #plum
  "#AC98DB"  #lavendar 
), 3)

custom_color_tile<-function (...) {
  formatter("span",
            style = function(x) style(display = "block", 
                                      padding = "0 4px", 
                                      `color` = "black", 
                                      `border-radius` = "4px", 
                                      `background-color` = csscolor(gradient(as.numeric(x), 
                                                                             ...))))
}


get_position_data <- function(data, index){
  
  data <- bloomberg_query_static(
    securities =  paste0(data$CIX[index]),
    fields = paste0(data$Field[index]),
    tidy_data = FALSE
  )  
  
  colnames(data) <- c("Security", "Value")
  
  return(data)
}

get_series_data <- function(data, index){
  
  data <- bloomberg_query(
    securities =  paste0(data$CIX[index]),
    fields = paste0(data$Field[index]),
    from_date  = start_date,
    to_date = end_date)
  
  colnames(data) <- c("Date", "Security", "Field", "Value")
  
  return(data)
}

#
colorbar <- function(color = "lightgray", fun = "comma", digits = 0) {
  fun <- match.fun(fun)
  formatter("span", x ~ fun(x, digits = digits),
            style = function(y) style(
              display = "inline-block",
              direction = "rtl",
              "border-radius" = "4px",
              "padding-right" = "2px",
              "background-color" = csscolor(color),
              width = percent(proportion(as.numeric(y), na.rm = TRUE))
            )
  )
}

# create_ssa_table <- function(df,underline_tickers = FALSE){
#   
#   as.datatable(df,
#                extensions = c("Buttons", "Scroller"),filter = "bottom",
#                
#                options = list(
#                  # Table components can be added/removed here
#                  #dom = "Blfrtip",
#                  
#                  # Freezing panes
#                  fixedHeader = TRUE,
#                  scrollX = T, 
#                  scrollY = "500px", 
#                  dom = 't',
#                  #ordering = FALSE,
#                  dom = "",
#                  paging = FALSE,
#                  
#                  # Specify buttons on top of table
#                  buttons = c("copy", "excel", I("colvis")),
#                  scrollX = T, 
#                  scrollY = "500px", 
#                  ordering = TRUE,
#                  # Page length and menu
#                  paging = TRUE,
#                  pageLength = 50,
#                  lengthMenu = list(
#                    c(5, 10, 20, -1),
#                    list("5", "10", "20", "All")
#                  )
#                )
#   ) %>%
#     sparkline:: spk_add_deps() %>%
#     # formatCurrency(
#     #   c(target_date, "Change", "Min", "Max",
#     #     "Average", "Stdev", "Z-score"),
#     #   digits = 3,
#     #   currency = ""
#     # ) %>%
#     formatStyle(
#       "CIX",
#       textDecoration = if_else(underline_tickers, "underline", "none")
#     ) %>%
#     formatStyle(
#       c("Z-score"),
#       fontWeight = "bold"
#     ) %>%
#     formatStyle(
#       "Z-score",
#       color = styleInterval(c(-3, -2, 2, 3),
#                             c("white", "white", "black", "white", "white")),
#       backgroundColor = styleInterval(
#         c(-3, -2, 2, 3),
#         c("#aa0b3c", "#b65e19", NA, "#b65e19", "#aa0b3c")
#       )
#     ) 
# }

create_ssa_table <- function(df,underline_tickers = FALSE){
  
  ism <- df %>% lapply(class) %>% unlist == "numeric"
  
  DT:: datatable(
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
      # rowGroup = list(
      #   dataSrc = row_group_index
      # ),
      fnDrawCallback = htmlwidgets::JS(
        "function() { HTMLWidgets.staticRender(); }"
      )
    )
  ) %>% formatRound(ism, digits = 2) %>% 
    formatStyle(ism, color = styleInterval(0, c("red", "black"))) %>%
    formatStyle('CARRY_1Y',
                backgroundColor = styleInterval(0, c('lightpink', 'lightgreen'))
    ) %>%#
    # formatStyle('CARRY_1M',
    #             backgroundColor = styleInterval(0, c('lightpink', 'lightgreen'))
    # ) %>%
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
    # formatStyle('1M_ROBs',
    #             background = styleColorBar(df$`1M_ROBs`, 'lightblue'),
    #             backgroundSize = '95% 30%',
    #             backgroundRepeat = 'no-repeat',
    #             backgroundPosition = 'left') %>%
    # formatStyle('1M_PnL',
    #             background = styleColorBar(df$`1M_PnL`, "#CAC0B6"),
    #             backgroundSize = '95% 30%',
    #             backgroundRepeat = 'no-repeat',
    #             backgroundPosition = 'left') %>%
    formatStyle('12M_PnL',
                background = styleColorBar(df$`12M_PnL`, "#CAC0B6"),
                backgroundSize = '95% 30%',
                backgroundRepeat = 'no-repeat',
                backgroundPosition = 'left') %>%
    formatStyle('PnL_Maturity',
                background = styleColorBar(df$`PnL_Maturity`, "#CAC0B6"),
                backgroundSize = '95% 30%',
                backgroundRepeat = 'no-repeat',
                backgroundPosition = 'left') %>%
    sparkline:: spk_add_deps() #%>%
  # formatStyle(
  #   c("Z-score"),
  #   fontWeight = "bold"
  # ) %>%
  # formatStyle(
  #   "Z-score",
  #   color = styleInterval(c(-3, -2, 2, 3),
  #                         c("white", "white", "black", "white", "white")),
  #   backgroundColor = styleInterval(
  #     c(-3, -2, 2, 3),
  #     c("#aa0b3c", "#b65e19", NA, "#b65e19", "#aa0b3c")
  #   )
  # )
}



get_eligible_futures_contracts <- function(contracts){
  
  if (is.null(contracts)) {
    
    return(data.frame())
    
  } else{
    
    bloomberg_query_dataset(contracts,
                            "FUT_DLVRBLE_BNDS_ISINS") %>%
      dplyr::mutate(Security = contract,
                    ISIN = substr(`ISIN of Deliverable Bonds`, 1, 12)) %>%
      dplyr::rename(ConversionFactor = `Conversion Factor`) %>%
      dplyr::select(-`ISIN of Deliverable Bonds`)
    
  }
  
}


match_maturities <- function(data, lag = 1, filter = NULL){
  
  if(!is.null(filter)){data <- dplyr:: filter(data, TICKER %in% filter)}
  
  if(nrow(data) > 0){
    
    
    matched <- lapply(1:nrow(data),
                      FUN = function(i){which.min(abs((data$MATURITY[-i] + (lag)) - data$MATURITY[i]))[1] #*365?
                      }
    )
    
    # matched <- lapply(1:nrow(data), 
    #                   FUN = function(i) min(DescTools:: Closest(
    #                     x = data$MATURITY[-i] + 1,
    #                     a = data$MATURITY[i],
    #                     which = TRUE,
    #                     na.rm = TRUE
    #                   )[1]))
    
    
    #data$LONG_SECURITY <- data$Security[unlist(matched)]
    data$MATCH_YTM <- data$YLD_YTM_MID[unlist(matched)]
    data$MATCHED_DISTANCE <- data$MATURITY - data$MATURITY[unlist(matched)]
    
  }else{
    data = data.frame()
  }
  
  return(data)
  
}


match_benchmarks <- function(data, filter, benchmark){
  
  if(!is.null(filter)){data <- dplyr:: filter(data, TICKER %in% c(filter, benchmark)) %>% tidyr:: drop_na(MATURITY)}
  
  if(nrow(data) > 0){
    
    matched <- lapply(1:nrow(data),
                      FUN = function(i){which.min(abs((data$MATURITY[-unique(c(which(data$TICKER == filter), i))]) - data$MATURITY[i]))[1]
                      }
    )
    
    # matched <- lapply(1:nrow(data), 
    #                   FUN = function(i) min(DescTools:: Closest(
    #                     x = data$MATURITY[-unique(c(which(data$TICKER == filter), i))],
    #                     a = data$MATURITY[i],
    #                     which = TRUE,
    #                     na.rm = TRUE
    #                   )[1]))
    
    x <- data[-unique(which(data$TICKER == filter)),]
    
    data$SHORT_SECURITY <- x$Security[unlist(matched)]
    data$BENCHMARK_YLD <- x$YLD_YTM_MID[unlist(matched)]
    data$BENCHMARK_DUR <- x$DURATION[unlist(matched)]
    data$BENCHMARK_ROLL <- x$ROLL_12M[unlist(matched)]
    data$MATCH_TICKER <- x$TICKER[unlist(matched)]
    data$MATCH_TERM <- x$MATURITY[unlist(matched)]
    
    
    
  } else{
    
    data = data.frame()
  }
  
  return(data)
  
  
}


ctd_bonds <- function() {
  
  d <- read_csv(file.path(baseDir, "config/futures/futures_for_ctd.txt"),
                col_names = TRUE,
                cols(Ticker = col_character(),
                     Description = col_character()))
  
  fields_static <- c("FUT_CTD_ISIN", "SECURITY_DES")
  
  data_from_bbg <- bloomberg_query_legacy(
    d$Ticker,
    fields = NULL,
    fields_static,
    from_date = Sys.Date(),
    to_date = Sys.Date()
  ) 
  
  data_from_bbg %>%
    rename(
      ISIN = FUT_CTD_ISIN,
      FUTURE = SECURITY_DES
    ) %>%
    select(
      ISIN,
      FUTURE
    ) %>%
    mutate(
      FUTURE = sapply(strsplit(FUTURE, " "), function(x) {x[1]})
    )
  
}

# Helper function to get a list of benchmarks
#

bmk_bonds <- function() {
  
  d <- read_csv(file.path(baseDir, "config/on_the_run_bonds/on_the_run_bonds.txt"),
                col_names = TRUE,
                cols(Ticker = col_character(),
                     Description = col_character()))
  
  fields_static <- c("ID_ISIN", "SECURITY_DES")
  
  data_from_bbg <- bloomberg_query_legacy(
    d$Ticker,
    fields = NULL,
    fields_static,
    from_date = Sys.Date(),
    to_date = Sys.Date()
  ) 
  
  data_from_bbg %>%
    rename(
      ISIN = ID_ISIN,
      BENCHMARK = SECURITY_DES
    ) %>%
    select(
      ISIN,
      BENCHMARK
    )
  
}




calculate_duration <- function(date, maturity, coupon, frequency, yield){
  
  jrvFinance:: bond.duration(
    settle = date,
    mature = maturity,
    coupon = coupon,
    freq = frequency,
    yield = yield,
    convention = c("ACT/ACT"),
    modified = FALSE
  )
  
}


create_data_table_metrics <- function(data_input,
                                      target_date,
                                      row_group_index,
                                      underline_tickers = FALSE) {
  DT:: datatable(
    data_input,
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
      rowGroup = list(
        dataSrc = row_group_index
      )#,
      # fnDrawCallback = htmlwidgets::JS(
      #   "function() { HTMLWidgets.staticRender(); }"
      # ),
      # columnDefs = list(list("targets" = row_group_index,
      #                        "visible" = FALSE))
    )
  ) %>%
    sparkline:: spk_add_deps() %>%
    # formatCurrency(
    #   c(target_date, "Change", "Min", "Max",
    #     "Average", "Stdev", "Z-score"),
    #   digits = 3,
    #   currency = ""
    # ) %>%
    formatStyle(
      "Security",
      textDecoration = if_else(underline_tickers, "underline", "none")
    ) %>%
    formatStyle(
      c("Z-score"),
      fontWeight = "bold"
    ) %>%
    formatStyle(
      "Z-score",
      color = styleInterval(c(-3, -2, 2, 3),
                            c("white", "white", "black", "white", "white")),
      backgroundColor = styleInterval(
        c(-3, -2, 2, 3),
        c("#aa0b3c", "#b65e19", NA, "#b65e19", "#aa0b3c")
      )
    )
}


calculate_carry <- function(data_raw, currency, BENCHMARK_SECURITY, repo){
  
  if(nrow(data_raw) < 5){return(data.frame())}
  
  tickers <- dplyr:: filter(data_raw, CRNCY == paste0(currency)) %>% pull(TICKER) %>% unique()
  
  carry_roll <-
    lapply(
      X = na.omit(tickers),
      match_maturities,
      data = data_raw,
      lag = 1
    ) %>% data.table::rbindlist()
  
  # calculate 12m roll 
  
  carry_roll <- carry_roll %>% 
    dplyr:: group_by(TICKER) %>% 
    dplyr:: mutate(ROLL_12M = (YLD_YTM_MID - MATCH_YTM)/as.numeric(MATCHED_DISTANCE)) %>% 
    dplyr:: group_by(Security) %>% 
    dplyr::mutate(BUCKET = sapply(MATURITY, function(x)
      Buckets[order(abs(x - Buckets))][1]),
      Distance = abs(BUCKET - MATURITY)) 
  
  tickers <- dplyr:: filter(data_raw, CRNCY == paste0(currency), TICKER != BENCHMARK_SECURITY) %>% pull(TICKER) %>% unique()
  
  carry_roll <-
    lapply(
      X = na.omit(tickers), # securities list
      match_benchmarks, # match securities against benchmarks, based on difference in TERM of security, nearest wins
      data = carry_roll,
      benchmark = BENCHMARK_SECURITY # choose benchmark security to compare C+R profile
    ) %>% 
    data.table::rbindlist() %>% 
    dplyr:: distinct(Security, TICKER, BUCKET,  .keep_all = TRUE) %>% 
    dplyr::mutate(
      ROLL_1Y = ROLL_12M - BENCHMARK_ROLL,
      CARRY_1Y = (((YLD_YTM_MID) / DURATION) - ((BENCHMARK_YLD + repo) / BENCHMARK_DUR)), 
      CARRY_ROLL_12M = ROLL_1Y + CARRY_1Y 
    ) %>% 
    dplyr:: select(-"MATCH_YTM", -"Distance", -"BENCHMARK_YLD", -"BENCHMARK_DUR", -"BENCHMARK_ROLL") %>% 
    dplyr:: arrange(TICKER, desc(CARRY_ROLL_12M)) %>% 
    dplyr:: ungroup()
  
  return(carry_roll)
  

}

calculate_metrics <- function(data, change_period_days){
  
  if (all(c("Date", "Security", "Value") %in% colnames(data)) & nrow(data) > 0) {
    
    if(!"Bmk" %in% colnames(data)) data$Bmk <- NA
    
    data <- data %>%
      dplyr::select(Date, Security, Bmk, Value) %>%
      dplyr::arrange(Security, Date) %>%
      dplyr::group_by(Security, Bmk) %>%
      dplyr::summarise(
        Current = last(Value),
        Change = last(Value) - head(tail(Value, change_period_days + 1), 1),
        Min = min(Value, na.rm = TRUE),
        Max = max(Value, na.rm = TRUE),
        Average = mean(Value, na.rm = TRUE),
        Stdev = sd(Value, na.rm = TRUE)
        
      ) %>%
      mutate(`Z-score` = (Current - Average) / Stdev) %>% 
      dplyr:: ungroup() %>% 
      dplyr:: mutate_if(is.numeric, round, digits=3)
    
  } else{
    
    data <- data.frame()
    
  }
  
  return(data)
  
}

create_data_table <- function(table_input, row_group_index) {
  
  df = as.data.frame(table_input)
  
  if(nrow(df) > 0){
    
    DT:: datatable(
      df,
      extensions = c("RowGroup", "Buttons", "Scroller"),
      filter = "top",
      rownames = FALSE,
      escape = FALSE,
      selection = "none",
      options = list(
        scrollX = T, 
        scrollY = "600px", 
        autoWidth = TRUE, 
        dom = 't',
        ordering = TRUE,
        dom = "",
        paging = FALSE,
        rowGroup = list(
          dataSrc = row_group_index
        ),
        fnDrawCallback = htmlwidgets::JS(
          "function() { HTMLWidgets.staticRender(); }"
        ),
        columnDefs = list(list("targets" = row_group_index,
                               "visible" = FALSE))
      )
    )
    
    
  }
  else{
    DT<- as.data.frame("No data to show in table")
    colnames(DT)[1] <- ""
    DT
  }
}


add_sparklines <- function(data){
  
  if (all(c("Date", "Security", "Value") %in% colnames(data)) &  nrow(data) > 0) {
    
    data <- data %>%
      dplyr::select(Date, Security, Value) %>%
      dplyr::arrange(Security, Date) %>%
      dplyr::group_by(Security) %>%
      dplyr::summarize(
        Timeseries = sparkline::spk_chr(
          Value,
          type = "line",
          chartRangeMin = min(Value),
          chartRangeMax = max(Value),
          lineColor = "#4a7e8f",
          spotColor = "#aa0b3c",
          minSpotColor = "#999999",
          maxSpotColor = "#999999",
          fillColor = "#c3d9e0"
        ),
        `Box plot` = sparkline::spk_chr(
          Value,
          type = "box",
          chartRangeMin = min(Value),
          chartRangeMax = max(Value),
          target = last(Value),
          targetColor = "#aa0b3c",
          medianColor = "#7f7f7f",
          boxLineColor = "#ffffff",
          whiskerColor = "#7f7f7f",
          lineColor = "#4a7e8f",
          boxFillColor = "#c3d9e0"
        )
        
      )
    
  } else{
    
    data <- data.frame()
    
  }
  
  return(data)
  
}

plot_horizontal_bar_chars <- function(df, xlab, ylab){
  
  bar_colors <- if_else(df[[xlab]] < 0,
                        "#4a7e8f", "#cf395c"
  )
  
  max_change <- max(df[xlab])
  min_change <- min(df[xlab])
  
  
  df$xx <- df[[xlab]]
  df$yy <- df[[ylab]]
  
  p <- plotly::plot_ly() %>%
    add_trace(
      data = df,
      type = "bar",
      orientation = "h",
      x = ~ xx,
      y =  ~ yy,
      marker = list(color = bar_colors),
      opacity = 0.4,
      hovertext = df[xlab],
      hoverinfo = "text"
    ) 
  
  return(p)
  
  
}

add_annotation_plot <- function(p, df, xlab, value, change){
  
  
  
  df$xx <- df[[xlab]]
  df$vv <- df[[value]]
  df$cc <- df[[change]]
  change <- rlang::sym(paste0(change))
  
  
  
  for (i in 1:nrow(df)) {
    current_data <- df[i,]#dplyr:: arrange(df, !! change)[i,]
    p <- p %>%
      add_annotations(
        xref = "paper",
        x = 0.6,
        y = i-1,
        text =current_data[[value]],
        showarrow = FALSE,
        font = list(color = app_config$boe_palette[10], size = 10)
      ) %>%
      add_annotations(
        xref = "paper",
        x = 1,
        y = i-1,
        text = current_data[[change]],
        showarrow = FALSE,
        font = list(color = "black", size = 10)
      )
    
    
  }
  
  return(p)
  
  
  
}


lineChart <- function(data, xlab = "Date", ylab = "Value", fill = "Desc") {
  
  if (nrow(data) > 0 &  all(c(paste0(xlab), paste0(ylab), paste0(fill)) %in% colnames(data))) {
    
    ylab <- rlang::sym(paste0(ylab))
    xlab <- rlang::sym(paste0(xlab))
    fill <- rlang::sym(paste0(fill))
    
    p <- ggplot2::ggplot(as.data.frame(data)) +
      ggplot2::geom_line(ggplot2::aes(
        x = !!xlab,
        y = !!ylab,
        group = !!fill,
        colour = !!fill, 
      ), size = 0.5, lty = 1, alpha = 0.5) +
      ggplot2::geom_point(
        ggplot2::aes(
          x = !!xlab,
          y = !!ylab,
          group = !!fill,
          colour = !!fill
        ),
        show.legend = T,
        size = 0.1
      ) +
      # ggplot2::geom_vline(xintercept = 0, colour = boe_palette[1], lty = 3, lwd = 0.5) +
      # ggplot2::geom_hline(yintercept = 0) +
      ggplot2::scale_color_manual(values = rep(unname(boe_palette), 100)) +
      ggplot2::theme_minimal(base_size = 13) +
      ggplot2::scale_y_continuous(position = "right", breaks = scales:: pretty_breaks(n = 10))
    
  }  else{
    text = paste("No rows in DF")
    p = ggplot2::ggplot() +
      ggplot2::theme_bw() +
      ggplot2::theme(panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank())
    
  }
  
  return(p)
  
}

calculate_carry_jpy <- function(data_raw, currency, BENCHMARK_SECURITY, repo){
  
  tickers <- dplyr:: filter(data_raw, CRNCY == paste0(currency)) %>% pull(TICKER) %>% unique()
  
  carry_roll <-
    lapply(
      X = na.omit(tickers),
      match_maturities,
      data = data_raw,
      lag = 1
    ) %>% data.table::rbindlist()
  
  # calculate 12m roll 
  
  carry_roll <- carry_roll %>% 
    dplyr::group_by(TICKER) %>% 
    dplyr::mutate(ROLL_12M = (YLD_YTM_MID - MATCH_YTM)/as.numeric(MATCHED_DISTANCE)) %>% 
    dplyr::group_by(Security) %>% 
    dplyr::mutate(BUCKET = sapply(MATURITY, function(x)
      Buckets[order(abs(x - Buckets))][1]),
      Distance = abs(BUCKET - MATURITY)) %>% 
    dplyr::distinct(Security, TICKER, BUCKET,  .keep_all = TRUE) %>% 
    dplyr::mutate(
      CARRY_ROLL_12M = ROLL_12M + CARRY12M
    ) %>% 
    dplyr::arrange(TICKER, desc(CARRY_ROLL_12M)) %>% 
    dplyr::ungroup()
  
  return(carry_roll)
  
}


calculate_vol_adjusted_carry <- function(data, benchmark_1, benchmark_2, repo_rate = 0){
  
  roll_periods <- as.numeric(benchmark_1) -  as.numeric(benchmark_2)
  period_1 <- rlang:: sym(benchmark_1)
  period_2 <- rlang:: sym(benchmark_2)
  
  data %>%
    dplyr::select("SPREAD", "TICKER", "Bmk", "Date") %>%
    tidyr::pivot_wider(id_cols = c(TICKER, Date), names_from = Bmk, values_from = SPREAD) %>%
    dplyr::mutate(
      CARRY_ROLL = (!! period_1 - !! period_2) / roll_periods + (!! period_1 - as.numeric(repo_rate))/
        as.numeric(benchmark_1)
    ) %>%
    dplyr::group_by(TICKER) %>%
    dplyr::mutate(SMOOTH = rollapply(CARRY_ROLL, 5, mean, align = 'right', fill =
                                       NA)) %>%
    dplyr::mutate(VOL_ADJUSTED = (SMOOTH / rollapply(
      SMOOTH, 60, sd, align = 'right', fill = NA
    )) / 100) %>% 
    dplyr:: mutate(Benchmark = paste0(benchmark_1, " year"))
  
}

get_govie_data <- function(data, CURRENCY, grouping = "TICKER") {
  
  data <-
    data %>%
    dplyr::filter(CRNCY %in% CURRENCY, MATURITY_YEARS < 35) %>%
    dplyr::group_by(TICKER) %>%
    dplyr::filter(n() > 3)  %>%
    dplyr::mutate(marker = if_else(is.na(Done) |  Done == 0, 0, 3), 
                  ASSET_SWAP_SPD_MID = ASSET_SWAP_SPD_MID/100, 
                  YAS_ISPREAD_TO_GOVT = YAS_ISPREAD_TO_GOVT/100, 
                  ) %>%
    dplyr::ungroup()
  
  return(data)
  
  
}


plot_yield_curve <- function(data, xlab, ylab, fill, polynomial_order = 4){
  
  if (nrow(data) > 0 & all(c(paste0(xlab), paste0(ylab), paste0(fill)) %in% colnames(data))) {
    
    tickers <- unique(data[[fill]])
    
    p <- plotly::plot_ly(data = data)
    
    for ( i in seq_len(length(tickers)) ) {
      
      plot_data <- data %>%
        dplyr::filter(!!rlang::sym(fill) == tickers[i]) %>% 
        tidyr::drop_na(!!rlang::sym(xlab), !!rlang::sym(ylab))
      
      if(nrow(plot_data) >0){
        
        formula <- as.formula(paste0(ylab, " ~ ", "poly(", xlab, ", ", polynomial_order, ")"))
        fit <- lm(formula, data = plot_data)
        
        p <- p %>%
          add_trace(
            x = plot_data[[xlab]],
            y = plot_data[[ylab]],
            name = plot_data[[fill]][1],
            type = "scatter",
            mode = "markers",
            marker = list(
              color = app_config$boe_palette[i],
              type = 3,
              symbol = plot_data$marker
            ),
            hovertext =   paste(
              "ISSUE:",
              plot_data$ID_BB_SEC_NUM_DES,
              "<br> ASW:",
              plot_data$ASSET_SWAP_SPD_MID,
              "<br> MATURITY:",
              plot_data$MATURITY,
              "<br> DONE:",
              plot_data$Done,
              "<br> CARRY12M:",
              plot_data$CARRY12M,
              "<br> BBG SPREAD TO SPLINE:",
              plot_data$BB_SPRD_TO_SPLINE_EXPONENTIAL_RT
            )
          ) %>%
          add_trace(
            x = sort(plot_data[[xlab]]),
            y = fitted(fit)[order(plot_data[[xlab]])],
            name = paste0(plot_data[[fill]][1], " fitted"),
            type = "scatter",
            mode = "lines",
            line = list(color = app_config$boe_palette[i], type = 3, smoothing = 1.3, shape = "spline"),
            showlegend = T, visible = "legendonly"
          )
      }
      
      
    }
    
  } else {
    
    text <- paste("No rows in DF")
    p <- ggplot2::ggplot() +
      ggplot2::theme_bw() +
      ggplot2::theme(panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank())
    
  }
  
  return(p)
  
}

plot_yield_curve_done<- function(data, done, xlab, ylab, fill, polynomial_order = 4){
  
  if (nrow(data) > 0 & all(c(paste0(xlab), paste0(ylab), paste0(fill)) %in% colnames(data))) {
    
    tickers <- unique(data[[fill]])
    
    p <- plotly::plot_ly(data = data)
    
    for ( i in seq_len(length(tickers)) ) {
      
      plot_data <- data %>%
        dplyr::filter(!!rlang::sym(fill) == tickers[i]) %>% tidyr::drop_na(!! rlang::sym(xlab))
      
      done_data <- done %>%
        dplyr::filter(!!rlang::sym(fill) == tickers[i]) %>% tidyr::drop_na(!! rlang::sym(xlab))
      
      if(nrow(plot_data) > polynomial_order){
        
        formula <- as.formula(paste0(ylab, " ~ ", "poly(", xlab, ", ", polynomial_order, ")"))
        fit <- lm(formula, data = plot_data)
 
        
        p <- p %>%
          add_trace(
            x = plot_data[[xlab]],
            y = plot_data[[ylab]],
            name = plot_data[[fill]][1],
            type = "scatter",
            mode = "markers",
            marker = list(color = app_config$boe_palette[i], symbol= "cross"), 
            hovertext =   paste("Coupon:", plot_data$CPN,
                                "<br> Issue Date:", plot_data$ISSUE_DT, 
                                "<br> Maturity:", plot_data$MATURITY)
          ) %>%
          add_trace(
            x = done_data[[xlab]],
            y = done_data[[ylab]],
            name = done_data[[fill]][1],
            type = "scatter",
            mode = "markers",
            marker = list(color = app_config$boe_palette[i], symbol= "diamond"), 
            hovertext =   paste("Issue Date:", done_data$ISSUE_DT, 
                                "<br> Maturity:", done_data$MATURITY, 
                                "<br> DONE:", done_data$Done, 
                                "<br> CARRY12M:", done_data$CARRY12M)
          ) %>%
          add_trace(
            x = sort(plot_data[[xlab]]),
            y = fitted(fit)[order(plot_data[[xlab]])],
            type = "scatter",
            mode = "lines",
            line = list(color = app_config$boe_palette[i], type = 3),
            showlegend = F, visible = "legendonly"
          )
      }
      
      
    }
    
  } else {
    
    text <- paste("No rows in DF")
    p <- ggplot2::ggplot() +
      ggplot2::theme_bw() +
      ggplot2::theme(panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank())
    
  }
  
  return(p)
  
}


plot_swaps_curve <- function(data, xlab, ylab, fill, se = F, add_facets = F){
  
  if (nrow(data) > 0 & all(c(paste0(xlab), paste0(ylab), paste0(fill)) %in% colnames(data))) {
    
    ylab <- rlang::sym(paste0(ylab))
    xlab <- rlang::sym(paste0(xlab))
    fill <- rlang::sym(paste0(fill))
    
    p <- ggplot2::ggplot(as.data.frame(data)) +
      ggplot2::geom_point(ggplot2::aes(
        x = !!xlab,
        y = !!ylab,
        group = !!fill,
        colour = !!fill
      ), size = 1.25, alpha = 0.7, shape = 3) +
      #ggplot2::geom_hline(yintercept = 0) +
      ggplot2::geom_smooth(
        ggplot2::aes(
          x = !!xlab,
          y = !!ylab,
          group = !!fill,
          colour = !!fill
        ),
        size = 0.8, alpha = 0.6, 
        method = "lm",
        se = se, 
        formula = y ~ poly(x, 4, raw = TRUE)
      ) +
      ggplot2::scale_color_manual(values = rep(unname(boe_palette), 100)) +
      ggplot2::theme_minimal(base_size = 13) +
      ggplot2::scale_y_continuous(position = "right", breaks = scales:: pretty_breaks(n = 10), labels = function(x) paste0(x, "%")) +
      ggplot2::scale_x_date(date_breaks = "2 years", date_labels = "%Y")
    
    
    if (add_facets) {
      p <- p + ggplot2::facet_wrap( ~ TICKER)
    }
    
  }  else {
    
    text = paste("No rows in DF")
    p = ggplot2::ggplot() +
      ggplot2::theme_bw() +
      ggplot2::theme(panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank())
    
  }
  
  return(p)
  
}


plot_yield_curve_facets <- function(data, data_facet, xlab, ylab, fill, fill_facet){
  
  if (nrow(data) > 0 & all(c(paste0(xlab), paste0(ylab), paste0(fill)) %in% colnames(data))) {
    
    ylab <- rlang::sym(paste0(ylab))
    xlab <- rlang::sym(paste0(xlab))
    fill <- rlang::sym(paste0(fill))
    fill_facet <- rlang::sym(paste0(fill_facet))
    
    
    p <- ggplot2::ggplot(data = data) +
      ggplot2::geom_point(
        data = data_facet,
        ggplot2::aes(
          x = !!xlab,
          y = !!ylab,
          group = !!fill_facet,
          colour = !!fill_facet
        ),
        size = 1.25,
        alpha = 0.7,
        shape = 3
      ) +
      ggplot2::geom_point(
        ggplot2::aes(
          x = !!xlab,
          y = !!ylab,
          group = !!fill,
          colour = !!fill
        ),
        size = 1.25,
        alpha = 0.7,
        shape = 3
      )  +
      ggplot2::geom_smooth(
        data = data,
        ggplot2::aes(
          x = !!xlab,
          y = !!ylab,
          group = !!fill,
          colour = !!fill
        ),
        size = 0.4,
        alpha = 0.6,
        method = "lm",
        se = F,
        formula = y ~ poly(x, 4, raw = TRUE)
      ) +
      ggplot2::geom_smooth(
        data = data_facet,
        ggplot2::aes(
          x = !!xlab,
          y = !!ylab,
          group = !!fill_facet,
          colour = !!fill_facet
        ),
        size = 0.4,
        alpha = 0.6,
        method = "lm",
        se = F,
        formula = y ~ poly(x, 4, raw = TRUE)
      ) +
      ggplot2::scale_color_manual(values = rep(unname(boe_palette), 100)) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::scale_y_continuous(
        breaks = scales::pretty_breaks(n = 4),
        labels = function(x)
          paste0(x, "%")
      ) +
      ggplot2::scale_x_date(date_breaks = "3 years", date_labels = "%Y") + 
      ggplot2::geom_hline(yintercept = 0)
    
    
  }  else {
    
    text = paste("No rows in DF")
    p = ggplot2::ggplot() +
      ggplot2::theme_bw() +
      ggplot2::theme(panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank())
    
  }
  
  return(p)
  
  
  
}


barChart <- function(data, xlab, ylab, fill, position = "stack"){
  
  if(nrow(data) > 0 & all(c(paste0(xlab), paste0(ylab), paste0(fill)) %in% colnames(data))){
    
    ylab <- rlang:: sym(paste0(ylab))
    xlab <- rlang:: sym(paste0(xlab))
    fill <- rlang:: sym(paste0(fill))
    
    p <- 
      ggplot2:: ggplot(data, aes(!! xlab, !! ylab, group = !! fill, fill = !! fill)) +
      ggplot2:: geom_col(position= paste0(position), width=0.5, show.legend = T) +
      ggplot2:: labs(title= NULL) +
      ggplot2:: scale_fill_manual(values = unname(boe_palette)) +
      ggplot2:: theme_minimal()
    
  }  else{
    
    text = paste("No rows in DF")
    p = ggplot2:: ggplot() +
      ggplot2:: theme_bw() +
      ggplot2:: theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank())
    
  }
  
  return(p)
  
  
} 

ScatterChart <- function(data, xlab = "MATURITY", ylab = "CARRY_ROLL", fill = "Ticker") {
  
  if(nrow(data) > 0 & all(c(paste0(xlab), paste0(ylab), paste0(fill)) %in% colnames(data))){
    
    ylab <- rlang:: sym(paste0(ylab))
    xlab <- rlang:: sym(paste0(xlab))
    fill <- rlang:: sym(paste0(fill))
    
    p <- ggplot2:: ggplot(as.data.frame(data)) +
      ggplot2:: geom_point( ggplot2:: aes(x = !! xlab, y = !! ylab, group = !! fill, colour = !! fill), show.legend = T, size = 1.5) +
      ggplot2:: geom_hline(yintercept = 0) +
      ggplot2:: scale_color_manual(values = rep(unname(boe_palette), 100)) +
      #ggplot2:: scale_x_date(labels = scales:: date_format("%b-%y")) +
      ggplot2:: theme_minimal()
    
  }  else{
    
    text = paste("No rows in DF")
    p = ggplot2:: ggplot() +
      ggplot2:: theme_bw() +
      ggplot2:: theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank())
    
  }
  
  return(p)
  
}


# Charts
theme_TradeIdeas <- function(base_size = 10) {
  theme_classic( base_size = base_size) +
    theme(
      axis.line = element_blank(),
      axis.title.x = element_text(
        angle = 0,
        size = base_size,
        colour = "black"
      ),
      axis.text.x = element_text(
        size = base_size *  1,
        hjust = 0,
        colour = "grey50"
      ),
      axis.text.y = element_text(
        size = base_size *  1,
        hjust = 0,
        colour = "grey50"
      ),
      axis.title.y = element_text(
        angle = 90,
        size = base_size,
        colour = "black"
      ),
      axis.title = element_text(
        size = base_size,
        colour = "black"
      ),
      panel.grid.major.x = ggplot2::element_line(colour = "grey"), panel.grid.major.y = ggplot2::element_blank(),
      axis.line.x = ggplot2::element_line(colour = "grey"),
      axis.line.y = ggplot2::element_line(colour = "grey"),
      axis.ticks.x = ggplot2::element_line(colour = "grey"),
      axis.ticks.y = ggplot2::element_line(colour = "grey"),
      plot.caption = ggplot2::element_text(colour = "grey50", hjust = 1),
      panel.background = element_rect(fill = "white", colour = "grey50"),
      panel.grid.minor.y = element_blank(), legend.position = "bottom")
}  


elapsed_months <- function(end_date, start_date) {
  ed <- as.POSIXlt(end_date)
  sd <- as.POSIXlt(start_date)
  12 * (ed$year - sd$year) + (ed$mon - sd$mon)
}


read_securities <- function(market, database_filename) {
  
  conn <- FIRVr:: db_lm_connect(database_filename)
  security_list <- FIRVr:: db_lm_get_membership(conn, market, date = Sys.Date())
  FIRVr::db_lm_disconnect(conn)
  
  security_list$Security <- paste(security_list$ISIN, "Govt")
  security_list$TICKER <- stringr:: str_extract(pattern = "[[:alnum:]]{1,6}", security_list$SecurityDes)
  
  return(security_list)
}

read_benchmarks <- function(ticker_groups, database_filename, currency = "USD",  benchmark_years = c(1:10)) {
  
  conn <- FIRVr:: db_lm_connect(database_filename)
  security_list <- FIRVr:: db_lm_get_benchmarks(conn, ticker_groups, currency = currency, benchmark_years = benchmark_years)
  FIRVr::db_lm_disconnect(conn)
  
  security_list$Security <- paste(security_list$ISIN, "Govt")
  security_list$TICKER <- stringr:: str_extract(pattern = "[[:alnum:]]{1,6}", security_list$SecurityDes)
  security_list$Bmk <- paste(security_list$Bmk)
  security_list$currency <- paste(currency)
  
  return(security_list)
}


# openlink functions  -----------------------------------------------------



#
# R script to fetch booking information from OpenLink
#



#
# Convert vectors into the format expected by the API
# "'a', 'b', 'c'"
#

openlink_query_vector_to_api_string <- function(v) {
  paste(shQuote(v, type = "sh"), collapse=",")
}


#
# Wrapper for querying the RM view over the Openlink API
#

openlink_query_api <- function(instrument_types = NULL,
                               portfolios = NULL,
                               tran_statuses = NULL,
                               deal_number_min = NULL,
                               deal_number_max = NULL,
                               trade_date_min = NULL,
                               trade_date_max = NULL) {
  
  #
  # Add optional parameters to the query if relevant
  #
  
  query <- list()
  
  if ( ! is.null(instrument_types) ) {
    query <- append(
      query, 
      list(instrument_types = openlink_query_vector_to_api_string(instrument_types))
    )
  }
  
  if ( ! is.null(portfolios) ) {
    query <- append(
      query, 
      list(portfolios = openlink_query_vector_to_api_string(portfolios))
    )
  }
  
  if ( ! is.null(tran_statuses) ) {
    query <- append(
      query, 
      list(tran_statuses = openlink_query_vector_to_api_string(tran_statuses))
    )
  }
  
  if ( ! is.null(deal_number_min) ) {
    query <- append(
      query, 
      list(deal_number_min = deal_number_min)
    )
  }
  
  if ( ! is.null(deal_number_max) ) {
    query <- append(
      query, 
      list(deal_number_max = deal_number_max)
    )
  }
  
  if ( ! is.null(trade_date_min) ) {
    query <- append(
      query, 
      list(trade_date_min = trade_date_min)
    )
  }
  
  if ( ! is.null(trade_date_max) ) {
    query <- append(
      query, 
      list(trade_date_max = trade_date_max)
    )
  }
  
  #
  # Setup the query
  #
  
  url <- "https://olf-plw-api01/RmView"
  
  auth = authenticate(user = ":", password = "", type = "gssnegotiate")
  
  column_types <- cols(
    .default = col_character(),
    DEAL_TRACKING_NUM	= col_double(),
    TRAN_NUM	= col_double(),
    INS_NUM	= col_double(),
    POSITION = col_double(),
    COUNTER_POSITION = col_double(),
    PRICE = col_double(),
    PROCEEDS = col_double(),
    YIELD	= col_double(),
    FUTURES_NOTNL = col_double(),
    BROKER_ID	= col_double(),
    TRADE_DATE = col_date(),
    SETTLE_DATE = col_date(),
    MATURITY_DATE = col_date(),
    START_DATE = col_date()
  )
  
  
  result <- tryCatch({
    
    response <- POST(url,
                     config = auth,
                     use_proxy(""),
                     encode = "json",
                     body = query)
    
    response_str <- httr::content(response, type = "text/plain", encoding = "UTF-8")
    
    
    #
    # Convert the response to a data frame
    #
    
    read_csv(response_str, na = "<NULL>", col_types = column_types)
    
  },
  error = function (e) {
    flog.error("Failed to get data from Openlink API: %s", e)
    tibble()
  })
  
  result  
  
}




openlink_rm_bond_holdings <- function(
  bond_isins = "",
  portfolios = c(
    "ABA - AUD$ Bond Trading",
    "ABM - AUD$ Bond Mgt",
    "CBA - CAN$ Bond Trading",
    "CBM - CAN$ Bond Mgt",
    "UBM - US$ Bond Mgt",
    "UBA - US$ Bond Trading",
    "EBA - European Bond Trading",
    "EBM - European Bond Mgt",
    "YBA - Yen Bond Trading",
    "YBM - YEN Bond Mgt",
    "ANRL",
    "ENRL - \u20AC Net Reserves Liability",
    "UNRL",
    "CNRL",
    "YNRL",
    "ASL - \u00A3/AUD$ Swapping Liability",
    "ESL - \u00A3/\u20AC Swapping Liability",
    "YSL - \u00A3/Yen Swapping Liability", 
    "USL - \u00A3/$ Swapping Liability",
    "CSL - \u00A3/CAN$ Swapping Liability")
) {
  
  result <- openlink_query_api(portfolios = portfolios,
                               instrument_types = "GBND",
                               tran_statuses = "Validated") %>%
    select(
      DEAL_TRACKING_NUM,
      INS_NUM,
      INTERNAL_PORTFOLIO,
      REFERENCE,
      ISIN,
      PRICE,
      TRADE_DATE,
      SETTLE_DATE,
      MATURITY_DATE,
      CURRENCY,
      POSITION
    ) %>% 
    arrange(INS_NUM)
  
  if ( length(bond_isins) == 1 & bond_isins[1] == "" ) {
    return(result) 
  } else {
    return(result %>% filter(ISIN %in% bond_isins))
  }
  
}




openlink_rm_trades <- function(from_date = "2008-01-01",
                               to_date = Sys.Date(),
                               portfolios = c(
                                 "ABA - AUD$ Bond Trading", 
                                 "ABM - AUD$ Bond Mgt",
                                 "CBA - CAN$ Bond Trading",
                                 "CBM - CAN$ Bond Mgt",
                                 "UBM - US$ Bond Mgt",
                                 "UBA - US$ Bond Trading",
                                 "EBA - European Bond Trading",
                                 "EBM - European Bond Mgt",
                                 "YBA - Yen Bond Trading",
                                 "YBM - YEN Bond Mgt"
                               ),
                               ins_types = c(
                                 "GBND",      # Bonds
                                 "IBOND",     # Inflation Bonds
                                 "MM-G-Bill", # Bills
                                 "IRS",       # Swaps
                                 "DFUT",      # IR Futures
                                 "BONDFUT"    # Bond Futures
                               ) ){
  
  openlink_query_api(portfolios = portfolios,
                     instrument_types = ins_types,
                     trade_date_min = from_date,
                     trade_date_max = to_date) %>%
    select(
      DEAL_TRACKING_NUM,
      INS_NUM,
      INS_TYPE,
      INTERNAL_PORTFOLIO,
      REFERENCE,
      ISIN,
      PRICE,
      TRADE_DATE,
      SETTLE_DATE,
      MATURITY_DATE,
      CURRENCY,
      POSITION,
      INTERNAL_CONTACT,
      EXTERNAL_LENTITY,
      FUTURES_NOTNL,
      TICKER,
      TRAN_TYPE
    ) %>% 
    mutate(POSITION = ifelse(INS_TYPE == "IRS", POSITION/1e6, POSITION)) %>%
    # Already have a ticker field in the trading volumnes module (for BBG query)
    rename(INS_REFERENCE = TICKER)
  
}


#' Pulls all the data for any input instrument number from OL database
#' Should only be used if there is a database connection in place.
#' 
#' 
#' @param OL_tran_number The OL transaction number   
#'  
#' @return data frame with transaction data 
#'
#' @examples
#' openlink_deal_info(4842021)
#' 
#' 
#' Reuired packages:
#' dplyr, dbplyr, (OL connection function) 
#'  
#' 

openlink_deal_info <- function(OL_deal_number) {
  
  url <- "https://olf-plw-api01/SQL"
  
  auth = authenticate(user = ":", password = "", type = "gssnegotiate")
  
  column_types <- cols(
    .default = col_character(),
    DEAL_TRACKING_NUM	= col_double(),
    TRAN_NUM	= col_double(),
    INS_NUM	= col_double(),
    POSITION = col_double(),
    COUNTER_POSITION = col_double(),
    PRICE = col_double(),
    PROCEEDS = col_double(),
    YIELD	= col_double(),
    FUTURES_NOTNL = col_double(),
    BROKER_ID	= col_double(),
    TRADE_DATE = col_date(format = "%d-%b-%y"),
    SETTLE_DATE = col_date(format = "%d-%b-%y"),
    MATURITY_DATE = col_date(format = "%d-%b-%y"),
    START_DATE = col_date(format = "%d-%b-%y")
  )
  
  deal_nums <- paste0("(", paste(OL_deal_number, collapse = ","), ")")
  
  query = paste(
    "SELECT * FROM OLF_MASTER.USER_DATA_MODEL_DEAL_VIEW WHERE DEAL_TRACKING_NUM IN",
    deal_nums
  )
  
  result <- tryCatch({
    
    response <- POST(url,
                     config = auth,
                     use_proxy(""),
                     encode = "raw",
                     body = query)
    
    response_str <- httr::content(response, type = "text/plain", encoding = "UTF-8")
    
    #
    # Convert the response to a data frame
    #
    
    read_csv(response_str, na = "<NULL>", col_types = column_types)
    
  },
  error = function (e) {
    flog.error("Failed to get data from Openlink API: %s", e)
    tibble()
  })
  
  return(result)
  
}

# openlink_deal_info_LEGACY <- function(OL_deal_number){ 
#  
#   tryCatch({
#   
#     con <- openlink_connection()
#     
#     openlink <- dplyr::tbl(con,
#                            dplyr::sql("SELECT * FROM OLF_MASTER.USER_DATA_MODEL_DEAL_VIEW"))
#     
#     deal_result <- openlink %>%
#       dplyr::filter(DEAL_TRACKING_NUM %in% OL_deal_number) %>% 
#       dplyr::collect()
#     
#     tryCatch(
#       DBI::dbDisconnect(con),
#       error = function(e){}
#     )
#     
#     return(deal_result)
#     
#   }, error = function(e) {
#     futile.logger::flog.error(e)
#     futile.logger::flog.error("Failed to get Openlink data. Please try again.")
#     tryCatch(
#       DBI::dbDisconnect(con),
#       error = function(e){}
#     )
#     return(NULL)
#   })
#     
# }


#' Opens or refreshes the Openlink connection
#' Then returns a logical depending on whether it has failed or succeeded.
#' If succeeded also returns a connection object
#' 
#'  
#' @return dataframe of openlink trades  
#'
#' @examples openlink_repo_query()
#' 
#' 
#' Reuired packages:
#' dplyr, dbplyr,
#'  
#' 

openlink_repo_query <- function(
  portfolios = c(
    "CBA - CAN$ Bond Trading",
    "CBM - CAN$ Bond Mgt",
    "CNRL",
    "CSL - \u00A3/CAN$ Swapping Liability",
    "UBA - US$ Bond Trading",
    "UBM - US$ Bond Mgt",
    "UNRL", "USL - \u00A3/$ Swapping Liability",
    "EBA - European Bond Trading",
    "EBM - European Bond Mgt",
    "ENRL - \u20AC Net Reserves Liability",
    "ESL - \u00A3/\u20AC Swapping Liability",
    "YBA - Yen Bond Trading",
    "YBM - YEN Bond Mgt", "YNRL",
    "YSL - \u00A3/Yen Swapping Liability", 
    "ABA - AUD$ Bond Trading",
    "ABM - AUD$ Bond Mgt",
    "ANRL",
    "ASL - \u00A3/AUD$ Swapping Liability"
  ),
  ins_types = c("GBND", "MM-G-Bill", "MM-ECP", "BONDFUT", "IBOND"),
  tran_statuses = "Validated") {
  
  openlink_query_api(portfolios = portfolios,
                     instrument_types = ins_types,
                     tran_statuses = tran_statuses) %>%
    select(
      DEAL_TRACKING_NUM,
      TRAN_NUM,
      INS_NUM,
      TRAN_STATUS,
      TRAN_TYPE,         
      ASSET_TYPE,
      TOOLSET,
      INS_TYPE,
      BASE_INS_TYPE,
      BUY_SELL,          
      INTERNAL_LENTITY,
      INTERNAL_BUNIT,
      INTERNAL_PORTFOLIO,
      EXTERNAL_LENTITY,
      EXTERNAL_BUNIT,
      EXTERNAL_PORTFOLIO,
      TRADE_DATE,
      SETTLE_DATE,
      MATURITY_DATE,
      CURRENCY,
      POSITION,
      PRICE,
      PROCEEDS,
      YIELD,
      REFERENCE,
      ISIN,
      INTERNAL_CONTACT,
      FUTURES_NOTNL,
      TICKER,
      SHORT
    ) %>%
    mutate(POSITION = ifelse(INS_TYPE == "IRS", POSITION/1e6, POSITION)) %>%
    arrange(INS_NUM)
  
}

# repo functions  ---------------------------------------------------------

t_plus_date <- function(t_plus_days = 1L, ref_day = Sys.Date()){
  
  if(t_plus_days < 0) flog.error("t_plus_days(): t_plus less than zero")
  if(t_plus_days != round(t_plus_days, digits = 0)) flog.error("t_plus_days(): t_plus is not an integer")
  if(!(is.Date(ref_day))) flog.error("t_plus_days(): ref_date is not a date")
  if(wday(ref_day) == 7 | wday(ref_day) == 1) flog.warn("t_plus_days(): ref_date is a weekend")
  
  t_plus_days_mod <- t_plus_days %% 5 
  add_days <- ((t_plus_days - t_plus_days_mod) / 5) * 7
  
  wday_t_plus <- wday(ref_day + t_plus_days_mod) %% 7
  if (wday_t_plus < t_plus_days_mod) add_days <- add_days + 2
  
  t_plus_date <- ref_day + add_days + t_plus_days_mod
  
  if(wday(t_plus_date) == 7 | wday(t_plus_date) == 1)  flog.error("t_plus_days(): output a weekend")
  
  t_plus_date
}

#' Function to find the position by portfolio by bond ticker
#' This is only for Bonds, T bills, ECP  and futures  
#'
#' packages required: dplyr
#'
#' @param position_data full OL data frame of validated trades
#' @param t_plus_day the date at which to calculate the position
#'
#' @return portfolio holding positions in tidy table on the date specified by t_plus_day
#'


position_tplus <- function(position_data, t_plus_day = Sys.Date()){
  
  # Sort relevant trades
  posit_dfr <- position_data %>% 
    filter(INS_TYPE %in% c("GBND", "MM-G-Bill", "MM-ECP", "BONDFUT", "IBOND", "DFUT")) %>% #not including repo, cash depos or swaps
    filter(SETTLE_DATE <= t_plus_day, MATURITY_DATE > t_plus_day) %>%   # filter for unmatured instruments that have settled on day t_plus_day 
    select(c(TICKER, INS_NUM, ISIN, INTERNAL_PORTFOLIO, TRAN_TYPE, CURRENCY, POSITION)) %>%      #select columns wanted from the 30
    group_by(TICKER, INS_NUM, ISIN, CURRENCY, INTERNAL_PORTFOLIO, TRAN_TYPE) %>%         
    summarise(Position = sum(POSITION)) %>%              # sum across all positions to get net position by portfolio 
    ungroup()
  
  posit_dfr 
}

#' Generates the flow by bond on every day
#' 
#'    This includes repo collateral, bond transaction and maturity flows for each bond, bill, ECP and bond future
#'    If there is no null values, only vlaues if a flow occurs, (completing with zeros creates a ~100mb dat frame given the time horizon)
#'    Starts from first trade in 1999 until the maturity of teh last cond owned 
#'
#' packages required: dplyr
#'
#' @param trade_data full OL data frame of validated trades
#'
#' @return portfolio holding positions in tidy table on the date specified by t_plus_day
#'


delta_date_tbl <- function(trade_data){
  
  #filter to get inflows by using settle date
  dfr_set <- trade_data %>%
    filter(INS_TYPE %in% c("GBND", "MM-G-Bill", "MM-ECP", "BONDFUT", "IBOND", "DFUT")) %>%
    select(TRAN_NUM, TICKER, INS_NUM, ISIN, CURRENCY, SETTLE_DATE, POSITION) %>%
    mutate(DELTA = POSITION, DATE = SETTLE_DATE) %>%
    select(-c(SETTLE_DATE, POSITION))
  
  #filter to get outflows by maturity date, then bind the inflows on to the bottom
  dfr_mat <- trade_data %>%
    filter(INS_TYPE %in% c("GBND", "MM-G-Bill", "MM-ECP", "BONDFUT", "IBOND", "DFUT")) %>%
    select(TRAN_NUM, TICKER, INS_NUM, ISIN, CURRENCY, MATURITY_DATE, POSITION) %>%
    mutate(DELTA = -POSITION, DATE = MATURITY_DATE) %>% 
    select(-c(MATURITY_DATE, POSITION)) %>%
    bind_rows(dfr_set) %>%    #adding inflows
    group_by(TICKER, INS_NUM, ISIN, CURRENCY, DATE) %>%
    summarise(DELTA = sum(DELTA)) %>%    # condense multiple entries for same bond on smae day
    ungroup() %>%
    rename(`Change on day` = DELTA) %>%
    filter(DATE >= "2000-01-01")  #transactions pre-2000 offset exactly
  
  dfr_mat
}


#' 
#' Function to find the short position in bonds on any specific day
#' 
#' packages required: dplyr
#' 
#' @param delta_table the table of flows generated by delta_date_tbl function 
#' @param tplus the date for which shorts may need to be covered, Date format
#' 
#' @return data frame of shorts to cover on tplus
#' 

shorts_to_cover <- function(delta_table, tplus){
  
  #get net positions on tplus, want only short ones  
  short_net <- delta_table %>%
    filter(DATE <= tplus) %>% 
    group_by(TICKER, INS_NUM, ISIN, CURRENCY) %>%
    summarise(POSITION = sum(`Change on day`)) %>%
    ungroup() %>%
    filter(POSITION < -0.00001)
  
  #find bonds with outflows on day, and join table of short positions to get shorts that need covering  
  shorts <- delta_table %>%
    filter(DATE == tplus, `Change on day` < 0) %>% 
    inner_join(short_net, by = c("TICKER", "INS_NUM", "ISIN", "CURRENCY"))
  
  shorts
  
}

#' Format short cover datatables 
#'
#' @param dfr
#'
#' @return formatted datatable
#' 
format_shortcover_datatable <- function(dfr) {
  
  # add totals to first visible column
  x <- dfr
  
  # identify numeric rows for formatting
  final_col <- rep(FALSE, ncol(x))
  final_col[[length(final_col)]] <- TRUE
  
  datatable(
    x,
    rownames = FALSE,
    options = list(
      pageLength = nrow(x),
      paging = FALSE, 
      ordering = FALSE,
      lengthChange = FALSE,
      searching = FALSE,
      info = FALSE
    )
  ) %>% 
    formatStyle(final_col, color = styleInterval(0, c("red", "black"))) %>% 
    formatStyle(final_col, backgroundColor = "palegreen")
}

#' Format holdings datatable 
#'
#' @param dfr
#'
#' @return formatted datatable
#' 
format_holdings_datatable <- function(dfr) {
  
  # add totals to first visible column
  x <- dfr
  
  
  # identify cols for formatting
  isn <- unlist(sapply(x, is.numeric))
  totals <- colnames(x) == "Total"
  
  datatable(
    x,
    rownames = FALSE,
    options = list(
      pageLength = nrow(x),
      paging = FALSE,
      info = FALSE,
      search = list(regex = TRUE)
    )
  ) %>% 
    formatStyle(isn, color = styleInterval(0, c("red", "black"))) %>% 
    formatStyle(totals, fontWeight = "bold") 
}

# Note that this function will fetch more rows than the new API version

get_holdings_data <- function(CURRENCIES = c("USD", "CAD", "AUD", "EUR", "JPY")){
  
  OL_trade_data <-  openlink_repo_query()
  
  #table breaking out the holdings for each portfolio
  allhold <-
    position_tplus(OL_trade_data, t_plus_day = t_plus_date(as.numeric(1))) %>%
    dplyr::filter(CURRENCY %in% CURRENCIES, abs(Position) >  0.1) %>%
    dplyr::mutate(
      PORTFOLIO_TYPE = case_when(
        INTERNAL_PORTFOLIO %in% c(
          "ABM - AUD$ Bond Mgt",
          "CBM - CAN$ Bond Mgt",
          "UBM - US$ Bond Mgt",
          "EBM - European Bond Mgt",
          "YBM - YEN Bond Mgt"
        ) ~ "Management",
        INTERNAL_PORTFOLIO %in% c(
          "ABA - AUD$ Bond Trading",
          "CBA - CAN$ Bond Trading",
          "UBA - US$ Bond Trading",
          "EBA - European Bond Trading",
          "YBA - Yen Bond Trading"
        ) ~ "Active",
        INTERNAL_PORTFOLIO %in% c(
          "ANRL",
          "ENRL - \u20AC Net Reserves Liability",
          "UNRL",
          "CNRL",
          "YNRL"
        ) ~ "Unhedged",
        TRUE ~ "Hedged"
      )
    ) %>%
    dplyr::select(-c(INTERNAL_PORTFOLIO)) %>%
    tidyr::spread(PORTFOLIO_TYPE, Position) 
  
  #Putting zeros in for NAs in portfolios with no holdings and add empty col if no holdings in that portfolio 
  if(!("Unhedged" %in% colnames(allhold))){allhold <- allhold %>% mutate(Unhedged = 0)}
  if(!("Hedged" %in% colnames(allhold))){allhold <- allhold %>% mutate(Hedged = 0)}
  if(!("Management" %in% colnames(allhold))){allhold <- allhold %>% mutate(Management = 0)}
  if(!("Active" %in% colnames(allhold))){allhold <- allhold %>% mutate(Active = 0)}
  allhold <- replace_na(allhold, replace = list(Active = 0, Management = 0, Hedged = 0, Unhedged = 0))
  
  
  # filter to get a table of repo collateral only 
  dfr_repo <- allhold %>%
    dplyr::filter(TRAN_TYPE == "Repo Coll") %>%
    dplyr::mutate(Repo = Active + Management + Hedged + Unhedged) %>%
    dplyr::select(c(TICKER, INS_NUM, ISIN, Repo))
  
  #filter table for holdings then join repo collateral table and format 
  holdings_report <- allhold %>%
    dplyr::filter(TRAN_TYPE == "Trading") %>%
    dplyr::mutate(Done = Active + Management + Hedged + Unhedged) %>%
    dplyr::full_join(dfr_repo, by = c("TICKER", "INS_NUM", "ISIN")) %>%
    tidyr::replace_na(replace = list(Active = 0, Management = 0, Hedged = 0, Unhedged = 0, Repo = 0, Done = 0)) %>%
    dplyr::mutate(Total = Done + Repo) %>%
    dplyr::select(-c(TRAN_TYPE, CURRENCY)) %>% 
    dplyr::mutate_if(is.numeric, round, digits = 3) %>%
    dplyr::mutate(instruments =  gsub(pattern = "_", " ", TICKER), 
                  issuer_coupon = gsub('.{6}$', '', instruments), 
                  date = stringr:: str_sub(instruments, - 6, - 1), 
                  day = stringr:: str_sub(date, 1, 2),
                  month = stringr:: str_sub(date, 3, 4), 
                  year = stringr:: str_sub(date, 5, 6), 
                  date = paste0(month, "/", day, "/", year)) %>% 
    dplyr::select(-day, -month, -year, -TICKER) %>% 
    dplyr::mutate(security = paste0(issuer_coupon, date))
  
  
  return(holdings_report)
}

