


installMissingPackages <- function(checkpoint_date = "2020-08-01") {
  #Install these packages before use
  list.of.packages <-
    c(
      "tidyverse",
      "data.table",
      "DT",
      "readr",
      "RSQLite",
      "jrvFinance",
      "sparkline",
      "formattable",
      "Rblpapi",
      "zoo",
      "flexdashboard",
      "janitor",
      "plotly",
      "ggplot2",
      "lubridate",
      "magrittr",
      "timeDate"#"DescTools",
    )
  
  new.packages <-
    list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
  
  if (length(new.packages))
    boeInstall.packages(pkgs = new.packages, snapshotDate = checkpoint_date)

}


app_config <- new.env()

app_config$boe_palette <- rep(c(
  "#4a7e8f", # teal
  "#cf395c", # bright pink
  "#a9c9d3", # light blue
  "#b25395", # pink
  "#3b855f", # green
  "#2f4f5b", # very dark teal
  "#b65e19", # orange
  "#0f7cbf", # blue
  "#555555"  # dark grey
), 3)

app_config$boe_markers_scatter <-
  c(rep("circle",  length(app_config$boe_palette)),
    rep("square",  length(app_config$boe_palette)),
    rep("diamond", length(app_config$boe_palette)))

app_config$cache_invalidation_seconds <- 5 * 60
app_config$default_height <- "600px"
app_config$font_style <- list(family = c("Cabin"))
app_config$headroom <- "60px"
app_config$headroom_0.75 <- "30px"


plotly_std_config <- function(x) {
  # Set a NULL element ID to avoid messages of the form:
  #  Ignoring explicitly provided widget ID "6c0e221658"; Shiny doesn't use them
  x$elementId <- NULL
  x
}


plotly_std_title <- function(x, title,
                             x_coord = 0.0,
                             y_coord = 1.1,
                             font = list(size = 10)) {
  add_annotations(x,
                  xref = "paper",
                  yref = "paper",
                  x = x_coord,
                  y = y_coord,
                  showarrow = FALSE,
                  text = title,
                  font = font
                  )
}


plotly_std_style <- function(x) {
  style(x,
    hoverlabel = list(
      bgcolor = "white",
      font = app_config$font_style
    )
  )
}


plotly_top_change_chart <- function(x) {

  axis_template <- list(
    showgrid = TRUE,
    zeroline = TRUE,
    showline = FALSE,
    showticklabels = TRUE,
    ticks = "",
    title = "",
    mirror = "all"
  )

  layout(
    x,
    xaxis = axis_template,
    yaxis = axis_template,
    margin = list(l = 30, r = 30, b = 15, t = 30, pad = 2)
  )

}


plotly_slope_chart <- function(x) {

  axis_template <- list(
    showgrid = FALSE,
    zeroline = FALSE,
    showline = FALSE,
    showticklabels = FALSE,
    ticks = "",
    title = "",
    mirror = "all"
  )

  layout(
    x,
    xaxis = axis_template,
    yaxis = axis_template,
    showlegend = FALSE,
    margin = list(l = 30, r = 30, b = 15, t = 30, pad = 4)
  )

}


plotly_heatmap_chart <- function(x) {

  axis_template <- list(
    showgrid = FALSE,
    zeroline = FALSE,
    showline = FALSE,
    showticklabels = FALSE,
    ticks = "",
    title = "",
    mirror = "all"
  )

  layout(
    x,
    xaxis = axis_template,
    yaxis = axis_template,
    margin = list(l = 30, r = 30, b = 25, t = 30, pad = 4)
  )

}


plotly_central_bank_pricing_chart <- function(x) {

  axis_template <- list(
    showgrid = FALSE,
    zeroline = FALSE,
    showline = FALSE,
    showticklabels = FALSE,
    ticks = "",
    title = "",
    mirror = "all"
  )

  layout(
    x,
    xaxis = list(title = ""),
    yaxis = axis_template,
    bargap = 0.8,
    margin = list(l = 30, r = 30, b = 15, t = 30, pad = 4)
  )

}



plotly_layout <- function(x){
  
  yaxis <- list(
    automargin = TRUE,
    titlefont = list(size=12)
  )
  xaxis <- list(
    automargin = TRUE,
    titlefont = list(size=12)
  )
  layout(x , 
         legend = list(orientation = "h",  x = 0.2, y = -0.2),
         autosize = T, 
         yaxis = yaxis,
         xaxis = xaxis, 
         margin = list(
           l = 50,
           r = 50,
           b = 50,
           t = 30)) 
  
}
  