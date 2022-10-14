#
# Script to manage the updating and distribution of the legacy swap report
#

source("\\\\istdba/BuildArchive/ShinyApps/EnvironmentScript/EnvironmentScript_Latest/LOCAL/Artifactory/environment.R")
# set parameters -----------------------------------------------------

source(file.path("./config/config.R"))

# boe checkpoint
checkpoint_date <- "2020-08-01"
message(paste0("checkpoint_date: ", checkpoint_date))
boeCheckpoint(checkpoint_date, scanForPackages = F)

DASHBOARD_ROOT <- readLines("N:/Offdata/RM/DASHBOARD_ROOT")
flog.info("Dashboard root set to %s", DASHBOARD_ROOT)

#"\\\\markets/DATA/DATA/Offdata/RM/_R code repository/MarketMonitor-Live" 

installMissingPackages(checkpoint_date = checkpoint_date)

library(dplyr)
library(DT)
library(futile.logger)
library(janitor)
library(readr)
library(sparkline)
library(taskscheduleR)

clean_tmpfiles_mod <- function() {
  message("Calling clean_tmpfiles_mod()")
}

assignInNamespace("clean_tmpfiles", clean_tmpfiles_mod, ns = "rmarkdown")

FIRVr_str <- "library(FIRVr, lib.loc = 'N:/Offdata/RM/_R code repository/RMPackages/_R4.0')"
eval(parse(text=FIRVr_str))

flog.info("--------------- Started ---------------")

if(as.numeric(R.Version()$major) < 4) flog.error("R version needs to be 4.0 or greater to  run market monitor!")

#
# Set the pandoc path for users without pandoc in their path
#

Sys.setenv(RSTUDIO_PANDOC = "C:/Program Files/RStudio/bin/pandoc")

#
# Set the working directory
#

file_name <- "ActiveMarketMonitor"

rmarkdown::render(
  input = file.path("report/ActiveMarketMonitor.Rmd"),
  output_file = file.path("../report/ActiveMarketMonitor.html")
  )


# Copy the files to the archive and to the dashboard
flog.info("Copying the report ... ")

file.copy(
  file.path("report/ActiveMarketMonitor.html"),
  file.path(DASHBOARD_ROOT, "/RMDashboard/www/reports/ActiveMarketMonitor.html"),
  overwrite = TRUE
)

flog.info(Sys.time())
flog.info("Finished")
