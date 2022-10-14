# environment.R script for environment LOCAL.
# Functions prefixed with a dot '.' are invisible.

# Paths to Artifactory
.artifactory_url = "https://binarycentral/artifactory/"
.boecran_remote_repo = "boe-cran-remote-repo/"
.snapshot_remote_repo = "boe-cransnapshots-remote-repo/"
.snapshot_from_date = as.Date("2014-09-08")
.artifactory_local_repo_url = paste0(.artifactory_url, "boe-cran-local-repo")


#' Gets the IPv4 addresses of the current machine.
#'
#' @return A list of IP addresses.
#'
.getCurrentMachineIpAddresses <- function(){
  ipconfig_text <- system("ipconfig", intern=TRUE)
  ipaddress_lines <- ipconfig_text[grep("IPv4", ipconfig_text)]
  gsub(".*? ([[:digit:]])", "\\1", ipaddress_lines)
}


#' Checks to see if an IP address is in the Data Zone network.
#'
#' @param ipAddress IP address.
#'
.isDataZoneIpAddress <- function(ipAddress){
  any(grepl("^10.66*", ipAddress))
}


#' #' Determines whether at least one IP address within a vector is an IP address of an HPC Server in the OSF network.
#'
#' @param ipAddress IP addresses
#'
#' @return TRUE if at least one IP address within a vector is an IP address of an HPC Server in the OSF network. FALSE in other case.
#'
.isOsfHpcServerIpAddress <- function(ipAddress){
  hpcServerIpAddresses = c(
    "10.80.17.160",
    "10.80.17.161",
    "10.80.17.162",
    "10.80.17.163",
    "10.80.18.100",
    "10.80.18.101",
    "10.80.18.102",
    "10.80.18.103",
    "10.80.24.55",
    "10.80.24.56",
    "10.80.24.57",
    "10.80.24.58"
  )
  return(length(intersect(hpcServerIpAddresses,ipAddress)) > 0)
}


# Get current machine IP addresses.
.currentMachineIpAddresses = .getCurrentMachineIpAddresses()


# boeCheckpoint.boeCheckpointRootPath is use below and Default path to contain the ".checkpoint" folder, which includes the folder structure used as target libraries for boeCheckpoint.

# Assign global and internal variables depending on the environment.
options(boeCheckpoint.boeCranRootPath = paste0(.artifactory_url, .boecran_remote_repo))
options(boeCheckpoint.boeCheckpointRootPath = "~/.boeCheckpoint")
.isUsingSharedLibrary = FALSE

if(.isDataZoneIpAddress(.currentMachineIpAddresses)){
  .environmentName = "DATA_ZONE"

} else if (.isOsfHpcServerIpAddress(.currentMachineIpAddresses)) {
  .environmentName = "HPC_SERVER_OSF"
  .isUsingSharedLibrary = TRUE
  options(boeCheckpoint.boeCheckpointRootPath = paste0(.artifactory_url, .boecran_remote_repo))
  
} else {
  .environmentName = "LOCAL"
}


# Look-up table with which version of shiny and dependencies to use for an R version.
# This needs to be updated for new Bank R versions and as a more strategic solution to be read from a central CSV file.
.boeShinyPackageSnapshot = as.data.frame(cbind(
  c("3.2.0", "3.2.3", "3.3.3", "3.5.1"),
  c("2017-08-07", "2017-08-07", "2017-08-07", "2018-10-10")
))
names(.boeShinyPackageSnapshot) = c("rVersion","snapshotDate")


#' Gets the local boeCheckpoint folder for a snapshot date. This is the "library" location.
#'
#' @param snapshotDate The snapshot date.
#'
#' @return Path to the specific snapshot date boeCheckpoint folder.
#'
.getBoeCheckpointFolder = function(snapshotDate) {
  
  return(
    normalizePath(
      file.path(
        getOption("boeCheckpoint.boeCheckpointRootPath"),
        ".checkpoint",
        snapshotDate,
        "lib",
        R.version$platform,
        paste0(R.version$major,".",R.version$minor)
      ),
      mustWork = FALSE
    )
  )
  
}


#' Prepares the R environment to have the boeCheckpoint ready for using it with Shiny Apps.
#' This function must be run on a fresh R session and without other R sessions opened.
#' This function installs the shiny package and its dependencies to the default library location,
#' so these packages become available when RStudio initializes the "shiny execution" and before boeCheckpoint is even called.
#' The installed packages are always the same for the same version of R.
#'
boePrepareShinyEnvironment = function() {
  
  # Get the snapshot to use for shiny packages for the current R version.
  snapshotDate = .boeShinyPackageSnapshot[.boeShinyPackageSnapshot$rVersion == paste0(R.version$major,".",R.version$minor),]$snapshotDate
  
  if (length(snapshotDate) == 0) {
    stop("Unable to find the snapshot for shiny packages. Please contact the Support team.")
  } else {
    # Call the main function to intall packages.
    .boeInstallPackages("shiny", snapshotDate, .libPaths()[1], TRUE, c())
  }
  
}

## check if snapshot exists in Artifactory
.snapshotExists = function(snapshotDate) {
  
  snapshots = seq(from = .snapshot_from_date, by = "day", to = Sys.Date())
  
  return (snapshotDate >= snapshots[1] && snapshotDate <= snapshots[length(snapshots)])
}

#' Configures the R environment to use the packages of a certain snapshot date.
#' This function scans the files of the current project looking for the usage of packages and installs the missing ones from the specified snapshot date.
#'
#' @param snapshotDate Date of snapshot to use in YYYY-MM-DD format, e.g. "2014-09-17".
#' @param scanForPackages If TRUE, scans for packages in project folder. If FALSE, skips the scanning process.
#' A use case for scanForPackages = FALSE is to skip the scanning and installation process, e.g. in production environments with a large number of R scripts in the project.
#' Only set scanForPackages = FALSE if you are certain that all package dependencies are already installed.
#' @param toIgnorePackages List of packages that will be ignored if they are missing from Artifactory, and not to get an error if packages are missing.
#'
boeCheckpoint <- function(snapshotDate, project = getwd(), scanForPackages = TRUE, toIgnorePackages = c()) {
  
  # Force not to check for pacakges if using shared library.
  if (.isUsingSharedLibrary) {
    scanForPackages = FALSE
  }
  
  # Check if the snapshot is in Artifactory
  if (scanForPackages && !.snapshotExists(snapshotDate)){
    stop(paste0(
      "\n",
      "Error accessing snapshot ", snapshotDate,".",
      "\n",
      "Run boeGetValidSnapshots() to get the list of available snapshots.",
      "\n",
      "If you are working off-line, and you previously installed all required packages,",
      "\n",
      "you can run the boeCheckpoint function with parameter scanForPackages=FALSE not to connect to Artifactory."
    ))
  }
  
  # If the boeCheckpoint folder does not exist.
  boeCheckpointFolder = .getBoeCheckpointFolder(snapshotDate)
  if (!dir.exists(boeCheckpointFolder))
  {
    # If using a the shared library throw an error.
    if (.isUsingSharedLibrary) {
      stop(paste0(
        "\n",
        "Error accessing boeCheckpoint for date ", snapshotDate,"."
      ))
      # If not using a the shared library create the boeCheckpoint folder.
    } else {
      dir.create(boeCheckpointFolder, recursive = TRUE)
      #print ('created boecheckpointFolder')
      #boeCheckpointFolder
    }
  }
  
  # Add as first library location the boeCheckpoint folder.
  .libPaths(c(boeCheckpointFolder,.libPaths()))
  
  # If packages need to be scanned.
  if (scanForPackages) {
    
    # Scan packages in current working path.    
    scannedPackages = .scanForPackages(project = project)$pkgs
    
    # Call the main function to install packages.
    .boeInstallPackages(scannedPackages, snapshotDate, .getBoeCheckpointFolder(snapshotDate), FALSE, toIgnorePackages)
  }
  
} 


#' Installs packages from a source Artifactory snapshot to a target library location.
#'
#' @param pkg Packages to install.
#' @param sourceSnapshotDate The Artifactory snapshot date to get the packages from.
#' @param targetLibLocation The path of the library location to install packages to.
#' @param isOverwritePackages Determines whether to overwrite existing packages or not.
#' @param toIgnorePackages List of packages that will be ignored if they are missing from Artifactory, and not to get an error if packages are missing.
#'
.boeInstallPackages = function(pkg, sourceSnapshotDate, targetLibLocation, isOverwritePackages, toIgnorePackages) {
  
  # Filter to get just non-based packages. As base ones should already be installed.
  packagesToInstall = setdiff(pkg, .basePkgs())

  # Capture current session's repos option so we can set it back 
  repos <- getOption("repos")
  
  # Point session's repos to artifactory
  r <- list("boe-cransnapshots-remote-repo" = paste0(.artifactory_url, .snapshot_remote_repo, sourceSnapshotDate)
            ,"boe-cran-local-repo" = .artifactory_local_repo_url)
  options(repos = r)
  
  # Get all available packages in Artifactory snapshot.
  allAvailablePackages = available.packages(type="win.binary")
  
  # Dertermine which pacakges are not in Artifactory.
  nonExistingPackages = setdiff(packagesToInstall, allAvailablePackages)
  packagesToInstall = setdiff(packagesToInstall, nonExistingPackages)
  
  # If there are existing packages to install.
  if (length(packagesToInstall) > 0) {
    
    # Get the union of the required packages and their dependencies.
    packagesToInstall = .pkgDep(packagesToInstall, availPkgs = allAvailablePackages, suggests = FALSE, type = "win.binary")
    
    # If the shiny package is already loaded we assume RStudio is launching a Shiny App and the function boePrepareShinyEnvironment() was called at a very first time.
    # Because all shiny packages are already installed we do not install them again which also may cause an error trying to install a loaded library.
    if ("package:shiny" %in% search()) {
      shinyPackageDepencencies = .pkgDep("shiny", availPkgs = allAvailablePackages, suggests = FALSE, type = "win.binary")
      packagesToInstall = setdiff(packagesToInstall, shinyPackageDepencencies)
    }
    
    # If not to overwrite packates, remove the already installed ones.
    if (!isOverwritePackages) {
      packagesToInstall = setdiff(packagesToInstall,installed.packages(lib.loc = targetLibLocation))  
    }
    
    # Complete the list of non-existing packages after getting the dependencies.
    nonExistingPackages = union(nonExistingPackages, setdiff(packagesToInstall,allAvailablePackages))
  }
  
  # Packages that are not available in Artifactory will raise an error, unless they are specified in parameter toIgnorePackages.
  nonExistingPackages = setdiff(nonExistingPackages,toIgnorePackages)
  
  if (length(nonExistingPackages) > 0) {
    # reset repos
    options(repos = repos)
    stop(paste(
      "\n",
      paste0("The following packages are not in artifactory for snapshot ", sourceSnapshotDate, ":"), paste(nonExistingPackages, collapse = ", "),
      "\n",
      "Please try on a newer snapshot (use boeGetValidSnapshots() function to get a list of available snapshots).",
      "\n",
      "While the packages get available, you may use the \"checkpoint\" mechanism that connects to Internet repositories. Refer to script:",
      "\n",
      "\\\\istdba\\BuildArchive\\ShinyApps\\BoeCranManagement\\BoeCranManagement_Latest\\boeCheckpointArtifactoryTests.R"
    )
    )
    # If all required packages are available they are installed to the boeCheckpoint library location.
  } else {
    if (length(packagesToInstall) > 0) {
      # Loop as a workaround for incompatibility with antivirus.
      # https://stackoverflow.com/questions/5700505/windows-7-update-packages-problem-unable-to-move-temporary-installation
      for(requiredPackage in packagesToInstall){
        
        install.packages(requiredPackage, available = allAvailablePackages, repos = c(boeGetBoeCranUrl(sourceSnapshotDate), .artifactory_local_repo_url), lib = targetLibLocation, dependencies = FALSE, verbose = TRUE, INSTALL_opts = "--pkglock", lock = TRUE)
        
      }
      
    }
    
    # reset repos
    options(repos = repos)
  }
  
}


#' Returns the URL of a specific snapshot in Artifactory.
#'
#' @param snapshotDate Optional snapshot date in format YYYY-MM-DD. If missing, defaults to the latest snapshot.
#'
#' @return
#' @export
#'
#' @examples
#' #### update it for artifactory
boeGetBoeCranUrl <- function(snapshotDate = boeGetLatestValidSnapshot()) {
  return(paste0(.artifactory_url, .snapshot_remote_repo, snapshotDate)) 
}


#' Installs packages from Artifactory as opposed to CRAN from a given snapshot.
#' 
#' @param pkgs Character vector of the names of packages to be downloaded from BOECRAN.
#' @param snapshotDate Optional snapshot date in format YYYY-MM-DD. If missing, defaults to the latest snapshot.
#' 
boeInstall.packages <- function(pkgs, snapshotDate = boeGetLatestValidSnapshot()){
  
  install.packages(pkgs, repos = c(boeGetBoeCranUrl(snapshotDate),.artifactory_local_repo_url))
}


#' Gets the available snapshot dates in Artifactory
#'
#' @return A vector of the available snapshot dates in Artifactory
#'

boeGetValidSnapshots <- function() {
  
  return (seq(from = .snapshot_from_date, by = "day", to = Sys.Date())) 
} 

#' Gets the latest available snapshot date in Artifactory
#'
#' @return The latest available snapshot date in Artifactory
#'
boeGetLatestValidSnapshot <- function() {
  sort(boeGetValidSnapshots(), decreasing = TRUE)[1]
}


#' Retruns a list of available packages in Artifactory with their corresponding version.
#'
#' @param snapshotDate Snapshot date of interest. Defaults to latest available. 
#' @param pattern A pattern to filter by
#' @return List of available packages in Artifactory with their corresponding version.
#' 
boeGetPackageList <- function(pattern=NULL, snapshotDate= boeGetLatestValidSnapshot()){
  boeCranUrl = boeGetBoeCranUrl(snapshotDate)
  allAvailablePackages = available.packages(type="win.binary", repos = boeCranUrl)
  if(!is.null(pattern)){
    pattern = tolower(pattern)
    filteredPackages = subset(allAvailablePackages,grepl(pattern = pattern,x = allAvailablePackages[,1]))
    return(filteredPackages[,1:2])
  }
  allAvailablePackages[,1:2]
}


#' Returns the name of the SQL Server driver to use in connection strings to SQL Server databases.
#'
#' @return The name of the SQL Server driver in the current environment.
#'
boeGetSqlServerDriver <- function() {
  return("SQL Server")
} 


#' Returns the name of the current environment.
#'
#' @return The name of the current environment.
#'
boeGetEnvironmentName <- function() {
  return(.environmentName)
}



#' Returns the particular format to use when accessing UNC paths from the current environment.
#'
#' @param uncPath A character vector with the path to access in the UNC format.
#'		Note that a backslash needs to be escaped with a backslash.
#'		The following examples of UNC paths are recognized:
#'			\\\\ServerName\\FolderName\\MyFile.csv
#'			//ServerName/FolderName/MyFile.csv
#'
#' @return The name of the current environment.
#'
boeGetPathFromUnc <- function(uncPath) {
  if(missing(uncPath)) {
    stop('UNC path is missing!')
  } else {
    # In LOCAL environment (Windows) no conversion is required for the UNC path.
    return(uncPath)
  }
}

#' Returns the path where the executables files of pandoc are installed in the current environment.
#' This is used to assign this path to the RSTUDIO_PANDOC session environment variable if it is not already set.
#' Pandoc https://pandoc.org/ is a dependency for some Markdown functionalities.
#' It is part of the RStudio installation, which automatically sets RSTUDIO_PANDOC to the relevant folder on a new RStudio session, 
#' but when using with R command line executables RSTUDIO_PANDOC has to be defined explicitly.
#'
#' @return The path to the executable files of pandoc.
#'
boeGetPandocPath <- function() {
  if (.environmentName == "HPC_SERVER_OSF") {
    return(normalizePath("C:/Program Files/RStudio/bin/pandoc", mustWork = FALSE))
  } else {
    stop("Function not implemented.")
  }
}


#
#
# Functions from checkpoint package v0.4.2
#
#



#
# From "checkpoint_internals.R "
#

# Simple wrapper around `message()`, that only displays a message if verbose=TRUE
.mssg <- function(verbose, ...) if(verbose) message(...) 


#
# From "setSnapshot.R"
#

.tryUrl <- function(url){
  timeout <- getOption("timeout")
  on.exit(options(timeout = timeout))
  options(timeout = 5)
  con <- suppressWarnings(tryCatch(url(url), error = function(e)e))
  msg <- paste0(
    "Invalid value for mranRootUrl.\n", 
    "Ensure you use the correct http://,  https:// or file:/// prefix."
  )
  if(inherits(con, "error")) {
    stop(msg, call. = FALSE)
  }
  con
}


#
# From "setSnapshot.R"
#

#' Read list of available snapshot dates from MRAN.
#' 
#' Returns vector of available dates from MRAN or local MRAN repository.
#' 
#' @param mranRootUrl MRAN root. This can be a URL, e.g. `https://mran.microsoft.com/snapshot/` or the path to a local MRAN repository, e.g.`file:///local/path`
#' 
#' @export
#' @return Character vector with dates of valid snapshots
#' @family checkpoint functions
.getValidSnapshots <- function(mranRootUrl = mranUrl()){
  con <- .tryUrl(mranRootUrl)
  on.exit(close(con))
  text <- if (inherits(con, "file")) {
    dir(summary(con)$description)
  } else {
    suppressWarnings(tryCatch(readLines(con, warn = TRUE), error = function(e) e))
  }
  if (inherits(text, "error")) {
    stop(sprintf("Unable to download from MRAN: %s", 
                 text$message))
  }
  ptn <- "\\d{4}-\\d{2}-\\d{2}"
  idx <- grep(ptn, text)
  gsub(sprintf("^<a href=.*?>(%s).*?</a>.*$", ptn), 
       "\\1", text[idx])
}


#
# From "scanRepoPackages.R"
#


#' Scans a project (or folder) for references to packages.
#' 
#' @inheritParams checkpoint
#' @return A list with two elements:
#' * pkgs: a character vector with the names of identified packages
#' * error: a character vector with information about files that could not be parsed
#' @export
.scanForPackages <- function(project = getwd(), verbose = TRUE, 
                             use.knitr = FALSE, 
                             auto.install.knitr = FALSE, 
                             scan.rnw.with.knitr = FALSE
){
  # detect all package dependencies for a project
  dir <- normalizePath(project, winslash='/', mustWork=FALSE)
  pattern <- if(!use.knitr) "\\.[rR]$|\\.[rR]nw$" else
    "\\.[rR]$|\\.[rR]nw$|\\.[rR]md$|\\.[rR]pres$"
  
  if(scan.rnw.with.knitr){
    ext_r <- c("R")
    ext_k <- c("Rmd", "Rpres", "Rhmtl", "Rnw") # knitr / rmarkdown extensions
  } else {
    ext_r <- c("R", "Rnw")
    ext_k <- c("Rmd", "Rpres", "Rhmtl") # knitr / rmarkdown extensions
  }
  
  makePtn <- function(x)sprintf("\\.(%s)$", paste(c(x, tolower(x)), collapse="|"))
  
  files_r <- list.files(dir, pattern = makePtn(ext_r), 
                        ignore.case = TRUE, recursive = TRUE)
  files_k <- list.files(dir, pattern = makePtn(ext_k), 
                        ignore.case = TRUE, recursive = TRUE)
  
  R_files <- files_r
  
  if(length(files_k) > 0) {
    if(use.knitr) {
      if(!knitr.is.installed()) {
        .mssg(verbose, "The knitr package is not available and Rmarkdown files will not be parsed")
      } else {
        R_files <- c(files_r, files_k)
      }
    } else {
      .mssg(verbose, "rmarkdown files found and will not be parsed. Set use.knitr = TRUE")
    }
  }
  
  if(length(R_files) == 0){
    list(pkgs = character(), error = character())
  } else {
    if(interactive()){
      z <- .lapplyProgressBar(R_files, .deps_by_ext, dir=dir, verbose=verbose,
                              scan.rnw.with.knitr = scan.rnw.with.knitr)
    } else {
      z <- lapply(R_files, .deps_by_ext, dir=dir, verbose=verbose, 
                  scan.rnw.with.knitr = scan.rnw.with.knitr)
    }
    
    pkgs <- sort(unique(do.call(c, lapply(z, "[[", "pkgs"))))
    if(length(files_k) > 0 && auto.install.knitr) {
      pkgs <- unique(c(pkgs, "knitr"))
    }
    error <- sort(unique(do.call(c, lapply(z, "[[", "error"))))
    error <- gsub(sprintf("%s[//|\\]*", dir), "", error)
    list(pkgs = pkgs, error = error)
  }
  
}


# Wraps lapply() in a progress bar, if the session is interactive and the list contains more than 10 elements
.lapplyProgressBar <- function(X, FUN, ...){
  if(interactive() && length(X) >= 10){
    env <- environment()
    N <- length(X)
    counter <- 0
    pb <- txtProgressBar(min = 0, max = N, style = 3)
    on.exit(close(pb))
    
    wrapper <- function(...){
      curVal <- get("counter", envir = env)
      assign("counter", curVal + 1, envir = env)
      setTxtProgressBar(get("pb", envir = env), curVal + 1)
      FUN(...)
    }
    lapply(X, wrapper, ...)
  } else {
    lapply(X, FUN, ...)
  }
}

.getFileExtension <- function(filename)tolower(gsub(".*\\.", "", filename))



# ad-hoc dispatch based on the file extension
.deps_by_ext <- function(file, dir, verbose = TRUE, scan.rnw.with.knitr = FALSE) {
  file <- file.path(dir, file)
  fileext <- .getFileExtension(file)
  switch(fileext,
         r = .deps.R(file, verbose = verbose),
         rnw = if(scan.rnw.with.knitr){
           .deps.Rmd(file, verbose = verbose)
         } else {
           .deps.Rnw(file, verbose = verbose)
         },
         rmd = .deps.Rmd(file, verbose = verbose),
         rpres = .deps.Rpres(file, verbose = verbose),
         txt = deps.txt(file, verbose = verbose),
         stop("Unrecognized file type '", file, "'")
  )
}

.deps.Rmd <- .deps.Rpres <- function(file, verbose=TRUE) {
  tempfile <- tempfile(fileext = ".Rmd")
  showErrors <- getOption("show.error.messages")
  options("show.error.messages" = FALSE)
  on.exit({unlink(tempfile); options("show.error.messages" = showErrors)})
  if(!knitr.is.installed()) stop("knitr is not installed")
  p <- try(
    suppressWarnings(
      knitr::knit(file, output = tempfile, tangle = TRUE, quiet = TRUE)
    ),
    silent = TRUE
  )
  
  if(inherits(p, "error")) {
    return(list(pkgs=character(), error=file))
  }
  
  p <- .deps.R(tempfile)
  if(length(p[["error"]]) != 0 ) {
    p[["error"]] <- file
  }
  p
}

.deps.Rnw <- function(file, verbose=TRUE) {
  tempfile <- tempfile(fileext = ".Rnw")
  showErrors <- getOption("show.error.messages")
  options("show.error.messages" = FALSE)
  on.exit({unlink(tempfile); options("show.error.messages" = showErrors)})
  p <- try(
    Stangle(file, output = tempfile, quiet = TRUE),
    silent = TRUE
  )
  if(inherits(p, "error")) {
    return(list(pkgs=character(), error=file))
  }
  
  p <- .deps.R(tempfile)
  if(length(p[["error"]]) != 0 ) {
    p[["error"]] <- file
  }
  p
}

.deps.R <- deps.txt <- function(file, verbose=TRUE) {
  if (!file.exists(file)) {
    .mssg(verbose, "No file at path '", file, "'.")
    return(list(pkgs=character(), error=file))
  }
  
  # build a list of package dependencies to return
  pkgs <- character()
  
  # parse file and examine expressions
  p <- tryCatch({
    exprs <- suppressWarnings(parse(file, n = -1L))
    for (i in seq_along(exprs))
      pkgs <- append(pkgs, .expressionDependencies(exprs[[i]]))
  }, error = function(e) e
  )
  if(inherits(p, "error")) {
    list(pkgs=character(), error=file)
  } else {
    list(pkgs=unique(pkgs), error=character())
  }
}

.expressionDependencies <- function(e) {
  # base case
  if (is.atomic(e) || is.name(e)) return()
  
  # recursive case: expression (= list of calls)
  if (is.expression(e)) {
    return(unlist(lapply(e, .expressionDependencies)))
  }
  
  # base case: a call
  fname <- as.character(e[[1L]])
  # a refclass method call, so return
  # if (length(fname) > 1) return()
  
  if (length(fname) == 1) {
    
    # base case: call to library/require
    if (fname %in% c("library", "require")) {
      mc <- match.call(get(fname, baseenv()), e)
      if (is.null(mc$package)) return(NULL)
      if (isTRUE(mc$character.only)) return(NULL)
      
      return(as.character(mc$package))
    }
    
    # base case: methods functions
    if (fname %in% c("setClass", "setRefClass", "setMethod", "setGeneric")) {
      return("methods")
    }
    
  } else {
    
    # base case: call to :: or :::
    if (fname[1] %in% c("::", ":::")) (
      return(as.character(fname[2]))
    )
  }
  
  # recursive case: all other calls
  children <- lapply(as.list(e[-1]), .expressionDependencies)
  unique(unlist(children))
}





#
#
# Functions from miniCRAN package v0.2.10
#
#



#
# From "pkgDep.R"
#


#' Returns names of base packages.
#'
#' Retrieves names of installed packages by calling [utils::installed.packages()] and returning only those packages where `Priority == "base"`.
#'
#' @export
#' @family dependency functions
#' 
#' @seealso [pkgDep()]
.basePkgs <- function()names(which(installed.packages()[, "Priority"] == "base"))



#' Retrieves package dependencies.
#'
#' Performs recursive retrieve for `Depends`, `Imports` and `LinkLibrary`. Performs non-recursive retrieve for `Suggests`.
#'
#'
#' @param pkg Character vector of packages.
#' 
#' @param availPkgs Vector of available packages.  Defaults to reading this list from CRAN, using [available.packages()]
#' 
#' @param repos URL(s) of the 'contrib' sections of the repositories, e.g. `"http://cran.us.r-project.org"`. Passed to [available.packages()]
#' 
#' @param type Possible values are (currently) "source", "mac.binary" and "win.binary": the binary types can be listed and downloaded but not installed on other platforms.  Passed to [download.packages()].
#' 
#' @param depends If TRUE, retrieves Depends, Imports and LinkingTo dependencies (non-recursively)
#' @param suggests If TRUE, retrieves Suggests dependencies (non-recursively)
#' @param enhances If TRUE, retrieves Enhances dependencies (non-recursively)
#' @param quiet If TRUE, suppresses warnings
#' 
#' @param includeBasePkgs If TRUE, include base R packages in results
#' @param Rversion Version of R. Can be specified as a character string with the two digit R version, e.g. "3.1".  Defaults to [R.version]
#' @param ... Other arguments passed to [available.packages()]
#'
#' @export
#' @family dependency functions
#'
#' @example /inst/examples/example_pkgDep.R
#' 
.pkgDep <- function(pkg, availPkgs, repos = getOption("repos"), type = "source",
                    depends = TRUE, suggests = TRUE, enhances = FALSE,
                    includeBasePkgs = FALSE, Rversion = R.version, quiet = FALSE, ...) {
  if (!depends & !suggests & !enhances) {
    warning("Returning nothing, since depends, suggests and enhances are all FALSE")
    return(character(0))
  }
  
  if (missing(pkg) || !is.character(pkg)) {
    stop("pkg should be a character vector with package names")
  }
  if (missing(availPkgs)) {
    if (!is.null(names(repos)) & repos["CRAN"] == "@CRAN@") {
      repos <- MRAN()
    }
    if (is.na(type)) type <- "source"
    availPkgs <- pkgAvail(repos = repos, type = type, Rversion = Rversion,
                          quiet = quiet, ...)
  }
  if (nrow(availPkgs) == 0) {
    stop("Unable to retrieve available packages from CRAN")
  }
  
  pkgInAvail <- pkg %in% availPkgs[, "Package"]
  if (sum(pkgInAvail) == 0 ) stop("No valid packages in pkg")
  if (sum(pkgInAvail) < length(pkg)) {
    warning("Package not recognized: ", paste(pkg[!pkgInAvail], collapse = ", "))
  }
  
  n_req <- pkg[pkgInAvail]
  n_req_all <- pkg
  
  # Suggests
  if (suggests) {
    p_sug <- tools::package_dependencies(n_req, availPkgs,
                                         which = "Suggests", recursive = FALSE)
    n_sug <- unique(unname(unlist(p_sug)))
    n_req_all <- c(n_req_all, n_sug)
  } else {
    p_sug <- NA
  }
  
  # Enhances
  if (enhances) {
    p_enh <- tools::package_dependencies(n_req, availPkgs,
                                         which = "Enhances", recursive = FALSE)
    n_enh <- unique(unname(unlist(p_enh)))
    n_req_all <- c(n_req_all, n_enh)
  } else {
    p_enh <- NA
  }
  
  # Depends, Imports and LinkingTo
  p_dep <- tools::package_dependencies(n_req_all, availPkgs,
                                       which = c("Depends", "Imports", "LinkingTo"),
                                       recursive = TRUE)
  n_dep <- unique(unname(unlist(p_dep)))
  
  p_all <- p_dep
  n_all <- unique(c(n_dep, n_req_all))
  n_all <- c(n_req, setdiff(n_all, n_req))
  
  ret <- n_all
  if(!includeBasePkgs) ret <- ret[!ret %in% .basePkgs()]
  attr(ret, "pkgs") <- list(
    n_req = n_req,
    n_all = n_all,
    p_dep = p_dep,
    p_sug = p_sug,
    p_enh = p_enh,
    p_all = p_all
  )
  class(ret) <- c("pkgDep", "character")
  ret
}

installMissingPackages<-function(){
  
  #Install these packages before use
  list.of.packages <- c("openNLP", "gofastr", "topicmodels", "quanteda", "futile.logger", "fuzzyjoin", "openNLP", "snow", "XML", "readtext", 
                        "RODBC", "snow", "reshape2", "data.table", "tidyverse", "glue", "readr", "rstudioapi", "zoo", "lubridate", "anytime"
  )
  
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  
  if(length(new.packages)) boeInstall.packages(pkgs = new.packages, snapshotDate = "2020-01-30")
  
  #run this code to reinstall the package
  # remove.packages(paste0("rlang"))
  # boeInstall.packages("rlang", snapshotDate = "2020-01-30")
}