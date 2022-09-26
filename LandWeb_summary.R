defineModule(sim, list(
  name = "LandWeb_summary",
  description = "LandWeb simulation post-processing and summary creation",
  keywords = "LandWeb",
  authors = c(
    person(c("Eliot", "J", "B"), "McIntire", email = "eliot.mcintire@nrcan-rncan.gc.ca", role = c("aut", "cre")),
    person(c("Alex", "M."), "Chubaty", email = "achubaty@for-cast.ca", role = c("aut"))
  ),
  childModules = character(0),
  version = list(LandWeb_summary = "0.0.0.9000"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.md", "LandWeb_summary.Rmd"), ## README generated from module Rmd
  reqdPkgs = list("data.table", "future", "ggplot2", "googldrive", "purrr", "raster", "sp",
                  "PredictiveEcology/LandR@development",
                  "PredictiveEcology/LandWebUtils@development",
                  "PredictiveEcology/map (>= 0.0.3.9004)",
                  "PredictiveEcology/reproducible@development (>= 1.2.10)",
                  "PredictiveEcology/SpaDES.core@development (>= 1.1.0.9000)"),
  parameters = bindrows(
    defineParameter("ageClasses", "character", LandWebUtils:::.ageClasses, NA, NA,
                    "descriptions/labels for age classes (seral stages)"),
    defineParameter("ageClassCutOffs", "integer", LandWebUtils:::.ageClassCutOffs, NA, NA,
                    "defines the age boundaries between age classes"),
    defineParameter("ageClassMaxAge", "integer", 400L, NA, NA,
                    "maximum possible age"),
    defineParameter("reps", "integer", 1L:10L, 1, NA,
                    paste("number of replicates/runs per study area and climate scenario.",
                          "NOTE: `mclapply` is used internally, so you should set",
                          "`options(mc.cores = nReps)` to run in parallel.")), ## TODO: use .useParallel mechanism
    defineParameter("simOutputPath", "character", outputPath(sim), NA, NA,
                    "Directory specifying the location of the simulation outputs."),
    defineParameter("sppEquivCol", "character", "EN_generic_short", NA, NA,
                    "The column in `sim$sppEquiv` data.table to use as a naming convention"),
    defineParameter("summaryInterval", "integer", 100L, NA, NA,
                    "simulation time interval at which to take 'snapshots' used for summary analyses"),
    defineParameter("summaryPeriod", "integer", c(700L, 1000L), NA, NA,
                    "lower and upper end of the range of simulation times used for summary analyses"),
    defineParameter("timeSeriesTimes", "integer", 601L:650L, NA, NA,
                    "timesteps to use to build timeseries rasters showing leading cover change over time"),
    defineParameter("upload", "logical", FALSE, NA, NA,
                    "if TRUE, uses the `googledrive` package to upload figures."),
    defineParameter("uploadTo", "character", NA, NA, NA,
                    paste("if `upload = TRUE`, a Google Drive folder id corresponding to `.studyAreaName`.")),
    defineParameter("vegLeadingProportion", "numeric", 0.8, 0.0, 1.0,
                    "a number that defines whether a species is leading for a given pixel"),
    defineParameter(".plots", "character", "screen", NA, NA,
                    "Used by Plots function, which can be optionally used here"),
    defineParameter(".plotInitialTime", "numeric", start(sim), NA, NA,
                    "Describes the simulation time at which the first plot event should occur."),
    defineParameter(".plotInterval", "numeric", NA, NA, NA,
                    "Describes the simulation time interval between plot events."),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA,
                    "Describes the simulation time at which the first save event should occur."),
    defineParameter(".saveInterval", "numeric", NA, NA, NA,
                    "This describes the simulation time interval between save events."),
    defineParameter(".studyAreaName", "character", NA, NA, NA,
                    "Human-readable name for the study area used - e.g., a hash of the study",
                          "area obtained using `reproducible::studyAreaName()`"),
    defineParameter(".useCache", "logical", FALSE, NA, NA,
                    "Should caching of events or module be used?"),
    defineParameter(".useParallel", "logical", getOption("map.useParallel", FALSE), NA, NA,
                    desc = paste("Logical. If `TRUE`, and there is more than one calculation to do at any stage,",
                                 "it will create and use a parallel cluster via `makeOptimalCluster()`."))
  ),
  inputObjects = bindrows(
    expectsInput("rasterToMatch", "RasterLayer", "DESCRIPTION NEEDED", sourceURL = NA),
    expectsInput("sppEquiv", "data.table",
                 desc = "table of species equivalencies. See `LandR::sppEquivalencies_CA`.")
  ),
  outputObjects = bindrows(
    #createsOutput("objectName", "objectClass", "output object description", ...),
    createsOutput(objectName = NA, objectClass = NA, desc = NA)
  )
))

## event types
#   - type `init` is required for initialization

doEvent.LandWeb_summary = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      ### check for more detailed object dependencies:
      ### (use `checkObject` or similar)

      # do stuff for this event
      sim <- Init(sim)

      # schedule future event(s)
      sim <- scheduleEvent(sim, P(sim)$.plotInitialTime, "LandWeb_summary", "plot")
      sim <- scheduleEvent(sim, P(sim)$.saveInitialTime, "LandWeb_summary", "save")
    },
    plot = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event

      plotFun(sim) # example of a plotting function
      # schedule future event(s)

      # e.g.,
      #sim <- scheduleEvent(sim, time(sim) + P(sim)$.plotInterval, "LandWeb_summary", "plot")

      # ! ----- STOP EDITING ----- ! #
    },
    save = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event

      # e.g., call your custom functions/methods here
      # you can define your own methods below this `doEvent` function

      # schedule future event(s)

      # e.g.,
      # sim <- scheduleEvent(sim, time(sim) + P(sim)$.saveInterval, "LandWeb_summary", "save")

      # ! ----- STOP EDITING ----- ! #
    },
    event1 = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event

      ## TODO: see e.g. LandR::plotLeadingSpecies()
      files2upload <- aFunThatReturnsFileNamesOfPlotsEtc(
            studyAreaName = P(sim)$.studyAreaName,
            Nreps = P(sim)$reps,
            outputDir = P(sim)$simOutputPath,
            sppEquiv = sim$sppEquiv,
            leadingPercentage = P(sim)$vegLeadingProportion,
            rasterToMatch = sim$rasterToMatch
      )
      files2upload <- unlist(files2upload, recursive = TRUE) ## TODO

      mod$files2upload <- c(mod$files2upload, files2upload)

      # ! ----- STOP EDITING ----- ! #
    },
    event2 = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event

      # e.g., call your custom functions/methods here
      # you can define your own methods below this `doEvent` function

      # schedule future event(s)

      # e.g.,
      # sim <- scheduleEvent(sim, time(sim) + increment, "LandWeb_summary", "templateEvent")

      # ! ----- STOP EDITING ----- ! #
    },
    upload = {
      # ! ----- EDIT BELOW ----- ! #
      mod$files2upload <- set_names(mod$files2upload, basename(mod$files2upload))

      gid <- as_id(sim$uploadTo[[P(sim)$studyAreaName]])
      prevUploaded <- drive_ls(gid)
      toUpload <- mod$files2upload[!(basename(mod$files2upload) %in% prevUploaded$name)]
      uploaded <- map(toUpload, ~ drive_upload(.x, path = gid))
      # ! ----- STOP EDITING ----- ! #
    },
    warning(paste("Undefined event type: \'", current(sim)[1, "eventType", with = FALSE],
                  "\' in module \'", current(sim)[1, "moduleName", with = FALSE], "\'", sep = ""))
  )
  return(invisible(sim))
}

## event functions
#   - keep event functions short and clean, modularize by calling subroutines from section below.

### template initialization
Init <- function(sim) {
  # # ! ----- EDIT BELOW ----- ! #

  ## TODO: inventory all files to ensure correct dir structure? compare against expected files?
  #filesUserHas <- fs::dir_ls(P(sim)$simOutputPath, recurse = TRUE, type = "file", glob = "*.qs")

  ## TODO: update below for LandWeb
  filesUserExpects <- rbindlist(lapply(P(sim)$studyAreaNames, function(studyAreaName) {
    rbindlist(lapply(P(sim)$climateScenarios, function(climateScenario) {
      rbindlist(lapply(P(sim)$reps, function(rep) {
        runName <- sprintf("%s_%s_run%02d", studyAreaName, climateScenario, as.integer(rep))
        f <- file.path(P(sim)$simOutputPath, runName, paste0(runName, ".qs"))

        data.table(file = f, exists = file.exists(f))
      }))
    }))
  }))

  if (!all(filesUserExpects$exists)) {
    missing <- filesUserExpects[exists == FALSE, ]$file
    stop("Some simulation files missing:\n", paste(missing, collapse = "\n"))
  }

  # ! ----- STOP EDITING ----- ! #

  return(invisible(sim))
}

### template for save events
Save <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # do stuff for this event
  sim <- saveFiles(sim)

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

### template for plot events
plotFun <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  # do stuff for this event
  sampleData <- data.frame("TheSample" = sample(1:10, replace = TRUE))
  Plots(sampleData, fn = ggplotFn)

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}


.inputObjects <- function(sim) {
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")

  # ! ----- EDIT BELOW ----- ! #

  tmp <- loadSimList(file.path("outputs", runNamePostProcessing,
                               paste0("simOutPreamble_", studyAreaName, ".qs"))) ## TODO: update for landweb

  if (!suppliedElsewhere("rasterToMatch", sim)) {
    sim$rasterToMatch <- tmp$rasterToMatchReporting
  }

  if (!suppliedElsewhere("sppEquiv", sim)) {
    sim$sppEquiv <- tmp$sppEquiv
  }

  rm(tmp)

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}
