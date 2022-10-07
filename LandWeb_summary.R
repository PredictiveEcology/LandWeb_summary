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
  reqdPkgs = list("animation", "data.table", "future", "ggplot2", "googledrive", "purrr", "qs", "raster", "sp",
                  "achubaty/amc@development",
                  "PredictiveEcology/LandR@development",
                  "PredictiveEcology/LandWebUtils@development",
                  "PredictiveEcology/map (>= 0.0.4)",
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
                    paste("number of replicates/runs per study area.")),
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
    defineParameter("version", "integer", 3L, 2L, 3L,
                    "LandWeb model version (2 for runs using vegetation parameter forcings, else 3)."),
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
    expectsInput("ml", "map",
                 desc = "map list object from LandWeb_preamble"),
    expectsInput("sppColorVect", "character",
                 desc = paste("A named vector of colors to use for plotting.",
                              "The names must be in `sim$sppEquiv[[P(sim)$sppEquivCol]]`,",
                              "and should also contain a color for 'Mixed'")),
    expectsInput("sppEquiv", "data.table", NA, NA, NA,
                 desc = "table of species equivalencies. See `LandR::sppEquivalencies_CA`.")
  ),
  outputObjects = bindrows(
    createsOutput("ml", "map", "map list object"),
  )
))

## event types
#   - type `init` is required for initialization

doEvent.LandWeb_summary = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      sim <- Init(sim)

      if (FALSE) {
        sim <- scheduleEvent(sim, start(sim), "LandWeb_summary", "animation") ## TODO: skip for now -- use future!!!!
      }

      sim <- scheduleEvent(sim, start(sim), "LandWeb_summary", "postprocess")

      if (isTRUE(P(sim)$upload)) {
        sim <- scheduleEvent(sim, end(sim), "LandWeb_summary", "upload", .last())
      }
    },
    animation = {
      ## create vtm and tsf stacks for animation

      if (isFALSE(isRstudio())) {
        Require("future")
        options("future.availableCores.custom" = function() { min(getOption("Ncpus"), 4) })
        future::plan("multiprocess") ## future::plan(future.callr::callr)
      }

      tsfTimeSeries <- gsub(".*vegTypeMap.*", NA, mod$allouts) %>%
        grep(paste(P(sim)$timeSeriesTimes, collapse = "|"), ., value = TRUE)
      vtmTimeSeries <- gsub(".*TimeSinceFire.*", NA, mod$allouts) %>%
        grep(paste(P(sim)$timeSeriesTimes, collapse = "|"), ., value = TRUE)

      if (length(tsfTimeSeries)) {
        sA <- studyArea(sim$ml, 2)
        gifName <- file.path(normPath(outputPath(sim)), "animation_tsf.gif")
        future({
          tsfStack <- raster::stack(tsfTimeSeries)# %>% writeRaster(file.path(outputPath(sim), "stack_tsf.tif"))
          animation::saveGIF(ani.height = 1200, ani.width = 1200, interval = 1.0,
                             movie.name = gifName, expr = {
                               brks <- c(0, 1, 40, 80, 120, 1000)
                               cols <- RColorBrewer::brewer.pal(5, "RdYlGn")
                               for (i in seq(numLayers(tsfStack))) {
                                 plot(raster::mask(tsfStack[[i]], sA), breaks = brks, col = cols)
                               }
                             })
        })
        rm(tsfStack)

        #if (length(vtmTimeSeries)) {
        #  vtmStack <- raster::stack(vtmTimeSeries)# %>% writeRaster(file.path(outputPath(sim), "stack_vtm.tif"))
        #  gifName <- file.path(normPath(outputPath(sim)), "animation_vtm.gif")
        #  animation::saveGIF(ani.height = 1200, ani.width = 1200, interval = 1.0,
        #                     movie.name = gifName, expr = {
        #                       for (i in seq(numLayers(vtmStack)))
        #                         plot(mask(vtmStack[[i]], studyArea(sim$ml, 2))) # TODO: this animation isn't great!
        #  })
        #  rm(vtmStack)
        #}
      }
    },
    postprocess = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event

      sim <- postprocessLandWeb(sim)

      # ! ----- STOP EDITING ----- ! #
    },
    upload = {
      # ! ----- EDIT BELOW ----- ! #
      browser()
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

  # ! ----- STOP EDITING ----- ! #

  return(invisible(sim))
}

.inputObjects <- function(sim) {
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")

  # ! ----- EDIT BELOW ----- ! #

  tmp <- loadSimList(file.path(outputPath(sim), paste0("simOutPreamble_", P(sim)$.studyAreaName, ".qs")))

  if (!suppliedElsewhere("ml", sim)) {
    sim$ml <- tmp$ml ## TODO: can't load ml objects from qs file !!
  }

  if (!suppliedElsewhere("sppEquiv", sim)) {
    sim$sppEquiv <- tmp$sppEquiv
  }

  rm(tmp)

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}
