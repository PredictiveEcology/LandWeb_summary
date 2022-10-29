defineModule(sim, list(
  name = "LandWeb_summary",
  description = paste("LandWeb simulation post-processing and summary creation.",
                      "Produces boxplots and histograms of simulated NRV ranges,",
                      "overlaid by indicator of current forest conditions."),
  keywords = c("LandWeb", "NRV"),
  authors = c(
    person(c("Eliot", "J", "B"), "McIntire", email = "eliot.mcintire@nrcan-rncan.gc.ca", role = c("aut", "cre")),
    person(c("Alex", "M."), "Chubaty", email = "achubaty@for-cast.ca", role = c("aut"))
  ),
  childModules = character(0),
  version = list(LandWeb_summary = "0.0.1"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.md", "LandWeb_summary.Rmd"), ## README generated from module Rmd
  reqdPkgs = list("animation", "data.table", "fs", "future", "future.callr", "ggplot2", "googledrive",
                  "purrr", "qs", "raster", "sp",
                  "achubaty/amc@development",
                  "PredictiveEcology/LandR@development",
                  "PredictiveEcology/LandWebUtils@development (>= 0.1.4.9003)",
                  "PredictiveEcology/map (>= 0.0.4.9000)",
                  "PredictiveEcology/reproducible@development (>= 1.2.10)",
                  "PredictiveEcology/SpaDES.core@development (>= 1.1.0.9000)"),
  parameters = bindrows(
    defineParameter("ageClasses", "character", LandWebUtils:::.ageClasses, NA, NA,
                    "descriptions/labels for age classes (seral stages)"),
    defineParameter("ageClassCutOffs", "integer", LandWebUtils:::.ageClassCutOffs, NA, NA,
                    "defines the age boundaries between age classes"),
    defineParameter("ageClassMaxAge", "integer", 400L, NA, NA,
                    "maximum possible age"),
    defineParameter("reps", "integer", 1L:10L, 1L, NA_integer_,
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
    defineParameter(".makeTiles", "logical", FALSE, NA, NA,
                    "If `TRUE`, will generate leaflet tiles during postprocessing."),
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
                    paste("Human-readable name for the study area used - e.g., a hash of the study",
                          "area obtained using `reproducible::studyAreaName()`")),
    defineParameter(".useCache", "character", c(".inputObjects", "animation", "postprocess"), NA, NA,
                    "Names of events to be cached."),
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
      # sim <- scheduleEvent(sim, start(sim), "LandWeb_summary", "animation") ## TODO: fix error
      sim <- scheduleEvent(sim, start(sim), "LandWeb_summary", "postprocess")

      if (isTRUE(P(sim)$upload)) {
        sim <- scheduleEvent(sim, end(sim), "LandWeb_summary", "upload", .last())
      }
    },
    animation = {
      ## create tsf stack for animation
      ## -- pop it into a future and come back to it after rest of postprocessing completed
      if (is.null(mod$animation_tsf)) {
        sA <- studyArea(sim$ml, 2)

        tsfStack <- raster::stack(mod$tsfTimeSeries)
        brks <- sort(c(1L, P(sim)$ageClassCutOffs, P(sim)$ageClassMaxAge))
        cols <- RColorBrewer::brewer.pal(5, "RdYlGn")

        gifNameTSF <- file.path(normPath(outputPath(sim)), "animation_tsf.gif")
        mod$animation_tsf <- future({
          ## TODO: Error in magick_image_animate(image, as.integer(delay), as.integer(loop),  :
          ## R: cache resources exhausted `/tmp/RtmpbuP1HC/Rplot1.png' @ error/cache.c/OpenPixelCache/4083
          animation::saveGIF(ani.height = 1200, ani.width = 1200, interval = 1.0,
                             movie.name = gifNameTSF, expr = {
                               for (i in seq(quickPlot::numLayers(tsfStack))) {
                                 raster::plot(raster::mask(tsfStack[[i]], sA), breaks = brks, col = cols)
                               }
                             })
          file.exists(gifNameTSF)
        }, label = paste0("animation_", P(sim)$.studyAreaName, "_TSF"), seed = TRUE)

        sim <- scheduleEvent(sim, time(sim) + 1, "LandWeb_summary", "animation", eventPriority = .last())
      } else {
        if (isFALSE(value(mod$animation_tsf))) {
          warning("TSF animation failed.")
        }
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
      browser() ## TODO
      mod$files2upload <- set_names(mod$files2upload, basename(mod$files2upload))

      gid <- as_id(sim$uploadTo[[P(sim)$.studyAreaName]])
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

  padL <- if (P(sim)$version == 2 &&
              grepl(paste("BlueRidge", "Edson", "FMANWT_", "LP_BC", "MillarWestern", "Mistik",
                          "prov", "Sundre", "Vanderwell", "WestFraser", "WeyCo", sep = "|"),
                    outputPath(sim))) {
    if (grepl("provMB", outputPath(sim))) 4 else 3
  } else {
    4
  } ## TODO: confirm this is always true now

  mod$analysesOutputsTimes <- analysesOutputsTimes(P(sim)$summaryPeriod, P(sim)$summaryInterval)

  mod$allouts <- fs::dir_ls(outputPath(sim), regexp = "vegType|TimeSince", recurse = 1, type = "file") %>%
    grep("gri|png|txt|xml", ., value = TRUE, invert = TRUE)
  mod$allouts2 <- grep(paste(paste0("year", paddedFloatToChar(
    setdiff(P(sim)$timeSeriesTimes, mod$analysesOutputsTimes), padL = padL)), collapse = "|"),
    mod$allouts, value = TRUE, invert = TRUE)

  ## TODO: inventory all files to ensure correct dir structure? compare against expected files?
  #filesUserHas <- fs::dir_ls(P(sim)$simOutputPath, recurse = TRUE, type = "file", glob = "*.qs")

  # filesNeeded <- data.table(file = mod$allouts2, exists = TRUE) ## TODO

  # if (!all(filesNeeded$exists)) {
  #   missing <- filesNeeded[exists == FALSE, ]$file
  #   stop("Some simulation files missing:\n", paste(missing, collapse = "\n"))
  # }

  stopifnot(length(mod$allouts2) == 2 * length(P(sim)$reps) * length(mod$analysesOutputsTimes))

  mod$layerName <- gsub(mod$allouts2, pattern = paste0(".*", outputPath(sim)), replacement = "")
  mod$layerName <- gsub(mod$layerName, pattern = "[/\\]", replacement = "_")
  mod$layerName <- gsub(mod$layerName, pattern = "^_", replacement = "")

  mod$tsf <- gsub(".*vegTypeMap.*", NA, mod$allouts2) %>%
    grep(paste(mod$analysesOutputsTimes, collapse = "|"), ., value = TRUE)
  mod$vtm <- gsub(".*TimeSinceFire.*", NA, mod$allouts2) %>%
    grep(paste(mod$analysesOutputsTimes, collapse = "|"), ., value = TRUE)

  mod$tsfTimeSeries <- gsub(".*vegTypeMap.*", NA, mod$allouts) %>%
    grep(paste(P(sim)$timeSeriesTimes, collapse = "|"), ., value = TRUE)
  mod$vtmTimeSeries <- gsub(".*TimeSinceFire.*", NA, mod$allouts) %>%
    grep(paste(P(sim)$timeSeriesTimes, collapse = "|"), ., value = TRUE)

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
