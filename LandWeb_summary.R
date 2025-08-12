defineModule(sim, list(
  name = "LandWeb_summary",
  description = paste("LandWeb simulation post-processing and summary creation.",
                      "Produces boxplots and histograms of simulated NRV ranges,",
                      "overlaid by indicator of current forest conditions."),
  keywords = c("LandWeb", "NRV"),
  authors = c(
    person(c("Eliot", "J", "B"), "McIntire", email = "eliot.mcintire@nrcan-rncan.gc.ca", role = "aut"),
    person(c("Alex", "M."), "Chubaty", email = "achubaty@for-cast.ca", role = c("aut", "cre"))
  ),
  childModules = character(0),
  version = list(LandWeb_summary = "1.0.4"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.md", "LandWeb_summary.Rmd"), ## README generated from module Rmd
  reqdPkgs = list("animation", "data.table", "fs", "future", "future.callr", "ggplot2", "googledrive",
                  "parallelly", "purrr", "qs", "raster", "sp", ## TODO: qs will be superceded by qs2
                  "achubaty/amc@development",
                  "PredictiveEcology/LandR@development (>= 1.1.0.9015)",
                  "PredictiveEcology/LandWebUtils@development (>= 1.0.3)",
                  "PredictiveEcology/map@development (>= 0.0.5)",
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
    defineParameter("standAgeMapFromCohorts", "logical", FALSE, NA, NA,
                    "should stand age maps be calculated from `cohortData` instead of time since fire"),
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
    defineParameter(".clInit", "{", NULL, NA, NA,
                    paste("Quoted expression to be evaluated on each node in a parallel cluster",
                          "when running `map` analyses.",
                          "Useful to set options or create non-serializable objects on each node.")),
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
                    paste("Logical. If `TRUE`, and there is more than one calculation to do at any stage,",
                          "it will create and use a parallel cluster via `makeOptimalCluster()`."))
  ),
  inputObjects = bindrows(
    expectsInput("flammableMap", "Raster",
                 desc = paste("A raster layer, with 0, 1 and NA, where 1 indicates areas",
                              "that are flammable, 0 not flammable (e.g., lakes)",
                              "and NA not applicable (e.g., masked)")),
    expectsInput("speciesLayers", "RasterStack",
                 desc = "initial percent cover raster layers used for simulation."),
    expectsInput("sppColorVect", "character",
                 desc = paste("A named vector of colors to use for plotting.",
                              "The names must be in `sim$sppEquiv[[P(sim)$sppEquivCol]]`,",
                              "and should also contain a color for 'Mixed'")),
    expectsInput("sppEquiv", "data.table",
                 desc = "table of species equivalencies. See `LandR::sppEquivalencies_CA`.")
  ),
  outputObjects = bindrows(
    createsOutput("reportingPolygons", "list", "reporting polygons to use for postprocessing")
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
      ## create sam stack for animation
      ## -- pop it into a future and come back to it after rest of postprocessing completed
      if (is.null(mod$animation_sam)) {
        sA <- studyArea(sim$ml, 2)

        samStack <- raster::stack(mod$samTimeSeries)
        brks <- sort(c(1L, P(sim)$ageClassCutOffs, P(sim)$ageClassMaxAge))
        cols <- RColorBrewer::brewer.pal(5, "RdYlGn")

        gifNamesam <- file.path(normPath(outputPath(sim)), "animation_sam.gif")
        mod$animation_sam <- future({
          ## TODO: Error in magick_image_animate(image, as.integer(delay), as.integer(loop),  :
          ## R: cache resources exhausted `/tmp/RtmpbuP1HC/Rplot1.png' @ error/cache.c/OpenPixelCache/4083
          animation::saveGIF(ani.height = 1200, ani.width = 1200, interval = 1.0,
                             movie.name = gifNamesam, expr = {
                               for (i in seq(quickPlot::numLayers(samStack))) {
                                 raster::plot(raster::mask(samStack[[i]], sA), breaks = brks, col = cols)
                               }
                             })
          file.exists(gifNamesam)
        }, label = paste0("animation_", P(sim)$.studyAreaName, "_sam"), seed = TRUE)

        sim <- scheduleEvent(sim, time(sim) + 1, "LandWeb_summary", "animation", eventPriority = .last())
      } else {
        if (isFALSE(value(mod$animation_sam))) {
          warning("SAM animation failed.")
        }
      }
    },
    postprocess = {
      # ! ----- EDIT BELOW ----- ! #
      ## each step separated here to make it easier to run different steps separately
      sim <- postprocessSetup(sim)          ## needs to be run
      sim <- postprocessLeading(sim)
      sim <- postprocessLargePatches(sim)
      sim <- postprocessPostHocFigures(sim) ## needs to be run

      ## files to be uploaded --------------------------------------------------------------------------

      ## TODO: archives (zip) instead of indiv files
      # files2upload <- c(
      #   sim$ml@metadata$sam,
      #   sim$ml@metadata$vtm,
      #   list.files(file.path(outputPath(sim), "boxplots"), recursive = TRUE),
      #   list.files(file.path(outputPath(sim), "histograms"), recursive = TRUE)
      # )
      # mod$files2upload <- c(mod$files2upload, files2upload)

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
              grepl(paste("Edson", "FMANWT_", "LP_BC", "MillarWestern", "Mistik",
                          "prov", "Vanderwell", "WestFraser", "WeyCo", sep = "|"),
                    outputPath(sim))) {
    if (grepl("provMB|provSK", outputPath(sim))) 4 else 3
  } else {
    4
  } ## TODO: confirm this is always true now

  mod$analysesOutputsTimes <- analysesOutputsTimes(P(sim)$summaryPeriod, P(sim)$summaryInterval)

  if (isTRUE(P(sim)$standAgeMapFromCohorts)) {
    standAgeFile <- "standAgeMap"
    standAgeRegex <- "standAge"
  } else {
    standAgeFile <- "rstTimeSinceFire"
    standAgeRegex <- "TimeSinceFire"
  }

  vegTypeFile <- "vegTypeMap"
  vegTypeRegex <- "vegType"

  mod$allouts <- fs::dir_ls(outputPath(sim), regexp = paste0(vegTypeRegex, "|", standAgeRegex),
                            recurse = 1, type = "file") |>
    grep("gri|png|txt|xml", x = _, value = TRUE, invert = TRUE)
  mod$allouts2 <- grep(paste(paste0("year", paddedFloatToChar(
    setdiff(c(0, P(sim)$timeSeriesTimes), mod$analysesOutputsTimes), padL = padL)), collapse = "|"),
    mod$allouts, value = TRUE, invert = TRUE)

  filesUserHas <- mod$allouts2

  dirsExpected <- file.path(outputPath(sim), sprintf("rep%02d", P(sim)$reps))
  filesExpected <- as.character(sapply(dirsExpected, function(d) {
    yr <- paddedFloatToChar(mod$analysesOutputsTimes, padL = padL)
    c(
      file.path(d, paste0(standAgeFile, "_year", yr, ".tif")),
      file.path(d, paste0(vegTypeFile, "_year", yr, ".grd"))
    )
  }))

  filesNeeded <- data.frame(file = filesExpected, exists = filesExpected %in% filesUserHas)

  ## proactively check for extra/unexpected files
  possiblyOldFiles <- findOldSimFiles(outputPath(sim))
  if (length(mod$allouts2) > 2 * length(P(sim)$reps) * length(mod$analysesOutputsTimes) ||
      length(possiblyOldFiles)) {
    stop(
      "additional unexpected files found; ensure old simulation files removed before running:\n",
      paste("  findOldSimFiles(", outputPath(sim), ", before = '2024-01-01') |> fs::file_delete()"),
    )
  }
  if (!all(filesNeeded$exists)) {
    missing <- filesNeeded[filesNeeded$exists == FALSE, ]$file
    stop(sum(!filesNeeded$exists), " simulation files appear to be missing:\n", paste(missing, collapse = "\n"))
  }

  mod$layerName <- gsub(mod$allouts2, pattern = paste0(".*", outputPath(sim)), replacement = "")
  mod$layerName <- gsub(mod$layerName, pattern = "[/\\]", replacement = "_")
  mod$layerName <- gsub(mod$layerName, pattern = "^_", replacement = "")

  mod$sam <- gsub(paste0(".*", vegTypeFile, ".*"), NA, mod$allouts2) |>
    grep(paste(mod$analysesOutputsTimes, collapse = "|"), x = _, value = TRUE)
  mod$vtm <- gsub(paste0(".*", standAgeFile, ".*"), NA, mod$allouts2) |>
    grep(paste(mod$analysesOutputsTimes, collapse = "|"), x = _, value = TRUE)

  mod$samTimeSeries <- gsub(paste0(".*", vegTypeFile, ".*"), NA, mod$allouts) |>
    grep(paste(P(sim)$timeSeriesTimes, collapse = "|"), x = _, value = TRUE)
  mod$vtmTimeSeries <- gsub(paste0(".*", standAgeFile, ".*"), NA, mod$allouts) |>
    grep(paste(P(sim)$timeSeriesTimes, collapse = "|"), x = _, value = TRUE)

  mod$flm <- file.path(outputPath(sim), "rstFlammable.tif")
  writeRaster(sim$flammableMap, mod$flm, overwrite = TRUE)

  # ! ----- STOP EDITING ----- ! #

  return(invisible(sim))
}

makeReportingPolygons <- function(sim) {
  ## Provincial Boundaries
  canProvs <- geodata::gadm(country = "CAN", level = 1, path = dPath) |>
    sf::st_as_sf()

  ml <- mapAdd(sim$canProvs, map = ml, layerName = "Provincial Boundaries",
               useSAcrs = TRUE, poly = TRUE, overwrite = TRUE,
               columnNameForLabels = "NAME_1", isStudyArea = FALSE, filename2 = NULL)


  ## Updated FMA boundaries
  ml <- mapAdd(map = ml, layerName = "FMA Boundaries Updated",
               useSAcrs = TRUE, poly = TRUE, overwrite = TRUE,
               url = "https://drive.google.com/file/d/1yCbq8rcRXCfUKHJGg-Fzlnrjl48LJfCO", ## 2024-08 added C5
               columnNameForLabels = "Name", isStudyArea = FALSE, filename2 = NULL)

  ## AB FMU boundaries
  ## TODO: only add if studyAreaReporting in AB
  ml <- mapAdd(map = ml, layerName = "AB FMU Boundaries",
               useSAcrs = TRUE, poly = TRUE, overwrite = TRUE,
               url = "https://drive.google.com/open?id=1OH3b5pwjumm1ToytDBDI6jthVe2pp0tS", # 2020-06
               columnNameForLabels = "FMU_NAME", isStudyArea = FALSE, filename2 = NULL)

  ### Rename some polygons:
  ###   - DMI is now Mercer (MPR)
  ids <- grep("Daishowa-Marubeni International Ltd", ml[["FMA Boundaries Updated"]][["Name"]])
  newNames <- c("Mercer Peace River Pulp Ltd. (East)", "Mercer Peace River Pulp Ltd. (West)")
  ml[["FMA Boundaries Updated"]][["Name"]][ids] <- newNames
  ml[["FMA Boundaries Updated"]][["shinyLabel"]][ids] <- newNames

  ## National ecozones
  ml <- mapAdd(map = ml, layerName = "National Ecozones",
               useSAcrs = TRUE, poly = TRUE, overwrite = TRUE,
               url = "https://sis.agr.gc.ca/cansis/nsdb/ecostrat/zone/ecozone_shp.zip",
               columnNameForLabels = "REGION_NAM", isStudyArea = FALSE, filename2 = NULL)
  ml[["National Ecozones"]][["Name"]] <- tools::toTitleCase(tolower(ml[["National Ecozones"]][["ZONE_NAME"]]))

  ## National ecoregions
  ml <- mapAdd(map = ml, layerName = "National Ecoregions",
               useSAcrs = TRUE, poly = TRUE, overwrite = TRUE,
               url = "https://sis.agr.gc.ca/cansis/nsdb/ecostrat/region/ecoregion_shp.zip",
               columnNameForLabels = "REGION_NAM", isStudyArea = FALSE, filename2 = NULL)
  ml[["National Ecoregions"]][["Name"]] <- ml[["National Ecoregions"]][["REGION_NAM"]]

  ## Alberta Natural Subregions (ANSRs)
  ## TODO: only add if studyAreaReporting in AB
  ml <- mapAdd(map = ml, layerName = "Alberta Natural Subregions",
               useSAcrs = TRUE, poly = TRUE, overwrite = TRUE,
               url = "https://drive.google.com/file/d/1hW6zy0CpUBdk-K2IAjzW4INjVl1J4aLJ",
               columnNameForLabels = "Name", isStudyArea = FALSE, filename2 = NULL)

  ## BC biogeoclimatic zones
  ## TODO: only add if studyAreaReporting in BC
  ml <- mapAdd(map = ml, layerName = "BC Biogeoclimatic zones",
               useSAcrs = TRUE, poly = TRUE, overwrite = TRUE,
               url = "https://drive.google.com/file/d/1NS15Gd7dHEhvPOy-Ol_LBtf-4Ch6mPnS",
               columnNameForLabels = "ZONE_NAME", isStudyArea = FALSE, filename2 = NULL)
  ml[["BC Biogeoclimatic zones"]][["Name"]] <- ml[["BC Biogeoclimatic zones"]][["ZONE_NAME"]]

  ## NWT ecoregions
  ## TODO: only add if studyAreaReporting in NWT
  ml <- mapAdd(map = ml, layerName = "Northwest Territories Ecoregions",
               useSAcrs = TRUE, poly = TRUE, overwrite = TRUE,
               url = "https://drive.google.com/file/d/1iRAQfARkmS6-XVHFnTkB-iltzMNPAczC",
               columnNameForLabels = "ECO4_NAM_1", isStudyArea = FALSE, filename2 = NULL)
  ml[["Northwest Territories Ecoregions"]][["Name"]] <- ml[["Northwest Territories Ecoregions"]][["ECO4_NAM_1"]]

  ## Caribou Ranges
  # ml <- mapAdd(map = ml, layerName = "Boreal Caribou Ranges",
  #              useSAcrs = TRUE, poly = TRUE, overwrite = TRUE,
  #              url = "https://drive.google.com/file/d/1PYLou8J1wcrme7Z2tx1wtA4GvaWnU1Jy",
  #              columnNameForLabels = "Name", isStudyArea = FALSE, filename2 = NULL)
  # ml <- mapAdd(map = ml, layerName = "BC Caribou Ranges",
  #              useSAcrs = TRUE, poly = TRUE, overwrite = TRUE,
  #              url = "https://drive.google.com/file/d/1uqEVID74y4enPMee2w3axBcR1agw_kMT",
  #              columnNameForLabels = "HERD_NAME", isStudyArea = FALSE, filename2 = NULL) ## untested
  # ml <- mapAdd(map = ml, layerName = "AB Caribou Ranges",
  #              useSAcrs = TRUE, poly = TRUE, overwrite = TRUE,
  #              url = "https://extranet.gov.ab.ca/srd/geodiscover/srd_pub/LAT/FWDSensitivity/CaribouRange.zip",
  #              columnNameForLabels = "SUBUNIT", isStudyArea = FALSE, filename2 = NULL) ## untested
  ml <- mapAdd(map = ml, layerName = "SK Caribou Ranges",
               useSAcrs = TRUE, poly = TRUE, overwrite = TRUE,
               url = "https://drive.google.com/file/d/1LiizDyXOfJPQ76FQM8SQ1_kYG9hJUDdK",
               columnNameForLabels = "RGEUNIT", isStudyArea = FALSE, filename2 = NULL)
  ml[["SK Caribou Ranges"]][["Name"]] <- ml[["SK Caribou Ranges"]][["RGEUNIT"]]

  if (grepl("provMB", P(sim)$.studyAreaName)) {
    ## TODO: .zipx file; needs 'manual' extract 1st time
    ml <- mapAdd(map = ml, layerName = "MB Caribou Ranges",
                 useSAcrs = TRUE, poly = TRUE, overwrite = TRUE,
                 url = "https://drive.google.com/file/d/1Y_Qi3twoU3fHaNgMzF5QEl1CosGmGyha/",
                 targetFile = "Boreal_caribou_MUs_MB_2015.shp", alsoExtract = "similar",
                 columnNameForLabels = "RANGE_NAME", isStudyArea = FALSE, filename2 = NULL)
    ml[["MB Caribou Ranges"]][["Name"]] <- ml[["MB Caribou Ranges"]][["RANGE_NAME"]]
  }

  ml <- mapAdd(map = ml, layerName = "LandWeb Caribou Ranges",
               useSAcrs = TRUE, poly = TRUE, overwrite = TRUE,
               url = "https://drive.google.com/file/d/1mrsxIJfdP-XxEZkO6vs2J6lYbGry67A2",
               columnNameForLabels = "Name", isStudyArea = FALSE, filename2 = NULL)




  plotFMA(
    studyAreaReporting,
    provs = LandWebUtils::studyAreaProv(studyAreaReporting),
    caribou = sar.caribou,
    xsr =  sim$studyArea,
    title = P(sim)$.studyAreaName,
    png = file.path(dataDir, glue::glue("{P(sim)$.studyAreaName}.png"))
  )


  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

.inputObjects <- function(sim) {
  dPath <- asPath(inputPath(sim), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")

  # ! ----- EDIT BELOW ----- ! #
  fsim <- file.path(outputPath(sim), paste0("simOutPreamble_", P(sim)$.studyAreaName, ".qs"))
  if (!file.exists(fsim)) {
    fsim <- file.path(tools::file_path_sans_ext(fsim), ".rds") ## fallback to rds if qs not used
  }
  tmp <- loadSimList(fsim)

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
