postprocessLandWeb <- function(sim) {
  .tilePath <- getOption("map.tilePath", file.path(outputPath(sim), "tiles"))

  ## TODO: put this file checking code back into init event once mod cache problem fixed
  padL <- if (P(sim)$version == 2 &&
              grepl(paste("BlueRidge", "Edson", "FMANWT_", "LP_BC", "MillarWestern", "Mistik",
                          "prov", "Sundre", "Vanderwell", "WestFraser", "WeyCo", sep = "|"),
                    outputPath(sim))) {
    if (grepl("provMB", outputPath(sim))) 4 else 3
  } else {
    4
  } ## TODO: confirm this is always true now

  mod$analysesOutputsTimes <- seq(P(sim)$summaryPeriod[1], P(sim)$summaryPeriod[2],
                                  by = P(sim)$summaryInterval)

  mod$allouts <- dir(outputPath(sim), full.names = TRUE, recursive = TRUE)
  mod$allouts <- grep("vegType|TimeSince", mod$allouts, value = TRUE)
  mod$allouts <- grep("gri|png|txt|xml", mod$allouts, value = TRUE, invert = TRUE)
  mod$allouts2 <- grep(paste(paste0("year", paddedFloatToChar(P(sim)$timeSeriesTimes, padL = padL)), collapse = "|"),
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
  ###

  vtmCC <- vegTypeMapGenerator(sim$speciesLayers, P(sim)$vegLeadingProportion, mixedType = 2,
                               sppEquiv = sim$sppEquiv, sppEquivCol = P(sim)$sppEquivCol,
                               colors = sim$sppColorVect, doAssertion = FALSE)

  fname <- file.path(outputPath(sim), "CurrentConditionVTM.grd")
  writeRaster(vtmCC, fname, datatype = "INT1U", overwrite = TRUE)

  fname2 <- file.path(outputPath(sim), "CurrentConditionTSF.tif")
  writeRaster(sim$ml[["CC TSF"]], fname2, datatype = "INT1U", overwrite = TRUE)

  sim$ml <- mapAdd(
    map = sim$ml, layerName = "CC VTM", analysisGroup1 = "CC",
    targetFile = asPath(fname),
    destinationPath = asPath(outputPath(sim)),
    filename2 = asPath("CurrentConditionVTM_temp.grd"), ## TODO: remove this workaround
    tsf = asPath(fname2),
    vtm = asPath(fname),
    CC = TRUE,
    overwrite = TRUE,
    #useCache = "overwrite",
    leaflet = if (isTRUE(P(sim)$.makeTiles)) .tilePath else FALSE
  )

  ## TODO: WORKAROUND for some funny business with col names.
  if (any(grepl("ANSR", names(sim$ml)))) {
    ids <- which(grepl("ANSR", names(sim$ml)))
    lapply(ids, function(id) {
      if (is.null(sim$ml[[names(sim$ml)[id]]][["Name"]])) {
        sim$ml[[names(sim$ml)[id]]][["Name"]] <- sim$ml[[names(sim$ml)[id]]][["Name.1"]]
        sim$ml[[names(sim$ml)[id]]][["Name.1"]] <- sim$ml[[names(sim$ml)[id]]][["Name.2"]] <- NULL
      }

      if (is.null(sim$ml[[names(sim$ml)[id]]][["shinyLabel"]])) {
        sim$ml[[names(sim$ml)[id]]][["shinyLabel"]] <- sim$ml[[names(sim$ml)[id]]][["shinyLabel.1"]]
        sim$ml[[names(sim$ml)[id]]][["shinyLabel.1"]] <- sim$ml[[names(sim$ml)[id]]][["shinyLabel.2"]] <- NULL
      }
    })
  }

  if (any(grepl("BGCZ", names(sim$ml)))) {
    ids <- which(grepl("BGCZ", names(sim$ml)))
    lapply(ids, function(id) {
      if (is.null(sim$ml[[names(sim$ml)[id]]][["Name"]])) {
        sim$ml[[names(sim$ml)[id]]][["Name"]] <- sim$ml[[names(sim$ml)[id]]][["Name.1"]]
        sim$ml[[names(sim$ml)[id]]][["Name.1"]] <- sim$ml[[names(sim$ml)[id]]][["Name.2"]] <- NULL
      }

      if (is.null(sim$ml[[names(sim$ml)[id]]][["shinyLabel"]])) {
        sim$ml[[names(sim$ml)[id]]][["shinyLabel"]] <- sim$ml[[names(sim$ml)[id]]][["shinyLabel.1"]]
        sim$ml[[names(sim$ml)[id]]][["shinyLabel.1"]] <- sim$ml[[names(sim$ml)[id]]][["shinyLabel.2"]] <- NULL
      }
    })
  }

  if (any(grepl("NWTER", names(sim$ml)))) {
    ids <- which(grepl("NWTER", names(sim$ml)))
    lapply(ids, function(id) {
      if (is.null(sim$ml[[names(sim$ml)[id]]][["Name"]])) {
        sim$ml[[names(sim$ml)[id]]][["Name"]] <- sim$ml[[names(sim$ml)[id]]][["Name.1"]]
        sim$ml[[names(sim$ml)[id]]][["Name.1"]] <- sim$ml[[names(sim$ml)[id]]][["Name.2"]] <- NULL
      }

      if (is.null(sim$ml[[names(sim$ml)[id]]][["shinyLabel"]])) {
        sim$ml[[names(sim$ml)[id]]][["shinyLabel"]] <- sim$ml[[names(sim$ml)[id]]][["shinyLabel.1"]]
        sim$ml[[names(sim$ml)[id]]][["shinyLabel.1"]] <- sim$ml[[names(sim$ml)[id]]][["shinyLabel.2"]] <- NULL
      }
    })
  }

  if (any(grepl("NATLER", names(sim$ml)))) {
    ids <- which(grepl("NATLER", names(sim$ml)))
    lapply(ids, function(id) {
      if (is.null(sim$ml[[names(sim$ml)[id]]][["Name"]])) {
        sim$ml[[names(sim$ml)[id]]][["Name"]] <- sim$ml[[names(sim$ml)[id]]][["Name.1"]]
        sim$ml[[names(sim$ml)[id]]][["Name.1"]] <- sim$ml[[names(sim$ml)[id]]][["Name.2"]] <- NULL
      }

      if (is.null(sim$ml[[names(sim$ml)[id]]][["shinyLabel"]])) {
        sim$ml[[names(sim$ml)[id]]][["shinyLabel"]] <- sim$ml[[names(sim$ml)[id]]][["shinyLabel.1"]]
        sim$ml[[names(sim$ml)[id]]][["shinyLabel.1"]] <- sim$ml[[names(sim$ml)[id]]][["shinyLabel.2"]] <- NULL
      }
    })
  }

  if (any(grepl("Caribou$|Caribou Joined", names(sim$ml)))) { ## be sure not to include "LandWeb Caribou Ranges" polygon
    ids <- which(grepl("Caribou$|Caribou Joined", names(sim$ml)))
    lapply(ids, function(id) {
      if (is.null(sim$ml[[names(sim$ml)[id]]][["Name"]])) {
        sim$ml[[names(sim$ml)[id]]][["Name"]] <<- sim$ml[[names(sim$ml)[id]]][["Name.1"]]
        sim$ml[[names(sim$ml)[id]]][["Name.1"]] <<- sim$ml[[names(sim$ml)[id]]][["Name.2"]] <- NULL
      }

      if (is.null(sim$ml[[names(sim$ml)[id]]][["shinyLabel"]])) {
        sim$ml[[names(sim$ml)[id]]][["shinyLabel"]] <<- sim$ml[[names(sim$ml)[id]]][["shinyLabel.1"]]
        sim$ml[[names(sim$ml)[id]]][["shinyLabel.1"]] <<- sim$ml[[names(sim$ml)[id]]][["shinyLabel.2"]] <- NULL
      }
    })
  }

  ag1 <- gsub(mod$layerName, pattern = "(.*)_.*_(.*)\\..*", replacement = "\\1_\\2") %>%
    grep(paste(mod$analysesOutputsTimes, collapse = "|"), ., value = TRUE)

  sim$ml <- mapAdd(
    map = sim$ml,
    layerName = mod$layerName,
    analysisGroup1 = ag1,
    targetFile = asPath(mod$allouts2),
    destinationPath = asPath(dirname(mod$allouts2)),
    filename2 = NULL,
    tsf = asPath(mod$tsf),
    vtm = asPath(mod$vtm),
    outfile = file.path(outputPath(sim), "log", "LandWeb_summary_tsf_vtm.log"),
    overwrite = TRUE,
    #useCache = "overwrite",
    leaflet = if (isTRUE(P(sim)$.makeTiles)) .tilePath else FALSE
  )

  fml <- list(
    simFile("ml", outputPath(sim), ext = "qs"),
    simFile("ml_leading", outputPath(sim), ext = "qs"),
    simFile("ml_large", outputPath(sim), ext = "qs"),
    simFile("ml_done", outputPath(sim), ext = "qs")
  )

  qs::qsave(sim$ml, fml[[1]])
  #sim$ml <- qs::qload(fml[[1]])

  ## next sets of analyses require more ram so don't use previously set num cpus
  prevNcores <- getOption("map.maxNumCores")
  options(map.maxNumCores = pemisc::optimalClusterNum(60000, parallel::detectCores() / 2))

  sim$ml <- mapAddAnalysis(
    sim$ml,
    functionName = "LeadingVegTypeByAgeClass",
    #purgeAnalyses = "LeadingVegTypeByAgeClass",
    ageClasses = P(sim)$ageClasses,
    ageClassCutOffs = P(sim)$ageClassCutOffs,
    sppEquivCol = "EN_generic_short",
    sppEquiv = sim$sppEquiv,
    outfile = file.path(outputPath(sim), "log", "LandWeb_summary_LeadingVegTypeByAgeClass.log")
  )

  qs::qsave(sim$ml, fml[[2]])
  #sim$ml <- qs::qload(fml[[2]])

  sim$ml <- mapAddAnalysis(
    sim$ml,
    functionName = "LargePatches",
    id = "1",
    labelColumn = "shinyLabel",
    #purgeAnalyses = "LargePatches",
    ageClasses = P(sim)$ageClasses,
    ageClassCutOffs = P(sim)$ageClassCutOffs,
    sppEquivCol = "EN_generic_short",
    sppEquiv = sim$sppEquiv,
    outfile = file.path(outputPath(sim), "log", "LandWeb_summary_LargePatches.log")
  )

  qs::qsave(sim$ml, fml[[3]])
  #sim$ml <- qs::qload(fml[[3]])

  options(map.maxNumCores = prevNcores)

  histDirOld <- file.path(outputPath(sim), "hists") %>% normPath(.)
  histDirNew <- file.path(outputPath(sim), "histograms") %>% normPath(.)
  if (dir.exists(histDirOld))
    file.rename(from = histDirOld, to = histDirNew)

  ## 'archive' previous largePatches results following bugfix (2021-05-05)
  histDirArchived <- paste0("histograms_archived_", format(Sys.Date(), "%Y-%m-%d"))
  histDirArchived <- file.path(outputPath(sim), histDirArchived) %>% normPath(.)
  if (dir.exists(histDirNew) && !dir.exists(histDirArchived))
    file.rename(from = histDirNew, to = histDirArchived)

  ## this analysisGroupReportingPolygon MUST be the same as one of ones already analysed
  sim$ml <- mapAddPostHocAnalysis(
    map = sim$ml,
    functionName = "rbindlistAG",
    postHocAnalysisGroups = "analysisGroupReportingPolygon",
    #purgeAnalyses = "rbindlistAG",
    postHocAnalyses = "all",
    outfile = file.path(outputPath(sim), "log", "LandWeb_summary_rbindlistAG.log")
  )

  sim$ml <- mapAddPostHocAnalysis(
    map = sim$ml,
    functionName = "runBoxPlotsVegCover",
    postHocAnalysisGroups = "analysisGroupReportingPolygon",
    postHocAnalyses = "rbindlistAG",
    #purgeAnalyses = "runBoxPlotsVegCover",
    dPath = file.path(outputPath(sim), "boxplots"),
    outfile = file.path(outputPath(sim), "log", "LandWeb_summary_runBoxPlotsVegCover.log")
  )

  sim$ml <- mapAddPostHocAnalysis(
    map = sim$ml,
    functionName = "runHistsVegCover",
    postHocAnalysisGroups = "analysisGroupReportingPolygon",
    postHocAnalyses = "rbindlistAG",
    #purgeAnalyses = "runHistsVegCover",
    dPath = file.path(outputPath(sim), "histograms"),
    outfile = file.path(outputPath(sim), "log", "LandWeb_summary_runHistsVegCover.log")
  )

  sim$ml <- mapAddPostHocAnalysis(
    map = sim$ml,
    functionName = "runHistsLargePatches",
    postHocAnalysisGroups = "analysisGroupReportingPolygon",
    postHocAnalyses = "rbindlistAG",
    #purgeAnalyses = "runHistsLargePatches",
    dPath = file.path(outputPath(sim), "histograms"),
    outfile = file.path(outputPath(sim), "log", "LandWeb_summary_runHistsLargePatches.log")
  )

  qs::qsave(sim$ml, fml[[4]])
  #ml <- qs::qload(fml[[4]])

  ## files to be uploaded --------------------------------------------------------------------------

  ## TODO: archives (zip) instead of indiv files
  # files2upload <- c(
  #   sim$ml@metadata$tsf,
  #   sim$ml@metadata$vtm,
  #   list.files(file.path(outputPath(sim), "boxplots"), recursive = TRUE),
  #   list.files(file.path(outputPath(sim), "histograms"), recursive = TRUE)
  # )
  # mod$files2upload <- c(mod$files2upload, files2upload)

  return(invisible(sim))
}
