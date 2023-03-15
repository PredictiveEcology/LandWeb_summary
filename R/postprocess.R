postprocessLandWeb <- function(sim) {
  .tilePath <- getOption("map.tilePath", file.path(outputPath(sim), "tiles"))

  vtmCC <- Cache(vegTypeMapGenerator,
                 x = sim$speciesLayers,
                 vegLeadingProportion = P(sim)$vegLeadingProportion,
                 mixedType = 2,
                 sppEquiv = sim$sppEquiv,
                 sppEquivCol = P(sim)$sppEquivCol,
                 colors = sim$sppColorVect,
                 doAssertion = FALSE)

  fname1 <- file.path(outputPath(sim), "CurrentConditionVTM.grd")
  raster::writeRaster(vtmCC, fname1, datatype = "INT1U", overwrite = TRUE)

  fname2 <- file.path(outputPath(sim), "CurrentConditionTSF.tif")
  tsfCC <- sim$ml[["CC TSF"]]
  raster::writeRaster(tsfCC, fname2, datatype = "INT1U", overwrite = TRUE)

  stopifnot(compareRaster(tsfCC, vtmCC))
  rm(tsfCC, vtmCC)

  sim$ml <- mapAdd(
    map = sim$ml,
    layerName = "CC VTM",
    analysisGroup1 = "CC",
    targetFile = asPath(fname1),
    destinationPath = asPath(outputPath(sim)),
    filename2 = NULL, ## don't write VTM to disk (rspatial/terra#976)
    tsf = asPath(fname2),
    vtm = asPath(fname1),
    CC = TRUE,
    overwrite = TRUE,
    #useCache = "overwrite",
    leaflet = if (isTRUE(P(sim)$.makeTiles)) .tilePath else FALSE
  )

  ## TODO: WORKAROUND for some funny business with col names ---------------------------------------
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

  if (any(grepl("FMAs$|FMA Boundaries Updated", names(sim$ml)))) {
    ids <- which(grepl("FMAs$|FMA Boundaries Updated", names(sim$ml)))
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

  if (any(grepl("NATLEZ", names(sim$ml)))) {
    ids <- which(grepl("NATLEZ", names(sim$ml)))
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
  ## END: WORKAROUND for some funny business with col names ----------------------------------------

  ## analysis group 1
  ag1 <- gsub(mod$layerName, pattern = "(.*)_.*_(.*)\\..*", replacement = "\\1_\\2") %>%
    grep(paste(mod$analysesOutputsTimes, collapse = "|"), ., value = TRUE)

  sim$ml <- mapAdd(
    map = sim$ml,
    layerName = names(ag1),
    analysisGroup1 = ag1,
    targetFile = asPath(mod$allouts2),
    destinationPath = asPath(dirname(mod$allouts2)),
    filename2 = NULL,
    tsf = asPath(mod$tsf),
    vtm = asPath(mod$vtm),
    outfile = file.path(outputPath(sim), "log", "LandWeb_summary_tsf_vtm.log"),
    overwrite = TRUE,
    #useCache = "overwrite",
    leaflet = if (isTRUE(P(sim)$.makeTiles)) .tilePath else FALSE,
    .clInit = P(sim)$.clInit
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
    outfile = file.path(outputPath(sim), "log", "LandWeb_summary_LeadingVegTypeByAgeClass.log"),
    .clInit = P(sim)$.clInit
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
    outfile = file.path(outputPath(sim), "log", "LandWeb_summary_LargePatches.log"),
    .clInit = P(sim)$.clInit
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
