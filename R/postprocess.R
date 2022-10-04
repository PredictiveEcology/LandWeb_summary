postprocessLandWeb <- function(sim) {
  ##  TODO: remove this once map works in parallel
  if (!isFALSE(P(sim)$.useParallel)) {
    warning("Post-processing does not currently work in parallel.\n",
            "Temporarily setting option 'map.useParallel' to FALSE.")
    opts <- options(map.useParallel = FALSE)
    on.exit(options(opts), add = TRUE)
  }

  ##  TODO: fix use of leaflet with mapAdd:
  ##    Error in rbindlist(list(map@metadata, dts), use.names = TRUE, fill = TRUE) :
  ##    Class attribute on column 5 of item 2 does not match with column 4 of item 1.
  if (!isFALSE(getOption("map.tilePath"))) {
    opts <- options(map.tilePath = FALSE)
    on.exit(options(opts), add = TRUE)
  }

  vtmCC <- vegTypeMapGenerator(sim$speciesLayers, P(sim)$vegLeadingProportion, mixedType = 2,
                               sppEquiv = sim$sppEquiv, sppEquivCol = P(sim)$sppEquivCol,
                               colors = sim$sppColorVect, doAssertion = FALSE)
  fname <- file.path(outputPath(sim), "CurrentConditionVTM.tif")
  writeRaster(vtmCC, fname, overwrite = TRUE)

  fname2 <- file.path(outputPath(sim), "CurrentConditionTSF.tif")
  writeRaster(sim$ml[["CC TSF"]], fname2, overwrite = TRUE)

  ## TODO: fix use of leaflet with mapAdd:
  ##       Error in rbindlist(list(map@metadata, dts), use.names = TRUE, fill = TRUE) :
  ##       Class attribute on column 5 of item 2 does not match with column 4 of item 1.
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
    leaflet = getOption("map.tilePath", FALSE)
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
browser()
  sim$ml <- mapAdd(
    map = sim$ml, layerName = mod$layerName, analysisGroup1 = ag1,
    targetFile = asPath(mod$allouts2),
    destinationPath = asPath(dirname(mod$allouts2)),
    filename2 = NULL, tsf = asPath(mod$tsf), vtm = asPath(mod$vtm),
    overwrite = TRUE,
    #useCache = "overwrite",
    leaflet = getOption("map.tilePath", FALSE)
  )

  fml <- list(
    simFile("ml", outputPath(sim), ext = "qs"),
    simFile("ml_leading", outputPath(sim), ext = "qs"),
    simFile("ml_large", outputPath(sim), ext = "qs"),
    simFile("ml_done", outputPath(sim), ext = "qs")
  )

  qs::qsave(sim$ml, fml[[1]])
  #sim$ml <- qs::qload(fml[[1]])

  sim$ml <- mapAddAnalysis(sim$ml, functionName = "LeadingVegTypeByAgeClass",
                           #purgeAnalyses = "LeadingVegTypeByAgeClass",
                           ageClasses = ageClasses, ageClassCutOffs = ageClassCutOffs,
                           sppEquivCol = "EN_generic_short", sppEquiv = sppEquiv)

  qs::qsave(sim$ml, fml[[2]])
  #sim$ml <- qs::qload(fml[[2]])

  sim$ml <- mapAddAnalysis(sim$ml, functionName = "LargePatches",
                           id = "1", labelColumn = "shinyLabel",
                           #purgeAnalyses = "LargePatches",
                           ageClasses = ageClasses, ageClassCutOffs = ageClassCutOffs,
                           sppEquivCol = "EN_generic_short", sppEquiv = sppEquiv)

  qs::qsave(sim$ml, fml[[3]])
  #sim$ml <- qs::qload(fml[[3]])

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
  sim$ml <- mapAddPostHocAnalysis(map = sim$ml, functionName = "rbindlistAG",
                                  postHocAnalysisGroups = "analysisGroupReportingPolygon",
                                  #purgeAnalyses = "rbindlistAG",
                                  postHocAnalyses = "all")
  sim$ml <- mapAddPostHocAnalysis(map = sim$ml, functionName = "runBoxPlotsVegCover",
                                  postHocAnalysisGroups = "analysisGroupReportingPolygon",
                                  postHocAnalyses = "rbindlistAG",
                                  #purgeAnalyses = "runBoxPlotsVegCover",
                                  dPath = file.path(outputPath(sim), "boxplots"))
  sim$ml <- mapAddPostHocAnalysis(map = sim$ml, functionName = "runHistsVegCover",
                                  postHocAnalysisGroups = "analysisGroupReportingPolygon",
                                  postHocAnalyses = "rbindlistAG",
                                  #purgeAnalyses = "runHistsVegCover",
                                  dPath = file.path(outputPath(sim), "histograms"))
  sim$ml <- mapAddPostHocAnalysis(map = sim$ml, functionName = "runHistsLargePatches",
                                  postHocAnalysisGroups = "analysisGroupReportingPolygon",
                                  postHocAnalyses = "rbindlistAG",
                                  #purgeAnalyses = "runHistsLargePatches",
                                  dPath = file.path(outputPath(sim), "histograms"))

  qs::qsave(sim$ml, fml[[4]])
  #ml <- qs::qload(fml[[4]])

  ## files to be uploaded --------------------------------------------------------------------------
  browser()
  files2upload <- unlist(files2upload, recursive = TRUE) ## TODO: from ml? boxpolts, hist, csvs, rasters?

  mod$files2upload <- c(mod$files2upload, files2upload)

  return(invisible(sim))
}
