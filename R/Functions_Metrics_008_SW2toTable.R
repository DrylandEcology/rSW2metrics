
#' Spreadsheet-like daily output of \pkg{SOILWAT2} simulations
#'
#' The main purpose of this function is the side-effect of writing
#' easy-to-share, daily simulation output of relevant variables to disk.
#'
#' @inheritParams metrics
#' @param dir_out_SW2toTable A character string. The path at which output
#'   is stored on disk.
#' @param format_share_SW2toTable A character string. The function produces
#'   output files in format of either \var{rds} via \code{\link{saveRDS}} or
#'   \var{csv} via \code{\link{write.csv}}.
#' @param outputs_SW2toTable A vector of character strings.
#'   If \var{"all"}, then all implemented output variables are included.
#' @param share_soillayer_ids A numeric vector. The indices of simulated
#'   soil layers to include in the output files. If \code{NULL}, then
#'   all simulated soil layers are included.
#'
#' @section Notes:
#'   This is not a metric as any of the regular (\code{\link{metrics}})
#'   produced by the package.
#'
#' @section Details:
#'   This function produces files as side-effect which
#'   utilizes a different output mechanism than the regular
#'   metrics provided by the package. To make this possible, a few hacks
#'   are required: \itemize{
#'     \item The argument \code{out} is set to \var{across_years}
#'           despite the fact that this function produces daily output.
#'     \item The function returns dummy output that is processed by the
#'           regular output mechanism of the package.
#'  }
#'
#' @rdname SW2toTable
#' @export
metric_SW2toTable_daily <- function(
  path, name_sw2_run, id_scen_used, list_years_scen_used,
  out = "across_years",
  dir_out_SW2toTable = file.path("..", "Outputs"),
  format_share_SW2toTable = c("rds", "csv"),
  outputs_SW2toTable = c(
    "all", "meteo", "snow",
    "radiation", "waterbalance", "evapotranspiration",
    "soiltemperature", "VWC", "SWP"
  ),
  share_soillayer_ids = NULL,
  ...
) {
  out <- match.arg(out)
  outputs_SW2toTable <- match.arg(outputs_SW2toTable, several.ok = TRUE)

  if ("all" %in% outputs_SW2toTable) {
    outputs_SW2toTable <- "all"
  }

  stopifnot(check_metric_arguments(out = "across_years"))
  format_share_SW2toTable <- match.arg(format_share_SW2toTable)

  if (!dir.exists(dir_out_SW2toTable)) {
    dir.create(dir_out_SW2toTable, recursive = TRUE, showWarnings = FALSE)
  }

  width_soiltag <- 3
  var_vwc <- "VWCBULK"
  var_swc <- "SWCBULK"


  for (k1 in seq_along(id_scen_used)) {

    fname_out <- file.path(
      dir_out_SW2toTable,
      paste0(
        name_sw2_run, "_sc", id_scen_used[k1], ".", format_share_SW2toTable
      )
    )


    # Extract rSOILWAT2 input object: `swRunScenariosData`
    sim_input <- new.env(parent = emptyenv())
    load(
      file = file.path(path, name_sw2_run, "sw_input.RData"),
      envir = sim_input
    )


    #--- Load rSOILWAT2 output object: `runDataSC`
    sim_data <- new.env(parent = emptyenv())
    load(
      file = file.path(
        path,
        name_sw2_run,
        paste0("sw_output_sc", id_scen_used[k1], ".RData")
      ),
      envir = sim_data
    )
    sim_data <- sim_data[["runDataSC"]]


    #--- Prepare data
    # Soil layers
    soils <- rSOILWAT2::swSoils_Layers(
      sim_input[["swRunScenariosData"]][[id_scen_used[k1]]]
    )
    depths0 <- formatC(
      c(0, soils[, "depth_cm"]),
      width = width_soiltag,
      flag = 0
    )
    soil_widths_str <- paste0(
      depths0[-length(depths0)],
      "to",
      depths0[-1],
      "_cm"
    )
    soil_lowerdepths_str <- paste0(depths0[-1], "_cm")

    nlyrs_sim <- nrow(soils)
    ids_cols <- if (!is.null(share_soillayer_ids)) {
      intersect(share_soillayer_ids, seq_len(nlyrs_sim))
    } else {
      seq_len(nlyrs_sim)
    }

    # VWC
    sim_vwc <- slot(slot(sim_data, var_vwc), "Day")

    # SWC
    sim_swc <- slot(slot(sim_data, var_swc), "Day")

    sim_sim <- if (nrow(sim_vwc) > 0) {
      sim_vwc
    } else if (nrow(sim_swc) > 0) {
      sim_swc
    } else {
      stop("Neither ", var_vwc, " nor ", var_swc, " was stored as output.")
    }

    # Calendar dates
    dates <- as.POSIXlt(strptime(
      paste(sim_sim[, "Year"], sim_sim[, "Day"], sep = "-"),
      format = "%Y-%j"
    ))


    #--- Put together data
    data_sim <- list(cbind(
      Year = sim_sim[, "Year"],
      DOY = sim_sim[, "Day"],
      Month = 1 + dates$mon,
      Day = dates$mday
    ))

    if (any(c("all", "meteo") %in% outputs_SW2toTable)) {
      data_sim[["meteo"]] <- cbind(
        Input_AirTemp_max_C = slot(slot(sim_data, "TEMP"), "Day")[, "max_C"],
        Input_AirTemp_min_C = slot(slot(sim_data, "TEMP"), "Day")[, "min_C"],
        Input_PPT_mm = 10 * slot(slot(sim_data, "PRECIP"), "Day")[, "ppt"]
      )
    }

    if (any(c("all", "snow") %in% outputs_SW2toTable)) {
      data_sim[["snow"]] <- cbind(
        Sim_SWE_mm = 10 * slot(
          slot(sim_data, "SNOWPACK"),
          "Day"
        )[, "snowpackWaterEquivalent_cm"]
      )
    }

    if (any(c("all", "radiation") %in% outputs_SW2toTable)) {
      data_sim[["radiation"]] <- cbind(
        `Sim_Hoh_MJm-2` = slot(slot(sim_data, "PET"), "Day")[, "H_oh_MJm-2"],
        `Sim_Hgt_MJm-2` = slot(slot(sim_data, "PET"), "Day")[, "H_gt_MJm-2"]
      )
    }

    if (any(c("all", "waterbalance") %in% outputs_SW2toTable)) {
      data_sim[["waterbalance"]] <- cbind(
        Sim_Infiltration_mm = 10 * slot(
          slot(sim_data, "SOILINFILT"),
          "Day"
        )[, "soil_inf"],
        Sim_DiffuseRecharge_mm = 10 * slot(
          slot(sim_data, "DEEPSWC"),
          "Day"
        )[, "lowLayerDrain_cm"]
      )
    }

    if (any(c("all", "evapotranspiration") %in% outputs_SW2toTable)) {
      et <- 10 * slot(slot(sim_data, "AET"), "Day")

      data_sim[["evapotranspiration"]] <- cbind(
        Sim_ET_mm = et[, "evapotr_cm"],
        Sim_T_mm = if ("tran_cm" %in% colnames(et)) {
          et[, "tran_cm"]
        } else {
          rSOILWAT2::get_transpiration(sim_data, "Day")
        },
        Sim_E_mm = rSOILWAT2::get_evaporation(sim_data),
        Sim_E_Snowloss_mm = if ("esnow_cm" %in% colnames(et)) {
          et[, "esnow_cm"]
        } else {
          10 * slot(slot(sim_data, "PRECIP"), "Day")[, "snowloss"]
        },
        Sim_E_Baresoil_mm = if ("esoil_cm" %in% colnames(et)) {
          et[, "esoil_cm"]
        } else {
          tmp <- 10 * slot(slot(sim_data, "EVAPSOIL"), "Day")
          rowSums(tmp[, grep("Lyr", colnames(tmp)), drop = FALSE])
        },
        Sim_E_InterceptedCanopy_mm = if ("ecnw_cm" %in% colnames(et)) {
          et[, "ecnw_cm"]
        } else {
          tmp <- 10 * slot(slot(sim_data, "EVAPSURFACE"), "Day")
          rowSums(
            tmp[, paste0("evap_", c("tree", "shrub", "forbs", "grass"))]
          )
        },
        Sim_E_SurfaceWater_mm = if ("esurf_cm" %in% colnames(et)) {
          et[, "esurf_cm"]
        } else {
          tmp <- 10 * slot(slot(sim_data, "EVAPSURFACE"), "Day")
          rowSums(
            tmp[, paste0("evap_", c("litter", "surfaceWater"))]
          )
        }
      )
    }

    if (any(c("all", "soiltemperature") %in% outputs_SW2toTable)) {
      tmp <- slot(
        slot(sim_data, "SOILTEMP"),
        "Day"
      )[, 2 + ids_cols, drop = FALSE]
      colnames(tmp) <- paste0(
        "Sim_SoilTemp_C_",
        soil_lowerdepths_str[ids_cols]
      )

      data_sim[["soiltemperature"]] <- cbind(
        Sim_SurfaceTemp_C = slot(
          slot(sim_data, "TEMP"),
          "Day"
        )[, "surfaceTemp_C"],
        tmp
      )
    }

    if (any(c("all", "VWC") %in% outputs_SW2toTable)) {
      tmp <- if (nrow(sim_vwc) > 0) {
        sim_vwc[, 2 + ids_cols, drop = FALSE]
      } else if (nrow(sim_swc) > 0) {
        sweep(
          sim_swc[, 2 + ids_cols, drop = FALSE],
          MARGIN = 2,
          STATS = soils[ids_cols, "depth_cm"],
          FUN = "/"
        )
      }
      colnames(tmp) <- paste0("Sim_VWC_", soil_widths_str[ids_cols])

      data_sim[["VWC"]] <- tmp
    }

    if (any(c("all", "SWP") %in% outputs_SW2toTable)) {
      tmp <- -1 / 10 * slot(
        slot(sim_data, "SWPMATRIC"),
        "Day"
      )[, 2 + ids_cols, drop = FALSE]
      colnames(tmp) <- paste0("Sim_SWP_MPa_", soil_widths_str[ids_cols])

      data_sim[["SWP"]] <- tmp
    }

    data_sim <- do.call(cbind, data_sim)

    # Write to disk
    if (format_share_SW2toTable == "rds") {
      saveRDS(data_sim, file = fname_out)

    } else if (format_share_SW2toTable == "csv") {
      utils::write.csv(
        data_sim,
        file = fname_out,
        row.names = FALSE
      )
    }
  }


  #--- Produce a mock "metric" as the package expects
  lapply(
    id_scen_used,
    function(k1) {
      t(do.call(
        rbind,
        lapply(
          list_years_scen_used[[k1]],
          function(yrs) data.frame(SW2toTable_doy001 = NA)
        )
      ))
    }
  )
}