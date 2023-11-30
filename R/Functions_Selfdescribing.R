
#' List all metric functions
#' @export
list_all_metrics <- function() {
  tmp <- ls(getNamespace("rSW2metrics"))
  tmp <- tmp[startsWith(tmp, "metric_")]
  tmp <- tmp[vapply(tmp, function(x) is.function(get0(x)), FUN.VALUE = NA)]
  # Exclude defunct metrics, i.e., those with `...` as the only argument
  tmpa <- vapply(
    tmp,
    function(foo) {
      frmls <- formals(foo)
      isTRUE(length(frmls) == 1 && names(frmls) == "...")
    },
    FUN.VALUE = NA
  )
  tmp[!tmpa]
}


has_fun_soils_as_arg <- function(fun) {
  isTRUE("soils" %in% names(formals(fun)))
}

has_fun_ts_as_output <- function(fun) {
  tmp <- sort(eval(formals(fun)[["out"]]))
  isTRUE(identical(tmp, "ts_years") || identical(tmp, c("raw", "ts_years")))
}

is_fun_collecting_inputs <- function(fun) {
  isTRUE(grepl("collect_input_", fun, fixed = TRUE))
}




check_metric_arguments <- function(out, req_soil_vars) {
  fun_args <- as.list(match.call(
    definition = sys.function(sys.parent()),
    call = sys.call(sys.parent()),
    envir = parent.frame(2L)
  ))

  # Evaluate arguments, but not function name (first element)
  fun_name <- if (is.function(fun_args[[1]])) {
    "anonymous"
  } else {
    as.character(fun_args[[1]])
  }

  fun_args <- lapply(fun_args[-1], eval)

  if (missing(out) || is.null(out)) {
    stop("'out' is a required argument to function ", shQuote(fun_name))
  }

  if ("out" %in% names(fun_args) && !(fun_args[["out"]] %in% c(out, "raw"))) {
    stop(
      "Inconsistency in 'out': ",
      shQuote(out), " versus ", shQuote(fun_args[["out"]])
    )
  }

  if (identical(out, "ts_years")) {
    tmp_is_list <- inherits(fun_args[["list_years_scen_used"]], "list")
    tmp_is_numeric <- vapply(
      fun_args[["list_years_scen_used"]],
      is.numeric,
      FUN.VALUE = NA
    )

    if (!(isTRUE(tmp_is_list) && all(tmp_is_numeric))) {
      stop(
        "'list_years_scen_used' ",
        "should be a list with integer vectors ",
        "for function ", shQuote(fun_name)
      )
    }

  } else if (identical(out, "across_years")) {
    tmp_is_list <- inherits(fun_args[["list_years_scen_used"]], "list")
    tmp_is_list2 <- vapply(
      fun_args[["list_years_scen_used"]],
      inherits,
      what = "list",
      FUN.VALUE = NA
    )

    tmp_is_numeric <- vapply(
      unlist(fun_args[["list_years_scen_used"]], recursive = FALSE),
      is.numeric,
      FUN.VALUE = NA
    )

    if (!(isTRUE(tmp_is_list) && all(tmp_is_list2) && all(tmp_is_numeric))) {
      stop(
        "'list_years_scen_used' ",
        "should be a list of lists with integer vectors ",
        "for function ", shQuote(fun_name)
      )
    }

    if (!"fun_aggs_across_yrs" %in% names(fun_args)) {
      stop(
        "'fun_aggs_across_yrs' ",
        "is a required argument ",
        "for function ", shQuote(fun_name)
      )
    }

  } else if (identical(out, "raw")) {
  }

  if (!missing(req_soil_vars)) {
    if ("soils" %in% names(fun_args)) {
      tmp <- !(req_soil_vars %in% names(list_soil_variables()))

      if (any(tmp)) {
        stop(
          "Requested soil variable(s) ",
          toString(shQuote(req_soil_vars[tmp])),
          " are not implemented, see `list_soil_variables()`."
        )
      }

      has_vars <-
        req_soil_vars %in% names(fun_args[["soils"]]) &
        vapply(
          req_soil_vars,
          function(x) !is.null(fun_args[["soils"]][[x]]),
          FUN.VALUE = NA
        )

      if (!all(has_vars)) {
        stop(
          "Requested soil variable(s) ",
          toString(shQuote(req_soil_vars[!has_vars])),
          " are missing from 'soils' object."
        )
      }

    } else {
      stop(
        "Soil variables are required for function ", shQuote(fun_name),
        " but there is no 'soils' object."
      )
    }
  }

  invisible(TRUE)
}



#' List all input collecting functions
#' @export
list_all_input_collectors <- function() {
  tmp <- ls(getNamespace("rSW2metrics"))
  tmp <- tmp[startsWith(tmp, "collect_input_")]
  tmp[vapply(tmp, function(x) is.function(get0(x)), FUN.VALUE = NA)]
}


#' List of implemented soil properties
#'
#' @export
list_soil_variables <- function() {
  c(
    depth_cm = "depth",
    sand_frac = "sand",
    clay_frac = "clay",
    gravel_content = "gravel"
  )
}


#' List possible sub-annual time step identifiers
#'
#' Sub-annual time steps are organized via the column \dQuote{group}.
#'
#' @return A list with the implemented sub-annual time step identifiers
#'   - seasonal: \var{\dQuote{seasonX}} where X can be 1 to a value in `1:12`
#'   - quarterly: \var{\dQuote{QX}} where X in `1:4`
#'   - monthly: \var{\dQuote{monX}} where X in `01:12`
#'   - daily: \var{\dQuote{doyX}} where X in `001:366`
#'
#' @export
list_subannual_timesteps <- function() {
  list(
    seasonal = "season[[:digit:]]{1,2}",
    quarterly = paste0("Q", seq_len(4)),
    monthly = paste0("mon", formatC(seq_len(12), width = 2, flag = 0)),
    daily = paste0("doy", formatC(seq_len(366), width = 3, flag = 0))
  )
}


#' @examples
#' identify_submetric_timesteps(
#'   paste0("PET_", list_subannual_timesteps()[["monthly"]])
#' )
#'
#' @noRd
identify_submetric_timesteps <- function(submetrics) {
  tag_timesteps <- list_subannual_timesteps()
  tag_timesteps2 <- unlist(tag_timesteps)
  names(tag_timesteps2) <- rep(names(tag_timesteps), lengths(tag_timesteps))

  vapply(
    submetrics,
    function(mm) {
      tmp <- vapply(
        tag_timesteps2,
        FUN = grepl,
        x = mm,
        FUN.VALUE = NA
      )
      res <- names(tag_timesteps2)[tmp]
      if (length(res) == 0) "yearly" else res
    },
    FUN.VALUE = NA_character_
  )
}


#' @examples
#' identify_metric_timestep(
#'   paste0("PET_", list_subannual_timesteps()[["monthly"]])
#' )
#'
#' @noRd
identify_metric_timestep <- function(submetrics) {

  tag_timesteps <- list_subannual_timesteps()

  tmp <- vapply(
    tag_timesteps,
    function(tss) {
      all(
        vapply(tss, function(val) any(grepl(val, submetrics)), FUN.VALUE = NA)
      )
    },
    FUN.VALUE = NA
  )

  timestep <- names(tmp)[tmp]


  if (length(timestep) > 1) {
    stop(
      "More than one time step detected ",
      toString(shQuote(timestep))
    )

  } else if (length(timestep) == 0) {
    list(
      timestep = "yearly",
      submetrics2s = submetrics,
      submetrics2u = submetrics
    )

  } else {
    tmp <- paste0(tag_timesteps[[timestep]], collapse = "|")
    # remove trailing "_"
    tmp <- gsub("_$", "", gsub(tmp, "", submetrics))

    list(
      timestep = timestep,
      submetrics2s = tmp,
      submetrics2u = unique(tmp)
    )
  }
}
