#--- Obtain input values -------------------------------------------------------
get_soillayers_variable <- function(
  path,
  name_sw2_run,
  id_scen = 1L,
  zipped_runs = FALSE,
  Nmax_soillayers = 23L,
  sw2_soil_var,
  ...
) {

  Nmax_soillayers <- as.integer(Nmax_soillayers)
  Nmax_soillayers <- if (is.finite(Nmax_soillayers) && Nmax_soillayers > 0) {
    min(Nmax_soillayers, 23L)
  } else {
    23L
  }

  # Extract rSOILWAT2 input object: `swRunScenariosData`
  sim_input <- load_sw2_rda(
    path = file.path(path, name_sw2_run),
    fname = "sw_input.RData",
    zipped_runs = zipped_runs
  )

  tmp <- slot(
    object = slot(sim_input[["swRunScenariosData"]][[id_scen]], "soils"),
    name = "Layers"
  )[, sw2_soil_var, drop = FALSE]

  res <- matrix(
    data = NA,
    nrow = length(sw2_soil_var),
    ncol = Nmax_soillayers,
    dimnames = list(sw2_soil_var, NULL)
  )

  res[, seq_len(nrow(tmp))] <- t(tmp)

  res
}





#--- Specific input value functions ------------------------------------------
collect_input_soillayers_depth <- function(
  path,
  name_sw2_run,
  id_scen = 1L,
  zipped_runs = FALSE,
  Nmax_soillayers = 23L,
  ...
) {
  get_soillayers_variable(
    path,
    name_sw2_run,
    id_scen,
    zipped_runs = zipped_runs,
    Nmax_soillayers = Nmax_soillayers,
    sw2_soil_var = "depth_cm"
  )
}

collect_input_soillayers_gravel <- function(
  path,
  name_sw2_run,
  id_scen = 1L,
  zipped_runs = FALSE,
  Nmax_soillayers = 23L,
  ...
) {
  get_soillayers_variable(
    path,
    name_sw2_run,
    id_scen,
    zipped_runs = zipped_runs,
    Nmax_soillayers = Nmax_soillayers,
    sw2_soil_var = "gravel_content"
  )
}

collect_input_soillayers_sand <- function(
  path,
  name_sw2_run,
  id_scen = 1L,
  zipped_runs = FALSE,
  Nmax_soillayers = 23L,
  ...
) {
  get_soillayers_variable(
    path,
    name_sw2_run,
    id_scen,
    zipped_runs = zipped_runs,
    Nmax_soillayers = Nmax_soillayers,
    sw2_soil_var = "sand_frac"
  )
}


collect_input_soillayers_clay <- function(
  path,
  name_sw2_run,
  id_scen = 1L,
  zipped_runs = FALSE,
  Nmax_soillayers = 23L,
  ...
) {
  get_soillayers_variable(
    path,
    name_sw2_run,
    id_scen,
    zipped_runs = zipped_runs,
    Nmax_soillayers = Nmax_soillayers,
    sw2_soil_var = "clay_frac"
  )
}


collect_input_soillayers_count <- function(
  path,
  name_sw2_run,
  id_scen = 1L,
  zipped_runs = FALSE,
  ...
) {
  sim_data <- collect_sw2_sim_data(
    path = path,
    name_sw2_run = name_sw2_run,
    id_scen = id_scen,
    output_sets = list(
      yr = list(
        sw2_tp = "Year",
        sw2_outs = "SWPMATRIC",
        sw2_vars = c(swp = "Lyr"),
        varnames_are_fixed = FALSE
      )
    ),
    zipped_runs = zipped_runs
  )

  ncol(sim_data[["yr"]][["values"]][["swp"]])
}
