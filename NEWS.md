# rSW2metrics v0.3.0-9000
* `metric_FrostDaysAtNeg5C()` returns now `NAs` instead of `+/-Inf` for years
  without a frost event.
* New metrics functions to extract `"EcologicalDroughtMetrics2023"`, i.e.,
    * `metric_EcologicalDroughtMetrics2023_annual()` returns annual time series
    * `metric_EcologicalDroughtMetrics2023_annualClim()` returns climatologies
      (summaries across years).
* `rSW2metrics` can now utilize new functionality for
  soil water retention curves `SWRC` introduced with `rSOILWAT2` `v6.0.0` while
  continuing to support earlier versions
    * new internal `convert_with_swrc()` translates between
      volumetric water content and soil water potential
      for any suitable version combination of `rSOILWAT2` input objects
      and currently installed `rSOILWAT2` package
    * new internal `has_fun_swrc_as_arg()` detects if a metric function
      requires a `SWRC` translation by checking for the presence of a
      `"swrcp_and_usage"` argument
    * metrics that require a `SWRC` translation gain argument
      `"swrcp_and_usage"`; this argument contains all necessary information
      about the `SWRC` which was active during a simulation, and is then
      internally used to call `convert_with_swrc()`
    * new functionality gathers information for the new `"swrcp_and_usage"`
      arguments
      (following structure of existing functionality that
      prepares soil properties information for the `"soils"` arguments), i.e.,
      such information can either be extracted from a `rSOILWAT2` input object
      when needed to calculate a metrics or, alternatively
      (but not implemented yet),
      be loaded from a pre-prepared file stored on disk
        * new `load_swrcp_and_usage()` determines available `SWRC` functionality
          and prepares required parameters depending on `rSOILWAT2` version
        * `process_values_one_site()`, `prepare_soils_for_site()`, and
          `get_soillayers_variable()` gain arguments to load and/or prepare
          necessary `SWRC` information at the same time as soil properties are
          prepared (to avoid reading multiple times a `rSOILWAT2` input objects
          from disk)
        * `extract_metrics()` now checks if a metric requires
          `SWRC` functionality and handles loading and/or preparing of
          necessary information to be passed on to the metric function.


# rSW2metrics v0.2.0
* `rSW2metrics` can now handle simulation output that is stored either in
  zipped archives (or in folders as before); metric and extraction functions
  gained a logical argument `zipped_runs`.
* `metric_SW2toTable_daily()` now correctly calculates
  bulk volumetric water content (from soil moisture content) for the case when
  volumetric water content was not stored in output object.
* `metric_SW2toTable_daily()` now provides additional outputs
    * daily potential evapotranspiration (group `"evapotranspiration"`)
    * daily total, wet, and dry degree days (group `"MDD"`)
* `metric_RR2022predictors_annualClim()` now correctly calculates
  `"DeepDrainage_mean"` and `"CWD_mon_corr_temp_mean"`.
* New `metric_RR2022predictors_annual()` provides annual time series of
  metrics underlying the `RR2022predictors` of
  `metric_RR2022predictors_annualClim()`.
* Tests of metrics that compare values against previous output now use
  the most recent, stored copy instead of requiring a copy created with the
  current version of `rSOILWAT2`.

# rSW2metrics v0.1.0
* Initial release.
