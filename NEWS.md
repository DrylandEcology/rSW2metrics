# rSW2metrics v0.3.0-9000
* `metric_FrostDaysAtNeg5C()` returns now `NAs` instead of `+/-Inf` for years
  without a frost event.
* New metrics functions to extract `"EcologicalDroughtMetrics2023"`, i.e.,
    * `metric_EcologicalDroughtMetrics2023_annual()` returns annual time series
    * `metric_EcologicalDroughtMetrics2023_annualClim()` returns climatologies
      (summaries across years).


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
