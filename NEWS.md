# rSW2metrics v0.2.0-9000
* `rSW2metrics` can now handle simulation output that is stored either in
  zipped archives (or in folders as before); metric and extraction functions
  gained a logical argument `zipped_runs`.
* `metric_SW2toTable_daily` now correctly calculates
  bulk volumetric water content (from soil moisture content) for the case when
  volumetric water content was not stored in output object.

# rSW2metrics v0.1.0
* Initial release.
