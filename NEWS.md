# aqpet 0.2.0

## New features

* Added `wenorm_method = "TuanVu"`, implementing the day-of-year/hour windowed
  weather-normalisation resampling method of Vu et al. (2019) as an
  alternative to the existing "default"/"revised" methods.

## Bug fixes

* Fixed `setWeNorm()`'s default `constant_variables`, which was set to the
  weather variables instead of the time variables. Since `wenorm()`
  resamples whatever is *not* listed in `constant_variables`, this inverted
  default meant the package's documented default configuration
  (`setWeNorm()` with no overrides) resampled the time variables and left
  weather untouched -- i.e. did not actually perform weather normalisation
  -- for every `wenorm_method`, not just the new "TuanVu" method.

## Other changes

* `wenorm()` no longer computes the fully-random baseline resample,
  ratio-correction, and `bsts` change-point smoothing stages when
  `wenorm_method = "TuanVu"`, since none of that is part of the published
  method and none of it was used downstream -- this roughly halves runtime
  for "TuanVu" runs.
* `buildMod()` no longer writes a duplicate `_final.csv` for
  `wenorm_method = "TuanVu"` (it was byte-identical to `_wn.csv`).
* Documented `wenorm_method` and clarified `constant_variables` semantics in
  roxygen across `params.R`, `wenorm.R`, and `run_wenorm.R`.
