# aqpet 0.2.1

## Performance

* `wenorm_method = "TuanVu"` resampling is dramatically faster. The
  per-row +/-14-day/same-hour candidate lookup is now precomputed once
  per `wenorm()` call (via a per-hour binary search, O(N log N)) and
  reused across all `num_iterations` Monte Carlo passes, instead of
  being rescanned in full -- an O(N) distance scan over every row --
  for every row on every pass (previously O(num_iterations * N^2)
  overall). Each iteration's resampling is also now a single
  vectorised sample-and-assign operation instead of a row-by-row
  `new_data[row_i, ...] <- ...` loop.

  On a 48,000-row hourly series (matching `Manchester_Piccadilly.csv`),
  this cuts the resampling step from ~51s to ~0.18s per iteration --
  roughly 255 minutes down to ~53 seconds in total at the package's
  default `num_iterations = 300`. Output is statistically equivalent
  to the previous implementation: same +/-14-day, same-hour candidate
  pool and fallback rules, just computed without the redundant
  re-scanning.

## Bug fixes

* Fixed a latent `sample(idx, 1)` bug in the "TuanVu" resampling step:
  when a row had exactly one matching candidate row, `sample()`
  interpreted that single integer as a range (`1:idx`) rather than
  returning `idx` itself, occasionally resampling weather from the
  wrong row. Now uses the safe `idx[sample.int(length(idx), 1)]` form.

## Other changes

* No change to the "default"/"revised" resampling paths, the
  baseline-resample/`h2o.predict()`/`bsts` change-point-smoothing
  stages, or `wenorm()`'s return structure -- this release only
  touches how the "TuanVu" candidate rows are found and sampled.

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
