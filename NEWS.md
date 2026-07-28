# aqpet 0.2.3

## Bug fixes

* `wenorm()`'s `seed` argument now actually makes the parallel resampling
  reproducible. The Monte Carlo resampling iterations run in parallel via
  `foreach` / `doParallel` (`%dopar%`), but `set.seed(seed)` only seeds the
  master R process -- it never reaches the worker sessions, which each drew
  from their own uncontrolled RNG. As a result the same `seed` (or the
  `setWeNorm()` default `seed = 123`) did not reproduce the same
  weather-normalised output, and worker streams were not guaranteed
  independent.

  `wenorm()` now registers a parallel-safe RNG with
  `doRNG::registerDoRNG(seed)` after setting up the cluster, so the
  `%dopar%` loops draw from independent L'Ecuyer-CMRG streams assigned
  per *iteration*. A given `seed` therefore reproduces the same result
  regardless of the number of CPU cores, and the two resampling loops
  receive separate, non-overlapping streams. With `seed = NULL` the
  streams are still parallel-safe, just not reproducible across runs.

## Dependencies

* Adds `doRNG` (Imports) for reproducible parallel random-number
  generation. Install with `install.packages("doRNG")` when updating.

# aqpet 0.2.2

## New features

* The `wenorm_method = "TuanVu"` resampler gains two optional, fully
  back-compatible knobs, set as fields on `params` / `setWeNorm()`:

  * `resample_window` (integer, default `14`) -- the half-width in days
    of the day-of-year resampling window, previously hard-coded at
    +/-14 days. Larger values widen the calendar window each
    replacement weather row may be drawn from. Governs the "TuanVu"
    method; accepted-but-unused by the other (non-windowed) methods.

  * `resample_pool` (default `"model"`) -- where replacement weather is
    drawn from. One of `"model"` (the modelling data itself, i.e. the
    v0.2.1 behaviour), `"input"` (the full per-site input *before*
    missing-response rows are dropped, so one frame can carry a long
    meteorological record alongside a pollutant column that is `NA`
    outside the study period), or a data frame (a separate multi-year
    MET pool, reused for every site -- the Tong et al. (2025) ULEZ
    style). A supplied pool must contain a `datetime` column and every
    resampled weather column; pool rows with any missing weather value
    are dropped automatically so a draw never injects `NA` met.

## Implementation

* `wenorm()` gains `resample_window` and `resample_data` arguments, and
  its internal `build_candidate_index()` is generalised from a
  self-index to a `(target, pool)` cross-index with a `window`
  argument. The TuanVu path now resolves a pool (defaulting to the
  modelling `data`), builds the index against it, and draws the
  resampled weather columns from the pool. Only the resampled weather
  columns are exported to the parallel workers, so a large multi-year
  pool is not copied in full to every worker.

* `run_wenorm()` reads `resample_window` / `resample_pool` off
  `model_params` and resolves the pool *before* `missing_treat()` drops
  the NA-response rows (`"model"` -> the modelling data; `"input"` ->
  the full pre-`missing_treat` input; a data frame -> itself), then
  threads `resample_window` / `resample_data` into the `wenorm()` calls.

* `buildMod()` documents the new `params` fields and validates them
  once, up front, for a clear early error instead of one buried in the
  per-site loop.

## Backward compatibility

* With the defaults (`resample_window = 14`, `resample_pool = "model"`)
  the output is identical to v0.2.1. Verified on the Marylebone
  modelling frame (2018-2019): the new candidate index produced
  identical candidate sets for all 17,518 rows. No change to the
  "default"/"revised" resampling paths or to `wenorm()`'s return
  structure.

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
