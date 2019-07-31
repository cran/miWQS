
# miWQS 0.1.0

## Breaking changes

  - The `plot.wqs()` no longer automatically saves the plots to reduce
    clutter and save the user’s workspace. However, you can still use
    `ggsave()` on the output if you wish to save WQS plots. If you
    depended on this behavior, you’ll need to condition on
    `packageVersion("miWQS") > "0.0.0"`.
  - The title of the package is changed from “analysis” to “regression”
    to make the name more specific.
  - Creates a README vignette.
  - Shortens description in DESCRIPTION file for clarity.

## New Features

  - New `analyze.individually()` does individual chemical analysis. The
    outcome is independently regressed on each chemical. Individual
    chemical analyses with the outcome can be used to determine whether
    the mixture of chemicals is positively or negatively related to the
    outcome (the `b1.pos` argument in `estimate.wqs()`).
  - New `do.many.wqs()` does many WQS analyses, which is useful in the
    second stage of multiple imputation.
  - New `combine.AIC()` combines AIC results from many WQS analyses,
    similar in spirit to `pool.mi()`.
  - New `impute.boot()` performs bootstrapping imputation for many
    chemicals, not just one.
  - New `impute.sub()` imputes the values below the detection limit with
    1/sqrt(2) of that’s chemical’s detection limit.

## Minor improvements and fixes

  - Documentation is clarified and keywords are added.
  - Most `cat()` used in the functions are changed to messages
    `message()` so that the user can suppress messages using
    `suppressMessage()`.
  - Consistent comments in all functions now use notation `#>`.
  - The `estimate.wqs()`
      - Arguments
          - `B` argument: Documentation is clarified when bootstrapping
            within WQS. You now can do WQS regression without
            bootstrapping, but it is not recommended.
          - `place.bdls.in.Q1` argument now does something. You can set
            it to FALSE and regular quantiles are made. Passed to
            `make.quantile.matrix()`.
          - `offset` argument fixes transfer to `glm2()`. User should
            enter the offset on the normal scale; the logarithm is not
            taken. It is passed to `glm2()` and `glm()`. FROM: The
            `offset` argument in `glm2()` has a default value of 0. The
            offset value in `estimate.wqs()` by default is a vector of
            1’s. NOW: When using `glm2()`, offset argument now takes
            the logarithm as expected in all instances, (especially in
            `wqs.fit()`). User does not change the default of offset
            value.
      - Inner Mechanics
          - Removes duplicate output in by adding the
            `suppressMessages()` function.
          - Uses `wqs.fit()` accessory function instead of code in
            `estimate.wqs()`.
          - Changes lower case “c” to upper case “C” to avoid conflict
            with `c()`.
  - The `impute.univariate.bayesian.mi()` function:
      - `T` argument: changes default length of chain to 1,000 in order
        to be consistent with other functions.
      - `indicator.miss` output now returns as a single number (a sum)
        rather than a vector.
      - Fixes bug in initial values for the standard deviation in MCMC
        chain. Calculates standard deviation using the logarithm of
        `cov()` function. FROM: The `complete.observations` argument was
        used for observed X. NOW: standard deviations are calculated
        based on the substituted imputed chemical matrix, X, as
        covariances may not exist if X has many missing values.
      - Fixes bug so that the imputed values now draw from the last
        state, instead of the second-to-last state.
      - Inner Mechanics
      - Reduced \# of objects to be used in finding `initial2`.
          - Changed object name `x.miss.initial` to
            `log.x.miss.initial`.
          - Examples still remain the same.
  - The `make.quantile.matrix()`:
      - Adds the `place.bdls.in.Q1` argument. The default automatically
        places any missing values in X placed into the first quantile.
        We suggest the user does not specify this argument unless the
        user wants to be specific in how missing values in components
        are handled. In version 0.0.9, this argument previously had no
        effect on how quantiles were made. This argument now has an
        effect.
      - Fixes an error if there are ties in the quantiles. An error
        occurred in `cut.default(...): 'breaks' are not unique`. We use
        the `.bincode()` function instead so that ties are handled if
        they arise.
  - The `print.wqs()` now concatenates, instead of prints, the
    convergence of the bootstrap samples.
  - The `simdata87` data:
      - element `Z` contains renamed covariate names for clarity.
      - elements `X.true, X.bdl, DL, delta, n0`: all chemical names are
        converted to plain text for clarity to be used in R. The “p-p\`”
        are removed, and the “alpha” and “gamma” are spelled out
        directly.

# miWQS 0.0.9

  - First Release of Package to the public.
  - For updates to CRAN team, see cran-comments.
  - Replaces examples using example dataset in package instead of using
    package wqs. Looks cleaner
  - Removes printed output from `estimate.wqs()`.
  - Makes documentation from `estimate.wqs()` clearer.
  - Cleans `print.wqs()` documentation

# miWQS 0.0.8

  - Reworked `plot.wqs()` function by using **ggplot2** instead of base
    plotting in R.

# miWQS 0.0.7

  - Fixes bug in doing Poisson Rate WQS regressions. Adds argument
    offset to the `check_function()` and `randomize.train()`.
  - For updates to CRAN team, see cran-comments.

# miWQS 0.0.0

  - Adds a `NEWS.md` file to track changes to the package.
  - First Release of the Package to CRAN team
  - Successfully passed windows check.
