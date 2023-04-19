#' One-sided Confidence Intervals
#'
#' @param x A model object
#' @param conf.level The confidence level to use for the confidence interval if
#' conf.int = TRUE. Must be strictly greater than 0 and less than 1. Defaults
#' to 0.95, which corresponds to a 95 percent confidence interval.
#' @param exponentiate Logical indicating whether or not to exponentiate the
#' the coefficient estimates. This is typical for logistic and multinomial
#' regressions, but a bad idea if there is no log or logit link. Defaults to
#' FALSE.
#' @param alternatives a character vector specifying the alternative
#' hypothesis for the test of each coefficient. Elements must be one of
#' "two.sided", "greater" or "less". Elements will be recycled to match the
#' number of tests.
#' @param ... additional arguments passed to other methods
#'
#' @return a [broom::tidy()] object with sided confidence intervals
#' @export
#'
#' @examples
#' ## alternative can be specified for each estimate
#' model <- lm(mpg ~ vs, data = mtcars)
#' tidy_with_sided_ci(model, alternatives = c("two.sided", "greater"))
#'
#' ## not all `broom::tidy()` implement the `conf.level` argument
#' bind_rows(
#'   # `tidy.htest()`
#'   broom::tidy(
#'     t.test(
#'       mpg ~ vs,
#'       data = mutate(mtcars, vs = -(vs - 1)),
#'       alternative = "greater",
#'       var.equal = TRUE,
#'       conf.level = 0.90 # ignored in `tidy.htest()`
#'     )
#'   ) %>%
#'     select(
#'       estimate,
#'       statistic,
#'       p.value,
#'       conf.low,
#'       conf.high,
#'       alternative
#'     ),
#'
#'   # `tidy.lm()`
#'   tidy_with_sided_ci(
#'     lm(mpg ~ vs, data = mtcars),
#'     conf.level = 0.90, # not ignored in `tidy.lm()`
#'     alternatives = "greater"
#'   ) %>%
#'     filter(term == "vs") %>%
#'     select(
#'       estimate,
#'       statistic,
#'       p.value,
#'       conf.low,
#'       conf.high,
#'       alternative
#'     )
#' )
tidy_with_sided_ci <- function(x,
                               conf.level = 0.95,
                               exponentiate = FALSE,
                               alternatives = "two.sided",
                               ...) {
  stopifnot(all(alternatives %in% c("less", "two.sided", "greater")))

  if (conf.level != 0.95) {
    warning(
      "Not all `broom::tidy()` implement the `conf.level` argument ",
      "(e.g., `broom:::tidy.htest`)."
    )
  }

  two.sided <- broom::tidy(
    x,
    conf.int = TRUE,
    conf.level = conf.level,
    exponentiate = exponentiate,
    ...
  )

  one.sided <- broom::tidy(
    x,
    conf.int = TRUE,
    conf.level = 1 - 2 * (1 - conf.level),
    exponentiate = exponentiate,
    ...
  )

  dplyr::left_join(
    two.sided,
    one.sided,
    by = setdiff(names(two.sided), c("p.value", "conf.low", "conf.high"))
  ) %>%
    dplyr::mutate(
      alternative = alternatives,
      p.value = dplyr::case_when(
        alternative == "less" ~ p.value.x / 2,
        alternative == "greater" ~ p.value.x / 2,
        TRUE ~ p.value.x
      ),
      conf.low = dplyr::case_when(
        alternative == "less" ~ -Inf,
        alternative == "greater" ~ conf.low.y,
        TRUE ~ conf.low.x
      ),
      conf.high = dplyr::case_when(
        alternative == "less" ~ conf.high.y,
        alternative == "greater" ~ Inf,
        TRUE ~ conf.high.x
      )
    ) %>%
    dplyr::select(
      -dplyr::matches("p.value.[xy]"),
      -dplyr::matches("conf.low.[xy]"),
      -dplyr::matches("conf.high.[xy]")
    )
}
