#' Add significance flag
#'
#' @param data a [dplyr::tibble()] containing a column of p-values
#' @param p.value name of the p-value column
#'
#' @return a [dplyr::tibble()] with an additional character column named p.flag
#' @export
#'
#' @examples
#' model <- lm(mpg ~ vs, data = mtcars)
#' model_summary <- broom::tidy(model)
#' augment_with_p.flag(model_summary)
augment_with_p.flag <- function(data,
                                p.value = "p.value") {
  p.value <- rlang::ensym(p.value)

  data %>%
    dplyr::mutate(p.flag = dplyr::case_when(
      .data[[rlang::as_string(p.value)]] < .001 ~ "***",
      .data[[rlang::as_string(p.value)]] < .010 ~ "**",
      .data[[rlang::as_string(p.value)]] < .050 ~ "*",
      .data[[rlang::as_string(p.value)]] < .100 ~ ".",
      TRUE ~ ""
    ))
}

#' Augment data with information from an object
#'
#' @param x Model object of class lmerModLmerTest with information to append
#' to observations.
#' @param data observations to be augmented
#' @param ... Addition arguments to augment method.
#'
#' @return A \link[tibble]{tibble} with information about data points.
augment.lmerModLmerTest <- function(x,
                                    data,
                                    ...) {
  fitted <- stats::predict(x, ...)

  data %>%
    tibble::rownames_to_column(".rowname") %>%
    dplyr::left_join(
      tibble::tibble(.rowname = names(fitted), .fitted = fitted),
      by = ".rowname"
    ) %>%
    dplyr::select(-".rowname")
}
