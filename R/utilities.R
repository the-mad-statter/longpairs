#' Keep Confidence Interval Columns
#'
#' @param data broom tidy dataframe
#' @param conf.int logical indicating whether to keep `conf.low` and `conf.high`
#'
#' @return the broom tidy dataframe with or without the confidence interval
#' columns
#'
#' @examples
#' \dontrun{
#' lm(mpg ~ vs, data = mtcars) %>%
#'   tidy(conf.int = TRUE) %>%
#'   keep_conf.int(FALSE)
#' }
keep_conf.int <- function(data,
                          conf.int) {
  if (!conf.int) {
    data <- data %>% dplyr::select(-dplyr::starts_with("conf"))
  }
  data
}

#' Formula Terms
#'
#' @param data dataset corresponding to the formula
#' @param formula the model formula as character
#'
#' @return character vector of terms in the formula
#' @export
#'
#' @examples
#' formula_terms(mtcars, "mpg ~ cyl")
formula_terms <- function(data,
                          formula) {
  data_names <- names(data)
  data_names[vapply(data_names, grepl, logical(1), x = formula)]
}
