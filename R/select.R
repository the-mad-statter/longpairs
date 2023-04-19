#' Subset columns using a correlation p-value
#'
#' @description Select variables in a data frame based on the p-value of the
#' correlation with one of the variables.
#'
#' @param data A data frame, data frame extension (e.g. a tibble), or a lazy
#' data frame (e.g. from dbplyr or dtplyr).
#' @param referent name of the common variable with which to compute
#' correlations
#' @param p.value the maximum desired p-value
#' @param ... additional arguments passed to [stats::cor.test()]
#'
#' @return [dplyr::tibble()]
#' @export
#'
#' @examples
#' select_via_cor_sig(mtcars, mpg, 0.001)
select_via_cor_sig <- function(data,
                               referent,
                               p.value,
                               ...) {
  referent <- rlang::ensym(referent)

  keepers <- data %>%
    names() %>%
    setdiff(rlang::as_string(referent)) %>%
    lapply(function(candidate) {
      x <- data[[candidate]]
      y <- data[[rlang::as_string(referent)]]
      stats::cor.test(x, y, ...) %>%
        broom::tidy() %>%
        dplyr::mutate(candidate = candidate) %>%
        dplyr::select("candidate", "p.value")
    }) %>%
    dplyr::bind_rows() %>%
    dplyr::filter(.data[["p.value"]] < {{ p.value }}) %>%
    dplyr::pull("candidate")

  data %>%
    dplyr::select(!!referent, dplyr::all_of(keepers))
}
