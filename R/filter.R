#' Filter Complete Cases
#'
#' @param data dataset to filter for complete cases
#'
#' @return data with only complete cases
#' @export
#'
#' @examples
#' tribble(
#'   ~x, ~y,
#'   11, 12,
#'   21, NA
#' ) %>%
#'   filter_complete.cases()
filter_complete.cases <- function(data) {
  data[stats::complete.cases(data), ]
}
