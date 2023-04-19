#' Multiple comparison independent sample t-tests
#'
#' @description Performs independent sample t-tests for each pair of groups
#' from tidy data.
#'
#' @param data tidy data containing the response and group variables
#' @param response response variable
#' @param class classification variable
#' @param conf.int whether to include a confidence interval
#' @param conf.level confidence level for CI
#' @param descriptives logical to add descriptive statistics or not
#' @param ... additional parameters passed onto [stats::t.test()]
#'
#' @return a [dplyr::tibble()] with information about the model components;
#' one model per row
#' @export
#'
#' @examples
#' lp_ttest_two(mtcars, mpg, cyl)
lp_ttest_two <- function(data,
                         response,
                         class,
                         conf.int = TRUE,
                         conf.level = 0.95,
                         descriptives = TRUE,
                         ...) {
  response <- rlang::ensym(response)
  class <- rlang::ensym(class)

  classes <- data %>%
    dplyr::pull(!!class) %>%
    factor() %>%
    levels()

  seq_along(classes) %>%
    utils::combn(2) %>%
    apply(2, I, simplify = FALSE) %>%
    lapply(function(p) {
      prefix_cols <- dplyr::tibble(
        "response" = rlang::as_string(response),
        "class"    = rlang::as_string(class),
        "class1"   = classes[p][1],
        "class2"   = classes[p][2]
      )

      s <- data %>%
        dplyr::select(!!response, !!class) %>%
        dplyr::filter(!!class %in% classes[p]) %>%
        filter_complete.cases()

      q <- rlang::quo(
        stats::t.test(
          !!response ~ !!class,
          data = s,
          conf.level = conf.level,
          ...
        )
      )

      inferential_cols <- NULL

      message_cols <- dplyr::tibble(
        message.class = NA_character_,
        message = NA_character_
      )

      inferential_cols <- tryCatch(
        {
          rlang::eval_tidy(q)
        },
        message = function(cond) {
          message_cols$message.class <<- "message"
          message_cols$message <<- cond$message
          suppressMessages(rlang::eval_tidy(q))
        },
        warning = function(cond) {
          message_cols$message.class <<- "warning"
          message_cols$message <<- cond$message
          suppressWarnings(rlang::eval_tidy(q))
        },
        error = function(cond) {
          message_cols$message.class <<- "error"
          message_cols$message <<- cond$message
          NULL
        }
      )

      inferential_cols <- inferential_cols %>%
        broom::tidy() %>%
        keep_conf.int(conf.int) %>%
        augment_with_p.flag()

      descriptive_cols <-
        if (descriptives) {
          s %>%
            dplyr::group_by(!!class) %>%
            dplyr::summarize(
              n = dplyr::n(),
              m = mean(!!response),
              s = stats::sd(!!response),
            ) %>%
            tidyr::pivot_wider(
              names_from = !!class,
              values_from = -!!class
            ) %>%
            dplyr::rename_with(~ c("n1", "n2", "m1", "m2", "s1", "s2"))
        } else {
          NULL
        }

      dplyr::bind_cols(
        prefix_cols,
        inferential_cols,
        descriptive_cols,
        message_cols
      )
    }) %>%
    dplyr::bind_rows()
}

#' Multiple comparison dependent sample t-tests
#'
#' @description Performs paired t-tests for each pair of groups where
#' observations are paired by unit from tidy data.
#'
#' @param data tidy data containing the response, group, and unit variables
#' @param response response variable
#' @param class class variable
#' @param unit sampling unit variable (e.g., persons)
#' @param conf.int whether to include a confidence interval
#' @param conf.level confidence level for CI
#' @param descriptives = logical to add descriptive statistics or not
#' @param ... additional parameters passed onto [stats::t.test()]
#'
#' @note Tests are performed on pairwise complete data.
#'
#' @return a [dplyr::tibble()] with information about the model components;
#' one model per row
#' @export
#'
#' @examples
#' lp_ttest_dep(sleep, extra, group, ID)
lp_ttest_dep <- function(data,
                         response,
                         class,
                         unit,
                         conf.int = TRUE,
                         conf.level = 0.95,
                         descriptives = TRUE,
                         ...) {
  response <- rlang::ensym(response)
  class <- rlang::ensym(class)
  unit <- rlang::ensym(unit)

  classes <- dplyr::pull(data, !!class) %>%
    factor() %>%
    levels()

  seq_along(classes) %>%
    utils::combn(2) %>%
    apply(2, I, simplify = FALSE) %>%
    lapply(function(p) {
      prefix_cols <- dplyr::tibble(
        "response" = rlang::as_string(response),
        "class"    = rlang::as_string(class),
        "class1"   = classes[p][1],
        "class2"   = classes[p][2]
      )

      s <- data %>%
        dplyr::select(!!response, !!class, !!unit) %>%
        dplyr::filter(!!class %in% classes[p]) %>%
        filter_complete.cases() %>%
        dplyr::group_by(!!unit) %>%
        dplyr::filter(dplyr::n() == 2) %>% # drop  any unpaired
        dplyr::ungroup()

      # split into (x,y)
      x <- s %>%
        dplyr::filter(!!class == classes[p][1]) %>%
        dplyr::arrange(!!unit) %>%
        dplyr::pull(!!response)

      y <- s %>%
        dplyr::filter(!!class == classes[p][2]) %>%
        dplyr::arrange(!!unit) %>%
        dplyr::pull(!!response)

      q <- rlang::quo(
        stats::t.test(
          x,
          y,
          paired = TRUE,
          conf.level = conf.level,
          ...
        )
      )

      inferential_cols <- NULL

      message_cols <- dplyr::tibble(
        message.class = NA_character_,
        message = NA_character_
      )

      inferential_cols <- tryCatch(
        {
          rlang::eval_tidy(q)
        },
        message = function(cond) {
          message_cols$message.class <<- "message"
          message_cols$message <<- cond$message
          suppressMessages(rlang::eval_tidy(q))
        },
        warning = function(cond) {
          message_cols$message.class <<- "warning"
          message_cols$message <<- cond$message
          suppressWarnings(rlang::eval_tidy(q))
        },
        error = function(cond) {
          message_cols$message.class <<- "error"
          message_cols$message <<- cond$message
          NULL
        }
      )

      inferential_cols <- inferential_cols %>%
        broom::tidy() %>%
        keep_conf.int(conf.int) %>%
        augment_with_p.flag()

      descriptive_cols <-
        if (descriptives) {
          s %>%
            dplyr::group_by(!!class) %>%
            dplyr::summarize(
              n = dplyr::n(),
              m = mean(!!response),
              s = stats::sd(!!response),
            ) %>%
            tidyr::pivot_wider(
              names_from = !!class,
              values_from = -!!class
            ) %>%
            dplyr::rename_with(~ c("n1", "n2", "m1", "m2", "s1", "s2"))
        } else {
          NULL
        }

      dplyr::bind_cols(
        prefix_cols,
        inferential_cols,
        descriptive_cols,
        message_cols
      )
    }) %>%
    dplyr::bind_rows()
}

#' Multiple comparison one-sample t-tests
#'
#' @description Performs one-sample t-tests for each group from tidy data.
#'
#' @param data tidy data containing the response and group variables
#' @param response response variable
#' @param class class variable
#' @param conf.int whether to include a confidence interval
#' @param conf.level confidence level for CI
#' @param descriptives = logical to add descriptive statistics or not
#' @param mu a number indicating the true value of the mean
#' @param ... additional parameters passed onto [stats::t.test()]
#'
#' @return a [dplyr::tibble()] with information about the model components; one
#' model per row
#' @export
#'
#' @examples
#' lp_ttest_one(mtcars, mpg, cyl, 10)
lp_ttest_one <- function(data,
                         response,
                         class,
                         conf.int = TRUE,
                         conf.level = 0.95,
                         descriptives = TRUE,
                         mu = 0,
                         ...) {
  response <- rlang::ensym(response)
  class <- rlang::ensym(class)

  classes <- dplyr::pull(data, !!class) %>%
    factor() %>%
    levels()

  classes %>%
    lapply(function(g) {
      prefix_cols <- dplyr::tibble(
        "response" = rlang::as_string(response),
        "class"    = rlang::as_string(class),
        "class1"   = g
      )

      s <- data %>%
        dplyr::select(!!response, !!class) %>%
        dplyr::filter(!!class == g) %>%
        filter_complete.cases()

      q <- rlang::quo(
        stats::t.test(
          !!response ~ 1,
          data = s,
          mu = mu,
          conf.level = conf.level,
          ...
        )
      )

      inferential_cols <- NULL

      message_cols <- dplyr::tibble(
        message.class = NA_character_,
        message = NA_character_
      )

      inferential_cols <- tryCatch(
        {
          rlang::eval_tidy(q)
        },
        message = function(cond) {
          message_cols$message.class <<- "message"
          message_cols$message <<- cond$message
          suppressMessages(rlang::eval_tidy(q))
        },
        warning = function(cond) {
          message_cols$message.class <<- "warning"
          message_cols$message <<- cond$message
          suppressWarnings(rlang::eval_tidy(q))
        },
        error = function(cond) {
          message_cols$message.class <<- "error"
          message_cols$message <<- cond$message
          NULL
        }
      )

      inferential_cols <- inferential_cols %>%
        broom::tidy() %>%
        keep_conf.int(conf.int) %>%
        augment_with_p.flag()

      descriptive_cols <-
        if (descriptives) {
          s %>%
            dplyr::summarize(
              n = dplyr::n(),
              m = mean(!!response),
              s = stats::sd(!!response)
            )
        } else {
          NULL
        }

      dplyr::bind_cols(
        prefix_cols,
        inferential_cols,
        descriptive_cols,
        message_cols
      )
    }) %>%
    dplyr::bind_rows()
}

#' Multiple comparison Wilcoxon (Mann Whitney U) Rank Sum Tests
#'
#' @description Performs independent sample Wilcoxon tests for each pair of
#' groups from tidy data.
#'
#' @param data tidy data containing the response and group variables
#' @param response response variable
#' @param class class variable
#' @param conf.int whether to include a confidence interval
#' @param conf.level confidence level for CI
#' @param descriptives = logical to add descriptive statistics or not
#' @param ... additional parameters passed onto [stats::wilcox.test()]
#'
#' @return a [dplyr::tibble()] with information about the model components;
#' one model per row
#' @export
#'
#' @examples
#' lp_wilcox_two(mtcars, mpg, cyl)
lp_wilcox_two <- function(data,
                          response,
                          class,
                          conf.int = TRUE,
                          conf.level = 0.95,
                          descriptives = TRUE,
                          ...) {
  response <- rlang::ensym(response)
  class <- rlang::ensym(class)

  classes <- data %>%
    dplyr::pull(!!class) %>%
    factor() %>%
    levels()

  seq_along(classes) %>%
    utils::combn(2) %>%
    apply(2, I, simplify = FALSE) %>%
    lapply(function(p) {
      prefix_cols <- dplyr::tibble(
        "response" = rlang::as_string(response),
        "class"    = rlang::as_string(class),
        "class1"   = classes[p][1],
        "class2"   = classes[p][2]
      )

      s <- data %>%
        dplyr::select(!!response, !!class) %>%
        dplyr::filter(!!class %in% classes[p]) %>%
        filter_complete.cases()

      q <- rlang::quo(
        stats::wilcox.test(
          !!response ~ !!class,
          data = s,
          conf.int = conf.int,
          conf.level = conf.level,
          ...
        )
      )

      inferential_cols <- NULL

      message_cols <- dplyr::tibble(
        message.class = NA_character_,
        message = NA_character_
      )

      inferential_cols <- tryCatch(
        {
          rlang::eval_tidy(q)
        },
        message = function(cond) {
          message_cols$message.class <<- "message"
          message_cols$message <<- cond$message
          suppressMessages(rlang::eval_tidy(q))
        },
        warning = function(cond) {
          message_cols$message.class <<- "warning"
          message_cols$message <<- cond$message
          suppressWarnings(rlang::eval_tidy(q))
        },
        error = function(cond) {
          message_cols$message.class <<- "error"
          message_cols$message <<- cond$message
          NULL
        }
      )

      inferential_cols <- inferential_cols %>%
        broom::tidy() %>%
        keep_conf.int(conf.int) %>%
        augment_with_p.flag()

      descriptive_cols <-
        if (descriptives) {
          s %>%
            dplyr::group_by(!!class) %>%
            dplyr::summarize(
              n    = dplyr::n(),
              q0   = stats::quantile(!!response, 0.00, names = FALSE),
              q25  = stats::quantile(!!response, 0.25, names = FALSE),
              q50  = stats::quantile(!!response, 0.50, names = FALSE),
              q75  = stats::quantile(!!response, 0.75, names = FALSE),
              q100 = stats::quantile(!!response, 1.00, names = FALSE),
              iqr  = .data[["q75"]] - .data[["q25"]]
            ) %>%
            tidyr::pivot_wider(
              names_from = !!class,
              values_from = -!!class
            ) %>%
            dplyr::rename_with(~ c(
              "n1",     "n2",
              "q0.1",   "q0.2",
              "q25.1",  "q25.2",
              "q50.1",  "q50.2",
              "q75.1",  "q75.2",
              "q100.1", "q100.2",
              "iqr1",   "iqr2"
            ))
        } else {
          NULL
        }

      dplyr::bind_cols(
        prefix_cols,
        inferential_cols,
        descriptive_cols,
        message_cols
      )
    }) %>%
    dplyr::bind_rows()
}

#' Multiple comparison correlation tests
#'
#' @description Test for association between paired samples, using one of
#' Pearson's product moment correlation coefficient, Kendall's tau or
#' Spearman's rho for each pair of variables.
#'
#' @param data tidy data containing the response, group, and unit variables
#' @param value response variable corresponding to name
#' @param name source of the value
#' @param id sampling unit variable (e.g., persons)
#' @param conf.int whether to include a confidence interval
#' @param conf.level confidence level for CI
#' @param descriptives = logical to add descriptive statistics or not
#' @param ... additional parameters passed onto [stats::cor.test()]
#'
#' @note Tests are performed on pairwise complete data.
#'
#' @return a [dplyr::tibble()] with information about the model components;
#' one model per row
#' @export
#'
#' @examples
#' lp_cor(sleep, extra, group, ID)
lp_cor <- function(data,
                   value,
                   name,
                   id,
                   conf.int = TRUE,
                   conf.level = 0.95,
                   descriptives = TRUE,
                   ...) {
  value <- rlang::ensym(value)
  name <- rlang::ensym(name)
  id <- rlang::ensym(id)

  names <- dplyr::pull(data, !!name) %>%
    factor() %>%
    levels()

  seq_along(names) %>%
    utils::combn(2) %>%
    apply(2, I, simplify = FALSE) %>%
    lapply(function(p) {
      prefix_cols <- dplyr::tibble(
        "name1" = !!paste0(name, "==", names[p][1]),
        "name2" = !!paste0(name, "==", names[p][2])
      )

      s <- data %>%
        dplyr::select(!!value, !!name, !!id) %>%
        dplyr::filter(!!name %in% names[p]) %>%
        filter_complete.cases() %>%
        dplyr::group_by(!!id) %>%
        dplyr::filter(dplyr::n() == 2) %>% # drop unpaired
        dplyr::ungroup()

      x <- s %>%
        dplyr::filter(!!name == names[p][1]) %>%
        dplyr::arrange(!!id) %>%
        dplyr::pull(!!value)

      y <- s %>%
        dplyr::filter(!!name == names[p][2]) %>%
        dplyr::arrange(!!id) %>%
        dplyr::pull(!!value)

      q <- rlang::quo(
        stats::cor.test(
          x,
          y,
          conf.level = conf.level,
          ...
        )
      )

      inferential_cols <- NULL

      message_cols <- dplyr::tibble(
        message.class = NA_character_,
        message = NA_character_
      )

      inferential_cols <- tryCatch(
        {
          rlang::eval_tidy(q)
        },
        message = function(cond) {
          message_cols$message.class <<- "message"
          message_cols$message <<- cond$message
          suppressMessages(rlang::eval_tidy(q))
        },
        warning = function(cond) {
          message_cols$message.class <<- "warning"
          message_cols$message <<- cond$message
          suppressWarnings(rlang::eval_tidy(q))
        },
        error = function(cond) {
          message_cols$message.class <<- "error"
          message_cols$message <<- cond$message
          NULL
        }
      )

      inferential_cols <- inferential_cols %>%
        broom::tidy() %>%
        keep_conf.int(conf.int) %>%
        augment_with_p.flag()

      descriptive_cols <-
        if (descriptives) {
          if (!is.null(inferential_cols$parameter)) { # param imply pearson
            s %>%
              dplyr::group_by(!!name) %>%
              dplyr::summarize(
                n = dplyr::n(),
                m = mean(!!value),
                s = stats::sd(!!value),
              ) %>%
              tidyr::pivot_wider(
                names_from = !!name,
                values_from = -!!name
              ) %>%
              dplyr::rename_with(~ c("n1", "n2", "m1", "m2", "s1", "s2"))
          } else {
            s %>%
              dplyr::group_by(!!name) %>%
              dplyr::summarize(
                n    = dplyr::n(),
                q0   = stats::quantile(!!value, 0.00, names = FALSE),
                q25  = stats::quantile(!!value, 0.25, names = FALSE),
                q50  = stats::quantile(!!value, 0.50, names = FALSE),
                q75  = stats::quantile(!!value, 0.75, names = FALSE),
                q100 = stats::quantile(!!value, 1.00, names = FALSE),
                iqr  = .data[["q75"]] - .data[["q25"]]
              ) %>%
              tidyr::pivot_wider(
                names_from = !!name,
                values_from = -!!name
              ) %>%
              dplyr::rename_with(~ c(
                "n1", "n2",
                "q0.1", "q0.2",
                "q25.1", "q25.2",
                "q50.1", "q50.2",
                "q75.1", "q75.2",
                "q100.1", "q100.2",
                "iqr1", "iqr2"
              ))
          }
        } else {
          NULL
        }

      dplyr::bind_cols(
        prefix_cols,
        inferential_cols,
        descriptive_cols,
        message_cols
      )
    }) %>%
    dplyr::bind_rows()
}

#' Multiple comparison odds ratios
#'
#' @description Computes odds ratios for each pair of exposures from tidy data.
#'
#' @param data tidy data containing the response and exposure variables
#' @param response response variable
#' @param class exposure variable
#' @param conf.int whether to include a confidence interval
#' @param conf.level confidence level for CI
#' @param descriptives = logical to add descriptive statistics or not
#' @param ... additional parameters ultimately passed onto [stats::glm()]
#'
#' @return a [dplyr::tibble()] with computed odds ratios and associated
#' information
#' @export
#'
#' @examples
#' tribble(
#'   ~admit, ~rank, ~n,
#'   0, "1", 28,
#'   0, "2", 97,
#'   0, "3", 93,
#'   0, "4", 55,
#'   1, "1", 33,
#'   1, "2", 54,
#'   1, "3", 28,
#'   1, "4", 12
#' ) %>%
#'   uncount(n) %>%
#'   lp_oddsratio(admit, rank)
lp_oddsratio <- function(data,
                         response,
                         class,
                         conf.int = TRUE,
                         conf.level = 0.95,
                         descriptives = TRUE,
                         ...) {
  response <- rlang::ensym(response)
  class <- rlang::ensym(class)

  classes <- data %>%
    dplyr::pull(!!class) %>%
    factor() %>%
    levels()

  seq_along(classes) %>%
    utils::combn(2) %>%
    apply(2, I, simplify = FALSE) %>%
    lapply(function(p) {
      prefix_cols <- dplyr::tibble(
        "response" = rlang::as_string(response),
        "class"    = rlang::as_string(class),
        "class1"   = classes[p][1],
        "class2"   = classes[p][2]
      )

      s <- data %>%
        dplyr::select(!!response, !!class) %>%
        dplyr::filter(!!class %in% classes[p]) %>%
        dplyr::mutate(!!class := factor(!!class, levels = classes[p])) %>%
        filter_complete.cases()

      q <- rlang::quo(
        stats::glm(!!response ~ !!class, stats::binomial(), s, ...)
      )

      inferential_cols <- NULL

      message_cols <- dplyr::tibble(
        message.class = NA_character_,
        message = NA_character_
      )

      inferential_cols <- tryCatch(
        {
          rlang::eval_tidy(q)
        },
        message = function(cond) {
          message_cols$message.class <<- "message"
          message_cols$message <<- cond$message
          suppressMessages(rlang::eval_tidy(q))
        },
        warning = function(cond) {
          message_cols$message.class <<- "warning"
          message_cols$message <<- cond$message
          suppressWarnings(rlang::eval_tidy(q))
        },
        error = function(cond) {
          message_cols$message.class <<- "error"
          message_cols$message <<- cond$message
          NULL
        }
      )

      inferential_cols <- inferential_cols %>%
        broom::tidy(
          conf.int = conf.int,
          conf.level = conf.level,
          exponentiate = TRUE
        ) %>%
        dplyr::slice(2) %>%
        augment_with_p.flag()

      descriptive_cols <-
        if (descriptives) {
          s %>%
            dplyr::count(!!response, !!class) %>%
            tidyr::complete(!!response, !!class, fill = list(n = 0)) %>%
            tidyr::pivot_wider(names_from = -"n", values_from = "n") %>%
            dplyr::rename_with(~ c("n.tn", "n.fp", "n.fn", "n.tp"))
        } else {
          NULL
        }

      dplyr::bind_cols(
        prefix_cols,
        inferential_cols,
        descriptive_cols,
        message_cols
      )
    }) %>%
    dplyr::bind_rows()
}

#' Multiple comparison risk ratios
#'
#' @description Computes risk ratios for each pair of exposures from tidy data.
#'
#' @param data tidy data containing the response and exposure variables
#' @param response response variable
#' @param class exposure variable
#' @param conf.int whether to include a confidence interval
#' @param conf.level confidence level for CI
#' @param descriptives = logical to add descriptive statistics or not
#' @param ... additional parameters ultimately passed onto [stats::glm()]
#'
#' @return a [dplyr::tibble()] with computed odds ratios and associated
#' information
#' @export
#'
#' @examples
#' tribble(
#'   ~admit, ~rank, ~n,
#'   0, "1", 28,
#'   0, "2", 97,
#'   0, "3", 93,
#'   0, "4", 55,
#'   1, "1", 33,
#'   1, "2", 54,
#'   1, "3", 28,
#'   1, "4", 12
#' ) %>%
#'   uncount(n) %>%
#'   lp_riskratio(admit, rank)
lp_riskratio <- function(data,
                         response,
                         class,
                         conf.int = TRUE,
                         conf.level = 0.95,
                         descriptives = TRUE,
                         ...) {
  response <- rlang::ensym(response)
  class <- rlang::ensym(class)

  classes <- data %>%
    dplyr::pull(!!class) %>%
    factor() %>%
    levels()

  seq_along(classes) %>%
    utils::combn(2) %>%
    apply(2, I, simplify = FALSE) %>%
    lapply(function(p) {
      prefix_cols <- dplyr::tibble(
        "response" = rlang::as_string(response),
        "class"    = rlang::as_string(class),
        "class1"   = classes[p][1],
        "class2"   = classes[p][2]
      )

      s <- data %>%
        dplyr::select(!!response, !!class) %>%
        dplyr::filter(!!class %in% classes[p]) %>%
        filter_complete.cases()

      q <- rlang::quo(
        stats::glm(!!response ~ !!class, stats::binomial(link = "log"), s, ...)
      )

      inferential_cols <- NULL

      message_cols <- dplyr::tibble(
        message.class = NA_character_,
        message = NA_character_
      )

      inferential_cols <- tryCatch(
        {
          rlang::eval_tidy(q)
        },
        message = function(cond) {
          message_cols$message.class <<- "message"
          message_cols$message <<- cond$message
          suppressMessages(rlang::eval_tidy(q))
        },
        warning = function(cond) {
          message_cols$message.class <<- "warning"
          message_cols$message <<- cond$message
          suppressWarnings(rlang::eval_tidy(q))
        },
        error = function(cond) {
          message_cols$message.class <<- "error"
          message_cols$message <<- cond$message
          NULL
        }
      )

      inferential_cols <- inferential_cols %>%
        broom::tidy(
          conf.int = conf.int,
          conf.level = conf.level,
          exponentiate = TRUE
        ) %>%
        dplyr::slice(2) %>%
        augment_with_p.flag()

      descriptive_cols <-
        if (descriptives) {
          s %>%
            dplyr::count(!!response, !!class) %>%
            tidyr::complete(!!response, !!class, fill = list(n = 0)) %>%
            tidyr::pivot_wider(names_from = -"n", values_from = "n") %>%
            dplyr::rename_with(~ c("n.tn", "n.fp", "n.fn", "n.tp"))
        } else {
          NULL
        }

      dplyr::bind_cols(
        prefix_cols,
        inferential_cols,
        descriptive_cols,
        message_cols
      )
    }) %>%
    dplyr::bind_rows()
}

#' Generalized linear mixed-effects models fit to all pairs of group predictor
#'
#' @param data data frame containing the variables named in formula
#' @param formula a two-sided linear formula object describing both the
#' fixed-effects and random-effects part of the model, with the response on the
#' left of a ~ operator and the terms, separated by + operators, on the right.
#' Random-effects terms are distinguished by vertical bars ("|") separating
#' expressions for design matrices from grouping factors.
#' @param response response variable
#' @param class classification variable
#' @param family a GLM family, see \code{\link[stats:glm]{glm}} and
#' \code{\link[stats:family]{family}}.
#' @param descriptives logical to add raw descriptive statistics or not
#' @param exponentiate  whether to exponentiate the fixed-effect coefficient
#' estimates and confidence intervals (common for logistic regression); if
#' \code{TRUE}, also scales the standard errors by the exponentiated
#' coefficient, transforming them to the new scale
#' @param conf.int whether to include a confidence interval
#' @param conf.level confidence level for CI
#' @param conf.method method for computing confidence intervals
#' (see \code{lme4::confint.merMod})
#' @param profile pre-computed profile object, for speed when using
#' \code{conf.method="profile"}
#' @param ... additional arguments passed to other methods
#'
#' @return a [dplyr::tibble()] with one row for each
#' @export
#'
#' @examples
#' lp_glmer(
#'   lme4::cake,
#'   "angle ~ recipe * temperature + (1 | recipe:replicate)",
#'   angle,
#'   recipe,
#'   poisson
#' )
lp_glmer <- function(data,
                     formula,
                     response,
                     class,
                     family = stats::gaussian(),
                     descriptives = TRUE,
                     exponentiate = FALSE,
                     conf.int = FALSE,
                     conf.level = 0.95,
                     conf.method = "Wald",
                     profile = NULL,
                     ...) {
  response <- rlang::ensym(response)
  class <- rlang::ensym(class)

  classes <- data %>%
    dplyr::pull(!!class) %>%
    factor() %>%
    levels()

  seq_along(classes) %>%
    utils::combn(2) %>%
    apply(2, I, simplify = FALSE) %>%
    lapply(function(p) {
      prefix_cols <- dplyr::tibble(
        "response" = rlang::as_string(response),
        "class"    = rlang::as_string(class),
        "class1"   = classes[p][1],
        "class2"   = classes[p][2]
      )

      s <- data %>%
        dplyr::select(dplyr::all_of(formula_terms(data, formula))) %>%
        dplyr::filter(!!class %in% classes[p]) %>%
        filter_complete.cases()

      q <- rlang::quo(
        lme4::glmer(
          stats::as.formula(formula),
          s,
          family,
          ...
        )
      )

      inferential_cols <- NULL

      message_cols <- dplyr::tibble(
        message.class = NA_character_,
        message = NA_character_
      )

      inferential_cols <- tryCatch(
        {
          rlang::eval_tidy(q)
        },
        message = function(cond) {
          message_cols$message.class <<- "message"
          message_cols$message <<- cond$message
          suppressMessages(rlang::eval_tidy(q))
        },
        warning = function(cond) {
          message_cols$message.class <<- "warning"
          message_cols$message <<- cond$message
          suppressWarnings(rlang::eval_tidy(q))
        },
        error = function(cond) {
          message_cols$message.class <<- "error"
          message_cols$message <<- cond$message
          NULL
        }
      )

      inferential_cols <- inferential_cols %>%
        broom.mixed::tidy(
          exponentiate = exponentiate,
          conf.int = conf.int,
          conf.level = conf.level,
          conf.method = conf.method,
          profile = profile,
          ...
        ) %>%
        dplyr::filter(.data[["term"]] == rlang::as_string(class)) %>%
        augment_with_p.flag()

      descriptive_cols <-
        if (descriptives) {
          s %>%
            dplyr::group_by(!!class) %>%
            dplyr::summarize(
              n = dplyr::n(),
              m = mean(!!response),
              s = stats::sd(!!response),
            ) %>%
            tidyr::pivot_wider(
              names_from = !!class,
              values_from = -!!class
            ) %>%
            dplyr::rename_with(~ c("n1", "n2", "m1", "m2", "s1", "s2"))
        } else {
          NULL
        }

      dplyr::bind_cols(
        prefix_cols,
        inferential_cols,
        descriptive_cols,
        message_cols
      )
    }) %>%
    dplyr::bind_rows()
}
