#' @importFrom ggplot2 autoplot
NULL

#' Plot an roptuna study
#'
#' @param object A `Study` R6 object.
#' @param type One of `"history"`, `"parallel_coordinate"`, `"param_importance"`.
#' @param ... Unused.
#' @return A `ggplot2` object.
#' @export
autoplot.Study <- function(object, type = "history", ...) {
  type <- match.arg(type, c("history", "parallel_coordinate", "param_importance"))
  switch(type,
    history             = .plot_history(object),
    parallel_coordinate = .plot_parallel(object),
    param_importance    = .plot_importance(object)
  )
}

.completed_df <- function(study) {
  trials  <- study$trials
  complete <- Filter(function(t) t$state == "complete", trials)
  if (length(complete) == 0) stop("No completed trials to plot.")

  rows <- lapply(complete, function(t) {
    row <- c(list(trial_number = t$number, value = t$value), t$params)
    as.data.frame(row, stringsAsFactors = FALSE)
  })
  Reduce(function(a, b) {
    missing_a <- setdiff(names(b), names(a))
    missing_b <- setdiff(names(a), names(b))
    for (col in missing_a) a[[col]] <- NA
    for (col in missing_b) b[[col]] <- NA
    rbind(a[, union(names(a), names(b))], b[, union(names(a), names(b))])
  }, rows)
}

.plot_history <- function(study) {
  df <- .completed_df(study)
  df <- df[order(df$trial_number), ]

  if (study$direction == "minimize") {
    df$best_so_far <- cummin(df$value)
  } else {
    df$best_so_far <- cummax(df$value)
  }

  ggplot2::ggplot(df, ggplot2::aes(x = .data$trial_number)) +
    ggplot2::geom_point(ggplot2::aes(y = .data$value), alpha = 0.4, size = 1.5) +
    ggplot2::geom_line(ggplot2::aes(y = .data$best_so_far),
                       colour = "#2171b5", linewidth = 1) +
    ggplot2::labs(
      title = paste("Optimization history --", study$study_name),
      x = "Trial number", y = "Objective value"
    ) +
    ggplot2::theme_minimal()
}

.plot_parallel <- function(study) {
  df <- .completed_df(study)
  param_cols <- setdiff(names(df), c("trial_number", "value"))

  if (length(param_cols) == 0)
    stop("No parameters to plot in parallel coordinates.")

  df_norm <- df
  for (col in param_cols) {
    x <- df[[col]]
    if (is.numeric(x)) {
      rng <- range(x, na.rm = TRUE)
      df_norm[[col]] <- if (diff(rng) == 0) 0.5 else (x - rng[1]) / diff(rng)
    } else {
      lvls <- unique(x)
      df_norm[[col]] <- (match(x, lvls) - 1) / max(length(lvls) - 1, 1)
    }
  }

  long <- stats::reshape(df_norm[, c("trial_number", "value", param_cols)],
    varying   = param_cols,
    v.names   = "norm_val",
    timevar   = "param",
    times     = param_cols,
    direction = "long"
  )

  ggplot2::ggplot(long,
    ggplot2::aes(x = .data$param, y = .data$norm_val,
                 group = .data$trial_number,
                 colour = .data$value)) +
    ggplot2::geom_line(alpha = 0.5) +
    ggplot2::scale_colour_viridis_c(
      direction = if (study$direction == "minimize") 1 else -1) +
    ggplot2::labs(
      title  = "Parallel coordinates -- hyperparameter values",
      x      = NULL, y = "Normalised value", colour = "Objective"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, hjust = 1))
}

.plot_importance <- function(study) {
  df <- .completed_df(study)
  param_cols <- setdiff(names(df), c("trial_number", "value"))

  if (length(param_cols) == 0)
    stop("No parameters to compute importance for.")

  df_enc <- df[, c(param_cols, "value"), drop = FALSE]
  for (col in param_cols) {
    if (!is.numeric(df_enc[[col]]))
      df_enc[[col]] <- as.integer(factor(df_enc[[col]]))
  }

  fit <- rpart::rpart(value ~ ., data = df_enc, method = "anova",
                      control = rpart::rpart.control(maxdepth = 4))

  imp <- fit$variable.importance
  if (is.null(imp) || length(imp) == 0) {
    imp <- stats::setNames(rep(1, length(param_cols)), param_cols)
  }
  imp_df <- data.frame(
    param      = names(imp),
    importance = as.numeric(imp) / sum(imp),
    stringsAsFactors = FALSE
  )
  imp_df <- imp_df[order(imp_df$importance, decreasing = TRUE), ]

  ggplot2::ggplot(imp_df,
    ggplot2::aes(x = stats::reorder(.data$param, .data$importance),
                 y = .data$importance)) +
    ggplot2::geom_col(fill = "#2171b5") +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = "Parameter importance (decision tree)",
      x = NULL, y = "Relative importance"
    ) +
    ggplot2::theme_minimal()
}
