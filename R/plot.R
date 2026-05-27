#' @importFrom ggplot2 autoplot
NULL

#' Plot an roptuna study
#'
#' @param object A `Study` R6 object.
#' @param type One of `"history"`, `"parallel_coordinate"`, `"param_importance"`,
#'   `"intermediate_values"`, `"contour"`, `"slice"`, `"edf"`.
#' @param params For `"contour"`: character vector of exactly two parameter names.
#'   For `"slice"`: character vector of parameter names to include (default: all).
#' @param ... Unused.
#' @return A `ggplot2` object.
#' @examples
#' study <- create_study(direction = "minimize", sampler = tpe_sampler(seed = 1L))
#' study$optimize(function(trial) {
#'   x <- trial$suggest_float("x", -5, 5)
#'   y <- trial$suggest_int("y", 1L, 3L)
#'   x^2 + y
#' }, n_trials = 10)
#' ggplot2::autoplot(study, type = "history")
#' @export
autoplot.Study <- function(object, type = "history", params = NULL, ...) {
  type <- match.arg(type, c("history", "parallel_coordinate", "param_importance",
                            "intermediate_values", "contour", "slice", "edf",
                            "pareto_front"))
  switch(type,
    history              = .plot_history(object),
    parallel_coordinate  = .plot_parallel(object),
    param_importance     = .plot_importance(object),
    intermediate_values  = .plot_intermediate(object),
    contour              = .plot_contour(object, params),
    slice                = .plot_slice(object, params),
    edf                  = .plot_edf(object),
    pareto_front         = .plot_pareto_front(object)
  )
}

# ── Helpers ──────────────────────────────────────────────────────────────────

.completed_df <- function(study) {
  trials   <- study$trials
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

# ── Plot types ────────────────────────────────────────────────────────────────

.plot_history <- function(study) {
  df <- .completed_df(study)
  df <- df[order(df$trial_number), ]

  df$best_so_far <- if (study$direction == "minimize")
    cummin(df$value) else cummax(df$value)

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
  df         <- .completed_df(study)
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
    varying   = param_cols, v.names = "norm_val",
    timevar   = "param",    times    = param_cols, direction = "long")

  ggplot2::ggplot(long,
    ggplot2::aes(x = .data$param, y = .data$norm_val,
                 group = .data$trial_number, colour = .data$value)) +
    ggplot2::geom_line(alpha = 0.5) +
    ggplot2::scale_colour_viridis_c(
      direction = if (study$direction == "minimize") 1 else -1) +
    ggplot2::labs(
      title  = "Parallel coordinates -- hyperparameter values",
      x = NULL, y = "Normalised value", colour = "Objective"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 30, hjust = 1))
}

.plot_importance <- function(study) {
  if (!requireNamespace("rpart", quietly = TRUE))
    stop("Package 'rpart' is required for param_importance plots.")
  df         <- .completed_df(study)
  param_cols <- setdiff(names(df), c("trial_number", "value"))
  if (length(param_cols) == 0) stop("No parameters to compute importance for.")

  df_enc <- df[, c(param_cols, "value"), drop = FALSE]
  for (col in param_cols)
    if (!is.numeric(df_enc[[col]]))
      df_enc[[col]] <- as.integer(factor(df_enc[[col]]))

  fit <- rpart::rpart(value ~ ., data = df_enc, method = "anova",
                      control = rpart::rpart.control(maxdepth = 4))
  imp <- fit$variable.importance
  if (is.null(imp) || length(imp) == 0)
    imp <- stats::setNames(rep(1, length(param_cols)), param_cols)

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
    ggplot2::labs(title = "Parameter importance (decision tree)", x = NULL,
                  y = "Relative importance") +
    ggplot2::theme_minimal()
}

.plot_intermediate <- function(study) {
  trials <- study$trials
  has_iv <- Filter(function(t) length(t$intermediate_values) > 0, trials)
  if (length(has_iv) == 0)
    stop("No trials with intermediate values to plot.")

  rows <- do.call(rbind, lapply(has_iv, function(t) {
    steps <- as.integer(names(t$intermediate_values))
    vals  <- unlist(t$intermediate_values)
    data.frame(
      trial_number = t$number,
      step         = steps,
      value        = vals,
      final_value  = if (t$state == "complete") t$value else NA_real_,
      state        = t$state,
      stringsAsFactors = FALSE
    )
  }))

  ggplot2::ggplot(rows, ggplot2::aes(
    x = .data$step, y = .data$value,
    group = .data$trial_number,
    colour = .data$final_value)) +
    ggplot2::geom_line(alpha = 0.7) +
    ggplot2::geom_point(size = 1, alpha = 0.5) +
    ggplot2::scale_colour_viridis_c(na.value = "grey70",
      direction = if (study$direction == "minimize") 1 else -1) +
    ggplot2::labs(
      title  = "Intermediate values per trial",
      x = "Step", y = "Intermediate value", colour = "Final value"
    ) +
    ggplot2::theme_minimal()
}

.plot_contour <- function(study, params = NULL) {
  df         <- .completed_df(study)
  param_cols <- setdiff(names(df), c("trial_number", "value"))

  if (!is.null(params)) {
    param_cols <- intersect(params, param_cols)
  }
  if (length(param_cols) < 2)
    stop("Need at least 2 parameters for a contour plot. ",
         "Use the `params` argument to specify two parameter names.")
  if (length(param_cols) > 2) param_cols <- param_cols[1:2]

  p1 <- param_cols[1]; p2 <- param_cols[2]

  ggplot2::ggplot(df, ggplot2::aes(
    x = .data[[p1]], y = .data[[p2]], colour = .data$value)) +
    ggplot2::geom_point(size = 2.5, alpha = 0.8) +
    ggplot2::scale_colour_viridis_c(
      direction = if (study$direction == "minimize") 1 else -1) +
    ggplot2::labs(
      title  = paste("Contour:", p1, "vs", p2),
      x = p1, y = p2, colour = "Objective"
    ) +
    ggplot2::theme_minimal()
}

.plot_slice <- function(study, params = NULL) {
  df         <- .completed_df(study)
  param_cols <- setdiff(names(df), c("trial_number", "value"))
  if (!is.null(params)) param_cols <- intersect(params, param_cols)
  if (length(param_cols) == 0) stop("No parameters to plot.")

  long <- do.call(rbind, lapply(param_cols, function(p) {
    data.frame(
      param       = p,
      param_value = as.character(df[[p]]),
      param_num   = if (is.numeric(df[[p]])) df[[p]] else
                      as.numeric(factor(df[[p]])),
      objective   = df$value,
      stringsAsFactors = FALSE
    )
  }))

  ggplot2::ggplot(long, ggplot2::aes(x = .data$param_num, y = .data$objective)) +
    ggplot2::geom_point(alpha = 0.6, colour = "#2171b5") +
    ggplot2::geom_smooth(method = "loess", formula = y ~ x,
                         se = FALSE, colour = "#c0392b", linewidth = 0.8) +
    ggplot2::facet_wrap(~ .data$param, scales = "free_x") +
    ggplot2::labs(
      title = "Slice plot: each parameter vs. objective",
      x = "Parameter value", y = "Objective"
    ) +
    ggplot2::theme_minimal()
}

.plot_pareto_front <- function(study) {
  trials   <- study$trials
  complete <- Filter(function(t) t$state == "complete" && !is.null(t$values), trials)
  if (length(complete) == 0) stop("No completed multi-objective trials to plot.")

  dirs <- study$directions
  if (length(dirs) < 2)
    stop("Pareto front plot requires at least 2 objectives (use directions= in create_study).")

  best     <- study$best_trials
  best_ids <- sapply(best, `[[`, "trial_id")

  df <- do.call(rbind, lapply(complete, function(t) {
    data.frame(
      trial_id = t$trial_id,
      obj1     = t$values[[1L]],
      obj2     = t$values[[2L]],
      pareto   = t$trial_id %in% best_ids,
      stringsAsFactors = FALSE
    )
  }))

  ggplot2::ggplot(df, ggplot2::aes(
    x = .data$obj1, y = .data$obj2, colour = .data$pareto)) +
    ggplot2::geom_point(size = 2.5, alpha = 0.8) +
    ggplot2::scale_colour_manual(
      values = c("FALSE" = "grey60", "TRUE" = "#c0392b"),
      labels = c("FALSE" = "Dominated", "TRUE" = "Pareto front")) +
    ggplot2::labs(
      title  = paste("Pareto front --", study$study_name),
      x = paste0("Objective 1 (", dirs[[1]], ")"),
      y = paste0("Objective 2 (", dirs[[2]], ")"),
      colour = NULL
    ) +
    ggplot2::theme_minimal()
}

.plot_edf <- function(study) {
  trials <- Filter(function(t) t$state == "complete", study$trials)
  if (length(trials) == 0) stop("No completed trials to plot.")

  vals <- sort(sapply(trials, `[[`, "value"))
  n    <- length(vals)
  df   <- data.frame(value = vals, ecdf = seq_len(n) / n)

  ggplot2::ggplot(df, ggplot2::aes(x = .data$value, y = .data$ecdf)) +
    ggplot2::geom_step(colour = "#2171b5", linewidth = 1) +
    ggplot2::labs(
      title = "Empirical distribution of objective values",
      x = "Objective value", y = "Cumulative proportion"
    ) +
    ggplot2::theme_minimal()
}
