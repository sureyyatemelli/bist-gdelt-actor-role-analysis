# ============================================================
# 05_kantil_regresyon.R
# Quantile regression: full tau grid, reaction/prediction,
# exact-sample centering, percentile marginal effects,
# bootstrap inference and internally consistent tables/figures.
#
# IMPORTANT:
# The previous "nid" standard-error estimator generated
# "non-positive fis" warnings. Therefore inference is switched
# to quantreg bootstrap standard errors.
# ============================================================

required_packages <- c(
  "readr", "dplyr", "tidyr", "quantreg", "ggplot2", "jsonlite"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  stop("Eksik R paketleri: ", paste(missing_packages, collapse = ", "))
}

source(file.path("code", "00_config.R"))
source(file.path("code", "01_utils.R"))

ROOT <- find_project_root()
ensure_dirs(ROOT)

panel_path <- file.path(ROOT, "data", "processed", "panel_gunluk.csv")
coef_path <- file.path(ROOT, "results", "tables", "kantil_katsayilari.csv")
me_path <- file.path(ROOT, "results", "tables", "marjinal_etki_persentiller.csv")
fig_coef <- file.path(ROOT, "figures", "kantil_katsayi_yolu.png")
fig_me <- file.path(ROOT, "figures", "marjinal_etki.png")
diag_path <- file.path(ROOT, "results", "metrics", "kantil_model_ozeti.json")

dat <- readr::read_csv(panel_path, show_col_types = FALSE) |>
  dplyr::arrange(date)

taus <- seq(0.05, 0.95, by = 0.05)
percentiles <- c(0.10, 0.25, 0.50, 0.75, 0.90)

# Reproducible bootstrap
BOOT_R <- 500L
BOOT_SEED <- 20260902L

# ============================================================
# 1. LAGGED POLITICAL VARIABLES
# ============================================================

dat <- dat |>
  dplyr::mutate(
    goldstein_a1_lag1 = dplyr::lag(goldstein_a1),
    goldstein_a2_lag1 = dplyr::lag(goldstein_a2),
    event_count_a1_lag1 = dplyr::lag(event_count_a1),
    event_count_a2_lag1 = dplyr::lag(event_count_a2)
  )

# ============================================================
# 2. SUMMARY EXTRACTOR
# ============================================================

extract_rqs_summary <- function(sum_obj, role, timing) {

  out <- lapply(seq_along(sum_obj), function(i) {

    s <- sum_obj[[i]]
    cf <- s$coefficients

    est <- cf[, 1]
    se <- cf[, 2]

    stat <- if (ncol(cf) >= 3) cf[, 3] else est / se
    p <- if (ncol(cf) >= 4) {
      cf[, 4]
    } else {
      2 * stats::pnorm(abs(stat), lower.tail = FALSE)
    }

    data.frame(
      role = role,
      timing = timing,
      tau = taus[i],
      term = rownames(cf),
      estimate_raw = as.numeric(est),
      std_error_raw = as.numeric(se),
      statistic = as.numeric(stat),
      p_value = as.numeric(p),
      conf_low_raw = as.numeric(est - 1.96 * se),
      conf_high_raw = as.numeric(est + 1.96 * se),
      stringsAsFactors = FALSE
    )
  })

  dplyr::bind_rows(out)
}

# ============================================================
# 3. QUANTILE BLOCK
# ============================================================

fit_quantile_block <- function(
    data,
    role = c("Actor1", "Actor2"),
    timing = c("reaction", "prediction")
) {

  role <- match.arg(role)
  timing <- match.arg(timing)

  suffix <- if (role == "Actor1") "a1" else "a2"

  if (timing == "reaction") {
    goldstein_var <- paste0("goldstein_", suffix)
    count_var <- paste0("event_count_", suffix)
  } else {
    goldstein_var <- paste0("goldstein_", suffix, "_lag1")
    count_var <- paste0("event_count_", suffix, "_lag1")
  }

  # Exact complete-case sample FIRST
  needed <- c(
    "date",
    "return_bist",
    goldstein_var,
    count_var,
    "return_vix",
    "return_usdtry",
    "return_bist_lag1"
  )

  d <- data |>
    dplyr::select(dplyr::all_of(needed)) |>
    tidyr::drop_na()

  # Center event count WITHIN EXACT ESTIMATION SAMPLE
  sample_event_mean <- mean(d[[count_var]])
  d$event_count_c <- d[[count_var]] - sample_event_mean
  center_check <- mean(d$event_count_c)

  f <- stats::as.formula(
    paste0(
      "return_bist ~ ",
      goldstein_var,
      " * event_count_c",
      " + return_vix",
      " + return_usdtry",
      " + return_bist_lag1"
    )
  )

  fit <- quantreg::rq(
    f,
    data = d,
    tau = taus,
    method = "br"
  )

  # ----------------------------------------------------------
  # BOOTSTRAP INFERENCE
  #
  # "nid" was abandoned because the sparsity-density estimate
  # produced many "non-positive fis" warnings.
  #
  # The same bootstrap summary object is used for BOTH tables
  # and figures, ensuring exact internal consistency.
  # ----------------------------------------------------------

  set.seed(BOOT_SEED)

  sum_obj <- summary(
    fit,
    se = "boot",
    R = BOOT_R,
    bsmethod = "xy",
    covariance = TRUE
  )

  coef_tab <- extract_rqs_summary(sum_obj, role, timing)

  # Event-count percentiles in the SAME estimation sample
  q_raw <- stats::quantile(
    d[[count_var]],
    probs = percentiles,
    na.rm = TRUE,
    names = FALSE,
    type = 7
  )

  q_centered <- q_raw - sample_event_mean

  me_rows <- list()

  for (i in seq_along(sum_obj)) {

    s <- sum_obj[[i]]
    cf <- s$coefficients
    rn <- rownames(cf)

    main_name <- goldstein_var

    int_candidates <- c(
      paste0(goldstein_var, ":event_count_c"),
      paste0("event_count_c:", goldstein_var)
    )

    int_name <- int_candidates[int_candidates %in% rn][1]

    if (!(main_name %in% rn) || is.na(int_name)) next

    b <- cf[, 1]
    covm <- s$cov

    for (j in seq_along(percentiles)) {

      ec <- q_centered[j]

      marginal_effect <- (
        b[main_name] +
          b[int_name] * ec
      )

      marginal_se <- NA_real_

      if (
        !is.null(covm) &&
        main_name %in% rownames(covm) &&
        int_name %in% rownames(covm)
      ) {

        vv <- covm[
          c(main_name, int_name),
          c(main_name, int_name),
          drop = FALSE
        ]

        a <- c(1, ec)

        marginal_se <- sqrt(
          as.numeric(t(a) %*% vv %*% a)
        )
      }

      me_rows[[length(me_rows) + 1]] <- data.frame(
        role = role,
        timing = timing,
        tau = taus[i],
        event_count_percentile = percentiles[j],
        event_count_raw = q_raw[j],
        event_count_centered = ec,
        marginal_effect_raw = marginal_effect,
        std_error_raw = marginal_se,
        conf_low_raw = ifelse(
          is.na(marginal_se),
          NA,
          marginal_effect - 1.96 * marginal_se
        ),
        conf_high_raw = ifelse(
          is.na(marginal_se),
          NA,
          marginal_effect + 1.96 * marginal_se
        ),
        N = nrow(d),
        event_count_sample_mean = sample_event_mean,
        centered_mean_check = center_check,
        stringsAsFactors = FALSE
      )
    }
  }

  list(
    fit = fit,
    summary = sum_obj,
    coef = coef_tab,
    marginal = dplyr::bind_rows(me_rows),
    N = nrow(d),
    center_check = center_check,
    sample_event_mean = sample_event_mean,
    main_term = goldstein_var
  )
}

# ============================================================
# 4. ESTIMATE FOUR BLOCKS
# ============================================================

blocks <- list(
  A_reaction = fit_quantile_block(dat, "Actor1", "reaction"),
  A_prediction = fit_quantile_block(dat, "Actor1", "prediction"),
  B_reaction = fit_quantile_block(dat, "Actor2", "reaction"),
  B_prediction = fit_quantile_block(dat, "Actor2", "prediction")
)

# ============================================================
# 5. COEFFICIENT TABLE
# ============================================================

coef_all <- dplyr::bind_rows(
  lapply(blocks, `[[`, "coef")
) |>
  dplyr::mutate(
    estimate = scale_return_for_output(estimate_raw),
    std_error = scale_return_for_output(std_error_raw),
    conf_low = scale_return_for_output(conf_low_raw),
    conf_high = scale_return_for_output(conf_high_raw),
    return_unit = return_unit_label,
    inference = paste0("bootstrap_xy_R", BOOT_R)
  )

readr::write_csv(coef_all, coef_path)

# ============================================================
# 6. MARGINAL EFFECT TABLE
# ============================================================

me_all <- dplyr::bind_rows(
  lapply(blocks, `[[`, "marginal")
) |>
  dplyr::mutate(
    marginal_effect =
      scale_return_for_output(marginal_effect_raw),
    std_error =
      scale_return_for_output(std_error_raw),
    conf_low =
      scale_return_for_output(conf_low_raw),
    conf_high =
      scale_return_for_output(conf_high_raw),
    return_unit = return_unit_label,
    inference = paste0("bootstrap_xy_R", BOOT_R)
  )

readr::write_csv(me_all, me_path)

# ============================================================
# 7. COEFFICIENT-PATH FIGURE
# ============================================================

goldstein_path <- coef_all |>
  dplyr::filter(
    (role == "Actor1" &
       timing == "reaction" &
       term == "goldstein_a1") |

    (role == "Actor1" &
       timing == "prediction" &
       term == "goldstein_a1_lag1") |

    (role == "Actor2" &
       timing == "reaction" &
       term == "goldstein_a2") |

    (role == "Actor2" &
       timing == "prediction" &
       term == "goldstein_a2_lag1")
  )

p1 <- ggplot2::ggplot(
  goldstein_path,
  ggplot2::aes(
    x = tau,
    y = estimate,
    group = interaction(role, timing),
    linetype = timing
  )
) +
  ggplot2::geom_ribbon(
    ggplot2::aes(
      ymin = conf_low,
      ymax = conf_high,
      group = interaction(role, timing)
    ),
    alpha = 0.15
  ) +
  ggplot2::geom_line() +
  ggplot2::geom_point(size = 1.5) +
  ggplot2::facet_wrap(
    ~ role,
    scales = "free_y"
  ) +
  ggplot2::geom_hline(
    yintercept = 0,
    linetype = 3
  ) +
  ggplot2::labs(
    x = "Quantile (tau)",
    y = paste0(
      "Goldstein coefficient (",
      return_unit_label,
      ")"
    ),
    linetype = "Timing",
    title = "Quantile coefficient path: reaction vs prediction"
  ) +
  ggplot2::theme_minimal(base_size = 12)

ggplot2::ggsave(
  fig_coef,
  p1,
  width = 9,
  height = 5.5,
  dpi = 300
)

# ============================================================
# 8. MARGINAL-EFFECT FIGURE
# ============================================================

p2 <- ggplot2::ggplot(
  me_all,
  ggplot2::aes(
    x = tau,
    y = marginal_effect,
    group = factor(event_count_percentile),
    linetype = factor(event_count_percentile)
  )
) +
  ggplot2::geom_line() +
  ggplot2::facet_grid(
    role ~ timing,
    scales = "free_y"
  ) +
  ggplot2::geom_hline(
    yintercept = 0,
    linetype = 3
  ) +
  ggplot2::labs(
    x = "Quantile (tau)",
    y = paste0(
      "Marginal effect (",
      return_unit_label,
      ")"
    ),
    linetype = "Event-count percentile",
    title = "Marginal effect of Goldstein score across event intensity"
  ) +
  ggplot2::theme_minimal(base_size = 11)

ggplot2::ggsave(
  fig_me,
  p2,
  width = 10,
  height = 7,
  dpi = 300
)

# ============================================================
# 9. MODEL SUMMARY / DIAGNOSTICS
# ============================================================

model_summary <- list(
  inference = list(
    method = "quantreg bootstrap",
    bsmethod = "xy",
    R = BOOT_R,
    seed = BOOT_SEED,
    reason = paste(
      "Bootstrap inference was used because nid standard errors",
      "generated non-positive sparsity-density estimates."
    )
  ),
  reaction_definition = "Goldstein_t -> BIST return_t",
  prediction_definition = "Goldstein_t-1 -> BIST return_t",
  blocks = list(
    A_reaction = list(
      N = blocks$A_reaction$N,
      event_count_mean = blocks$A_reaction$sample_event_mean,
      centered_mean_check = blocks$A_reaction$center_check
    ),
    A_prediction = list(
      N = blocks$A_prediction$N,
      event_count_mean = blocks$A_prediction$sample_event_mean,
      centered_mean_check = blocks$A_prediction$center_check
    ),
    B_reaction = list(
      N = blocks$B_reaction$N,
      event_count_mean = blocks$B_reaction$sample_event_mean,
      centered_mean_check = blocks$B_reaction$center_check
    ),
    B_prediction = list(
      N = blocks$B_prediction$N,
      event_count_mean = blocks$B_prediction$sample_event_mean,
      centered_mean_check = blocks$B_prediction$center_check
    )
  )
)

json_write(
  model_summary,
  diag_path
)

# ============================================================
# 10. CONSOLE OUTPUT
# ============================================================

cat("\nKantil katsayilari:", coef_path, "\n")
cat("Marjinal etkiler:", me_path, "\n")
cat("Katsayi yolu grafigi:", fig_coef, "\n")
cat("Marjinal etki grafigi:", fig_me, "\n")
cat("Model ozeti:", diag_path, "\n\n")

cat("Reaction   = Goldstein_t   -> return_t\n")
cat("Prediction = Goldstein_t-1 -> return_t\n")
cat("Inference  = bootstrap (xy), R =", BOOT_R, "\n\n")

for (nm in names(blocks)) {
  cat(
    nm,
    "N =", blocks[[nm]]$N,
    "| event-count mean =",
    round(blocks[[nm]]$sample_event_mean, 6),
    "| centered mean =",
    format(
      blocks[[nm]]$center_check,
      scientific = TRUE
    ),
    "\n"
  )
}

cat(
  "\nCentering check: values around 1e-12 or smaller are numerical zero.\n"
)

check_outputs(ROOT)
