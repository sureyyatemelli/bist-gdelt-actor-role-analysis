# ============================================================
# 04b_volatilite_modelleri.R
# Absolute-return volatility models for Actor1 / Actor2
# Reaction + prediction, HAC Newey-West, pooled Wald, SUR
# ============================================================

required_packages <- c(
  "readr", "dplyr", "tidyr", "sandwich", "lmtest",
  "car", "systemfit", "jsonlite"
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
table_path <- file.path(ROOT, "results", "tables", "volatilite_modelleri.csv")
json_path <- file.path(ROOT, "results", "metrics", "volatilite_aktor_rolu_esitlik_testi.json")

dat <- readr::read_csv(panel_path, show_col_types = FALSE) |>
  dplyr::arrange(date) |>
  dplyr::mutate(
    abs_return_bist_lag1 = dplyr::lag(abs_return_bist),
    goldstein_a1_lag1 = dplyr::lag(goldstein_a1),
    goldstein_a2_lag1 = dplyr::lag(goldstein_a2),
    event_count_a1_lag1 = dplyr::lag(event_count_a1),
    event_count_a2_lag1 = dplyr::lag(event_count_a2)
  )

fit_role_vol <- function(data, role = c("Actor1", "Actor2"),
                         timing = c("reaction", "prediction")) {

  role <- match.arg(role)
  timing <- match.arg(timing)
  suffix <- if (role == "Actor1") "a1" else "a2"

  if (timing == "reaction") {
    g <- paste0("goldstein_", suffix)
    e <- paste0("event_count_", suffix)
  } else {
    g <- paste0("goldstein_", suffix, "_lag1")
    e <- paste0("event_count_", suffix, "_lag1")
  }

  needed <- c(
    "abs_return_bist", g, e,
    "return_vix", "return_usdtry", "abs_return_bist_lag1"
  )

  d <- data |>
    dplyr::select(dplyr::all_of(needed)) |>
    tidyr::drop_na()

  event_mean <- mean(d[[e]])
  d$event_count_c <- d[[e]] - event_mean
  center_check <- mean(d$event_count_c)

  f <- stats::as.formula(
    paste0(
      "abs_return_bist ~ ", g,
      " + event_count_c",
      " + return_vix",
      " + return_usdtry",
      " + abs_return_bist_lag1"
    )
  )

  m <- stats::lm(f, data = d)
  tab <- hac_coeftable(m) |>
    dplyr::mutate(
      role = role,
      timing = timing,
      model = "role_specific",
      outcome = "abs_return_bist",
      N = stats::nobs(m),
      event_count_sample_mean = event_mean,
      centered_mean_check = center_check,
      estimate_reported = scale_return_for_output(estimate),
      std_error_reported = scale_return_for_output(std_error),
      return_unit = return_unit_label
    )

  list(
    model = m,
    data = d,
    table = tab,
    N = nrow(d),
    event_mean = event_mean,
    center_check = center_check
  )
}

A_reaction <- fit_role_vol(dat, "Actor1", "reaction")
B_reaction <- fit_role_vol(dat, "Actor2", "reaction")
A_prediction <- fit_role_vol(dat, "Actor1", "prediction")
B_prediction <- fit_role_vol(dat, "Actor2", "prediction")

role_tabs <- dplyr::bind_rows(
  A_reaction$table, B_reaction$table,
  A_prediction$table, B_prediction$table
)

fit_pooled_vol <- function(data, timing = c("reaction", "prediction")) {

  timing <- match.arg(timing)

  if (timing == "reaction") {
    g1 <- "goldstein_a1"; g2 <- "goldstein_a2"
    e1 <- "event_count_a1"; e2 <- "event_count_a2"
  } else {
    g1 <- "goldstein_a1_lag1"; g2 <- "goldstein_a2_lag1"
    e1 <- "event_count_a1_lag1"; e2 <- "event_count_a2_lag1"
  }

  needed <- c(
    "abs_return_bist", "return_vix", "return_usdtry",
    "abs_return_bist_lag1", g1, g2, e1, e2
  )

  d0 <- data |>
    dplyr::select(dplyr::all_of(needed)) |>
    tidyr::drop_na()

  d1 <- d0 |>
    dplyr::transmute(
      abs_return_bist, return_vix, return_usdtry, abs_return_bist_lag1,
      role_dummy = 1,
      goldstein = .data[[g1]],
      event_count = .data[[e1]]
    )

  d2 <- d0 |>
    dplyr::transmute(
      abs_return_bist, return_vix, return_usdtry, abs_return_bist_lag1,
      role_dummy = 0,
      goldstein = .data[[g2]],
      event_count = .data[[e2]]
    )

  d <- dplyr::bind_rows(d1, d2)
  event_mean <- mean(d$event_count)
  d$event_count_c <- d$event_count - event_mean

  f <- abs_return_bist ~
    goldstein * role_dummy +
    event_count_c +
    return_vix +
    return_usdtry +
    abs_return_bist_lag1

  m <- stats::lm(f, data = d)

  lag_nw <- nw_lag(stats::nobs(m))
  vc <- sandwich::NeweyWest(
    m, lag = lag_nw, prewhite = FALSE, adjust = TRUE
  )

  ct <- lmtest::coeftest(m, vcov. = vc)

  tab <- data.frame(
    term = rownames(ct),
    estimate = unname(ct[, 1]),
    std_error = unname(ct[, 2]),
    statistic = unname(ct[, 3]),
    p_value = unname(ct[, 4]),
    stringsAsFactors = FALSE
  ) |>
    dplyr::mutate(
      role = "Pooled",
      timing = timing,
      model = "pooled_interaction_HAC",
      outcome = "abs_return_bist",
      N = stats::nobs(m),
      event_count_sample_mean = event_mean,
      centered_mean_check = mean(d$event_count_c),
      estimate_reported = scale_return_for_output(estimate),
      std_error_reported = scale_return_for_output(std_error),
      return_unit = return_unit_label
    )

  wald <- car::linearHypothesis(
    m,
    "goldstein:role_dummy = 0",
    vcov. = vc,
    test = "Chisq"
  )

  wr <- wald[nrow(wald), , drop = FALSE]

  list(
    model = m,
    table = tab,
    wald_stat = as.numeric(wr[["Chisq"]]),
    wald_p = as.numeric(wr[["Pr(>Chisq)"]]),
    nw_lag = lag_nw,
    N = stats::nobs(m)
  )
}

pool_reaction <- fit_pooled_vol(dat, "reaction")
pool_prediction <- fit_pooled_vol(dat, "prediction")

fit_sur_vol <- function(data, timing = c("reaction", "prediction")) {

  timing <- match.arg(timing)

  if (timing == "reaction") {
    g1 <- "goldstein_a1"; g2 <- "goldstein_a2"
    e1 <- "event_count_a1"; e2 <- "event_count_a2"
  } else {
    g1 <- "goldstein_a1_lag1"; g2 <- "goldstein_a2_lag1"
    e1 <- "event_count_a1_lag1"; e2 <- "event_count_a2_lag1"
  }

  needed <- c(
    "abs_return_bist", "return_vix", "return_usdtry",
    "abs_return_bist_lag1", g1, g2, e1, e2
  )

  d <- data |>
    dplyr::select(dplyr::all_of(needed)) |>
    tidyr::drop_na()

  d$event_a1_c <- d[[e1]] - mean(d[[e1]])
  d$event_a2_c <- d[[e2]] - mean(d[[e2]])

  eq1 <- stats::as.formula(
    paste0(
      "abs_return_bist ~ ", g1,
      " + event_a1_c + return_vix + return_usdtry + abs_return_bist_lag1"
    )
  )

  eq2 <- stats::as.formula(
    paste0(
      "abs_return_bist ~ ", g2,
      " + event_a2_c + return_vix + return_usdtry + abs_return_bist_lag1"
    )
  )

  sys <- systemfit::systemfit(
    list(Actor1 = eq1, Actor2 = eq2),
    method = "SUR",
    data = d
  )

  b <- stats::coef(sys)
  V <- stats::vcov(sys)

  n1 <- grep(paste0("^Actor1_", g1, "$"), names(b), value = TRUE)
  n2 <- grep(paste0("^Actor2_", g2, "$"), names(b), value = TRUE)

  if (length(n1) != 1 || length(n2) != 1) {
    n1 <- grep(paste0("Actor1.*", g1), names(b), value = TRUE)
    n2 <- grep(paste0("Actor2.*", g2), names(b), value = TRUE)
  }

  if (length(n1) < 1 || length(n2) < 1) {
    stop("SUR Goldstein coefficient names could not be identified.")
  }

  n1 <- n1[1]; n2 <- n2[1]

  diff_est <- unname(b[n1] - b[n2])
  diff_var <- V[n1, n1] + V[n2, n2] - 2 * V[n1, n2]
  wald_stat <- as.numeric(diff_est^2 / diff_var)
  p_value <- stats::pchisq(wald_stat, df = 1, lower.tail = FALSE)

  coef_tab <- data.frame(
    term = names(b),
    estimate = unname(b),
    std_error = sqrt(diag(V)),
    statistic = unname(b) / sqrt(diag(V)),
    p_value = 2 * stats::pnorm(
      abs(unname(b) / sqrt(diag(V))), lower.tail = FALSE
    ),
    role = "SUR",
    timing = timing,
    model = "SUR",
    outcome = "abs_return_bist",
    N = nrow(d),
    event_count_sample_mean = NA_real_,
    centered_mean_check = NA_real_,
    estimate_reported = scale_return_for_output(unname(b)),
    std_error_reported = scale_return_for_output(sqrt(diag(V))),
    return_unit = return_unit_label,
    stringsAsFactors = FALSE
  )

  list(
    model = sys,
    table = coef_tab,
    wald_stat = wald_stat,
    wald_p = p_value,
    difference = diff_est,
    N = nrow(d)
  )
}

sur_reaction <- fit_sur_vol(dat, "reaction")
sur_prediction <- fit_sur_vol(dat, "prediction")

all_tabs <- dplyr::bind_rows(
  role_tabs,
  pool_reaction$table,
  pool_prediction$table,
  sur_reaction$table,
  sur_prediction$table
)

readr::write_csv(all_tabs, table_path)

test_summary <- list(
  outcome = "absolute BIST 100 log return",
  interpretation = "model-free daily volatility proxy",
  return_unit = return_unit_label,
  reaction = list(
    pooled_HAC = list(
      N = pool_reaction$N,
      newey_west_lag = pool_reaction$nw_lag,
      chi_square = pool_reaction$wald_stat,
      p_value = pool_reaction$wald_p
    ),
    SUR = list(
      N = sur_reaction$N,
      chi_square = sur_reaction$wald_stat,
      p_value = sur_reaction$wald_p,
      coefficient_difference_actor1_minus_actor2 = sur_reaction$difference
    )
  ),
  prediction = list(
    pooled_HAC = list(
      N = pool_prediction$N,
      newey_west_lag = pool_prediction$nw_lag,
      chi_square = pool_prediction$wald_stat,
      p_value = pool_prediction$wald_p
    ),
    SUR = list(
      N = sur_prediction$N,
      chi_square = sur_prediction$wald_stat,
      p_value = sur_prediction$wald_p,
      coefficient_difference_actor1_minus_actor2 = sur_prediction$difference
    )
  ),
  note = paste(
    "The pooled interaction Wald test is the main actor-role equality test",
    "and uses a Newey-West HAC covariance matrix.",
    "SUR is reported as a complementary cross-equation robustness test",
    "and does not use HAC covariance."
  )
)

json_write(test_summary, json_path)

cat("\nVolatility table:", table_path, "\n")
cat("Volatility equality-test JSON:", json_path, "\n")
cat(
  "Reaction pooled HAC Wald p-value:",
  format(pool_reaction$wald_p, digits = 6), "\n"
)
cat(
  "Prediction pooled HAC Wald p-value:",
  format(pool_prediction$wald_p, digits = 6), "\n"
)

check_outputs(ROOT)
