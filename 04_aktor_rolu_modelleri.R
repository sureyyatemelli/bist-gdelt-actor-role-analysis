# ============================================================
# 04_aktor_rolu_modelleri.R
# Actor1 / Actor2 modelleri + bicimsel katsayi esitlik testleri
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
table_path <- file.path(ROOT, "results", "tables", "aktor_rolu_modelleri.csv")
json_path <- file.path(ROOT, "results", "metrics", "aktor_rolu_esitlik_testi.json")

dat <- readr::read_csv(panel_path, show_col_types = FALSE) |>
  dplyr::arrange(date)

# ------------------------------------------------------------
# A ve B: Actor-specific OLS + HAC (Newey-West)
# ------------------------------------------------------------

f_A <- stats::as.formula(
  "return_bist ~ goldstein_a1 + event_count_a1_c + return_vix + return_usdtry + return_bist_lag1"
)
f_B <- stats::as.formula(
  "return_bist ~ goldstein_a2 + event_count_a2_c + return_vix + return_usdtry + return_bist_lag1"
)

m_A <- stats::lm(f_A, data = dat, na.action = na.omit)
m_B <- stats::lm(f_B, data = dat, na.action = na.omit)

tab_A <- hac_coeftable(m_A) |>
  dplyr::mutate(model = "Model A - Actor1", N = stats::nobs(m_A))
tab_B <- hac_coeftable(m_B) |>
  dplyr::mutate(model = "Model B - Actor2", N = stats::nobs(m_B))

# ------------------------------------------------------------
# Pooled long model:
# return ~ goldstein * role_dummy + event_count_c + controls
# role_dummy = 1 Actor1, 0 Actor2
#
# Etkilesim katsayisi goldstein etkisinin Actor1 ve Actor2
# arasinda farkli olup olmadiginin dogrudan testidir.
# ------------------------------------------------------------

common_vars <- c(
  "date", "return_bist", "return_vix", "return_usdtry", "return_bist_lag1",
  "goldstein_a1", "goldstein_a2", "event_count_a1_c", "event_count_a2_c"
)
wide_common <- dat |>
  dplyr::select(dplyr::all_of(common_vars)) |>
  tidyr::drop_na()

long_A <- wide_common |>
  dplyr::transmute(
    date, return_bist, return_vix, return_usdtry, return_bist_lag1,
    goldstein = goldstein_a1,
    event_count_c = event_count_a1_c,
    role_dummy = 1
  )

long_B <- wide_common |>
  dplyr::transmute(
    date, return_bist, return_vix, return_usdtry, return_bist_lag1,
    goldstein = goldstein_a2,
    event_count_c = event_count_a2_c,
    role_dummy = 0
  )

pooled <- dplyr::bind_rows(long_A, long_B) |>
  dplyr::arrange(date, role_dummy)

m_pool <- stats::lm(
  return_bist ~ goldstein * role_dummy + event_count_c +
    return_vix + return_usdtry + return_bist_lag1,
  data = pooled
)

V_pool <- sandwich::NeweyWest(
  m_pool,
  lag = nw_lag(stats::nobs(m_pool)),
  prewhite = FALSE,
  adjust = TRUE
)

tab_pool <- lmtest::coeftest(m_pool, vcov. = V_pool)
tab_pool <- data.frame(
  term = rownames(tab_pool),
  estimate = tab_pool[, 1],
  std_error = tab_pool[, 2],
  statistic = tab_pool[, 3],
  p_value = tab_pool[, 4],
  hac_lag = nw_lag(stats::nobs(m_pool)),
  model = "Pooled role-interaction",
  N = stats::nobs(m_pool),
  row.names = NULL,
  check.names = FALSE
)

wald_pool <- car::linearHypothesis(
  m_pool,
  "goldstein:role_dummy = 0",
  vcov. = V_pool,
  test = "Chisq"
)

wald_pool_chisq <- as.numeric(wald_pool[2, "Chisq"])
wald_pool_p <- as.numeric(wald_pool[2, "Pr(>Chisq)"])

# ------------------------------------------------------------
# SUR alternative
#
# Not: SUR testi systemfit'in sistem kovaryans matrisini kullanir.
# Model A/B'nin raporlanan katsayilari ise yukarida HAC Newey-West'tir.
# Pooled Wald testi, HAC ile duzeltilmis ana bicimsel esitlik testidir.
# ------------------------------------------------------------

sur_result <- list(
  status = "not_run",
  statistic = NA_real_,
  p_value = NA_real_,
  hypothesis = "Actor1 Goldstein coefficient = Actor2 Goldstein coefficient"
)

sur_table <- NULL

try({
  sur_dat <- wide_common

  eq_A <- return_bist ~ goldstein_a1 + event_count_a1_c +
    return_vix + return_usdtry + return_bist_lag1
  eq_B <- return_bist ~ goldstein_a2 + event_count_a2_c +
    return_vix + return_usdtry + return_bist_lag1

  fit_sur <- systemfit::systemfit(
    list(A = eq_A, B = eq_B),
    method = "SUR",
    data = sur_dat
  )

  cn <- names(stats::coef(fit_sur))
  a_name <- cn[grepl("^A_.*goldstein_a1$|^A_goldstein_a1$", cn)][1]
  b_name <- cn[grepl("^B_.*goldstein_a2$|^B_goldstein_a2$", cn)][1]

  if (is.na(a_name) || is.na(b_name)) {
    stop("SUR katsayi adlari otomatik bulunamadi: ", paste(cn, collapse = ", "))
  }

  hyp <- paste0(a_name, " - ", b_name, " = 0")
  sur_lh <- car::linearHypothesis(fit_sur, hyp, test = "Chisq")

  sur_result <- list(
    status = "ok",
    statistic = as.numeric(sur_lh[2, "Chisq"]),
    p_value = as.numeric(sur_lh[2, "Pr(>Chisq)"]),
    hypothesis = hyp,
    N = nrow(sur_dat),
    covariance_note = paste(
      "SUR equality test uses systemfit system covariance.",
      "Actor-specific baseline models and the main pooled Wald test use HAC Newey-West."
    )
  )

  sc <- summary(fit_sur)$coefficients
  if (is.matrix(sc)) {
    sur_table <- data.frame(
      term = rownames(sc),
      estimate = sc[, 1],
      std_error = sc[, 2],
      statistic = sc[, 3],
      p_value = sc[, 4],
      hac_lag = NA_integer_,
      model = "SUR alternative",
      N = nrow(sur_dat),
      row.names = NULL,
      check.names = FALSE
    )
  }
}, silent = TRUE)

all_tabs <- dplyr::bind_rows(tab_A, tab_B, tab_pool, sur_table) |>
  dplyr::mutate(return_unit = return_unit_label)

readr::write_csv(all_tabs, table_path)

summary_json <- list(
  return_unit = return_unit_label,
  baseline_actor1_N = stats::nobs(m_A),
  baseline_actor2_N = stats::nobs(m_B),
  pooled_model_N = stats::nobs(m_pool),
  main_test = list(
    method = "Pooled role interaction with HAC Newey-West covariance",
    hypothesis = "goldstein:role_dummy = 0",
    chi_square = wald_pool_chisq,
    p_value = wald_pool_p
  ),
  alternative_SUR_test = sur_result
)

json_write(summary_json, json_path)

cat("Actor-role table:", table_path, "\n")
cat("Equality-test JSON:", json_path, "\n")
cat("Pooled HAC Wald p-value:", format(wald_pool_p, digits = 5), "\n")

check_outputs(ROOT)
