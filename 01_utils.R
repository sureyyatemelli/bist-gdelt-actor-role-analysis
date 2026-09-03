# ============================================================
# 01_utils.R
# Ortak yardimci fonksiyonlar
# ============================================================

find_project_root <- function(start = getwd()) {
  p <- normalizePath(start, winslash = "/", mustWork = TRUE)
  repeat {
    candidate <- file.path(p, "data", "processed", "panel_gunluk.csv")
    if (file.exists(candidate)) return(p)
    parent <- dirname(p)
    if (identical(parent, p)) break
    p <- parent
  }
  stop(
    "Proje kok dizini bulunamadi. ",
    "data/processed/panel_gunluk.csv dosyasinin proje icinde oldugunu kontrol edin."
  )
}

ensure_dirs <- function(root) {
  dirs <- c(
    file.path(root, "results", "tables"),
    file.path(root, "results", "metrics"),
    file.path(root, "figures"),
    file.path(root, "data", "processed")
  )
  invisible(lapply(dirs, dir.create, recursive = TRUE, showWarnings = FALSE))
}

check_outputs <- function(root) {
  targets <- c(
    results = file.path(root, "results"),
    figures = file.path(root, "figures"),
    processed = file.path(root, "data", "processed")
  )

  cat("\n--- Cikti klasoru kontrolu ---\n")
  for (nm in names(targets)) {
    p <- targets[[nm]]
    ff <- if (dir.exists(p)) {
      list.files(p, recursive = TRUE, all.files = FALSE)
    } else {
      character(0)
    }
    if (length(ff) == 0) {
      cat(sprintf("[UYARI] %s bos: %s\n", nm, p))
    } else {
      cat(sprintf("[OK] %s: %d dosya\n", nm, length(ff)))
    }
  }
}

nw_lag <- function(n) {
  # Newey-West icin yaygin otomatik bant genisligi
  max(1L, floor(4 * (n / 100)^(2 / 9)))
}

hac_coeftable <- function(model) {
  n <- stats::nobs(model)
  L <- nw_lag(n)
  V <- sandwich::NeweyWest(
    model,
    lag = L,
    prewhite = FALSE,
    adjust = TRUE
  )
  ct <- lmtest::coeftest(model, vcov. = V)
  out <- data.frame(
    term = rownames(ct),
    estimate = ct[, 1],
    std_error = ct[, 2],
    statistic = ct[, 3],
    p_value = ct[, 4],
    row.names = NULL,
    check.names = FALSE
  )
  out$hac_lag <- L
  out
}

safe_quantile <- function(x, p) {
  as.numeric(stats::quantile(x, probs = p, na.rm = TRUE, names = FALSE, type = 7))
}

json_write <- function(x, path) {
  jsonlite::write_json(
    x,
    path = path,
    auto_unbox = TRUE,
    pretty = TRUE,
    na = "null"
  )
}
