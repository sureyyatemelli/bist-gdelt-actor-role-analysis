# ============================================================
# 03_tanimlayici_istatistikler.R
# Tum degiskenler icin tanimlayici istatistikler
# ============================================================

required_packages <- c("readr", "dplyr", "tidyr")
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
out_path <- file.path(ROOT, "results", "tables", "tanimlayici_istatistikler.csv")

dat <- readr::read_csv(panel_path, show_col_types = FALSE)

# RETURN_UNIT tek kaynaktan yonetilir.
# Ham model verisi degistirilmez; yalnizca raporlanan return degiskenleri olceklenir.
report_dat <- dat
return_vars_present <- intersect(RETURN_VARS, names(report_dat))
report_dat[return_vars_present] <- lapply(
  report_dat[return_vars_present],
  scale_return_for_output
)

numeric_names <- names(report_dat)[vapply(report_dat, is.numeric, logical(1))]

desc <- dplyr::bind_rows(lapply(numeric_names, function(v) {
  x <- report_dat[[v]]
  data.frame(
    variable = v,
    N = sum(!is.na(x)),
    mean = mean(x, na.rm = TRUE),
    median = stats::median(x, na.rm = TRUE),
    min = min(x, na.rm = TRUE),
    max = max(x, na.rm = TRUE),
    sd = stats::sd(x, na.rm = TRUE),
    unit = if (v %in% return_vars_present) return_unit_label else "native",
    stringsAsFactors = FALSE
  )
}))

readr::write_csv(desc, out_path)

cat("RETURN_UNIT:", RETURN_UNIT, "(", return_unit_label, ")\n")
cat("Tanimlayici istatistikler:", out_path, "\n")
cat("Degisken sayisi:", nrow(desc), "\n")

check_outputs(ROOT)
