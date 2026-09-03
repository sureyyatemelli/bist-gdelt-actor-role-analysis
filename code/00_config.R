# ============================================================
# 00_config.R
# Ortak proje ayarlari
# ============================================================

# Tek bir yerde degistirin:
# "yuzde"  -> raporlanan getiriler 100 x log-return
# "ondalik" -> raporlanan getiriler ham log-return
RETURN_UNIT <- "yuzde"

if (!RETURN_UNIT %in% c("yuzde", "ondalik")) {
  stop("RETURN_UNIT yalnizca 'yuzde' veya 'ondalik' olabilir.")
}

return_multiplier <- if (RETURN_UNIT == "yuzde") 100 else 1
return_unit_label <- if (RETURN_UNIT == "yuzde") "percent" else "decimal"

# Model tahminleri ham log-return olceginde yapilir.
# Tablo/sekil sunumunda RETURN_UNIT kullanilir.
scale_return_for_output <- function(x) {
  x * return_multiplier
}

RETURN_VARS <- c(
  "return_bist",
  "return_bist_lag1",
  "abs_return_bist",
  "real_return_bist",
  "return_vix",
  "return_usdtry"
)
