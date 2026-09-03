# ============================================================
# run_all.R
# Tum analiz akisini tek komutla calistirir.
# ============================================================

scripts <- c(
  "code/03_tanimlayici_istatistikler.R",
  "code/04_aktor_rolu_modelleri.R",
  "code/04b_volatilite_modelleri.R",
  "code/05_kantil_regresyon.R",
  "code/06_saglamlik_ekonomik_anlamlilik.R",
  "code/07_session_info.R"
)

cat("\n============================================================\n")
cat("BIST-GDELT RIBAF ANALYSIS PIPELINE\n")
cat("============================================================\n")
cat("Return inference     : Newey-West HAC\n")
cat("Volatility inference : Newey-West HAC\n")
cat("Quantile inference   : bootstrap (xy), R = 500\n")
cat("Reaction             : Goldstein_t   -> outcome_t\n")
cat("Prediction           : Goldstein_t-1 -> outcome_t\n")
cat("============================================================\n")

for (s in scripts) {

  if (!file.exists(s)) {
    stop(
      "Script bulunamadi: ", s,
      "\nCalisma dizininin proje kok dizini oldugunu kontrol edin."
    )
  }

  cat("\n============================================================\n")
  cat("RUNNING:", s, "\n")
  cat("============================================================\n")

  start_time <- Sys.time()
  source(s, echo = FALSE, chdir = FALSE)
  elapsed <- difftime(Sys.time(), start_time, units = "secs")

  cat(
    "\nCOMPLETED:", s,
    "| elapsed:", round(as.numeric(elapsed), 2), "sec\n"
  )
}

cat("\n============================================================\n")
cat("ALL ANALYSES COMPLETED SUCCESSFULLY\n")
cat("============================================================\n")
cat("Expected outputs:\n")
cat(" - results/tables/tanimlayici_istatistikler.csv\n")
cat(" - results/tables/aktor_rolu_modelleri.csv\n")
cat(" - results/metrics/aktor_rolu_esitlik_testi.json\n")
cat(" - results/tables/volatilite_modelleri.csv\n")
cat(" - results/metrics/volatilite_aktor_rolu_esitlik_testi.json\n")
cat(" - results/tables/kantil_katsayilari.csv\n")
cat(" - results/tables/marjinal_etki_persentiller.csv\n")
cat(" - results/metrics/kantil_model_ozeti.json\n")
cat(" - figures/kantil_katsayi_yolu.png\n")
cat(" - figures/marjinal_etki.png\n")
cat(" - results/tables/saglamlik_modelleri.csv\n")
cat(" - results/tables/ekonomik_anlamlilik_var.csv\n")
cat(" - results/metrics/tani_kontrolleri.json\n")
cat(" - results/metrics/session_info.txt\n")
cat("============================================================\n")
