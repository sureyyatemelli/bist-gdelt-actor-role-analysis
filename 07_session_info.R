# ============================================================
# 07_session_info.R
# R ortam bilgisini kaydet
# ============================================================

source(file.path("code", "01_utils.R"))
ROOT <- find_project_root()
ensure_dirs(ROOT)

out <- file.path(ROOT, "results", "metrics", "session_info.txt")
sink(out)
cat("Generated:", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"), "\n\n")
print(sessionInfo())
sink()

cat("sessionInfo kaydedildi:", out, "\n")
check_outputs(ROOT)
