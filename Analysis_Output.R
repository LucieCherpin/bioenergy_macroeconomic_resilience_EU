# ===================================================================
# 1. LOAD SAVED MODEL RESULTS
# ===================================================================

results_2030 <-
  readRDS(
    "model_results_2030.rds"
  )

REF <- results_2030$REF
S1  <- results_2030$S1
S2  <- results_2030$S2
S3  <- results_2030$S3
