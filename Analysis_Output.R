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


 BIO <- unname(BIOFUEL_SECTORS)

  NONBIO <- setdiff(
    seq_len(nIndustries),
    BIO
  )

nIndustries <- length(REF$X)

# ===================================================================
# 2. BASIC CHECKS
# ===================================================================

length(REF$X)
length(S1$X)
length(S2$X)
length(S3$X)

sum(is.na(S1$X))
sum(is.na(S1$GVA))
sum(is.na(S1$Z_imp))

# ===================================================================
# 3. OUTPUT EFFECTS
# ===================================================================

delta_X_S1 <- S1$X - REF$X
delta_X_S2 <- S2$X - REF$X
delta_X_S3 <- S3$X - REF$X

output_summary_2030 <- data.frame(

  scenario = c("S1", "S2", "S3"),

  bio_output_change = c(
    sum(delta_X_S1[BIO]),
    sum(delta_X_S2[BIO]),
    sum(delta_X_S3[BIO])
  ),

  nonbio_output_change = c(
    sum(delta_X_S1[NONBIO]),
    sum(delta_X_S2[NONBIO]),
    sum(delta_X_S3[NONBIO])
  ),

  total_output_change = c(
    sum(delta_X_S1),
    sum(delta_X_S2),
    sum(delta_X_S3)
  )
)

output_summary_2030


