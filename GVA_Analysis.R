# ================================================================
# GVA ANALYSIS - 2030
# ================================================================

rm(list = ls())
options(scipen = 999)

# ---------------------------------------------------------------
# LOAD MODEL RESULTS
# ---------------------------------------------------------------

results_2030 <-
  readRDS(
    "model_results_2030.rds"
  )

REF <- results_2030$REF
S1  <- results_2030$S1
S2  <- results_2030$S2
S3  <- results_2030$S3

BIO <-
  results_2030$metadata$BIO

NONBIO <-
  results_2030$metadata$NONBIO

sector_names <-
  results_2030$metadata$sector_names


# ================================================================
# 1. BASIC CHECKS
# ================================================================

length(REF$GVA)
length(S1$GVA)
length(S2$GVA)
length(S3$GVA)

all.equal(
  REF$GVA,
  REF$X - REF$P2_ADJ
)

all.equal(
  S1$GVA,
  S1$X - S1$P2_ADJ
)

all.equal(
  S2$GVA,
  S2$X - S2$P2_ADJ
)

all.equal(
  S3$GVA,
  S3$X - S3$P2_ADJ
)

# ================================================================
# 2. AGGREGATE GVA CHANGES
# ================================================================

delta_GVA_S1 <-
  S1$GVA -
  REF$GVA

delta_GVA_S2 <-
  S2$GVA -
  REF$GVA

delta_GVA_S3 <-
  S3$GVA -
  REF$GVA


# ================================================================
# 3. GVA CHANGES GENERATED IN BIO VS  NONBIO SECTORS
# ================================================================


GVA_summary_2030 <- data.frame(

  scenario =
    c("S1", "S2", "S3"),

  bio_GVA_change = c(
    sum(delta_GVA_S1[BIO]),
    sum(delta_GVA_S2[BIO]),
    sum(delta_GVA_S3[BIO])
  ),

  nonbio_GVA_change = c(
    sum(delta_GVA_S1[NONBIO]),
    sum(delta_GVA_S2[NONBIO]),
    sum(delta_GVA_S3[NONBIO])
  ),

  total_GVA_change = c(
    sum(delta_GVA_S1),
    sum(delta_GVA_S2),
    sum(delta_GVA_S3)
  )
)

print(GVA_summary_2030)

# ================================================================
# 4. SHARES OF GVA CHANGES GENERATED OUTSIDE THE BIO SECTORS
# ================================================================


GVA_summary_2030$BIO_share_percent <-
  100 *
  GVA_summary_2030$bio_GVA_change /
  GVA_summary_2030$total_GVA_change

GVA_summary_2030$NONBIO_share_percent <-
  100 *
  GVA_summary_2030$nonbio_GVA_change /
  GVA_summary_2030$total_GVA_change

print(GVA_summary_2030)


# ================================================================
# 5. GVA RELATIVE TO GROSS OUTPUT
# ================================================================

delta_X_S1 <-
  S1$X -
  REF$X

delta_X_S2 <-
  S2$X -
  REF$X

delta_X_S3 <-
  S3$X -
  REF$X

GVA_output_comparison <- data.frame(

  scenario =
    c("S1", "S2", "S3"),

  total_output_change = c(
    sum(delta_X_S1),
    sum(delta_X_S2),
    sum(delta_X_S3)
  ),

  total_GVA_change = c(
    sum(delta_GVA_S1),
    sum(delta_GVA_S2),
    sum(delta_GVA_S3)
  ),

  GVA_per_EUR_output = c(
    sum(delta_GVA_S1) /
      sum(delta_X_S1),

    sum(delta_GVA_S2) /
      sum(delta_X_S2),

    sum(delta_GVA_S3) /
      sum(delta_X_S3)
  )
)

print(GVA_output_comparison)

# ================================================================
# 6. BIO vs NONBIO
# ================================================================


GVA_capture_by_group <- data.frame(

  scenario =
    c("S1", "S2", "S3"),

  BIO_GVA_per_output = c(
    sum(delta_GVA_S1[BIO]) /
      sum(delta_X_S1[BIO]),

    sum(delta_GVA_S2[BIO]) /
      sum(delta_X_S2[BIO]),

    sum(delta_GVA_S3[BIO]) /
      sum(delta_X_S3[BIO])
  ),

  NONBIO_GVA_per_output = c(
    sum(delta_GVA_S1[NONBIO]) /
      sum(delta_X_S1[NONBIO]),

    sum(delta_GVA_S2[NONBIO]) /
      sum(delta_X_S2[NONBIO]),

    sum(delta_GVA_S3[NONBIO]) /
      sum(delta_X_S3[NONBIO])
  )
)

print(GVA_capture_by_group)


# ================================================================
# 7. GVA PER EUR ADDITIONAL BIOFUEL MARKET VALUE
# ================================================================

delta_bio_market_S1 <-
  sum(
    S1$X_bio[BIO] -
      REF$X_bio[BIO]
  )

delta_bio_market_S2 <-
  sum(
    S2$X_bio[BIO] -
      REF$X_bio[BIO]
  )

delta_bio_market_S3 <-
  sum(
    S3$X_bio[BIO] -
      REF$X_bio[BIO]
  )


GVA_multiplier_2030 <- data.frame(

  scenario =
    c("S1", "S2", "S3"),

  additional_bio_market_value = c(
    delta_bio_market_S1,
    delta_bio_market_S2,
    delta_bio_market_S3
  ),

  total_GVA_change = c(
    sum(delta_GVA_S1),
    sum(delta_GVA_S2),
    sum(delta_GVA_S3)
  ),

  GVA_per_EUR_bio_market = c(
    sum(delta_GVA_S1) /
      delta_bio_market_S1,

    sum(delta_GVA_S2) /
      delta_bio_market_S2,

    sum(delta_GVA_S3) /
      delta_bio_market_S3
  )
)

print(GVA_multiplier_2030)

# ================================================================
# 8. SPLITTING THE MULTIPLIER INTO DIRECT BIO AND UPSTREAM NONBIO GVA
# ================================================================


GVA_multiplier_decomposition <- data.frame(

  scenario =
    c("S1", "S2", "S3"),

  BIO_GVA_per_EUR_bio_market = c(
    sum(delta_GVA_S1[BIO]) /
      delta_bio_market_S1,

    sum(delta_GVA_S2[BIO]) /
      delta_bio_market_S2,

    sum(delta_GVA_S3[BIO]) /
      delta_bio_market_S3
  ),

  NONBIO_GVA_per_EUR_bio_market = c(
    sum(delta_GVA_S1[NONBIO]) /
      delta_bio_market_S1,

    sum(delta_GVA_S2[NONBIO]) /
      delta_bio_market_S2,

    sum(delta_GVA_S3[NONBIO]) /
      delta_bio_market_S3
  ),

  total_GVA_per_EUR_bio_market = c(
    sum(delta_GVA_S1) /
      delta_bio_market_S1,

    sum(delta_GVA_S2) /
      delta_bio_market_S2,

    sum(delta_GVA_S3) /
      delta_bio_market_S3
  )
)

print(GVA_multiplier_decomposition)

# ================================================================
# 9. SECTORAL GVA EFFECTS
# ================================================================

GVA_sector_changes <- function(
    scenario,
    ref,
    NONBIO,
    sector_names
) {

  delta_GVA <-
    scenario$GVA[NONBIO] -
    ref$GVA[NONBIO]

  ref_GVA <-
    ref$GVA[NONBIO]

  data.frame(

    sector =
      sector_names[NONBIO],

    REF_GVA =
      ref_GVA,

    GVA_change =
      delta_GVA,

    percent_change =
      ifelse(
        ref_GVA != 0,
        100 *
          delta_GVA /
          ref_GVA,
        NA
      )
  )
}


GVA_sector_S1 <-
  GVA_sector_changes(
    S1,
    REF,
    NONBIO,
    sector_names
  )

GVA_sector_S2 <-
  GVA_sector_changes(
    S2,
    REF,
    NONBIO,
    sector_names
  )

GVA_sector_S3 <-
  GVA_sector_changes(
    S3,
    REF,
    NONBIO,
    sector_names
  )

head(
  GVA_sector_S1[
    order(-GVA_sector_S1$GVA_change),
  ],
  15
)

head(
  GVA_sector_S2[
    order(-GVA_sector_S2$GVA_change),
  ],
  15
)

head(
  GVA_sector_S3[
    order(-GVA_sector_S3$GVA_change),
  ],
  15
)


# ================================================================
# CHECK FOR NEGATIVE GVA EFFECTS
# ================================================================


BIO_GVA_changes <- data.frame(

  biofuel =
    sector_names[BIO],

  S1_change =
    delta_GVA_S1[BIO],

  S2_change =
    delta_GVA_S2[BIO],

  S3_change =
    delta_GVA_S3[BIO]
)

print(BIO_GVA_changes)

BIO_GVA_changes[
  BIO_GVA_changes$S1_change < 0 |
    BIO_GVA_changes$S2_change < 0 |
    BIO_GVA_changes$S3_change < 0,
]

# ================================================================
#  GVA INTENSITY OF INDIVIDUAL BIOFUELS
# ================================================================


BIO_GVA_intensity <- data.frame(

  biofuel =
    sector_names[BIO],

  REF =
    REF$GVA[BIO] /
    REF$X[BIO],

  S1 =
    S1$GVA[BIO] /
    S1$X[BIO],

  S2 =
    S2$GVA[BIO] /
    S2$X[BIO],

  S3 =
    S3$GVA[BIO] /
    S3$X[BIO]
)

print(BIO_GVA_intensity)
