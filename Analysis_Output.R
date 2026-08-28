# ===================================================================
# 1. LOAD MODEL RESULTS
# ===================================================================

rm(list = ls())

options(scipen = 999)

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


# ===================================================================
# 2. BASIC CHECKS
# ===================================================================


length(BIO)
length(NONBIO)
length(sector_names)

length(REF$X)

length(REF$X)
length(S1$X)
length(S2$X)
length(S3$X)

sum(is.na(S1$X))
sum(is.na(S1$GVA))
sum(is.na(S1$Z_imp))

# ===================================================================
# 3. AGGREGATE OUTPUT EFFECTS: BIO VS NONBIO
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

# ===================================================================
# $. DECOMPOSE NONBIO OUTPUT:
#    FIRST-ORDER VS HIGHER-ORDER EFFECTS
# ===================================================================

decompose_NONBIO_output <- function(
    scenario,
    ref,
    BIO,
    NONBIO
) {

  # ---------------------------------------------------------------
  # First-order upstream effect
  # Additional demand placed directly by BIO sectors on NONBIO
  # domestic suppliers
  # ---------------------------------------------------------------

  first_order_scenario <-
    scenario$A_dom_tech[
      NONBIO,
      BIO,
      drop = FALSE
    ] %*%
    scenario$X_bio[BIO]

  first_order_ref <-
    ref$A_dom_tech[
      NONBIO,
      BIO,
      drop = FALSE
    ] %*%
    ref$X_bio[BIO]

  first_order_change <-
    sum(
      first_order_scenario -
        first_order_ref
    )


  # ---------------------------------------------------------------
  # Total endogenous NONBIO output change
  # ---------------------------------------------------------------

  total_NONBIO_change <-
    sum(
      scenario$X[NONBIO] -
        ref$X[NONBIO]
    )


  # ---------------------------------------------------------------
  # Higher-order effect
  # Suppliers of suppliers and subsequent production rounds
  # ---------------------------------------------------------------

  higher_order_change <-
    total_NONBIO_change -
    first_order_change


  # ---------------------------------------------------------------
  # Return aggregated results
  # ---------------------------------------------------------------

  data.frame(

    first_order_output =
      first_order_change,

    higher_order_output =
      higher_order_change,

    total_NONBIO_output =
      total_NONBIO_change
  )
}

 # ---------------------------------------------------------------
  # Application: Run the function once for each scenario
  # ---------------------------------------------------------------


NONBIO_decomposition_S1 <-
  decompose_NONBIO_output(
    S1,
    REF,
    BIO,
    NONBIO
  )

NONBIO_decomposition_S2 <-
  decompose_NONBIO_output(
    S2,
    REF,
    BIO,
    NONBIO
  )

NONBIO_decomposition_S3 <-
  decompose_NONBIO_output(
    S3,
    REF,
    BIO,
    NONBIO
  )

# ---------------------------------------------------------------
  # Combine the scenarios (putting the 3 separate scenario result objects into once comparison table
  # ---------------------------------------------------------------

NONBIO_output_decomposition_2030 <- data.frame(

  scenario =
    c("S1", "S2", "S3"),

  first_order_output = c(
    NONBIO_decomposition_S1$first_order_output,
    NONBIO_decomposition_S2$first_order_output,
    NONBIO_decomposition_S3$first_order_output
  ),

  higher_order_output = c(
    NONBIO_decomposition_S1$higher_order_output,
    NONBIO_decomposition_S2$higher_order_output,
    NONBIO_decomposition_S3$higher_order_output
  ),

  total_NONBIO_output = c(
    NONBIO_decomposition_S1$total_NONBIO_output,
    NONBIO_decomposition_S2$total_NONBIO_output,
    NONBIO_decomposition_S3$total_NONBIO_output
  )
)

NONBIO_output_decomposition_2030
