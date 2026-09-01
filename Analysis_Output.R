# ===================================================================
# 1. LOAD MODEL RESULTS
# ===================================================================

rm(list = ls())

options(scipen = 999)

results <- readRDS(
  "model_results.rds"
)


# ===================================================================
# 2. BASIC SETTINGS
# ===================================================================

BIO <- results$metadata$BIO
NONBIO <- results$metadata$NONBIO
sector_names <- results$metadata$sector_names

benchmark_years <- c("2030", "2035", "2040")
scenario_names <- c("S1", "S2", "S3")

# Complete annual dynamic paths: 2023-2040
dynamic_results <- results$dynamic

annual_years <- dynamic_results$REF$years


# ===================================================================
# 3. BASIC CHECKS
# ===================================================================

stopifnot(
  length(BIO) + length(NONBIO) == length(sector_names),
  all(benchmark_years %in% names(results))
)

for (year in benchmark_years) {

  year_results <- results[[year]]

  stopifnot(
    all(c("REF", "S1", "S2", "S3") %in% names(year_results)),
    length(year_results$REF$X) == length(sector_names),
    length(year_results$S1$X) == length(sector_names),
    length(year_results$S2$X) == length(sector_names),
    length(year_results$S3$X) == length(sector_names),

    !anyNA(year_results$REF$X),
    !anyNA(year_results$S1$X),
    !anyNA(year_results$S2$X),
    !anyNA(year_results$S3$X)
  )
}

# ===================================================================
# CHECK COMPLETE ANNUAL DYNAMIC PATHS
# ===================================================================

stopifnot(
  "dynamic" %in% names(results),

  all(
    c("REF", "S1", "S2", "S3") %in%
      names(dynamic_results)
  ),

  identical(
    dynamic_results$REF$years,
    2023:2040
  ),

  identical(
    dynamic_results$S1$years,
    dynamic_results$REF$years
  ),

  identical(
    dynamic_results$S2$years,
    dynamic_results$REF$years
  ),

  identical(
    dynamic_results$S3$years,
    dynamic_results$REF$years
  ),

  nrow(dynamic_results$REF$X) == length(2023:2040),
  nrow(dynamic_results$S1$X) == length(2023:2040),
  nrow(dynamic_results$S2$X) == length(2023:2040),
  nrow(dynamic_results$S3$X) == length(2023:2040),

  ncol(dynamic_results$REF$X) == length(sector_names),
  ncol(dynamic_results$S1$X) == length(sector_names),
  ncol(dynamic_results$S2$X) == length(sector_names),
  ncol(dynamic_results$S3$X) == length(sector_names),

  !anyNA(dynamic_results$REF$X),
  !anyNA(dynamic_results$S1$X),
  !anyNA(dynamic_results$S2$X),
  !anyNA(dynamic_results$S3$X)
)


# ===================================================================
# 4. AGGREGATE OUTPUT EFFECTS:
#    SCENARIO VS SAME-YEAR REFERENCE
# ===================================================================

calculate_output_summary <- function(
    year_results,
    year,
    BIO,
    NONBIO
) {

  REF <- year_results$REF

  scenario_results <- lapply(
    scenario_names,
    function(scenario_name) {

      scenario <- year_results[[scenario_name]]

      delta_X <- scenario$X - REF$X

      data.frame(
        year = as.integer(year),
        scenario = scenario_name,

        bio_output_change =
          sum(delta_X[BIO]),

        nonbio_output_change =
          sum(delta_X[NONBIO]),

        total_output_change =
          sum(delta_X)
      )
    }
  )

  do.call(
    rbind,
    scenario_results
  )
}


output_summary_all <- do.call(
  rbind,
  lapply(
    benchmark_years,
    function(year) {

      calculate_output_summary(
        year_results = results[[year]],
        year = year,
        BIO = BIO,
        NONBIO = NONBIO
      )
    }
  )
)

rownames(output_summary_all) <- NULL


# ===================================================================
# 5. YEAR-SPECIFIC OUTPUT TABLES
# ===================================================================

output_summary_2030 <-
  subset(
    output_summary_all,
    year == 2030
  )

output_summary_2035 <-
  subset(
    output_summary_all,
    year == 2035
  )

output_summary_2040 <-
  subset(
    output_summary_all,
    year == 2040
  )


# ===================================================================
# 6. DECOMPOSE NONBIO OUTPUT:
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
  #
  # Additional demand placed directly by BIO sectors
  # on domestic NONBIO suppliers
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
  # Higher-order upstream effect
  #
  # Suppliers of suppliers and subsequent Leontief rounds
  # ---------------------------------------------------------------

  higher_order_change <-
    total_NONBIO_change -
    first_order_change


  # ---------------------------------------------------------------
  # Return
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


# ===================================================================
# 7. APPLY NONBIO DECOMPOSITION TO ALL YEARS AND SCENARIOS
# ===================================================================

calculate_NONBIO_decomposition <- function(
    year_results,
    year,
    BIO,
    NONBIO
) {

  REF <- year_results$REF

  decomposition_results <- lapply(
    scenario_names,
    function(scenario_name) {

      scenario <- year_results[[scenario_name]]

      decomposition <-
        decompose_NONBIO_output(
          scenario = scenario,
          ref = REF,
          BIO = BIO,
          NONBIO = NONBIO
        )

      data.frame(
        year = as.integer(year),
        scenario = scenario_name,
        decomposition
      )
    }
  )

  do.call(
    rbind,
    decomposition_results
  )
}


NONBIO_output_decomposition_all <- do.call(
  rbind,
  lapply(
    benchmark_years,
    function(year) {

      calculate_NONBIO_decomposition(
        year_results = results[[year]],
        year = year,
        BIO = BIO,
        NONBIO = NONBIO
      )
    }
  )
)

rownames(NONBIO_output_decomposition_all) <- NULL


# ===================================================================
# 8. YEAR-SPECIFIC NONBIO DECOMPOSITION TABLES
# ===================================================================

NONBIO_output_decomposition_2030 <-
  subset(
    NONBIO_output_decomposition_all,
    year == 2030
  )

NONBIO_output_decomposition_2035 <-
  subset(
    NONBIO_output_decomposition_all,
    year == 2035
  )

NONBIO_output_decomposition_2040 <-
  subset(
    NONBIO_output_decomposition_all,
    year == 2040
  )


# ===================================================================
# 9. SCENARIO-TO-SCENARIO OUTPUT COMPARISONS
# ===================================================================
#
# In addition to S1/S2/S3 vs REF, compare scenarios directly
# within the same benchmark year.
#
# S2 - S1
# S3 - S1
# S3 - S2
# ===================================================================

calculate_pairwise_output <- function(
    year_results,
    year,
    BIO,
    NONBIO
) {

  comparisons <- list(
    "S2-S1" = c("S2", "S1"),
    "S3-S1" = c("S3", "S1"),
    "S3-S2" = c("S3", "S2")
  )

  comparison_results <- lapply(
    names(comparisons),
    function(comparison_name) {

      scenario_high <-
        year_results[[
          comparisons[[comparison_name]][1]
        ]]

      scenario_low <-
        year_results[[
          comparisons[[comparison_name]][2]
        ]]

      delta_X <-
        scenario_high$X -
        scenario_low$X

      data.frame(
        year = as.integer(year),
        comparison = comparison_name,

        bio_output_difference =
          sum(delta_X[BIO]),

        nonbio_output_difference =
          sum(delta_X[NONBIO]),

        total_output_difference =
          sum(delta_X)
      )
    }
  )

  do.call(
    rbind,
    comparison_results
  )
}


pairwise_output_comparison_all <- do.call(
  rbind,
  lapply(
    benchmark_years,
    function(year) {

      calculate_pairwise_output(
        year_results = results[[year]],
        year = year,
        BIO = BIO,
        NONBIO = NONBIO
      )
    }
  )
)

rownames(pairwise_output_comparison_all) <- NULL


# ===================================================================
# 10. SECTOR-LEVEL OUTPUT EFFECTS
# ===================================================================

calculate_sector_output_changes <- function(
    year_results,
    year,
    sector_names
) {

  REF <- year_results$REF

  sector_results <- lapply(
    scenario_names,
    function(scenario_name) {

      scenario <- year_results[[scenario_name]]

      data.frame(
        year = as.integer(year),
        scenario = scenario_name,
        sector_id = seq_along(sector_names),
        sector = sector_names,
        output_REF = REF$X,
        output_scenario = scenario$X,
        output_change = scenario$X - REF$X
      )
    }
  )

  do.call(
    rbind,
    sector_results
  )
}


sector_output_changes_all <- do.call(
  rbind,
  lapply(
    benchmark_years,
    function(year) {

      calculate_sector_output_changes(
        year_results = results[[year]],
        year = year,
        sector_names = sector_names
      )
    }
  )
)

rownames(sector_output_changes_all) <- NULL


# ===================================================================
# 11. ANNUAL OUTPUT PATH: 2023-2040
#     SCENARIO VS SAME-YEAR REFERENCE
# ===================================================================

calculate_annual_output_path <- function(
    dynamic_results,
    BIO,
    NONBIO
) {

  REF <- dynamic_results$REF

  annual_results <- lapply(
    scenario_names,
    function(scenario_name) {

      scenario <-
        dynamic_results[[scenario_name]]

      delta_X <-
        scenario$X -
        REF$X


      data.frame(

        year =
          scenario$years,

        scenario =
          scenario_name,

        bio_output_change =
          rowSums(
            delta_X[
              ,
              BIO,
              drop = FALSE
            ]
          ),

        nonbio_output_change =
          rowSums(
            delta_X[
              ,
              NONBIO,
              drop = FALSE
            ]
          ),

        total_output_change =
          rowSums(
            delta_X
          )
      )
    }
  )


  do.call(
    rbind,
    annual_results
  )
}


annual_output_path <-
  calculate_annual_output_path(
    dynamic_results = dynamic_results,
    BIO = BIO,
    NONBIO = NONBIO
  )

rownames(annual_output_path) <- NULL


# ===================================================================
# 12. ANNUAL ABSOLUTE OUTPUT PATH
# ===================================================================

annual_absolute_output <- data.frame(

  year =
    dynamic_results$REF$years,

  REF_total_output =
    rowSums(
      dynamic_results$REF$X
    ),

  S1_total_output =
    rowSums(
      dynamic_results$S1$X
    ),

  S2_total_output =
    rowSums(
      dynamic_results$S2$X
    ),

  S3_total_output =
    rowSums(
      dynamic_results$S3$X
    )
)


# ===================================================================
# 13. CHECK:
#     DYNAMIC PATH MUST MATCH STORED BENCHMARK RESULTS
# ===================================================================


for (year in as.integer(benchmark_years)) {

  for (
    scenario_name in
    c("REF", "S1", "S2", "S3")
  ) {

    annual_idx <-
      which(
        dynamic_results[[scenario_name]]$years ==
          year
      )

    stopifnot(
      isTRUE(
        all.equal(
          dynamic_results[[scenario_name]]$X[
            annual_idx,
          ],
          results[[as.character(year)]][[
            scenario_name
          ]]$X
        )
      )
    )
  }
}
# ===================================================================
# 14. SHOW MAIN RESULTS
# ===================================================================

print(
  output_summary_all
)

print(
  NONBIO_output_decomposition_all
)

print(
  pairwise_output_comparison_all
)

print(
  annual_output_path
)

# ===================================================================
# 15. OPTIONAL: SAVE ANALYSIS TABLES
# ===================================================================

write.csv(
  output_summary_all,
  "output_summary_2030_2040.csv",
  row.names = FALSE
)

write.csv(
  NONBIO_output_decomposition_all,
  "NONBIO_output_decomposition_2030_2040.csv",
  row.names = FALSE
)

write.csv(
  pairwise_output_comparison_all,
  "pairwise_output_comparison_2030_2040.csv",
  row.names = FALSE
)

write.csv(
  sector_output_changes_all,
  "sector_output_changes_2030_2040.csv",
  row.names = FALSE
)

write.csv(
  annual_output_path,
  "annual_output_path_2023_2040.csv",
  row.names = FALSE
)

write.csv(
  annual_absolute_output,
  "annual_absolute_output_2023_2040.csv",
  row.names = FALSE
)
