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



# FUEL GROUPS FOR ANALYSIS
# The full model contains conventional biofuels, advanced biofuels
# and RFNBOs. The main empirical analysis focuses on advanced biofuels.
# Conventional biofuels and RFNBOs remain part of the full model
# but are reported separately.

BIOFUEL_SECTORS <- c(
  conv_biodiesel    = 12,
  adv_biodiesel     = 13,
  conv_biogasoline  = 14,
  adv_biogasoline   = 15,
  conv_bio_kerosene = 16,
  adv_bio_kerosene  = 17,
  adv_bio_hfo       = 18,
  RFNBOs             = 19,
  adv_biogas         = 33
)

ADV_BIO <- unname(
  BIOFUEL_SECTORS[
    c(
      "adv_biodiesel",
      "adv_biogasoline",
      "adv_bio_kerosene",
      "adv_bio_hfo",
      "adv_biogas"
    )
  ]
)

CONV_BIO <- unname(
  BIOFUEL_SECTORS[
    c(
      "conv_biodiesel",
      "conv_biogasoline",
      "conv_bio_kerosene"
    )
  ]
)

RFNBO_SECTOR <- unname(
  BIOFUEL_SECTORS["RFNBOs"]
)

# Named labels for output tables
biofuel_names <- setNames(
  names(BIOFUEL_SECTORS),
  as.character(BIOFUEL_SECTORS)
)

# Consistency check
stopifnot(
  setequal(
    BIO,
    unname(BIOFUEL_SECTORS)
  )
)

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
# --> relative change (with REF scenario as baseline)
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
# 14. OUTPUT MULTIPLIER INDICATORS
# ===================================================================

output_summary_all$nonbio_per_eur_bio <-
  ifelse(
    abs(output_summary_all$bio_output_change) > 1e-12,
    output_summary_all$nonbio_output_change /
      output_summary_all$bio_output_change,
    NA_real_
  )

output_summary_all$nonbio_share_total_change <-
  ifelse(
    abs(output_summary_all$total_output_change) > 1e-12,
    output_summary_all$nonbio_output_change /
      output_summary_all$total_output_change,
    NA_real_
  )


NONBIO_intensity_decomposition_all <-
  merge(
    NONBIO_output_decomposition_all,
    output_summary_all[
      ,
      c(
        "year",
        "scenario",
        "bio_output_change"
      )
    ],
    by = c(
      "year",
      "scenario"
    )
  )


NONBIO_intensity_decomposition_all$first_order_per_BIO <-
  NONBIO_intensity_decomposition_all$first_order_output /
  NONBIO_intensity_decomposition_all$bio_output_change


NONBIO_intensity_decomposition_all$higher_order_per_BIO <-
  NONBIO_intensity_decomposition_all$higher_order_output /
  NONBIO_intensity_decomposition_all$bio_output_change


NONBIO_intensity_decomposition_all$first_order_share_NONBIO <-
  NONBIO_intensity_decomposition_all$first_order_output /
  NONBIO_intensity_decomposition_all$total_NONBIO_output


NONBIO_intensity_decomposition_all$higher_order_share_NONBIO <-
  NONBIO_intensity_decomposition_all$higher_order_output /
  NONBIO_intensity_decomposition_all$total_NONBIO_output


# ===================================================================
# 15. DIRECT DOMESTIC NONBIO INPUT INTENSITY BY BIOFUEL
# ===================================================================

calculate_biofuel_input_intensity <- function(
    year_results,
    year,
    BIO,
    NONBIO,
    sector_names
) {

  results_list <- lapply(
    scenario_names,
    function(scenario_name) {

      scenario <-
        year_results[[scenario_name]]

      intensity <-
        sapply(
          BIO,
          function(bio_sector) {

            sum(
              scenario$A_dom_tech[
                NONBIO,
                bio_sector
              ]
            )
          }
        )


      data.frame(
        year = as.integer(year),
        scenario = scenario_name,
        bio_sector_id = BIO,
        biofuel = sector_names[BIO],

        direct_domestic_NONBIO_intensity =
          intensity
      )
    }
  )

  do.call(
    rbind,
    results_list
  )
}


biofuel_input_intensity_all <-
  do.call(
    rbind,
    lapply(
      benchmark_years,
      function(year) {

        calculate_biofuel_input_intensity(
          year_results = results[[year]],
          year = year,
          BIO = BIO,
          NONBIO = NONBIO,
          sector_names = sector_names
        )
      }
    )
  )

rownames(biofuel_input_intensity_all) <- NULL




# Advanced-biofuel-specific direct input intensities


advanced_biofuel_input_intensity_all <-
  biofuel_input_intensity_all[
    biofuel_input_intensity_all$bio_sector_id %in% ADV_BIO,
  ]


advanced_biofuel_input_intensity_all$biofuel_name <-
  biofuel_names[
    as.character(
      advanced_biofuel_input_intensity_all$bio_sector_id
    )
  ]

# ===================================================================
# 16. BIOFUEL-SPECIFIC CONTRIBUTION TO NONBIO OUTPUT
#     INCLUDING HIGHER-ORDER EFFECTS
# ===================================================================

calculate_biofuel_NONBIO_contributions <- function(
    year_results,
    year,
    BIO,
    NONBIO,
    sector_names
) {

  REF <- year_results$REF


  A_NN_ref <-
    REF$A_dom_tech[
      NONBIO,
      NONBIO,
      drop = FALSE
    ]


  L_NN <-
    solve(
      base::diag(length(NONBIO)) -
        A_NN_ref
    )


  scenario_results <- lapply(
    scenario_names,
    function(scenario_name) {

      scenario <-
        year_results[[scenario_name]]


      # NONBIO technology must remain unchanged
      stopifnot(
        isTRUE(
          all.equal(
            scenario$A_dom_tech[
              NONBIO,
              NONBIO,
              drop = FALSE
            ],
            A_NN_ref
          )
        )
      )


      biofuel_results <- lapply(
        BIO,
        function(bio_sector) {


          # First-order effect generated by this BIO sector
          direct_shock <-
            scenario$A_dom_tech[
              NONBIO,
              bio_sector
            ] *
            scenario$X_bio[bio_sector] -
            REF$A_dom_tech[
              NONBIO,
              bio_sector
            ] *
            REF$X_bio[bio_sector]


          # Full NONBIO effect including higher-order rounds
          total_NONBIO_effect <-
            as.numeric(
              L_NN %*%
                direct_shock
            )


          data.frame(
            year = as.integer(year),
            scenario = scenario_name,

            bio_sector_id =
              bio_sector,

            biofuel =
              sector_names[bio_sector],

            first_order_contribution =
              sum(direct_shock),

            higher_order_contribution =
              sum(total_NONBIO_effect) -
              sum(direct_shock),

            total_NONBIO_contribution =
              sum(total_NONBIO_effect)
          )
        }
      )


      do.call(
        rbind,
        biofuel_results
      )
    }
  )


  do.call(
    rbind,
    scenario_results
  )
}


biofuel_NONBIO_contributions_all <-
  do.call(
    rbind,
    lapply(
      benchmark_years,
      function(year) {

        calculate_biofuel_NONBIO_contributions(
          year_results = results[[year]],
          year = year,
          BIO = BIO,
          NONBIO = NONBIO,
          sector_names = sector_names
        )
      }
    )
  )

rownames(biofuel_NONBIO_contributions_all) <- NULL


# Add total NONBIO change in order to calculate contribution shares

biofuel_NONBIO_contributions_all <-
  merge(
    biofuel_NONBIO_contributions_all,
    output_summary_all[
      ,
      c(
        "year",
        "scenario",
        "nonbio_output_change"
      )
    ],
    by = c(
      "year",
      "scenario"
    )
  )


biofuel_NONBIO_contributions_all$contribution_share <-
  biofuel_NONBIO_contributions_all$total_NONBIO_contribution /
  biofuel_NONBIO_contributions_all$nonbio_output_change


# Check that the contributions of all BIO sectors together
# reproduce the total NONBIO output effect

contribution_check <-
  aggregate(
    total_NONBIO_contribution ~ year + scenario,
    data = biofuel_NONBIO_contributions_all,
    FUN = sum
  )


contribution_check <-
  merge(
    contribution_check,
    output_summary_all[
      ,
      c(
        "year",
        "scenario",
        "nonbio_output_change"
      )
    ],
    by = c(
      "year",
      "scenario"
    )
  )


stopifnot(
  isTRUE(
    all.equal(
      contribution_check$total_NONBIO_contribution,
      contribution_check$nonbio_output_change,
      tolerance = 1e-8
    )
  )
)

# ===================================================================
# 17. ADVANCED BIOFUEL FOCUS:
#     OUTPUT AND ATTRIBUTABLE NONBIO EFFECTS
# ===================================================================


# -------------------------------------------------------------------
# 17.1 Advanced, conventional and RFNBO output changes
# -------------------------------------------------------------------

calculate_fuel_group_output_summary <- function(
    year_results,
    year,
    ADV_BIO,
    CONV_BIO,
    RFNBO_SECTOR
) {

  REF <- year_results$REF

  scenario_results <- lapply(
    scenario_names,
    function(scenario_name) {

      scenario <- year_results[[scenario_name]]

      delta_X <-
        scenario$X -
        REF$X

      data.frame(
        year = as.integer(year),
        scenario = scenario_name,

        advanced_biofuel_output_change =
          sum(delta_X[ADV_BIO]),

        conventional_biofuel_output_change =
          sum(delta_X[CONV_BIO]),

        rfnbo_output_change =
          sum(delta_X[RFNBO_SECTOR])
      )
    }
  )

  do.call(
    rbind,
    scenario_results
  )
}


fuel_group_output_summary_all <-
  do.call(
    rbind,
    lapply(
      benchmark_years,
      function(year) {

        calculate_fuel_group_output_summary(
          year_results = results[[year]],
          year = year,
          ADV_BIO = ADV_BIO,
          CONV_BIO = CONV_BIO,
          RFNBO_SECTOR = RFNBO_SECTOR
        )
      }
    )
  )

rownames(fuel_group_output_summary_all) <- NULL


# -------------------------------------------------------------------
# 17.2 NONBIO output attributable specifically to advanced biofuels
# -------------------------------------------------------------------

advanced_biofuel_contributions_all <-
  biofuel_NONBIO_contributions_all[
    biofuel_NONBIO_contributions_all$bio_sector_id %in% ADV_BIO,
  ]


# Add readable fuel names
advanced_biofuel_contributions_all$biofuel_name <-
  biofuel_names[
    as.character(
      advanced_biofuel_contributions_all$bio_sector_id
    )
  ]


advanced_NONBIO_decomposition_all <-
  aggregate(
    cbind(
      first_order_contribution,
      higher_order_contribution,
      total_NONBIO_contribution
    ) ~ year + scenario,
    data = advanced_biofuel_contributions_all,
    FUN = sum
  )


names(
  advanced_NONBIO_decomposition_all
)[
  names(advanced_NONBIO_decomposition_all) ==
    "first_order_contribution"
] <- "advanced_first_order_NONBIO"


names(
  advanced_NONBIO_decomposition_all
)[
  names(advanced_NONBIO_decomposition_all) ==
    "higher_order_contribution"
] <- "advanced_higher_order_NONBIO"


names(
  advanced_NONBIO_decomposition_all
)[
  names(advanced_NONBIO_decomposition_all) ==
    "total_NONBIO_contribution"
] <- "advanced_total_NONBIO"

# -------------------------------------------------------------------
# Add advanced-specific contribution share for each advanced biofuel
# -------------------------------------------------------------------

advanced_biofuel_contributions_all <-
  merge(
    advanced_biofuel_contributions_all,
    advanced_NONBIO_decomposition_all[
      ,
      c(
        "year",
        "scenario",
        "advanced_total_NONBIO"
      )
    ],
    by = c(
      "year",
      "scenario"
    )
  )

 -------------------------------------------------------------------
# Aggregate advanced-biofuel output summary
# -------------------------------------------------------------------

advanced_biofuel_contributions_all$advanced_contribution_share <-
  advanced_biofuel_contributions_all$total_NONBIO_contribution /
  advanced_biofuel_contributions_all$advanced_total_NONBIO

advanced_output_summary_all <-
  merge(
    fuel_group_output_summary_all,
    advanced_NONBIO_decomposition_all,
    by = c(
      "year",
      "scenario"
    )
  )


advanced_output_summary_all$NONBIO_per_eur_advanced_biofuel <-
  advanced_output_summary_all$advanced_total_NONBIO /
  advanced_output_summary_all$advanced_biofuel_output_change


advanced_output_summary_all$first_order_per_eur_advanced_biofuel <-
  advanced_output_summary_all$advanced_first_order_NONBIO /
  advanced_output_summary_all$advanced_biofuel_output_change


advanced_output_summary_all$higher_order_per_eur_advanced_biofuel <-
  advanced_output_summary_all$advanced_higher_order_NONBIO /
  advanced_output_summary_all$advanced_biofuel_output_change


advanced_output_summary_all$first_order_share <-
  advanced_output_summary_all$advanced_first_order_NONBIO /
  advanced_output_summary_all$advanced_total_NONBIO


advanced_output_summary_all$higher_order_share <-
  advanced_output_summary_all$advanced_higher_order_NONBIO /
  advanced_output_summary_all$advanced_total_NONBIO


# ===================================================================
# 18. NONBIO SECTORAL OUTPUT EFFECTS
#     WHICH NONBIO SECTORS GAIN THE MOST?
# ===================================================================

NONBIO_sector_output_changes_all <-
  sector_output_changes_all[
    sector_output_changes_all$sector_id %in% NONBIO,
  ]


get_top_NONBIO_sectors <- function(
    year,
    scenario,
    n = 10
) {

  x <-
    NONBIO_sector_output_changes_all[
      NONBIO_sector_output_changes_all$year == year &
        NONBIO_sector_output_changes_all$scenario == scenario,
    ]


  x <-
    x[
      order(
        x$output_change,
        decreasing = TRUE
      ),
    ]


  x$rank <- seq_len(nrow(x))

  head(
    x,
    n
  )
}


top_NONBIO_sectors_all <-
  do.call(
    rbind,
    lapply(
      benchmark_years,
      function(year) {

        do.call(
          rbind,
          lapply(
            scenario_names,
            function(scenario_name) {

              get_top_NONBIO_sectors(
                year = as.integer(year),
                scenario = scenario_name,
                n = 10
              )
            }
          )
        )
      }
    )
  )

rownames(top_NONBIO_sectors_all) <- NULL

# ===================================================================
# 18. NONBIO SECTOR EFFECTS ATTRIBUTABLE TO ADVANCED BIOFUELS
# ===================================================================

calculate_advanced_NONBIO_sector_effects <- function(
    year_results,
    year,
    ADV_BIO,
    NONBIO,
    sector_names
) {

  REF <- year_results$REF


  A_NN <-
    REF$A_dom_tech[
      NONBIO,
      NONBIO,
      drop = FALSE
    ]


  L_NN <-
    solve(
      base::diag(length(NONBIO)) -
        A_NN
    )


  scenario_results <- lapply(
    scenario_names,
    function(scenario_name) {

      scenario <-
        year_results[[scenario_name]]


      direct_ADV_scenario <-
        scenario$A_dom_tech[
          NONBIO,
          ADV_BIO,
          drop = FALSE
        ] %*%
        scenario$X_bio[ADV_BIO]


      direct_ADV_ref <-
        REF$A_dom_tech[
          NONBIO,
          ADV_BIO,
          drop = FALSE
        ] %*%
        REF$X_bio[ADV_BIO]


      direct_shock <-
        direct_ADV_scenario -
        direct_ADV_ref


      total_effect <-
        as.numeric(
          L_NN %*%
            direct_shock
        )


      data.frame(
        year = as.integer(year),
        scenario = scenario_name,
        sector_id = NONBIO,
        sector = sector_names[NONBIO],

        advanced_first_order_effect =
          as.numeric(direct_shock),

        advanced_total_output_effect =
          total_effect,

        advanced_higher_order_effect =
          total_effect -
          as.numeric(direct_shock)
      )
    }
  )


  do.call(
    rbind,
    scenario_results
  )
}


advanced_NONBIO_sector_effects_all <-
  do.call(
    rbind,
    lapply(
      benchmark_years,
      function(year) {

        calculate_advanced_NONBIO_sector_effects(
          year_results = results[[year]],
          year = year,
          ADV_BIO = ADV_BIO,
          NONBIO = NONBIO,
          sector_names = sector_names
        )
      }
    )
  )

rownames(advanced_NONBIO_sector_effects_all) <- NULL


get_top_advanced_NONBIO_sectors <- function(
    year,
    scenario,
    n = 10
) {

  x <-
    advanced_NONBIO_sector_effects_all[
      advanced_NONBIO_sector_effects_all$year == year &
        advanced_NONBIO_sector_effects_all$scenario == scenario,
    ]


  x <-
    x[
      order(
        x$advanced_total_output_effect,
        decreasing = TRUE
      ),
    ]


  x$rank <- seq_len(nrow(x))

  head(
    x,
    n
  )
  }



# ===================================================================
# 18. S2 VS S3:
#     SCALE VS TECHNOLOGY / COEFFICIENT-STRUCTURE EFFECT
# ===================================================================

decompose_S2_S3_scale_technology <- function(
    year_results,
    year,
    BIO,
    NONBIO
) {

  S2 <- year_results$S2
  S3 <- year_results$S3


  # ---------------------------------------------------------------
  # NONBIO technology
  # ---------------------------------------------------------------

  A_NN <-
    S2$A_dom_tech[
      NONBIO,
      NONBIO,
      drop = FALSE
    ]


  stopifnot(
    isTRUE(
      all.equal(
        A_NN,
        S3$A_dom_tech[
          NONBIO,
          NONBIO,
          drop = FALSE
        ]
      )
    )
  )


  # ---------------------------------------------------------------
  # NONBIO final demand
  # ---------------------------------------------------------------

  Y_NONBIO <-
    S2$Y_dom[NONBIO]


  stopifnot(
    isTRUE(
      all.equal(
        Y_NONBIO,
        S3$Y_dom[NONBIO]
      )
    )
  )


  # ---------------------------------------------------------------
  # S2 BIO -> NONBIO coefficient structure
  # ---------------------------------------------------------------

  A_NB_S2 <-
    S2$A_dom_tech[
      NONBIO,
      BIO,
      drop = FALSE
    ]


  # ---------------------------------------------------------------
  # Counterfactual:
  # S3 production volumes + S2 coefficients
  # ---------------------------------------------------------------

  X_NONBIO_counterfactual <-
    solve(
      base::diag(length(NONBIO)) -
        A_NN,

      Y_NONBIO +
        A_NB_S2 %*%
        S3$X_bio[BIO]
    )


  # ---------------------------------------------------------------
  # Scale effect
  # ---------------------------------------------------------------

  scale_effect <-
    as.numeric(
      X_NONBIO_counterfactual
    ) -
    S2$X[NONBIO]


  # ---------------------------------------------------------------
  # Technology / coefficient-structure effect
  # ---------------------------------------------------------------

  technology_structure_effect <-
    S3$X[NONBIO] -
    as.numeric(
      X_NONBIO_counterfactual
    )


  # ---------------------------------------------------------------
  # Actual S3-S2 difference
  # ---------------------------------------------------------------

  total_difference <-
    S3$X[NONBIO] -
    S2$X[NONBIO]


  data.frame(
    year = as.integer(year),

    scale_effect =
      sum(scale_effect),

    technology_structure_effect =
      sum(technology_structure_effect),

    total_S3_minus_S2 =
      sum(total_difference),

    decomposition_check =
      sum(scale_effect) +
      sum(technology_structure_effect) -
      sum(total_difference)
  )
}


S2_S3_scale_technology_all <-
  do.call(
    rbind,
    lapply(
      benchmark_years,
      function(year) {

        decompose_S2_S3_scale_technology(
          year_results = results[[year]],
          year = year,
          BIO = BIO,
          NONBIO = NONBIO
        )
      }
    )
  )


stopifnot(
  max(
    abs(
      S2_S3_scale_technology_all$decomposition_check
    )
  ) < 1e-8
)
# ===================================================================
# 19. SHOW MAIN RESULTS
# ===================================================================

print(
  output_summary_all
)

print(
  NONBIO_output_decomposition_all
)

print(
  NONBIO_intensity_decomposition_all
)

print(
  pairwise_output_comparison_all
)

print(
  biofuel_input_intensity_all
)

print(
  biofuel_NONBIO_contributions_all
)

print(
  top_NONBIO_sectors_all
)

print(
  S2_S3_scale_technology_all
)

print(
  annual_output_path
)

  print(
  fuel_group_output_summary_all
)

print(
  advanced_output_summary_all
)

print(
  advanced_biofuel_contributions_all
)


  print(
  get_top_advanced_NONBIO_sectors(
    year = 2030,
    scenario = "S2",
    n = 10
  )
)
  
# ===================================================================
# 20. OPTIONAL: SAVE ANALYSIS TABLES
# ===================================================================

write.csv(
  NONBIO_intensity_decomposition_all,
  "NONBIO_intensity_decomposition_2030_2040.csv",
  row.names = FALSE
)

write.csv(
  biofuel_input_intensity_all,
  "biofuel_input_intensity_2030_2040.csv",
  row.names = FALSE
)

write.csv(
  biofuel_NONBIO_contributions_all,
  "biofuel_NONBIO_contributions_2030_2040.csv",
  row.names = FALSE
)

write.csv(
  top_NONBIO_sectors_all,
  "top_NONBIO_sectors_2030_2040.csv",
  row.names = FALSE
)

write.csv(
  S2_S3_scale_technology_all,
  "S2_S3_scale_technology_2030_2040.csv",
  row.names = FALSE
)


  write.csv(
  fuel_group_output_summary_all,
  "fuel_group_output_summary_2030_2040.csv",
  row.names = FALSE
)

write.csv(
  advanced_output_summary_all,
  "advanced_biofuel_output_summary_2030_2040.csv",
  row.names = FALSE
)

write.csv(
  advanced_biofuel_contributions_all,
  "advanced_biofuel_NONBIO_contributions_2030_2040.csv",
  row.names = FALSE
)

write.csv(
  advanced_biofuel_input_intensity_all,
  "advanced_biofuel_input_intensity_2030_2040.csv",
  row.names = FALSE
)

write.csv(
  advanced_NONBIO_sector_effects_all,
  "advanced_biofuel_NONBIO_sector_effects_2030_2040.csv",
  row.names = FALSE
)
