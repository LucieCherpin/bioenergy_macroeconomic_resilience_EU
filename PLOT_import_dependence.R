# ===================================================================
# IMPORT DEPENDENCE AND DOMESTIC VALUE CAPTURE
#
# Scenario-internal analysis: S1 / S2 / S3
# Benchmark years: 2030 / 2035 / 2040
#
# NO comparison to REF.
#
# Figures:
#   A1  Absolute finished-biofuel imports
#   A2  Finished-biofuel import reliance
#   B1  Import share by upstream input channel
#   B2  Imported input intensity per EUR domestic BIO output
#   D   Domestic GVA associated with biofuel production per EUR BIO
#
# IMPORTANT:
# - Feedstock / OPEX / CAPEX import shares describe DIRECT sourcing
#   of inputs into BIO production.
# - CAPEX is annualized, consistent with NEW_model_CAPEX_separate.R.
# ===================================================================


rm(list = ls())
options(scipen = 999)

library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)


# ===================================================================
# 1. SETTINGS
# ===================================================================

RESULTS_FILE <- "model_results_CAPEX_separate.rds"

OUT_DIR <- "plots_import_dependence"

TABLE_DIR <- "analysis_outputs"

dir.create(
  OUT_DIR,
  showWarnings = FALSE,
  recursive = TRUE
)

dir.create(
  TABLE_DIR,
  showWarnings = FALSE,
  recursive = TRUE
)


results <- readRDS(
  RESULTS_FILE
)


BIO <- results$metadata$BIO

NONBIO <- results$metadata$NONBIO

sector_names <- results$metadata$sector_names


benchmark_years <- c(
  "2030",
  "2035",
  "2040"
)

scenario_names <- c(
  "S1",
  "S2",
  "S3"
)


# Model data are in million EUR.
TO_BN <- 1 / 1000


safe_ratio <- function(
    num,
    den,
    tol = 1e-12
) {

  ifelse(
    abs(den) > tol,
    num / den,
    NA_real_
  )
}


# ===================================================================
# 2. COLORS / THEME
# ===================================================================

scenario_colors <- c(
  S1 = "#2a78d6",
  S2 = "#eb6834",
  S3 = "#1baf7a"
)


channel_colors <- c(
  "Feedstock" = "#1baf7a",
  "OPEX"      = "#2a78d6",
  "CAPEX"     = "#e34948"
)


gva_colors <- c(
  "Direct BIO GVA"        = "#eb6834",
  "Recurrent upstream GVA" = "#2a78d6",
  "CAPEX-related upstream GVA" = "#e34948"
)


ink_primary   <- "#0b0b0b"
ink_secondary <- "#52514e"
ink_muted     <- "#898781"
grid_hairline <- "#e1e0d9"
axis_line     <- "#c3c2b7"


base_theme <- theme_minimal(
  base_size = 12
) +
  theme(
    panel.grid.minor =
      element_blank(),

    panel.grid.major =
      element_line(
        color = grid_hairline,
        linewidth = 0.35
      ),

    axis.line =
      element_line(
        color = axis_line,
        linewidth = 0.3
      ),

    axis.text =
      element_text(
        color = ink_secondary
      ),

    axis.title =
      element_text(
        color = ink_secondary
      ),

    plot.title =
      element_text(
        face = "bold",
        size = 13,
        color = ink_primary
      ),

    plot.subtitle =
      element_text(
        color = ink_secondary,
        size = 10.5
      ),

    plot.caption =
      element_text(
        color = ink_muted,
        size = 8.5,
        hjust = 0
      ),

    legend.position = "top",

    legend.title =
      element_blank(),

    strip.text =
      element_text(
        face = "bold",
        color = ink_primary
      ),

    strip.background =
      element_rect(
        fill = "grey95",
        color = NA
      )
  )


# ===================================================================
# 3. PRE-FLIGHT CHECK
# ===================================================================

required_fields <- c(
  "X",
  "GVA",
  "X_bio",

  "A_dom_tech",
  "A_imp_tech",

  "A_feed_dom_tech",
  "A_feed_imp_tech",

  "A_opex_dom_tech",
  "A_opex_imp_tech",

  "A_capex_dom_tech",
  "A_capex_imp_tech",

  "Y_imp_FCE",
  "exports"
)


for (year in benchmark_years) {

  for (scenario_name in scenario_names) {

    endpoint <-
      results[[year]][[scenario_name]]


    missing_fields <-
      setdiff(
        required_fields,
        names(endpoint)
      )


    if (length(missing_fields) > 0) {

      stop(
        paste0(
          "Missing field(s) in ",
          year,
          "/",
          scenario_name,
          ": ",
          paste(
            missing_fields,
            collapse = ", "
          )
        )
      )
    }


    # Recurrent = Feedstock + OPEX
    stopifnot(

      isTRUE(
        all.equal(
          endpoint$A_dom_tech[
            ,
            BIO,
            drop = FALSE
          ],

          endpoint$A_feed_dom_tech[
            ,
            BIO,
            drop = FALSE
          ] +

            endpoint$A_opex_dom_tech[
              ,
              BIO,
              drop = FALSE
            ],

          tolerance = 1e-10
        )
      ),

      isTRUE(
        all.equal(
          endpoint$A_imp_tech[
            ,
            BIO,
            drop = FALSE
          ],

          endpoint$A_feed_imp_tech[
            ,
            BIO,
            drop = FALSE
          ] +

            endpoint$A_opex_imp_tech[
              ,
              BIO,
              drop = FALSE
            ],

          tolerance = 1e-10
        )
      )
    )
  }
}


# ===================================================================
# 4. CALCULATE IMPORT DEPENDENCE
# ===================================================================

calculate_import_dependence <- function(
    endpoint,
    year,
    scenario_name
) {

  X_bio <-
    endpoint$X_bio[BIO]


  total_bio_output <-
    sum(
      X_bio
    )


  # ---------------------------------------------------------------
  # FEEDSTOCK
  # ---------------------------------------------------------------

  feed_dom <-
    sum(
      endpoint$A_feed_dom_tech[
        ,
        BIO,
        drop = FALSE
      ] %*%
        X_bio
    )


  feed_imp <-
    sum(
      endpoint$A_feed_imp_tech[
        ,
        BIO,
        drop = FALSE
      ] %*%
        X_bio
    )


  feed_total <-
    feed_dom +
    feed_imp


  # ---------------------------------------------------------------
  # OPEX
  # ---------------------------------------------------------------

  opex_dom <-
    sum(
      endpoint$A_opex_dom_tech[
        ,
        BIO,
        drop = FALSE
      ] %*%
        X_bio
    )


  opex_imp <-
    sum(
      endpoint$A_opex_imp_tech[
        ,
        BIO,
        drop = FALSE
      ] %*%
        X_bio
    )


  opex_total <-
    opex_dom +
    opex_imp


  # ---------------------------------------------------------------
  # CAPEX
  # ---------------------------------------------------------------

  capex_dom <-
    sum(
      endpoint$A_capex_dom_tech[
        ,
        BIO,
        drop = FALSE
      ] %*%
        X_bio
    )


  capex_imp <-
    sum(
      endpoint$A_capex_imp_tech[
        ,
        BIO,
        drop = FALSE
      ] %*%
        X_bio
    )


  capex_total <-
    capex_dom +
    capex_imp


  # ---------------------------------------------------------------
  # FINISHED BIOFUEL IMPORTS
  # ---------------------------------------------------------------

  finished_imports <-
    sum(
      endpoint$Y_imp_FCE[BIO]
    )


  exports <-
    sum(
      endpoint$exports[BIO]
    )


  # Gross origin-based availability:
  # domestic production + finished imports.
  #
  # This denominator deliberately does NOT subtract exports.
  # It therefore measures the imported share of gross biofuel
  # availability by origin, not final domestic consumption.
  gross_biofuel_availability <-
    total_bio_output +
    finished_imports


  # Additional apparent-supply measure retained in the table
  # for later diagnostics.
  apparent_biofuel_supply <-
    total_bio_output +
    finished_imports -
    exports


  data.frame(

    year =
      as.integer(year),

    scenario =
      scenario_name,


    # -------------------------------------------------------------
    # Domestic BIO scale
    # -------------------------------------------------------------

    domestic_BIO_output =
      total_bio_output,


    # -------------------------------------------------------------
    # Feedstock
    # -------------------------------------------------------------

    feedstock_domestic =
      feed_dom,

    feedstock_imported =
      feed_imp,

    feedstock_total =
      feed_total,

    feedstock_import_share =
      safe_ratio(
        feed_imp,
        feed_total
      ),

    feedstock_import_per_eur_BIO =
      safe_ratio(
        feed_imp,
        total_bio_output
      ),


    # -------------------------------------------------------------
    # OPEX
    # -------------------------------------------------------------

    opex_domestic =
      opex_dom,

    opex_imported =
      opex_imp,

    opex_total =
      opex_total,

    opex_import_share =
      safe_ratio(
        opex_imp,
        opex_total
      ),

    opex_import_per_eur_BIO =
      safe_ratio(
        opex_imp,
        total_bio_output
      ),


    # -------------------------------------------------------------
    # CAPEX
    # -------------------------------------------------------------

    capex_domestic =
      capex_dom,

    capex_imported =
      capex_imp,

    capex_total =
      capex_total,

    capex_import_share =
      safe_ratio(
        capex_imp,
        capex_total
      ),

    capex_import_per_eur_BIO =
      safe_ratio(
        capex_imp,
        total_bio_output
      ),


    # -------------------------------------------------------------
    # Finished fuels
    # -------------------------------------------------------------

    finished_biofuel_imports =
      finished_imports,

    biofuel_exports =
      exports,

    gross_biofuel_availability =
      gross_biofuel_availability,

    apparent_biofuel_supply =
      apparent_biofuel_supply,

    finished_fuel_import_share =
      safe_ratio(
        finished_imports,
        gross_biofuel_availability
      ),

    finished_fuel_import_share_apparent_supply =
      safe_ratio(
        finished_imports,
        apparent_biofuel_supply
      )
  )
}


import_dependence_all <-
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

              calculate_import_dependence(
                endpoint =
                  results[[year]][[scenario_name]],

                year =
                  year,

                scenario_name =
                  scenario_name
              )
            }
          )
        )
      }
    )
  )


rownames(
  import_dependence_all
) <- NULL



# ===================================================================
# 5. CALCULATE DOMESTIC GVA ASSOCIATED WITH BIOFUEL PRODUCTION
# ===================================================================

calculate_domestic_GVA_capture <- function(
    endpoint,
    year,
    scenario_name
) {

  X_bio <-
    endpoint$X_bio[BIO]


  total_bio_output <-
    sum(
      X_bio
    )


  # ---------------------------------------------------------------
  # NONBIO Leontief structure
  # ---------------------------------------------------------------

  A_NN <-
    endpoint$A_dom_tech[
      NONBIO,
      NONBIO,
      drop = FALSE
    ]


  L_NN <-
    solve(
      base::diag(
        length(NONBIO)
      ) -
        A_NN
    )


  # ---------------------------------------------------------------
  # DIRECT DOMESTIC DEMAND GENERATED BY BIO PRODUCTION
  # ---------------------------------------------------------------

  feed_direct <-
    endpoint$A_feed_dom_tech[
      NONBIO,
      BIO,
      drop = FALSE
    ] %*%
      X_bio


  opex_direct <-
    endpoint$A_opex_dom_tech[
      NONBIO,
      BIO,
      drop = FALSE
    ] %*%
      X_bio


  capex_direct <-
    endpoint$A_capex_dom_tech[
      NONBIO,
      BIO,
      drop = FALSE
    ] %*%
      X_bio


  # ---------------------------------------------------------------
  # TOTAL DOMESTIC UPSTREAM OUTPUT
  # ---------------------------------------------------------------

  feed_output <-
    as.numeric(
      L_NN %*%
        feed_direct
    )


  opex_output <-
    as.numeric(
      L_NN %*%
        opex_direct
    )


  capex_output <-
    as.numeric(
      L_NN %*%
        capex_direct
    )


  recurrent_output <-
    feed_output +
    opex_output


  # ---------------------------------------------------------------
  # SECTORAL GVA COEFFICIENTS
  #
  # GVA / gross output for each NONBIO sector
  # ---------------------------------------------------------------

  gva_coeff_NONBIO <-
    safe_ratio(
      endpoint$GVA[NONBIO],
      endpoint$X[NONBIO]
    )


  # If a sector has zero output, set its contribution to zero.
  gva_coeff_NONBIO[
    is.na(
      gva_coeff_NONBIO
    )
  ] <- 0


  # ---------------------------------------------------------------
  # GVA COMPONENTS
  # ---------------------------------------------------------------

  direct_BIO_GVA <-
    sum(
      endpoint$GVA[BIO]
    )


  recurrent_upstream_GVA <-
    sum(
      gva_coeff_NONBIO *
        recurrent_output
    )


  capex_upstream_GVA <-
    sum(
      gva_coeff_NONBIO *
        capex_output
    )


  total_domestic_GVA_associated <-
    direct_BIO_GVA +
    recurrent_upstream_GVA +
    capex_upstream_GVA


  data.frame(

    year =
      as.integer(year),

    scenario =
      scenario_name,

    domestic_BIO_output =
      total_bio_output,


    direct_BIO_GVA =
      direct_BIO_GVA,

    recurrent_upstream_GVA =
      recurrent_upstream_GVA,

    capex_upstream_GVA =
      capex_upstream_GVA,

    total_domestic_GVA_associated =
      total_domestic_GVA_associated,


    direct_BIO_GVA_per_eur_BIO =
      safe_ratio(
        direct_BIO_GVA,
        total_bio_output
      ),

    recurrent_upstream_GVA_per_eur_BIO =
      safe_ratio(
        recurrent_upstream_GVA,
        total_bio_output
      ),

    capex_upstream_GVA_per_eur_BIO =
      safe_ratio(
        capex_upstream_GVA,
        total_bio_output
      ),

    total_domestic_GVA_per_eur_BIO =
      safe_ratio(
        total_domestic_GVA_associated,
        total_bio_output
      )
  )
}


domestic_GVA_capture_all <-
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

              calculate_domestic_GVA_capture(
                endpoint =
                  results[[year]][[scenario_name]],

                year =
                  year,

                scenario_name =
                  scenario_name
              )
            }
          )
        )
      }
    )
  )


rownames(
  domestic_GVA_capture_all
) <- NULL



# ===================================================================
# 6. SAVE UNDERLYING TABLES
# ===================================================================

write.csv(
  import_dependence_all,
  file.path(
    TABLE_DIR,
    "import_dependence_channel_specific.csv"
  ),
  row.names = FALSE
)


write.csv(
  domestic_GVA_capture_all,
  file.path(
    TABLE_DIR,
    "domestic_GVA_capture_scenario_internal.csv"
  ),
  row.names = FALSE
)



# ===================================================================
# FIGURE A1
# ABSOLUTE FINISHED-BIOFUEL IMPORTS
# ===================================================================

plot_A1_data <-
  import_dependence_all %>%
  mutate(

    scenario =
      factor(
        scenario,
        levels = scenario_names
      ),

    year =
      factor(
        year,
        levels = c(
          2030,
          2035,
          2040
        )
      ),

    finished_imports_bn =
      finished_biofuel_imports *
      TO_BN
  )


p_A1 <-
  ggplot(
    plot_A1_data,
    aes(
      x = scenario,
      y = finished_imports_bn,
      fill = scenario
    )
  ) +

  geom_col(
    width = 0.62,
    show.legend = FALSE
  ) +

  facet_wrap(
    ~year,
    nrow = 1
  ) +

  scale_fill_manual(
    values = scenario_colors
  ) +

  scale_y_continuous(
    labels =
      label_number(
        accuracy = 0.1
      )
  ) +

  labs(

    title =
      "Finished biofuel imports",

    subtitle =
      "Absolute imports of finished biofuels by scenario",

    x =
      NULL,

    y =
      "Finished biofuel imports (bn EUR)",

    caption =
      paste(
        "Scenario-internal comparison; no reference scenario is used.",
        "Source: own calculation based on model_results_CAPEX_separate.rds.",
        sep = "\n"
      )
  ) +

  base_theme


ggsave(
  file.path(
    OUT_DIR,
    "fig_import_A1_finished_fuel_imports_absolute.png"
  ),
  p_A1,
  width = 8.5,
  height = 5.2,
  dpi = 300,
  bg = "white"
)



# ===================================================================
# FIGURE A2
# FINISHED-FUEL IMPORT RELIANCE
# ===================================================================

plot_A2_data <-
  import_dependence_all %>%
  mutate(

    scenario =
      factor(
        scenario,
        levels = scenario_names
      ),

    year =
      factor(
        year,
        levels = c(
          2030,
          2035,
          2040
        )
      )
  )


p_A2 <-
  ggplot(
    plot_A2_data,
    aes(
      x = scenario,
      y = finished_fuel_import_share,
      fill = scenario
    )
  ) +

  geom_col(
    width = 0.62,
    show.legend = FALSE
  ) +

  facet_wrap(
    ~year,
    nrow = 1
  ) +

  scale_fill_manual(
    values = scenario_colors
  ) +

  scale_y_continuous(
    labels =
      label_percent(
        accuracy = 1
      ),
    expand =
      expansion(
        mult = c(
          0,
          0.05
        )
      )
  ) +

  labs(

    title =
      "Finished-fuel import reliance",

    subtitle =
      "Finished imports as a share of domestic production plus finished-fuel imports",

    x =
      NULL,

    y =
      "Finished-fuel import share",

    caption =
      paste(
        "The denominator is domestic biofuel production plus finished-biofuel imports.",
        "Exports are reported separately and are not deducted from this denominator.",
        sep = "\n"
      )
  ) +

  base_theme


ggsave(
  file.path(
    OUT_DIR,
    "fig_import_A2_finished_fuel_import_share.png"
  ),
  p_A2,
  width = 8.5,
  height = 5.2,
  dpi = 300,
  bg = "white"
)



# ===================================================================
# FIGURE B1
# IMPORT SHARE BY UPSTREAM INPUT CHANNEL
#
# IMPORTANT:
# These three values have DIFFERENT denominators.
# Therefore they must NOT be stacked.
# ===================================================================

plot_B1_data <-
  import_dependence_all %>%

  select(
    year,
    scenario,

    feedstock_import_share,
    opex_import_share,
    capex_import_share
  ) %>%

  pivot_longer(

    cols = c(
      feedstock_import_share,
      opex_import_share,
      capex_import_share
    ),

    names_to =
      "channel",

    values_to =
      "import_share"
  ) %>%

  mutate(

    scenario =
      factor(
        scenario,
        levels = scenario_names
      ),

    year =
      factor(
        year,
        levels = c(
          2030,
          2035,
          2040
        )
      ),

    channel =
      recode(
        channel,

        feedstock_import_share =
          "Feedstock",

        opex_import_share =
          "OPEX",

        capex_import_share =
          "CAPEX"
      ),

    channel =
      factor(
        channel,
        levels = c(
          "Feedstock",
          "OPEX",
          "CAPEX"
        )
      )
  )


p_B1 <-
  ggplot(
    plot_B1_data,
    aes(
      x = scenario,
      y = import_share,
      fill = channel
    )
  ) +

  geom_col(
    position =
      position_dodge(
        width = 0.72
      ),
    width = 0.64
  ) +

  facet_wrap(
    ~year,
    nrow = 1
  ) +

  scale_fill_manual(
    values =
      channel_colors
  ) +

  scale_y_continuous(
    labels =
      label_percent(
        accuracy = 1
      )
  ) +

  coord_cartesian(
    ylim = c(
      0,
      1
    )
  ) +

  labs(

    title =
      "Import dependence of domestic biofuel production by input channel",

    subtitle =
      "Imported share of feedstock, OPEX and annualized CAPEX requirements",

    x =
      NULL,

    y =
      "Imported share of channel-specific input requirement",

    caption =
      paste(
        "Each channel has its own denominator: imported / (domestic + imported input requirement).",
        "The figure measures direct import sourcing of inputs into BIO production.",
        "CAPEX represents annualized capital requirements.",
        sep = "\n"
      )
  ) +

  base_theme


ggsave(
  file.path(
    OUT_DIR,
    "fig_import_B1_channel_import_shares.png"
  ),
  p_B1,
  width = 9,
  height = 5.3,
  dpi = 300,
  bg = "white"
)



# ===================================================================
# FIGURE B2
# IMPORTED INPUT INTENSITY PER EUR DOMESTIC BIO OUTPUT
#
# Unlike B1, these components have the SAME denominator (BIO output),
# so stacking is meaningful.
# ===================================================================

plot_B2_data <-
  import_dependence_all %>%

  select(
    year,
    scenario,

    feedstock_import_per_eur_BIO,
    opex_import_per_eur_BIO,
    capex_import_per_eur_BIO
  ) %>%

  pivot_longer(

    cols = c(
      feedstock_import_per_eur_BIO,
      opex_import_per_eur_BIO,
      capex_import_per_eur_BIO
    ),

    names_to =
      "channel",

    values_to =
      "import_intensity"
  ) %>%

  mutate(

    scenario =
      factor(
        scenario,
        levels = scenario_names
      ),

    year =
      factor(
        year,
        levels = c(
          2030,
          2035,
          2040
        )
      ),

    channel =
      recode(
        channel,

        feedstock_import_per_eur_BIO =
          "Feedstock",

        opex_import_per_eur_BIO =
          "OPEX",

        capex_import_per_eur_BIO =
          "CAPEX"
      ),

    channel =
      factor(
        channel,
        levels = c(
          "Feedstock",
          "OPEX",
          "CAPEX"
        )
      )
  )


p_B2 <-
  ggplot(
    plot_B2_data,
    aes(
      x = scenario,
      y = import_intensity,
      fill = channel
    )
  ) +

  geom_col(
    width = 0.62
  ) +

  facet_wrap(
    ~year,
    nrow = 1
  ) +

  scale_fill_manual(
    values =
      channel_colors
  ) +

  scale_y_continuous(
    labels =
      label_number(
        accuracy = 0.01
      )
  ) +

  labs(

    title =
      "Imported input intensity of domestic biofuel production",

    subtitle =
      "Imported feedstock, OPEX and CAPEX requirements per euro of domestic biofuel output",

    x =
      NULL,

    y =
      "Imported input requirement per EUR BIO output (EUR/EUR)",

    caption =
      paste(
        "All components use domestic biofuel output as the common denominator and can therefore be stacked.",
        "The measure controls for differences in the scale of domestic biofuel production.",
        "CAPEX represents annualized capital requirements.",
        sep = "\n"
      )
  ) +

  base_theme


ggsave(
  file.path(
    OUT_DIR,
    "fig_import_B2_imported_input_intensity.png"
  ),
  p_B2,
  width = 8.5,
  height = 5.3,
  dpi = 300,
  bg = "white"
)



# ===================================================================
# FIGURE D
# DOMESTIC GVA ASSOCIATED WITH BIOFUEL PRODUCTION
# PER EUR DOMESTIC BIO OUTPUT
# ===================================================================

plot_D_data <-
  domestic_GVA_capture_all %>%

  select(
    year,
    scenario,

    direct_BIO_GVA_per_eur_BIO,

    recurrent_upstream_GVA_per_eur_BIO,

    capex_upstream_GVA_per_eur_BIO
  ) %>%

  pivot_longer(

    cols = c(
      direct_BIO_GVA_per_eur_BIO,
      recurrent_upstream_GVA_per_eur_BIO,
      capex_upstream_GVA_per_eur_BIO
    ),

    names_to =
      "component",

    values_to =
      "GVA_intensity"
  ) %>%

  mutate(

    scenario =
      factor(
        scenario,
        levels = scenario_names
      ),

    year =
      factor(
        year,
        levels = c(
          2030,
          2035,
          2040
        )
      ),

    component =
      recode(
        component,

        direct_BIO_GVA_per_eur_BIO =
          "Direct BIO GVA",

        recurrent_upstream_GVA_per_eur_BIO =
          "Recurrent upstream GVA",

        capex_upstream_GVA_per_eur_BIO =
          "CAPEX-related upstream GVA"
      ),

    component =
      factor(
        component,
        levels = c(
          "Direct BIO GVA",
          "Recurrent upstream GVA",
          "CAPEX-related upstream GVA"
        )
      )
  )


p_D <-
  ggplot(
    plot_D_data,
    aes(
      x = scenario,
      y = GVA_intensity,
      fill = component
    )
  ) +

  geom_col(
    width = 0.62
  ) +

  facet_wrap(
    ~year,
    nrow = 1
  ) +

  scale_fill_manual(
    values =
      gva_colors
  ) +

  scale_y_continuous(
    labels =
      label_number(
        accuracy = 0.01
      )
  ) +

  labs(

    title =
      "Domestic value added associated with biofuel production",

    subtitle =
      "Direct and upstream domestic GVA per euro of domestic biofuel output",

    x =
      NULL,

    y =
      "Domestic GVA per EUR BIO output (EUR/EUR)",

    caption =
      paste(
        "Direct BIO GVA is combined with GVA associated with domestic recurrent upstream production.",
        "CAPEX-related upstream GVA reflects annualized capital requirements and should be interpreted cautiously.",
        "The figure is scenario-internal and does not rely on the stationary reference scenario.",
        sep = "\n"
      )
  ) +

  base_theme


ggsave(
  file.path(
    OUT_DIR,
    "fig_import_D_domestic_GVA_capture.png"
  ),
  p_D,
  width = 8.8,
  height = 5.4,
  dpi = 300,
  bg = "white"
)



# ===================================================================
# 7. PRINT MAIN TABLES
# ===================================================================

cat(
  "\n===============================================================\n"
)

cat(
  "IMPORT DEPENDENCE BY CHANNEL\n"
)

cat(
  "===============================================================\n"
)

print(
  import_dependence_all[
    ,
    c(
      "year",
      "scenario",

      "finished_fuel_import_share",

      "feedstock_import_share",
      "opex_import_share",
      "capex_import_share",

      "feedstock_import_per_eur_BIO",
      "opex_import_per_eur_BIO",
      "capex_import_per_eur_BIO"
    )
  ]
)


cat(
  "\n===============================================================\n"
)

cat(
  "DOMESTIC GVA PER EUR BIO OUTPUT\n"
)

cat(
  "===============================================================\n"
)

print(
  domestic_GVA_capture_all[
    ,
    c(
      "year",
      "scenario",

      "direct_BIO_GVA_per_eur_BIO",

      "recurrent_upstream_GVA_per_eur_BIO",

      "capex_upstream_GVA_per_eur_BIO",

      "total_domestic_GVA_per_eur_BIO"
    )
  ]
)


cat(
  "\nAll import-dependence figures saved to: ",
  OUT_DIR,
  "\n",
  sep = ""
)
