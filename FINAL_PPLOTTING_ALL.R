# ===================================================================
# FINAL THESIS PLOTTING
#
# Focus:
#   4.1 Economy-wide scenario effects
#   4.2 Production linkages of advanced biofuels
#   4.3 Domestic value creation of advanced biofuels
#   5   Supply-chain import dependence
#   6   Synthesis: GVA and import exposure
#
# Reads tables created by NEW_Output_Analysis.R
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

IN_DIR <-
  "analysis_outputs"

OUT_DIR <-
  "plots_final"

dir.create(
  OUT_DIR,
  showWarnings = FALSE,
  recursive = TRUE
)


scenario_order <-
  c(
    "S1",
    "S2",
    "S3"
  )


scenario_colors <-
  c(
    S1 = "#2a78d6",
    S2 = "#eb6834",
    S3 = "#1baf7a"
  )


channel_colors <-
  c(
    "Feedstock" = "#1baf7a",
    "OPEX"      = "#2a78d6",
    "CAPEX"     = "#e34948"
  )


gva_colors <-
  c(
    "Direct advanced-biofuel GVA" =
      "#eb6834",

    "Feedstock-driven upstream GVA" =
      "#1baf7a",

    "OPEX-driven upstream GVA" =
      "#2a78d6"
  )


ink_primary <-
  "#0b0b0b"

ink_secondary <-
  "#52514e"

ink_muted <-
  "#898781"

grid_hairline <-
  "#e1e0d9"

axis_line <-
  "#c3c2b7"


TO_BN <-
  1 / 1000


base_theme <-
  theme_minimal(
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
        hjust = 0,
        lineheight = 1.15
      ),

    legend.position =
      "top",

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
# HELPER: CHECK INPUT FILE
# ===================================================================

read_analysis_table <- function(
    file_name
) {

  path <-
    file.path(
      IN_DIR,
      file_name
    )

  if (
    !file.exists(path)
  ) {

    stop(
      paste0(
        "Missing analysis table: ",
        path,
        "\nRun NEW_Output_Analysis.R first."
      )
    )
  }

  read.csv(
    path
  )
}



# ===================================================================
# 4.1 ECONOMY-WIDE IMPLICATIONS
# ===================================================================


# ===================================================================
# FIGURE 1
# Annual economy-wide output effect vs fixed 2023 reference
# ===================================================================

annual_macro <-
  read_analysis_table(
    "annual_macro_path.csv"
  )


fig1_data <-
  annual_macro %>%

  mutate(

    scenario =
      factor(
        scenario,
        levels = scenario_order
      ),

    output_change_bn =
      total_output_change *
      TO_BN
  )


p1 <-
  ggplot(
    fig1_data,
    aes(
      x = year,
      y = output_change_bn,
      color = scenario
    )
  ) +

  geom_hline(
    yintercept = 0,
    color = axis_line,
    linewidth = 0.4
  ) +

  geom_line(
    linewidth = 1.05
  ) +

  scale_color_manual(
    values = scenario_colors
  ) +

  scale_x_continuous(
    breaks =
      seq(
        2023,
        2040,
        by = 2
      )
  ) +

  scale_y_continuous(
    labels =
      label_number(
        accuracy = 0.1
      )
  ) +

  labs(

    title =
      "Economy-wide output effect",

    subtitle =
      "Total output difference relative to the fixed 2023 reference structure",

    x =
      NULL,

    y =
      "Output difference vs. REF (bn EUR)",

    caption =
      paste(
        "Economy-wide scenario result; includes the complete biofuel transition represented in each scenario.",
        "REF is a stationary 2023 counterfactual rather than a forecast of the 2040 economy.",
        sep = "\n"
      )
  ) +

  base_theme


ggsave(
  file.path(
    OUT_DIR,
    "fig1_economy_wide_output.png"
  ),
  p1,
  width = 8.5,
  height = 5.2,
  dpi = 300,
  bg = "white"
)



# ===================================================================
# FIGURE 2
# Annual economy-wide GVA effect vs fixed 2023 reference
# ===================================================================

fig2_data <-
  annual_macro %>%

  mutate(

    scenario =
      factor(
        scenario,
        levels = scenario_order
      ),

    GVA_change_bn =
      total_GVA_change *
      TO_BN
  )


p2 <-
  ggplot(
    fig2_data,
    aes(
      x = year,
      y = GVA_change_bn,
      color = scenario
    )
  ) +

  geom_hline(
    yintercept = 0,
    color = axis_line,
    linewidth = 0.4
  ) +

  geom_line(
    linewidth = 1.05
  ) +

  scale_color_manual(
    values = scenario_colors
  ) +

  scale_x_continuous(
    breaks =
      seq(
        2023,
        2040,
        by = 2
      )
  ) +

  scale_y_continuous(
    labels =
      label_number(
        accuracy = 0.1
      )
  ) +

  labs(

    title =
      "Economy-wide gross value added effect",

    subtitle =
      "Total GVA difference relative to the fixed 2023 reference structure",

    x =
      NULL,

    y =
      "GVA difference vs. REF (bn EUR)",

    caption =
      paste(
        "Economy-wide scenario result; includes the complete biofuel transition represented in each scenario.",
        "REF is a stationary 2023 counterfactual rather than a forecast of the 2040 economy.",
        sep = "\n"
      )
  ) +

  base_theme


ggsave(
  file.path(
    OUT_DIR,
    "fig2_economy_wide_GVA.png"
  ),
  p2,
  width = 8.5,
  height = 5.2,
  dpi = 300,
  bg = "white"
)



# ===================================================================
# 4.2 PRODUCTION LINKAGES OF ADVANCED BIOFUELS
# ===================================================================

adv_upstream <-
  read_analysis_table(
    "advanced_upstream_structure_all.csv"
  )


# ===================================================================
# FIGURE 3
# Total domestic upstream output per EUR advanced-biofuel output
# ===================================================================

fig3_data <-
  adv_upstream %>%

  mutate(

    scenario =
      factor(
        scenario,
        levels = scenario_order
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


p3 <-
  ggplot(
    fig3_data,
    aes(
      x = scenario,
      y = total_upstream_output_per_eur_ADV,
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
        accuracy = 0.01
      )
  ) +

  labs(

    title =
      "Domestic upstream production linkages of advanced biofuels",

    subtitle =
      "Domestic upstream output associated with one euro of domestic advanced-biofuel output",

    x =
      NULL,

    y =
      "Domestic upstream output per EUR advanced-biofuel output (EUR/EUR)",

    caption =
      paste(
        "Conventional biofuels and RFNBOs are excluded.",
        "The measure includes first-order and higher-order domestic upstream production.",
        "CAPEX represents annualized capital requirements.",
        sep = "\n"
      )
  ) +

  base_theme


ggsave(
  file.path(
    OUT_DIR,
    "fig3_ADV_upstream_output_intensity.png"
  ),
  p3,
  width = 8.5,
  height = 5.3,
  dpi = 300,
  bg = "white"
)



# ===================================================================
# FIGURE 4
# Same upstream output intensity, decomposed by originating channel
# ===================================================================

fig4_data <-
  adv_upstream %>%

  select(
    year,
    scenario,

    feedstock_output_per_eur_ADV,
    opex_output_per_eur_ADV,
    capex_output_per_eur_ADV
  ) %>%

  pivot_longer(

    cols =
      c(
        feedstock_output_per_eur_ADV,
        opex_output_per_eur_ADV,
        capex_output_per_eur_ADV
      ),

    names_to =
      "channel",

    values_to =
      "output_intensity"
  ) %>%

  mutate(

    scenario =
      factor(
        scenario,
        levels = scenario_order
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

        feedstock_output_per_eur_ADV =
          "Feedstock",

        opex_output_per_eur_ADV =
          "OPEX",

        capex_output_per_eur_ADV =
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


p4 <-
  ggplot(
    fig4_data,
    aes(
      x = scenario,
      y = output_intensity,
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
    values = channel_colors
  ) +

  scale_y_continuous(
    labels =
      label_number(
        accuracy = 0.01
      )
  ) +

  labs(

    title =
      "Transmission channels of advanced-biofuel production",

    subtitle =
      "Domestic upstream output per euro of advanced-biofuel output, decomposed by originating input channel",

    x =
      NULL,

    y =
      "Domestic upstream output per EUR advanced-biofuel output (EUR/EUR)",

    caption =
      paste(
        "Feedstock, OPEX and CAPEX identify the originating demand channel.",
        "Each component includes first-order and higher-order domestic production effects.",
        "CAPEX represents annualized capital requirements.",
        sep = "\n"
      )
  ) +

  base_theme


ggsave(
  file.path(
    OUT_DIR,
    "fig4_ADV_upstream_channels.png"
  ),
  p4,
  width = 8.5,
  height = 5.3,
  dpi = 300,
  bg = "white"
)



# ===================================================================
# 4.3 DOMESTIC VALUE CREATION OF ADVANCED BIOFUELS
# ===================================================================

adv_GVA <-
  read_analysis_table(
    "advanced_GVA_summary_all.csv"
  )


# ===================================================================
# FIGURE 5
# Aggregate advanced-biofuel GVA intensity
#
# CAPEX-related GVA deliberately excluded from the primary measure.
# ===================================================================

fig5_data <-
  adv_GVA %>%

  select(
    year,
    scenario,

    direct_BIO_GVA_per_eur_ADV,
    feedstock_upstream_GVA_per_eur_ADV,
    opex_upstream_GVA_per_eur_ADV
  ) %>%

  pivot_longer(

    cols =
      c(
        direct_BIO_GVA_per_eur_ADV,
        feedstock_upstream_GVA_per_eur_ADV,
        opex_upstream_GVA_per_eur_ADV
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
        levels = scenario_order
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

        direct_BIO_GVA_per_eur_ADV =
          "Direct advanced-biofuel GVA",

        feedstock_upstream_GVA_per_eur_ADV =
          "Feedstock-driven upstream GVA",

        opex_upstream_GVA_per_eur_ADV =
          "OPEX-driven upstream GVA"
      ),

    component =
      factor(
        component,
        levels = c(
          "Direct advanced-biofuel GVA",
          "Feedstock-driven upstream GVA",
          "OPEX-driven upstream GVA"
        )
      )
  )


p5 <-
  ggplot(
    fig5_data,
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
    values = gva_colors
  ) +

  scale_y_continuous(
    labels =
      label_number(
        accuracy = 0.01
      )
  ) +

  labs(

    title =
      "Domestic GVA intensity of advanced-biofuel production",

    subtitle =
      "Domestic value added associated with one euro of domestic advanced-biofuel output",

    x =
      NULL,

    y =
      "Domestic GVA per EUR advanced-biofuel output (EUR/EUR)",

    caption =
      paste(
        "Includes direct advanced-biofuel GVA and domestic recurrent upstream GVA.",
        "Conventional biofuels and RFNBOs are excluded.",
        "CAPEX-related GVA is excluded from this primary operating-chain measure.",
        sep = "\n"
      )
  ) +

  base_theme


ggsave(
  file.path(
    OUT_DIR,
    "fig5_ADV_GVA_intensity.png"
  ),
  p5,
  width = 8.8,
  height = 5.4,
  dpi = 300,
  bg = "white"
)



# ===================================================================
# FIGURE 6
# Product-specific GVA intensity:
# five advanced biofuel pathways
# ===================================================================

adv_product_GVA <-
  read_analysis_table(
    "advanced_biofuel_GVA_all.csv"
  )


fuel_labels <-
  c(
    adv_biodiesel =
      "Adv. biodiesel",

    adv_biogasoline =
      "Adv. biogasoline",

    adv_bio_kerosene =
      "Adv. bio-kerosene",

    adv_bio_hfo =
      "Adv. bio-HFO",

    adv_biogas =
      "Adv. biogas"
  )


fig6_data <-
  adv_product_GVA %>%

  filter(
    BIO_output > 1e-12
  ) %>%

  mutate(

    scenario =
      factor(
        scenario,
        levels = scenario_order
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

    biofuel =
      recode(
        biofuel,
        !!!fuel_labels
      ),

    biofuel =
      factor(
        biofuel,
        levels =
          unname(
            fuel_labels
          )
      )
  )


p6 <-
  ggplot(
    fig6_data,
    aes(
      x = biofuel,
      y = operating_chain_GVA_per_eur_BIO,
      fill = scenario
    )
  ) +

  geom_col(
    position =
      position_dodge(
        width = 0.78
      ),
    width = 0.7
  ) +

  facet_wrap(
    ~year,
    nrow = 1
  ) +

  scale_fill_manual(
    values =
      scenario_colors
  ) +

  scale_y_continuous(
    labels =
      label_number(
        accuracy = 0.01
      )
  ) +

  labs(

    title =
      "Domestic GVA intensity by advanced-biofuel pathway",

    subtitle =
      "Operating-chain GVA associated with one euro of domestic production",

    x =
      NULL,

    y =
      "Domestic GVA per EUR product output (EUR/EUR)",

    caption =
      paste(
        "Only advanced biofuels with positive domestic production are shown.",
        "The measure includes direct BIO-sector GVA and recurrent domestic upstream GVA.",
        "CAPEX-related GVA is excluded.",
        sep = "\n"
      )
  ) +

  base_theme +

  theme(

    axis.text.x =
      element_text(
        angle = 35,
        hjust = 1
      )
  )


ggsave(
  file.path(
    OUT_DIR,
    "fig6_ADV_product_specific_GVA.png"
  ),
  p6,
  width = 12,
  height = 6,
  dpi = 300,
  bg = "white"
)



# ===================================================================
# 5. SUPPLY-CHAIN IMPORT DEPENDENCE
# ===================================================================

adv_imports <-
  read_analysis_table(
    "advanced_supply_chain_import_summary_all.csv"
  )


# ===================================================================
# FIGURE 7
# Direct and higher-order supply-chain import intensity
#
# IMPORTANT:
# Upstream imports are not exogenous BIO import shares.
# They arise because domestic suppliers themselves use imported inputs.
# ===================================================================

fig7_direct <-
  adv_imports %>%

  select(
    year,
    scenario,

    feedstock_direct_import_per_eur_ADV,
    opex_direct_import_per_eur_ADV,
    capex_direct_import_per_eur_ADV
  ) %>%

  rename(

    Feedstock =
      feedstock_direct_import_per_eur_ADV,

    OPEX =
      opex_direct_import_per_eur_ADV,

    CAPEX =
      capex_direct_import_per_eur_ADV
  ) %>%

  pivot_longer(
    cols =
      c(
        Feedstock,
        OPEX,
        CAPEX
      ),
    names_to =
      "channel",
    values_to =
      "import_intensity"
  ) %>%

  mutate(
    import_stage =
      "Direct imports into advanced-biofuel production"
  )


fig7_upstream <-
  adv_imports %>%

  select(
    year,
    scenario,

    feedstock_upstream_import_per_eur_ADV,
    opex_upstream_import_per_eur_ADV,
    capex_upstream_import_per_eur_ADV
  ) %>%

  rename(

    Feedstock =
      feedstock_upstream_import_per_eur_ADV,

    OPEX =
      opex_upstream_import_per_eur_ADV,

    CAPEX =
      capex_upstream_import_per_eur_ADV
  ) %>%

  pivot_longer(
    cols =
      c(
        Feedstock,
        OPEX,
        CAPEX
      ),
    names_to =
      "channel",
    values_to =
      "import_intensity"
  ) %>%

  mutate(
    import_stage =
      "Imports embodied in domestic upstream supply chains"
  )


fig7_data <-
  bind_rows(
    fig7_direct,
    fig7_upstream
  ) %>%

  mutate(

    scenario =
      factor(
        scenario,
        levels = scenario_order
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
      factor(
        channel,
        levels = c(
          "Feedstock",
          "OPEX",
          "CAPEX"
        )
      ),

    import_stage =
      factor(
        import_stage,
        levels = c(
          "Direct imports into advanced-biofuel production",
          "Imports embodied in domestic upstream supply chains"
        )
      )
  )


p7 <-
  ggplot(
    fig7_data,
    aes(
      x = scenario,
      y = import_intensity,
      fill = channel
    )
  ) +

  geom_col(
    width = 0.62
  ) +

  facet_grid(
    import_stage ~ year
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
      "Supply-chain import intensity of advanced-biofuel production",

    subtitle =
      "Direct and upstream imported-input requirements per euro of domestic advanced-biofuel output",

    x =
      NULL,

    y =
      "Imported inputs per EUR advanced-biofuel output (EUR/EUR)",

    caption =
      paste(
        "Direct imports reflect imported inputs entering advanced-biofuel production itself.",
        "Upstream imports arise endogenously because domestically sourced suppliers themselves require imported intermediates.",
        "Upstream exposure is attributed to the originating Feedstock, OPEX or CAPEX demand channel.",
        sep = "\n"
      )
  ) +

  base_theme


ggsave(
  file.path(
    OUT_DIR,
    "fig7_ADV_supply_chain_import_intensity.png"
  ),
  p7,
  width = 10,
  height = 7,
  dpi = 300,
  bg = "white"
)



# ===================================================================
# 6. SYNTHESIS:
# DOMESTIC GVA VS. SUPPLY-CHAIN IMPORT EXPOSURE
#
# CAPEX excluded from BOTH axes here for consistency:
#
# y = direct ADV GVA + recurrent upstream GVA
# x = Feedstock + OPEX direct/upstream imports
# ===================================================================

synthesis_data <-
  adv_GVA %>%

  select(
    year,
    scenario,
    operating_chain_GVA_per_eur_ADV
  ) %>%

  left_join(

    adv_imports %>%
      select(
        year,
        scenario,
        recurrent_supply_chain_import_per_eur_ADV
      ),

    by =
      c(
        "year",
        "scenario"
      )
  ) %>%

  mutate(

    scenario =
      factor(
        scenario,
        levels = scenario_order
      )
  )


# ===================================================================
# FIGURE 8
# GVA vs recurrent supply-chain import exposure
# ===================================================================

p8 <-
  ggplot(
    synthesis_data,
    aes(
      x =
        recurrent_supply_chain_import_per_eur_ADV,

      y =
        operating_chain_GVA_per_eur_ADV,

      color =
        scenario,

      group =
        scenario
    )
  ) +

  geom_path(
    linewidth = 0.7,
    alpha = 0.55
  ) +

  geom_point(
    size = 3
  ) +

  geom_text(
    aes(
      label = year
    ),
    nudge_y = 0.012,
    size = 3.2,
    show.legend = FALSE
  ) +

  scale_color_manual(
    values =
      scenario_colors
  ) +

  scale_x_continuous(
    labels =
      label_number(
        accuracy = 0.01
      )
  ) +

  scale_y_continuous(
    labels =
      label_number(
        accuracy = 0.01
      )
  ) +

  labs(

    title =
      "Domestic value creation and import exposure",

    subtitle =
      "Advanced-biofuel operating-chain GVA versus recurrent supply-chain import intensity",

    x =
      "Recurrent supply-chain imports per EUR advanced-biofuel output (EUR/EUR)",

    y =
      "Domestic operating-chain GVA per EUR advanced-biofuel output (EUR/EUR)",

    caption =
      paste(
        "Each point represents one scenario-year; lines connect 2030, 2035 and 2040 within a scenario.",
        "CAPEX is excluded from both axes to maintain a consistent recurrent production-chain comparison.",
        "The relationship is descriptive and should not be interpreted as a causal estimate.",
        sep = "\n"
      )
  ) +

  base_theme


ggsave(
  file.path(
    OUT_DIR,
    "fig8_ADV_GVA_vs_import_exposure.png"
  ),
  p8,
  width = 8,
  height = 5.8,
  dpi = 300,
  bg = "white"
)



# ===================================================================
# DONE
# ===================================================================

cat(
  "\nFinal thesis figures saved to: ",
  OUT_DIR,
  "\n",
  sep = ""
)
