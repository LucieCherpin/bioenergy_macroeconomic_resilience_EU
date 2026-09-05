# ===================================================================
# PLOTTING - OUTPUT-LEVEL CORE RESULTS FOR THE THESIS
# Reads the CSVs saved by NEW_Output_Analysis.R from analysis_outputs/
# and produces the four core "result dimensions":
#   1) Annual path of the total output effect 2023-2040 (vs. REF), S1/S2/S3
#   2) Top NONBIO sectors by output gain, 2030 / 2035 / 2040, by scenario
#   3) Composition of the NONBIO effect: recurrent input demand vs. CAPEX
#      (capital intensity of the active technology mix, NOT a build phase!)
#   4) Import dependence: CAPEX import share over time
#
# Colors: taken from the dataviz-skill reference palette and checked with
# scripts/validate_palette.js for CVD separability/contrast (all checks
# passed; slots 1-3 for S1-S3, slots 1+8 for the two-component breakdown
# in Figure 3).
# ===================================================================

library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)

IN_DIR  <- "analysis_outputs"
OUT_DIR <- "plots_output"
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

scenario_order <- c("S1", "S2", "S3")

# validated categorical palette (dataviz skill, slots 1-3: blue/orange/aqua;
# all-pairs CVD and normal-vision checks passed in both modes)
scenario_colors <- c(
  S1 = "#2a78d6",  # blue
  S2 = "#eb6834",  # orange
  S3 = "#1baf7a"   # aqua
)

component_colors <- c(
  "Recurrent input demand (feedstock/OPEX)" = "#2a78d6",  # slot 1, blue
  "CAPEX-driven demand"                     = "#e34948"   # slot 8, red
)

# chrome/ink from the reference palette (references/palette.md)
ink_primary   <- "#0b0b0b"
ink_secondary <- "#52514e"
ink_muted     <- "#898781"
grid_hairline <- "#e1e0d9"
axis_line     <- "#c3c2b7"


channel_colors <- c(
  "Feedstock" = "#1baf7a",
  "OPEX"      = "#2a78d6",
  "CAPEX"     = "#e34948"
)

# Results are stored in million EUR (Eurostat IO table basis) -
# converted to billion EUR for the figures.
TO_BN <- 1 / 1000

base_theme <- theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = grid_hairline, linewidth = 0.35),
    axis.line = element_line(color = axis_line, linewidth = 0.3),
    axis.text = element_text(color = ink_secondary),
    axis.title = element_text(color = ink_secondary),
    plot.title = element_text(face = "bold", size = 13, color = ink_primary),
    plot.subtitle = element_text(color = ink_secondary, size = 10.5),
    plot.caption = element_text(color = ink_muted, size = 8.5, hjust = 0),
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(color = ink_primary),
    strip.text = element_text(face = "bold", color = ink_primary),
    strip.background = element_rect(fill = "grey95", color = NA)
  )

# ===================================================================
# FIGURE 1: Annual path of the total output effect, 2023-2040 (vs. REF)
# ===================================================================

annual_macro <- read.csv(file.path(IN_DIR, "annual_macro_path.csv"))

plot1_data <- annual_macro %>%
  mutate(
    scenario = factor(scenario, levels = scenario_order),
    total_output_change_bn = total_output_change * TO_BN
  )

p1 <- ggplot(plot1_data, aes(x = year, y = total_output_change_bn, color = scenario)) +
  geom_hline(yintercept = 0, color = axis_line, linewidth = 0.4) +
  geom_line(linewidth = 1.05, lineend = "round") +
  scale_color_manual(values = scenario_colors) +
  scale_x_continuous(breaks = seq(2023, 2040, by = 2)) +
  scale_y_continuous(labels = label_number(accuracy = 0.1)) +
  labs(
    title = "Economy-wide output effect relative to the reference",
    subtitle = "Difference in total output vs. REF (stationary 2023 structure), 2023-2040",
    x = NULL,
    y = "Output difference vs. REF (bn EUR)",
    caption = "Source: own calculation, NEW_model_CAPEX_separate.R / NEW_Output_Analysis.R (annual_macro_path)."
  ) +
  base_theme

ggsave(file.path(OUT_DIR, "fig1_annual_output_effect.png"), p1, width = 8, height = 5, dpi = 300, bg = "white")


# ===================================================================
# FIGURE 2: Top NONBIO sectors by output gain, 2030/2035/2040
# ===================================================================

top_gains <- read.csv(file.path(IN_DIR, "top_NONBIO_output_gains_all.csv"))

# shorten sector names for axis labels
shorten_sector <- function(x) {
  x <- sub(";.*$", "", x)
  x <- sub(" and related services$", "", x)
  x <- sub(" services$", "", x)
  x <- sub(" products$", "", x)
  ifelse(nchar(x) > 38, paste0(substr(x, 1, 36), "…"), x)
}

TOP_N <- 6  # per panel (scenario x year) - 9 panels total, so kept more compact than before

plot2_data <- top_gains %>%
  filter(year %in% c(2030, 2035, 2040), rank <= TOP_N) %>%
  mutate(
    scenario = factor(scenario, levels = scenario_order),
    year = factor(year, levels = c(2030, 2035, 2040)),
    sector_short = shorten_sector(sector),
    output_change_bn = output_change * TO_BN,
    facet_label = factor(
      paste0(as.character(scenario), " – ", as.character(year)),
      levels = as.vector(outer(scenario_order, c(2030, 2035, 2040),
                                function(s, y) paste0(s, " – ", y)))
    )
  ) %>%
  arrange(facet_label, output_change_bn) %>%
  mutate(sector_key = paste0(sector_short, "___", facet_label))

# Facet-local ordering without a tidytext dependency: each panel gets its
# own x key (sector_key), whose suffix is stripped again at label time.
# facet_wrap() (not facet_grid!) gives each panel a truly independent
# scale - needed here because both the top sectors AND their magnitude
# differ by scenario and year.
# Watch out for this ggplot2 gotcha: after coord_flip(), "free_x"/"free_y"
# feel swapped - "free_x" is what appears as the (horizontal) value axis
# on screen, because facet scales are trained BEFORE the flip.
plot2_data$sector_key <- factor(plot2_data$sector_key, levels = plot2_data$sector_key)

p2 <- ggplot(
  plot2_data,
  aes(x = sector_key, y = output_change_bn, fill = scenario)
) +
  geom_col(show.legend = FALSE, width = 0.72) +
  coord_flip() +
  scale_x_discrete(labels = function(x) sub("___.*$", "", x)) +
  scale_fill_manual(values = scenario_colors) +
  facet_wrap(~facet_label, ncol = 3, scales = "free") +
  scale_y_continuous(labels = label_number(accuracy = 0.1)) +
  labs(
    title = "Sectors with the largest output gain (NONBIO)",
    subtitle = paste0("Top ", TOP_N, " sectors by scenario and year, output change vs. REF"),
    x = NULL,
    y = "Output gain vs. REF (bn EUR)",
    caption = "Source: own calculation, NEW_Output_Analysis.R (top_NONBIO_output_gains_all)."
  ) +
  base_theme +
  theme(
    panel.spacing = unit(1, "lines"),
    plot.title = element_text(face = "bold", size = 13, margin = margin(b = 4)),
    axis.title.x = element_text(margin = margin(t = 8)),
    axis.text.y = element_text(size = 8.3),
    plot.caption = element_text(margin = margin(t = 10))
  )

ggsave(file.path(OUT_DIR, "fig2_top_sectors_2030_2035_2040.png"), p2, width = 14, height = 11, dpi = 300, bg = "white")


# ===================================================================
# FIGURE 3: Composition of the NONBIO effect (recurrent vs. CAPEX)
# ===================================================================

decomp <- read.csv(file.path(IN_DIR, "NONBIO_decomposition_all.csv"))

plot3_data <- decomp %>%
  select(year, scenario, recurrent_total_NONBIO_output, capex_total_NONBIO_output) %>%
  pivot_longer(
    cols = c(recurrent_total_NONBIO_output, capex_total_NONBIO_output),
    names_to = "component",
    values_to = "value"
  ) %>%
  mutate(
    scenario = factor(scenario, levels = scenario_order),
    year = factor(year, levels = c(2030, 2035, 2040)),
    value_bn = value * TO_BN,
    component = recode(
      component,
      recurrent_total_NONBIO_output = "Recurrent input demand (feedstock/OPEX)",
      capex_total_NONBIO_output = "CAPEX-driven demand"
    )
  )

p3 <- ggplot(plot3_data, aes(x = scenario, y = value_bn, fill = component)) +
  geom_col(position = "stack", width = 0.62) +
  facet_wrap(~year, nrow = 1) +
  scale_fill_manual(values = component_colors) +
  scale_y_continuous(labels = label_number(accuracy = 1)) +
  labs(
    title = "Composition of the NONBIO output effect",
    subtitle = "Share of recurrent input demand vs. CAPEX-driven demand in the NONBIO effect vs. REF",
    x = NULL,
    y = "NONBIO output effect (bn EUR)",
    caption = paste(
      "Both components are recalculated every year, proportional to current output (annualized); the split",
      "reflects the capital intensity (alpha cost shares) of the active technology mix, not a build- vs.",
      "operating-phase distinction.",
      "Source: own calculation, NEW_Output_Analysis.R (NONBIO_decomposition_all).",
      sep = "\n"
    )
  ) +
  base_theme +
  theme(plot.caption = element_text(hjust = 0, lineheight = 1.15))

ggsave(file.path(OUT_DIR, "fig3_recurrent_vs_capex.png"), p3, width = 8.5, height = 5.3, dpi = 300, bg = "white")


# ===================================================================
# FIGURE 4: Import dependence - CAPEX import share over time
# ===================================================================

annual_trade <- read.csv(file.path(IN_DIR, "annual_import_trade_path.csv"))

plot4_data <- annual_trade %>%
  mutate(
    scenario = factor(scenario, levels = scenario_order),
    CAPEX_import_share_pct = CAPEX_import_share * 100
  )

p4 <- ggplot(plot4_data, aes(x = year, y = CAPEX_import_share_pct, color = scenario)) +
  geom_line(linewidth = 1.05, lineend = "round") +
  scale_color_manual(values = scenario_colors) +
  scale_x_continuous(breaks = seq(2023, 2040, by = 2)) +
  scale_y_continuous(labels = label_number(suffix = "%", accuracy = 1)) +
  labs(
    title = "Import share of CAPEX-driven demand",
    subtitle = "Share of CAPEX demand (machinery, construction, electronics, etc.) that is imported",
    x = NULL,
    y = "Imported CAPEX share",
    caption = "Source: own calculation, NEW_Output_Analysis.R (annual_import_trade_path)."
  ) +
  base_theme

ggsave(file.path(OUT_DIR, "fig4_capex_import_share.png"), p4, width = 8.8, height = 5, dpi = 300, bg = "white")

# ===================================================================
# FIGURE 5A: Absolute upstream output requirements
#            Feedstock / OPEX / CAPEX
# ===================================================================

channel_structure <- read.csv(
  file.path(
    IN_DIR,
    "channel_output_structure_all.csv"
  )
)

channel_abs <- channel_structure %>%
  select(
    year,
    scenario,
    feedstock_total_output,
    opex_total_output,
    capex_total_output
  ) %>%
  pivot_longer(
    cols = c(
      feedstock_total_output,
      opex_total_output,
      capex_total_output
    ),
    names_to = "channel",
    values_to = "value"
  ) %>%
  mutate(
    scenario = factor(
      scenario,
      levels = scenario_order
    ),

    year = factor(
      year,
      levels = c(2030, 2035, 2040)
    ),

    channel = recode(
      channel,
      feedstock_total_output = "Feedstock",
      opex_total_output      = "OPEX",
      capex_total_output     = "CAPEX"
    ),

    channel = factor(
      channel,
      levels = c(
        "Feedstock",
        "OPEX",
        "CAPEX"
      )
    ),

    value_bn = value * TO_BN
  )


p5a <- ggplot(
  channel_abs,
  aes(
    x = scenario,
    y = value_bn,
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
    labels = label_number(
      accuracy = 0.1
    )
  ) +
  labs(
    title =
      "Domestic upstream output requirements by input channel",

    subtitle =
      "Absolute feedstock-, OPEX- and CAPEX-driven upstream output associated with each scenario",

    x = NULL,

    y =
      "Domestic upstream output requirement (bn EUR)",

    caption = paste(
      "Scenario-internal structural comparison; not a change relative to REF.",
      "CAPEX represents annualized capital requirements at their absolute scenario level.",
      "Source: own calculation, NEW_Output_Analysis.R (channel_output_structure_all).",
      sep = "\n"
    )
  ) +
  base_theme +
  theme(
    plot.caption =
      element_text(
        hjust = 0,
        lineheight = 1.15
      )
  )


ggsave(
  file.path(
    OUT_DIR,
    "fig5a_channel_output_absolute.png"
  ),
  p5a,
  width = 8.5,
  height = 5.3,
  dpi = 300,
  bg = "white"
)

                   # ===================================================================
# FIGURE 5B: Composition shares
#            Feedstock / OPEX / CAPEX
# ===================================================================

channel_shares <- channel_structure %>%
  select(
    year,
    scenario,
    feedstock_share,
    opex_share,
    capex_share
  ) %>%
  pivot_longer(
    cols = c(
      feedstock_share,
      opex_share,
      capex_share
    ),
    names_to = "channel",
    values_to = "share"
  ) %>%
  mutate(
    scenario = factor(
      scenario,
      levels = scenario_order
    ),

    year = factor(
      year,
      levels = c(2030, 2035, 2040)
    ),

    channel = recode(
      channel,
      feedstock_share = "Feedstock",
      opex_share      = "OPEX",
      capex_share     = "CAPEX"
    ),

    channel = factor(
      channel,
      levels = c(
        "Feedstock",
        "OPEX",
        "CAPEX"
      )
    )
  )


p5b <- ggplot(
  channel_shares,
  aes(
    x = scenario,
    y = share,
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
    labels = label_percent(
      accuracy = 1
    ),
    limits = c(0, 1),
    expand = expansion(
      mult = c(0, 0)
    )
  ) +
  labs(
    title =
      "Composition of domestic upstream output requirements",

    subtitle =
      "Relative contribution of feedstock, OPEX and CAPEX to scenario-specific upstream output requirements",

    x = NULL,

    y =
      "Share of upstream output requirement",

    caption = paste(
      "Shares sum to 100% within each scenario-year.",
      "This figure compares production structures rather than changes relative to REF.",
      "Source: own calculation, NEW_Output_Analysis.R (channel_output_structure_all).",
      sep = "\n"
    )
  ) +
  base_theme


ggsave(
  file.path(
    OUT_DIR,
    "fig5b_channel_output_shares.png"
  ),
  p5b,
  width = 8.5,
  height = 5.3,
  dpi = 300,
  bg = "white"
)


                   # ===================================================================
# FIGURE 5C: Upstream output intensity per EUR BIO output
# ===================================================================

channel_intensity <- channel_structure %>%
  select(
    year,
    scenario,
    feedstock_output_per_eur_BIO,
    opex_output_per_eur_BIO,
    capex_output_per_eur_BIO
  ) %>%
  pivot_longer(
    cols = c(
      feedstock_output_per_eur_BIO,
      opex_output_per_eur_BIO,
      capex_output_per_eur_BIO
    ),
    names_to = "channel",
    values_to = "intensity"
  ) %>%
  mutate(
    scenario = factor(
      scenario,
      levels = scenario_order
    ),

    year = factor(
      year,
      levels = c(2030, 2035, 2040)
    ),

    channel = recode(
      channel,
      feedstock_output_per_eur_BIO = "Feedstock",
      opex_output_per_eur_BIO      = "OPEX",
      capex_output_per_eur_BIO     = "CAPEX"
    ),

    channel = factor(
      channel,
      levels = c(
        "Feedstock",
        "OPEX",
        "CAPEX"
      )
    )
  )


p5c <- ggplot(
  channel_intensity,
  aes(
    x = scenario,
    y = intensity,
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
    labels = label_number(
      accuracy = 0.01
    )
  ) +
  labs(
    title =
      "Domestic upstream output intensity of biofuel production",

    subtitle =
      "Upstream output requirements per euro of domestic biofuel output, by input channel",

    x = NULL,

    y =
      "Upstream output requirement per EUR biofuel output (EUR/EUR)",

    caption = paste(
      "Normalizing by domestic biofuel output separates production-structure effects from scenario scale.",
      "CAPEX represents annualized capital requirements.",
      "Source: own calculation, NEW_Output_Analysis.R (channel_output_structure_all).",
      sep = "\n"
    )
  ) +
  base_theme


ggsave(
  file.path(
    OUT_DIR,
    "fig5c_channel_output_intensity.png"
  ),
  p5c,
  width = 8.5,
  height = 5.3,
  dpi = 300,
  bg = "white"
)

# ===================================================================
# ADVANCED BIOFUELS:
# DOMESTIC OPERATING-CHAIN GVA PER EUR ADVANCED BIOFUEL OUTPUT
#
# CAPEX deliberately excluded from the main measure.
# ===================================================================

adv_gva <-
  read.csv(
    file.path(
      IN_DIR,
      "advanced_GVA_summary_all.csv"
    )
  )


adv_gva_plot <-
  adv_gva %>%

  select(
    year,
    scenario,
    direct_BIO_GVA_per_eur_ADV,
    feedstock_upstream_GVA_per_eur_ADV,
    opex_upstream_GVA_per_eur_ADV
  ) %>%

  pivot_longer(
    cols = c(
      direct_BIO_GVA_per_eur_ADV,
      feedstock_upstream_GVA_per_eur_ADV,
      opex_upstream_GVA_per_eur_ADV
    ),
    names_to = "component",
    values_to = "GVA_intensity"
  ) %>%

  mutate(

    scenario =
      factor(
        scenario,
        levels = c(
          "S1",
          "S2",
          "S3"
        )
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


adv_gva_colors <- c(
  "Direct advanced-biofuel GVA" =
    "#eb6834",

  "Feedstock-driven upstream GVA" =
    "#1baf7a",

  "OPEX-driven upstream GVA" =
    "#2a78d6"
)


p_adv_gva <-
  ggplot(
    adv_gva_plot,
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
      adv_gva_colors
  ) +

  scale_y_continuous(
    labels =
      scales::label_number(
        accuracy = 0.01
      )
  ) +

  labs(

    title =
      "Domestic GVA intensity of advanced biofuel production",

    subtitle =
      "Direct and recurrent upstream GVA per euro of domestic advanced-biofuel output",

    x =
      NULL,

    y =
      "Domestic GVA per EUR advanced-biofuel output (EUR/EUR)",

    caption =
      paste(
        "Scenario-internal comparison; conventional biofuels and RFNBOs are excluded.",
        "Upstream GVA includes domestic feedstock- and OPEX-driven supply-chain effects.",
        "CAPEX-related GVA is excluded from this main measure.",
        sep = "\n"
      )
  ) +

  base_theme


ggsave(
  file.path(
    OUT_DIR,
    "fig_advanced_GVA_intensity.png"
  ),
  p_adv_gva,
  width = 8.8,
  height = 5.4,
  dpi = 300,
  bg = "white"
)

                   # ===================================================================
# TECHNOLOGY-SPECIFIC ADVANCED-BIOFUEL GVA INTENSITY
# 2040
# ===================================================================

adv_product_gva <-
  read.csv(
    file.path(
      IN_DIR,
      "advanced_biofuel_GVA_all.csv"
    )
  ) %>%

  filter(
    year == 2040,
    BIO_output > 1e-10
  ) %>%

  select(
    year,
    scenario,
    biofuel,
    BIO_output,

    direct_BIO_GVA_per_eur_BIO,
    feedstock_upstream_GVA_per_eur_BIO,
    opex_upstream_GVA_per_eur_BIO
  ) %>%

  pivot_longer(
    cols = c(
      direct_BIO_GVA_per_eur_BIO,
      feedstock_upstream_GVA_per_eur_BIO,
      opex_upstream_GVA_per_eur_BIO
    ),
    names_to = "component",
    values_to = "GVA_intensity"
  ) %>%

  mutate(

    scenario =
      factor(
        scenario,
        levels = c(
          "S1",
          "S2",
          "S3"
        )
      ),

    biofuel =
      recode(
        biofuel,

        adv_biodiesel =
          "Advanced biodiesel",

        adv_biogasoline =
          "Advanced biogasoline",

        adv_bio_kerosene =
          "Advanced bio-kerosene",

        adv_bio_hfo =
          "Advanced bio-HFO",

        adv_biogas =
          "Advanced biogas"
      ),

    component =
      recode(
        component,

        direct_BIO_GVA_per_eur_BIO =
          "Direct advanced-biofuel GVA",

        feedstock_upstream_GVA_per_eur_BIO =
          "Feedstock-driven upstream GVA",

        opex_upstream_GVA_per_eur_BIO =
          "OPEX-driven upstream GVA"
      )
  )


p_adv_product_gva <-
  ggplot(
    adv_product_gva,
    aes(
      x = biofuel,
      y = GVA_intensity,
      fill = component
    )
  ) +

  geom_col(
    width = 0.67
  ) +

  facet_wrap(
    ~scenario,
    nrow = 1
  ) +

  scale_fill_manual(
    values =
      adv_gva_colors
  ) +

  scale_y_continuous(
    labels =
      scales::label_number(
        accuracy = 0.01
      )
  ) +

  labs(

    title =
      "Domestic GVA intensity by advanced biofuel pathway",

    subtitle =
      "Domestic GVA associated with one euro of domestic production, 2040",

    x =
      NULL,

    y =
      "Domestic GVA per EUR advanced-biofuel output (EUR/EUR)",

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
    "fig_advanced_GVA_by_product_2040.png"
  ),
  p_adv_product_gva,
  width = 12,
  height = 6,
  dpi = 300,
  bg = "white"
)
                   
cat("All output figures saved to:", OUT_DIR, "\n")
