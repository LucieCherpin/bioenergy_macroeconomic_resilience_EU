# ==========================================================
# ==========================================================
##### right now: three goals: 
##### 1) showing the absolute changes in X and GVA
##### 2) showing the relative changes in X and GVA  (rleative to entire economy) 
##### 3) showing WHERE the change comes from, i.e. in BIO or NONBIO (upsteram) sectors
# ==========================================================
# ==========================================================

# ==========================================================
# LOAD MODEL RESULTS
# ==========================================================
source("C:/Users/Lucie Cherpin/OneDrive/BEST MA/Scenarios_all_variables_tryout_NEWEST version.R")

library(ggplot2)
library(tidyr)
library(dplyr)


# ==========================================================
# Changes in output between X in S1 and X in S_REF
# ==========================================================

delta_X_S1_2030 <-
  S1_endpoint_2030$X -
  REF_endpoint_2030$X

delta_X_S2_2030 <-
  S2_endpoint_2030$X -
  REF_endpoint_2030$X


delta_GVA_S1_2030 <-
  S1_endpoint_2030$GVA -
  REF_endpoint_2030$GVA

delta_GVA_S2_2030 <-
  S2_endpoint_2030$GVA -
  REF_endpoint_2030$GVA


# Aggregate macroeconomic effects
macro_effects_2030 <- data.frame(

  scenario = c("S1", "S2"),

  output_change_mEUR = c(
    sum(delta_X_S1_2030),
    sum(delta_X_S2_2030)
  ),

  GVA_change_mEUR = c(
    sum(delta_GVA_S1_2030),
    sum(delta_GVA_S2_2030)
  )
)

macro_effects_2030

macro_effects_2030$output_change_pct <-
  100 *
  macro_effects_2030$output_change_mEUR /
  sum(REF_endpoint_2030$X)


macro_effects_2030$GVA_change_pct <-
  100 *
  macro_effects_2030$GVA_change_mEUR /
  sum(REF_endpoint_2030$GVA)


macro_effects_2030


# ==========================================================
# PLOT 1: ABSOLUTE macroeconomic changes
# ==========================================================

library(ggplot2)
library(tidyr)
library(dplyr)

plot_macro_abs <- macro_effects_2030 %>%
  select(
    scenario,
    output_change_mEUR,
    GVA_change_mEUR
  ) %>%
  pivot_longer(
    cols = c(
      output_change_mEUR,
      GVA_change_mEUR
    ),
    names_to = "indicator",
    values_to = "change_mEUR"
  ) %>%
  mutate(
    indicator = recode(
      indicator,
      output_change_mEUR = "Gross output",
      GVA_change_mEUR = "Gross value added"
    )
  )

ggplot(
  plot_macro_abs,
  aes(
    x = scenario,
    y = change_mEUR,
    fill = indicator
  )
) +
  geom_col(
    position = position_dodge(width = 0.7),
    width = 0.6
  ) +
  geom_hline(
    yintercept = 0,
    linewidth = 0.4
  ) +
  scale_y_continuous(
    labels = scales::label_number(
      big.mark = ",",
      accuracy = 1
    )
  ) +
  labs(
    title = "Macroeconomic effects of the biofuel policy scenarios in 2030",
    subtitle = "Change relative to the reference scenario",
    x = NULL,
    y = "Change relative to REF (million EUR)",
    fill = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top"
  )



# ==========================================================
# PLOT 2: RELATIVE macroeconomic chnages
# ==========================================================

plot_macro_pct <- macro_effects_2030 %>%
  select(
    scenario,
    output_change_pct,
    GVA_change_pct
  ) %>%
  pivot_longer(
    cols = c(
      output_change_pct,
      GVA_change_pct
    ),
    names_to = "indicator",
    values_to = "change_pct"
  ) %>%
  mutate(
    indicator = recode(
      indicator,
      output_change_pct = "Gross output",
      GVA_change_pct = "Gross value added"
    )
  )

ggplot(
  plot_macro_pct,
  aes(
    x = scenario,
    y = change_pct,
    fill = indicator
  )
) +
  geom_col(
    position = position_dodge(width = 0.7),
    width = 0.6
  ) +
  geom_hline(
    yintercept = 0,
    linewidth = 0.4
  ) +
  labs(
    title = "Relative macroeconomic effects of the biofuel policy scenarios",
    subtitle = "2030 change relative to REF",
    x = NULL,
    y = "Change relative to REF (%)",
    fill = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top"
  )

macro_effects_2030$output_change_pct <-
  100 *
  macro_effects_2030$output_change_mEUR /
  sum(REF_endpoint_2030$X)


macro_effects_2030$GVA_change_pct <-
  100 *
  macro_effects_2030$GVA_change_mEUR /
  sum(REF_endpoint_2030$GVA)


macro_effects_2030


macro_decomposition_2030 <- data.frame(

  scenario = rep(
    c("S1", "S2"),
    each = 2
  ),

  effect = rep(
    c(
      "Biofuel sectors",
      "Upstream / other sectors"
    ),
    times = 2
  ),

  output_change = c(

    sum(delta_X_S1_2030[BIO]),
    sum(delta_X_S1_2030[NONBIO]),

    sum(delta_X_S2_2030[BIO]),
    sum(delta_X_S2_2030[NONBIO])
  ),

  GVA_change = c(

    sum(delta_GVA_S1_2030[BIO]),
    sum(delta_GVA_S1_2030[NONBIO]),

    sum(delta_GVA_S2_2030[BIO]),
    sum(delta_GVA_S2_2030[NONBIO])
  )
)

macro_decomposition_2030


ggplot(
  macro_decomposition_2030,
  aes(
    x = scenario,
    y = GVA_change,
    fill = effect
  )
) +
  geom_col() +
  geom_hline(
    yintercept = 0,
    linewidth = 0.4
  ) +
  scale_y_continuous(
    labels = scales::label_number(
      big.mark = ","
    )
  ) +
  labs(
    title = "Sources of the GVA effect of the biofuel policy scenarios",
    subtitle = "2030 change relative to REF",
    x = NULL,
    y = "GVA change (million EUR)",
    fill = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top"
  )
