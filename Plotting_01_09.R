# ===================================================================
# PLOTTING – ADVANCED BIOFUEL OUTPUT ANALYSIS
# ===================================================================

# Run analysis first and create all required analysis objects
source("Analysis_Output.R")

library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)


# ===================================================================
# BASIC SETTINGS
# ===================================================================

scenario_order <- c("S1", "S2", "S3")

benchmark_year_order <- c(2030, 2035, 2040)

# Results are reported in million EUR in the model.
# Divide by 1,000 to report billion EUR in figures.


# ===================================================================
# FIGURE 1
# ADVANCED BIOFUEL OUTPUT AND ASSOCIATED NONBIO OUTPUT
# ===================================================================

plot_data_1 <-
  advanced_output_summary_all %>%
  select(
    year,
    scenario,
    advanced_biofuel_output_change,
    advanced_total_NONBIO
  ) %>%
  pivot_longer(
    cols = c(
      advanced_biofuel_output_change,
      advanced_total_NONBIO
    ),
    names_to = "effect",
    values_to = "change_mEUR"
  ) %>%
  mutate(
    scenario = factor(
      scenario,
      levels = scenario_order
    ),

    year = factor(
      year,
      levels = benchmark_year_order
    ),

    effect = recode(
      effect,
      advanced_biofuel_output_change =
        "Advanced biofuel output",
      advanced_total_NONBIO =
        "Associated non-biofuel output"
    ),

    change_bnEUR =
      change_mEUR / 1000
  )


p1 <- ggplot(
  plot_data_1,
  aes(
    x = scenario,
    y = change_bnEUR,
    fill = effect
  )
) +
  geom_col(
    position = position_dodge(width = 0.75),
    width = 0.65
  ) +
  geom_hline(
    yintercept = 0,
    linewidth = 0.4
  ) +
  facet_wrap(
    ~ year,
    nrow = 1,
    scales = "free_y"
  ) +
  labs(
    title =
      "Advanced biofuel production and associated upstream output effects",
    subtitle =
      "Change relative to the same-year reference scenario",
    x = NULL,
    y = "Change in gross output (€ billion)",
    fill = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    panel.grid.minor = element_blank()
  )


p1

# ===================================================================
# FIGURE 2
# NONBIO OUTPUT PER EUR OF ADDITIONAL ADVANCED BIOFUEL OUTPUT
# ===================================================================

plot_data_2 <-
  advanced_output_summary_all %>%
  mutate(
    scenario = factor(
      scenario,
      levels = scenario_order
    ),

    year = factor(
      year,
      levels = benchmark_year_order
    )
  )


p2 <- ggplot(
  plot_data_2,
  aes(
    x = scenario,
    y = NONBIO_per_eur_advanced_biofuel
  )
) +
  geom_col(
    width = 0.6
  ) +
  geom_hline(
    yintercept = 0,
    linewidth = 0.4
  ) +
  facet_wrap(
    ~ year,
    nrow = 1
  ) +
  geom_text(
    aes(
      label = sprintf(
        "%.2f",
        NONBIO_per_eur_advanced_biofuel
      )
    ),
    vjust = -0.4,
    size = 3.5
  ) +
  labs(
    title =
      "Domestic upstream output intensity of advanced biofuel production",
    subtitle =
      "Additional non-biofuel output per €1 of additional advanced biofuel output",
    x = NULL,
    y = "€ non-biofuel output per €1 advanced biofuel output"
  ) +
  expand_limits(y = 0) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank()
  )


p2


# ===================================================================
# FIGURE 3
# COMPOSITION OF ADVANCED-BIOFUEL-SECTOR CONTRIBUTIONS
# ===================================================================

plot_data_3 <-
  advanced_biofuel_contributions_all %>%
  mutate(
    scenario = factor(
      scenario,
      levels = scenario_order
    ),

    year = factor(
      year,
      levels = benchmark_year_order
    ),

    biofuel = factor(
      biofuel,
      levels = c(
        "Advanced biodiesel",
        "Advanced biogasoline",
        "Advanced bio-kerosene",
        "Advanced bio-HFO",
        "Advanced biogas"
      )
    )
  )


p3 <- ggplot(
  plot_data_3,
  aes(
    x = scenario,
    y = advanced_contribution_share,
    fill = biofuel
  )
) +
  geom_col(
    width = 0.65
  ) +
  facet_wrap(
    ~ year,
    nrow = 1
  ) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1)
  ) +
  labs(
    title =
      "Composition of advanced-biofuel-sector contributions to upstream output",
    subtitle =
      "Share of non-biofuel output associated with advanced-biofuel producing sectors",
    x = NULL,
    y = "Share of advanced-biofuel-associated non-biofuel output",
    fill = "Advanced biofuel sector"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    panel.grid.minor = element_blank()
  )


p3

# ===================================================================
# FIGURE 4
# ANNUAL ADVANCED-BIOFUEL-ASSOCIATED NONBIO OUTPUT, 2023-2040
# ===================================================================


calculate_annual_advanced_NONBIO <- function(
    scenario_name
) {

  scenario_dynamic <-
    dynamic_results[[scenario_name]]

  REF_dynamic <-
    dynamic_results$REF


  annual_results <-
    lapply(
      seq_along(annual_years),
      function(i) {


        # NONBIO -> NONBIO coefficient block
        A_NN <-
          REF_dynamic$A_dom[
            i,
            NONBIO,
            NONBIO
          ]


        # NONBIO Leontief inverse
        L_NN <-
          solve(
            base::diag(length(NONBIO)) -
              A_NN
          )


        # Direct NONBIO requirements of advanced biofuel sectors
        # in the scenario
        direct_ADV_scenario <-
          scenario_dynamic$A_dom[
            i,
            NONBIO,
            ADV_BIO
          ] %*%
          scenario_dynamic$X_bio[
            i,
            ADV_BIO
          ]


        # Same requirements in REF
        direct_ADV_REF <-
          REF_dynamic$A_dom[
            i,
            NONBIO,
            ADV_BIO
          ] %*%
          REF_dynamic$X_bio[
            i,
            ADV_BIO
          ]


        # Advanced-biofuel-associated direct NONBIO shock
        direct_shock <-
          direct_ADV_scenario -
          direct_ADV_REF


        # Including subsequent NONBIO production rounds
        total_NONBIO_effect <-
          as.numeric(
            L_NN %*%
              direct_shock
          )


        data.frame(
          year =
            annual_years[i],

          scenario =
            scenario_name,

          advanced_biofuel_output_change =
            sum(
              scenario_dynamic$X_bio[
                i,
                ADV_BIO
              ] -
                REF_dynamic$X_bio[
                  i,
                  ADV_BIO
                ]
            ),

          advanced_total_NONBIO =
            sum(
              total_NONBIO_effect
            )
        )
      }
    )


  do.call(
    rbind,
    annual_results
  )
}


advanced_annual_output_path <-
  do.call(
    rbind,
    lapply(
      scenario_order,
      calculate_annual_advanced_NONBIO
    )
  )


rownames(
  advanced_annual_output_path
) <- NULL


advanced_annual_output_path <-
  advanced_annual_output_path %>%
  mutate(
    scenario = factor(
      scenario,
      levels = scenario_order
    ),

    advanced_total_NONBIO_bn =
      advanced_total_NONBIO / 1000
  )


# -------------------------------------------------------------------
# CHECK:
# Annual reconstruction must reproduce benchmark results
# -------------------------------------------------------------------

annual_benchmark_check <-
  advanced_annual_output_path %>%
  filter(
    year %in% c(
      2030,
      2035,
      2040
    )
  ) %>%
  select(
    year,
    scenario,
    annual_advanced_NONBIO =
      advanced_total_NONBIO
  ) %>%
  left_join(
    advanced_output_summary_all %>%
      select(
        year,
        scenario,
        benchmark_advanced_NONBIO =
          advanced_total_NONBIO
      ),
    by = c(
      "year",
      "scenario"
    )
  ) %>%
  mutate(
    difference =
      annual_advanced_NONBIO -
      benchmark_advanced_NONBIO
  )


annual_benchmark_check


p4 <- ggplot(
  advanced_annual_output_path,
  aes(
    x = year,
    y = advanced_total_NONBIO_bn,
    group = scenario,
    linetype = scenario
  )
) +
  geom_hline(
    yintercept = 0,
    linewidth = 0.4
  ) +
  geom_line(
    linewidth = 1
  ) +
  geom_point(
    data =
      advanced_annual_output_path %>%
      filter(
        year %in%
          c(
            2030,
            2035,
            2040
          )
      ),
    size = 2
  ) +
  scale_x_continuous(
    breaks = c(
      2023,
      2025,
      2030,
      2035,
      2040
    )
  ) +
  labs(
    title =
      "Evolution of upstream output associated with advanced biofuel production",
    subtitle =
      "Annual change relative to the reference scenario",
    x = "Year",
    y = "Additional non-biofuel output (€ billion)",
    linetype = "Scenario"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    panel.grid.minor = element_blank()
  )


p4


# ===================================================================
# FIGURE 5
# TOP NONBIO SECTORS ASSOCIATED WITH ADVANCED BIOFUEL PRODUCTION
# ===================================================================


plot_top_advanced_NONBIO <- function(
    selected_year,
    selected_scenario,
    n = 10
) {

  plot_data <-
    get_top_advanced_NONBIO_sectors(
      year = selected_year,
      scenario = selected_scenario,
      n = n
    ) %>%
    arrange(
      advanced_total_output_effect
    ) %>%
    mutate(
      sector =
        factor(
          sector,
          levels = sector
        ),

      output_change_bn =
        advanced_total_output_effect /
        1000
    )


  ggplot(
    plot_data,
    aes(
      x = sector,
      y = output_change_bn
    )
  ) +
    geom_col(
      width = 0.65
    ) +
    coord_flip() +
    geom_hline(
      yintercept = 0,
      linewidth = 0.4
    ) +
    labs(
      title =
        paste0(
          "Non-biofuel sectors most affected by advanced biofuel production – ",
          selected_scenario,
          ", ",
          selected_year
        ),
      subtitle =
        "Total first- and higher-order output effect relative to REF",
      x = NULL,
      y = "Additional gross output (€ billion)"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid.minor = element_blank()
    )
}

p5_S1 <-
  plot_top_advanced_NONBIO(
    selected_year = 2030,
    selected_scenario = "S1",
    n = 10
  )

p5_S2 <-
  plot_top_advanced_NONBIO(
    selected_year = 2030,
    selected_scenario = "S2",
    n = 10
  )

p5_S3 <-
  plot_top_advanced_NONBIO(
    selected_year = 2030,
    selected_scenario = "S3",
    n = 10
  )


p5_S1
p5_S2
p5_S3
