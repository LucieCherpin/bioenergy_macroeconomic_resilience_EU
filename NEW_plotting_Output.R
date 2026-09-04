# ===================================================================
# PLOTTING - OUTPUT-EBENE, KERNERGEBNISSE FUER DIE MA
# Liest die von NEW_Output_Analysis.R gespeicherten CSVs aus
# analysis_outputs/ und erzeugt die vier Kern-"Ergebnisdimensionen":
#   1) Jahrespfad Gesamtoutput-Effekt 2023-2040 (ggue. REF), S1/S2/S3
#   2) Top NONBIO-Sektoren nach Output-Zuwachs, 2030 / 2035 / 2040, je Szenario
#   3) Zusammensetzung des NONBIO-Effekts: laufende Vorleistungen vs. CAPEX
#      (Kapitalintensitaet des aktiven Technologiemixes, KEINE Bauphase!)
#   4) Importabhaengigkeit: CAPEX-Importanteil ueber die Zeit
#
# Farben: aus dem dataviz-Skill-Referenzpalette entnommen und mit
# scripts/validate_palette.js gegen CVD-Trennschaerfe/Kontrast geprueft
# (alle Checks bestanden; Kategorie 1-3 fuer S1-S3, Kategorie 1+8 fuer
# die Zwei-Komponenten-Zerlegung in Abbildung 3).
# ===================================================================

library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)

IN_DIR  <- "analysis_outputs"
OUT_DIR <- "plots_output"
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

scenario_order <- c("S1", "S2", "S3")

# validierte kategoriale Palette (dataviz-Skill, Slots 1-3: blau/orange/aqua;
# all-pairs CVD- und Normalsicht-Checks in beiden Modi bestanden)
scenario_colors <- c(
  S1 = "#2a78d6",  # blau
  S2 = "#eb6834",  # orange
  S3 = "#1baf7a"   # aqua
)

component_colors <- c(
  "Laufende Vorleistungen (Feedstock/OPEX)" = "#2a78d6",  # Slot 1, blau
  "CAPEX-getriebene Nachfrage"              = "#e34948"   # Slot 8, rot
)

# Chrome/Ink aus der Referenzpalette (references/palette.md)
ink_primary   <- "#0b0b0b"
ink_secondary <- "#52514e"
ink_muted     <- "#898781"
grid_hairline <- "#e1e0d9"
axis_line     <- "#c3c2b7"

# Ergebnisse liegen in Mio. EUR vor (Eurostat-IO-Tabellenbasis) -
# fuer die Abbildungen auf Mrd. EUR umgerechnet.
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
# ABBILDUNG 1: Jahrespfad Gesamtoutput-Effekt 2023-2040 (vs. REF)
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
    title = "Gesamtwirtschaftlicher Output-Effekt gegenüber der Referenz",
    subtitle = "Differenz des Gesamtoutputs zu REF (stationäre 2023-Struktur), 2023-2040",
    x = NULL,
    y = "Output-Differenz ggü. REF (Mrd. EUR)",
    caption = "Quelle: eigene Berechnung, NEW_model_CAPEX_separate.R / NEW_Output_Analysis.R (annual_macro_path)."
  ) +
  base_theme

ggsave(file.path(OUT_DIR, "fig1_annual_output_effect.png"), p1, width = 8, height = 5, dpi = 300, bg = "white")


# ===================================================================
# ABBILDUNG 2: Top NONBIO-Sektoren nach Output-Zuwachs, 2030/2035/2040
# ===================================================================

top_gains <- read.csv(file.path(IN_DIR, "top_NONBIO_output_gains_all.csv"))

# Sektornamen kuerzen fuer die Achsenbeschriftung
shorten_sector <- function(x) {
  x <- sub(";.*$", "", x)
  x <- sub(" and related services$", "", x)
  x <- sub(" services$", "", x)
  x <- sub(" products$", "", x)
  ifelse(nchar(x) > 38, paste0(substr(x, 1, 36), "…"), x)
}

TOP_N <- 6  # pro Panel (Szenario x Jahr) - 9 Panels insgesamt, daher kompakter als vorher

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

# Facet-lokale Sortierung ohne tidytext-Abhaengigkeit: pro Panel ein
# eigener x-Schluessel (sector_key), dessen Suffix beim Beschriften wieder
# entfernt wird. facet_wrap() (nicht facet_grid!) vergibt pro Panel eine
# wirklich unabhaengige Skalierung - das ist hier noetig, weil sich die
# Top-Sektoren UND die Groessenordnung je Szenario und Jahr unterscheiden.
# Achtung ggplot2-Falle: nach coord_flip() vertauschen sich "free_x"/"free_y"
# gefuehlt - "free_x" ist hier das, was auf dem Bild als Werte-Achse
# (horizontal) erscheint, weil facet-Skalen VOR dem Flip trainiert werden.
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
    title = "Sektoren mit dem größten Output-Zuwachs (NONBIO)",
    subtitle = paste0("Top ", TOP_N, " Sektoren je Szenario und Jahr, Output-Veränderung gegenüber REF"),
    x = NULL,
    y = "Output-Zuwachs ggü. REF (Mrd. EUR)",
    caption = "Quelle: eigene Berechnung, NEW_Output_Analysis.R (top_NONBIO_output_gains_all)."
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
# ABBILDUNG 3: Zusammensetzung des NONBIO-Effekts (recurrent vs. CAPEX)
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
      recurrent_total_NONBIO_output = "Laufende Vorleistungen (Feedstock/OPEX)",
      capex_total_NONBIO_output = "CAPEX-getriebene Nachfrage"
    )
  )

p3 <- ggplot(plot3_data, aes(x = scenario, y = value_bn, fill = component)) +
  geom_col(position = "stack", width = 0.62) +
  facet_wrap(~year, nrow = 1) +
  scale_fill_manual(values = component_colors) +
  scale_y_continuous(labels = label_number(accuracy = 1)) +
  labs(
    title = "Zusammensetzung des NONBIO-Output-Effekts",
    subtitle = "Anteil laufender Vorleistungen vs. CAPEX-getriebener Nachfrage am NONBIO-Effekt ggü. REF",
    x = NULL,
    y = "NONBIO-Output-Effekt (Mrd. EUR)",
    caption = paste(
      "Beide Komponenten sind jährlich neu und proportional zum aktuellen Output berechnet (annualisiert);",
      "der Anteil spiegelt die Kapitalintensität (alpha-Kostenanteile) des aktiven Technologiemixes wider,",
      "nicht eine Bau- vs. Betriebsphase.",
      "Quelle: eigene Berechnung, NEW_Output_Analysis.R (NONBIO_decomposition_all).",
      sep = "\n"
    )
  ) +
  base_theme +
  theme(plot.caption = element_text(hjust = 0, lineheight = 1.15))

ggsave(file.path(OUT_DIR, "fig3_recurrent_vs_capex.png"), p3, width = 8.5, height = 5.3, dpi = 300, bg = "white")


# ===================================================================
# ABBILDUNG 4: Importabhaengigkeit - CAPEX-Importanteil ueber die Zeit
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
    title = "Importanteil der CAPEX-getriebenen Nachfrage",
    subtitle = "Anteil der CAPEX-Nachfrage (Maschinen, Bau, Elektronik u.Ä.), der importiert wird",
    x = NULL,
    y = "Importierter CAPEX-Anteil",
    caption = "Quelle: eigene Berechnung, NEW_Output_Analysis.R (annual_import_trade_path)."
  ) +
  base_theme

ggsave(file.path(OUT_DIR, "fig4_capex_import_share.png"), p4, width = 8.8, height = 5, dpi = 300, bg = "white")

cat("Alle 4 Abbildungen gespeichert in:", OUT_DIR, "\n")
