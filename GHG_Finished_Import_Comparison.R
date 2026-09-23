# Finished-fuel import emissions comparison
#
# This is a read-only post-processing diagnostic. It does not alter the
# economic solve or the domestic-production GHG accounting in GHG_Analysis.R.
# It compares (i) the workbook's finished-import EI method from
# Imports_Exports_All_Scenarios.xlsx and (ii) a direct
# imported-sector environmental-extension method for the same imported fuel
# volumes. The latter is not a foreign Leontief lifecycle.

options(stringsAsFactors = FALSE, scipen = 999)
suppressPackageStartupMessages(library(readxl))

IMPORT_WORKBOOK <- "Imports_Exports_All_Scenarios.xlsx"
RESULTS_FILE <- "model_results_CAPEX_separate.rds"
IMP_EXT_FILE <- "IOT_EU27_2022_IMP_environmental_extensions.csv"
OUTPUT_DIR <- "ghg_outputs"
MTOE_TO_MJ <- 41868000000

assert <- function(ok, msg) {
  if (!isTRUE(ok)) stop(msg, call. = FALSE)
}

gwp100_cf <- function(stressor, unit) {
  s <- trimws(as.character(stressor))
  u <- trimws(as.character(unit))
  if (u == "kg CO2-eq" && s %in% c("HFC - air", "PFC - air")) return(1)
  if (s == "SF6 - air") return(25184)
  if (s == "NF3 - air") return(17423)
  if (grepl("^CO2_bio ", s) || s == "CO2 - waste - biogenic - air") return(0)
  if (grepl("^CO2 ", s)) return(1)
  if (s %in% c("CH4_bio - combustion - air", "CH4 - combustion - air",
               "CH4 - agriculture - air", "CH4 - waste - air")) return(27)
  if (grepl("^CH4 - non combustion", s)) return(29.8)
  if (grepl("^N2O", s)) return(273)
  0
}

category_manifest <- function() {
  data.frame(
    import_category = c(
      "Advanced biodiesel (FAME / HVO)",
      "Advanced biogasoline (ethanol)",
      "Advanced biogas / biomethane",
      "Advanced bio-kerosene (HEFA / ATJ / FT-SPK)",
      "Advanced bio-HFO",
      "Conventional biodiesel (food/feed crops)",
      "Conventional biogasoline (food/feed crops)",
      "RFNBO e-methanol (IVC 8c)",
      "RFNBO e-methane (IVC 9b)"
    ),
    model_biofuel = c(
      "adv_biodiesel", "adv_biogasoline", "adv_biogas",
      "adv_bio_kerosene", "adv_bio_hfo", "conv_biodiesel",
      "conv_biogasoline", "RFNBOs", "RFNBOs"
    ),
    exio_sector = c(
      "CPA_C20_BIODIESEL", "CPA_C20_BIOGASOLINE", "CPA_D_BIOGAS",
      "CPA_C20_OTHER_LIQUID_BIOFUELS", "CPA_C20_OTHER_LIQUID_BIOFUELS",
      "CPA_C20_BIODIESEL", "CPA_C20_BIOGASOLINE",
      "CPA_C20_OTHER_LIQUID_BIOFUELS", "CPA_D_BIOGAS"
    ),
    exio_mapping_status = c(
      "sector_match", "sector_match", "sector_match",
      "product_proxy", "product_proxy", "sector_match",
      "sector_match", "product_proxy", "product_proxy"
    ),
    exio_mapping_note = c(
      "CPA sector is biodiesel; route/feedstock composition is not identified.",
      "CPA sector is biogasoline; route/feedstock composition is not identified.",
      "CPA sector is biogas; this is not an IVC-specific foreign lifecycle.",
      "Generic other-liquid-biofuels sector; no exact imported kerosene row.",
      "Generic other-liquid-biofuels sector; no exact imported HFO row.",
      "CPA sector is biodiesel; conventional and advanced routes are pooled.",
      "CPA sector is biogasoline; conventional and advanced routes are pooled.",
      "Generic liquid-biofuels sector used only as an e-methanol proxy.",
      "Biogas sector used only as an e-methane proxy; not an exact RFNBO row."
    ),
    workbook_source = IMPORT_WORKBOOK,
    stringsAsFactors = FALSE
  )
}

read_import_volumes <- function(path) {
  z <- read_excel(path, sheet = "Import Emissions", col_names = FALSE,
                  .name_repair = "minimal")
  assert(nrow(z) >= 43L && ncol(z) >= 11L,
         "Import Emissions sheet is smaller than the expected volume block.")
  categories <- as.character(z[[1]][35:43])
  values <- as.data.frame(z[35:43, 2:11], stringsAsFactors = FALSE)
  names(values) <- c("2023/S0", "2030/S1", "2030/S2", "2030/S3",
                     "2035/S1", "2035/S2", "2035/S3",
                     "2040/S1", "2040/S2", "2040/S3")
  out <- do.call(rbind, lapply(seq_along(categories), function(i) {
    data.frame(import_category = categories[i], values[i, ],
               check.names = FALSE, stringsAsFactors = FALSE)
  }))
  out <- out[out$import_category %in% category_manifest()$import_category, , drop = FALSE]
  long <- do.call(rbind, lapply(names(out)[-1], function(key) {
    p <- strsplit(key, "/", fixed = TRUE)[[1]]
    data.frame(import_category = out$import_category,
               year = if (p[1] == "2023") 2023L else as.integer(p[1]),
               scenario = p[2],
               imported_Mtoe = as.numeric(out[[key]]),
               stringsAsFactors = FALSE)
  }))
  long[long$year %in% c(2030L, 2035L, 2040L), , drop = FALSE]
}

read_workbook_ei <- function(path) {
  z <- read_excel(path, sheet = "Import Emissions", range = "A8:C16",
                  col_names = FALSE, .name_repair = "minimal")
  data.frame(import_category = as.character(z[[1]]),
             workbook_EI_gCO2e_per_MJ = as.numeric(z[[3]]),
             stringsAsFactors = FALSE)
}

read_exio_intensities <- function(path) {
  x <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  req <- c("sector", "boundary", "extension", "stressor", "unit", "intensity")
  assert(all(req %in% names(x)), "Imported extension has an incomplete schema.")
  x <- x[x$boundary == "external_imports_direct" &
           x$extension == "air_emissions", , drop = FALSE]
  x$cf <- mapply(gwp100_cf, x$stressor, x$unit)
  x$ghg <- as.numeric(x$intensity) * x$cf
  aggregate(ghg ~ sector, x, sum)
}

read_model_import_values <- function(path, volume_table, manifest) {
  results <- readRDS(path)
  cfg <- results$metadata$ghg_inputs
  sectors <- cfg$biofuel_sectors
  keys <- unique(volume_table[, c("year", "scenario")])
  rows <- list(); k <- 1L
  for (i in seq_len(nrow(keys))) {
    year <- as.character(keys$year[i]); scenario <- keys$scenario[i]
    endpoint <- results[[year]][[scenario]]
    y <- endpoint$Y_imp_FCE
    y_meur <- as.numeric(y[unname(sectors)])
    names(y_meur) <- names(sectors)
    for (fuel in unique(manifest$model_biofuel)) {
      rows[[k]] <- data.frame(year = as.integer(year), scenario = scenario,
                              model_biofuel = fuel,
                              model_import_value_MEUR = y_meur[[fuel]],
                              stringsAsFactors = FALSE)
      k <- k + 1L
    }
  }
  do.call(rbind, rows)
}

build_comparison <- function(import_path = IMPORT_WORKBOOK,
                              results_path = RESULTS_FILE,
                              extension_path = IMP_EXT_FILE) {
  manifest <- category_manifest()
  volumes <- merge(read_import_volumes(import_path), manifest,
                   by = "import_category", sort = FALSE)
  workbook_ei <- read_workbook_ei(import_path)
  volumes <- merge(volumes, workbook_ei, by = "import_category", sort = FALSE)
  model_values <- read_model_import_values(results_path, volumes, manifest)
  volumes <- merge(volumes, model_values,
                   by = c("year", "scenario", "model_biofuel"), all.x = TRUE,
                   sort = FALSE)

  # The model stores RFNBO finished imports as one aggregate monetary value.
  # Allocate that value by the two Excel physical import volumes solely to
  # obtain category-level direct-import diagnostics; the allocation is marked
  # as a proxy and is not a claim about foreign RFNBO route shares.
  rf <- volumes$model_biofuel == "RFNBOs"
  rf_total <- ave(volumes$model_import_value_MEUR[rf], volumes$year[rf],
                  volumes$scenario[rf], FUN = function(x) x[1])
  rf_vol <- ave(volumes$imported_Mtoe[rf], volumes$year[rf],
                volumes$scenario[rf], FUN = sum)
  volumes$import_value_allocation <- "model endpoint fuel value"
  volumes$import_value_MEUR <- volumes$model_import_value_MEUR
  volumes$import_value_MEUR[rf] <- rf_total * volumes$imported_Mtoe[rf] / rf_vol
  volumes$import_value_allocation[rf] <-
    "RFNBO aggregate value allocated by imported Mtoe share"

  # Join the extension once per sector. The manifest intentionally reuses a
  # sector for several model categories; joining the non-unique manifest here
  # would create Cartesian duplicates in the production table.
  exio <- merge(
    unique(manifest[, c("exio_sector"), drop = FALSE]),
    read_exio_intensities(extension_path),
    by.x = "exio_sector", by.y = "sector", all.x = TRUE, sort = FALSE
  )
  names(exio)[names(exio) == "ghg"] <- "exio_kgCO2e_per_MEUR"
  volumes <- merge(volumes, exio, by = "exio_sector",
                   all.x = TRUE, sort = FALSE)

  volumes$workbook_MtCO2e <- volumes$imported_Mtoe *
    volumes$workbook_EI_gCO2e_per_MJ * 41868 / 1e6
  volumes$workbook_gCO2e_per_MJ <- volumes$workbook_EI_gCO2e_per_MJ
  # The extension intensity is kg CO2e per MEUR of imported use. Convert
  # kilograms to megatonnes with 1e9, not 1e3.
  volumes$exio_MtCO2e <- volumes$import_value_MEUR *
    volumes$exio_kgCO2e_per_MEUR / 1e9
  volumes$energy_MJ <- volumes$imported_Mtoe * MTOE_TO_MJ
  # A zero imported quantity has no meaningful per-MJ EXIOBASE ratio. Keep
  # the absolute result at zero but expose the normalized comparison as NA,
  # rather than leaking an R NaN into the production CSV.
  volumes$exio_gCO2e_per_MJ <- ifelse(
    volumes$energy_MJ > 0,
    volumes$exio_MtCO2e * 1e9 / volumes$energy_MJ * 1e3,
    NA_real_
  )
  volumes$comparison_boundary <-
    "finished imported production; direct import extension, no foreign Leontief"
  volumes$source_note <-
    "Imports_Exports_All_Scenarios.xlsx cached Import Emissions EI; EXIOBASE uses external_imports_direct"
  volumes <- volumes[order(volumes$year, volumes$scenario,
                           volumes$import_category), , drop = FALSE]
  volumes
}

main <- function() {
  for (f in c(IMPORT_WORKBOOK, RESULTS_FILE, IMP_EXT_FILE))
    assert(file.exists(f), paste("Missing required input:", f))
  dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)
  out <- build_comparison()
  write.csv(category_manifest(),
            file.path(OUTPUT_DIR, "ghg_finished_import_mapping.csv"),
            row.names = FALSE, na = "")
  write.csv(out[, c("year", "scenario", "import_category", "model_biofuel",
                    "workbook_source", "imported_Mtoe",
                    "workbook_EI_gCO2e_per_MJ",
                    "workbook_MtCO2e", "workbook_gCO2e_per_MJ")],
            file.path(OUTPUT_DIR, "ghg_finished_import_workbook.csv"),
            row.names = FALSE, na = "")
  write.csv(out[, c("year", "scenario", "import_category", "model_biofuel",
                    "workbook_source", "imported_Mtoe", "import_value_MEUR",
                    "import_value_allocation", "exio_sector",
                    "exio_mapping_status", "exio_mapping_note",
                    "exio_kgCO2e_per_MEUR", "exio_MtCO2e",
                    "exio_gCO2e_per_MJ")],
            file.path(OUTPUT_DIR, "ghg_finished_import_exiobase.csv"),
            row.names = FALSE, na = "")
  write.csv(out,
            file.path(OUTPUT_DIR, "ghg_finished_import_comparison.csv"),
            row.names = FALSE, na = "")
  cat("Finished-import comparison written for", nrow(out),
      "year/scenario/category rows.\n")
  cat("EXIOBASE direct-import values are not foreign recursive lifecycles.\n")
  invisible(out)
}

if (sys.nframe() == 0L) main()
