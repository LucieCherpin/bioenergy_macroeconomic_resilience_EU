# Read-only plotting and audit layer for the saved GHG benchmark results.
#
# The script does not rerun the economic model or GHG production analysis. It
# verifies the cached workbook values against their IVC/product row labels,
# aggregates route-level comparators with physical fuel-energy weights, and
# writes the numerical figure data before rendering figures.

options(scipen = 999)

assert <- function(ok, message) {
  if (!isTRUE(ok)) stop(message, call. = FALSE)
}

require_columns <- function(x, required, object_name) {
  missing <- setdiff(required, names(x))
  assert(!length(missing), paste0(
    object_name, " is missing required columns: ", paste(missing, collapse = ", ")
  ))
}

fuel_order <- c(
  "adv_biodiesel", "adv_biogasoline", "adv_bio_kerosene",
  "adv_bio_hfo", "adv_biogas", "RFNBOs",
  "conv_biodiesel", "conv_biogasoline", "conv_bio_kerosene"
)

fuel_labels <- c(
  adv_biodiesel = "Advanced biodiesel",
  adv_biogasoline = "Advanced biogasoline",
  adv_bio_kerosene = "Advanced bio-kerosene",
  adv_bio_hfo = "Advanced bio-HFO",
  adv_biogas = "Advanced biogas",
  RFNBOs = "RFNBO",
  conv_biodiesel = "Conventional biodiesel",
  conv_biogasoline = "Conventional biogasoline",
  conv_bio_kerosene = "Conventional bio-kerosene"
)

normalise_text <- function(x) {
  x <- ifelse(is.na(x), "", as.character(x))
  trimws(gsub("[[:space:]]+", " ", x))
}

excel_column <- function(index) {
  assert(length(index) == 1L && is.finite(index) && index >= 1,
         "Excel column index must be a positive scalar.")
  index <- as.integer(index)
  result <- ""
  while (index > 0L) {
    remainder <- (index - 1L) %% 26L
    result <- paste0(LETTERS[remainder + 1L], result)
    index <- (index - 1L) %/% 26L
  }
  result
}

find_sheet <- function(workbook, accepted_names) {
  sheets <- readxl::excel_sheets(workbook)
  hit <- sheets[tolower(normalise_text(sheets)) %in%
                  tolower(normalise_text(accepted_names))]
  assert(length(hit) == 1L, paste0(
    "Expected exactly one workbook sheet named ",
    paste(shQuote(accepted_names), collapse = " or "),
    "; available sheets: ", paste(sheets, collapse = " | ")
  ))
  hit
}

scan_sheet <- function(workbook, sheet) {
  sheet_data <- readxl::read_excel(
    workbook, sheet = sheet, col_names = FALSE, col_types = "text",
    .name_repair = "minimal", guess_max = 10000
  )
  values <- as.matrix(sheet_data)
  rows <- vector("list", length(values))
  count <- 0L
  for (row in seq_len(nrow(values))) {
    for (column in seq_len(ncol(values))) {
      value <- values[row, column]
      if (is.na(value) || !nzchar(normalise_text(value))) next
      count <- count + 1L
      rows[[count]] <- data.frame(
        sheet = sheet,
        cell = paste0(excel_column(column), row),
        row = row,
        column = column,
        value = as.character(value),
        stringsAsFactors = FALSE
      )
    }
  }
  if (!count) {
    return(data.frame(
      sheet = character(), cell = character(), row = integer(),
      column = integer(), value = character()
    ))
  }
  do.call(rbind, rows[seq_len(count)])
}

# This crosswalk records exact source cells and explicit model-route mappings.
# Several source rows are legitimate alternative feedstock cases for one IVC;
# they are retained as an interval rather than averaged without route evidence.
workbook_manifest_definition <- function() {
  rows <- list(
    c("adv_biodiesel", "IVC1", 2, "1", "FAME", "UCO / animal fat", "direct_candidate"),
    c("adv_biodiesel", "IVC1", 3, "1", "FAME", "POME", "direct_candidate"),
    c("adv_biodiesel", "IVC2_HVO", 4, "2", "HVO", "UCO / animal fat", "direct_candidate"),
    c("adv_biodiesel", "IVC2_HVO", 5, "2", "HVO", "POME / tall-oil proxy", "proxy_candidate"),
    c("adv_biodiesel", "IVC2_HVO", 6, "2", "HVO", "FPBO + biocrude", "proxy_candidate"),
    c("adv_bio_kerosene", "IVC2_HEFA", 7, "2", "HEFA / SAF", "Oil crops", "direct_candidate"),
    c("adv_bio_kerosene", "IVC2_HEFA", 8, "2", "HEFA / SAF", "UCO / animal fat", "direct_candidate"),
    c("adv_bio_kerosene", "IVC2_HEFA", 9, "2", "HEFA / SAF", "POME / tall-oil proxy", "proxy_candidate"),
    c("adv_bio_kerosene", "IVC2_HEFA", 10, "2", "HEFA / SAF", "FPBO + biocrude", "proxy_candidate"),
    c("adv_biogasoline", "IVC5", 12, "5", "Advanced ethanol", "", "direct_candidate"),
    c("adv_bio_kerosene", "IVC6", 14, "6", "ATJ-SPK", "", "direct_candidate"),
    c("adv_biogas", "IVC7", 15, "7", "Biomethane", "", "credit_sensitive"),
    c("adv_bio_hfo", "IVC8a", 16, "8a", "Methanol", "", "direct_candidate"),
    c("adv_bio_hfo", "IVC8b", 17, "8b", "Methanol", "", "direct_candidate"),
    c("RFNBOs", "IVC8c", 18, "8c", "e-Methanol (RFNBO)", "", "direct_candidate"),
    c("adv_biogas", "IVC9a", 20, "9a", "Methane", "", "direct_candidate"),
    c("RFNBOs", "IVC9b", 21, "9b", "e-Methane (RFNBO)", "", "direct_candidate"),
    c("adv_biodiesel", "IVC11a_road", 22, "11a", "Biodiesel", "", "direct_candidate"),
    c("adv_bio_kerosene", "IVC11a_SAF", 23, "11a", "FT-SPK", "", "workbook_category_anomaly"),
    c("adv_biogasoline", "IVC12", 24, "12", "Pathway output", "", "direct_candidate"),
    c("adv_biodiesel", "IVC13a", 25, "13a", "Gasoline+Diesel", "", "composite_product"),
    c("adv_biogasoline", "IVC13a", 25, "13a", "Gasoline+Diesel", "", "composite_product"),
    c("adv_biogasoline", "IVC13b_road", 26, "13b", "Biogasoline", "", "direct_candidate"),
    c("adv_bio_hfo", "IVC13b_mar", 27, "13b", "Bio-heavy fuel oil", "", "direct_candidate"),
    c("adv_bio_kerosene", "IVC13b_SAF", 28, "13b", "aviation", "", "direct_candidate"),
    c("conv_biodiesel", "IVC_T_FF", 29, "transesterification of food/feed crops", "diesel", "", "direct_candidate"),
    c("conv_biodiesel", "IVC_HT_FF", 30, "hydrotreatment of food/feed crops", "diesel", "", "direct_candidate"),
    c("conv_biogasoline", "IVC_EF_FF", 31, "ethanol fermentation of food/feed crops", "Gasoline", "", "direct_candidate"),
    c("conv_biodiesel", "IVC_T_CC", 32, "transesterification of cover crops from marginal lands", "diesel", "", "direct_candidate"),
    c("conv_biodiesel", "IVC_HT_CC", 33, "hydrotreatment of cover crops from marginal lands (non-aviation part)", "diesel", "", "direct_candidate"),
    c("conv_biodiesel", "IVC_T_lipids", 34, "transesterification of UCO and AF", "diesel", "", "direct_candidate"),
    c("conv_biodiesel", "IVC_HT_lipids", 35, "hydrotreatment of UCO and AF", "diesel", "", "direct_candidate"),
    c("conv_biodiesel", "IVC_HT_lipids_SAF", 35, "hydrotreatment of UCO and AF", "diesel", "", "product_proxy"),
    c("conv_bio_kerosene", "IVC_HT_lipids_SAF", 35, "hydrotreatment of UCO and AF", "diesel", "", "product_proxy"),
    c("conv_bio_kerosene", "IVC_HT_CC_SAF", 33, "hydrotreatment of cover crops from marginal lands (non-aviation part)", "diesel", "", "product_proxy")
  )
  manifest <- as.data.frame(do.call(rbind, rows), stringsAsFactors = FALSE)
  names(manifest) <- c(
    "model_biofuel", "model_ivc", "row", "expected_ivc",
    "expected_product", "expected_pathway", "mapping_status"
  )
  manifest$row <- as.integer(manifest$row)
  manifest$cell <- paste0("D", manifest$row)
  manifest$unit <- "gCO2e/MJ"
  manifest$capital_boundary <- "unknown"
  manifest$source_note <- paste0(
    "Providing sectors.xlsx weighted-emission row ", manifest$row,
    "; model mapping status: ", manifest$mapping_status
  )
  manifest
}

load_workbook_factors <- function(workbook) {
  weighted_sheet <- find_sheet(
    workbook, c("Weighetd emission intensities", "Weighted emission intensities")
  )
  raw <- readxl::read_excel(
    workbook, sheet = weighted_sheet, col_names = FALSE, col_types = "text",
    .name_repair = "minimal", guess_max = 1000
  )
  assert(ncol(raw) >= 5L, "Weighted-emission sheet has fewer than five columns.")
  manifest <- workbook_manifest_definition()
  for (i in seq_len(nrow(manifest))) {
    row <- manifest$row[i]
    assert(row <= nrow(raw), paste("Workbook mapping row is absent:", row))
    actual_ivc <- normalise_text(raw[[1]][row])
    actual_product <- normalise_text(raw[[2]][row])
    actual_pathway <- normalise_text(raw[[5]][row])
    assert(actual_ivc == normalise_text(manifest$expected_ivc[i]), paste0(
      "Workbook IVC label changed at A", row, ": expected ",
      shQuote(manifest$expected_ivc[i]), ", found ", shQuote(actual_ivc)
    ))
    assert(actual_product == normalise_text(manifest$expected_product[i]), paste0(
      "Workbook product label changed at B", row, ": expected ",
      shQuote(manifest$expected_product[i]), ", found ", shQuote(actual_product)
    ))
    if (nzchar(manifest$expected_pathway[i])) {
      assert(actual_pathway == normalise_text(manifest$expected_pathway[i]), paste0(
        "Workbook pathway label changed at E", row, ": expected ",
        shQuote(manifest$expected_pathway[i]), ", found ", shQuote(actual_pathway)
      ))
    }
  }
  manifest$sheet <- weighted_sheet
  manifest$workbook_gCO2e_per_MJ <- vapply(
    manifest$row,
    function(row) suppressWarnings(as.numeric(raw[[4]][row])),
    numeric(1)
  )
  assert(all(is.finite(manifest$workbook_gCO2e_per_MJ)),
         "A mapped workbook intensity is blank or not a numeric cached value.")
  manifest
}

inspect_workbook <- function(workbook, output_dir) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  weighted_sheet <- find_sheet(
    workbook, c("Weighetd emission intensities", "Weighted emission intensities")
  )
  ivc_sheet <- find_sheet(workbook, "Emission intensities")
  weighted_cells <- scan_sheet(workbook, weighted_sheet)
  ivc_cells <- scan_sheet(workbook, ivc_sheet)
  factors <- load_workbook_factors(workbook)
  write.csv(
    weighted_cells, file.path(output_dir, "ghg_weighted_sheet_cells.csv"),
    row.names = FALSE, na = ""
  )
  write.csv(
    ivc_cells, file.path(output_dir, "ghg_ivc_sheet_cells.csv"),
    row.names = FALSE, na = ""
  )
  write.csv(
    factors, file.path(output_dir, "ghg_workbook_source_manifest.csv"),
    row.names = FALSE, na = ""
  )
  cat("Workbook inspection complete.\n")
  cat("  Weighted cells:", nrow(weighted_cells), "\n")
  cat("  IVC/feedstock cells:", nrow(ivc_cells), "\n")
  cat("  Verified model mappings:", nrow(factors), "\n")
  invisible(factors)
}

add_endpoint_fields <- function(data) {
  data$endpoint <- paste(data$scenario, data$year, sep = "_")
  endpoints <- as.vector(t(outer(c("S1", "S2", "S3"), c(2030, 2035, 2040), paste, sep = "_")))
  data$endpoint <- factor(data$endpoint, levels = endpoints)
  data$endpoint_index <- match(data$endpoint, levels(data$endpoint))
  data$endpoint_label <- factor(
    data$endpoint,
    levels = endpoints,
    labels = sub("_", "\n", endpoints, fixed = TRUE)
  )
  data$fuel_label <- factor(
    data$biofuel, levels = fuel_order, labels = unname(fuel_labels[fuel_order])
  )
  data
}

route_energy_table <- function(hybrid, results, energy_factors) {
  require_columns(
    hybrid,
    c("year", "scenario", "biofuel", "fuel_energy_MJ"),
    "Hybrid benchmark"
  )
  require_columns(
    energy_factors,
    c("model_biofuel", "ivc_id", "lhv_mj_per_kg"),
    "Energy-factor table"
  )
  ghg_inputs <- results$metadata$ghg_inputs
  assert(!is.null(ghg_inputs), "Model RDS lacks metadata$ghg_inputs.")
  configs <- ghg_inputs$scenario_configs
  technologies <- ghg_inputs$ivc_tech_library
  rows <- list()
  count <- 0L
  for (i in seq_len(nrow(hybrid))) {
    h <- hybrid[i, ]
    config <- configs[[as.character(h$year)]][[h$scenario]][[h$biofuel]]
    assert(!is.null(config) && length(config$weights), paste0(
      "Missing saved route weights for ", h$year, "/", h$scenario, "/", h$biofuel
    ))
    weights <- as.numeric(config$weights)
    names(weights) <- names(config$weights)
    assert(all(is.finite(weights)) && all(weights >= -1e-12),
           "Route weights must be finite and nonnegative.")
    weight_sum <- sum(weights)
    assert(is.finite(weight_sum) && weight_sum > 0 && abs(weight_sum - 1) < 1e-3,
           paste0(
      "Route weights do not sum to one for ", h$year, "/", h$scenario,
      "/", h$biofuel
    ))
    # GHG_Analysis.R normalizes the saved, rounded scenario weights before
    # constructing route tonnes and the physical-energy denominator. Reuse that
    # contract exactly rather than absorbing the rounding residual in tolerance.
    weights <- weights / weight_sum
    endpoint_energy <- 0
    for (ivc_id in names(weights)) {
      technology <- technologies[[ivc_id]]
      factor <- energy_factors[
        energy_factors$model_biofuel == h$biofuel &
          energy_factors$ivc_id == ivc_id,
        , drop = FALSE
      ]
      assert(!is.null(technology$market_price) &&
               is.finite(technology$market_price) && technology$market_price > 0,
             paste("Missing positive market price for", h$biofuel, ivc_id))
      assert(nrow(factor) == 1L && is.finite(factor$lhv_mj_per_kg),
             paste("Missing unique LHV for", h$biofuel, ivc_id))
      fuel_tonnes <- config$abs_market_value * weights[[ivc_id]] /
        technology$market_price
      route_energy <- fuel_tonnes * 1000 * factor$lhv_mj_per_kg
      count <- count + 1L
      rows[[count]] <- data.frame(
        year = h$year,
        scenario = h$scenario,
        biofuel = h$biofuel,
        ivc_id = ivc_id,
        route_weight_market_value = weights[[ivc_id]],
        fuel_tonnes = fuel_tonnes,
        lhv_mj_per_kg = factor$lhv_mj_per_kg,
        route_energy_MJ = route_energy,
        stringsAsFactors = FALSE
      )
      endpoint_energy <- endpoint_energy + route_energy
    }
    tolerance <- max(1e-3, 1e-7 * abs(h$fuel_energy_MJ))
    assert(abs(endpoint_energy - h$fuel_energy_MJ) <= tolerance, paste0(
      "Route energy does not reconcile to saved fuel energy for ",
      h$year, "/", h$scenario, "/", h$biofuel,
      ": reconstructed=", signif(endpoint_energy, 12),
      ", saved=", signif(h$fuel_energy_MJ, 12)
    ))
  }
  do.call(rbind, rows)
}

summarise_factor_candidates <- function(factors, value_min, value_max = value_min) {
  require_columns(factors, c("model_biofuel", "model_ivc", value_min, value_max),
                  "Factor candidates")
  keys <- unique(factors[c("model_biofuel", "model_ivc")])
  rows <- vector("list", nrow(keys))
  for (i in seq_len(nrow(keys))) {
    selected <- factors[
      factors$model_biofuel == keys$model_biofuel[i] &
        factors$model_ivc == keys$model_ivc[i],
      , drop = FALSE
    ]
    rows[[i]] <- data.frame(
      biofuel = keys$model_biofuel[i],
      ivc_id = keys$model_ivc[i],
      factor_min_gCO2e_per_MJ = min(selected[[value_min]]),
      factor_max_gCO2e_per_MJ = max(selected[[value_max]]),
      candidate_count = nrow(selected),
      proxy_candidate = if ("mapping_status" %in% names(selected)) {
        any(grepl("proxy|anomaly|composite", selected$mapping_status))
      } else if ("comparison_class" %in% names(selected)) {
        any(grepl("proxy|configuration", selected$comparison_class))
      } else FALSE,
      candidate_ids = if ("benchmark_id" %in% names(selected)) {
        paste(selected$benchmark_id, collapse = ";")
      } else if ("cell" %in% names(selected)) {
        paste(unique(selected$cell), collapse = ";")
      } else "",
      stringsAsFactors = FALSE
    )
  }
  do.call(rbind, rows)
}

aggregate_route_ranges <- function(route_energy, route_factors, prefix) {
  joined <- merge(
    route_energy, route_factors,
    by = c("biofuel", "ivc_id"), all.x = TRUE, sort = FALSE
  )
  endpoint_keys <- unique(joined[c("year", "scenario", "biofuel")])
  rows <- vector("list", nrow(endpoint_keys))
  for (i in seq_len(nrow(endpoint_keys))) {
    selected <- joined[
      joined$year == endpoint_keys$year[i] &
        joined$scenario == endpoint_keys$scenario[i] &
        joined$biofuel == endpoint_keys$biofuel[i],
      , drop = FALSE
    ]
    mapped <- is.finite(selected$factor_min_gCO2e_per_MJ) &
      is.finite(selected$factor_max_gCO2e_per_MJ) & selected$route_energy_MJ > 0
    total_energy <- sum(selected$route_energy_MJ)
    covered_energy <- sum(selected$route_energy_MJ[mapped])
    lower_grams <- sum(
      selected$route_energy_MJ[mapped] * selected$factor_min_gCO2e_per_MJ[mapped]
    )
    upper_grams <- sum(
      selected$route_energy_MJ[mapped] * selected$factor_max_gCO2e_per_MJ[mapped]
    )
    rows[[i]] <- data.frame(
      year = endpoint_keys$year[i],
      scenario = endpoint_keys$scenario[i],
      biofuel = endpoint_keys$biofuel[i],
      total_energy_MJ = total_energy,
      covered_energy_MJ = covered_energy,
      coverage_energy_fraction = if (total_energy > 0) covered_energy / total_energy else NA_real_,
      proxy_energy_fraction = if (total_energy > 0) {
        sum(selected$route_energy_MJ[mapped & selected$proxy_candidate]) / total_energy
      } else NA_real_,
      range_energy_fraction = if (total_energy > 0) {
        sum(selected$route_energy_MJ[mapped & selected$candidate_count > 1L]) / total_energy
      } else NA_real_,
      min_gCO2e_per_MJ = if (covered_energy > 0) lower_grams / covered_energy else NA_real_,
      max_gCO2e_per_MJ = if (covered_energy > 0) upper_grams / covered_energy else NA_real_,
      covered_Mt_min = lower_grams / 1e12,
      covered_Mt_max = upper_grams / 1e12,
      missing_ivcs = paste(selected$ivc_id[!mapped & selected$route_energy_MJ > 0], collapse = ";"),
      stringsAsFactors = FALSE
    )
  }
  result <- do.call(rbind, rows)
  names(result)[-(1:3)] <- paste0(prefix, "_", names(result)[-(1:3)])
  result
}

build_workbook_comparison <- function(hybrid, route_energy, workbook_factors) {
  route_factors <- summarise_factor_candidates(
    workbook_factors, "workbook_gCO2e_per_MJ"
  )
  workbook <- aggregate_route_ranges(route_energy, route_factors, "workbook")
  result <- merge(hybrid, workbook, by = c("year", "scenario", "biofuel"),
                  all.x = TRUE, sort = FALSE)
  assert(nrow(result) == nrow(hybrid),
         "Workbook aggregation changed endpoint/fuel cardinality.")
  result$model_recursive_no_capex_Mt <-
    result$recursive_hybrid_no_capex_total_kgCO2e / 1e9
  result$model_recursive_full_Mt <- result$recursive_hybrid_total_kgCO2e / 1e9
  result$model_recursive_capex_increment_Mt <-
    result$model_recursive_full_Mt - result$model_recursive_no_capex_Mt
  result$model_recursive_capex_increment_gCO2e_per_MJ <-
    result$recursive_hybrid_gCO2e_per_MJ -
    result$recursive_hybrid_no_capex_gCO2e_per_MJ
  result$production_status <- ifelse(
    result$fuel_energy_MJ > 0, "produced", "no_production"
  )
  result$workbook_capital_boundary <- "unknown"
  result$comparison_note <- paste(
    "Workbook route values are energy-weighted; multiple feedstock rows form",
    "unresolved bounds. Workbook capital-goods coverage is unknown."
  )
  result
}

build_geographic_components <- function(hybrid, io_channels) {
  require_columns(
    io_channels,
    c("year", "scenario", "biofuel", "channel", "domestic_kgCO2e",
      "imported_direct_kgCO2e"),
    "IO-channel benchmark"
  )
  keys <- c("year", "scenario", "biofuel")
  result <- hybrid
  for (channel in c("OPEX", "CAPEX")) {
    selected <- io_channels[io_channels$channel == channel, , drop = FALSE]
    assert(!anyDuplicated(selected[keys]), paste("Duplicate", channel, "IO rows."))
    selected <- selected[c(keys, "domestic_kgCO2e", "imported_direct_kgCO2e")]
    names(selected)[4:5] <- paste0(
      tolower(channel), c("_domestic_kgCO2e", "_imported_direct_kgCO2e")
    )
    result <- merge(result, selected, by = keys, all.x = TRUE, sort = FALSE)
  }
  assert(nrow(result) == nrow(hybrid),
         "IO-channel merge changed endpoint/fuel cardinality.")
  numeric_components <- c(
    "feedstock_total_kgCO2e", "opex_domestic_kgCO2e",
    "opex_imported_direct_kgCO2e", "capex_domestic_kgCO2e",
    "capex_imported_direct_kgCO2e"
  )
  assert(all(vapply(result[numeric_components], function(x) all(is.finite(x)), logical(1))),
         "Geographic component table contains non-finite values.")
  reconstructed_opex <- result$opex_domestic_kgCO2e +
    result$opex_imported_direct_kgCO2e
  reconstructed_capex <- result$capex_domestic_kgCO2e +
    result$capex_imported_direct_kgCO2e
  tolerance_opex <- pmax(1e-3, 1e-9 * abs(result$opex_kgCO2e))
  tolerance_capex <- pmax(1e-3, 1e-9 * abs(result$capex_kgCO2e))
  assert(all(abs(reconstructed_opex - result$opex_kgCO2e) <= tolerance_opex),
         "Domestic plus imported-direct OPEX does not reconcile.")
  assert(all(abs(reconstructed_capex - result$capex_kgCO2e) <= tolerance_capex),
         "Domestic plus imported-direct CAPEX does not reconcile.")
  reconstructed_total <- result$feedstock_total_kgCO2e +
    reconstructed_opex + reconstructed_capex
  tolerance_total <- pmax(1e-3, 1e-9 * abs(result$hybrid_total_kgCO2e))
  assert(all(abs(reconstructed_total - result$hybrid_total_kgCO2e) <= tolerance_total),
         "Geographic stage components do not reconstruct the saved hybrid total.")
  result$feedstock_origin_status <- paste(
    "Origin unallocated: physical factors and the saved IO fallback do not",
    "provide a complete domestic/import provenance split."
  )
  result
}

external_factor_tables <- function(benchmarks) {
  require_columns(
    benchmarks,
    c("benchmark_id", "model_biofuel", "model_ivc", "ghg_min_gCO2e_per_MJ",
      "ghg_max_gCO2e_per_MJ", "comparison_class"),
    "External benchmark catalogue"
  )
  ordinary_classes <- c(
    "route_feedstock_candidate", "route_candidate",
    "configuration_range_candidate", "proxy_candidate",
    "road_to_maritime_proxy", "indicative_proxy"
  )
  ordinary <- benchmarks[
    benchmarks$comparison_class %in% ordinary_classes &
      benchmarks$model_biofuel != "ALL_CONTEXT",
    , drop = FALSE
  ]
  ordinary$mapping_status <- ordinary$comparison_class
  ordinary_routes <- summarise_factor_candidates(
    ordinary, "ghg_min_gCO2e_per_MJ", "ghg_max_gCO2e_per_MJ"
  )
  credit <- benchmarks[
    benchmarks$comparison_class == "credit_sensitive_candidate" &
      benchmarks$model_biofuel != "ALL_CONTEXT",
    , drop = FALSE
  ]
  credit$mapping_status <- "credit_sensitive"
  credit_routes <- if (nrow(credit)) {
    summarise_factor_candidates(
      credit, "ghg_min_gCO2e_per_MJ", "ghg_max_gCO2e_per_MJ"
    )
  } else ordinary_routes[FALSE, ]
  list(ordinary = ordinary_routes, credit = credit_routes)
}

build_external_comparison <- function(hybrid, route_energy, benchmarks) {
  factor_tables <- external_factor_tables(benchmarks)
  ordinary <- aggregate_route_ranges(route_energy, factor_tables$ordinary, "external")

  # A credit-sensitive scenario substitutes a credit benchmark only for routes
  # that have one; all other route bounds retain their ordinary comparator.
  credit_routes <- factor_tables$ordinary
  credit_keys <- paste(factor_tables$credit$biofuel, factor_tables$credit$ivc_id)
  if (length(credit_keys)) {
    ordinary_keys <- paste(credit_routes$biofuel, credit_routes$ivc_id)
    for (i in seq_len(nrow(factor_tables$credit))) {
      hit <- match(credit_keys[i], ordinary_keys)
      if (is.na(hit)) {
        credit_routes <- rbind(credit_routes, factor_tables$credit[i, ])
      } else {
        credit_routes[hit, ] <- factor_tables$credit[i, ]
      }
    }
  }
  credit <- aggregate_route_ranges(route_energy, credit_routes, "credit_case")
  credit_endpoint_keys <- unique(merge(
    route_energy, factor_tables$credit[c("biofuel", "ivc_id")],
    by = c("biofuel", "ivc_id"), all = FALSE
  )[c("year", "scenario", "biofuel")])
  credit$credit_case_available <- paste(credit$year, credit$scenario, credit$biofuel) %in%
    paste(credit_endpoint_keys$year, credit_endpoint_keys$scenario,
          credit_endpoint_keys$biofuel)
  credit_columns <- grep("^credit_case_", names(credit), value = TRUE)
  for (column in setdiff(credit_columns, "credit_case_available")) {
    credit[[column]][!credit$credit_case_available] <- NA
  }

  result <- merge(hybrid, ordinary, by = c("year", "scenario", "biofuel"),
                  all.x = TRUE, sort = FALSE)
  result <- merge(result, credit, by = c("year", "scenario", "biofuel"),
                  all.x = TRUE, sort = FALSE)
  assert(nrow(result) == nrow(hybrid),
         "External aggregation changed endpoint/fuel cardinality.")
  result$model_recursive_no_capex_Mt <-
    result$recursive_hybrid_no_capex_total_kgCO2e / 1e9
  result$feedstock_only_gCO2e_per_MJ <-
    result$feedstock_total_kgCO2e * 1000 / result$fuel_energy_MJ
  result$feedstock_only_Mt <- result$feedstock_total_kgCO2e / 1e9
  result$production_status <- ifelse(
    result$fuel_energy_MJ > 0, "produced", "no_production"
  )
  result$comparison_note <- paste(
    "External route intervals are energy-weighted over covered routes.",
    "Credit-sensitive avoided-methane cases are separate alternatives, not",
    "part of the ordinary interval."
  )
  result
}

plot_theme <- function() {
  ggplot2::theme_minimal(base_size = 10) +
    ggplot2::theme(
      legend.position = "bottom",
      legend.box = "vertical",
      strip.text = ggplot2::element_text(face = "bold", size = 9),
      panel.grid.minor = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(size = 7),
      plot.title = ggplot2::element_text(face = "bold"),
      plot.subtitle = ggplot2::element_text(size = 9),
      plot.caption = ggplot2::element_text(size = 8, hjust = 0)
    )
}

facet_fuels <- function() {
  ggplot2::facet_wrap(
    ggplot2::vars(fuel_label), ncol = 3, scales = "free_y", drop = FALSE
  )
}

endpoint_scale <- function() {
  ggplot2::scale_x_continuous(
    breaks = seq_len(9),
    labels = c(
      "S1\n2030", "2035", "2040",
      "S2\n2030", "2035", "2040",
      "S3\n2030", "2035", "2040"
    ),
    minor_breaks = NULL,
    limits = c(0.55, 9.45)
  )
}

scenario_separators <- function() {
  ggplot2::geom_vline(
    xintercept = c(3.5, 6.5), colour = "#bdbdbd", linewidth = 0.25
  )
}

plot_workbook_measure <- function(comparison, measure = c("absolute", "intensity")) {
  measure <- match.arg(measure)
  data <- add_endpoint_fields(comparison)
  if (measure == "absolute") {
    no_capex <- data$model_recursive_no_capex_Mt
    capex <- data$model_recursive_capex_increment_Mt
    benchmark_min <- data$workbook_covered_Mt_min
    benchmark_max <- data$workbook_covered_Mt_max
    y_label <- "Mt CO2e per fuel"
    title <- "Workbook lifecycle reconstruction and model per-fuel emissions"
  } else {
    no_capex <- data$recursive_hybrid_no_capex_gCO2e_per_MJ
    capex <- data$model_recursive_capex_increment_gCO2e_per_MJ
    benchmark_min <- data$workbook_min_gCO2e_per_MJ
    benchmark_max <- data$workbook_max_gCO2e_per_MJ
    y_label <- "g CO2e / MJ"
    title <- "Workbook lifecycle reconstruction and model emission intensity"
  }
  model <- rbind(
    data.frame(data, component = "Model lifecycle excluding CAPEX", value = no_capex),
    data.frame(data, component = "Model CAPEX increment", value = capex)
  )
  model$component <- factor(
    model$component,
    levels = c("Model lifecycle excluding CAPEX", "Model CAPEX increment")
  )
  model <- model[is.finite(model$value), , drop = FALSE]
  benchmark <- data.frame(
    data,
    benchmark_min = benchmark_min,
    benchmark_max = benchmark_max,
    benchmark_mid = (benchmark_min + benchmark_max) / 2
  )
  ggplot2::ggplot() +
    ggplot2::geom_col(
      data = model,
      ggplot2::aes(x = endpoint_index - 0.12, y = value, fill = component),
      width = 0.46
    ) +
    ggplot2::geom_linerange(
      data = benchmark[is.finite(benchmark$benchmark_min), ],
      ggplot2::aes(
        x = endpoint_index + 0.2,
        ymin = benchmark_min,
        ymax = benchmark_max,
        colour = "Workbook lifecycle range"
      ),
      linewidth = 1.05
    ) +
    ggplot2::geom_point(
      data = benchmark[is.finite(benchmark$benchmark_mid), ],
      ggplot2::aes(
        x = endpoint_index + 0.2,
        y = benchmark_mid,
        colour = "Workbook lifecycle range"
      ),
      size = 1.8
    ) +
    ggplot2::geom_hline(yintercept = 0, colour = "#555555", linewidth = 0.3) +
    scenario_separators() +
    facet_fuels() + endpoint_scale() +
    ggplot2::scale_fill_manual(values = c("#2878B5", "#9ECAE1"), name = NULL) +
    ggplot2::scale_colour_manual(values = c("Workbook lifecycle range" = "#D95F02"), name = NULL) +
    ggplot2::labs(
      x = "Benchmark year within scenario", y = y_label, title = title,
      subtitle = paste(
        "Workbook factors are weighted by modeled route energy; multiple",
        "feedstock rows remain ranges. The model bar stacks no-CAPEX lifecycle",
        "GHG and the recursively embodied CAPEX increment."
      ),
      caption = paste(
        "Workbook capital-goods coverage is unknown. Negative biogas values",
        "retain avoided-emission credits. Per-fuel recursive and workbook gross",
        "emissions are not additive across interdependent fuel sectors."
      )
    ) + plot_theme()
}

geographic_long <- function(components, measure = c("absolute", "intensity")) {
  measure <- match.arg(measure)
  data <- add_endpoint_fields(components)
  columns <- c(
    "feedstock_total_kgCO2e", "opex_domestic_kgCO2e",
    "opex_imported_direct_kgCO2e", "capex_domestic_kgCO2e",
    "capex_imported_direct_kgCO2e"
  )
  labels <- c(
    "Feedstock: origin unallocated", "OPEX: domestic production chain",
    "OPEX: imported direct", "CAPEX: domestic production chain",
    "CAPEX: imported direct"
  )
  rows <- lapply(seq_along(columns), function(i) {
    value <- data[[columns[i]]]
    if (measure == "absolute") value <- value / 1e9
    else value <- value * 1000 / data$fuel_energy_MJ
    data.frame(data, component = labels[i], value = value)
  })
  result <- do.call(rbind, rows)
  result$component <- factor(result$component, levels = labels)
  result <- result[is.finite(result$value), , drop = FALSE]
  result
}

plot_geographic_measure <- function(components, measure = c("absolute", "intensity")) {
  measure <- match.arg(measure)
  data <- geographic_long(components, measure)
  y_label <- if (measure == "absolute") "Mt CO2e (stage-attributed)" else "g CO2e / MJ (stage-attributed)"
  title <- if (measure == "absolute") {
    "Stage-attributed emissions by domestic, imported-direct and unallocated origin"
  } else {
    "Stage-attributed emission intensity by domestic, imported-direct and unallocated origin"
  }
  ggplot2::ggplot(
    data, ggplot2::aes(x = endpoint_index, y = value, fill = component)
  ) +
    ggplot2::geom_col(width = 0.68) +
    ggplot2::geom_hline(yintercept = 0, colour = "#555555", linewidth = 0.3) +
    scenario_separators() + facet_fuels() + endpoint_scale() +
    ggplot2::scale_fill_manual(
      values = c("#7B3294", "#008837", "#80CDC1", "#C2A5CF", "#F6E8C3"),
      name = NULL
    ) +
    ggplot2::labs(
      x = "Benchmark year within scenario", y = y_label, title = title,
      subtitle = paste(
        "Domestic and imported-direct labels apply only to saved IO OPEX/CAPEX",
        "channels. Physical and IO-fallback feedstock GHG remains origin-unallocated."
      ),
      caption = paste(
        "Imported IO is direct external-import GHG with no foreign Leontief",
        "closure. This is an additive stage decomposition, not a territorial",
        "inventory and not a geographic split of the recursive lifecycle total."
      )
    ) + plot_theme()
}

plot_external_measure <- function(comparison, measure = c("absolute", "intensity")) {
  measure <- match.arg(measure)
  data <- add_endpoint_fields(comparison)
  if (measure == "absolute") {
    model <- data$model_recursive_no_capex_Mt
    feedstock <- data$feedstock_only_Mt
    external_min <- data$external_covered_Mt_min
    external_max <- data$external_covered_Mt_max
    credit_min <- data$credit_case_covered_Mt_min
    credit_max <- data$credit_case_covered_Mt_max
    y_label <- "Mt CO2e per fuel"
    title <- "Model and official pathway-comparator emissions excluding model CAPEX"
  } else {
    model <- data$recursive_hybrid_no_capex_gCO2e_per_MJ
    feedstock <- data$feedstock_only_gCO2e_per_MJ
    external_min <- data$external_min_gCO2e_per_MJ
    external_max <- data$external_max_gCO2e_per_MJ
    credit_min <- data$credit_case_min_gCO2e_per_MJ
    credit_max <- data$credit_case_max_gCO2e_per_MJ
    y_label <- "g CO2e / MJ"
    title <- "Model and official pathway-comparator emission intensity"
  }
  ordinary <- data.frame(
    data, lower = external_min, upper = external_max,
    midpoint = (external_min + external_max) / 2
  )
  credit <- data.frame(
    data, lower = credit_min, upper = credit_max,
    midpoint = (credit_min + credit_max) / 2
  )
  model_data <- data.frame(data, model = model, feedstock = feedstock)
  model_bars <- model_data[is.finite(model_data$model), , drop = FALSE]
  feedstock_points <- model_data[is.finite(model_data$feedstock), , drop = FALSE]
  ggplot2::ggplot() +
    ggplot2::geom_col(
      data = model_bars,
      ggplot2::aes(
        x = endpoint_index - 0.16, y = model,
        fill = "Model recursive lifecycle excluding CAPEX"
      ),
      width = 0.42
    ) +
    ggplot2::geom_point(
      data = feedstock_points,
      ggplot2::aes(
        x = endpoint_index - 0.16, y = feedstock,
        colour = "Model feedstock-only diagnostic"
      ),
      shape = 4, size = 1.8, stroke = 0.7
    ) +
    ggplot2::geom_linerange(
      data = ordinary[is.finite(ordinary$lower), ],
      ggplot2::aes(
        x = endpoint_index + 0.15, ymin = lower, ymax = upper,
        colour = "JEC/RED/CORSIA ordinary interval"
      ),
      linewidth = 1
    ) +
    ggplot2::geom_point(
      data = ordinary[is.finite(ordinary$midpoint), ],
      ggplot2::aes(
        x = endpoint_index + 0.15, y = midpoint,
        colour = "JEC/RED/CORSIA ordinary interval"
      ),
      size = 1.7
    ) +
    ggplot2::geom_linerange(
      data = credit[credit$credit_case_available & is.finite(credit$lower), ],
      ggplot2::aes(
        x = endpoint_index + 0.31, ymin = lower, ymax = upper,
        colour = "Avoided-emission-credit alternative"
      ),
      linewidth = 0.8
    ) +
    ggplot2::geom_point(
      data = credit[credit$credit_case_available & is.finite(credit$midpoint), ],
      ggplot2::aes(
        x = endpoint_index + 0.31, y = midpoint,
        colour = "Avoided-emission-credit alternative"
      ),
      shape = 18, size = 2
    ) +
    ggplot2::geom_hline(yintercept = 0, colour = "#555555", linewidth = 0.3) +
    scenario_separators() + facet_fuels() + endpoint_scale() +
    ggplot2::scale_fill_manual(
      values = c("Model recursive lifecycle excluding CAPEX" = "#2878B5"),
      name = NULL
    ) +
    ggplot2::scale_colour_manual(
      values = c(
        "Model feedstock-only diagnostic" = "#E69F00",
        "JEC/RED/CORSIA ordinary interval" = "#7A3E9D",
        "Avoided-emission-credit alternative" = "#C44E52"
      ),
      name = NULL
    ) +
    ggplot2::labs(
      x = "Benchmark year within scenario", y = y_label, title = title,
      subtitle = paste(
        "Official route intervals are energy-weighted over covered scenario",
        "routes. The model comparator includes operating inputs but excludes CAPEX."
      ),
      caption = paste(
        "Feedstock-only crosses are not lifecycle estimates. Proxy and route",
        "coverage are reported in the companion CSV. Avoided-methane credits",
        "are separate alternatives; no pass/fail range judgement is applied."
      )
    ) + plot_theme()
}

save_two_panel <- function(top, bottom, output_base, width = 18, height = 20) {
  draw <- function() {
    grid::grid.newpage()
    layout <- grid::grid.layout(2, 1, heights = grid::unit(c(1, 1), "null"))
    grid::pushViewport(grid::viewport(layout = layout))
    print(top, vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 1))
    print(bottom, vp = grid::viewport(layout.pos.row = 2, layout.pos.col = 1))
    grid::popViewport()
  }
  grDevices::png(
    paste0(output_base, ".png"), width = width, height = height,
    units = "in", res = 180
  )
  draw()
  grDevices::dev.off()
  grDevices::pdf(paste0(output_base, ".pdf"), width = width, height = height)
  draw()
  grDevices::dev.off()
}

render_all <- function(workbook = "Providing sectors.xlsx",
                       output_dir = "ghg_outputs") {
  required_files <- c(
    workbook,
    "model_results_CAPEX_separate.rds",
    file.path(output_dir, "ghg_hybrid_benchmark.csv"),
    file.path(output_dir, "ghg_io_channels_benchmark.csv"),
    file.path(output_dir, "ghg_fuel_energy_factors_used.csv"),
    file.path(output_dir, "ghg_external_benchmarks_used.csv")
  )
  missing <- required_files[!file.exists(required_files)]
  assert(!length(missing), paste0(
    "Missing required saved input(s): ", paste(missing, collapse = ", "),
    ". Run the existing GHG production analysis before plotting."
  ))
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  hybrid <- read.csv(
    file.path(output_dir, "ghg_hybrid_benchmark.csv"),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  io_channels <- read.csv(
    file.path(output_dir, "ghg_io_channels_benchmark.csv"),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  energy_factors <- read.csv(
    file.path(output_dir, "ghg_fuel_energy_factors_used.csv"),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  external_benchmarks <- read.csv(
    file.path(output_dir, "ghg_external_benchmarks_used.csv"),
    stringsAsFactors = FALSE, check.names = FALSE
  )
  results <- readRDS("model_results_CAPEX_separate.rds")
  assert(nrow(hybrid) == 81L,
         "Expected 81 GHG benchmark rows (3 years x 3 scenarios x 9 fuels).")
  assert(!anyDuplicated(hybrid[c("year", "scenario", "biofuel")]),
         "Hybrid benchmark contains duplicate endpoint/fuel keys.")
  assert(setequal(unique(hybrid$biofuel), fuel_order),
         "Hybrid benchmark fuel set differs from the nine-sector plotting contract.")

  workbook_factors <- load_workbook_factors(workbook)
  route_energy <- route_energy_table(hybrid, results, energy_factors)
  workbook_comparison <- build_workbook_comparison(
    hybrid, route_energy, workbook_factors
  )
  geographic_components <- build_geographic_components(hybrid, io_channels)
  external_comparison <- build_external_comparison(
    hybrid, route_energy, external_benchmarks
  )

  write.csv(
    workbook_factors,
    file.path(output_dir, "ghg_workbook_source_manifest.csv"),
    row.names = FALSE, na = ""
  )
  write.csv(
    route_energy,
    file.path(output_dir, "ghg_scenario_route_energy.csv"),
    row.names = FALSE, na = ""
  )
  write.csv(
    workbook_comparison,
    file.path(output_dir, "ghg_workbook_lifecycle_comparison.csv"),
    row.names = FALSE, na = ""
  )
  write.csv(
    geographic_components,
    file.path(output_dir, "ghg_geographic_component_comparison.csv"),
    row.names = FALSE, na = ""
  )
  write.csv(
    external_comparison,
    file.path(output_dir, "ghg_external_lifecycle_plot_data.csv"),
    row.names = FALSE, na = ""
  )

  save_two_panel(
    plot_workbook_measure(workbook_comparison, "absolute"),
    plot_workbook_measure(workbook_comparison, "intensity"),
    file.path(output_dir, "ghg_workbook_lifecycle_comparison")
  )
  save_two_panel(
    plot_geographic_measure(geographic_components, "absolute"),
    plot_geographic_measure(geographic_components, "intensity"),
    file.path(output_dir, "ghg_geographic_component_comparison")
  )
  save_two_panel(
    plot_external_measure(external_comparison, "absolute"),
    plot_external_measure(external_comparison, "intensity"),
    file.path(output_dir, "ghg_external_lifecycle_comparison")
  )

  cat("GHG workbook/geographic/external plotting complete.\n")
  cat("Outputs written to", normalizePath(output_dir), "\n")
  positive_coverage <- workbook_comparison$workbook_coverage_energy_fraction[
    workbook_comparison$fuel_energy_MJ > 0
  ]
  cat("Workbook route coverage for produced fuels:",
      sprintf("%.1f%% to %.1f%%",
              100 * min(positive_coverage, na.rm = TRUE),
              100 * max(positive_coverage, na.rm = TRUE)), "\n")
  cat("No-production endpoints with undefined intensity:",
      sum(workbook_comparison$production_status == "no_production"), "\n")
  cat("WARNING: recursive/workbook per-fuel gross emissions are not additive across fuels.\n")
  invisible(list(
    workbook = workbook_comparison,
    geographic = geographic_components,
    external = external_comparison
  ))
}

main <- function(args = commandArgs(trailingOnly = TRUE)) {
  assert(
    length(args) <= 1L && (length(args) == 0L || args[[1]] %in% c("--inspect", "--help")),
    "Usage: Rscript GHG_Workbook_Comparison.R [--inspect|--help]"
  )
  if (length(args) == 1L && args[[1]] == "--help") {
    cat(
      "Usage: Rscript GHG_Workbook_Comparison.R [--inspect|--help]\n",
      "  no argument  verify inputs, export comparison tables and render figures\n",
      "  --inspect    export workbook cell censuses and verified source manifest\n",
      sep = ""
    )
    return(invisible(TRUE))
  }
  for (package in c("readxl", "ggplot2")) {
    assert(requireNamespace(package, quietly = TRUE),
           paste("Required R package is unavailable:", package))
  }
  if (length(args) == 1L) {
    inspect_workbook("Providing sectors.xlsx", "ghg_outputs")
  } else {
    render_all()
  }
}

if (sys.nframe() == 0L) main()
