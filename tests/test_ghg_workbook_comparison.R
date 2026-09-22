options(warn = 2)

source("GHG_Workbook_Comparison.R", local = TRUE)

stopifnot(
  excel_column(1L) == "A",
  excel_column(26L) == "Z",
  excel_column(27L) == "AA",
  excel_column(703L) == "AAA"
)

# Saved scenario weights are rounded source values. Route energy must normalize
# them exactly as GHG_Analysis.R does rather than treating the residual as fuel.
mock_hybrid <- data.frame(
  year = 2030, scenario = "S1", biofuel = "adv_biodiesel",
  fuel_energy_MJ = 50
)
mock_results <- list(metadata = list(ghg_inputs = list(
  scenario_configs = list(`2030` = list(S1 = list(
    adv_biodiesel = list(abs_market_value = 1, weights = c(A = 0.4, B = 0.600001))
  ))),
  ivc_tech_library = list(A = list(market_price = 1000), B = list(market_price = 1000))
)))
mock_energy <- data.frame(
  model_biofuel = c("adv_biodiesel", "adv_biodiesel"),
  ivc_id = c("A", "B"), lhv_mj_per_kg = c(50, 50)
)
mock_routes <- route_energy_table(mock_hybrid, mock_results, mock_energy)
stopifnot(isTRUE(all.equal(sum(mock_routes$route_energy_MJ), 50)))

# Energy weighting must preserve unresolved route bounds and signed credits.
routes <- data.frame(
  year = c(2030, 2030),
  scenario = c("S1", "S1"),
  biofuel = c("adv_biogas", "adv_biogas"),
  ivc_id = c("A", "B"),
  route_energy_MJ = c(80, 20)
)
factors <- data.frame(
  biofuel = c("adv_biogas", "adv_biogas"),
  ivc_id = c("A", "B"),
  factor_min_gCO2e_per_MJ = c(-50, 10),
  factor_max_gCO2e_per_MJ = c(-40, 20),
  candidate_count = c(1L, 2L),
  proxy_candidate = c(FALSE, TRUE),
  candidate_ids = c("A1", "B1;B2")
)
weighted <- aggregate_route_ranges(routes, factors, "test")
stopifnot(
  nrow(weighted) == 1L,
  isTRUE(all.equal(weighted$test_coverage_energy_fraction, 1)),
  isTRUE(all.equal(weighted$test_proxy_energy_fraction, 0.2)),
  isTRUE(all.equal(weighted$test_range_energy_fraction, 0.2)),
  isTRUE(all.equal(weighted$test_min_gCO2e_per_MJ, -38)),
  isTRUE(all.equal(weighted$test_max_gCO2e_per_MJ, -28)),
  isTRUE(all.equal(weighted$test_covered_Mt_min, -3800 / 1e12))
)

# Geographic labels apply only to IO channels. Feedstock remains a separate,
# origin-unallocated component and all components reconstruct the stage total.
hybrid <- data.frame(
  year = 2030,
  scenario = "S1",
  biofuel = "adv_biodiesel",
  fuel_energy_MJ = 1e9,
  feedstock_total_kgCO2e = 100,
  opex_kgCO2e = 50,
  capex_kgCO2e = 20,
  hybrid_total_kgCO2e = 170
)
io <- data.frame(
  year = c(2030, 2030),
  scenario = c("S1", "S1"),
  biofuel = c("adv_biodiesel", "adv_biodiesel"),
  channel = c("OPEX", "CAPEX"),
  domestic_kgCO2e = c(30, 12),
  imported_direct_kgCO2e = c(20, 8)
)
geographic <- build_geographic_components(hybrid, io)
stopifnot(
  geographic$feedstock_total_kgCO2e == 100,
  geographic$opex_domestic_kgCO2e == 30,
  geographic$opex_imported_direct_kgCO2e == 20,
  geographic$capex_domestic_kgCO2e == 12,
  geographic$capex_imported_direct_kgCO2e == 8,
  grepl("Origin unallocated", geographic$feedstock_origin_status, fixed = TRUE)
)

# Workbook aggregation must keep the model no-CAPEX/full distinction and must
# not reinterpret a negative workbook credit as missing or zero.
hybrid_lifecycle <- data.frame(
  year = 2030,
  scenario = "S1",
  biofuel = "adv_biogas",
  fuel_energy_MJ = 80,
  recursive_hybrid_no_capex_total_kgCO2e = 500,
  recursive_hybrid_total_kgCO2e = 700,
  recursive_hybrid_no_capex_gCO2e_per_MJ = 5,
  recursive_hybrid_gCO2e_per_MJ = 7
)
workbook_factor <- data.frame(
  model_biofuel = "adv_biogas",
  model_ivc = "A",
  workbook_gCO2e_per_MJ = -50,
  mapping_status = "credit_sensitive",
  cell = "D15"
)
workbook_result <- build_workbook_comparison(
  hybrid_lifecycle, routes[1, ], workbook_factor
)
stopifnot(
  workbook_result$workbook_min_gCO2e_per_MJ == -50,
  workbook_result$workbook_max_gCO2e_per_MJ == -50,
  workbook_result$model_recursive_capex_increment_Mt == 200 / 1e9,
  workbook_result$model_recursive_capex_increment_gCO2e_per_MJ == 2,
  workbook_result$workbook_capital_boundary == "unknown",
  workbook_result$production_status == "produced"
)

cat("GHG workbook comparison synthetic tests passed.\n")
