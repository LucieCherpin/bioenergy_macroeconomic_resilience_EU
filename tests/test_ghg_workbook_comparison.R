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
unmapped <- aggregate_route_ranges(
  routes[1, ], factors[FALSE, ], "test"
)
stopifnot(
  unmapped$test_coverage_energy_fraction == 0,
  is.na(unmapped$test_covered_Mt_min),
  is.na(unmapped$test_covered_Mt_max)
)

# The physical feedstock allocation and exact IO-fallback channels, together
# with OPEX/CAPEX channels, must reconstruct the stage total.
hybrid <- data.frame(
  year = 2030,
  scenario = "S1",
  biofuel = "adv_biodiesel",
  fuel_energy_MJ = 1e9,
  feedstock_total_kgCO2e = 100,
  feedstock_physical_domestic_kgCO2e = 54,
  feedstock_physical_imported_kgCO2e = 36,
  feedstock_IO_fallback_domestic_kgCO2e = 6,
  feedstock_IO_fallback_imported_direct_kgCO2e = 4,
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
  geographic$feedstock_physical_domestic_kgCO2e == 54,
  geographic$feedstock_physical_imported_kgCO2e == 36,
  geographic$feedstock_IO_fallback_domestic_kgCO2e == 6,
  geographic$feedstock_IO_fallback_imported_direct_kgCO2e == 4,
  geographic$opex_domestic_kgCO2e == 30,
  geographic$opex_imported_direct_kgCO2e == 20,
  geographic$capex_domestic_kgCO2e == 12,
  geographic$capex_imported_direct_kgCO2e == 8,
  grepl("positive model feedstock expenditure", geographic$feedstock_origin_status,
        fixed = TRUE)
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
feedstock_detail <- data.frame(
  year = 2030, scenario = "S1", biofuel = "adv_biogas", ivc_id = "A",
  feedstock_key = "manure", feedstock_mix_share = 1
)
workbook_result <- build_workbook_comparison(
  hybrid_lifecycle, routes[1, ], workbook_factor, feedstock_detail
)
stopifnot(
  workbook_result$workbook_gCO2e_per_MJ == -50,
  workbook_result$workbook_Mt == -4000 / 1e12,
  workbook_result$model_recursive_capex_increment_Mt == 200 / 1e9,
  workbook_result$model_recursive_capex_increment_gCO2e_per_MJ == 2,
  workbook_result$workbook_capital_boundary == "unknown",
  workbook_result$production_status == "produced"
)

# Multiple workbook rows for one IVC are fixed feedstock cases. The scenario
# mix yields one weighted value; their spread is not an uncertainty interval.
fixed_candidates <- data.frame(
  model_biofuel = c("adv_biodiesel", "adv_biodiesel"),
  model_ivc = c("A", "A"),
  feedstock_key = c("x", "y"),
  workbook_gCO2e_per_MJ = c(10, 30),
  mapping_status = c("direct_candidate", "direct_candidate"),
  cell = c("D2", "D3")
)
fixed_mix <- data.frame(
  year = c(2030, 2030), scenario = c("S1", "S1"),
  biofuel = c("adv_biodiesel", "adv_biodiesel"), ivc_id = c("A", "A"),
  feedstock_key = c("x", "y"), feedstock_mix_share = c(0.25, 0.75)
)
fixed_route <- data.frame(
  year = 2030, scenario = "S1", biofuel = "adv_biodiesel", ivc_id = "A",
  route_energy_MJ = 100
)
resolved_fixed <- resolve_workbook_route_values(
  fixed_route, fixed_candidates, fixed_mix
)
stopifnot(
  resolved_fixed$workbook_gCO2e_per_MJ == 25,
  identical(resolved_fixed$workbook_route_mapping_status,
            "feedstock_mix_weighted_fixed_values")
)

# The workbook labels weighted-emission cell D5 as a POME/tall-oil proxy for
# HVO. The S1 production analysis now resolves HVO to POME only, so this route
# must use D5 as an explicit proxy rather than fail or borrow the IVC1 POME value.
if (file.exists("Providing sectors.xlsx")) {
  audited_workbook_factors <- load_workbook_factors("Providing sectors.xlsx")
  pome_factor <- audited_workbook_factors[
    audited_workbook_factors$model_biofuel == "adv_biodiesel" &
      audited_workbook_factors$model_ivc == "IVC2_HVO" &
      audited_workbook_factors$feedstock_key == "palm_oil_mill_effluent_raw",
    , drop = FALSE
  ]
  pome_route <- data.frame(
    year = 2030, scenario = "S1", biofuel = "adv_biodiesel",
    ivc_id = "IVC2_HVO", route_energy_MJ = 100
  )
  pome_mix <- data.frame(
    year = 2030, scenario = "S1", biofuel = "adv_biodiesel",
    ivc_id = "IVC2_HVO", feedstock_key = "palm_oil_mill_effluent_raw",
    feedstock_mix_share = 1
  )
  resolved_pome <- resolve_workbook_route_values(
    pome_route, audited_workbook_factors, pome_mix
  )
  stopifnot(
    nrow(pome_factor) == 1L,
    identical(pome_factor$cell[[1L]], "D5"),
    identical(pome_factor$mapping_status[[1L]], "proxy_candidate"),
    isTRUE(all.equal(
      resolved_pome$workbook_gCO2e_per_MJ[[1L]],
      pome_factor$workbook_gCO2e_per_MJ[[1L]]
    )),
    identical(resolved_pome$workbook_source_cells[[1L]], "D5"),
    identical(resolved_pome$workbook_route_mapping_status[[1L]],
              "feedstock_mix_weighted_proxy_values")
  )
}

cat("GHG workbook comparison synthetic tests passed.\n")
