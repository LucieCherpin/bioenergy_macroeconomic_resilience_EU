options(warn = 2)

stop_if_not <- function(ok, msg) {
  if (!isTRUE(ok)) stop(msg, call. = FALSE)
}

extract_assignment <- function(path, object_name) {
  exprs <- parse(file = path)
  hits <- which(vapply(exprs, function(e) {
    is.call(e) && length(e) >= 3L &&
      identical(e[[1L]], as.name("<-")) &&
      identical(e[[2L]], as.name(object_name))
  }, logical(1)))
  stop_if_not(length(hits) == 1L,
              paste0("Expected exactly one assignment to ", object_name, "."))
  env <- new.env(parent = globalenv())
  eval(exprs[[hits]], envir = env)
  get(object_name, envir = env, inherits = FALSE)
}

diagnostic <- read.csv(
  "ivc8b_allocation_diagnostic.csv",
  stringsAsFactors = FALSE,
  check.names = FALSE
)
required <- c(
  "year", "scenario", "route_weight", "current_feed_coefficient",
  "physical_price_yield_feed_coefficient", "absolute_coefficient_difference",
  "relative_difference_fraction", "current_adv_biogas_eur_per_t_methanol",
  "physical_adv_biogas_eur_per_t_methanol",
  "unexplained_residual_eur_per_t_methanol",
  "aggregated_adv_bio_hfo_adv_biogas_coefficient_current",
  "aggregated_adv_bio_hfo_adv_biogas_coefficient_physical",
  "aggregated_coefficient_difference", "interpretation"
)
stop_if_not(all(required %in% names(diagnostic)),
            "IVC8b diagnostic schema is incomplete.")
stop_if_not(nrow(diagnostic) == 9L,
            "IVC8b diagnostic must cover all nine endpoint scenarios.")

physical_cost <- 900 / 2.0934
physical_coefficient <- physical_cost / 935
scenario_names <- paste0(
  rep(c("S1", "S2", "S3"), times = 3L),
  "_",
  rep(c("2030", "2035", "2040"), each = 3L)
)

for (scenario_name in scenario_names) {
  scenario_cfg <- extract_assignment("Final_main_code.R", scenario_name)
  fuel_cfg <- scenario_cfg$adv_bio_hfo
  route_weight <- fuel_cfg$weights[["IVC8b"]]
  prod_cost <- fuel_cfg$prod_cost[["IVC8b"]]
  alpha_feed <- fuel_cfg$alpha[["IVC8b"]][["feed"]]
  current_cost <- prod_cost * alpha_feed
  current_coefficient <- current_cost / 935
  year <- as.integer(sub(".*_", "", scenario_name))
  scenario <- sub("_.*", "", scenario_name)
  row <- diagnostic[
    diagnostic$year == year & diagnostic$scenario == scenario,
    , drop = FALSE
  ]

  stop_if_not(nrow(row) == 1L,
              paste0("Missing IVC8b diagnostic row for ", scenario_name, "."))
  expected <- c(
    route_weight = route_weight,
    current_feed_coefficient = current_coefficient,
    physical_price_yield_feed_coefficient = physical_coefficient,
    absolute_coefficient_difference = current_coefficient - physical_coefficient,
    relative_difference_fraction = (current_cost - physical_cost) / physical_cost,
    current_adv_biogas_eur_per_t_methanol = current_cost,
    physical_adv_biogas_eur_per_t_methanol = physical_cost,
    unexplained_residual_eur_per_t_methanol = current_cost - physical_cost,
    aggregated_adv_bio_hfo_adv_biogas_coefficient_current =
      route_weight * current_coefficient,
    aggregated_adv_bio_hfo_adv_biogas_coefficient_physical =
      route_weight * physical_coefficient,
    aggregated_coefficient_difference =
      route_weight * (current_coefficient - physical_coefficient)
  )
  got <- unlist(row[names(expected)], use.names = TRUE)
  stop_if_not(
    isTRUE(all.equal(as.numeric(got), as.numeric(expected), tolerance = 1e-12)),
    paste0("Incorrect IVC8b diagnostic values for ", scenario_name, ".")
  )
}

stop_if_not(
  identical(
    sort(unique(diagnostic$interpretation)),
    sort(c(
      "allocation_residual_requires_sensitivity",
      "rounding_equivalent_to_physical_cost"
    ))
  ),
  "IVC8b diagnostic classifications are incomplete."
)

cat("IVC8b allocation diagnostic tests passed.\n")
