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
  stop_if_not(
    length(hits) == 1L,
    paste0("Expected exactly one assignment to ", object_name, ".")
  )
  env <- new.env(parent = globalenv())
  eval(exprs[[hits]], envir = env)
  get(object_name, envir = env, inherits = FALSE)
}

correct_ivc11a_saf_fpbo_cost <- extract_assignment(
  "Final_main_code.R",
  "correct_ivc11a_saf_fpbo_cost"
)
corrected_source_costs <- extract_assignment(
  "Final_main_code.R",
  "IVC11A_SAF_CORRECTED_FEED_COST_EUR_PER_T"
)

# Recompute the source totals from price, physical yield and scenario mix share.
# This test intentionally does not use the cached monetary workbook cells whose
# row-78 formula caused the defect.
mix <- readxl::read_excel(
  "Providing sectors.xlsx",
  sheet = "Feedstock MIX per IVC",
  col_names = FALSE,
  .name_repair = "minimal"
)
ivc11a_rows <- 69:78
price <- as.numeric(unlist(mix[ivc11a_rows, 2L], use.names = FALSE))
yield <- as.numeric(unlist(mix[ivc11a_rows, 3L], use.names = FALSE))
source_columns <- c(F = 6L, I = 9L, K = 11L, M = 13L)
recomputed_source_costs <- vapply(source_columns, function(column) {
  share <- as.numeric(unlist(mix[ivc11a_rows, column], use.names = FALSE))
  sum(share * price / yield)
}, numeric(1))
stop_if_not(
  isTRUE(all.equal(corrected_source_costs, recomputed_source_costs,
                   tolerance = 1e-12)),
  "Stored IVC11a_SAF corrected totals do not match price/yield source data."
)

# This fixture exposes both source defects: the feed total excludes the
# dimensionally correct FPBO cost, and the complete old FPBO share is duplicated
# across two model BIO sectors as it was in the 2040 configurations.
fixture <- list(
  prod_cost = list(IVC11a_SAF = 1000),
  alpha = list(
    IVC11a_SAF = c(feed = 0.6, capex = 0.25, opex = 0.15)
  ),
  dist_feed = list(
    IVC11a_SAF = c(
      agriculture = 0.8,
      forestry = 0.1,
      adv_biodiesel = 0.05,
      adv_biogasoline = 0.05
    )
  )
)

old_prod_cost <- fixture$prod_cost$IVC11a_SAF
old_alpha <- fixture$alpha$IVC11a_SAF
old_components <- old_prod_cost * old_alpha[["feed"]] *
  fixture$dist_feed$IVC11a_SAF
corrected_feed_cost <- 700

got <- correct_ivc11a_saf_fpbo_cost(fixture, corrected_feed_cost)
got_prod_cost <- got$prod_cost$IVC11a_SAF
got_alpha <- got$alpha$IVC11a_SAF
got_components <- got_prod_cost * got_alpha[["feed"]] *
  got$dist_feed$IVC11a_SAF

stop_if_not(
  isTRUE(all.equal(got_prod_cost * got_alpha[["feed"]], corrected_feed_cost)),
  "Corrected IVC11a_SAF feedstock expenditure does not match its source total."
)
stop_if_not(
  isTRUE(all.equal(got_prod_cost * got_alpha[["capex"]],
                   old_prod_cost * old_alpha[["capex"]])),
  "The FPBO correction changed absolute CAPEX."
)
stop_if_not(
  isTRUE(all.equal(got_prod_cost * got_alpha[["opex"]],
                   old_prod_cost * old_alpha[["opex"]])),
  "The FPBO correction changed absolute OPEX."
)
stop_if_not(
  isTRUE(all.equal(got_components[c("agriculture", "forestry")],
                   old_components[c("agriculture", "forestry")])),
  "The FPBO correction changed a non-FPBO feedstock expenditure."
)
stop_if_not(
  isTRUE(all.equal(
    sum(got_components[c("adv_biodiesel", "adv_biogasoline")]),
    corrected_feed_cost - sum(old_components[c("agriculture", "forestry")])
  )),
  "Corrected FPBO expenditure was not allocated exactly once."
)
stop_if_not(
  isTRUE(all.equal(
    got_components[["adv_biodiesel"]] / got_components[["adv_biogasoline"]],
    old_components[["adv_biodiesel"]] / old_components[["adv_biogasoline"]]
  )),
  "The correction changed the existing model-sector split of FPBO."
)
stop_if_not(
  isTRUE(all.equal(sum(got_alpha), 1)) &&
    isTRUE(all.equal(sum(got$dist_feed$IVC11a_SAF), 1)),
  "Corrected IVC11a_SAF cost shares do not close."
)

# Exercise every scenario in which IVC11a_SAF is active.  This protects the
# complete relevant year/scenario state space and verifies that each top-level
# correction call is present, uses the intended workbook source, and preserves
# the non-FPBO economic components of the actual model configuration.
scenario_sources <- c(
  S1_2030 = "F",
  S1_2035 = "F",
  S2_2035 = "I",
  S3_2035 = "I",
  S1_2040 = "K",
  S2_2040 = "M",
  S3_2040 = "M"
)
exprs <- parse(file = "Final_main_code.R")
for (scenario_name in names(scenario_sources)) {
  original <- extract_assignment("Final_main_code.R", scenario_name)
  original_fuel <- original$adv_bio_kerosene
  old_prod_cost <- original_fuel$prod_cost$IVC11a_SAF
  old_alpha <- original_fuel$alpha$IVC11a_SAF
  old_components <- old_prod_cost * old_alpha[["feed"]] *
    original_fuel$dist_feed$IVC11a_SAF
  non_fpbo <- setdiff(
    names(old_components),
    c("adv_biodiesel", "adv_biogasoline")
  )

  lhs <- paste0(scenario_name, "$adv_bio_kerosene")
  correction_hits <- which(vapply(exprs, function(e) {
    is.call(e) && length(e) >= 3L &&
      identical(e[[1L]], as.name("<-")) &&
      identical(paste(deparse(e[[2L]]), collapse = ""), lhs) &&
      is.call(e[[3L]]) &&
      identical(e[[3L]][[1L]], as.name("correct_ivc11a_saf_fpbo_cost"))
  }, logical(1)))
  stop_if_not(
    length(correction_hits) == 1L,
    paste0("Expected one IVC11a_SAF correction call for ", scenario_name, ".")
  )

  env <- new.env(parent = globalenv())
  env$correct_ivc11a_saf_fpbo_cost <- correct_ivc11a_saf_fpbo_cost
  env$IVC11A_SAF_CORRECTED_FEED_COST_EUR_PER_T <- corrected_source_costs
  assign(scenario_name, original, envir = env)
  eval(exprs[[correction_hits]], envir = env)
  corrected_fuel <- get(scenario_name, envir = env)$adv_bio_kerosene
  new_prod_cost <- corrected_fuel$prod_cost$IVC11a_SAF
  new_alpha <- corrected_fuel$alpha$IVC11a_SAF
  new_components <- new_prod_cost * new_alpha[["feed"]] *
    corrected_fuel$dist_feed$IVC11a_SAF
  expected_feed <- corrected_source_costs[[scenario_sources[[scenario_name]]]]

  stop_if_not(
    isTRUE(all.equal(new_prod_cost * new_alpha[["feed"]], expected_feed)),
    paste0("Wrong corrected feed total for ", scenario_name, ".")
  )
  stop_if_not(
    isTRUE(all.equal(new_prod_cost * new_alpha[["capex"]],
                     old_prod_cost * old_alpha[["capex"]])) &&
      isTRUE(all.equal(new_prod_cost * new_alpha[["opex"]],
                       old_prod_cost * old_alpha[["opex"]])),
    paste0("CAPEX or OPEX changed for ", scenario_name, ".")
  )
  stop_if_not(
    isTRUE(all.equal(new_components[non_fpbo], old_components[non_fpbo])),
    paste0("A non-FPBO feedstock changed for ", scenario_name, ".")
  )
  stop_if_not(
    isTRUE(all.equal(sum(new_alpha), 1)) &&
      isTRUE(all.equal(sum(corrected_fuel$dist_feed$IVC11a_SAF), 1)),
    paste0("Corrected cost shares do not close for ", scenario_name, ".")
  )
}

cat("IVC11a_SAF source-cost correction tests passed.\n")
