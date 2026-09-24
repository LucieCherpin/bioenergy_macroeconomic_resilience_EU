options(warn = 2)
suppressPackageStartupMessages(library(readxl))

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
  # Extracted helpers use standard R functions such as utils::read.csv.
  # The global environment retains the normal R search path while isolating
  # the assignment under test from this test file's local bindings.
  env <- new.env(parent = globalenv())
  eval(exprs[[hits]], envir = env)
  get(object_name, envir = env, inherits = FALSE)
}

announce_file <- extract_assignment("GHG_Analysis.R", "announce_file")
read_extension <- extract_assignment("GHG_Analysis.R", "read_extension")
recursive_bio_intensity <- extract_assignment("GHG_Analysis.R", "recursive_bio_intensity")
num <- extract_assignment("GHG_Analysis.R", "num")
norm_label <- extract_assignment("GHG_Analysis.R", "norm_label")
workbook_mix_column_for <- extract_assignment(
  "GHG_Analysis.R", "workbook_mix_column_for"
)
is_pure_recursive_ivc <- extract_assignment(
  "GHG_Analysis.R", "is_pure_recursive_ivc"
)
feedstock_sourcing_shares <- extract_assignment(
  "GHG_Analysis.R", "feedstock_sourcing_shares"
)
feedstock_key_from_label <- extract_assignment(
  "GHG_Analysis.R", "feedstock_key_from_label"
)
s1_hvo_pome_candidate <- extract_assignment(
  "GHG_Analysis.R", "s1_hvo_pome_candidate"
)
get_ivc_prod_cost <- extract_assignment("GHG_Analysis.R", "get_ivc_prod_cost")
get_ivc_alpha <- extract_assignment("GHG_Analysis.R", "get_ivc_alpha")
choose_advanced_mix <- extract_assignment("GHG_Analysis.R", "choose_advanced_mix")

# R compiles this TRE pattern only when num() executes, not while parsing.
stop_if_not(
  isTRUE(all.equal(num("-1.23e-4 kg"), -1.23e-4)),
  "num() failed to parse a negative scientific-notation value with a unit."
)
stop_if_not(
  isTRUE(all.equal(num("+2.5E+3 kg"), 2.5e3)),
  "num() failed to parse a signed scientific-notation value."
)

expected_mix_sources <- c(
  "2030/S1"="F", "2030/S2"="I", "2030/S3"="I",
  "2035/S1"="F", "2035/S2"="I", "2035/S3"="I",
  "2040/S1"="K", "2040/S2"="M", "2040/S3"="M"
)
for (key in names(expected_mix_sources)) {
  parts <- strsplit(key, "/", fixed=TRUE)[[1]]
  stop_if_not(
    identical(workbook_mix_column_for(parts[1],parts[2]),expected_mix_sources[[key]]),
    paste0("Wrong workbook feedstock-mix source mapping for ",key,".")
  )
}
stop_if_not(
  all(is_pure_recursive_ivc(c("IVC6","IVC8b","IVC12"))),
  "Known BIO-intermediate-only IVCs must bypass primary-feedstock reconstruction."
)
stop_if_not(
  !is_pure_recursive_ivc("IVC11a_SAF"),
  "A primary-feedstock IVC was incorrectly classified as pure recursive."
)

# Scenario 1's HVO route is explicitly POME-only in all three scenario-cost
# sheets. It must use that recipe rather than the shared HVO mix-sheet recipe.
if (file.exists("Providing sectors.xlsx")) {
  WORKBOOK_FILE <- "Providing sectors.xlsx"
  hvo_cfg <- list(
    prod_cost=list(IVC2_HVO=1572.75),
    alpha=list(IVC2_HVO=c(feed=0.6358))
  )
  for (year in c("2030","2035","2040")) {
    pome <- s1_hvo_pome_candidate(year,"Providing sectors.xlsx")
    stop_if_not(
      nrow(pome)==1L && identical(pome$feedstock_key[[1L]],
                                  "palm_oil_mill_effluent_raw"),
      paste0("Scenario 1 HVO did not resolve to its POME feedstock in ",year,".")
    )
    stop_if_not(
      isTRUE(all.equal(pome$q_t_feedstock_per_t_fuel[[1L]],1,tolerance=1e-10)) &&
        isTRUE(all.equal(pome$price_eur_per_t[[1L]],1000,tolerance=1e-10)) &&
        isTRUE(all.equal(pome$cost_eur_per_t_fuel[[1L]],1000,tolerance=1e-8)),
      paste0("Scenario 1 HVO POME physical recipe is inconsistent in ",year,".")
    )
    selected <- choose_advanced_mix(hvo_cfg,"IVC2_HVO",year,"S1")
    stop_if_not(
      identical(selected$table$feedstock_key[[1L]],
                "palm_oil_mill_effluent_raw") &&
        isTRUE(all.equal(selected$cost,1000,tolerance=1e-8)) &&
        selected$error<=5,
      paste0("S1 HVO did not select/reconcile its POME override in ",year,".")
    )
  }
}

# Domestic/import feedstock allocation follows the model's positive purchased
# input coefficients. Negative gate-fee entries are revenues and must not enter
# the sourcing denominator.
sourcing <- feedstock_sourcing_shares(c(
  agriculture=-0.5, sewerage=0.6, food_bev_imp=0.4
))
stop_if_not(
  isTRUE(all.equal(unname(sourcing),c(0.6,0.4))),
  "Feedstock sourcing shares did not preserve the model's domestic/import split."
)

# Regression for the production failure: a valid file can contain multiple
# boundary roles, and the reader must select the requested role rather than
# requiring every row to have that role.
tmp <- tempfile(fileext = ".csv")
on.exit(unlink(tmp), add = TRUE)
fixture <- data.frame(
  sector_position = c(1L, 1L),
  sector = c("CPA_A01", "CPA_A01"),
  boundary = c("scope_production", "scope_final_demand_direct"),
  extension = c("air_emissions", "air_emissions"),
  stressor = c("CO2 - combustion - air", "CO2 - combustion - air"),
  unit = c("kg", "kg"),
  intensity = c(1, 2),
  stringsAsFactors = FALSE
)
write.csv(fixture, tmp, row.names = FALSE)
selected <- read_extension(tmp, "scope_production")
stop_if_not(nrow(selected) == 1L, "Mixed-boundary fixture was not filtered to one row.")
stop_if_not(identical(unique(selected$boundary), "scope_production"),
            "Reader retained an unrequested boundary.")
missing_boundary_error <- tryCatch({
  read_extension(tmp, "external_imports_direct")
  FALSE
}, error = function(e) grepl("does not contain requested boundary", conditionMessage(e), fixed = TRUE))
stop_if_not(missing_boundary_error, "Missing requested boundary did not fail clearly.")

# Exercise the real extension files when present, without running the economic
# production analysis.
if (file.exists("IOT_EU27_2022_DOM_environmental_extensions.csv")) {
  dom <- read_extension("IOT_EU27_2022_DOM_environmental_extensions.csv", "scope_production")
  stop_if_not(nrow(dom) > 0L, "Real domestic production extension is empty after filtering.")
  stop_if_not(identical(unique(dom$boundary), "scope_production"),
              "Real domestic extension boundary filter failed.")
}
if (file.exists("IOT_EU27_2022_IMP_environmental_extensions.csv")) {
  imp <- read_extension("IOT_EU27_2022_IMP_environmental_extensions.csv", "external_imports_direct")
  stop_if_not(nrow(imp) > 0L, "Real import extension is empty after filtering.")
  stop_if_not(identical(unique(imp$boundary), "external_imports_direct"),
              "Real import extension boundary filter failed.")
}

energy <- read.csv("ghg_fuel_energy_factors.csv", stringsAsFactors = FALSE, check.names = FALSE)
energy_req <- c("model_biofuel", "ivc_id", "lhv_mj_per_kg", "red_iii_basis",
                "basis_quality", "source_id", "source_locator", "applicability_note")
stop_if_not(all(energy_req %in% names(energy)), "Energy-factor CSV schema is incomplete.")
stop_if_not(nrow(energy) > 0L, "Energy-factor CSV is empty.")
stop_if_not(all(is.finite(energy$lhv_mj_per_kg) & energy$lhv_mj_per_kg > 0),
            "Energy factors must be positive finite LHV values.")
keys <- paste(energy$model_biofuel, energy$ivc_id, sep = "||")
stop_if_not(!anyDuplicated(keys), "Energy-factor model_biofuel/ivc_id keys must be unique.")
stop_if_not(all(energy$basis_quality %in% c("direct", "proxy")),
            "Energy-factor basis_quality must be direct or proxy.")
aviation_lipid_in_diesel <- energy[
  energy$model_biofuel == "conv_biodiesel" &
    energy$ivc_id == "IVC_HT_lipids_SAF",
  , drop = FALSE
]
stop_if_not(
  nrow(aviation_lipid_in_diesel) == 1L &&
    aviation_lipid_in_diesel$lhv_mj_per_kg[[1L]] == 44 &&
    aviation_lipid_in_diesel$basis_quality[[1L]] == "direct",
  "The 2035 S1 aviation-tagged lipid route lacks its direct 44 MJ/kg product mapping."
)

sources <- read.csv("ghg_validation_sources.csv", stringsAsFactors = FALSE, check.names = FALSE)
source_req <- c("source_id", "authors_or_institution", "year", "title", "doi", "url",
                "source_locator", "scope_and_boundary", "independence_note", "citation_full")
stop_if_not(all(source_req %in% names(sources)), "Validation-source CSV schema is incomplete.")
stop_if_not(!anyDuplicated(sources$source_id), "Validation source_id values must be unique.")
stop_if_not(all(nzchar(sources$url) & nzchar(sources$citation_full)),
            "Every validation source needs a URL and full citation.")

bench <- read.csv("ghg_external_benchmarks.csv", stringsAsFactors = FALSE, check.names = FALSE)
bench_req <- c("benchmark_id", "model_biofuel", "model_ivc", "technology", "feedstock_or_case",
               "ghg_min_gCO2e_per_MJ", "ghg_max_gCO2e_per_MJ", "source_id",
               "underlying_source_ids", "source_locator", "system_boundary",
               "comparison_class", "circularity_or_comparability_note")
stop_if_not(all(bench_req %in% names(bench)), "External-benchmark CSV schema is incomplete.")
stop_if_not(!anyDuplicated(bench$benchmark_id), "benchmark_id values must be unique.")
stop_if_not(all(is.finite(bench$ghg_min_gCO2e_per_MJ) & is.finite(bench$ghg_max_gCO2e_per_MJ)),
            "All benchmark bounds must be finite.")
stop_if_not(all(bench$ghg_min_gCO2e_per_MJ <= bench$ghg_max_gCO2e_per_MJ),
            "Benchmark minimum exceeds maximum.")
stop_if_not(all(energy$source_id %in% sources$source_id),
            "Energy-factor source_id missing from source catalogue.")
stop_if_not(all(bench$source_id %in% sources$source_id),
            "Benchmark extraction source_id missing from source catalogue.")
underlying <- unique(unlist(strsplit(bench$underlying_source_ids, ";", fixed = TRUE)))
underlying <- underlying[nzchar(underlying)]
stop_if_not(all(underlying %in% sources$source_id),
            "Benchmark underlying_source_id missing from source catalogue.")

# Synthetic two-BIO recursion. For A = [[0, .2], [.1, 0]], c = [10,20],
# t' = c'(I-A)^-1 gives [12.244897959..., 22.448979592...].
A <- matrix(c(0, 0.2, 0.1, 0), nrow = 2, byrow = TRUE)
got <- recursive_bio_intensity(A, c(10, 20))
expected <- c(12.2448979591837, 22.4489795918367)
stop_if_not(isTRUE(all.equal(got, expected, tolerance = 1e-12)),
            "Recursive BIO intensity formula/orientation is wrong.")

cat("GHG contract tests passed.\n")
