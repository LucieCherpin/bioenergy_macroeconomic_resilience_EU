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
  # Extracted helpers use standard R functions such as utils::read.csv.
  # The global environment retains the normal R search path while isolating
  # the assignment under test from this test file's local bindings.
  env <- new.env(parent = globalenv())
  eval(exprs[[hits]], envir = env)
  get(object_name, envir = env, inherits = FALSE)
}

read_extension <- extract_assignment("GHG_Analysis.R", "read_extension")
recursive_bio_intensity <- extract_assignment("GHG_Analysis.R", "recursive_bio_intensity")
num <- extract_assignment("GHG_Analysis.R", "num")

# R compiles this TRE pattern only when num() executes, not while parsing.
stop_if_not(
  isTRUE(all.equal(num("-1.23e-4 kg"), -1.23e-4)),
  "num() failed to parse a negative scientific-notation value with a unit."
)
stop_if_not(
  isTRUE(all.equal(num("+2.5E+3 kg"), 2.5e3)),
  "num() failed to parse a signed scientific-notation value."
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
