options(warn = 2)

stop_if_not <- function(ok, message) {
  if (!isTRUE(ok)) stop(message, call. = FALSE)
}

extract_assignment <- function(path, object_name) {
  expressions <- parse(file = path)
  hits <- which(vapply(expressions, function(expression) {
    is.call(expression) && length(expression) >= 3L &&
      identical(expression[[1L]], as.name("<-")) &&
      identical(expression[[2L]], as.name(object_name))
  }, logical(1)))
  stop_if_not(
    length(hits) == 1L,
    paste0("Expected exactly one assignment to ", object_name, ".")
  )
  environment <- new.env(parent = globalenv())
  eval(expressions[[hits]], envir = environment)
  get(object_name, envir = environment, inherits = FALSE)
}

mix <- readxl::read_excel(
  "Providing sectors.xlsx",
  sheet = "Feedstock MIX per IVC",
  col_names = FALSE,
  .name_repair = "minimal"
)

# IVC11a_SAF follows the same monetary construction as the other advanced
# routes: source mix share times EUR/t feedstock divided by t fuel/t feedstock.
# The model stores rounded parameters, so comparisons allow only rounding-scale
# discrepancies while preserving the absolute feed, CAPEX and OPEX identities.
source_columns <- c(F = 6L, I = 9L, K = 11L, M = 13L)
scenario_sources <- c(
  S1_2030 = "F",
  S1_2035 = "F",
  S2_2035 = "I",
  S3_2035 = "I",
  S1_2040 = "K",
  S2_2040 = "M",
  S3_2040 = "M"
)
saf_rows <- 69:78
saf_sectors <- c(
  "agriculture", "agriculture", "sewerage", "food_bev", "agriculture",
  "agriculture", "forestry", "paper", "sewerage", "fpbo"
)

workbook_saf_case <- function(source_column) {
  price <- as.numeric(unlist(mix[saf_rows, 2L], use.names = FALSE))
  yield <- as.numeric(unlist(mix[saf_rows, 3L], use.names = FALSE))
  share <- as.numeric(unlist(mix[saf_rows, source_column], use.names = FALSE))
  component_cost <- share * price / yield
  sector_cost <- tapply(component_cost, saf_sectors, sum)
  distribution <- c(
    agriculture = sector_cost[["agriculture"]],
    forestry = sector_cost[["forestry"]],
    paper = sector_cost[["paper"]],
    food_bev = sector_cost[["food_bev"]],
    sewerage = sector_cost[["sewerage"]],
    adv_biodiesel = sector_cost[["fpbo"]] / 2,
    adv_biogasoline = sector_cost[["fpbo"]] / 2
  )
  list(
    feed_cost = sum(component_cost),
    distribution = distribution / sum(distribution)
  )
}

stop_if_not(
  isTRUE(all.equal(
    as.numeric(mix[[4L]][78L]),
    as.numeric(mix[[2L]][78L]) / as.numeric(mix[[3L]][78L]),
    tolerance = 1e-12
  )),
  "Workbook D78 does not implement the IVC11a_SAF price/yield identity."
)

for (scenario_name in names(scenario_sources)) {
  scenario <- extract_assignment("Final_main_code.R", scenario_name)
  configuration <- scenario$adv_bio_kerosene
  production_cost <- configuration$prod_cost[["IVC11a_SAF"]]
  alpha <- configuration$alpha[["IVC11a_SAF"]]
  distribution <- configuration$dist_feed[["IVC11a_SAF"]]
  source_case <- scenario_sources[[scenario_name]]
  workbook <- workbook_saf_case(source_columns[[source_case]])

  stop_if_not(
    abs(production_cost * alpha[["feed"]] - workbook$feed_cost) < 1e-5,
    paste0("IVC11a_SAF feed cost differs from workbook for ", scenario_name, ".")
  )
  stop_if_not(
    abs(production_cost * alpha[["capex"]] - 1445) < 1e-5 &&
      abs(production_cost * alpha[["opex"]] - 716.25) < 1e-5,
    paste0("IVC11a_SAF CAPEX/OPEX identity failed for ", scenario_name, ".")
  )
  stop_if_not(
    abs(sum(alpha) - 1) < 2e-9 && abs(sum(distribution) - 1) < 2e-5,
    paste0("IVC11a_SAF shares do not close for ", scenario_name, ".")
  )
  common <- intersect(names(workbook$distribution), names(distribution))
  stop_if_not(
    setequal(names(workbook$distribution), names(distribution[distribution > 0])) &&
      max(abs(distribution[common] - workbook$distribution[common])) < 1e-4,
    paste0("IVC11a_SAF feed distribution differs from workbook for ",
           scenario_name, ".")
  )
}

# IVC8b uses one biomethane input. Every benchmark endpoint must preserve the
# workbook's 900 EUR/t feedstock divided by 2.0934 t methanol/t biomethane.
ivc8b_feed_cost <- as.numeric(mix[[2L]][41L]) / as.numeric(mix[[3L]][41L])
for (year in c(2030, 2035, 2040)) {
  for (scenario_id in c("S1", "S2", "S3")) {
    scenario_name <- paste0(scenario_id, "_", year)
    scenario <- extract_assignment("Final_main_code.R", scenario_name)
    configuration <- scenario$adv_bio_hfo
    production_cost <- configuration$prod_cost[["IVC8b"]]
    alpha <- configuration$alpha[["IVC8b"]]
    distribution <- configuration$dist_feed[["IVC8b"]]

    stop_if_not(
      abs(production_cost * alpha[["feed"]] - ivc8b_feed_cost) < 1e-3,
      paste0("IVC8b feed cost differs from workbook for ", scenario_name, ".")
    )
    stop_if_not(
      abs(sum(alpha) - 1) < 2e-6 &&
        identical(names(distribution), "adv_biogas") &&
        distribution[["adv_biogas"]] == 1,
      paste0("IVC8b cost shares or feed distribution are invalid for ",
             scenario_name, ".")
    )
  }
}

cat("IVC parameter consistency tests passed.\n")
