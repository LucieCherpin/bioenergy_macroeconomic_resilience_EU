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

source_correction <- extract_assignment(
  "Final_main_code.R", "IVC11A_SAF_SOURCE_CORRECTION"
)
correct_ivc11a_saf_source_lineage <- extract_assignment(
  "Final_main_code.R", "correct_ivc11a_saf_source_lineage"
)
environment(correct_ivc11a_saf_source_lineage)$IVC11A_SAF_SOURCE_CORRECTION <-
  source_correction

record <- read.csv(
  "source_data_corrections.csv",
  stringsAsFactors = FALSE,
  check.names = FALSE
)
stop_if_not(nrow(record) == 1L, "Expected one source-data correction record.")
stop_if_not(
  identical(record$cell[[1L]], "D78") &&
    identical(record$original_formula[[1L]], "=B78*C78") &&
    identical(record$corrected_formula[[1L]], "=B78/C78"),
  "The source correction record does not identify the D78 formula change."
)
expected_d78 <- 300 / 0.15708584686774943
stop_if_not(
  isTRUE(all.equal(record$corrected_value[[1L]], expected_d78,
                   tolerance = 1e-12)),
  "The correction record violates the dimensional price/yield identity."
)

# Recompute feed costs and monetary distributions from the physical source rows.
# Row 78 is intentionally recomputed as price/yield; cached monetary cells are not
# used. Rows 58:67 are the corresponding IVC11a_road source block and provide an
# independent cross-check of the source-derived distributions.
mix <- readxl::read_excel(
  "Providing sectors.xlsx",
  sheet = "Feedstock MIX per IVC",
  col_names = FALSE,
  .name_repair = "minimal"
)
source_columns <- c(F = 6L, I = 9L, K = 11L, M = 13L)
sector_by_row <- c(
  "agriculture", "agriculture", "sewerage", "food_bev", "agriculture",
  "agriculture", "forestry", "paper", "sewerage", "fpbo"
)
source_distribution <- function(rows, column) {
  price <- as.numeric(unlist(mix[rows, 2L], use.names = FALSE))
  conversion <- as.numeric(unlist(mix[rows, 3L], use.names = FALSE))
  share <- as.numeric(unlist(mix[rows, column], use.names = FALSE))
  cost <- share * price / conversion
  by_sector <- tapply(cost, sector_by_row, sum)
  out <- c(
    agriculture = by_sector[["agriculture"]],
    forestry = by_sector[["forestry"]],
    paper = by_sector[["paper"]],
    food_bev = by_sector[["food_bev"]],
    sewerage = by_sector[["sewerage"]],
    adv_biodiesel = by_sector[["fpbo"]] / 2,
    adv_biogasoline = by_sector[["fpbo"]] / 2
  )
  list(feed_cost = sum(cost), dist_feed = out / sum(out))
}

market_price <- 2380
for (source_case in names(source_columns)) {
  column <- source_columns[[source_case]]
  saf_source <- source_distribution(69:78, column)
  road_source <- source_distribution(58:67, column)
  stored <- source_correction$cases[[source_case]]
  prod_cost <- stored$feed_eur_per_t +
    source_correction$capex_eur_per_t +
    source_correction$opex_eur_per_t
  alpha <- c(
    feed = stored$feed_eur_per_t,
    capex = source_correction$capex_eur_per_t,
    opex = source_correction$opex_eur_per_t
  ) / prod_cost

  stop_if_not(
    isTRUE(all.equal(stored$feed_eur_per_t, saf_source$feed_cost,
                     tolerance = 1e-12)),
    paste0("Wrong source-derived feed cost for case ", source_case, ".")
  )
  stop_if_not(
    isTRUE(all.equal(stored$dist_feed, saf_source$dist_feed,
                     tolerance = 1e-12)),
    paste0("Wrong source-derived feed distribution for case ", source_case, ".")
  )
  stop_if_not(
    isTRUE(all.equal(saf_source$dist_feed, road_source$dist_feed,
                     tolerance = 1e-12)),
    paste0("Corrected SAF and road source distributions differ for case ",
           source_case, ".")
  )
  stop_if_not(
    isTRUE(all.equal(prod_cost,
                     1445 + 716.25 + stored$feed_eur_per_t,
                     tolerance = 1e-12)) &&
      isTRUE(all.equal(sum(alpha), 1, tolerance = 1e-12)) &&
      isTRUE(all.equal(sum(stored$dist_feed), 1, tolerance = 1e-12)),
    paste0("Cost identity or normalization failed for case ", source_case, ".")
  )
  stop_if_not(
    isTRUE(all.equal(prod_cost * alpha[["capex"]] / market_price,
                     1445 / market_price, tolerance = 1e-12)) &&
      isTRUE(all.equal(prod_cost * alpha[["opex"]] / market_price,
                       716.25 / market_price, tolerance = 1e-12)) &&
      isTRUE(all.equal(prod_cost * alpha[["feed"]] / market_price,
                       stored$feed_eur_per_t / market_price,
                       tolerance = 1e-12)),
    paste0("Technical-coefficient identity failed for case ", source_case, ".")
  )
}

# Exercise every active scenario and verify its explicit source-case application.
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

  lhs <- paste0(scenario_name, "$adv_bio_kerosene")
  correction_hits <- which(vapply(exprs, function(e) {
    is.call(e) && length(e) >= 3L &&
      identical(e[[1L]], as.name("<-")) &&
      identical(paste(deparse(e[[2L]]), collapse = ""), lhs) &&
      is.call(e[[3L]]) &&
      identical(e[[3L]][[1L]], as.name("correct_ivc11a_saf_source_lineage"))
  }, logical(1)))
  stop_if_not(
    length(correction_hits) == 1L,
    paste0("Expected one IVC11a_SAF correction call for ", scenario_name, ".")
  )

  env <- new.env(parent = globalenv())
  env$correct_ivc11a_saf_source_lineage <- correct_ivc11a_saf_source_lineage
  env$IVC11A_SAF_SOURCE_CORRECTION <- source_correction
  assign(scenario_name, original, envir = env)
  eval(exprs[[correction_hits]], envir = env)
  corrected_fuel <- get(scenario_name, envir = env)$adv_bio_kerosene
  new_prod_cost <- corrected_fuel$prod_cost$IVC11a_SAF
  new_alpha <- corrected_fuel$alpha$IVC11a_SAF
  new_components <- new_prod_cost * new_alpha[["feed"]] *
    corrected_fuel$dist_feed$IVC11a_SAF
  expected <- source_correction$cases[[scenario_sources[[scenario_name]]]]
  expected_prod_cost <- expected$feed_eur_per_t + 1445 + 716.25

  stop_if_not(
    isTRUE(all.equal(new_prod_cost, expected_prod_cost, tolerance = 1e-12)) &&
      isTRUE(all.equal(new_prod_cost * new_alpha[["feed"]],
                       expected$feed_eur_per_t, tolerance = 1e-12)),
    paste0("Wrong corrected feed total for ", scenario_name, ".")
  )
  stop_if_not(
    isTRUE(all.equal(new_prod_cost * new_alpha[["capex"]],
                     1445, tolerance = 1e-12)) &&
      isTRUE(all.equal(new_prod_cost * new_alpha[["opex"]],
                       716.25, tolerance = 1e-12)),
    paste0("CAPEX or OPEX lineage is wrong for ", scenario_name, ".")
  )
  stop_if_not(
    isTRUE(all.equal(new_components / sum(new_components),
                     expected$dist_feed, tolerance = 1e-12)),
    paste0("Wrong corrected feed distribution for ", scenario_name, ".")
  )
  stop_if_not(
    isTRUE(all.equal(sum(new_alpha), 1)) &&
      isTRUE(all.equal(sum(corrected_fuel$dist_feed$IVC11a_SAF), 1)),
    paste0("Corrected cost shares do not close for ", scenario_name, ".")
  )
}

cat("IVC11a_SAF source-cost correction tests passed.\n")
