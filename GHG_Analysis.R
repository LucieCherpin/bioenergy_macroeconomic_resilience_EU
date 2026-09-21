# ===================================================================
# OPTIONAL HYBRID GHG ANALYSIS
# ===================================================================
# Run:
#   Rscript GHG_Analysis.R
#
# hybrid GHG =
#   physical primary-feedstock GHG
# + IO-based OPEX GHG
# + IO-based CAPEX GHG
# + explicitly flagged IO fallback where a physical coefficient cannot
#   be reconstructed from Providing sectors.xlsx.
#
# Primary feedstocks are reconstructed at IVC level. We do NOT decompose
# the already-aggregated Eurostat "agriculture"/"forestry"/etc. flow back
# into biology after the model solve.
#
# Intermediate bioenergy carriers get no second feedstock factor at the
# consuming IVC; their producer-sector footprint is counted separately.
#
# Imported IO emissions use the repo's external_imports_direct extension:
# this is a lower bound because no foreign Leontief system is present.
# ===================================================================

rm(list = ls())
options(scipen = 999)
suppressPackageStartupMessages(library(readxl))

RESULTS_FILE <- "model_results_CAPEX_separate.rds"
WORKBOOK_FILE <- "Providing sectors.xlsx"
INVENTORY_FILE <- "feedstock_inventory_from_providing_sectors.csv"
AUDIT_FILE <- "feedstock_ghg_coverage_audit.csv"
FACTOR_FILE <- "feedstock_ghg_factors_updated.csv"
ENERGY_FACTOR_FILE <- "ghg_fuel_energy_factors.csv"
VALIDATION_SOURCE_FILE <- "ghg_validation_sources.csv"
BENCHMARK_FILE <- "ghg_external_benchmarks.csv"
DOM_EXT_FILE <- "IOT_EU27_2022_DOM_environmental_extensions.csv"
IMP_EXT_FILE <- "IOT_EU27_2022_IMP_environmental_extensions.csv"
OUTPUT_DIR <- "ghg_outputs"
dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)

needed <- c(RESULTS_FILE, WORKBOOK_FILE, INVENTORY_FILE, AUDIT_FILE,
            FACTOR_FILE, ENERGY_FACTOR_FILE, VALIDATION_SOURCE_FILE,
            BENCHMARK_FILE, DOM_EXT_FILE, IMP_EXT_FILE)
missing <- needed[!file.exists(needed)]
if (length(missing)) stop("Missing required file(s): ", paste(missing, collapse=", "))

results <- readRDS(RESULTS_FILE)
if (is.null(results$metadata$ghg_inputs)) {
  stop("RDS lacks metadata$ghg_inputs. Re-run Final_main_code.R after applying patch.")
}

BIO <- results$metadata$BIO
NONBIO <- results$metadata$NONBIO
sector_names <- results$metadata$sector_names
G <- results$metadata$ghg_inputs
BIOFUEL_SECTORS <- G$biofuel_sectors
INPUT_SECTORS <- G$input_sectors
IVC_TECH_LIBRARY <- G$ivc_tech_library
SCENARIO_EUR_TO_IO_UNIT <- G$scenario_eur_to_io_unit
SCENARIO_CONFIGS <- G$scenario_configs

benchmark_years <- c("2030","2035","2040")
scenario_names <- c("S1","S2","S3")
stopifnot(
  SCENARIO_EUR_TO_IO_UNIT == 1e6,
  setequal(BIO, unname(BIOFUEL_SECTORS)),
  all(benchmark_years %in% names(SCENARIO_CONFIGS))
)

num <- function(x) {
  if (!length(x) || is.na(x)) return(NA_real_)
  if (is.numeric(x)) return(as.numeric(x))
  z <- trimws(as.character(x))
  z <- gsub(",", ".", z, fixed=TRUE)
  z <- gsub("[^0-9eE+.-]", "", z)
  if (!nzchar(z) || z %in% c("-",".","-.")) return(NA_real_)
  suppressWarnings(as.numeric(z))
}
norm_label <- function(x) {
  x <- iconv(as.character(x), to="ASCII//TRANSLIT")
  x[is.na(x)] <- ""
  x <- tolower(x)
  trimws(gsub("\\s+", " ", gsub("[^a-z0-9]+", " ", x)))
}
normalize_weights <- function(w, tol=1e-3) {
  if (is.null(w) || !length(w) || anyNA(w) || any(w < 0)) stop("Invalid IVC weights.")
  sw <- sum(w)
  if (abs(sw-1) > tol) stop("IVC weights sum to ", sw)
  w/sw
}

inventory <- read.csv(INVENTORY_FILE, stringsAsFactors=FALSE, check.names=FALSE)
audit <- read.csv(AUDIT_FILE, stringsAsFactors=FALSE, check.names=FALSE)
factors <- read.csv(FACTOR_FILE, stringsAsFactors=FALSE, check.names=FALSE)
energy_factors <- read.csv(ENERGY_FACTOR_FILE, stringsAsFactors=FALSE, check.names=FALSE)
validation_sources <- read.csv(VALIDATION_SOURCE_FILE, stringsAsFactors=FALSE, check.names=FALSE)
external_benchmarks <- read.csv(BENCHMARK_FILE, stringsAsFactors=FALSE, check.names=FALSE)

energy_req <- c("model_biofuel","ivc_id","lhv_mj_per_kg","basis_quality","source_id")
if (!all(energy_req %in% names(energy_factors))) stop("Energy-factor CSV has wrong schema.")
if (anyDuplicated(paste(energy_factors$model_biofuel, energy_factors$ivc_id, sep="||"))) {
  stop("Energy-factor model_biofuel/ivc_id keys are not unique.")
}
if (any(!is.finite(energy_factors$lhv_mj_per_kg) | energy_factors$lhv_mj_per_kg <= 0)) {
  stop("Energy-factor LHV values must be positive and finite.")
}
if (!all(energy_factors$basis_quality %in% c("direct","proxy"))) {
  stop("Energy-factor basis_quality must be direct or proxy.")
}
if (!all(c("source_id","citation_full","url") %in% names(validation_sources)) ||
    anyDuplicated(validation_sources$source_id) ||
    any(!nzchar(validation_sources$citation_full)) ||
    any(!nzchar(validation_sources$url))) {
  stop("Validation-source catalogue is incomplete or has duplicate source IDs.")
}
bench_req <- c("benchmark_id","model_biofuel","model_ivc","ghg_min_gCO2e_per_MJ",
               "ghg_max_gCO2e_per_MJ","source_id","source_locator","comparison_class",
               "circularity_or_comparability_note")
if (!all(bench_req %in% names(external_benchmarks)) ||
    anyDuplicated(external_benchmarks$benchmark_id)) {
  stop("External-benchmark CSV has wrong schema or duplicate benchmark IDs.")
}
if (any(!is.finite(external_benchmarks$ghg_min_gCO2e_per_MJ)) ||
    any(!is.finite(external_benchmarks$ghg_max_gCO2e_per_MJ)) ||
    any(external_benchmarks$ghg_min_gCO2e_per_MJ > external_benchmarks$ghg_max_gCO2e_per_MJ)) {
  stop("External-benchmark GHG bounds are invalid.")
}
if (!all(energy_factors$source_id %in% validation_sources$source_id)) {
  stop("Energy-factor table references an unknown source_id.")
}
if (!all(external_benchmarks$source_id %in% validation_sources$source_id)) {
  stop("External benchmark references an unknown source_id.")
}
underlying_ids <- unique(unlist(strsplit(
  external_benchmarks$underlying_source_ids, ";", fixed=TRUE
)))
underlying_ids <- underlying_ids[nzchar(underlying_ids)]
if (length(underlying_ids) && !all(underlying_ids %in% validation_sources$source_id)) {
  stop("External benchmark references an unknown underlying_source_id: ",
       paste(setdiff(underlying_ids,validation_sources$source_id),collapse=", "))
}

effective_bio_coefficients <- function(A_dom,A_imp,BIO,NONBIO,bio_sector,nonbio_output_coeff) {
  if (length(nonbio_output_coeff)!=length(NONBIO) || any(!is.finite(nonbio_output_coeff))) {
    stop("Invalid NONBIO output coefficient vector for BIO recursion.")
  }
  list(
    domestic=as.numeric(
      A_dom[BIO,bio_sector] +
        A_dom[BIO,NONBIO,drop=FALSE] %*% nonbio_output_coeff
    ),
    imported=as.numeric(
      A_imp[BIO,bio_sector] +
        A_imp[BIO,NONBIO,drop=FALSE] %*% nonbio_output_coeff
    )
  )
}

energy_factor_for <- function(fuel_name, ivc_id) {
  z <- energy_factors[
    energy_factors$model_biofuel == fuel_name & energy_factors$ivc_id == ivc_id,
    , drop=FALSE
  ]
  if (nrow(z) != 1) {
    stop("Expected exactly one energy factor for ",fuel_name,"/",ivc_id,
         "; found ",nrow(z),".")
  }
  z
}

preferred <- merge(
  audit[, c("feedstock_key","preferred_factor_id","coverage_status")],
  factors,
  by.x=c("feedstock_key","preferred_factor_id"),
  by.y=c("feedstock_key","factor_id"),
  all.x=TRUE,
  sort=FALSE
)
preferred_factor <- function(key) {
  z <- preferred[preferred$feedstock_key == key, , drop=FALSE]
  if (nrow(z) != 1 || is.na(z$factor_value[1]) || !nzchar(z$factor_value[1])) return(NULL)
  z$factor_value <- as.numeric(z$factor_value)
  z
}

# -------------------------------------------------------------------
# 68-sector environmental extensions -> 73-sector model.
# The five-sector difference is entirely extra biofuel splitting.
# -------------------------------------------------------------------
extension_position_for_model <- function(m) {
  if (m %in% 1:10) return(m)
  if (m == 11) return(14L)       # C20_others = chemicals
  if (m %in% 20:31) return(m-5L)
  if (m == 32) return(28L)       # D_others = electricity
  if (m %in% 34:73) return(m-5L)
  NA_integer_                     # 12:19 and 33 are BIO
}
read_extension <- function(path, boundary) {
  x <- read.csv(path, stringsAsFactors=FALSE, check.names=FALSE)
  req <- c("sector_position","sector","boundary","extension","stressor","unit","intensity")
  if (!all(req %in% names(x))) stop(path, " has wrong schema.")
  available <- unique(as.character(x$boundary))
  if (!boundary %in% available) {
    stop(path, " does not contain requested boundary '",boundary,
         "'. Available boundaries: ",paste(sort(available),collapse=", "),".")
  }
  x <- x[x$boundary == boundary, , drop=FALSE]
  if (!nrow(x) || !identical(unique(as.character(x$boundary)), boundary)) {
    stop(path, " failed to isolate requested boundary '",boundary,"'.")
  }
  x
}
dom_ext <- read_extension(DOM_EXT_FILE, "scope_production")
imp_ext <- read_extension(IMP_EXT_FILE, "external_imports_direct")

# IPCC AR6 GWP100: CO2=1; CH4 non-fossil/combustion=27.0;
# fossil fugitive/process CH4=29.8; N2O=273; SF6=25184; NF3=17423.
# HFC/PFC rows in the repo are already kg CO2-eq and are used as supplied.
gwp100_cf <- function(stressor, unit) {
  s <- trimws(stressor); u <- trimws(unit)
  if (u == "kg CO2-eq" && s %in% c("HFC - air","PFC - air")) return(1)
  if (s == "SF6 - air") return(25184)
  if (s == "NF3 - air") return(17423)
  if (grepl("^CO2_bio ", s) || s == "CO2 - waste - biogenic - air") return(0)
  if (grepl("^CO2 ", s)) return(1)
  if (s %in% c("CH4_bio - combustion - air","CH4 - combustion - air",
               "CH4 - agriculture - air","CH4 - waste - air")) return(27.0)
  if (grepl("^CH4 - non combustion", s)) return(29.8)
  if (grepl("^N2O", s)) return(273)
  0
}
aggregate_ghg_intensity <- function(ext) {
  ext <- ext[ext$extension == "air_emissions", , drop=FALSE]
  ext$cf <- mapply(gwp100_cf, ext$stressor, ext$unit)
  ext$ghg <- as.numeric(ext$intensity) * ext$cf
  ext$ghg[is.na(ext$ghg)] <- 0
  aggregate(ghg ~ sector_position + sector, data=ext, FUN=sum)
}
dom68 <- aggregate_ghg_intensity(dom_ext)
imp68 <- aggregate_ghg_intensity(imp_ext)
if (!setequal(unique(dom68$sector_position), 1:68) ||
    !setequal(unique(imp68$sector_position), 1:68)) {
  stop("Expected complete 68-sector extension tables.")
}

expected_spots <- c(
  `1`="CPA_A01", `2`="CPA_A02", `5`="CPA_C10-12", `8`="CPA_C17",
  `11`="CPA_C20_others", `24`="CPA_C25", `25`="CPA_C26",
  `26`="CPA_C27", `27`="CPA_C28", `31`="CPA_C33",
  `32`="CPA_D_others", `35`="CPA_E37-39", `36`="CPA_F",
  `40`="CPA_H49", `54`="CPA_M69_70", `55`="CPA_M71"
)
for (mp in names(expected_spots)) {
  ep <- extension_position_for_model(as.integer(mp))
  actual <- dom68$sector[match(ep, dom68$sector_position)]
  if (!identical(actual, unname(expected_spots[[mp]]))) {
    stop("68->73 crosswalk failed at model sector ", mp, ": got ", actual)
  }
}

ghg_dom <- rep(NA_real_, length(sector_names))
ghg_imp <- rep(NA_real_, length(sector_names))
ext_sector <- rep(NA_character_, length(sector_names))
for (m in NONBIO) {
  e <- extension_position_for_model(m)
  ghg_dom[m] <- dom68$ghg[match(e, dom68$sector_position)]
  ghg_imp[m] <- imp68$ghg[match(e, imp68$sector_position)]
  ext_sector[m] <- dom68$sector[match(e, dom68$sector_position)]
}
if (anyNA(ghg_dom[NONBIO]) || anyNA(ghg_imp[NONBIO])) stop("Incomplete NONBIO GHG intensities.")

write.csv(data.frame(
  model_sector_position=seq_along(sector_names),
  model_sector=sector_names,
  is_biofuel=seq_along(sector_names) %in% BIO,
  extension_sector=ext_sector,
  domestic_kgCO2e_per_MEUR=ghg_dom,
  imported_direct_kgCO2e_per_MEUR=ghg_imp
), file.path(OUTPUT_DIR,"ghg_io_intensity_model_sectors.csv"), row.names=FALSE)

# ===================================================================
# Workbook physical-feedstock reconstruction
# ===================================================================
mix <- read_excel(WORKBOOK_FILE, sheet="Feedstock MIX per IVC",
                  col_names=FALSE, .name_repair="minimal")

advanced_blocks <- list(
  IVC1=4:4, IVC2_HVO=6:7, IVC2_HEFA=9:11, IVC5=13:16,
  IVC6=18:18, IVC7=20:28, IVC8a=30:39, IVC8b=41:41,
  IVC8c=43:43, IVC9a=45:54, IVC9b=56:56,
  IVC11a_road=58:67, IVC11a_SAF=69:78, IVC12=80:80,
  IVC13a=84:91, IVC13b_road=102:109,
  IVC13b_mar=111:118, IVC13b_SAF=111:118
)
candidate_share_cols <- c(F=6L, I=9L, K=11L)

feedstock_key_from_label <- function(label) {
  z <- norm_label(label)
  if (grepl("pome",z)) return("palm_oil_mill_effluent_raw")
  if (grepl("tall oil",z)) return("tall_oil")
  if (grepl("oil crops abandoned",z)) return("oil_crops_abandoned_degraded")
  if (z=="straw") return("winter_wheat_straw")
  if (grepl("lignocrops abandoned degraded",z)) return("lignocrops_abandoned_degraded")
  if (grepl("lignocrops inter.*cover cropping",z)) return("lignocrops_inter_cover_cropping")
  if (grepl("prunings.*damaged crops",z)) return("prunings_damaged_crops")
  if (grepl("agroprocessing residues.*maize cobs",z)) return("agroprocessing_residues_maize_cobs")
  if (z=="agroprocessing residues") return("agroprocessing_residues")
  if (grepl("^manure",z)) return("animal_manure_raw")
  if (grepl("sewage sludge",z)) return("sewage_sludge_raw")
  if (grepl("biowastes.*post consumer woods excluded",z)) return("industrial_biowaste_non_postconsumer_wood")
  if (grepl("primary forestry residues",z)) return("primary_forestry_residues")
  if (grepl("secondary forest residues",z)) return("secondary_forest_residues")
  if (grepl("biowastes.*post consumer wood",z)) return("post_consumer_wood_biowaste")
  if (grepl("fpbo.*biocrude",z)) return("fpbo_biocrude_intermediate")
  if (grepl("ethanol.*methanol",z)) return("ethanol_methanol_intermediate")
  if (grepl("crude glycerine",z)) return("crude_glycerine_intermediate")
  if (grepl("fpbo.*lower quality",z)) return("fpbo_low_quality_intermediate")
  if (grepl("biomethane",z)) return("biomethane_intermediate")
  if (grepl("syngas",z)) return("syngas_intermediate")
  if (grepl("e biofuel feedstocks",z)) return("biogenic_co2_electrolytic_h2_composite")
  NA_character_
}
advanced_candidate_mix <- function(ivc_id, share_col) {
  rows <- advanced_blocks[[ivc_id]]
  if (is.null(rows)) return(NULL)

  out <- lapply(rows, function(r) {
    label <- as.character(mix[[1]][r])
    price <- num(mix[[2]][r])
    conversion <- num(mix[[3]][r])
    share <- num(mix[[share_col]][r])
    data.frame(
      workbook_row=r,
      workbook_label=label,
      feedstock_key=feedstock_key_from_label(label),
      price_eur_per_t=price,
      conversion_t_fuel_per_t_feedstock=conversion,
      mix_share=share,
      stringsAsFactors=FALSE
    )
  })
  out <- do.call(rbind,out)
  out <- out[!is.na(out$mix_share) & abs(out$mix_share)>1e-15,,drop=FALSE]
  if (!nrow(out)) return(NULL)

  ok <- is.finite(out$conversion_t_fuel_per_t_feedstock) &
        out$conversion_t_fuel_per_t_feedstock > 0
  out$q_t_feedstock_per_t_fuel <- NA_real_
  out$q_t_feedstock_per_t_fuel[ok] <-
    out$mix_share[ok] / out$conversion_t_fuel_per_t_feedstock[ok]
  out$cost_eur_per_t_fuel <- out$q_t_feedstock_per_t_fuel * out$price_eur_per_t
  out
}
get_ivc_prod_cost <- function(fuel_cfg,ivc_id) {
  z <- NULL
  if (!is.null(fuel_cfg$prod_cost)) z <- fuel_cfg$prod_cost[[ivc_id]]
  if (is.null(z)) z <- IVC_TECH_LIBRARY[[ivc_id]]$prod_cost
  if (is.null(z)) stop("Missing production cost for ",ivc_id)
  as.numeric(z)
}
get_ivc_alpha <- function(fuel_cfg,ivc_id) {
  z <- NULL
  if (!is.null(fuel_cfg$alpha)) z <- fuel_cfg$alpha[[ivc_id]]
  if (is.null(z)) z <- IVC_TECH_LIBRARY[[ivc_id]]$alpha
  if (is.null(z) || is.null(z[["feed"]])) stop("Missing feed alpha for ",ivc_id)
  z
}
choose_advanced_mix <- function(fuel_cfg,ivc_id) {
  target <- get_ivc_prod_cost(fuel_cfg,ivc_id) * get_ivc_alpha(fuel_cfg,ivc_id)[["feed"]]
  candidates <- lapply(names(candidate_share_cols), function(nm) {
    tab <- advanced_candidate_mix(ivc_id,candidate_share_cols[[nm]])
    if (is.null(tab)) return(NULL)
    cost <- sum(tab$cost_eur_per_t_fuel,na.rm=TRUE)
    list(name=nm,table=tab,cost=cost,error=abs(cost-target))
  })
  candidates <- Filter(Negate(is.null),candidates)
  if (!length(candidates)) return(NULL)
  best <- candidates[[which.min(vapply(candidates,`[[`,numeric(1),"error"))]]
  tol <- max(5,0.025*max(1,abs(target)))
  if (!is.finite(best$error) || best$error>tol) {
    stop("No workbook mix reconciles with model for ",ivc_id,
         ": target=",signif(target,8),", best=",signif(best$cost,8))
  }
  best$target_cost <- target
  best
}

# Conventional source workbook. Numeric values remain in Excel.
conv <- read_excel(WORKBOOK_FILE, sheet="Feedtstocks only CONVENTIONAL",
                   col_names=FALSE, .name_repair="minimal")

conv_feedstock_key <- function(label) {
  z <- norm_label(label)
  if (z=="rapeseed oil") return("rapeseed_oil")
  if (z=="soybean oil") return("soybean_oil")
  if (z=="sunflower oil") return("sunflower_oil")
  if (z=="palm oil") return("palm_oil")
  if (z=="uco") return("used_cooking_oil")
  if (grepl("animal fat",z)) return("rendered_animal_fat")
  if (grepl("wheat kernels",z)) return("wheat_kernels")
  if (grepl("corn kern",z)) return("corn_kernels")
  if (grepl("sugar beet",z)) return("sugar_beets")
  if (grepl("cover crops",z)) return("cover_crop_oil_marginal_land")
  NA_character_
}
extract_conventional_rows <- function(rows) {
  out <- lapply(rows,function(r) {
    label <- as.character(conv[[4]][r])
    if (is.na(label) || !nzchar(trimws(label))) label <- as.character(conv[[3]][r])
    data.frame(
      workbook_row=r,
      workbook_label=label,
      feedstock_key=conv_feedstock_key(label),
      price_eur_per_t=num(conv[[5]][r]),
      conversion_t_fuel_per_t_feedstock=num(conv[[6]][r]),
      mix_share=num(conv[[8]][r]),
      stringsAsFactors=FALSE
    )
  })
  out <- do.call(rbind,out)
  out <- out[!is.na(out$feedstock_key),,drop=FALSE]
  out$q_t_feedstock_per_t_fuel <- out$mix_share/out$conversion_t_fuel_per_t_feedstock
  out
}
conv_blocks <- list(
  food_oils=4:7,
  lipid_transesterification=16:18,
  lipid_hydrotreatment=37:39,
  ethanol_crops=52:57,
  cover_crop=9:9
)
conventional_physical_mix <- function(ivc_id) {
  rows <- NULL
  if (ivc_id %in% c("IVC_T_FF","IVC_HT_FF")) {
    rows <- extract_conventional_rows(conv_blocks$food_oils)
  } else if (ivc_id=="IVC_T_lipids") {
    rows <- extract_conventional_rows(conv_blocks$lipid_transesterification)
  } else if (ivc_id %in% c("IVC_HT_lipids","IVC_HT_lipids_SAF")) {
    rows <- extract_conventional_rows(conv_blocks$lipid_hydrotreatment)
    if (!nrow(rows) || any(!is.finite(rows$q_t_feedstock_per_t_fuel))) {
      rows <- extract_conventional_rows(conv_blocks$lipid_transesterification)
    }
  } else if (ivc_id=="IVC_EF_FF") {
    rows <- extract_conventional_rows(conv_blocks$ethanol_crops)
  } else if (ivc_id %in% c("IVC_T_CC","IVC_HT_CC","IVC_HT_CC_SAF")) {
    rows <- extract_conventional_rows(conv_blocks$cover_crop)
  }
  if (is.null(rows) || !nrow(rows)) return(NULL)
  ok <- is.finite(rows$q_t_feedstock_per_t_fuel) &
        rows$q_t_feedstock_per_t_fuel >= 0 &
        !is.na(rows$feedstock_key)
  if (!all(ok)) return(NULL)
  rows
}

# ===================================================================
# IO GHG for OPEX/CAPEX and explicit feedstock fallbacks
# ===================================================================
io_ghg_from_direct_vectors <- function(endpoint,dom_direct,imp_direct) {
  stopifnot(length(dom_direct)==length(NONBIO),
            length(imp_direct)==length(NONBIO))
  A_NN <- endpoint$A_dom_tech[NONBIO,NONBIO,drop=FALSE]
  L_NN <- solve(diag(length(NONBIO))-A_NN)
  x_dom <- as.numeric(L_NN %*% dom_direct)

  # Direct imports of the channel plus imports required by its domestic
  # upstream chain. Imported GHG remains external_imports_direct only.
  imp_use <- as.numeric(
    imp_direct +
      endpoint$A_imp_tech[NONBIO,NONBIO,drop=FALSE] %*% x_dom
  )
  e_dom <- sum(ghg_dom[NONBIO]*x_dom)
  e_imp <- sum(ghg_imp[NONBIO]*imp_use)
  list(
    domestic_kgCO2e=e_dom,
    imported_direct_kgCO2e=e_imp,
    total_kgCO2e=e_dom+e_imp,
    domestic_output_MEUR=sum(x_dom),
    imported_use_MEUR=sum(imp_use),
    domestic_output_vector_MEUR=x_dom,
    imported_use_vector_MEUR=imp_use
  )
}
channel_ghg_for_biofuel <- function(endpoint,channel,bio_sector) {
  dn <- paste0("A_",channel,"_dom_tech")
  im <- paste0("A_",channel,"_imp_tech")
  if (is.null(endpoint[[dn]]) || is.null(endpoint[[im]])) {
    stop("Missing ",channel," channel matrices.")
  }
  xbio <- endpoint$X_bio[bio_sector]
  ddom <- as.numeric(endpoint[[dn]][NONBIO,bio_sector]*xbio)
  dimp <- as.numeric(endpoint[[im]][NONBIO,bio_sector]*xbio)
  io_ghg_from_direct_vectors(endpoint,ddom,dimp)
}
feedstock_io_fallback <- function(endpoint,fuel_cfg,ivc_id,ivc_value_MEUR) {
  tech <- IVC_TECH_LIBRARY[[ivc_id]]
  prod_cost <- get_ivc_prod_cost(fuel_cfg,ivc_id)
  alpha <- get_ivc_alpha(fuel_cfg,ivc_id)
  dist <- fuel_cfg$dist_feed[[ivc_id]]
  if (is.null(dist)) stop("No dist_feed for fallback ",ivc_id)

  s_feed <- (prod_cost/tech$market_price)*alpha[["feed"]]
  purchased <- pmax(s_feed*dist,0)

  dom <- numeric(length(NONBIO)); imp <- numeric(length(NONBIO))
  for (nm in names(purchased)) {
    v <- purchased[[nm]]*ivc_value_MEUR
    if (v==0) next
    is_imp <- grepl("_imp$",nm)
    clean <- sub("_imp$","",nm)
    if (!clean %in% names(INPUT_SECTORS)) next
    model_row <- INPUT_SECTORS[[clean]]
    if (model_row %in% BIO || !model_row %in% NONBIO) next
    j <- match(model_row,NONBIO)
    if (is_imp) imp[j] <- imp[j]+v else dom[j] <- dom[j]+v
  }
  io_ghg_from_direct_vectors(endpoint,dom,imp)
}
# ===================================================================
# Benchmark-year analysis (2030/2035/2040)
# ===================================================================
feed_detail <- list()
feed_recon <- list()
io_detail <- list()
hybrid_rows <- list()
recursion_support <- list()
fc <- rc <- ic <- hc <- 1L

for (year in benchmark_years) {
  for (scenario_name in scenario_names) {
    endpoint <- results[[year]][[scenario_name]]
    scenario_cfg <- SCENARIO_CONFIGS[[year]][[scenario_name]]
    if (is.null(scenario_cfg)) stop("Missing saved config ",year,"/",scenario_name)

    for (fuel_name in names(scenario_cfg)) {
      if (!fuel_name %in% names(BIOFUEL_SECTORS)) next
      bio_sector <- BIOFUEL_SECTORS[[fuel_name]]
      fuel_cfg <- scenario_cfg[[fuel_name]]
      weights <- normalize_weights(fuel_cfg$weights)

      # OPEX and CAPEX use the exact solved channel matrices.
      opex <- channel_ghg_for_biofuel(endpoint,"opex",bio_sector)
      capex <- channel_ghg_for_biofuel(endpoint,"capex",bio_sector)

      for (channel in c("OPEX","CAPEX")) {
        z <- if (channel=="OPEX") opex else capex
        io_detail[[ic]] <- data.frame(
          year=as.integer(year), scenario=scenario_name, biofuel=fuel_name,
          bio_sector=bio_sector, channel=channel,
          domestic_kgCO2e=z$domestic_kgCO2e,
          imported_direct_kgCO2e=z$imported_direct_kgCO2e,
          total_kgCO2e=z$total_kgCO2e,
          domestic_output_MEUR=z$domestic_output_MEUR,
          imported_use_MEUR=z$imported_use_MEUR,
          imported_boundary="external_imports_direct; no foreign Leontief",
          stringsAsFactors=FALSE
        )
        ic <- ic+1L
      }

      feed_phys <- 0
      feed_io <- 0
      flags <- character()
      fuel_energy_MJ <- 0
      fallback_domestic_output_MEUR <- numeric(length(NONBIO))

      for (ivc_id in names(weights)) {
        w <- weights[[ivc_id]]
        tech <- IVC_TECH_LIBRARY[[ivc_id]]
        if (is.null(tech) || is.null(tech$market_price)) {
          stop("Missing technology/market price for ",ivc_id)
        }

        ivc_value_eur <- fuel_cfg$abs_market_value*w
        ivc_value_MEUR <- ivc_value_eur/SCENARIO_EUR_TO_IO_UNIT
        fuel_tonnes <- ivc_value_eur/tech$market_price

        energy_factor <- energy_factor_for(fuel_name,ivc_id)
        fuel_energy_MJ <- fuel_energy_MJ +
          fuel_tonnes*1000*energy_factor$lhv_mj_per_kg[1]
        if (energy_factor$basis_quality[1] != "direct") {
          flags <- c(flags,paste0(ivc_id,":energy_basis_",energy_factor$basis_quality[1]))
        }

        physical <- NULL
        physical_method <- NA_character_

        if (ivc_id %in% names(advanced_blocks)) {
          choice <- choose_advanced_mix(fuel_cfg,ivc_id)
          if (!is.null(choice)) {
            physical <- choice$table
            physical_method <- paste0(
              "Providing sectors.xlsx / Feedstock MIX per IVC / mix column ",
              choice$name
            )
            feed_recon[[rc]] <- data.frame(
              year=as.integer(year), scenario=scenario_name,
              biofuel=fuel_name, ivc_id=ivc_id,
              target_feed_cost_eur_per_t_fuel=choice$target_cost,
              workbook_feed_cost_eur_per_t_fuel=choice$cost,
              absolute_error_eur_per_t_fuel=choice$error,
              selected_workbook_mix_column=choice$name,
              stringsAsFactors=FALSE
            )
            rc <- rc+1L
          }
        } else {
          physical <- conventional_physical_mix(ivc_id)
          if (!is.null(physical)) {
            physical_method <- "Providing sectors.xlsx / Feedtstocks only CONVENTIONAL"
          }
        }

        has_primary <- FALSE
        ivc_primary_ghg <- 0

        if (!is.null(physical) && nrow(physical)) {
          for (j in seq_len(nrow(physical))) {
            key <- physical$feedstock_key[j]
            q <- physical$q_t_feedstock_per_t_fuel[j]
            if (is.na(key) || !is.finite(q) || q<0) next

            inv <- inventory[inventory$feedstock_key==key,,drop=FALSE]
            if (nrow(inv)!=1) stop("Inventory key missing/duplicated: ",key)
            role <- inv$role[1]

            if (role=="primary_physical_feedstock") {
              fac <- preferred_factor(key)
              if (is.null(fac)) stop("No preferred numeric GHG factor for ",key)
              if (!grepl("^kgCO2e_per_t",fac$factor_unit[1])) {
                stop("Unsupported preferred factor unit for ",key,": ",fac$factor_unit[1])
              }

              feed_tonnes <- fuel_tonnes*q
              e <- feed_tonnes*fac$factor_value[1]
              feed_phys <- feed_phys+e
              ivc_primary_ghg <- ivc_primary_ghg+e
              has_primary <- TRUE

              feed_detail[[fc]] <- data.frame(
                year=as.integer(year), scenario=scenario_name,
                biofuel=fuel_name, bio_sector=bio_sector, ivc_id=ivc_id,
                ivc_weight_market_value=w,
                ivc_market_value_eur=ivc_value_eur,
                ivc_fuel_tonnes=fuel_tonnes,
                feedstock_key=key,
                feedstock_label=inv$workbook_label[1],
                q_t_feedstock_per_t_fuel=q,
                feedstock_tonnes=feed_tonnes,
                factor_id=fac$preferred_factor_id[1],
                factor_value=fac$factor_value[1],
                factor_unit=fac$factor_unit[1],
                quality_flag=fac$quality_flag[1],
                feedstock_kgCO2e=e,
                treatment="physical_feedstock_factor",
                physical_source=physical_method,
                stringsAsFactors=FALSE
              )
              fc <- fc+1L

              good <- c(
                "direct_category_match","direct_literature",
                "direct_literature_internal_source_inconsistency_noted",
                "direct_product_global_median","close_product_match",
                "close_product_match_category_specific"
              )
              if (!fac$quality_flag[1] %in% good) {
                flags <- c(flags,paste0(ivc_id,":",key,":",fac$quality_flag[1]))
              }

            } else if (role=="intermediate_bioenergy_carrier") {
              # Counted at the producer sector; no second footprint here.
              feed_detail[[fc]] <- data.frame(
                year=as.integer(year), scenario=scenario_name,
                biofuel=fuel_name, bio_sector=bio_sector, ivc_id=ivc_id,
                ivc_weight_market_value=w,
                ivc_market_value_eur=ivc_value_eur,
                ivc_fuel_tonnes=fuel_tonnes,
                feedstock_key=key,
                feedstock_label=inv$workbook_label[1],
                q_t_feedstock_per_t_fuel=q,
                feedstock_tonnes=fuel_tonnes*q,
                factor_id=NA_character_, factor_value=NA_real_,
                factor_unit=NA_character_,
                quality_flag="recursive_intermediate_no_extra_factor",
                feedstock_kgCO2e=0,
                treatment="recursive_intermediate_counted_at_producer_sector",
                physical_source=physical_method,
                stringsAsFactors=FALSE
              )
              fc <- fc+1L
            }
          }
        }

        # IO fallback only when the physical primary-feedstock route is
        # unresolved, or for RFNBO H2+CO2 composite feedstocks.
        pure_recursive <- ivc_id %in% c("IVC6","IVC8b","IVC12")
        rfnbio_process <- ivc_id %in% c("IVC8c","IVC9b")
        needs_fallback <- (!has_primary && !pure_recursive) || rfnbio_process

        if (needs_fallback) {
          fb <- feedstock_io_fallback(endpoint,fuel_cfg,ivc_id,ivc_value_MEUR)
          feed_io <- feed_io+fb$total_kgCO2e
          fallback_domestic_output_MEUR <- fallback_domestic_output_MEUR +
            fb$domestic_output_vector_MEUR
          flags <- c(flags,paste0(ivc_id,":IO_feedstock_fallback"))

          feed_detail[[fc]] <- data.frame(
            year=as.integer(year), scenario=scenario_name,
            biofuel=fuel_name, bio_sector=bio_sector, ivc_id=ivc_id,
            ivc_weight_market_value=w,
            ivc_market_value_eur=ivc_value_eur,
            ivc_fuel_tonnes=fuel_tonnes,
            feedstock_key=NA_character_, feedstock_label=NA_character_,
            q_t_feedstock_per_t_fuel=NA_real_, feedstock_tonnes=NA_real_,
            factor_id=NA_character_, factor_value=NA_real_,
            factor_unit=NA_character_, quality_flag="IO_fallback",
            feedstock_kgCO2e=fb$total_kgCO2e,
            treatment="IO_feedstock_fallback",
            physical_source="model dist_feed + environmental extensions",
            stringsAsFactors=FALSE
          )
          fc <- fc+1L
        }
      }

      stage_no_capex <- feed_phys+feed_io+opex$total_kgCO2e
      hybrid <- stage_no_capex+capex$total_kgCO2e
      xfuel <- endpoint$X_bio[bio_sector]
      if (xfuel>0 && (!is.finite(fuel_energy_MJ) || fuel_energy_MJ<=0)) {
        stop("Positive fuel output has no valid physical-energy denominator for ",
             year,"/",scenario_name,"/",fuel_name,".")
      }

      hybrid_rows[[hc]] <- data.frame(
        year=as.integer(year), scenario=scenario_name,
        biofuel=fuel_name, bio_sector=bio_sector,
        fuel_market_value_MEUR=xfuel,
        fuel_energy_MJ=fuel_energy_MJ,
        feedstock_physical_kgCO2e=feed_phys,
        feedstock_IO_fallback_kgCO2e=feed_io,
        feedstock_total_kgCO2e=feed_phys+feed_io,
        opex_kgCO2e=opex$total_kgCO2e,
        capex_kgCO2e=capex$total_kgCO2e,
        stage_hybrid_no_capex_kgCO2e=stage_no_capex,
        hybrid_total_kgCO2e=hybrid,
        hybrid_kgCO2e_per_MEUR_fuel=
          ifelse(xfuel>0,hybrid/xfuel,NA_real_),
        stage_hybrid_no_capex_kgCO2e_per_MEUR_fuel=
          ifelse(xfuel>0,stage_no_capex/xfuel,NA_real_),
        stage_hybrid_gCO2e_per_MJ=
          ifelse(fuel_energy_MJ>0,hybrid*1000/fuel_energy_MJ,NA_real_),
        stage_hybrid_no_capex_gCO2e_per_MJ=
          ifelse(fuel_energy_MJ>0,stage_no_capex*1000/fuel_energy_MJ,NA_real_),
        method_flags=paste(unique(flags),collapse=";"),
        stringsAsFactors=FALSE
      )
      hc <- hc+1L
    }
  }
}

feed_detail_df <- if (length(feed_detail)) do.call(rbind,feed_detail) else data.frame()
feed_recon_df <- if (length(feed_recon)) do.call(rbind,feed_recon) else data.frame()
io_detail_df <- if (length(io_detail)) do.call(rbind,io_detail) else data.frame()
hybrid_df <- if (length(hybrid_rows)) do.call(rbind,hybrid_rows) else data.frame()

# ===================================================================
# Domestic BIO-to-BIO recursion for per-fuel lifecycle validation
# ===================================================================
# Stage-attributed totals are additive accounting components. For comparison
# against per-unit-fuel JEC/RED/CORSIA values, domestic model bioenergy
# intermediates must carry the upstream footprint of their producer stage.
recursive_bio_intensity <- function(A_BB, stage_intensity) {
  if (!is.matrix(A_BB) || nrow(A_BB)!=ncol(A_BB) ||
      nrow(A_BB)!=length(stage_intensity)) {
    stop("Invalid BIO recursion dimensions.")
  }
  if (any(!is.finite(A_BB)) || any(!is.finite(stage_intensity))) {
    stop("BIO recursion received non-finite values.")
  }
  M <- diag(nrow(A_BB))-A_BB
  condition <- rcond(M)
  if (!is.finite(condition) || condition < 1e-12) {
    stop("Domestic BIO intermediate system is singular/ill-conditioned for GHG recursion.")
  }
  as.numeric(stage_intensity %*% solve(M))
}

hybrid_df$recursive_hybrid_kgCO2e_per_MEUR_fuel <- NA_real_
hybrid_df$recursive_hybrid_no_capex_kgCO2e_per_MEUR_fuel <- NA_real_
hybrid_df$recursive_hybrid_total_kgCO2e <- NA_real_
hybrid_df$recursive_hybrid_no_capex_total_kgCO2e <- NA_real_
hybrid_df$recursive_hybrid_gCO2e_per_MJ <- NA_real_
hybrid_df$recursive_hybrid_no_capex_gCO2e_per_MJ <- NA_real_

for (year in benchmark_years) {
  for (scenario_name in scenario_names) {
    endpoint <- results[[year]][[scenario_name]]
    idx <- which(hybrid_df$year==as.integer(year) & hybrid_df$scenario==scenario_name)
    if (!length(idx)) next

    A_BB_imp <- endpoint$A_imp_tech[BIO,BIO,drop=FALSE]
    if (any(abs(A_BB_imp)>1e-12)) {
      stop("Recursive external-validation footprint found imported BIO-to-BIO intermediate coefficients for ",
           year,"/",scenario_name,
           ". A foreign biofuel upstream closure is not available; refusing to treat them as domestic or zero.")
    }

    stage_full <- numeric(length(BIO))
    stage_no_capex <- numeric(length(BIO))
    for (k in seq_along(BIO)) {
      bio_sector <- BIO[k]
      xfuel <- endpoint$X_bio[bio_sector]
      fuel_name <- names(BIOFUEL_SECTORS)[match(bio_sector,unname(BIOFUEL_SECTORS))]
      row <- idx[hybrid_df$biofuel[idx]==fuel_name]
      if (length(row)>1) stop("Duplicate hybrid row for ",year,"/",scenario_name,"/",fuel_name)
      if (is.finite(xfuel) && xfuel>1e-12) {
        if (length(row)!=1) stop("Missing hybrid row for positive BIO output ",fuel_name)
        stage_full[k] <- hybrid_df$hybrid_total_kgCO2e[row]/xfuel
        stage_no_capex[k] <- hybrid_df$stage_hybrid_no_capex_kgCO2e[row]/xfuel
      }
    }

    A_BB <- endpoint$A_dom_tech[BIO,BIO,drop=FALSE]
    recursive_full <- recursive_bio_intensity(A_BB,stage_full)
    recursive_no_capex <- recursive_bio_intensity(A_BB,stage_no_capex)

    for (row in idx) {
      bio_sector <- hybrid_df$bio_sector[row]
      k <- match(bio_sector,BIO)
      if (is.na(k)) stop("Hybrid row bio_sector is not in BIO.")
      xfuel <- hybrid_df$fuel_market_value_MEUR[row]
      hybrid_df$recursive_hybrid_kgCO2e_per_MEUR_fuel[row] <- recursive_full[k]
      hybrid_df$recursive_hybrid_no_capex_kgCO2e_per_MEUR_fuel[row] <- recursive_no_capex[k]
      hybrid_df$recursive_hybrid_total_kgCO2e[row] <- recursive_full[k]*xfuel
      hybrid_df$recursive_hybrid_no_capex_total_kgCO2e[row] <- recursive_no_capex[k]*xfuel
      if (hybrid_df$fuel_energy_MJ[row]>0) {
        hybrid_df$recursive_hybrid_gCO2e_per_MJ[row] <-
          hybrid_df$recursive_hybrid_total_kgCO2e[row]*1000/hybrid_df$fuel_energy_MJ[row]
        hybrid_df$recursive_hybrid_no_capex_gCO2e_per_MJ[row] <-
          hybrid_df$recursive_hybrid_no_capex_total_kgCO2e[row]*1000/hybrid_df$fuel_energy_MJ[row]
      }
    }
  }
}

if (any(!is.finite(hybrid_df$recursive_hybrid_kgCO2e_per_MEUR_fuel[
      hybrid_df$fuel_market_value_MEUR>0]))) {
  stop("Non-finite recursive hybrid GHG intensity for positive fuel output.")
}

method_comparison <- rbind(
  data.frame(
    hybrid_df[c("year","scenario","biofuel","bio_sector","fuel_market_value_MEUR","fuel_energy_MJ")],
    method="stage_hybrid_full",
    kgCO2e_per_MEUR=hybrid_df$hybrid_kgCO2e_per_MEUR_fuel,
    gCO2e_per_MJ=hybrid_df$stage_hybrid_gCO2e_per_MJ,
    boundary_note="Stage-attributed physical feedstock + IO OPEX + IO CAPEX; domestic BIO intermediates not recursively embodied.",
    stringsAsFactors=FALSE
  ),
  data.frame(
    hybrid_df[c("year","scenario","biofuel","bio_sector","fuel_market_value_MEUR","fuel_energy_MJ")],
    method="stage_hybrid_no_capex",
    kgCO2e_per_MEUR=hybrid_df$stage_hybrid_no_capex_kgCO2e_per_MEUR_fuel,
    gCO2e_per_MJ=hybrid_df$stage_hybrid_no_capex_gCO2e_per_MJ,
    boundary_note="Stage-attributed physical feedstock + IO OPEX; CAPEX excluded for closer fuel-cycle comparison.",
    stringsAsFactors=FALSE
  ),
  data.frame(
    hybrid_df[c("year","scenario","biofuel","bio_sector","fuel_market_value_MEUR","fuel_energy_MJ")],
    method="recursive_hybrid_full",
    kgCO2e_per_MEUR=hybrid_df$recursive_hybrid_kgCO2e_per_MEUR_fuel,
    gCO2e_per_MJ=hybrid_df$recursive_hybrid_gCO2e_per_MJ,
    boundary_note="Stage full footprint recursively embodies domestic BIO-to-BIO intermediates through (I-A_BB)^-1.",
    stringsAsFactors=FALSE
  ),
  data.frame(
    hybrid_df[c("year","scenario","biofuel","bio_sector","fuel_market_value_MEUR","fuel_energy_MJ")],
    method="recursive_hybrid_no_capex",
    kgCO2e_per_MEUR=hybrid_df$recursive_hybrid_no_capex_kgCO2e_per_MEUR_fuel,
    gCO2e_per_MJ=hybrid_df$recursive_hybrid_no_capex_gCO2e_per_MJ,
    boundary_note="Stage no-CAPEX footprint recursively embodies domestic BIO-to-BIO intermediates; preferred internal comparator for JEC/RED/CORSIA, subject to pathway-boundary matching.",
    stringsAsFactors=FALSE
  )
)

validation_comparison <- merge(
  hybrid_df[,c(
    "year","scenario","biofuel","bio_sector","fuel_market_value_MEUR","fuel_energy_MJ",
    "recursive_hybrid_no_capex_gCO2e_per_MJ","recursive_hybrid_gCO2e_per_MJ",
    "stage_hybrid_no_capex_gCO2e_per_MJ","stage_hybrid_gCO2e_per_MJ"
  )],
  external_benchmarks[external_benchmarks$model_biofuel!="ALL_CONTEXT",,drop=FALSE],
  by.x="biofuel", by.y="model_biofuel", all=FALSE, sort=FALSE
)
validation_comparison$scenario_ivc_weight <- mapply(
  function(year,scenario_name,fuel_name,model_ivc) {
    if (is.na(model_ivc) || !nzchar(model_ivc) || grepl("/",model_ivc,fixed=TRUE)) return(NA_real_)
    cfg <- SCENARIO_CONFIGS[[as.character(year)]][[scenario_name]][[fuel_name]]
    if (is.null(cfg) || is.null(cfg$weights) || !model_ivc %in% names(cfg$weights)) return(0)
    as.numeric(cfg$weights[[model_ivc]])
  },
  validation_comparison$year,
  validation_comparison$scenario,
  validation_comparison$biofuel,
  validation_comparison$model_ivc
)
validation_comparison$scenario_route_status <- ifelse(
  is.na(validation_comparison$scenario_ivc_weight),
  "context_or_composite_route",
  ifelse(validation_comparison$scenario_ivc_weight>0,"route_present_in_scenario","route_absent_in_scenario")
)
validation_comparison$internal_preferred_method <- "recursive_hybrid_no_capex"
validation_comparison$interpretation_note <- paste(
  "No pass/fail range test is applied. Compare only after checking feedstock, allocation/credit,",
  "geography, electricity, capital and lifecycle boundaries; benchmark rows can be proxies or secondary syntheses."
)

if (!nrow(feed_detail_df) || !nrow(io_detail_df) || !nrow(hybrid_df)) {
  stop("GHG analysis produced empty output.")
}
if (any(!is.finite(hybrid_df$hybrid_total_kgCO2e))) {
  stop("Non-finite hybrid GHG result.")
}

write.csv(feed_detail_df,
          file.path(OUTPUT_DIR,"ghg_feedstock_detail_benchmark.csv"),
          row.names=FALSE)
write.csv(feed_recon_df,
          file.path(OUTPUT_DIR,"ghg_feedstock_mix_reconciliation.csv"),
          row.names=FALSE)
write.csv(io_detail_df,
          file.path(OUTPUT_DIR,"ghg_io_channels_benchmark.csv"),
          row.names=FALSE)
write.csv(hybrid_df,
          file.path(OUTPUT_DIR,"ghg_hybrid_benchmark.csv"),
          row.names=FALSE)
write.csv(method_comparison,
          file.path(OUTPUT_DIR,"ghg_method_comparison_benchmark.csv"),
          row.names=FALSE)
write.csv(validation_comparison,
          file.path(OUTPUT_DIR,"ghg_external_validation_comparison.csv"),
          row.names=FALSE)
write.csv(external_benchmarks,
          file.path(OUTPUT_DIR,"ghg_external_benchmarks_used.csv"),
          row.names=FALSE)
write.csv(validation_sources,
          file.path(OUTPUT_DIR,"ghg_validation_sources_used.csv"),
          row.names=FALSE)
write.csv(energy_factors,
          file.path(OUTPUT_DIR,"ghg_fuel_energy_factors_used.csv"),
          row.names=FALSE)

notes <- c(
  "HYBRID GHG METHOD NOTES",
  "",
  "1. Economic solver unchanged; GHG is optional post-processing.",
  "2. Primary feedstock identity/quantity is reconstructed at IVC level from Providing sectors.xlsx.",
  "3. The aggregate Eurostat feedstock vector is NOT reverse-split into straw/wood/etc after the solve.",
  "4. Model dist_feed is used for reconciliation and explicit IO fallback only.",
  "5. Intermediate bioenergy carriers receive no second primary-feedstock factor at the consuming IVC.",
  "6. OPEX/CAPEX domestic upstream output uses the scenario NONBIO Leontief inverse.",
  "7. Imports include direct channel imports plus imports induced by the domestic upstream chain.",
  "8. Imported GHG uses external_imports_direct, so it is a lower bound without a foreign Leontief system.",
  "9. The domestic extension file contains multiple valid boundaries; this analysis explicitly filters scope_production and excludes scope_final_demand_direct.",
  "10. The 68-sector extension is explicitly crosswalked to the 73-sector model.",
  "11. Direct biogenic CO2 is zero-characterized; CH4/N2O/SF6 use IPCC AR6 GWP100; HFC/PFC use source-native kg CO2-eq.",
  "12. Missing physical coefficients are never guessed: they become explicit IO_feedstock_fallback rows.",
  "13. Feedstock factor boundaries vary. Some include logistics while model OPEX contains land transport; test this overlap before publication.",
  "14. Stage-attributed hybrid columns are retained. Recursive hybrid columns additionally embody domestic BIO-to-BIO intermediates through t' = c'(I-A_BB)^-1.",
  "15. Recursive results are per-fuel lifecycle diagnostics and must not be summed across all BIO outputs because that would double-count intermediates.",
  "16. Imported BIO-to-BIO intermediate coefficients cause an explicit failure because no foreign recursive biofuel closure is available.",
  "17. gCO2e/MJ denominators use versioned RED III lower heating values; proxy energy mappings are flagged in method_flags and ghg_fuel_energy_factors.csv.",
  "18. No-CAPEX recursive intensity is the preferred internal quantity for JEC/RED/CORSIA side-by-side comparison, but it is not assumed boundary-identical.",
  "19. ghg_external_benchmarks.csv is a frozen evidence catalogue, not a calibration target. No automatic pass/fail comparison is made.",
  "20. EC Annex 4 GHG values often reproduce JEC/RED/CORSIA values; their source and circularity/comparability notes are preserved in the benchmark/source CSVs.",
  "21. BEST Matschegg et al. 2026 is context-only for feedstocks already sourced from that paper and is not treated as independent validation at that level.",
  "22. This implementation reports 2030/2035/2040 only. The current model comments state that the 2023 feedstock/OPEX diagnostic split is incomplete for adv_biogas and conv_biogasoline, so annual 2023-2040 GHG should not silently inherit that baseline gap."
)
writeLines(notes,file.path(OUTPUT_DIR,"ghg_method_notes.txt"))

cat("GHG analysis complete.\n",
    "Output directory: ",OUTPUT_DIR,"\n",
    "Hybrid rows: ",nrow(hybrid_df),"\n",
    "Feedstock detail rows: ",nrow(feed_detail_df),"\n",
    "Method-comparison rows: ",nrow(method_comparison),"\n",
    "External-validation rows: ",nrow(validation_comparison),"\n",sep="")
