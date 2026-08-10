#####################################################################
# advanced_biodiesel_coeff_2030.R
#
# Create a technical-coefficient column for the advanced-biodiesel sector
# (domestic production, Scenario 1, year 2030) using the intermediate
# consumption breakdown you provided in the screenshot.
#
# This file builds a numeric intermediate vector (monetary flows),
# converts/scales values (optional), computes the coefficient column
# a_ij = intermediate_ij / total_output_value_j and prints diagnostics.
# It also shows how to inject the column into your A matrix by sector index.
#
# NOTE: you must set adv_idx to the correct IO column index for advanced
# biodiesel in your A matrix (1-based integer). Also ensure A_base,
# baseline_final_demand_2030 and other model objects are available in the
# workspace when you run the injection / Leontief inversion steps.
#####################################################################

# --- Helper: scale monetary units (EUR -> Mio EUR) --------------------------------
scale_money <- function(x_eur, scale = 1e6) {
  return(as.numeric(x_eur) / scale)
}

# --- Build intermediate vector by industry names (from screenshot) ----------------
# The industry names below reflect the labels visible in your screenshot.
# If you prefer index-based input, replace these names by numeric names where
# the name is the row index (as a string) and use build_intermediate_vector_by_index()
intermediate_adv_biodiesel_domestic_2030 <- c(
  "Products_of_agriculture_hunting" = 22317472.46,
  "Products_of_forestry_logging" = 8355518.70,
  "Paper_and_paper_products" = 7310016.07,
  "Food_beverages_and_tobacco" = 540799.33,
  # small / omitted rows from screenshot (fill with 0 if none)
  "Sewerage_services" = 0,
  # larger supplying sectors shown in screenshot
  "Fabricated_metal_products" = 22347404.41,
  "Machinery_and_equipment_n.e.c" = 131968213.24,
  "Electricity_gas_steam_air" = 209009464.03,
  "Chemicals_and_chemical_products" = 159249106.72,
  "Legal_and_accounting_services" = 127399285.37,
  "Repair_and_installation_services" = 106003928.06,
  "Land_transport_services_and_transport" = 35334642.69
)

# If you prefer to work in Mio EUR for readability, convert now (optional)
intermediate_adv_biodiesel_domestic_2030_mio <- scale_money(intermediate_adv_biodiesel_domestic_2030, 1e6)

# --- Total output (denominator) for advanced biodiesel (domestic, Sc1, 2030) ----
# You provided: 1,999,703,662.10 EUR (production costs / total production costs)
# Ensure this is in basic-price terms consistent with your IO. We'll convert to Mio EUR
total_output_value_adv_biodiesel_2030_eur <- 1999703662.10
total_output_value_adv_biodiesel_2030_mio <- scale_money(total_output_value_adv_biodiesel_2030_eur, 1e6)

# --- Compute coefficient column (unitless) -------------------------------------
# We compute a_ij = intermediate_ij / total_output_value_j
# The division yields the same result whether we use EUR or Mio EUR, but using
# Mio EUR keeps numbers compact for inspection.
compute_coeff_col_from_named_intermediate <- function(intermediate_named_mio, total_output_mio, nIndustries = NULL, industry_order = NULL) {
  # If an industry_order is provided, expand/pad vector to that order (NA -> 0)
  if (!is.null(industry_order)) {
    vec <- rep(0, length(industry_order))
    names(vec) <- industry_order
    for (nm in names(intermediate_named_mio)) {
      if (nm %in% industry_order) vec[nm] <- intermediate_named_mio[[nm]]
      else warning(paste("Intermediate industry name not found in industry_order:", nm))
    }
    intermediate_full <- vec
  } else {
    intermediate_full <- intermediate_named_mio
  }
  if (total_output_mio <= 0) stop("total_output_mio must be > 0")
  coeff_col <- intermediate_full / total_output_mio
  return(as.numeric(coeff_col))
}

# Example: produce coefficient column for industries we listed (no full industry_order)
coeff_col_adv_biodiesel_2030 <- compute_coeff_col_from_named_intermediate(
  intermediate_adv_biodiesel_domestic_2030_mio,
  total_output_value_adv_biodiesel_2030_mio,
  industry_order = names(intermediate_adv_biodiesel_domestic_2030_mio)
)

# Diagnostics: show coefficients and column sum
coeff_df <- data.frame(
  industry = names(intermediate_adv_biodiesel_domestic_2030_mio),
  intermediate_mio = as.numeric(intermediate_adv_biodiesel_domestic_2030_mio),
  coeff = coeff_col_adv_biodiesel_2030,
  stringsAsFactors = FALSE
)

print("Advanced biodiesel (domestic) - intermediate shares (Mio EUR -> coeff) - Sc1 2030")
print(coeff_df)
cat("Sum of column coefficients (should be < 1):", sum(coeff_col_adv_biodiesel_2030), "\n")

# --- How to inject into A (index-based) ---------------------------------------
# You should set adv_idx to the correct column index for advanced biodiesel in
# your A matrix (1-based). If you prefer to inject by name, provide an industry
# ordering vector and use the inject_single_col_into_A helper below.

# Example placeholder (REPLACE adv_idx with actual integer index in your IO):
adv_idx <- NA  # <- replace NA with the integer column index for adv_biodiesel in your A matrix

inject_single_col_into_A <- function(A_base, coeff_col_named_or_vector, sector_idx, industry_order = NULL) {
  A_t <- A_base
  if (!is.null(industry_order)) {
    # coeff_col_named_or_vector must be named and aligned to industry_order
    if (is.null(names(coeff_col_named_or_vector))) stop("coeff vector must be named when providing industry_order")
    colvec <- rep(0, nrow(A_base))
    names(colvec) <- industry_order
    for (nm in names(coeff_col_named_or_vector)) {
      if (nm %in% industry_order) colvec[nm] <- coeff_col_named_or_vector[[nm]]
      else warning(paste("Name in coeff vector not found in industry_order:", nm))
    }
    A_t[, sector_idx] <- colvec
  } else {
    # assume coeff_col_named_or_vector is a numeric vector of length nrow(A_base)
    if (length(coeff_col_named_or_vector) != nrow(A_base)) stop("coeff vector length mismatch with A_base")
    A_t[, sector_idx] <- as.numeric(coeff_col_named_or_vector)
  }
  return(A_t)
}

# --- Example: injecting the computed small-column into a full A matrix ------
# NOTE: the coefficient vector we computed contains only rows for industries we
# listed above. In practice you will inject a full-length vector (length nIndustries)
# aligned with your A_base ordering. Below we show how to expand the small vector
# into a full-length one once you provide 'industry_order' and 'A_base'.

# Example usage (to run interactively):
# industry_order_full <- c("Products_of_agriculture_hunting", "Products_of_forestry_logging", ..., "Other")
# coeff_full <- compute_coeff_col_from_named_intermediate(intermediate_adv_biodiesel_domestic_2030_mio, total_output_value_adv_biodiesel_2030_mio, industry_order = industry_order_full)
# A_t <- inject_single_col_into_A(A_base, coeff_full, adv_idx)
# L_t <- solve(diag(nrow(A_t)) - A_t)
# Then run your scenario shock using L_t as usual.

# Save the computed small coefficient table for reference
save(coeff_df, file = "coeff_adv_biodiesel_domestic_2030_small.RData")

cat("Saved coeff_df to coeff_adv_biodiesel_domestic_2030_small.RData\n")

#####################################################################
# End of script
#####################################################################
