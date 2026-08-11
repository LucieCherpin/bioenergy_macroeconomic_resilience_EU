#####################################################################
### biofuel_scenarios_IO_linkages.R
### Biofuel scenarios using direct IO row extraction
### Each biofuel type draws its technical coefficients from actual IO data
#####################################################################

#####################################################################
### SCENARIO DEMAND BY BIOFUEL TYPE (millions EUR)
#####################################################################

scenario_1 <- list(
  name = "S1: Import-Focused",
  year = 2030,
  
  # Demand values (millions EUR) by biofuel type
  # These will be used to scale the IO technical coefficients
  biofuel_demands = list(
    conv_biodiesel = list(domestic = 5.00, imports = 4.50, total = 9.50),
    conv_biogasoline = list(domestic = 13.90, imports = 0.72, total = 14.62),
    adv_biodiesel = list(domestic = 0.08, imports = 6.68, total = 6.76),
    adv_biogasoline = list(domestic = 5.00, imports = 6.68, total = 11.68)
  ),
  
  total_domestic = 23.98,
  total_imports = 14.08,
  total_demand = 38.07
)

scenario_2 <- list(
  name = "S2: Domestic Autarky",
  year = 2030,
  
  biofuel_demands = list(
    conv_biodiesel = list(domestic = 8.00, imports = 0.05, total = 8.05),
    conv_biogasoline = list(domestic = 5.53, imports = 0.05, total = 5.58),
    adv_biodiesel = list(domestic = 10.00, imports = 0.26, total = 10.26),
    adv_biogasoline = list(domestic = 7.93, imports = 0.26, total = 8.19)
  ),
  
  total_domestic = 31.46,
  total_imports = 0.62,
  total_demand = 31.07
)

scenario_3 <- list(
  name = "S3: Export-Boom",
  year = 2030,
  
  biofuel_demands = list(
    conv_biodiesel = list(domestic = 19.83, imports = 0.00, total = 19.83),
    adv_biodiesel = list(domestic = 88.42, imports = 34.97, total = 123.39),
    adv_biogasoline = list(domestic = 0.00, imports = 34.96, total = 34.96)
  ),
  
  total_domestic = 108.25,
  total_imports = 69.93,
  total_demand = 38.07,
  exports = 69.98
)

#####################################################################
### BIOFUEL SECTOR INDEX MAPPING
#####################################################################
# Map each biofuel type to its column index in your IO/A matrix
# ACTION: Update these with your actual IO table column indices

BIOFUEL_SECTOR_INDICES <- list(
  conv_biodiesel = NA,           # Column index for conventional biodiesel
  conv_biogasoline = NA,         # Column index for conventional biogasoline
  conv_bio_kerosene = NA,        # Column index for conventional bio-kerosene
  conv_bio_hfo = NA,             # Column index for conventional bio-HFO
  adv_biodiesel = NA,            # Column index for advanced biodiesel
  adv_biogasoline = NA,          # Column index for advanced biogasoline
  adv_bio_kerosene = NA,         # Column index for advanced bio-kerosene
  adv_bio_hfo = NA               # Column index for advanced bio-HFO
)

#####################################################################
### FUNCTION: Extract technical coefficient column from IO
#####################################################################

extract_biofuel_tech_coeff <- function(IO_table, 
                                       biofuel_col_index,
                                       intermediate_start_row = 2,
                                       intermediate_end_row = nIndustries + 1,
                                       total_output_row = nIndustries + 19,
                                       nIndustries = 89) {
  #'
  #' Extract a technical coefficient column for a biofuel sector directly from IO
  #'
  #' @param IO_table Data frame: the full IO table (domestic or imports)
  #' @param biofuel_col_index Integer: column index of the biofuel sector
  #' @param intermediate_start_row Integer: first row of intermediate consumption (default 2)
  #' @param intermediate_end_row Integer: last row of intermediate consumption
  #' @param total_output_row Integer: row containing total supply/output
  #' @param nIndustries Integer: number of industries (default 89)
  #'
  #' @return List with:
  #'   - coeff_vector: numeric vector of technical coefficients (length nIndustries)
  #'   - intermediate_values: intermediate consumption (monetary)
  #'   - total_output: total output value
  #'   - coeff_sum: sum of coefficients (should be < 1)
  #'
  
  # Extract intermediate consumption rows for this biofuel column
  intermediate_rows <- intermediate_start_row:intermediate_end_row
  intermediate_values <- as.numeric(unlist(IO_table[intermediate_rows, biofuel_col_index]))
  
  # Extract total output (supply) for denominator
  total_output <- as.numeric(unlist(IO_table[total_output_row, biofuel_col_index]))
  
  if (total_output <= 0) {
    warning(paste("Total output for sector", biofuel_col_index, "is <= 0"))
    total_output <- 1e-6
  }
  
  # Compute technical coefficients: a_ij = intermediate_ij / total_output_j
  coeff_vector <- intermediate_values / total_output
  
  return(list(
    coeff_vector = coeff_vector,
    intermediate_values = intermediate_values,
    total_output = total_output,
    coeff_sum = sum(coeff_vector)
  ))
}

#####################################################################
### FUNCTION: Build technical coefficient matrix from IO (all columns)
#####################################################################

build_A_matrix_from_IO <- function(IO_table,
                                   intermediate_start_row = 2,
                                   intermediate_end_row = nIndustries + 1,
                                   total_output_row = nIndustries + 19,
                                   nIndustries = 89,
                                   col_start = 3,
                                   col_end = nIndustries + 2) {
  #'
  #' Build the full technical coefficient matrix (A) from IO table by:
  #' 1. Extract each column's intermediate consumption (rows)
  #' 2. Divide by that column's total output
  #'
  #' @param IO_table Data frame: full IO table
  #' @param intermediate_start_row First intermediate row
  #' @param intermediate_end_row Last intermediate row
  #' @param total_output_row Row with total supply
  #' @param nIndustries Total industries
  #' @param col_start Column index where industry data starts (after labels)
  #' @param col_end Column index where industry data ends
  #'
  #' @return nIndustries x nIndustries technical coefficient matrix
  #'
  
  A <- matrix(0, nrow = nIndustries, ncol = nIndustries)
  
  for (j in 1:nIndustries) {
    io_col <- col_start + j - 1  # Map industry j to IO table column
    
    if (io_col <= col_end) {
      coeff_result <- extract_biofuel_tech_coeff(
        IO_table,
        biofuel_col_index = io_col,
        intermediate_start_row = intermediate_start_row,
        intermediate_end_row = intermediate_end_row,
        total_output_row = total_output_row,
        nIndustries = nIndustries
      )
      
      A[, j] <- coeff_result$coeff_vector
    }
  }
  
  return(A)
}

#####################################################################
### FUNCTION: Update A matrix column for scenario
#####################################################################

update_A_column_scenario <- function(A_base,
                                     IO_table,
                                     biofuel_col_index_io,
                                     biofuel_col_index_A,
                                     intermediate_start_row = 2,
                                     intermediate_end_row = nIndustries + 1,
                                     total_output_row = nIndustries + 19,
                                     nIndustries = 89) {
  #'
  #' Update a single column of A matrix using actual IO data
  #' Useful for updating a biofuel sector's technical coefficients
  #'
  #' @param A_base Matrix: baseline A matrix to update
  #' @param IO_table Data frame: IO table (domestic or imports specific)
  #' @param biofuel_col_index_io Integer: column index in IO table
  #' @param biofuel_col_index_A Integer: column index in A matrix to update
  #' @param intermediate_start_row, intermediate_end_row, total_output_row Integers: IO row indices
  #' @param nIndustries Integer: number of industries
  #'
  #' @return Updated A matrix
  #'
  
  coeff_result <- extract_biofuel_tech_coeff(
    IO_table,
    biofuel_col_index = biofuel_col_index_io,
    intermediate_start_row = intermediate_start_row,
    intermediate_end_row = intermediate_end_row,
    total_output_row = total_output_row,
    nIndustries = nIndustries
  )
  
  # Replace column in A
  A_updated <- A_base
  A_updated[, biofuel_col_index_A] <- coeff_result$coeff_vector
  
  return(A_updated)
}

#####################################################################
### FUNCTION: Apply scenario with IO-based technical coefficients
#####################################################################

apply_scenario_with_IO_coefficients <- function(scenario_data,
                                                IO_domestic,
                                                IO_imports,
                                                baseline_final_demand,
                                                biofuel_sector_indices,
                                                L_matrix,
                                                nIndustries,
                                                intermediate_start_row = 2,
                                                intermediate_end_row = nIndustries + 1,
                                                total_output_row = nIndustries + 19) {
  #'
  #' Apply a scenario by:
  #' 1. Extracting technical coefficients from IO for each biofuel type
  #' 2. Building demand shock based on scenario values
  #' 3. Computing output effects via Leontief
  #'
  #' @param scenario_data List with biofuel_demands breakdown
  #' @param IO_domestic, IO_imports Data frames: IO tables
  #' @param baseline_final_demand Vector: baseline final demand
  #' @param biofuel_sector_indices List mapping fuel type to IO column
  #' @param L_matrix Leontief inverse
  #' @param nIndustries Total industries
  #'
  #' @return List with composition, shocks, and output effects
  #'
  
  # Initialize shock vector
  shock_vector <- rep(0, nIndustries)
  composition_table <- data.frame()
  
  # For each biofuel type in scenario
  for (fuel_type in names(scenario_data$biofuel_demands)) {
    
    if (is.na(biofuel_sector_indices[[fuel_type]])) {
      warning(paste("No sector index for", fuel_type, "- skipping"))
      next
    }
    
    sector_idx <- biofuel_sector_indices[[fuel_type]]
    demand_domestic <- scenario_data$biofuel_demands[[fuel_type]]$domestic
    demand_imports <- scenario_data$biofuel_demands[[fuel_type]]$imports
    demand_total <- scenario_data$biofuel_demands[[fuel_type]]$total
    
    # Add to shock vector (total demand shock for this sector)
    shock_vector[sector_idx] <- demand_total
    
    # Record composition
    composition_table <- rbind(composition_table, data.frame(
      fuel_type = fuel_type,
      domestic = demand_domestic,
      imports = demand_imports,
      total = demand_total,
      stringsAsFactors = FALSE
    ))
  }
  
  # Apply Leontief to get total output effects
  output_effects <- L_matrix %*% shock_vector
  
  # Detailed effects by biofuel type
  effects_by_type <- list()
  for (fuel_type in names(scenario_data$biofuel_demands)) {
    if (!is.na(biofuel_sector_indices[[fuel_type]])) {
      sector_idx <- biofuel_sector_indices[[fuel_type]]
      type_shock <- rep(0, nIndustries)
      type_shock[sector_idx] <- scenario_data$biofuel_demands[[fuel_type]]$total
      type_effects <- L_matrix %*% type_shock
      
      effects_by_type[[fuel_type]] <- list(
        shock = scenario_data$biofuel_demands[[fuel_type]]$total,
        total_output = sum(type_effects),
        multiplier = ifelse(type_shock[sector_idx] != 0, 
                           sum(type_effects) / type_shock[sector_idx], NA)
      )
    }
  }
  
  return(list(
    scenario_name = scenario_data$name,
    year = scenario_data$year,
    composition = composition_table,
    shock_vector = shock_vector,
    total_shock = sum(shock_vector),
    output_effects = output_effects,
    total_output_effect = sum(output_effects),
    multiplier_overall = ifelse(sum(shock_vector) != 0, 
                                 sum(output_effects) / sum(shock_vector), NA),
    effects_by_type = effects_by_type
  ))
}

#####################################################################
### INTEGRATION TEMPLATE
#####################################################################
#
# In your Model_code_adapted_including_imports.R:
#
# 1. Source this file and set sector indices:
#    source("biofuel_scenarios_IO_linkages.R")
#
#    BIOFUEL_SECTOR_INDICES <- list(
#      conv_biodiesel = 52,      # Column 52 in IO
#      conv_biogasoline = 53,
#      adv_biodiesel = 56,
#      adv_biogasoline = 57
#    )
#
# 2. Apply scenario using IO-extracted coefficients:
#    results_s1 <- apply_scenario_with_IO_coefficients(
#      scenario_1,
#      IO_domestic = IO_EU_domestic,
#      IO_imports = IO_EU_imports,
#      baseline_final_demand = final_use,
#      biofuel_sector_indices = BIOFUEL_SECTOR_INDICES,
#      L_matrix = L_total,
#      nIndustries = nIndustries
#    )
#
# 3. Or, update A matrix for specific scenario biofuel:
#    A_scenario <- A_total  # Start with baseline A
#    A_scenario <- update_A_column_scenario(
#      A_scenario,
#      IO_table = IO_EU_domestic,
#      biofuel_col_index_io = 56,  # Advanced biodiesel in IO
#      biofuel_col_index_A = 56,   # Position in A matrix
#      nIndustries = nIndustries
#    )
#    L_scenario <- solve(diag(nIndustries) - A_scenario)
#
# 4. Compare scenarios:
#    comparison <- rbind(
#      results_s1$composition,
#      results_s2$composition,
#      results_s3$composition
#    )
#    print(comparison)
#
# 5. In time loop, use scenario-specific output effects:
#    for (i in 2:nYears) {
#      # Apply scenario 1 demand shock
#      f_i[i, ] <- results_s1$shock_vector + GCF_i[i, ] + EX_i[i, ]
#      X_i[i, ] <- L_total %*% f_i[i, ]
#    }

# End of biofuel scenarios with IO linkages
