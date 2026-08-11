#####################################################################
### biofuel_scenarios_detailed.R
### Biofuel scenarios with detailed IO structure
### Accounts for different input requirements per biofuel type/substitute
### Data extracted from IO table showing sector-specific consumption patterns
#####################################################################

#####################################################################
### BIOFUEL SECTOR MAPPING FROM IO TABLE
#####################################################################
#
# The biofuel sectors include:
#
# CONVENTIONAL BIOFUELS (1st generation):
#   - Conventional biodiesel
#   - Conventional biogasoline  
#   - Conventional bio-kerosene
#   - Conventional bio-HFO (heavy fuel oil)
#
# ADVANCED BIOFUELS (2nd generation):
#   - Advanced biodiesel
#   - Advanced biogasoline
#   - Advanced bio-kerosene
#   - Advanced bio-HFO
#   - Advanced biogas
#
# RENEWABLE FUELS OF NON-BIOLOGICAL ORIGIN (RFNBOs):
#   - RFNBOs
#
# Each sector has DIFFERENT INPUT REQUIREMENTS from other industries

# Map biofuel sectors to  IO table indices
# Replace these with actual row/column indices from your IO classification
BIOFUEL_SECTORS <- list(
  conv_biodiesel = 23,           # Row index for conventional biodiesel
  conv_biogasoline = 25,         # Row index for conventional biogasoline
  conv_bio_kerosene = 27,        # Row index for conventional bio-kerosene
  conv_bio_hfo = 29,             # Row index for conventional bio-HFO
  adv_biodiesel = 24,            # Row index for advanced biodiesel
  adv_biogasoline = 26,          # Row index for advanced biogasoline
  adv_bio_kerosene = 28,         # Row index for advanced bio-kerosene
  adv_bio_hfo = 30,              # Row index for advanced bio-HFO
  RFNBOs = 31,
  adv_biogas = 46
)

#####################################################################
### SCENARIO DEFINITIONS WITH BIOFUEL TYPE BREAKDOWN
#####################################################################

# Scenario 1: Import-Focused

### here I already put in the right numbers, based on "Receiving sectors" excel file
scenario_1 <- list(
  name = "S1: Import-Focused",
  description = "Import-dependent biofuel strategy",
  
  # Total demand by biofuel type (millions EUR)
  # Distribution across fuel types 
  conv_biodiesel = list(
    domestic = 3018.19,
    imports = 19194.17,
    total = 22212.36
  ),
  conv_biogasoline = list(
    domestic = 567.06,
    imports = 4436.26,
    total = 5003.31
  ),
  conv_bio_kerosene = list(
    domestic = 1848,
    imports = 966.98,
    total = 2814.98
  ),
  conv_bio_hfo = list(
    domestic = 0.00,
    imports = 0.00,
    total = 0.00
  ),
  adv_biodiesel = list(
    domestic = 0.08,
    imports = 6.68,
    total = 6.76
  ),
  adv_biogasoline = list(
    domestic = 5.00,
    imports = 6.68,
    total = 11.68
  ),
  adv_bio_kerosene = list(
    domestic = 0.00,
    imports = 0.00,
    total = 0.00
  ),
  adv_bio_hfo = list(
    domestic = 900,
    imports = 0.00,
    total = 900.00
  ),
  
  total_domestic = 33330.27,
  total_imports = 44952.96,
  total_demand = 78283.23
)

# Scenario 2: Domestic Autarky
### here still need to put in right numbers
scenario_2 <- list(
  name = "S2: Domestic Autarky",
  description = "Domestic-focused biofuel strategy with advanced biofuel adoption",
  
  conv_biodiesel = list(
    domestic = 8.00,
    imports = 0.05,
    total = 8.05
  ),
  conv_biogasoline = list(
    domestic = 5.53,
    imports = 0.05,
    total = 5.58
  ),
  conv_bio_kerosene = list(
    domestic = 0.00,
    imports = 0.00,
    total = 0.00
  ),
  conv_bio_hfo = list(
    domestic = 0.00,
    imports = 0.00,
    total = 0.00
  ),
  adv_biodiesel = list(
    domestic = 10.00,
    imports = 0.26,
    total = 10.26
  ),
  adv_biogasoline = list(
    domestic = 7.93,
    imports = 0.26,
    total = 8.19
  ),
  adv_bio_kerosene = list(
    domestic = 0.00,
    imports = 0.00,
    total = 0.00
  ),
  adv_bio_hfo = list(
    domestic = 0.00,
    imports = 0.00,
    total = 0.00
  ),
  
  total_domestic = 31.46,
  total_imports = 0.62,
  total_demand = 31.07
)

# Scenario 3: Export-Boom
scenario_3 <- list(
  name = "S3: Export-Boom",
  description = "Export-oriented high-volume biofuel production",
  
  conv_biodiesel = list(
    domestic = 19.83,
    imports = 0.00,
    total = 19.83
  ),
  conv_biogasoline = list(
    domestic = 0.00,
    imports = 0.00,
    total = 0.00
  ),
  conv_bio_kerosene = list(
    domestic = 0.00,
    imports = 0.00,
    total = 0.00
  ),
  conv_bio_hfo = list(
    domestic = 0.00,
    imports = 0.00,
    total = 0.00
  ),
  adv_biodiesel = list(
    domestic = 88.42,
    imports = 34.97,
    total = 123.39
  ),
  adv_biogasoline = list(
    domestic = 0.00,
    imports = 34.96,
    total = 34.96
  ),
  adv_bio_kerosene = list(
    domestic = 0.00,
    imports = 0.00,
    total = 0.00
  ),
  adv_bio_hfo = list(
    domestic = 0.00,
    imports = 0.00,
    total = 0.00
  ),
  
  total_domestic = 108.25,
  total_imports = 69.93,
  total_demand = 38.07,
  exports = 69.98
)


##################################################################################################################
#### Version 2 - IVC - specific #####
##################################################################################################################

##### Scenario 1, 2030 #####

#####################################################################
### 1. IVC-PROFILE ALS RELATIVE TECHNOLOGIE-REZEPTE (a_ij)
### (Alle Werte als Anteile am Marktverkaufserlös X = 1.00 EUR)
#####################################################################

# --- IVC 1 Profile (Import-basiert, POME) ---
# Basis: X_IVC1 = 1,70 Mio. t * 1.750 €/t = 2,975 Mrd. EUR
ivc1_profile <- list(
  name = "IVC 1: Imported POME",
  a_dom = c(
    agriculture = 0.0000, forestry = 0.0000, paper = 0.0000, food_bev = 0.0000,
    # CAPEX-Anteile
    fab_metal = 0.002371, elec_equip = 0.000949, machinery = 0.021343,
    construction = 0.007114, architecture = 0.014229, computer_opt = 0.001423,
    # OPEX-Anteile
    electricity = 0.053400, chemicals = 0.038143, legal_head = 0.030514,
    repair_inst = 0.022886, land_transp = 0.007629
  ),
  a_imp = c(
    agriculture_imp = 0.297143 # 884 Mio. EUR / 2,975 Mrd. EUR
  )
)

# --- IVC 2 Profile (HVO Import) ---
# Basis: X_IVC2 = 0,28 Mio. t * 1.750 €/t = 490 Mio. EUR
ivc2_profile <- list(
  name = "IVC 2: HVO Import",
  a_dom = c(
    agriculture = 0.0000, forestry = 0.0000, paper = 0.0000, food_bev = 0.0000,
    # CAPEX-Anteile
    fab_metal = 0.007686, elec_equip = 0.003074, machinery = 0.069171,
    construction = 0.023057, architecture = 0.045992, computer_opt = 0.004611,
    # OPEX-Anteile
    electricity = 0.081000, chemicals = 0.057857, legal_head = 0.046286,
    repair_inst = 0.034714, land_transp = 0.011571
  ),
  a_imp = c(
    agriculture_imp = 0.350676 # 171,8 Mio. EUR / 490 Mio. EUR
  )
)

# --- IVC 13a Profile (Heimisch, Stroh/Lignozellulose) ---
# Basis: X_IVC13a = 60.905,7 t * 1.750 €/t = 106,58 Mio. EUR
ivc13a_profile <- list(
  name = "IVC 13a: Domestic Straw/Lignocellulose",
  a_dom = c(
    agriculture = 0.059728, forestry = 0.030374, paper = 0.024157, food_bev = 0.001674,
    # CAPEX-Anteile
    fab_metal = 0.006899, elec_equip = 0.000920, machinery = 0.020700,
    construction = 0.004600, architecture = 0.011500, computer_opt = 0.001380,
    # OPEX-Anteile
    electricity = 0.049886, chemicals = 0.083143, legal_head = 0.066514,
    repair_inst = 0.099771, land_transp = 0.033257
  ),
  a_imp = c(
    agriculture_imp = 0.000000 # Kein Feedstock-Import
  )
)

# --- IVC 13b Profile (Heimisch, Maritim) ---
# Basis: X_IVC13b = 40.603,8 t * 1.750 €/t = 71,06 Mio. EUR
ivc13b_profile <- list(
  name = "IVC 13b: Domestic Maritime Path",
  a_dom = c(
    agriculture = 0.224467, forestry = 0.072027, paper = 0.066639, food_bev = 0.005100,
    # CAPEX-Anteile
    fab_metal = 0.058714, elec_equip = 0.007828, machinery = 0.176140,
    construction = 0.039143, architecture = 0.097857, computer_opt = 0.011743,
    # OPEX-Anteile
    electricity = 0.072300, chemicals = 0.120500, legal_head = 0.096400,
    repair_inst = 0.144600, land_transp = 0.048200
  ),
  a_imp = c(
    agriculture_imp = 0.000000
  )
)


################################################################################################################################
##### VERSION 1 ###############
#################################################################################################################################

#####################################################################
### INPUT COEFFICIENTS BY BIOFUEL TYPE  - Scenario 1
#####################################################################


#####################################################################
### INPUT COEFFICIENTS: ADVANCED BIODIESEL (2030, Sc 1)
### Computed on Market Selling Price (Gross Output Value X)
#####################################################################

# 1. Bruttoproduktionswert (Total Market Value / Sales Revenue in EUR)
# 2.081.509,51 t * 1.750,00 €/t
X_total <- 3642641635.62 

# 2. Absoluter Zwischenverbrauch aus  Excel-Tabelle "Providing sectors" (in EUR)

# DOMESTIC INTERMEDIATE INPUTS (domestic feedstock inpust)

z_dom_abs <- c(                ## absolute domestci intermediate transactions
  agriculture  = 22317472.46,  # Products of agriculture etc. 
  forestry     = 8355518.70,   # Products of forestry, logging etc. 
  paper        = 7310016.07,   # Paper and paper products
  food_bev     = 540799.33,    # Food, beverages and tobacco products
  sewerage     = 0.00          # Sewerage services
)

# IMPORTED INTERMEDIATE INPUTS (Imported inputs - Feedstock only)
z_imp_abs <- c(
  agriculture_imp = 1055831366.35 # Imported Feedstock (POME) 
  

# CAPEX INTERMEDIATE INPUTS (domestic only)
z_capex_abs <- c(
  fab_metal    = 22347404.41,  # Fabricated metal products
  elec_equip   = 5865253.32,   # Electrical equipment
  machinery    = 131968213.24, # Machinery and equipment n.e.c.
  construction = 40147269.61,  # Constructions and construction works
  architecture = 84136674.02,  # Architectural and engineering
  computer_opt = 8797880.88    # Computer, electronic and optical
)

# OPEX INTERMEDIATE INPUTS (domestic only
z_opex_abs <- c(
  electricity  = 209009464.03, # Electricity, gas, steam and air conditioning
  chemicals    = 159249106.72, # Chemicals and chemical products
  legal_head   = 127399285.37, # Legal and accounting services; head office
  repair_inst  = 106003928.06, # Repair and installation services
  land_transp  = 35334642.69   # Land transport services
)

#####################################################################
### 3. Berechnung der IOT-Koeffizienten (a_ij = z_ij / X_total)
#####################################################################

# Inländische Koeffizienten (A_dom Spalte für Adv. Biodiesel)
a_dom_adv_biodiesel <- c(z_dom_abs, z_capex_abs, z_opex_abs) / X_total

# Importierte Koeffizienten (A_imp Spalte für Adv. Biodiesel)
a_imp_adv_biodiesel <- z_imp_abs / X_total

# Gesamte Vorleistungsquote (Intermediate Consumption Share)
total_intermediate_share <- sum(a_dom_adv_biodiesel) + sum(a_imp_adv_biodiesel)

# Wertschöpfungsquote (Direct Value Added Share v_j)
gva_share <- 1 - total_intermediate_share

#####################################################################
### 4. Kontrollausgabe der Ergebnisse
#####################################################################

## check what this does
  
cat("--- ERGEBNISSE FÜR ADVANCED BIODIESEL --- \n")
cat("Bruttoproduktionswert (Marktwert):", round(X_total / 1e6, 2), "Mio. EUR\n")
cat("Gesamte Vorleistungsquote (sum a_ij):", round(total_intermediate_share * 100, 2), "%\n")
cat("  - davon inländische Vorleistungen:", round(sum(a_dom_adv_biodiesel) * 100, 2), "%\n")
cat("  - davon importierte Vorleistungen:", round(sum(a_imp_adv_biodiesel) * 100, 2), "%\n")
cat("Integrierte Wertschöpfungsquote (v_j):", round(gva_share * 100, 2), "%\n")


  

###########################################################################################################################
### DELETE ###
###########################################################################################################################

# Each biofuel type consumes different inputs based on:
#   1. Feedstock types; 
#   2. CAPEX components; 
#   3. OPEX components; 
####   4. and add later: Co-products that then later sell  
#
# From the IO table, we can extract consumption patterns:
# This matrix shows intermediate input requirements per unit of output

biofuel_input_coefficients <- list(
  
  # Conventional biodiesel typical inputs:
  conv_biodiesel = c(
    agriculture = 0.35,              # Oilseed production
    food_beverages = 0.05,           # Processing oils
    chemicals = 0.12,                # Catalysts, methanol
    coke_refined_petroleum = 0.08,   # Energy, heat
    other_inputs = 0.40
  ),
  
  # Conventional biogasoline typical inputs:
  conv_biogasoline = c(
    food_beverages = 0.40,           # Sugar/starch feedstock
    chemicals = 0.15,                # Enzymes, catalysts
    coke_refined_petroleum = 0.10,   # Energy
    water_treatment = 0.05,          # Processing water
    other_inputs = 0.30
  ),
  
  # Advanced biodiesel typical inputs:
  adv_biodiesel = c(
    agriculture = 0.10,              # Waste/residues (lower than conv)
    food_beverages = 0.15,           # Waste streams
    chemicals = 0.20,                # More intensive processing
    coke_refined_petroleum = 0.15,   # Energy intensive
    waste_management = 0.10,         # Feedstock handling
    other_inputs = 0.30
  ),
  
  # Advanced biogasoline typical inputs:
  adv_biogasoline = c(
    agriculture = 0.08,              # Residue-based
    waste_management = 0.25,         # Higher waste utilization
    chemicals = 0.22,                # Conversion chemicals
    coke_refined_petroleum = 0.20,   # Energy intensive
    other_inputs = 0.25
  )
  
  # Add other fuel types as needed...
)

#####################################################################
### FUNCTION: Build scenario-specific demand vector
#####################################################################

build_scenario_demand <- function(scenario_data, 
                                   baseline_final_demand,
                                   biofuel_sector_indices,
                                   nIndustries) {
  #'
  #' Build a new final demand vector incorporating scenario-specific
  #' biofuel composition and type breakdown
  #'
  #' @param scenario_data List with biofuel type breakdown (conv/adv, fuel type)
  #' @param baseline_final_demand Vector: baseline final demand (nIndustries)
  #' @param biofuel_sector_indices List with named indices for each biofuel type
  #' @param nIndustries Integer: total industries
  #'
  #' @return List with:
  #'   - new_demand: final demand vector for scenario
  #'   - shock_by_sector: shock per biofuel sector type
  #'   - composition: breakdown of demand across fuel types
  #'
  
  # Start with baseline
  new_demand <- baseline_final_demand
  shock_by_sector <- list()
  composition <- data.frame()
  
  # List of biofuel types to iterate over
  biofuel_types <- c(
    "conv_biodiesel", "conv_biogasoline", "conv_bio_kerosene", "conv_bio_hfo",
    "adv_biodiesel", "adv_biogasoline", "adv_bio_kerosene", "adv_bio_hfo"
  )
  
  # For each biofuel type, calculate shock
  for (fuel_type in biofuel_types) {
    
    if (!is.null(scenario_data[[fuel_type]]) && !is.na(biofuel_sector_indices[[fuel_type]])) {
      
      sector_idx <- biofuel_sector_indices[[fuel_type]]
      scenario_total <- scenario_data[[fuel_type]]$total
      baseline_value <- baseline_final_demand[sector_idx]
      shock <- scenario_total - baseline_value
      
      # Apply shock to this sector
      new_demand[sector_idx] <- scenario_total
      shock_by_sector[[fuel_type]] <- shock
      
      # Record composition
      composition <- rbind(composition, data.frame(
        fuel_type = fuel_type,
        domestic = scenario_data[[fuel_type]]$domestic,
        imports = scenario_data[[fuel_type]]$imports,
        total = scenario_total,
        baseline = baseline_value,
        shock = shock
      ))
    }
  }
  
  return(list(
    scenario_name = scenario_data$name,
    new_demand = new_demand,
    shock_by_sector = shock_by_sector,
    composition = composition,
    total_shock = sum(unlist(shock_by_sector))
  ))
}

#####################################################################
### FUNCTION: Apply scenario with input-output linkages
#####################################################################

apply_biofuel_scenario_with_linkages <- function(scenario_data,
                                                  baseline_final_demand,
                                                  biofuel_sector_indices,
                                                  L_matrix,
                                                  nIndustries,
                                                  input_coefficients = NULL) {
  #'
  #' Apply biofuel scenario accounting for sector-specific input requirements
  #'
  #' Different biofuel types (conventional vs advanced, diesel vs gasoline)
  #' consume different mixes of inputs. This function:
  #'   1. Determines output targets per biofuel type
  #'   2. Calculates intermediate input requirements based on type
  #'   3. Propagates demand through supply chains via Leontief
  #'
  #' @param scenario_data Scenario with biofuel type breakdown
  #' @param baseline_final_demand Baseline final demand vector
  #' @param biofuel_sector_indices Named list of sector indices
  #' @param L_matrix Leontief inverse
  #' @param nIndustries Total industries
  #' @param input_coefficients Optional: custom input-output matrix by type
  #'
  #' @return List with output effects decomposed by biofuel type
  #'
  
  # Build scenario demand vector
  scenario_demand <- build_scenario_demand(scenario_data, 
                                            baseline_final_demand,
                                            biofuel_sector_indices,
                                            nIndustries)
  
  # Total shock vector
  shock_vector <- scenario_demand$new_demand - baseline_final_demand
  
  # Apply Leontief to get total economy-wide effects
  output_effects <- L_matrix %*% shock_vector
  
  # Detailed breakdown by biofuel type
  effects_by_type <- list()
  
  for (fuel_type in names(scenario_demand$shock_by_sector)) {
    shock_value <- scenario_demand$shock_by_sector[[fuel_type]]
    
    # Get sector index
    sector_idx <- biofuel_sector_indices[[fuel_type]]
    if (!is.na(sector_idx) && !is.null(shock_value)) {
      
      # Create shock vector for this type alone
      type_shock <- rep(0, nIndustries)
      type_shock[sector_idx] <- shock_value
      
      # Apply Leontief
      type_effects <- L_matrix %*% type_shock
      
      effects_by_type[[fuel_type]] <- list(
        shock = shock_value,
        direct_output = shock_value,
        total_output = sum(type_effects),
        multiplier = ifelse(shock_value != 0, sum(type_effects) / shock_value, NA),
        effects_vector = type_effects
      )
    }
  }
  
  return(list(
    scenario_name = scenario_data$name,
    composition = scenario_demand$composition,
    total_shock = scenario_demand$total_shock,
    shocked_demand = scenario_demand$new_demand,
    output_effects_total = output_effects,
    total_output_effect = sum(output_effects),
    multiplier_overall = ifelse(scenario_demand$total_shock != 0, 
                                 sum(output_effects) / scenario_demand$total_shock, NA),
    effects_by_biofuel_type = effects_by_type
  ))
}

#####################################################################
### FUNCTION: Compare scenarios with type-level detail
#####################################################################

compare_scenarios_detailed <- function(scenario_results_list) {
  #'
  #' Create detailed comparison across scenarios, showing:
  #'   - Total demand and composition by fuel type
  #'   - Output effects and multipliers per type
  #'   - Import vs domestic intensity
  #'
  
  # Aggregate comparison
  summary_comparison <- data.frame(
    Scenario = sapply(scenario_results_list, function(x) x$scenario_name),
    Total_Shock = sapply(scenario_results_list, function(x) x$total_shock),
    Total_Output_Effect = sapply(scenario_results_list, function(x) x$total_output_effect),
    Overall_Multiplier = sapply(scenario_results_list, function(x) x$multiplier_overall)
  )
  
  # Detailed breakdown by fuel type (longer format for comparison)
  detailed_comparison <- data.frame()
  
  for (i in seq_along(scenario_results_list)) {
    result <- scenario_results_list[[i]]
    comp_df <- result$composition
    comp_df$scenario <- result$scenario_name
    detailed_comparison <- rbind(detailed_comparison, comp_df)
  }
  
  return(list(
    summary = summary_comparison,
    detailed_by_type = detailed_comparison
  ))
}

#####################################################################
### INTEGRATION TEMPLATE
#####################################################################
#
# In your Model_code_adapted_including_imports.R:
#
# 1. Source this file and identify sector indices:
#    source("biofuel_scenarios_detailed.R")
#    
#    # Map biofuel sectors from your IO table
#    BIOFUEL_SECTOR_INDICES <- list(
#      conv_biodiesel = 52,
#      conv_biogasoline = 53,
#      conv_bio_kerosene = 54,
#      conv_bio_hfo = 55,
#      adv_biodiesel = 56,
#      adv_biogasoline = 57,
#      adv_bio_kerosene = 58,
#      adv_bio_hfo = 59
#    )
#
# 2. Apply scenarios with input linkages:
#    results_s1 <- apply_biofuel_scenario_with_linkages(
#      scenario_1,
#      baseline_final_demand = final_use,
#      biofuel_sector_indices = BIOFUEL_SECTOR_INDICES,
#      L_matrix = L_total,
#      nIndustries = nIndustries
#    )
#
#    results_s2 <- apply_biofuel_scenario_with_linkages(scenario_2, ...)
#    results_s3 <- apply_biofuel_scenario_with_linkages(scenario_3, ...)
#
# 3. Compare across scenarios:
#    comparison <- compare_scenarios_detailed(
#      list(results_s1, results_s2, results_s3)
#    )
#    print(comparison$summary)
#    print(comparison$detailed_by_type)
#
# 4. In time loop, use shocked demand:
#    for (i in 2:nYears) {
#      # Use scenario 1 demand (or switch scenarios for comparison)
#      f_i[i, ] <- results_s1$shocked_demand + GCF_i[i, ] + EX_i[i, ]
#      
#      # Supply side response via Leontief
#      X_i[i, ] <- L_total %*% f_i[i, ]
#      
#      # ... rest of model
#    }
#
# 5. Extract effects by biofuel type:
#    for (fuel_type in names(results_s1$effects_by_biofuel_type)) {
#      effects <- results_s1$effects_by_biofuel_type[[fuel_type]]
#      cat(fuel_type, ": Multiplier =", effects$multiplier, "\n")
#    }

# End of detailed biofuel scenarios file
