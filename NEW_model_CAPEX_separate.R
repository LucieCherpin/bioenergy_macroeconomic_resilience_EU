#####################################################################
### STANDALONE MODEL: CAPEX SEPARATED FROM INTERMEDIATE CONSUMPTION
#####################################################################
#
# Biofuel-sector intermediate-input coefficients contain only:
#   - purchased feedstock inputs; and
#   - recurrent OPEX inputs.
#
# CAPEX assumptions and distributions remain documented in the
# technology/scenario data, but CAPEX does not enter intermediate
# consumption in this version. No alternative CAPEX investment/GFCF
# treatment yet.
#
#####################################################################

#####################################################################
###STEP 1: Prepare Working Space and Data
#####################################################################

#Clear working environment
rm(list=ls()) 

#Install packages
library(readxl)
library(dplyr)

#Clear plots
if(!is.null(dev.list())) dev.off()

#Clear console
cat("\014")

#Prevent scientific notation
options(scipen = 999)

##########################################
###Import Data
##########################################

#Input-Output Table EU27 (Eurostat)

IO_EU_domestic <- read_xlsx("domestic_iot.xlsx", sheet= "Sheet_1", range = "A2:CP104", col_names = T)
IO_EU_domestic <- as.data.frame(IO_EU_domestic)

IO_EU_imports <- read_xlsx("imports_iot.xlsx", sheet= "Sheet_1", range = "A2:CP104", col_names = T)
IO_EU_imports <- as.data.frame(IO_EU_imports)

#####################################################################
### STEP 2: Construct 2023 Input-Output Baseline
#####################################################################

# Number of product/industry sectors in the Eurostat IO table
nIndustries <- 73

# Imported final consumption expenditure by product.
# Used as the 2023 anchor for finished-biofuel imports.
t_f_e_imp <- as.numeric(
  unlist(
    IO_EU_imports[
      2:(nIndustries + 1),
      nIndustries + 3
    ]
  )
)

# Observed 2023 domestic output by sector (total supply at basic prices)
q_s_dom <- as.numeric(
  unlist(
    IO_EU_domestic[
      nIndustries + 19,
      3:(nIndustries + 2)
    ]
  )
)

# Safe denominator used only for constructing technical coefficients.
# Zero observed output remains zero in the actual output vector.
q_s_dom_safe <- q_s_dom
q_s_dom_safe[q_s_dom_safe == 0] <- 1e-6

# Observed domestic government final consumption by product.
# Kept fixed at 2023 levels in the current no-autonomous-growth setup.
g_dom <- as.numeric(
  unlist(
    IO_EU_domestic[
      2:(nIndustries + 1),
      nIndustries + 4
    ]
  )
)

# Observed household and NPISH final consumption by product.
hh_cons_dom <- as.numeric(
  unlist(
    IO_EU_domestic[
      2:(nIndustries + 1),
      nIndustries + 5
    ]
  )
)

npish_cons_dom <- as.numeric(
  unlist(
    IO_EU_domestic[
      2:(nIndustries + 1),
      nIndustries + 6
    ]
  )
)

consumption_dom <-
  hh_cons_dom +
  npish_cons_dom

##########################################
### Intermediate transaction matrices and technical coefficients
##########################################

# Goods x goods matrix Z (Domestic)
Z_dom <- matrix(
  as.numeric(as.character(unlist(IO_EU_domestic[2:(nIndustries + 1), 3:(nIndustries + 2)]))),
  nrow = nIndustries, ncol = nIndustries
)
Z_dom[is.na(Z_dom)] <- 0
# Goods x goods matrix Z (Imports)
Z_imp <- matrix(
  as.numeric(as.character(unlist(IO_EU_imports[2:(nIndustries + 1), 3:(nIndustries + 2)]))),
  nrow = nIndustries, ncol = nIndustries
)
Z_imp[is.na(Z_imp)] <- 0

# ===================================================================
# sector-specific taxes less subsidies on product
# ===================================================================

# D21X31 - Taxes less subsidies on products by industry
tax_products_2023 <- as.numeric(
  unlist(
    IO_EU_domestic[
      nIndustries + 8,
      3:(nIndustries + 2)
    ]
  )
)

# Intermediate inputs represented by the domestic + imported Z matrices
intermediate_inputs_2023 <-
  colSums(Z_dom) +
  colSums(Z_imp)


# Sector-specific effective tax/subsidy rate in 2023
# This rate will be kept constant over time.
p2_tax_rate_2023 <- numeric(nIndustries)

nonzero_intermediate <-
  abs(intermediate_inputs_2023) > 1e-12

p2_tax_rate_2023[nonzero_intermediate] <-
  tax_products_2023[nonzero_intermediate] /
  intermediate_inputs_2023[nonzero_intermediate]


# Check for sectors in which intermediate inputs are zero
# but D21X31 is not zero
if (
  any(
    !nonzero_intermediate &
      abs(tax_products_2023) > 1e-8
  )
) {
  warning(
    "At least one sector has zero intermediate inputs but non-zero D21X31."
  )
}

# Technical coefficients matrices Domestic
## use the q_s_dom_safe vector here
A_dom <- as.matrix(sweep(Z_dom, 2, q_s_dom_safe, FUN = '/'))  # Domestic technical coefficients    

na_pos <- which(is.na(A_dom), arr.ind = TRUE)
if (nrow(na_pos) > 0) {
  message("Replacing ", nrow(na_pos), " NA(s) in A_dom with 0.")
  print(head(na_pos, 20))
}
A_dom[is.na(A_dom)] <- 0

A_imp <- as.matrix(sweep(Z_imp, 2, q_s_dom_safe, FUN = '/'))  # Import technical coefficients
na_pos <- which(is.na(A_imp), arr.ind = TRUE)
if (nrow(na_pos) > 0) {
  message("Replacing ", nrow(na_pos), " NA(s) in A_imp with 0.")
  print(head(na_pos, 20))
}
A_imp[is.na(A_imp)] <- 0


############################################################
# 1. BASIC SETTINGS
############################################################

nIndustries <- 73

BIOFUEL_SECTORS <- c(
  conv_biodiesel    = 12,
  adv_biodiesel     = 13,
  conv_biogasoline  = 14,
  adv_biogasoline   = 15,
  conv_bio_kerosene = 16,
  adv_bio_kerosene  = 17,
  adv_bio_hfo       = 18,
  RFNBOs            = 19,
  adv_biogas        = 33
)

FUEL_TECH_GROUP <- c(

  conv_biodiesel =
    "conventional",

  conv_biogasoline =
    "conventional",

  conv_bio_kerosene =
    "conventional",

  adv_biodiesel =
    "advanced",

  adv_biogasoline =
    "advanced",

  adv_bio_kerosene =
    "advanced",

  adv_bio_hfo =
    "advanced",

  RFNBOs =
    "advanced",

  adv_biogas =
    "advanced"
)


INPUT_SECTORS <- c(
  agriculture      = 1,
  forestry         = 2,
  food_bev         = 5,
  paper            = 8,
  chemicals        = 11,

  adv_biodiesel    = 13,
  adv_biogasoline  = 15,
  adv_bio_hfo      = 18,
  adv_biogas       = 33,

  fab_metal        = 24,
  computer_el      = 25,
  elec_equip       = 26,
  machinery        = 27,
  repair_inst      = 31,

  electricity      = 32,
  sewerage         = 35,
  construction     = 36,
  land_transp      = 40,

  legal_acc        = 54,
  architecture     = 55
)

#### make all sectors also exist as XY_import sector

DOM_INPUT_NAMES <- names(INPUT_SECTORS)

IMP_INPUT_NAMES <-
  paste0(
    DOM_INPUT_NAMES,
    "_imp"
  )

BIO <-
  unname(BIOFUEL_SECTORS)

NONBIO <-
  setdiff(
    seq_len(nIndustries),
    BIO
  )

sector_names <-
  as.character(
    IO_EU_domestic[[1]][
      2:(nIndustries + 1)
    ]
  )

# ================================================================
# DOMESTIC / IMPORT SOURCING OF CAPEX AND OPEX
# ================================================================

CAPEX_IMPORT_SHARES <- list(

  conventional = c(
    machinery    = 0.10,
    fab_metal    = 0.05,
    computer_el  = 0.40,
    elec_equip   = 0.00,
    construction = 0.00,
    architecture = 0.00
  ),

  advanced = c(
    machinery    = 0.40,
    fab_metal    = 0.50,
    computer_el  = 0.70,
    elec_equip   = 0.10,
    construction = 0.00,
    architecture = 0.00
  )
)


OPEX_IMPORT_SHARES <- list(

  conventional = c(
    chemicals    = 0.10,
    electricity  = 0.00,
    legal_acc    = 0.00,
    repair_inst  = 0.00,
    land_transp  = 0.00,
    sewerage     = 0.00
  ),

  advanced = c(
    chemicals    = 0.60,
    electricity  = 0.00,
    legal_acc    = 0.00,
    repair_inst  = 0.00,
    land_transp  = 0.00,
    sewerage     = 0.00
  )
)





############################################################
# 2. TECHNOLOGY LIBRARY
############################################################

IVC_TECH_LIBRARY <- list(
  IVC1 = list(
    market_price = 1450,
    dist_capex   = c(fab_metal = 0.05, elec_equip = 0.02, machinery = 0.45,
                     construction = 0.15, architecture = 0.30, computer_el = 0.03),
    dist_opex    = c(electricity = 0.35, chemicals = 0.25, legal_acc = 0.20,
                     repair_inst = 0.15, land_transp = 0.05)
  ),
  
  IVC2_HVO = list(
    market_price = 1750,
    dist_capex   = c(fab_metal = 0.05, elec_equip = 0.02, machinery = 0.45,
                     construction = 0.15, architecture = 0.30, computer_el = 0.03),
    dist_opex    = c(electricity = 0.35, chemicals = 0.25, legal_acc = 0.20,
                     repair_inst = 0.15, land_transp = 0.05)
  ),
  
  IVC2_HEFA = list(
    market_price = 2380,
    dist_capex   = c(fab_metal = 0.05, elec_equip = 0.02, machinery = 0.45,
                     construction = 0.15, architecture = 0.30, computer_el = 0.03),
    dist_opex    = c(electricity = 0.35, chemicals = 0.25, legal_acc = 0.20,
                     repair_inst = 0.15, land_transp = 0.05)
  ),
  
  IVC5 = list(
    market_price = 1000,
    dist_capex   = c(fab_metal = 0.00, elec_equip = 0.02, machinery = 0.50,
                     construction = 0.15, architecture = 0.30, computer_el = 0.03),
    dist_opex    = c(electricity = 0.20, chemicals = 0.50, legal_acc = 0.15,
                     repair_inst = 0.10, land_transp = 0.05)
  ),
  IVC6 = list(
    market_price = 2380,
    dist_capex   = c(fab_metal    = 0.00, elec_equip   = 0.02, machinery    = 0.50, construction = 0.15, architecture = 0.30, computer_el  = 0.03),
    dist_opex    = c(electricity = 0.20, chemicals   = 0.50, legal_acc   = 0.15, repair_inst = 0.10, land_transp = 0.05)
  ),
  
  IVC7 = list(
    market_price = 1275,
    dist_capex   = c( fab_metal    = 0.05, elec_equip   = 0.02, machinery    = 0.40, construction = 0.30, architecture = 0.20, computer_el  = 0.03),
    dist_opex    = c(electricity = 0.20, chemicals   = 0.50, legal_acc   = 0.15, repair_inst = 0.10, land_transp = 0.05)
  ),
  
  IVC8a = list(
    market_price = 935,
    dist_capex   = c(fab_metal = 0.00, elec_equip = 0.02, machinery = 0.55,
                     construction = 0.10, architecture = 0.30, computer_el = 0.03),
    dist_opex    = c(electricity = 0.25, chemicals = 0.15, legal_acc = 0.30,
                     repair_inst = 0.30, land_transp = 0.00)
  ),
  
  IVC8b = list(
    market_price = 935,
    dist_capex   = c(fab_metal = 0.00, elec_equip = 0.02, machinery = 0.55,
                     construction = 0.10, architecture = 0.30, computer_el = 0.03),
    dist_opex    = c(electricity = 0.25, chemicals = 0.15, legal_acc = 0.30,
                     repair_inst = 0.30, land_transp = 0.00)
  ),
  
  IVC8c = list(
  market_price = 935,
  dist_capex = c(fab_metal = 0.20, elec_equip = 0.07, machinery = 0.40, construction = 0.15, architecture = 0.15, computer_el = 0.03),
  dist_opex = c(electricity = 0.35, chemicals = 0.15, legal_acc = 0.12, repair_inst = 0.30, land_transp = 0.08)
),
  
  
  IVC9a = list(
    market_price = 1275,
    dist_capex   = c(fab_metal = 0.00, elec_equip = 0.02, machinery = 0.55,
                     construction = 0.10, architecture = 0.30, computer_el = 0.03),
    dist_opex    = c(electricity = 0.25, chemicals = 0.15, legal_acc = 0.30,
                     repair_inst = 0.30, land_transp = 0.00)
  ),
  
 IVC9b = list(
  market_price = 1275,
  dist_capex = c(fab_metal = 0.20, elec_equip = 0.07, machinery = 0.40, construction = 0.15, architecture = 0.15, computer_el = 0.03),
  dist_opex = c(electricity = 0.35, chemicals = 0.15, legal_acc = 0.12, repair_inst = 0.30, land_transp = 0.08)
),
  
  IVC11a_road = list(
    market_price = 1750,
    dist_capex   = c(fab_metal = 0.00, elec_equip = 0.02, machinery = 0.55,
                     construction = 0.10, architecture = 0.30, computer_el = 0.03),
    dist_opex    = c(electricity = 0.25, chemicals = 0.15, legal_acc = 0.30,
                     repair_inst = 0.30, land_transp = 0.00)
  ),
  
  IVC11a_SAF = list(
    market_price = 2380,
    dist_capex   = c(fab_metal = 0.00, elec_equip = 0.02, machinery = 0.55,
                     construction = 0.10, architecture = 0.30, computer_el = 0.03),
    dist_opex    = c(electricity = 0.25, chemicals = 0.15, legal_acc = 0.30,
                     repair_inst = 0.30, land_transp = 0.00)
  ),
  
  IVC12 = list(
    market_price = 1000,
    dist_capex   = c(fab_metal = 0.00, elec_equip = 0.02, machinery = 0.50,
                     construction = 0.15, architecture = 0.30, computer_el = 0.03),
    dist_opex    = c(electricity = 0.20, chemicals = 0.50, legal_acc = 0.15,
                     repair_inst = 0.10, land_transp = 0.05)
  ),
  
  IVC13a = list(
    market_price = 1750,
    dist_capex   = c(fab_metal = 0.15, elec_equip = 0.02, machinery = 0.45,
                     construction = 0.10, architecture = 0.25, computer_el = 0.03),
    dist_opex    = c(electricity = 0.15, chemicals = 0.25, legal_acc = 0.20,
                     repair_inst = 0.30, land_transp = 0.10)
  ),
  
  IVC13b_road = list(
    market_price = 1150,
    dist_capex   = c(fab_metal = 0.15, elec_equip = 0.02, machinery = 0.45,
                     construction = 0.10, architecture = 0.25, computer_el = 0.03),
    dist_opex    = c(electricity = 0.15, chemicals = 0.25, legal_acc = 0.20,
                     repair_inst = 0.30, land_transp = 0.10)
  ),
  
  IVC13b_mar = list(
    market_price = 1150,
    dist_capex   = c(fab_metal = 0.15, elec_equip = 0.02, machinery = 0.45,
                     construction = 0.10, architecture = 0.25, computer_el = 0.03),
    dist_opex    = c(electricity = 0.15, chemicals = 0.25, legal_acc = 0.20,
                     repair_inst = 0.30, land_transp = 0.10)
  ),
  
  IVC13b_SAF = list(
  market_price = 2380,
  dist_capex = c(fab_metal = 0.15, elec_equip = 0.02, machinery = 0.45, construction = 0.10, architecture = 0.25,  computer_el = 0.03),
  dist_opex = c(electricity = 0.15, chemicals = 0.25, legal_acc = 0.20, repair_inst = 0.30, land_transp = 0.10)
),
  
  # ==========================================================
  # CONVENTIONAL BIOFUEL TECHNOLOGIES
  # Here, more stays the same, hence TECH LIBRARY is longer: production costs stay constant, as well as feed/ CAPEX/ OPEX shares
  # ==========================================================
  

IVC_T_FF = list(
  prod_cost = 629,
  market_price = 1165.6261363636365,
  alpha = c(feed = 0.75, capex = 0.065, opex = 0.185),
  dist_capex = c(fab_metal = 0.07, machinery = 0.45, construction = 0.15, architecture = 0.30, computer_el = 0.03),
  dist_opex = c(electricity = 0.20, chemicals = 0.30, legal_acc = 0.25, repair_inst = 0.15, land_transp = 0.10, sewerage = 0.0)
),

IVC_HT_FF = list(
  prod_cost = 665.824761904762,
  market_price = 1165.6261363636365,
  alpha = c(feed = 0.67, capex = 0.13, opex = 0.20),
  dist_capex = c(fab_metal = 0.07, machinery = 0.55, construction = 0.10, architecture = 0.25, computer_el = 0.03),
  dist_opex = c(electricity = 0.20, chemicals = 0.40, legal_acc = 0.20, repair_inst = 0.15, land_transp = 0.05)
),

IVC_T_CC = list(
  prod_cost = 1598.13,
  market_price =  1165.6261363636365,
  alpha = c(feed = 0.74, capex = 0.07, opex = 0.19),
  dist_capex = c(fab_metal = 0.07, elec_equip = 0.00, machinery = 0.45, construction = 0.15, architecture = 0.30, computer_el = 0.03),
  dist_opex = c(electricity = 0.20, chemicals = 0.30, legal_acc = 0.25, repair_inst = 0.15, land_transp = 0.10)
), # not present in Scenario 2 table -> current values retained

IVC_HT_CC = list(
  prod_cost = 1218,
  market_price = 1165.6261363636365,
  alpha = c(feed = 0.67, capex = 0.13, opex = 0.20),
  dist_capex = c(fab_metal = 0.07, machinery = 0.55, construction = 0.10, architecture = 0.25, computer_el = 0.03),
  dist_opex = c(electricity = 0.20, chemicals = 0.40, legal_acc = 0.20, repair_inst = 0.15, land_transp = 0.05)
),


## revise: should be differnet price and costs taht HT_CC (road)
IVC_HT_CC_SAF = list(
  prod_cost = 1218,
  market_price = 1165.6261363636365,
  alpha = c(feed = 0.67, capex = 0.13, opex = 0.20),
  dist_capex = c(fab_metal = 0.07, machinery = 0.55, construction = 0.10, architecture = 0.25, computer_el = 0.03),
  dist_opex = c(electricity = 0.20, chemicals = 0.40, legal_acc = 0.20, repair_inst = 0.15, land_transp = 0.05)
),

IVC_T_lipids = list(
  prod_cost = 1253,
  market_price = 1165.6261363636365,
  alpha = c(feed = 0.74, capex = 0.04, opex = 0.22),
  dist_capex = c(fab_metal = 0.07, elec_equip = 0.00, machinery = 0.45, construction = 0.15, architecture = 0.30, computer_el = 0.03),
  dist_opex = c(electricity = 0.20, chemicals = 0.30, legal_acc = 0.25, repair_inst = 0.15, land_transp = 0.10)
),

IVC_HT_lipids_road = list(
  prod_cost = 1598.13,
  market_price =  1165.6261363636365,
  alpha = c(feed = 0.67, capex = 0.13, opex = 0.20),
  dist_capex = c(fab_metal = 0.07, elec_equip = 0.00, machinery = 0.55, construction = 0.10, architecture = 0.25, computer_el = 0.03),
  dist_opex = c(electricity = 0.20, chemicals = 0.40, legal_acc = 0.20, repair_inst = 0.15, land_transp = 0.05)
), 

IVC_HT_lipids_SAF = list(
  prod_cost = 2903,
  market_price = 2100,
  alpha = c(feed = 0.68, capex = 0.13, opex = 0.19),
  dist_capex = c(fab_metal = 0.07, elec_equip = 0.00, machinery = 0.55, construction = 0.10, architecture = 0.25, computer_el = 0.03),
  dist_opex = c(electricity = 0.20, chemicals = 0.40, legal_acc = 0.20, repair_inst = 0.15, land_transp = 0.05)
),

IVC_EF_FF = list(
  prod_cost = 903.57,
  market_price = 1018.96958174905,
  alpha = c(feed = 0.70, capex = 0.10, opex = 0.20),
  dist_capex = c(fab_metal = 0.07, elec_equip = 0.00, machinery = 0.50, construction = 0.15, architecture = 0.25, computer_el = 0.03),
  dist_opex = c(electricity = 0.40, chemicals = 0.15, legal_acc = 0.20, repair_inst = 0.15, land_transp = 0.10)
) 
)

############################################################
# 3. HELPER FUNCTIONS
############################################################

get_v <- function(vec, k) {
  if (k %in% names(vec)) unname(vec[k]) else 0
}
    ############################################################
    # Distribute the CAPEX/ OPEX shares onto domestic vs imported
    ############################################################


apply_import_shares <- function(
    distribution,
    import_shares
) {

  if (is.null(distribution)) {
    return(numeric(0))
  }

  out <- numeric(0)

  for (sector in names(distribution)) {

    total_share <-
      distribution[[sector]]

    import_share <-
      if (sector %in% names(import_shares)) {
        import_shares[[sector]]
      } else {
        0
      }

    if (
      import_share < 0 ||
      import_share > 1
    ) {
      stop(
        paste(
          "Invalid import share for",
          sector
        )
      )
    }

    domestic_value <-
      total_share *
      (1 - import_share)

    imported_value <-
      total_share *
      import_share

    if (domestic_value != 0) {
      out[sector] <-
        domestic_value
    }

    if (imported_value != 0) {
      out[
        paste0(
          sector,
          "_imp"
        )
      ] <-
        imported_value
    }
  }

  out
}

split_dom_imp <- function(x) {

  # Sum duplicate entries, e.g. chemicals from feed + OPEX
  x <-
    tapply(
      x,
      names(x),
      sum
    )

  a_dom <-
    setNames(
      numeric(length(DOM_INPUT_NAMES)),
      DOM_INPUT_NAMES
    )

  a_imp <-
    setNames(
      numeric(length(IMP_INPUT_NAMES)),
      IMP_INPUT_NAMES
    )

  dom_names <-
    intersect(
      names(x),
      DOM_INPUT_NAMES
    )

  imp_names <-
    intersect(
      names(x),
      IMP_INPUT_NAMES
    )

  a_dom[dom_names] <-
    x[dom_names]

  a_imp[imp_names] <-
    x[imp_names]

  list(
    dom = a_dom,
    imp = a_imp
  )
}




##############################################################################################
## Technical coefficient vector for each IVC
##############################################################################################

build_ivc_vector <- function(
    prod_cost,
    market_price,
    alpha_cost,
    dist_feed,
    dist_capex,
    dist_opex,
    sourcing_group
) {

  # ------------------------------------------------------------
  # Cost intensity per EUR of biofuel output
  # ------------------------------------------------------------

  r_k <- prod_cost / market_price

  s_feed  <- r_k * alpha_cost[["feed"]]
  s_capex <- r_k * alpha_cost[["capex"]]
  s_opex  <- r_k * alpha_cost[["opex"]]


  # ------------------------------------------------------------
  # FEEDSTOCK
  # Domestic/import split is already explicitly contained
  # in dist_feed, e.g. food_bev / food_bev_imp
  # ------------------------------------------------------------

  # Raw monetary feedstock components
  a_feed_raw <-
    s_feed *
    dist_feed

  # Negative values represent revenues / gate-fee credits,
  # not purchases of intermediate inputs
  gate_fee_coeff <-
    -sum(
      pmin(
        a_feed_raw,
        0
      )
    )

  # Only positive purchased feedstock inputs enter A
  a_feed <-
    pmax(
      a_feed_raw,
      0
    )


  # ------------------------------------------------------------
  # CAPEX
  # ------------------------------------------------------------
  # CAPEX is deliberately kept OUTSIDE intermediate consumption
  # (a_feed / a_opex / operating_inputs / A_dom / A_imp / Z).
  # OPTION A (continuous, annualized investment treatment):
  # CAPEX is built here as its OWN separate technical-coefficient
  # vector (a_capex), sourced domestic/import exactly like OPEX,
  # so it can later be applied as an annual GFCF-equivalent
  # investment demand term in the NONBIO solve step - NOT as an
  # addition to intermediate consumption.
  # ------------------------------------------------------------

  dist_capex_sourced <-
    apply_import_shares(
      distribution = dist_capex,
      import_shares =
        CAPEX_IMPORT_SHARES[[sourcing_group]]
    )

  a_capex <-
    s_capex *
    dist_capex_sourced


  # ------------------------------------------------------------
  # OPEX
  # Apply domestic/import sourcing assumptions
  # ------------------------------------------------------------

  dist_opex_sourced <-
    apply_import_shares(
      distribution = dist_opex,
      import_shares =
        OPEX_IMPORT_SHARES[[sourcing_group]]
    )

  a_opex <-
    s_opex *
    dist_opex_sourced


  # ------------------------------------------------------------
  # COMBINE RECURRENT INTERMEDIATE INPUTS ONLY
  # ------------------------------------------------------------
  # Feedstock and OPEX remain intermediate inputs.
  # CAPEX is not included and the remaining components are NOT
  # renormalized.
  # ------------------------------------------------------------

  operating_inputs <-
    c(
      a_feed,
      a_opex
    )


  # ------------------------------------------------------------
  # SPLIT INTO DOMESTIC AND IMPORTED INPUT VECTORS
  # ------------------------------------------------------------

  input_split <-
    split_dom_imp(
      operating_inputs
    )

  # CAPEX is split domestic/import separately - it must NEVER be
  # merged into operating_inputs, so it never enters a_dom/a_imp
  # or the recurrent intermediate-consumption matrices.
  capex_split <-
    split_dom_imp(
      a_capex
    )


  # ------------------------------------------------------------
  # RETURN
  # ------------------------------------------------------------

  list(
    a_dom =
      input_split$dom,

    a_imp =
      input_split$imp,

    a_capex_dom =
      capex_split$dom,

    a_capex_imp =
      capex_split$imp,

    gate_fee_coeff =
      gate_fee_coeff
  )
}

##############################################################################################
## Technical coefficient vector for each final biofuel product
##############################################################################################

## Builds the aggregated technical coefficient vector of one final biofuel sector.
##
## Scenario/year-specific:
## - IVC weights
## - production costs
## - feed/CAPEX/OPEX cost shares (alpha)
## - feedstock distributions
##
## Fixed by IVC in IVC_TECH_LIBRARY:
## - market prices
## - CAPEX component distributions
## - OPEX component distributions
##
## CAPEX information is retained in the model inputs but excluded from
## the technical coefficient matrices in build_ivc_vector().
##
## For conventional IVCs, production costs and alpha may also fall back
## to the constant values stored in IVC_TECH_LIBRARY.


build_fuel_column <- function(
    fuel_cfg,
    sourcing_group
) {

  # ------------------------------------------------------------
  # IVC WEIGHTS
  # ------------------------------------------------------------

  weights <- fuel_cfg$weights

  if (is.null(weights) || length(weights) == 0) {
    stop("No IVC weights supplied.")
  }

  if (any(weights < 0)) {
    stop("IVC weights must not be negative.")
  }

  # Normalize to avoid small deviations caused by rounding
  weights <- weights / sum(weights)

  ivc_ids <- names(weights)


  # ------------------------------------------------------------
  # INITIALISE AGGREGATED RESULTS
  # ------------------------------------------------------------

  a_dom_agg <- NULL
  a_imp_agg <- NULL
  a_capex_dom_agg <- NULL
  a_capex_imp_agg <- NULL
  gate_fee_coeff_agg <- 0


  # ------------------------------------------------------------
  # LOOP OVER ACTIVE IVCs
  # ------------------------------------------------------------

  for (ivc_id in ivc_ids) {

    w <- weights[[ivc_id]]


    # ----------------------------------------------------------
    # TECHNOLOGY LIBRARY ENTRY
    # ----------------------------------------------------------

    tech_base <- IVC_TECH_LIBRARY[[ivc_id]]

    if (is.null(tech_base)) {
      stop(
        paste(
          "No technology-library entry found for",
          ivc_id
        )
      )
    }


    # ----------------------------------------------------------
    # PRODUCTION COST
    # Scenario/year-specific first;
    # technology library used as fallback
    # ----------------------------------------------------------

    ivc_prod_cost <- NULL

    if (!is.null(fuel_cfg$prod_cost)) {
      ivc_prod_cost <- fuel_cfg$prod_cost[[ivc_id]]
    }

    if (is.null(ivc_prod_cost)) {
      ivc_prod_cost <- tech_base$prod_cost
    }


    # ----------------------------------------------------------
    # MARKET PRICE
    # Fixed by IVC across scenarios and years
    # ----------------------------------------------------------

    ivc_market_price <- tech_base$market_price


    # ----------------------------------------------------------
    # FEED / CAPEX / OPEX COST SHARES
    # Scenario/year-specific first;
    # technology library used as fallback
    # ----------------------------------------------------------

    ivc_alpha <- NULL

    if (!is.null(fuel_cfg$alpha)) {
      ivc_alpha <- fuel_cfg$alpha[[ivc_id]]
    }

    if (is.null(ivc_alpha)) {
      ivc_alpha <- tech_base$alpha
    }


    # ----------------------------------------------------------
    # FEEDSTOCK DISTRIBUTION
    # Scenario/year-specific
    # ----------------------------------------------------------

    ivc_dist_feed <- NULL

    if (!is.null(fuel_cfg$dist_feed)) {
      ivc_dist_feed <- fuel_cfg$dist_feed[[ivc_id]]
    }


    # ----------------------------------------------------------
    # CAPEX / OPEX COMPONENT DISTRIBUTIONS
    # Fixed by IVC in the technology library.
    # CAPEX distribution is retained for later separate treatment.
    # ----------------------------------------------------------

    ivc_dist_capex <- tech_base$dist_capex
    ivc_dist_opex  <- tech_base$dist_opex


    # ----------------------------------------------------------
    # CHECK REQUIRED INPUTS
    # ----------------------------------------------------------

    if (
      is.null(ivc_prod_cost) ||
      is.null(ivc_market_price) ||
      is.null(ivc_alpha) ||
      is.null(ivc_dist_feed) ||
      is.null(ivc_dist_capex) ||
      is.null(ivc_dist_opex)
    ) {

      stop(
        paste(
          "Missing technology input for",
          ivc_id
        )
      )
    }


    # ----------------------------------------------------------
    # BUILD IVC-SPECIFIC OPERATING TECHNICAL COEFFICIENT VECTOR
    # ----------------------------------------------------------

    ivc_res <- build_ivc_vector(
      prod_cost      = ivc_prod_cost,
      market_price   = ivc_market_price,
      alpha_cost     = ivc_alpha,
      dist_feed      = ivc_dist_feed,
      dist_capex     = ivc_dist_capex,
      dist_opex      = ivc_dist_opex,
      sourcing_group = sourcing_group
    )


    # ----------------------------------------------------------
    # INITIALISE AGGREGATED VECTORS
    # ----------------------------------------------------------

    if (is.null(a_dom_agg)) {

      a_dom_agg <-
        setNames(
          numeric(length(ivc_res$a_dom)),
          names(ivc_res$a_dom)
        )

      a_imp_agg <-
        setNames(
          numeric(length(ivc_res$a_imp)),
          names(ivc_res$a_imp)
        )

      a_capex_dom_agg <-
        setNames(
          numeric(length(ivc_res$a_capex_dom)),
          names(ivc_res$a_capex_dom)
        )

      a_capex_imp_agg <-
        setNames(
          numeric(length(ivc_res$a_capex_imp)),
          names(ivc_res$a_capex_imp)
        )
    }


    # ----------------------------------------------------------
    # WEIGHTED AGGREGATION ACROSS IVCs
    # ----------------------------------------------------------

    a_dom_agg <-
      a_dom_agg +
      w * ivc_res$a_dom

    a_imp_agg <-
      a_imp_agg +
      w * ivc_res$a_imp

    a_capex_dom_agg <-
      a_capex_dom_agg +
      w * ivc_res$a_capex_dom

    a_capex_imp_agg <-
      a_capex_imp_agg +
      w * ivc_res$a_capex_imp

    gate_fee_coeff_agg <-
      gate_fee_coeff_agg +
      w * ivc_res$gate_fee_coeff
  }


  # ------------------------------------------------------------
  # RETURN
  # ------------------------------------------------------------

  list(
    a_dom = a_dom_agg,
    a_imp = a_imp_agg,
    a_capex_dom = a_capex_dom_agg,
    a_capex_imp = a_capex_imp_agg,
    gate_fee_coeff = gate_fee_coeff_agg
  )
}


#####################################################################################
# 4. SCENARIO 1
#####################################################################################


S1_2030 <- list(

  adv_biodiesel = list(
    abs_market_value = 3061584981.37,

    weights = c(IVC1 = 0.805139, IVC2_HVO = 0.160048, IVC13a = 0.034814),

    prod_cost = list(IVC1 = 1283.250000, IVC2_HVO = 2329.904479, IVC13a = 1469.657534),

    alpha = list(
      IVC1 = c(feed = 0.779271, capex = 0.064680, opex = 0.156049),
      IVC2_HVO = c(feed = 0.754174, capex = 0.115455, opex = 0.130370),
      IVC13a = c(feed = 0.155245, capex = 0.547747, opex = 0.297008)
    ),

    dist_feed = list(
      IVC1 = c(food_bev_imp = 1.000000),
      IVC2_HVO = c(agriculture = 0.071078, adv_biodiesel = 0.394792, chemicals = 0.139338, adv_biogasoline = 0.394792),
      IVC13a = c(agriculture = 0.570765, food_bev = 0.012837, forestry = 0.211788, paper = 0.179757, sewerage = 0.024852)
    )
  ),

  adv_biogasoline = list(
    abs_market_value = 615781224.37,

    weights = c(IVC5 = 0.405988, IVC12 = 0.383008, IVC13a = 0.173089, IVC13b_road = 0.037915),

    prod_cost = list(IVC5 = 1281.878637, IVC12 = 1166.350000, IVC13a = 1469.657534, IVC13b_road = 1546.274935),

    alpha = list(
      IVC5 = c(feed = 0.265141, capex = 0.432958, opex = 0.301901),
      IVC12 = c(feed = 0.508938, capex = 0.238350, opex = 0.252711),
      IVC13a = c(feed = 0.155245, capex = 0.547747, opex = 0.297008),
      IVC13b_road = c(feed = 0.147871, capex = 0.443000, opex = 0.409128)
    ),

    dist_feed = list(
      IVC5 = c(agriculture = 0.798502, paper = 0.201498),
      IVC12 = c(adv_biogas = 1.000000),
      IVC13a = c(agriculture = 0.570765, food_bev = 0.012837, forestry = 0.211788, paper = 0.179757, sewerage = 0.024852),
      IVC13b_road = c(agriculture = 0.569536, food_bev = 0.012810, forestry = 0.211332, paper = 0.179370, sewerage = 0.024798)
    )
  ),

  adv_biogas = list(
    abs_market_value = 1993280912.14,

    weights = c(IVC7 = 0.920044, IVC9a = 0.079956),

    prod_cost = list(IVC7 = 917.570994, IVC9a = 1303.901317),

    alpha = list(
      IVC7 = c(feed = -0.019403, capex = 0.659350, opex = 0.360054),
      IVC9a = c(feed = 0.179386, capex = 0.613543, opex = 0.207071)
    ),

    dist_feed = list(
      IVC7 = c(food_bev = -0.220097, agriculture = -1.032750, sewerage = 2.376712, adv_biodiesel = 0.000000, chemicals = -0.123865),
      IVC9a = c(sewerage = 0.026151, food_bev = 0.012566, agriculture = 0.558703, forestry = 0.193274, paper = 0.159288, adv_biodiesel = 0.050018)
    )
  ),

  adv_bio_kerosene = list(
    abs_market_value = 2094400000.00,

    weights = c(IVC2_HEFA = 0.941860, IVC11a_SAF = 0.058140),

    prod_cost = list(IVC2_HEFA = 2458.911636, IVC11a_SAF = 2774.146474),

    alpha = list(
      IVC2_HEFA = c(feed = 0.674449, capex = 0.152913, opex = 0.172637),
      IVC11a_SAF = c(feed = 0.220932, capex = 0.520881, opex = 0.258188)
    ),

    dist_feed = list(
      IVC2_HEFA = c(agriculture = 0.298742, adv_biodiesel = 0.298035, chemicals = 0.105189, adv_biogasoline = 0.298035),
      IVC11a_SAF = c(food_bev = 0.013196, agriculture = 0.586714, forestry = 0.202964, paper = 0.167274, sewerage = 0.027462, chemicals = 0.000000, adv_biodiesel = 0.001194, adv_biogasoline = 0.001194)
    )
  ),

  adv_bio_hfo = list(
    abs_market_value = 888194372.79,

    weights = c(IVC8a = 0.473714, IVC8b = 0.473714, IVC13b_mar = 0.052572),

    prod_cost = list(IVC8a = 556.884890, IVC8b = 936.000000, IVC13b_mar = 2000.298884),

    alpha = list(
      IVC8a = c(feed = 0.244907, capex = 0.504593, opex = 0.250501),
      IVC8b = c(feed = 0.459319, capex = 0.270340, opex = 0.270340),
      IVC13b_mar = c(feed = 0.341286, capex = 0.342449, opex = 0.316265)
    ),

    dist_feed = list(
      IVC8a = c(food_bev = 0.012093, agriculture = 0.537670, forestry = 0.185998, paper = 0.153292, sewerage = 0.025167, chemicals = 0.000000, adv_biodiesel = 0.042890, adv_biogasoline = 0.042890),
      IVC8b = c(adv_biogas = 1.000000),
      IVC13b_mar = c(agriculture = 0.603022, food_bev = 0.013074, forestry = 0.195262, paper = 0.165730, sewerage = 0.022913)
    )
  ),

  RFNBOs = list(
    abs_market_value = 320717510.60,

    weights = c(IVC8c = 0.503067, IVC9b = 0.496933),

    prod_cost = list(IVC8c = 370.619686, IVC9b = 1937.814918),

    alpha = list(
      IVC8c = c(feed = 0.787518, capex = 0.153796, opex = 0.058685),
      IVC9b = c(feed = 0.875504, capex = 0.066054, opex = 0.058442)
    ),

    dist_feed = list(
      IVC8c = c(chemicals = 1.000000),
      IVC9b = c(chemicals = 1.000000)
    )
  ),

  conv_biodiesel = list(
    abs_market_value = 20642403242.71,

    weights = c(IVC_T_FF = 0.245841, IVC_HT_FF = 0.282090, IVC_T_CC = 0.180696, IVC_HT_CC = 0.090913, IVC_T_lipids = 0.141169, IVC_HT_lipids = 0.059291),

    prod_cost = list(IVC_T_FF = 800.000000, IVC_HT_FF = 900.000000, IVC_T_CC = 1253.000000, IVC_HT_CC = 1203.000000, IVC_T_lipids = 1253.000000, IVC_HT_lipids = 2016.000000),

    alpha = list(
      IVC_T_FF = c(feed = 0.750000, capex = 0.065000, opex = 0.185000),
      IVC_HT_FF = c(feed = 0.666667, capex = 0.111111, opex = 0.222222),
      IVC_T_CC = c(feed = 0.720670, capex = 0.066241, opex = 0.213089),
      IVC_HT_CC = c(feed = 0.750623, capex = 0.083126, opex = 0.166251),
      IVC_T_lipids = c(feed = 0.720670, capex = 0.066241, opex = 0.213089),
      IVC_HT_lipids = c(feed = 0.665675, capex = 0.133433, opex = 0.200893)
    ),

    dist_feed = list(
      IVC_T_FF = c(food_bev = 1.000000),
      IVC_HT_FF = c(food_bev = 1.000000),
      IVC_T_CC = c(agriculture = 1.000000),
      IVC_HT_CC = c(agriculture = 1.000000),
      IVC_T_lipids = c(food_bev = 0.715489, food_bev_imp = 0.284511),
      IVC_HT_lipids = c(food_bev = 0.715489, food_bev_imp = 0.284511)
    )
  ),

  conv_biogasoline = list(
    abs_market_value = 4436256756.88,

    weights = c(IVC_EF_FF = 1.000000, IVC_HT_lipids_SAF = 0.217971),

    prod_cost = list(IVC_EF_FF = 903.570000, IVC_HT_lipids_SAF = 2903.000000),

    alpha = list(
      IVC_EF_FF = c(feed = 0.700000, capex = 0.100000, opex = 0.200000),
      IVC_HT_lipids_SAF = c(feed = 0.675508, capex = 0.129521, opex = 0.194971)
    ),

    dist_feed = list(
      IVC_EF_FF = c(agriculture = 1.000000),
      IVC_HT_lipids_SAF = c(food_bev = 0.715489, food_bev_imp = 0.284511)
    )
  ),

  conv_bio_kerosene = list(
    abs_market_value = 966976744.19,

    weights = c(IVC_HT_lipids_SAF = 1.000000),

    prod_cost = list(IVC_HT_lipids_SAF = 2903.000000),

    alpha = list(
      IVC_HT_lipids_SAF = c(feed = 0.675508, capex = 0.129521, opex = 0.194971)
    ),

    dist_feed = list(
      IVC_HT_lipids_SAF = c(food_bev = 0.715489, food_bev_imp = 0.284511)
    )
  ),

)


# ==========================================================
# SCENARIO 2 - 2030
# ==========================================================

S2_2030 <- list(

  adv_biodiesel = list(
    abs_market_value = 10232825407.60,

    weights = c(IVC2_HVO = 0.050782, IVC11a_road = 0.732292, IVC13a = 0.216926),

    prod_cost = list(IVC2_HVO = 2245.172862, IVC11a_road = 2502.277056, IVC13a = 1469.889719),

    alpha = list(
      IVC2_HVO = c(feed = 0.744897, capex = 0.119813, opex = 0.135290),
      IVC11a_road = c(feed = 0.136287, capex = 0.577474, opex = 0.286239),
      IVC13a = c(feed = 0.155379, capex = 0.547660, opex = 0.296961)
    ),

    dist_feed = list(
      IVC2_HVO = c(agriculture = 0.107549, adv_biodiesel = 0.379292, chemicals = 0.133868, adv_biogasoline = 0.379292),
      IVC11a_road = c(agriculture = 0.597717, forestry = 0.129200, paper = 0.149189, food_bev = 0.012600, sewerage = 0.023309, adv_biodiesel = 0.043992, chemicals = 0.000000, adv_biogasoline = 0.043992),
      IVC13a = c(agriculture = 0.639006, forestry = 0.148157, paper = 0.176186, food_bev = 0.013471, sewerage = 0.023180, adv_biodiesel = 0.000000, chemicals = 0.000000)
    )
  ),

  adv_biogasoline = list(
    abs_market_value = 10469280187.84,

    weights = c(IVC5 = 0.347543, IVC12 = 0.347543, IVC13a = 0.212027, IVC13b_road = 0.092888),

    prod_cost = list(IVC5 = 1284.229498, IVC12 = 1166.350000, IVC13a = 1469.889719, IVC13b_road = 1546.595519),

    alpha = list(
      IVC5 = c(feed = 0.266486, capex = 0.432166, opex = 0.301348),
      IVC12 = c(feed = 0.508938, capex = 0.238350, opex = 0.252711),
      IVC13a = c(feed = 0.155379, capex = 0.547660, opex = 0.296961),
      IVC13b_road = c(feed = 0.148048, capex = 0.442908, opex = 0.409044)
    ),

    dist_feed = list(
      IVC5 = c(agriculture = 0.819758, paper = 0.180242),
      IVC12 = c(adv_biogas = 1.000000),
      IVC13a = c(food_bev = 0.013471, agriculture = 0.639006, forestry = 0.148157, paper = 0.176186, sewerage = 0.023180),
      IVC13b_road = c(food_bev = 0.013136, agriculture = 0.647966, forestry = 0.144480, paper = 0.171813, sewerage = 0.022605)
    )
  ),

  adv_biogas = list(
    abs_market_value = 5897762899.10,

    weights = c(IVC7 = 0.508657, IVC9a = 0.491343),

    prod_cost = list(IVC7 = 922.732606, IVC9a = 1307.893370),

    alpha = list(
      IVC7 = c(feed = -0.013701, capex = 0.655661, opex = 0.358040),
      IVC9a = c(feed = 0.181890, capex = 0.611671, opex = 0.206439)
    ),

    dist_feed = list(
      IVC7 = c(food_bev = -0.338184, agriculture = -1.699925, chemicals = -0.127397, sewerage = 3.165507),
      IVC9a = c(food_bev = 0.013129, agriculture = 0.622776, forestry = 0.134617, paper = 0.155444, sewerage = 0.024286, adv_biodiesel = 0.024875, adv_biogasoline = 0.024875)
    )
  ),

  adv_bio_kerosene = list(
    abs_market_value = 3789697352.38,

    weights = c(IVC2_HEFA = 0.250030, IVC6 = 0.749970),

    prod_cost = list(IVC2_HEFA = 2356.102815, IVC6 = 2513.829787),

    alpha = list(
      IVC2_HEFA = c(feed = 0.660244, capex = 0.159586, opex = 0.180170),
      IVC6 = c(feed = 0.423191, capex = 0.298350, opex = 0.278460)
    ),

    dist_feed = list(
      IVC2_HEFA = c(agriculture = 0.430907, adv_biodiesel = 0.241864, chemicals = 0.085364, adv_biogasoline = 0.241864),
      IVC6 = c(adv_bio_hfo = 1.000000)
    )
  ),

  adv_bio_hfo = list(
    abs_market_value = 7394141436.00,

    weights = c(IVC8a = 0.368481, IVC8b = 0.368481, IVC13b_mar = 0.263038),

    prod_cost = list(IVC8a = 559.184571, IVC8b = 936.000000, IVC13b_mar = 2005.891082),

    alpha = list(
      IVC8a = c(feed = 0.248012, capex = 0.502517, opex = 0.249470),
      IVC8b = c(feed = 0.756410, capex = 0.145299, opex = 0.098291),
      IVC13b_mar = c(feed = 0.343122, capex = 0.341494, opex = 0.315384)
    ),

    dist_feed = list(
      IVC8a = c(food_bev = 0.012637, agriculture = 0.599452, forestry = 0.129575, paper = 0.149622, sewerage = 0.023376, adv_biodiesel = 0.042669, adv_biogasoline = 0.042669),
      IVC8b = c(adv_biogas = 1.000000),
      IVC13b_mar = c(agriculture = 0.669460, forestry = 0.135131, paper = 0.160695, food_bev = 0.013572, sewerage = 0.021142, adv_biodiesel = 0.000000, chemicals = 0.000000)
    )
  ),

  RFNBOs = list(
    abs_market_value = 679812591.45,

    weights = c(IVC8c = 0.237334, IVC9b = 0.762666),

    prod_cost = list(IVC8c = 370.619686, IVC9b = 1937.814918),

    alpha = list(
      IVC8c = c(feed = 0.787518, capex = 0.153796, opex = 0.058685),
      IVC9b = c(feed = 0.875504, capex = 0.066054, opex = 0.058442)
    ),

    dist_feed = list(
      IVC8c = c(chemicals = 1.000000),
      IVC9b = c(chemicals = 1.000000)
    )
  ),

  conv_biodiesel = list(
    abs_market_value = 15867820045.06,

    weights = c(IVC_T_FF = 0.267121, IVC_HT_FF = 0.315488, IVC_T_lipids = 0.388809, IVC_HT_CC = 0.028581),

    prod_cost = list(IVC_T_FF = 800.000000, IVC_HT_FF = 900.000000, IVC_T_lipids = 1253.000000, IVC_HT_CC = 1203.000000),

    alpha = list(
      IVC_T_FF = c(feed = 0.750000, capex = 0.065000, opex = 0.185000),
      IVC_HT_FF = c(feed = 0.666667, capex = 0.111111, opex = 0.222222),
      IVC_T_lipids = c(feed = 0.720670, capex = 0.066241, opex = 0.213089),
      IVC_HT_CC = c(feed = 0.750623, capex = 0.083126, opex = 0.166251)
    ),

    dist_feed = list(
      IVC_T_FF = c(food_bev = 1.000000),
      IVC_HT_FF = c(food_bev = 0.800553, food_bev_imp = 0.199446),
      IVC_T_lipids = c(food_bev = 0.715489, food_bev_imp = 0.284511),
      IVC_HT_CC = c(agriculture = 1.000000)
    )
  ),

  conv_biogasoline = list(
    abs_market_value = 79556645.66,

    weights = c(IVC_EF_FF = 1.000000),

    prod_cost = list(IVC_EF_FF = 903.570000),

    alpha = list(
      IVC_EF_FF = c(feed = 0.700000, capex = 0.100000, opex = 0.200000)
    ),

    dist_feed = list(
      IVC_EF_FF = c(agriculture = 1.000000)
    )
  ),

  conv_bio_kerosene = list(
    abs_market_value = 1102347635.59,

    weights = c(IVC_HT_lipids_SAF = 1.000000),

    prod_cost = list(IVC_HT_lipids_SAF = 2903.000000),

    alpha = list(
      IVC_HT_lipids_SAF = c(feed = 0.675508, capex = 0.129521, opex = 0.194971)
    ),

    dist_feed = list(
      IVC_HT_lipids_SAF = c(food_bev = 0.715489, food_bev_imp = 0.284511)
    )
  ),

)



# ==========================================================
# SCENARIO 3 - 2030
# ==========================================================


S3_2030 <- list(

  adv_biodiesel = list(
    abs_market_value = 22417240676.81,

    weights = c(IVC2_HVO = 0.035002, IVC11a_road = 0.675818, IVC13a = 0.289179),

    prod_cost = list(IVC2_HVO = 2245.172862, IVC11a_road = 2502.277056, IVC13a = 1469.889719),

    alpha = list(
      IVC2_HVO = c(feed = 0.744897, capex = 0.119813, opex = 0.135290),
      IVC11a_road = c(feed = 0.136287, capex = 0.577474, opex = 0.286239),
      IVC13a = c(feed = 0.155379, capex = 0.547660, opex = 0.296961)
    ),

    dist_feed = list(
      IVC2_HVO = c(agriculture = 0.107549, adv_biodiesel = 0.758583, chemicals = 0.133868),
      IVC11a_road = c(sewerage = 0.023309, food_bev = 0.012600, agriculture = 0.597717, forestry = 0.129200, paper = 0.149189, adv_biodiesel = 0.087985),
      IVC13a = c(sewerage = 0.023180, food_bev = 0.013471, agriculture = 0.639006, forestry = 0.148157, paper = 0.176186)
    )
  ),

  adv_biogasoline = list(
    abs_market_value = 52371751577.73,

    weights = c(IVC5 = 0.150813, IVC12 = 0.559335, IVC13a = 0.123781, IVC13b_road = 0.166071),

    prod_cost = list(IVC5 = 1284.229498, IVC12 = 1166.350000, IVC13a = 1469.889719, IVC13b_road = 1546.595519),

    alpha = list(
      IVC5 = c(feed = 0.266486, capex = 0.432166, opex = 0.301348),
      IVC12 = c(feed = 0.508938, capex = 0.238350, opex = 0.252711),
      IVC13a = c(feed = 0.155379, capex = 0.547660, opex = 0.296961),
      IVC13b_road = c(feed = 0.148048, capex = 0.442908, opex = 0.409044)
    ),

    dist_feed = list(
      IVC5 = c(agriculture = 0.819758, paper = 0.180242),
      IVC12 = c(adv_biogas = 1.000000),
      IVC13a = c(sewerage = 0.023180, food_bev = 0.013471, agriculture = 0.639006, forestry = 0.148157, paper = 0.176186, adv_biodiesel = 0.000000),
      IVC13b_road = c(sewerage = 0.022605, food_bev = 0.013136, agriculture = 0.647966, forestry = 0.144480, paper = 0.171813, adv_biodiesel = 0.000000)
    )
  ),

  adv_biogas = list(
    abs_market_value = 73538061676.69,

    weights = c(IVC7 = 0.725960, IVC9a = 0.274040),

    prod_cost = list(IVC7 = 922.732606, IVC9a = 1307.893370),

    alpha = list(
      IVC7 = c(feed = -0.013701, capex = 0.655661, opex = 0.358040),
      IVC9a = c(feed = 0.181890, capex = 0.611671, opex = 0.206439)
    ),

    dist_feed = list(
      IVC7 = c(food_bev = -0.338184, agriculture = -1.699925, sewerage = 3.165507, chemicals = -0.127397),
      IVC9a = c(sewerage = 0.024286, food_bev = 0.013129, agriculture = 0.622776, forestry = 0.134617, paper = 0.155444, adv_biodiesel = 0.049749)
    )
  ),

  adv_bio_kerosene = list(
    abs_market_value = 16338637286.22,

    weights = c(IVC2_HEFA = 0.140028, IVC6 = 0.859972),

    prod_cost = list(IVC2_HEFA = 2356.102815, IVC6 = 2513.829787),

    alpha = list(
      IVC2_HEFA = c(feed = 0.660244, capex = 0.159586, opex = 0.180170),
      IVC6 = c(feed = 0.423191, capex = 0.298350, opex = 0.278460)
    ),

    dist_feed = list(
      IVC2_HEFA = c(agriculture = 0.430907, adv_biodiesel = 0.241864, chemicals = 0.085364, adv_biogasoline = 0.241864),
      IVC6 = c(adv_bio_hfo = 1.000000)
    )
  ),

  adv_bio_hfo = list(
    abs_market_value = 66686073843.74,

    weights = c(IVC8a = 0.378756, IVC8b = 0.577605, IVC13b_mar = 0.043639),

    prod_cost = list(IVC8a = 559.184571, IVC8b = 936.000000, IVC13b_mar = 2005.891082),

    alpha = list(
      IVC8a = c(feed = 0.248012, capex = 0.502517, opex = 0.249470),
      IVC8b = c(feed = 0.756410, capex = 0.145299, opex = 0.098291),
      IVC13b_mar = c(feed = 0.343122, capex = 0.341494, opex = 0.315384)
    ),

    dist_feed = list(
      IVC8a = c(sewerage = 0.023376, food_bev = 0.012637, agriculture = 0.599452, forestry = 0.129575, paper = 0.149622, adv_biodiesel = 0.085337),
      IVC8b = c(adv_biogas = 1.000000),
      IVC13b_mar = c(sewerage = 0.021142, food_bev = 0.013572, agriculture = 0.669460, forestry = 0.135131, paper = 0.160695, adv_biodiesel = 0.000000)
    )
  ),

  RFNBOs = list(
    abs_market_value = 17729895555.06,

    weights = c(IVC8c = 0.577244, IVC9b = 0.422756),

    prod_cost = list(IVC8c = 370.619686, IVC9b = 1937.814918),

    alpha = list(
      IVC8c = c(feed = 0.787518, capex = 0.153796, opex = 0.058685),
      IVC9b = c(feed = 0.875504, capex = 0.066054, opex = 0.058442)
    ),

    dist_feed = list(
      IVC8c = c(chemicals = 1.000000),
      IVC9b = c(chemicals = 1.000000)
    )
  ),

  conv_biodiesel = list(
    abs_market_value = 14567166158.83,

    weights = c(IVC_T_FF = 0.290972, IVC_HT_FF = 0.343657, IVC_T_lipids = 0.334238, IVC_HT_CC = 0.031133),

    prod_cost = list(IVC_T_FF = 800.000000, IVC_HT_FF = 900.000000, IVC_T_lipids = 1253.000000, IVC_HT_CC = 1203.000000),

    alpha = list(
      IVC_T_FF = c(feed = 0.750000, capex = 0.065000, opex = 0.185000),
      IVC_HT_FF = c(feed = 0.666667, capex = 0.111111, opex = 0.222222),
      IVC_T_lipids = c(feed = 0.720670, capex = 0.066241, opex = 0.213089),
      IVC_HT_CC = c(feed = 0.750623, capex = 0.083126, opex = 0.166251)
    ),

    dist_feed = list(
      IVC_T_FF = c(food_bev = 1.000000),
      IVC_HT_FF = c(food_bev = 0.800553, food_bev_imp = 0.199446),
      IVC_T_lipids = c(food_bev = 0.715489, food_bev_imp = 0.284511),
      IVC_HT_CC = c(agriculture = 1.000000)
    )
  ),

  conv_biogasoline = list(
    # No conv_biogasoline production reported in this scenario's source sheet.
    abs_market_value = 0,
    weights = c(IVC_EF_FF = 1.000000),
    dist_feed = list(IVC_EF_FF = c(agriculture = 1.000000))
  ),

  conv_bio_kerosene = list(
    # No conv_bio_kerosene production reported in this scenario's source sheet.
    abs_market_value = 0,
    weights = c(IVC_HT_lipids_SAF = 1.000000),
    dist_feed = list(IVC_HT_lipids_SAF = c(agriculture = 1.000000))
  ),

)




# ==========================================================
# FINISHED BIOFUEL IMPORTS - 2030
# Monetary value in EUR, at market value
# All imported finished fuels go to final consumption expenditure
# ==========================================================
## note: so far only an exogenous scenario vraiavle, stored in each endpoint, but not yet inegrated into annual SFC dynamics


S1_imports_2030 <- c(
  conv_biodiesel    = 879154718.43,
  adv_biodiesel     = 13022074304.18,
  conv_biogasoline  = 0,
  adv_biogasoline   = 4661504967.96,
  conv_bio_kerosene = 0,
  adv_bio_kerosene  = 1865379175.15,
  adv_bio_hfo       = 1937302197.55,
  RFNBOs            = 359095080.85,
  adv_biogas        = 0
)

S2_imports_2030 <- c(
  conv_biodiesel    = 7535613093.35,
  adv_biodiesel     = 0,
  conv_biogasoline  = 0,
  adv_biogasoline   = 0,
  conv_bio_kerosene = 0,
  adv_bio_kerosene  = 0,
  adv_bio_hfo       = 0,
  RFNBOs            = 0,
  adv_biogas        = 0
)


# S3 2030 imports are explicitly "NONE" in the source workbook - all zero.
S3_imports_2030 <- setNames(rep(0, length(BIOFUEL_SECTORS)), names(BIOFUEL_SECTORS))

## Conversion from scenario values (stored in EUR) into the monetary unit
## of the Eurostat IO tables. Keep 1 if the IO tables are in EUR;
## use 1e6 if the IO tables are in million EUR, etc.

SCENARIO_EUR_TO_IO_UNIT <- 1e6


# ==========================================================
# BIOFUEL EXPORTS - 2030
# Monetary values in EUR
# ==========================================================

S1_exports_2030 <- setNames(rep(0, length(BIOFUEL_SECTORS)), names(BIOFUEL_SECTORS))        ## vector with all zero, because in S1 and S2 there are NO exports
S2_exports_2030 <- setNames(rep(0, length(BIOFUEL_SECTORS)), names(BIOFUEL_SECTORS))

S3_exports_2030 <- c(
  conv_biodiesel    = 0,
  adv_biodiesel     = 0,
  conv_biogasoline  = 0,
  adv_biogasoline   = 36212333077.31,
  conv_bio_kerosene = 0,
  adv_bio_kerosene  = 11401449827.92,
  adv_bio_hfo       = 44606111011.19,
  RFNBOs            = 17050082963.60,
  adv_biogas        = 25917437239.16
)
######################################################################################################
### YEAR 2035 ###
######################################################################################################


#####################################################################################
# SCENARIO 1 - 2035
#####################################################################################

S1_2035 <- list(

  adv_biodiesel = list(
    abs_market_value = 4083938421.66,

    weights = c(IVC1 = 0.707430, IVC2_HVO = 0.254044, IVC13a = 0.038526),

    prod_cost = list(IVC1 = 1283.250000, IVC2_HVO = 2329.904479, IVC13a = 1469.657534),

    alpha = list(
      IVC1 = c(feed = 0.779271, capex = 0.064680, opex = 0.156049),
      IVC2_HVO = c(feed = 0.754174, capex = 0.115455, opex = 0.130370),
      IVC13a = c(feed = 0.155245, capex = 0.547747, opex = 0.297008)
    ),

    dist_feed = list(
      IVC1 = c(food_bev_imp = 1.000000),
      IVC2_HVO = c(food_bev_imp = 1.000000),
      IVC13a = c(agriculture = 0.570765, forestry = 0.211788, paper = 0.179757, food_bev = 0.012837, sewerage = 0.024852)
    )
  ),

  adv_biogasoline = list(
    abs_market_value = 936805946.66,

    weights = c(IVC5 = 0.381235, IVC12 = 0.431586, IVC13a = 0.167953, IVC13b_road = 0.019226),

    prod_cost = list(IVC5 = 1281.878637, IVC12 = 1166.350000, IVC13a = 1469.657534, IVC13b_road = 1546.274935),

    alpha = list(
      IVC5 = c(feed = 0.265141, capex = 0.432958, opex = 0.301901),
      IVC12 = c(feed = 0.508938, capex = 0.238350, opex = 0.252711),
      IVC13a = c(feed = 0.155245, capex = 0.547747, opex = 0.297008),
      IVC13b_road = c(feed = 0.147871, capex = 0.443000, opex = 0.409128)
    ),

    dist_feed = list(
      IVC5 = c(agriculture = 0.798502, paper = 0.201498),
      IVC12 = c(adv_biogas = 1.000000),
      IVC13a = c(agriculture = 0.570765, forestry = 0.211788, paper = 0.179757, food_bev = 0.012837, sewerage = 0.024852, adv_biogas = 0.000000),
      IVC13b_road = c(agriculture = 0.579730, forestry = 0.207365, paper = 0.176002, food_bev = 0.012569, sewerage = 0.024333, adv_biogas = 0.000000)
    )
  ),

  adv_biogas = list(
    abs_market_value = 2605285106.34,

    weights = c(IVC7 = 1.000000, IVC9a = 0.262295),

    prod_cost = list(IVC7 = 917.570994, IVC9a = 1303.901317),

    alpha = list(
      IVC7 = c(feed = -0.019403, capex = 0.659350, opex = 0.360054),
      IVC9a = c(feed = 0.179386, capex = 0.613543, opex = 0.207071)
    ),

    dist_feed = list(
      IVC7 = c(food_bev = -0.220097, agriculture = -1.032750, sewerage = 2.376712, chemicals = -0.123865),
      IVC9a = c(sewerage = 0.026151, food_bev = 0.012566, agriculture = 0.558703, forestry = 0.193274, paper = 0.159288, adv_biodiesel = 0.050018)
    )
  ),

  adv_bio_kerosene = list(
    abs_market_value = 3213661588.91,

    weights = c(IVC2_HEFA = 0.896382, IVC11a_SAF = 0.064955, IVC13b_SAF = 0.038662),

    prod_cost = list(IVC2_HEFA = 2458.911636, IVC11a_SAF = 2774.146474, IVC13b_SAF = 2000.298884),

    alpha = list(
      IVC2_HEFA = c(feed = 0.674449, capex = 0.152913, opex = 0.172637),
      IVC11a_SAF = c(feed = 0.220932, capex = 0.520881, opex = 0.258188),
      IVC13b_SAF = c(feed = 0.341286, capex = 0.342449, opex = 0.316265)
    ),

    dist_feed = list(
      IVC2_HEFA = c(agriculture = 0.298742, adv_biodiesel = 0.298035, chemicals = 0.105189, adv_biogasoline = 0.298035),
      IVC11a_SAF = c(agriculture = 0.586714, forestry = 0.202964, paper = 0.167274, sewerage = 0.027462, food_bev = 0.013196, adv_biodiesel = 0.002388),
      IVC13b_SAF = c(agriculture = 0.603022, forestry = 0.195262, paper = 0.165730, sewerage = 0.022913, food_bev = 0.013074)
    )
  ),

  adv_bio_hfo = list(
    abs_market_value = 1483794935.51,

    weights = c(IVC8a = 0.485839, IVC8b = 0.485839, IVC13b_mar = 0.028323),

    prod_cost = list(IVC8a = 556.884890, IVC8b = 936.000000, IVC13b_mar = 2000.298884),

    alpha = list(
      IVC8a = c(feed = 0.244907, capex = 0.504593, opex = 0.250501),
      IVC8b = c(feed = 0.459319, capex = 0.270340, opex = 0.270340),
      IVC13b_mar = c(feed = 0.341286, capex = 0.342449, opex = 0.316265)
    ),

    dist_feed = list(
      IVC8a = c(agriculture = 0.537670, forestry = 0.185998, paper = 0.153292, sewerage = 0.025167, food_bev = 0.012093, adv_biodiesel = 0.085781),
      IVC8b = c(adv_biogas = 1.000000),
      IVC13b_mar = c(agriculture = 0.603022, forestry = 0.195262, paper = 0.165730, food_bev = 0.013074, sewerage = 0.022913)
    )
  ),

  RFNBOs = list(
    abs_market_value = 994008214.29,

    weights = c(IVC8c = 0.725230, IVC9b = 0.274770),

    prod_cost = list(IVC8c = 370.619686, IVC9b = 1937.814918),

    alpha = list(
      IVC8c = c(feed = 0.787518, capex = 0.153796, opex = 0.058685),
      IVC9b = c(feed = 0.875504, capex = 0.066054, opex = 0.058442)
    ),

    dist_feed = list(
      IVC8c = c(chemicals = 1.000000),
      IVC9b = c(chemicals = 1.000000)
    )
  ),

  conv_biodiesel = list(
    abs_market_value = 16263122131.49,

    weights = c(IVC_T_FF = 0.268713, IVC_HT_FF = 0.193709, IVC_T_CC = 0.000000, IVC_HT_CC = 0.197817, IVC_HT_lipids_SAF = 0.339761),

    prod_cost = list(IVC_T_FF = 800.000000, IVC_HT_FF = 900.000000, IVC_T_CC = 1253.000000, IVC_HT_CC = 1203.000000, IVC_HT_lipids_SAF = 2903.000000),

    alpha = list(
      IVC_T_FF = c(feed = 0.750000, capex = 0.065000, opex = 0.185000),
      IVC_HT_FF = c(feed = 0.666667, capex = 0.111111, opex = 0.222222),
      IVC_T_CC = c(feed = 0.720670, capex = 0.066241, opex = 0.213089),
      IVC_HT_CC = c(feed = 0.750623, capex = 0.083126, opex = 0.166251),
      IVC_HT_lipids_SAF = c(feed = 0.675508, capex = 0.129521, opex = 0.194971)
    ),

    dist_feed = list(
      IVC_T_FF = c(food_bev = 0.800554, food_bev_imp = 0.199446),
      IVC_HT_FF = c(food_bev = 0.800554, food_bev_imp = 0.199446),
      IVC_T_CC = c(agriculture = 1.000000),
      IVC_HT_CC = c(agriculture = 1.000000),
      IVC_HT_lipids_SAF = c(food_bev = 0.610000, food_bev_imp = 0.390000)
    )
  ),

  conv_biogasoline = list(
    abs_market_value = 4038618047.10,

    weights = c(IVC_EF_FF = 1.000000),

    prod_cost = list(IVC_EF_FF = 903.570000),

    alpha = list(
      IVC_EF_FF = c(feed = 0.700000, capex = 0.100000, opex = 0.200000)
    ),

    dist_feed = list(
      IVC_EF_FF = c(agriculture = 1.000000)
    )
  ),

  conv_bio_kerosene = list(
    abs_market_value = 3226744186.05,

    weights = c(IVC_HT_lipids_SAF = 1.000000),

    prod_cost = list(IVC_HT_lipids_SAF = 2903.000000),

    alpha = list(
      IVC_HT_lipids_SAF = c(feed = 0.675508, capex = 0.129521, opex = 0.194971)
    ),

    dist_feed = list(
      IVC_HT_lipids_SAF = c(food_bev = 0.610000, food_bev_imp = 0.390000)
    )
  ),

)



#####################################################################################
# SCENARIO 2 - 2035
#####################################################################################

S2_2035 <- list(

  adv_biodiesel = list(
    abs_market_value = 17066996781.46,

    weights = c(IVC2_HVO = 0.042682, IVC11a_road = 0.722008, IVC13a = 0.235310),

    prod_cost = list(IVC2_HVO = 2245.172862, IVC11a_road = 2502.277056, IVC13a = 1469.889719),

    alpha = list(
      IVC2_HVO = c(feed = 0.744897, capex = 0.119813, opex = 0.135290),
      IVC11a_road = c(feed = 0.136287, capex = 0.577474, opex = 0.286239),
      IVC13a = c(feed = 0.155379, capex = 0.547660, opex = 0.296961)
    ),

    dist_feed = list(
      IVC2_HVO = c(agriculture = 1.000000),
      IVC11a_road = c(agriculture = 0.597717, forestry = 0.129200, paper = 0.149189, food_bev = 0.012600, sewerage = 0.023309, adv_biodiesel = 0.043992, adv_biogasoline = 0.043992),
      IVC13a = c(agriculture = 0.639006, forestry = 0.148157, paper = 0.176186, food_bev = 0.013471, sewerage = 0.023180, adv_biodiesel = 0.000000)
    )
  ),

  adv_biogasoline = list(
    abs_market_value = 8302496718.09,

    weights = c(IVC5 = 0.210463, IVC12 = 0.210463, IVC13a = 0.483714, IVC13b_road = 0.095361),

    prod_cost = list(IVC5 = 1284.229498, IVC12 = 1166.350000, IVC13a = 1469.889719, IVC13b_road = 1546.595519),

    alpha = list(
      IVC5 = c(feed = 0.266486, capex = 0.432166, opex = 0.301348),
      IVC12 = c(feed = 0.508938, capex = 0.238350, opex = 0.252711),
      IVC13a = c(feed = 0.155379, capex = 0.547660, opex = 0.296961),
      IVC13b_road = c(feed = 0.148048, capex = 0.442908, opex = 0.409044)
    ),

    dist_feed = list(
      IVC5 = c(agriculture = 0.819758, paper = 0.180242),
      IVC12 = c(adv_biogas = 1.000000),
      IVC13a = c(agriculture = 0.639006, forestry = 0.148157, paper = 0.176186, food_bev = 0.013471, sewerage = 0.023180),
      IVC13b_road = c(agriculture = 0.647966, forestry = 0.144480, paper = 0.171813, food_bev = 0.013136, sewerage = 0.022605)
    )
  ),

  adv_biogas = list(
    abs_market_value = 5220497110.81,

    weights = c(IVC7 = 0.566664, IVC9a = 0.433336),

    prod_cost = list(IVC7 = 922.732606, IVC9a = 1307.893370),

    alpha = list(
      IVC7 = c(feed = -0.013701, capex = 0.655661, opex = 0.358040),
      IVC9a = c(feed = 0.181890, capex = 0.611671, opex = 0.206439)
    ),

    dist_feed = list(
      IVC7 = c(agriculture = -1.699925, sewerage = 3.165507, food_bev = -0.338184, forestry = 0.000000, paper = 0.000000, chemicals = -0.127397, adv_biodiesel = 0.000000),
      IVC9a = c(agriculture = 0.622776, sewerage = 0.024286, food_bev = 0.013129, forestry = 0.134617, paper = 0.155444, chemicals = 0.000000, adv_biodiesel = 0.024875, adv_biogasoline = 0.024875)
    )
  ),

  adv_bio_kerosene = list(
    abs_market_value = 8244827779.80,

    weights = c(IVC2_HEFA = 0.197015, IVC6 = 0.056213, IVC11a_SAF = 0.084320, IVC13b_SAF = 0.662452),

    prod_cost = list(IVC2_HEFA = 2356.102815, IVC6 = 2513.829787, IVC11a_SAF = 2784.774710, IVC13b_SAF = 2005.891082),

    alpha = list(
      IVC2_HEFA = c(feed = 0.660244, capex = 0.159586, opex = 0.180170),
      IVC6 = c(feed = 0.423191, capex = 0.298350, opex = 0.278460),
      IVC11a_SAF = c(feed = 0.223905, capex = 0.518893, opex = 0.257202),
      IVC13b_SAF = c(feed = 0.343122, capex = 0.341494, opex = 0.315384)
    ),

    dist_feed = list(
      IVC2_HEFA = c(agriculture = 0.430907, adv_biogasoline = 0.241864, chemicals = 0.085364, adv_biodiesel = 0.241864),
      IVC6 = c(adv_bio_hfo = 1.000000),
      IVC11a_SAF = c(sewerage = 0.025497, food_bev = 0.013783, agriculture = 0.653824, forestry = 0.141328, paper = 0.163193, adv_biodiesel = 0.001187, adv_biogasoline = 0.001187),
      IVC13b_SAF = c(food_bev = 0.013572, agriculture = 0.669460, forestry = 0.135131, paper = 0.160695, sewerage = 0.021142, adv_biodiesel = 0.000000)
    )
  ),

  adv_bio_hfo = list(
    abs_market_value = 6854139144.72,

    weights = c(IVC8a = 0.318895, IVC8b = 0.411578, IVC13b_mar = 0.269527),

    prod_cost = list(IVC8a = 559.184571, IVC8b = 936.000000, IVC13b_mar = 2005.891082),

    alpha = list(
      IVC8a = c(feed = 0.248012, capex = 0.502517, opex = 0.249470),
      IVC8b = c(feed = 0.756410, capex = 0.145299, opex = 0.098291),
      IVC13b_mar = c(feed = 0.343122, capex = 0.341494, opex = 0.315384)
    ),

    dist_feed = list(
      IVC8a = c(food_bev = 0.012637, agriculture = 0.599452, forestry = 0.129575, paper = 0.149622, sewerage = 0.023376, adv_biogasoline = 0.042669, adv_biodiesel = 0.042669),
      IVC8b = c(adv_biogas = 1.000000),
      IVC13b_mar = c(agriculture = 0.669460, forestry = 0.135131, paper = 0.160695, food_bev = 0.013572, sewerage = 0.021142)
    )
  ),

  RFNBOs = list(
    abs_market_value = 13310018195.37,

    weights = c(IVC8c = 0.300253, IVC9b = 0.699747),

    prod_cost = list(IVC8c = 370.619686, IVC9b = 1937.814918),

    alpha = list(
      IVC8c = c(feed = 0.787518, capex = 0.153796, opex = 0.058685),
      IVC9b = c(feed = 0.875504, capex = 0.066054, opex = 0.058442)
    ),

    dist_feed = list(
      IVC8c = c(chemicals = 1.000000),
      IVC9b = c(chemicals = 1.000000)
    )
  ),

  conv_biodiesel = list(
    abs_market_value = 8355523573.25,

    weights = c(IVC_HT_CC = 0.093048, IVC_T_lipids = 0.906952),

    prod_cost = list(IVC_HT_CC = 1203.000000, IVC_T_lipids = 1253.000000),

    alpha = list(
      IVC_HT_CC = c(feed = 0.750623, capex = 0.083126, opex = 0.166251),
      IVC_T_lipids = c(feed = 0.720670, capex = 0.066241, opex = 0.213089)
    ),

    dist_feed = list(
      IVC_HT_CC = c(agriculture = 1.000000),
      IVC_T_lipids = c(food_bev = 0.573200, food_bev_imp = 0.426800)
    )
  ),

  conv_bio_kerosene = list(
    abs_market_value = 6101362522.63,

    weights = c(IVC_HT_lipids_SAF = 1.000000),

    prod_cost = list(IVC_HT_lipids_SAF = 2903.000000),

    alpha = list(
      IVC_HT_lipids_SAF = c(feed = 0.675508, capex = 0.129521, opex = 0.194971)
    ),

    dist_feed = list(
      IVC_HT_lipids_SAF = c(food_bev = 0.573200, food_bev_imp = 0.426800)
    )
  ),

)



#####################################################################################
# SCENARIO 3 - 2035
#####################################################################################

S3_2035 <- list(

  adv_biodiesel = list(
    abs_market_value = 17842831191.38,

    weights = c(IVC2_HVO = 0.037676, IVC11a_road = 0.580218, IVC13a = 0.382106),

    prod_cost = list(IVC2_HVO = 2245.172862, IVC11a_road = 2502.277056, IVC13a = 1469.889719),

    alpha = list(
      IVC2_HVO = c(feed = 0.744897, capex = 0.119813, opex = 0.135290),
      IVC11a_road = c(feed = 0.136287, capex = 0.577474, opex = 0.286239),
      IVC13a = c(feed = 0.155379, capex = 0.547660, opex = 0.296961)
    ),

    dist_feed = list(
      IVC2_HVO = c(agriculture = 1.000000),
      IVC11a_road = c(agriculture = 0.597717, forestry = 0.129200, paper = 0.149189, food_bev = 0.012600, sewerage = 0.023309, adv_biodiesel = 0.043992, adv_biogasoline = 0.043992),
      IVC13a = c(agriculture = 0.639006, forestry = 0.148157, paper = 0.176186, food_bev = 0.013471, sewerage = 0.023180, adv_biodiesel = 0.000000)
    )
  ),

  adv_biogasoline = list(
    abs_market_value = 56754841569.82,

    weights = c(IVC5 = 0.147810, IVC12 = 0.526756, IVC13a = 0.120128, IVC13b_road = 0.205305),

    prod_cost = list(IVC5 = 1284.229498, IVC12 = 1166.350000, IVC13a = 1469.889719, IVC13b_road = 1546.595519),

    alpha = list(
      IVC5 = c(feed = 0.266486, capex = 0.432166, opex = 0.301348),
      IVC12 = c(feed = 0.508938, capex = 0.238350, opex = 0.252711),
      IVC13a = c(feed = 0.155379, capex = 0.547660, opex = 0.296961),
      IVC13b_road = c(feed = 0.148048, capex = 0.442908, opex = 0.409044)
    ),

    dist_feed = list(
      IVC5 = c(agriculture = 0.819758, paper = 0.180242),
      IVC12 = c(adv_biogas = 1.000000),
      IVC13a = c(agriculture = 0.639006, forestry = 0.148157, paper = 0.176186, food_bev = 0.013471, sewerage = 0.023180),
      IVC13b_road = c(agriculture = 0.647966, forestry = 0.144480, paper = 0.171813, food_bev = 0.013136, sewerage = 0.022605)
    )
  ),

  adv_biogas = list(
    abs_market_value = 75318023116.41,

    weights = c(IVC7 = 0.726932, IVC9a = 0.273068),

    prod_cost = list(IVC7 = 922.732606, IVC9a = 1307.893370),

    alpha = list(
      IVC7 = c(feed = -0.013701, capex = 0.655661, opex = 0.358040),
      IVC9a = c(feed = 0.181890, capex = 0.611671, opex = 0.206439)
    ),

    dist_feed = list(
      IVC7 = c(agriculture = -1.699925, sewerage = 3.165507, food_bev = -0.338184, forestry = 0.000000, paper = 0.000000, chemicals = -0.127397, adv_biodiesel = 0.000000),
      IVC9a = c(agriculture = 0.622776, sewerage = 0.024286, food_bev = 0.013129, forestry = 0.134617, paper = 0.155444, chemicals = 0.000000, adv_biodiesel = 0.024875, adv_biogasoline = 0.024875)
    )
  ),

  adv_bio_kerosene = list(
    abs_market_value = 22292239597.26,

    weights = c(IVC2_HEFA = 0.144782, IVC6 = 0.639978, IVC11a_SAF = 0.215240),

    prod_cost = list(IVC2_HEFA = 2356.102815, IVC6 = 2513.829787, IVC11a_SAF = 2784.774710),

    alpha = list(
      IVC2_HEFA = c(feed = 0.660244, capex = 0.159586, opex = 0.180170),
      IVC6 = c(feed = 0.423191, capex = 0.298350, opex = 0.278460),
      IVC11a_SAF = c(feed = 0.223905, capex = 0.518893, opex = 0.257202)
    ),

    dist_feed = list(
      IVC2_HEFA = c(agriculture = 0.430907, adv_biogasoline = 0.142273, chemicals = 0.284546, adv_biodiesel = 0.142273),
      IVC6 = c(adv_bio_hfo = 1.000000),
      IVC11a_SAF = c(sewerage = 0.025497, food_bev = 0.013783, agriculture = 0.653824, forestry = 0.141328, paper = 0.163193, adv_biodiesel = 0.001187, adv_biogasoline = 0.001187)
    )
  ),

  adv_bio_hfo = list(
    abs_market_value = 69072668587.09,

    weights = c(IVC8a = 0.377418, IVC8b = 0.566210, IVC13b_mar = 0.056372),

    prod_cost = list(IVC8a = 559.184571, IVC8b = 936.000000, IVC13b_mar = 2005.891082),

    alpha = list(
      IVC8a = c(feed = 0.248012, capex = 0.502517, opex = 0.249470),
      IVC8b = c(feed = 0.756410, capex = 0.145299, opex = 0.098291),
      IVC13b_mar = c(feed = 0.343122, capex = 0.341494, opex = 0.315384)
    ),

    dist_feed = list(
      IVC8a = c(food_bev = 0.012637, agriculture = 0.599452, forestry = 0.129575, paper = 0.149622, sewerage = 0.023376, adv_biogasoline = 0.042669, adv_biodiesel = 0.042669),
      IVC8b = c(adv_biogas = 1.000000),
      IVC13b_mar = c(agriculture = 0.669460, forestry = 0.135131, paper = 0.160695, food_bev = 0.013572, sewerage = 0.021142)
    )
  ),

  RFNBOs = list(
    abs_market_value = 22030805806.36,

    weights = c(IVC8c = 0.577244, IVC9b = 0.422756),

    prod_cost = list(IVC8c = 370.619686, IVC9b = 1937.814918),

    alpha = list(
      IVC8c = c(feed = 0.787518, capex = 0.153796, opex = 0.058685),
      IVC9b = c(feed = 0.875504, capex = 0.066054, opex = 0.058442)
    ),

    dist_feed = list(
      IVC8c = c(chemicals = 1.000000),
      IVC9b = c(chemicals = 1.000000)
    )
  ),

  conv_biodiesel = list(
    abs_market_value = 15154198406.98,

    weights = c(IVC_T_FF = 0.271605, IVC_HT_FF = 0.228395, IVC_HT_CC = 0.228395, IVC_T_lipids = 0.271605),

    prod_cost = list(IVC_T_FF = 800.000000, IVC_HT_FF = 900.000000, IVC_HT_CC = 1203.000000, IVC_T_lipids = 1253.000000),

    alpha = list(
      IVC_T_FF = c(feed = 0.750000, capex = 0.065000, opex = 0.185000),
      IVC_HT_FF = c(feed = 0.666667, capex = 0.111111, opex = 0.222222),
      IVC_HT_CC = c(feed = 0.750623, capex = 0.083126, opex = 0.166251),
      IVC_T_lipids = c(feed = 0.720670, capex = 0.066241, opex = 0.213089)
    ),

    dist_feed = list(
      IVC_T_FF = c(food_bev = 0.800554, food_bev_imp = 0.199446),
      IVC_HT_FF = c(food_bev = 0.800554, food_bev_imp = 0.199446),
      IVC_HT_CC = c(agriculture = 1.000000),
      IVC_T_lipids = c(food_bev = 0.573200, food_bev_imp = 0.426800)
    )
  ),

)


# ==========================================================
# FINISHED BIOFUEL IMPORTS / EXPORTS - 2035
# Monetary values in EUR, at market value.
# Derived from Imports_Exports_All_Scenarios.xlsx ('2035 Sc 1/2/3' sheets).
# ==========================================================

S1_imports_2035 <- c(
  conv_biodiesel    = 0,
  adv_biodiesel     = 18862095594.02,
  conv_biogasoline  = 0,
  adv_biogasoline   = 2658381656.52,
  conv_bio_kerosene = 0,
  adv_bio_kerosene  = 8222014081.21,
  adv_bio_hfo       = 762619367.67,
  RFNBOs            = 35603837506.48,
  adv_biogas        = 0
)
S1_exports_2035 <- setNames(rep(0, length(BIOFUEL_SECTORS)), names(BIOFUEL_SECTORS))

S2_imports_2035 <- c(
  conv_biodiesel    = 0,
  adv_biodiesel     = 0,
  conv_biogasoline  = 0,
  adv_biogasoline   = 0,
  conv_bio_kerosene = 0,
  adv_bio_kerosene  = 0,
  adv_bio_hfo       = 0,
  RFNBOs            = 5485908772.16,
  adv_biogas        = 0
)
S2_exports_2035 <- setNames(rep(0, length(BIOFUEL_SECTORS)), names(BIOFUEL_SECTORS))

# S3 2035 imports are explicitly "NONE" in the source workbook - all zero.
S3_imports_2035 <- setNames(rep(0, length(BIOFUEL_SECTORS)), names(BIOFUEL_SECTORS))

S3_exports_2035 <- c(
  conv_biodiesel    = 0,
  adv_biodiesel     = 0,
  conv_biogasoline  = 0,
  adv_biogasoline   = 42698721669.95,
  conv_bio_kerosene = 0,
  adv_bio_kerosene  = 7341873991.52,
  adv_bio_hfo       = 44735575368.87,
  RFNBOs            = 4871514959.82,
  adv_biogas        = 26315422128.06
)


######################################################################################################
### YEAR 2040 ###
######################################################################################################
 
#####################################################################################
# SCENARIO 1 - 2040
#####################################################################################
 
# Derived from the "2040 Sc 1" sheet in Providing_sectors.xlsx.
#
# DATA CAVEATS (from the source workbook, not introduced here):
#  - The "Advanced biodiesel" TOTAL cell is #REF! in the sheet; abs_market_value
#    below is the sum of IVC1 + IVC2_HVO + IVC13a instead.
#  - adv_bio_hfo: IVC8a and IVC8b have identical Mtoe/market-value figures in
#    the sheet (a deliberate 50/50 technology split, as in 2035), so both are
#    counted in the total even though the sheet's own TOTAL row does not do
#    so. Please double check this assumption.
#  - conv_bio_kerosene's only production route in this sheet is hydrotreatment
#    of cover crops from marginal lands - a different feedstock than the
#    UCO/animal-fats route (IVC_HT_lipids_SAF) used for conv_bio_kerosene in
#    2030/2035. Named IVC_HT_CC_road here; please confirm this is consistent
#    with the rest of the pipeline / your naming conventions elsewhere.
 
S1_2040 <- list(

  adv_biodiesel = list(
    abs_market_value = 4896291861.95,

    weights = c(IVC1 = 0.676675, IVC2_HVO = 0.280825, IVC13a = 0.042500),

    prod_cost = list(IVC1 = 1283.250000, IVC2_HVO = 2370.684677, IVC13a = 1471.244445),

    alpha = list(
      IVC1 = c(feed = 0.779271, capex = 0.064680, opex = 0.156049),
      IVC2_HVO = c(feed = 0.758403, capex = 0.113469, opex = 0.128128),
      IVC13a = c(feed = 0.156157, capex = 0.547156, opex = 0.296688)
    ),

    dist_feed = list(
      IVC1 = c(food_bev_imp = 1.000000),
      IVC2_HVO = c(agriculture = 0.054751, adv_biodiesel = 0.803462, chemicals = 0.141787),
      IVC13a = c(agriculture = 0.631062, forestry = 0.167348, paper = 0.166531, food_bev = 0.011138, sewerage = 0.023920)
    )
  ),

  adv_biogasoline = list(
    abs_market_value = 1263933076.99,

    weights = c(IVC5 = 0.367334, IVC12 = 0.447838, IVC13a = 0.164640, IVC13b_road = 0.020187),

    prod_cost = list(IVC5 = 1286.581514, IVC12 = 1166.350000, IVC13a = 1471.244445, IVC13b_road = 1547.615646),

    alpha = list(
      IVC5 = c(feed = 0.267827, capex = 0.431376, opex = 0.300797),
      IVC12 = c(feed = 0.508938, capex = 0.238350, opex = 0.252711),
      IVC13a = c(feed = 0.156157, capex = 0.547156, opex = 0.296688),
      IVC13b_road = c(feed = 0.148610, capex = 0.442616, opex = 0.408774)
    ),

    dist_feed = list(
      IVC5 = c(agriculture = 0.825048, paper = 0.174952),
      IVC12 = c(adv_biogas = 1.000000),
      IVC13a = c(agriculture = 0.631062, forestry = 0.167348, paper = 0.166531, food_bev = 0.011138, sewerage = 0.023920, adv_biogas = 0.000000),
      IVC13b_road = c(agriculture = 0.635454, forestry = 0.165197, paper = 0.164679, food_bev = 0.011007, sewerage = 0.023663, adv_biogas = 0.000000)
    )
  ),

  adv_biogas = list(
    abs_market_value = 4045070410.30,

    weights = c(IVC7 = 1.000000, IVC9a = 0.095640),

    prod_cost = list(IVC7 = 926.533199, IVC9a = 1308.971617),

    alpha = list(
      IVC7 = c(feed = -0.009543, capex = 0.652972, opex = 0.356571),
      IVC9a = c(feed = 0.182564, capex = 0.611167, opex = 0.206269)
    ),

    dist_feed = list(
      IVC7 = c(sewerage = 4.329353, chemicals = -0.214912, food_bev = -0.484699, agriculture = -2.629741),
      IVC9a = c(agriculture = 0.616007, sewerage = 0.025065, food_bev = 0.010844, forestry = 0.151716, paper = 0.146887, chemicals = 0.000000, adv_biodiesel = 0.024740, adv_biogasoline = 0.024740)
    )
  ),

  adv_bio_kerosene = list(
    abs_market_value = 4552687250.96,

    weights = c(IVC2_HEFA = 0.896382, IVC11a_SAF = 0.064955, IVC13b_SAF = 0.038662),

    prod_cost = list(IVC2_HEFA = 2483.258230, IVC11a_SAF = 2778.651297, IVC13b_SAF = 2007.966733),

    alpha = list(
      IVC2_HEFA = c(feed = 0.677641, capex = 0.151414, opex = 0.170945),
      IVC11a_SAF = c(feed = 0.222195, capex = 0.520036, opex = 0.257769),
      IVC13b_SAF = c(feed = 0.343801, capex = 0.341141, opex = 0.315058)
    ),

    dist_feed = list(
      IVC2_HEFA = c(agriculture = 0.269808, adv_biodiesel = 0.310332, chemicals = 0.109529, adv_biogasoline = 0.310332),
      IVC11a_SAF = c(agriculture = 0.644871, forestry = 0.159718, paper = 0.154188, sewerage = 0.026295, adv_biodiesel = 0.003533, food_bev = 0.011396, adv_biogasoline = 0.003533),
      IVC13b_SAF = c(agriculture = 0.657475, forestry = 0.154729, paper = 0.154244, sewerage = 0.022164, food_bev = 0.011388)
    )
  ),

  adv_bio_hfo = list(
    abs_market_value = 2101575325.31,

    weights = c(IVC8a = 0.485836, IVC8b = 0.485836, IVC13b_mar = 0.028329),

    prod_cost = list(IVC8a = 559.784995, IVC8b = 936.000000, IVC13b_mar = 2007.966733),

    alpha = list(
      IVC8a = c(feed = 0.248819, capex = 0.501978, opex = 0.249203),
      IVC8b = c(feed = 0.756410, capex = 0.145299, opex = 0.098291),
      IVC13b_mar = c(feed = 0.343801, capex = 0.341141, opex = 0.315058)
    ),

    dist_feed = list(
      IVC8a = c(agriculture = 0.593056, forestry = 0.146064, paper = 0.141415, sewerage = 0.024132, adv_biodiesel = 0.084894, food_bev = 0.010440, adv_biogasoline = 0.000000),
      IVC8b = c(adv_biogas = 1.000000),
      IVC13b_mar = c(agriculture = 0.657475, forestry = 0.154729, paper = 0.154244, food_bev = 0.011388, sewerage = 0.022164)
    )
  ),

  RFNBOs = list(
    abs_market_value = 1407891428.57,

    weights = c(IVC8c = 0.725212, IVC9b = 0.274788),

    prod_cost = list(IVC8c = 370.619686, IVC9b = 1937.814918),

    alpha = list(
      IVC8c = c(feed = 0.787518, capex = 0.153796, opex = 0.058685),
      IVC9b = c(feed = 0.875504, capex = 0.066054, opex = 0.058442)
    ),

    dist_feed = list(
      IVC8c = c(chemicals = 1.000000),
      IVC9b = c(chemicals = 1.000000)
    )
  ),

  conv_biodiesel = list(
    abs_market_value = 8690668294.15,

    weights = c(IVC_T_FF = 0.421769, IVC_HT_FF = 0.264488, IVC_T_CC = 0.313743),

    prod_cost = list(IVC_T_FF = 800.000000, IVC_HT_FF = 900.000000, IVC_T_CC = 1253.000000),

    alpha = list(
      IVC_T_FF = c(feed = 0.750000, capex = 0.065000, opex = 0.185000),
      IVC_HT_FF = c(feed = 0.666667, capex = 0.111111, opex = 0.222222),
      IVC_T_CC = c(feed = 0.720670, capex = 0.066241, opex = 0.213089)
    ),

    dist_feed = list(
      IVC_T_FF = c(food_bev = 0.800553, food_bev_imp = 0.199446),
      IVC_HT_FF = c(food_bev = 0.800553, food_bev_imp = 0.199446),
      IVC_T_CC = c(agriculture = 1.000000)
    )
  ),

  conv_biogasoline = list(
    abs_market_value = 3640979337.32,

    weights = c(IVC_EF_FF = 1.000000),

    prod_cost = list(IVC_EF_FF = 903.570000),

    alpha = list(
      IVC_EF_FF = c(feed = 0.700000, capex = 0.100000, opex = 0.200000)
    ),

    dist_feed = list(
      IVC_EF_FF = c(agriculture = 1.000000)
    )
  ),

  conv_bio_kerosene = list(
    abs_market_value = 4238566319.66,

    weights = c(IVC_HT_lipids_SAF = 1.000000),

    prod_cost = list(IVC_HT_lipids_SAF = 2903.000000),

    alpha = list(
      IVC_HT_lipids_SAF = c(feed = 0.675508, capex = 0.129521, opex = 0.194971)
    ),

    dist_feed = list(
      IVC_HT_lipids_SAF = c(food_bev = 0.720000, food_bev_imp = 0.280000)
    )
  ),

)

 
 
# ==========================================================
# FINISHED BIOFUEL IMPORTS / EXPORTS - S1 2040
# TODO: not present in Providing_sectors.xlsx - fill in from
# the same source used for S1_imports_2030 / S1_imports_2035.
# ==========================================================
 
 S1_imports_2040 <- c(
   conv_biodiesel    = 0,
   adv_biodiesel     = 18362241739.48,
   conv_biogasoline  = 0,
   adv_biogasoline   = 955687153.39,
   conv_bio_kerosene = 0,
  adv_bio_kerosene  = 11040737556.26,
   adv_bio_hfo       = 11067382314.28,
   RFNBOs            =  29197359699.92,
   adv_biogas        = 0
 )

 S1_exports_2040 <-
   setNames(
     rep(0, length(BIOFUEL_SECTORS)),
     names(BIOFUEL_SECTORS)
   )
 
 
#####################################################################################
# SCENARIO 2 - 2040
#####################################################################################
 
# Derived from the "2040 Sc 2" sheet in Providing_sectors.xlsx.
#
# DATA CAVEATS (from the source workbook, not introduced here):
#  - conv_biodiesel and conv_bio_kerosene are almost entirely #REF! in this
#    sheet (broken formula references). abs_market_value and weights are left
#    as NA below - the sheet needs to be fixed at the source before these can
#    be populated. prod_cost/alpha/dist_feed shown use the same fixed
#    conventional-technology values as elsewhere for reference only.
#  - adv_bio_kerosene IVC6 has a market value but no cost breakdown ("n-d-" in
#    the sheet) and is excluded from the fuel's weights/total here, consistent
#    with the sheet's own TOTAL row.
#  - conv_biogasoline is legitimately zero in this scenario (not broken).
 
S2_2040 <- list(

  adv_biodiesel = list(
    abs_market_value = 17322238764.89,

    weights = c(IVC2_HVO = 0.031159, IVC11a_road = 0.633301, IVC13a = 0.335539),

    prod_cost = list(IVC2_HVO = 2328.318868, IVC11a_road = 2515.879822, IVC13a = 1471.360538),

    alpha = list(
      IVC2_HVO = c(feed = 0.754007, capex = 0.115534, opex = 0.130459),
      IVC11a_road = c(feed = 0.140957, capex = 0.574352, opex = 0.284692),
      IVC13a = c(feed = 0.156223, capex = 0.547113, opex = 0.296664)
    ),

    dist_feed = list(
      IVC2_HVO = c(agriculture = 0.071728, adv_biodiesel = 0.394515, chemicals = 0.139241, adv_biogasoline = 0.394515),
      IVC11a_road = c(agriculture = 0.595247, forestry = 0.113460, paper = 0.133678, food_bev = 0.010277, sewerage = 0.022231, adv_biodiesel = 0.062554, adv_biogasoline = 0.062554),
      IVC13a = c(agriculture = 0.664934, forestry = 0.135759, paper = 0.164764, food_bev = 0.011454, sewerage = 0.023090)
    )
  ),

  adv_biogasoline = list(
    abs_market_value = 5775324475.38,

    weights = c(IVC5 = 0.256220, IVC12 = 0.256220, IVC13a = 0.289156, IVC13b_road = 0.198405),

    prod_cost = list(IVC5 = 1287.756944, IVC12 = 1166.350000, IVC13a = 1471.360538, IVC13b_road = 1547.775937),

    alpha = list(
      IVC5 = c(feed = 0.268495, capex = 0.430982, opex = 0.300523),
      IVC12 = c(feed = 0.508938, capex = 0.238350, opex = 0.252711),
      IVC13a = c(feed = 0.156223, capex = 0.547113, opex = 0.296664),
      IVC13b_road = c(feed = 0.148698, capex = 0.442571, opex = 0.408732)
    ),

    dist_feed = list(
      IVC5 = c(agriculture = 0.835477, paper = 0.164523),
      IVC12 = c(adv_biogas = 1.000000),
      IVC13a = c(agriculture = 0.664934, forestry = 0.135759, paper = 0.164764, food_bev = 0.011454, sewerage = 0.023090, adv_biogas = 0.000000),
      IVC13b_road = c(agriculture = 0.669358, forestry = 0.133945, paper = 0.162603, food_bev = 0.011290, sewerage = 0.022804, adv_biogas = 0.000000)
    )
  ),

  adv_biogas = list(
    abs_market_value = 10947031499.09,

    weights = c(IVC7 = 0.702635, IVC9a = 0.297365),

    prod_cost = list(IVC7 = 929.114005, IVC9a = 1310.967643),

    alpha = list(
      IVC7 = c(feed = -0.006739, capex = 0.651158, opex = 0.355581),
      IVC9a = c(feed = 0.183809, capex = 0.610236, opex = 0.205955)
    ),

    dist_feed = list(
      IVC7 = c(sewerage = 5.930618, chemicals = -0.256007, food_bev = -0.712993, agriculture = -3.961618),
      IVC9a = c(agriculture = 0.647160, sewerage = 0.024154, food_bev = 0.011136, forestry = 0.123106, paper = 0.145092, chemicals = 0.000000, adv_biodiesel = 0.024676, adv_biogasoline = 0.024676)
    )
  ),

  adv_bio_kerosene = list(
    abs_market_value = 15416913779.75,

    weights = c(IVC2_HEFA = 0.140481, IVC6 = 0.257227, IVC11a_SAF = 0.346789, IVC13b_SAF = 0.512730),

    prod_cost = list(IVC2_HEFA = 2431.853820, IVC6 = 2513.829787, IVC11a_SAF = 2783.965415, IVC13b_SAF = 2010.762832),

    alpha = list(
      IVC2_HEFA = c(feed = 0.670827, capex = 0.154615, opex = 0.174558),
      IVC6 = c(feed = 0.423191, capex = 0.298350, opex = 0.278460),
      IVC11a_SAF = c(feed = 0.223679, capex = 0.519044, opex = 0.257277),
      IVC13b_SAF = c(feed = 0.344714, capex = 0.340667, opex = 0.314619)
    ),

    dist_feed = list(
      IVC2_HEFA = c(agriculture = 0.331911, adv_biodiesel = 0.283938, chemicals = 0.100213, adv_biogasoline = 0.283938),
      IVC6 = c(adv_bio_hfo = 1.000000),
      IVC11a_SAF = c(agriculture = 0.677974, forestry = 0.129228, paper = 0.152256, sewerage = 0.025321, adv_biodiesel = 0.003516, food_bev = 0.011705, adv_biogasoline = 0.003516),
      IVC13b_SAF = c(agriculture = 0.690241, forestry = 0.125038, paper = 0.151791, sewerage = 0.021288, food_bev = 0.011642)
    )
  ),

  adv_bio_hfo = list(
    abs_market_value = 16769402806.91,

    weights = c(IVC8a = 0.258212, IVC8b = 0.582351, IVC13b_mar = 0.159437),

    prod_cost = list(IVC8a = 560.934836, IVC8b = 936.000000, IVC13b_mar = 2010.762832),

    alpha = list(
      IVC8a = c(feed = 0.250359, capex = 0.500949, opex = 0.248692),
      IVC8b = c(feed = 0.459319, capex = 0.270340, opex = 0.270340),
      IVC13b_mar = c(feed = 0.344714, capex = 0.340667, opex = 0.314619)
    ),

    dist_feed = list(
      IVC8a = c(agriculture = 0.623109, forestry = 0.118531, paper = 0.139700, sewerage = 0.023256, adv_biodiesel = 0.084682, food_bev = 0.010722, adv_biogasoline = 0.000000),
      IVC8b = c(adv_biogas = 1.000000),
      IVC13b_mar = c(agriculture = 0.690241, forestry = 0.125038, paper = 0.151791, food_bev = 0.011642, sewerage = 0.021288)
    )
  ),

  RFNBOs = list(
    abs_market_value = 17244435024.22,

    weights = c(IVC8c = 0.414219, IVC9b = 0.585781),

    prod_cost = list(IVC8c = 370.619686, IVC9b = 1937.814918),

    alpha = list(
      IVC8c = c(feed = 0.787518, capex = 0.153796, opex = 0.058685),
      IVC9b = c(feed = 0.875504, capex = 0.066054, opex = 0.058442)
    ),

    dist_feed = list(
      IVC8c = c(chemicals = 1.000000),
      IVC9b = c(chemicals = 1.000000)
    )
  ),

  conv_biodiesel = list(
    abs_market_value = 13777273858.11,

    weights = c(IVC_T_FF = 0.158169, IVC_HT_FF = 0.187664, IVC_T_CC = 0.089475, IVC_T_lipids = 0.564692),

    prod_cost = list(IVC_T_FF = 800.000000, IVC_HT_FF = 900.000000, IVC_T_CC = 1253.000000, IVC_T_lipids = 1253.000000),

    alpha = list(
      IVC_T_FF = c(feed = 0.750000, capex = 0.065000, opex = 0.185000),
      IVC_HT_FF = c(feed = 0.666667, capex = 0.111111, opex = 0.222222),
      IVC_T_CC = c(feed = 0.720670, capex = 0.066241, opex = 0.213089),
      IVC_T_lipids = c(feed = 0.720670, capex = 0.066241, opex = 0.213089)
    ),

    dist_feed = list(
      IVC_T_FF = c(food_bev = 0.800553, food_bev_imp = 0.199446),
      IVC_HT_FF = c(food_bev = 0.800553, food_bev_imp = 0.199446),
      IVC_T_CC = c(agriculture = 1.000000),
      IVC_T_lipids = c(food_bev = 0.720000, food_bev_imp = 0.280000)
    )
  ),

  conv_biogasoline = list(
    # No conv_biogasoline production reported in this scenario's source sheet.
    abs_market_value = 0,
    weights = c(IVC_EF_FF = 1.000000),
    dist_feed = list(IVC_EF_FF = c(agriculture = 1.000000))
  ),

  conv_bio_kerosene = list(
    abs_market_value = 2475163914.40,

    weights = c(IVC_HT_lipids_SAF = 1.000000),

    prod_cost = list(IVC_HT_lipids_SAF = 2903.000000),

    alpha = list(
      IVC_HT_lipids_SAF = c(feed = 0.675508, capex = 0.129521, opex = 0.194971)
    ),

    dist_feed = list(
      IVC_HT_lipids_SAF = c(food_bev = 0.720000, food_bev_imp = 0.280000)
    )
  ),

)

 
 
# ==========================================================
# FINISHED BIOFUEL IMPORTS / EXPORTS - S2 2040
# TODO: not present in Providing_sectors.xlsx - fill in from
# the same source used for S2_imports_2030 / S2_imports_2035.
# ==========================================================
 
 S2_imports_2040 <- c(
   conv_biodiesel    = 0,
   adv_biodiesel     = 0,
   conv_biogasoline  = 0,
   adv_biogasoline   = 0,
   conv_bio_kerosene = 0,
   adv_bio_kerosene  = 0,
   adv_bio_hfo       = 0,
   RFNBOs            = 13360816104.27,
   adv_biogas        = 0
 )

 S2_exports_2040 <-
   setNames(
     rep(0, length(BIOFUEL_SECTORS)),
     names(BIOFUEL_SECTORS)
   )
 


####################################################################################
# SCENARIO 3 - 2040
#####################################################################################
 
# Derived from the "2040 costs Sc 3" sheet in Providing_sectors.xlsx.
# Unlike the Sc1/Sc2 sheets, this sheet has NO #REF! errors - every total
# below was cross-checked against the sheet's own TOTAL row and matches
# exactly.
#
# NOTES:
#  - adv_bio_hfo: IVC8b has zero production in this scenario (weight = 0),
#    same treatment as "IVC6 has zero production in S3 2035" elsewhere in
#    this script. Its cost structure is kept for reference.
#  - conv_biodiesel/biogasoline/bio-kerosene: ALL conventional biofuel
#    production is reported as zero in this scenario (matches "technical
#    capacity = no limitations", i.e. fully advanced-technology buildout).
#    This is a real zero in the sheet, not a broken formula.
 
S3_2040 <- list(

  adv_biodiesel = list(
    abs_market_value = 18683142916.96,

    weights = c(IVC2_HVO = 0.029966, IVC11a_road = 0.587171, IVC13a = 0.382863),

    prod_cost = list(IVC2_HVO = 2328.318868, IVC11a_road = 2515.879822, IVC13a = 1471.360538),

    alpha = list(
      IVC2_HVO = c(feed = 0.754007, capex = 0.115534, opex = 0.130459),
      IVC11a_road = c(feed = 0.140957, capex = 0.574352, opex = 0.284692),
      IVC13a = c(feed = 0.156223, capex = 0.547113, opex = 0.296664)
    ),

    dist_feed = list(
      IVC2_HVO = c(agriculture = 0.071728, adv_biodiesel = 0.394515, chemicals = 0.139241, adv_biogasoline = 0.394515),
      IVC11a_road = c(agriculture = 0.595247, forestry = 0.113460, paper = 0.133678, food_bev = 0.010277, sewerage = 0.022231, adv_biodiesel = 0.062554, adv_biogasoline = 0.062554),
      IVC13a = c(agriculture = 0.664934, forestry = 0.135759, paper = 0.164764, food_bev = 0.011454, sewerage = 0.023090)
    )
  ),

  adv_biogasoline = list(
    abs_market_value = 61137931561.92,

    weights = c(IVC5 = 0.145238, IVC12 = 0.498849, IVC13a = 0.116999, IVC13b_road = 0.238913),

    prod_cost = list(IVC5 = 1287.756944, IVC12 = 1166.350000, IVC13a = 1471.360538, IVC13b_road = 1547.775937),

    alpha = list(
      IVC5 = c(feed = 0.268495, capex = 0.430982, opex = 0.300523),
      IVC12 = c(feed = 0.508938, capex = 0.238350, opex = 0.252711),
      IVC13a = c(feed = 0.156223, capex = 0.547113, opex = 0.296664),
      IVC13b_road = c(feed = 0.148698, capex = 0.442571, opex = 0.408732)
    ),

    dist_feed = list(
      IVC5 = c(agriculture = 0.835477, paper = 0.164523),
      IVC12 = c(adv_biogas = 1.000000),
      IVC13a = c(agriculture = 0.664934, forestry = 0.135759, paper = 0.164764, food_bev = 0.011454, sewerage = 0.023090, adv_biogas = 0.000000),
      IVC13b_road = c(agriculture = 0.669358, forestry = 0.133945, paper = 0.162603, food_bev = 0.011290, sewerage = 0.022804, adv_biogas = 0.000000)
    )
  ),

  adv_biogas = list(
    abs_market_value = 77097984556.13,

    weights = c(IVC7 = 0.727859, IVC9a = 0.272141),

    prod_cost = list(IVC7 = 929.114005, IVC9a = 1310.967643),

    alpha = list(
      IVC7 = c(feed = -0.006739, capex = 0.651158, opex = 0.355581),
      IVC9a = c(feed = 0.183809, capex = 0.610236, opex = 0.205955)
    ),

    dist_feed = list(
      IVC7 = c(sewerage = 5.930618, chemicals = -0.256007, food_bev = -0.712993, agriculture = -3.961618),
      IVC9a = c(agriculture = 0.647160, sewerage = 0.024154, food_bev = 0.011136, forestry = 0.123106, paper = 0.145092, chemicals = 0.000000, adv_biodiesel = 0.024676, adv_biogasoline = 0.024676)
    )
  ),

  adv_bio_kerosene = list(
    abs_market_value = 15042940399.52,

    weights = c(IVC2_HEFA = 0.277018, IVC6 = 0.950686, IVC11a_SAF = 0.387473, IVC13b_SAF = 0.335508),

    prod_cost = list(IVC2_HEFA = 2431.853820, IVC6 = 2513.829787, IVC11a_SAF = 2783.965415, IVC13b_SAF = 2010.762832),

    alpha = list(
      IVC2_HEFA = c(feed = 0.670827, capex = 0.154615, opex = 0.174558),
      IVC6 = c(feed = 0.423191, capex = 0.298350, opex = 0.278460),
      IVC11a_SAF = c(feed = 0.223679, capex = 0.519044, opex = 0.257277),
      IVC13b_SAF = c(feed = 0.344714, capex = 0.340667, opex = 0.314619)
    ),

    dist_feed = list(
      IVC2_HEFA = c(agriculture = 0.331911, adv_biodiesel = 0.567876, chemicals = 0.100213),
      IVC6 = c(adv_bio_hfo = 1.000000),
      IVC11a_SAF = c(agriculture = 0.677974, forestry = 0.129228, paper = 0.152256, sewerage = 0.025321, adv_biodiesel = 0.003516, food_bev = 0.011705, adv_biogasoline = 0.003516),
      IVC13b_SAF = c(agriculture = 0.690241, forestry = 0.125038, paper = 0.151791, sewerage = 0.021288, food_bev = 0.011642)
    )
  ),

  adv_bio_hfo = list(
    abs_market_value = 68523882426.01,

    weights = c(IVC8a = 0.392283, IVC8b = 0.572128, IVC13b_mar = 0.035589),

    prod_cost = list(IVC8a = 560.934836, IVC8b = 936.000000, IVC13b_mar = 2010.762832),

    alpha = list(
      IVC8a = c(feed = 0.250359, capex = 0.500949, opex = 0.248692),
      IVC8b = c(feed = 0.459319, capex = 0.270340, opex = 0.270340),
      IVC13b_mar = c(feed = 0.344714, capex = 0.340667, opex = 0.314619)
    ),

    dist_feed = list(
      IVC8a = c(agriculture = 0.623109, forestry = 0.118531, paper = 0.139700, sewerage = 0.023256, adv_biodiesel = 0.084682, food_bev = 0.010722, adv_biogasoline = 0.000000),
      IVC8b = c(adv_biogas = 1.000000),
      IVC13b_mar = c(agriculture = 0.690241, forestry = 0.125038, paper = 0.151791, food_bev = 0.011642, sewerage = 0.021288)
    )
  ),

  RFNBOs = list(
    abs_market_value = 26331716057.67,

    weights = c(IVC8c = 0.577244, IVC9b = 0.422756),

    prod_cost = list(IVC8c = 370.619686, IVC9b = 1937.814918),

    alpha = list(
      IVC8c = c(feed = 0.787518, capex = 0.153796, opex = 0.058685),
      IVC9b = c(feed = 0.875504, capex = 0.066054, opex = 0.058442)
    ),

    dist_feed = list(
      IVC8c = c(chemicals = 1.000000),
      IVC9b = c(chemicals = 1.000000)
    )
  ),

  conv_biodiesel = list(
    abs_market_value = 12554667358.04,

    weights = c(IVC_T_FF = 0.173572, IVC_HT_FF = 0.205940, IVC_HT_CC = 0.082567, IVC_T_lipids = 0.537921),

    prod_cost = list(IVC_T_FF = 800.000000, IVC_HT_FF = 900.000000, IVC_HT_CC = 1203.000000, IVC_T_lipids = 1253.000000),

    alpha = list(
      IVC_T_FF = c(feed = 0.750000, capex = 0.065000, opex = 0.185000),
      IVC_HT_FF = c(feed = 0.666667, capex = 0.111111, opex = 0.222222),
      IVC_HT_CC = c(feed = 0.750623, capex = 0.083126, opex = 0.166251),
      IVC_T_lipids = c(feed = 0.720670, capex = 0.066241, opex = 0.213089)
    ),

    dist_feed = list(
      IVC_T_FF = c(food_bev = 0.800553, food_bev_imp = 0.199446),
      IVC_HT_FF = c(food_bev = 0.800553, food_bev_imp = 0.199446),
      IVC_HT_CC = c(agriculture = 1.000000),
      IVC_T_lipids = c(food_bev = 0.720000, food_bev_imp = 0.280000)
    )
  ),

)

 
 
# ==========================================================
# FINISHED BIOFUEL IMPORTS / EXPORTS - S3 2040
# TODO: not present in Providing_sectors.xlsx - fill in from
# the same source used for S3_imports_2030 / S3_imports_2035.
# ==========================================================
 
 S3_imports_2040 <- c(
   conv_biodiesel    = 0,
   adv_biodiesel     = 0,
   conv_biogasoline  = 0,
   adv_biogasoline   = 0,
   conv_bio_kerosene = 0,
   adv_bio_kerosene  = 0,
   adv_bio_hfo       = 0,
   RFNBOs            = 0,
  adv_biogas        = 0
 )

 S3_exports_2040 <- c(
   conv_biodiesel    = 0,
   adv_biodiesel     = 0,
   conv_biogasoline  = 0,
   adv_biogasoline   = 48875983085.77,
   conv_bio_kerosene = 0,
   adv_bio_kerosene  = 4909200712.67,
   adv_bio_hfo       = 37422924814.15,
   RFNBOs            = 949431082.25,
   adv_biogas        = 26379203962.07
 )
 

# ===================================================================
# BUILD IMPORTED FINAL CONSUMPTION EXPENDITURE VECTOR
# ===================================================================

build_Y_imp_FCE <- function(import_cfg_eur) {
  
  # Start from observed 2023 imported final consumption expenditure
  Y_imp_FCE_target <- t_f_e_imp
  
  # Replace biofuel rows with scenario-specific finished-fuel imports
  for (fuel_name in names(import_cfg_eur)) {
    
    if (!fuel_name %in% names(BIOFUEL_SECTORS)) {
      stop(
        paste(
          "Unknown biofuel in import configuration:",
          fuel_name
        )
      )
    }
    
    sector_idx <- BIOFUEL_SECTORS[[fuel_name]]
    
    Y_imp_FCE_target[sector_idx] <-
      import_cfg_eur[[fuel_name]] /
      SCENARIO_EUR_TO_IO_UNIT
  }
  
  Y_imp_FCE_target
}

# ===================================================================
# BUILD BIOFUEL EXPORT VECTOR
# ===================================================================

build_exports <- function(
    export_cfg_eur,
    base_exports
) {

  # Keep non-biofuel exports at observed 2023 values
  exports_target <- base_exports

  # Replace only biofuel exports with scenario assumptions
  for (fuel_name in names(export_cfg_eur)) {

    if (!fuel_name %in% names(BIOFUEL_SECTORS)) {
      stop(
        paste(
          "Unknown biofuel in export configuration:",
          fuel_name
        )
      )
    }

    sector_idx <- BIOFUEL_SECTORS[[fuel_name]]

    exports_target[sector_idx] <-
      export_cfg_eur[[fuel_name]] /
      SCENARIO_EUR_TO_IO_UNIT
  }

  exports_target
}








# ===================================================================
# 6. BUILD EXOGENOUS TARGET CONDITIONS FOR ONE SCENARIO
# ===================================================================

build_endpoint <- function(
    scenario_cfg,
    import_cfg_eur,
    export_cfg_eur
) {
  
  
  # Biofuel sectors
  BIO <- unname(BIOFUEL_SECTORS)
  
  
  # ---------------------------------------------------------------
  # 6.1. START FROM 2023 TECHNICAL COEFFICIENTS
  # ---------------------------------------------------------------
  
  # Non-biofuel technology remains at its 2023 Eurostat structure.
  # Biofuel columns are replaced below where scenario assumptions exist.
  
A_dom_target <-
  BASE_2023$A_dom

A_imp_target <-
  BASE_2023$A_imp

# OPTION A - CAPEX investment coefficients (separate from A_dom/A_imp).
# There is no 2023 equivalent (2023 is observed Eurostat data, not a
# scenario built from IVC technology), so these start at zero for every
# sector. Only the BIO columns of biofuel sectors present in this
# scenario are filled in below.
A_capex_dom_target <-
  matrix(
    0,
    nrow = nIndustries,
    ncol = nIndustries,
    dimnames = dimnames(BASE_2023$A_dom)
  )

A_capex_imp_target <-
  matrix(
    0,
    nrow = nIndustries,
    ncol = nIndustries,
    dimnames = dimnames(BASE_2023$A_imp)
  )

  # ---------------------------------------------------------------
  # 6.2. EXOGENOUS DOMESTIC BIOFUEL OUTPUT
  # ---------------------------------------------------------------
  
  # Start from observed 2023 domestic output of the biofuel sectors.
  # Scenario-specific biofuel sectors are replaced below.
  
 X_bio_target <-
  BASE_2023$X_fuel[BIO]
  
  
  # ---------------------------------------------------------------
  # 6.3. EXOGENOUS IMPORTS OF FINISHED BIOFUELS
  # ---------------------------------------------------------------
  
  Y_imp_FCE_target <-
    build_Y_imp_FCE(import_cfg_eur)
# ---------------------------------------------------------------
# EXOGENOUS BIOFUEL EXPORTS
# ---------------------------------------------------------------

exports_target <-
  build_exports(
    export_cfg_eur,
    BASE_2023$exports
  )
 # ---------------------------------------------------------------
  # 6.4.DERIVED GATE-FEE REVENUE COEFFICIENT --> NOT an independent sceanrio assumption; but derived form the scenario-specific  IVC technology mix
  # ---------------------------------------------------------------
# Gate-fee revenue coefficient per unit of sector output.
# Zero for sectors without such revenues.

gate_fee_coeff_target <-
  numeric(nIndustries)
  
  
  # ---------------------------------------------------------------
  # 6.4. APPLY SCENARIO-SPECIFIC BIOFUEL ASSUMPTIONS
  # ---------------------------------------------------------------
  
  for (fuel_name in names(scenario_cfg)) {
    
    if (!fuel_name %in% names(BIOFUEL_SECTORS)) next
    
    
    target_col <- BIOFUEL_SECTORS[[fuel_name]]
    fuel_cfg   <- scenario_cfg[[fuel_name]]

sourcing_group <-
  FUEL_TECH_GROUP[[fuel_name]]

if (is.null(sourcing_group)) {
  stop(
    paste(
      "No sourcing group defined for",
      fuel_name
    )
  )
}
    
    # -------------------------------------------------------------
    # 6.4a. EXOGENOUS DOMESTIC BIOFUEL OUTPUT
    # -------------------------------------------------------------
    
    X_bio_target[
      match(target_col, BIO)
    ] <-
      fuel_cfg$abs_market_value /
      SCENARIO_EUR_TO_IO_UNIT
    
    
    # -------------------------------------------------------------
    # 6.4b. SCENARIO-SPECIFIC BIOFUEL OPERATING TECHNOLOGY
    # -------------------------------------------------------------
    # build_fuel_column() now returns feedstock + OPEX only.
    # CAPEX is not inserted into A_dom_target / A_imp_target.
    # -------------------------------------------------------------
    
 res_fuel <-
  build_fuel_column(
    fuel_cfg = fuel_cfg,
    sourcing_group = sourcing_group
  )
    
    gate_fee_coeff_target[target_col] <-
  res_fuel$gate_fee_coeff

    # Replace the old Eurostat technology column for this biofuel.
    A_dom_target[, target_col] <- 0
    A_imp_target[, target_col] <- 0
    
    
    # Domestic intermediate input coefficients
    for (sec_name in names(res_fuel$a_dom)) {
      
      if (sec_name %in% names(INPUT_SECTORS)) {
        
        row_idx <- INPUT_SECTORS[[sec_name]]
        
        A_dom_target[row_idx, target_col] <-
          res_fuel$a_dom[[sec_name]]
      }
    }
    
    
    # Imported intermediate input coefficients
    for (imp_sec in names(res_fuel$a_imp)) {
      
      clean_sec <- gsub("_imp$", "", imp_sec)
      
      if (clean_sec %in% names(INPUT_SECTORS)) {
        
        row_idx <- INPUT_SECTORS[[clean_sec]]
        
        A_imp_target[row_idx, target_col] <-
          res_fuel$a_imp[[imp_sec]]
      }
    }


    # -------------------------------------------------------------
    # OPTION A - CAPEX investment coefficients (kept in their own
    # matrices, NEVER written into A_dom_target / A_imp_target).
    # -------------------------------------------------------------

    A_capex_dom_target[, target_col] <- 0
    A_capex_imp_target[, target_col] <- 0

    # Domestic CAPEX (investment-good) coefficients
    for (sec_name in names(res_fuel$a_capex_dom)) {

      if (sec_name %in% names(INPUT_SECTORS)) {

        row_idx <- INPUT_SECTORS[[sec_name]]

        A_capex_dom_target[row_idx, target_col] <-
          res_fuel$a_capex_dom[[sec_name]]
      }
    }

    # Imported CAPEX (investment-good) coefficients
    for (imp_sec in names(res_fuel$a_capex_imp)) {

      clean_sec <- gsub("_imp$", "", imp_sec)

      if (clean_sec %in% names(INPUT_SECTORS)) {

        row_idx <- INPUT_SECTORS[[clean_sec]]

        A_capex_imp_target[row_idx, target_col] <-
          res_fuel$a_capex_imp[[imp_sec]]
      }
    }
  }
  
  
  # ---------------------------------------------------------------
  # 6.5. RETURN EXOGENOUS TARGET CONDITIONS ONLY
  # ---------------------------------------------------------------
  
  # X for NONBIO sectors, Z_dom, Z_imp, Y_dom, P2_ADJ and GVA are
  # NOT calculated here. They are endogenous outcomes calculated
  # annually in run_dynamic_scenario().
  
  list(
    A_dom = A_dom_target,
    A_imp = A_imp_target,
    A_capex_dom = A_capex_dom_target,
    A_capex_imp = A_capex_imp_target,
    X_bio = X_bio_target,
    Y_imp_FCE = Y_imp_FCE_target,
    exports = exports_target,
    gate_fee_coeff = gate_fee_coeff_target
  )
}


# ===================================================================
# 7. Common 2023 starting point derived from IOT
# ===================================================================

# Sector indices for gate-fee treatment
ADV_BIOGAS <-
  BIOFUEL_SECTORS[["adv_biogas"]]

SEWERAGE <-
  INPUT_SECTORS[["sewerage"]]

# -------------------------------------------------------------------
# Observed 2023 quantities
# -------------------------------------------------------------------

X_BASE_2023 <- q_s_dom

Z_dom_BASE_2023 <- Z_dom
Z_imp_BASE_2023 <- Z_imp


exports_BASE_2023 <- as.numeric(
  unlist(
    IO_EU_domestic[
      2:(nIndustries + 1),
      nIndustries + 9
    ]
  )
)



# -------------------------------------------------------------------
# Observed 2023 gate-fee revenue
# -------------------------------------------------------------------

gate_fee_revenue_BASE_2023 <-
  Z_dom_BASE_2023[
    ADV_BIOGAS,
    SEWERAGE
  ]


# Fuel market value excludes gate-fee revenue.
# Total observed output X_BASE_2023 includes it.
X_fuel_BASE_2023 <-
  X_BASE_2023

X_fuel_BASE_2023[ADV_BIOGAS] <-
  X_BASE_2023[ADV_BIOGAS] -
  gate_fee_revenue_BASE_2023


# -------------------------------------------------------------------
# 2023 technical coefficients
# -------------------------------------------------------------------

# These are the actual monetary IO coefficients:
# A_IO = Z / total accounting output X.
# They therefore remain fully consistent with observed Z and X.
# -------------------------------------------------------------------
# Actual monetary 2023 IO coefficients
# -------------------------------------------------------------------
# These are based on total accounting output X and therefore satisfy:
# Z = A_IO %*% diag(X)

A_dom_IO_BASE_2023 <- A_dom
A_imp_IO_BASE_2023 <- A_imp


# -------------------------------------------------------------------
# 2023 technology coefficients
# -------------------------------------------------------------------
# These are used to generate technical intermediate requirements.
# For advanced biogas they are expressed per unit of fuel market value,
# excluding gate-fee revenue.

A_dom_BASE_2023 <- A_dom
A_imp_BASE_2023 <- A_imp

# For advanced biogas, technical coefficients must be expressed
# per unit of fuel market value, excluding gate-fee revenue.
A_dom_BASE_2023[, ADV_BIOGAS] <-
  Z_dom_BASE_2023[, ADV_BIOGAS] /
  X_fuel_BASE_2023[ADV_BIOGAS]

A_imp_BASE_2023[, ADV_BIOGAS] <-
  Z_imp_BASE_2023[, ADV_BIOGAS] /
  X_fuel_BASE_2023[ADV_BIOGAS]

# Gate-fee transaction is monetary accounting,
# not a Leontief technical coefficient.
A_dom_BASE_2023[
  ADV_BIOGAS,
  SEWERAGE
] <- 0


# -------------------------------------------------------------------
# Observed 2023 gate-fee coefficient
# -------------------------------------------------------------------

gate_fee_coeff_BASE_2023 <-
  numeric(nIndustries)

if (X_fuel_BASE_2023[ADV_BIOGAS] != 0) {

  gate_fee_coeff_BASE_2023[ADV_BIOGAS] <-
    gate_fee_revenue_BASE_2023 /
    X_fuel_BASE_2023[ADV_BIOGAS]
}




# -------------------------------------------------------------------
# Remaining 2023 accounting quantities
# -------------------------------------------------------------------

tax_products_BASE_2023 <-
  tax_products_2023

Y_dom_BASE_2023 <-
  X_BASE_2023 -
  rowSums(Z_dom_BASE_2023)

Y_domestic_final_BASE_2023 <-
  Y_dom_BASE_2023 -
  exports_BASE_2023

P2_ADJ_BASE_2023 <-
  colSums(Z_dom_BASE_2023) +
  colSums(Z_imp_BASE_2023) +
  tax_products_BASE_2023

GVA_BASE_2023 <-
  X_BASE_2023 -
  P2_ADJ_BASE_2023


# Leontief inverse based on the actual monetary IO coefficient matrix
L_dom_BASE_2023 <-
  solve(
    base::diag(nIndustries) -
      A_dom_IO_BASE_2023
  )



# -------------------------------------------------------------------
# OPTION A - real 2023 CAPEX investment coefficients
# -------------------------------------------------------------------
# Derived from the '2023' sheet of 2023_REF_plus_2030_all_S.xlsx
# (feedstock/CAPEX/OPEX breakdown per IVC: adv_biodiesel, adv_biogasoline,
# adv_biogas, conv_biodiesel, conv_biogasoline/ethanol), aggregated per
# biofuel product as: sum(Absolute CAPEX per Eurostat sector) / (total
# 2023 market value of that biofuel product), then split into
# domestic/import shares via the same CAPEX_IMPORT_SHARES used for
# scenario years. Unlike scenario endpoints (2030+), which get their
# A_capex_dom/A_capex_imp from the IVC technology library via
# build_ivc_vector(), 2023 is the observed base year and does not run
# through that pipeline - these coefficients are therefore computed
# directly here, once, from the 2023 IVC-level cost data.
#
# conv_bio_kerosene, adv_bio_kerosene, adv_bio_hfo and RFNBOs had zero
# 2023 production and are left at zero (no CAPEX data to derive from).

A_capex_dom_BASE_2023 <-
  matrix(
    0,
    nrow = nIndustries,
    ncol = nIndustries,
    dimnames = dimnames(A_dom_BASE_2023)
  )

A_capex_imp_BASE_2023 <-
  matrix(
    0,
    nrow = nIndustries,
    ncol = nIndustries,
    dimnames = dimnames(A_imp_BASE_2023)
  )

# --- adv_biodiesel (column 13) ---
A_capex_dom_BASE_2023[24, 13] <- 0.00223515  # fab_metal
A_capex_dom_BASE_2023[26, 13] <- 0.00133829  # elec_equip
A_capex_dom_BASE_2023[27, 13] <- 0.02007431  # machinery
A_capex_dom_BASE_2023[36, 13] <- 0.01077598  # construction
A_capex_dom_BASE_2023[55, 13] <- 0.02192837  # architecture
A_capex_dom_BASE_2023[25, 13] <- 0.00066914  # computer_el
A_capex_imp_BASE_2023[24, 13] <- 0.00223515  # fab_metal_imp
A_capex_imp_BASE_2023[26, 13] <- 0.00014870  # elec_equip_imp
A_capex_imp_BASE_2023[27, 13] <- 0.01338287  # machinery_imp
A_capex_imp_BASE_2023[25, 13] <- 0.00156134  # computer_el_imp

# --- adv_biogasoline (column 15) ---
A_capex_dom_BASE_2023[24, 15] <- 0.00510384  # fab_metal
A_capex_dom_BASE_2023[26, 15] <- 0.00973703  # elec_equip
A_capex_dom_BASE_2023[27, 15] <- 0.16024225  # machinery
A_capex_dom_BASE_2023[36, 15] <- 0.07773933  # construction
A_capex_dom_BASE_2023[55, 15] <- 0.15888123  # architecture
A_capex_dom_BASE_2023[25, 15] <- 0.00486851  # computer_el
A_capex_imp_BASE_2023[24, 15] <- 0.00510384  # fab_metal_imp
A_capex_imp_BASE_2023[26, 15] <- 0.00108189  # elec_equip_imp
A_capex_imp_BASE_2023[27, 15] <- 0.10682817  # machinery_imp
A_capex_imp_BASE_2023[25, 15] <- 0.01135987  # computer_el_imp

# --- adv_biogas (column 33) ---
A_capex_dom_BASE_2023[24, 33] <- 0.01186275  # fab_metal
A_capex_dom_BASE_2023[26, 33] <- 0.00854118  # elec_equip
A_capex_dom_BASE_2023[27, 33] <- 0.11388235  # machinery
A_capex_dom_BASE_2023[36, 33] <- 0.14235294  # construction
A_capex_dom_BASE_2023[55, 33] <- 0.09490196  # architecture
A_capex_dom_BASE_2023[25, 33] <- 0.00427059  # computer_el
A_capex_imp_BASE_2023[24, 33] <- 0.01186275  # fab_metal_imp
A_capex_imp_BASE_2023[26, 33] <- 0.00094902  # elec_equip_imp
A_capex_imp_BASE_2023[27, 33] <- 0.07592157  # machinery_imp
A_capex_imp_BASE_2023[25, 33] <- 0.00996471  # computer_el_imp

# --- conv_biodiesel (column 12) ---
A_capex_dom_BASE_2023[36, 12] <- 0.00748975  # construction
A_capex_dom_BASE_2023[24, 12] <- 0.00441749  # fab_metal
A_capex_dom_BASE_2023[25, 12] <- 0.00119571  # computer_el
A_capex_imp_BASE_2023[24, 12] <- 0.00023250  # fab_metal_imp
A_capex_imp_BASE_2023[25, 12] <- 0.00079714  # computer_el_imp

# --- conv_biogasoline / conv_ethanol (column 14) ---
A_capex_dom_BASE_2023[36, 14] <- 0.01330123  # construction
A_capex_dom_BASE_2023[24, 14] <- 0.00589688  # fab_metal
A_capex_dom_BASE_2023[25, 14] <- 0.00159615  # computer_el
A_capex_imp_BASE_2023[24, 14] <- 0.00031036  # fab_metal_imp
A_capex_imp_BASE_2023[25, 14] <- 0.00106410  # computer_el_imp


# -------------------------------------------------------------------
# Collect 2023 baseline
# -------------------------------------------------------------------

BASE_2023 <- list(

  A_dom = A_dom_BASE_2023,
  A_imp = A_imp_BASE_2023,

  # OPTION A - real, IVC-derived 2023 CAPEX investment coefficients
  # (see block above). CAPEX is no longer part of Z_dom_BASE_2023 /
  # Z_imp_BASE_2023 (moved to the real "Gross fixed capital formation"
  # column directly in the source domestic_iot.xlsx / imports_iot.xlsx
  # tables); these coefficients are the separate, X_bio-proportional
  # CAPEX investment demand for 2023, giving build_scenario_driver() a
  # real (non-zero) starting point to interpolate from towards the
  # first scenario endpoint's A_capex_dom/A_capex_imp.
  A_capex_dom = A_capex_dom_BASE_2023,

  A_capex_imp = A_capex_imp_BASE_2023,

  # Actual monetary IO coefficients, consistent with observed Z and X
  A_dom_IO = A_dom_IO_BASE_2023,
  A_imp_IO = A_imp_IO_BASE_2023,

  X = X_BASE_2023,
  X_fuel = X_fuel_BASE_2023,

  # Observed Z still contains the 2023 gate-fee transaction.
  # CAPEX no longer appears here (see A_capex_dom / A_capex_imp above).
  Z_dom = Z_dom_BASE_2023,
  Z_imp = Z_imp_BASE_2023,

  tax_products = tax_products_BASE_2023,
  P2_ADJ = P2_ADJ_BASE_2023,
  GVA = GVA_BASE_2023,

  Y_dom = Y_dom_BASE_2023,
  L_dom = L_dom_BASE_2023,

  gate_fee_coeff = gate_fee_coeff_BASE_2023,

  Y_imp_FCE = t_f_e_imp,
  exports = exports_BASE_2023
)

# ===================================================================
# NEW 7A:  REFERENCE SCENARIO: 2023 -> 2030
# ===================================================================
# Purpose:
# - BASE_2023 is the observed starting point.
# - REF represents a no-biofuel-shock reference path.
# - For now, there is no autonomous growth and no SFC feedback yet.
# - 2023 biofuel production, technologies and finished imports are kept.
# - The SFC behavioral dynamics will be added later to this reference path.
# ===================================================================


# ===================================================================
# a) DECOMPOSE 2023 DOMESTIC FINAL DEMAND
# ===================================================================

# Household + NPISH consumption
C_dom_BASE_2023 <-
  consumption_dom

# Government consumption
G_dom_BASE_2023 <-
  g_dom

# Remaining final demand:
# investment, exports, inventories, etc.
OTHER_dom_BASE_2023 <-
  BASE_2023$Y_dom -
  C_dom_BASE_2023 -
  G_dom_BASE_2023


# Check that the decomposition exactly reproduces 2023 final demand
stopifnot(
  isTRUE(
    all.equal(
      BASE_2023$Y_dom,
      C_dom_BASE_2023 +
        G_dom_BASE_2023 +
        OTHER_dom_BASE_2023
    )
  )
)



# ===================================================================
# 8. BUILD EXOGENOUS SCENARIO ENDPOINTS
# ===================================================================

# These endpoints contain ONLY the exogenous scenario conditions:
#
# - A_dom      technical coefficients for recurrent domestic inputs
# - A_imp      technical coefficients for recurrent imported inputs
# - X_bio      domestic biofuel output
# - Y_imp_FCE  imported finished biofuels
#
# CAPEX is excluded from A_dom and A_imp.
#
# X_NONBIO, total X, Z_dom, Z_imp, Y_dom, P2_ADJ, GVA and L_dom
# are calculated endogenously later in run_dynamic_scenario().

S1_endpoint_2030 <- build_endpoint(
  scenario_cfg = S1_2030,
  import_cfg_eur = S1_imports_2030,
  export_cfg_eur = S1_exports_2030
)

S2_endpoint_2030 <- build_endpoint(
  scenario_cfg = S2_2030,
  import_cfg_eur = S2_imports_2030,
  export_cfg_eur = S2_exports_2030
)

S3_endpoint_2030 <- build_endpoint(
  scenario_cfg = S3_2030,
  import_cfg_eur = S3_imports_2030,
  export_cfg_eur = S3_exports_2030
)

S1_endpoint_2035 <- build_endpoint(
  scenario_cfg   = S1_2035,
  import_cfg_eur = S1_imports_2035,
  export_cfg_eur = S1_exports_2035
)

S2_endpoint_2035 <- build_endpoint(
  scenario_cfg   = S2_2035,
  import_cfg_eur = S2_imports_2035,
  export_cfg_eur = S2_exports_2035
)

S3_endpoint_2035 <- build_endpoint(
  scenario_cfg   = S3_2035,
  import_cfg_eur = S3_imports_2035,
  export_cfg_eur = S3_exports_2035
)

S1_endpoint_2040 <- build_endpoint(
  scenario_cfg   = S1_2040,
  import_cfg_eur = S1_imports_2040,
  export_cfg_eur = S1_exports_2040
)

S2_endpoint_2040 <- build_endpoint(
  scenario_cfg   = S2_2040,
  import_cfg_eur = S2_imports_2040,
  export_cfg_eur = S2_exports_2040
)

S3_endpoint_2040 <- build_endpoint(
  scenario_cfg   = S3_2040,
  import_cfg_eur = S3_imports_2040,
  export_cfg_eur = S3_exports_2040
)




# ===================================================================
# 9. CHECK EXOGENOUS SCENARIO ENDPOINTS
# ===================================================================

BIO <- unname(BIOFUEL_SECTORS)


check_endpoint <- function(endpoint) {

  stopifnot(
    all(dim(endpoint$A_dom) == c(nIndustries, nIndustries)),
    all(dim(endpoint$A_imp) == c(nIndustries, nIndustries)),
    length(endpoint$X_bio) == length(BIO),
    length(endpoint$Y_imp_FCE) == nIndustries,
    length(endpoint$exports) == nIndustries,
    length(endpoint$gate_fee_coeff) == nIndustries,

    !anyNA(endpoint$A_dom),
    !anyNA(endpoint$A_imp),
    !anyNA(endpoint$X_bio),
    !anyNA(endpoint$Y_imp_FCE),
    !anyNA(endpoint$exports),
    !anyNA(endpoint$gate_fee_coeff),

    all(endpoint$gate_fee_coeff >= 0)
  )
}

check_endpoint(S1_endpoint_2030)
check_endpoint(S1_endpoint_2035)
check_endpoint(S1_endpoint_2040)

check_endpoint(S2_endpoint_2030)
check_endpoint(S2_endpoint_2035)
check_endpoint(S2_endpoint_2040)

check_endpoint(S3_endpoint_2030)
check_endpoint(S3_endpoint_2035)
check_endpoint(S3_endpoint_2040)


############################################################################################################################################################
#######  build linear development between 2023 and the three 2030 endpoints - fix only Aimp, Adom and X(BIO) --> build annual EGOGENOUS scenario drivers####
############################################################################################################################################################
####### note Y(NONBIO) and Zdom and Zimp are NOT calculated here, since thery are endoegenously calculated in the loop  ####################################
############################################################################################################################################################



build_scenario_driver <- function(
    start_endpoint,
    end_endpoint,
    start_year,
    end_year
) {

  BIO <- unname(BIOFUEL_SECTORS)

  years <- start_year:end_year
  n_years <- length(years)


  # ---------------------------------------------------------------
  # Storage
  # ---------------------------------------------------------------

  A_dom_path <- array(
    NA_real_,
    dim = c(n_years, nIndustries, nIndustries),
    dimnames = list(
      year = as.character(years),
      input_sector = NULL,
      output_sector = NULL
    )
  )

  A_imp_path <- array(
    NA_real_,
    dim = c(n_years, nIndustries, nIndustries),
    dimnames = list(
      year = as.character(years),
      input_sector = NULL,
      output_sector = NULL
    )
  )

  # OPTION A - CAPEX investment-coefficient path (kept separate from
  # A_dom_path / A_imp_path throughout).
  A_capex_dom_path <- array(
    NA_real_,
    dim = c(n_years, nIndustries, nIndustries),
    dimnames = list(
      year = as.character(years),
      input_sector = NULL,
      output_sector = NULL
    )
  )

  A_capex_imp_path <- array(
    NA_real_,
    dim = c(n_years, nIndustries, nIndustries),
    dimnames = list(
      year = as.character(years),
      input_sector = NULL,
      output_sector = NULL
    )
  )

  # Only BIO output is exogenously prescribed
  X_bio_path <- matrix(
    NA_real_,
    nrow = n_years,
    ncol = nIndustries,
    dimnames = list(
      year = as.character(years),
      sector = NULL
    )
  )

  # Imported finished fuels
  Y_imp_FCE_path <- matrix(
    NA_real_,
    nrow = n_years,
    ncol = nIndustries,
    dimnames = list(
      year = as.character(years),
      sector = NULL
    )
  )

exports_path <- matrix(
  NA_real_,
  nrow = n_years,
  ncol = nIndustries,
  dimnames = list(
    year = as.character(years),
    sector = NULL
  )
)

# Derived gate-fee revenue coefficient
# (not an independent scenario assumption)
gate_fee_coeff_path <- matrix(
  NA_real_,
  nrow = n_years,
  ncol = nIndustries,
  dimnames = list(
    year = as.character(years),
    sector = NULL
  )
)



# ---------------------------------------------------------------
  # BIO OUTPUT AT START AND END POINT
  # ---------------------------------------------------------------

  # BASE_2023 stores the complete 73-sector output vector as $X.
  # Scenario endpoints store only exogenous biofuel output as $X_bio.

 X_bio_start <-
  if (!is.null(start_endpoint$X_bio)) {

    start_endpoint$X_bio

  } else if (!is.null(start_endpoint$X_fuel)) {

    start_endpoint$X_fuel[BIO]

  } else {

    start_endpoint$X[BIO]
  }

 X_bio_end <-
  if (!is.null(end_endpoint$X_bio)) {

    end_endpoint$X_bio

  } else if (!is.null(end_endpoint$X_fuel)) {

    end_endpoint$X_fuel[BIO]

  } else {

    end_endpoint$X[BIO]
  }

exports_start <- start_endpoint$exports
exports_end   <- end_endpoint$exports

  gate_fee_coeff_start <-
    start_endpoint$gate_fee_coeff

  gate_fee_coeff_end <-
    end_endpoint$gate_fee_coeff


  # ---------------------------------------------------------------
  # Annual interpolation
  # ---------------------------------------------------------------

  for (i in seq_along(years)) {

    current_year <- years[i]

    lambda <-
      (current_year - start_year) /
      (end_year - start_year)


    # -------------------------------------------------------------
    # A MATRICES
    #
    # Keep all NONBIO technology coefficients at their 2023 values.
    # Only the BIO columns evolve towards the scenario endpoint.
    # -------------------------------------------------------------

    A_dom_current <- start_endpoint$A_dom
    A_imp_current <- start_endpoint$A_imp

    A_dom_current[, BIO] <-                              ### here, also technical coeffcicinets of BIO sectors are modified, meaning that NONBIO sectors' Adom and Aimp is kept constant
      start_endpoint$A_dom[, BIO, drop = FALSE] +
      lambda * (
        end_endpoint$A_dom[, BIO, drop = FALSE] -
          start_endpoint$A_dom[, BIO, drop = FALSE]
      )

    A_imp_current[, BIO] <-
      start_endpoint$A_imp[, BIO, drop = FALSE] +
      lambda * (
        end_endpoint$A_imp[, BIO, drop = FALSE] -
          start_endpoint$A_imp[, BIO, drop = FALSE]
      )


    # -------------------------------------------------------------
    # OPTION A - CAPEX MATRICES
    #
    # Interpolated exactly like A_dom / A_imp above, but kept in
    # their own separate matrices throughout.
    # -------------------------------------------------------------

    A_capex_dom_current <- start_endpoint$A_capex_dom
    A_capex_imp_current <- start_endpoint$A_capex_imp

    A_capex_dom_current[, BIO] <-
      start_endpoint$A_capex_dom[, BIO, drop = FALSE] +
      lambda * (
        end_endpoint$A_capex_dom[, BIO, drop = FALSE] -
          start_endpoint$A_capex_dom[, BIO, drop = FALSE]
      )

    A_capex_imp_current[, BIO] <-
      start_endpoint$A_capex_imp[, BIO, drop = FALSE] +
      lambda * (
        end_endpoint$A_capex_imp[, BIO, drop = FALSE] -
          start_endpoint$A_capex_imp[, BIO, drop = FALSE]
      )


    # -------------------------------------------------------------
    # DOMESTIC BIOFUEL OUTPUT - exogeneously set
    # -------------------------------------------------------------

    X_bio_current <- rep(NA_real_, nIndustries)

    X_bio_current[BIO] <-
      X_bio_start +
      lambda * (
        X_bio_end -
          X_bio_start
      )

    # -------------------------------------------------------------
    # FINISHED BIOFUEL IMPORTS
    #
    # Non-biofuel imported final expenditure stays at 2023.
    # Only the biofuel rows change towards the scenario target.
    # -------------------------------------------------------------

    Y_imp_FCE_current <-
      start_endpoint$Y_imp_FCE

    Y_imp_FCE_current[BIO] <-
      start_endpoint$Y_imp_FCE[BIO] +
      lambda * (
        end_endpoint$Y_imp_FCE[BIO] -
          start_endpoint$Y_imp_FCE[BIO]
      )

exports_current <-
  exports_start +
  lambda * (
    exports_end -
    exports_start
  )


gate_fee_coeff_current <-
  gate_fee_coeff_start +
  lambda * (
    gate_fee_coeff_end -
      gate_fee_coeff_start
  )

    # -------------------------------------------------------------
    # Store
    # -------------------------------------------------------------

    A_dom_path[i, , ] <- A_dom_current
    A_imp_path[i, , ] <- A_imp_current

    A_capex_dom_path[i, , ] <- A_capex_dom_current
    A_capex_imp_path[i, , ] <- A_capex_imp_current

    X_bio_path[i, ] <- X_bio_current

    Y_imp_FCE_path[i, ] <-
      Y_imp_FCE_current

exports_path[i, ] <- exports_current
gate_fee_coeff_path[i, ] <-
  gate_fee_coeff_current
  }


  # ---------------------------------------------------------------
  # Return
  # ---------------------------------------------------------------

  list(
    years = years,

    A_dom = A_dom_path,
    A_imp = A_imp_path,

    A_capex_dom = A_capex_dom_path,
    A_capex_imp = A_capex_imp_path,

    X_bio = X_bio_path,

    Y_imp_FCE = Y_imp_FCE_path,

    exports = exports_path,
  
    gate_fee_coeff = gate_fee_coeff_path
  )
}


# ===================================================================
# 11. BUILD PATHWAYS BRIDGING THE YEARS - INTERPOLATION WITH THREE FIXED ENDPOINTS
# ===================================================================

# ===================================================================
# SCENARIO 1: PIECEWISE EXOGENOUS PATH
# 2023 -> 2030 -> 2035 -> 2040
# ===================================================================

S1_driver_2023_2030 <- build_scenario_driver(
  start_endpoint = BASE_2023,
  end_endpoint   = S1_endpoint_2030,
  start_year     = 2023,
  end_year       = 2030
)

S1_driver_2030_2035 <- build_scenario_driver(
  start_endpoint = S1_endpoint_2030,
  end_endpoint   = S1_endpoint_2035,
  start_year     = 2030,
  end_year       = 2035
)

S1_driver_2035_2040 <- build_scenario_driver(
  start_endpoint = S1_endpoint_2035,
  end_endpoint   = S1_endpoint_2040,
  start_year     = 2035,
  end_year       = 2040
)

# ===================================================================
# SCENARIO 2: PIECEWISE EXOGENOUS PATH
# 2023 -> 2030 -> 2035 -> 2040
# ===================================================================

S2_driver_2023_2030 <- build_scenario_driver(
  start_endpoint = BASE_2023,
  end_endpoint   = S2_endpoint_2030,
  start_year     = 2023,
  end_year       = 2030
)

S2_driver_2030_2035 <- build_scenario_driver(
  start_endpoint = S2_endpoint_2030,
  end_endpoint   = S2_endpoint_2035,
  start_year     = 2030,
  end_year       = 2035
)

S2_driver_2035_2040 <- build_scenario_driver(
  start_endpoint = S2_endpoint_2035,
  end_endpoint   = S2_endpoint_2040,
  start_year     = 2035,
  end_year       = 2040
)


# ===================================================================
# SCENARIO 3: PIECEWISE EXOGENOUS PATH
# 2023 -> 2030 -> 2035 -> 2040
# ===================================================================

S3_driver_2023_2030 <- build_scenario_driver(
  start_endpoint = BASE_2023,
  end_endpoint   = S3_endpoint_2030,
  start_year     = 2023,
  end_year       = 2030
)

S3_driver_2030_2035 <- build_scenario_driver(
  start_endpoint = S3_endpoint_2030,
  end_endpoint   = S3_endpoint_2035,
  start_year     = 2030,
  end_year       = 2035
)

S3_driver_2035_2040 <- build_scenario_driver(
  start_endpoint = S3_endpoint_2035,
  end_endpoint   = S3_endpoint_2040,
  start_year     = 2035,
  end_year       = 2040
)


# ===================================================================
# COMBINE CONSECUTIVE SCENARIO-DRIVER SEGMENTS
# ===================================================================

combine_scenario_drivers <- function(...) {

  drivers <- list(...)

  if (length(drivers) == 0) {
    stop("No scenario drivers supplied.")
  }

  out <- drivers[[1]]

  if (length(drivers) == 1) {
    return(out)
  }

  for (k in 2:length(drivers)) {

    d <- drivers[[k]]

    # Skip first observation because it duplicates
    # the previous segment's endpoint
    keep <- 2:length(d$years)

    old_n <- length(out$years)
    new_n <- length(keep)

    combined_years <-
      c(
        out$years,
        d$years[keep]
      )


    # ------------------------------------------------------------
    # A_dom
    # ------------------------------------------------------------

    A_dom_new <-
      array(
        NA_real_,
        dim = c(
          old_n + new_n,
          nIndustries,
          nIndustries
        )
      )

    A_dom_new[1:old_n, , ] <- out$A_dom

    A_dom_new[
      (old_n + 1):(old_n + new_n),
      ,
    ] <-
      d$A_dom[keep, , , drop = FALSE]


    # ------------------------------------------------------------
    # A_imp
    # ------------------------------------------------------------

    A_imp_new <-
      array(
        NA_real_,
        dim = c(
          old_n + new_n,
          nIndustries,
          nIndustries
        )
      )

    A_imp_new[1:old_n, , ] <- out$A_imp

    A_imp_new[
      (old_n + 1):(old_n + new_n),
      ,
    ] <-
      d$A_imp[keep, , , drop = FALSE]


    # ------------------------------------------------------------
    # Remaining matrices
    # ------------------------------------------------------------

    out$X_bio <-
      rbind(
        out$X_bio,
        d$X_bio[keep, , drop = FALSE]
      )

    out$Y_imp_FCE <-
      rbind(
        out$Y_imp_FCE,
        d$Y_imp_FCE[keep, , drop = FALSE]
      )

    out$exports <-
      rbind(
        out$exports,
        d$exports[keep, , drop = FALSE]
      )

    out$gate_fee_coeff <-
      rbind(
        out$gate_fee_coeff,
        d$gate_fee_coeff[keep, , drop = FALSE]
      )


    out$years <- combined_years
    out$A_dom <- A_dom_new
    out$A_imp <- A_imp_new
  }


  # ------------------------------------------------------------
  # Restore year names
  # ------------------------------------------------------------

  dimnames(out$A_dom) <- list(
    year = as.character(out$years),
    input_sector = NULL,
    output_sector = NULL
  )

  dimnames(out$A_imp) <- list(
    year = as.character(out$years),
    input_sector = NULL,
    output_sector = NULL
  )

  rownames(out$X_bio) <-
    as.character(out$years)

  rownames(out$Y_imp_FCE) <-
    as.character(out$years)

  rownames(out$exports) <-
    as.character(out$years)

  rownames(out$gate_fee_coeff) <-
    as.character(out$years)


  out
}



# ===================================================================
# CREATE ONE COMPLETE DRIVER FOR EACH SCENARIO
# ===================================================================


S1_driver_2023_2040 <-
  combine_scenario_drivers(
    S1_driver_2023_2030,
    S1_driver_2030_2035,
    S1_driver_2035_2040
  )

S2_driver_2023_2040 <-
  combine_scenario_drivers(
    S2_driver_2023_2030,
    S2_driver_2030_2035,
    S2_driver_2035_2040
  )

S3_driver_2023_2040 <-
  combine_scenario_drivers(
    S3_driver_2023_2030,
    S3_driver_2030_2035,
    S3_driver_2035_2040
  )

REF_driver_2023_2040 <-                  ### stationary base line scenario/ counterfactional
  build_scenario_driver(
    start_endpoint = BASE_2023,
    end_endpoint   = BASE_2023,
    start_year     = 2023,
    end_year       = 2040
  )

stopifnot(
  identical(S1_driver_2023_2040$years, 2023:2040),
  identical(S2_driver_2023_2040$years, 2023:2040),
  identical(S3_driver_2023_2040$years, 2023:2040),
  identical(REF_driver_2023_2040$years, 2023:2040)
)


############################################################################################################################################################
#### this is the ONE common annual simulatio used for all scenarios --> the scenario driver provides the exoegnous BIO assumptions, the model calculates the engoengousr esponse of NONBIO sectors
############################################################################################################################################################

run_dynamic_scenario <- function(scenario_driver) {

  BIO <- unname(BIOFUEL_SECTORS)

  NONBIO <- setdiff(
    seq_len(nIndustries),
    BIO
  )

  years <- scenario_driver$years
  n_years <- length(years)


# =================================================================
  # STORAGE
  # =================================================================

# Actual monetary IO coefficient matrices.
# These are derived annually from the final Z and total accounting X,
# so that A_IO = Z / X always holds.

A_dom_IO_path <- array(
  NA_real_,
  dim = c(n_years, nIndustries, nIndustries),
  dimnames = list(
    year = as.character(years),
    input_sector = NULL,
    output_sector = NULL
  )
)

A_imp_IO_path <- array(
  NA_real_,
  dim = c(n_years, nIndustries, nIndustries),
  dimnames = list(
    year = as.character(years),
    input_sector = NULL,
    output_sector = NULL
  )
)

  X_path <- matrix(
    NA_real_,
    nrow = n_years,
    ncol = nIndustries,
    dimnames = list(
      year = as.character(years),
      sector = NULL
    )
  )

  Y_dom_path <- matrix(
    NA_real_,
    nrow = n_years,
    ncol = nIndustries,
    dimnames = list(
      year = as.character(years),
      sector = NULL
    )
  )

  # OPTION A - diagnostic-only paths for CAPEX-driven GFCF demand.
  # Not needed for the solve or for the accounting identities (Y_dom
  # is a residual and already reflects this correctly - see below),
  # but kept so the CAPEX-driven share of NONBIO activation can be
  # reported separately from the OPEX/feedstock-driven share.
  GFCF_capex_dom_path <- matrix(
    NA_real_,
    nrow = n_years,
    ncol = nIndustries,
    dimnames = list(
      year = as.character(years),
      sector = NULL
    )
  )

  CAPEX_imp_leakage_path <- matrix(
    NA_real_,
    nrow = n_years,
    ncol = nIndustries,
    dimnames = list(
      year = as.character(years),
      sector = NULL
    )
  )

  Z_dom_path <- array(
    NA_real_,
    dim = c(n_years, nIndustries, nIndustries),
    dimnames = list(
      year = as.character(years),
      input_sector = NULL,
      output_sector = NULL
    )
  )

  Z_imp_path <- array(
    NA_real_,
    dim = c(n_years, nIndustries, nIndustries),
    dimnames = list(
      year = as.character(years),
      input_sector = NULL,
      output_sector = NULL
    )
  )

  tax_products_path <- matrix(
    NA_real_,
    nrow = n_years,
    ncol = nIndustries,
    dimnames = list(
      year = as.character(years),
      sector = NULL
    )
  )

  P2_ADJ_path <- matrix(
    NA_real_,
    nrow = n_years,
    ncol = nIndustries,
    dimnames = list(
      year = as.character(years),
      sector = NULL
    )
  )

  GVA_path <- matrix(
    NA_real_,
    nrow = n_years,
    ncol = nIndustries,
    dimnames = list(
      year = as.character(years),
      sector = NULL
    )
  )

  L_dom_path <- array(
    NA_real_,
    dim = c(n_years, nIndustries, nIndustries),
    dimnames = list(
      year = as.character(years),
      input_sector = NULL,
      output_sector = NULL
    )
  )

  Y_imp_FCE_path <- matrix(
    NA_real_,
    nrow = n_years,
    ncol = nIndustries,
    dimnames = list(
      year = as.character(years),
      sector = NULL
    )
  )

# Gate-fee revenue by sector
gate_fee_revenue_path <- matrix(
  NA_real_,
  nrow = n_years,
  ncol = nIndustries,
  dimnames = list(
    year = as.character(years),
    sector = NULL
  )
)

exports_path <- matrix(
  NA_real_,
  nrow = n_years,
  ncol = nIndustries,
  dimnames = list(
    year = as.character(years),
    sector = NULL
  )
)

Y_domestic_final_path <- matrix(
  NA_real_,
  nrow = n_years,
  ncol = nIndustries,
  dimnames = list(
    year = as.character(years),
    sector = NULL
  )
)


# Household and government demand are stored separately
  # because these will later become part of the SFC dynamics.

  C_dom_path <- matrix(
    NA_real_,
    nrow = n_years,
    ncol = nIndustries,
    dimnames = list(
      year = as.character(years),
      sector = NULL
    )
  )

  G_dom_path <- matrix(
    NA_real_,
    nrow = n_years,
    ncol = nIndustries,
    dimnames = list(
      year = as.character(years),
      sector = NULL
    )
  )

# =================================================================
  # 2023 INITIAL CONDITION
  # =================================================================
  #
  # Do NOT solve 2023 again.
  # It is the observed empirical starting point.
  # =================================================================

A_dom_IO_path[1, , ] <-
  BASE_2023$A_dom_IO

A_imp_IO_path[1, , ] <-
  BASE_2023$A_imp_IO 

X_path[1, ] <-
    BASE_2023$X

  Y_dom_path[1, ] <-
    BASE_2023$Y_dom

  # OPTION A diagnostics: zero in 2023 (observed base year, no
  # scenario-specific CAPEX investment vector applied to it).
  GFCF_capex_dom_path[1, ] <-
    0

  CAPEX_imp_leakage_path[1, ] <-
    0

exports_path[1, ] <-
  scenario_driver$exports[1, ]

Y_domestic_final_path[1, ] <-
  BASE_2023$Y_dom -
  exports_path[1, ]

  Z_dom_path[1, , ] <-
    BASE_2023$Z_dom

  Z_imp_path[1, , ] <-
    BASE_2023$Z_imp

  tax_products_path[1, ] <-
    BASE_2023$tax_products

  P2_ADJ_path[1, ] <-
    BASE_2023$P2_ADJ

  GVA_path[1, ] <-
    BASE_2023$GVA

  L_dom_path[1, , ] <-
    BASE_2023$L_dom

  Y_imp_FCE_path[1, ] <-
    BASE_2023$Y_imp_FCE

  C_dom_path[1, ] <-
    C_dom_BASE_2023

  G_dom_path[1, ] <-
    G_dom_BASE_2023

gate_fee_revenue_path[1, ] <-
  BASE_2023$gate_fee_coeff *
  BASE_2023$X_fuel


  # =================================================================
  # ANNUAL DYNAMIC LOOP
  # =================================================================

  for (i in 2:n_years) {

    current_year <- years[i]

    cat(
      "Simulating year:",
      current_year,
      "\n"
    )


    # ===============================================================
    # READ EXOGENOUS SCENARIO CONDITIONS FOR THIS YEAR
    # ===============================================================

    A_dom_tech_current <-
  scenario_driver$A_dom[
    i,
    ,
  ]

A_imp_tech_current <-
  scenario_driver$A_imp[
    i,
    ,
  ]

# OPTION A - CAPEX investment coefficients for this year
A_capex_dom_tech_current <-
  scenario_driver$A_capex_dom[
    i,
    ,
  ]

A_capex_imp_tech_current <-
  scenario_driver$A_capex_imp[
    i,
    ,
  ]

    X_bio_current <-
      scenario_driver$X_bio[
        i,
        BIO
      ]

    Y_imp_FCE_current <-
      scenario_driver$Y_imp_FCE[
        i,
      ]

gate_fee_coeff_current <-
  scenario_driver$gate_fee_coeff[
    i,
  ]


    # ===============================================================
    # SFC BEHAVIOURAL BLOCK
    # ===============================================================
    #
    # CURRENT VERSION:
    # household and government consumption are fixed at 2023.
    #
    # THIS is the precise place where the SFC feedback will later be:
    #
    # previous/current output
    #       -> employment
    #       -> wages
    #       -> disposable income
    #       -> household consumption
    #       -> final demand
    #
    # ===============================================================

    C_dom_current <-
      C_dom_BASE_2023

    G_dom_current <-
      G_dom_BASE_2023

    OTHER_dom_current <-
      OTHER_dom_BASE_2023


    # ===============================================================
    # 3. NON-BIOFUEL FINAL DEMAND
    # ===============================================================


## final demand (Y) for NONBIO sectors is kept constant, since C_dom, G_dom, and OTHER_dom (consumption) are kept constant at 2023 (see rigth above)
## would only chnage if we activated SFC dynamics
    Y_nonbio_current <-
      (
        C_dom_current +
          G_dom_current +
          OTHER_dom_current
      )[NONBIO]


    # ===============================================================
    # 4. ENDOGENOUS NON-BIOFUEL OUTPUT
    # ===============================================================

    A_NN_current <-
  A_dom_tech_current[
    NONBIO,
    NONBIO,
    drop = FALSE
  ]

A_NB_current <-
  A_dom_tech_current[
    NONBIO,
    BIO,
    drop = FALSE
  ]

## output of NONBIO sectors is both from NONBIO inputs required by NONBIO and BIO production
## In this model version A_NB contains recurrent feedstock/OPEX inputs only;
## CAPEX is excluded from A_NB / A_dom_tech / Z, by design.

## ===============================================================
## OPTION A - CAPEX AS SEPARATE, CONTINUOUS GFCF-EQUIVALENT DEMAND
## ===============================================================
## CAPEX enters the NONBIO solve as its own additive demand term,
## NOT inside A_NB / A_dom_tech, and NEVER inside Z_dom / Z_imp
## (Z is still built later purely from A_dom_tech_current, i.e.
## feedstock + OPEX only - see INTERMEDIATE FLOWS section below).
## This keeps CAPEX out of intermediate consumption and out of the
## BIO sectors' own GVA calculation, while still letting it trigger
## real domestic demand for machinery/construction/electronics etc.
## in the NONBIO sectors that actually supply investment goods.

A_capex_NB_current <-
  A_capex_dom_tech_current[
    NONBIO,
    BIO,
    drop = FALSE
  ]

GFCF_capex_demand_current <-
  A_capex_NB_current %*%
  X_bio_current

# Imported CAPEX share: logged for reporting (e.g. "CAPEX import
# dependency"), but must NOT enter the domestic solve below, since
# imported investment goods do not create additional EU production.
CAPEX_imp_leakage_current <-
  A_capex_imp_tech_current[
    NONBIO,
    BIO,
    drop = FALSE
  ] %*%
  X_bio_current

    X_nonbio_current <-
      solve(
        base::diag(length(NONBIO)) -
          A_NN_current,

        Y_nonbio_current +
          GFCF_capex_demand_current +
          A_NB_current %*%
          X_bio_current
      )


# ===============================================================
# 5. BUILD FUEL / TECHNICAL OUTPUT VECTOR
# ===============================================================

# For biofuel sectors this is the market value of fuel production
# (physical production * fuel market price), excluding gate fees.
X_fuel_current <-
  numeric(nIndustries)

X_fuel_current[BIO] <-
  X_bio_current

X_fuel_current[NONBIO] <-
  as.numeric(
    X_nonbio_current
  )


# ===============================================================
# 5.2. GATE-FEE REVENUE
# ===============================================================

gate_fee_revenue_current <-
  gate_fee_coeff_current *
  X_fuel_current


# ===============================================================
# 5.3. TOTAL ACCOUNTING OUTPUT
# ===============================================================

# Total sector output includes the additional gate-fee service
# revenue of advanced biogas.
X_current <-
  X_fuel_current

X_current[ADV_BIOGAS] <-
  X_fuel_current[ADV_BIOGAS] +
  gate_fee_revenue_current[ADV_BIOGAS]


# ===============================================================
# 6. INTERMEDIATE FLOWS
# ===============================================================

# Technical intermediate requirements are driven by fuel production,
# NOT by gate-fee revenue. For scenario BIO columns these technical
# requirements contain feedstock + OPEX only; CAPEX is excluded.
Z_dom_current <-
  A_dom_tech_current %*%
  base::diag(X_fuel_current)

Z_imp_current <-
  A_imp_tech_current %*%
  base::diag(X_fuel_current)


# Gate fee is added separately as a monetary transaction:
# sewerage purchases a waste-treatment service from advanced biogas.
Z_dom_current[
  ADV_BIOGAS,
  SEWERAGE
] <-
  Z_dom_current[
    ADV_BIOGAS,
    SEWERAGE
  ] +
  gate_fee_revenue_current[ADV_BIOGAS]



# ===============================================================
# 6.2. NEW: !!:  ACTUAL MONETARY IO COEFFICIENT MATRICES
# ===============================================================
#
# The technology matrices above are defined relative to X_fuel.
# After all monetary transactions, including gate-fee revenue,
# have been entered into Z and total accounting output X is known,
# derive the actual IO coefficients:
#
# A_IO = Z / X
#
# This guarantees:
#
# Z = A_IO %*% diag(X)

X_safe_current <- X_current
X_safe_current[
  abs(X_safe_current) < 1e-12
] <- 1e-6

A_dom_IO_current <-
  sweep(
    Z_dom_current,
    2,
    X_safe_current,
    "/"
  )

A_imp_IO_current <-
  sweep(
    Z_imp_current,
    2,
    X_safe_current,
    "/"
  )

    # ===============================================================
    # 7. DOMESTIC FINAL DEMAND AS ACCOUNTING RESIDUAL
    # ===============================================================

    Y_dom_current <-
      X_current -
      rowSums(Z_dom_current)

exports_current <-
  scenario_driver$exports[i, ]

Y_domestic_final_current <-
  Y_dom_current -
  exports_current


## Plausibilitätscheck
if (
  any(
    Y_domestic_final_current[BIO] < -1e-8
  )
) {
  stop(
    paste(
      "Biofuel exports exceed available final use in year",
      current_year
    )
  )
}


    # ===============================================================
    # 8. TAXES, INTERMEDIATE CONSUMPTION AND GVA
    # ===============================================================

    intermediate_inputs_current <-
      colSums(Z_dom_current) +
      colSums(Z_imp_current)

    tax_products_current <-
      p2_tax_rate_2023 *
      intermediate_inputs_current

    P2_ADJ_current <-
      intermediate_inputs_current +
      tax_products_current

    GVA_current <-
      X_current -
      P2_ADJ_current


    # ===============================================================
    # 9. LEONTIEF INVERSE
    # ===============================================================

    L_dom_current <-
  solve(
    base::diag(nIndustries) -
      A_dom_IO_current
  )


    # ===============================================================
    # 10. STORE THIS YEAR
    # ===============================================================
A_dom_IO_path[i, , ] <-
  A_dom_IO_current

A_imp_IO_path[i, , ] <-
  A_imp_IO_current
   

X_path[i, ] <-
      X_current

    Y_dom_path[i, ] <-
      Y_dom_current

    # OPTION A diagnostics: expand the NONBIO-only CAPEX demand
    # vectors to full sector length (zero for BIO sectors, which
    # do not receive CAPEX demand themselves) and store them.
    GFCF_capex_dom_current <-
      numeric(nIndustries)

    GFCF_capex_dom_current[NONBIO] <-
      as.numeric(GFCF_capex_demand_current)

    GFCF_capex_dom_path[i, ] <-
      GFCF_capex_dom_current

    CAPEX_imp_leakage_full_current <-
      numeric(nIndustries)

    CAPEX_imp_leakage_full_current[NONBIO] <-
      as.numeric(CAPEX_imp_leakage_current)

    CAPEX_imp_leakage_path[i, ] <-
      CAPEX_imp_leakage_full_current

    Z_dom_path[i, , ] <-
      Z_dom_current

    Z_imp_path[i, , ] <-
      Z_imp_current

    tax_products_path[i, ] <-
      tax_products_current

    P2_ADJ_path[i, ] <-
      P2_ADJ_current

    GVA_path[i, ] <-
      GVA_current

    L_dom_path[i, , ] <-
      L_dom_current

    Y_imp_FCE_path[i, ] <-
      Y_imp_FCE_current

exports_path[i, ] <- exports_current
Y_domestic_final_path[i, ] <- Y_domestic_final_current

    C_dom_path[i, ] <-
      C_dom_current

    G_dom_path[i, ] <-
      G_dom_current

gate_fee_revenue_path[i, ] <-
  gate_fee_revenue_current
  }


  # =================================================================
  # RETURN COMPLETE DYNAMIC PATH
  # =================================================================

  list(

    years = years,

    # Actual monetary IO coefficients
A_dom =
  A_dom_IO_path,

A_imp =
  A_imp_IO_path,

# Technology coefficients used internally to generate Z
A_dom_tech =
  scenario_driver$A_dom,

A_imp_tech =
  scenario_driver$A_imp,
X_bio =
  scenario_driver$X_bio,

    X =
      X_path,

    Y_dom =
      Y_dom_path,

    # OPTION A diagnostics: CAPEX-driven GFCF demand (domestic) and
    # the imported-CAPEX leakage, both by NONBIO supplying sector.
    # Not used in any further calculation - purely for reporting the
    # CAPEX-driven share of NONBIO activation separately from the
    # OPEX/feedstock-driven share.
    GFCF_capex_dom =
      GFCF_capex_dom_path,

    CAPEX_imp_leakage =
      CAPEX_imp_leakage_path,

    Z_dom =
      Z_dom_path,

    Z_imp =
      Z_imp_path,

    tax_products =
      tax_products_path,

    P2_ADJ =
      P2_ADJ_path,

    GVA =
      GVA_path,

    L_dom =
      L_dom_path,

    Y_imp_FCE =
      Y_imp_FCE_path,
exports =
  exports_path,

Y_domestic_final =
  Y_domestic_final_path,

    C_dom =
      C_dom_path,

    G_dom =
      G_dom_path,

gate_fee_coeff =
  scenario_driver$gate_fee_coeff,

gate_fee_revenue =
  gate_fee_revenue_path
  )
}

# ===================================================================
# 13. RUN ALL SCENARIOS THROUGH THE SAME DYNAMIC MODEL
# ===================================================================


REF_dynamic_2023_2040 <-
  run_dynamic_scenario(
    REF_driver_2023_2040
  )

S1_dynamic_2023_2040 <-
  run_dynamic_scenario(
    S1_driver_2023_2040
  )

S2_dynamic_2023_2040 <-
  run_dynamic_scenario(
    S2_driver_2023_2040
  )

S3_dynamic_2023_2040 <-
  run_dynamic_scenario(
    S3_driver_2023_2040
  )

# ===================================================================
# 14. EXTRACT ONE YEAR FROM A DYNAMIC RESULT
# ===================================================================

extract_dynamic_endpoint <- function(
    dynamic_result,
    year
) {

  year_char <- as.character(year)

  if (!year_char %in%
      as.character(dynamic_result$years)) {

    stop(
      paste(
        "Year",
        year,
        "not contained in dynamic result."
      )
    )
  }


  list(

    A_dom =
      dynamic_result$A_dom[
        year_char,
        ,
      ],

    A_imp =
      dynamic_result$A_imp[
        year_char,
        ,
      ],

A_dom_tech =
  dynamic_result$A_dom_tech[
    year_char,
    ,
  ],

A_imp_tech =
  dynamic_result$A_imp_tech[
    year_char,
    ,
  ],

X_bio =
  dynamic_result$X_bio[
    year_char,
  ],

    X =
      dynamic_result$X[
        year_char,
      ],

    Z_dom =
      dynamic_result$Z_dom[
        year_char,
        ,
      ],

    Z_imp =
      dynamic_result$Z_imp[
        year_char,
        ,
      ],

    Y_dom =
      dynamic_result$Y_dom[
        year_char,
      ],

    GFCF_capex_dom =
      dynamic_result$GFCF_capex_dom[
        year_char,
      ],

    CAPEX_imp_leakage =
      dynamic_result$CAPEX_imp_leakage[
        year_char,
      ],
exports =
  dynamic_result$exports[
    year_char,
  ],

Y_domestic_final =
  dynamic_result$Y_domestic_final[
    year_char,
  ],

    tax_products =
      dynamic_result$tax_products[
        year_char,
      ],

    P2_ADJ =
      dynamic_result$P2_ADJ[
        year_char,
      ],

    GVA =
      dynamic_result$GVA[
        year_char,
      ],

    Y_imp_FCE =
      dynamic_result$Y_imp_FCE[
        year_char,
      ],

    L_dom =
      dynamic_result$L_dom[
        year_char,
        ,
      ],

    gate_fee_coeff =
    dynamic_result$gate_fee_coeff[
      year_char,
    ],

    gate_fee_revenue =
    dynamic_result$gate_fee_revenue[
      year_char,
  ]
  )
}

# ===================================================================
# EXTRACT REFERENCE ENDPOINTS
# ===================================================================

REF_dynamic_endpoint_2030 <-
  extract_dynamic_endpoint(
    REF_dynamic_2023_2040,
    2030
  )

REF_dynamic_endpoint_2035 <-
  extract_dynamic_endpoint(
    REF_dynamic_2023_2040,
    2035
  )

REF_dynamic_endpoint_2040 <-
  extract_dynamic_endpoint(
    REF_dynamic_2023_2040,
    2040
  )


# ===================================================================
# EXTRACT S1 ENDPOINTS
# ===================================================================

S1_dynamic_endpoint_2030 <-
  extract_dynamic_endpoint(
    S1_dynamic_2023_2040,
    2030
  )

S1_dynamic_endpoint_2035 <-
  extract_dynamic_endpoint(
    S1_dynamic_2023_2040,
    2035
  )

S1_dynamic_endpoint_2040 <-
  extract_dynamic_endpoint(
    S1_dynamic_2023_2040,
    2040
  )


# ===================================================================
# EXTRACT S2 ENDPOINTS
# ===================================================================

S2_dynamic_endpoint_2030 <-
  extract_dynamic_endpoint(
    S2_dynamic_2023_2040,
    2030
  )

S2_dynamic_endpoint_2035 <-
  extract_dynamic_endpoint(
    S2_dynamic_2023_2040,
    2035
  )

S2_dynamic_endpoint_2040 <-
  extract_dynamic_endpoint(
    S2_dynamic_2023_2040,
    2040
  )


# ===================================================================
# EXTRACT S3 ENDPOINTS
# ===================================================================

S3_dynamic_endpoint_2030 <-
  extract_dynamic_endpoint(
    S3_dynamic_2023_2040,
    2030
  )

S3_dynamic_endpoint_2035 <-
  extract_dynamic_endpoint(
    S3_dynamic_2023_2040,
    2035
  )

S3_dynamic_endpoint_2040 <-
  extract_dynamic_endpoint(
    S3_dynamic_2023_2040,
    2040
  )

# ===================================================================
# EXTRACT S1 / 2030 RESULTS AS STANDALONE OBJECTS
# ===================================================================

A_dom_S1_2030 <- S1_dynamic_endpoint_2030$A_dom
A_imp_S1_2030 <- S1_dynamic_endpoint_2030$A_imp
X_S1_2030     <- S1_dynamic_endpoint_2030$X
Z_dom_S1_2030 <- S1_dynamic_endpoint_2030$Z_dom
Z_imp_S1_2030 <- S1_dynamic_endpoint_2030$Z_imp
Y_dom_S1_2030 <- S1_dynamic_endpoint_2030$Y_dom
GVA_S1_2030   <- S1_dynamic_endpoint_2030$GVA

# ===================================================================
# EXTRACT S2 / 2030 RESULTS AS STANDALONE OBJECTS
# ===================================================================

A_dom_S2_2030 <- S2_dynamic_endpoint_2030$A_dom
A_imp_S2_2030 <- S2_dynamic_endpoint_2030$A_imp
X_S2_2030     <- S2_dynamic_endpoint_2030$X
Z_dom_S2_2030 <- S2_dynamic_endpoint_2030$Z_dom
Z_imp_S2_2030 <- S2_dynamic_endpoint_2030$Z_imp
Y_dom_S2_2030 <- S2_dynamic_endpoint_2030$Y_dom
GVA_S2_2030   <- S2_dynamic_endpoint_2030$GVA

# ===================================================================
# EXTRACT S3 / 2030 RESULTS AS STANDALONE OBJECTS
# ===================================================================

A_dom_S3_2030 <- S3_dynamic_endpoint_2030$A_dom
A_imp_S3_2030 <- S3_dynamic_endpoint_2030$A_imp
X_S3_2030     <- S3_dynamic_endpoint_2030$X
Z_dom_S3_2030 <- S3_dynamic_endpoint_2030$Z_dom
Z_imp_S3_2030 <- S3_dynamic_endpoint_2030$Z_imp
Y_dom_S3_2030 <- S3_dynamic_endpoint_2030$Y_dom
GVA_S3_2030   <- S3_dynamic_endpoint_2030$GVA


# ===================================================================
# 15. BASIC DYNAMIC ENDPOINT CONSISTENCY CHECKS - checks whether X = Z^dom *1 + Y^dom
# ===================================================================

check_accounting_identity <- function(endpoint) {

  stopifnot(
    isTRUE(
      all.equal(
        endpoint$X,
        rowSums(endpoint$Z_dom) +
          endpoint$Y_dom
      )
    )
  )
}

# -------------------------------------------------------------------
# IO coefficient consistency: Z = A %*% diag(X)
# -------------------------------------------------------------------


check_io_consistency <- function(endpoint) {

  stopifnot(

    isTRUE(
      all.equal(
        endpoint$Z_dom,
        endpoint$A_dom %*%
          base::diag(endpoint$X),
        tolerance = 1e-10
      )
    ),

    isTRUE(
      all.equal(
        endpoint$Z_imp,
        endpoint$A_imp %*%
          base::diag(endpoint$X),
        tolerance = 1e-10
      )
    )
  )
}

endpoints_to_check <- list(

  REF_2030 = REF_dynamic_endpoint_2030,
  REF_2035 = REF_dynamic_endpoint_2035,
  REF_2040 = REF_dynamic_endpoint_2040,

  S1_2030 = S1_dynamic_endpoint_2030,
  S1_2035 = S1_dynamic_endpoint_2035,
  S1_2040 = S1_dynamic_endpoint_2040,

  S2_2030 = S2_dynamic_endpoint_2030,
  S2_2035 = S2_dynamic_endpoint_2035,
  S2_2040 = S2_dynamic_endpoint_2040,

  S3_2030 = S3_dynamic_endpoint_2030,
  S3_2035 = S3_dynamic_endpoint_2035,
  S3_2040 = S3_dynamic_endpoint_2040
)

invisible(
  lapply(
    endpoints_to_check,
    check_accounting_identity
  )
)

invisible(
  lapply(
    endpoints_to_check,
    check_io_consistency
  )
)


# -------------------------------------------------------------------
# Reference path must reproduce the unchanged 2023 baseline
# -------------------------------------------------------------------

stopifnot(

  isTRUE(
    all.equal(
      REF_dynamic_endpoint_2030$X,
      BASE_2023$X
    )
  ),

  isTRUE(
    all.equal(
      REF_dynamic_endpoint_2030$Z_dom,
      BASE_2023$Z_dom
    )
  ),

  isTRUE(
    all.equal(
      REF_dynamic_endpoint_2030$GVA,
      BASE_2023$GVA
    )
  )
)


# -------------------------------------------------------------------
# Gate-fee accounting check
# -------------------------------------------------------------------
# Gate-fee revenue of advanced biogas must appear exactly as the
# monetary transaction from sewerage to advanced biogas in Z_dom.

stopifnot(

  isTRUE(
    all.equal(
      S1_dynamic_endpoint_2030$Z_dom[
        ADV_BIOGAS,
        SEWERAGE
      ],
      S1_dynamic_endpoint_2030$gate_fee_revenue[
        ADV_BIOGAS
      ]
    )
  ),

  isTRUE(
    all.equal(
      S2_dynamic_endpoint_2030$Z_dom[
        ADV_BIOGAS,
        SEWERAGE
      ],
      S2_dynamic_endpoint_2030$gate_fee_revenue[
        ADV_BIOGAS
      ]
    )
  )
)



# ===================================================================
# SAVE MODEL RESULTS
# ===================================================================

# ===================================================================
# SAVE MODEL RESULTS:
# benchmark endpoints + complete annual dynamic paths
# ===================================================================

model_results <- list(

  # ---------------------------------------------------------------
  # Benchmark-year results
  # ---------------------------------------------------------------

  `2030` = list(
    REF = REF_dynamic_endpoint_2030,
    S1  = S1_dynamic_endpoint_2030,
    S2  = S2_dynamic_endpoint_2030,
    S3  = S3_dynamic_endpoint_2030
  ),

  `2035` = list(
    REF = REF_dynamic_endpoint_2035,
    S1  = S1_dynamic_endpoint_2035,
    S2  = S2_dynamic_endpoint_2035,
    S3  = S3_dynamic_endpoint_2035
  ),

  `2040` = list(
    REF = REF_dynamic_endpoint_2040,
    S1  = S1_dynamic_endpoint_2040,
    S2  = S2_dynamic_endpoint_2040,
    S3  = S3_dynamic_endpoint_2040
  ),


  # ---------------------------------------------------------------
  # Complete annual paths: 2023-2040
  # ---------------------------------------------------------------

  dynamic = list(
    REF = REF_dynamic_2023_2040,
    S1  = S1_dynamic_2023_2040,
    S2  = S2_dynamic_2023_2040,
    S3  = S3_dynamic_2023_2040
  ),


  # ---------------------------------------------------------------
  # Metadata
  # ---------------------------------------------------------------

  metadata = list(
    BIO = BIO,
    NONBIO = NONBIO,
    sector_names = sector_names,
    model_variant = "CAPEX excluded from biofuel intermediate-input coefficients",
    capex_treatment = "CAPEX excluded from A_dom/A_imp; no alternative CAPEX investment treatment implemented yet"
  )
)

saveRDS(
  model_results,
  "model_results_CAPEX_separate.rds"
)
