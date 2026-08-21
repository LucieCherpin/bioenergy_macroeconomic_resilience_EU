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

#National Accounts
national_accounts <- read_xlsx("nama_10_gdp__EU27.xlsx", sheet = "Sheet 1", range = "A10:B48", col_names = F)

#Here Integrate Extensions
### maybe don't use that one if I us ethe exiosbase extensions
#Employment E.G:
#employment <- read_xlsx("nama_10_a64_EU27.xlsx", sheet = "Sheet 1", range = "A11:X77", col_names = T)

#####################################################################
###STEP 2: Defining Coefficients (Exogenous Variables and Parameters)
#####################################################################

##########################################
###General Parameters
##########################################

nYears = 20             #Number of years
nIndustries = 73        #Industries of Eurostat input-output table

##################################################################################################################################################
### from here on, I still need to adaot and then add to MERGED code ###
##################################################################################################################################################

#Initialisation

#P3 - Final expenditure
final_expenditure <- as.numeric(national_accounts[3,2])       #P3 - Total final expenditure (C + G)


###P3 - Total final expenditure (C + G) by industry

t_f_e_dom <- as.numeric(unlist(IO_EU_domestic[2:(nIndustries+1), nIndustries+3]))                  
t_f_e_imp <- as.numeric(unlist(IO_EU_imports[2:(nIndustries+1), nIndustries+3]))

#Total Final Expenditure - Do we need this?
#D_dom = matrix(rep(t_f_e_dom, times = nYears), nrow = nYears, byrow = TRUE)
#D_imp = matrix(rep(t_f_e_imp, times = nYears), nrow = nYears, byrow = TRUE)

### D21X31 - Taxes less subsidies on products as part of final expenditure
#expenditure_tax_dom <- as.numeric(unlist(IO_EU_domestic[nIndustries + 8, nIndustries + 3]))    ### column nIndustries + 3 is "final consumotion expenditure"


#D_tax_dom = matrix(rep(expenditure_tax_dom, times = nYears), nrow = nYears, byrow = TRUE)   #D21X31 - Taxes less subsidies on products as part of final expenditure by industry matrix



final_use_dom <- as.numeric(unlist(IO_EU_domestic[2:(nIndustries + 1), nIndustries + 18]))    #TFU - Total final use by industry
final_use_imp <- as.numeric(unlist(IO_EU_imports[2:(nIndustries + 1), nIndustries + 18]))

f_dom = matrix(rep(final_use_dom, times = nYears), nrow = nYears, byrow = TRUE)      #TFU - Total final use by industry matrix
f_imp = matrix(rep(final_use_imp, times = nYears), nrow = nYears, byrow = TRUE)

#Production and sales, only looking at domestic
#gva_dom <- as.numeric(unlist(IO_EU_domestic[nIndustries + 2, 3:(nIndustries + 2)])) #B1G - Gross value added by industry
#gva_imp <- as.numeric(unlist(IO_EU_imports[nIndustries + 2, 3:(nIndustries + 2)]))
#GVA_dom = matrix(rep(gva_dom, times = nYears), nrow = nYears, byrow = TRUE)   #B1G - Gross value added by industry matrix  
#GVA_imp = matrix(rep(gva_imp, times = nYears), nrow = nYears, byrow = TRUE)

## only measuring doemstic EU output generation by industry, 
#sales_dom <- as.numeric(unlist(IO_EU_domestic[nIndustries + 10, 3:(nIndustries + 2)]))  #P1 - Total sales (output) by industry
#sales_imp <- as.numeric(unlist(IO_EU_imports[nIndustries + 10, 3:(nIndustries + 2)]))

#SALES_dom = matrix(rep(sales_dom, times = nYears), nrow = nYears, byrow = TRUE)    #P1 - Total sales (output) by industry matrix
#SALES_imp = matrix(rep(sales_imp, times = nYears), nrow = nYears, byrow = TRUE)

q_s_dom <- as.numeric(unlist(IO_EU_domestic[nIndustries + 19, 3:(nIndustries + 2)]))  #TS_BP - Total supply by industry
#q_s_imp <- as.numeric(unlist(IO_EU_imports[nIndustries + 19, 3:(nIndustries + 2)]))
q_s_dom[q_s_dom == 0] <- 1e-6                                                                #Necessary for calculations
#q_s_imp[q_s_imp == 0] <- 1e-6                                                                
Q_s_dom_i = matrix(rep(q_s_dom, times = nYears), nrow = nYears, byrow = TRUE)   #TS_BP - Total supply by industry matrix
#Q_s_imp_i = matrix(rep(q_s_imp, times = nYears), nrow = nYears, byrow = TRUE)

### total use of intermediate inputs
x_dom <- as.numeric(unlist(IO_EU_domestic[2:(nIndustries + 1), nIndustries + 17]))                   #TU - Total use by industry
#x_imp <- as.numeric(unlist(IO_EU_imports[2:(nIndustries + 1), nIndustries + 17]))
X_dom = matrix(rep(x_dom, times = nYears), nrow = nYears, byrow = TRUE)                             #TU - Total use by industry matrix
#X_imp = matrix(rep(x_imp, times = nYears), nrow = nYears, byrow = TRUE)

#P2_ADJ - Total intermediate consumption by industry
i_d_dom <- as.numeric(unlist(IO_EU_domestic[nIndustries + 11, 3:(nIndustries + 2)]))   #P2_ADJ - Total intermediate consumption by industry
i_d_imp <- as.numeric(unlist(IO_EU_imports[nIndustries + 11, 3:(nIndustries + 2)]))
I_D_dom_i <- matrix(rep(i_d_dom, times = nYears), nrow = nYears, byrow = TRUE)    #P2_ADJ - Total intermediate consumption by industry matrix
I_D_imp_i <- matrix(rep(i_d_imp, times = nYears), nrow = nYears, byrow = TRUE)

##########################################
###Final Expenditure
##########################################

#Total consumption shares by industry
beta_bar_dom <- as.matrix(
  (as.numeric(unlist(IO_EU_domestic[2:(nIndustries + 1), nIndustries + 4])) +
     as.numeric(unlist(IO_EU_domestic[2:(nIndustries + 1), nIndustries + 5])) +
     as.numeric(unlist(IO_EU_domestic[2:(nIndustries + 1), nIndustries + 6])))
  /
    (as.numeric(unlist(IO_EU_domestic[1, nIndustries + 4])) +
       as.numeric(unlist(IO_EU_domestic[1, nIndustries + 5])) +
       as.numeric(unlist(IO_EU_domestic[1, nIndustries + 6])))
)

beta_bar_imp <- as.matrix(
  (as.numeric(unlist(IO_EU_imports[2:(nIndustries + 1), nIndustries + 4])) +
     as.numeric(unlist(IO_EU_imports[2:(nIndustries + 1), nIndustries + 5])) +
     as.numeric(unlist(IO_EU_imports[2:(nIndustries + 1), nIndustries + 6])))
  /
    (as.numeric(unlist(IO_EU_imports[1, nIndustries + 4])) +
       as.numeric(unlist(IO_EU_imports[1, nIndustries + 5])) +
       as.numeric(unlist(IO_EU_imports[1, nIndustries + 6])))
)

# Household consumption shares by industry (HH + NPISH)
beta_c_bar_dom <- as.matrix(
  (as.numeric(unlist(IO_EU_domestic[2:(nIndustries + 1), nIndustries + 5])) +
     as.numeric(unlist(IO_EU_domestic[2:(nIndustries + 1), nIndustries + 6])))
  /
    (as.numeric(unlist(IO_EU_domestic[1, nIndustries + 5])) +
       as.numeric(unlist(IO_EU_domestic[1, nIndustries + 6])))
)

beta_c_bar_imp <- as.matrix(
  (as.numeric(unlist(IO_EU_imports[2:(nIndustries + 1), nIndustries + 5])) +
     as.numeric(unlist(IO_EU_imports[2:(nIndustries + 1), nIndustries + 6])))
  /
    (as.numeric(unlist(IO_EU_imports[1, nIndustries + 5])) +
       as.numeric(unlist(IO_EU_imports[1, nIndustries + 6])))
)

#Household consumption shares by industry (HH + NPISH) Matrix
beta_C_dom = matrix(rep(beta_c_bar_dom, times = nYears), nrow = nYears, byrow = TRUE)   # Household Consumption Shares Matrix (Domestic)
beta_C_imp = matrix(rep(beta_c_bar_imp, times = nYears), nrow = nYears, byrow = TRUE)   # Household Consumption Shares Matrix (imports)

# Government expenditure shares by industry
beta_g_bar_dom <- as.matrix(
  as.numeric(unlist(IO_EU_domestic[2:(nIndustries + 1), nIndustries + 4])) /
    as.numeric(unlist(IO_EU_domestic[1, nIndustries + 4]))
)

beta_g_bar_imp <- as.matrix(
  as.numeric(unlist(IO_EU_imports[2:(nIndustries + 1), nIndustries + 4])) /
    as.numeric(unlist(IO_EU_imports[1, nIndustries + 4]))
)

#Real government expenditure composition
beta_G_dom = matrix(rep(beta_g_bar_dom, times = nYears), nrow = nYears, byrow = TRUE)   # Government Expenditure Shares Matrix (Domestic)
beta_G_imp = matrix(rep(beta_g_bar_imp, times = nYears), nrow = nYears, byrow = TRUE)   # Government Expenditure Shares Matrix (imports)

#P3_S13 - Government expenditure
g <- as.numeric(national_accounts[4,2])                                               #P3_S13 - Government expenditure
g_dom <- as.numeric(unlist(IO_EU_domestic[2:(nIndustries + 1), nIndustries + 4]))    #P3_S13 - Government expenditure by industry
g_imp <- as.numeric(unlist(IO_EU_imports[2:(nIndustries + 1), nIndustries + 4]))
g_cons_tax_dom <- as.numeric(unlist(IO_EU_domestic[nIndustries + 8, nIndustries + 4]))   #D21X31_S13 - Taxes less subsidies on products as part of final government expenditure
#g_cons_tax_imp <- as.numeric(unlist(IO_EU_imports[nIndustries + 8, nIndustries + 4]))
g_cons_tax_i_dom <- beta_g_bar_dom * g_cons_tax_dom                 #D21X31_S13 - Taxes less subsidies on products as part of final government expenditure by industry
#g_cons_tax_i_imp <- beta_g_bar_imp * g_cons_tax_imp

G_dom = matrix(data = sum(g_dom), ncol = nYears)              #P3_S13 - Government expenditure matrix
G_imp = matrix(data = sum(g_imp), ncol = nYears)
#G_tax = matrix(data = sum(g_cons_tax_dom), ncol = nYears)         #D21X31_S13 - Taxes less subsidies on products as part of final government expenditure matrix
#G_tax = matrix(data = sum(g_cons_tax_imp), ncol = nYears)

G_i_dom = matrix(rep(g_dom, times = nYears), nrow = nYears, byrow = TRUE)                #P3_S13 - Government expenditure by industry matrix
#G_i_tax_dom = matrix(rep(g_cons_tax_i_dom, times = nYears), nrow = nYears, byrow = TRUE)   #D21X31_S13 - Taxes less subsidies on products as part of final government expenditure by indusrty matrix
G_i_imp = matrix(rep(g_imp, times = nYears), nrow = nYears, byrow = TRUE)                
#G_i_tax_dom = matrix(rep(g_cons_tax_i_imp, times = nYears), nrow = nYears, byrow = TRUE)

#P3_S14_S15 - Household consumption
cons <- as.numeric(national_accounts[7,2])                    #P3_S14_S15 - Household and NPISH consumption
hh_cons_dom <- as.numeric(unlist(IO_EU_domestic[2:(nIndustries + 1), nIndustries + 5]))
hh_cons_imp <- as.numeric(unlist(IO_EU_imports[2:(nIndustries + 1), nIndustries + 5]))                       #P3_S14 - Household consumption by industry

npish_cons_dom <- as.numeric(unlist(IO_EU_domestic[2:(nIndustries + 1), nIndustries + 6]))
npish_cons_imp <- as.numeric(unlist(IO_EU_imports[2:(nIndustries + 1), nIndustries + 6]))                    #P3_S15 - NPISH consumption by industry

consumption_dom = hh_cons_dom + npish_cons_dom
consumption_imp = hh_cons_imp + npish_cons_imp                         #P3_S14_S15 - Household and NPISH consumption by industry

consumption_tax_dom <- as.numeric(unlist(IO_EU_domestic[nIndustries + 8, nIndustries + 5]))
#consumption_tax_imp <- as.numeric(unlist(IO_EU_imports[nIndustries + 8, nIndustries + 5]))             #D21X31_S14_S15 - Taxes less subsidies on products as part of final household and NPISH consumption#

consumption_tax_i_dom <- beta_c_bar_dom * consumption_tax_dom                                                      #D21X31_S14_S15 - Taxes less subsidies on products as part of final household and NPISH consumption by industry
#consumption_tax_i_imp <- beta_c_bar_imp * consumption_tax_imp     

C_dom = matrix(data = sum(consumption_dom), ncol = nYears)            #P3_S14_S15 - Household and NPISH consumption matrix
C_imp = matrix(data = sum(consumption_imp), ncol = nYears)           

C_tax_dom = matrix(data = sum(consumption_tax_dom), ncol = nYears)    #D21X31_S14_S15 - Taxes less subsidies on products as part of final household and NPISH consumption matrix
#C_tax_imp = matrix(data = sum(consumption_tax_imp), ncol = nYears) 

C_i_dom = matrix(rep(consumption_dom, times = nYears), nrow = nYears, byrow = TRUE)            #P3_S14_S15 - Household and NPISH consumption by industry matrix
C_i_tax_dom <- matrix(rep(consumption_tax_i_dom, times = nYears),nrow = nYears, byrow = TRUE)  #D21X31_S14_S15 - Taxes less subsidies on products as part of final household and NPISH consumption by industry matrix
C_i_imp = matrix(rep(consumption_imp, times = nYears), nrow = nYears, byrow = TRUE)    
#C_i_tax_imp <- matrix(rep(consumption_tax_i_imp, times = nYears),nrow = nYears, byrow = TRUE)


D_dom <- matrix(0, nrow = nYears, ncol = nIndustries)
D_imp <- matrix(0, nrow = nYears, ncol = nIndustries)
f_dom = matrix(rep(final_use_dom, times = nYears), nrow = nYears, byrow = TRUE)
f_imp = matrix(rep(final_use_imp, times = nYears), nrow = nYears, byrow = TRUE)

##########################################
###Technical coefficients matrix (A matrix)
##########################################

# Goods x goods matrix Z (Domestic)
Z_dom <- matrix(
  as.numeric(as.character(unlist(IO_EU_domestic[2:(nIndustries + 1), 3:(nIndustries + 2)]))),
  nrow = nIndustries, ncol = nIndustries
)

# Goods x goods matrix Z (Imports)
Z_imp <- matrix(
  as.numeric(as.character(unlist(IO_EU_imports[2:(nIndustries + 1), 3:(nIndustries + 2)]))),
  nrow = nIndustries, ncol = nIndustries
)



# Technical coefficients matrices Domestic
A_dom <- as.matrix(sweep(Z_dom, 2, q_s_dom, FUN = '/'))  # Domestic technical coefficients    

na_pos <- which(is.na(A_dom), arr.ind = TRUE)
if (nrow(na_pos) > 0) {
  message("Replacing ", nrow(na_pos), " NA(s) in A_dom with 0.")
  print(head(na_pos, 20))
}
A_dom[is.na(A_dom)] <- 0

A_imp <- as.matrix(sweep(Z_imp, 2, q_s_dom, FUN = '/'))  # Import technical coefficients
na_pos <- which(is.na(A_imp), arr.ind = TRUE)
if (nrow(na_pos) > 0) {
  message("Replacing ", nrow(na_pos), " NA(s) in A_imp with 0.")
  print(head(na_pos, 20))
}
A_imp[is.na(A_imp)] <- 0


diag <- diag(1, nrow = nIndustries, ncol = nIndustries)                        #Create diagonal matrix
L_dom <- solve(diag-A_dom)                                                  #Leontief inverse         
I_n <- diag(nrow(A_imp))
#L_imp <- solve(I_n - A_imp)

#Imports Relevant stuff
M_intermediate <- matrix(0, nrow = nYears, ncol = nIndustries)
M_final        <- matrix(0, nrow = nYears, ncol = nIndustries)
M_total        <- matrix(0, nrow = nYears, ncol = nIndustries)

M_intermediate[1, ] <- as.numeric(A_imp %*% X_dom[1, ])
M_final[1, ]        <- f_imp[1, ]
M_total[1, ]        <- M_intermediate[1, ] + M_final[1, ]

#L_imp <- solve(diag-A_imp)

# ---- Consistency checks separated by domestic / imports ----
#TotalFinalUse_dom  <- total_use_dom - (A_dom %*% x_dom)
#TotalFinalUseCheck_dom <- (I_n - A_dom) %*% x_dom
#isTRUE(all.equal(TotalFinalUse_dom, TotalFinalUseCheck_dom, f_tol = 1e-3))

#TotalFinalUse_imp  <- total_use_imp - (A_imp %*% x_imp)
#TotalFinalUseCheck_imp <- (I_n - A_imp) %*% x_imp
#isTRUE(all.equal(TotalFinalUse_imp, TotalFinalUseCheck_imp, f_tol = 1e-3))

# Total use checks (domestic and imports)
#TotalUse_dom  <- (A_dom %*% q_s_dom) + final_use_dom
#TotalUseCheck_dom <- L_dom %*% final_use_dom
#isTRUE(all.equal(TotalUse_dom, TotalUseCheck_dom, f_tol = 1e-3))

#TotalUse_imp  <- (A_imp %*% q_s_imp) + final_use_imp
#TotalUseCheck_imp <- L_imp %*% final_use_impisTRUE(all.equal(TotalUse_imp, TotalUseCheck_imp, f_tol = 1e-3))


##########################################
###Extensions
##########################################

#Employment Intensity
#EMPL_IN[i,] = EMPL_IN / X_i [i,] # gives us employment per output of sector

#Emission Intensity
#EM_IN[i,] = EM_IN[i,] / X_i[i,] # gives us emissions per output of sector

#Energy Intensity
#EN_IN[i,] = EN_IN[i,] / X_i[i,] # gives us energy per output of sector

#Land-Use Intensity
#LU_IN[i,] = LU_IN[i,] / X_i[i,] # gives us land-use per output of sector

#Deforestation Intensity
#DEF_IN[i,] = DEF_IN[i,] / X_i[i,] # gives us deforestation per output of sector


# ===================================================================
#  SCENARIOS: SPECIFIC Z, X, AND Y MATRIX EXTRACTION
# ===================================================================

#Initialise Variables

#Scenario 1
A_dom_S1_2030 <- A_dom
A_imp_S1_2030 <- A_imp
Z_dom_S1_2030 <- Z_dom
Z_imp_S1_2030 <- Z_imp
L_dom_S1_2030 <- L_dom
X_S1_2030     <- X_dom


#Scenario 2
A_dom_S2_2030 <- A_dom
A_imp_S2_2030 <- A_imp
Z_dom_S2_2030 <- Z_dom
Z_imp_S2_2030 <- Z_imp
L_dom_S2_2030 <- L_dom
X_S2_2030     <- X_dom


#Scenario 3
A_dom_S3_2030 <- A_dom
A_imp_S3_2030 <- A_imp
Z_dom_S3_2030 <- Z_dom
Z_imp_S3_2030 <- Z_imp
L_dom_S3_2030 <- L_dom
X_S3_2030     <- X_dom



#Source Scenarios Script
#source("Scenarios_biofuels")
#Initialise Value Chains

#2030 Scenario Parameters
#Integrate Here Variable-Specific Changes per Scenario per Time Step
# 1. BASIC SETTINGS
############################################################

nIndustries <- 73

BIOFUEL_SECTORS <- c(
  conv_biodiesel    = 23,
  adv_biodiesel     = 24,
  conv_biogasoline  = 25,
  adv_biogasoline   = 26,
  conv_bio_kerosene = 27,
  adv_bio_kerosene  = 28,
  adv_bio_hfo       = 29,
  RFNBOs            = 30,
  adv_biogas        = 44
)

INPUT_SECTORS <- c(
  agriculture      = 1,
  forestry         = 2,
  food_bev         = 15,
  paper            = 18,
  chemicals        = 21,
  adv_biodiesel    = 23,
  adv_biogasoline  = 26,
  fab_metal        = 34,
  computer_el      = 35,
  elec_equip       = 36,
  machinery        = 37,
  repair_inst      = 41,
  electricity      = 42,
  adv_biogas       = 44,
  sewerage         = 45,
  construction     = 46,
  land_transp      = 50,
  legal_acc        = 64,
  architecture     = 65
)

TAX_RATES <- c(
  default    = 0.0091481,
  adv_biogas = 0.0017690
)

############################################################
# 2. TECHNOLOGY LIBRARY
############################################################

IVC_TECH_LIBRARY <- list(
  IVC1 = list(
    market_value = 1450,
    dist_capex   = c(fab_metal = 0.05, elec_equip = 0.02, machinery = 0.45,
                     construction = 0.15, architecture = 0.30, computer_el = 0.03),
    dist_opex    = c(electricity = 0.35, chemicals = 0.25, legal_acc = 0.20,
                     repair_inst = 0.15, land_transp = 0.05)
  ),
  
  IVC2_HVO = list(
    market_value = 1750,
    dist_capex   = c(fab_metal = 0.05, elec_equip = 0.02, machinery = 0.45,
                     construction = 0.15, architecture = 0.30, computer_el = 0.03),
    dist_opex    = c(electricity = 0.35, chemicals = 0.25, legal_acc = 0.20,
                     repair_inst = 0.15, land_transp = 0.05)
  ),
  
  IVC2_HEFA = list(
    market_value = 2380,
    dist_capex   = c(fab_metal = 0.05, elec_equip = 0.02, machinery = 0.45,
                     construction = 0.15, architecture = 0.30, computer_el = 0.03),
    dist_opex    = c(electricity = 0.35, chemicals = 0.25, legal_acc = 0.20,
                     repair_inst = 0.15, land_transp = 0.05)
  ),
  
  IVC5 = list(
    market_value = 1000,
    dist_capex   = c(fab_metal = 0.00, elec_equip = 0.02, machinery = 0.50,
                     construction = 0.15, architecture = 0.30, computer_el = 0.03),
    dist_opex    = c(electricity = 0.20, chemicals = 0.50, legal_acc = 0.15,
                     repair_inst = 0.10, land_transp = 0.05)
  ),
  
  IVC8a = list(
    market_value = 935,
    dist_capex   = c(fab_metal = 0.00, elec_equip = 0.02, machinery = 0.55,
                     construction = 0.10, architecture = 0.30, computer_el = 0.03),
    dist_opex    = c(electricity = 0.25, chemicals = 0.15, legal_acc = 0.30,
                     repair_inst = 0.30, land_transp = 0.00)
  ),
  
  IVC8b = list(
    market_value = 935,
    dist_capex   = c(fab_metal = 0.00, elec_equip = 0.02, machinery = 0.55,
                     construction = 0.10, architecture = 0.30, computer_el = 0.03),
    dist_opex    = c(electricity = 0.25, chemicals = 0.15, legal_acc = 0.30,
                     repair_inst = 0.30, land_transp = 0.00)
  ),
  
  IVC8c = list(
    market_value = 935,
    dist_capex   = c(fab_metal = 0.00, elec_equip = 0.02, machinery = 0.60,
                     construction = 0.10, architecture = 0.25, computer_el = 0.03),
    dist_opex    = c(electricity = 0.65, chemicals = 0.10, legal_acc = 0.15,
                     repair_inst = 0.10, land_transp = 0.00)
  ),
  
  IVC9b = list(
    market_value = 1275,
    dist_capex   = c(fab_metal = 0.00, elec_equip = 0.02, machinery = 0.60,
                     construction = 0.10, architecture = 0.25, computer_el = 0.03),
    dist_opex    = c(electricity = 0.65, chemicals = 0.10, legal_acc = 0.15,
                     repair_inst = 0.10, land_transp = 0.00)
  ),
  
  IVC11a_SAF = list(
    market_value = 2380,
    dist_capex   = c(fab_metal = 0.00, elec_equip = 0.02, machinery = 0.55,
                     construction = 0.10, architecture = 0.30, computer_el = 0.03),
    dist_opex    = c(electricity = 0.25, chemicals = 0.15, legal_acc = 0.30,
                     repair_inst = 0.30, land_transp = 0.00)
  ),
  
  IVC12 = list(
    market_value = 1000,
    dist_capex   = c(fab_metal = 0.00, elec_equip = 0.02, machinery = 0.50,
                     construction = 0.15, architecture = 0.30, computer_el = 0.03),
    dist_opex    = c(electricity = 0.20, chemicals = 0.50, legal_acc = 0.15,
                     repair_inst = 0.10, land_transp = 0.05)
  ),
  
  IVC13a = list(
    market_value = 1750,
    dist_capex   = c(fab_metal = 0.15, elec_equip = 0.02, machinery = 0.45,
                     construction = 0.10, architecture = 0.25, computer_el = 0.03),
    dist_opex    = c(electricity = 0.15, chemicals = 0.25, legal_acc = 0.20,
                     repair_inst = 0.30, land_transp = 0.10)
  ),
  
  IVC13b_road = list(
    market_value = 1150,
    dist_capex   = c(fab_metal = 0.15, elec_equip = 0.02, machinery = 0.45,
                     construction = 0.10, architecture = 0.25, computer_el = 0.03),
    dist_opex    = c(electricity = 0.15, chemicals = 0.25, legal_acc = 0.20,
                     repair_inst = 0.30, land_transp = 0.10)
  ),
  
  IVC13b_mar = list(
    market_value = 1150,
    dist_capex   = c(fab_metal = 0.15, elec_equip = 0.02, machinery = 0.45,
                     construction = 0.10, architecture = 0.25, computer_el = 0.03),
    dist_opex    = c(electricity = 0.15, chemicals = 0.25, legal_acc = 0.20,
                     repair_inst = 0.30, land_transp = 0.10)
  )
)

############################################################
# 3. HELPER FUNCTIONS
############################################################

get_v <- function(vec, k) {
  if (k %in% names(vec)) unname(vec[k]) else 0
}

## this  is like the technical coefficient vector for each IVC (not full biofuel product yet!)

build_ivc_vector <- function(prod_cost, market_value, alpha_cost,
                             dist_feed, dist_capex, dist_opex) {
  
  r_k     <- prod_cost / market_value
  s_feed  <- r_k * alpha_cost["feed"]
  s_capex <- r_k * alpha_cost["capex"]
  s_opex  <- r_k * alpha_cost["opex"]
  
  a_feed  <- s_feed  * dist_feed
  a_capex <- s_capex * dist_capex
  a_opex  <- s_opex  * dist_opex
  
  a_dom <- c(
    agriculture    = get_v(a_feed, "agriculture"),
    forestry       = get_v(a_feed, "forestry"),
    paper          = get_v(a_feed, "paper"),
    food_bev       = get_v(a_feed, "food_bev"),
    sewerage       = get_v(a_feed, "sewerage") + get_v(a_opex, "sewerage"),
    adv_biodiesel  = get_v(a_feed, "adv_biodiesel"),
    adv_biogas     = get_v(a_feed, "adv_biogas"),
    fab_metal      = get_v(a_capex, "fab_metal"),
    elec_equip     = get_v(a_capex, "elec_equip"),
    machinery      = get_v(a_capex, "machinery"),
    construction   = get_v(a_capex, "construction"),
    architecture   = get_v(a_capex, "architecture"),
    computer_el    = get_v(a_capex, "computer_el"),
    electricity    = get_v(a_opex, "electricity"),
    chemicals      = get_v(a_feed, "chemicals") + get_v(a_opex, "chemicals"),
    legal_acc      = get_v(a_opex, "legal_acc"),
    repair_inst    = get_v(a_opex, "repair_inst"),
    land_transp    = get_v(a_opex, "land_transp")
  )
  
  a_imp <- c(
    agriculture_imp = get_v(a_feed, "agriculture_imp"),
    food_bev_imp    = get_v(a_feed, "food_bev_imp")
  )
  
  list(a_dom = a_dom, a_imp = a_imp)
}


## now technical coefficient vector for teh final bioofuel product
build_fuel_column <- function(fuel_cfg) {
  
  weights <- fuel_cfg$weights
  ivc_ids <- names(weights)
  
  sample_ivc <- ivc_ids[1]
  sample_res <- build_ivc_vector(
    prod_cost    = fuel_cfg$prod_cost[[sample_ivc]],
    market_value = IVC_TECH_LIBRARY[[sample_ivc]]$market_value,
    alpha_cost   = fuel_cfg$alpha[[sample_ivc]],
    dist_feed    = fuel_cfg$dist_feed[[sample_ivc]],
    dist_capex   = IVC_TECH_LIBRARY[[sample_ivc]]$dist_capex,
    dist_opex    = IVC_TECH_LIBRARY[[sample_ivc]]$dist_opex
  )
  
  a_dom_agg <- setNames(numeric(length(sample_res$a_dom)), names(sample_res$a_dom))
  a_imp_agg <- setNames(numeric(length(sample_res$a_imp)), names(sample_res$a_imp))
  
  for (ivc_id in ivc_ids) {
    w <- weights[[ivc_id]]
    
    ivc_res <- build_ivc_vector(
      prod_cost    = fuel_cfg$prod_cost[[ivc_id]],
      market_value = IVC_TECH_LIBRARY[[ivc_id]]$market_value,
      alpha_cost   = fuel_cfg$alpha[[ivc_id]],
      dist_feed    = fuel_cfg$dist_feed[[ivc_id]],
      dist_capex   = IVC_TECH_LIBRARY[[ivc_id]]$dist_capex,
      dist_opex    = IVC_TECH_LIBRARY[[ivc_id]]$dist_opex
    )
    
    a_dom_agg <- a_dom_agg + w * ivc_res$a_dom
    a_imp_agg <- a_imp_agg + w * ivc_res$a_imp
  }
  
  list(a_dom = a_dom_agg, a_imp = a_imp_agg)
}

############################################################
# 4. CLEANED PARTIAL SCENARIO 1 FOR 2030
############################################################

scenario_S1_2030 <- list(
  
  adv_biodiesel = list(
    abs_production_eur = 3108280000,
    weights = c(IVC1 = 0.79304, IVC2_HVO = 0.15764, IVC13a = 0.03429, IVC13b_mar = 0.01502),
    prod_cost = list(
      IVC1 = 870,
      IVC2_HVO = 1287.68,
      IVC13a = 1598.13,
      IVC13b_mar = 2160.57
    ),
    alpha = list(
      IVC1 = c(feed = 0.5977011, capex = 0.0954023, opex = 0.3068966),
      IVC2_HVO = c(feed = 0.476579435, capex = 0.208902273, opex = 0.314518292),
      IVC13a = c(feed = 0.132113077, capex = 0.50371231, opex = 0.364174614),
      IVC13b_mar = c(feed = 0.292548351, capex = 0.317045718, opex = 0.390405932)
    ),
    dist_feed = list(
      IVC1 = c(agriculture_imp = 1.0),
      IVC2_HVO = c(agriculture = 0.14497, adv_biodiesel = 0.72677, chemicals = 0.12825),
      IVC13a = c(agriculture = 0.55703, forestry = 0.22890, paper = 0.20022, food_bev = 0.01387),
      IVC13b_mar = c(agriculture = 0.590480, forestry = 0.210894, paper = 0.184505, food_bev = 0.014121)
    )
  ),
  
  adv_biogasoline = list(
    abs_production_eur = 615780000,
    weights = c(IVC5 = 0.3663, IVC12 = 0.3663, IVC13a = 0.2234, IVC13b_road = 0.0440),
    prod_cost = list(
      IVC5 = 1385.43,
      IVC12 = 988.04,
      IVC13a = 1598.13,
      IVC13b_road = 1740.45
    ),
    alpha = list(
      IVC5 = c(feed = 0.2270, capex = 0.4006, opex = 0.3724),
      IVC12 = c(feed = 0.6200, capex = 0.1000, opex = 0.2800),
      IVC13a = c(feed = 0.5500, capex = 0.1500, opex = 0.3000),
      IVC13b_road = c(feed = 0.5800, capex = 0.1200, opex = 0.3000)
    ),
    dist_feed = list(
      IVC5 = c(agriculture = 0.0, paper = 1.0),
      IVC12 = c(adv_biogas = 1.0),
      IVC13a = c(agriculture = 0.515152, forestry = 0.262022, paper = 0.208388, food_bev = 0.014438, sewerage = 0.0),
      IVC13b_road = c(agriculture = 0.609576, forestry = 0.195602, paper = 0.180971, food_bev = 0.013851, sewerage = 0.0)
    )
  ),
  
  adv_bio_kerosene = list(
    abs_production_eur = 2094400000,
    weights = c(IVC2_HEFA = 0.9418605, IVC11a_SAF = 0.0581395),
    prod_cost = list(
      IVC2_HEFA = 1811.29,
      IVC11a_SAF = 2967.02
    ),
    alpha = list(
      IVC2_HEFA = c(feed = 0.48000000, capex = 0.20800000, opex = 0.31200000),
      IVC11a_SAF = c(feed = 0.19110682, capex = 0.48702100, opex = 0.32187200)
    ),
    dist_feed = list(
      IVC2_HEFA = c(agriculture = 0.569930, adv_biodiesel = 0.365560, chemicals = 0.064511),
      IVC11a_SAF = c(agriculture = 0.572754, forestry = 0.219387, paper = 0.191936, food_bev = 0.014264, adv_biodiesel = 0.001660)
    )
  ),
  
  adv_bio_hfo = list(
    abs_production_eur = 841500000,
    weights = c(IVC8a = 0.5, IVC8b = 0.5),
    prod_cost = list(
      IVC8a = 586.20,
      IVC8b = 936.00
    ),
    alpha = list(
      IVC8a = c(feed = 0.20333994, capex = 0.47936076, opex = 0.31729929),
      IVC8b = c(feed = 0.75641026, capex = 0.14529915, opex = 0.09829060)
    ),
    dist_feed = list(
      IVC8a = c(agriculture = 0.555604, forestry = 0.212818, paper = 0.186189, adv_biodiesel = 0.031552, food_bev = 0.013837),
      IVC8b = c(adv_biogas = 1.0)
    )
  ),
  
  RFNBOs = list(
    abs_production_eur = 201190000,
    weights = c(IVC8c = 0.8019559, IVC9b = 0.1980441),
    prod_cost = list(
      IVC8c = 132.07,
      IVC9b = 394.26
    ),
    alpha = list(
      IVC8c = c(feed = 0.35000000, capex = 0.43160000, opex = 0.21960000),
      IVC9b = c(feed = 0.29000000, capex = 0.32470000, opex = 0.38300000)
    ),
    dist_feed = list(
      IVC8c = c(chemicals = 1.0),
      IVC9b = c(chemicals = 1.0)
    )
  )
)

for (fuel_name in names(scenario_S1_2030)) {
  
  target_col <- BIOFUEL_SECTORS[[fuel_name]]
  fuel_cfg   <- scenario_S1_2030[[fuel_name]]
  
  X_S1_2030[target_col] <- fuel_cfg$abs_production_eur
  res_fuel <- build_fuel_column(fuel_cfg)
  
  for (sec_name in names(res_fuel$a_dom)) {
    if (sec_name %in% names(INPUT_SECTORS)) {
      row_idx <- INPUT_SECTORS[[sec_name]]
      A_dom_S1_2030[row_idx, target_col] <- res_fuel$a_dom[[sec_name]]
    }
  }
  
  for (imp_sec in names(res_fuel$a_imp)) {
    clean_sec <- gsub("_imp$", "", imp_sec)
    if (clean_sec %in% names(INPUT_SECTORS)) {
      row_idx <- INPUT_SECTORS[[clean_sec]]
      A_imp_S1_2030[row_idx, target_col] <- res_fuel$a_imp[[imp_sec]]
    }
  }
}

# ==========================================================
# TO RE-INTEGRATE LATER
# ==========================================================

# adv_biogas = list(
#   abs_production_eur = 1550.80,
#   weights = c(IVC7 = 1.0, IVC9a = 0.0),
#   prod_cost = list(
#     IVC7 = 1024.42,
#     IVC9a = 1369.48
#   ),
#   alpha = list(
#     IVC7 = c(feed = -0.020579, capex = 0.590579, opex = 0.430000),
#     IVC9a = c(feed = 0.154612, capex = 0.583026, opex = 0.262362)
#   ),
#   dist_feed = list(
#     IVC7 = c(agriculture = ?, food_bev = ?, sewerage = ?, chemicals = ?),
#     IVC9a = c(agriculture = ?, food_bev = ?, sewerage = ?, forestry = ?, paper = ?, adv_biodiesel = ?)
#   )
# ),

# conv_biodiesel = list(
#   abs_production_eur = 19194.17,
#   weights = c(
#     IVC_T_FF = 0.2644,
#     IVC_HT_FF = 0.2279,
#     IVC_T_CC = 0.1943,
#     IVC_HT_CC = 0.0978,
#     IVC_T_lipids = 0.1518,
#     IVC_HT_lipids_road = 0.0638
#   ),
#   alpha = list(
#     IVC_T_FF = c(feed = 0.74, capex = 0.07, opex = 0.19),
#     IVC_HT_FF = c(feed = 0.72, capex = 0.10, opex = 0.18),
#     IVC_T_CC = c(feed = 0.74, capex = 0.07, opex = 0.19),
#     IVC_HT_CC = c(feed = 0.67, capex = 0.13, opex = 0.20),
#     IVC_T_lipids = c(feed = 0.74, capex = 0.04, opex = 0.22),
#     IVC_HT_lipids_road = c(feed = 0.67, capex = 0.13, opex = 0.20)
#   ),
#   dist_feed = list(
#     IVC_T_FF = c(food_bev = 1.0),
#     IVC_HT_FF = c(food_bev = 1.0),
#     IVC_T_CC = c(agriculture = 1.0),
#     IVC_HT_CC = c(agriculture = 1.0),
#     IVC_T_lipids = c(food_bev = 1.0),
#     IVC_HT_lipids_road = c(food_bev = 1.0)
#   )
# ),

# conv_biogasoline = list(
#   abs_production_eur = 4436.26,
#   weights = c(IVC_EF_FF = 1.0),
#   alpha = list(
#     IVC_EF_FF = c(feed = 0.70, capex = 0.10, opex = 0.20)
#   ),
#   dist_feed = list(
#     IVC_EF_FF = c(agriculture = 1.0)
#   )
# ),

# conv_bio_kerosene = list(
#   abs_production_eur = 966.98,
#   weights = c(IVC_HT_lipids_SAF = 1.0),
#   alpha = list(
#     IVC_HT_lipids_SAF = c(feed = 0.68, capex = 0.13, opex = 0.19)
#   ),
#   dist_feed = list(
#     IVC_HT_lipids_SAF = c(food_bev = 1.0)
#   )
# )

Z_dom_S1_2030 <- A_dom_S1_2030 %*% diag(x_dom)

#2035 Scenario Parameters


#2040 Scenario Parameters



#Define time loop
for (i in 2:nYears){
  
  cat("Starting iteration:", i, "\n")
  
  for (iterations in 1:100){
    
    # Final expenditure by industry (domestic / imports)
    D_dom[i, ] <- beta_C_dom[i, ] * C_dom[i] + beta_G_dom[i, ] * G_dom[i]
    D_imp[i, ] <- beta_C_imp[i, ] * C_imp[i] + beta_G_imp[i, ] * G_imp[i]
    
    # Total final use f = D + GCF + EX (split)
    #f_dom[i, ] <- D_dom[i, ] + GCF_dom[i, ] + EX_dom[i, ]
    #f_imp[i, ] <- D_imp[i, ] + GCF_imp[i, ] + EX_imp[i, ]
    
    # Domestic production from domestic final demand (Leontief)
    X_dom[i, ] <- L_dom %*% f_dom[i, ]
    #X_imp[i, ] <- L_imp %*% f_imp[i, ]
    
    #Exports ## couple exports to total use
    #EX_i[i, ] <- EX_i[i-1,] 
    
    #Rest of the World
    #RoW[i,] = EX_i[i,] - IM_i[i,]
    
    #Optional totals
    f_total <- f_dom[i, ] + f_imp[i, ]
    #X_total <- X_dom[i, ] + X_imp[i, ]
    
    #Household Consumption
    #C
    
    #Government Consumption
    #G
    
    #Wages ?
    #
    
    #Employment
    
    # Energy
    #EN
    
    # Emissions
    #EM
    
    #Land Use
    
    #Deforestation
    #Water Impact
    
    
  } 
}
