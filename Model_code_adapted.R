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

#### adpated: switch from  Austria to EU; have two tables for doemstic cosnumption and imports

#Input-Output Table EU27 (Eurostat)

IO_EU_domestic <- read_xlsx("IOT_EU_2023_domestic_biofuels_disaggregated.xlsx", sheet= "Sheet 1", range = "A9:CP104", col_names = T)
IO_EU_domestic <- as.data.frame(IO_EU_domestic)

IO_EU_imports <- read_xlsx("IOT_EU_2023_imports_biofuels_disaggregated.xlsx", sheet= "Sheet 1", range = "A9:CP104", col_names = T)
IO_EU_imports <- as.data.frame(IO_EU_imports)


#National Accounts
national_accounts <- read_xlsx("nama_10_gdp_EU27.xlsx", sheet = "Sheet 1", range = "A10:B48", col_names = F)

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

t_f_e_dom <- as.numeric(unlist(IO_EU_domestic[2:(nIndustries+1), nIndustries+3]))                  ## "2:" because the first row is the label row and not yet values
t_f_e_im <- as.numeric(unlist(IO_EU_imports[2:(nIndustries+1), nIndustries+3]))

#Total Final Expenditure - Do we need this?
D_dom = matrix(rep(t_f_e_dom, times = nYears), nrow = nYears, byrow = TRUE)
D_imp = matrix(rep(t_f_e_imp, times = nYears), nrow = nYears, byrow = TRUE)

### D21X31 - Taxes less subsidies on products as part of final expenditure
expenditure_tax_dom <- as.numeric(unlist(IO_EU_domestic[nIndustries + 8, nIndustries + 3]))    ### column nIndustries + 3 is "final consumotion expenditure"
expenditure_tax_imp <- as.numeric(unlist(IO_EU_imports[nIndustries + 8, nIndustries + 3]))          

D_tax_dom = matrix(rep(expenditure_tax_dom, times = nYears), nrow = nYears, byrow = TRUE)   #D21X31 - Taxes less subsidies on products as part of final expenditure by industry matrix
D_tax_imp = matrix(rep(expenditure_tax_imp, times = nYears), nrow = nYears, byrow = TRUE)

#TU & TFU - Total use & total final use
total_use_dom <- as.numeric(unlist(IO_EU_domestic[2:(nIndustries + 1), nIndustries + 17]))     #TU - Total use by industry
total_use_imp <- as.numeric(unlist(IO_EU_imports[2:(nIndustries + 1), nIndustries + 17]))

FINAL_dom = matrix(rep(total_use_dom, times = nYears), nrow = nYears, byrow = TRUE)    #TU - Total use by industry matrix
FINAL_imp = matrix(rep(total_use_imp, times = nYears), nrow = nYears, byrow = TRUE)

final_use_dom <- as.numeric(unlist(IO_EU_domestic[2:(nIndustries + 1), nIndustries + 18]))    #TFU - Total final use by industry
final_use_imp <- as.numeric(unlist(IO_EU_imports[2:(nIndustries + 1), nIndustries + 18]))

f_dom = matrix(rep(final_use_dom, times = nYears), nrow = nYears, byrow = TRUE)      #TFU - Total final use by industry matrix
f_imp = matrix(rep(final_use_imp, times = nYears), nrow = nYears, byrow = TRUE)

#Production and sales, only looking at domestic
gva_dom <- as.numeric(unlist(IO_EU_domestic[nIndustries + 2, 3:(nIndustries + 2)])) #B1G - Gross value added by industry
gva_imp <- as.numeric(unlist(IO_EU_imports[nIndustries + 2, 3:(nIndustries + 2)]))
GVA_dom = matrix(rep(gva_dom, times = nYears), nrow = nYears, byrow = TRUE)   #B1G - Gross value added by industry matrix  
GVA_imp = matrix(rep(gva_imp, times = nYears), nrow = nYears, byrow = TRUE)

## only measuring doemstic EU output generation by industry, 
sales_dom <- as.numeric(unlist(IO_EU_domestic[nIndustries + 10, 3:(nIndustries + 2)]))  #P1 - Total sales (output) by industry
sales_imp <- as.numeric(unlist(IO_EU_imports[nIndustries + 10, 3:(nIndustries + 2)]))

SALES_dom = matrix(rep(sales_dom, times = nYears), nrow = nYears, byrow = TRUE)    #P1 - Total sales (output) by industry matrix
SALES_imp = matrix(rep(sales_imp, times = nYears), nrow = nYears, byrow = TRUE)

q_s_dom <- as.numeric(unlist(IO_EU_domestic[nIndustries + 19, 3:(nIndustries + 2)]))  #TS_BP - Total supply by industry
q_s_imp <- as.numeric(unlist(IO_EU_imports[nIndustries + 19, 3:(nIndustries + 2)]))
q_s_dom[q_s_dom == 0] <- 1e-6                                                                #Necessary for calculations
q_s_imp[q_s_imp == 0] <- 1e-6                                                                
Q_s_dom_i = matrix(rep(q_s_dom, times = nYears), nrow = nYears, byrow = TRUE)   #TS_BP - Total supply by industry matrix
Q_s_imp_i = matrix(rep(q_s_imp, times = nYears), nrow = nYears, byrow = TRUE)

### total use of intermediate inputs
x_dom <- as.numeric(unlist(IO_EU_domestic[2:(nIndustries + 1), nIndustries + 17]))                   #TU - Total use by industry
x_imp <- as.numeric(unlist(IO_EU_imports[2:(nIndustries + 1), nIndustries + 17]))
X_dom = matrix(rep(x_dom, times = nYears), nrow = nYears, byrow = TRUE)                             #TU - Total use by industry matrix
X_imp = matrix(rep(x_imp, times = nYears), nrow = nYears, byrow = TRUE)

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
beta_g_bar <- as.matrix(
  (as.numeric(unlist(IO_EU_domestic[2:(nIndustries + 1), nIndustries + 4])) + as.numeric(unlist(IO_EU_imports[2:(nIndustries + 1), nIndustries + 4]))) /
  (as.numeric(unlist(IO_EU_domestic[1, nIndustries + 4])) + as.numeric(unlist(IO_EU_imports[1, nIndustries + 4])))
)

#Real government expenditure composition
beta_G = matrix(rep(beta_g_bar, times = nYears), nrow = nYears, byrow = TRUE) 

#P3_S13 - Government expenditure
g <- as.numeric(national_accounts[4,2])                 #P3_S13 - Government expenditure
g_i <- as.numeric(unlist(IO_EU_domestic[2:(nIndustries + 1), nIndustries + 4])) +            #P3_S13 - Government expenditure by industry
       as.numeric(unlist(IO_EU_imports[2:(nIndustries + 1), nIndustries + 4]))
g_cons_tax <- as.numeric(unlist(IO_EU_domestic[nIndustries + 8, nIndustries + 4])) +           #D21X31_S13 - Taxes less subsidies on products as part of final government expenditure
               as.numeric(unlist(IO_EU_imports[nIndustries + 8, nIndustries + 4]))

g_cons_tax_i <- beta_g_bar * g_cons_tax                 #D21X31_S13 - Taxes less subsidies on products as part of final government expenditure by industry

G = matrix(data = sum(g_i), ncol = nYears)              #P3_S13 - Government expenditure matrix
G_tax = matrix(data = sum(g_cons_tax), ncol = nYears)   #D21X31_S13 - Taxes less subsidies on products as part of final government expenditure matrix

G_i = matrix(rep(g_i, times = nYears), nrow = nYears, byrow = TRUE)                #P3_S13 - Government expenditure by industry matrix
G_i_tax = matrix(rep(g_cons_tax_i, times = nYears), nrow = nYears, byrow = TRUE)   #D21X31_S13 - Taxes less subsidies on products as part of final government expenditure by indusrty matrix

#P3_S14_S15 - Household consumption
cons <- as.numeric(national_accounts[7,2])                    #P3_S14_S15 - Household and NPISH consumption
hh_cons <- as.matrix(as.numeric(unlist(IO_EU_domestic[2:(nIndustries + 1), nIndustries + 5])) +           #P3_S14 - Household consumption by industry
                     as.numeric(unlist(IO_EU_imports[2:(nIndustries + 1), nIndustries + 5])))
npish_cons <- as.matrix(as.numeric(unlist(IO_EU_domestic[2:(nIndustries + 1), nIndustries + 6])) +           #P3_S15 - NPISH consumption by industry
                        as.numeric(unlist(IO_EU_imports[2:(nIndustries + 1), nIndustries + 6])))

consumption <- hh_cons + npish_cons                           #P3_S14_S15 - Household and NPISH consumption by industry

consumption_tax <- as.numeric(unlist(IO_EU_domestic[nIndustries + 8, nIndustries + 5])) +             #D21X31_S14_S15 - Taxes less subsidies on products as part of final household and NPISH consumption
                    as.numeric(unlist(IO_EU_imports[nIndustries + 8, nIndustries + 5]))
consumption_tax_i <- beta_c_bar * consumption_tax                                                      #D21X31_S14_S15 - Taxes less subsidies on products as part of final household and NPISH consumption by industry


C = matrix(data = sum(consumption), ncol = nYears)            #P3_S14_S15 - Household and NPISH consumption matrix

C_tax = matrix(data = sum(consumption_tax), ncol = nYears)    #D21X31_S14_S15 - Taxes less subsidies on products as part of final household and NPISH consumption matrix

C_i = matrix(rep(consumption, times = nYears), nrow = nYears, byrow = TRUE)            #P3_S14_S15 - Household and NPISH consumption by industry matrix
C_i_tax <- matrix(rep(consumption_tax_i, times = nYears),nrow = nYears, byrow = TRUE)  #D21X31_S14_S15 - Taxes less subsidies on products as part of final household and NPISH consumption by industry matrix

##########################################
###Technical coefficients matrix (A matrix)
##########################################

# Goods x goods matrix Z (Domestic)
Z_dom   <- as.matrix(IO_EU_domestic[2:(nIndustries + 1), 3:(nIndustries + 2)], col_names = F)

# Goods x goods matrix Z (Imports)
Z_imp   <- as.matrix(IO_EU_imports[2:(nIndustries + 1), 3:(nIndustries + 2)], col_names = F)

# Total intermediate transactions matrix Z
Z_tot <- Z_dom + Z_imp

# Technical coefficients matrices
A_dom <- as.matrix(sweep(Z_dom, 2, q_s, FUN = '/'))  # Domestic technical coefficients              #Divide each entry of the IOT by total output - Jan: Needs to be put into the loop and get updated via some change. Aditya: The shares should be changed,
A_imp <- as.matrix(sweep(Z_imp, 2, q_s, FUN = '/'))  # Import technical coefficients
A_tot <- A_dom + A_imp                                 # Total technological coefficients

diag <- diag(ncol = ncol(A_dom), nrow = nrow(A_dom))                         #Create diagonal matrix
L_dom <- solve(diag-A_dom)                                                   #Leontief inverse          ## imports not cosnidered, because they don't trigger domestic production feedback loops 

#Check for total final use
TotalFinalUse <- total_use - (A_dom %*% x)    
TotalFinalUseCheck <- (diag-A_dom) %*% x
isTRUE(all.equal(TotalFinalUse, TotalFinalUseCheck, final_use, tolerance = 1e-3))

#Check for total use
TotalUse <- (A_dom %*% q_s) + final_use
TotalUseCheck <- L_dom%*%final_use
isTRUE(all.equal(TotalUse, TotalUseCheck, total_use, tolerance = 1e-3))

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

##########################################
###Imports and Exports
##########################################


#Define time loop
for (i in 2:nYears){
  
  cat("Starting iteration:", i, "\n")
  
  for (iterations in 1:100){
    
    #####################################
    #Define System of Equations     
    #####################################
    
  
    

    #A) Define input-output structure
    
    #Total final expenditure by industry // here we have to substract taxes from C and G (when we have integrated taxes in C and G)
    D_i[i,] = beta_C[i,]*C[i] + beta_G[i,]*G[i] 
    
    #Total final use
    f_i[i,] = D_i[i,] + GCF_i[i,] + EX_i[i,]
  
    #Exports ## couple exports to total use
    EX_i[i, ] <- EX_i[i-1,] 

    #Total use by industry (Leontief production function): Domestic
    X_i[i,] = L %*% f_i[i, ]   
    
    #Rest of the World
    RoW[i,] = EX_i[i,] - IM_i[i,]


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
