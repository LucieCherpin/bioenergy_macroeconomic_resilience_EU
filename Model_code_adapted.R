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

IO_EU_domestic <- read_xlsx("IOT_EU27_Domestic.xlsx", sheet= "Sheet 1", range = "A9:CP104", col_names = T)
IO_EU_domestic <- as.data.frame(IO_EU_domestic)

IO_EU_imports <- read_xlsx("IOT_EU27_imports_10_08.xlsx", sheet= "Sheet 1", range = "A9:CP104", col_names = T)
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

t_f_e_dom <- as.numeric(unlist(IO_EU_domestic[2:(nIndustries+1), nIndustries+3]))                  ## "2:" because the first row is the label row and not yet values
t_f_e_imp <- as.numeric(unlist(IO_EU_imports[2:(nIndustries+1), nIndustries+3]))

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
g_cons_tax_imp <- as.numeric(unlist(IO_EU_imports[nIndustries + 8, nIndustries + 4]))
g_cons_tax_i_dom <- beta_g_bar_dom * g_cons_tax_dom                 #D21X31_S13 - Taxes less subsidies on products as part of final government expenditure by industry
g_cons_tax_i_imp <- beta_g_bar_imp * g_cons_tax_imp

G_dom = matrix(data = sum(g_dom), ncol = nYears)              #P3_S13 - Government expenditure matrix
G_tax = matrix(data = sum(g_cons_tax_dom), ncol = nYears)         #D21X31_S13 - Taxes less subsidies on products as part of final government expenditure matrix
G_tax = matrix(data = sum(g_cons_tax_imp), ncol = nYears)

G_i_dom = matrix(rep(g_dom, times = nYears), nrow = nYears, byrow = TRUE)                #P3_S13 - Government expenditure by industry matrix
G_i_tax_dom = matrix(rep(g_cons_tax_i_dom, times = nYears), nrow = nYears, byrow = TRUE)   #D21X31_S13 - Taxes less subsidies on products as part of final government expenditure by indusrty matrix
G_i_imp = matrix(rep(g_imp, times = nYears), nrow = nYears, byrow = TRUE)                
G_i_tax_dom = matrix(rep(g_cons_tax_i_imp, times = nYears), nrow = nYears, byrow = TRUE)

#P3_S14_S15 - Household consumption
cons <- as.numeric(national_accounts[7,2])                    #P3_S14_S15 - Household and NPISH consumption
hh_cons_dom <- as.numeric(unlist(IO_EU_domestic[2:(nIndustries + 1), nIndustries + 5]))
hh_cons_imp <- as.numeric(unlist(IO_EU_imports[2:(nIndustries + 1), nIndustries + 5]))                       #P3_S14 - Household consumption by industry
                     
npish_cons_dom <- as.numeric(unlist(IO_EU_domestic[2:(nIndustries + 1), nIndustries + 6]))
npish_cons_imp <- as.numeric(unlist(IO_EU_imports[2:(nIndustries + 1), nIndustries + 6]))                    #P3_S15 - NPISH consumption by industry

consumption_dom = hh_cons_dom + npish_cons_dom
consumption_imp = hh_cons_imp + npish_cons_imp                         #P3_S14_S15 - Household and NPISH consumption by industry

consumption_tax_dom <- as.numeric(unlist(IO_EU_domestic[nIndustries + 8, nIndustries + 5]))
consumption_tax_imp <- as.numeric(unlist(IO_EU_imports[nIndustries + 8, nIndustries + 5]))             #D21X31_S14_S15 - Taxes less subsidies on products as part of final household and NPISH consumption

consumption_tax_i_dom <- beta_c_bar_dom * consumption_tax_dom                                                      #D21X31_S14_S15 - Taxes less subsidies on products as part of final household and NPISH consumption by industry
consumption_tax_i_imp <- beta_c_bar_imp * consumption_tax_imp     

C_dom = matrix(data = sum(consumption_dom), ncol = nYears)            #P3_S14_S15 - Household and NPISH consumption matrix
C_imp = matrix(data = sum(consumption_imp), ncol = nYears)           

C_tax_dom = matrix(data = sum(consumption_tax_dom), ncol = nYears)    #D21X31_S14_S15 - Taxes less subsidies on products as part of final household and NPISH consumption matrix
C_tax_imp = matrix(data = sum(consumption_tax_imp), ncol = nYears) 

C_i_dom = matrix(rep(consumption_dom, times = nYears), nrow = nYears, byrow = TRUE)            #P3_S14_S15 - Household and NPISH consumption by industry matrix
C_i_tax_dom <- matrix(rep(consumption_tax_i_dom, times = nYears),nrow = nYears, byrow = TRUE)  #D21X31_S14_S15 - Taxes less subsidies on products as part of final household and NPISH consumption by industry matrix
C_i_imp = matrix(rep(consumption_imp, times = nYears), nrow = nYears, byrow = TRUE)    
C_i_tax_imp <- matrix(rep(consumption_tax_i_imp, times = nYears),nrow = nYears, byrow = TRUE)

##########################################
###Technical coefficients matrix (A matrix)
##########################################

# Goods x goods matrix Z (Domestic)
Z_dom   <- as.matrix(IO_EU_domestic[2:(nIndustries + 1), 3:(nIndustries + 2)], col_names = F)

# Goods x goods matrix Z (Imports)
Z_imp   <- as.matrix(IO_EU_imports[2:(nIndustries + 1), 3:(nIndustries + 2)], col_names = F)

# Total intermediate transactions matrix Z -  Outcommented
#Z_tot <- Z_dom + Z_imp

# Technical coefficients matrices
A_dom <- as.matrix(sweep(Z_dom, 2, q_s_dom, FUN = '/'))  # Domestic technical coefficients              #Divide each entry of the IOT by total output - Jan: Needs to be put into the loop and get updated via some change. Aditya: The shares should be changed,
A_imp <- as.matrix(sweep(Z_imp, 2, q_s_imp, FUN = '/'))  # Import technical coefficients
#A_tot <- A_dom + A_imp                                 # Total technological coefficients

diag <- diag(1, nrow = nIndustries, ncol = nIndustries)                        #Create diagonal matrix
L_dom <- solve(diag  -A_dom)                                                  #Leontief inverse         
L_imp <- solve(diag-A_imp)

# ---- Consistency checks separated by domestic / imports ----
TotalFinalUse_dom  <- total_use_dom - (A_dom %*% x_dom)
TotalFinalUseCheck_dom <- (I_n - A_dom) %*% x_dom
isTRUE(all.equal(TotalFinalUse_dom, TotalFinalUseCheck_dom, f_tol = 1e-3))

TotalFinalUse_imp  <- total_use_imp - (A_imp %*% x_imp)
TotalFinalUseCheck_imp <- (I_n - A_imp) %*% x_imp
isTRUE(all.equal(TotalFinalUse_imp, TotalFinalUseCheck_imp, f_tol = 1e-3))

# Total use checks (domestic and imports)
TotalUse_dom  <- (A_dom %*% q_s_dom) + final_use_dom
TotalUseCheck_dom <- L_dom %*% final_use_dom
isTRUE(all.equal(TotalUse_dom, TotalUseCheck_dom, f_tol = 1e-3))

TotalUse_imp  <- (A_imp %*% q_s_imp) + final_use_imp
TotalUseCheck_imp <- L_imp %*% final_use_imp
isTRUE(all.equal(TotalUse_imp, TotalUseCheck_imp, f_tol = 1e-3))


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


#Define time loop
for (i in 2:nYears){
  
  cat("Starting iteration:", i, "\n")
  
  for (iterations in 1:100){
    
    # Final expenditure by industry (domestic / imports)
     D_dom[i, ] <- beta_C_dom[i, ] * C_dom[i] + beta_G_dom[i, ] * G_dom[i]
     D_imp[i, ] <- beta_C_imp[i, ] * C_imp[i] + beta_G_imp[i, ] * G_imp[i]

    # Total final use f = D + GCF + EX (split)
     f_dom[i, ] <- D_dom[i, ] + GCF_dom[i, ] + EX_dom[i, ]
     f_imp[i, ] <- D_imp[i, ] + GCF_imp[i, ] + EX_imp[i, ]

    # Domestic production from domestic final demand (Leontief)
     X_dom[i, ] <- L_dom %*% f_dom[i, ]
     X_imp[i, ] <- L_imp %*% f_imp[i, ]

    #Exports ## couple exports to total use
    #EX_i[i, ] <- EX_i[i-1,] 
    
    #Rest of the World
    #RoW[i,] = EX_i[i,] - IM_i[i,]

    #Optional totals
    f_total <- f_dom[i, ] + f_imp[i, ]
    X_total <- X_dom_ts[i, ] + X_imp_ts[i, ]

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
