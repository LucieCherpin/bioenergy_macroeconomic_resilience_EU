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

t_f_e <- as.numeric(unlist(IO_EU_domestic[2:(nIndustries+1), nIndustries+3])) +                   ## is this really in row 2 that sectors start? isn't it row 3?
         as.numeric(unlist(IO_EU_imports[2:(nIndustries+1), nIndustries+3]))

t_f_e <- as.numeric(unlist(IO_Austria[2:(nIndustries + 1), nIndustries + 3]))           


expenditure_tax <- as.numeric(unlist(IO_Austria[nIndustries + 8, nIndustries + 3]))             #D21X31 - Taxes less subsidies on products as part of final expenditure
expenditure_tax_i <- consumption_tax_i + g_cons_tax_i         #D21X31 - Taxes less subsidies on products as part of final expenditure by industry

D_i = matrix(rep(t_f_e, times = nYears), nrow = nYears, byrow = TRUE)                   #Total final expenditure by industry matrix
D_tax_i = matrix(rep(expenditure_tax_i, times = nYears),nrow = nYears, byrow = TRUE)    #D21X31 - Taxes less subsidies on products as part of final expenditure by industry matrix

#TU & TFU - Total use & total final use
total_use <- as.matrix(as.numeric(unlist(IO_Austria[2:(nIndustries + 1), nIndustries + 17])))      #TU - Total use by industry
FINAL = matrix(rep(total_use, times = nYears), nrow = nYears, byrow = TRUE)   #TU - Total use by industry matrix
final_use <- as.numeric(unlist(IO_Austria[2:(nIndustries + 1), nIndustries + 18]))                  #TFU - Total final use by industry
f_i = matrix(rep(final_use, times = nYears), nrow = nYears, byrow = TRUE)       #TFU - Total final use by industry matrix


#Production and sales
gva_i <- t(as.matrix(as.numeric(unlist(IO_Austria[nIndustries + 2, 3:(nIndustries + 2)]))))   #B1G - Gross value added by industry
GVA_i = matrix(rep(gva_i, times = nYears), nrow = nYears, byrow = TRUE)     #B1G - Gross value added by industry matrix  
GVA = matrix(rep(sum(gva_i), times = nYears), nrow = nYears, byrow = TRUE)  #B1G - Gross value added matrix 
sales <- as.matrix(as.numeric(unlist(IO_Austria[nIndustries + 10, 3:(nIndustries + 2)])))      #P1 - Total sales (output) by industry
SALES_i <- matrix(rep(sales, times = nYears), nrow = nYears, byrow = TRUE)    #P1 - Total sales (output) by industry matrix
#delta_SALES <- matrix(rep(0, times = nYears), ncol= nIndustries, nrow = nYears, byrow = TRUE)     #Excess Supply: Change of Sales
q_s <- as.matrix(as.numeric(unlist(IO_Austria[nIndustries + 19, 3:(nIndustries + 2)])))         #TS_BP - Total supply by industry
q_s[q_s == 0] <- 1e-6                                                       #Necessary for calculations
Q_s_i = matrix(rep(q_s, times = nYears), nrow = nYears, byrow = TRUE)         #TS_BP - Total supply by industry matrix
x <- as.matrix(as.numeric(unlist(IO_Austria[2:(nIndustries + 1), nIndustries + 17])))           #TU - Total use by industry
X_i = matrix(rep(x, times = nYears), nrow = nYears, byrow = TRUE)             #TU - Total use by industry matrix
i_d <- as.numeric(unlist(IO_Austria[nIndustries + 11, 3:(nIndustries + 2)]))                    #P2_ADJ - Total intermediate consumption by industry
I_D_i = matrix(rep(i_d, times = nYears), nrow = nYears, byrow = TRUE)       #P2_ADJ - Total intermediate consumption by industry matrix

##########################################
###Final Expenditure
##########################################

#Total consumption shares by industry
beta_bar <- as.matrix(
  (as.numeric(unlist(IO_Austria[2:(nIndustries + 1), nIndustries + 4])) +
     as.numeric(unlist(IO_Austria[2:(nIndustries + 1), nIndustries + 5])) +
     as.numeric(unlist(IO_Austria[2:(nIndustries + 1), nIndustries + 6]))) /
    (as.numeric(unlist(IO_Austria[1, nIndustries + 4])) +
       as.numeric(unlist(IO_Austria[1, nIndustries + 5])) +
       as.numeric(unlist(IO_Austria[1, nIndustries + 6])))
)                                                                              

#Household consumption shares by industry
beta_c_bar <- as.matrix(
  (as.numeric(unlist(IO_Austria[2:(nIndustries + 1), nIndustries + 5])) +
     as.numeric(unlist(IO_Austria[2:(nIndustries + 1), nIndustries + 6]))) /
    (as.numeric(unlist(IO_Austria[1, nIndustries + 5])) +
       as.numeric(unlist(IO_Austria[1, nIndustries + 6])))
)                                                                               

#Real household consumption composition
beta_C = matrix(rep(beta_c_bar, times = nYears), nrow = nYears, byrow = TRUE)   

#Government expenditure shares by industry 
beta_g_bar <- as.matrix(
  as.numeric(unlist(IO_Austria[2:(nIndustries + 1), nIndustries + 4])) /
    as.numeric(unlist(IO_Austria[1, nIndustries + 4]))
)                                                                                                                       

#Real government expenditure composition
beta_G = matrix(rep(beta_g_bar, times = nYears), nrow = nYears, byrow = TRUE) 

#P3_S13 - Government expenditure
g <- as.numeric(national_accounts[4,2])                 #P3_S13 - Government expenditure
g_i <- as.numeric(unlist(IO_Austria[2:(nIndustries + 1), nIndustries + 4]))         #P3_S13 - Government expenditure by industry
g_cons_tax <- as.numeric(unlist(IO_Austria[nIndustries + 8, nIndustries + 4]))            #D21X31_S13 - Taxes less subsidies on products as part of final government expenditure
g_cons_tax_i <- beta_g_bar * g_cons_tax                 #D21X31_S13 - Taxes less subsidies on products as part of final government expenditure by industry

G = matrix(data = sum(g_i), ncol = nYears)              #P3_S13 - Government expenditure matrix
G_tax = matrix(data = sum(g_cons_tax), ncol = nYears)   #D21X31_S13 - Taxes less subsidies on products as part of final government expenditure matrix

G_i = matrix(rep(g_i, times = nYears), nrow = nYears, byrow = TRUE)                #P3_S13 - Government expenditure by industry matrix
G_i_tax = matrix(rep(g_cons_tax_i, times = nYears), nrow = nYears, byrow = TRUE)   #D21X31_S13 - Taxes less subsidies on products as part of final government expenditure by indusrty matrix

#P3_S14_S15 - Household consumption
cons <- as.numeric(national_accounts[7,2])                    #P3_S14_S15 - Household and NPISH consumption
hh_cons <- as.matrix(as.numeric(unlist(IO_Austria[2:(nIndustries + 1), nIndustries + 5])))        #P3_S14 - Household consumption by industry
npish_cons <- as.matrix(as.numeric(unlist(IO_Austria[2:(nIndustries + 1), nIndustries + 6])))     #P3_S15 - NPISH consumption by industry
consumption <- hh_cons + npish_cons                           #P3_S14_S15 - Household and NPISH consumption by industry
consumption_tax <- as.numeric(unlist(IO_Austria[nIndustries + 8, nIndustries + 5]))             #D21X31_S14_S15 - Taxes less subsidies on products as part of final household and NPISH consumption
consumption_tax_i <- beta_c_bar * consumption_tax             #D21X31_S14_S15 - Taxes less subsidies on products as part of final household and NPISH consumption by industry

C = matrix(data = sum(consumption), ncol = nYears)            #P3_S14_S15 - Household and NPISH consumption matrix
C_tax = matrix(data = sum(consumption_tax), ncol = nYears)    #D21X31_S14_S15 - Taxes less subsidies on products as part of final household and NPISH consumption matrix

C_i = matrix(rep(consumption, times = nYears), nrow = nYears, byrow = TRUE)            #P3_S14_S15 - Household and NPISH consumption by industry matrix
C_i_tax <- matrix(rep(consumption_tax_i, times = nYears),nrow = nYears, byrow = TRUE)  #D21X31_S14_S15 - Taxes less subsidies on products as part of final household and NPISH consumption by industry matrix

##########################################
###Technical coefficients matrix (A matrix)
##########################################
Z <- as.matrix(IO_Austria[2:(nIndustries + 1), 3:(nIndustries + 2)], col_names = F)    #Goods x goods matrix Z
Z_i <- matrix(as.numeric(Z), nrow = nIndustries, ncol = nIndustries)                     #Numeric (necessary for sweep function)

A <- as.matrix(sweep(Z_i, 2, q_s, FUN = '/'))                             #Divide each entry of the IOT by total output - Jan: Needs to be put into the loop and get updated via some change. Aditya: The shares should be changed,
diag <- diag(ncol = ncol(A), nrow = nrow(A))                              #Create diagonal matrix
L <- solve(diag-A)                                                        #Leontief inverse

#Check for total final use
TotalFinalUse <- total_use - (A %*% x)    
TotalFinalUseCheck <- (diag-A) %*% x
isTRUE(all.equal(TotalFinalUse, TotalFinalUseCheck, final_use, tolerance = 1e-3))

#Check for total use
TotalUse <- (A %*% q_s) + final_use
TotalUseCheck <- L%*%final_use
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
    
    # POPULATION
    
    # Adult population (ages 20+) — groups II, III, IV
    #pop_adult_i <- sum(POP[i, 2:4]) * 1e6
    
    # Population change
    #delta_pop <- numeric(length(agegroups))

    #Total population (million people)
    #POP[i, ] <- POP[i-1, ] + delta_pop
      
    #Working age population (people)
    #working_age_pop <- sum(POP[i, 2:4]) * 1e6
    

    #A) Define input-output structure
    
    #Total final expenditure by industry // here we have to substract taxes from C and G (when we have integrated taxes in C and G)
    D_i[i,] = beta_C[i,]*C[i] + beta_G[i,]*G[i] 
    
    #Total final use
    f_i[i,] = D_i[i,] + GCF_i[i,] + EX_i[i,]
    
    #Imports-  Linus: work in progress, developing the skeleton for later integration - see line 770 for initialisation
    #IM_i[i,] = IM_L %*% IM_f[i, ]   #  for the case that we are developing an A-matrix for imports
    #IM_i[i,] = IM_i[i-1,] - this should be 
    

    
    #Exports
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
