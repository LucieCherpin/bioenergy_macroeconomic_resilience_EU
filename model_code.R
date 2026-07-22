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

#Input-Output Table Austria (Eurostat)
#IO_Austria <- read_xlsx("FINAL_IOT_AT_2021_TOTAL_fuels_reaggregated.xlsx", sheet= "Sheet 1", range = "A11:CE95", col_names = T)
IO_Austria <- read_xlsx("IOT_AT_2021_TOTAL_fuels_reaggregated.xlsx", sheet= "Sheet 1", range = "A11:DC119", col_names = T)
IO_Austria <- as.data.frame(IO_Austria)

#Here Integrate Extensions
#Employment E.G:
#employment <- read_xlsx("nama_10_a64_e.xlsx", sheet = "Sheet 1", range = "A11:X77", col_names = T)

#####################################################################
###STEP 2: Defining Coefficients (Exogenous Variables and Parameters)
#####################################################################

##########################################
###General Parameters
##########################################

nYears = 20             #Number of years
nIndustries = 89        #Industries of Eurostat input-output table


#Initialisation
x <- as.matrix(as.numeric(unlist(IO_Austria[2:(nIndustries + 1), nIndustries + 17])))           #TU - Total use by industry
X_i = matrix(rep(x, times = nYears), nrow = nYears, byrow = TRUE)             #TU - Total use by industry matrix
i_d <- as.numeric(unlist(IO_Austria[nIndustries + 11, 3:(nIndustries + 2)]))                    #P2_ADJ - Total intermediate consumption by industry
I_D_i = matrix(rep(i_d, times = nYears), nrow = nYears, byrow = TRUE)       #P2_ADJ - Total intermediate consumption by industry matrix
