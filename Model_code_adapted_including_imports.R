#####################################################################
### Model_code_adapted_including_imports.R
### Adapted version of Model_code_adapted.R that uses EU domestic and
### imports IO tables and builds technical coefficients including imports.
#####################################################################

# Clear working environment
rm(list = ls())

# Packages
library(readxl)
library(dplyr)

# Clear plots/console
if (!is.null(dev.list())) dev.off()
cat("\014")
options(scipen = 999)

##########################################
### Import Data
##########################################

IO_EU_domestic <- read_xlsx("IOT_EU_2023_domestic_biofuels_disaggregated.xlsx",
                            sheet = "Sheet 1", range = "A9:CP104", col_names = TRUE)
IO_EU_domestic <- as.data.frame(IO_EU_domestic)

IO_EU_imports <- read_xlsx("IOT_EU_2023_imports_biofuels_disaggregated.xlsx",
                           sheet = "Sheet 1", range = "A9:CP104", col_names = TRUE)
IO_EU_imports <- as.data.frame(IO_EU_imports)

national_accounts <- read_xlsx("nama_10_gdp_EU27.xlsx",
                               sheet = "Sheet 1", range = "A10:B48", col_names = FALSE)

#####################################################################
### Parameters
#####################################################################

nYears <- 20
nIndustries <- 89

##########################################
### Build Z, q and A matrices (including imports)
##########################################

# Numeric Z matrices (goods x goods)
Z_dom <- as.matrix(IO_EU_domestic[2:(nIndustries + 1), 3:(nIndustries + 2)])
Z_imp <- as.matrix(IO_EU_imports[2:(nIndustries + 1), 3:(nIndustries + 2)])
Z_total <- Z_dom + Z_imp

# Total supply by industry (row n + 19 in original layout)
q_dom <- as.numeric(unlist(IO_EU_domestic[nIndustries + 19, 3:(nIndustries + 2)]))
q_imp <- as.numeric(unlist(IO_EU_imports[nIndustries + 19, 3:(nIndustries + 2)]))
q_total <- q_dom + q_imp

# Avoid zero division
q_dom[q_dom == 0] <- 1e-6
q_total[q_total == 0] <- 1e-6

# Technical coefficient matrices (including imports)
A_total <- matrix(as.numeric(Z_total), nrow = nIndustries, ncol = nIndustries)
A_total <- sweep(A_total, 2, q_total, FUN = "/")

# Leontief inverse using A_total (includes imported intermediate inputs)
diag_mat <- diag(nIndustries)
L <- solve(diag_mat - A_total)

##########################################
### Read IO rows/columns (domestic + imports where relevant)
##########################################

# Total final expenditure by industry: sum domestic + imports
t_f_e_dom <- as.numeric(unlist(IO_EU_domestic[2:(nIndustries + 1), nIndustries + 3]))
t_f_e_imp <- as.numeric(unlist(IO_EU_imports[2:(nIndustries + 1), nIndustries + 3]))
t_f_e <- t_f_e_dom + t_f_e_imp

# Taxes less subsidies on products as part of final expenditure (row n+8, col n+3)
expenditure_tax_dom <- as.numeric(unlist(IO_EU_domestic[nIndustries + 8, nIndustries + 3]))
expenditure_tax_imp <- as.numeric(unlist(IO_EU_imports[nIndustries + 8, nIndustries + 3]))
expenditure_tax <- expenditure_tax_dom + expenditure_tax_imp

# TU & TFU - Total use & total final use
total_use_dom <- as.matrix(as.numeric(unlist(IO_EU_domestic[2:(nIndustries + 1), nIndustries + 17])))
total_use_imp <- as.matrix(as.numeric(unlist(IO_EU_imports[2:(nIndustries + 1), nIndustries + 17])))
total_use <- total_use_dom + total_use_imp
FINAL <- matrix(rep(total_use, times = nYears), nrow = nYears, byrow = TRUE)

final_use_dom <- as.numeric(unlist(IO_EU_domestic[2:(nIndustries + 1), nIndustries + 18]))
final_use_imp <- as.numeric(unlist(IO_EU_imports[2:(nIndustries + 1), nIndustries + 18]))
final_use <- final_use_dom + final_use_imp
f_i <- matrix(rep(final_use, times = nYears), nrow = nYears, byrow = TRUE)

# Production and sales
# GVA: typically domestic only
gva_dom <- t(as.matrix(as.numeric(unlist(IO_EU_domestic[nIndustries + 2, 3:(nIndustries + 2)]))))
gva_i <- gva_dom
GVA_i <- matrix(rep(gva_i, times = nYears), nrow = nYears, byrow = TRUE)
GVA <- matrix(rep(sum(gva_i), times = nYears), nrow = nYears, byrow = TRUE)

# Total supply (domestic + imports)
q_s_dom <- as.matrix(as.numeric(unlist(IO_EU_domestic[nIndustries + 19, 3:(nIndustries + 2)])))
q_s_imp <- as.matrix(as.numeric(unlist(IO_EU_imports[nIndustries + 19, 3:(nIndustries + 2)])))
q_s <- as.numeric(q_s_dom + q_s_imp)
q_s[q_s == 0] <- 1e-6
Q_s_i <- matrix(rep(q_s, times = nYears), nrow = nYears, byrow = TRUE)

# Intermediate consumption (domestic + imports)
i_d_dom <- as.numeric(unlist(IO_EU_domestic[nIndustries + 11, 3:(nIndustries + 2)]))
i_d_imp <- as.numeric(unlist(IO_EU_imports[nIndustries + 11, 3:(nIndustries + 2)]))
i_d <- i_d_dom + i_d_imp
I_D_i <- matrix(rep(i_d, times = nYears), nrow = nYears, byrow = TRUE)

##########################################
### Final Expenditure shares
##########################################

beta_bar <- as.matrix(
  (as.numeric(unlist(IO_EU_domestic[2:(nIndustries + 1), nIndustries + 4])) +
     as.numeric(unlist(IO_EU_domestic[2:(nIndustries + 1), nIndustries + 5])) +
     as.numeric(unlist(IO_EU_domestic[2:(nIndustries + 1), nIndustries + 6]))) /
    (as.numeric(unlist(IO_EU_domestic[1, nIndustries + 4])) +
       as.numeric(unlist(IO_EU_domestic[1, nIndustries + 5])) +
       as.numeric(unlist(IO_EU_domestic[1, nIndustries + 6])))
)

beta_c_bar <- as.matrix(
  (as.numeric(unlist(IO_EU_domestic[2:(nIndustries + 1), nIndustries + 5])) +
     as.numeric(unlist(IO_EU_domestic[2:(nIndustries + 1), nIndustries + 6]))) /
    (as.numeric(unlist(IO_EU_domestic[1, nIndustries + 5])) +
       as.numeric(unlist(IO_EU_domestic[1, nIndustries + 6])))
)

beta_C <- matrix(rep(beta_c_bar, times = nYears), nrow = nYears, byrow = TRUE)

beta_g_bar <- as.matrix(
  as.numeric(unlist(IO_EU_domestic[2:(nIndustries + 1), nIndustries + 4])) /
    as.numeric(unlist(IO_EU_domestic[1, nIndustries + 4]))
)

beta_G <- matrix(rep(beta_g_bar, times = nYears), nrow = nYears, byrow = TRUE)

# Government expenditure
g <- as.numeric(national_accounts[4, 2])
g_i <- as.numeric(unlist(IO_EU_domestic[2:(nIndustries + 1), nIndustries + 4]))

g_cons_tax_dom <- as.numeric(unlist(IO_EU_domestic[nIndustries + 8, nIndustries + 4]))
g_cons_tax_imp <- as.numeric(unlist(IO_EU_imports[nIndustries + 8, nIndustries + 4]))
g_cons_tax <- g_cons_tax_dom + g_cons_tax_imp
g_cons_tax_i <- beta_g_bar * g_cons_tax

G <- matrix(data = sum(g_i), ncol = nYears)
G_tax <- matrix(data = sum(g_cons_tax), ncol = nYears)
G_i <- matrix(rep(g_i, times = nYears), nrow = nYears, byrow = TRUE)
G_i_tax <- matrix(rep(g_cons_tax_i, times = nYears), nrow = nYears, byrow = TRUE)

# Household consumption
cons <- as.numeric(national_accounts[7, 2])
hh_cons_dom <- as.matrix(as.numeric(unlist(IO_EU_domestic[2:(nIndustries + 1), nIndustries + 5])))
npish_cons_dom <- as.matrix(as.numeric(unlist(IO_EU_domestic[2:(nIndustries + 1), nIndustries + 6])))

hh_cons_imp <- as.matrix(as.numeric(unlist(IO_EU_imports[2:(nIndustries + 1), nIndustries + 5])))
npish_cons_imp <- as.matrix(as.numeric(unlist(IO_EU_imports[2:(nIndustries + 1), nIndustries + 6])))

hh_cons <- hh_cons_dom + hh_cons_imp
npish_cons <- npish_cons_dom + npish_cons_imp
consumption <- hh_cons + npish_cons

consumption_tax_dom <- as.numeric(unlist(IO_EU_domestic[nIndustries + 8, nIndustries + 5]))
consumption_tax_imp <- as.numeric(unlist(IO_EU_imports[nIndustries + 8, nIndustries + 5]))
consumption_tax <- consumption_tax_dom + consumption_tax_imp
consumption_tax_i <- beta_c_bar * consumption_tax

C <- matrix(data = sum(consumption), ncol = nYears)
C_tax <- matrix(data = sum(consumption_tax), ncol = nYears)
C_i <- matrix(rep(consumption, times = nYears), nrow = nYears, byrow = TRUE)
C_i_tax <- matrix(rep(consumption_tax_i, times = nYears), nrow = nYears, byrow = TRUE)

##########################################
### Consistency checks
##########################################

A <- A_total
diag <- diag(ncol = ncol(A), nrow = nrow(A))
L <- solve(diag - A)

# Check for total final use
TotalFinalUse <- total_use - (A %*% as.numeric(x <- as.matrix(as.numeric(unlist(IO_EU_domestic[2:(nIndustries + 1), nIndustries + 17])))))
TotalFinalUseCheck <- (diag - A) %*% as.numeric(x)
if (!isTRUE(all.equal(TotalFinalUse, TotalFinalUseCheck, final_use, tolerance = 1e-3))) {
  warning("Total final use checks failed — check row/column offsets and A construction")
}

# Check for total use
TotalUse <- (A %*% q_s) + final_use
TotalUseCheck <- L %*% final_use
if (!isTRUE(all.equal(TotalUse, TotalUseCheck, total_use, tolerance = 1e-3))) {
  warning("Total use checks failed — check row/column offsets and A construction")
}

##########################################
### Initialize time series matrices
##########################################

IM_init <- as.numeric(unlist(IO_EU_imports[2:(nIndustries + 1), nIndustries + 17]))
IM_init[is.na(IM_init)] <- 0
IM_i <- matrix(rep(IM_init, times = nYears), nrow = nYears, byrow = TRUE)

EX_init_dom <- as.numeric(unlist(IO_EU_domestic[2:(nIndustries + 1), nIndustries + 18]))
EX_init_imp <- as.numeric(unlist(IO_EU_imports[2:(nIndustries + 1), nIndustries + 18]))
EX_init <- EX_init_dom + EX_init_imp
EX_init[is.na(EX_init)] <- 0
EX_i <- matrix(rep(EX_init, times = nYears), nrow = nYears, byrow = TRUE)

GCF_init <- rep(0, nIndustries)
GCF_i <- matrix(rep(GCF_init, times = nYears), nrow = nYears, byrow = TRUE)

RoW <- matrix(0, nrow = nYears, ncol = nIndustries)

# placeholders
expenditure_tax_i <- rep(expenditure_tax / nIndustries, nIndustries)
D_i <- matrix(rep(t_f_e, times = nYears), nrow = nYears, byrow = TRUE)
D_tax_i <- matrix(rep(expenditure_tax_i, times = nYears), nrow = nYears, byrow = TRUE)

##########################################
### Time loop (skeleton)
##########################################

for (i in 2:nYears) {
  cat("Starting iteration:", i, "\n")

  for (iterations in 1:100) {

    # Total final expenditure by industry
    D_i[i, ] <- beta_C[i, ] * C[i] + beta_G[i, ] * G[i]

    # Total final use
    f_i[i, ] <- D_i[i, ] + GCF_i[i, ] + EX_i[i, ]

    # Exports (simple persistence)
    EX_i[i, ] <- EX_i[i - 1, ]

    # Total use by industry (Leontief production function) using A_total/L
    X_i[i, ] <- (L %*% f_i[i, ])

    # Rest of the world balance
    RoW[i, ] <- EX_i[i, ] - IM_i[i, ]

    # (other equations to be filled in: wages, employment, energy, emissions, land use...)

  }
}

# End of adapted file
