library(ggplot2)

# ==========================================================
# 2030 AGGREGATE RESULTS
# ==========================================================

results_2030 <- data.frame(
  scenario = factor(
    c("REF", "S1", "S2"),
    levels = c("REF", "S1", "S2")
  ),

  output = c(
    sum(REF_endpoint_2030$X),
    sum(S1_endpoint_2030$X),
    sum(S2_endpoint_2030$X)
  ),

  GVA = c(
    sum(REF_endpoint_2030$GVA),
    sum(S1_endpoint_2030$GVA),
    sum(S2_endpoint_2030$GVA)
  )
)

results_2030
