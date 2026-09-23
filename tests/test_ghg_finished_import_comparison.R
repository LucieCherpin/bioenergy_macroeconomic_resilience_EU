# Focused contract checks for the finished-product import addendum.

stop_if_not <- function(ok, msg) {
  if (!isTRUE(ok)) stop(msg, call. = FALSE)
}

stop_if_not(file.exists("GHG_Finished_Import_Comparison.R"),
            "Finished-import comparison script is missing.")
parse("GHG_Finished_Import_Comparison.R")
source("GHG_Finished_Import_Comparison.R", local = TRUE)

manifest <- category_manifest()
stop_if_not(nrow(manifest) == 9L,
            "The import manifest must contain the nine workbook categories.")
stop_if_not(!anyDuplicated(manifest$import_category),
            "Import manifest categories must be unique.")

volumes <- read_import_volumes(IMPORT_WORKBOOK)
stop_if_not(nrow(volumes) == 81L,
            "Workbook import-volume block must contain 9 categories x 9 retained periods.")
stop_if_not(nrow(read_workbook_ei(IMPORT_WORKBOOK)) == 9L,
            "Workbook EI block must contain nine categories.")

comparison <- build_comparison()
stop_if_not(nrow(comparison) == 81L,
            "Comparison must contain 9 categories x 3 years x 3 scenarios.")
stop_if_not(!anyDuplicated(comparison[, c("year", "scenario", "import_category")]),
            "Comparison keys must be unique.")

sample <- comparison[comparison$year == 2030 & comparison$scenario == "S1", ]
stop_if_not(nrow(sample) == 9L, "2030/S1 must contain all nine categories.")
stop_if_not(isTRUE(all.equal(
  sample$workbook_gCO2e_per_MJ[
    sample$import_category == "Advanced biodiesel (FAME / HVO)"], 13.178)),
  "Workbook normalized biodiesel value does not match the workbook EI.")
stop_if_not(all(sample$workbook_MtCO2e[sample$imported_Mtoe == 0] == 0),
            "Zero-volume workbook absolute emissions must be zero.")
stop_if_not(all(is.na(sample$exio_gCO2e_per_MJ[sample$imported_Mtoe == 0])),
            "Zero-volume EXIOBASE normalized values must be unavailable.")
stop_if_not(any(sample$exio_mapping_status == "sector_match") &&
              any(sample$exio_mapping_status == "product_proxy"),
            "The comparison must distinguish sector matches from proxies.")
stop_if_not(any(grepl("RFNBO aggregate value allocated", 
                      sample$import_value_allocation)),
            "RFNBO monetary allocation must be explicitly flagged.")

cat("Finished-import comparison contract tests passed.\n")
