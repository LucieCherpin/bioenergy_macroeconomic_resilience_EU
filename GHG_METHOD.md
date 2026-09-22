# Hybrid GHG method and external validation

`GHG_Analysis.R` is post-processing. It does not change the economic solver. The benchmark-year analysis is run for 2030, 2035 and 2040 after `Final_main_code.R` has written `model_results_CAPEX_separate.rds`.

## 1. Environmental-extension boundary

The domestic environmental-extension CSV legitimately contains both `scope_production` and `scope_final_demand_direct`. The GHG analysis uses **only** `scope_production` to build sector production intensities. Direct final-demand rows are a different accounting boundary and are filtered out rather than treated as an input error. The import extension uses `external_imports_direct`.

## 2. Stage-attributed hybrid footprint

For a model biofuel stage `j`, the existing hybrid accounting is preserved:

```text
E_stage,j = E_feedstock,physical,j
          + E_feedstock,IO-fallback,j
          + E_OPEX,j
          + E_CAPEX,j .
```

Primary-feedstock identity and physical quantity are reconstructed at IVC level from `Providing sectors.xlsx`; no aggregate agriculture/forestry IO row is reverse-disaggregated after solving. OPEX and CAPEX use the model's saved domestic/import channel matrices and the NONBIO domestic Leontief system. Imported emissions remain a lower bound because the repository has direct external-import extensions but no foreign Leontief system.

The workbook source case is selected from its explicit scenario-sheet links,
not by choosing whichever physical reconstruction is closest to a model monetary
total. Those links select F for S1 and I for S2/S3 in 2030 and 2035, then K for
S1 and M for S2/S3 in 2040. Pure recursive intermediate routes IVC6, IVC8b and
IVC12 receive no second primary-feedstock reconstruction at the consuming stage;
their domestic upstream burden enters through the BIO-to-BIO recursion below.

For IVC11a_SAF, workbook row 78 lists FPBO at EUR/t feedstock and a conversion
yield in t SAF/t feedstock, but its cached monetary formula uses price times
yield. Dimensional consistency and the neighbouring rows require price divided
by yield. The binary workbook remains unchanged as provenance, while
`source_data_corrections.csv` records the exact source formula correction.
`Final_main_code.R` uses the corrected F/I/K/M feed costs and distributions
before constructing scenario endpoints. Source CAPEX remains 1,445 EUR/t SAF,
source OPEX remains 716.25 EUR/t SAF, and the existing equal FPBO proxy split
between advanced biodiesel and advanced biogasoline is unchanged. This raises
recurrent FPBO intermediate use at fixed scenario
biofuel output. In the economic accounts it raises intermediate consumption and
reduces residual value added in the affected fuel column; the model keeps BIO
output exogenous. In the lifecycle diagnostic it raises the recursively embodied
upstream burden. It does not alter physical yields, GHG factors, lifecycle
boundaries or scenario fuel volumes.

IVC8b is not changed by this correction. Its workbook contains an explicit
allocation uncertainty: some cases assign approximately 708 EUR/t methanol to
the biomethane feed component, while the physical price/yield calculation gives
approximately 429.923 EUR/t. `ivc8b_allocation_diagnostic.csv` reports every
year/scenario route weight, both coefficients, their monetary residual, and the
effect on the aggregate advanced-bio-HFO biomethane coefficient. This is a
read-only sensitivity record, not a corrected baseline or a calibration target.

The legacy columns in `ghg_hybrid_benchmark.csv` remain stage-attributed for backward compatibility. A second stage metric excludes CAPEX because JEC/CORSIA/RED fuel-cycle comparators generally do not share the repo's explicit capital-goods boundary.

## 3. Domestic bioenergy-intermediate recursion

A fuel can consume another model biofuel (for example biomethane, FPBO-derived carriers, or other endogenous bioenergy intermediates). A per-fuel external lifecycle comparison must therefore carry upstream domestic BIO-stage burdens through those links rather than assigning zero burden to the intermediate.

Let `A_BB` be the domestic technical-coefficient submatrix whose rows and columns are model BIO sectors, and let `c` be the vector of stage-attributed GHG intensities in kg CO2e per MEUR of each BIO output. With the model convention `A[i,j] = input i per unit output j`, the recursively embodied intensity row vector is

```text
t' = c' (I - A_BB)^(-1) .
```

The script reports both full (`feedstock + OPEX + CAPEX`) and no-CAPEX recursive intensities. It fails rather than silently inventing a foreign recursive closure if imported BIO-to-BIO intermediate coefficients are non-zero. Recursive pathway intensities are **diagnostics for one unit of a final fuel**; they must not be summed across all biofuel outputs, because doing so would double-count intermediate biofuel production.

## 4. Energy normalisation

External fuel-cycle studies are commonly reported in g CO2e/MJ rather than kg CO2e/MEUR. For each IVC contribution, fuel mass is derived using the same scenario market value and IVC market price already used by the model's physical-feedstock reconstruction. Energy is

```text
MJ_fuel = sum_i tonnes_fuel,i * 1000 kg/t * LHV_i .
```

and the comparison intensity is

```text
gCO2e_per_MJ = kgCO2e * 1000 g/kg / MJ_fuel .
```

`ghg_fuel_energy_factors.csv` contains the lower heating values. Direct matches use RED III replacement Annex III values; non-identical product categories are explicitly marked `proxy` rather than hidden.
The 2035 S1 source places the aviation-tagged UCO/animal-fat hydrotreatment
route in the conventional-biodiesel model pool. Its product-specific entry is
still a direct 44 MJ/kg mapping because RED III assigns that lower heating value
to both hydrotreated diesel and hydrotreated jet hydrocarbons; no route weight
or modeled fuel identity is changed by the denominator lookup.

## 5. External benchmark catalogue

The validation data are versioned repository inputs, not values scraped during a production run:

- `ghg_validation_sources.csv` is the citation/source catalogue.
- `ghg_external_benchmarks.csv` contains pathway/feedstock GHG values and exact locators.
- `ghg_fuel_energy_factors.csv` contains the energy-denominator mapping.

Most pathway values in the 2026 European Commission Annex 4 are a synthesis of JEC WTT v5, RED II and ICAO CORSIA rather than new BEST calculations. The benchmark file therefore records both the extraction source and the underlying source IDs. It also records when a value is only an indicative proxy or depends on avoided-emission credits.

The script intentionally does **not** classify a model result as "passing" or "failing" a literature range. System boundaries, feedstock mixes, allocation rules, avoided-methane credits, electricity mixes, geographic scope and capital treatment differ. `ghg_external_validation_comparison.csv` places comparable quantities side by side and reports whether the cited model IVC is actually present in that scenario; scientific interpretation remains explicit rather than being reduced to a range test.

## 6. Core references

- Prussi, M., Yugo, M., De Prada, L., Padella, M., Edwards, R. & Lonza, L. (2020), *JEC Well-to-Tank report v5*, JRC119036, doi:10.2760/959137.
- Prussi, M., Yugo, M., Padella, M., Edwards, R., Lonza, L. & De Prada, L.; Hamje, H. (ed.) (2020), *JEC Well-to-Tank report v5: Annexes*, JRC119036, doi:10.2760/06704; the annex dataset comprises nine fuel-category Excel workbooks.
- European Union, Directive (EU) 2018/2001 (RED II), consolidated 16 July 2024, especially Annexes V, VI and IX.
- European Union, Directive (EU) 2023/2413 (RED III), replacement Annex III for transport-fuel energy content.
- ICAO (2024), *CORSIA Default Life Cycle Emissions Values for CORSIA Eligible Fuels*, Sixth Edition, October 2024.
- European Commission DG RTD; EXERGIA; Politecnico di Torino; BEST (2026), *Mobilization of industrial capacity building for advanced biofuels - Annex: industrial value chains business models*, doi:10.2777/9062748.
- Matschegg, T., Kumpan, M., Strasser, C. & Dissauer, C. (2026), *Global warming potential of bioenergy conversion pathways in Austria: A comparative life cycle assessment*, *Biomass and Bioenergy*, doi:10.1016/j.biombioe.2026.109750.
- Giuntoli, J. et al. (2019), *Definition of input data to assess GHG default emissions from biofuels in EU legislation*, JRC115952, doi:10.2760/69179.

Full URLs, editions, locators, scope notes and independence/circularity notes are stored in `ghg_validation_sources.csv` and `ghg_external_benchmarks.csv` so generated comparison tables remain auditable.
