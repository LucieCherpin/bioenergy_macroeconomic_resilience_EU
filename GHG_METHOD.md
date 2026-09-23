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

Physical feedstock GHG is also reported by domestic and imported origin. This is
an allocation using the model's positive purchased-feedstock monetary
coefficients in `dist_feed`, not an observation of physical tonnes by origin.
For an IVC with positive feedstock coefficients `d_k`, the allocated share is
`d_k / sum(d_k)` within the domestic or `_imp` channel. Negative gate-fee or
revenue entries are excluded because they do not enter the purchased-input
technical coefficient. IO-fallback feedstock GHG instead retains its exact
domestic and direct-import environmental-extension channels. Both splits are
required to reconstruct their unsplit parent totals.

The workbook source case is selected from its explicit scenario-sheet links,
not by choosing whichever physical reconstruction is closest to a model monetary
total. Those links select F for S1 and I for S2/S3 in 2030 and 2035, then K for
S1 and M for S2/S3 in 2040. Pure recursive intermediate routes IVC6, IVC8b and
IVC12 receive no second primary-feedstock reconstruction at the consuming stage;
their domestic upstream burden enters through the BIO-to-BIO recursion below.

For IVC11a_SAF, workbook row 78 lists FPBO at EUR/t feedstock and a conversion
yield in t SAF/t feedstock. The workbook now uses the dimensionally consistent
price divided by yield identity: `300 / 0.157085 = 1,909.783 EUR/t SAF` in the
base source case. `Final_main_code.R` stores the resulting F/I/K/M feed costs and
cost shares directly in the seven scenario endpoints that use IVC11a_SAF; there
is no post-hoc runtime overlay. Source CAPEX remains 1,445 EUR/t SAF, source OPEX
remains 716.25 EUR/t SAF, and the existing equal FPBO proxy split between
advanced biodiesel and advanced biogasoline is unchanged. At fixed biofuel
output, the corrected recurrent FPBO coefficient raises intermediate demand and
reduces residual value added in the affected fuel column relative to the former
understated feed budget. It also raises recursively embodied upstream GHG. The
correction does not alter physical yields, GHG factors, lifecycle boundaries or
scenario fuel volumes.

IVC8b uses one biomethane intermediate and is already consistent with the
workbook identity. Its stored feed component is `936 * 0.459319 = 429.922584
EUR/t methanol`, while the source price/yield calculation is `900 / 2.0934 =
429.922614 EUR/t methanol`; the difference is only decimal rounding. All nine
benchmark endpoints preserve that identity. No alternative 708 EUR/t allocation
is used in the model, so no unresolved IVC8b sensitivity or separate diagnostic
enters the baseline GHG accounting.

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

## 7. Workbook, geographic-channel and external-comparator figures

`GHG_Workbook_Comparison.R` is read-only post-processing of the saved benchmark
artifacts. It does not rerun or modify the economic model, the GHG production
analysis, or `Providing sectors.xlsx`. Numerical comparison CSVs are written
before the PNG/PDF figures. `--inspect` exports nonempty-cell censuses for the
workbook's `Weighetd emission intensities` and `Emission intensities` sheets and
the verified source-cell manifest used by the renderer.

Workbook intensities are mapped at model-fuel, IVC and, where the workbook has
more than one row for an IVC, feedstock level after checking the source row's
IVC and product labels. These are fixed pathway values, not statistical ranges.
For route `r`, the scenario-specific workbook intensity is
`I_r = sum_f m_rf I_rf`, where `m_rf` is the explicit workbook feedstock-mix
share used by the model. The fuel value is then
`I_fuel = sum_r Q_r I_r / sum_r Q_r`, where `Q_r = tonnes_r * LHV_r`.
Accordingly, workbook results are plotted as bars without error bars. A value is
left unavailable if any positive-energy modeled route lacks a verified mapping.
Negative biomethane values retain their avoided-emission credit. Workbook
capital-goods coverage remains `unknown`; the model/workbook figure displays the
model's recursively embodied full result but labels this boundary mismatch and
does not claim a like-for-like validation.

The model component figures are an additive stage decomposition. Physical
feedstock GHG is split by the positive monetary sourcing shares described
above. IO-fallback feedstock, OPEX and CAPEX retain separate domestic-chain and
direct-import channels. Imported IO remains a lower-bound direct-extension
result because no foreign Leontief system is available. These components must
not be interpreted as a territorial inventory, as observed physical origin, or
as a geographic split of the recursive lifecycle footprint.

The external comparison uses recursive hybrid GHG excluding CAPEX, which still
includes feedstock and operating supply chains. JEC/RED/CORSIA pathway ranges
are shown only when every positive-energy scenario route is mapped; partial or
zero coverage is unavailable rather than a zero or partial-total bar. Proxy,
range and coverage fractions remain in the output table. Avoided-manure-storage
credits are displayed as separate counterfactual alternatives rather than
merged into the ordinary pathway range. No figure applies a pass/fail range
judgement.

Each family is exported as separate `_total` and `_normalized` PNG/PDF files.
The workbook-comparison and model-only figures follow the same visual grammar:
S1/S2/S3 columns, 2030/2035/2040 on the horizontal axis, and a stable colour per
fuel. The model-only bars use the stage-attributed full footprint because that
quantity is additive across fuel sectors. The recursive comparison bars are
per-fuel lifecycle diagnostics and remain nonadditive across interdependent
fuel sectors.

Workbook and external gross-output lifecycle values, and the model's recursive
per-fuel totals, are diagnostics for one unit or gross output of a final fuel.
They must not be summed across interdependent biofuel sectors. Only the existing
stage-attributed hybrid accounting remains additive across the nine BIO sectors.

## 8. Finished-product import emissions addendum

`GHG_Finished_Import_Comparison.R` is a read-only addendum for finished biofuel
imports. It reads `Imports_Exports_All_Scenarios.xlsx` and writes
`ghg_finished_import_workbook.csv`,
`ghg_finished_import_exiobase.csv`, `ghg_finished_import_comparison.csv` and
the auditable `ghg_finished_import_mapping.csv` under `ghg_outputs/`. These
files do not replace or alter the domestic production accounting in
`GHG_Analysis.R`; they make the two available finished-import constructions
explicit so that they can be tabulated by year, scenario and fuel.

The workbook construction reproduces the `Import Emissions` sheet of
`Imports_Exports_All_Scenarios.xlsx`:

`E_import [Mt CO2e] = M_import [Mtoe] x EI [g CO2e/MJ] x 41,868 [MJ/toe] / 10^6`.

The EI values are the cached workbook inputs, including the workbook's route
averages and avoided-emission credits. They are not recomputed from the
domestic model and the circular source formulas described in the workbook are
therefore provenance information, not an additional uncertainty interval.

The EXIOBASE construction uses the imported finished-use endpoint `Y_imp_FCE`
from the saved model results (MEUR) and the imported environmental-extension
boundary `external_imports_direct`. For a mapped sector `s`,

`E_import [Mt CO2e] = Y_imp_FCE [MEUR] x e_s [kg CO2e/MEUR] / 10^9`,

where `e_s` is the sum of the extension stressors after the existing AR6
100-year characterization factors. The normalized value is then
`E_import x 10^9 x 1000 / (M_import x 41,868,000,000)` in g CO2e/MJ. A zero
import quantity has no defined normalized EXIOBASE value and is recorded as
unavailable rather than as zero. This is a direct imported-sector extension
estimate, not a foreign Leontief lifecycle: it excludes the unobserved foreign
upstream supply chain.

The mapping manifest is deliberately explicit. Biodiesel, biogasoline and
biogas use their corresponding CPA sectors, while kerosene, HFO and RFNBO
categories use labelled product proxies where the extension has no exact
product row. Conventional and advanced products are pooled where the CPA
sector itself does not distinguish them. The model stores RFNBO finished-import
value as one aggregate endpoint; the script allocates that value between
e-methanol and e-methane in proportion to their workbook imported Mtoe and
flags the allocation in every output row. That allocation is a diagnostic, not
a claim about foreign route composition.

Consequently, the two normalized columns answer different questions. The
workbook column is a cached pathway life-cycle intensity, while the EXIOBASE
column is a direct imported-use extension intensity at a mapped sector or
explicit proxy. Differences are expected from route composition, boundary,
capital treatment, geographic scope and the absence of foreign recursive
accounting. They must not be interpreted as a pass/fail test, and neither set
of finished-import values should be added to the domestic recursive footprint
without first resolving overlap in the accounting boundary.
