https://zenodo.org/records/23428262

# Data and code for "Effects of habitat complexity on short-term food-web dynamics after piscivore stocking in shallow lake mesocosms"

Authors: Tian Qian, Xiaoyu Ning, Fei Diao, Erik Jeppesen, Ruijie Shen, Hu He, Kuanyi Li

This deposit contains the raw mesocosm data and the R code used to produce all figures and
tables in the manuscript. Every table and figure in the paper can be regenerated from
`data/nutrient.csv` by running the corresponding script.

## Experimental design

Twelve outdoor mesocosms were assigned to a 2 x 2 factorial design crossing artificial
plant density (low, high) with the presence of mandarin fish (*Siniperca chuata*), a
piscivore. Three mesocosms served as replicates in each of the four combinations, giving
12 experimental units. Mandarin fish were stocked at day 18. Mesocosms were sampled eight
times, on days 0, 3, 6, 9, 12, 15, 18 and 21, giving 96 rows in the data file.

`ID` is the permanent mesocosm identifier. The `treat` code is built from the two
treatment factors, where the first letter gives plant density (LP = low, HP = high) and the
second letter gives piscivore state (MA = mandarin absent, MP = mandarin present). The
`treat`, `Plant` and `Mandarin` columns are mutually consistent in all 96 rows.

## File inventory

| File | Description |
| --- | --- |
| `README.md` | This file. File-level and column-level metadata for the whole deposit. |
| `data/nutrient.csv` | The only data file. Raw and derived mesocosm measurements, 96 rows x 38 columns, one row per mesocosm per sampling day. Column definitions are given in the next section. |
| `Fig1.R` | R code for Fig. 1. Plots crucian carp biomass and abundance at day 21 by plant density and piscivore presence. Writes `figures/carp_biomass.tiff`, `figures/carp_number.tiff` and the combined `figures/carp.tiff`. |
| `Fig2.R` | R code for Fig. 2. Plots Daphnia, cladocerans, copepods, rotifers, total zooplankton and phytoplankton at day 21. Writes the six component files and the combined `figures/zooplankton_Day21.tiff`. |
| `Fig3.R` | R code for Fig. 3. Time series of chlorophyll *a*, TN, DTN, TP and DTP across the eight sampling days. Writes the five component files and the combined `figures/nutrient_conc.tiff`. |
| `Fig4.R` | R code for Fig. 4. Zooplankton body mass, zooplankton to phytoplankton ratio, and the chlorophyll *a* to TN and to TP ratios. Writes `figures/Zoopsizezppp.tiff`, `figures/ChlTNTP.tiff` and the combined `figures/ratio.tiff`. |
| `FigS2.R` | R code for Fig. S2. Same quantities as Fig. 2 but for day 0, giving the pre-stocking baseline. Writes the six component files and `figures/zooplankton_Day0.tiff`. |
| `FigS3.R` | R code for Fig. S3. Relative composition of the seven phytoplankton groups at day 21. Writes `figures/phyt_community.tiff`. |
| `Table1.R` | R code for Table 1. Fits the GLMMs for the day 21 biological response variables. Prints the coefficient tables to the console. |
| `Table2.R` | R code for Table 2. Fits the GLMMs for the physicochemical response variables across all sampling days. Prints the coefficient tables to the console. |
| `figures/README.txt` | Placeholder note. The `figures/` folder is the output directory for all scripts and is kept in the deposit so the scripts run without editing. |

## How to run the code

All scripts begin with `setwd()` and expect to be run from the deposit root, so that the
relative paths `./data/nutrient.csv` and `./figures/` resolve correctly. Each script reads
only `data/nutrient.csv`; no other input is needed and no network access is required.

Required R packages: `ggplot2`, `dplyr`, `tidyr`, `tidyverse`, `Rmisc`, `ggpubr`,
`patchwork`, `data.table` and `glmmTMB`. `glmmTMB` is needed only for `Table1.R` and
`Table2.R`.

## Column-level metadata for `data/nutrient.csv`

The file is comma-delimited UTF-8 with a header row, no quoting and no index column.
Missing values are recorded as empty fields and are read as `NA`. Only the physicochemical
variables were measured on all eight sampling days. Biological and derived variables were
measured on a subset of days, as noted in the table. Ranges are the observed minima and
maxima across the whole file and are given only as a reading aid.

### Design and identifier columns

| Column | Type | Unit / values | Description |
| --- | --- | --- | --- |
| `Days` | numeric, integer | day, 0 to 21 | Experimental day on which the sample was taken. Eight levels: 0, 3, 6, 9, 12, 15, 18, 21. Day 0 is before fish stocking and day 21 is the final day. |
| `ID` | numeric, integer | 1 to 12 | Mesocosm identifier. Unique across the experiment and constant across sampling days. |
| `treat` | categorical | LPMA, LPMP, HPMA, HPMP | Treatment combination. LP = low plant density, HP = high plant density, MA = mandarin fish absent, MP = mandarin fish present. Each level has three replicate mesocosms. |
| `Plant` | categorical | Low, High | Artificial plant density treatment. |
| `Mandarin` | categorical | Absent, Present | Mandarin fish stocking treatment. `Absent` mesocosms received no piscivore. |

### Physicochemical variables (measured on all 8 sampling days, n = 96)

| Column | Type | Unit | Range | Description |
| --- | --- | --- | --- | --- |

| `TN` | numeric | mg/L | 0.46 to 0.96 | Total nitrogen concentration. |
| `DTN` | numeric | mg/L | 0.39 to 0.65 | Dissolved total nitrogen concentration. |
| `TP` | numeric | mg/L | 0.0058 to 0.0186 | Total phosphorus concentration. Multiply by 1000 for µg/L. |
| `DTP` | numeric | mg/L | 0.0028 to 0.0124 | Dissolved total phosphorus concentration. Multiply by 1000 for µg/L. |
| `Chl` | numeric | µg/L | 3.5 to 17.2 | Chlorophyll *a* concentration. |

### Fish variables

| Column | Type | Unit | Description |
| --- | --- | --- | --- | --- |
| `Carpbiomass` | numeric | g/m² | Crucian carp biomass. `0` means no crucian carp were present in the mesocosm, which is an observed value rather than a missing value. |
| `Mandarinbiomass` | numeric | g/m² | Mandarin fish biomass. `0` in `Absent` mesocosms and a positive value in `Present` mesocosms. Recorded on day 0 as the pre-stocking baseline and on day 21 as the final biomass. |
| `Carpdensity` | numeric | ind./m² | Crucian carp density. 

### Zooplankton variables

Zooplankton biomass is expressed in µg/L dry weight. `Daphnia` is a subset of `Cladoceran`, so
`Zooplanktonbiomass` is the sum of `Cladoceran`, `Copepod` and `Rotifer` and does not include `Daphnia`
a second time. This relationship holds exactly in all rows where the data are present.

| Column | Type | Unit | Days present | Description |
| --- | --- | --- | --- | --- |
| `Daphniabiomass` | numeric | µg/L | 0, 21 | Biomass of the genus *Daphnia*, a subset of the cladocerans. |
| `Cladoceranbiomass` | numeric | µg/L | 0, 21 | Biomass of all Cladocerans. |
| `Copepodbiomass` | numeric | µg/L | 0, 21 | Biomass of Copepoda. |
| `Rotiferbiomass` | numeric | µg/L | 0, 21 | Biomass of Rotifera. |
| `Zooplanktonbiomass` | numeric | µg/L | 0, 21 | Total zooplankton biomass. Derived, equal to `Cladoceran + Copepod + Rotifer`. |
| `Zoopbodymass` | numeric | µg/ind. | 21 | Mean zooplankton body mass. Higher values indicate larger zooplankton. |

### Phytoplankton variables

| Column | Type | Unit | Days present | Description |
| --- | --- | --- | --- | --- |
| `Phytoplanktonbiomass` | numeric | µg/L | 0, 21 | Total phytoplankton biomass. |
| `Cyanophyta` | numeric | proportion, 0 to 1 | 21 | Relative contribution of Cyanophyta to total phytoplankton biomass. |
| `Bacillariophyta` | numeric | proportion, 0 to 1 | 21 | Relative contribution of Bacillariophyta (diatoms). |
| `Pyrrophyta` | numeric | proportion, 0 to 1 | 21 | Relative contribution of Pyrrophyta (dinoflagellates). |
| `Chrysophyta` | numeric | proportion, 0 to 1 | 21 | Relative contribution of Chrysophyta. |
| `Cryptophyta` | numeric | proportion, 0 to 1 | 21 | Relative contribution of Cryptophyta. |
| `Euglenophyta` | numeric | proportion, 0 to 1 | 21 | Relative contribution of Euglenophyta. |
| `Chlorophyta` | numeric | proportion, 0 to 1 | 21 | Relative contribution of Chlorophyta. |

Note on units. The seven phytoplankton group columns are relative proportions, not
concentrations, and each row sums to exactly 1. They are grouped in
`FigS3.R` with `position = "fill"` and are plotted as biomass percentage. The absolute
total is given by `Phytoplankton`. Both are reported in µg/L in the raw file, so the two
must not be summed.

### Derived variables

| Column | Type | Unit | Days present | Description |
| --- | --- | --- | --- | --- |
| `ChlTN` | numeric | ratio x 1000 | all 8 | Chlorophyll *a* to total nitrogen ratio, stored as `Chl / TN / 1000`. Multiply by 1000 for the dimensionless ratio. |
| `ChlTP` | numeric | ratio | all 8 | Chlorophyll *a* to total phosphorus ratio, stored as `Chl / TP`. |
| `zppp` | numeric | ratio | 21 | Zooplankton to phytoplankton ratio, equal to `Total / Phytoplankton`. |
| `Cla_proportion` | numeric | proportion, 0 to 1 | 21 | Proportion of total zooplankton biomass contributed by cladocerans, `Cladoceranbiomass / Zooplanktonbiomass`. |
| `Cop_proportion` | numeric | proportion, 0 to 1 | 21 | Proportion of total zooplankton biomass contributed by copepods, `Copepodbiomass / Zooplanktonbiomass`. |
| `Rot_proportion` | numeric | proportion, 0 to 1 | 21 | Proportion of total zooplankton biomass contributed by rotifers, `Rotiferbiomass / Zooplanktonbiomass`. |

The three proportions `Cla_proportion`, `Cop_proportion` and `Rot_proportion` sum to 1 in every row where they are present.
`zppp` reproduces exactly from `Total / Phytoplankton`. `ChlTN` reproduces exactly from
`Chl / TN / 1000`.

## Missing value conventions

An empty field means the variable was not measured on that occasion. A value of `0` is a
real observation and always means true absence or a measured zero. The two cases are
therefore distinct and must not be conflated. For example, `Mandarinbiomass` is `0` in
every `Absent` mesocosm because no fish were stocked, whereas `Daphnia` is empty on days 3
to 18 because zooplankton was not sampled on those dates.

## Known data issues

Three points require attention before the deposit is published. They are documented here
rather than silently corrected, because each one changes values that appear in the figures.

1. `Carpdensity` does not hold integer counts. The values are multiples of 1/9 and range
   from 0 to 4.44444, so they cannot be individual counts. In the `LPMA` and `HPMA`
   mesocosms the value is a constant 4.44444, and in `LPMP` it takes the values 0, 0.22222
   and 0.88889. `Fig1.R` labels this column "Crucian carp density (ind./m²)". The column
   appears to be an index or a rescaled quantity. Either the true count needs to be
   supplied or the variable needs to be relabelled.

2. `Fig1.R` and `FigS2.R` filter the sampling day down to a single value before plotting.
   The filters in `Fig1.R` retain only day 21, and those in `FigS2.R` select day 0. The
   plots are therefore cross-sectional comparisons rather than time series, which is
   consistent with the axis labels in the manuscript but is easy to misread from the code.

## Derived data and external sources

`data/nutrient.csv` contains original measurements from this experiment. The derived
columns listed above were calculated from the measured columns within the same file, and
the formulas are given in the metadata table. No data in this deposit were downloaded from
an external source, and no external dataset was clipped or otherwise merged in. The
phylogenetic groups are standard algal taxonomic divisions and required no external
reference table.
