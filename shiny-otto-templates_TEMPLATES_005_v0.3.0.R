## ----------------------------------- ##
## Notes ####
## ----------------------------------- ##

# - This template file is read in by the main outlier app program. Its ID (string starting
#   after "shiny-otto-templates_" until end of filename excluding file extension) will be
#   displayed in the app output to identify which templates have been used in the generation of
#   output
# - To change the template file used by the app, create a copy of this file, change the ID
#   part of the filename, and modify the assay templates as needed. IMPORTANT: Keep only
#   the template file to be currently used by the app in the main app directory (where the main
#   app file is located). Move all other template files to another directory, e.g. a subdirectory
#   of the main directory. Templates can then be changed by moving the corresponding files between
#   the main directory and the subdirectory
# - Permissible changes to the assay template objects <dat_assay_templates_col> and
#   <dat_assay_templates_row>:
#   - Changing values of the entries in accordance with the column info given below. E.g., the
#     action limit for a given assay template could be changed from 10 to 20
#   - Addition of new assay templates. Must conform to the column structure described in column
#     info below (best copy an existing assay template and modify the entries as needed). Should
#     not be placed as the last row of the assay template objects (this row is reserved for the
#     "User defined" entry)


## ----------------------------------- ##
## Assay templates (concentration gradient over columns) ####
## ----------------------------------- ##

# column info
#-------------#
# assay                     : Name of assay template
# conc_start                : Start concentration (final concentration in the first well in ng/mL)
# conc_start_col            : Column number (1-12), where start concentration is located
# conc_dilution_factor      : Dilution factor(1->2->3...)
# meas_mult                 : Positive numeric value by which measurements should be multiplied prior
#                             to plotting and fitting (changing this may help resolve model fitting
#                             convergence issues)
# row_A, ..., row_H         : Row identifier (Standard, Probe, etc.)
# rows_cols_empty           : Empty rows and/or columns - inside "..." and separated by comma (,)
# pval_rosner               : p-value for Rosner outlier test (values allowed: 0.1, 0.05, 0.01, 0.001)
# pval_rout                 : same as above, but for ROUT outlier test
# actionlimit               : action limit as percentage of the value range (difference between upper 
#                             and lower asymptote); must be between 0 and 100
# nofit_ids                 : IDs of rows for which no model fit and outlier tests are performed.
#                             Inside "..." and separated by comma (,). Data in all such rows is plotted
#                             separately by data type
# fit_mode                  : Do fitting before/after outlier removal based on means or indiviaul values?
#                             (must be one of the two values "Mean values" or "Individual values" exactly
#                             in this spelling)
# dev_only                  : Include this assay only in the development version? (boolean)

dat_assay_templates_col <- tribble(
  # column names
  ~assay,
  ~conc_start, ~conc_start_col, ~conc_dilution_factor, ~meas_mult,
  ~row_A, ~row_B, ~row_C, ~row_D, ~row_E, ~row_F, ~row_G, ~row_H,
  ~rows_cols_empty, ~pval_rosner, ~pval_rout, ~actionlimit, ~nofit_ids, ~fit_mode,
  ~dev_only,
  #-----|-------------|----#
  "836858-01: ADCC Bioassay (018-QA061954)",
  2000, 1, 2.25, 1,
  "Reference Standard", "Sample", "Reference Standard", "Sample", "Reference Standard", "Sample", "Reference Standard", "Sample",
  NA, 0.05, 0.05, 19.0483, NA, "Mean values",
  FALSE,
  #-----|-------------|----#
  "BI905711: Bioassay für TRAILR2/CDH17 induzierte Apoptose in COLO 205 Zellen (018-QA062220)",
  7.5, 1, 1.7, 0.001,
  "Reference Standard", "Sample", "Reference Standard", "Sample", "Reference Standard", "Sample", "Reference Standard", "Sample",
  NA, 0.05, 0.05, 11.0407, NA, "Mean values",
  FALSE,
  #-----|-------------|----#
  "655066-01: DB Bioassay (018-GB54629)",
  250, 1, 2, 1,
  "ELISA Standard", "Reference Standard", "Sample", "Reference Standard", "Sample", "Reference Standard", "Sample", "IL23 Control/Cell Growth Control",
  NA, 0.05, 0.05, 29.6309, 'A,H', "Mean values",
  FALSE,
  #-----|-------------|----#
  "655066-01: DT40 IL23-Luciferase Bioassay (018-QA061559)",
  1000, 1, 3, 1,
  "Reference Standard", "Sample", "Reference Standard", "Sample", "Reference Standard", "Sample", "Reference Standard", "Sample",
  NA, 0.05, 0.05, 19.624, NA, "Mean values",
  FALSE,
  #-----|-------------|----#
  "BI905681 Bioassay: Inhibition der Wnt1 induzierten Signalkaskade (018-QA062582)",
  15000, 1, 4, 1,
  "Empty", "Reference Standard", "Sample", "Reference Standard", "Sample", "Reference Standard", "Sample", "Empty",  
  "A,H,12", 0.05, 0.05, 15, NA, "Mean values",
  TRUE,
  #-----|-------------|----#
  "BI905681 Bioassay: Inhibition der Wnt3a induzierten Signalkaskade (018-QA062583)",
  15000, 1, 4, 1,
  "Empty", "Reference Standard", "Sample", "Reference Standard", "Sample", "Reference Standard", "Sample", "Empty",  
  "A,H,12", 0.05, 0.05, 15, NA, "Mean values",
  TRUE,
  #-----|-------------|----#
  "BI655130: Inhibition der IL36 Bindung an IL36R exprimierende Zellen (018-QA062152)",
  15000, 1, 2.5, 1,
  "Reference Standard", "Sample 1", "Sample 2", "Sample 3", "Reference Standard", "Sample 1", "Sample 2", "Sample 3",  
  NA, 0.05, 0.05, 15, NA, "Mean values",
  TRUE,
  #-----|-------------|----#
  "BI754091: PD1 T-Zell Reaktivierungs-Bioassay (018-QA062054)",
  25000, 2, 3, 0.001,
  "Empty", "Reference Standard/Sample", "Sample/Reference Standard", "Reference Standard/Sample", "Sample/Reference Standard", "Reference Standard/Sample", "Sample/Reference Standard", "Empty",  
  "A,H,1,12", 0.05, 0.05, 15, NA, "Mean values",
  TRUE,
  #-----|-------------|----#
  "BI754111: LAG-3 Bioassay (018-QA062048)",
  1750, 1, 1.8, 1,
  "Reference Standard", "Sample", "Control Sample", "Reference Standard", "Sample", "Control Sample", "Reference Standard", "Sample",  
  NA, 0.05, 0.05, 20, NA, "Mean values",
  TRUE,
  #-----|-------------|----#
  "BI764532: CD3 DLL3 induzierte T-Zell vermittelte Zytotoxizitaet (018-QA063000)",
  8000, 1, 3, 1,
  "Reference Standard", "Sample 1", "Sample 2", "Sample 3", "Reference Standard", "Sample 1", "Sample 2", "Sample 3",
  NA, 0.05, 0.05, 15, NA, "Mean values",
  TRUE,
  #-----|-------------|----#
  "BI765080: Bestimmung der Bindungsaktivitaet an Ang2 (ELISA)",
  1800, 1, 3, 1,
  "Reference Standard", "Sample 1", "Sample 2", "Sample 3", "Reference Standard", "Sample 1", "Sample 2", "Sample 3",
  NA, 0.05, 0.05, 7, NA, "Mean values",
  TRUE,
  
  # leave this as last (DO NOT CHANGE) 
  #-----|-------------|----#
  "User defined",
  NA, NA, NA, NA,
  NA, NA, NA, NA, NA, NA, NA, NA, 
  NA, NA, NA, NA, NA, NA,
  TRUE
)


## ----------------------------------- ##
## Assay templates (concentration gradient over rows) ####
## ----------------------------------- ##

# column info
#-------------#
# assay                     : Name of assay template
# conc_start                : Start concentration (final concentration in the first well in ng/mL)
# conc_start_row            : row (A-H), where start concentration is located
# conc_dilution_factor      : Dilution factor(A->B->C...)
# meas_mult                 : Positive numeric value by which measurements should be multiplied prior
#                             to plotting and fitting (changing this may help resolve model fitting
#                             convergence issues)
# col_01,...,col_12         : Column identifier (Standard, Probe, etc.)
# rows_cols_empty           : Empty rows and/or columns - inside "..." and separated by comma (,)
# pval_rosner               : p-value for Rosner outlier test (values allowed: 0.1, 0.05, 0.01, 0.001)
# pval_rout                 : same as above, but for ROUT outlier test
# actionlimit               : action limit as percentage of the value range (difference between upper 
#                             and lower asymptote); must be between 0 and 100
# nofit_ids                 : IDs of columns for which no model fit and outlier tests are performed.
#                             Inside "..." and separated by comma (,). Data in all such columns is plotted
#                             separately by data type
# fit_mode                  : Do fitting before/after outlier removal based on means or indiviaul values?
#                             (must be one of the two values "Mean values" or "Individual values" exactly
#                             in this spelling)
# dev_only                  : Include this assay only in the development version? (boolean)

dat_assay_templates_row <- tribble(
  # column names
  ~assay,
  ~conc_start, ~conc_start_row, ~conc_dilution_factor, ~meas_mult,
  ~col_01, ~col_02, ~col_03, ~col_04, ~col_05, ~col_06, ~col_07, ~col_08, ~col_09, ~col_10, ~col_11, ~col_12,
  ~rows_cols_empty, ~pval_rosner, ~pval_rout, ~actionlimit, ~nofit_ids, ~fit_mode,
  ~dev_only,
  #-----|-------------|----#
  # Currently not implemented due to model convergence issues:
  "836845-01: SK-ES-1 Bioassay (018-GB52371)",
  10000, 'B', 1.5, 0.001,
  "Standard", "Probe 1", "Probe 2", "Kontrolle", "Standard", "Probe 1", "Probe 2", "Kontrolle", "Standard", "Probe 1", "Probe 2", "Kontrolle",
  'A', 0.05, 0.05, 10, NA, "Mean values",
  FALSE,
  
  # leave this as last (DO NOT CHANGE) 
  #-----|-------------|----#
  "User defined",
  NA, NA, NA, NA,
  NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
  NA, NA, NA, NA, NA, NA,
  TRUE
)

