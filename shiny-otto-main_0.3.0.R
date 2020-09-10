######################################################################################################
#  Staburo GmbH
#================================================================================;
# Program name      : shiny-otto-main_0.3.0.R
#
# Author            : Hans Bauer
#
# Date created      : 06NOV2019
#
# Study             : 04_Stat_outlier
#
# Purpose           : Optimal Tool for Tracking Outliers (OTTO)
#
# Template          :
#
# Inputs            : Sourced code files:
#                       - Settings file (optional)
#                       - Assay templates file
#                       - Static functions file
#                     Input data:
#                       - Assay results file
#
# Outputs           : HTML results report file (optional)
#
# Program completed : Yes
#
# Updated by        : See accompanying changelog for version history
#                     
#######################################################################################################;

## ----------------------------------- ##
## Notes ####
## ----------------------------------- ##

# - This version's direct predecessor is OTTO v0.2.1
# - This version requires functions file v0.3.0 or higher
# - This version requires the following columns to be present in the assay templates:
#   assay, conc_start, conc_dilution_factor, meas_mult,
#   rows_cols_empty, pval_rosner, pval_rout, actionlimit, nofit_ids, fit_mode, dev_only
#   For assay template for concentration gradient over columns additionally:
#   conc_start_col, row_A, ..., row_H
#   For assay template for concentration gradient over rows additionally:
#   conc_start_row, col_01,...,col_12


## ----------------------------------- ##
## Set root and library directory ####
## ----------------------------------- ##
# The root directory must be the directory containing the main app file (i.e. this file). The library
# directory should contain all required R packages in the versions for which the app has been validated

rootdir <- 'P:/BI/04_Stat_outlier/outlier-app/OTTO v0.3.0'

# MEMO: Set library path here before formal validation ----

## ----------------------------------- ##
## Load required packages (throw error if not installed) ####
## ----------------------------------- ##

required_packages <- c("tibble", "shiny", "magrittr", "ggplot2", "shinycssloaders",
                       "xtable", "knitr", "kableExtra", "rmarkdown",
                       "dplyr", "tidyr", "data.table", "bbmle", "minpack.lm")

sapply(required_packages, function(pack){
  loaded <- require(pack, character.only = T)
  if(!loaded) {
    # MEMO: Comment in the following and remove automatic installation of packages before formal validation ----
    # stop(paste0("Required package '", pack, "' is not installed! Please install package in required version ",
    #             "prior to re-running the app."))
    install.packages(pack)
    loaded <- require(pack, character.only = T)
  }
  loaded
})


## ---------------------------------------------------------- ##
## l---------------l      Read in and process static data       l--------------l ####
## ---------------------------------------------------------- ##

## ----------------------------------- ##
## Read in and process settings ####
## ----------------------------------- ##

### Set default values

# Need to use withAutoprint(local = F) to load settings into correct environment
withAutoprint({
  
  # Use development mode (full functionality) instead of GMP mode (reduced functionality)?
  dev_mode <- F
  
  # Maximum number of plates displayed in results
  nmax_plates <- 20
  
  # Labels of measurement types which, if present, should always be displayed first
  priority_meas_types <- c("Standard", "Kontrolle")
  
  # Master seed to be used throughout app (needed to ensure consistent results e.g. when same
  # function is called several times)
  master_seed <- 123
  
}, local = F)

### Read in settings file
# - Default values are retained if no file is found
# - If file is found but does not contain settings for all required parameters, default
#   values are retained for those not contained
# - Error is thrown if >1 settings file is in main directory, since it is otherwise unclear
#   which one to use

setting_fn <- list.files(path = rootdir,
                         pattern = 'shiny-otto-settings_.*\\.R$')
if(length(setting_fn) == 0) {
  ## No file found
  
  # Need to use withAutoprint(local = F) to obtain correct environment
  withAutoprint({
    
    # Indicate that no settings file was found in output
    setting_id <- 'No settings file found, used default values'
    
  }, local = F)
  
} else if (length(setting_fn) > 1) {
  
  stop(paste0("There must be only one assay settings file in the app root directory! ",
              "Please move all settings files except the chosen one to a different folder."))
  
} else {
  
  # Read file
  source(file = paste(rootdir, setting_fn, sep = '/'), echo = T,
         max.deparse.length = 10000, encoding = 'UTF-8')
  
  # Get settings file ID
  setting_id <- gsub('^.*shiny-otto-settings_(.*)\\.R$', '\\1', setting_fn)
  
}


### Derive parameters from settings

# Get current version of script from filename
# Error is thrown if >1 file is in main directory, since app version is read from file name

script_fn <- list.files(path = rootdir,
                        pattern = 'shiny-otto-main_.*\\.R$')
if(length(script_fn) == 0) stop(paste0("Main app file not found in root directory!"))
if(length(script_fn) > 1) stop(paste0("There must be only one main app file in the app root directory! ",
                                      "Please move all versions except the current one to a different folder."))
script_version <- gsub('^.*shiny-otto-main_(.*)\\.R$', '\\1', script_fn)

# Combine version and mode information
script_version_mode <- paste(script_version, if(dev_mode) "Dev" else "GMP", sep = '/')



## ----------------------------------- ##
## Read in assay templates file ####
## ----------------------------------- ##

### Read in file
# Error is thrown if >1 template file is in main directory, since it is otherwise unclear which one to use

assay_template_fn <- list.files(path = rootdir,
                                pattern = 'shiny-otto-templates_.*\\.R$')
if(length(assay_template_fn) == 0) stop(paste0("Assay template file not found in root directory!"))
if(length(assay_template_fn) > 1) stop(paste0("There must be only one assay template file in the app root directory! ",
                                              "Please move all template files except the chosen one to a different folder."))
source(file = paste(rootdir, assay_template_fn, sep = '/'), echo = T,
       max.deparse.length = 10000, encoding = 'UTF-8')

### Get template file ID

assay_template_id <- gsub('^.*shiny-otto-templates_(.*)\\.R$', '\\1', assay_template_fn)

### Post read-in processing and checking

# Discard non-GMP mode assays (if applicable)
if(!dev_mode) dat_assay_templates_col <- dat_assay_templates_col %>% filter(!dev_only)
if(!dev_mode) dat_assay_templates_row <- dat_assay_templates_row %>% filter(!dev_only)

# check whether all assay templates have unique names (column: assay)
if(length(unique(dat_assay_templates_col$assay)) != nrow(dat_assay_templates_col)){
  stop("Assay names must be unique.")
}
if(length(unique(dat_assay_templates_row$assay)) != nrow(dat_assay_templates_row)){
  stop("Assay names must be unique.")
}

## ----------------------------------- ##
## Read in functions file ####
## ----------------------------------- ##

### Read in file
# Error is thrown if >1 functions file is in main directory, since it is otherwise unclear which one to use

function_fn <- list.files(path = rootdir,
                          pattern = 'shiny-otto-functions_.*\\.R$')
if(length(function_fn) == 0) stop(paste0("Functions file not found in root directory!"))
if(length(function_fn) > 1) stop(paste0("There must be only one functions file in the app root directory! ",
                                        "Please move all versions except the current one to a different folder."))
source(file = paste(rootdir, function_fn, sep = '/'), echo = T,
       max.deparse.length = 10000, encoding = 'UTF-8')

### Get functions file version number

function_version <- gsub('^.*shiny-otto-functions_(.*)\\.R$', '\\1', function_fn)



## ---------------------------------------------------------- ##
## l---------------l Main app program code l--------------l ####
## ---------------------------------------------------------- ##


## ----------------------------------- ##
## UI  ####
## ----------------------------------- ##


ui <- fluidPage(
  
  # Customize styles
  tags$head(
    tags$style(HTML(
      paste0(
        # Column widths for summary table
        '#outlier_summary th:nth-child(1) {width: 10%;} ',
        '#outlier_summary th:nth-child(2) {width: 40%;} ',
        '#outlier_summary th:nth-child(3) {width: 50%;} '
      )
    ))
  ),
  
  # title
  titlePanel("Optimal Tool for Tracking Outliers"),
  
  sidebarLayout(
    
    # control panel
    sidebarPanel(style = "position:fixed;width:30%;height:90%;overflow-y:auto;",
                 width = 4,
                 
                 # input of plate layout
                 conditionalPanel(
                   "output.dev_mode == 'TRUE'",
                   selectInput("plate_layout", 
                               "Plate layout: Concentration gradient over...",
                               choices = c("Columns", "Rows"))
                 ),
                 
                 # dynamic assay template selection
                 uiOutput("ui_input_assay_template"),
                 
                 # browse file
                 fileInput("file1", 
                           "Select TXT file to import",
                           accept = c("text/plain", ".txt"),
                           placeholder = ""),
                 
                 # dynamic selection of plate inside file (updated after file uploaded)
                 uiOutput("ui_select_plates"),
                 
                 # select official test
                 conditionalPanel(
                   "output.dev_mode == 'TRUE'",
                   selectInput("official_test", 
                               "Choose official outlier test",
                               choices = c("Rosner", "ROUT"))
                 ),
                 
                 
                 # check if manual masking of wells needed
                 checkboxInput("check_mask_wells_manual", 
                               "Mask wells manually?"),
                 # if before checked yes, then show select fields for masking
                 conditionalPanel("input.check_mask_wells_manual == true",
                                  uiOutput("ui_mask_wells")),
                 
                 
                 # user interface controls, if assay template is "User defined"
                 uiOutput("ui_controls"),
                 
                 
                 # horizontal rule
                 tags$hr(style="border-color: black;"),
                 
                 # button to run tests
                 actionButton("run_tests", "Run outlier tests"),
                 
                 # if tests have been run, show download HTML button
                 uiOutput("ui_create_html")
                 
                 
                 
                 
    ),
    
    # panel with results
    mainPanel(width = 8,
              
              h1("General information"),
              
              tags$p(paste0("Main app version/mode: ", script_version_mode),
                     br(),
                     paste0("Settings file ID: ", setting_id),
                     br(),
                     paste0("Functions file version: ", function_version),
                     br(),
                     paste0("Assay template file ID: ", assay_template_id)),
              
              br(),

              # info from uploaded file
              h4("Trial"),
              verbatimTextOutput("imported_info1"),
              uiOutput("imported_info2"),
              
              # info on chosen/defined assay
              h4("Assay template"),
              uiOutput("ui_assay_template"),
              
              # some space
              br(), br(), br(),
              
              h1("Results"),
              
              br(),
              
              # summary of outliers
              uiOutput("outlier_summary"),
              
              br(), br(), br(),
              
              # imported data and results of tests (all in one, for all selected plates)
              uiOutput("imported_plates"),
              
              br(), br(), br(),
              
              h1("Session information"),
              tags$pre(session_info_str())
              
    )
  )
)


## ----------------------------------- ##
## server ####
## ----------------------------------- ##



server <- function(input, output, session) {
  
  ## ----------------------------------- ##
  ## run in development mode? (set via global option) ####
  ## ----------------------------------- ##
  
  output$dev_mode <- renderText({if(dev_mode) "TRUE" else "FALSE"})
  outputOptions(output, 'dev_mode', suspendWhenHidden = F)
  
  ## ----------------------------------- ##
  ## input: assay templates (based on plate layout) ####
  ## ----------------------------------- ##
  
  
  output$ui_input_assay_template <- renderUI({
    
    # switch choices of selectInput based on plate layout
    if(input$plate_layout == "Columns"){
      selectInput("assay_template", 
                  "Choose template for assay",
                  choices = dat_assay_templates_col$assay)
    } else {
      selectInput("assay_template", 
                  "Choose template for assay",
                  choices = dat_assay_templates_row$assay)
    }
    
  })
  
  
  ## ----------------------------------- ##
  ## input: ui controls (assay = User defined)  ####
  ## ----------------------------------- ##
  
  
  output$ui_controls <- renderUI({
    
    # require assay_template to be specified (otherwise silent error)
    req(input$assay_template)
    
    # only show anything, if template == "User defined"
    if(input$assay_template == "User defined"){
      
      
      if(input$plate_layout == "Columns"){
        
        # controls needed if plate_layout is concentration over columns
        
        list(
          tags$hr(style="border-color: black;"),
          
          # check if settings should be imported from existing template
          checkboxInput("import_from_template", "Import settings from template"),
          # if yes, then select which template and add button
          conditionalPanel("input.import_from_template == true",
                           selectInput("import_template", 
                                       "Choose template to import",
                                       choices = dat_assay_templates_col$assay[dat_assay_templates_col$assay != "User defined"]),
                           actionButton("do_import_template", "Import")),
          tags$hr(style="border-color: black;"),
          
          # dynamic button to guess settings from SMP (if available in file)
          uiOutput("ui_import_info_smp"),
          
          # input of concentration (start value, column number, dilution factor)
          textInput("conc_start", "Start concentration", "100"),
          selectInput("conc_start_col", "Column number with start concentration",
                      choices = sprintf("%02d", 1:12)),
          textInput("conc_dilution_factor", "Dilution factor (col: 1 -> 2, 2 -> 3, ...)", "1.5"),
          
          # measurement multiplier
          textInput("meas_mult", "Multiplier applied to all measurements prior to plotting and fitting", "1"),
          
          # pre-define group labels for rows
          textInput("rowcol_factor_labels", 
                    "Choose names for row factors (separated by comma)",
                    value = "Standard, Probe"),
          # dynamic select fields for all rows with previously defined labels
          tags$label("Choose for each row:"),
          uiOutput("ui_plate_info_rows"),
          
          # input empty rows/cols
          selectInput("rows_cols_empty", "Empty rows/columns",
                      choices = c(sprintf("%02d", 1:12), LETTERS[1:8]), 
                      selected = NULL, multiple = T),
          
          # (non-empty/masked) rows to be excluded from fitting
          selectInput("nofit_ids", "Rows to be excluded from fitting",
                      choices = c(LETTERS[1:8]), 
                      selected = NULL, multiple = T),
          
          # mode of curve fitting pre-/post outlier removal (mean/individual values)
          selectInput("fit_mode", 
                      "Fitting/R2: ",
                      choices = c("Mean values", "Individual values"), selected = "Mean values"),
          
          # select p-values for outlier tests
          selectInput("pval_rosner", "P-value for Rosner outlier test", 
                      choices = c("0.1", "0.05", "0.01", "0.001"), selected = "0.01"),
          selectInput("pval_rout", "P-value for ROUT outlier test", 
                      choices = c("0.1", "0.05", "0.01", "0.001"), selected = "0.01"),
          
          # input actionlimit
          textInput("actionlimit", "Action limit [% of difference between asymptotes; values allowed: 0-100]", "0")
          
        )
        
      } else {
        # controls needed if plate_layout is concentration over rows
        
        list(
          tags$hr(style="border-color: black;"),
          
          # check if settings should be imported from existing template
          checkboxInput("import_from_template", "Import settings from template"),
          # if yes, then select which template and add button
          conditionalPanel("input.import_from_template == true",
                           selectInput("import_template", "Choose template to import",
                                       choices = dat_assay_templates_row$assay[dat_assay_templates_row$assay != "User defined"]),
                           actionButton("do_import_template", "Import")),
          tags$hr(style="border-color: black;"),
          
          # MEMO: Import from SMP for row layout not yet implemented ----
          # uiOutput("ui_import_info_smp")
          
          # input of concentration (start value, row, dilution factor)
          textInput("conc_start", "Start concentration", "100"),
          selectInput("conc_start_row", "Row with start concentration",
                      choices = LETTERS[1:8]),
          textInput("conc_dilution_factor", "Dilution factor (col: 1 -> 2, 2 -> 3, ...)", "1.5"),
          
          # measurement multiplier
          textInput("meas_mult", "Multiplier applied to all measurements prior to plotting and fitting", "1"),
          
          # pre-define group labels for cols
          textInput("rowcol_factor_labels", 
                    "Choose names for column factors (separated by comma)",
                    value = "Standard, Probe"),
          # dynamic select fields for all columns with previously defined labels
          tags$label("Choose for each column:"),
          uiOutput("ui_plate_info_cols"),
          
          # input empty rows/cols
          selectInput("rows_cols_empty", "Empty rows/columns",
                      choices = c(sprintf("%02d", 1:12), LETTERS[1:8]), 
                      selected = NULL, multiple = T),
          
          # (non-empty/masked) columns to be excluded from fitting
          selectInput("nofit_ids", "Columns to be excluded from fitting",
                      choices = c(sprintf("%02d", 1:12)), 
                      selected = NULL, multiple = T),
          
          # select p-values for outlier tests
          selectInput("pval_rosner", "P-value for Rosner outlier test", 
                      choices = c("0.1", "0.05", "0.01", "0.001"), selected = "0.05"),
          selectInput("pval_rout", "P-value for ROUT outlier test", 
                      choices = c("0.1", "0.05", "0.01", "0.001"), selected = "0.05"),
          
          # input actionlimit
          textInput("actionlimit", "Action limit [% of difference between asymptotes; values allowed: 0-100]", "0")
          
          
        )
        
      }
      
    }
    
    
  })
  
  
  ## ----------------------------------- ##
  ## input: selection of plates (based on import) ####
  ## ----------------------------------- ##
  
  output$ui_select_plates <- renderUI({
    
    # get choices (if no file uploaded, otherwise number of plates)
    plates <- if(is.null(input$file1)){
      "(must import file first)"
    } else {
      1:list_imported()[["n_plates"]]
    }
    
    selectInput("selected_plates", "Select plates",
                choices = plates, multiple = T, selected = plates)
    
  })
  
  
  ## ----------------------------------- ##
  ## input: masked wells (manually)  ####
  ## ----------------------------------- ##
  
  output$ui_mask_wells <- renderUI({
    
    # needed: file uploaded
    req(input$file1)
    
    # vector of all possible wells
    lapply(LETTERS[1:8], function(x) paste0(x, 1:12)) %>% unlist -> choices_wells
    
    # add complete rows/cols
    choices <- c(paste0(LETTERS[1:8], " (complete row)"), 
                 paste0(sprintf("%02d", 1:12), " (complete column)"), 
                 choices_wells)
    
    # dynamic input field for all selected plates
    l <- lapply(sort(input$selected_plates), function(i){
      f_mask_input(paste0("plate_", i), choices)
    }) 
    
    # input form, with some tweaks to show hr()
    list(tags$label("Choose wells (or complete rows and columns) to mask per plate:"),
         do.call(tags$form, c(list(class="form-vertical"), l)),
         tags$hr(style="border-color: black;"),
         tags$span(style="color:rgb(246, 247, 249)", "blank empty line"))
    
  })
  
  
  
  ## ----------------------------------- ##
  ## input: plate info - row/col fields  ####
  ## ----------------------------------- ##
  
  output$ui_plate_info_rows <- renderUI({
    
    init_rowcol_factor_labels <- c("Standard", "Probe")
    
    tags$form(
      class="form-vertical",
      f_row_label_input("A", init_rowcol_factor_labels),
      f_row_label_input("B", init_rowcol_factor_labels),
      f_row_label_input("C", init_rowcol_factor_labels),
      f_row_label_input("D", init_rowcol_factor_labels),
      f_row_label_input("E", init_rowcol_factor_labels),
      f_row_label_input("F", init_rowcol_factor_labels),
      f_row_label_input("G", init_rowcol_factor_labels),
      f_row_label_input("H", init_rowcol_factor_labels)
    )
    
    
  })
  
  output$ui_plate_info_cols <- renderUI({
    
    init_rowcol_factor_labels <- c("Standard", "Probe")
    
    tags$form(
      class="form-vertical",
      f_col_label_input("01", init_rowcol_factor_labels),
      f_col_label_input("02", init_rowcol_factor_labels),
      f_col_label_input("03", init_rowcol_factor_labels),
      f_col_label_input("04", init_rowcol_factor_labels),
      f_col_label_input("05", init_rowcol_factor_labels),
      f_col_label_input("06", init_rowcol_factor_labels),
      f_col_label_input("07", init_rowcol_factor_labels),
      f_col_label_input("08", init_rowcol_factor_labels),
      f_col_label_input("09", init_rowcol_factor_labels),
      f_col_label_input("10", init_rowcol_factor_labels),
      f_col_label_input("11", init_rowcol_factor_labels),
      f_col_label_input("12", init_rowcol_factor_labels)
    )
    
    
  })
  
  
  
  
  ## ----------------------------------- ##
  ## input: button, import from SMP  ####
  ## ----------------------------------- ##
  
  
  output$ui_import_info_smp <- renderUI({
    
    # file is needed
    req(input$file1)
    
    # only show button if information has been exported into file
    if(!is.null(list_imported()$smp_add_info)) {
      
      list(actionButton("do_import_info_smp", "Guess row/col info from SMP file"),
           tags$hr(style="border-color: black;"))
      
    }
    
    
  })
  
  ## ----------------------------------- ##
  ## input: routine for updating of row/column fields  ####
  ## ----------------------------------- ##
  
  # get values of current selection (rows, if plate layout is columns)
  current_rows <- reactive({
    
    l <- list(input$row_A, input$row_B, input$row_C, input$row_D,
              input$row_E, input$row_F, input$row_G, input$row_H)
    names(l) <- LETTERS[1:8]
    l
    
  })
  
  # get values of current selection (cols, if plate layout is rows)
  current_cols <- reactive({
    
    l <- list(input$col_01, input$col_02, input$col_03, input$col_04,
              input$col_05, input$col_06, input$col_07, input$col_08,
              input$col_09, input$col_10, input$col_11, input$col_12)
    names(l) <- sprintf("col_%02d", 1:12)
    l
    
  })
  
  # get current group labels (row/col)
  current_rowcol_factor_labels <- reactive({
    req(input$rowcol_factor_labels)
    trimws(strsplit(input$rowcol_factor_labels, ",")[[1]])
  })
  
  
  # routine to keep selected values, if choices have changed
  observe({
    
    if(isolate(input$plate_layout) == "Columns"){
      
      for(x in LETTERS[1:8]){
        updateSelectInput(session, paste0("row_", x),
                          selected = current_rows()[[x]],
                          choices = isolate(current_rowcol_factor_labels()))
      }
      
    } else {
      
      for(x in sprintf("col_%02d", 1:12)){
        updateSelectInput(session, x,
                          selected = current_cols()[[x]],
                          choices = isolate(current_rowcol_factor_labels()))
      }
      
    }
    
    
  })
  
  # routine to update selection choices, but to keep selected values
  observe({
    
    if(isolate(input$plate_layout) == "Columns"){
      
      for(x in LETTERS[1:8]){
        updateSelectInput(session, paste0("row_", x),
                          selected = isolate(current_rows()[[x]]),
                          choices = current_rowcol_factor_labels())
      }
      
    } else {
      
      for(x in sprintf("col_%02d", 1:12)){
        updateSelectInput(session, x,
                          selected = isolate(current_cols()[[x]]),
                          choices = current_rowcol_factor_labels())
      }
      
    }
    
  })
  
  
  ## ----------------------------------- ##
  ## input: show create HMTL button  ####
  ## ----------------------------------- ##
  
  output$ui_create_html <- renderUI({
    
    if(!is.null(show_results$data)) {
      list(tags$hr(style="border-color: black;"),
           downloadButton("create_html.html", "Create HTML file of results"),
           helpText("Note: Please select a filename that does not already exist,",
                    "if running from within the RStudio browser, since",
                    "this will result in weird output otherwise.",
                    "If running from any other browser (IE, Chrome, etc.),",
                    "filenames should be predefined (using the current date and time)."))
    }
    
  })
  
  
  ## ----------------------------------- ##
  ## action: Import from template  ####
  ## ----------------------------------- ##
  
  
  observeEvent(input$do_import_template, {
    
    # assay to import
    current_assay <- filter(current_assay_templates(), assay == input$import_template)
    
    
    # update all controls from template
    # some depending on plate layout
    
    if(input$plate_layout == "Columns"){
      
      updateSelectInput(session,
                        "conc_start_col", selected = sprintf("%02d", current_assay$conc_start_col))
      
      current_assay %>% 
        select(row_A : row_H) %>% 
        unlist %>% 
        unique -> in_rowcol_factor_labels
      
      updateTextInput(session,
                      "rowcol_factor_labels", value = paste0(in_rowcol_factor_labels, collapse = ", "))
      
      for(x in LETTERS[1:8]){
        updateSelectInput(session, paste0("row_", x),
                          choices = in_rowcol_factor_labels,
                          selected = current_assay[[paste0("row_", x)]])
      }
      
    } else {
      
      updateSelectInput(session,
                        "conc_start_row", selected = current_assay$conc_start_row)
      
      current_assay %>% 
        select(col_01 : col_12) %>% 
        unlist %>% 
        unique -> in_rowcol_factor_labels
      
      updateTextInput(session,
                      "rowcol_factor_labels", value = paste0(in_rowcol_factor_labels, collapse = ", "))
      
      for(x in sprintf("col_%02d", 1:12)){
        updateSelectInput(session, x,
                          choices = in_rowcol_factor_labels,
                          selected = current_assay[[x]])
      }
      
    }
    
    
    updateTextInput(session,
                    "conc_start", value = current_assay$conc_start)
    
    updateTextInput(session,
                    "conc_dilution_factor", value = current_assay$conc_dilution_factor)
    
    updateTextInput(session,
                    "meas_mult", value = current_assay$meas_mult)
    
    current_assay$rows_cols_empty %>% 
      as.character %>% 
      strsplit(",") %>% 
      .[[1]] -> rows_cols_empty 
    id_num <- which(!rows_cols_empty %in% LETTERS[1:8])
    rows_cols_empty[id_num] <- sprintf("%02d", as.integer(rows_cols_empty[id_num]))
    
    updateSelectInput(session,
                      "rows_cols_empty", selected = rows_cols_empty)
    
    current_assay$nofit_ids %>% 
      as.character %>% 
      strsplit(",") %>% 
      .[[1]] -> nofit_ids 
    
    updateSelectInput(session,
                      "nofit_ids", selected = nofit_ids)
    
    updateSelectInput(session,
                      "fit_mode", selected = current_assay$fit_mode)
    
    updateSelectInput(session,
                      "pval_rosner", selected = current_assay$pval_rosner)
    
    updateSelectInput(session,
                      "pval_rout", selected = current_assay$pval_rout)
    
    updateSelectInput(session,
                      "actionlimit", selected = current_assay$actionlimit)
    
    
    
    
    
  })
  
  
  
  ## ----------------------------------- ##
  ## action: Import from SMP (only row/col info)  ####
  ## ----------------------------------- ##
  
  
  
  observeEvent(input$do_import_info_smp, {
    
    # get additonal imported info from SMP
    dat_info <- list_imported()$smp_add_info
    
    
    # update controls
    
    updateTextInput(session,
                    "conc_start", value = dat_info$conc_start)
    updateSelectInput(session,
                      "conc_start_col", selected = sprintf("%02d", dat_info$conc_start_col))
    updateTextInput(session,
                    "conc_dilution_factor", value = dat_info$conc_dilution_factor)
    
    dat_info %>% 
      select(row_A : row_H) %>% 
      unlist %>% 
      unique -> in_rowcol_factor_labels
    
    updateTextInput(session,
                    "rowcol_factor_labels", value = paste0(in_rowcol_factor_labels, collapse = ", "))
    
    for(x in LETTERS[1:8]){
      updateSelectInput(session, paste0("row_", x),
                        choices = in_rowcol_factor_labels,
                        selected = dat_info[[paste0("row_", x)]])
    }
    
    
  })
  
  
  ## ----------------------------------- ##
  ## reactiveness: show/compute results only after button pressed  ####
  ## ----------------------------------- ##  
  
  # create reactive value data
  show_results <- reactiveValues(data = NULL)
  
  
  observe({
    # take dependency on pressing of button
    input$run_tests
    
    # change (activate) showresults 
    show_results$data <- input$run_tests
  })
  
  
  observe({
    # take dependency on other changes in inputs that affect results
    input$fit_mode
    input$plate_layout
    input$assay_template
    input$file1
    input$conc_start
    input$conc_start_col
    input$conc_start_row
    input$conc_dilution_factor
    input$meas_mult
    input$rowcol_factor_labels
    input$rows_cols_empty
    input$nofit_ids
    input$pval_rosner
    input$pval_rout
    input$actionlimit
    input$selected_plates
    
    lapply(input$selected_plates, function(i) input[[paste0("masked_wells_plate_", i)]])
    lapply(LETTERS[1:8], function(x) input[[paste0("row_", x)]])
    lapply(sprintf("col_%02d", 1:12), function(x) input[[x]])
    
    # change (deactivate) show_results
    show_results$data <- NULL
  })
  
  
  
  ## ----------------------------------- ##
  ## data: current assay templates (= current_assay_templates) ####
  ## ----------------------------------- ##
  
  current_assay_templates <- reactive({
    
    if(input$plate_layout == "Columns"){
      dat_assay_templates_col
    } else {
      dat_assay_templates_row
    }
    
  })
  
  ## ----------------------------------- ##
  ## data: current assay (= current_assay) ####
  ## ----------------------------------- ##
  
  current_assay <- reactive({
    
    # take dependency on chosen assay template
    input$assay_template
    
    # isolate from choice of templates
    isolate({
      current_assay_templates() %>% 
        filter(assay == input$assay_template)
    })
    
  })
  
  
  
  
  
  ## ----------------------------------- ##
  ## data: imported data from file (= list_imported) ####
  ## ----------------------------------- ##
  
  list_imported <- reactive({
    
    # only do if file is uploaded
    req(input$file1)
    
    # try to read file
    tryCatch(
      {
        f_read(input$file1$datapath)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
  })
  
  
  
  ## ----------------------------------- ##
  ## data: row identifiers (= dat_info_rows) ####
  ## ----------------------------------- ##
  
  
  dat_info_rows <- reactive({
    
    
    if(isolate(input$plate_layout) == "Columns"){
      # ---------------------------------------- #
      # plate layout (gradient over columns)
      
      
      if(input$assay_template == 'User defined'){
        
        # get input as defined in UI  
        req(input$row_A)
        
        dat_info <- tibble(rows = LETTERS[1:8],
                           rowcol_factor = c(input$row_A, input$row_B, input$row_C, input$row_D,
                                             input$row_E, input$row_F, input$row_G, input$row_H))
        
      } else {
        
        # get info from template
        current_assay() %>% 
          select(row_A : row_H) %>% 
          gather(key = "rows", value = "rowcol_factor") %>% 
          mutate(rows = gsub("row_", "", rows)) -> dat_info
        
      }
      
      # ordering of row factors (put prioritized measurement types first)
      dat_info$rowcol_factor %>% 
        unique %>% 
        .[!. %in% priority_meas_types] %>% 
        sort -> else_rowcol_factor
      
      # return data
      dat_info %>% 
        mutate(rowcol_factor = factor(rowcol_factor, levels = c(priority_meas_types, else_rowcol_factor))) %>% 
        add_column(row_n = 1:8, .before = 1)
      
      
    } else {
      # ---------------------------------------- #
      # plate layout (gradient over rows)
      
      if(input$assay_template == 'User defined'){
        
        # get input as defined in UI
        req(input$conc_start)
        
        input$conc_start %>% 
          gsub(",", ".", .) %>% 
          as.numeric -> in_conc_start
        
        input$conc_dilution_factor %>% 
          gsub(",", ".", .) %>% 
          as.numeric -> in_conc_dilution_factor
        
        row_n_start <- which(LETTERS[1:8] == input$conc_start_row)
        
        # return data
        tibble(row_n = 1:8,
               rows = LETTERS[1:8],
               dilution_factor = if_else(row_n < row_n_start,
                                         NA_real_,
                                         in_conc_dilution_factor^(row_n - row_n_start)),
               conc = in_conc_start / dilution_factor,
               log10_conc = log10(conc)) 
        
      } else {
        
        # get data from template
        current_assay <- current_assay()
        
        row_n_start <- which(LETTERS[1:8] == current_assay$conc_start_row)
        
        tibble(row_n = 1:8,
               rows = LETTERS[1:8],
               dilution_factor = if_else(row_n < row_n_start,
                                         NA_real_,
                                         current_assay$conc_dilution_factor^(row_n - row_n_start)),
               conc = current_assay$conc_start / dilution_factor,
               log10_conc = log10(conc)) 
        
        
      }
      
    }
    
    
    
  })
  
  
  
  
  
  ## ----------------------------------- ##
  ## data: column concentrations  (= dat_info_cols) ####
  ## ----------------------------------- ##
  
  
  dat_info_cols <- reactive({
    
    if(isolate(input$plate_layout) == "Columns"){
      # ---------------------------------------- #
      # plate layout (gradient over columns)
      
      
      if(input$assay_template == 'User defined'){
        
        # get input from UI
        req(input$conc_start)
        
        input$conc_start %>% 
          gsub(",", ".", .) %>% 
          as.numeric -> in_conc_start
        
        input$conc_start_col %>% 
          as.numeric -> in_conc_start_col
        
        input$conc_dilution_factor %>% 
          gsub(",", ".", .) %>% 
          as.numeric -> in_conc_dilution_factor
        
        tibble(col_n = 1:12,
               cols = sprintf("col_%02d", 1:12),
               dilution_factor = if_else(col_n < in_conc_start_col,
                                         NA_real_,
                                         in_conc_dilution_factor^(col_n - in_conc_start_col)),
               conc = in_conc_start / dilution_factor,
               log10_conc = log10(conc)) 
        
      } else {
        
        # get data from template
        current_assay <- current_assay()
        
        
        tibble(col_n = 1:12,
               cols = sprintf("col_%02d", 1:12),
               dilution_factor = if_else(col_n < current_assay$conc_start_col,
                                         NA_real_,
                                         current_assay$conc_dilution_factor^(col_n - current_assay$conc_start_col)),
               conc = current_assay$conc_start / dilution_factor,
               log10_conc = log10(conc))
        
      }
      
    } else {
      # ---------------------------------------- #
      # plate layout (gradient over rows)
      
      
      if(input$assay_template == 'User defined'){
        
        # get info from UI
        req(input$col_01)
        
        dat_info <- tibble(cols =  sprintf("col_%02d", 1:12),
                           rowcol_factor = c(input$col_01, input$col_02, input$col_03, input$col_04,
                                             input$col_05, input$col_06, input$col_07, input$col_08,
                                             input$col_09, input$col_10, input$col_11, input$col_12))
        
      } else {
        
        # get data from template
        current_assay() %>% 
          select(col_01 : col_12) %>% 
          gather(key = "cols", value = "rowcol_factor") -> dat_info
        
      }
      
      # ordering of row factors (put prioritized measurement types first)
      dat_info$rowcol_factor %>% 
        unique %>% 
        .[!. %in% priority_meas_types] %>% 
        sort -> else_rowcol_factor
      
      dat_info %>% 
        mutate(rowcol_factor = factor(rowcol_factor, levels = c(priority_meas_types, else_rowcol_factor))) %>% 
        add_column(col_n = 1:12, .before = 1)
      
    }
    
  })
  
  
  
  
  
  ## ----------------------------------- ##
  ## data: empty cols and rows from template (= l_empty_template) ####
  ## ----------------------------------- ##
  
  
  l_empty_template <- reactive({
    
    to_remove <- if(input$assay_template == 'User defined') {
      # get input from UI
      input$rows_cols_empty 
      
    } else {
      # get info from template
      current_assay() %>% 
        pull(rows_cols_empty) %>% 
        as.character %>% 
        strsplit(",") %>% 
        .[[1]]
    } 
    
    # check if any values inside
    if(length(to_remove) && !is.na(to_remove)){
      
      # split vector into letters (rows) and numbers (columns)
      id_letters <- which(to_remove %in% LETTERS[1:8])
      to_remove_rows <- to_remove[id_letters]
      
      to_remove_cols1 <- if(length(id_letters)) to_remove[-id_letters] else to_remove
      to_remove_cols2 <- sprintf("col_%02d", as.integer(to_remove_cols1))
      
      # return list
      list(rows = to_remove_rows,
           cols = to_remove_cols2)
      
    } else {
      # return NULL if none removed
      NULL
    }
    
    
  })
  
  
  
  
  ## ----------------------------------- ##
  ## data: IDs of rows to be excluded from fitting, from template (= nofit_template) ####
  ## ----------------------------------- ##
  
  nofit_template <- reactive({
    
    # take dependency on selected assay template
    # input$assay_template
    
    nofit_template_out <- if(input$assay_template == 'User defined') {
      # get input from UI
      input$nofit_ids 
      
    } else {
      # get info from template
      current_assay() %>% 
        pull(nofit_ids) %>% 
        as.character %>% 
        strsplit(",") %>% 
        .[[1]]
    } 
    
    # check if any values inside
    if(length(nofit_template_out) && !is.na(nofit_template_out)) {
      
      # Row layout: reformat column IDs
      if(input$plate_layout == "Rows") {
        # Use error handler to catch cases where current_assay() is not yet updated
        # (may happen when plate layout selection is changed)
        nofit_template_out <- tryCatch(sprintf("col_%02d", as.integer(nofit_template_out)),
                                       error = function(e) NULL,
                                       warning = function(e) NULL)
      }
      
      nofit_template_out
      
    } else {
      # return NULL if none
      NULL
    }
    
    
  })
  
  
  ## ----------------------------------- ##
  ## data: masked wells, manual (= l_masked_wells_manual) ####
  ## ----------------------------------- ##
  
  l_masked_wells_manual <- reactive({
    
    # require plate selection
    req(input$selected_plates)
    
    # only run after file has finished uploading
    if(input$selected_plates[1] != "(must import file first)"){
      
      # create empty list with length n_plates
      l <- vector("list", list_imported()$n_plates)
      
      # fill list with values as input in UI
      for(i in as.numeric(sort(input$selected_plates))){
        in_mask <- input[[paste0("masked_wells_plate_", i)]]
        
        # get complete rows and fill with column values
        grep("complete row", in_mask, value = T) %>% 
          substr(1,1) %>% 
          lapply(function(row) tibble(rows = row, cols = sprintf("col_%02d", 1:12))) %>% 
          bind_rows -> dat_complete_rows
        
        # get complete cols and fill with row values
        grep("complete column", in_mask, value = T) %>% 
          substr(1, 2) %>% 
          lapply(function(col) tibble(rows = LETTERS[1:8], cols = sprintf("col_%02d", as.integer(col)))) %>% 
          bind_rows -> dat_complete_cols
        
        # get single wells
        in_wells <- grep("complete", in_mask, value = T, invert = T) 
        dat_wells <- tibble(rows = substr(in_wells, 1, 1),
                            cols = sprintf("col_%02d", as.integer(substr(in_wells, 2, 4))))
        
        # return if any marked
        if(length(in_mask)){
          l[[i]] <- bind_rows(dat_complete_rows, dat_complete_cols, dat_wells)
        } 
      }
      
      l
    }
    
  })
  
  
  
  ## ----------------------------------- ##
  ## data: internal multiplier to apply to measurements (= meas_mult) ####
  ## ----------------------------------- ##
  
  meas_mult <- reactive({
    
    req(input$assay_template)
    
    meas_mult_out <- if(input$assay_template == 'User defined') {
      
      req(input$meas_mult)
      
      # get input from UI
      if(grepl('^ *\\d+(?:[.,]\\d+)? *$', input$meas_mult) && input$meas_mult != 0) {
        as.numeric(input$meas_mult %>% gsub(",", ".", .))
      } else {
        NA
      }
      
      
    } else {
      # get info from template
      current_assay() %>% pull(meas_mult)
    } 
    
    # check if any values inside
    if(length(meas_mult_out) && !is.na(meas_mult_out)) {
      
      meas_mult_out
      
    } else {
      # return NULL if none
      NULL
    }
    
    
  })
  
  
  ## ----------------------------------- ##
  ## data: fit mode (=fit_mode) ####
  ## ----------------------------------- ##
  
  fit_mode <- reactive({
    
    req(input$assay_template)
    
    if(input$assay_template == 'User defined'){
      # get from user input
      req(input$fit_mode)
      input$fit_mode
    } else {
      # get from template
      current_assay() %>% 
        pull(fit_mode)
    }
    
  })
  
  
  ## ----------------------------------- ##
  ## data: pre-processing of data (= l_data) ####
  ## ----------------------------------- ##
  
  # remove empty rows/cols; add masked values id
  l_data <- reactive({
    
    # run only after button pressed
    if(is.null(show_results$data)) return()
    
    # remove dependency from other changes in inputs
    isolate({
      
      
      # do for all plates
      lapply(1:list_imported()$n_plates, function(i){
        
        # get raw plate data
        dat <- list_imported()[["list_plates"]][[i]]
        
        # if any empty rows/cols in template, remove them
        if(!is.null(l_empty_template())){
          dat %>% 
            filter(! rows %in% l_empty_template()$rows) %>% 
            select(-one_of(l_empty_template()$cols)) -> dat
        }
        
        # remove empty rows/cols that have been guessed from imported data
        dat %>% 
          filter(! rows %in% list_imported()[["list_empty"]][[i]][["rows"]]) %>% 
          select(-one_of(list_imported()[["list_empty"]][[i]][["cols"]])) -> dat
        
        
        # put plate data from wide into long format
        # join row & col information
        dat %>% 
          gather(key = "cols", value = "value", -rows) %>% 
          left_join(dat_info_rows(), by = "rows") %>% 
          left_join(dat_info_cols(), by = "cols") -> dat_plot
        
        
        # combine masked wells (guessed from imported file and manually input in UI) 
        dat_masked <- bind_rows(list_imported()[["list_masked"]][[i]] %>% select(rows, cols),
                                l_masked_wells_manual()[[i]])
        if(nrow(dat_masked)){
          # if any masked, then add identifier and join to data
          dat_masked$masked <- "yes"
          dat_plot <- left_join(dat_plot, dat_masked, by = c("rows", "cols"))
          dat_plot$masked[is.na(dat_plot$masked)] <- "no"
        } else {
          # if none masked, create id vector with "no" only
          dat_plot$masked <- "no"
        }
        
        # add flag for cases from rows to be excluded from fitting
        # Note: This requires the sets of admissible values for column and row identifiers
        # to be mutually exclusive (which is guaranteed through the use of letters/numbers
        # as row/column identifiers)
        dat_plot$nofit <- ifelse(dat_plot$rows %in% nofit_template() |
                                   dat_plot$cols %in% nofit_template(), "yes", "no")
        
        # store original measurement value from input file and then apply measurement multiplier
        dat_plot$value_infile <- dat_plot$value
        if(!is.null(meas_mult())) dat_plot$value <- dat_plot$value_infile * meas_mult()
        
        # add boolean indicator whether observation is outside of initially determined
        # actionlimit, based on all measurements that are not masked or excluded from fitting
        # if for a given measurement type (e.g. "Standard"), a model cannot be fit, measurements
        # of this type are excluded from all subsequent fitting and outlier removal procedures,
        # and plotted separately
        
        dat_reduced <- dat_plot %>%
          filter(masked == "no") %>%
          filter(nofit == "no")
        dat_reduced$outside_al_init <- NA
        
        meas_types <- data.frame(meas_type = unique(dat_reduced$rowcol_factor), fit_err = 'no',
                                 stringsAsFactors = F)
        for(mt in meas_types$meas_type) {
          
          outside_al_meas_type <- try({
            add_outside_actionlimit(
              xx = dat_reduced[which(dat_reduced$rowcol_factor == mt), 'log10_conc', drop = T],
              yy = dat_reduced[which(dat_reduced$rowcol_factor == mt), 'value', drop = T],
              actionlimit())
            }, silent = T)
          
          if(inherits(outside_al_meas_type, "try-error")) {
            print(paste0("Model fitting failure in trying to determine whether point is outside action limit ",
                         "for measurement type <", mt, "> in plate ", i, ". No action limit and outlier analysis ",
                         "is possible for these measurements."))
            meas_types[which(meas_types$meas_type == mt), 'fit_err'] <- "yes"
          } else {
            dat_reduced[which(dat_reduced$rowcol_factor == mt), 'outside_al_init'] <- outside_al_meas_type
          }
        }
        
        dat_plot <- left_join(dat_plot, dat_reduced[, c('rows', 'cols', 'rowcol_factor', 'outside_al_init')],
                              by = c('rows', 'cols', 'rowcol_factor'))
        dat_plot <- left_join(dat_plot, meas_types, by = c('rowcol_factor' = 'meas_type'))
        dat_plot[which(dat_plot$nofit == 'yes'), 'fit_err'] <- NA
        
        dat_plot
        
      })
      
    })
    
    
    
  })
  
  
  
  ## ----------------------------------- ##
  ## data: action limit value (= actionlimit) ####
  ## ----------------------------------- ##
  
  
  actionlimit <- reactive({
    
    if(input$assay_template == 'User defined'){
      
      # get actionlimit from UI
      
      req(input$actionlimit)
      
      # change decimal separator comma to point
      # convert numeric
      input$actionlimit %>% 
        gsub(",", ".", .) %>% 
        as.numeric -> al
      
      # if no actionlimit, or any other NA, set to zero
      if(is.na(al)) al <- 0
      
      # if actionlimit < 0, set to 0 and update UI field
      if(al < 0){
        al <- 0
        updateTextInput(session, "actionlimit", value = "0")
      }
      
      # if actionlimit > 100, set to 100 and update UI field
      if(al > 100){
        al <- 100
        updateTextInput(session, "actionlimit", value = "100")
      }
      
    } else {
      
      # get actionlimit from template
      
      current_assay() %>% 
        pull(actionlimit) -> al
      
      # if actionlimit is NA or outside bounds, set to zero
      if(is.na(al) | al < 0 | al > 100) al <- 0
    }
    
    # return
    al
    
  })
  
  
  ## ----------------------------------- ##
  ## data: no fit plot height (= plot_height_nofit)####
  ## ----------------------------------- ##
  
  plot_height_nofit <- reactive({
    
    if(isolate(input$plate_layout) == "Columns") {
      
      # get number of groups from row info, keeping only rows to be excluded from fit
      dat <- dat_info_rows() %>% filter(rows %in% nofit_template())
      
      # remove empty rows from template
      if(!is.null(l_empty_template())){
        dat %>% 
          filter(! rows %in% l_empty_template()$rows) -> dat
      }
      
      # remove empty rows guessed from import
      lapply(list_imported()[["list_empty"]], "[[", "rows") %>% 
        unlist -> empty_rows_guessed
      
      dat %>% 
        filter(! rows %in% empty_rows_guessed) -> dat
      
    } else {
      
      # get number of groups from row info, keeping only rows to be excluded from fit
      dat <- dat_info_cols() %>% filter(cols %in% nofit_template())
      
      # remove empty cols from template
      if(!is.null(l_empty_template())){
        dat %>% 
          filter(! cols %in% l_empty_template()$cols) -> dat
      }
      
      # remove empty cols guessed from imported file
      lapply(list_imported()[["list_empty"]], "[[", "cols") %>% 
        unlist -> empty_cols_guessed
      
      dat %>% 
        filter(! cols %in% empty_cols_guessed) -> dat
      
    }
    
    # get number of groups
    n_groups <- length(unique(dat$rowcol_factor))
    
    # since plot has panels (two columns),
    # make plot height 400px for 2 groups, 800px for 3 and 4 groups, ...
    400 * ceiling(n_groups / 2)
    
  })
  
  
  
  ## ----------------------------------- ##
  ## data: fit plot height (= plot_height_fit)####
  ## ----------------------------------- ##
  
  plot_height_fit <- reactive({
    
    if(isolate(input$plate_layout) == "Columns") {
      
      # get number of groups from row info, discarding rows to be excluded from fit
      dat <- dat_info_rows() %>% filter(!(rows %in% nofit_template()))
      
      # remove empty rows from template
      if(!is.null(l_empty_template())){
        dat %>% 
          filter(! rows %in% l_empty_template()$rows) -> dat
      }
      
      # remove empty rows guessed from import
      lapply(list_imported()[["list_empty"]], "[[", "rows") %>% 
        unlist -> empty_rows_guessed
      
      dat %>% 
        filter(! rows %in% empty_rows_guessed) -> dat
      
    } else {
      
      # get number of groups from col info, discarding cols to be excluded from fit
      dat <- dat_info_cols() %>% filter(!(cols %in% nofit_template()))
      
      # remove empty cols from template
      if(!is.null(l_empty_template())){
        dat %>% 
          filter(! cols %in% l_empty_template()$cols) -> dat
      }
      
      # remove empty cols guessed from imported file
      lapply(list_imported()[["list_empty"]], "[[", "cols") %>% 
        unlist -> empty_cols_guessed
      
      dat %>% 
        filter(! cols %in% empty_cols_guessed) -> dat
      
    }
    
    # get number of groups
    n_groups <- length(unique(dat$rowcol_factor))
    
    # since plot has panels (two columns),
    # make plot height 400px for 2 groups, 800px for 3 and 4 groups, ...
    400 * ceiling(n_groups / 2)
    
  })
  
  
  
  ## ----------------------------------- ##
  ## data: p-value Rosner test (= pval_rosner) ####
  ## ----------------------------------- ##
  
  pval_rosner <- reactive({
    if(input$assay_template == 'User defined'){
      # get p-value from input
      as.numeric(input$pval_rosner)
    } else {
      # get p-values from template
      current_assay() %>% 
        pull(pval_rosner)
    }
  })
  
  
  
  ## ----------------------------------- ##
  ## data: p-value ROUT test (= pval_rout) ####
  ## ----------------------------------- ##
  
  pval_rout <- reactive({
    if(input$assay_template == 'User defined'){
      # get p-values from input
      as.numeric(input$pval_rout)
    } else {
      # get p-value from template
      current_assay() %>% 
        pull(pval_rout)
    }
  })
  
  
  
  
  ## ----------------------------------- ##
  ## data: Rosner outlier test (= l_rosner) ####
  ## ----------------------------------- ##
  
  l_rosner <- reactive({
    
    # run only after button pressed
    if(is.null(show_results$data)) return() 
    
    # remove dependency from other changes in inputs
    isolate({
      
      # try and catch errors
      try({
        # apply for all plates
        lapply(1:list_imported()$n_plates, function(i){
          
          # get pre-processed data
          dat_plot <- l_data()[[i]]
          
          # add id of observations
          dat_plot <- mutate(dat_plot, id_rosner = 1:n())
          
          # filter out masked values and values to be excluded from fit (including those where model cannot be fit)
          # group by row/col groups
          # add logical outside actionlimit
          # apply outlier detection function (f_rosner)
          try_roesfit <- try(dat_plot %>% 
                          filter(masked == "no") %>%
                          filter(nofit == "no" & fit_err != 'yes') %>%
                          group_by(rowcol_factor) %>%
                          do(f_rosner(xx = .$log10_conc, yy = .$value, id = .$id_rosner, 
                                      pval = pval_rosner(), outside_actionlimit = .$outside_al_init)) -> dat_rosner, silent = TRUE)
          
          if(inherits(try_roesfit, "try-error")){
            
            dat_rosner <- data.frame(rowcol_factor = dat_plot$rowcol_factor, 
                                     xx = dat_plot$log10_conc, 
                                     yy = dat_plot$value, 
                                     id = dat_plot$id_rosner, 
                                     yy_orig = dat_plot$value, 
                                     outside_actionlimit = dat_plot$outside_al_init, 
                                     test_out = 0)
            
            dat_rosner <-  dat_rosner[ with(dat_rosner, order(rowcol_factor, id)),]
            
          } else { try_roesfit }
          
          
          # rejoin to original data
          dat_rosner %>% 
            rename(id_rosner = id) %>% 
            right_join(dat_plot, by = c("rowcol_factor", "id_rosner")) -> dat_rosner
          
          # get values outside action limit and format into separate tbl
          dat_rosner %>%
            filter(outside_actionlimit == T)  %>%
            arrange(rows, col_n) %>% 
            summarise(row_col = paste0(paste0(rows, col_n), collapse = ", ")) -> tab_outside_al
          
          # get outliers and format into separate tbl
          dat_rosner %>%
            filter(test_out == 1) %>%
            arrange(rows, col_n) %>% 
            summarise(row_col = paste0(paste0(rows, col_n), collapse = ", ")) -> tab_removed
          
          # get number of points outside action limit
          dat_rosner %>% 
            filter(masked == "no",
                   nofit == "no",
                   fit_err != "yes",
                   !is.na(log10_conc),
                   !is.na(value)) %>% 
            pull(outside_actionlimit) %>% 
            sum -> sum_oal
          
          # get number of total outliers
          dat_rosner %>% 
            filter(masked == "no",
                   nofit == "no",
                   fit_err != "yes",
                   !is.na(log10_conc),
                   !is.na(value)) %>% 
            pull(test_out) %>% 
            sum -> sum_out
          
          
          # return data
          list(any_outside_al = sum_oal > 0,
               any_removed = sum_out > 0,
               tab_outside_al = tab_outside_al,
               tab_removed = tab_removed,
               dat_rosner = dat_rosner)
          
        })
        
      })
      
    })
    
    
  })
  
  
  
  
  ## ----------------------------------- ##
  ## data: ROUT outlier test (= l_rout) ####
  ## ----------------------------------- ##
  
  l_rout <- reactive({
    
    # run only after button pressed
    if(is.null(show_results$data)) return() 
    
    # remove dependency from other changes in inputs
    isolate({
      
      # try and catch errors
      try({
        # apply for all plates
        lapply(1:list_imported()$n_plates, function(i){
          
          # get pre-processed data
          dat_plot <- l_data()[[i]]
          
          # add id of observations
          dat_plot <- mutate(dat_plot, id_rout = 1:n())
          
          # filter out masked values and values to be excluded from fit (including those where model cannot be fit)
          # group by row/col groups
          # add logical outside actionlimit
          # apply outlier detection function (f_rout)
          try_routfit <- try(dat_plot %>% 
                               filter(masked == "no") %>% 
                               filter(nofit == "no" & fit_err != 'yes') %>%
                               group_by(rowcol_factor) %>%
                               do(f_rout(xx = .$log10_conc, yy = .$value, id = .$id_rout, 
                                         pval = pval_rout(), outside_actionlimit = .$outside_al_init)) -> dat_rout, silent = T)
          
          if(inherits(try_routfit, "try-error")){
            
            dat_rout <- data.frame(rowcol_factor = dat_plot$rowcol_factor, 
                                   xx = dat_plot$log10_conc, 
                                   yy = dat_plot$value, 
                                   id = dat_plot$id_rout, 
                                   yy_orig = dat_plot$value, 
                                   outside_actionlimit = dat_plot$outside_al_init, 
                                   test_out = 0)
            
          } else { try_routfit }
          
          
          # rejoin to original data
          dat_rout %>% 
            rename(id_rout = id) %>% 
            right_join(dat_plot, by = c("rowcol_factor", "id_rout")) -> dat_rout
          
          # get values outside action limit and format into separate tbl
          dat_rout %>%
            filter(outside_actionlimit == T) %>%
            arrange(rows, col_n) %>% 
            summarise(row_col = paste0(paste0(rows, col_n), collapse = ", ")) -> tab_outside_al
          
          # get outliers and format into separate tbl
          dat_rout %>%
            filter(test_out == 1)  %>%
            arrange(rows, col_n) %>% 
            summarise(row_col = paste0(paste0(rows, col_n), collapse = ", ")) -> tab_removed
          
          # get number of points outside action limit
          dat_rout %>% 
            filter(masked == "no",
                   nofit == "no",
                   fit_err != "yes",
                   !is.na(log10_conc),
                   !is.na(value)) %>% 
            pull(outside_actionlimit) %>% 
            sum -> sum_oal
          
          # get number of total outliers
          dat_rout %>% 
            filter(masked == "no",
                   nofit == "no",
                   fit_err != "yes",
                   !is.na(log10_conc),
                   !is.na(value)) %>% 
            pull(test_out) %>% 
            sum -> sum_out
          
          
          # return data
          list(any_outside_al = sum_oal > 0,
               any_removed = sum_out > 0,
               tab_outside_al = tab_outside_al,
               tab_removed = tab_removed,
               dat_rout = dat_rout)
          
        })
        
      })
      
    })
    
  })
  
  ## ----------------------------------- ##
  ## data: overall outlier summary (= dat_summary) ####
  ## ----------------------------------- ##
  
  dat_summary <- reactive({

    # run only after button pressed
    if(is.null(show_results$data)) return()

    # remove dependency from other changes in inputs
    isolate({

      # initialize empty results dataframe
      d <- tibble(plate_no = integer(0),
                  rowcol_factor = character(0),
                  outside_actionlimit = character(0),
                  outlier = character(0))
      
      # collate cases identified as outside action limit and as outliers according to official
      # test across all plates
      if(input$official_test == "Rosner"){
        
        for(i in 1:list_imported()[["n_plates"]]) {
          
          this_outside_al <- if(ncol(l_rosner()[[i]]$tab_outside_al) == 1) {
            # catch problem case (e.g. due to fitting failure)
            tibble(plate_no = i, rowcol_factor = NA, outside_actionlimit = NA)
          } else {
            l_rosner()[[i]]$tab_outside_al %>% 
            transmute(plate_no = i,
                      rowcol_factor = rowcol_factor,
                      outside_actionlimit = row_col)
          }
          
          this_removed <- if(ncol(l_rosner()[[i]]$tab_removed) == 1) {
            # catch problem case (e.g. due to fitting failure)
            tibble(plate_no = i, rowcol_factor = NA, outlier = NA)
          } else {
            l_rosner()[[i]]$tab_removed %>% 
              transmute(plate_no = i,
                        rowcol_factor = rowcol_factor,
                        outlier = row_col)
          }
          
          this_summary <-full_join(this_outside_al, this_removed, by = c('plate_no', 'rowcol_factor'))
          
          d <- rbind(d, this_summary)
          
        }
        
      } else {
        
        for(i in 1:list_imported()[["n_plates"]]) {
          
          this_outside_al <- if(ncol(l_rout()[[i]]$tab_outside_al) == 1) {
            # catch problem case (e.g. due to fitting failure)
            tibble(plate_no = i, rowcol_factor = NA, outside_actionlimit = NA)
          } else {
            l_rout()[[i]]$tab_outside_al %>% 
              transmute(plate_no = i,
                        rowcol_factor = rowcol_factor,
                        outside_actionlimit = row_col)
          }
          
          this_removed <- if(ncol(l_rout()[[i]]$tab_removed) == 1) {
            # catch problem case (e.g. due to fitting failure)
            tibble(plate_no = i, rowcol_factor = NA, outlier = NA)
          } else {
            l_rout()[[i]]$tab_removed %>% 
              transmute(plate_no = i,
                        rowcol_factor = rowcol_factor,
                        outlier = row_col)
          }
          
          this_summary <-full_join(this_outside_al, this_removed, by = c('plate_no', 'rowcol_factor'))
          
          d <- rbind(d, this_summary)
          
        }
        
      }
      
      # clean up and return results
      d[which(is.na(d$outside_actionlimit)), 'outside_actionlimit'] <- ''
      d[which(is.na(d$outlier)), 'outlier'] <- ''
      d <- d %>% filter(!is.na(rowcol_factor))
      
      d
      
    })

  })

  
  
  ## ----------------------------------- ##
  ## output: info - imported file and trial  ####
  ## ----------------------------------- ##
  
  observe({
    
    # show info only if file uploaded
    req(input$file1)
    
    
    # trial info
    output$imported_info1 <- renderText({
      
      paste0(list_imported()[["info"]], "\n")
      
    })
    
    # some file information + info on selected plates
    output$imported_info2 <- renderUI({
      
      l <- list(renderText(paste0("Filename: ", input$file1$name)),
                renderText(paste0("Exported from: ", list_imported()[["read_from"]])),
                renderText(paste0("Measurement dates: ", 
                                  paste0(unique(list_imported()[["measurement_dates"]]), collapse = " / " ))),
                renderText(paste0("Total number of plates in file: ", list_imported()[["n_plates"]])),
                renderText(paste0("Selected plate numbers: ", paste0(sort(as.numeric(input$selected_plates)), collapse = ', ')))
      )
      
      do.call(tagList, l)
      
    })
    
  })
  
  
  ## ----------------------------------- ##
  ## output: info - assay template, rows, cols, etc.  ####
  ## ----------------------------------- ##
  
  
  output$ui_assay_template <- renderUI({
    
    # info on assay template (chosen or specified)
    l <- list(renderText(paste0("Template chosen: ", input$assay_template)),
              
              renderTable(dat_info_rows() %>% select(-row_n), 
                          spacing = "xs", caption = "Row information", caption.placement = "top",
                          digits = 6),
              renderTable(dat_info_cols() %>% select(-col_n), 
                          spacing = "xs", caption = "Column information", caption.placement = "top",
                          digits = 6),
              
              renderText(paste0("Empty rows (as defined in template): ", 
                                if(is.null(l_empty_template()$rows)) "None" else paste(l_empty_template()$rows, collapse = ", "))),
              renderText(paste0("Empty columns (as defined in template): ",
                                if(is.null(l_empty_template()$cols)) "None" else paste(l_empty_template()$cols, collapse = ", "))),
              renderText(paste0(if(input$plate_layout == "Columns") "Rows " else "Columns ",
                                "excluded from fitting (as defined in template): ", 
                                if(is.null(nofit_template())) "None" else paste(nofit_template(), collapse = ", "))),
              
              renderText("Empty rows/columns (as identified from data):"),
              renderTable(spacing = "xs", 
                          {
                            l_empty <- list_imported()$list_empty
                            names(l_empty) <- paste0("Plate ", 1 : list_imported()$n_plates)
                            lapply(l_empty, function(l) tibble(rows = paste(l$rows, collapse = ", "), 
                                                               columns = paste(l$cols, collapse = ", "))) %>% 
                              bind_rows(.id = "Plate") -> dat
                            if(any(c(dat$rows, dat$columns) != "")) dat else tibble(None = character(0))
                          }),
              
              renderText("Masked wells (as identified from data):"),
              renderTable(spacing = "xs", 
                          {
                            l_masked <- list_imported()$list_masked
                            names(l_masked) <- paste0("Plate ", 1 : list_imported()$n_plates)
                            dat <- bind_rows(l_masked, .id = "Plate")
                            if(nrow(dat)) dat else tibble(None = character(0))
                          }),
              
              renderText("Masked wells (manual):"),
              renderTable(spacing = "xs", 
                          {
                            req(l_masked_wells_manual())
                            l_masked <- l_masked_wells_manual()
                            names(l_masked) <- paste0("Plate ", 1 : list_imported()$n_plates)
                            dat <- bind_rows(l_masked, .id = "Plate")
                            if(nrow(dat)) dat else tibble(None = character(0))
                            
                          }),
              
              renderText(paste0("Outlier test Rosner, p-value: ", pval_rosner())),
              if(dev_mode) renderText(paste0("Outlier test ROUT, p-value: ", pval_rout())),
              renderText(paste0("Action limit (in % of the difference between upper and lower asymptote): ", actionlimit(), "%")),
              
              renderText(paste0("Fit mode: ", fit_mode())),

              renderText({
                if(!is.null(meas_mult()) && meas_mult() != 1) {
                  paste0("NOTE: Measurements as read from file have been scaled by the factor ", meas_mult(),
                         " prior to plotting and fitting.")
                } else {
                  NULL
                }
              })
    )
    
    do.call(tagList, l)
    
  })
  
  
  ## ----------------------------------- ##
  ## output: results - outlier summary  ####
  ## ----------------------------------- ##

  output$outlier_summary <- renderUI({

    # display only if button pressed
    if(is.null(show_results$data)) return()

    # remove dependency from other changes in inputs
    isolate({

      # check whether outlier tests failed (try-error)
      if(dev_mode) {
        validate(
          need(l_rosner(), "Rosner test failed. Check if plate layout and assay is correctly specified."),
          need(l_rout(), "ROUT test failed. Check if plate layout and assay is correctly specified.")
        )
      } else {
        validate(
          need(l_rosner(), "Rosner test failed. Check if plate layout and assay is correctly specified.")
        )
      }

      # restrict summary to measurement types within selected plates where
      # outliers have been identified, discard action limit info, and print
      summary_selected <- dat_summary() %>%
        filter(plate_no %in% as.numeric(input$selected_plates) &
                 outlier != "") %>%
        select(-outside_actionlimit)
      if(nrow(summary_selected) == 0) summary_selected <- tibble(None = character(0))

      l <- list(h4(paste0("Summary of outliers according to ", input$official_test, " test")))
      l <- c(l,list(renderTable(summary_selected, spacing = "xs", width = '100%', align = 'l')))
      

      do.call(tagList, l)
      
    })

  })
  
  ## ----------------------------------- ##
  ## output: results - raw data, fits, outlier tests, etc.  ####
  ## ----------------------------------- ##  
  
  # put all output in one element
  output$imported_plates <- renderUI({
    
    # display only if button pressed
    if(is.null(show_results$data)) return() 
    
    # remove dependency from other changes in inputs
    isolate({
      
      # check whether outlier tests failed (try-error)
      if(dev_mode) {
        validate(
          need(l_rosner(), "Rosner test failed. Check if plate layout and assay is correctly specified."),
          need(l_rout(), "ROUT test failed. Check if plate layout and assay is correctly specified.")
        )
      } else {
        validate(
          need(l_rosner(), "Rosner test failed. Check if plate layout and assay is correctly specified.")
        )
      }
      
      # official test labels
      if(input$official_test == "Rosner"){
        suffix_rosner <- " (official test)"
        suffix_rout <- " (for internal information only)"
      } else {
        suffix_rosner <- " (for internal information only)"
        suffix_rout <- " (official test)"
      }
      
      # apply over all selected plates
      l <- lapply(sort(as.numeric(input$selected_plates)), function(i) {
        
        # raw data plate overview and 4pl fit
        l_plate <- list(h2(paste0("Plate ", i)))
        
        l_plate <- if(!is.null(nofit_template())) {
          c(l_plate, list(
            withSpinner(plotOutput(paste0("imported_plate_", i)), type = 8),
            h4("Data not used in fitting"),
            withSpinner(plotOutput(paste0("imported_plot_nofit_", i), height = plot_height_nofit()), type = 8),
            h4("Original fit"),
            withSpinner(plotOutput(paste0("imported_plot_fit_", i), height = plot_height_fit()), type = 8),
            h4("Points outside action limit"),
            withSpinner(tableOutput(paste0("outside_al_", i)), type = 8)
          ))
        } else {
          c(l_plate, list(
            withSpinner(plotOutput(paste0("imported_plate_", i)), type = 8),
            h4("Original fit"),
            withSpinner(plotOutput(paste0("imported_plot_fit_", i), height = plot_height_fit()), type = 8),
            h4("Points outside action limit"),
            withSpinner(tableOutput(paste0("outside_al_", i)), type = 8)
          ))
        }
        
        # results Rosner test
        
        l_plate <- c(l_plate, list(h3(paste0("Rosner outlier test", suffix_rosner))))
        
        # if any points identified as outliers, produce outlier test output
        if(!is.na(l_rosner()[[i]]$any_outside_al) & l_rosner()[[i]]$any_outside_al == T) {
          
          # check whether any outliers removed by Rosner test
          if(l_rosner()[[i]][["any_removed"]]){
            # if yes, show points, plate, and fit without outliers
            l_plate <- c(l_plate, list(renderText("Points identified as outliers by Rosner test:"),
                                       tableOutput(paste0("rosner_tab_removed_", i)),
                                       h4(paste0("Plate ", i, ": overview with outliers detected by Rosner test")),
                                       withSpinner(plotOutput(paste0("rosner_plate_", i)), type = 8),
                                       h4("Fit with outliers removed by Rosner test"),
                                       withSpinner(plotOutput(paste0("rosner_plot_", i), height = plot_height_fit()), type = 8)))
            
          } else {
            # if no, display text
            l_plate <- c(l_plate, 
                         list(renderText(paste0("No points identified as outliers by Rosner test", suffix_rosner, "."))))
          }
          
        } else {
          # otherwise, display text
          l_plate <- c(l_plate, 
                       list(renderText(paste0("No points outside action limit. No outlier testing was performed."))))
        }
        
        # results ROUT test (only displayed in development mode)
        
        if(dev_mode) {
          
          l_plate <- c(l_plate, list(h3(paste0("ROUT outlier test", suffix_rout))))
          
          # if any points identified as outliers, produce outlier test output
          if(!is.na(l_rout()[[i]]$any_outside_al) & l_rout()[[i]]$any_outside_al == T) {
            
            # check whether any outliers removed by ROUT test
            if(l_rout()[[i]][["any_removed"]]){
              # if yes, show points, plate, and fit without outliers
              l_plate <- c(l_plate, list(renderText("Points identified as outliers by ROUT test:"),
                                         tableOutput(paste0("rout_tab_removed_", i)),
                                         h4(paste0("Plate ", i, ": overview with outliers detected by ROUT test")),
                                         withSpinner(plotOutput(paste0("rout_plate_", i)), type = 8),
                                         h4("Fit with outliers removed by ROUT test"),
                                         withSpinner(plotOutput(paste0("rout_plot_", i), height = plot_height_fit()), type = 8)))
              
            } else {
              # if no, display text
              l_plate <- c(l_plate, 
                           list(renderText(paste0("No points identified as outliers by ROUT test", suffix_rout, "."))))
            }
            
          } else {
            # otherwise, display text
            l_plate <- c(l_plate, 
                         list(renderText(paste0("No points outside action limit. No outlier testing was performed."))))
          }
          
        }
        
        # add some space
        l_plate <- c(l_plate, list(br(), br(), br()))
        
        # return all output elements of one plate
        l_plate
        
      })
      
      # return all plates
      do.call(tagList, unlist(l, recursive = F))
      
    })
    
    
  })
  
  
  
  
  # single plot and table generation... (only called when needed)
  # maximum number of plates (20) can be changed
  for(i in 1:nmax_plates){
    
    # needs local wrapping to work correctly
    local({
      
      # needed to work correctly
      my_i <- i
      
      ## ----------------------------------- ##
      ## output-element: plate - imported data (imported_plate_xx) ####
      ## ----------------------------------- ##
      
      output[[paste0("imported_plate_", my_i)]] <- renderPlot({
        
        dat_plot <- try(l_data()[[my_i]])
        
        if(is.null(dat_plot) | inherits(dat_plot, "try-error")) return()
        
        f_plot_plate(dat_plot)
        
      })
      
      ## ----------------------------------- ##
      ## output-element: plot - imported data, without fit (imported_plot_nofit_xx) ####
      ## ----------------------------------- ##
      
      output[[paste0("imported_plot_nofit_", my_i)]] <- renderPlot({
        
        dat_plot <- try(l_data()[[my_i]])
        
        if(is.null(nofit_template()) | is.null(dat_plot) | inherits(dat_plot, "try-error")) return()
        
        f_plot_nofit(dat_plot = dat_plot)
        
      })
      
      ## ----------------------------------- ##
      ## output-element: plot - imported data, with fit (imported_plot_fit_xx) ####
      ## ----------------------------------- ##
      
      output[[paste0("imported_plot_fit_", my_i)]] <- renderPlot({
        
        dat_plot <- try(l_data()[[my_i]])
        
        if(is.null(dat_plot) | inherits(dat_plot, "try-error")) return()
        
        f_plot_fit(dat_plot = dat_plot,
                   actionlimit = actionlimit(),
                   use_means = if(fit_mode() == 'Mean values') T else F)
        
      })
      
      
      ## ----------------------------------- ##
      ## output-element: table - points outside action limit (outside_al_xx) ####
      ## ----------------------------------- ##
      
      output[[paste0("outside_al_", my_i)]] <- renderTable({
        
        dat_outside_al <- try(dat_summary()[which(dat_summary()$plate_no == my_i),])
        
        if(is.null(dat_outside_al) | inherits(dat_outside_al, "try-error")) return()
        
        dat_outside_al <- try(dat_outside_al %>%
                                filter(outside_actionlimit != "") %>%
                                select(-plate_no, -outlier))
        
        if(nrow(dat_outside_al) == 0) dat_outside_al <- tibble(msg = 'None', dummycol = '')
        
        dat_outside_al
        
      }, 
      spacing = "xs", colnames = F)
      
      
      ## ----------------------------------- ##
      ## ----------------------------------- ##
      ## output-element: table - Rosner test removed values (rosner_tab_removed_xx) ####
      ## ----------------------------------- ##
      
      output[[paste0("rosner_tab_removed_", my_i)]] <- renderTable(l_rosner()[[my_i]][["tab_removed"]],
                                                                   spacing = "xs",
                                                                   colnames = F)
      
      ## ----------------------------------- ##
      ## output-element: plot - fit after Rosner test removed values (rosner_plot_xx) ####
      ## ----------------------------------- ##
      
      output[[paste0("rosner_plot_", my_i)]] <- renderPlot({
        
        dat_test <-  try(l_rosner()[[my_i]][["dat_rosner"]])
        
        if(is.null(dat_test) | inherits(dat_test, "try-error")) return()
        
        f_plot_fit_outlier_test(dat_test = dat_test,
                                actionlimit = actionlimit(),
                                use_means = if(fit_mode() == 'Mean values') T else F)
        
      })
      
      ## ----------------------------------- ##
      ## output-element: plot - plate, summary Rosner (rosner_plate_xx) ####
      ## ----------------------------------- ##
      
      output[[paste0("rosner_plate_", my_i)]] <- renderPlot({
        
        dat_test <- try(l_rosner()[[my_i]][["dat_rosner"]])
        
        if(is.null(dat_test) | inherits(dat_test, "try-error")) return()
        
        f_plot_plate_outlier(dat_test)
        
      })
      
      ## ----------------------------------- ##
      ## output-element: table - ROUT test removed values (rout_tab_removed_xx) ####
      ## ----------------------------------- ##
      
      output[[paste0("rout_tab_removed_", my_i)]] <- renderTable(l_rout()[[my_i]][["tab_removed"]],
                                                                 spacing = "xs",
                                                                 colnames = F)
      
      ## ----------------------------------- ##
      ## output-element: plot - fit after ROUT test removed values (rout_plot_xx) ####
      ## ----------------------------------- ##
      
      output[[paste0("rout_plot_", my_i)]] <- renderPlot({
        
        dat_test <- try(l_rout()[[my_i]][["dat_rout"]])
        
        if(is.null(dat_test) | inherits(dat_test, "try-error")) return()
        
        f_plot_fit_outlier_test(dat_test = dat_test,
                                actionlimit = actionlimit(),
                                use_means = if(fit_mode() == 'Mean values') T else F)
        
      })
      
      ## ----------------------------------- ##
      ## output-element: plot - plate, summary ROUT (rout_plate_xx) ####
      ## ----------------------------------- ##
      
      output[[paste0("rout_plate_", my_i)]] <- renderPlot({
        
        dat_test <- try(l_rout()[[my_i]][["dat_rout"]])
        
        if(is.null(dat_test) | inherits(dat_test, "try-error")) return()
        
        f_plot_plate_outlier(dat_test)
        
      })
      
    })
    
  }
  
  
  ## ----------------------------------- ##
  ## HTML: create downloadable report ####
  ## ----------------------------------- ##
  
  output$create_html.html <- downloadHandler(
    # specify filename (only works if not in RStudio browser)
    filename = function() paste0("outlier-detection ", format(Sys.time(), "%Y-%m-%d %Hh%Mm%Ss"), ".html"),
    # content of file
    content = function(file){
      
      # copy the Rmd file to a temporary directory, in case no write permissions
      tempReport <- file.path(tempdir(), "report.Rmd")
      
      # Rmd code passed directly to cat() so that no separate Rmd file is necessary
      # warning: when copy-pasting content of Rmd file, better to use notepad++ or similar, 
      #          since RStudio might change indentation, which is important for Rmd
      cat(file = tempReport,
'

---
title: "Optimal Tool for Tracking Outliers"
output: html_document
params:
  time: NA
  dev_mode: NA
  fit_mode: NA
  version_info: NA
  session_info: NA
  funs: NA
  dat: NA
  input: NA
---

```{r global, echo=FALSE}
# knitr global options
knitr::opts_chunk$set(echo = FALSE, 
                      warning = FALSE,
                      message = FALSE)
```


```{r setup}
# required packages
library(magrittr)
library(dplyr)
library(tibble)
library(knitr)
library(kableExtra)
library(ggplot2)
```


```{r, echo=FALSE}
# show content of params (for debugging)
# params
```


# General information

Report generated on: `r params$time`

Using main app version/mode: `r params$version_info$script_version_mode`
<br>
Using setting file ID: `r params$version_info$setting_id`
<br>
Using functions file version: `r params$version_info$function_version`
<br>
Using assay template file ID: `r params$version_info$assay_template_id`
<br><br>

#### Trial

```{r, comment=""}
params$dat$list_imported$info %>% 
  paste0(collapse = "\n") %>% 
  cat

```

Filename: `r params$input$filename`    
Exported from: `r params$dat$list_imported$read_from`    
Measurement dates: `r paste0(unique(params$dat$list_imported$measurement_dates), collapse = " / ")`    
Total number of plates in file: `r params$dat$list_imported$n_plates`   
Selected plate numbers: `r paste0(sort(as.numeric(params$input$selected_plates)), collapse = ", ")`   

<br>

#### Assay template

Template chosen: `r params$input$assay_template`

```{r, results="asis"}


params$dat$dat_info_rows %>% 
  select(-row_n) %>% 
  kable(caption = "Row information", digits = 6) %>% 
  kable_styling(full_width = F, position = "left", bootstrap_options = "condensed")

cat("<p style=\'page-break-before: always; font-size:10px;padding-top: 1cm\'>")

params$dat$dat_info_cols %>% 
  select(-col_n) %>% 
  kable(caption = "Column information", digits = 6) %>% 
  kable_styling(full_width = F, position = "left", bootstrap_options = "condensed")


```


Empty rows (as defined in template): `r if(is.null(params$dat$l_empty_template$rows)) "None" else paste(params$dat$l_empty_template$rows, collapse = ", ")`     
Empty columns (as defined in template): `r if(is.null(params$dat$l_empty_template$cols)) "None" else paste(params$dat$l_empty_template$cols, collapse = ", ")`    

`r if(params$input$plate_layout == "Columns") "Rows" else "Columns"` excluded from fitting (as defined in template): `r if(is.null(params$dat$nofit_template)) "None" else paste(params$dat$nofit_template, collapse = ", ")`     

Empty rows/columns (as identified from data):

```{r, results="asis"}
l_empty <- params$dat$list_imported$list_empty
names(l_empty) <- paste0("Plate ", 1 : params$dat$list_imported$n_plates)

lapply(l_empty, function(l) tibble(rows = paste(l$rows, collapse = ", "), 
                                   columns = paste(l$cols, collapse = ", "))) %>% 
  bind_rows(.id = "Plate") -> dat
res <- if(any(c(dat$rows, dat$columns) != "")) dat else tibble(None = character(0))

res %>% 
  kable() %>% 
  kable_styling(full_width = F, position = "left", bootstrap_options = "condensed")

```


Masked wells (as identified from data):

```{r, results="asis"}
l_masked <- params$dat$list_imported$list_masked
names(l_masked) <- paste0("Plate ", 1 : params$dat$list_imported$n_plates)
dat <- bind_rows(l_masked, .id = "Plate")
res <- if(nrow(dat)) dat else tibble(None = character(0))

res %>% 
  kable() %>% 
  kable_styling(full_width = F, position = "left", bootstrap_options = "condensed")

```



Masked wells (manual):

```{r, results="asis"}
l_masked <- params$dat$l_masked_wells_manual
names(l_masked) <- paste0("Plate ", 1 : params$dat$list_imported$n_plates)
dat <- bind_rows(l_masked, .id = "Plate")
res <- if(nrow(dat)) dat else tibble(None = character(0))
                            

res %>% 
  kable() %>% 
  kable_styling(full_width = F, position = "left", bootstrap_options = "condensed")

```


Outlier test Rosner, p-value: `r params$dat$pval_rosner`    
`r if(params$dev_mode) paste0("Outlier test ROUT, p-value: ", params$dat$pval_rout)`    
Action limit (in % of the difference between upper and lower asymptote): `r params$dat$actionlimit`    
Fit mode: `r params$fit_mode`    

```{r, results="asis"}
if(!is.null(params$dat$meas_mult) && params$dat$meas_mult != 1) {
  cat(paste0("NOTE: Measurements as read from file have been scaled by the factor ", params$dat$meas_mult,
             " prior to plotting and fitting."))
}
```



<p style="page-break-before: always; font-size:10px;">

<br>

# Results

<br><br>

#### Summary of outliers according to `r params$input$official_test` test

```{r, results="asis"}

res <- params$dat$dat_summary %>%
  filter(plate_no %in% as.numeric(params$input$selected_plates) & outlier != "") %>% 
  select(-outside_actionlimit)
  
if(nrow(res) == 0) {

  res <- tibble(None = character(0))
  res %>%
    kable() %>% 
    kable_styling(full_width = F, position = "left", bootstrap_options = "condensed") %>% 
    print

} else {

  res %>%
    kable(align=rep("l", 3)) %>%
    kable_styling(full_width = F, position = "left", bootstrap_options = "condensed") %>%
    column_spec(1, width = "10%") %>%
    column_spec(2, width = "40%") %>%
    column_spec(3, width = "50%") %>% 
    print

}

```

```{r, results="asis"}

if(params$input$official_test == "Rosner"){
  suffix_rosner <- " (official test)"
  suffix_rout <- " (for internal information only)"
} else {
  suffix_rosner <- " (for internal information only)"
  suffix_rout <- " (official test)"
}


for(i in sort(as.numeric(params$input$selected_plates))){
  
  dat_plot <- params$dat$l_data[[i]]
  
  cat("<p style=\'page-break-before: always; font-size:10px;padding-top: 1cm\'>")
  
  cat("## Plate", i, "\n\n")
  
  plate_i <- params$funs$f_plot_plate(dat_plot)
  subchunk_plot(plate_i, paste0("subchunk_", i, "_", 1), 5, 12)

  if(!is.null(params$dat$nofit_template)) {

    cat("\n\n#### Data not used in fitting \n\n")

    nofit_i <- params$funs$f_plot_nofit(dat_plot = dat_plot)
    subchunk_plot(nofit_i, paste0("subchunk_", i, "_", 2), params$dat$plot_height_nofit/400*4, 12)

  }

  cat("\n\n#### Original fit \n\n")
  
  fit_i <- params$funs$f_plot_fit(dat_plot = dat_plot,
                                  actionlimit = params$dat$actionlimit,
                                  use_means = if(params$fit_mode == "Mean values") T else F)
  subchunk_plot(fit_i, paste0("subchunk_", i, "_", 3), params$dat$plot_height_fit/400*4, 12)
  
  cat("\n\n#### Points outside action limit \n\n")
  
  res <- params$dat$dat_summary %>%
    filter(plate_no == i & outside_actionlimit != "") %>% 
    select(-plate_no, -outlier)
  if(nrow(res) == 0) res <- tibble(None = character(0))
  
  res %>%
    kable() %>% 
    kable_styling(full_width = F, position = "left", bootstrap_options = "condensed") %>% 
    print
  
  cat("\n\n### Rosner outlier test", suffix_rosner, "\n\n")
  
  if(!is.na(params$dat$l_rosner[[i]]$any_outside_al) & params$dat$l_rosner[[i]]$any_outside_al == T) {
    
    if(params$dat$l_rosner[[i]][["any_removed"]]) {
      
      cat("\n\nPoints identified as outliers by Rosner test:\n\n")
  
      params$dat$l_rosner[[i]][["tab_removed"]] %>% 
        kable() %>% 
        kable_styling(full_width = F, position = "left", bootstrap_options = "condensed") %>% print
      
      cat("\n\n#### Plate ", i, ": overview with outliers detected by Rosner test\n\n")
      
      plate_roes_i <- params$funs$f_plot_plate_outlier(params$dat$l_rosner[[i]][["dat_rosner"]])
      subchunk_plot(plate_roes_i, paste0("subchunk_", i, "_", 4), 5, 12)
        
      cat("\n\n#### Fit with outliers removed by Rosner test\n\n")
      
      fit_roes_i <- params$funs$f_plot_fit_outlier_test(
        dat_test = params$dat$l_rosner[[i]][["dat_rosner"]],
        actionlimit = params$dat$actionlimit,
        use_means = if(params$fit_mode == "Mean values") T else F
      )
      subchunk_plot(fit_roes_i, paste0("subchunk_", i, "_", 5), params$dat$plot_height_fit/400*4, 12)
      
    } else {
      
      cat("\n\nNo points identified as outliers by Rosner test", suffix_rosner, ".\n\n")
      
    }
    
  } else {
  
    cat("\n\nNo points outside action limit. No outlier testing was performed.\n\n")
    
  }
  
  
  
  if(params$dev_mode) {
  
    cat("\n\n### ROUT outlier test", suffix_rout, "\n\n")
    
    if(!is.na(params$dat$l_rout[[i]]$any_outside_al) & params$dat$l_rout[[i]]$any_outside_al == T) {
    
      if(params$dat$l_rout[[i]][["any_removed"]]) {
        
        cat("\n\nPoints identified as outliers by ROUT test:\n\n")
    
        params$dat$l_rout[[i]][["tab_removed"]] %>% 
          kable() %>% 
          kable_styling(full_width = F, position = "left", bootstrap_options = "condensed") %>% print
        
        cat("\n\n#### Plate ", i, ": overview with outliers detected by ROUT test\n\n")
        
        plate_rout_i <- params$funs$f_plot_plate_outlier(params$dat$l_rout[[i]][["dat_rout"]])
        subchunk_plot(plate_rout_i, paste0("subchunk_", i, "_", 6), 5, 12)
          
        cat("\n\n#### Fit with outliers removed by ROUT test\n\n")
        
        fit_rout_i <- params$funs$f_plot_fit_outlier_test(
          dat_test = params$dat$l_rout[[i]][["dat_rout"]],
          actionlimit = params$dat$actionlimit,
          use_means = if(params$fit_mode == "Mean values") T else F
        )
        subchunk_plot(fit_rout_i, paste0("subchunk_", i, "_", 7), params$dat$plot_height_fit/400*4, 12)
        
      } else {
        
        cat("\n\nNo points identified as outliers by ROUT test", suffix_rout, ".\n\n")
        
      }
    
    } else {
    
      cat("\n\nNo points outside action limit. No outlier testing was performed.\n\n")
    
    }

  }
  
}


```

<p style="page-break-before: always; font-size:10px;">

<br>

# Session information

<p>

<code>`r params$session_info`</code>

<br><br>

```{r, results="asis"}

tibble(Analyst = c("Date", "Signature"),
       Reviewer = c("Date", "Signature")) %>% 
  kable() %>% 
  kable_styling(bootstrap_options = "bordered", font_size = 20)


```


')
   
      # Set up parameters to pass to Rmd document
      params <- list(time = format(Sys.time()),
                     
                     dev_mode = dev_mode,
                     
                     fit_mode = fit_mode(),
                     
                     version_info = list(script_version_mode = script_version_mode,
                                         setting_id = setting_id,
                                         function_version = function_version,
                                         assay_template_id = assay_template_id),
                     
                     session_info = session_info_str(linebreak_tag = T),
                     
                     funs = list(f_plot_plate = f_plot_plate,
                                 f_plot_nofit = f_plot_nofit,
                                 f_plot_fit = f_plot_fit,
                                 f_plot_fit_outlier_test = f_plot_fit_outlier_test,
                                 f_plot_plate_outlier = f_plot_plate_outlier,
                                 subchunk_plot = subchunk_plot),
                     
                     dat = list(list_imported = list_imported(),
                                l_empty_template = l_empty_template(),
                                nofit_template = nofit_template(),
                                l_masked_wells_manual = l_masked_wells_manual(),
                                l_data = l_data(),
                                dat_info_rows = dat_info_rows(),
                                dat_info_cols = dat_info_cols(),
                                meas_mult = meas_mult(),
                                pval_rosner = pval_rosner(),
                                pval_rout = if(dev_mode) pval_rout() else NULL,
                                actionlimit = actionlimit(),
                                l_rosner = l_rosner(),
                                l_rout = if(dev_mode) l_rout() else NULL,
                                plot_height_nofit = plot_height_nofit(),
                                plot_height_fit = plot_height_fit(),
                                dat_summary = dat_summary()),
                     
                     input = list(plate_layout = input$plate_layout,
                                  filename = input$file1$name,
                                  assay_template = input$assay_template,
                                  official_test = input$official_test,
                                  selected_plates = input$selected_plates))
      
      # add progress notification
      withProgress(message = "Creating HTML...", {
        
        # Knit the document, passing in the `params` list, and eval it in a
        # child of the global environment (this isolates the code in the document
        # from the code in this app).
        rmarkdown::render(tempReport, 
                          output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv()))
        
        # show notification when finished
        showNotification("Done creating HTML.")
        
      })
      
    }
  )
  
}


# Run the application 
shinyApp(ui = ui, server = server)


## EOF ####
