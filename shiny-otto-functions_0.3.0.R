######################################################################################################
#  Staburo GmbH
#================================================================================;
# Program name      : shiny-otto-functions_0.3.0.R
#
# Author            : Hans Bauer
#
# Date created      : 06NOV2019
#
# Study             : 04_Stat_outlier
#
# Purpose           : Optimal Tool for Tracking Outliers (OTTO) static functions
#
# Template          :
#
# Inputs            : -
#
# Outputs           : -
#
# Program completed : Yes
#
# Updated by        : See accompanying changelog for version history
#                     
#######################################################################################################;

## ----------------------------------- ##
## Notes ####
## ----------------------------------- ##

# - This file contains static (i.e. non-reactive) functions used in the context of 4PL curve fitting
#   and outlier detection (including miscellaneous utility functions)
# - Only the current version of the functions file should be stored in the main app directory (where
#   the main app file is located), otherwise unexpected results may occur. Move all other functions files
#   (e.g. older versions) to another directory, e.g. a subdirectory of the main directory
# - This version's direct predecessor is functions file v0.2.0


## ----------------------------------- ##
## Functions: Read in ELISA and SMP  ####
## ----------------------------------- ##

## f_read: check if file from ELISA or SMP and dispatch

f_read <- function(fn) {
  
  line1 <- readLines(fn, n = 1, skipNul = T)
  if(grepl("BLOCKS", line1)) {
    read_smp(fn)
  } else {
    read_elisa(fn)
  }
  
}

## read_smp: read a file from SMP

read_smp <- function(fn) {
  
  # read all lines and remove empty lines
  lines_all <- readLines(fn, skipNul = T)
  lines_all <- lines_all[lines_all != ""]
  
  # info in first lines?
  if(startsWith(lines_all[2], "Note")) {
    i_end <- which(startsWith(lines_all, "~End"))
    info <- lines_all[3 : (i_end[1] - 1)]
  } else {
    info <- ""
  }
  
  # get line numbers of all plates (block with Plate)
  id_plate <- which(startsWith(lines_all, "Plate"))
  lines_all[id_plate] %>% strsplit("\t") %>% sapply("[", 2) -> id_plate_names
  
  
  # define empty containers
  list_plates <- list()
  list_empty <- list()
  list_masked <- list()
  
  
  # get values of all plates
  for(i in seq_along(id_plate)) {
    
    # first lines contain plate data
    lines_plate <- lines_all[c(id_plate[i] + 1:9)]
    
    # transform into tbl
    tibble(rows = LETTERS[1:8], all = lines_plate[-1]) %>% 
      separate(all, c("empty1", "temp", sprintf("col_%02d", 1:12), "empty2", "empty3"), sep = "\t") %>% 
      select(rows, starts_with("col_")) -> dat_plate
    
    
    # get empty rows/cols
    dat_plate %>% 
      gather("cols", "value", -rows) %>% 
      filter(value == "") -> dat_empty
    
    # check whether empty values are full rows or cols
    dat_empty %>% 
      group_by(rows) %>% 
      summarise(nn = n()) %>% 
      filter(nn == 12) %>% 
      pull(rows) -> empty_rows
    
    dat_empty %>% 
      group_by(cols) %>% 
      summarise(nn = n()) %>% 
      filter(nn == 8) %>% 
      pull(cols) -> empty_cols
    
    # save empty rows/cols
    list_empty[[i]] <- list(rows = empty_rows, cols = empty_cols)
    
    
    # plate values: remove empty wells, convert to numeric, and save
    dat_plate %>% 
      mutate_at(vars(-rows), function(x) {
        x[x == ""] <- NA
        as.numeric(x)
      }) -> list_plates[[i]]
    
    
    # get masked values (can be found inside group block)
    # first get all lines from plate to next plate
    if(i != length(id_plate)) {
      lines_plate <- lines_all[c(id_plate[i] : (id_plate[i+1] - 1))]
    } else {
      lines_plate <- lines_all[c(id_plate[i] : length(lines_all))]
    }
    
    # line numbers of group blocks
    id_plate_groups <- which(startsWith(lines_plate, "Group:"))
    
    if(length(id_plate_groups)) {
      
      # loop over all groups
      l_groups <- lapply(id_plate_groups, function(ii) {
        
        # select lines until end of group
        lines_grp_end <- lines_plate[ii : length(lines_plate)]
        ii_end <- which(startsWith(lines_grp_end, "Group Column"))
        lines_grp <- lines_grp_end[2 : (ii_end[1] - 1)]
        
        # column names of group
        lines_grp[1] %>% strsplit("\t") %>% .[[1]] -> lines_grp_cols
        tibble(all = lines_grp[-1]) %>% 
          separate(all, c(lines_grp_cols, "empty1"), sep = "\t", convert = T) -> dat_grp
        
        # column information
        ii_end2 <- which(startsWith(lines_grp_end, "Group Summaries"))
        
        # get column number with well values
        lines_grp_end[(ii_end[1] + 1) : (ii_end2[1] - 1)] %>% 
          strsplit("\t") %>% 
          sapply(function(x) any(x == "!WellValues")) %>% 
          which -> col_i_wellvalues
        
        # get masked values
        dat_grp %>% 
          select(Wells, value = !!col_i_wellvalues) %>% 
          filter(value == "Masked") %>% 
          mutate(rows = substr(Wells, 1, 1),
                 cols = sprintf("col_%02d", as.numeric(substr(Wells, 2, 4)))) %>% 
          select(rows, cols)
        
      })
      
      # save masked values of all groups
      list_masked[[i]] <- bind_rows(l_groups)
      
    } else {
      # No group information contained in file
      
      list_masked[[i]] <- tibble(rows = character(0), cols = character(0))
      
    }
    
  }
  
  
  
  # get additional info:
  # row/col factor, start concentration, dilution factor
  id_groups <- which(startsWith(lines_all, "Group:"))
  if(length(id_groups)) {
    
    # total number of unique groups (n groups for one plate)
    n_groups <- length(id_groups) / length(id_plate)
    
    # group labels
    lines_all[id_groups[1:n_groups]] %>% 
      gsub("Group: ", "", .) -> rowcol_factor_labels
    
    # apply over all groups
    l_groups <- lapply(id_groups[1:n_groups], function(i) {
      
      # get lines of group
      n_lines_i <- which(startsWith(lines_all[i : length(lines_plate)], "Group Column"))
      lines_i <- lines_all[i + 1:(n_lines_i[1] - 2)]
      
      # column names
      lines_i[1] %>% strsplit("\t") %>% .[[1]] -> lines_i_cols
      
      # convert to tbl
      tibble(all = lines_i[-1]) %>% 
        separate(all, c(lines_i_cols, "empty1"), sep = "\t", convert = T) -> dat_i
      
      # get rows of wells of group
      dat_i$Wells %>% substr(1,1) %>% unique %>% sort -> rows
      
      # get data with wells and concentrations
      dat_i %>% 
        select(Wells, starts_with("Konz")) %>% 
        na.omit() -> dat_i_conc
      
      # start concentration in first non-na well
      conc_start <- dat_i_conc[[2]][1]
      
      # column with start concentration
      dat_i_conc$Wells[1] %>% 
        substr(2, 4) %>% 
        as.numeric -> conc_start_col
      
      # dilution factor defined as first divided by second well
      # rounded to max 4 digits
      conc_dilution_factor <- round(dat_i_conc[[2]][1] / dat_i_conc[[2]][2], 4)
      
      # return data
      tibble(rows = rows, 
             conc_start = conc_start, 
             conc_start_col = conc_start_col,
             conc_dilution_factor = conc_dilution_factor)
      
    })
    
    names(l_groups) <- rowcol_factor_labels
    
    # put into format as assay template data
    l_groups %>% 
      bind_rows(.id = "rowcol_factor") %>% 
      mutate(rows = paste0("row_", rows)) %>% 
      spread(rows, rowcol_factor) -> dat_info
    
    smp_add_info <- dat_info
    
  } else {
    
    smp_add_info <- NULL
    
  }
  
  
  # return
  list(read_from = "SMP",
       info = info,
       n_plates = length(list_plates),
       measurement_dates = "",
       list_plates = list_plates,
       list_empty = list_empty,
       list_masked = list_masked,
       smp_add_info = smp_add_info)
  
}


## read_elisa: read a file from ELISA

read_elisa <- function(fn) {
  
  # read all lines and remove empty lines
  lines_all <- readLines(fn)
  lines_all <- lines_all[lines_all != ""]
  
  # line numbers of all plates
  id_line_plate <- which(startsWith(lines_all, "Platt"))
  
  # info in header (until first plate)
  info <- lines_all[1 : (id_line_plate[1] - 1)]
  
  # number of plates
  n_plates <- length(id_line_plate)
  
  # measurement dates in second line of plate
  lines_all[id_line_plate + 1] %>% 
    strsplit("datum: ") %>% 
    sapply(., "[", 2) -> measurement_dates
  
  
  # define empty containers
  list_plates <- list()
  list_empty <- list()
  list_masked <- list()
  
  
  # run over all plates
  for(i in 1 : n_plates) {
    
    # select all lines of current plate
    if(i != n_plates) {
      xx <- lines_all[id_line_plate[i] : (id_line_plate[i + 1] - 1)]
    } else {
      xx <- lines_all[id_line_plate[i] : length(lines_all)]
    }
    
    # convert to tbl
    tail(xx, 8) %>% 
      gsub(",", ".", .) %>%
      tibble(rows = LETTERS[1:8], all = .) %>% 
      separate(all, sprintf("col_%02d", 1:13), sep = ";") %>% 
      select(-col_13) -> dat_plate
    
    # get empty rows/cols
    dat_plate %>% 
      gather("cols", "value", -rows) %>% 
      filter(value == "leer") -> dat_empty
    
    # check whether empty values are full rows or cols
    dat_empty %>% 
      group_by(rows) %>% 
      summarise(nn = n()) %>% 
      filter(nn == 12) %>% 
      pull(rows) -> empty_rows
    
    dat_empty %>% 
      group_by(cols) %>% 
      summarise(nn = n()) %>% 
      filter(nn == 8) %>% 
      pull(cols) -> empty_cols
    
    # save empty rows/cols
    list_empty[[i]] <- list(rows = empty_rows, cols = empty_cols)
    
    # get masked values (marked with parantheses)
    dat_plate %>% 
      gather("cols", "value", -rows) %>% 
      filter(grepl("(", value, fixed = T)) -> list_masked[[i]]
    
    # plate values: remove empty, unmask, convert to numeric, and save
    dat_plate %>% 
      mutate_at(vars(-rows), function(x) {
        x[x == "leer"] <- NA
        x %>% 
          gsub("(", "", ., fixed = T) %>% 
          gsub(")", "", ., fixed = T) %>% 
          as.numeric
      }) -> list_plates[[i]]
    
  }
  
  
  # return
  list(read_from = "Elisa",
       info = info,
       n_plates = n_plates,
       measurement_dates = measurement_dates,
       list_plates = list_plates,
       list_empty = list_empty,
       list_masked = list_masked)
  
}



## ----------------------------------- ##
## Function: 4 parameter logistic function ####
## ----------------------------------- ##



## f_4pl: 4 par logistic function (LS)

f_4pl <- function(yy, xx, id = 1:length(yy), seed = master_seed) {
  
  # set seed to ensure that different runs of function on same data will
  # yield identical results
  set.seed(seed)
  
  # remove NA
  any_na <- is.na(yy) | is.na(xx)
  yy <- yy[!any_na]
  xx <- xx[!any_na]
  id <- id[!any_na]
  
  # starting values
  start0 <- try(getInitial(yy ~ SSfpl(xx, A, B, xmid, scal), data.frame(yy = yy, xx = xx)),
                silent = T)
  
  # randomize getting of starting values, if convergence problems in getInitial
  n_try <- 1
  while(inherits(start0, "try-error") & n_try <= 20) {
    # take random 80% subsample
    ii <- sample(1:length(xx), floor(length(xx) * 0.8))
    xx2 <- xx[ii]
    yy2 <- yy[ii]
    start0 <- try(getInitial(yy2 ~ SSfpl(xx2, A, B, xmid, scal), data.frame(yy2 = yy2, xx2 = xx2)),
                  silent = T)
    n_try <- n_try + 1
  }
  
  # fit model
  fm <- nlsLM(yy ~ A+(B-A)/(1+exp((xmid-xx)/scal)),
              start = start0,
              control = list(maxiter = 1000))
  
  
  # make smoothed version for better plotting
  cf <- coef(fm)
  xx_smooth <- seq(min(xx), max(xx), length.out = 50)
  if (inherits(start0, "try-error")) {
    
    print("model error")
    fit_smooth <- c(rep_len(0, length.out = length(xx_smooth)))
    
  } else {
    
    fit_smooth <- SSfpl(input = xx_smooth, A = cf["A"], B = cf["B"], xmid = cf["xmid"], scal = cf["scal"])
    
  }
  
  
  
  
  
  # return
  # fm: model
  # dat: tbl of original values, fit, resid
  # dat_smooth: smoothed data for nicer plotting of lines
  list(fm = fm,
       coef = coef(fm),
       dat = tibble(id = id,
                    xx = xx,
                    yy = yy,
                    fit = fitted(fm),
                    res = resid(fm)),
       dat_smooth = tibble(xx_smooth = xx_smooth,
                           fit_smooth = fit_smooth))
}



## ----------------------------------- ##
## Functions: 4 parameter logistic function with robust estimation ####
## ----------------------------------- ##


## ll_robust: likelihood of 4 par logistic with t-errors (df=1)
# (logsigma parametrization)
ll_robust <- function(resp, input, A, B, xmid, scal, logsigma2) {
  pred <- A+(B-A)/(1+exp((xmid-input)/scal))
  z <- (resp - pred)/sqrt(exp(logsigma2))
  ll <- -0.5*logsigma2 - log(1 + z*z)
  -sum(ll)
}


## f_4pl_rout: fit robust 4pl

f_4pl_rout <- function(xx, yy, id,
                       RSDR_fun,
                       seed = master_seed,
                       lambda_start = 0, lambda_ssize = 10,
                       ptol = sqrt(.Machine$double.eps),
                       maxiter_main = 1000, maxiter_sub = 1000, maxiter_init = 1000) {
  
  # Note: This function implements the robust fitting algorithm as described in
  # Motulsky & Brown (2006, abbreviated as "MB" in the following), section "Minimizing
  # the merit function". 4PL model is hardcoded into this function as function to be
  # fitted
  
  ##### Setup
  
  ## Define subroutines
  
  # Fitted values
  fitting_fun <- function(xx, A, B, xmid, scal) A+(B-A)/(1+exp((xmid-xx)/scal))
  yhat <- function(xx, params) do.call('fitting_fun', c(list(xx = xx), as.list(params)))
  
  # Matrix of partial derivatives of fitting function
  d_yhat <- deriv(expression(A+(B-A)/(1+exp((xmid-xx)/scal))), c('A', 'B', 'xmid', 'scal'), func = T)
  # Next statement is necessary because function created by deriv() always seems to be global environment
  environment(d_yhat) <- environment()
  
  # Set seed to ensure that different runs of function on same data will yield identical results
  set.seed(seed)
  
  ## Remove NA
  any_na <- is.na(yy) | is.na(xx)
  yy <- yy[!any_na]
  xx <- xx[!any_na]
  id <- id[!any_na]
  
  ##### Estimation
  
  # Set convergence indicator
  conv <- F
  
  ### Get initial parameter estimates
  # Based on ordinary Levenberg-Marquardt nonlinear least squares fitting
  
  ## Guess starting values
  
  # First guess
  start0 <- try(getInitial(yy ~ SSfpl(xx, A, B, xmid, scal), data.frame(yy = yy, xx = xx)),
                silent = T)
  
  # Randomize getting of starting values, if convergence problems in getInitial
  n_try <- 1
  while(inherits(start0, "try-error") & n_try <= 20) {
    # take random 80% subsample
    ii <- sample(1:length(xx), floor(length(xx) * 0.8))
    xx2 <- xx[ii]
    yy2 <- yy[ii]
    start0 <- try(getInitial(yy2 ~ SSfpl(xx2, A, B, xmid, scal), data.frame(yy2 = yy2, xx2 = xx2)),
                  silent = if(n_try == 20) F else T)
    n_try <- n_try + 1
  }
  
  ## Fit model
  fm <- nlsLM(yy ~ A+(B-A)/(1+exp((xmid-xx)/scal)),
              start = start0,
              control = list(maxiter = maxiter_init))
  
  ## Initialize data frame of parameter estimate iterates with first estimate 
  iter_res <- data.frame(as.list(coef(fm)))
  
  ### Begin iteration using modified Levenberg-Marquardt algorithm as described in MB
  for(i in 1:maxiter_main) {
    
    # Derive required quantities from current iterate
    par_i <- unlist(iter_res[i, ])
    yhat_i <- yhat(xx, par_i)
    resid_i <- yy - yhat_i
    RSDR_i <- RSDR_fun(resid_i, length(par_i))
    RR_i <- resid_i/RSDR_i
    
    # Get gradient vectors of fitting function at current iterate
    d_yhat_i <- attr(do.call('d_yhat', as.list(par_i)), 'gradient')
    
    # Calculate vector of first derivatives of Lorentzian merit function at current iterate (Eq. 15 of MB)
    d_LM_i <- (-2/RSDR_i^2) * apply(d_yhat_i, MARGIN = 2, function(col) sum((resid_i * col)/(1 + RR_i^2)))
    
    # Calculate modified error Jacobian matrix (Eq. 16 of MB, called Hessian there)
    dd_LM_i <- (2/RSDR_i^2) * apply(d_yhat_i, MARGIN = 2, function(col1) apply(d_yhat_i, MARGIN = 2, function(col2) sum((col1 * col2)/(1 + RR_i^2))))
    
    # Determine candidate iterate and compare Lorentzian merit based on RSDR of candidate iterate
    # Proceed to next iteration if fit is improved, otherwise discard candidate iterate, change lambda and repeat
    lambda <- lambda_start
    for(j in 1:maxiter_sub) {
      
      par_c <- par_i + solve(-dd_LM_i + lambda * diag(length(par_i)), d_LM_i)
      yhat_c <- yhat(xx, par_c)
      resid_c <- yy - yhat_c
      RSDR_c <- RSDR_fun(resid_c, length(par_i))
      RR_c <- resid_c/RSDR_c
      LM_c <- sum(log(1 + RR_c^2))
      RR_i_rescaled <- resid_i/RSDR_c
      LM_i_rescaled <- sum(log(1 + RR_i_rescaled^2))
      
      if(LM_c < LM_i_rescaled) {
        # Improvement of fit
        
        parchg <- sqrt(sum((par_c - par_i)^2))/sqrt(sum(par_i^2))
        
        if(parchg < ptol) {
          # Convergence criterion met
          
          conv <- T
          
          # Create and return output object
          # Does *not* have a <fm> element. Use element <coef> instead
          res <- list(
            coef = par_i,
            dat = tibble(id = id,
                         xx = xx,
                         yy = yy,
                         fit = yhat_i,
                         res = resid_i),
            dat_smooth = tibble(xx_smooth = seq(min(xx), max(xx), length.out = 50),
                                fit_smooth = yhat(xx_smooth, par_i))
          )
          
          return(res)
          
        } else {
          # Convergence criterion not met
          iter_res <- rbind(iter_res, data.frame(as.list(par_c)))
          break()
        }
      } else {
        # No improvement of fit --> increase lambda and get new candidate estimate
        lambda <- lambda + lambda_ssize
      }
      
    }
    
  }
  
  if(!conv) stop('ROUT_fit(): Algorithm did not converge!')
  
}



## f_4pl_rout_old: fit robust 4pl (old version using approximation to RSDR)
# Not used any more as of OTTO v0.3.0. Retained only for documentation/comparison purposes

f_4pl_rout_old <- function(yy, xx, id = 1:length(yy), seed = master_seed) {
  
  # set seed to ensure that different runs of function on same data will
  # yield identical results
  set.seed(seed)
  
  # remove NA
  any_na <- is.na(yy) | is.na(xx)
  yy <- yy[!any_na]
  xx <- xx[!any_na]
  id <- id[!any_na]
  
  # get initial values from LS fit
  start0 <- try(getInitial(yy ~ SSfpl(xx, A, B, xmid, scal), data.frame(yy = yy, xx = xx)),
                silent = T)
  
  # randomize getting of starting values, if convergence problems in getInitial
  n_try <- 1
  while(inherits(start0, "try-error") & n_try <= 20) {
    # take random 80% subsample
    ii <- sample(1:length(xx), floor(length(xx) * 0.8))
    xx2 <- xx[ii]
    yy2 <- yy[ii]
    start0 <- try(getInitial(yy2 ~ SSfpl(xx2, A, B, xmid, scal), data.frame(yy2 = yy2, xx2 = xx2)),
                  silent = if(n_try == 20) F else T)
    n_try <- n_try + 1
  }
  
  # add startpar logsigma2, adapt to input needed
  start0 <- as.list(c(start0, logsigma2 = 1))
  
  # fit model (try nlminb first)
  fm <- try({
    mle2(ll_robust, 
         start = start0, 
         data = list(resp = yy, input = xx),
         optimizer = "nlminb")
  })
  
  # if failed, use optim instead 
  # (converges more often, but sometimes not in the right solution)
  if(inherits(fm, "try-error")) {
    fm <- mle2(ll_robust, 
               start = start0, 
               data = list(resp = yy, input = xx),
               optimizer = "optim")
  }
  
  
  # get fitted values
  cf <- coef(fm)
  fit <- SSfpl(input = xx, A = cf["A"], B = cf["B"], xmid = cf["xmid"], scal = cf["scal"])
  
  # make smoothed version for better plotting
  xx_smooth <- seq(min(xx), max(xx), length.out = 50)
  fit_smooth <- SSfpl(input = xx_smooth, A = cf["A"], B = cf["B"], xmid = cf["xmid"], scal = cf["scal"])
  
  # return
  # fm: model
  # dat: tbl of original values, fit, resid
  # dat_smooth: smoothed data for nicer plotting of lines
  list(fm = fm,
       dat = tibble(id = id,
                    xx = xx,
                    yy = yy,
                    fit = fit,
                    res = yy-fit),
       dat_smooth = tibble(xx_smooth = xx_smooth,
                           fit_smooth = fit_smooth))
}




## ----------------------------------- ##
## Functions: Outlier detection procedures ####
## ----------------------------------- ##



## calc_rosner_crit: helper function, critical values for Rosner test

calc_rosner_crit <- function(i, N, alpha = 0.05) {
  
  tval <- qt(p = 1 - alpha / (2 * (N - i + 1)), 
             df = N - i - 1)
  
  (N - i) * tval / sqrt((N - i - 1 + tval*tval) * (N - i + 1))
}

## f_rosner: Rosner test with actionlimit

f_rosner <- function(xx, 
                     yy, 
                     id = 1:length(yy), 
                     rosner_n_max = 5, 
                     pval = 0.05, 
                     outside_actionlimit = rep(TRUE, length(yy))) {
  
  # initialize data
  
  dat_rosner <- data.table(xx = xx,
                           yy = yy,
                           id = id,
                           yy_orig = yy,
                           outside_actionlimit = outside_actionlimit,
                           rosner_i = 0,
                           rosner_r = 0,
                           rosner_crit = 0,
                           test_out = 0)
  
  # run for 1 until maximum number of outliers
  
  for(i in 1:rosner_n_max) {
    
    # subset to current data (yy is set NA for previous steps)
    dat_rosner_current <- dat_rosner[!is.na(yy),]
    
    # apply 4pl logisitic function
    fm <- f_4pl(yy = dat_rosner_current$yy, 
                xx = dat_rosner_current$xx,
                id = dat_rosner_current$id)
    
    # get resid and standardize
    dat_curr <- data.table(fm$dat)
    dat_curr[, res_std := (res - mean(res))/sd(res)]
    
    # identify maximum outlying residual
    # get test and critical values, set to NA for further processing
    dat_rosner[id == dat_curr[which.max(abs(res_std)), id], 
               ":="(rosner_i = i, 
                    rosner_r = max(abs(dat_curr$res_std)),
                    rosner_crit = calc_rosner_crit(i, nrow(dat_rosner), alpha = pval),
                    yy = NA)]
    
  }
  
  # get maximum number of outliers
  # which fulfil the necessary condition
  max_i <- dat_rosner[rosner_r > rosner_crit, 
                      if(length(rosner_i)) max(rosner_i) else 0]
  
  # update data with points identified as outliers
  dat_rosner[outside_actionlimit == TRUE & rosner_i != 0 & rosner_i <= max_i, 
             test_out := 1]
  
  # return
  dat_rosner
}

## f_rout: ROUT outlier test with actionlimit

f_rout <- function(xx, yy, id = 1:length(yy), 
                   rout_k = 4, pval = 0.01, max_prop_outlier = 0.3,
                   outside_actionlimit = rep(TRUE, length(yy))) {
  
  # Define function to compute robust standard deviation of residuals
  # Note: Using type 7 quantile of Hyndman & Fan (1996). This is also the type of quantile used by Motulsky & Brown (2006).
  # Actually it is also the default of quantile() function but stated here to make this explicit
  get_RSDR <- function(resid, npar) as.numeric(quantile(abs(resid), 0.6827, type = 7) * length(resid)/(length(resid) - npar))
  
  # fit robust 4pl logistic
  fm_rob <- f_4pl_rout(id = id, yy = yy, xx = xx, RSDR_fun = get_RSDR)
  
  # get fit data, and add columns needed for further processing
  dat_rout <- data.table(fm_rob$dat)
  dat_rout[, yy_orig := yy]
  # Add <outside_actionlimit> as column (throws error if <outside_actionlimit> contains some
  # NA values and some non-NA values. If <outside_actionlimit> == NA or consists exclusively
  # of NA values, the resulting column will contain only NA values)
  dat_rout[, outside_actionlimit := outside_actionlimit[!is.na(outside_actionlimit)]]
  dat_rout[, test_out := 0]
  dat_rout[, res_abs := abs(res)]
  setkey(dat_rout, res_abs)
  dat_rout[, i_res_abs := 1:.N]
  N <- nrow(dat_rout)
  
  # calculate the RSDR (robust SD of residuals)
  RSDR <- get_RSDR(dat_rout$res, rout_k)
  
  # calcualte standardized residuals (=t value)
  dat_rout[, res_abs_RSDR := res_abs / RSDR]
  
  # Observation-specific threshold value according to FDR
  # Note: <pval> here actually denotes a Q value in the sense of FDR
  dat_rout[, alpha_i := pval * (N - (i_res_abs - 1)) / N]
  
  # p value
  dat_rout[, pt := (1 - pt(res_abs_RSDR, N - rout_k))*2]
  
  # FDR procedure described in Motulsky & Brown (2006) section "Applying the FDR method to detecting outliers"
  # 1. Add indicator whether observation should be considered (default: only 30% largest absolute residuals)
  # 2. Set all observations as outliers who belong to the set of considered observations according to step 1 and
  #    whose p value is below corresponding threshold value
  # 3. Set all observations as outliers where a smaller absolute residual has been set as outlier according to step 2 
  dat_rout[, FDR_tested := i_res_abs > ceiling((1-max_prop_outlier)*N)]
  dat_rout[, FDR_outlier := FDR_tested & pt < alpha_i]
  dat_rout[cumsum(FDR_outlier) > 0, FDR_outlier := T]
  
  # Mark points as outliers which both satisfy action limit and test criterion
  dat_rout[outside_actionlimit & FDR_outlier, test_out := 1]
  
  # return
  dat_rout
  
}



## ----------------------------------- ##
## Functions: other helpers (shiny, plot routines, data handling, ...) ####
## ----------------------------------- ##

## session info string

session_info_str <- function(linebreak_tag = F) {
  
  linebreak_str <- if(linebreak_tag) '<br>' else '\r\n'
  
  head_str <- paste(
    c("R version", "Platform", "OS"),
    c(paste(R.version$major, R.version$minor, sep = '.'), sessionInfo()$platform, sessionInfo()$running),
    collapse = linebreak_str,
    sep = ': '
  )
  
  pkg_vers <- sapply(loadedNamespaces(), function(ns) as.character(packageVersion(ns)))
  pkg_vers <- pkg_vers[order(names(pkg_vers))]
  pkg_vers_str <- paste(
    names(pkg_vers),
    pkg_vers,
    collapse = linebreak_str,
    sep = ': '
  )
  
  paste0(head_str, linebreak_str, linebreak_str, 'Package versions:' , linebreak_str, pkg_vers_str)
  
}

## row fields, label and input side-by-side
f_row_label_input <- function(x, rowcol_factor_labels) {
  tags$div(
    class="form-group",
    tags$label(class = "col-sm-3 control-label", `for` = paste0("row_", x), x),
    column(width = 9, selectInput(paste0("row_", x), NULL, choices = rowcol_factor_labels))
  )
}

## columns fields, label and input side-by-side
f_col_label_input <- function(x, rowcol_factor_labels) {
  tags$div(
    class="form-group",
    tags$label(class = "col-sm-3 control-label", `for` = paste0("col_", x), paste0("col_", x)),
    column(width = 9, selectInput(paste0("col_", x), NULL, choices = rowcol_factor_labels))
  )
}

## plate fields, label and input side-by-side
f_mask_input <- function(x, choices) {
  tags$div(
    class="form-group",
    tags$label(class = "col-sm-3 control-label", `for` = paste0("masked_wells_", x), x),
    column(width = 9, selectInput(paste0("masked_wells_", x), NULL, choices = choices, multiple = T))
  )
}


## get logical id (of same length as input) if outside actionlimit
add_outside_actionlimit <- function(yy, xx, actionlimit) {
  
  # id vector of all NA
  any_na <- is.na(yy) | is.na(xx)
  yy_na <- yy[!any_na]
  xx_na <- xx[!any_na]
  
  # fit 4pl logisitic function
  l_fit <- f_4pl(yy_na, xx_na)
  cf <- l_fit$coef
  
  # get absolute actionlimit value 
  # (difference between upper and lower asymptote, as identified from model
  #  times the input relative actionlimit)
  actionlimit_abs <- abs((cf["A"] - cf["B"])) * actionlimit/100
  
  # create empty result of same length as input
  res <- rep(NA, length(xx))
  
  # update non-NA values with result
  res[!any_na] <- abs(l_fit$dat$res) > actionlimit_abs
  
  # return
  res
}



## plot routine: plate (imported data)

f_plot_plate <- function(dat_plot) {
  
  # pre-process data for plotting:
  # remove empty values, add parantheses for masked points, create numeric version of rows and cols
  dat_plot %>%
    filter(!is.na(log10_conc)) %>% 
    mutate(value_char = as.character(value),
           value_char = if_else(masked == "no", value_char, paste0("(", value_char, ")"))) %>% 
    mutate(rows_rev = factor(rows, levels = LETTERS[8:1]),
           rows_i = as.numeric(rows_rev),
           cols_i = as.numeric(gsub("col_", "", cols)),
           xx = cols_i - 0.5,
           yy = rows_i - 0.5) %>% 
    
    # plot routine
    ggplot(aes(xx, yy)) +
    geom_tile(width = 1, height = 1, aes(fill = nofit), linetype = "solid", colour = "black") +
    geom_text(aes(label = value_char)) +
    theme_bw() +
    theme(axis.title = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank()) +
    scale_x_continuous(expand = c(0,0), limits = c(0,12), breaks = 1:12 - 0.5, labels = 1:12, position = "top") +
    scale_y_continuous(expand = c(0,0), limits = c(0,8), breaks = 1:8 - 0.5, labels = LETTERS[8:1]) +
    scale_fill_manual(guide = F, 
                      values = c("no" = "white", 
                                 "yes" = "grey75"))
  
}


## plot routine: scatter plot of rows excluded from fitting (imported data)

f_plot_nofit <- function(dat_plot) {
  
  # only keep data which is to be excluded from fitting
  dat_plot_nofit <- dat_plot %>% filter(nofit == 'yes')
  
  # simple scatter plot of retained data, with no constraint of equal y-axes
  ggplot(dat_plot_nofit, aes(log10_conc, value)) +
    geom_point(aes(shape = masked), size = 2) +
    facet_wrap(~rowcol_factor, ncol = 2, scales = "free_y") +
    theme_bw() +
    scale_shape_manual("Masked point", values = c("no" = 16, "yes" = 1))
  
}


## plot routine: 4pl fit (imported data)

f_plot_fit <- function(dat_plot, actionlimit, use_means = F, rsq = T) {
  
  # discard data to be excluded from fit (data where fit error occurred are
  # kept, but not used for fitting)
  dat_plot_fit <- dat_plot %>% filter(nofit == 'no')
  
  # get 4pl fits for further processing (only measurements where <fit_err> != 'yes')
  # optionally, fitting is done on mean values by concentration level
  try_plotfit <- if(use_means) {
    try(fit_output <- dat_plot_fit %>%
          filter(masked == 'no' & fit_err != 'yes') %>%
          group_by(rowcol_factor, log10_conc) %>% 
          summarise(value = mean(value)) %>% 
          do(l_fit = f_4pl(yy = .$value, xx = .$log10_conc)),
        silent = T)
  } else {
    try(fit_output <- dat_plot_fit %>%
          filter(masked == 'no' & fit_err != 'yes') %>%
          group_by(rowcol_factor) %>% 
          do(l_fit = f_4pl(yy = .$value, xx = .$log10_conc)),
        silent = T)
  }
  
  if (inherits(try_plotfit, "try-error")) {
    
    # Unexpected model fitting error (i.e. error occurs even after removal of data with
    # <fit_err> == 'yes') --> error message + scatter plots for plate as a whole
    
    print(paste0("Model fit error occurred after removal of data with <fit_err> == 'yes'"))
    
    ggplot(dat_plot_fit, aes(log10_conc, value)) +
      ggtitle("WARNING: Logistic curve could not be fit to the data") +
      geom_point(aes(shape = masked), size = 2) +
      facet_wrap(~rowcol_factor, ncol = 2) +
      theme_bw() +
      theme(plot.title = element_text(size = 20,face="bold", colour="Red")) +
      scale_linetype_manual("", values = c("fit" = "solid", "action limit" = "dashed")) +
      scale_colour_manual("", values = c("fit" = "black", "action limit" = "grey60")) +
      scale_shape_manual("Masked point", values = c("no" = 16, "yes" = 1))
    
  } else {
    
    # Get smoothed values with action limits for plotting (only measurements where <fit_err> != 'yes')
    dat_smooth <- fit_output %>%
      do(add_column(.$l_fit$dat_smooth, 
                    rowcol_factor = .$rowcol_factor, 
                    A_B = abs(diff(.$l_fit$coef[c("B", "A")])),
                    .before = 1)) %>% 
      mutate(al_min = fit_smooth - actionlimit/100*A_B,
             al_max = fit_smooth + actionlimit/100*A_B)
    
    # Create plot object
    plot_output <- ggplot(dat_plot_fit, aes(log10_conc, value)) +
      
      geom_line(data = dat_smooth, aes(x = xx_smooth, y = al_min, 
                                       linetype = "action limit", colour = "action limit")) +
      geom_line(data = dat_smooth, aes(x = xx_smooth, y = al_max, 
                                       linetype = "action limit", colour = "action limit")) +
      
      geom_point(aes(shape = masked), size = 2) +
      geom_line(data = dat_smooth, aes(x = xx_smooth, y = fit_smooth, 
                                       linetype = "fit", colour = "fit")) +
      
      facet_wrap(~rowcol_factor, ncol = 2) +
      theme_bw() +
      scale_linetype_manual("", values = c("fit" = "solid",
                                           "action limit" = "dashed")) +
      scale_colour_manual("", values = c("fit" = "black",
                                         "action limit" = "grey60")) +
      scale_shape_manual("Masked point", values = c("no" = 16,
                                                    "yes" = 1))
    
    # If model fitting errors occurred for any measurement type, add corresponding note
    
    if(any(dat_plot_fit$fit_err == 'yes', na.rm = T)) {
      plot_output <- plot_output +
        ggtitle(paste("NOTE: Model could not be fit for the following measurement types:",
                      paste(unique(dat_plot_fit %>% filter(fit_err == 'yes') %>% .$rowcol_factor), collapse = ', '),
                      "These data are not subjected to outlier testing.",
                      sep = '\n')) +
        theme(plot.title = element_text(size = 12))
    }
    
    # Optionally add (quasi) R^2 to plot
    
    if(rsq) {
      
      fit_output$rsq <- sapply(fit_output$l_fit, function(model) cor(model$dat$yy, model$dat$fit))^2
      fit_output$rsq_txt <- paste0('R² = ', trimws(sprintf("%8.4f", fit_output$rsq)))
      fit_output$rsq_y <- sapply(fit_output$l_fit, function(model) model$coef['A'])
      fit_output$rsq_y_vj <- sapply(
        fit_output$l_fit, function(model) {
          if(model$coef['B'] > model$coef['A']) 'bottom' else 'top'
        }
      )
      fit_output$rsq_x <- sapply(fit_output$l_fit, function(model) max(model$dat_smooth$xx_smooth))
      
      plot_output <- plot_output + geom_text(
        data = fit_output,
        mapping = aes(x = rsq_x, y = rsq_y,
                      label = rsq_txt,
                      vjust = rsq_y_vj, hjust = 'right',
                      fontface = "bold", cex = 1.3),
        show.legend = F
      )
    }
    
    # Draw plot
    plot_output
    
  }
  
}


## plot routine: 4pl fit after outlier test (without outlier points)

f_plot_fit_outlier_test <- function(dat_test, actionlimit, use_means = F, rsq = T) {
  
  # discard data to be excluded from fit and where fit error occurred
  dat_test_fit <- dat_test %>% filter(nofit == 'no' & fit_err != 'yes')
  
  # add outlier column
  dat_plot_fit <- dat_test_fit %>% 
    mutate(outlier = if_else(!is.na(test_out) & test_out == 1, "yes", "no"),
           outlier_mask = ifelse(masked == "yes", "masked", outlier))
  
  # get 4pl fits for non-outlier points for further processing
  # optionally, fitting is done on mean values by concentration level
  try_plotfit <- if(use_means) {
    try(fit_output <- dat_plot_fit %>%
          filter(outlier_mask == 'no') %>%
          group_by(rowcol_factor, log10_conc) %>% 
          summarise(value = mean(value)) %>% 
          do(l_fit = f_4pl(yy = .$value, xx = .$log10_conc)),
        silent = T)
  } else {
    try(fit_output <- dat_plot_fit %>%
          filter(outlier_mask == 'no') %>%
          group_by(rowcol_factor) %>% 
          do(l_fit = f_4pl(yy = .$value, xx = .$log10_conc)),
        silent = T)
  }
  
  # Get smoothed values with action limits for plotting
  dat_smooth <- fit_output %>%
    do(add_column(.$l_fit$dat_smooth, 
                  rowcol_factor = .$rowcol_factor, 
                  A_B = abs(diff(.$l_fit$coef[c("B", "A")])),
                  .before = 1)) %>% 
    mutate(al_min = fit_smooth - actionlimit/100*A_B,
           al_max = fit_smooth + actionlimit/100*A_B)
  
  # Create plot object
  plot_output <- ggplot(dat_plot_fit, aes(log10_conc, value)) +
    geom_line(data = dat_smooth, aes(x = xx_smooth, y = al_min, linetype = "action limit")) +
    geom_line(data = dat_smooth, aes(x = xx_smooth, y = al_max, linetype = "action limit")) +
    geom_point(aes(shape = outlier_mask), size = 2) +
    geom_line(data = dat_smooth, aes(x = xx_smooth, y = fit_smooth, linetype = "fit")) +
    facet_wrap(~rowcol_factor, ncol = 2) +
    theme_bw() +
    scale_shape_manual("Outliers", 
                       values = c("masked" = 1,
                                  "no" = 16, 
                                  "yes" = 8)) +
    scale_linetype_manual("", values = c("fit" = "solid", "action limit" = "dashed")) +
    ylab("value")
  
  # Optionally add (quasi) R^2 to plot
  
  if(rsq) {
    
    fit_output$rsq <- sapply(fit_output$l_fit, function(model) cor(model$dat$yy, model$dat$fit))^2
    fit_output$rsq_txt <- paste0('R² = ', trimws(sprintf("%8.4f", fit_output$rsq)))
    fit_output$rsq_y <- sapply(fit_output$l_fit, function(model) model$coef['A'])
    fit_output$rsq_y_vj <- sapply(
      fit_output$l_fit, function(model) {
        if(model$coef['B'] > model$coef['A']) 'bottom' else 'top'
      }
    )
    fit_output$rsq_x <- sapply(fit_output$l_fit, function(model) max(model$dat_smooth$xx_smooth))
    
    plot_output <- plot_output + geom_text(
      data = fit_output,
      mapping = aes(x = rsq_x, y = rsq_y,
                    label = rsq_txt,
                    vjust = rsq_y_vj,
                    hjust = 'right', fontface = "bold",
                    cex = 1.3),
      show.legend = F
    )
  }
  
  # Draw plot
  plot_output
  
}


## plot routine: plate overview after outlier testing

f_plot_plate_outlier <- function(dat_test) {
  
  # pre-process data
  dat_test %>%
    filter(!is.na(log10_conc)) %>% 
    mutate(outlier = if_else(!is.na(test_out) & test_out == 1, "yes", "no"),
           outlier_fitexcl = factor(ifelse(nofit == "yes" | fit_err == 'yes', "not tested", outlier),
                                  levels = c('no', 'yes', 'not tested')),
           outlier_mask = ifelse(masked == "yes", "masked", outlier),
           value_char = as.character(value),
           value_char = if_else(masked == "no", value_char, paste0("(", value_char, ")"))) %>% 
    mutate(rows_rev = factor(rows, levels = LETTERS[8:1]),
           rows_i = as.numeric(rows_rev),
           cols_i = as.numeric(gsub("col_", "", cols)),
           xx = cols_i - 0.5,
           yy = rows_i - 0.5) %>% 
    
    # plot
    ggplot(aes(xx, yy, label = value_char)) +
    geom_tile(width = 1, height = 1, aes(fill = outlier_fitexcl), linetype = "solid", colour = "black") +
    geom_text() +
    theme_bw() +
    theme(axis.title = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank()) +
    scale_x_continuous(expand = c(0,0), limits = c(0,12), breaks = 1:12 - 0.5, labels = 1:12, position = "top") +
    scale_y_continuous(expand = c(0,0), limits = c(0,8), breaks = 1:8 - 0.5, labels = LETTERS[8:1]) +
    scale_fill_manual("Outliers", 
                      values = c("not tested" = "grey75", 
                                 "no" = "white", 
                                 "yes" = "grey50"))
  
}

## Helper function to produce figures of different sizes within same R markdown chunk
# Modified from: http://michaeljw.com/blog/post/subchunkify/ [15MAY2019]

subchunk_plot <- function(
  plobj,                      # Plot object, e.g. from ggplot()
  sub_chunk_id,               # ID of sub-chunk used for current plot. Must be distinct from all
  # other Rmarkdown chunk IDs in given Rmd file (including IDs from
  # other calls to this function)
  fig_height,                 # Figure height value as passed on to chunk option <fig.height>
  fig_width                   # Figure width value as passed on to chunk option <fig.width>
) {
  plobj_deparsed <- paste0(deparse(
    function() {plobj}
  ), collapse = '')
  
  sub_chunk <- paste0(
    "`","``{r ", sub_chunk_id, ", fig.height=", fig_height, ", fig.width=", fig_width, ", echo=FALSE}",
    "\n(", plobj_deparsed, ")()",
    "\n`","``"
  )
  
  cat(knitr::knit(text = knitr::knit_expand(text = sub_chunk), quiet = TRUE))
}
