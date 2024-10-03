#Regression functions
#Functions----
# This script runs specific functions to prepare, run, and format effect 
# estimates.

# Pre-regression functions
# Well-being (short term (2011-2014)) formatting pre-regression
WBrename1114 <- function(dataset, varE, treatMINE,Yplug) {
   dataset <-   dataset %>% mutate(podesyr = "period1114",
                                   podesyr = as.factor(podesyr)) %>% 
      rename(Edchange1to3 = Edchange1114, Enchange1to3 = Enchange1114,
             Hchange1to3 = Hchange1114, Ichange1to3 = Ichange1114,
             LSchange1to3 = LSchange1114, Schange1to3 = Schange1114,
             MPIchange1to3 = MPIchange1114) %>% 
      rename(treatMINE = paste0(treatMINE)) %>% 
      dplyr::select(paste0(covariates.ot), paste0(varE),
                    paste0(Yplug), c("UniqueID","podesyr",
                                     "treatMINE", "weights", "subclass",
                                     "kab_code", "id"))
}

# Well-being (short term (2014-2018)) formatting pre-regression
WBrename1418 <- function(dataset, varE, treatMINE, Yplug) {
   dataset <-   dataset %>% mutate(podesyr = "period1418",
                                   podesyr = as.factor(podesyr)) %>% 
      rename(Edchange1to3 = Edchange1418, Enchange1to3 = Enchange1418,
             Hchange1to3 = Hchange1418, Ichange1to3 = Ichange1418,
             LSchange1to3 = LSchange1418, Schange1to3 = Schange1418,
             MPIchange1to3 = MPIchange1418) %>% 
      rename(treatMINE = paste0(treatMINE)) %>% 
      dplyr::select(paste0(covariates.ot),paste0(varE),paste0(Yplug),
                    c("podesyr", "UniqueID", "treatMINE",
                      "weights", "subclass", "kab_code", "id"))
}

# Deforestation (short term, 2011-2014) formatting
DFrename1114 <- function(dataset, varE, treatMINE) {
   dataset <-  dataset %>% 
      mutate(podesyr = "period1114", podesyr = as.factor(podesyr)) %>% 
   rename(LOSS1to3 = LOSS1113_pt,
          treatMINE = paste0(treatMINE)) %>% 
   dplyr::select(paste0(covariates.ot),
                 paste0(varE),
                 c("LOSS1to3",
                   "UniqueID","podesyr",
                   "treatMINE", "weights",
                   "subclass", "kab_code", "id"))
}



# Deforestation (short term, 2014-2018) formatting
DFrename1418 <- function(dataset, varE, treatMINE) {
   dataset <-  dataset %>% 
      mutate(podesyr = "period1418", podesyr = as.factor(podesyr)) %>% 
      rename(LOSS1to3 = LOSS1417_pt,
             treatMINE = paste0(treatMINE)) %>% 
      dplyr::select(paste0(covariates.ot),
                    paste0(varE),
                    c("LOSS1to3",
                      "UniqueID","podesyr",
                      "treatMINE", "weights",
                      "subclass", "kab_code", "id"))
}
#Regression models
#1a. Regression function across several outcome variables
regtest <- function (data, Yvariate, Treatment, covariates, FacVar){
   f <- as.formula(paste(Yvariate[[i]], paste0(" ~ ",Treatment, "+"),
                         paste0(covariates, collapse = "+"),
                         paste0(" + as.factor(", FacVar, ")",
                                collapse = "")))
   fit <- lm(f, data = data, weights = weights)
   coef.res <- coeftest(fit, vcov. = vcovCL,
                        cluster = ~ subclass + id + kab_code, df = Inf)
   
   return(list(fit = fit,
               coef.res = coef.res))
}

#1b. Regression function across one outcome variable
regtest2 <- function (data, Yvariate, Treatment, covariates, FacVar){
   
   f <- as.formula(paste(Yvariate, paste0(" ~ ",Treatment, "+"),
                         paste0(covariates, collapse = "+"),
                         paste0(" + ", FacVar, collapse = "")))
   fit <- lm(f, data = data, weights = weights)
   
   coef.res <- coeftest(fit, vcov. = vcovCL,
                        cluster = ~ subclass + id + kab_code, df = Inf)
   return(list(fit = fit,
               coef.res = coef.res))
   
}


# Post-regression functions


# coefficients table function for well-being regression
coeftab <- function(dataset) {
   
   change2pct <- function(var) {
      -(var*100)
   }  
   vartrans <- c("Estimate", "lwr","upr","lwr5", "upr95")  
   dataset <- dataset %>% as.data.frame() %>% rename(
      "se" = "Std. Error", 
      "zval" = "Pr(>|z|)",
      "lwr" = "97.5 %", # note we reverse the directionality as it currently measures poverty 
      "upr" = "2.5 %", # note we reverse the directionality as it currently measures poverty
      "lwr5" = "95 %",
      "upr95" = "5 %",
      "Dim" = "V1") %>% 
      mutate_at(vars(
         Estimate, se, zval, lwr, upr, lwr5, upr95), as.numeric) %>% 
      mutate_at(vars(all_of(vartrans)),change2pct) %>% 
      mutate(Dimension =
                ifelse(grepl("^Edchange", Dim),
                       'Education',
                       ifelse(grepl("^Enchange", Dim),
                              'Environment',
                              ifelse(grepl("^Hchange", Dim),
                                     'Health',
                                     ifelse(grepl("^Ichange", Dim),
                                            'Infrastructure',
                                            ifelse(grepl("^LSchange",Dim),
                                                   'Living Standards',
                                                   ifelse(grepl("^Schange", Dim),
                                                          'Social',
                                                          'Total'))))))) %>% 
      mutate(Dimension = factor(Dimension, 
                    levels = c("Education","Environment", "Health",
                               "Infrastructure","Living Standards",
                               "Social", "Total")))
}

# coefficients table function for deforestation regression
coeftabDF <- function(dataset) {
   DFchange2pct <- function(var) {
      (var*100)
   }
   vartrans <- c("Estimate", "lwr","upr","lwr5", "upr95")  
   dataset <- dataset %>% as.data.frame() %>% 
      rename("se" = "Std. Error", "zval" = "Pr(>|z|)",
             "lwr" = "2.5 %", "upr" = "97.5 %", "lwr5" = "5 %",
             "upr95" = "95 %",
             "Mining_type" = "V2") %>%
      mutate_at(vars(Estimate,
                     se, zval,
                     lwr,
                     upr,
                     lwr5,
                     upr95), as.numeric) %>%
      mutate_at(vars(all_of(vartrans)), DFchange2pct)
      
}
