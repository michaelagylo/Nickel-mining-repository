#                 #3. Regressions for figures and tables
# Introduction:
# This R script performs regressions and the outputs for the manuscript
# and Supplemental Information 
# All prepared data are sourced from "final_output/final_data/"
# All data will be processed and saved in the "final_output/" folder

# Prepare work space ----
rm(list=ls())

#Download Packages
library("dplyr")
library("ggpubr")
library("jtools")
library("openxlsx")
library("car")

install.packages("remotes")
remotes::install_github("Ngendahimana/sensitivityR5")
library("SensitivityR5")

#set options
options(scipen=9999) # avoid scientific notation for readability

# If not using the RStudio project, set working directory to the repository
# directory. 
# setwd("../")


# Define output folder
output_path <- c("final_output/")

# Source functions 
# loads a collection of functions to prepare, run and format regressions
source("regression_function.R")
# loads a collection of functions to perform interaction effects 
source("interaction_function.R")

# define covariates
# Continuous variables for matching
covariates.ni <- c("LOG_ELEV", "LOG_SLOPE", "LOG_ACCESS","LOG_AREA",
                   "LOG_popkm", "DRYMEAN", "WETMEAN","FOR11_pt",
                   "pt_APL","pt_PRODFOR", "pt_HL")
covariates.ot <- c("LOG_ELEV", "LOG_SLOPE", "LOG_ACCESS","LOG_AREA", 
                   "LOG_popkm", "DRYMEAN", "WETMEAN","FOR11_pt","pt_APL",
                   "pt_PRODFOR", "pt_HL","pt_Lstone"  ,"pt_volcanic")

# Categorical/exact variables for matching
var.exact <- c("MPIBASE_CLS2", "LTYPE.11", "prov_code")
var.experiod <- c("MPIBASE_CLS2", "LTYPE.11", "prov_code", "podesyr")
var.exactPOV <- c("LTYPE.11", "prov_code")
var.experpov <- c("LTYPE.11", "prov_code", "podesyr")

# define outcome variables

# Deforestation outcomes
#LOSS1to3 <- forest cover change 103 years since baseline year
#LOSS1118_pt <- forest cover change 1-7 years since baseline year

#Well-being change 1-3 years since baseline year
Yvar <- c("Edchange1to3", "Enchange1to3", "Hchange1to3", 
          "Ichange1to3", "LSchange1to3", "Schange1to3",
          "MPIchange1to3")
#Well-being change 1-7 years since baseline year
Y_1118 <- c("Edchange1118", "Enchange1118", "Hchange1118",
            "Ichange1118", "LSchange1118", "Schange1118",
            "MPIchange1118")

# define data_path to retrieve matched sample data
data_path <- c("final_output/final_data/")

# data for Figure 4a ----

# Nickel mining matched data - short term (1-3 years, 2011-2014)
Nickshort1.dt <- readRDS(paste0(data_path, "Nickel_short1.RDS")) 
Nickshort1.dt <- DFrename1114(Nickshort1.dt, var.exact, "Nickel_1114") 

# Nickel mining matched data - short term (1-3 years, 2014-2017)
Nickshort2.dt <- readRDS(paste0(data_path, "Nickel_short2.RDS")) 
Nickshort2.dt <- DFrename1418(Nickshort2.dt,var.exact, "Nickel_1417") 
nickeldfshort <- rbind(Nickshort1.dt ,Nickshort2.dt)

# create an empty table to store regression results
df.res <- NULL
col2 <- "Nickel"
col3 <- "1-3 years"

# apply the regtest2 function, which runs a regression to to estimate effects
# of treatment in the matched sample for one outcome of interest
# see regression_function.R for more details
tmp.out <- regtest2(data = nickeldfshort,        # matched sample data
                 Yvariate = "LOSS1to3",       # deforestation outcome (%)
                 Treatment = "treatMINE",     # treatment variable
                 covariates = covariates.ni,  # continuous covariates
                 FacVar = var.experiod)       # factor covariates

# create table of results
df.res <- rbind(df.res,
                c("LOSS1to3",
                  paste0(col2),
                  paste0(col3),
                  round(tmp.out$coef.res[2,1:4],3),
                  round(confint(tmp.out$coef.res)[2,],3),
                  round(confint(tmp.out$coef.res, level = 0.9)[2,],3)))

# Nickel mining matched data - long term (4-7 years, 2011-2018)
Nickel_long <- readRDS(paste0(data_path, "Nickel_long.RDS")) 
col2 <- "Nickel"
col3 <- "4-7 years"

tmp.out <- regtest2(Nickel_long,
                 "LOSS1118_pt",
                 "Nickel_1114",
                 covariates.ni,
                 var.exact)

#store results
df.res <- rbind(df.res,
                c("LOSS1118_pt", paste0(col2),
                  paste0(col3),
                  round(tmp.out$coef.res[2,1:4],3),
                  round(confint(tmp.out$coef.res)[2,],3),
                  round(confint(tmp.out$coef.res,
                                level = 0.9)[2,],3)
                )
)

## Short term (1-3 yrs) 
# Other mining matched data - short term (1-3 years, 2011-2014)
Othershort1.dt <- readRDS(paste0(data_path, "Other_short1.RDS")) 
Othershort1.dt <- DFrename1114(Othershort1.dt,var.exact, "pMINE1114")

# Other mining matched data - short term (1-3 years, 2014-2017)
Othershort2.dt <- readRDS(paste0(data_path, "Other_short2.RDS")) 
Othershort2.dt <- DFrename1418(Othershort2.dt,var.exact, "pMINE1417") 
otherdfshort <- rbind(Othershort1.dt ,Othershort2.dt)

col2 <- "Other"
col3 <- "1-3 years"

tmp.out <- regtest2(otherdfshort,
              "LOSS1to3",
              "treatMINE",
              covariates.ot,
              var.experiod)
df.res <- rbind(df.res, c("LOSS1to3",
                          paste0(col2),
                          paste0(col3),
                          round(tmp.out$coef.res[2,1:4],3),
                          round(confint(tmp.out$coef.res)[2,],3),
                          round(confint(tmp.out$coef.res, level = 0.9)[2,],3)))

# Other mining matched data - long term (4-7yrs, 2011-2018)
Other_long <- readRDS(paste0(data_path, "Other_long.RDS")) 
col2 <- "Other"
col3 <- "4-7 years"

tmp.out <- regtest2(Other_long,
              "LOSS1118_pt",
              "pMINE1114",
              covariates.ni,
              var.exact)
df.res <- rbind(df.res,
                c("LOSS1118_pt",
                  paste0(col2),
                  paste0(col3),
                  round(tmp.out$coef.res[2,1:4],3),
                  round(confint(tmp.out$coef.res)[2,],3),
                  round(confint(tmp.out$coef.res,
                                level = 0.9)[2,],3)
                )
)   

# format results table
df.res <- coeftabDF(df.res)
df.res <- rename(df.res, "period" =V3)
initial <- df.res %>%
   select(-c(V1)) %>% 
   mutate(Estimate = 0,
          "se" = 0, 
          "z value" = 0,
          "zval" = 0,
          "lwr" = 0,"upr" = 0,"lwr5" = 0,
          "upr95" = 0, period = "0") %>% distinct()
df.res <- select(df.res, -c(V1))
df.res <- rbind(df.res,initial)
df.res$period <- factor(df.res$period,
                        levels = c("0", "1-3 years", "4-7 years")) 
df.res_ordered <- df.res %>% arrange(Mining_type, period)
write.csv(df.res_ordered, paste0(output_path, "Fig4a.csv"))

# Figure 4b-c ----
##short term (1-3 years)
Nickshort1.dt <- readRDS(paste0(data_path, "Nickel_short1.RDS")) 
Nickshort1.dt <- WBrename1114(Nickshort1.dt,var.exact,"Nickel_1114", Yvar) 


Nickshort2.dt <- readRDS(paste0(data_path, "Nickel_short2.RDS")) 
Nickshort2.dt <- WBrename1418(Nickshort2.dt,
                              var.exact,
                              "Nickel_1417",
                              Yvar) 

Nickel_short_combine <- rbind(Nickshort1.dt, Nickshort2.dt)

# create an empty table to store regression results
Nick.res <- NULL
col2 <- "Nickel"
col3 <- "1-3 years"

# apply the regtest function, which runs a regression to to estimate effects
# of treatment in the matched sample across multiple well-being dimensions
# see regression_function.R for more details
for (i in 1:length(Yvar)) {
   tmp.out <- regtest(data = Nickel_short_combine,
                      Yvariate = Yvar,
                      Treatment = "treatMINE",
                      covariates = covariates.ni,
                      FacVar = var.experiod)
   
# Store results in a table
   Nick.res <- rbind(Nick.res,
                     c(Yvar[i],
                       paste0(col2),
                       paste0(col3),
                       round(tmp.out$coef.res[2,1:4],3),
                       round(confint(tmp.out$coef.res)[2,],3),
                       round(confint(tmp.out$coef.res, level = 0.9)[2,],3)))
}

Nickel_long <- readRDS(paste0(data_path, "Nickel_long.RDS")) 
col3 <- "4-7 years" 

for (i in 1:length(Y_1118)) {
   tmp.out<- regtest(Nickel_long,
                     Y_1118,
                     "Nickel_1114",
                     covariates.ni,
                     var.exact)
   Nick.res <- rbind(Nick.res, 
                     c(Y_1118[i],
                       paste0(col2),
                       paste0(col3),
                       round(tmp.out$coef.res[2,1:4],3),
                       round(confint(tmp.out$coef.res)[2,],3),
                       round(confint(tmp.out$coef.res,level = 0.9)[2,],3)))
}

tab.Nick.res <- coeftab(Nick.res)
tab.Nick.res <- tab.Nick.res %>% rename("Mine_type" = "V2", "period" = "V3")

Short1.RDS <- readRDS(paste0(data_path, "Other_short1.RDS"))
Short1.RDS <- WBrename1114(Short1.RDS,
                           var.exact,
                           "pMINE1114",
                           Yvar) 

Short2.RDS <- readRDS(paste0(data_path, "Other_short2.RDS"))
Short2.RDS <- WBrename1418(Short2.RDS,
                           var.exact,
                           "pMINE1417",
                           Yvar) 

Other_short_combine <- rbind(Short1.RDS, Short2.RDS)

tab.Other.res <- NULL
period <- "1-3 years"

# Note: prov_code removed from covariates from regression
#       due to multicolinearity
#       (tested using vif() function in "car" package)
for (i in 1:length(Yvar)){
   tmp.out <- regtest(Other_short_combine,
                      Yvar,
                      "treatMINE",
                      covariates.ot,
                      var.experiod[var.experiod != "prov_code"] 
                      )
   
   tab.Other.res <- rbind(tab.Other.res,
                          c(Yvar[i],
                          paste0(period),
                          round(tmp.out$coef.res[2,1:4],3),
                          round(confint(tmp.out$coef.res)[2,],3), 
                          round(confint(tmp.out$coef.res, level = 0.9)[2,],3)))
}   
##long term (4-7 years)
Other_long <- readRDS(paste0(data_path, "Other_long.RDS"))
period <- "4-7 years"
for (i in 1:length(Y_1118))
   {
   tmp.out <- regtest(Other_long,
                      Y_1118,
                      "pMINE1114",
                      # remove APL due to multicolinearity
                      covariates.ot[!covariates.ot %in% c("pt_APL")],
                      # remove prov_code due to multicolinearity
                      var.exact[!var.exact %in% c("prov_code")]
                      )
   tab.Other.res <- rbind(tab.Other.res,
                          c(Y_1118[i],
                            paste0(period),
                            round(tmp.out$coef.res[2,1:4],3),
                            round(confint(tmp.out$coef.res)[2,],3),
                            round(confint(tmp.out$coef.res,
                                          level = 0.9)[2,],3)))
}
# warming message may occur - ok to ignore

#clean
tab.Other.res <- coeftab(tab.Other.res) %>% rename("period" = "V2")

Minetab <- tab.Other.res %>% 
   mutate(Mine_type = "Other") %>% 
   select(Dimension, Mine_type, period, Estimate, zval ,lwr, upr, lwr5, upr95)
Nicktab <- tab.Nick.res %>% 
   select(Dimension, Mine_type, period, Estimate, zval, lwr, upr, lwr5, upr95)

all <- rbind(Minetab, Nicktab)

initial <- all %>% mutate(Estimate = 0,"zval" = 0,
                          "lwr" = 0,"upr" = 0,"lwr5" = 0,
                          "upr95" = 0, period = "0") %>% distinct()
WB.res <- rbind(all,initial)

WB.res$period <- factor(WB.res$period,
                        levels = c("0", "1-3 years", "4-7 years")) 
WB.res_ordered <- WB.res %>% arrange(Mine_type, period, Dimension)
write.csv(WB.res_ordered, paste0(output_path, "Fig4b-c.csv"))

# Figure 5 (baseline poverty conditions) ----
# Figure 5a: Nickel & Deforestation ====

Nickel_low.dt1 <- readRDS(paste0(data_path, "Nickel_short1_Low.RDS"))
Nickel_low.dt2 <- readRDS(paste0(data_path, "Nickel_short2_Low.RDS"))
Nickel_high.dt1 <- readRDS(paste0(data_path, "Nickel_short1_High.RDS"))
Nickel_high.dt2 <- readRDS(paste0(data_path, "Nickel_short2_High.RDS"))

Nickel_low.dt1 <- DFrename1114(Nickel_low.dt1, var.exactPOV, "Nickel_1114") 
Nickel_low.dt2 <- DFrename1418(Nickel_low.dt2, var.exactPOV, "Nickel_1417") 
Nickel_high.dt1 <- DFrename1114(Nickel_high.dt1, var.exactPOV, "Nickel_1114") 
Nickel_high.dt2 <- DFrename1418(Nickel_high.dt2, var.exactPOV, "Nickel_1417") 

#variables

nickel_low_short <- bind_rows(Nickel_low.dt1, Nickel_low.dt2)
nickel_low_short <- nickel_low_short %>% 
   select(paste0(covariates.ni),"LOSS1to3",
          paste0(var.exactPOV), c("UniqueID","podesyr", "treatMINE",
                                  "weights", "subclass", "kab_code", "id"))

nickel_high_short <- bind_rows(Nickel_high.dt1, Nickel_high.dt2)
nickel_high_short <- nickel_high_short %>% 
   select(paste0(covariates.ni),"LOSS1to3",
          paste0(var.exactPOV), c("UniqueID","podesyr", "treatMINE",
                                  "weights", "subclass", "kab_code", "id"))

#regressions
Nick.res.df <- NULL

commod <- "Nickel"
col3 <- "1-3 years"
pov <- "Low"
tmp.out <- regtest2(nickel_low_short,
                 "LOSS1to3",
                 "treatMINE",
                 covariates.ni,
                 var.experpov)

Nick.res.df <- rbind(Nick.res.df,
                     c("LOSS1to3",
                       paste0(commod),
                       paste0(pov),
                       paste0(col3),
                       round(tmp.out$coef.res[2,1:4],3),
                       round(confint(tmp.out$coef.res)[2,],3),
                       round(confint(tmp.out$coef.res,
                                     level = 0.9)[2,],3)))

commod <- "Nickel"
col3 <- "1-3 years"
pov <- "High"

tmp.out <- regtest2(nickel_high_short,
                 "LOSS1to3",
                 "treatMINE",
                 covariates.ni,
                 var.experpov)

Nick.res.df <- rbind(Nick.res.df,
                     c("LOSS1to3",
                       paste0(commod),
                       paste0(pov),
                       paste0(col3),
                       round(tmp.out$coef.res[2,1:4],3),
                       round(confint(tmp.out$coef.res)[2,],3),
                       round(confint(tmp.out$coef.res,
                                     level = 0.9)[2,],3)))

##long term (4-7 years)
Nickel_long_low <- readRDS(paste0(data_path, "Nickel_long_Low.RDS"))
Nickel_long_high <- readRDS(paste0(data_path, "Nickel_long_High.RDS"))

col3 <- "4-7 years"
pov <- "Low"

tmp.out <- regtest2(Nickel_long_low,
                 "LOSS1118_pt",
                 "Nickel_1114",
                 covariates.ni,
                 var.exactPOV)

Nick.res.df <- rbind(Nick.res.df,
                     c("LOSS1118_pt",paste0(commod),
                       paste0(pov),
                       paste0(col3),
                       round(tmp.out$coef.res[2,1:4],3),
                       round(confint(tmp.out$coef.res)[2,],3),
                       round(confint(tmp.out$coef.res,
                                     level = 0.9)[2,],3)))

pov <- "High"
tmp.out <- regtest2(Nickel_long_high,
                 "LOSS1118_pt",
                 "Nickel_1114",
                 covariates.ni,
                 var.exactPOV)

Nick.res.df <- rbind(Nick.res.df,
                     c("LOSS1118_pt",
                       paste0(commod),
                       paste0(pov),
                       paste0(col3),
                       round(tmp.out$coef.res[2,1:4],3),
                       round(confint(tmp.out$coef.res)[2,],3),
                       round(confint(tmp.out$coef.res,
                                     level = 0.9)[2,],3)))
Nick.res.df <- coeftabDF(Nick.res.df) 
Nick.res.df <- Nick.res.df %>% rename("period" = "V4",
                                      "MPI_Class" = "V3")
Nick.res.df$V1 <- NULL
Nick.res.df$MPI_Class <- factor(Nick.res.df$MPI_Class,
                                levels = c("Low","High"))

write.csv(Nick.res.df, paste0(output_path, "Fig5a.csv"))


#Figure 5b and 5c: Nickel and Well-being ----

#Prep data
##short term (1-3 years)
Nickel_low.dt1 <- readRDS(paste0(data_path, "Nickel_short1_Low.RDS"))
Nickel_low.dt2 <- readRDS(paste0(data_path, "Nickel_short2_Low.RDS"))

Nickel_high.dt1 <- readRDS(paste0(data_path, "Nickel_short1_High.RDS"))
Nickel_high.dt2 <- readRDS(paste0(data_path, "Nickel_short2_High.RDS"))

Nickel_low.dt1 <- WBrename1114(Nickel_low.dt1,
                               var.exactPOV,
                               "Nickel_1114",
                               Yvar) 
Nickel_low.dt2 <- WBrename1418(Nickel_low.dt2,
                               var.exactPOV,
                               "Nickel_1417",
                               Yvar) 
Nickel_high.dt1 <- WBrename1114(Nickel_high.dt1,
                                var.exactPOV,
                                "Nickel_1114",
                                Yvar) 
Nickel_high.dt2 <- WBrename1418(Nickel_high.dt2,
                                var.exactPOV,
                                "Nickel_1417",
                                Yvar) 

nickel_low_short <- bind_rows(Nickel_low.dt1,
                              Nickel_low.dt2)
nickel_high_short <- bind_rows(Nickel_high.dt1,
                               Nickel_high.dt2)

#Run regressions
Nick.res.pov <- NULL

col2 <- "Nickel"
col3 <- "1-3 years"
pov <- "Low"

for (i in 1:length(Yvar)) {
   tmp.out <- regtest(nickel_low_short,
                   Yvar,
                   "treatMINE",
                   # remove APL due to multicolinearity
                   covariates.ni[!covariates.ni %in% c("pt_APL")],
                   # remove prov_code due to multicolinearity
                  var.experpov[!var.experpov %in% c("prov_code")]
                   )
   Nick.res.pov <- rbind(Nick.res.pov, 
                         c(Yvar[i],
                           paste0(col2),
                           paste0(col3),
                           paste0(pov),
                           round(tmp.out$coef.res[2,1:4],3),
                           round(confint(tmp.out$coef.res)[2,],3),
                           round(confint(tmp.out$coef.res,level = 0.9)[2,],3)))
}
# warning make occur - okay to ignore

pov <- "High"
for (i in 1:length(Yvar)) {
   tmp.out <- regtest(nickel_high_short,
                   Yvar,
                   "treatMINE",
                   # remove APL due to multicolinearity
                   covariates.ni[!covariates.ni %in% c("pt_APL")],
                   # remove prov_code due to multicolinearity
                   var.exactPOV[!var.exactPOV %in% c("prov_code")]
   )
   Nick.res.pov <- rbind(Nick.res.pov, 
                         c(Yvar[i],
                           paste0(col2),
                           paste0(col3),
                           paste0(pov),
                           round(tmp.out$coef.res[2,1:4],3),
                           round(confint(tmp.out$coef.res)[2,],3),
                           round(confint(tmp.out$coef.res,level = 0.9)[2,],3)))
}
# warning make occur - okay to ignore


col3 <- "4-7 years"
pov <- "Low"

for (i in 1:length(Y_1118)) {
   tmp.out <- regtest(Nickel_long_low,
                      Y_1118,
                      "Nickel_1114",
                      covariates.ni,
                      var.exactPOV)
   Nick.res.pov <- rbind(Nick.res.pov, 
                         c(Y_1118[i],
                           paste0(col2),
                           paste0(col3),
                           paste0(pov),
                           round(tmp.out$coef.res[2,1:4],3),
                           round(confint(tmp.out$coef.res)[2,],3),
                           round(confint(tmp.out$coef.res,level = 0.9)[2,],3)))
}

pov <- "High"
for (i in 1:length(Y_1118)) {
   tmp.out <- regtest(Nickel_long_high,
                      Y_1118,
                      "Nickel_1114",
                      covariates.ni,
                      var.exactPOV)
   Nick.res.pov <- rbind(Nick.res.pov, 
                         c(Y_1118[i],
                           paste0(col2),
                           paste0(col3),
                           paste0(pov),
                           round(tmp.out$coef.res[2,1:4],3),
                           round(confint(tmp.out$coef.res)[2,],3),
                           round(confint(tmp.out$coef.res,level = 0.9)[2,],3)))
}

Nick.res.pov.tab <- coeftab(Nick.res.pov)
Nick.res.pov.tab <- Nick.res.pov.tab %>%
   as.data.frame %>% 
   rename("Mine_type" = "V2",
          "period" = "V3",
          "MPI_Class" = "V4")
Nick.res.pov.tab$Dim <- NULL
write.csv(Nick.res.pov.tab, paste0(output_path, "Fig5b-c.csv"))


# Figure S1 and Table S5----
Nickel_whole <- readRDS(paste0(data_path, "Nickel_whole.RDS")) 

#Back transform logged variables for readability 
Nickel_whole$ACCESSMEAN <- exp(Nickel_whole$LOG_ACCESS) - 1
Nickel_whole$ELEVMEAN <- exp(Nickel_whole$LOG_ELEV) - 1
Nickel_whole$SLOPEMEAN <- exp(Nickel_whole$LOG_SLOPE) - 1
Nickel_whole$LOSS1118_pt <- Nickel_whole$LOSS1118_pt*100

#PredictInt function estimates interaction effects on deforestation between: 
# Base_ACC: Accessibility and Nickel mining
# Base_ELEV: Elevation and Nickel mining
# Base_SLOPE: Slope and Nickel mining
Base_ACC <- predictInt("Nickel_1117",
                       "ACCESSMEAN",
                       "LOSS1118_pt",
                       Nickel_whole,
                       Nickel_whole$Nickel_1117)
Base_ELEV <- predictInt("Nickel_1117",
                        "ELEVMEAN",
                        "LOSS1118_pt",
                        Nickel_whole,
                        Nickel_whole$Nickel_1117)
Base_SLOPE <- predictInt("Nickel_1117",
                         "SLOPEMEAN",
                         "LOSS1118_pt",
                         Nickel_whole,
                         Nickel_whole$Nickel_1117)

export_summs(Base_ACC$coef.res,
             Base_ELEV$coef.res,
             Base_SLOPE$coef.res,
             model.names = c(
                "Nickel - Accessibility",
                "Nickel - Elevation (m)",
                "Nickel - Slope (deg)"),
             scale = TRUE,
             stars = NULL ,
             number_format = "%.5g",
             error_format = "({std.error})\n[{conf.low}, {conf.high}]",
             statistics = c("No. of villages" = "nobs",
                            "AIC" = "AIC", "BIC" = "BIC"), 
             coefs = c(
                "Mining impact" = "Nickel_11171",
                "Covariate" = "ACCESSMEAN",
                "Covariate" = "ELEVMEAN",
                "Covariate" = "SLOPEMEAN",
                "Interaction" = "Nickel_11171:ACCESSMEAN",
                "Interaction" = "Nickel_11171:ELEVMEAN",
                "Interaction" = "Nickel_11171:SLOPEMEAN"),
             to.file = "docx", file.name = paste(output_path, "TabS5.docx"))

# Figure S2 ----
Nickel_whole <- readRDS(paste0(data_path, "Nickel_whole.RDS")) 

#Back transform logged variables readability 
Nickel_whole$ACCESSMEAN <- exp(Nickel_whole$LOG_ACCESS)-1 

#convert poverty score to well-being score 
Nickel_whole$WBchange1118 <- -(Nickel_whole$MPIchange1118)*100

#PredictInt function estimates interaction effects on well-being between: 
# Base_MPI: Poverty baseline conditions and Nickel mining
# Base_LTYPE: Livelihood type and Nickel mining
# Base_ACC: Accessibility and Nickel mining
# see interaction.function.R for more details
WB_MPI <- predictInt(Tr = "Nickel_1117",           # treatment variable
                     Xind = "MPIBASE_CLS2",        # control variable
                     Yvariate = "WBchange1118",   # outcome of interest
                     data = Nickel_whole,          # data 
                     Tr2 = Nickel_whole$Nickel_1117) # interaction terms

WB_LTYPE <- predictInt("Nickel_1117",
                       "LTYPE.11",
                       "WBchange1118",
                       Nickel_whole,
                       Nickel_whole$Nickel_1117)
WB_ACC <- predictInt("Nickel_1117",
                     "ACCESSMEAN",
                     "WBchange1118",
                     Nickel_whole,
                     Nickel_whole$Nickel_1117)

write.csv(WB_MPI$pred.plot, paste0(output_path, "FigS2A.csv"))
write.csv(WB_LTYPE$pred.plot, paste0(output_path, "FigS2B.csv"))
write.csv(WB_ACC$pred.plot, paste0(output_path, "FigS2C.csv"))


# Table S3 and Figure 3 ----

Nickel_whole <- readRDS(paste0(data_path, "Nickel_whole.RDS")) 

#plot the data
table <- NULL
tmp.out <- regtest2(Nickel_whole,
                  "LOSS1118_pt",
                  "Nickel_1117",
                  covariates.ni,
                  var.exact)

table <- rbind(table, 
               c("LOSS1118_pt",
                 round(tmp.out$coef.res[2,1:4],3),
                 round(confint(tmp.out$coef.res)[2,],3),
                 round(confint(tmp.out$coef.res,level = 0.9)[2,],3)))

avg_comparisons(tmp.out$fit, variables = "Nickel_1117",
                vcov = ~ subclass + id,
                newdata = subset(Nickel_whole, Nickel_1117 == 1),
                wts = "weights")

pred1 <- avg_predictions(tmp.out$fit, variables = "Nickel_1117",
                         vcov = ~ subclass + id,
                         newdata = subset(Nickel_whole, Nickel_1117 == 1),
                         wts = "weights")

pred1.plot <- plot_predictions(tmp.out$fit, 
                               condition = "Nickel_1117", 
                               vcov = ~ subclass + id,
                               draw = FALSE)

tabprep1 <- pred1 %>% mutate(Nickel_1117 = 
                                dplyr::recode(Nickel_1117, 
                                       "1" = "Nickel",
                                       "0" = "Non-mining"), 
                             Analysis = "Nickel") %>% 
   rename("Intervention" = "Nickel_1117", "Estimate" = "estimate") 

col.keep <- c("Estimate", "std.error", "p.value",
              "conf.low", "conf.high", "Intervention",
              "Analysis")
tabprep1 <- tabprep1 %>% dplyr::select(all_of(col.keep))

#Other
Other_whole <- readRDS(paste0(data_path, "Other_whole.RDS")) 

tmp.out <- regtest2(Other_whole,
                  "LOSS1118_pt",
                  "pMINE1117",
                  covariates.ot,
                  var.exact)
table <- rbind(table, 
               c("LOSS1118_pt",
                 round(tmp.out$coef.res[2,1:4],3),
                 round(confint(tmp.out$coef.res)[2,],3),
                 round(confint(tmp.out$coef.res,level = 0.9)[2,],3)))
avg_comparisons(tmp.out$fit, variables = "pMINE1117",
                vcov = ~ subclass + id,
                newdata = subset(Other_whole, pMINE1117 == 1),
                wts = "weights")
pred2 <- avg_predictions(tmp.out$fit, variables = "pMINE1117",
                         vcov = ~ subclass + id,
                         wts = "weights")

pred2.plot <- plot_predictions(tmp.out$fit,
                               condition = "pMINE1117",
                               vcov = ~ subclass + id,
                               draw = FALSE)

tabprep2 <- pred2 %>% mutate(pMINE1117 = 
                                dplyr::recode(pMINE1117,
                                       "1" = "Other mining",
                                       "0" = "Non-mining "),
                             Analysis = "Other") %>% 
   rename("Intervention" = "pMINE1117", "Estimate" = "estimate") 

tabprep2 <- tabprep2 %>% dplyr::select(all_of(col.keep))


predall <- rbind(tabprep1,tabprep2)

table.S3 <- as.data.frame(predall) %>% select(c(Estimate,
                                                conf.low,
                                                conf.high,
                                                Intervention, 
                                                Analysis))  
table.S3$Estimate <- (table.S3$Estimate)*100 
table.S3$conf.low <- (table.S3$conf.low)*100 
table.S3$conf.high <- (table.S3$conf.high)*100 

write.csv(table.S3, paste0(output_path, "TabS3.csv"))

# Table S4 ----
Nickel_whole <- readRDS(paste0(data_path, "Nickel_whole.RDS")) 

#convert poverty score to well-being score 
Nickel_whole$WBchange1118 <- -(Nickel_whole$MPIchange1118)*100

#plot the data
table <- NULL

tmp.out <- regtest2(Nickel_whole,
              "WBchange1118",
              "Nickel_1117",
              covariates.ni,
              var.exact)
table <- rbind(table, 
               c("WBchange1118",
                 round(tmp.out$coef.res[2, 1:4],3),
                 round(confint(tmp.out$coef.res)[2, ], 3),
                 round(confint(tmp.out$coef.res,level = 0.9)[2, ], 3)))

# compare the average estimated outcomes
# between Nickel and non mining villages 
# note: kab_code has been removed due to multicolinearity with subclass and id
avg_comparisons(tmp.out$fit, variables = "Nickel_1117",
                vcov = ~ subclass + id,
                newdata = subset(Nickel_whole, Nickel_1117 == 1),
                wts = "weights")

pred1 <- avg_predictions(tmp.out$fit, variables = "Nickel_1117",
                         vcov = ~ subclass + id,
                         newdata = subset(Nickel_whole, Nickel_1117 == 1),
                         wts = "weights")
tabprep1 <- pred1 %>% mutate(Nickel_1117 = 
                                dplyr::recode(Nickel_1117, 
                                       "1" = "Nickel",
                                       "0" = "Non-mining"), 
                             Analysis = "Nickel") %>% 
   rename("Intervention" = "Nickel_1117", "Estimate" = "estimate") 

col.keep <- c("Estimate",
              "std.error",
              "p.value",
              "conf.low",
              "conf.high",
              "Intervention",
              "Analysis")

tabprep1 <- tabprep1 %>% dplyr::select(all_of(col.keep))

#Other
Other_whole <- readRDS(paste0(data_path, "Other_whole.RDS")) 
#convert poverty score to well-being score 
Other_whole$WBchange1118 <- -(Other_whole$MPIchange1118)*100

tmp.out <- regtest2(Other_whole,
              "WBchange1118",
              "pMINE1117",
              covariates.ot,
              var.exact)

avg_comparisons(tmp.out$fit, variables = "pMINE1117",
                vcov = ~ subclass + id,
                newdata = subset(Other_whole, pMINE1117 == 1),
                wts = "weights")

pred2 <- avg_predictions(tmp.out$fit, variables = "pMINE1117",
                         vcov = ~ subclass + id,
                         wts = "weights")

tabprep2 <- pred2 %>% mutate(pMINE1117 = 
                                dplyr::recode(pMINE1117,
                                       "1" = "Other mining",
                                       "0" = "Non-mining "),
                             Analysis = "Other") %>% 
   rename("Intervention" = "pMINE1117", "Estimate" = "estimate") 

tabprep2 <- tabprep2 %>% dplyr::select(all_of(col.keep))


predall <- rbind(tabprep1,tabprep2)

table.S4 <- as.data.frame(predall) %>% select(c(Estimate,
                                                conf.low,
                                                conf.high,
                                                Intervention, 
                                                Analysis))  

write.csv(table.S4, paste0(output_path, "TabS4.csv"))

# END ----