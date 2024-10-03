#                 #1. Matching procedure and process
# Introduction:
# This R script carries out the statistical 
# matching process and matching output.
# All data for matching is sourced from the folder "gm_data/". 
# All data will be processed and saved in the "gm_output/" folder.
# Note: Statistical matching requires high computational power
#     - To skip this step, move to R script #2 and used the prepared data 
#     - which can be found in the folder (prepared_data/)

#Prepare work space----
# Clean the working environment
rm(list=ls())

#load packages
library("MatchIt")
library("cobalt")
library("rbounds")
library("janitor")
library("dplyr")
library("ggplot2")

# If not using the RStudio project, set working directory to the repository
# directory. 
# setwd("../")

#get data 
gm_file_path <- c("gm_data/") 

# Nidata contains village observations, identifying villages that overlap
# with nickel mining areas, and villages with 
# no overlap with any mining concessions between 2000 and 2018.
# data also contain covariates used for the matching process.
Nidata <- readRDS(paste0(gm_file_path,"Nidata_prep.RDS"))
Nidata <- Nidata %>% as.data.frame() 

# OTdata contains village observations, identifying villages that overlap
# with other mining areas, and villages with 
# no overlap with any mining concessions between 2000 and 2018.
# The data also exclude villages where both nickel and other mining areas
# overlap
# data also contain covariates used for the matching process.
OTdata <- readRDS(paste0(gm_file_path,"OTdata_prep.RDS"))
OTdata <- OTdata %>% as.data.frame() 

# define covariates for matching 
# Continuous variables for matching
## Covariates used for matching nickel mining with non-mining villages
covariates.ni <- c("LOG_ELEV", "LOG_SLOPE", "LOG_ACCESS","LOG_AREA",
                   "LOG_popkm", "DRYMEAN", "WETMEAN","FOR11_pt",
                   "pt_APL","pt_PRODFOR", "pt_HL")

## Covariates used for matching other mining with non-mining villages
covariates.ot <- c("LOG_ELEV", "LOG_SLOPE", "LOG_ACCESS","LOG_AREA", 
                   "LOG_popkm", "DRYMEAN", "WETMEAN","FOR11_pt","pt_APL",
                   "pt_PRODFOR", "pt_HL","pt_Lstone"  ,"pt_volcanic")

# Categorical/exact variables for matching
var.exact <- c("MPIBASE_CLS", "LTYPE.11", "prov_code")
## Categorical/exact variables for matching by poverty baseline condition
var.exactPOV <- c("LTYPE.11", "prov_code")

# Define treatment variables
# Treatment variables are binary. 0 represents no overlap with mining concession
# 1 represents overlap with mining concession 
# Nickel = Nickel mining concession overlap
# pMINE = Other mining concession overlap
# 1117 = mining concession established 2011-2017
# 1114 = mining concession established 2011-2014
# 1417 = mining concession established 2014-2017
Treatvar <- c("Nickel_1117", "Nickel_1114", "Nickel_1417", 
              "pMINE1117", "pMINE1417", "pMINE1114")

#convert categorical variables to factors
col_names <- OTdata %>% select(c("prov_code", "kab_code"),
                               paste0(Treatvar)) %>% names()
OTdata[col_names] <- lapply(OTdata[col_names] , as.factor)

col_names <- Nidata %>% select(c("prov_code", "kab_code"),
                               paste0(Treatvar)) %>% names()
Nidata[col_names] <- lapply(Nidata[col_names] , as.factor)

# Create Genetic matching function ----
# Set class - creates a class definition for objects 
# generated from the Gmat function (see below)
setClass("Gmat", slots=c(NN = "matrix",
                         plot = "ANY",
                         CovBal = "matrix",
                         Matched = "data.frame",
                         Mout = "ANY"
                         ))

# Set function - The Gmat function generates objects resulting from the genetic
# matching procedure.
# TrM - the treatment variables
# xmat - the unmatched data with village observations
# covars - the continuous variables used to match villages with similar
# characteristics
# varex - the factor variables used to match villages with similar 
# characteristics 
Gmat <- function(TrM, xmat, covars,varex){
   m.outgen <- matchit(as.formula(paste0(TrM,"~",paste0(covars, collapse='+'))), 
                       data = xmat, 
                       exact = varex, 
                       method = "genetic", 
                       distance = "glm", 
                       link = "logit", 
                       estimand = "ATT", 
                       pop.size = 1000, 
                       replace = TRUE, 
                       discard = "control",
                       reestimate = TRUE)
   #Plot covariates balance
   plot <- love.plot(m.outgen,  poly = 1, abs = FALSE,
                     estimand = "ATT",
                     stats = c("mean.diffs"),
                     binary = "std",
                     drop.distance = FALSE, 
                     thresholds = c(m =0.2),
                     var.order = "unadjusted",
                     line = TRUE,
                     title = paste0("Treatment: ", TrM))
   #Create matched table of results
   matchedtab <- summary(m.outgen)
   mtab <- cbind(matchedtab$sum.all[,1], 
                 matchedtab$sum.all[,2], 
                 matchedtab$sum.matched[,1], 
                 matchedtab$sum.matched[,2],
                 matchedtab$sum.matched[,3],
                 matchedtab$sum.matched[,6])
   mtab <- round(mtab, digits = 3)
   colnames(mtab) <- c("Mean treat - Unmatched",
                       "Mean control - Unmatched",
                       "Mean treat - Matched",
                       "Mean control - Matched",
                       "SMD",
                       "eCDF Max")
   obtab <- matchedtab$nn
   # Get matched dataset
   Matched.dt <- as.data.frame(get_matches(m.outgen))
   result <- new("Gmat",
                 NN=obtab,
                 Matched = Matched.dt, 
                 plot = plot,
                 CovBal=mtab,
                 Mout = m.outgen
                 )
   # Return the object
   return(result)
   
}   

# Set Method- creates a method for the Gmat function, calling objects to 
# the classes, as outlined above.  
setMethod("summary", signature(object="Gmat"),
          definition=function(object, ...){
             NN <- object@NN
             Matched <- object@Matched
             plot <- object@plot
             CovBal <- object@CovBal
             Mout <- object@Mout
          }
)

# Set directories to store output files ---
dir.create("gm_output/")
dir.create("gm_output/nn/")                  # no. of units matched 
dir.create("gm_output/loveplots/")           # loveplots for covariate check
dir.create("gm_output/covbal/")              # covariate balance check
dir.create("gm_output/matched-data/")        # folder for matched samples
dir.create("gm_output/matched-process/")     # folder for matched process

nn_dir <- c("gm_output/nn/")
plot_dir <- c("gm_output/loveplots/")        # loveplots for covariate check
covbal_dir <- c("gm_output/covbal/")         # covariate balance check
mat_dir <- c("gm_output/matched-data/")      # folder for matched samples
mout_dir <- c("gm_output/matched-process/")  # folder for matched process

      # Statistical matching - Nickel vs no mines ----

## Nickel - Non Mine 1-7yr period (2011-2018) ====
TrM <- c("Nickel_1117")                      # Defines treatment variable
treat <- Nidata %>% subset(Nickel_1117 == 1) # Selects treatment villages
control <- Nidata %>% subset(pMINE1117 == 0) # Selects control villages
xmatt <- rbind(treat, control)               # combines treatment-control
                                             # villages
matgen <- Gmat(TrM, xmatt,covariates.ni, var.exact) # Runs genetic matching
                                                    # function

# save data on number of villages matches
saveRDS(matgen@NN,paste0(nn_dir, "Nickel_whole.RDS"))           
# loveplots to check covariate balance across variables 
ggsave(plot = matgen@plot,paste0(plot_dir, "Nickel_whole.png"), 
       width = 10, height = 7, dpi = 300)
# save covariate balance results in table form 
saveRDS(matgen@CovBal,paste0(covbal_dir,"Nickel_whole.RDS"))
# saves matched data as data frame
saveRDS(matgen@Matched, paste0(mat_dir, "Nickel_whole.RDS"))
# saves Matched data, also containing matching procedure details 
saveRDS(matgen@Mout, paste0(mout_dir, "Nickel_whole.RDS"))

## Nickel - Non Mine 1-3yr period (2011-2014) ====
TrM <- c("Nickel_1114")
treat <- Nidata %>% subset(Nickel_1114 == 1)
control <- Nidata %>% subset(pMINE1117 == 0)
xmatt <- rbind(treat, control) 
matgen <- Gmat(TrM, xmatt, covariates.ni,var.exact)

saveRDS(matgen@NN,paste0(nn_dir, "Nickel_short1.RDS"))
ggsave(plot = matgen@plot,paste0(plot_dir, "Nickel_short1.png"),
       width = 10, height = 7, dpi = 300)
saveRDS(matgen@CovBal,paste0(covbal_dir,"Nickel_short1.RDS"))
saveRDS(matgen@Matched, paste0(mat_dir, "Nickel_short1.RDS"))

## Nickel - Non Mine 1-3yr period (2014-2018) ====
TrM <- c("Nickel_1417")
treat <- Nidata %>% subset(Nickel_1417 == 1 & pMINE1114 != 1)
control <- Nidata %>% subset(pMINE1117 == 0)
xmatt <- rbind(treat, control) 
matgen <- Gmat(TrM, xmatt, covariates.ni,var.exact)

saveRDS(matgen@NN,paste0(nn_dir, "Nickel_short2.RDS"))
ggsave(plot = matgen@plot,paste0(plot_dir, "Nickel_short2.png"),
       width = 10, height = 7, dpi = 300)
saveRDS(matgen@CovBal,paste0(covbal_dir,"Nickel_short2.RDS"))
saveRDS(matgen@Matched, paste0(mat_dir, "Nickel_short2.RDS"))

## Nickel - Non Mine 4-7yr period (2014-2018) ====
TrM <- c("Nickel_1114")
treat <- Nidata %>% subset(Nickel_1114 == 1 & pMINE1417!=1)
control <- Nidata %>% subset(pMINE1117 == 0)
xmatt <- rbind(treat, control) 
matgen <- Gmat(TrM, xmatt, covariates.ni,var.exact)

saveRDS(matgen@NN,paste0(nn_dir, "Nickel_long.RDS"))
ggsave(plot = matgen@plot,paste0(plot_dir, "Nickel_long.png"),
       width = 10, height = 7, dpi = 300)
saveRDS(matgen@CovBal,paste0(covbal_dir,"Nickel_long.RDS"))
saveRDS(matgen@Matched, paste0(mat_dir, "Nickel_long.RDS"))

      #Statistical matching - Other mines vs no mines ----
      
## Mine non-mine 7 yr period ====
TrM <- c("pMINE1117")
treat <- OTdata %>% subset(pMINE1117 == 1)
control <- OTdata %>% subset(pMINE1117 == 0)
xmatt <- rbind(treat, control)
matgen <- Gmat(TrM, xmatt, covariates.ot,var.exact)

saveRDS(matgen@NN,paste0(nn_dir, "Other_whole.RDS"))
ggsave(plot = matgen@plot, paste0(plot_dir, "Other_whole.png"),
       width = 10, height = 7, dpi = 300)
saveRDS(matgen@CovBal, paste0(covbal_dir, "Other_whole.RDS"))
saveRDS(matgen@Matched, paste0(mat_dir, "Other_whole.RDS"))
saveRDS(matgen@Mout, paste0(mout_dir, "Other_whole.RDS"))

## Mine non-mine 1-3 yr period (2011-2014) ====
TrM <- c("pMINE1114")
treat <- OTdata %>% subset(pMINE1114==1)
control <- OTdata %>% subset(pMINE1117 == 0)
xmatt <- rbind(treat, control)
matgen <- Gmat(TrM, xmatt, covariates.ot,var.exact)

saveRDS(matgen@NN,paste0(nn_dir, "Other_short1.RDS"))
ggsave(plot = matgen@plot, paste0(plot_dir, "Other_short1.png"),
       width = 10, height = 7, dpi = 300)
saveRDS(matgen@CovBal,paste0(covbal_dir, "Other_short1.RDS"))
saveRDS(matgen@Matched, paste0(mat_dir, "Other_short1.RDS"))

## Mine non-mine 1-3 yr period (2014-2018) ====
TrM <- c("pMINE1417")
treat <- OTdata %>% subset(pMINE1417==1 & pMINE1114!=1)
control <- OTdata %>% subset(pMINE1117 == 0)
xmatt <- rbind(treat, control)
matgen <- Gmat(TrM, xmatt, covariates.ot,var.exact)

saveRDS(matgen@NN,paste0(nn_dir,"Other_short2.RDS"))
ggsave(plot = matgen@plot,paste0(plot_dir, "Other_short2.png"),
       width = 10, height = 7, dpi = 300)
saveRDS(matgen@CovBal,paste0(covbal_dir,"Other_short2.RDS"))
saveRDS(matgen@Matched, paste0(mat_dir, "Other_short2.RDS"))

## Mine non-mine (4-7 yr period) (2014-2018))====
TrM <- c("pMINE1114")
treat <- OTdata %>% subset(pMINE1114==1 & pMINE1417 != 1)
control <- OTdata %>% subset(pMINE1117 == 0)
xmatt <- rbind(treat, control)
matgen <- Gmat(TrM, xmatt, covariates.ot,var.exact)

saveRDS(matgen@NN,paste0(nn_dir, "Other_long.RDS"))
ggsave(plot = matgen@plot,paste0(plot_dir, "Other_long.png"),
       width = 10, height = 7, dpi = 300)
saveRDS(matgen@CovBal,paste0(covbal_dir,"Other_long.RDS"))
saveRDS(matgen@Matched, paste0(mat_dir, "Other_long.RDS"))

         #Statistical matching using poverty baselines      ----
         
##Nickel - Non Mine 1-3 years (period 1)====
###Low Pov. ####
TrM <- c("Nickel_1114")
treat <- Nidata %>% subset(Nickel_1114 == 1 & MPIBASE_CLS2 == "Low")
control <- Nidata %>% subset(pMINE1117 == 0)
xmatt <- rbind(treat, control) 
matgen <- Gmat(TrM, xmatt, covariates.ni,var.exact)

saveRDS(matgen@NN,paste0(nn_dir, "Nickel_short1_Low.RDS"))
ggsave(plot = matgen@plot,paste0(plot_dir, "Nickel_short1_Low.png"),
       width = 10, height = 7, dpi = 300)
saveRDS(matgen@CovBal,paste0(covbal_dir,"Nickel_short1_Low.RDS"))
saveRDS(matgen@Matched, paste0(mat_dir, "Nickel_short1_Low.RDS"))

### High Pov. ####
TrM <- c("Nickel_1114")
treat <- Nidata %>% subset(Nickel_1114 == 1 & MPIBASE_CLS2 == "High")

control <- Nidata %>% subset(pMINE1117 == 0)
xmatt <- rbind(treat, control) 
matgen <- Gmat(TrM, xmatt, covariates.ni,var.exact)

saveRDS(matgen@NN,paste0(nn_dir, "Nickel_short1_High.RDS"))
ggsave(plot = matgen@plot,paste0(plot_dir, "Nickel_short1_High.png"),
       width = 10, height = 7, dpi = 300)
saveRDS(matgen@CovBal,paste0(covbal_dir,"Nickel_short1_High.RDS"))
saveRDS(matgen@Matched, paste0(mat_dir, "Nickel_short1_High.RDS"))

## Nickel - Non Mine 1-3yr period (2014-2018) ====
### Low Pov.####
TrM <- c("Nickel_1417")
treat <- Nidata %>% 
   subset(Nickel_1417 == 1 & pMINE1114 != 1 & MPIBASE_CLS2 =="Low")
control <- Nidata %>% subset(pMINE1117 == 0)
xmatt <- rbind(treat, control) 
matgen <- Gmat(TrM, xmatt, covariates.ni,var.exact)

saveRDS(matgen@NN,paste0(nn_dir, "Nickel_short2_Low.RDS"))
ggsave(plot = matgen@plot,paste0(plot_dir, "Nickel_short2_Low.png"),
       width = 10, height = 7, dpi = 300)
saveRDS(matgen@CovBal,paste0(covbal_dir,"Nickel_short2_Low.RDS"))
saveRDS(matgen@Matched, paste0(mat_dir, "Nickel_short2_Low.RDS"))

###High Pov. ####
TrM <- c("Nickel_1417")
treat <- Nidata %>% 
   subset(Nickel_1417 == 1 & pMINE1114 != 1 & MPIBASE_CLS2=="High")
control <- Nidata %>% subset(pMINE1117 == 0)
xmatt <- rbind(treat, control) 
matgen <- Gmat(TrM, xmatt, covariates.ni,var.exact)

saveRDS(matgen@NN,paste0(nn_dir, "Nickel_short2_High.RDS"))
ggsave(plot = matgen@plot,paste0(plot_dir, "Nickel_short2_High.png"),
       width = 10, height = 7, dpi = 300)
saveRDS(matgen@CovBal,paste0(covbal_dir,"Nickel_short2_High.RDS"))
saveRDS(matgen@Matched, paste0(mat_dir, "Nickel_short2_High.RDS"))

## Nickel - Non Mine 4-7yr period (2014-2018) ====
### Low Pov. ####
TrM <- c("Nickel_1114")
treat <- Nidata %>% subset(Nickel_1114 == 1 & 
                              pMINE1417 != 1 & 
                              MPIBASE_CLS2 == "Low")
control <- Nidata %>% subset(pMINE1117 == 0)
xmatt <- rbind(treat, control) 
matgen <- Gmat(TrM, xmatt, covariates.ni,var.exact)

saveRDS(matgen@NN,paste0(nn_dir, "Nickel_long_Low.RDS"))
ggsave(plot = matgen@plot,paste0(plot_dir, "Nickel_long_Low.png"),
       width = 10, height = 7, dpi = 300)
saveRDS(matgen@CovBal,paste0(covbal_dir,"Nickel_long_Low.RDS"))
saveRDS(matgen@Matched, paste0(mat_dir, "Nickel_long_Low.RDS"))

###High Pov. ####
TrM <- c("Nickel_1114")
treat <- Nidata %>% subset(Nickel_1114 == 1 & 
                              pMINE1417 != 1 & 
                              MPIBASE_CLS2 == "High")
control <- Nidata %>% subset(pMINE1117 == 0)
xmatt <- rbind(treat, control) 
matgen <- Gmat(TrM, xmatt, covariates.ni,var.exact)

saveRDS(matgen@NN,paste0(nn_dir, "Nickel_long_High.RDS"))
ggsave(plot = matgen@plot,paste0(plot_dir, "Nickel_long_High.png"),
       width = 10, height = 7, dpi = 300)
saveRDS(matgen@CovBal,paste0(covbal_dir,"Nickel_long_High.RDS"))
saveRDS(matgen@Matched, paste0(mat_dir, "Nickel_long_High.RDS"))

# END
