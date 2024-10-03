#                 Interaction function
# Introduction:
# This script contains the functions to carry out interaction effects 
# The script is already sourced in R script 
# #3. Regressions for figures and tables
# All data will be processed and saved in the "final_output/final_data/" folder

# load packages
library("marginaleffects")
   library("lmtest")
   library("sandwich")

predictInt <- function(Tr, Xind, Yvariate,data, Tr2){
   library("marginaleffects")
   #model
   f <- as.formula(paste(Yvariate,
                         paste0(" ~ ",Tr,"*",Xind,"+",Xind,"^2"),
                         collapse = ""))
   fit <- lm(f, data = data, weights = weights)
   coef.res <- coeftest(fit,
                        vcov. = vcovCL,
                        cluster = ~ subclass + id + kab_code,
                        df = Inf)
   coef.ci <- coefci(fit,
                     vcov. = vcovCL,
                     cluster = ~ subclass + id + kab_code,
                     df = Inf)
   pred.plot <- plot_predictions(fit, condition = c(Tr,Xind),
                                 vcov = ~subclass + id + kab_code,
                                 draw = FALSE, type = "response")
   pred.plot2 <- plot_predictions(fit, condition = c(Tr,Xind),
                                  vcov = ~subclass + id + kab_code,
                                  type = "response") + theme_classic()
   pred.plot3 <- plot_predictions(fit,
                                  condition = list(Xind,Tr),
                                  vcov = ~subclass + id + kab_code) + 
      theme_classic()
   pred.plot4 <- plot_comparisons(fit, variables = Tr,
                                  condition = c(Xind),
                                  vcov = ~subclass + id + kab_code,
                                  rug = TRUE) + 
      theme_classic() + 
      geom_hline(yintercept = 0, colour = "darkgrey", linetype = 2)
   
   return(list(
      fit = fit,
      coef.res = coef.res, #To find the significance of interaction terms
      coef.ci  = coef.ci,  
      pred.plot  = pred.plot, # table of results when covariates held constant
      pred.plot2 = pred.plot2, #Difference by dot plots
      pred.plot3 = pred.plot3,  # Difference by lines
      pred.plot4 = pred.plot4 #comparison (one line)
      
   ))
}

