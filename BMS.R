#bayesian model selectiion scrip
#based on the following citation
# Stephan, Penny, Daunizeau, Moran, and Friston (2009). Bayesian model selection for group studies. NeuroImage, 46(4):1004–1017

#install.packages("remotes")
#remotes::install_github("mattelisi/bmsR")
#remotes::install_github("mattelisi/mlisi") # required for some dependancies
library(bmsR)
library(devtools)
library(readxl)
d <- read_excel("~/Desktop/BehavData/Analyses/ModelFitting/BMS_092522.xlsx")
# input some example data, a matrix [N-by-K] of model evidences
# where N is the number of subjects and K the number of models
#exclude ID column in matrix for BMS
d1= data.matrix(d[2:length(d)])


# run model selection procedure
# VB_bms() is the main function of the package
bms0 <- VB_bms(na.omit(d1))
print(bms0)
# the code output a structure with fields
#names(bms0)

# Dirichlet parameters
# bms0$alpha

# model frequencies
# bms0$r

# exceedance probabilities 
# bms0$xp

# Bayesian omnibus risk (i.e. probability that model differences are due to chance)
# bms0$bor

# protected exceedance probabilities
# bms0$pxp