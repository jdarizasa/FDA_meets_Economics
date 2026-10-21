
# Load libraries and data -------------------------------------------------

library(fda)
library(MFPCA)
library(funData)

load("~/Consultorias/FDA+ECO/FDA+ECO/Data_processed/CundiPrecip_fd.RData")
load("~/Consultorias/FDA+ECO/FDA+ECO/Data_processed/BoyacaPrecip_fd.RData")
load("~/Consultorias/FDA+ECO/FDA+ECO/Data_processed/HuilaPrecip_fd.RData")
load("~/Consultorias/FDA+ECO/FDA+ECO/Data_processed/MetaPrecip_fd.RData")
load("~/Consultorias/FDA+ECO/FDA+ECO/Data_processed/TolimaPrecip_fd.RData")

load("~/Consultorias/FDA+ECO/FDA+ECO/Data_processed/CundiTemp_fd.RData")
load("~/Consultorias/FDA+ECO/FDA+ECO/Data_processed/BoyacaTemp_fd.RData")
load("~/Consultorias/FDA+ECO/FDA+ECO/Data_processed/HuilaTemp_fd.RData")
load("~/Consultorias/FDA+ECO/FDA+ECO/Data_processed/MetaTemp_fd.RData")
load("~/Consultorias/FDA+ECO/FDA+ECO/Data_processed/TolimaTemp_fd.RData")


# MfPCA for Temp ---------------------------------------------------------

argvals <- 1:31

CundiTemp_funData <- fd2funData(CundiTempfd, argvals = argvals)
BoyacaTemp_funData <- fd2funData(BoyacaTempfd, argvals = argvals)
HuilaTemp_funData <- fd2funData(HuilaTempfd, argvals = argvals)
MetaTemp_funData <- fd2funData(MetaTempfd, argvals = argvals)
TolimaTemp_funData <- fd2funData(TolimaTempfd, argvals = argvals)

multi_temp <- multiFunData(CundiTemp_funData,
                           BoyacaTemp_funData,
                           HuilaTemp_funData,
                           MetaTemp_funData,
                           TolimaTemp_funData)

# MFPCA based on univariate spline expansions
sFPCA_Temp <- MFPCA(multi_temp, M = 5, uniExpansions = list(list(type = "splines1D", k = 12),
                                                            list(type = "splines1D", k = 12),
                                                            list(type = "splines1D", k = 12),
                                                            list(type = "splines1D", k = 12),
                                                            list(type = "splines1D", k = 12)))

summary(sFPCA_Temp)
plot(sFPCA_Temp) # plot the eigenfunctions as perturbations of the mean
scoreplot(sFPCA_Temp) # plot the scores

save(sFPCA_Temp, file = "Data_processed/MFPCA_Temp.RData")

# MfPCA for Precip ---------------------------------------------------------

argvals <- 1:31

CundiPrecip_funData <- fd2funData(CundiPrecipfd, argvals = argvals)
BoyacaPrecip_funData <- fd2funData(BoyacaPrecipfd, argvals = argvals)
HuilaPrecip_funData <- fd2funData(HuilaPrecipfd, argvals = argvals)
MetaPrecip_funData <- fd2funData(MetaPrecipfd, argvals = argvals)
TolimaPrecip_funData <- fd2funData(TolimaPrecipfd, argvals = argvals)

multi_Precip <- multiFunData(CundiPrecip_funData,
                           BoyacaPrecip_funData,
                           HuilaPrecip_funData,
                           MetaPrecip_funData,
                           TolimaPrecip_funData)

# MFPCA based on univariate spline expansions
sFPCA_Precip <- MFPCA(multi_Precip, M = 5, uniExpansions = list(list(type = "splines1D", k = 12),
                                                            list(type = "splines1D", k = 12),
                                                            list(type = "splines1D", k = 12),
                                                            list(type = "splines1D", k = 12),
                                                            list(type = "splines1D", k = 12)))

summary(sFPCA_Precip)
plot(sFPCA_Precip) # plot the eigenfunctions as perturbations of the mean
scoreplot(sFPCA_Precip) # plot the scores

save(sFPCA_Precip, file = "Data_processed/MFPCA_Precip.RData")
