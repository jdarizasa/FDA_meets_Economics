
# Loadings ----------------------------------------------------------------

library(fda)
library(readr)

load("~/Consultorias/FDA+ECO/FDA+ECO/Data_processed/CundiPrecip_fd.RData")
load("~/Consultorias/FDA+ECO/FDA+ECO/Data_processed/CundiTemp_fd.RData")
IPC_raw <- read_csv("Data_Raw/IPC_Alimentos_Ciudades_Base100.csv", 
                    col_types = cols(fecha = col_datetime(format = "%Y-%m-%d")))


# Processing --------------------------------------------------------------

y_raw <- IPC_raw[,c(1,3)]
y_m <- 100 * diff(log(y_raw$Bogota)) #differences so we lose one

nombres_curvas <- CundiTempfd$fdnames$reps[-101]
mes <- as.integer(sub(".*-", "", nombres_curvas))
mes_factor <- factor(mes, levels = 1:12)
table(mes_factor)
dummies <- model.matrix(~ mes_factor)[, -1]

# Temp Model 1.1 -----------------------------------------------------------

plot(CundiTempfd)

  # X(t)
templist = vector("list", 2+ncol(dummies))
templist[[1]] <- rep(1, 100)            # α (intercepto)
templist[[2]] <- CundiTempfd[-101]      # β(s) predictor funcional
for (j in 1:ncol(dummies)) {
  templist[[2 + j]] <- dummies[, j]     # γ_m: 11 dummies mensuales
}

  # Basis Coefficient 1
conbasis = create.constant.basis(c(0,31))
betabasis = create.bspline.basis(c(0, 31), nbasis = 25)
betafdPar = fdPar(betabasis, lambda = 10^6)

betalist = vector("list",length(templist))
betalist[[1]] <- conbasis               # α constante
betalist[[2]] <- betafdPar              # β(s) suavizado
for (j in 1:ncol(dummies)) {
  betalist[[2 + j]] <- conbasis         # cada dummy con coeficiente constante
}

  # Model 1.1
IPC_Temp = fRegress(y_m, templist, betalist)

  # Betas estimate
betaestlist = IPC_Temp$betaestlist
  # \hat(y)
IPC_prec_1.1 = IPC_Temp$yhatfdobj

plot(IPC_prec_1.1, y_m, xlab="predicted", ylab = "observed")
abline(a=0,b=1)

plot(betaestlist[[2]]$fd, xlab="Day", ylab="Beta for temperature")
