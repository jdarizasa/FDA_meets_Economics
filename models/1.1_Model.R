
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
IPC_Temp_m11 = fRegress(y_m, templist, betalist)

  # Betas estimate
betaestlist = IPC_Temp_m11$betaestlist
  # \hat(y)
IPC_prec_1.1 = IPC_Temp_m11$yhatfdobj

plot(IPC_prec_1.1, y_m, xlab="predicted", ylab = "observed")
abline(a=0,b=1)

plot(betaestlist[[2]]$fd, xlab="Day", ylab="Beta for temperature")


# Quality Model 1.1 -------------------------------------------------------

SSE0 = sum((y_m - mean(y_m))^2) #Null model
SSE1.1 = sum((y_m-IPC_prec_1.1)^2)
(RSQ1.1 = (SSE0 - SSE1.1)/SSE0)
(Fratio1.1 = ((SSE0-SSE1.1)/(IPC_Temp_m11$df-1))/(SSE1.1/(100 - IPC_Temp_m11$df)))
(pval_1.1   <- pf(Fratio1.1, IPC_Temp_m11$df-1, 100 - IPC_Temp_m11$df, lower.tail = FALSE))
