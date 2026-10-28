# Loadings ----------------------------------------------------------------

library(fda)
library(readr)
library(MFPCA)
library(funData)
library(sandwich)

IPC_raw <- read_csv("Data_Raw/IPC_Alimentos_Ciudades_Base100.csv", 
                    col_types = cols(fecha = col_datetime(format = "%Y-%m-%d")))
load("~/Consultorias/FDA+ECO/FDA+ECO/Data_processed/MFPCA_Temp.RData")

# Processing --------------------------------------------------------------

y_raw <- IPC_raw[,c(1,3)]
y_m <- 100 * diff(log(y_raw$Bogota)) #differences so we lose one

mes <- factor(format(y_raw$fecha[-1], "%m"))

K <- 3
psi = sFPCA_Temp$functions
scores = sFPCA_Temp$scores[-101,c(1:K)]
colnames(scores) <- paste0("xi", 1:K)

dat <- data.frame(y = y_m, mes = mes, scores)

# Temp model 1.3 ----------------------------------------------------------

# Model
pcamodel = lm(y_m ~ mes + xi1 + xi2 + xi3, data = dat)
b <- coef(pcamodel)[paste0("xi", 1:K)]
V <- NeweyWest(pcamodel)[paste0("xi", 1:K), paste0("xi", 1:K)]

# beta_d(s) y su error estandar puntual, por departamento
beta_d <- lapply(psi, function(f) {
  Psi <- f@X[1:K, , drop = FALSE]                    # K x T
  list(s  = f@argvals[[1]],
       b  = as.numeric(b %*% Psi),
       se = sqrt(rowSums((t(Psi) %*% V) * t(Psi))))
})

x11()
par(mfrow = c(3,2))
for (i in 1:5) {
  d <- beta_d[[i]]
  plot(d$s, d$b, type = "l", ylim = range(d$b + 2*d$se, d$b - 2*d$se))
  lines(d$s, d$b + 2*d$se, lty = 2); lines(d$s, d$b - 2*d$se, lty = 2)
}


# Quality -----------------------------------------------------------------

summary(pcamodel)
x11()
plot(pcamodel$fitted.values, y_m, xlab="predicted", ylab = "observed")
abline(a=0,b=1)
