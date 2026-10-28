# Métodos

# Model 1
Para estos modelos se definirá la variable escalar de respuesta como $y_m = 100 \cdot (ln (IPC_m) − ln (IPC_{m−1}))$.
Para $\gamma_m$ es una variable dummy de mes

## Model 1.1

$$
y_m = \alpha_c + \gamma_m + \int_0^K \beta(s) X_{m-1}(s) ds + \epsilon_m
$$

## Model 1.2

$$
y_m = \alpha_c + \gamma_m + \int_0^K \beta_1(s) X_{c,m-1}(s) ds + \int_0^K \beta_2(s) X_{v,m-1}(s) ds + \epsilon_m
$$

## Model 1.3

$$
y_m = \alpha + \gamma_m + \beta_{c,m} \eta_{c,m-1} + \epsilon_m
$$

$\eta_{c,m-1}$ son los scores del departamento c del multivariate fPCA, un script aparte será destinada a la estimación de este método
