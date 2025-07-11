# TFG - Modelo Lineal Multivariante


# 1) Cargamos librerías necesarias
library(dplyr)       # Manipulación de datos
library(car)         # Durbin–Watson, vif, etc. si los necesitas más adelante
library(tseries)     # runs.test()
library(stats)       # shapiro.test(), binom.test(), bartlett.test()
library(Metrics)   # MAE y RMSE

# 2) Lectura y limpieza de datos
raw <- read.csv(
  "C:/Users/Iván/Documents/cochenuevo3.csv",
  header = TRUE,
  sep    = ",",
  dec    = ","
)

df <- raw %>%
  rename(
    longitud  = longitud.extra.pontones..mm.,
    alfa_tras = angulo.tras....,
    alfa_del  = angulo.del....,
    Z_force   = Z.Force.3..N.,          # ① renombramos la columna original
    drag      = Y.Force.2..N.
  ) %>%
  mutate(across(
    c(longitud, alfa_tras, alfa_del, Z_force, drag),
    ~ as.numeric(gsub(",", ".", as.character(.)))
  )) %>%
  mutate(
    down = -Z_force                  # ② downforce real (negativo de Z)
  )

# 3) Ajuste de modelos lineales univariantes
lm_down <- lm(down ~ longitud + alfa_tras + alfa_del, data = df)
lm_drag <- lm(drag ~ longitud + alfa_tras + alfa_del, data = df)

# 4) Extracción de residuos y valores ajustados
res_down <- resid(lm_down)
res_drag <- resid(lm_drag)
fit_down <- fitted(lm_down)
fit_drag <- fitted(lm_drag)

# 5) Tests de supuestos modelo completo

## 5.1 Normalidad univariante (Shapiro–Wilk)
sh_down <- shapiro.test(res_down)
sh_drag <- shapiro.test(res_drag)

## 5.2 Mediana cero de los errores (Test de signos)
sign_down <- binom.test(sum(res_down > 0), length(res_down), p = 0.5)
sign_drag <- binom.test(sum(res_drag > 0), length(res_drag), p = 0.5)


## 5.3 Independencia de los errores (Correlación por rangos de Spearman)
spearman_down <- cor.test(res_down[-1], res_down[-length(res_down)], method = "spearman")
spearman_drag <- cor.test(res_drag[-1], res_drag[-length(res_drag)], method = "spearman")

## 5.4 Homocedasticidad (Test de Bartlett)
grp_down <- cut(fit_down,
                breaks = quantile(fit_down, probs = seq(0,1, by = 0.25)),
                include.lowest = TRUE)
grp_drag <- cut(fit_drag,
                breaks = quantile(fit_drag, probs = seq(0,1, by = 0.25)),
                include.lowest = TRUE)
bart_down <- bartlett.test(res_down ~ grp_down)
bart_drag <- bartlett.test(res_drag ~ grp_drag)

# 6) Resumen de resultados
cat("\n--- Shapiro–Wilk ---\n")
print(sh_down)
print(sh_drag)

cat("\n--- Test de signos (binom.test) ---\n")
print(sign_down)
print(sign_drag)

cat("\n--- Test de independencia (correlación por rangos de Spearman entre residuos consecutivos) ---\n")
print(spearman_down)
print(spearman_drag)


cat("\n--- Bartlett (homocedasticidad) ---\n")
print(bart_down)
print(bart_drag)

# 7) Coeficientes y bondad de ajuste
coef_down <- coef(summary(lm_down))
coef_drag <- coef(summary(lm_drag))
r2_down <- summary(lm_down)$r.squared
r2_drag <- summary(lm_drag)$r.squared

cat("\n--- Coeficientes Downforce ---\n")
print(coef_down)
cat("R-squared:", r2_down, "\n")

cat("\n--- Coeficientes Drag ---\n")
print(coef_drag)
cat("R-squared:", r2_drag, "\n")


# 8) Gráficos de diagnóstico para drag


# Colocamos 2x2 gráficos en pantalla
par(mfrow = c(2,2), mar = c(4,4,2,1))

# (1) Residuos vs Ajustados
plot(fit_drag, res_drag,
     main = "Residuos vs Ajustados (drag)",
     xlab = "Valores ajustados",
     ylab = "Residuos",
     pch  = 16)
abline(h = 0, lty = 2, col = "red")

# (2) QQ-plot de residuos
qqnorm(res_drag, main = "QQ-plot residuos (drag)")
qqline(res_drag, col = "blue", lwd = 2)

# (3) Histograma de drag (no de los residuos)
hist(df$drag,
     main   = "Histograma de drag",
     xlab   = "drag (N)",
     breaks = 20,
     col    = "lightgray",
     border = "white")

# (4) Boxplot de drag
boxplot(df$drag,
        main = "Boxplot de drag",
        ylab = "drag (N)",
        col  = "lightblue")

# Restauramos parámetro gráfico por defecto
par(mfrow = c(1,1))


# 9) Gráficos de diagnóstico para downforce


# Disposición 2×2 de gráficos
par(mfrow = c(2,2), mar = c(4,4,2,1))

# (1) Residuos vs Ajustados
plot(fit_down, res_down,
     main = "Residuos vs Ajustados (downforce)",
     xlab = "Valores ajustados",
     ylab = "Residuos",
     pch  = 16)
abline(h = 0, lty = 2, col = "red")

# (2) QQ‐plot de residuos
qqnorm(res_down, main = "QQ‐plot residuos (downforce)")
qqline(res_down, col = "blue", lwd = 2)

# (3) Histograma de downforce
hist(df$down,
     main   = "Histograma de downforce",
     xlab   = "downforce (N)",
     breaks = 20,
     col    = "lightgray",
     border = "white")

# (4) Boxplot de downforce
boxplot(df$down,
        main = "Boxplot de downforce",
        ylab = "downforce (N)",
        col  = "lightblue")

# Restauramos los parámetros gráficos
par(mfrow = c(1,1))



# MODELO REDUCIDO
# 1) Reducimos el dataset
df2 <- df %>% slice_head(n=216)


# 2) Reajustamos los modelos lineales
lm2_down <- lm(down ~ longitud + alfa_tras + alfa_del, data = df2)
lm2_drag <- lm(drag ~ longitud + alfa_tras + alfa_del, data = df2)

# 3) Extraemos residuos y fitted
res2_down <- resid(lm2_down); fit2_down <- fitted(lm2_down)
res2_drag <- resid(lm2_drag); fit2_drag <- fitted(lm2_drag)

# 4) Repetimos los tests 

## 4.1 Shapiro–Wilk
sh2_down <- shapiro.test(res2_down)
sh2_drag <- shapiro.test(res2_drag)

## 4.2 Test de signos
sign2_down <- binom.test(sum(res2_down > 0), length(res2_down), p = 0.5)
sign2_drag <- binom.test(sum(res2_drag > 0), length(res2_drag), p = 0.5)

## 4.3 Independencia de los errores (Spearman)
spearman2_down <- cor.test(res2_down[-1], res2_down[-length(res2_down)], method = "spearman")
spearman2_drag <- cor.test(res2_drag[-1], res2_drag[-length(res2_drag)], method = "spearman")


## 4.4 Bartlett por cuartiles de fitted
grp2_down <- cut(fit2_down, quantile(fit2_down, probs = seq(0,1,0.25)),
                 include.lowest = TRUE)
grp2_drag <- cut(fit2_drag, quantile(fit2_drag, probs = seq(0,1,0.25)),
                 include.lowest = TRUE)
bart2_down <- bartlett.test(res2_down ~ grp2_down)
bart2_drag <- bartlett.test(res2_drag ~ grp2_drag)

# 5) Mostrar resultados
a <- list(
  Shapiro  = list(down = sh2_down, drag = sh2_drag),
  SignTest = list(down = sign2_down, drag = sign2_drag),
  Spearman     = list(down = spearman2_down,  drag = spearman2_drag),
  Bartlett = list(down = bart2_down, drag = bart2_drag)
)
a


# 6) Modelo lineal multivariante (df2: 216 filas)


## Dimensiones y tamaño muestral
p <- 3                     # nº de predictores SIN contar el intercepto
q <- 2                     # nº de respuestas (down, drag)
n <- nrow(df2)             # 216

## Matrices X y Y
X <- model.matrix(~ longitud + alfa_tras + alfa_del, data = df2)   # (n × (p+1))
Y <- as.matrix(df2[, c("down", "drag")])                           # (n × q)

## Estimación por MCO:  B̂ = (X'X)⁻¹ X'Y
B_hat <- solve(t(X) %*% X) %*% t(X) %*% Y
rownames(B_hat) <- colnames(X)           # Intercept, longitud, alfa_tras, alfa_del
colnames(B_hat) <- c("down", "drag")

## Residuos y Σ̂ = E'E / (n – k)
E_hat     <- Y - X %*% B_hat
Sigma_hat <- t(E_hat) %*% E_hat / (n - ncol(X))   # k = p+1 = 4

## Ajuste equivalente con lm() multivariante
mod_mv <- lm(cbind(down, drag) ~ longitud + alfa_tras + alfa_del, data = df2)


# 7) Resultados

cat("\n--- Modelo multivariante (Y = cbind(down, drag)) ---\n")

cat("\nMatriz de coeficientes B̂:\n")
print(round(B_hat, 5))

cat("\nMatriz de covarianzas de los residuos Σ̂:\n")
print(round(Sigma_hat, 5))

cat("\nResumen (lm multivariante):\n")
print(summary(mod_mv))  # da dos resúmenes separados, uno por respuesta


#8) Métricas


## Downforce
mae_down  <- mae(df2$down,  fit2_down)
rmse_down <- rmse(df2$down, fit2_down)
r2_down   <- 1 - sum((df2$down - fit2_down)^2) /
  sum((df2$down - mean(df2$down))^2)

## Drag
mae_drag  <- mae(df2$drag,  fit2_drag)
rmse_drag <- rmse(df2$drag, fit2_drag)
r2_drag   <- 1 - sum((df2$drag - fit2_drag)^2) /
  sum((df2$drag - mean(df2$drag))^2)

cat("\n--- Métricas en df2 (216 observaciones) ---\n")
cat("Downforce:  MAE =", round(mae_down,  4),
    "| RMSE =", round(rmse_down, 4),
    "| R² =", round(r2_down,   4), "\n")

cat("Drag:       MAE =", round(mae_drag,  4),
    "| RMSE =", round(rmse_drag, 4),
    "| R² =", round(r2_drag,   4), "\n")


# 9) Gráficos de diagnóstico para drag


# Ajuste ya hecho: lm2_drag
res2_drag <- resid(lm2_drag)
fit2_drag <- fitted(lm2_drag)

par(mfrow = c(2,2), mar = c(4,4,2,1))

# (1) Residuos vs Ajustados
plot(fit2_drag, res2_drag,
     main = "Residuos vs Ajustados (drag, n = 216)",
     xlab = "Valores ajustados",
     ylab = "Residuos",
     pch  = 16)
abline(h = 0, lty = 2, col = "red")

# (2) QQ‑plot de residuos
qqnorm(res2_drag, main = "QQ‑plot residuos (drag, n = 216)")
qqline(res2_drag, col = "blue", lwd = 2)

# (3) Histograma de drag
hist(df2$drag,
     main   = "Histograma de drag (n = 216)",
     xlab   = "drag (N)",
     breaks = 20,
     col    = "lightgray",
     border = "white")

# (4) Boxplot de drag
boxplot(df2$drag,
        main = "Boxplot de drag (n = 216)",
        ylab = "drag (N)",
        col  = "lightblue")

par(mfrow = c(1,1))  # restaurar

# 10) Gráficos de diagnóstico para downforce

# Ajuste ya hecho: lm2_down
res2_down <- resid(lm2_down)
fit2_down <- fitted(lm2_down)

par(mfrow = c(2,2), mar = c(4,4,2,1))

# (1) Residuos vs Ajustados
plot(fit2_down, res2_down,
     main = "Residuos vs Ajustados (downforce, n = 216)",
     xlab = "Valores ajustados",
     ylab = "Residuos",
     pch  = 16)
abline(h = 0, lty = 2, col = "red")

# (2) QQ‑plot de residuos
qqnorm(res2_down, main = "QQ‑plot residuos (downforce, n = 216)")
qqline(res2_down, col = "blue", lwd = 2)

# (3) Histograma de downforce
hist(df2$down,
     main   = "Histograma de downforce (n = 216)",
     xlab   = "downforce (N)",
     breaks = 20,
     col    = "lightgray",
     border = "white")

# (4) Boxplot de downforce
boxplot(df2$down,
        main = "Boxplot de downforce (n = 216)",
        ylab = "downforce (N)",
        col  = "lightblue")

par(mfrow = c(1,1))  # restaurar

par(mfrow = c(1,1), mar = c(4,4,2,1))

