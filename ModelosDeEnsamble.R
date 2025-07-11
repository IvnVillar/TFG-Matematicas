
# TFG - Modelos predictivos con Random Forest y GBM


# 1) Cargamos librerías necesarias
library(dplyr)         # Manipulación de datos
library(caret)         # Entrenamiento y validación
library(randomForest)  # Random Forest
library(gbm)           # Gradient Boosting Machine 
library(readr)         # Lectura de CSV
library(ggplot2)       # Visualización
library(tibble)        # Mejor manejo de data frames



# 2) Limpieza de datos
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
    Z_force   = Z.Force.3..N.,
    drag      = Y.Force.2..N.
  ) %>%
  mutate(across(c(longitud, alfa_tras, alfa_del, Z_force, drag),
                ~ as.numeric(gsub(",", ".", as.character(.))))) %>%
  mutate(down = -Z_force) %>%   # convertir sustentación (negativa) en downforce positivo
  slice_head(n = 216)


# 3) Configuración de validación cruzada

set.seed(123)
cv_ctrl <- trainControl(
  method           = "cv",
  number           = 10,
  savePredictions  = "final",
  summaryFunction  = defaultSummary
)


# 4) Grids de hiperparámetros


# 4.1 Random Forest
rf_grid <- expand.grid(mtry = c(1, 2, 3))

# 4.2 GBM (Gradient Boosting Machine)
gbm_grid <- expand.grid(
  n.trees           = c(50, 100, 200),
  interaction.depth = c(1, 3, 5),
  shrinkage         = c(0.01, 0.1),
  n.minobsinnode    = 10
)

# 5) Entrenamiento de los modelos

## 5.1 Random Forest –Downforce
set.seed(123)
rf_down <- train(
  down ~ longitud + alfa_tras + alfa_del,
  data      = df,
  method    = "rf",
  metric    = "RMSE",
  tuneGrid  = rf_grid,
  trControl = cv_ctrl,
  importance = TRUE,
  ntree     = 500
)

## 5.2 Random Forest –Drag
set.seed(123)
rf_drag <- train(
  drag ~ longitud + alfa_tras + alfa_del,
  data      = df,
  method    = "rf",
  metric    = "RMSE",
  tuneGrid  = rf_grid,
  trControl = cv_ctrl,
  importance = TRUE,
  ntree     = 500
)

## 5.3 GBM –Downforce
set.seed(123)
gbm_down <- train(
  down ~ longitud + alfa_tras + alfa_del,
  data      = df,
  method    = "gbm",
  metric    = "RMSE",
  tuneGrid  = gbm_grid,
  trControl = cv_ctrl,
  verbose   = FALSE
)

## 5.4 GBM –Drag
set.seed(123)
gbm_drag <- train(
  drag ~ longitud + alfa_tras + alfa_del,
  data      = df,
  method    = "gbm",
  metric    = "RMSE",
  tuneGrid  = gbm_grid,
  trControl = cv_ctrl,
  verbose   = FALSE
)


# 6) Función para extraer y normalizar importancia de variables

get_var_importance <- function(model, model_type, target) {
  imp <- varImp(model)$importance              # importancia cruda
  imp_df <- as.data.frame(imp)
  imp_df$variable <- rownames(imp_df)
  colnames(imp_df)[1] <- "Importance"
  imp_df$Importance <- imp_df$Importance / max(imp_df$Importance)  # normalizar
  imp_df$model  <- model_type
  imp_df$target <- target
  imp_df %>% select(variable, Importance, model, target)
}


# 7) Extraer y unificar todas las importancias

vi_rf_down  <- get_var_importance(rf_down,  "RF",  "Downforce")
vi_rf_drag  <- get_var_importance(rf_drag,  "RF",  "Drag")
vi_gbm_down <- get_var_importance(gbm_down, "GBM", "Downforce")
vi_gbm_drag <- get_var_importance(gbm_drag, "GBM", "Drag")

vi_all <- bind_rows(vi_rf_down, vi_rf_drag, vi_gbm_down, vi_gbm_drag)


# 8) Gráficos de importancia relativa


## 8.1 Downforce
ggplot(filter(vi_all, target == "Downforce"),
       aes(x = reorder(variable, Importance), y = Importance, fill = model)) +
  geom_col(position = "dodge") +
  labs(
    title    = "Importancia relativa – Downforce",
    subtitle = "Valores normalizados (0 a 1)",
    x        = "Variable",
    y        = "Importancia relativa"
  ) +
  theme_minimal()

## 8.2 Drag
ggplot(filter(vi_all, target == "Drag"),
       aes(x = reorder(variable, Importance), y = Importance, fill = model)) +
  geom_col(position = "dodge") +
  labs(
    title    = "Importancia relativa – Drag",
    subtitle = "Valores normalizados (0 a 1)",
    x        = "Variable",
    y        = "Importancia relativa"
  ) +
  theme_minimal()


# 9) Mostrar mejores parámetros ajustados

cat("\nMejor configuración Random Forest:\n")
print(rf_down$bestTune)
print(rf_drag$bestTune)

cat("\nMejor configuración GBM:\n")
print(gbm_down$bestTune)
print(gbm_drag$bestTune)


get_best_row <- function(model, id) {
  best  <- model$bestTune
  res   <- model$results
  
  # Coincidir todas las columnas que existan en bestTune
  idx <- apply(res[, names(best), drop = FALSE], 1,
               function(x) all(x == best[1, names(best)]))
  res_best <- res[idx, ][1, ]   # primera coincidencia
  tibble(
    Modelo = id,
    RMSE   = res_best$RMSE,
    MAE    = res_best$MAE,
    R2     = res_best$Rsquared
  )
}

#Extraer métricas para cada modelo
tbl_metrics <- bind_rows(
  get_best_row(rf_down,  "RF–Downforce"),
  get_best_row(rf_drag,  "RF-Drag"),
  get_best_row(gbm_down, "GBM–Downforce"),
  get_best_row(gbm_drag, "GBM–Drag")
)

# Mostrar en consola
print(tbl_metrics)




