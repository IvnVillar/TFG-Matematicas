# 📚 Trabajo Fin de Grado: Técnicas estadísticas para el análisis de datos relativos al modelado de piezas de Fórmula 1

> **Autor:** Iván Villar  
> **Universidad:** Universidad de Oviedo 
> **Fecha:** 11/07/2025

## 🧾 Descripción del Proyecto

Este repositorio contiene los materiales y código desarrollados durante mi **Trabajo Fin de Grado (TFG)** en Matemáticas, centrado en el uso de **técnicas estadísticas para el análisis de datos aerodinámicos** aplicados al modelado de piezas de Fórmula 1.  

El objetivo principal es comparar distintas técnicas estadísticas y de aprendizaje automático para predecir variables clave como **drag** (resistencia aerodinámica) y **downforce** (fuerza hacia abajo generada por el coche a alta velocidad), basadas en simulaciones realizadas con el software **ANSYS** donde se variaron parámetros aerodinámicos.

---

## 📊 Dataset

- El archivo `cochenuevo3.csv` contiene los datos obtenidos mediante simulaciones CFD (*Computational Fluid Dynamics*) usando ANSYS.
- Este dataset incluye:
  - Valores de los parámetros geométricos y aerodinámicos utilizados.
  - Resultados de las simulaciones: drag, downforce y otras variables derivadas.

---

## 🛠️ Métodos Utilizados

Se han aplicado y comparado tres técnicas principales:

### 1. **Modelo Lineal Multivariante**
- Implementado en R (`ModeloRegresionMultivariante.R`)
- Se estudia la relación lineal entre los parámetros de entrada y las variables de salida (drag y downforce).

### 2. **Random Forest**
- Implementado en R (`ModelosDeEnsamble.R`)
- Modelo de ensamble no lineal basado en árboles de decisión.

### 3. **Gradient Boosting**
- También implementado en R (`ModelosDeEnsamble.R`)
- Otra técnica de ensamble que mejora iterativamente el modelo base.

---


## 🎯 Objetivos del TFG

- Comparar el rendimiento predictivo de diferentes técnicas estadísticas y de machine learning.
- Estudiar teóricamente cada una de las técnicas utilizadas
- Evaluar qué tipo de modelo se ajusta mejor a los datos aerodinámicos complejos.

---

## 📚 Referencias y Material Adicional

Si necesitas acceder al documento completo del TFG (memoria, gráficos, conclusiones), puedes contactarme a través de [LindedIn](https://www.linkedin.com/in/ivan-villar-naredo/)

---

## ✅ Requisitos para Ejecutar el Código

- R (versión >= 4.0)
- Librerías utilizadas:
  - `caret`
  - `randomForest`
  - `gbm` 
  - 'dplyr'      
  - 'car'
  - 'tseries'
  - 'stats'
  - 'Metrics'
  - 'dplyr'
  - 'readr'
  - 'ggplot2'
  - 'tibble'     

---

## 🤝 Contacto

¿Tienes dudas sobre el proyecto? ¿Quieres colaborar?

🐙 GitHub: [@IvnVillar](https://github.com/IvnVillar )
