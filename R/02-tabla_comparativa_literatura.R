tabla_comp_lit <- function() {
  library(kableExtra)
  
  tabla_literatura <- data.frame(
    "Fuente" = c(
      "López-Montenegro et al. (2019)",
      "Eastin et al. (2014)",
      "Zhao et al. (2020)",
      "Martinez et al. (2011)",
      "Gharbi et al. (2011)",
      "Chen y Moraga (2025)",
      "Navarro Valencia et al. (2021)",
      "Hasan et al. (2024)"
    ),
    "Datos" = c(
      "Casos confirmados de dengue y dengue grave, por semana epidemiológica en Colombia (2009-2017).",
      "Casos de dengue confirmados, quincenal y mensual (2001-2011), y datos meteorológicos en Cali, Colombia.",
      "Casos de dengue por semana y departamento en Colombia (2014-2018), EVI, población, lluvias, índice Gini y cobertura educativa.",
      "Casos de dengue en Campinas, São Paulo, Brasil por mes (1998-2008).",
      "Casos semanales de dengue, temperaturas, humedad relativa y precipitación en la isla de Guadalupe (2000-2006).",
      "Registros semanales de casos de dengue y datos meteorológicos del área metropolitana de Río de Janeiro (2016-2023).",
      "Casos confirmados de dengue semanales y variables meteorológicas de la Región Metropolitana de Ciudad de Panamá (1999-2017).",
      "Casos de dengue mensuales en Bangladesh entre enero de 2008 y septiembre de 2023."
    ),
    "Método" = c(
      "ARIMA(5,1,1). No se usa referente.",
      "ARIMA. Referentes: AR sin variables climáticas.",
      "RF y redes neuronales artificiales. El referente fue un modelo ARIMA.",
      "SARIMA(2,1,1)(1,1,1)12. Sin referente.",
      "SARIMA(0,1,1) multivariado. Referente: SARIMA univariado.",
      "AR, MA, ARIMA, ETS, SARIMAX, VAR en contraste con RF, XGBoost, SVM, LSTM y Prophet.",
      "SARIMA, RNN-LSTM y SARIMAX. El referente es SARIMA univariado.",
      "ARIMA(1,1,1) y SARIMA(1,1,2)(1,1,2)12. Sin referente reportado."
    ),
    "Horizonte" = c(
      "5 años o 260 semanas (del 2018 al 2022).",
      "Pronósticos intraestacionales (quincenales) e interestacionales (mensuales).",
      "1 a 12 semanas.",
      "Mensual, 12 meses fuera de muestra.",
      "4, 13 y 52 semanas para el año 2007.",
      "1, 2, 3, 4, 8 y 12 semanas.",
      "3 años o 156 semanas.",
      "12 meses."
    ),
    "Métricas" = c(
      "RMSE.",
      "MAE",
      "MAE (ARIMA).",
      "No reporta.",
      "RMSE.",
      "MAE, MAPE y RMSE.",
      "RMSE y MAPE.",
      "MAE, RMSE."
    ),
    "Resultados" = c(
      "Proyectó el mayor número de casos para 2019 (88.221) y estimó un número básico de reproducción de 1.73, 2.03, 1.02 y 1.40 para cada trimestre de ese año. Al no tener método referente, no reporta comparación de precisión predictiva.",
      "Los predictores climáticos mejoran la predicción, sobre todo en los brotes severos, donde los modelos AR subestimaban la incidencia. Quincenal: R² de 0.884 a 0.928 y AIC de 134 a 131 al agregar el clima. Mensual: R² de 0.846 a 0.901 y AIC de 92 a 76. En 2011, MAE de 0.61 (quincenal) y 1.4 (mensual).",
      "Los predictores ambientales fueron mejores a corto plazo, los sociodemográficos para largo plazo. El mejor método fue RF, MAE nacional de 9.32 (1 semana) a 24.56 (12 semanas).",
      "El modelo SARIMA(2,1,2)(1,1,1)12 fue el de mejor ajuste (menor AIC). No reportan métricas de error, cobertura empírica ni pruebas como Diebold y Mariano.",
      "La inclusión de la temperatura mínima incrementó significativamente la capacidad predictiva del modelo a mediano plazo (13 semanas). RMSE de 0.98 (52 sem.), 0.85 (13 sem.) y 0.76 (4 sem.).",
      "Sin covariables, ARIMA ganó (RMSE: 124.33 a 1 sem; 277.48 a 4 sem; 583.04 a 12 sem). Con covariables, LSTM dominó (RMSE: 101.53 a 1 sem; 223.35 a 4 sem; 573.32 a 12 sem). A 12 semanas, Prophet fue superior (MAE: 342.47). La cobertura al 95% varió entre 86.54% y 92.31% para ARIMA, y entre 75.96% y 87.50% para LSTM (univariado). Sin prueba de Diebold-Mariano.",
      "RMSE de 25.83 (SARIMA), 25.76 (SARIMAX) y 26.16 (LSTM). MAPE de 112.70, 108.44 y 59.68. Los modelos sobrestiman 2015-2016 y subestiman 2017.",
      "El modelo SARIMA(1,1,2)(1,1,2)12 logró mayor precisión (MAE=1246.4, RMSE=5175.8) frente al ARIMA(1,1,1) (MAE=1266.1, RMSE=5343.2)."
    ),
    check.names = FALSE
  )
  
  tabla_literatura %>%
    kbl(
      caption = "Comparación de estudios previos sobre la predicción de casos de dengue",
      align = "llllll",
      escape = TRUE
    ) %>%
    kable_styling(
      bootstrap_options = c("striped", "hover", "condensed", "responsive"),
      font_size = 12,
      full_width = TRUE
    ) %>%
    column_spec(1, width = "12em") %>%
    column_spec(2, width = "18em") %>%
    column_spec(3, width = "15em") %>%
    column_spec(4, width = "12em") %>%
    column_spec(5, width = "10em") %>%
    column_spec(6, width = "25em")
}