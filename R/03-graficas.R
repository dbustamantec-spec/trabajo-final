# Serie, acf y pacf:
graficas <- function(serie, titulo){
  library(ggplot2); library(forecast);library(patchwork)
  library(ggfortify)
  g1 <- autoplot(serie, ts.colour = "darkslategray", linewidth = 1) +
    labs(title = "Serie de tiempo", x = "Tiempo", y = "Casos") +
    theme_minimal()
  
  g2 <- ggAcf(serie, color = "darkslategray", lag.max = 50) +
    labs(title = "Correlograma ACF") +
    theme_minimal()
  g3 <- ggPacf(serie, color = "darkslategray", lag.max = 50) +
    labs(title = "Correlograma PACF") + 
    theme_minimal()
  (g1 / (g2 + g3)) + 
    plot_annotation(title = titulo)
}


## Gráficas de las series diferenciadas (si se les aplicó), con banda de 
# Barlett para la ACF
graficas_dif <- function(serie, titulo){
  library(ggplot2); library(forecast);library(patchwork)
  library(ggfortify)
  n <- length(serie)
  acf_obj <- acf(serie, lag.max = 50, plot = FALSE)
  r <- as.numeric(acf_obj$acf)[-1]
  lags <- 1:length(r)
  
  # Banda acumulada de Bartlett: +/- 2 * sqrt((1 + 2 * sum_{i=1}^{k-1} r_i^2) / n)
  bartlett <- 2 * sqrt((1 + 2 * c(0, cumsum(r[-length(r)]^2))) / n)
  df_acf <- tibble(lag = lags, acf = r, sup = bartlett, inf = -bartlett)
  g1 <- autoplot(serie, ts.colour = "darkslategray", linewidth = 1) +
    labs(title = 
           "Serie transformada y diferenciada (si fue necesario)",
         x = "Tiempo", y = "Valor") +
    theme_minimal()
  g2 <- ggplot(df_acf, aes(x = lag, y = acf)) +
    geom_hline(yintercept = 0, color = "gray40") +
    geom_segment(aes(xend = lag, yend = 0), color = "darkslategray", linewidth = 0.7) +
    geom_step(aes(y = sup), direction = "mid", linetype = "dashed", color = "steelblue") +
    geom_step(aes(y = inf), direction = "mid", linetype = "dashed", color = "steelblue") +
    labs(title = "ACF (Banda de Bartlett)", x = "Rezago", y = "ACF") +
    theme_minimal()
  g3 <- ggPacf(serie, color = "darkslategray", lag.max = 50) +
    labs(title = "PACF") +
    theme_minimal()
  (g1 / (g2 + g3)) + 
    plot_annotation(title = titulo)
}
