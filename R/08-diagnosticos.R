library(ggplot2)
library(tseries)
library(kableExtra)

validar_sarima <- function(z,
                           orden = c(0, 1, 1),
                           estacional = c(1, 0, 0),
                           s = frequency(z),
                           incluir_constante = FALSE) {
  d <- orden[2]; D <- estacional[2]
  p <- orden[1]; q <- orden[3]
  P <- estacional[1]; Q <- estacional[3]
  m <- p + q + P + Q
  
  # 1. Estimación exacta por máxima verosimilitud sobre Z_t (diapositiva 101)
  fit <- arima(z,
               order = orden,
               seasonal = list(order = estacional, period = s),
               include.mean = incluir_constante,
               method = "ML", optim.control = list(maxit=1000))
  
  # 2. Tabla de coeficientes con signos del curso (theta = -ma, Theta = -sma)
  co <- coef(fit)
  ee <- sqrt(diag(fit$var.coef))
  idx_ma <- grep("^(ma|sma)", names(co))
  co_curso <- co
  co_curso[idx_ma] <- -co_curso[idx_ma]
  
  df_coef <- data.frame(
    Parametro  = names(co_curso),
    Estimacion = round(co_curso, 4),
    EE         = round(ee, 4),
    t_stat     = round(co_curso / ee, 2),
    row.names  = NULL
  )
  
  # 3. Extracción de residuos descartando los primeros d + sD (diapositiva 67 y 101)
  descarte <- d + s * D
  e_all    <- as.numeric(residuals(fit))
  e        <- if (descarte > 0) e_all[-(1:descarte)] else e_all
  n_eff    <- length(e)
  
  # 4. Prueba de Ljung-Box en múltiplos de s: K = s, 2s, 3s (diapositiva 83)
  Ks <- s * (1:3)
  df_lb <- do.call(rbind, lapply(Ks, function(K) {
    gl <- K - m
    if (gl <= 0) return(NULL)
    bt <- Box.test(e, lag = K, type = "Ljung-Box", fitdf = m)
    data.frame(
      K       = K,
      Q_LB    = round(as.numeric(bt$statistic), 2),
      gl      = gl,
      Crit_5  = round(qchisq(0.95, gl), 2),
      Valor_p = round(bt$p.value, 3)
    )
  }))
  
  # 5. Normalidad: asimetría, curtosis y Jarque-Bera (diapositiva 82)
  z_e  <- (e - mean(e)) / sd(e)
  asim <- mean(z_e^3)
  curt <- mean(z_e^4)
  jb   <- jarque.bera.test(e)
  
  df_norm <- data.frame(
    n_residuos = n_eff,
    Asimetria  = round(asim, 3),
    Curtosis   = round(curt, 3),
    JB         = round(as.numeric(jb$statistic), 2),
    Valor_p    = round(jb$p.value, 3)
  )
  
  # 6. Sobreajuste: un parámetro adicional en cada reloj (diapositiva 85 y 102)
  probar_extra <- function(ord_new, est_new, nombre_par) {
    f_ext <- tryCatch(
      arima(z, order = ord_new,
            seasonal = list(order = est_new, period = s),
            include.mean = incluir_constante, method = "ML", 
            optim.control = list(maxit=1000)),
      error = function(err) NULL
    )
    if (is.null(f_ext) || !(nombre_par %in% names(coef(f_ext)))) return(NULL)
    val <- coef(f_ext)[nombre_par]
    if (grepl("^(ma|sma)", nombre_par)) val <- -val
    se  <- sqrt(f_ext$var.coef[nombre_par, nombre_par])
    data.frame(
      Sobreajuste = sprintf("SARIMA(%d,%d,%d)(%d,%d,%d)_%d",
                            ord_new[1], ord_new[2], ord_new[3],
                            est_new[1], est_new[2], est_new[3], s),
      Parametro   = nombre_par,
      Estimacion  = round(as.numeric(val), 4),
      t_stat      = round(as.numeric(val / se), 2),
      row.names   = NULL
    )
  }
  
  df_sobre <- rbind(
    probar_extra(c(p + 1, d, q), estacional, paste0("ar", p + 1)),
    probar_extra(c(p, d, q + 1), estacional, paste0("ma", q + 1)),
    probar_extra(orden, c(P + 1, D, Q), paste0("sar", P + 1)),
    probar_extra(orden, c(P, D, Q + 1), paste0("sma", Q + 1))
  )
  
  # 7. Gráficos de diagnóstico en ggplot2 (diapositiva 82)
  lag_max <- 3 * s
  r_acf   <- as.numeric(acf(e, lag.max = lag_max, plot = FALSE)$acf)[-1]
  banda   <- 1.96 / sqrt(n_eff)
  
  df_acf <- data.frame(
    rezago     = seq_along(r_acf),
    acf        = r_acf,
    estacional = (seq_along(r_acf) %% s) == 0
  )
  
  p_acf <- ggplot(df_acf, aes(x = rezago, y = acf, fill = estacional)) +
    geom_hline(yintercept = 0, linewidth = 0.4) +
    geom_hline(yintercept = c(-banda, banda), linetype = "dashed", color = "steelblue4",
               linewidth = 0.5) +
    geom_col(width = 0.45, show.legend = FALSE) +
    scale_fill_manual(values = c("FALSE" = "darkslategray", "TRUE" = "firebrick")) +
    scale_x_continuous(breaks = seq(0, lag_max, by = s)) +
    coord_cartesian(ylim = c(-0.5, 0.5)) +
    labs(title = "ACF de los residuos", x = "Rezago k", y = "ACF") +
    theme_minimal(base_size = 11) +
    theme(panel.grid.minor = element_blank())
  
  p_qq <- ggplot(data.frame(res = z_e), aes(sample = res)) +
    stat_qq(color = "darkslategray", size = 1.6) +
    stat_qq_line(color = "firebrick", linewidth = 0.7) +
    labs(title = "Normalidad (Q-Q)", x = "Cuantil normal", y = "Residuo estandarizado") +
    theme_minimal(base_size = 11) +
    theme(panel.grid.minor = element_blank())
  
  list(
    modelo      = fit,
    residuos    = e,
    coeficientes = kbl(df_coef, row.names = FALSE, caption = "Parámetros estimados (ML)") |> kable_styling(full_width = FALSE),
    ljung_box   = kbl(df_lb, row.names = FALSE, caption = "Prueba de Ljung-Box") |> kable_styling(full_width = FALSE),
    normalidad  = kbl(df_norm, row.names = FALSE, caption = "Diagnóstico de normalidad") |> kable_styling(full_width = FALSE),
    sobreajuste = kbl(df_sobre, row.names = FALSE, caption = "Pruebas de sobreajuste") |> kable_styling(full_width = FALSE),
    graf_acf    = p_acf,
    graf_qq     = p_qq
  )
}
