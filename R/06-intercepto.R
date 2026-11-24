constante <- function(w, s = frequency(w), orden_reg = c(1, 0), orden_est = c(1, 0)) {
  w_num <- as.numeric(w)
  n_eff <- length(w_num)
  h     <- s + 1
  
  # 1. Cálculo de S_W con h = s + 1 (Proposición 10, diapositiva 59)
  r      <- as.numeric(acf(w_num, lag.max = h, plot = FALSE)$acf)[-1]
  factor <- 1 + 2 * sum(r)
  gamma0 <- mean((w_num - mean(w_num))^2)
  
  S_W <- if (factor > 0) sqrt(gamma0 * factor / n_eff) else NA_real_
  t_muestral <- if (!is.na(S_W)) mean(w_num) / S_W else NA_real_
  
  # 2. Ajuste sobre W_t con constante para verificar su estadístico t (diapositiva 60)
  fit0 <- arima(w_num,
                order = c(orden_reg[1], 0, orden_reg[2]),
                seasonal = list(order = c(orden_est[1], 0, orden_est[2]), period = s),
                include.mean = TRUE,
                method = "ML")
  
  est_c <- coef(fit0)["intercept"]
  ee_c  <- sqrt(fit0$var.coef["intercept", "intercept"])
  t_mod <- as.numeric(est_c / ee_c)
  
  list(
    w_barra     = mean(w_num),
    factor_suma = factor,
    S_W         = S_W,
    t_muestral  = t_muestral,
    intercepto  = as.numeric(est_c),
    ee_interc   = as.numeric(ee_c),
    t_modelo    = t_mod,
    incluir_c   = abs(t_mod) > 2
  )
}
