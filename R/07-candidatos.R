library(kableExtra)

comparar_candidatos <- function(w,
                                d = 1,
                                D = 0,
                                s = frequency(w),
                                candidatos,
                                incluir_constante = FALSE) {
  w_num <- as.numeric(w)
  if (s < 2) stop("La serie no tiene período estacional (s debe ser >= 2).")
  
  # Muestra común: c = max(p + s * P) entre todos los candidatos
  nc <- max(vapply(candidatos, function(o) o[1] + s * o[3], numeric(1)))
  
  # Varianza muestral de W_t evaluada en los mismos N puntos finales
  w_comun  <- w_num[-(1:nc)]
  sigma2_Z <- mean((w_comun - mean(w_comun))^2)
  
  filas <- lapply(candidatos, function(o) {
    p <- o[1]; q <- o[2]; P <- o[3]; Q <- o[4]
    
    fit <- tryCatch(
      arima(w_num,
            order = c(p, 0, q),
            seasonal = list(order = c(P, 0, Q), period = s),
            include.mean = incluir_constante,
            method = "CSS",
            n.cond = nc),
      error = function(e) NULL
    )
    
    if (is.null(fit)) return(NULL)
    
    a      <- residuals(fit)[-(1:nc)]
    N      <- length(a)
    M      <- p + q + P + Q + as.integer(incluir_constante)
    sigma2 <- mean(a^2)
    
    aic <- N * log(sigma2) + 2 * M
    sbc <- N * log(sigma2) + M * log(N)
    bic <- if (M > 0 && sigma2 < sigma2_Z && M < N) {
      N * log(sigma2) - (N - M) * log(1 - M / N) +
        M * log(N) + M * log(((sigma2_Z / sigma2) - 1) / M)
    } else NA_real_
    
    data.frame(
      Modelo = sprintf("$\\text{SARIMA}(%d,%d,%d)(%d,%d,%d)_{%d}$", p, d, q, P, D, Q, s),
      M      = M,
      AIC    = round(aic, 2),
      BIC    = round(bic, 2),
      SBC    = round(sbc, 2),
      stringsAsFactors = FALSE
    )
  })
  
  tabla <- do.call(rbind, filas)
  tabla <- tabla[order(tabla$SBC, tabla$AIC, tabla$BIC), , drop = FALSE]
  
  kbl(tabla, row.names = FALSE, escape = FALSE, caption = "Criterios de información") |>
    kable_styling(full_width = FALSE, position = "center")
}
