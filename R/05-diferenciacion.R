#  Diferenciación

# Aplica d diferencias ordinarias y D estacionales
diferenciar <- function(z, d = 0, D = 0, s = frequency(z)){
  library(forecast)
  w <- as.ts(z)
  if (D > 0) w <- diff(w, lag = s, differences = D)
  if (d > 0) w <- diff(w, differences = d)
  w
}

# Paso 2: las cuatro ACF + sugerencia automática de d y D
dife_bc <- function(z, s = frequency(z), lag_max = 4 * s, grafico = TRUE){
  library(forecast)
  z <- as.ts(z)
  if (s < 2) stop("La serie no tiene período estacional (s debe ser >= 2).")
  
  series <- list("Z"      = z,
                 "∇Z"     = diff(z),
                 "∇s Z"   = diff(z, lag = s),
                 "∇∇s Z"  = diff(diff(z, lag = s)))
  
  if (grafico) {
    op <- par(mfrow = c(2, 2)); on.exit(par(op))
    for (nm in names(series))        # as.numeric: el eje queda en rezagos 1, 2, 3...
      acf(as.numeric(series[[nm]]), lag.max = lag_max, main = nm)
  }
  
  # ACF en los dos relojes: rezagos 1-3 (meses) y s, 2s, 3s (años)
  tabla <- t(sapply(series, function(x) {
    r <- as.numeric(acf(as.numeric(x), lag.max = 3 * s, plot = FALSE)$acf)[-1]
    round(r[c(1:3, s * (1:3))], 2)
  }))
  colnames(tabla) <- paste0("r", c(1:3, s * (1:3)))
  
  # Sugerencia automática: primero D, luego d sobre la serie ya diferenciada
  D_sug <- min(nsdiffs(z), 1)                    # D = 2 casi nunca hace falta
  d_sug <- min(ndiffs(diferenciar(z, D = D_sug, s = s)), 2 - D_sug)  # d + D <= 2
  
  list(acf = tabla, d_sugerido = d_sug, D_sugerido = D_sug)
}
