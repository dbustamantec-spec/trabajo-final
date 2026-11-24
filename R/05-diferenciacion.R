library(ggplot2)
library(forecast)

diferenciar <- function(z, d = 0, D = 0, s = frequency(z)) {
  w <- if (is.ts(z) && frequency(z) == s) z else ts(as.numeric(z), frequency = s)
  if (D > 0) w <- diff(w, lag = s, differences = D)
  if (d > 0) w <- diff(w, differences = d)
  w
}

bc_dif <- function(z, s = frequency(z), lag_max = 3 * s) {
  z <- if (is.ts(z) && frequency(z) == s) z else ts(as.numeric(z), frequency = s)
  if (s < 2) stop("La serie no tiene período estacional (s debe ser >= 2).")
  if (length(z) <= 2 * s + 1) stop("Serie demasiado corta para este análisis.")
  
  series <- list(
    "log(Z)"     = z,
    "∇log(Z)"    = diff(z),
    "∇s log(Z)"  = diff(z, lag = s),
    "∇∇s log(Z)" = diff(diff(z, lag = s))
  )
  
  # Tabla ACF en los dos relojes: rezagos 1..3 y s, 2s, 3s
  tabla <- t(sapply(series, function(x) {
    r <- as.numeric(acf(as.numeric(x), lag.max = max(lag_max, 3 * s), plot = FALSE)$acf)[-1]
    round(r[c(1:3, s * (1:3))], 3)
  }))
  colnames(tabla) <- paste0("r", c(1:3, s * (1:3)))
  
  # Construcción del dataframe para los 4 paneles en ggplot2
  df_list <- lapply(names(series), function(nm) {
    x_num <- as.numeric(series[[nm]])
    n_eff <- length(x_num)
    r     <- as.numeric(acf(x_num, lag.max = lag_max, plot = FALSE)$acf)[-1]
    rez   <- seq_along(r)
    data.frame(
      serie      = factor(nm, levels = names(series)),
      rezago     = rez,
      acf        = r,
      estacional = (rez %% s) == 0,
      banda      = 1.96 / sqrt(n_eff)
    )
  })
  df_acf <- do.call(rbind, df_list)
  
  p <- ggplot(df_acf, aes(x = rezago, y = acf, fill = estacional)) +
    geom_hline(yintercept = 0, color = "black", linewidth = 0.4) +
    geom_hline(aes(yintercept = banda), linetype = "dashed", color = "steelblue4", linewidth = 0.5) +
    geom_hline(aes(yintercept = -banda), linetype = "dashed", color = "steelblue4", linewidth = 0.5) +
    geom_col(width = 0.45, show.legend = FALSE) +
    scale_fill_manual(values = c("FALSE" = "darkslategray", "TRUE" = "firebrick")) +
    scale_x_continuous(breaks = seq(0, lag_max, by = s)) +
    coord_cartesian(ylim = c(-1, 1)) +
    facet_wrap(~ serie, ncol = 2) +
    labs(
      title = "Identificación de d y D: ACF en los dos relojes",
      x = "Rezago k",
      y = "ACF"
    ) +
    theme_minimal(base_size = 11) +
    theme(panel.grid.minor = element_blank())
  
  # Sugerencia auxiliar (recordar validar visualmente que no haya sobre-diferenciación)
  D_sug <- min(nsdiffs(z), 1)
  d_sug <- min(ndiffs(diferenciar(z, D = D_sug, s = s)), 2 - D_sug)
  
  list(acf = tabla,
    d_sugerido = d_sug, D_sugerido = D_sug,
    series     = series,
    grafico    = p)
}
