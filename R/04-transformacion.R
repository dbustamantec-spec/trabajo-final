library(ggplot2)

bc_trans <- function(z, s = frequency(z),
                     malla = round(seq(-2, 4, by = 0.01), 2),
                     alfa = 0.05) {
  x <- as.numeric(z)
  if (anyNA(x) || any(x <= 0)) stop("Box-Cox requiere una serie positiva y sin NA.")
  if (s < 2) stop("La serie no tiene período estacional (s debe ser >= 2).")
  
  n <- length(x)
  m <- n - 1 - s - 4                      
  if (m < 10) stop("Serie demasiado corta para este criterio.")
  
  malla <- sort(unique(malla))
  k     <- length(malla)
  gm    <- exp(mean(log(x)))            
  
  bc <- function(l) {               
    if (l == 0) gm * log(x) else (x^l - 1) / (l * gm^(l - 1))
  }
  
  S_lambda <- function(l){
    w <- diff(diff(bc(l), lag = s))      
    X <- embed(w, 5)                      
    sum(resid(lm(X[, 1] ~ X[, -1]))^2)    
  }
  
  S  <- vapply(malla, S_lambda, numeric(1))
  i  <- which.min(S)
  G  <- m * log(S / min(S))
  cr <- qchisq(1-alfa, 1)
  
  dentro <- which(G <= cr)
  inf <- if (min(dentro) > 1){
    j <- min(dentro)
    approx(G[(j - 1):j], malla[(j - 1):j], xout = cr)$y
  } else NA_real_
  
  sup <- if (max(dentro) < k) {
    j <- max(dentro)
    approx(G[j:(j + 1)], malla[j:(j + 1)], xout = cr)$y
  } else NA_real_
  
  G0 <- m * log(S_lambda(0) / min(S))
  G1 <- m * log(S_lambda(1) / min(S))
  
  df_perfil <- data.frame(lambda = malla, G = G)
  
  p <- ggplot(df_perfil, aes(x = lambda, y = G)) +
    geom_line(color = "darkslategray", linewidth = 0.9) +
    geom_vline(xintercept = malla[i], linetype = "dotted", color = "steelblue4", linewidth = 0.7) + 
    labs(
      title = "Perfil de Box y Cox",
      x = expression(lambda),
      y = expression(G(lambda))
    ) +
    theme_minimal(base_size = 11) +
    theme(panel.grid.minor = element_blank())
  
  list(lambda  = malla[i],
    ic      = c(inf = inf, sup = sup),
    G0      = G0,
    G1      = G1,
    m       = m,
    malla   = malla,
    G       = G,
    grafico = p)
}
