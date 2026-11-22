# Función para transformacion de la varianza
trans <- function(z, s = frequency(z),
                      malla = round(seq(-1, 2, by = 0.01), 2),
                      alfa = 0.05, grafico = TRUE){
  x <- as.numeric(z)
  if (anyNA(x) || any(x <= 0)) stop("Box-Cox requiere una serie positiva y sin NA.")
  if (s < 2) stop("La serie no tiene período estacional (s debe ser >= 2).")
  n <- length(x)
  m <- n - 1 - s - 4                      # ecuaciones del AR(4) sobre nabla nabla_s
  if (m < 10) stop("Serie demasiado corta para este criterio.")
  gm <- exp(mean(log(x)))                 # media geométrica
  
  bc <- function(l)                       # escala normalizada de la diapositiva 50
    if (l == 0) gm * log(x) else (x^l - 1) / (l * gm^(l - 1))
  
  S_lambda <- function(l) {
    w <- diff(diff(bc(l), lag = s))       # nabla nabla_s
    X <- embed(w, 5)                      # w_t y sus rezagos 1 a 4
    sum(resid(lm(X[, 1] ~ X[, -1]))^2)    # AR(4) con constante
  }
  
  S  <- vapply(malla, S_lambda, numeric(1))
  i  <- which.min(S)
  G  <- m * log(S / min(S))
  cr <- qchisq(1 - alfa, 1)               # 3,841 si alfa = 0,05
  k  <- length(malla)
  
  inf <- if (i > 1) approx(G[1:i], malla[1:i], xout = cr)$y else NA
  sup <- if (i < k) approx(G[i:k], malla[i:k], xout = cr)$y else NA
  
  if (grafico){
    plot(malla, G, type = "l", xlab = expression(lambda),
         ylab = expression(G(lambda)), main = "Perfil de Box y Cox")
    abline(h = cr, lty = 2)
    abline(v = malla[i], lty = 3)
  }
  
  list(lambda = malla[i], ic = c(inf = inf, sup = sup),
       G0 = G[which.min(abs(malla))], G1 = G[which.min(abs(malla - 1))],
       m = m, crit = cr, G = G)
}

# Aplica la transformación elegida, conservando los atributos de ts
bc_transformar <- function(z, lambda)
  if (lambda == 0) log(z) else (z^lambda - 1) / lambda
