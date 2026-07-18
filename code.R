### IID

 

iid_tests <- function(Z, label) {

  Z <- Z[!is.na(Z)]

  n <- length(Z)

 

  lb5_z   <- Box.test(Z,      lag = 5,  type = "Ljung-Box")

  lb10_z  <- Box.test(Z,      lag = 10, type = "Ljung-Box")

  lb5_az  <- Box.test(abs(Z), lag = 5,  type = "Ljung-Box")

  lb10_az <- Box.test(abs(Z), lag = 10, type = "Ljung-Box")

 

  sw <- if (n <= 5000) shapiro.test(Z) else list(statistic = NA, p.value = NA)

  jb <- jarque.bera.test(Z)

 

  data.frame(

    Label          = label,

    N              = n,

    LB5_Z_stat     = round(lb5_z$statistic,    4),

    LB5_Z_p        = round(lb5_z$p.value,       4),

    LB10_Z_stat    = round(lb10_z$statistic,   4),

    LB10_Z_p       = round(lb10_z$p.value,      4),

    LB5_absZ_stat  = round(lb5_az$statistic,   4),

    LB5_absZ_p     = round(lb5_az$p.value,      4),

    LB10_absZ_stat = round(lb10_az$statistic,  4),

    LB10_absZ_p    = round(lb10_az$p.value,     4),

    SW_stat        = round(as.numeric(sw$statistic), 4),

    SW_p           = round(sw$p.value,           4),

    JB_stat        = round(jb$statistic,         4),

    JB_p           = round(jb$p.value,           4),

    row.names      = NULL  

  )

}

 

### ACF , QQ plot

 

plot_diagnostics <- function(Z, label) {

  Z <- Z[!is.na(Z)]

  par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))

 

  # ACF(Z)

  acf(Z, main = paste("ACF of Z:", label), lag.max = 20)

 

  # ACF(|Z|)

  acf(abs(Z), main = paste("ACF of |Z|:", label), lag.max = 20)

 

  # QQ plot

  qqnorm(Z, main = paste("QQ Plot:", label))

  qqline(Z, col = "red")

 

  # Histogram

  hist(Z, breaks = 30, probability = TRUE,

       main = paste("Histogram:", label), xlab = "Z")

  curve(dnorm(x, mean(Z), sd(Z)), add = TRUE, col = "red", lwd = 2)

 

  par(mfrow = c(1, 1))

}

 

### Models

 

fit_models <- function(X, vix_vec, label) {

 

  results <- list()

  n <- length(X)

 

  

  X_curr <- X[-1]           # X(t)

  X_lag  <- X[-n]           # X(t-1)

  dX     <- diff(X)         # X(t) - X(t-1)

  V      <- vix_vec[-1]     # V(t), X(t)

 

 

  idx <- complete.cases(X_curr, X_lag, dX, V)

  X_curr <- X_curr[idx]; X_lag <- X_lag[idx]

  dX <- dX[idx]; V <- V[idx]

 

  # Model 1

  Z1 <- dX

  results[["RW_noSV"]] <- iid_tests(Z1, paste(label, "| RW w/o SV"))

 

  # Model 2

  Z2 <- dX / V

  results[["RW_SV"]] <- iid_tests(Z2, paste(label, "| RW with SV"))

 

  # Model 3

  fit3 <- lm(X_curr ~ X_lag)

  Z3   <- residuals(fit3)

  results[["AR1_noSV"]] <- iid_tests(Z3, paste(label, "| AR(1) w/o SV"))

 

  # Model 4

  # (X(t)-X(t-1))/V = a*(1/V) + (b-1)*(X(t-1)/V) + c*V + Z*(t)

  dep  <- dX / V

  r1   <- 1 / V        # a

  r2   <- X_lag / V    # (b-1)

  r3   <- V            # c

  

  fit4 <- lm(dep ~ 0 + r1 + r2 + r3)

  Z4   <- residuals(fit4)

  results[["AR1_SV"]] <- iid_tests(Z4, paste(label, "| AR(1) with SV"))

 

  bind_rows(results)

}
