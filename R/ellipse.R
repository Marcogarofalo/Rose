compute_ellipse <- function(x, y, resampling = "bootstrap", p=0.68) {
  df <- data.frame(
    "v1" = as.numeric(x),
    "v2" = as.numeric(y)
  )
  s <- -2 * log(1 - p)
  N <- length(x)
  Ny <- length(y)
  if (N != Ny) stop("Error: N and Ny must be equal")
  if (resampling == "bootstrap") {
    C <- cov(df)*(N-1)/N
  } else if (resampling == "jack") {
    C <- cov(df) * (N-1)*(N-1)/N
  } else if (resampling == "no") {
    C <- cov(df) / sqrt(N)
  } else {
    error("invalid resampling")
  }

  # l1 <- (C[1, 1] + C[2, 2]) / 2 + sqrt(((C[1, 1] - C[2, 2]) / 2)^2 + C[1, 2]^2)
  # l2 <- (C[1, 1] + C[2, 2]) / 2 - sqrt(((C[1, 1] - C[2, 2]) / 2)^2 + C[1, 2]^2)
  eig <- eigen(C)
  l1 <- eig$values[1]
  l2 <- eig$values[2]
  # cat("eigenvalue computed with eigen()  ", eig$values[1], "\n")
  # cat("eigenvalue computed by hand  ", l1, "\n")
  theta <- atan(eig$vectors[2] / eig$vectors[1])

  de <- data.frame(
    "x0" = mean(df[, 1]),
    "y0" = mean(df[, 2]),
    "a" = sqrt(s * l1),
    "b" = sqrt(s * l2),
    "angle" = theta
  )
  return(de)
}
