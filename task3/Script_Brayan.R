library(fields)

###### 1. SIMULACIÓN ######
n <- 100
sigma <- 1
phi <- 2

# Define la matriz de distancias
dist_matrix <- matrix(0, n, n)
for (i in 1:n) {
  for (j in 1:n) {
    dist_matrix[i, j] <- abs(i - j)}}

# Define la función de covarianza exponencial
cov_exponential <- function(h, sigma, phi) {
  return(sigma^2 * exp(-h / phi))}

Sigma <- cov_exponential(dist_matrix, sigma, phi) #matriz de covarianza

mu <- rep(0, n) #Vector de medias

# Simula un proceso gaussiano
set.seed(123)
simulated_data <- rmvnorm(1, mean = mu, sigma = Sigma)

View(simulated_data)

# Visualiza los datos simulados
plot(1:n, simulated_data, type = "l", main = "Simulación de Proceso Gaussiano",
     xlab = "Ubicación Espacial", ylab = "Valor", col = "coral1")


###### 2. Calcular estimador de Matheron ######

# Calcular estimador de Matheron para diferentes distancias h
distancias_h <- 1:50

resultados_matheron <- numeric(length(distancias_h))  # Vector para almacenar resultados

# Calcular estimador de Matheron para cada h
for (i in seq_along(distancias_h)) {
  resultado_matheron <- estimador_matheron(simulated_data, distancias_h[i])
  resultados_matheron[i] <- resultado_matheron
}

# Graficar resultados
plot(distancias_h, resultados_matheron, type = "b", 
     main = "Estimador de Matheron para diferentes distancias",
     xlab = "Distancia (h)", ylab = "Estimador de Matheron")

###### 4. Estimar el modelo de COV exponencial con la función OPTIM ######

# Definir la función objetivo (negativo del logaritmo de la verosimilitud)
neg_log_likelihood <- function(params, data, dist_matrix) {
  sigma_sq <- params[1]
  phi <- params[2]
  n <- length(data)
  
  # Matriz de covarianza exponencial
  Sigma <- sigma_sq * exp(-dist_matrix / phi)
  
  # Convertir data en un vector
  data_vector <- as.vector(data)
  
  # Logaritmo de la verosimilitud
  log_likelihood <- -0.5 * log(det(Sigma)) - 0.5 * t(data_vector) %*% solve(Sigma) %*% data_vector
  
  return(-log_likelihood)  # Negativo del logaritmo de la verosimilitud
}

# Datos y matriz de distancias
data <- as.matrix(simulated_data)

# Parámetros iniciales y restricciones
initial_params <- c(1, 1)  # Valores iniciales de los parámetros
lower_bound <- c(0, 0)      # Límites inferiores para los parámetros
upper_bound <- c(Inf, Inf)   # Límites superiores para los parámetros

# Optimización usando optim
optimal_params <- optim(par = initial_params, fn = neg_log_likelihood, 
                        data = data, dist_matrix = dist_matrix, 
                        method = "L-BFGS-B", lower = lower_bound, upper = upper_bound)$par

# Imprimir resultados
cat("Estimación de sigma^2:", optimal_params[1], "\n")
cat("Estimación de phi:", optimal_params[2], "\n")
