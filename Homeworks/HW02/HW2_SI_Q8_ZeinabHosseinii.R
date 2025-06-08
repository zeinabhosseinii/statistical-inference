estimate_pi <- function(num_points) {
  inside_circle <- 0
  for (i in 1:num_points) {
    x <- runif(1, -1, 1)
    y <- runif(1, -1, 1)
    if (x^2 + y^2 <= 1) {
      inside_circle <- inside_circle + 1
    }
  }
  pi_estimate <- 4 * inside_circle / num_points
  return(pi_estimate)
}

print(paste("Estimate of pi with 1000 points:", estimate_pi(1000)))
print(paste("Estimate of pi with 10000 points:", estimate_pi(10000)))
print(paste("Estimate of pi with 100000 points:", estimate_pi(100000)))
print(paste("Estimate of pi with 1000000 points:", estimate_pi(1000000)))
print(paste("True value of pi:", pi))

