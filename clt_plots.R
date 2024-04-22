# clt plots

# Load necessary library
library(ggplot2)
library(gridExtra)

clt_plot <- function(p_0,tau,p,n,m) {
  
  # Implicit Parameters
  p_1 <- p_0 + tau          
  n_0 <- floor(n * (1 - p)) 
  n_1 <- floor(n * p)      
  
  # Function to generate a vector of length n of independent Bernoulli random variables
  generate_sample <- function(n, p_conv) {
    return(rbinom(n, 1, p_conv))
  }
  
  # For each of m repetitions, generate a vector of n_0/n_1 independent Bernoulli
  # random variables and compute and store the associated sample means.
  control_means <- replicate(m, mean(generate_sample(n_0, p_0)))
  treated_means <- replicate(m, mean(generate_sample(n_1, p_1)))
  
  # Vector of differences in mean estimates
  sample_means <- treated_means - control_means 
  
  # Create a data frame of sample means for ggplot
  df <- data.frame(sample_means)
  
  # Compute Variance of Difference-in-Means estimator under repeated sampling
  sigma.sq.0 <- p_0 * (1-p_0) / (1-p) # Var(Y_i(0)) / P(W_i=0)
  sigma.sq.1 <- p_1 * (1-p_1) / (p)   # Var(Y_i(1)) / P(W_i=1)
  
  sigma_dm <- sqrt( (sigma.sq.1 + sigma.sq.0)/n )
  
  # Number of bins for histogram
  nbins <- 200
  
  # Plot histogram of sample means
  p <- ggplot(df, aes(x = sample_means, color="emp")) +
    geom_histogram(bins = nbins, aes(y = ..density..), alpha = 0.5, fill = "blue") +
    stat_function(fun = dnorm, args = list(mean = tau, sd = sigma_dm), aes(color= "clt")) + 
    ggtitle("Empirical Distribution of the Difference-in-Means Estimator and CLT Approximation") +
    scale_color_manual(name = "Legend",
                       values = c("emp" = "blue", "clt" = "black"),
                       labels = c("emp" = "Empirical Distribution", "clt" = "CLT Approximation")) + 
    xlab("Difference-in-Means") +
    ylab("Density") +
    theme(plot.title = element_text(hjust = 0.5)) +
    xlim(tau - 0.05, tau + 0.05) +
    #xlim(0.05, 0.15) +
    ylim(0,150) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5),
          legend.position = c(0.8, 0.8), 
          legend.title = element_blank(),
          legend.text = element_text(size = 12)) +
    annotate(
      "label", 
      x = tau, y = 1.1* dnorm(tau,mean=tau,sd=sigma_dm), label = TeX(r"($ N( \tau_{ate}, V_{DM}/n$$))", output = "character"), 
      parse = TRUE)
  
  return(p)
}


p50 <- clt_plot(p_0 = 0.5,
          tau = 0.1,
          p = 0.5,
          n = 50000,
          m = 10000)


p5 <- clt_plot(p_0 = 0.5,
                tau = 0.1,
                p = 0.5,
                n = 5000,
                m = 10000)



gridExtra::grid.arrange(p5, p50, ncol = 2)




