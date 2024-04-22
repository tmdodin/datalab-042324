# Load necessary library
library(ggplot2)
library(gridExtra)

# Set seed for reproducibility
set.seed(42)

# Simulate 5000 observations from a Poisson distribution with scale parameter 5
# These are the untreated potential outcomes Y(0)
y_0 <- rpois(n = 5000, lambda = 5)

# Define a constant treatment effect tau
tau <- 1

# Generate treated outcomes as Y(1) = Y(0) + tau
y_1 <- y_0 + tau

# Display summary statistics of potential outcomes
summary(y_0)
summary(y_1)


# Define a logistic probability function. This function is used 
# to model the stylized fact that there is positive selection 
# based on baseline orders Y(0) into tPro.
logistic_probability <- function(x) {
  return(1 / (1 +  exp(-0.5*x)) - (max(x) - x )/max(x) * 0.5 )
}

# Calculate logistic probabilities for the observations
logistic_probabilities <- logistic_probability(y_0)

# Display summary statistics of the logistic probabilities
summary(logistic_probabilities)

# Display selection pattern
plot(y_0,logistic_probabilities)


generate_bernoulli <- function(probabilities) {
  # Generate random numbers from uniform distribution
  random_numbers <- runif(length(probabilities))
  # Compare random numbers with probabilities and generate Bernoulli output
  bernoulli_output <- as.integer(random_numbers <= probabilities)
  return(bernoulli_output)
}

# These are the assignments observed under selection
W <- generate_bernoulli(logistic_probabilities)

# Generate observed outcomes
Y = W * y_1 + (1-W) * y_0

# Data frame for ggplot
data <- data.frame(Y,y_1,y_0,W)

# Calculate means
mean_y_0 <- mean(data$y_0)
mean_y_1 <- mean(data$y_1)
mean_Y_W0 <- mean(data$Y[data$W == 0])
mean_Y_W1 <- mean(data$Y[data$W == 1])

# Plot 1: Distribution of y_1 and y_0
plot1 <- ggplot(data, aes(x = y_0, color = "y_0")) + 
  geom_histogram(aes(x = y_0, color = "y_0", y= stat(density)), fill = "blue", alpha = 0.3, binwidth = 1) +
  geom_histogram(aes(x = y_1, color = "y_1" ,y= stat(density)), fill = "red", alpha = 0.3, binwidth = 1) +
  labs(title = "Potential Outcomes",
       subtitle = expression( paste("Marginal Distributions of ''Potential Orders'' ", Y[i](0), " and ", Y[i](1)))) +
  geom_vline(xintercept = mean_y_0, color = "blue", linetype = "dashed", aes(color = "Mean of y_0")) +
  geom_vline(xintercept = mean_y_1, color = "red", linetype = "dashed", aes(color = "Mean of y_1")) +
  xlab("Potential Orders") +
  ylab("Density") + 
  scale_color_manual(name = "Legend", values = c("y_0" = "blue", "y_1" = "red", "Mean of y_0" = "blue", "Mean of y_1" = "red"),
                     labels = c("y_0" = expression( paste("Distribution of ",Y[i](0))), "y_1" = expression( paste("Distribution of ",Y[i](1))), "Mean of y_0" = "Mean of y_0", "Mean of y_1" = "Mean of y_1")) +
  theme_minimal() +
  guides(color = guide_legend(title = "Densities")) +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = c(0.80, 0.9), legend.text = element_text(size = 10), legend.title = element_text(size = 10)) +
  ylim(0,0.25) + 
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  annotate(
    "label", 
    x = 12, y = 0.1925, label = TeX(r"($ E[ Y_{i}(1) ]  - E[ Y_{i}(0)] = 1$)", output = "character"), 
    parse = TRUE)


# Plot 2: Distribution of y|w=1 and y|w=0
plot2 <- ggplot(data) + 
  geom_histogram(data = subset(data, W == 0), aes(x = Y, color = "Distribution of Y|W=0",y= stat(density)), fill = "blue", alpha = 0.3, binwidth = 1) +
  geom_histogram(data = subset(data, W == 1), aes(x = Y, color = "Distribution of Y|W=1",y= stat(density)), fill = "red", alpha = 0.3, binwidth = 1) +
  labs(title = "Observed Outcomes",
       subtitle = expression( paste("Conditional Distributions of Observed Orders ",Y[i], " given tPro status"))) +
  geom_vline(xintercept = mean_Y_W0, color = "blue", linetype = "dashed") +
  geom_vline(xintercept = mean_Y_W1, color = "red", linetype = "dashed") +
  theme_minimal()  +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = c(0.80, 0.9), legend.text = element_text(size = 10), legend.title = element_text(size = 10)) +
  ylim(0,0.25) + 
  xlab("Observed Orders") +
  ylab("Density") +
  scale_color_manual(values = c("blue", "red"), labels = c(expression( paste("Distribution of ",Y[i],"|",W[i],"=0")), expression( paste("Distribution of ",Y[i],"|",W[i],"=1")))) +
  guides(color = guide_legend(title = "Densities")) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) + 
  annotate(
    "label", 
    x = 12, y = 0.1925, label = TeX(r"($ E[ Y_{i} | W_{i} = 1 ]  - E[ Y_{i} | W_{i} = 0 ] = 2.52$)", output = "character"), 
    parse = TRUE)

# Display the plots side by side
gridExtra::grid.arrange(plot1, plot2, ncol = 2)

# Difference in means
mean(data[W==1,]$Y) - mean(data[W==0,]$Y)
# ATT 
mean(data[W==1,]$y_1) - mean(data[W==1,]$y_0)
# ATE
mean(data$y_1) - mean(data$y_0)
