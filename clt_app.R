library(shiny)
library(ggplot2)
library(mathjaxr) # for rendering Latex expressions in Shiny

# Define UI
ui <- fluidPage(
  
  # Application title
  titlePanel("Illustrating the Central Limit Theorem (CLT)"),
  
  # Sidebar layout with input and output definitions
  sidebarLayout(
    sidebarPanel(
      numericInput("p_0",  label = "Baseline Conversion Probability", value = 0.5, min = 0.001, max = 0.999),
      numericInput("tau", "Average Treatment Effect", value = 0.1, min = -0.5, max = 0.5, step = 0.1),
      numericInput("p", "Treatment Assignment Probability", value = 0.5, min = 0.001, max = 0.999),
      sliderInput("n", "Total Sample Size", min = 1000, max = 100000, step= 1000, value = 1000),
      sliderInput("m", "Number of Bootstrap Samples", min = 500, max = 10000, value = 2000)
      ),
  

    
    # Show output
    mainPanel(
      plotOutput("output_plot")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  output$output_plot <- renderPlot({
    # Extracting parameters from inputs
    p_0 <- input$p_0
    tau <- input$tau
    p <- input$p
    n <- input$n
    m <- input$m
    
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
    ggplot(df, aes(x = sample_means, color="emp")) +
      geom_histogram(bins = nbins, aes(y = ..density..), alpha = 0.5, fill = "blue") +
      stat_function(fun = dnorm, args = list(mean = tau, sd = sigma_dm), aes(color= "clt")) + 
      #ggtitle("Empirical Distribution of the Difference-in-Means Estimator and CLT Approximation") +
      scale_color_manual(name = "Legend",
                         values = c("emp" = "blue", "clt" = "black"),
                         labels = c("emp" = "Empirical Distribution", "clt" = "CLT Approximation")) + 
      xlab("Difference-in-Means") +
      ylab("Density") +
      theme(plot.title = element_text(hjust = 0.5)) +
      xlim(tau - 0.05, tau + 0.05) +
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
    
        
  })
}

floor(sqrt(n))
# Run the application
shinyApp(ui = ui, server = server)
