library(shiny)
ui <- fluidPage(
  titlePanel("Illustrating Power"),
  sidebarPanel(
    numericInput("mean1",
                "Average Lift under Null",
                min = 0,
                max = 10,
                value = 0),
    numericInput("mean2",
                "Average Lift under Alternative",
                min = 0,
                max = 10,
                value = 1),
    numericInput("sd1",
                "Outcome Standard Deviation",
                min = 0,
                max = 10,
                value = 100),
    #numericInput("sd2",
    #            "sd2",
    #            min = 0,
    #            max = 10,
    #            value = 100),
    numericInput("alpha",
                "Alpha",
                min = 0,
                max = 1,
                value = 0.05),
    sliderInput("n",
                "Sample Size per Arm",
                 min = 100,
                 max = 150000,
                 value = 1000,
                 step=100)),
  mainPanel(
    plotOutput("distPlot",width = "100%")
  )
)
server <- function(input, output) {
  output$distPlot <- renderPlot({
    
# Load necessary libraries
library(ggplot2)
library(latex2exp)
    
# Function to generate ggplot for visualizing two normal distributions
plot_two_normals <- function(mean1, mean2, sd1, sd2, alpha,n) {
  # Calculate quantiles for shading
  lower_quantile <- qnorm(alpha, mean = 0, sd = sd1)
  upper_quantile <- qnorm(1 - alpha, mean = 0, sd =sd2)
  
  # Generate x values
  x <- seq(min(mean1 - 4*sd1, mean2 - 4*sd2), max(mean1 + 4*sd1, mean2 + 4*sd2), length.out = 1000)
  
  # Calculate y values for each normal distribution
  y1 <- dnorm(x, mean = mean1, sd = sd1)
  y2 <- dnorm(x, mean = mean2, sd = sd2)
  
  # Create data frame
  df <- data.frame(x = x, y1 = y1, y2 = y2)
  
  # Compute power against the alternative H1: tau = mean2-mean1
  power <- round(1-pnorm(upper_quantile,mean=mean2,sd=sd2),3)
  
  # Plot
  p <- ggplot(df, aes(x = x)) +
    geom_line(aes(y = y1), color = "blue", linetype = "solid", size = 1,alpha=0.8) +
    geom_line(aes(y = y2), color = "red", linetype = "dashed", size = 1,alpha=0.8) +
    #geom_ribbon(data = subset(df, x < lower_quantile), aes(ymin = y1, ymax = 0, fill = "Type I Error Prob. (\u03B1)"), alpha = 0.3) +
    geom_ribbon(data = subset(df, x > upper_quantile), aes(ymin = y1, ymax = 0, fill = "Type I Error Prob. (\u03B1)"), alpha = 0.3) +
    geom_ribbon(data = subset(df, x > upper_quantile), aes(ymin = y2, ymax = 0, fill = "Power (1- \u03B2)"), alpha = 0.3) +
    geom_vline(xintercept = c(upper_quantile), linetype = "dashed", color = "black", alpha =0.3) +
    #annotate("text", x = c(lower_quantile, upper_quantile) * c(1.25,1.05), y = 0.005, label = c(expression(alpha/2), expression(alpha/2)), vjust = 0.0, hjust = 0.0) +
    annotate("text", x = 0, y = max(df$y1, df$y2), label = "Null True", vjust = -0.5, hjust = 1.0) +
    annotate("text", x = mean2, y = max(df$y2), label = "Alternative True", vjust = -0.5, hjust = 0) +
    #annotate("text", x = upper_quantile + 0.15, y = 0.2, label = expression(1-beta), vjust = 1.5, hjust = -0.5) +
    #geom_segment(aes(x = mean1 + 0.5, y = dnorm(mean1 + 0.5, mean = mean1, sd = sd1), xend = mean1, yend = dnorm(mean1, mean = mean1, sd = sd1)), 
    #             color = "blue") +
    #annotate("text", x = upper_quantile, y = 0.2, label = expression(paste0(power)), vjust = 0, hjust = 0) +
    annotate("text", x= mean2 , y= mean(y2), label=paste("Power \u2248", power),size=5) + 
    labs(title = "Approximate Sampling Distributions of the Equal Proportions \n Difference in Means Estimator under different Hypotheses",
         x = "Test Statistic",
         y = "Density") +
    theme_minimal() + 
    theme(plot.title = element_text(hjust = 0.5)) +
    xlim(c(min(mean1 - 4*sd1, mean2 - 4*sd2), max(mean1 + 4*sd1, mean2 + 4*sd2))) +
    ylim(c(0, max(max(df$y1), max(df$y2)))) +
    scale_fill_manual(name = "Legend",
                      values = c("Type I Error Prob. (\u03B1)" = "blue", "Power (1- \u03B2)" = "red"),
                      labels = c("Power (1- \u03B2)", "Type I Error Prob. (\u03B1)"))
  

  return(p)
}


# Example:
plot_two_normals(input$mean1, input$mean2, input$sd1/sqrt(input$n), input$sd1/sqrt(input$n), input$alpha,input$n)

  })
}
shinyApp(ui = ui, server = server)
