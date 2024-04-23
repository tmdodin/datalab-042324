
# Installs required packages if not installed already. 

list.of.packages <- c("shiny", "ggplot2", "gridExtra",  "latex2exp", "mathjaxr") 
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)