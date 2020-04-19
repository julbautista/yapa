# Fit model and render site
source("R/fit_general.R") 
source("R/fit_states.R") 
setwd("docs")
rmarkdown::render_site()
setwd("..")

