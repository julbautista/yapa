# Fit model and render site
exec_date <- Sys.Date() -1

start <- Sys.time()

source("R/fit_general.R")
source("R/fit_states.R")

setwd("docs")
rmarkdown::render_site()
setwd("..")

print(Sys.time() - start)
