# Fit model and render site
exec_date <- Sys.Date()

dates <- seq.Date(as.Date('2020-04-01'), Sys.Date(), by = 'day')

s <- Sys.time()
for(d in 11:length(dates)) {
  exec_date <- dates[d]
  print(paste0(Sys.time(), ": ", dates[d]))
  suppressWarnings(source("R/fit_general.R")) 
  suppressWarnings(source("R/fit_states.R")) 
}
print(Sys.time() - s)


source("R/fit_general.R")
source("R/fit_states.R")

setwd("docs")
rmarkdown::render_site()
setwd("..")

