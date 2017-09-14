###################################################################################################
### "Zweitstimme.org. Ein strukturell-dynamisches Vorhersagemodell für Bundestagswahlen Election
### polling forecasting for election 2002"
###
### Simon Munzert, Lukas Stötzer, Thomas Gschwend, Marcel Neunhoeffer, Sebastian Sternberg
###
### Replication file: Packages


p_needed <- c("haven", "lubridate", "stringr", "ggplot2","dlm","plyr", "dplyr", "magrittr", 
              "broom", "tidyr",  "stringr", "reshape2", "rjags", "runjags", "readr", "rvest", 
              "mcmcplots", "parallel", "twilio", "xtable")

packages <- rownames(installed.packages())
p_to_install <- p_needed[!(p_needed %in% packages)]
if (length(p_to_install) > 0) {
  install.packages(p_to_install)
}
lapply(p_needed, require, character.only = TRUE)
