###################################################################################################
### "Zweitstimme.org. Ein strukturell-dynamisches Vorhersagemodell für Bundestagswahlen Election
### polling forecasting for election 2002"
###
### Simon Munzert, Lukas Stötzer, Thomas Gschwend, Marcel Neunhoeffer, Sebastian Sternberg
###
### Replication file: Functions


# recode to character 
char <- function(x) as.character(x)

# recode to numeric
num <- function(x) as.numeric(x)

# simpleCap <- function(x) {
#   s <- strsplit(x, " ")[[1]]
#   paste0(toupper(substring(s, 1,1)), substring(s, 2),
#         collapse = " ")
# }

# recode party names from abbreviations to full titles
recode_partynames <- function(x, longnames = FALSE) {
  require(stringr)
  x_recoded <- x %>% str_replace("cdu", "Union") %>%
    str_replace("fdp", "FDP") %>% 
    str_replace("spd", "SPD") %>%
    str_replace("gru", "Grüne") %>%
    str_replace("lin", "Linke") %>%
    str_replace("afd", "AfD") %>%
    str_replace("oth", "Andere")
  if(longnames == TRUE) {
    x_recoded <- x_recoded %>%
      str_replace("Grüne", "B'90/Die Grünen") %>%
      str_replace("Union", "CDU/CSU") %>%
      str_replace("Linke", "Die Linke")
  }
  x_recoded
}

# recode years to remove hundreds
recode_years <- function(x) {
  x_recoded <- x %>% str_replace("19|20", "'")
  x_recoded
}


# get quantities of interest from JAGS MCMC matrix
jags_summary <- function(x) {
  dat <- data.frame(var = colnames(x),
                    mean = apply(x, 2, mean),
                    sd = apply(x, 2, sd),
                    q95lo = apply(x, 2, quantile, probs = 0.025),
                    q95hi = apply(x, 2, quantile, probs = 0.975),
                    q90lo = apply(x, 2, quantile, probs = 0.05),
                    q90hi = apply(x, 2, quantile, probs = 0.95),
                    q80lo = apply(x, 2, quantile, probs = 0.10),
                    q80hi = apply(x, 2, quantile, probs = 0.90),
                    stringsAsFactors = FALSE
  )
  dat
}


# transform Mean and Variance from Normal Prior to Beta
estBetaParams <- function(mu, var) {
  alpha <- ((1 - mu) / var - 1 / mu) * mu ^ 2
  beta <- alpha * (1 / mu - 1)
  return(params = c(alpha, beta))
}


# ggPlot Functions

# laballer function 
party_labeller <- function(variable,value){
  party_names <- list(
    'cdu' = "CDU/CSU",
    'fdp' = "FDP",
    'gru' = "B'90/Die Grünen",
    'lin' = "Die Linke",
    'spd' = "SPD",
    "afd" = "AFD"
  )
  return(party_names[as.character(value)])
}


# plot JAGS results 
# plot_model_res <- function(jags.out , polls_data_long = polls_long, pn = party_names){
#   df <- t(apply(as.matrix(jags.out), 2, quantile, c(0.95, 0.5, 0.05)))
#   df <- as.data.frame(cbind(df,str_split_fixed(rownames(df),"\\[|,|\\]",3)))
#   names(df) <- c("high", "mid", "low", "par", "time", "party")
#   df[,c(1:3,5)] <- apply(df[,c(1:3,5)],2,as.numeric)
#   levels(df$party) <- c("", pn)
#   ggplot() + 
#     geom_line(data = df[df$par == "alpha",], aes(x = (time), y = mid)) + 
#     geom_ribbon(data = df[df$par == "alpha",], aes(x = (time), y = mid, ymin = low, ymax = high, fill = party), alpha = 0.3) + 
#     facet_wrap( ~ party, scales = "free") + 
#     theme_bw() +
#     geom_point(data = polls_data_long, aes(x = t, y = support/100))
# }


# # create mean filtered values
# create_df_mean <- function(dta = model_res, pn = party_names){
#   mr <- as.matrix(dta)
#   sel <- grep("alpha",colnames(mr))
#   df <- apply(mr[,sel],2,mean)
#   df <- as.data.frame(cbind(df,str_split_fixed(names(df),"\\[|,|\\]",3)))
#   names(df) <- c("mean","par","time","party")
#   df$mean <- as.numeric(as.character(df$mean))
#   df$time <- as.numeric(as.character(df$time))
#   levels(df$party) <- c(party_names)
#   df$year <- ElectionYear
#   return(df)
# }

# coalition Liklihood Majority
majority_function <- function(parties, sel_parties){
  parties <- ifelse(parties < 0.05, 0, parties) # 5 % Hurdel
  parties <- parties/sum(parties) # Standardize
  coal_parties <- parties[sel_parties]
  if(any(coal_parties == 0)){
    return(0) # if one of the coalition partners is below Hurdel return 0
  } else {
    return(as.numeric(sum(coal_parties) > 0.5)) # above 50 % return 1, below 0
  }
}

# coalition probability
p_coal_maj <- function(parties, sel = sel_forcast){
  paste0(round(mean(apply(rb[,sel[-7]], 1, majority_function,parties)), 2) * 100, "%")
}

