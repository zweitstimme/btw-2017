###################################################################################################
### "Zweitstimme.org. Ein strukturell-dynamisches Vorhersagemodell für Bundestagswahlen Election
### polling forecasting for election 2002"
###
### Simon Munzert, Lukas F. Stoetzer, Thomas Gschwend, Marcel Neunhoeffer, Sebastian Sternberg
###
### Replication file: Estimation


### Preparatory Steps  ----------------------------------------------------------------------------

## Set working directory to current file location (in RStudio)
p_needed <- "rstudioapi"
packages <- rownames(installed.packages())
p_to_install <- p_needed[!(p_needed %in% packages)]
if (length(p_to_install) > 0) {
  install.packages(p_to_install)
}
lapply(p_needed, require, character.only = TRUE)

setwd(dirname(getActiveDocumentContext()$path))

## Set it manually if you are not working with RStudio
# setwd("path/to/file")

## Load required packages and functions
source("packages.R")
source("functions.R")


### Structural Model  -----------------------------------------------------------------------------

### Load the data - election results 
load("../data/model_input/ger_model_df.RData")

election_years <- unique(ger_df_long$year)
election_years_id <- seq_along(election_years)
election_years_df <- data.frame(year = election_years, election_id = election_years_id)


### Run model in JAGS for complete sample

# variables
predictors <-  c("voteshare_l1", "chancellor_party", "polls_200_230")
dependent <- "voteshare"

# data 
which_cases <- complete.cases(ger_df_long[,predictors])
dat <- ger_df_long[,c(dependent, predictors, "election")]
dat_sub <- dat[which_cases, ]
dat_full <- ger_df_long[which_cases, ]
dat_full$case_label <- paste(dat_full$party, dat_full$year, sep = ", ") %>% recode_partynames 

dataList = list(
  x = as.matrix(dat_sub[,predictors]), # Predictor Variables
  y = dat_sub[,dependent], # Dependent variable
  N = nrow(dat_sub), # Number of data points
  K = length(predictors), # Number of predictors
  L = length(unique(dat_sub$election)) + 1, # Number of elections
  election = dat_sub$election # vector of election ids
  )

# set up simulation
jags_mod <- jags.model(file = "structural_model.jags",
                       data = dataList,
                       n.chains = 3,
                       n.adapt = 100000)

jags_out <- coda.samples(jags_mod,
                         n.iter = 100000,
                         thin = 100,
                         variable.names = c("b0", "b","sigma", "sigma.b0", 
                                            "sigma.b", "drift", "mu"))

jags_matrix <- as.matrix(jags_out)

# diagnostics and summary not in paper. Just in case.
# summary(jags_out)
# plot(jags_out)
# mcmcplot(jags_out)

# est_mean <- summary(jags_out)$statistics[, "Mean"]
# mu_mean <- est_mean[str_detect(names(est_mean), "mu")]
# mean(abs(mu_mean - dataList$y), na.rm = TRUE)

# calculate summary stats from the draws 
jags_summary_df <- jags_summary(jags_matrix)

save(jags_summary_df, file = "../data/model_output/structural_summary.RData")

### Run model in JAGS, out-of-sample predictions 

jags_out_list <- list()
jags_matrix_list <- list()
jags_summary_list <- list()

for (i in election_years_id) {
  # vars
  predictors = c("voteshare_l1", "chancellor_party", "polls_200_230")
  dependent = "voteshare"
  
  # data 
  which_cases <- complete.cases(ger_df_long[,predictors])
  dat <- ger_df_long[,c(dependent, predictors, "election")]
  dat_sub <- dat[which_cases,]
  dat_sub[dat_sub$election == i, dependent] <- NA # make case out-of-sample
  dat_full <- ger_df_long[which_cases,]
  dat_full$case_label <- paste(dat_full$party, dat_full$year, sep = ", ") %>% 
    recode_partynames 
  
  dataList = list(
    x = as.matrix(dat_sub[,predictors]), # Predictor Variables
    y = dat_sub[,dependent], # Dependent variable
    N = nrow(dat_sub), # Number of data points
    K = length(predictors), # Number of predictors
    L = length(unique(dat_sub$election))+1, # Number of elections
    election = dat_sub$election # vector of election ids
  )
  
  # set up simulation
  jags_mod <- jags.model(file = "structural_model.jags",
                         data = dataList,
                         n.chains = 3,
                         n.adapt = 100000)
  jags_out <- coda.samples(jags_mod,
                           n.iter = 100000,
                           thin = 100,
                           variable.names = c("b0", "b", "drift", "mu", 
                                              "y", "sigma", "sigma.b0", "sigma.b"))
  jags_out_list[[i]] <- jags_out
  jags_matrix_list[[i]] <- as.matrix(jags_out_list[[i]])
  jags_summary_list[[i]] <- jags_summary(jags_matrix_list[[i]])
}

# examine convergence
# plot(jags_out)
# mcmcplot(jags_out)

save(jags_out_list, jags_matrix_list, jags_summary_list, dataList, dat_full,
     file = "../data/model_output/structural_jags_simulations.RData")


# extract out-of-sample predictions
load("../data/model_output/structural_jags_simulations.RData")
jags_oospreds_list <- list()
jags_oospreds_summary_list <- list()

# collect predictions by election, implement sum-to-100 constraint
ys <- paste0("y[", seq_along(dat_sub$election), "]")

for (i in election_years_id) {
  ys_which <- ys[which(dat_sub$election == i)]
  jags_oospreds_list[[i]] <- jags_matrix_list[[i]][,ys_which] 
  rowsums <- rowSums(jags_oospreds_list[[i]])
  jags_oospreds_list[[i]] <- jags_oospreds_list[[i]] / rowsums * 100
  jags_oospreds_summary_list[[i]] <- try(jags_summary(jags_oospreds_list[[i]]))
}

jags_oospreds_summary_df <- rbind.fill(jags_oospreds_summary_list[-1])
jags_oospreds_summary_df$party <- dat_full$party
jags_oospreds_summary_df$election <- dat_full$year
jags_oospreds_summary_df$partyname <- dat_full$party %>% recode_partynames(longnames = TRUE)
save(jags_oospreds_summary_df, file = "../data/model_output/structural_forecasts.RData")

# assess out-of-sample error
mean(abs(jags_oospreds_summary_df$mean - dataList$y), na.rm = TRUE)
sqrt(mean((jags_oospreds_summary_df$mean - dataList$y)^2, na.rm = TRUE))

load("../data/model_output/structural_forecasts.RData")
jags_oospreds_summary_df$res <- dat_sub$voteshare



### Dynamic Model  -----------------------------------------------------------------------------


## ATTENTION: The following code will run the model for four elections and seven cutoffs each.
## This will take some time. A crude estimation is 42 hours. So only run it if you don't actually 
## need your computer in the next 42 hours (or parallelize it ;) ).

## load the polls data 
load("../data/model_input/polls_comb_results.RData")

## Get Structural Forcasts
load("../data/model_output/structural_forecasts.RData")

# Sampler
nBurn <- 100000
nThin <- 100
nIter <- 100000
model_file <- "polling-model.jags"
save_var <- c("alpha", "s", "house_effect", "forcast")


## Year 2002 -----------------------------------------------------------

# Specifications
Election <- 2002 

# Filter the 2002 election cases from structural forecasts
structural_forcasts <- jags_oospreds_summary_df[jags_oospreds_summary_df$election == Election, ]

prior_mean <- structural_forcasts$mean / 100
prior_sigma <- structural_forcasts$sd / 100

beta_priors <- t(apply(cbind(prior_mean,prior_sigma), 1, function(x) estBetaParams(x[1],x[2]^2)))   
rownames(beta_priors) <- structural_forcasts$party

# Cutoffs
cutoffs <- c(seq(from = 148, to = 1, by = -(28)), 1) 
forcast_df <- data.frame("high" = NULL,  "mid" = NULL, "low" = NULL, "party" = NULL, "time" = NULL)

for(cutoff in cutoffs){
  # Print
  cat("\n Estimating Model for Election", Election, "with a cutoff of ", cutoff, "\n")
  
  # Set time_window of Data 
  max_days_to_election <- 365
  
  # How many days before the election should be included?
  time_window <- max_days_to_election:cutoff  
  
  # parties
  parties <- "cdu|spd|fdp|lin|afd|gru|other|oth"
  
  # Prepare Data;  spread to wide format
  polls_long <- ger_polls_results %>%
    filter(days_to_election %in% time_window) %>%
    filter(year==Election) %>%
    filter(!is.na(support)) %>%
    select(party, support, days_to_election, sample_size, institute) %>%
    mutate(t= time_window[1] - days_to_election +1 ) %>%
    arrange(desc(days_to_election)) %>%
    mutate(iid = match(institute, sort(unique(institute)))) 
  
  polls <- polls_long %>%
    spread(party, support)  %>% # Reshape to wide format
    mutate(oth = 100 - rowSums(.[grep(parties, names(.))], na.rm = TRUE)) %>% 
    mutate(sample_size = replace(sample_size, is.na(sample_size), 1000)) %>%
    na.omit()
  
  # Party names that are included in data
  party_names <- grep(parties, names(polls), value = T)
  
  # Prepare Data for Jags
  # Grep existing Parties and transform share to number
  Y <- round(as.matrix(polls[,grep(parties, names(polls))]/100) * polls$sample_size) 
  NObs <- apply(Y,1,sum)
  nParties <-ncol(Y)
  
  forJags <- list(y = Y,
                  nParties = nParties,
                  nPeriods =  time_window[1]+1,
                  nPolls = nrow(Y),
                  iid = polls$iid,
                  nInst = max(polls$iid),
                  date = polls$t,
                  size = NObs, 
                  beta_priors = beta_priors[party_names,], # Order
                  R0 = diag(rep(1,(nParties-1))), 
                  n0 = (nParties)
  )
  
  # Run Jags Model
  jags.mod <- jags.model(file = model_file,
                         data = forJags,
                         n.chains = 2,
                         n.adapt = 1000)
  
  update(jags.mod,nBurn)
  
  res_brw <- coda.samples(jags.mod,
                          n.iter = nIter,
                          thin = nThin,
                          variable.names = save_var)
  
  # Prepare Results
  r <- as.matrix(res_brw)
  df_res <- as.data.frame(t(apply(r, 2, quantile, c(0.01, 0.025, 0.05, 0.5, 0.95, 0.975, 0.99))))
  names(df_res) <- c(paste0("low", 1:3), "mid", paste0("high", 3:1))
  df_res$election <- Election
  df_res$cutoff <- cutoff
  split_rownames <- as.data.frame(str_split_fixed(rownames(df_res), "\\[|,|\\]", 4)[,1:3])
  colnames(split_rownames) <- c("par", "i1", "i2")
  split_rownames[,2:3] <- apply(split_rownames[,2:3], 2, as.numeric)
  df <- cbind(df_res, split_rownames)
  
  # Save Results
  cat("\n Save Results for Election", Election, "with a cutoff of ", cutoff, "\n")
  saveRDS(df, file = paste0("../data/model_output/results_", Election, "_", cutoff, ".rds"))
}


## Year 2005 -----------------------------------------------------------

# Specifications
Election <- 2005 

# Filter the 2005 election cases from structural forecasts
structural_forcasts <- jags_oospreds_summary_df[jags_oospreds_summary_df$election == Election, ]

prior_mean <- structural_forcasts$mean / 100
prior_sigma <- structural_forcasts$sd / 100

beta_priors <- t(apply(cbind(prior_mean,prior_sigma), 1, function(x) estBetaParams(x[1],x[2]^2)))   
rownames(beta_priors) <- structural_forcasts$party

# Cutoffs
cutoffs <- c(seq(from = 148, to = 1, by = -(28)), 1) 
forcast_df <- data.frame("high" = NULL,  "mid" = NULL, "low" = NULL, "party" = NULL, "time" = NULL)

for(cutoff in cutoffs){
  
  # Print
  cat("\n Estimating Model for Election", Election, "with a cutoff of ", cutoff, "\n")
  
  # Set time_window of Data 
  max_days_to_election <- 365
  
  # How many days before the election should be included?
  time_window <- max_days_to_election:cutoff  
  
  # parties
  parties <- "cdu|spd|fdp|lin|afd|gru|other|oth"
  
  # Prepare Data; filter=year; select=variables; spread to wide format
  polls_long <- ger_polls_results %>%
    filter(days_to_election %in% time_window) %>%
    filter(year==Election) %>%
    filter(!is.na(support)) %>%
    select(party, support, days_to_election, sample_size, institute) %>%
    mutate(t= time_window[1] - days_to_election +1 ) %>%
    arrange(desc(days_to_election)) %>%
    mutate(iid = match(institute , sort(unique(institute)))) 
  
  polls <- polls_long %>%
    spread(party, support)  %>% # Reshape to wide format
    mutate(oth = 100 - rowSums(.[grep(parties, names(.))], na.rm = TRUE)) %>% 
    mutate(sample_size = replace(sample_size, is.na(sample_size), 1000)) %>%
    na.omit()
  
  # Party names that are included in data
  party_names <- grep(parties, names(polls),value = T)
  
  # Prepare Data for Jags
  # Grep existing Parties and transform share to number
  Y <- round(as.matrix(polls[,grep(parties, names(polls))]/100) * polls$sample_size) 
  NObs <- apply(Y,1,sum)
  nParties <-ncol(Y)
  
  forJags <- list(y = Y,
                  nParties = nParties,
                  nPeriods =  time_window[1]+1,
                  nPolls = nrow(Y),
                  iid = polls$iid,
                  nInst = max(polls$iid),
                  date = polls$t,
                  size = NObs, 
                  beta_priors = beta_priors[party_names,] # Order
  )
  
  # Run Jags Model
  jags.mod <- jags.model(file = model_file,
                         data = forJags,
                         n.chains = 2,
                         n.adapt = 1000)
  update(jags.mod, nBurn)
  res_brw <- coda.samples(jags.mod,
                          n.iter = nIter,
                          thin = nThin,
                          variable.names = save_var)
  
  # Prepare Results
  r <- as.matrix(res_brw)
  df_res <- as.data.frame(t(apply(r, 2, quantile, c(0.01, 0.025, 0.05, 0.5, 0.95, 0.975, 0.99))))
  names(df_res) <- c(paste0("low", 1:3), "mid", paste0("high", 3:1))
  df_res$election <- Election
  df_res$cutoff <- cutoff
  split_rownames <- as.data.frame(str_split_fixed(rownames(df_res), "\\[|,|\\]", 4)[,1:3])
  colnames(split_rownames) <- c("par", "i1", "i2")
  split_rownames[,2:3] <- apply(split_rownames[,2:3], 2, as.numeric)
  df <- cbind(df_res, split_rownames)
  
  # Save Results
  cat("\n Save Results for Election", Election, "with a cutoff of ", cutoff, "\n")
  saveRDS(df, file = paste0("../data/model_output/results_", Election, "_", cutoff, ".rds"))
}

## Year 2009 -----------------------------------------------------------

# Specifications
Election <- 2009

# Filter the 2009 election cases from structural forecasts
structural_forcasts <- jags_oospreds_summary_df[jags_oospreds_summary_df$election == Election, ]

prior_mean <- structural_forcasts$mean / 100
prior_sigma <- structural_forcasts$sd / 100

beta_priors <- t(apply(cbind(prior_mean,prior_sigma), 1, function(x) estBetaParams(x[1],x[2]^2)))   
rownames(beta_priors) <- structural_forcasts$party

# Cutoffs
cutoffs <- c(seq(from = 148, to = 1, by = -(28)), 1) 
forcast_df <- data.frame("high" = NULL,  "mid" = NULL, "low" = NULL, "party" = NULL, "time" = NULL)

for(cutoff in cutoffs){
  
  # Print
  cat("\n Estimating Model for Election", Election, "with a cutoff of", cutoff, "\n")
  
  # Set time_window of Data 
  max_days_to_election <- 365
  
  # How many days before the election should be included?
  time_window <- max_days_to_election:cutoff 
  
  # parties
  parties <- "cdu|spd|fdp|lin|afd|gru|other|oth"
  
  # Prepare Data; filter=year; select=variables; spread to wide format
  polls_long <- ger_polls_results %>%
    filter(days_to_election %in% time_window) %>%
    filter(year == Election) %>%
    filter(!is.na(support)) %>%
    select(party, support, days_to_election, sample_size, institute) %>%
    mutate(t= time_window[1] - days_to_election +1 ) %>%
    arrange(desc(days_to_election)) %>%
    mutate(iid = match(institute, sort(unique(institute))))
  
  polls <- polls_long %>%
    spread(party, support)  %>% # Reshape to wide format
    mutate(oth = 100 - rowSums(.[grep(parties, names(.))], na.rm = TRUE)) %>% 
    mutate(sample_size = replace(sample_size, is.na(sample_size), 1000)) %>%
    na.omit()
  
  # Party names that are included in data
  party_names <- grep(parties, names(polls),value = T)
  
  # Prepare Data for Jags
  # Grep existing Parties and transform share to number
  Y <- round(as.matrix(polls[,grep(parties, names(polls))]/100) * polls$sample_size) 
  NObs <- apply(Y,1,sum)
  nParties <-ncol(Y)
  
  forJags <- list(y = Y,
                  nParties = nParties,
                  nPeriods =  time_window[1]+1,
                  nPolls = nrow(Y),
                  iid = polls$iid,
                  nInst = max(polls$iid),
                  date = polls$t,
                  size = NObs, 
                  beta_priors = beta_priors[party_names,], # Order
                  R0 = diag(rep(1,(nParties-1))), n0 = (nParties)
  )
  
  # Run Jags Model
  jags.mod <- jags.model(file = model_file,
                         data = forJags,
                         n.chains = 2,
                         n.adapt = 1000)
  update(jags.mod,nBurn)
  res_brw <- coda.samples(jags.mod,
                          n.iter = nIter,
                          thin = nThin,
                          variable.names = save_var)
  
  # Prepare Results
  r <- as.matrix(res_brw)
  df_res <- as.data.frame(t(apply(r, 2, quantile, c(0.01, 0.025, 0.05, 0.5, 0.95, 0.975, 0.99))))
  names(df_res) <- c(paste0("low", 1:3), "mid", paste0("high", 3:1))
  df_res$election <- Election
  df_res$cutoff <- cutoff
  split_rownames <- as.data.frame(str_split_fixed(rownames(df_res), "\\[|,|\\]", 4)[,1:3])
  colnames(split_rownames) <- c("par", "i1", "i2")
  split_rownames[,2:3] <- apply(split_rownames[,2:3], 2, as.numeric)
  df <- cbind(df_res, split_rownames)
  
  # Save Results
  cat("\n Save Results for Election", Election, "with a cutoff of ", cutoff, "\n")
  saveRDS(df, file = paste0("../data/model_output/results_", Election, "_", cutoff, ".rds"))
}

## Year 2013 -----------------------------------------------------------

# Specifications
Election <- 2013 

# Filter the 2013 election cases from structural forecasts
structural_forcasts <- jags_oospreds_summary_df[jags_oospreds_summary_df$election == Election, ]

## Add AFD Forcast to others
prior_mean <-  structural_forcasts$mean[-1]/100

# Add AFD to Other
prior_mean[5] <- prior_mean[5] + structural_forcasts$mean[1]/100
prior_sigma <- structural_forcasts$sd[-1]/100

beta_priors <- t(apply(cbind(prior_mean,prior_sigma), 1, function(x) estBetaParams(x[1],x[2]^2)))   
rownames(beta_priors) <- structural_forcasts$party[-1]

# Cutoffs
cutoffs <- c(seq(from = 148, to = 1, by = -(28)), 1) 
forcast_df <- data.frame("high" = NULL,  "mid" = NULL, "low" = NULL, "party" = NULL, "time" = NULL)

for(cutoff in cutoffs){
  
  # Print
  cat("\n Estimating Model for Election", Election, "with a cutoff of ", cutoff, "\n")
  
  # Set time_window of Data 
  max_days_to_election <- 365
  
  # How many days before the election should be included?
  time_window <- max_days_to_election:cutoff  
  
  # parties
  parties <- "cdu|spd|fdp|lin|afd|gru|other|oth"
  
  # Prepare Data; filter=year; select=variables; spread to wide format
  polls_long <- ger_polls_results %>%
    filter(days_to_election %in% time_window) %>%
    filter(year==Election) %>%
    select(party, support, days_to_election, sample_size, institute) %>%
    mutate(t= time_window[1] - days_to_election +1 ) %>%
    arrange(desc(days_to_election)) %>%
    mutate(iid = match(institute , sort(unique(institute))))
  
  polls <- polls_long %>%
    spread(party, support)  %>% # Reshape to wide format
    mutate(oth = 100 - rowSums(.[grep(parties, names(.))], na.rm = TRUE)) %>% 
    mutate(sample_size = replace(sample_size, is.na(sample_size), 1000)) %>%
    mutate(afd = replace(afd, is.na(afd), 0)) %>%
    mutate(oth = oth + afd) %>% # Put afd Voteshare to others
    select(-afd) %>%
    na.omit() # Drop afd 
  
  # Party names that are included in data
  party_names <- grep(parties, names(polls),value = T)
  
  # Prepare Data for Jags
  # Grep existing Parties and transform share to number
  Y <- round(as.matrix(polls[,grep(parties, names(polls))]/100) * polls$sample_size) 
  NObs <- apply(Y,1,sum)
  nParties <- ncol(Y)
  
  forJags <- list(y = Y,
                  nParties = nParties,
                  nPeriods =  time_window[1]+1,
                  nPolls = nrow(Y),
                  iid = polls$iid,
                  nInst = max(polls$iid),
                  date = polls$t,
                  size = NObs, 
                  beta_priors = beta_priors[party_names,], # Order
                  R0 = diag(rep(1,(nParties-1))), 
                  n0 = (nParties)
  )
  
  
  # Run Jags Model
  jags.mod <- jags.model(file = model_file,
                         data=forJags,
                         n.chains=2,
                         n.adapt=1000)
  update(jags.mod,nBurn)
  res_brw <- coda.samples(jags.mod,
                          n.iter=nIter,thin=nThin,
                          variable.names=save_var)
  
  # Prepare Results
  r <- as.matrix(res_brw)
  df_res <- as.data.frame(t(apply(r, 2, quantile, c(0.01, 0.025, 0.05, 0.5, 0.95, 0.975, 0.99))))
  names(df_res) <- c(paste0("low", 1:3), "mid", paste0("high", 3:1))
  df_res$election <- Election
  df_res$cutoff <- cutoff
  split_rownames <- as.data.frame(str_split_fixed(rownames(df_res), "\\[|,|\\]" ,4)[,1:3])
  colnames(split_rownames) <- c("par", "i1", "i2")
  split_rownames[,2:3] <- apply(split_rownames[,2:3], 2, as.numeric)
  df <- cbind(df_res, split_rownames)
  
  # Save Results
  cat("\n Save Results for Election", Election, "with a cutoff of ", cutoff, "\n")
  saveRDS(df, file = paste0("../data/model_output/results_", Election, "_", cutoff, ".rds"))
}


## Year 2017 -----------------------------------------------------------


# Specifications
Election <- 2017 # For 2017 election

# Sampler
nBurn <- 150000
nThin <- 200
nIter <- 200000
nChains <- 5
nSamples <- 2000/nChains
thin <- 100


# Load Poll Data-base
ger_polls <- ger_polls_results %>% 
  filter(year == Election) %>% # Filter Polls for Election
  mutate(party = as.character(as_factor(party))) 

# Load Structural Forcasts and transform to beta - priors
structural_forcasts <- jags_oospreds_summary_df[jags_oospreds_summary_df$election == Election, ]
prior_mean <- structural_forcasts$mean/100
prior_sigma <- structural_forcasts$sd/100
beta_priors <- t(apply(cbind(prior_mean,prior_sigma), 1, function(x) estBetaParams(x[1],x[2]^2)))   
rownames(beta_priors) <- structural_forcasts$party

# Cutoffs (until which Date polls should be included)
cutoff <- min(ger_polls$days_to_election, na.rm = T)

# Print Model estimation
cat("\n Estimating Model for Election", Election, "with a cutoff of ", cutoff, "\n")

# Set time_window for data 
max_days_to_election <- 365 # Start point up from which Polls are included
time_window <- max_days_to_election:cutoff  # time window

# Party ordering include 
party_names <- c("cdu", "spd","lin","gru","fdp","afd","oth") 

# Prepare Data
polls <- ger_polls %>%
  filter(days_to_election %in% time_window) %>% # Filter Polls in time-window
  select(party, support, days_to_election, sample_size, institute) %>% # Select important information
  mutate(t= time_window[1] - days_to_election + 1) %>% # create t variable from 1:366
  arrange(desc(days_to_election)) %>% # Arrange according to days to election
  mutate(iid = match(institute , sort(unique(institute)))) %>% # Creat Institute id
  spread(party, support)  %>% # Reshape to wide format
  na.omit() # Omit missing

# Prepare Data for Jags
Y <- round(as.matrix(polls[,party_names]/100) * 
             polls$sample_size) # Grep existing Parties and transform share to number
NObs <- apply(Y,1,sum) # Number of obserations
nParties <-ncol(Y) # Number of parties

forJags <- list(y = Y,
                nParties = nParties,
                nPeriods =  time_window[1]+1, 
                nPolls = nrow(Y),
                iid = polls$iid,
                nInst = max(polls$iid),
                date = polls$t,
                size = NObs, 
                beta_priors = beta_priors[party_names,] # Order priors 
)


cl <- makeCluster(ifelse(nChains<(detectCores()-1),nChains, (detectCores()-1)))
# Run the chains in parallel rjags models (4 models with 2 chains each) on this cluster:
results <- run.jags(model = model_file, n.chains = nChains, data = forJags,
                    method = "rjparallel", cl = cl, monitor = save_var, sample = nSamples, 
                    thin = thin, burnin = nBurn)
stopCluster(cl)


saveRDS(results, file = "../data/model_output/res_brw.RDS")

# # Diagnostics. Not in paper. Just in case.
# 
# sink(paste("../data/model_output/draws-",cutoff,".text",sep=""))
# 
# s <- paste("alpha[",1:(365-158),",",1:7,"]",sep="")
# gelman.diag(results$mcmc[,s]) #
# 
# s <- paste("alpha[",(365-160):366,",",1:7,"]",sep="")
# gelman.diag(results$mcmc[,s]) # 
# 
# s <- paste("alpha[",366,",",7,"]",sep="")
# gelman.diag(results$mcmc[,s]) # 
# 
# s <- paste("s[",1:6,"]",sep="")
# gelman.diag(results$mcmc[,s]) # 
# s <- paste("house_effect[",1,",",1:6,"]",sep="")
# gelman.diag(results$mcmc[,s]) # 
# 
# s <- paste("forcast[",1:6,"]",sep="")
# gelman.diag(results$mcmc[,s])
# summary(results$mcmc[,s])
# 
# s <- paste("forcast[",7,"]",sep="")
# gelman.diag(results$mcmc[,s])
# summary(results$mcmc[,s])
# sink()

draws_forcast_levels <- list() # Output Object

# Grep Levels, put in array and attach to list
levels <- array(NA, c(2000, nParties, 366))

for(t in 1:366){
  sel_levels_temp <- paste0("alpha[", t, ",", 1:nParties, "]")
  levels[,,t] <- as.matrix(results$mcmc[,sel_levels_temp])
}

draws_forcast_levels[["levels"]] <- levels

# Grep forcast and attach to list
sel_forcast <- paste("forcast[",1:nParties,"]",sep="")
draws_forcast_levels[["forcast"]] <- as.matrix(results$mcmc[,sel_forcast])

# Attach partynames to list
draws_forcast_levels[["party_names"]] <- party_names

# Attach Polls used for estimation
draws_forcast_levels[["polls"]] <- polls

saveRDS(draws_forcast_levels, 
        file = paste0("..data/model_output/dynamic_model/draws_forcast_levels_", 
                   Election, "_", cutoff, ".rds"))


