###################################################################################################
### "Zweitstimme.org. Ein strukturell-dynamisches Vorhersagemodell für Bundestagswahlen Election
### polling forecasting for election 2002"
###
###
### Simon Munzert, Lukas F. Stoetzer, Thomas Gschwend, Marcel Neunhoeffer, Sebastian Sternberg
###
###
### Replication file: Visualization


# In order to replicate a single graph or table, please run the "Preparatory Steps" part and then 
# the code below the object of interest. After running the sript, the files in folder "Graphs"
# will be overwritten. 


### Preparatory Steps  ----------------------------------------------------------------------------

## Set working directory to current file location (for Rstudio)
p_needed <- "rstudioapi"
packages <- rownames(installed.packages())
p_to_install <- p_needed[!(p_needed %in% packages)]
if (length(p_to_install) > 0) {install.packages(p_to_install)}
require(rstudioapi)

setwd(dirname(getActiveDocumentContext()$path))

## Set it manually if you are not working with RStudio
# setwd("path/to/file")

Sys.setlocale("LC_CTYPE", "de_DE")

## Load required packages and functions
source("packages.R")
source("functions.R")


# Make Paper: If set to TRUE plots will be saved to directory. 
# Set TRUE for reproducing results in the paper
MAKE_PAPER <- FALSE

### Abbildung 1: Bivariate Zusammenhänge zwischen Prädiktoren im strukturellen Modell und --------- 
### Zweitstimmenanteil der Parteien, 1953 – auf Wahlergebnisse der SPD und Union beschränkt   

## Load all required data
load("../data/model_input/ger_model_df.RData")

## Abbildung 1
if ( MAKE_PAPER ) {
pdf(file = "../graphs/Abbildung_1.pdf", height = 9, width = 8, family = "URWTimes")
}

par(oma = c(0, 0.5, 0.5, 0.5))
par(mar = c(4, 4, 3, 0))
par(pty = "s")
layout(matrix(c(1, 2, 3, 3), 2, 2, byrow = TRUE))

# (a) Zurückliegender Wahlanteil
plot(ger_df_long$voteshare_l1, ger_df_long$voteshare, xaxt = "n", yaxt = "n", xlab = "", ylab = "", 
     main = "(a)", xlim = c(0, 50), ylim = c(0, 50))
axis(1, seq(0, 100, 10), seq(0, 100, 10))
axis(1, 25, "Stimmenanteil (%), letzte Wahl", line = 1, tick = F)
axis(2, seq(0, 100, 10), seq(0, 100, 10))
axis(2, 25, "Stimmenanteil (%)", line = 1, tick = F)

# run model, add regression line
model_out <- lm(voteshare ~ voteshare_l1 - 1, data = ger_df_long)
model_out_aug <- broom::augment(model_out)
model_out_aug$case_label <- paste(ger_df_long$party, ger_df_long$year) %>% 
  recode_partynames(longnames = FALSE) %>% 
  .[model_out_aug$.rownames %>% num()] %>% 
  recode_years
abline(model_out, lty = 2)

# identify important outliers
obs_id <- abs(model_out_aug$.std.resid) > 1.53
points(model_out_aug$voteshare_l1[obs_id], model_out_aug$voteshare[obs_id], pch = 20)

# plot labels of outliers based on resid or cooksd 
label_position <- ifelse(model_out_aug$.resid > 0, 3, 1)
text(model_out_aug$voteshare_l1[obs_id], model_out_aug$voteshare[obs_id], 
     label = model_out_aug$case_label[obs_id], cex = .7, 
     pos = label_position[obs_id], offset = .47)
grid()

# (b) Umfragen
plot(ger_df_long$polls_200_230, ger_df_long$voteshare, xaxt = "n", yaxt = "n", xlab = "",
     ylab = "", main = "(b)", xlim = c(0, 60), ylim = c(0, 60))
axis(1, seq(0, 100, 10), seq(0, 100, 10))
axis(1, 30, "Wahlabsicht in Umfragen, \n200-230 Tage vor der Wahl", line = 2, tick = F)
axis(2, seq(0, 100, 10), seq(0, 100, 10))
axis(2, 30, "Stimmenanteil (%)", line = 1, tick = F)

# run model, add regression line
model_out <- lm(voteshare ~ polls_200_230 - 1, data = ger_df_long)
model_out_aug <- augment(model_out)
model_out_aug$case_label <- paste(ger_df_long$party, ger_df_long$year) %>%
  recode_partynames(longnames = FALSE) %>%
  .[model_out_aug$.rownames %>% num()] %>%
  recode_years
abline(model_out, lty = 2)

# identify important outliers
obs_id <- abs(model_out_aug$.std.resid) > 1.53
points(model_out_aug$polls_200_230[obs_id], model_out_aug$voteshare[obs_id], pch = 20)

# plot labels of outliers based on resid or cooksd 
label_position <- ifelse(model_out_aug$.resid > 0, 3, 1)
text(model_out_aug$polls_200_230[obs_id], model_out_aug$voteshare[obs_id], 
     label = model_out_aug$case_label[obs_id], cex = .7, 
     pos = label_position[obs_id], offset = .47)
grid()

par(mar = c(3, 15, 3, 11))

# (c) Kanzlerpartei
dat <- filter(ger_df_long, major == 1)
dat$chancellor_party_lab <- ifelse(dat$chancellor_party == 0, "keine Kanzlerpartei", "Kanzlerpartei")
plot(dat$chancellor_party,  dat$voteshare, xaxt = "n", yaxt = "n", xlab = "", ylab = "", 
     main = "(c)", xlim = c(-.5, 1.5), ylim = c(20, 52))
axis(2, seq(0, 100, 10), seq(0, 100, 10))
axis(1, seq(0, 1, 1), c("keine Kanzlerpartei", "Kanzlerpartei"), tick = F)
axis(2, 35, "Stimmenanteil (%)", line = 1, tick = F)

# run model, add regression line
model_out <- lm(voteshare ~ chancellor_party, data = dat)
model_out_aug <- augment(model_out)
model_out_aug$case_label <- paste(dat$party, dat$year) %>%
  recode_partynames(longnames = FALSE) %>% 
  .[model_out_aug$.rownames %>% num()] %>%
  recode_years
abline(model_out, lty = 2)

# identify important outliers
model_out_aug <- group_by(model_out_aug, chancellor_party) %>%
  arrange(.std.resid) %>%
  mutate(label_position = c(4, rep(c(2, 4), 8))) %>%
  ungroup
obs_id <- abs(model_out_aug$.std.resid) > 1.3
points(model_out_aug$chancellor_party[obs_id], model_out_aug$voteshare[obs_id], pch = 20)

# plot labels of outliers based on resid or cooksd 
text(model_out_aug$chancellor_party[obs_id], model_out_aug$voteshare[obs_id], 
     label = model_out_aug$case_label[obs_id], cex = .7, 
     pos = model_out_aug$label_position[obs_id], offset = .47)
grid()

if ( MAKE_PAPER ) {
dev.off()
}

### Abbildung 2: Koeffizientenschätzungen des strukturellen Modells, 1953–2017  -------------------

load("../data/model_output/structural_summary.RData")
load("../data/model_input/ger_model_df.RData")

# extract the data for plotting 
b_0_mean <- filter(jags_summary_df, str_detect(var, "b0\\[")) %>% magrittr::extract2("mean")
b_0_95lo <- filter(jags_summary_df, str_detect(var, "b0\\[")) %>% magrittr::extract2("q95lo")
b_0_95hi <- filter(jags_summary_df, str_detect(var, "b0\\[")) %>% magrittr::extract2("q95hi")

b_voteshare_l1_mean <- filter(jags_summary_df, str_detect(var, "b\\[.{1,2},1\\]")) %>% 
  magrittr::extract2("mean")
b_voteshare_l1_95lo <- filter(jags_summary_df, str_detect(var, "b\\[.{1,2},1\\]")) %>% 
  magrittr::extract2("q95lo")
b_voteshare_l1_95hi <- filter(jags_summary_df, str_detect(var, "b\\[.{1,2},1\\]")) %>% 
  magrittr::extract2("q95hi")

b_chancellor_party_mean <- filter(jags_summary_df, str_detect(var, "b\\[.{1,2},2\\]")) %>% 
  magrittr::extract2("mean")
b_chancellor_party_95lo <- filter(jags_summary_df, str_detect(var, "b\\[.{1,2},2\\]")) %>% 
  magrittr::extract2("q95lo")
b_chancellor_party_95hi <- filter(jags_summary_df, str_detect(var, "b\\[.{1,2},2\\]")) %>% 
  magrittr::extract2("q95hi")

b_polls_200_230_mean <- filter(jags_summary_df, str_detect(var, "b\\[.{1,2},3\\]")) %>% 
  magrittr::extract2("mean")
b_polls_200_230_95lo <- filter(jags_summary_df, str_detect(var, "b\\[.{1,2},3\\]")) %>% 
  magrittr::extract2("q95lo")
b_polls_200_230_95hi <- filter(jags_summary_df, str_detect(var, "b\\[.{1,2},3\\]")) %>% 
  magrittr::extract2("q95hi")


election_years <- unique(ger_df_long$year)
election_years_id <- seq_along(election_years)
election_years_df <- data.frame(year = election_years, election_id = election_years_id)

### Abbildung 2
if ( MAKE_PAPER ) {
pdf(file="../graphs/Abbildung_2.pdf", height=8, width=6, family="URWTimes")
}

par(mar=c(4,4,2,0)+.1)
par(oma=c(0,0,0,0)+.1)
par(mfrow=c(3,1))

# Stimmenanteil bei der letzten Bundestagswahl
min_yaxis <- round(min(b_voteshare_l1_95lo[-1]), 1) - .1
max_yaxis <- round(max(b_voteshare_l1_95hi[-1]), 1) + .1
plot(election_years_id[-1], b_voteshare_l1_mean[-1], type = "l", xlab = "", ylab = "", 
     xaxt = "n", yaxt = "n", ylim = c(min_yaxis, max_yaxis), 
     main = "Stimmenanteil bei der letzten Bundestagswahl")
axis(1, election_years_id[-1], labels = FALSE)
text(x = election_years_id[-1], y = -.32, labels = election_years[-1], par("usr")[1] - .5, 
     srt = 45, xpd = TRUE)
axis(2, seq(min_yaxis, max_yaxis, .2), labels = FALSE)
text(y = seq(min_yaxis, max_yaxis, .2), labels = seq(min_yaxis, max_yaxis, .2), 
     par("usr")[1]-.25, srt = 0, pos = 2, xpd = TRUE)
polygon(x = c(election_years_id[-1], rev(election_years_id[-1])), 
        y = c(b_voteshare_l1_95hi[-1], rev(b_voteshare_l1_95lo[-1])),
        col = rgb(.8, .8, .8, .5),
        border = NA)
lines(election_years_id[-1], b_voteshare_l1_mean[-1])
grid()

# Wahlabsicht in Umfragen, 200−230 Tage vor der Wahl
min_yaxis <- round(min(b_polls_200_230_95lo[-1]), 1) - .1
max_yaxis <- round(max(b_polls_200_230_95hi[-1]), 1) + .1
plot(election_years_id[-1], b_polls_200_230_mean[-1], type = "l", xlab = "", 
     ylab = "", xaxt = "n", yaxt = "n", ylim = c(min_yaxis, max_yaxis), 
     main = "Wahlabsicht in Umfragen, 200-230 Tage vor der Wahl")
axis(1, election_years_id[-1], labels = FALSE)
text(x = election_years_id[-1], y = 0.08, labels = election_years[-1], 
     par("usr")[1]-.5, srt = 45, xpd = TRUE)
axis(2, seq(min_yaxis, max_yaxis, .2), labels = FALSE)
text(y = seq(min_yaxis, max_yaxis, .2), labels = seq(min_yaxis, max_yaxis, .2), 
     par("usr")[1]-.25, srt = 0, pos = 2, xpd = TRUE)
polygon(x = c(election_years_id[-1], rev(election_years_id[-1])), 
        y = c(b_polls_200_230_95hi[-1], rev(b_polls_200_230_95lo[-1])),
        col = rgb(.8, .8, .8, .5),
        border = NA)
lines(election_years_id[-1], b_polls_200_230_mean[-1])
grid()

# Kanzlerpartei
min_yaxis <- round(min(b_chancellor_party_95lo[-1]), 0) - 1
max_yaxis <- round(max(b_chancellor_party_95hi[-1]), 0) + 1
plot(election_years_id[-1], b_chancellor_party_mean[-1], type = "l", xlab = "", 
     ylab = "", xaxt = "n", yaxt = "n", ylim = c(min_yaxis, max_yaxis), 
     main = "Kanzlerpartei")
axis(1, election_years_id[-1], labels = FALSE)
text(x = election_years_id[-1], y = -9.5, labels = election_years[-1], 
     par("usr")[1]-.5, srt = 45, xpd = TRUE)
axis(2, seq(-5, 20, 5), labels = FALSE)
text(y = seq(-5, 20, 5), labels = seq(-5, 20, 5), par("usr")[1]-.25, srt = 0,
     pos = 2, xpd = TRUE)
polygon(x = c(election_years_id[-1], rev(election_years_id[-1])), 
        y = c(b_chancellor_party_95hi[-1], rev(b_chancellor_party_95lo[-1])),
        col = rgb(.8, .8, .8, .5),
        border = NA)
lines(election_years_id[-1], b_chancellor_party_mean[-1])
grid()

if ( MAKE_PAPER ) {
dev.off()
}
### Abbildung 3: Out-of-sample-Vorhersagen des strukturellen Modells von Parteianteilen bei -------
### Bundestagswahlen, 1953–2013 

load("../data/model_output/structural_forecasts.RData")
load("../data/model_output/structural_jags_simulations.RData")

if ( MAKE_PAPER ) {
pdf(file = "../graphs/Abbildung_3.pdf", height = 5, width = 10, family = "URWTimes")
}

par(mar = c(4, 4, 0, 0) + .1)
par(oma = c(0, 0, 0, 0) + .1)
par(pty = "s")
par(mfrow = c(1, 2))

plot(jags_oospreds_summary_df$mean, dataList$y, xlab = "", ylab = "", ylim = c(0, 54), 
     xlim = c(0, 54))
axis(1, 26, "Vorhergesagter Wahlanteil", line = 1, tick = F)
axis(2, 26, "Beobachteter Wahlanteil", line = 1, tick = F)
obs_resid <- jags_oospreds_summary_df$mean - dataList$y
obs_show <- abs(obs_resid) > 4 & !is.na(obs_resid)
label_position <- ifelse(obs_resid < 0, 3, 1)
text(jags_oospreds_summary_df$mean[obs_show], dataList$y[obs_show], dat_full$case_label[obs_show], 
     pos = label_position[obs_show], offset = .5, cex = .8)
grid()
abline(0, 1)

swing_pred <- jags_oospreds_summary_df$mean - dataList$x[,"voteshare_l1"]
swing_real <- dataList$y - dataList$x[,"voteshare_l1"]
plot(swing_pred, swing_real, xlab = "", ylab = "", ylim = c(-12, 16), xlim = c(-12, 16))
axis(1, 1, "Vorhergesagte Änderung des Wahlanteils", line = 1, tick = F)
axis(2, 1, "Beobachtete Änderung des Wahlanteils", line = 1, tick = F)
obs_resid <- swing_pred - swing_real
obs_show <- abs(obs_resid) > 4 & !is.na(obs_resid)
label_position <- ifelse(obs_resid < 0, 3, 1)
text(swing_pred[obs_show], swing_real[obs_show], dat_full$case_label[obs_show], 
     pos = label_position[obs_show], offset = .5, cex = .8)
grid()
abline(0, 1)
abline(v = 0)
abline(h = 0)

if ( MAKE_PAPER ) {
dev.off()
}

### Abbildung 4: Dynamische Vorhersagen des SPD-Stimmenanteils, Bundestagswahl 2013  --------------

## Load required datasets
load("../data/model_output/structural_forecasts.RData")
load("../data/model_input/polls_comb_results.RData")

## Load results of dynamic forecasting model (for 2002 - 2013 elections)
res_folder <- "../data/model_output/dynamic_model/"
res_files <- dir(res_folder)[!grepl("2017", dir(res_folder))]
df_res <- readRDS(paste0(res_folder, res_files[1]))
for(i in 2:length(res_files)){
  df_res <- rbind(df_res, readRDS(paste0(res_folder, res_files[i])))
}

## Prepare a dataframe of dynamic forecasts
df_res <- as_data_frame(df_res)
df_res[,c(1:7, 11:12)] <- apply(df_res[, c(1:7, 11:12)], 2, function(x) as.numeric(as.character(x)))

## Prepare a dataframe of structural forecasts
struc_forcast <- jags_oospreds_summary_df %>% # structiral model results, out of sample forecast
  select(election, party, mean, sd) %>%
  dplyr::rename(year = election)

## Combine the structural forecast dataframe and election results data
df_res_for <- inner_join(struc_forcast, ger_results_lag[,1:3], by = c("year", "party"))
df_res_for$share <- ifelse(df_res_for$share == "NaN", NA, df_res_for$share)

## Create Plots for Levels
df_levels <- df_res %>%  
  filter(par == "alpha")  %>% 
  dplyr::rename(party = i2, t = i1) %>%
  group_by(election) %>%
  do(mutate(., days_to_election = max(t) - t)) 

# Change Party Labels
df_levels$party <- recode(df_levels$party, `1` = "cdu",`2` = "fdp", `3` = "gru", 
                          `4` = "lin", `5` = "spd", `6` = "oth")

# Select the cutoffs and elections that exist in dynamic model data
unique_cutoffs <-  sort(unique(df_levels$cutoff))
unique_elections <- unique(df_levels$election)

# Choose the party and year for the graph
p <- "spd"
y <- 2013
c <- c(148, 64, 8, 1)

# Subset the data required for plotting
df_plot_party <- filter(df_levels,
                        party == p,
                        days_to_election < 200,
                        election == y,
                        cutoff %in% c)

# Name Cutoffs
df_plot_party$cutoff <- factor(df_plot_party$cutoff)
df_plot_party <- transform(df_plot_party,
                           cutoff = factor(cutoff, levels = c("148", "64", "8", "1")))
levels(df_plot_party$cutoff) <- c(paste("Umfragen bis", c("148", "64", "8"), "Tage vor der Wahl"),
                                  "Umfragen bis einen Tag vor der Wahl") 

# Get Poll Results for cases (first cutoff only)
df_polls_party <- ger_polls_results %>% # dataset of election polls 
  filter(days_to_election %in%  200:c[1]) %>% # select the cases between the 148 and 200 days
  mutate(t = 365 - days_to_election + 1) %>% # chronological day order until elections (for plotting)  
  filter(year == y, party == p) %>% # subset 2013 SPD results
  mutate(cutoff = c[1])  # create a cutoff variable

# Get Poll Results for cases (the rest of cutoffs)
for(i in 2:length(c)){
  df_tmp <- ger_polls_results %>% # dataset of election polls 
    mutate(t= 365 - days_to_election + 1) %>% # select the cases between the 148 and 200 days
    filter(days_to_election %in%  200:c[i]) %>% # chronological order of days until elections (for plotting) 
    filter(year == y, party == p) %>% # subset 2013 SPD results
    mutate(cutoff = c[i]) # create a cutoff variable
  df_polls_party <- rbind(df_polls_party, df_tmp)
}

# Name Institutes
df_polls_party$institute <- factor(df_polls_party$institute)
levels(df_polls_party$institute) <- c("IfD Allensbach", "Emnid", "F'Gruppe Wahlen",
                                      "forsa", "GMS", "Infratest dimap", "INSA")

# Name Cutoffs
df_polls_party$cutoff <- factor(df_polls_party$cutoff)
df_polls_party <- transform(df_polls_party, cutoff = factor(cutoff, levels = c("148", "64", "8", "1")))
levels(df_polls_party$cutoff) <- c(paste("Umfragen bis", c("148","64","8"),"Tage vor der Wahl"),
                                   "Umfragen bis einen Tag vor der Wahl") 

minmax <- round(c(min(df_plot_party$low1)*0.95, max(df_plot_party$high1)*1.05), 2)

abbildung4 <- ggplot() + 
  geom_hline(data = filter(df_res_for, year == y, party == p), 
             aes(yintercept = share/100), alpha = 0.3, size = 1.1) +
  geom_hline(data = filter(df_res_for, year == y, party == p),
             aes(yintercept = mean/100), alpha = 0.3, linetype = 2, size = 1.1) +
  geom_line(data = df_plot_party, aes(y = mid, x = t)) + 
  geom_ribbon(data = df_plot_party, aes(ymin = low2, ymax = high2, x = t), alpha = 0.1) +
  geom_ribbon(data = df_plot_party, aes(ymin = low3, ymax = high3, x = t), alpha = 0.2) +
  geom_point(data = df_polls_party, aes(x = t, y = support/100, pch = institute), alpha = 0.3) + 
  facet_wrap( ~ cutoff) +  
  xlab("Tage bis zur Wahl") + 
  ylab(paste("Stimmenanteil (%) der", toupper(p))) +
  scale_x_continuous(breaks = 365 - c, label = paste(c)) + 
  scale_y_continuous(breaks = seq(minmax[1], minmax[2], by = 0.02),
                     label = paste(seq(minmax[1], minmax[2], by = 0.02)*100)) +
  theme_bw() + 
  theme(legend.title = element_blank()) +
  scale_shape_manual(values = seq(0, 15))

if ( MAKE_PAPER == F) {
  abbildung4
}

if ( MAKE_PAPER ) {
ggsave(abbildung4, width = 12, height = 6, filename = "../graphs/Abbildung_4.pdf")  
}
### Abbildung 5: Entwicklung der Modellvorhersage für die Bundestagswahl 2013, 148 Tage bis -------
### einen Tag vor der Wahl  

## Load required datasets
load("../data/model_output/structural_forecasts.RData")
load("../data/model_input/polls_comb_results.RData")

## Load results of dynamic forecasting model (for 2002 - 2013 elections)
res_folder <- "../data/model_output/dynamic_model/"
res_files <- dir(res_folder)[!grepl("2017", dir(res_folder))]
df_res <- readRDS(paste0(res_folder, res_files[1]))
for(i in 2:length(res_files)){
  df_res <- rbind(df_res, readRDS(paste0(res_folder, res_files[i])))
}

## Prepare a dataframe of dynamic forecasts
df_res <- as_data_frame(df_res)
df_res[,c(1:7, 11:12)] <- apply(df_res[, c(1:7, 11:12)], 2, function(x) as.numeric(as.character(x)))

## Prepare a dataframe of structural forecasts
struc_forcast <- jags_oospreds_summary_df %>% # structiral model results, out of sample forecast
  select(election, party, mean, sd) %>%
  dplyr::rename(year = election)

## Combine the structural forecast dataframe and election results data
df_res_for <- inner_join(struc_forcast, ger_results_lag[,1:3], by = c("year", "party"))
df_res_for$share <- ifelse(df_res_for$share == "NaN", NA, df_res_for$share)


## Forecast Distributions
df_forcast <- df_res %>%  
  filter(par == "forcast")  %>% 
  dplyr::rename(party = i1)  

# Change Party Labels
df_forcast$party <- as.factor(recode(df_forcast$party, `1` = "cdu",`2` = "fdp", `3` ="gru", 
                                     `4`="lin", `5`="spd",`6`="oth"))

df_forcast <- transform(df_forcast, 
                        party = factor(party, 
                                       levels = c("cdu", "spd", "lin", "gru", "fdp", "afd", "oth")))
df_res_for <- transform(df_res_for, 
                        party = factor(party, 
                                       levels = c("cdu", "spd", "lin", "gru", "fdp", "afd", "oth")))

abbildung5 <- ggplot() + 
  geom_point(data = filter(df_forcast, election == 2013, party != "oth", party != "afd"), 
             aes(y = mid, ymin = low2, ymax = high2, x = -cutoff)) +
  geom_linerange(data = filter(df_forcast, election == 2013, party != "oth", party != "afd"), 
                 aes(y = mid, ymin = low2, ymax = high2, x = -cutoff)) + 
  geom_linerange(data = filter(df_forcast, election == 2013, party != "oth", party != "afd"), 
                 aes(y = mid, ymin = low3, ymax = high3, x = -cutoff), size = 1.2) + 
  geom_hline(data = filter(df_res_for, year == 2013, party != "oth", party != "afd"), 
             aes(yintercept = share/100), alpha = 0.3, size = 1.1) +
  geom_hline(data = filter(df_res_for, year == 2013, party != "oth", party != "afd"), 
             aes(yintercept = mean/100), alpha = 0.3, linetype = 2, size = 1.1) +
  facet_grid( ~ party, labeller = party_labeller) + 
  xlab("Tage bis zur Wahl") + 
  ylab("Stimmenanteil (%)") + 
  scale_x_continuous(breaks = -1*c(seq(from = 148, to = 1, by =-(28)), 1), 
                     label = c(seq(from = 148, to = 1, by =-(28)), 1)) +
  scale_y_continuous(breaks = seq(0, 0.5, by = 0.1), label = paste(seq(0, 0.5, by = 0.1)*100)) + 
  theme_bw() 

if ( MAKE_PAPER == F) {
  abbildung5
}

if ( MAKE_PAPER ) {
ggsave(abbildung5, width = 12, height = 6, filename = "../graphs/Abbildung_5.pdf")  
}

### Tabelle 1: Entwicklung des RMSE der out-of-sample-Vorhersagen nach Datengrundlage und Wahl. ----
### Strukturelles Modell vs. Umfragemodell, gestaffelt nach Zeitpunkt vor der Wahl 


## Load required datasets
load("../data/model_output/structural_forecasts.RData")
load("../data/model_input/polls_comb_results.RData")

## Load results of dynamic forecasting model (for 2002 - 2013 elections)
res_folder <- "../data/model_output/dynamic_model/"
res_files <- dir(res_folder)[!grepl("2017", dir(res_folder))]
df_res <- readRDS(paste0(res_folder, res_files[1]))
for(i in 2:length(res_files)){
  df_res <- rbind(df_res, readRDS(paste0(res_folder, res_files[i])))
}

## Prepare a dataframe of dynamic forecasts
df_res <- as_data_frame(df_res)
df_res[,c(1:7, 11:12)] <- apply(df_res[, c(1:7, 11:12)], 2, function(x) as.numeric(as.character(x)))

## Prepare a dataframe of structural forecasts
struc_forcast <- jags_oospreds_summary_df %>% # structiral model results, out of sample forecast
  select(election, party, mean, sd) %>%
  dplyr::rename(year = election)

## Combine the structural forecast dataframe and election results data
df_res_for <- inner_join(struc_forcast, ger_results_lag[,1:3], by = c("year", "party"))
df_res_for$share <- ifelse(df_res_for$share == "NaN", NA, df_res_for$share)


## Forecast Distributions
df_forcast <- df_res %>%  
  filter(par == "forcast")  %>% 
  dplyr::rename(party = i1)  

# Change Party Labels
df_forcast$party <- as.factor(recode(df_forcast$party, `1` = "cdu",`2` = "fdp", `3` ="gru", 
                                     `4`="lin", `5`="spd",`6`="oth"))

df_forcast <- transform(df_forcast, 
                        party = factor(party, 
                                       levels = c("cdu", "spd", "lin", "gru", "fdp", "afd", "oth")))
df_res_for <- transform(df_res_for, 
                        party = factor(party, 
                                       levels = c("cdu", "spd", "lin", "gru", "fdp", "afd", "oth")))


# Forecast-statistics RMSE
# Calculate Coverage and squared error for all observations   
df_forcasts_res <- df_forcast  %>% 
  select(mid, election, party, cutoff, low1, high1, low2, high2) %>% 
  dplyr::rename(year = election) %>% 
  inner_join(., ger_results_lag[,1:3], by = c("year", "party")) %>%
  mutate(se = (mid - share/100)^2) %>%
  mutate(covered = ifelse(low2 <= share/100 & share/100 <= high2, 1, 0)) 

# Tabel of RMSE 
brw_rmse_year <- df_forcasts_res %>% 
  group_by(cutoff, year) %>% 
  summarise(rmse = sqrt(mean(se))*100) %>% # calculate the RMSE for cases with certain cutoff and year
  spread(year, rmse) # dataset in wide format 

# name the cutoff
brw_rmse_year$cutoff <- paste("BRW", (brw_rmse_year$cutoff), "Tag(e) vor der Wahl")

# 
struc_rmse <- df_res_for %>% 
  filter(year %in% c(2002,2005,2009,2013)) %>% 
  mutate(se = (mean - share)^2) %>%
  mutate(covered = ifelse(mean - 1.96*sd <= share &  share <= mean + 1.96*sd, 1, 0) ) 

struc_rmse_year <- struc_rmse %>% 
  group_by(year) %>%
  summarise(rmse = sqrt(mean(se))) %>% 
  spread(year, rmse)

table_mat_years <- rbind(c("Strukturelles Modell",round(as.numeric(struc_rmse_year),2)),
                         cbind(as.matrix(brw_rmse_year[,1]),round(as.matrix(brw_rmse_year[,-1]),2))
)

# RMSE for dynamic models
brw_rmse <- df_forcasts_res %>% 
  group_by(cutoff) %>%
  summarise(rmse = sqrt(mean(se))*100) %>%
  select(rmse)

# RMSE for structural model
struc_rmse <- struc_rmse %>%
  summarise(rmse = sqrt(mean(se)))

# combine RMSEs into one table 
table_mat <- cbind(table_mat_years[,1],
                   round(c(as.numeric(struc_rmse),as.matrix(brw_rmse[,1])),2),
                   table_mat_years[,-1])

colnames(table_mat) <- c("Modell", "RMSE Durchschnitt", paste("RMSE", c(2002, 2005, 2009, 2013)))
xt <- xtable(table_mat)

if ( MAKE_PAPER == F) {
  xt
}

if ( MAKE_PAPER ) {
print(xt, type = "html", file = "../graphs/table_1.html")
}

### Abbildung 6: Dynamische Vorhersagen der Stimmenanteile für die Bundestagswahl 2017 ------------

## Load required datasets
load("../data/model_output/structural_forecasts.RData")
load("../data/model_input/polls_comb_results.RData")

## Load day 116 data 
res_folder <- "../data/model_output/dynamic_model/"
df_res <- readRDS(paste0(res_folder, "draws_forcast_levels_2017_116.rds"))

# Set the year and cutoff
Election <- 2017
cutoff <- 116

# Extract Levels draws from the list 
res_brw <- list()
res_temp <- df_res[["levels"]]
res_temp_party <- df_res[["party_names"]]

for (i in 1:366){
  
  r <- as.matrix(res_temp[,,i])
  df_res3 <- as.data.frame(t(apply(r, 2, quantile, c(0.01, 0.025, 0.05, 0.5, 0.95, 0.975, 0.99))))
  names(df_res3) <- c(paste0("low", 1:3), "mid", paste0("high", 3:1))
  df_res3$election <- Election
  df_res3$cutoff <- cutoff
  split_rownames <- as.data.frame(str_split_fixed(rownames(df_res3), "\\[|,|\\]", 4)[,1:3])
  colnames(split_rownames) <- c("par", "i1", "i2")
  split_rownames[,2:3] <- apply(split_rownames[,2:3], 2, as.numeric)
  split_rownames[,3] <- res_temp_party
  split_rownames[,2] <- i
  split_rownames[,1] <- "alpha"
  df <- cbind(df_res3, split_rownames)
  res_brw[[i]] <- df
}

# Unlist the dataframes for graph 
df_res <- do.call("rbind", res_brw)	
df_res <- as_data_frame(df_res)

# required data from structural forecasts
df_res_for <- jags_oospreds_summary_df %>% 
  select(election, party, mean, sd) %>%
  rename(year = election) %>% 
  filter(year == 2017)

# Create Plots for Levels
df_levels <- df_res %>%  
  filter(par == "alpha")  %>% 
  rename(party = i2, t = i1) %>%
  group_by(election) %>%
  do(mutate(.,days_to_election = max(t) - t)) 

unique_cutoffs <-  sort(unique(df_levels$cutoff))
unique_elections <- unique(df_levels$election)

for(sel_election in unique_elections){
  for(c in unique_cutoffs){
    
    # Filtered Levels
    df_plot <- filter(df_levels, 
                      party != "oth",
                      election == sel_election, 
                      days_to_election < 365,
                      cutoff == c)
    
    polls_long <- ger_polls_results %>%
      mutate(t = 365 - days_to_election + 1) %>% 
      filter(days_to_election %in% 365:c) %>%
      filter(year == sel_election) %>%
      filter(!is.na(support)) %>%
      select(party, support, days_to_election, sample_size, institute, t) 
    
    polls_long$institute <- factor(polls_long$institute)
    levels(polls_long$institute) <- c("allensbach", "emnid", "fgruppe_wahlen", 
                                      "forsa", "gms", "infratest_dimap", "insa")
    
    # Change order
    df_res_for <- transform(df_res_for, 
                            party = factor(party,
                                           levels = c("cdu","spd","lin","gru", "fdp","afd","oth")))
    df_plot <- transform(df_plot, 
                         party = factor(party, 
                                        levels = c("cdu","spd","lin","gru","fdp", "afd","oth")))
    polls_long <- transform(polls_long, 
                            party = factor(party, 
                                           levels = c("cdu","spd","lin","gru","fdp","afd","oth")))
    
    abbildung6 <- 
      ggplot() + 
        geom_hline(data = filter(
          df_res_for, year == sel_election, party %in% unique(df_plot$party)), 
          aes(yintercept = mean), alpha = 0.3, linetype = 2, size = 1.1) +
        geom_line(data = df_plot, aes(y = mid*100, x = t)) + 
        geom_ribbon(data = df_plot, 
                    aes(y = mid*100, ymin = low2*100, ymax = high2*100, x = t), alpha = 0.2) +
        geom_ribbon(data = df_plot, 
                    aes(y = mid*100, ymin = low3*100, ymax = high3*100, x = t), alpha = 0.3) +
        geom_point(data = polls_long, aes(x = t, y = support, pch = institute), alpha = 0.2) +
        facet_wrap( ~ party, scales = "free", labeller = party_labeller) + 
        xlab("Tage bis zur Wahl") + 
        ylab("Stimmenanteil (%)") +
        scale_x_continuous(breaks = max(df_plot$t) - c(365, 200, c, 1),
                           label = c(365, 200, c, 1)) +
        theme_bw() +
        theme(legend.title = element_blank()) +
        scale_shape_manual(values = seq(0, 15))
  
    if ( MAKE_PAPER ) {
       ggsave(abbildung6, width = 15, height = 7, filename = "../graphs/Abbildung_6.pdf")  
    }
  
  }
}

if ( MAKE_PAPER == F) {
  abbildung6
}

### Abbildung 7: Vorhersagen der Stimmenanteile für die Bundestagswahl 2017 -----------------------

## Load required datasets
load("../data/model_output/structural_forecasts.RData")
load("../data/model_input/polls_comb_results.RData")

## Load day 116 data 
res_folder <- "../data/model_output/dynamic_model/"
df_res <- readRDS(paste0(res_folder, "draws_forcast_levels_2017_116.rds"))

## select the parameters of interest
Election <- 2017
cutoff <- 116

## Subset the draws for forecasts in day 116
res_brw <- list()
res_temp <- df_res[["forcast"]]
res_temp_party <- df_res[["party_names"]]

# Prepare the dataframe 
r <- as.matrix(res_temp)
df_res3 <- as.data.frame(t(apply(r, 2, quantile, c(0.01, 0.025, 0.05, 0.5, 0.95, 0.975, 0.99))))
names(df_res3) <- c(paste0("low", 1:3), "mid", paste0("high", 3:1))
df_res3$election <- Election
df_res3$cutoff <- cutoff
split_rownames <- as.data.frame(str_split_fixed(rownames(df_res3), "\\[|,|\\]", 4)[,1:3])
colnames(split_rownames) <- c("par", "i1", "i2")
split_rownames[,2:3] <- apply(split_rownames[,2:3], 2, as.numeric)
split_rownames[,3] <- res_temp_party
split_rownames[,1] <- "forcast"
df_res <- cbind(df_res3, split_rownames)

# Forcast Level on Election Day plus Shock
df_forcast <- df_res %>%  
  filter(par == "forcast")  %>% 
  rename(party = i2) 

# prepare for plotting
df_forcast[,1:7] <- df_forcast[,1:7] * 100
df_forcast <- arrange(df_forcast, mid)
df_forcast$partyrank <- seq_along(df_forcast$party)
df_forcast$partyname <- c("Andere", "B'90/Die Grünen", "FDP", "Die Linke", "AfD", "SPD", "CDU/CSU")

if ( MAKE_PAPER ) {
pdf(file = "../graphs/Abbildung_7.pdf", height=3, width=6, family = "URWTimes")
}

par(mar=c(3.5,7,0,3)+.1)
par(oma=c(0,0,0,0)+.1)
plot(df_forcast$mid, df_forcast$partyrank, ylim = c(0.5, 7.5), xlim = c(0, 45), xaxt = "n",
     yaxt = "n", ylab = "", xlab = "", pch = 20)
axis(1, seq(0, 45, 5), seq(0, 45, 5))
axis(1, mean(c(0, 45)), "Vorhergesagter Stimmenanteil (%)", line = 1, tick = F)
axis(2, df_forcast$partyrank, labels = df_forcast$partyname, las = 1, tick = F)
axis(4, df_forcast$partyrank, labels = paste0(format(df_forcast$mid, digits = 2, trim = TRUE),  "%"),
     line = 1.5, tick = F, las = 2, hadj = 1)

abline(v = seq(0, 45, 5), col = "darkgrey", lty = 2)
for (i in df_forcast$partyrank){
  lines(x=c(df_forcast$low2[i],df_forcast$high2[i]), y=c(i,i), lwd = 1)
  lines(x=c(df_forcast$low3[i],df_forcast$high3[i]), y=c(i,i), lwd = 3)
}

if ( MAKE_PAPER ) {
 dev.off()
}

### Tabelle 2: Vorhergesagte Wahrscheinlichkeiten auf Basis der simulierten Stimmenanteile --------
### für Ereignisse im Zuge der Bundestagswahl 2017 

## Load day 116 data 
res_folder <- "../data/model_output/dynamic_model/"
df_res <- readRDS(paste0(res_folder, "draws_forcast_levels_2017_116.rds"))

## Subset the draws - forecasts and levels (day 366)
res_temp_f <- df_res[["forcast"]]
res_temp_l <- df_res[["levels"]][,,366]
res_temp_party <- df_res[["party_names"]]
colnames(res_temp_l) <- paste0("alpha[366,", 1:7, "]")

rb <- as.matrix(cbind(res_temp_f, res_temp_l))

sel_level <- grep("alpha\\[366,*", colnames(rb), val = T)
sel_forcast <- grep("forcast", colnames(rb), val = T)

# Wahrscheinlichkeiten  

# Probability Small Parties enter Bundestag
events_lower_five <- paste(c("AFD","FDP","Gruene","Die Linke"), "unter 5%")

lower_five_forcast <- apply(rb[,sel_forcast], 2, function(x) round(1 - mean(x > 0.05), 2))
lower_five_level <- apply(rb[,sel_level], 2, function(x) round(1 - mean(x > 0.05), 2))
sel_small <- c(6, 5, 4, 3)

df_lower_five <- cbind(events_lower_five,
                       paste(lower_five_forcast[sel_small]*100, "%"),
                       paste(lower_five_level[sel_small]*100, "%"))

# Coalition Probabilities 
# 1 "cdu" 2 "spd" 3 "lin" 4 "gru" 5 "fdp" 6 "afd" 7 "oth"
coal_gc <- c(c("Koalition CDU + SPD Mehrheit"), p_coal_maj(c(1, 2)), p_coal_maj(c(1, 2), sel_level))
coal_sg <- c(c("Koalition CDU + FDP Mehrheit"), p_coal_maj(c(1, 5)), p_coal_maj(c(1, 5), sel_level))
coal_rg <- c(c("Koalition SPD + Grüne Mehrheit"), p_coal_maj(c(2, 4)), p_coal_maj(c(2, 4),sel_level))
coal_sw <- c(c("Koalition CDU + Grüne Mehrheit"), p_coal_maj(c(1, 4)),p_coal_maj(c(1, 4), sel_level))
coal_rrg <- c(c("Koalition SPD + Die Linke + Grüne Mehrheit"), p_coal_maj(c(2, 3, 4)), 
              p_coal_maj(c(2, 3, 5), sel_level))
coal_ampel <- c(c("Koalition SPD + FDP + Grüne Mehrheit"), p_coal_maj(c(2, 5, 4)), 
                p_coal_maj(c(2, 5, 3), sel_level))
coal_jam <- c(c("Koalition CDU + FDP + Grüne Mehrheit"), p_coal_maj(c(1, 5, 4)), 
              p_coal_maj(c(1, 5, 4), sel_level))

df_coalition  <- rbind(coal_gc, coal_sg, coal_sw, coal_jam, coal_rg, coal_rrg, coal_ampel)  


# Better result than in previous elections
# 1 "cdu" 2 "spd" 3 "lin" 4 "gru" 5 "fdp" 6 "afd" 7 "oth"
greater_as <- function(party,val) c(mean(rb[,sel_forcast[party]] > val/100),
                                    mean(rb[,sel_level[party]] > val/100))

cdu_better <- c(c("CDU/CSU (>41.5%)"), round(greater_as(1,41.5), 2)*100)
spd_better <- c(c("SPD (>25.7%)"), round(greater_as(2,25.7), 2)*100)
left_better <- c(c("Die Linke (>8.6%)"), round(greater_as(3, 8.6), 2)*100)
green_better <- c(c("Gruene (>8.4%)"), round(greater_as(4, 8.4), 2)*100)
fdp_better <- c(c("FDP (>4.8%)"), round(greater_as(5, 4.8), 2)*100)
afd_better <- c(c("AFD (>4.7%)"), round(greater_as(6, 4.7), 2)*100)

df_better <- rbind(cdu_better, spd_better, left_better, green_better, fdp_better, afd_better)

# Others
# 1 "cdu" 2 "spd" 3 "lin" 4 "gru" 5 "fdp" 6 "afd" 7 "oth"
six_parties_for <- mean(apply(rb[,sel_forcast[-7]], 1, function(x) sum(x > 0.05) == 6))
six_parties_lev <- mean(apply(rb[,sel_level[-7]], 1, function(x) sum(x > 0.05) == 6))
six_parties  <- c(c("Six Parties"), round(c(six_parties_for,six_parties_lev), 2)*100)

afd_third_for <- mean(apply(rb[,sel_forcast[-7]], 1, function(x) rank(x)[6] == 4))
afd_third_lev <- mean(apply(rb[,sel_level[-7]], 1, function(x) rank(x)[6] == 4))
afd_third <- c(c("AFD Third"), round(c(afd_third_for, afd_third_lev), 2)*100)

cdu_largest_party_for <- mean(apply(rb[,sel_forcast[-7]], 1, function(x) rank(x)[1] == 6))
cdu_largest_party_level <- mean(apply(rb[,sel_level[-7]], 1, function(x) rank(x)[1] == 6))
cdu_largest <- c(c("CDU largest"), round(c(cdu_largest_party_for, cdu_largest_party_level), 2)*100)

spd_largest_party_for <- mean(apply(rb[,sel_forcast[-7]], 1, function(x) rank(x)[2] == 6))
spd_largest_party_level <- mean(apply(rb[,sel_level[-7]], 1, function(x) rank(x)[2] == 6))
spd_largest <- c(c("SPD largest"), round(c(spd_largest_party_for, spd_largest_party_level), 2)*100)

df_p <- rbind(cdu_largest, spd_largest, df_coalition, df_lower_five, 
              df_better, six_parties, afd_third)
rownames(df_p) <- NULL
colnames(df_p) <- c("Event", "Wahrscheinlichkeit Vorhersage", "Wahrscheinlichkeit Level")

xt <- xtable(df_p)

if ( MAKE_PAPER == F) {
  xt
}

if ( MAKE_PAPER ) {
print(xt, type = "html", file = "../graphs/table_2.html")
}
