#================================================================================#
# Program : 08_analysis.R                                                        #
# Aim     : This script contains the code to produce our the analysis            #
# Date    : 03-01-2020                                                           #
# Authors : Frederik Skjerning & Mathias Rask                                    #
#================================================================================#

#### SETUP ####

# clear enviroment
rm(list=ls())

# set working directory
getwd()
setwd("/Users/mathiasrask/Desktop/Racism-in-Premier-League/")

# load libraries
library(lfe)
library(dplyr)
library(broom)
library(ggplot2)
library(cowplot)
library(boot)

# extract functions from 08_functions.R
source('08_functions.R')

# load data
load('data/08_finaldf.Rdata')

# We employ the followings variables:
# - Dependent variables:
#     1) share_racist_tweets
#     2) share_racist_fans
#
# - Independent variables:
#     1) share_skin1      (Fitzpatrick 5+6)
#     2) share_min_skin1  (Fitzpatrick 5+6)
#     3) share_skin2      (Fitzpatrick 6)
#     4) share_min_skin2  (Fitzpatrick 6)
#
# - Moderator:
#     1) performance1 (The Winner Takes it All)
#     2) performance2 (Objective performance)
#     3) performance3 (Performance towards the mean)

# this will be our workhorse model - it includes a covariate measuring whether the
# team played in the top division, Premier League, club and season fixed effects
# and standard errors clustered on club and season.

#===================================================================================#

#### ROPE-LADDER PLOT####

# make character vector of moderator and independent variables to loop over 
interaction <- names(df)[13:15] 
independent <- names(df)[17:20] 
dependent   <- names(df)[21:22]
# make empty list to store estimated models
models <- list()

# make two helpers
helper1 <- list()
helper2 <- list()

# loop throught the combinations
for (d in seq_along(dependent)) {
  for (c in seq_along(interaction)) {
    for (k in seq_along(independent)) {
      
      # apply function defined to estimate a generalized difference-in-differences
      helper1[[k]]<- models_fun(dep = df[, dependent[d]], ind = df[, independent[k]], int = df[, interaction[c]], data = df)
      
      # add description of the model and change name of variables
      helper1[[k]]$model <- paste(independent[k], interaction[c], dependent[d])
      helper1[[k]][1,1] <- paste0("share ", "skin",k)
      helper1[[k]][2,1] <- interaction[c]
      helper1[[k]][3,1] <- 'interaction'
    }
    
    # row bind the results forinteraction variables
    helper2[[c]] <- do.call(rbind, helper1)
  }
  
  # row bind the results for dependent variables
  models[[d]] <- do.call(rbind, helper2)
}

# row bind all models and make a data set for both tweets and fans
models_racist <- do.call(rbind, models)

models_racist_tweets <- models_racist[seq(3, 36,  by = 3),]
models_racist_fans   <- models_racist[seq(39, 72,  by = 3),]

# add model specifications
models_racist_tweets$performance <- as.factor(rep(c('The Winner Takes It All', 'Objective Performance', 'Performance to the Mean'), each = 4))
models_racist_fans$performance   <- as.factor(rep(c('The Winner Takes It All','Objective Performance', 'Performance to the Mean'), each = 4))

models_racist_tweets$skin <- rep(c('% team skin1','% min. skin1', '% team skin2', '% min. skin2'), each = 1)
models_racist_fans$skin <- rep(c('% team skin1','% min. skin1', '% team skin2', '% min. skin2'), each = 1)

models_racist_tweets$number <- c(1,2,3,4,1,2,3,4,1,2,3,4)
models_racist_fans$number   <- c(1,2,3,4,1,2,3,4,1,2,3,4) 


p1_1 <- coef_fun(data = models_racist_tweets, x = models_racist_tweets$skin, y = models_racist_tweets$estimate,
                 sd = models_racist_tweets$std.error, col = models_racist_tweets$performance, 
                 num = models_racist_tweets$number) + ggtitle('a) Share of racist tweets') 

p1_2 <- coef_fun(data =models_racist_fans, x = models_racist_fans$skin, y = models_racist_fans$estimate,
                 sd = models_racist_fans$std.error, col = models_racist_fans$performance, 
                 num = models_racist_fans$number) + ggtitle('b) Share of racist fans') 

# extract legend 
legend <- get_legend(
  p1_1 + 
    guides(color = guide_legend(nrow = 2)) +
    theme(legend.position = c(1.1,0.75))
)

plot1 <- plot_grid(p1_1, p1_2, legend, ncol = 2,  rel_heights = c(1, .2))
ggsave2(filename = 'figures/plot1_svm.eps', plot1, height = 6.5, width = 7.75)

#===================================================================================#

#### MARGINSPLOT ####

# Margins 1)
margins1 <- margins_fun(dep = df$share_racist_tweets_svm, ind = df$share_skin2_min, int = df$performance2) +
  labs(x = 'Objective Performance', y = 'Marginal Effects', title = 'a) Share of racist tweets') +
  theme(axis.text = element_text(size = 7), axis.title = element_text(size = 7), plot.title = element_text(size = 7.5)) +
  scale_y_continuous(limits = c(-0.075, 0.075))
xden <- density_fun(plot = margins1, var = df$performance2)
margins1 <- insert_xaxis_grob(margins1, xden, position = "bottom")

# Margins 2)
margins2 <- margins_fun(dep = df$share_racist_fans_svm, ind = df$share_skin2_min, int = df$performance2) + 
  labs(x = 'Objective Performance', y = 'Marginal Effects', title = 'b) Share of racist fans') +
  theme(axis.text = element_text(size = 7), axis.title = element_text(size = 7), plot.title = element_text(size = 7.5)) +
  scale_y_continuous(limits = c(-0.075, 0.075))
xden <- density_fun(plot = margins2, var = df$performance2)
margins2 <- insert_xaxis_grob(margins2, xden, position = "bottom")

margins_racist <- plot_grid(margins1, margins2, ncol = 2, nrow = 1)
ggsave(filename = 'figures/plot2_svm.png', margins_racist, height = 4, width = 6)
#---------------------------------------------------------------------------------------------------------#

#### CLUB-LEVEL ANALYSIS ####

# In this section we investigate any potential club differences.
# We use these estimates to:
# 1) bootstrap the average and the corresponding CI to test the robustness
# 2) to see whether there is differences between the clubs

# make character vector of moderator and independent variables to loop over 
interaction <- names(df)[13:15] 
independent <- names(df)[17:20] 
dependent   <- names(df)[21:22]

# make list of club names to loop pver
clubs <- c(unique(df$club))

# make empty list to store estimated models, two helpers and the final list
models   <- list()
helper1  <- list()
helper2  <- list()
clublist <- list()

for (w in seq_along(clubs)) {
  
  # make club dataframe
  club_df <- df %>%
    filter(club == clubs[w])
  
  for (d in seq_along(dependent)) {
    for (c in seq_along(interaction)) {
      for (k in seq_along(independent)) {
        # apply function defined to estimate a generalized difference-in-differences
        helper1[[k]]<- clubmodels_fun(dep = club_df[, dependent[d]], ind = club_df[, independent[k]], 
                                      int = club_df[,interaction[c]])
        
        # add description of the model and change name of variables
        helper1[[k]]$model <- paste(independent[k], interaction[c], dependent[d])
        helper1[[k]]$club <- clubs[w]
        helper1[[k]]$performance <- interaction[c]
      }
      
      # row bind the results forinteraction variables
      helper2[[c]] <- do.call(rbind, helper1)
    }
    
    # row bind the results for dependent variables
    models[[d]] <- do.call(rbind, helper2)
  }
  
  # row bind the results for the club
  help <- do.call(rbind, models)
  clublist[[w]] <- help
}

# make one big dataframe of estimates for all clubs
club_df <- do.call(rbind, clublist)  

# split data into two - racist and negative
club_racist <- club_df[club_df$model %in% c('share_skin2_min performance1 share_racist_tweets_svm',
                                            'share_skin2_min performance2 share_racist_tweets_svm',
                                            'share_skin2_min performance3 share_racist_tweets_svm',
                                            'share_skin2_min performance1 share_racist_fans_svm',
                                            'share_skin2_min performance2 share_racist_fans_svm',
                                            'share_skin2_min performance3 share_racist_fans_svm'),]

# add model specifications to the DFs
club_racist$spec   <- rep(c(1:2), each = 3)
club_racist$clubnr <- rep(c(1:15), each = 6)
club_racist$grid   <- c(rep(1, times = 48), rep(2, times = nrow(club_racist)-48))


plot3 <- club_coef_fun(data = club_racist, y = club_racist$estimate, x = club_racist$club, 
                       reorder = club_racist$estimate, col = club_racist$performance,
                       labels = c('1'="Share of racist tweets", '2'="Share of racist fans")) +
  theme(legend.position = c(0.5,-0.16)) + guides(color = guide_legend(nrow = 1))

ggsave2(filename = 'figures/plot3_svm.eps', plot3, height = 4.5, width = 5.5)

#### ROBUSTNESS SETUP: WEIGHTS  ####

# make vector of variable names
interaction <- names(df)[13:15] 
independent <- names(df)[17:20] 
dependent   <- names(df)[21:22]

# make list of club names to loop pver
clubs <- c(unique(df$club))

# make some changes for baseline
baseline <- models_racist[seq(3, 72, by = 3),]

# calculate 90% CI's for baseline
baseline$upper <- baseline$estimate + 1.64*baseline$std.error
baseline$lower <- baseline$estimate - 1.64*baseline$std.error

# drop variables and add specifications
baseline[, c(1,3)] <- NULL
baseline$spec <- 'baseline'


#### ROBUSTNESS 1: CLUB-SPECIFIC TIME-TRENDS ####

# make empty list to store estimated models
models <- list()

# make two helpers
helper1 <- list()
helper2 <- list()

# loop throught the combinations
for (d in seq_along(dependent)) {
  for (c in seq_along(interaction)) {
    for (k in seq_along(independent)) {
      
      # apply function defined to estimate a generalized difference-in-differences
      helper1[[k]] <- tidy(felm(df[,dependent[d]] ~ df[,independent[k]] +df[,interaction[c]] + df[,independent[k]]:df[,interaction[c]] + factor(premier_league) | 
                                  factor(club) + factor(season) + factor(club):as.numeric(season) | 0 | club + season, data = df))[-1,-4]
      # add description of the model and change name of variables
      helper1[[k]]$model <- paste(independent[k], interaction[c], dependent[d])
      helper1[[k]][1,1] <- paste0("share ", "skin",k)
      helper1[[k]][2,1] <- interaction[c]
      helper1[[k]][3,1] <- 'interaction'
    }
    
    # row bind the results forinteraction variables
    helper2[[c]] <- do.call(rbind, helper1)
  }
  
  # row bind the results for dependent variables
  models[[d]] <- do.call(rbind, helper2)
}

# row bind all models and make a data set for both tweets and fans
weight1 <- do.call(rbind, models)

# make some changes for baseline
clubtrend <- weight1[seq(3, 72, by = 3),]

# calculate 90% CI's for baseline
clubtrend$upper <- clubtrend$estimate + 1.64*clubtrend$std.error
clubtrend$lower <- clubtrend$estimate - 1.64*clubtrend$std.error

# drop variables and add specifications
clubtrend[, c(1,3)] <- NULL
clubtrend$spec <- 'club-specific trend'

#### ROBUSTNESS 2: EACH CLUB AT A TIME ####

# make vector to loop over
models <- c(unique(club_df$model))

# make empty data frame
eachclub <- data.frame('model' = NA, 'estimate' = NA, 'upper' = NA, 'lower' = NA)

for (i in seq_along(models)) {
  temp <- club_df[club_df$model==models[i],]
  boot <- boot(temp$estimate, mean.fun, R = 500)
  boot_ci <- boot.ci(boot, conf = 0.90, type = 'bca')
  
  eachclub[i, 'model'] <- models[i]
  eachclub[i, 'estimate'] <- mean(boot$t)
  eachclub[i, 'upper'] <- boot_ci$bca[,5]
  eachclub[i, 'lower'] <- boot_ci$bca[,4]
}

eachclub$spec <- 'each club'
weights2 <- club_racist

#### ROBUSTNESS 3: JACKKNIFE - LEAVE-ONE-OUT #####

# make empty list to store estimated models, two helpers and the final list
models   <- list()
helper1  <- list()
helper2  <- list()
helper3  <- list() 

for (c in seq_along(clubs)) {
  # subset all clubs except club c 
  club_df <- df[!df$club == clubs[c],]
  for (d in seq_along(dependent)) {
    for (i in seq_along(interaction)) {
      for (k in seq_along(independent)) {
      # apply function defined to estimate a generalized difference-in-differences
      helper1[[k]] <- models_fun(dep = club_df[,dependent[d]], ind = club_df[, independent[k]],
                                 int = club_df[, interaction[i]], data = club_df)[3,]
      helper1[[k]]$model <- paste(independent[k], interaction[i], dependent[d]) 
      helper1[[k]]$club <- clubs[c]
      helper1[[k]]$performance <- interaction[i]
      } 
    
      helper2[[i]] <- do.call(rbind, helper1)
    }
    # row bind the results forinteraction variables
    helper3[[d]] <- do.call(rbind, helper2)
  }
  # row bind the results for dependent variables
  models[[c]] <- do.call(rbind, helper3)
}
weights3 <- do.call(rbind, models)

# make vector of performance names to subset on and empty list
models <- c(unique(weights3$model))

jackknife <- data.frame('model' = NA, 'estimate' = NA, 'upper' = NA, 'lower' = NA)

for (i in seq_along(models)) {
  temp <- weights3[weights3$model==models[i],]
  boot <- boot(temp$estimate, mean.fun, R = 250)
  boot_ci <- boot.ci(boot, conf = 0.90, type = 'bca')
  
  jackknife[i, 'model'] <- models[i]
  jackknife[i, 'estimate'] <- mean(boot$t)
  jackknife[i, 'upper'] <- boot_ci$bca[,5]
  jackknife[i, 'lower'] <- boot_ci$bca[,4]
}

jackknife$spec <- 'jackknife'


#### ROBUSTNESS 4: TWO-YEAR PERIOD AND 15 CLUBS ####

# make empty list to store estimated models, two helpers and the final list
models   <- list()
helper1  <- list()
helper2  <- list()
helper3  <- list()

# construct matrix of alxl possible combinations
twoyear <- combn(seq(2011, 2019, by = 1), m = 2)

# loop over each 
for (y in 1:ncol(twoyear)) {
  
  # extract the two years
  year1 <- twoyear[1,y]
  year2 <- twoyear[2,y]
  
  # and subset a data frame based to calculate 2x2 
  year_df <- df[df$season %in% c(year1, year2),]
  
  for (d in seq_along(dependent)) {
    for (i in seq_along(interaction)) {
      for (k in seq_along(independent)) {
        # apply function defined to estimate a generalized difference-in-differences
        helper1[[k]] <- models_2by2fun(dep = year_df[,dependent[d]], ind = year_df[, independent[k]],
                                       int = year_df[, interaction[i]], data = year_df)[2,]
        helper1[[k]]$model <- paste(independent[k], interaction[i], dependent[d]) 
        helper1[[k]]$year <- paste(year1, year2)
        helper1[[k]]$performance <- interaction[i]
      }
      helper2[[i]] <- do.call(rbind, helper1)
    }
    # row bind the results forinteraction variables
    helper3[[d]] <- do.call(rbind, helper2)
  }
  # row bind the results for dependent variables
  models[[y]] <- do.call(rbind, helper3)
}

# collapse to one big dataframe
weights4 <- do.call(rbind, models)

# make vector of performance names to subset on and empty list
models <- c(unique(weights4$model))

twoyear <- data.frame('model' = NA, 'estimate' = NA, 'upper' = NA, 'lower' = NA)

for (i in seq_along(models)) {
  temp <- weights4[weights4$model==models[i],]
  boot <- boot(temp$estimate, mean.fun, R = 250)
  boot_ci <- boot.ci(boot, conf = 0.90, type = 'bca')
  
  twoyear[i, 'model'] <- models[i]
  twoyear[i, 'estimate'] <- mean(boot$t)
  twoyear[i, 'upper'] <- boot_ci$bca[,5]
  twoyear[i, 'lower'] <- boot_ci$bca[,4]
}

# add specification
twoyear$spec <- 'two-year period'


#### ROBUSTNESS 5: 2x2 DIFF-IN-DIFF ####

# make empty list to store estimated models, two helpers and the final list
models   <- list()
helper1  <- list()
helper2  <- list()
helper3  <- list()
helper4  <- list()

# construct matrix of alxl possible combinations
twoyear_com <- combn(seq(2011, 2019, by = 1), m = 2)
twoclub_com <- combn(clubs, m = 2)

# loop over each two-club combination
for (c in 1:ncol(twoclub_com)) {
  
  print(c)
  # extract data frames
  comb_df <- df %>% 
    filter(club == twoclub_com[1,c] | club == twoclub_com[2,c])

  # loop over each two-year combination
  for (y in 1:ncol(twoyear_com)) {
    # extract the two years
    year1 <- twoyear_com[1,y]
    year2 <- twoyear_com[2,y]
    
    comb_year <- comb_df[comb_df$season %in% c(year1, year2),]
    
    for (d in seq_along(dependent)) {
      for (i in seq_along(interaction)) {
        for (k in seq_along(independent)) {
          helper1[[k]] <- models_2by2fun(dep = comb_year[, dependent[d]], ind = comb_year[, independent[k]],
                                         int = comb_year[, interaction[i]], data = comb_year)[2,]
          helper1[[k]]$model <- paste(independent[k], interaction[i], dependent[d])
          helper1[[k]]$year  <- paste(year1, year2)
          helper1[[k]]$performance <- interaction[i]
          helper1[[k]]$club  <- clubs[c]
          helper1[[k]]$clubcombi <- paste(twoclub_com[1,c], twoclub_com[2,c])
        }
        
        helper2[[i]] <- do.call(rbind, helper1)
      }
      
      helper3[[d]] <- do.call(rbind, helper2)
  }
  
  helper4[[y]] <- do.call(rbind, helper3)
      
  }
  models[[c]] <- do.call(rbind, helper4)
}

# collapse to one big dataframe
weights5 <- do.call(rbind, models)

# make vector of performance names to subset on and empty list
models <- c(unique(weights5$model))

twobytwo <- data.frame('model' = NA, 'estimate' = NA, 'upper' = NA, 'lower' = NA)

for (i in seq_along(models)) {
  print(i)
  temp <- weights5[weights5$model==models[i],]
  boot <- boot(temp$estimate, mean.fun, R = 4500)
  print(i)
  boot_ci <- boot.ci(boot, conf = 0.90, type = 'bca')
  
  twobytwo[i, 'model'] <- models[i]
  twobytwo[i, 'estimate'] <- mean(boot$t)
  twobytwo[i, 'upper'] <- boot_ci$bca[,5]
  twobytwo[i, 'lower'] <- boot_ci$bca[,4]
}
# add specification
twobytwo$spec <- '2x2 diff-in-diff'

#### ROBUSTNESS: PLOT ####

negativeweights <- rbind(baseline[,-2],  clubtrend[,-2], eachclub, jackknife, twoyear, twobytwo)

# add performance variable
negativeweights$performance <- rep(c('The Winner Takes it All', 'Objective Performance', 'Performance to the Mean'), each = 4)

negativeweights$type <- c(1,2,3,4)
negativeweights$dep  <- rep(c(1:2), each = 12)  

negativeweights$grid <- rep(c('a) Share racist tweets','b) Share racist fans'), each = 12)

# add number
negativeweights$num <- rep(c(1,2,3,4,5,6), each = 24)

# create for separate df's
weights_skin1     <- negativeweights[negativeweights$type==1,]
weights_skin1_min <- negativeweights[negativeweights$type==2,]
weights_skin2     <- negativeweights[negativeweights$type==3,]
weights_skin2_min <- negativeweights[negativeweights$type==4,]

# make plots
robust_skin1     <- plot_robust(weights_skin1)     + ggtitle('Share of team skin1')
robust_skin1_min <- plot_robust(weights_skin1_min) + ggtitle('Share of minutes skin1')
robust_skin2     <- plot_robust(weights_skin2)     + ggtitle('Share of team skin2')
robust_skin2_min <- plot_robust(weights_skin2_min) + ggtitle('Share of minutes skin2')

ggsave(filename = 'figures/robust_skin1.eps', robust_skin1, height = 8, width = 10)
ggsave(filename = 'figures/robust_skin1_min.eps', robust_skin1_min, height = 8, width = 10)
ggsave(filename = 'figures/robust_skin2.eps', robust_skin2, height = 8, width = 10)
ggsave(filename = 'figures/robust_skin2_min.eps', robust_skin2_min, height = 8, width = 10)


#### ROBUSTNESS NAIVE BAYES ####

# make character vector of moderator and independent variables to loop over 
interaction <- names(df)[13:15] 
independent <- names(df)[17:20] 
dependent   <- names(df)[23:24]
# make empty list to store estimated models
models <- list()

# make two helpers
helper1 <- list()
helper2 <- list()

# loop throught the combinations
for (d in seq_along(dependent)) {
  for (c in seq_along(interaction)) {
    for (k in seq_along(independent)) {
      
      # apply function defined to estimate a generalized difference-in-differences
      helper1[[k]]<- models_fun(dep = df[, dependent[d]], ind = df[, independent[k]], int = df[, interaction[c]], data = df)
      
      # add description of the model and change name of variables
      helper1[[k]]$model <- paste(independent[k], interaction[c], dependent[d])
      helper1[[k]][1,1] <- paste0("share ", "skin",k)
      helper1[[k]][2,1] <- interaction[c]
      helper1[[k]][3,1] <- 'interaction'
    }
    
    # row bind the results forinteraction variables
    helper2[[c]] <- do.call(rbind, helper1)
  }
  
  # row bind the results for dependent variables
  models[[d]] <- do.call(rbind, helper2)
}

# row bind all models and make a data set for both tweets and fans
models_racist <- do.call(rbind, models)

models_racist_tweets <- models_racist[seq(3, 36,  by = 3),]
models_racist_fans   <- models_racist[seq(39, 72,  by = 3),]

# add model specifications
models_racist_tweets$performance <- as.factor(rep(c('The Winner Takes It All', 'Objective Performance', 'Performance to the Mean'), each = 4))
models_racist_fans$performance   <- as.factor(rep(c('The Winner Takes It All','Objective Performance', 'Performance to the Mean'), each = 4))

models_racist_tweets$skin <- rep(c('% team skin1','% min. skin1', '% team skin2', '% min. skin2'), each = 1)
models_racist_fans$skin <- rep(c('% team skin1','% min. skin1', '% team skin2', '% min. skin2'), each = 1)

models_racist_tweets$number <- c(1,2,3,4,1,2,3,4,1,2,3,4)
models_racist_fans$number   <- c(1,2,3,4,1,2,3,4,1,2,3,4) 


p1_1 <- coef_fun(data = models_racist_tweets, x = models_racist_tweets$skin, y = models_racist_tweets$estimate,
                 sd = models_racist_tweets$std.error, col = models_racist_tweets$performance, 
                 num = models_racist_tweets$number) + ggtitle('a) Share of racist tweets') 

p1_2 <- coef_fun(data =models_racist_fans, x = models_racist_fans$skin, y = models_racist_fans$estimate,
                 sd = models_racist_fans$std.error, col = models_racist_fans$performance, 
                 num = models_racist_fans$number) + ggtitle('b) Share of racist fans') 

# extract legend 
legend <- get_legend(
  p1_1 + 
    guides(color = guide_legend(nrow = 2)) +
    theme(legend.position = c(1.1,0.75))
)

robust_nb <- plot_grid(p1_1, p1_2, legend, ncol = 2,  rel_heights = c(1, .2))
ggsave(filename = 'figures/robust_nb_1.eps', robust_nb, width = 7, height = 6)

# Margins 1)
margins1 <- margins_fun(dep = df$share_racist_tweets_nb, ind = df$share_skin2_min, int = df$performance2) +
  labs(x = 'Objective Performance', y = 'Marginal Effects', title = 'a) Share of racist tweets') +
  theme(axis.text = element_text(size = 7), axis.title = element_text(size = 7), plot.title = element_text(size = 7.5)) +
  scale_y_continuous(limits = c(-0.075, 0.075))
xden <- density_fun(plot = margins1, var = df$performance2)
margins1 <- insert_xaxis_grob(margins1, xden, position = "bottom")

# Margins 2)
margins2 <- margins_fun(dep = df$share_racist_fans_nb, ind = df$share_skin2_min, int = df$performance2) + 
  labs(x = 'Objective Performance', y = 'Marginal Effects', title = 'b) Share of racist fans') +
  theme(axis.text = element_text(size = 7), axis.title = element_text(size = 7), plot.title = element_text(size = 7.5)) +
  scale_y_continuous(limits = c(-0.075, 0.075))
xden <- density_fun(plot = margins2, var = df$performance2)
margins2 <- insert_xaxis_grob(margins2, xden, position = "bottom")

margins_racist <- plot_grid(margins1, margins2, ncol = 2, nrow = 1)
ggsave(filename = 'figures/robust_nb_2.png', margins_racist, height = 4, width = 6)

#### ODDS-GRAPH ####
odds <- data.frame(clubrank = rep(seq(0,1, by = .1), times = 3),
                   performance = rep(c('The Winner Takes it All', 'Objective Performance', 
                                       'Performance to the Mean'), each = 11),
                   value = c(seq(-5, 5, by = 1), seq(-1, -2, by = -(1/10)), seq(-3, 1.5, by = 4.5/10)))

oddsplot <- ggplot(odds, aes(x = clubrank, y = value, col = factor(performance))) +
  geom_line() + geom_hline(yintercept = 0, lty = 3) +
  scale_y_continuous(limits = c(-10, 10), breaks = seq(-10, 10, by = 10),
                     labels = c("", 0, ""), name = 'Deviance') +
  scale_x_continuous(breaks = seq(0, 1, by = .5), labels =  c("low rank", 'middle rank', 'high rank'),
                     name = 'Club rank') + theme_classic() + 
  scale_color_manual(name = NULL, values = c('#619CFF','#00BA38', '#F8766D'), 
                     labels = c('Objective performance', 'Performance to the mean','The Winner Takes it All')) +
  theme(axis.ticks = element_blank())
ggsave(filename = 'figures/00_oddsplot.eps', height = 4.5, width = 5.5)


