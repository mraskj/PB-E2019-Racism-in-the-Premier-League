#================================================================================#
#                                                                                #
# Program: functions.R                                                        #
# Aim    : This script produces functions used in the analysis                   #
#                                                                                #
#================================================================================#

#### SETUP ####

# clear enviroment
rm(list=ls())

# set working directory
getwd()
setwd("/Users/mathiasrask/Desktop/Racism-in-Premier-League/")

# load libraries
library(lfe)
library(broom)
library(ggplot2)
library(boot)

#### FUNCTIONS ####

# Function to estimate a generalized difference-in-differences
models_fun <- function(dep, ind, int, data) {
  tidy(felm(dep ~ ind + int + ind:int + factor(premier_league) | factor(club) + 
              factor(season) | 0 | club + season, data = data))[-1,-4]
}

# Function to estimate 2x2 difference-in-differences
models_2by2fun <- function(dep, ind, int, data) {
  tidy(felm(dep ~ ind + int + ind:int | factor(club) + factor(season)  | 0 | club + season , data = data))[-1,-4]
}

# Function to estimate a generalized difference-in-differences
clubmodels_fun <- function(dep, ind, int) {
  tidy(felm(dep ~ ind + int + ind:int |  factor(season) | 0 | season, data = club_df))[3,]
}

# Function to construct coefficient plots (plot1 and plot2)
coef_fun <- function(data, x, sd, y, col, num) {
  ggplot(data, aes(y = y, x = reorder(factor(x), num), col = factor(col))) +
    geom_point(position = position_dodge(width = 1/2)) + 
    geom_hline(yintercept = 0, lty = 3) +
    geom_errorbar(aes(ymin = y - 1.64*sd, ymax = y + 1.64*sd), width = 0,
                  position = position_dodge(width = 1/2)) +
    scale_color_manual(name = NULL, values = c('#619CFF','#00BA38', '#F8766D'),
                       labels = c('Objective performance', 'Performance to the mean', 'The Winner Takes It All')) +
    labs(x = NULL, y = 'Estimated ATT\n(90% CI)')  +
    coord_flip() + theme_classic() +
    theme(axis.title.x = element_text(size = 9.5),
          legend.position = 'none',
          title = element_text(size = 8))
}

# Function to estimate the model and visualize the marginal effects
margins_fun <- function(dep, ind, int, data = df) {
  model <- felm(dep ~ ind + int + ind:int + factor(premier_league) | factor(club) + 
                  factor(season) | 0 | club + season, data = df)
  
  betahat <- coef(model)
  vcov    <- vcov(model)
  moderator <- seq(min(int), max(int), length.out = 1000) 
  dydx <- betahat["ind"] + betahat["ind:int"]*moderator 
  se_dydx <- sqrt(vcov["ind", "ind"] + (moderator^2)*vcov["int", "int"] + 2*moderator*vcov["ind", "int"])
  upper <- dydx + 1.64*se_dydx
  lower <- dydx - 1.64*se_dydx
  
  plot <- ggplot(data=NULL, aes(x=moderator, y=dydx)) +
    geom_line(aes(moderator, dydx),size = 0.5) +
    geom_hline(yintercept=0, size = 0.5, lty=3) +
    geom_ribbon(aes(ymin=lower, ymax=upper), alpha=0.3) +
    theme_classic() + 
    theme(plot.title = element_text(size = 8.5, hjust = 0.5),
          axis.title = element_text(size = 8))
  
  return(plot)
}

# Function to produce marginal distributions to be added on marginsplot
density_fun <- function(plot, var) {
  xden <- axis_canvas(plot, axis = 'x') + 
    geom_density(data = df, aes(x=var), alpha = .5,
                 fill = '#436db2', col = 'white')
  density <- ggplot_build(xden)$data[[1]] 
  quan75 <- quantile(var, prob = 0.75)
  quan25 <- quantile(var, prob = 0.25)
  xden <- xden + 
    geom_area(data = subset(density, x > quan75), aes(x=x, y=y), fill="#619cff", alpha = .5) +
    geom_area(data = subset(density, x < quan25), aes(x=x, y=y), fill="#619cff", alpha = .5) 
  
  return(xden) 
}

# Function to produce club plots
club_coef_fun <- function(data, y, x, reorder, col, labels) {
  ggplot(data, aes(y = y, x = reorder(x, -reorder), col = factor(col))) +
    geom_point(position = position_dodge(width = 1/2), size = .7) +
    geom_hline(yintercept = 0, lty = 3) +
    geom_errorbar(aes(ymin = y - 1.64*std.error, ymax = y + 1.64*std.error), width = 0,
                  position = position_dodge(width = 1/2), size = .4) +
    scale_color_manual(name = NULL, values = c('#F8766D', '#619CFF', '#00BA38'), 
                       labels = c('The Winner Takes it All','Objective performance', 'Performance to the mean')) +
    labs(x = NULL, y = 'Estimated ATT (90% CI)')  +
    coord_flip() + theme_classic() + facet_grid(.~spec, labeller = as_labeller(labels)) +
    theme(legend.position = 'none',
          plot.margin = unit(c(.1, .1, 1, .1), "cm"),
          axis.text.y = element_text(size = 5.5),
          axis.text.x = element_text(size = 6),
          strip.text = element_text(size = 6),
          axis.title.x = element_text(size = 6),
          legend.text = element_text(size = 6))
}

# Function to be used when bootstrapping
mean.fun <- function(dat, idx) mean(dat[idx], na.rm = T)

# Function to construct robustness plot
plot_robust <- function(data) {
  ggplot(data, aes(y = estimate, x = reorder(spec, -num), col = factor(performance))) +
    geom_point(position = position_dodge(width = 1/2)) + 
    geom_hline(yintercept = 0, lty = 3) +
    geom_errorbar(aes(ymin = lower, ymax =  upper), width = 0,
                  position = position_dodge(width = 1/2)) +
    coord_flip() + scale_color_manual(name = NULL, values = c('#619CFF','#00BA38', '#F8766D'),
                                      labels = c('Objective performance', 'Performance to the mean', 'The Winner Takes It All'))+
    labs(x = NULL, y = 'Estimated ATT\n(90% CI)')  + theme_classic() + facet_grid(. ~ grid) +
    theme(axis.title.x    = element_text(size = 9),
          plot.title      = element_text(size = 9, hjust = 0.5), 
          axis.text.x     = element_text(size = 8),
          axis.text.y     = element_text(size = 9), 
          strip.text      = element_text(size = 9),
          plot.margin     = unit(c(.1, .1, .9, .1), "cm"),
          legend.position = c(0.5, -0.45),
          legend.text     = element_text(size = 8)) +
    guides(color = guide_legend(nrow = 1))
}

