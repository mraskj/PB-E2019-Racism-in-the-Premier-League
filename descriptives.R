#=========================================================================================#
#                                                                                         #
# Program: 01_descriptives.R                                                              #
# Aim    : This script produces the plots for the data and measurement section            #
# Date   : 20-01-2020                                                                     #
# Authors: Frederik Skjerning and Mathias Rask                                            #
#                                                                                         #
#=========================================================================================#

#### SETUP ####

# clear environment
rm(list=ls())

# load libraries
library(ggplot2)
library(cowplot)
library(data.table)

# set working directory
setwd("/Users/mathiasrask/Desktop/Racism-in-Premier-League/")

load('data/08_finaldf.Rdata')

df <- setDT(df)
#=========================================================================================#

#### RACIST - ACROSS CLUBS AND TIME ####

df_ym <- df[, `:=`(share_tweets_ym = mean(share_racist_tweets_svm),
                share_fans_ym   = mean(share_racist_fans_svm)), by = yearmonth
         ][, .SD[1], by = yearmonth]

time_racist <- ggplot(df_ym, aes(x = factor(yearmonth))) +
  geom_line(aes(y = share_tweets_ym, group = 1, col = 'tweets')) +
  geom_line(aes(y = share_fans_ym,   group = 1, col = 'fans')) +
  scale_x_discrete(breaks = paste0(c(2011:2019), '-08')) +
  scale_color_manual(name = NULL, values = c("yellow3", "black")) +
  scale_y_continuous(limits = c(0, 0.15)) +
  labs(x='time', y = 'share') + 
  theme_light() +
  theme(axis.title  = element_text(size=9),
        axis.text   = element_text(size = 7),
        plot.margin = unit(c(0.25, 0.25, 1.2, 0.25), "cm"),
        legend.text = element_text(size = 8),
        legend.position = c(0.5,-0.15)) +
  guides(col = guide_legend(nrow=1))

ggsave('figures/descriptives/plot1_racistovertime.eps', time_racist, height = 4.5, width = 5.5)

#### DISTRIBUTIONS ####

## SKIN 
skin_min_dist <- ggplot(df) +
  geom_density(aes(x=share_skin1_min, fill = 'skin1'), alpha = .3) +
  geom_density(aes(x=share_skin2_min, fill = 'skin2'), alpha = .3) +
  scale_fill_manual(name = NULL, values = c("#999999", "#E69F00")) +
  labs(y = 'density', x = '% total minutes') + ggtitle('a) Density for % minutes') +
  theme_light() + theme(axis.title = element_text(size=9),
                        legend.position = c(0.80,0.87),
                        legend.text = element_text(size = 8),
                        plot.margin = unit(c(0.25,0.2,0.2,0.2), "cm"),
                        plot.title = element_text(size = 8))

skin_team_dst <- ggplot(df) +
  geom_density(aes(x=share_skin1, fill = 'skin1'), alpha = .3) +
  geom_density(aes(x=share_skin2, fill = 'skin2'), alpha = .3) +
  scale_fill_manual(name = NULL, values = c("#56B4E9", "#009E73")) + 
  labs(y = 'density', x = '% of team') + ggtitle('b) Density for % team') +
  theme_light() + theme(axis.title = element_text(size=9),
                        legend.position = c(0.80,0.87),
                        legend.text = element_text(size = 8),
                        plot.margin = unit(c(0.25,0.2,0.2,0.2), "cm"),
                        plot.title = element_text(size = 8))

performance_dist <- ggplot(df) +
  geom_density(aes(x=performance1, fill = 'orchid4'), alpha = .3) +
  geom_density(aes(x=performance2, fill = 'skyblue3'), alpha = .3) +
  geom_density(aes(x=performance3, fill = 'springgreen3'), alpha = .3) +
  scale_fill_manual(name = NULL, values = c('#F8766D', '#619CFF','#00BA38'), 
                    labels = c('The Winner Takes it All', 'Objective Performance',
                               'Performance to the Mean')) +
  labs(y = 'density', x = 'performance') + ggtitle('c) Density for performance') +
  theme_light() + theme(axis.title = element_text(size=9),
                        plot.margin = unit(c(0.25,0.2,1.6,0.2), "cm"),
                        legend.position = c(0.5, -0.18),
                        legend.text = element_text(size = 6),
                        plot.title = element_text(size = 8)) +
  guides(fill = guide_legend(nrow = 2))   

racist_dist <- ggplot(df) +
  geom_density(aes(x=share_racist_tweets_svm, fill = 'racist tweets'), alpha = .3) +
  geom_density(aes(x=share_racist_fans_svm, fill = 'racist fans'), alpha = .3) +
  scale_fill_manual(name = NULL, values = c('yellow3', 'black')) +
  labs(y = 'density', x = 'share') + ggtitle('d) Density for racial hate speech') +
  theme_light() + theme(axis.title = element_text(size=9),
                        plot.margin = unit(c(0.25,0.2,1.6,0.2), "cm"),
                        legend.position = c(0.5, -0.17),
                        legend.text = element_text(size = 7),
                        plot.title = element_text(size = 8)) +
  guides(fill = guide_legend(nrow = 1))           
   
skin_dist <- plot_grid(skin_min_dist, skin_team_dst, ncol = 2)

ggsave('figures/descriptives/plot2_1_dist_skin.png', skin_dist, height = 5, width = 6)
ggsave('figures/descriptives/plot2_2_dist_perm.png', performance_dist, height = 5, width = 6)
ggsave('figures/descriptives/plot2_3_dist_race.png', racist_dist, height = 5, width = 6)

#### WITHIN CLUB OVER TIME ####  

club_df <- df[, `:=`(share_tweets_season = mean(share_racist_tweets_svm),
                        share_fans_season   = mean(share_racist_fans_svm),
                        performance1_season = mean(performance1),
                        performance2_season = mean(performance2),
                        performance3_season = mean(performance3)), by = .(club, season)
                 ][, .SD[1], by = .(club, season)]

players <- ggplot(club_df, aes(x = factor(season))) +
  geom_line(aes(y = share_skin2_min, group = 1, col = '% minutes skin2')) +
  geom_line(aes(y = share_skin2,     group = 1, col = '% team skin2')) +
  geom_line(aes(y = share_skin1_min, group = 1, col = '% minutes skin1')) +
  geom_line(aes(y = share_skin1,     group = 1, col = '% team skin1')) +
  scale_x_discrete(breaks = c(2011, 2013, 2015, 2017, 2019)) +
  facet_wrap(.~factor(club)) + labs(x = 'season', y = 'share') + 
  scale_color_manual(name = NULL, values = c("#999999", "#E69F00", "#56B4E9", "#009E73")) +
  theme_light() +
  theme(axis.title  = element_text(size=8),
        axis.text   = element_text(size = 7),
        plot.margin = unit(c(0.25, 0.25, 0.25, 0.25), "cm"),
        legend.text = element_text(size = 6),
        legend.position = c(0.85,0.075),
        strip.text = element_text(size = 7))


performance <- ggplot(club_df, aes(x = factor(season))) +
  geom_line(aes(y = performance1, group = 1, col = 'The Winner Takes It All')) +
  geom_line(aes(y = performance2, group = 1, col = 'Objective Performance')) +
  geom_line(aes(y = performance3, group = 1, col = 'Performance to the Mean')) +
  scale_x_discrete(breaks = c(2011, 2013, 2015, 2017, 2019)) +
  facet_wrap(.~factor(club)) + labs(x = 'season', y = 'performance') + 
  scale_color_manual(name = NULL, values = c('#619CFF','#00BA38', '#F8766D')) +
  theme_light() +
  theme(axis.title  = element_text(size=8),
        axis.text   = element_text(size = 7),
        plot.margin = unit(c(0.25, 0.25, 0.25, 0.25), "cm"),
        legend.text = element_text(size = 6),
        legend.position = c(0.88,0.075),
        strip.text = element_text(size = 7))

racist <- ggplot(club_df, aes(x = factor(season))) +
  geom_line(aes(y = share_tweets_season, group = 1, col = 'tweets')) +
  geom_line(aes(y = share_fans_season, group = 1, col = 'fans')) +
  scale_x_discrete(breaks = c(2011, 2013, 2015, 2017, 2019)) +
  facet_wrap(.~factor(club)) + labs(x = 'season', y = 'performance') + 
  scale_color_manual(name = NULL, values = c("yellow3", "black")) +
  theme_light() +
  theme(axis.title  = element_text(size=8),
        axis.text   = element_text(size = 7),
        plot.margin = unit(c(0.25, 0.25, 0.25, 0.25), "cm"),
        legend.text = element_text(size = 6),
        legend.position = c(0.88,0.075),
        strip.text = element_text(size = 7))

ggsave(filename = 'figures/descriptives/plot3_1_players.eps',     players, height = 4.5, width = 5.5)
ggsave(filename = 'figures/descriptives/plot3_2_performance.eps', performance, height = 4.5, width = 5.5)
ggsave(filename = 'figures/descriptives/plot3_3_racial.eps', racist, height = 4.5, width = 5.5)

