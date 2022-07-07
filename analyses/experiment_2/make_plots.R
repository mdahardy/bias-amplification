### --- Script for generating and saving the figures for Experiment 2 --- ###

library(ggplot2)
library(dplyr)
library(ggsignif)
library(ggthemes)
# Helper functions
source('utils.R')
source('plotting_utils.R')

### --- --- --- --- ### 
### --- Set up --- ###
### --- --- --- --- ###

# The units for defining the dimensions of the plots: cm is centimeters
plots_units = 'cm'
# The height in plots_units for saving the figures
plots_height = 13
# The width in plots_units for saving the figures
plots_width = 14

# Fill colors for each condition in the bar plots
plot_colors = get_plot_colors(2)

# Add line breaks for better styling
condition_names_with_breaks = c('Asocial\nmotivated','Social\nmotivated','Social\nresampling')
# The directory where the plots should be saved
plots_directory = 'plots/e2/'
# Make this directory if it doesn't exist
make_directory(plots_directory)

# -- -- -- -- -- --
# Plot styling
# -- -- -- -- -- --

# Colors
plot_colors = get_plot_colors(2)
condition_colors = plot_colors$condition_colors
blue_green_colors = plot_colors$blue_green_colors

# Condition shapes
condition_shapes = c(
  'Asocial motivated' = 21,
  'Social motivated' = 22,
  'Social resampling'=24)

### --- --- --- --- ### 
### --- Load and prepare data --- ###
### --- --- --- --- ### 

# All e2 data
e2_data = load_e2_data(include_cloned = T) %>% 
  pretty_condition_names()

### --- Choice data for comparing original vs. resampled choices --- ###
original_choice_data = e2_data %>%
  subset(condition == 'Social resampling') %>%
  summarize_choice_data(c('generation','network_identifier','randomization_color')) %>%
  mutate(type='original')

resampled_choice_data = e2_data %>%
  subset(condition == 'Social resampling') %>%
  subset(generation>1) %>%
  group_by(generation,network_identifier,randomization_color) %>%
  summarize(
    n=n(),
    bias = mean(social_info_bias),
    bias_se = sqrt(var(social_info_bias) / n),
    green = mean(social_info_green),
    green_se = sqrt(var(social_info_green) / n),
    accuracy = mean(social_info_accuracy),
    accuracy_se = sqrt(var(social_info_accuracy) / length(accuracy)),
  ) %>%
  mutate(generation = generation -1,type = 'resampled')

choice_data = rbind(original_choice_data,resampled_choice_data) %>%
  arrange(match(randomization_color, c('green','blue'))) %>%
  mutate(randomization_color = factor(randomization_color,levels=c('green','blue')))

### --- Organize choice_data differently for scatter plots --- ###
original_data = choice_data %>%
  subset(generation<8) %>%
  subset(type=='original') %>%
  transmute(
    generation_network = paste(generation,network_identifier,sep='-'),
    network_identifier,
    randomization_color,
    original_bias = bias,
    original_bias_se = bias_se,
    original_green = green,
    original_green_se = green_se
  )

resampled_data = choice_data %>%
  subset(generation<8) %>%
  subset(type=='resampled') %>%
  transmute(
    generation_network = paste(generation,network_identifier,sep='-'),
    randomization_color,
    network_identifier,
    resampled_bias = bias,
    resampled_bias_se = bias_se,
    resampled_green = green,
    resampled_green_se = green_se
  )

scatter_data = merge(original_data,resampled_data,by='generation_network')

### --- --- --- --- --- --- --- --- ### 
### --- --- --- PLOTS --- --- --- ---###
### --- --- --- --- --- --- --- --- ### 

### --- --- --- --- --- --- --- --- ### 
### --- Bias by condition bar plot --- ###
### --- --- --- --- --- --- --- --- ### 

# Bias bar plot
bias_plot = e2_data %>%
  subset(is_cloned == F) %>% 
  summarize_choice_data('condition') %>%
  ggplot(aes(x=condition,y=bias,fill=condition)) + 
  geom_bar(stat='identity') +
  scale_fill_manual("legend", values = plot_colors$condition_colors)+
  geom_errorbar(aes(ymin=bias-bias_se,ymax=bias+bias_se),width=0.2) +
  geom_signif(y_position = c(0.575,0.575), xmin = c(1,2.05), 
              xmax = c(1.95,3), annotation = c('***','**'),
              textsize=7,vjust=0.3) +
  ylab('Proportion of biased choices') + 
  coord_cartesian(ylim=c(0.52,0.5772))+
  scale_x_discrete(labels=condition_names_with_breaks) +
  theme_styling() + 
  barplot_styling()

# Save
paste0(plots_directory,'bias.pdf') %>%
  ggsave(bias_plot,width=plots_width,height=plots_height,units=plots_units)

### --- --- --- --- --- --- --- --- --- ### 
### --- Accuracy by condition bar plot --- ###
### --- --- --- --- --- --- --- --- --- ### 

# Accuracy bar plot
accuracy_plot = e2_data %>%
  subset(is_cloned == F) %>% 
  summarize_choice_data('condition') %>%
  ggplot(aes(x=condition,y=accuracy,fill=condition)) + 
  geom_bar(stat='identity') +
  scale_fill_manual("legend", values = plot_colors$condition_colors)+
  geom_errorbar(aes(ymin=accuracy-accuracy_se,ymax=accuracy+accuracy_se),width=0.2) +
  geom_signif(y_position = c(0.64,0.65), xmin = c(1,1), 
              xmax = c(2,3), annotation = c('***','***'),
              textsize=7,vjust=0.3) +
  ylab('Proportion of correct choices') + 
  coord_cartesian(ylim=c(0.55,0.6548))+
  scale_x_discrete(labels=condition_names_with_breaks)+
  theme_styling() +
  barplot_styling()

# Save
paste0(plots_directory,'accuracy.pdf') %>%
  ggsave(accuracy_plot,width=plots_width,height=plots_height,units=plots_units)

### --- --- --- --- --- --- --- --- --- ### 
### --- Green bias of social information by condition and randomization color --- ###
### --- --- --- --- --- --- --- --- --- ### 

social_green_bar = e2_data %>%
  subset(is_cloned == F) %>% 
  summarize_observed_data(c('condition','randomization_color')) %>%
  exclude_asocial_data() %>%
  ggplot(aes(x=condition,y=social_info_green,fill=randomization_color)) + 
  geom_bar(stat='identity', position=position_dodge(0.9)) +
  geom_errorbar(aes(ymin=social_info_green-social_info_green_se,
                    ymax=social_info_green+social_info_green_se),
                width=0.3,
                size=0.75,
                position=position_dodge(0.9)) +
  scale_fill_manual(name='Bias color', labels=c('Blue','Green'), values = plot_colors$blue_green_colors)+
  scale_shape_manual(name='Bias color', labels=c('Blue','Green'),values=c(16,15)) +
  scale_x_discrete(labels=c('Social\nmotivated','Social\nresampling'))+
  ylab('Proportion of observed votes for green') + 
  coord_cartesian(ylim=c(0.31,0.82)) +
  scale_y_continuous(expand=c(0,0)) +
  theme_styling() +
  theme(axis.text.x = element_text(size=16,color='black'),axis.title.x=element_blank())

# Add points for network-generation means
social_green_network_means = e2_data %>%
  subset(is_cloned == F) %>% 
  summarize_observed_data(c('randomization_color','condition','condition_replication','generation')) %>%
  exclude_asocial_data() %>%
  geom_point(data=., 
             aes(x = condition, y = social_info_green,shape=randomization_color),
             alpha=0.225,
             position = position_jitterdodge(jitter.width = 0.25,dodge.width = 0.9),
             size=2.5) 

# Put both together
green_votes_barplot = social_green_bar + social_green_network_means +
  theme(legend.position = 'none',
        panel.grid.major = element_line(colour = "#ededed"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

# Save
paste0(plots_directory,'green_votes_barplot.pdf') %>%
  ggsave(green_votes_barplot,width=13,height=15,units=plots_units)

### --- --- --- --- --- --- --- --- --- ### 
### --- Participant level data: Average green votes observed by bias color and condition --- ###
### --- --- --- --- --- --- --- --- --- ### 

participant_level_data = e2_data %>%
  subset(is_cloned==F) %>%
  summarize_observed_data(c('participant_id','randomization_color','condition')) %>%
  exclude_asocial_data() %>%
  arrange(condition,-social_info_green) %>%
  mutate(
    observed_green_votes = social_info_green * 8,
    green_ordering = rep(1:(n()/2),2)
  )

participant_observations = participant_level_data %>%
  ggplot(aes(x=green_ordering,y=observed_green_votes/8,fill=randomization_color)) +
  geom_bar(stat='identity')+
  facet_grid(cols = vars(condition)) +
  coord_cartesian(ylim=c(0.31,0.82),expand=c(0,0))+
  scale_fill_manual(name='Bias color', labels=c('Blue','Green'), 
                    values = plot_colors$blue_green_colors)+
  labs(y='Proportion of observed votes green') + 
  theme_styling() +
  theme(
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.ticks.x = element_blank(),
    strip.background = element_blank(),
    legend.position = 'none',
    panel.spacing =unit(1.25, "lines"),
    panel.grid.major = element_line(colour = "#ededed"),
    strip.text = element_blank())

# Save
paste0(plots_directory,'participant_observations.pdf') %>%
  ggsave(participant_observations,width=25,height=15,units=plots_units)

### --- --- --- --- --- --- --- --- ### 
### --- --- --- --- --- --- --- --- ### 
# Exploratory plots below
### --- --- --- --- --- --- --- --- ### 
### --- --- --- --- --- --- --- --- ### 
stop()

### --- --- --- --- --- --- --- --- ### 
### --- Bias of original vs. resampled choices: scatter --- ###
### --- --- --- --- --- --- --- --- ###

bias_scatter = scatter_data %>%
  ggplot(aes(x=original_bias,y=resampled_bias,color=randomization_color.x)) +
  geom_abline(slope=1, intercept=0)+
  geom_point(size=2)+
  scale_color_manual(values=blue_green_colors)+
  labs(x='Original bias proportion',y='Resampled bias proportion')+
  ylim(0.25,0.75)+
  xlim(0.25,0.75)+
  theme_styling() +
  theme(
    legend.position='none'
  )

# Save
paste0(plots_directory,'bias_scatter.pdf') %>%
  ggsave(bias_scatter,width=20,height=20,units=plots_units)


### --- --- --- --- --- --- --- --- ### 
### --- Green of original vs. resampled choices: scatter --- ###
### --- --- --- --- --- --- --- --- ###

green_scatter = scatter_data %>%
  ggplot(aes(x=original_green,y=resampled_green,color=randomization_color.x)) +
  geom_point(size=2)+
  scale_color_manual(values=blue_green_colors)+
  ylim(0.45,0.7)+
  xlim(0.39,0.75)+
  labs(x='Original green proportion',y='Resampled green proportion')+
  theme_styling() +
  theme(
    legend.position='none'
  )

# Save
paste0(plots_directory,'green_scatter.pdf') %>%
  ggsave(green_scatter,width=20,height=20,units=plots_units)

### --- --- --- --- --- --- --- --- ### 
### --- Green facet: Bias of original choices vs. resampled, by network --- ###
### --- --- --- --- --- --- --- --- ###

green_network_plot = scatter_data %>%
  subset(randomization_color.x=='green') %>%
  mutate(condition = ifelse(as.integer(substr(generation_network,0,1))==1,'Asocial motivated','Social resampling')) %>%
  arrange(desc(row_number())) %>% 
  ggplot(aes(x=original_bias,y=resampled_bias,fill=condition)) + 
  geom_hline(yintercept=0.46,size=2.5) + 
  geom_vline(xintercept=0.46,size=2.5)+
  geom_segment(aes(x=0.46,xend=0.8,y=0.46,yend=0.8),linetype='dashed',size=1.25)+
  #geom_abline(slope=1, intercept=0,linetype='dashed',size=1.25)+
  geom_errorbarh(aes(xmax=original_bias+original_bias_se,
                    xmin=original_bias-original_bias_se,
                    height=0.025),
                 size=1.75,
                color='#6e6e6e')+
  geom_errorbar(aes(ymax=resampled_bias+resampled_bias_se,
                    ymin=resampled_bias-resampled_bias_se),
                width=0.025,
                size=1.75,
                color='#6e6e6e') +
  geom_point(shape=21,color='#6e6e6e',size=12,stroke=2.5) +
  scale_fill_manual(name='Condition', 
                    values = c(condition_colors['Asocial motivated'],condition_colors['Social resampling'])) +
  #geom_point(color='#a1dab4',size=4) +
  #facet_wrap(~ network_identifier.x,scales='free',nrow=1) +
  facet_grid(cols=vars(network_identifier.x),scales='free')+
  scale_x_continuous(limits=c(0.46,0.8),expand=c(0,0))+
  scale_y_continuous(limits=c(0.46,0.8),expand=c(0,0))+
  theme_styling() +
  labs(x='Bias of original choices',y='Bias of\nresampled choices')+
  theme(
    strip.text.x = element_blank(),
    panel.grid.major = element_line(colour = "#ededed"),
    panel.spacing = unit(3, "lines"),
    axis.line=element_blank(),
    axis.text = element_text(color='black',size=38),
    axis.title = element_text(size=42),
    axis.ticks = element_line(colour = "black"),
    legend.position='none'
  )

# Save
paste0(plots_directory,'green_network_plot.pdf') %>%
  ggsave(green_network_plot,width=95.9,height=15.18,units=plots_units)

### --- --- --- --- --- --- --- --- ### 
### --- Blue facet: Bias of original choices vs. resampled, by network --- ###
### --- --- --- --- --- --- --- --- ###

blue_network_plot = scatter_data %>%
  subset(randomization_color.x=='blue') %>%
  mutate(condition = ifelse(as.integer(substr(generation_network,0,1))==1,'Asocial motivated','Social resampling')) %>%
  arrange(desc(row_number())) %>% 
  ggplot(aes(x=original_green,y=resampled_green,fill=randomization_color)) + 
  geom_hline(yintercept=0.23,size=2.5) + 
  geom_vline(xintercept=0.23,size=2.5)+
  geom_segment(aes(x=0.23,xend=0.65,y=0.23,yend=0.65),linetype='dashed',size=1.25)+
  #geom_abline(slope=1, intercept=0,linetype='dashed',size=1.25)+
  # geom_errorbarh(aes(xmax=original_green+original_green_se,
  #                    xmin=original_green-original_green_se,
  #                    height=0.025),
  #                size=1.75,
  #                color='#6e6e6e')+
  # geom_errorbar(aes(ymax=resampled_green+resampled_green_se,
  #                   ymin=resampled_green-resampled_green_se),
  #               width=0.025,
  #               size=1.75,
  #               color='#6e6e6e') +
  geom_point(shape=21,color='#6e6e6e',size=12,stroke=2.5) +
  # scale_fill_manual(name='Condition', 
  #                   values = c(condition_colors['Asocial bias'],condition_colors['Social resampling'])) +
  #geom_point(color='#a1dab4',size=4) +
  #facet_wrap(~ network_identifier.x,scales='free',nrow=1) +
  facet_grid(cols=vars(network_identifier.x),scales='free')+
  scale_x_continuous(limits=c(0.23,0.65),expand=c(0,0))+
  scale_y_continuous(limits=c(0.23,0.65),expand=c(0,0))+
  theme_styling() +
  labs(x='Proportion of original choices green',y='Proportion of\nresampled choices green')+
  theme(
    strip.text.x = element_blank(),
    panel.grid.major = element_line(colour = "#ededed"),
    panel.spacing = unit(6, "lines"),
    axis.line=element_blank(),
    axis.text = element_text(color='black',size=38),
    axis.title = element_text(size=42),
    axis.ticks = element_line(colour = "black"),
    legend.position='none'
  )

# Save
paste0(plots_directory,'blue_network_plot.pdf') %>%
  ggsave(blue_network_plot,width=95.9,height=15.18,units=plots_units)

### --- --- --- --- --- --- --- --- ### 
### ---Bias of original choices vs. resampled, by network --- ###
### Basically, above two plots (blue and green) put together into one plot ###
### --- --- --- --- --- --- --- --- ###

bias_by_generation_and_network = scatter_data %>%
  mutate(
    condition = ifelse(as.integer(substr(generation_network,0,1))==1,'Asocial motivated','Social resampling'),
    condition_replication = substr(network_identifier.x,14,14)
  ) %>%
  arrange(desc(row_number())) %>% 
  ggplot(aes(x=original_green,y=resampled_green,fill=randomization_color.y)) + 
  #geom_segment(aes(x=0.23,xend=0.65,y=0.23,yend=0.65),linetype='dashed',size=1.25)+
  geom_abline(slope=1, intercept=0,linetype='dashed',size=1.25)+
  geom_errorbarh(aes(xmax=original_green+original_green_se,
                     xmin=original_green-original_green_se,
                     height=0.025),
                 size=1.75,
                 color='#6e6e6e')+
  geom_errorbar(aes(ymax=resampled_green+resampled_green_se,
                    ymin=resampled_green-resampled_green_se),
                width=0.025,
                size=1.75,
                color='#6e6e6e') +
  geom_point(shape=21,color='#6e6e6e',size=10,stroke=2) +
  # scale_fill_manual(name='Condition', 
  #                   values = c(condition_colors['Asocial bias'],condition_colors['Social resampling'])) +
  #geom_point(color='#a1dab4',size=4) +
  #facet_wrap(~ network_identifier.x,scales='free',nrow=1) +
  #facet_grid(rows=vars(randomization_color.x),cols=vars(condition_replication),scales='free')+
  # scale_x_continuous(limits=c(0.23,0.65),expand=c(0,0))+
  # scale_y_continuous(limits=c(0.23,0.65),expand=c(0,0))+
  theme_styling() +
  labs(x='Green of original choices',y='Green of nresampled choices')+
  theme(
    strip.text = element_blank(),
    panel.grid.major = element_line(colour = "#ededed"),
    panel.spacing = unit(3.5, "lines"),
    axis.line=element_blank(),
    axis.text = element_text(color='black',size=38),
    axis.title = element_text(size=42),
    #axis.ticks = element_line(colour = "black"),
    axis.ticks = element_blank(),
    legend.position='none'
  )

# Save
paste0(plots_directory,'bias_by_generation_and_network.pdf') %>%
  ggsave(bias_by_generation_and_network,width=100,height=25,units=plots_units)

### --- --- --- --- --- --- --- --- ### 
### --- Scatter plot: participant biases and how often their were transmitted  --- ###
### --- --- --- --- --- --- --- --- ###

# THIS IS THE RELEVANT PLOT

participant_bias_plot = e2_data %>%
  subset(is_cloned==F) %>%
  mutate(
    condition = ifelse(first_genration==1,'Asocial motivated','Social resampling'),
    condition_replication = substr(condition_replication,1,1)
  ) %>%
  arrange(first_genration) %>% 
  ggplot(aes(x=decision_origin_id_bias,y=impressions,fill=condition)) +
  geom_hline(yintercept=128,linetype='dashed',size=1.25) +
  geom_vline(xintercept=0,linetype='dashed',size=1.25) +
  geom_point(shape=21,color='#6e6e6e',size=5,stroke=0,alpha=0.6) +
  scale_y_continuous(breaks=c(60,90,120,150))+
  scale_fill_manual(name='Condition', 
                  values = c(condition_colors['Asocial motivated'],condition_colors['Social resampling'])) +
  theme_styling() +
  labs(y='Number of choices transmitted',x='Bias towards green') +
  theme(
    strip.text = element_blank(),
    panel.grid.major = element_line(colour = "#ededed"),
    panel.spacing = unit(3.5, "lines"),
    axis.line=element_blank(),
    axis.text = element_text(color='black',size=38),
    axis.title = element_text(size=42),
    axis.ticks = element_blank(),
    legend.position='none'
  )

# Save
paste0(plots_directory,'participant_biases.pdf') %>%
  ggsave(participant_bias_plot,width=100,height=25,units=plots_units)
