# 1_simulation_results.R
# plot the simulation results
library(dplyr)
library(ggplot2)
g.theme = theme_bw() + theme(panel.grid.minor = element_blank())

# from 0_simulation.R
load('0_simulation.RData')

# remove results that are not needed; add nicer facet for plot
labels = paste('Impact of funding = ', c(0,0.5,1), sep='')
results = filter(results, 
                 model != 'Randomised - adjusted') %>%
  mutate(
    model = ifelse(model == 'RD', 'Regression discontinuity', model),
    facet = factor(extra_papers, levels = c(0,0.5,1), labels = labels))

# calculate average stats
stats = group_by(results, model, n_applicants_exp, extra_papers, facet) %>%
  summarise(n = n(),
            power = 100*sum(power_null)/n, # using power_null which is any significant effect, regardless of direction
            coverage = 100*sum(coverage) / n)
# plot power
x.breaks = c(20, 100, 200, 1000, 2000) # breaks at observed sample sizes
pplot = ggplot(data = stats, aes(x = n_applicants_exp , y = power, col=factor(model)))+
  geom_line(linewidth=1.05)+
  scale_color_manual('Study design', values=c('dark red','skyblue'))+
  xlab('Number of applicants in experiment')+
  ylab('Statistical power (%)')+
  g.theme +
  scale_x_log10(breaks = x.breaks)+ # use log scale
  theme(
    axis.text.x = element_text(size=7),
    legend.position = c(0.16,0.88))+
  facet_wrap(~facet)
pplot
jpeg('figures/power.jpg', width=6.5, height=4.5, units='in', res=500, quality=100)
print(pplot)
dev.off()

# plot coverage
cplot = ggplot(data = stats, aes(x = n_applicants_exp , y = coverage, col=factor(model)))+
  geom_hline(lty=2, col=grey(0.20), yintercept=95)+
  geom_line(linewidth=1.05)+
  scale_color_manual('Study design', values=c('dark red','skyblue'))+
  xlab('Number of applicants in the experiment')+
  ylab('Coverage (%)')+
  g.theme +
  scale_x_log10(breaks = x.breaks)+ # use log scale
  theme(axis.text.x = element_text(size=7),
        legend.position = c(0.84,0.12),
        legend.background = element_rect(fill='transparent'))+
  facet_wrap(~facet)
cplot
jpeg('figures/coverage.jpg', width=6.5, height=4.5, units='in', res=500, quality=100)
print(cplot)
dev.off()

# plot estimated effects
eplot = ggplot(data=results, aes(x = model, y = exp(estimate), col=factor(extra_papers)))+
  #  geom_hline(yintercept = true_increase, lty=2)+ # reference line at true effect
  geom_boxplot()+
  scale_color_manual('Treatment effect', values=c('grey','pink','dark red'))+
  ylab('Relative ratio due to funding')+
  g.theme+
  theme(legend.position = c(0.8,0.2))+
  facet_wrap(~n_applicants_exp, scales='free_y')
eplot
# plot bias
bplot = ggplot(data=results, aes(x = model, y = bias, col=factor(extra_papers)))+
  geom_boxplot()+
  scale_color_manual('Treatment effect', values=c('grey','pink','dark red'))+
  ylab('Bias')+
  g.theme+
  theme(legend.position = c(0.8,0.2))+
  facet_wrap(~n_applicants_exp, scales='free_y')
bplot

# additional checks of simulation:
#source('99_checks.R')

# for paper
filter(stats, extra_papers == 0.5, n_applicants_exp==200)

## plot latent productivity distribution


# random latent productivity; gamma with mean equal to baseline productivity, avoids zero abilities:
n_applicants = 100000 # huge for plot to show smooth distribution
baseline_productivity = 4 # from 0_simulation.R
gamma_rate = 1.1 # to avoid zero productivity
productivity = rgamma(n = n_applicants, shape = baseline_productivity*gamma_rate, rate = gamma_rate) 
frame = data.frame(x = productivity)
hplot = ggplot(data = frame, aes(x=x))+
  geom_histogram(fill='darkseagreen', col='grey44')+
  xlab('Researcher productivity')+
  ylab('Count')+
  theme_bw()
jpeg('figures/latent_productivity.jpg', width=5.5, height=4.5, units='in', res=500, quality=100)
print(hplot)
dev.off()
