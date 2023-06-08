# 99_checks.R
# additional checks of simulation, called by 0_simulation.R

# checks of observed papers and ability in simulated data
ggplot(data = all_stats, aes(x=funded, y=mean))+
  geom_boxplot() +
  facet_grid(n_applicants~model)+
  ylab('Mean outcome')+
  theme_bw()
# the following plot shows there is a difference in ability in the RDD but not the randomised design
ggplot(data = all_stats, aes(x=funded, y=meana))+
  geom_boxplot() +
  facet_grid(n_applicants~model)+
  ylab('Mean ability')+
  theme_bw()
group_by(all_stats, model, n_applicants, funded) %>%
  summarise(mean(meana))