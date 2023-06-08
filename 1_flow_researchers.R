# 1_flow_researchers.R
# make flow diagram of researchers over time
# June 2021
library(dplyr)
library(janitor)
library(ggplot2)

# get the data
load('data/researchers.RData') # from 0_read_data.R
# make random number per researcher
researchers = mutate(researchers, 
                     num = as.numeric(as.factor(name)))
# now order numbers by first year
start_year =  group_by(researchers, num) %>%
  summarise(miny = min(year))
to_plot = left_join(researchers, start_year, by='num') %>%
  mutate(res_num = ifelse(result=='Funded', 1, 2), 
         new_num = miny+(num/100),
         new_num = as.numeric(as.factor(new_num)))
# add 2021 as last year of follow-up for everyone
last_year = data.frame(new_num = 1:max(to_plot$new_num), year=2021, res_num=3)
to_plot = bind_rows(to_plot, last_year) %>%
  select(year, new_num, result, res_num)

# plot
lplot = ggplot(data=to_plot, aes(x=year, y=new_num, col=factor(res_num), group=new_num))+
  geom_line(col='grey')+
  geom_point()+
  scale_colour_manual(NULL, values=c('goldenrod1','dodgerblue','transparent'), labels=c('Funded','Not Funded',''))+
  theme_bw()+
  xlab('Year')+
  ylab('')+
  scale_y_continuous(breaks=NULL, expand=c(0.01,0.01))+
  theme(legend.position=c(0.15,0.85))
lplot
jpeg('figures/follow_up.jpg', width=4.5, height=5.5, units='in', res=600, quality=100)
print(lplot)
dev.off()

# calculate years of follow-up
max_year = 2021
follow = arrange(researchers, name, year) %>%
  group_by(name) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(fu = max_year - year) %>%
  summarise(total = sum(fu))
cat('There are ', follow$total, ' years of follow-up.\n', sep='')