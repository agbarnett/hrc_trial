# 99_response_referee.R
# power calculation for response to referee
library(dplyr)
library(broom)
library(broom.mixed)
library(lme4)

# key constants
n_per_group = 44
mean = 4.5 # papers per year
n_sim = 1000
sd_researcher = 0.1
effect = 10 # absolute increase in percent
reffect = (100 + effect)/100 # relative multiplier effect
n_years = 4 # years of follow-up

# set up basic frame
f1 = expand.grid(year = 1:n_years, id = paste('a.', 1:n_per_group, sep='')) %>% 
  mutate(group = 0, lmean = mean) 
f1_noise = data.frame(id = paste('a.', 1:n_per_group, sep=''), noise = rnorm(n = n_per_group, mean = 0, sd = sd_researcher))
f1 = full_join(f1, f1_noise, by='id')                   
f2 = expand.grid(year = 1:n_years, id = paste('c.', 1:n_per_group, sep='')) %>% 
  mutate(group = 1, lmean = mean * reffect)
f2_noise = data.frame(id = paste('c.', 1:n_per_group, sep=''), noise = rnorm(n = n_per_group, mean = 0, sd = sd_researcher))
f2 = full_join(f2, f2_noise, by='id')                   
frame = bind_rows(f1, f2)

# big loop
results = NULL
for (k in 1:n_sim){
  this_sim = mutate(frame, 
                    papers = rpois(n=n(), lambda= lmean + noise))
  if(n_years == 1){
    model = glm(papers ~ group , family=poisson(), data=this_sim)
  }
  if(n_years > 1){
    model = glmer(papers ~ group + (1|id), family=poisson(), data=this_sim)
  }
  e = tidy(model) %>%
    filter(term == 'group') %>%
    mutate(sim = k)
  results = bind_rows(results, e)
}

# power
hist(results$estimate)
summary(results$estimate)
prop.table(table(results$p.value <0.05))
