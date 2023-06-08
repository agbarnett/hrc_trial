# 0_simulation.R
# simulate randomised trial versus regression discontinuity
# January 2023
library(dplyr)
library(janitor)
library(stringr)
source('99_functions.R')
TeachingDemos::char2seed('orient') # to give consistent results

## part 1: key constants ###
# top proportion that are funded
funding_line = 0.1
# top proportion that enter the randomised trial, assume double the funded proportion
randomisation_line = funding_line * 2 
# proportion around funding line considered equivalent for regression discontinuity 
rdd_window = funding_line # 
# size of sample (small to large)
sample_size = c(100, 500, 1000, 5000, 10000) #
# number of reviewers per application
n_reviews = 4 
# noise standard deviation, controls the signal-to-noise ratio
noise_sd = 7.1 
# baseline productivity, mean number of papers per year
baseline_productivity = 4
# extra papers associated with funding (per year), include null effect
effect_sizes = c(0, 0.5, 1)
# number of simulations
n_sim = 10000 # see Morris for sample size calculation ( Monte Carlo SE)

## loop
results = all_stats = r_squareds = NULL
for (extra_papers in effect_sizes){ # loop through effect sizes
  for (n_applicants in sample_size){ # loop through sample sizes
      for (sim in 1:n_sim){
  
  ## report - add sys.time
  if(sim==1){cat('Extra papers = ', extra_papers, ', number of applicants = ', n_applicants, '.\n', sep='')}
  
  ## part 2: simulate scores and outcomes ##
  source('0_create_random_data.R')
  
  ## part 3: run models ##
  rdd_data = mutate(rdd_data, scorec = score - mean(score)) # centre score
  rand_data = mutate(rand_data, scorec = score - mean(score)) # centre score
  # Allowing over-dispersion
  model_rdd = glm(outcome ~ funded + scorec, data = rdd_data, family = quasipoisson()) # has score to control for productivity
  s1 = make_nice_estimates(model_rdd, name = 'RD', n_applicants = n_applicants, extra_papers = extra_papers, true_increase = true_increase)
  model_rand = glm(outcome ~ funded, data = rand_data, family = quasipoisson())
  s2 = make_nice_estimates(model_rand, name = 'Randomised', n_applicants = n_applicants, extra_papers = extra_papers, true_increase = true_increase)
  model_rand_adjusted = glm(outcome ~ funded + scorec, data = rand_data, family = quasipoisson())
  s3 = make_nice_estimates(model_rand_adjusted, name = 'Randomised - adjusted', n_applicants = n_applicants, extra_papers = extra_papers, true_increase = true_increase)
  results = bind_rows(results, s1, s2, s3)
  
}
} # end of sample size loop
} # end of effect size loop

## part 4: create stats and save results ##
# calculate bias and coverage
row.names(results) = NULL
results = mutate(results, 
                 n_applicants_exp = n_applicants * (funding_line*2), # number of applicants in the experiment
                 power_null = pr_t < 0.05, # statistically significant effect in either direction
                 power = (estimate > 0) & pr_t < 0.05, # statistically significant positive effect
                 bias = perc - true_increase,
                 coverage = perc_lower < true_increase & true_increase < perc_upper) # truth within estimated CI

# save results
meta = list(funding_line = funding_line,
            randomisation_line = randomisation_line,
            n_reviews = n_reviews,
            noise_sd = noise_sd,
            baseline_productivity = baseline_productivity,
            n_sim = n_sim) # meta-data about the simulation
save(meta, results, file='0_simulation.RData')

