# 0_create_random_data.R
# create the random applicant and outcome data
# called by 0_simulation.R

# random latent productivity; gamma with mean equal to baseline productivity, avoids zero abilities:
gamma_rate = 1.1 # to avoid zero productivity
productivity = rgamma(n = n_applicants, shape = baseline_productivity*gamma_rate, rate = gamma_rate) 
productivity_matrix = matrix(rep(productivity, n_reviews), nrow = n_applicants)

## estimate mean productivity at funding cut-off for rdd design
at_cut_rdd = qgamma(p = 1 - funding_line - rdd_window, shape = baseline_productivity*gamma_rate, rate = gamma_rate)
# true increase as a ratio; adjust for funding cut-off as mean will be higher in those in top half
adjust = at_cut_rdd - baseline_productivity
true_increase_rdd = (baseline_productivity + adjust + extra_papers) / (baseline_productivity + adjust)

## estimate mean productivity at funding cut-off for randomised design
at_cut = qgamma(p = 1 - randomisation_line, shape = baseline_productivity*gamma_rate, rate = gamma_rate)
# true increase as a ratio; adjust for funding cut-off as mean will be higher in those in top half
adjust = at_cut - baseline_productivity
true_increase = (baseline_productivity + adjust + extra_papers) / (baseline_productivity + adjust)

# random noise in reviews:
noise = rnorm(n_applicants*n_reviews, mean = 0, sd = noise_sd) 
noise_matrix = matrix(noise, nrow = n_applicants)
# observed scores are averaged across all reviewers (reviewers can judge productivity with some noise):
scores = rowMeans(noise_matrix + productivity_matrix) 
sim_data = data.frame(productivity = productivity, score = scores) %>%
  mutate(outcome = rpois(n = n(), lambda = productivity)) # outcome: publication counts based only on productivity with no funding
r = cor(scores, productivity)
r_squared = r^2 # R-squared for signal, aim for 23% as per DOI: 10.1371/journal.pone.0165147; trial-and-error based on noise_sd
r_squareds = c(r_squareds, r_squared)

# create groups based on RDD design
observed_funding_line = quantile(sim_data$score, probs = 1 - funding_line)
rdd_lower = quantile(sim_data$score, probs = 1 - funding_line - rdd_window) # lower window for RDD
rdd_upper = quantile(sim_data$score, probs = 1 - funding_line + rdd_window) # upper window for RDD
rdd_data = filter(sim_data,
                  score >= rdd_lower, # apply RDD window
                  score <= rdd_upper) %>%
  mutate(funded = score >= observed_funding_line, # binary funded or not variable
         boost = rpois(n = n(), lambda = extra_papers), # random number of extra papers  
         outcome = outcome + boost*funded) # update outcome to account for funding

# create groups based on randomised design
rand_lower = quantile(sim_data$score, probs = 1 - randomisation_line) # lower window for randomised design
funded_vector = c(rep(TRUE, funding_line*n_applicants), rep(FALSE, funding_line*n_applicants)) # vector of funded, needed for draw below
rand_data = filter(sim_data,
                   score >= rand_lower) %>%
  mutate(funded = sample(funded_vector, size=n(), replace = FALSE), # funded or not
         boost = rpois(n = n(), lambda = extra_papers), # random number of extra papers  
         outcome = outcome + boost*funded) # update outcome to account for funding

# add error if numbers are not the same 
if(nrow(rdd_data) != nrow(rand_data)){cat('error, simulations not equal size')}
if(sum(rdd_data$funded) != sum(rand_data$funded)){cat('error, simulations groups not equal size')}

## check paper numbers by group
stats1 = group_by(rdd_data, funded) %>% 
  summarise(mean = mean(outcome), # average paper numbers
            meana = mean(productivity), # average productivity
            mina = min(productivity)) %>% # smallest productivity
  mutate(sim = sim,
         true_increase = true_increase,
         extra_papers = extra_papers,
         n_applicants = n_applicants,
         model = 'rdd')
stats2 = group_by(rand_data, funded) %>% 
  summarise(mean = mean(outcome), # average paper numbers
            meana = mean(productivity), # average productivity
            mina = min(productivity)) %>% # smallest productivity
  mutate(sim = sim,
         n_applicants = n_applicants,
         model = 'rand')
all_stats = bind_rows(all_stats, stats1, stats2)
