####################
## Set up session ##
####################

# Load packages and variables
source("0_global.R")

####################
## Set up factors ##
####################

# Set up factor levels
lev_var <- c("variant", "invariant")
lab_var <- str_to_title(lev_var)
lev_sim_1a <- c("direct", "indirect", "control")
lab_sim_1a <- str_to_title(lev_sim_1a)
lev_sim <- c("direct", "indirect")
lab_sim <- str_to_title(lev_sim)
lev_exp_1a <- c("direct-variant", "direct-invariant",
                "indirect-variant", "indirect-invariant",
                "control-variant", "control-invariant")
lab_exp_1a <- str_to_title(lev_exp_1a)
lev_exp <- lev_exp_1a[1:4]
lab_exp <- lab_exp_1a[1:4]
lev_match <- c("identity", "competitor", "unrelated")
lab_match <- str_to_title(lev_match)

# Set up factor table: Experiment 1
tab_exp_1a <- tibble(cond_exp = c("test-only", lev_exp_1a),
                     cond_exp_fac = factor(cond_exp,
                                           levels = c("test-only", lev_exp_1a),
                                           labels = c("Test-only", lab_exp_1a)))

# Factor table with Variability x Similarity: Experiment 1
tab_factor_1a <- tibble(cond_var = rep(lev_var, 3),
                        cond_sim = sort(rep(lev_sim_1a, 2)),
                        cond_var_fac = factor(cond_var, levels = lev_var,
                                              labels = lab_var),
                        cond_sim_fac = factor(cond_sim, levels = lev_sim_1a,
                                              labels = lab_sim_1a)) %>%
  mutate(cond_exp_fac = paste(cond_sim_fac, cond_var_fac, sep = "-"),
         cond_exp_fac = factor(cond_exp_fac, levels = lab_exp_1a))

# Factor table for other experiments
tab_exp <- tibble(cond_exp = c("test-only", lev_exp),
                  cond_exp_fac = factor(cond_exp,
                                        levels = c("test-only", lev_exp),
                                        labels = c("Test-only", lab_exp)))

# Factor table with Variability x Similarity for Experiments 2 and 3
tab_factor <- tibble(cond_var = rep(lev_var, 2),
                     cond_sim = sort(rep(lev_sim, 2)),
                     cond_var_fac = factor(cond_var, levels = lev_var,
                                           labels = lab_var),
                     cond_sim_fac = factor(cond_sim, levels = lev_sim,
                                           labels = lab_sim)) %>%
  mutate(cond_exp_fac = paste(cond_sim_fac, cond_var_fac, sep = "-"),
         cond_exp_fac = factor(cond_exp_fac, levels = lab_exp))

# Word type
tab_type <- tibble(stim_type = c("real", "pseudo"),
                   stim_type_fac = factor(stim_type, levels = c("real", "pseudo"),
                                     labels = c("Real words", "Pseudowords")))

# Match type
tab_match <- tibble(match_type = lev_match, 
                    match_type_fac = factor(match_type, 
                                            levels = lev_match,
                                            labels = lab_match))

######################
## Set up contrasts ##
######################

# Create contrast codes: Variability
contrast_var <- matrix(c(1/2, -1/2), 
                       dimnames = list(c("variant", "invariant"), 
                                       c("var_invar")))

# Create contrast codes: Similarity
contrast_sim_1a <- matrix(c(1/2, -1/2, 0,
                            -1/3, -1/3, 2/3), 
                          ncol = 2,
                          dimnames = list(c("direct", "indirect", "control"), 
                                          c("dir_ind", "vot_aff")))

# Create contrast codes: Similarity
contrast_sim <- matrix(c(1/2, -1/2), 
                       dimnames = list(c("direct", "indirect"), 
                                       c("dir_ind")))

# Create contrast codes: Training (individual)
contrast_train_1a <- matrix(c(-1/7, 6/7, -1/7, -1/7, -1/7, -1/7, -1/7,
                              -1/7, -1/7, 6/7, -1/7, -1/7, -1/7, -1/7,
                              -1/7, -1/7, -1/7, 6/7, -1/7, -1/7, -1/7,
                              -1/7, -1/7, -1/7, -1/7, 6/7, -1/7, -1/7,
                              -1/7, -1/7, -1/7, -1/7, -1/7, 6/7, -1/7,
                              -1/7, -1/7, -1/7, -1/7, -1/7, -1/7, 6/7),
                            ncol = 6,
                            dimnames = list(c("test-only", lev_exp_1a), 
                                            c("no_var_dir", "no_invar_dir", 
                                              "no_var_ind", "no_invar_ind",
                                              "no_var_cont", "no_invar_cont")))

# Create contrast codes: Training (individual)
contrast_train <- matrix(c(-1/5, 4/5, -1/5, -1/5, -1/5,
                           -1/5, -1/5, 4/5, -1/5, -1/5,
                           -1/5, -1/5, -1/5, 4/5, -1/5,
                           -1/5, -1/5, -1/5, -1/5, 4/5), 
                         ncol = 4,
                         dimnames = list(c("test-only", lev_exp), 
                                         c("no_var_dir", "no_invar_dir", 
                                           "no_var_ind", "no_invar_ind")))

# Create contrast codes: Match type
contrast_match <- matrix(c(1/2, -1/2, 0,
                           -1/3, -1/3, 2/3), 
                         ncol = 2,
                         dimnames = list(c("identity", "competitor", "unrelated"), 
                                         c("id_comp", "rel_unrel")))

# Create contrast codes: Word type
contrast_type <- matrix(c(1/2, -1/2), 
                        dimnames = list(c("real", "pseudo"), 
                                        c("real_pseudo")))

###############
## Load data ##
###############

# Load exposure data
dat_exp_filt <- read.csv("data/exposure_phase_critical.csv")

# Load test data
dat_test_filt <- read.csv("data/test_phase_critical.csv")

# Filter for RT
dat_exp_rt <- dat_exp_filt %>% dplyr::filter(resp_acc == 1)

# Filter for RT
dat_test_rt <- dat_test_filt %>% dplyr::filter(resp_acc == 1)


