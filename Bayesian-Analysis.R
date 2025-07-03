attach(adm_data)
Data <- adm_data

library(ggplot2)

###############################################################################
# Descriptive analysis
###############################################################################


###########################
#Training and  Testing data

## 75% of the sample size as training
smp_size <- floor(0.75 * nrow(Data))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(Data)), size = smp_size)

Train_Data <- Data[train_ind, ]
Test_Data <- Data[-train_ind, ]


##############################
# Plots
par(mfrow=c(2,2))
# Change line color and fill color
ggplot(Train_Data, aes(x=CGPA))+
  geom_histogram(color="darkblue", fill="lightblue")

# Change line color and fill color
ggplot(Train_Data, aes(x=GRE.Score))+
  geom_histogram(color="darkblue", fill="lightblue")

# Change line color and fill color
ggplot(Train_Data, aes(x=TOEFL.Score))+
  geom_histogram(color="darkblue", fill="lightblue")


###############################
# Numerical summary

#CGPA
summary(Train_Data$CGPA)
sd(Train_Data$CGPA)

#GRE Score
summary(Train_Data$GRE.Score)
sd(Train_Data$GRE.Score)

#TOEFL Score
summary(Train_Data$TOEFL.Score)
sd(Train_Data$TOEFL.Score)




#######################################
#Data Generative models, priors  and posteriors



#######
#CGPA
#######

# Consider data genarative model as normal with mean 8.5 and Variance 0.35

#Data-generating parameters
mu = 8.5
sigma_2 =0.35

#sample size 
nsamp = 1000

set.seed(1234) 
# X when n equal to 100
x_sample = rnorm(nsamp, mean = mu, sd = sqrt(sigma_2))
rm()

##############################################
#Prior Type 1

# when prior Normal with mean 2 and variance 1



#Hyperparameters for normal prior on mean 
mu0 = 2
tau0 = 1

#Posterior parameters
mu_n = (tau0*mean(x_sample)*nsamp + mu0*sigma_2) / (nsamp*tau0  + sigma_2)
sigman_2 = tau0* sigma_2/(nsamp * tau0  + sigma_2)




set.seed(1234)
posterior_samples = rnorm(1000, mean = mu_n, sd = sqrt(sigman_2))
rm()

set.seed(1234)
#Monte Carlo 
posterior_check_sample_MC = rep(NA, 1000)
for (kk in 1:1000){
  posterior_check_sample_MC[kk] = rnorm(1,mean = rnorm(1, mean = mu_n, sd = sqrt(sigman_2)), sd =sqrt(sigman_2))
}

rm()


checkdata = data.frame(x = c(posterior_samples,posterior_check_sample_MC),
                       ind = rep(c('Extact posterior','MC posterior'),each = 1000))
ggplot(checkdata,aes(x=x, color=ind)) + 
  geom_histogram(fill='white',alpha=0.5)+
  ggtitle(" comparision the exact posterior density curve with the empirical distribution")



###############################
# Posterior predictive distribution
#Also follows normal distribution with 
# mean = Posterior mean
# Variance = Posterior variance + Likelihood variance



#Hyperparameters for normal posterior predictive are
mu1 = mu_n
tau1 = sigman_2+sigma_2



set.seed(1234)
posterior_pred_samples = rnorm(1000, mean = mu1, sd = sqrt(tau1))
rm()

posterior_pred_samples <- data.frame(posterior_pred_samples)

lowerbound = qnorm(0.025,mu1,sqrt(tau1))
upperbound = qnorm(0.975,mu1,sqrt(tau1))

library(ggplot2)
test_CGPA <- tibble(x = Test_Data$CGPA, y = 0)

p <- ggplot(posterior_pred_samples, aes(x=posterior_pred_samples)) + 
  geom_density() +
  geom_jitter(data = test_CGPA, aes(x,y), height = 0.01)


p +
  geom_vline(xintercept =  lowerbound, size = 1.5, color='blue') + 
  geom_vline(xintercept =  upperbound, size = 1.5, color='blue')

CGPA_Test <- Test_Data$CGPA

# Count observations between bounds
count <- sum(CGPA_Test >= lowerbound & CGPA_Test <= upperbound)

print(count)



##############################################
#Prior Type 1

# Using flat prior with mu and sigma proportional to 1

#Posterior parameters
mu_n = mu
sigman_2 = sigma_2




set.seed(1234)
posterior_samples = rnorm(1000, mean = mu_n, sd = sqrt(sigman_2))
rm()

set.seed(1234)
#Monte Carlo 
posterior_check_sample_MC = rep(NA, 1000)
for (kk in 1:1000){
  posterior_check_sample_MC[kk] = rnorm(1,mean = rnorm(1, mean = mu_n, sd = sqrt(sigman_2)), sd =sqrt(sigman_2))
}

rm()


checkdata = data.frame(x = c(posterior_samples,posterior_check_sample_MC),
                       ind = rep(c('Extact posterior','MC posterior'),each = 1000))
ggplot(checkdata,aes(x=x, color=ind)) + 
  geom_histogram(fill='white',alpha=0.5)+
  ggtitle(" comparision the exact posterior density curve with the empirical distribution")



###############################
# Posterior predictive distribution
#Also follows normal distribution with 
# mean = Posterior mean
# Variance = Posterior variance + Likelihood variance



#Hyperparameters for normal posterior predictive are
mu1 = mu_n
tau1 = sigman_2+sigma_2



set.seed(1234)
posterior_pred_samples = rnorm(1000, mean = mu1, sd = sqrt(tau1))
rm()

posterior_pred_samples <- data.frame(posterior_pred_samples)

lowerbound = qnorm(0.025,mu1,sqrt(tau1))
upperbound = qnorm(0.975,mu1,sqrt(tau1))

library(ggplot2)
test_CGPA <- tibble(x = Test_Data$CGPA, y = 0)

p <- ggplot(posterior_pred_samples, aes(x=posterior_pred_samples)) + 
  geom_density() +
  geom_jitter(data = test_CGPA, aes(x,y), height = 0.01)


p +
  geom_vline(xintercept =  lowerbound, size = 1.5, color='blue') + 
  geom_vline(xintercept =  upperbound, size = 1.5, color='blue')

CGPA_Test <- Test_Data$CGPA

# Count observations between bounds
count <- sum(CGPA_Test >= lowerbound & CGPA_Test <= upperbound)

print(count)




#######
#GRE Score
#######

# Consider data genarative model as normal with mean 320 and variance 120

#Data-generating parameters
mu = 320
sigma_2 =120

#sample size 
nsamp = 1000

set.seed(1234) 
# X when n equal to 100
x_sample = rnorm(nsamp, mean = mu, sd = sqrt(sigma_2))
rm()

##############################################
#Prior Type 1

# when prior Normal with mean 250 and variance 100



#Hyperparameters for normal prior on mean 
mu0 = 250
tau0 = 100

#Posterior parameters
mu_n = (tau0*mean(x_sample)*nsamp + mu0) / (nsamp*tau0  + 1)
sigman_2 = tau0 /(nsamp * tau0  + 1)




set.seed(1234)
posterior_samples = rnorm(1000, mean = mu_n, sd = sqrt(sigman_2))
rm()

set.seed(1234)
#Monte Carlo 
posterior_check_sample_MC = rep(NA, 1000)
for (kk in 1:1000){
  posterior_check_sample_MC[kk] = rnorm(1,mean = rnorm(1, mean = mu_n, sd = sqrt(sigman_2)), sd =sqrt(sigman_2))
}

rm()


checkdata = data.frame(x = c(posterior_samples,posterior_check_sample_MC),
                       ind = rep(c('Extact posterior','MC posterior'),each = 1000))
ggplot(checkdata,aes(x=x, color=ind)) + 
  geom_histogram(fill='white',alpha=0.5)+
  ggtitle(" comparision the exact posterior density curve with the empirical distribution")



###############################
# Posterior predictive distribution
#Also follows normal distribution with 
# mean = Posterior mean
# Variance = Posterior variance + Likelihood variance



#Hyperparameters for normal posterior predictive are
mu1 = mu_n
tau1 = sigman_2+sigma_2



set.seed(1234)
posterior_pred_samples = rnorm(1000, mean = mu1, sd = sqrt(tau1))
rm()

posterior_pred_samples <- data.frame(posterior_pred_samples)

lowerbound = qnorm(0.025,mu1,sqrt(tau1))
upperbound = qnorm(0.975,mu1,sqrt(tau1))

library(ggplot2)
test_GRE <- tibble(x = Test_Data$GRE.Score, y = 0)

p <- ggplot(posterior_pred_samples, aes(x=posterior_pred_samples)) + 
  geom_density() +
  geom_jitter(data = test_GRE, aes(x,y), height = 0.01)


p +
  geom_vline(xintercept =  lowerbound, size = 1.5, color='blue') + 
  geom_vline(xintercept =  upperbound, size = 1.5, color='blue')

GRE_Test <- Test_Data$GRE.Score

# Count observations between bounds
count <- sum(GRE_Test >= lowerbound & GRE_Test <= upperbound)

print(count)



##############################################
#Prior Type 2

# Using flat prior with mu and sigma proportional to 1

#Posterior parameters
mu_n = mu
sigman_2 = sigma_2




set.seed(1234)
posterior_samples = rnorm(1000, mean = mu_n, sd = sqrt(sigman_2))
rm()

set.seed(1234)
#Monte Carlo 
posterior_check_sample_MC = rep(NA, 1000)
for (kk in 1:1000){
  posterior_check_sample_MC[kk] = rnorm(1,mean = rnorm(1, mean = mu_n, sd = sqrt(sigman_2)), sd =sqrt(sigman_2))
}

rm()


checkdata = data.frame(x = c(posterior_samples,posterior_check_sample_MC),
                       ind = rep(c('Extact posterior','MC posterior'),each = 1000))
ggplot(checkdata,aes(x=x, color=ind)) + 
  geom_histogram(fill='white',alpha=0.5)+
  ggtitle(" comparision the exact posterior density curve with the empirical distribution")



###############################
# Posterior predictive distribution
#Also follows normal distribution with 
# mean = Posterior mean
# Variance = Posterior variance + Likelihood variance



#Hyperparameters for normal posterior predictive are
mu1 = mu_n
tau1 = sigman_2+sigma_2



set.seed(1234)
posterior_pred_samples = rnorm(1000, mean = mu1, sd = sqrt(tau1))
rm()

posterior_pred_samples <- data.frame(posterior_pred_samples)

lowerbound = qnorm(0.025,mu1,sqrt(tau1))
upperbound = qnorm(0.975,mu1,sqrt(tau1))

library(ggplot2)
test_GRE <- tibble(x = Test_Data$GRE.Score, y = 0)

p <- ggplot(posterior_pred_samples, aes(x=posterior_pred_samples)) + 
  geom_density() +
  geom_jitter(data = test_GRE, aes(x,y), height = 0.01)


p +
  geom_vline(xintercept =  lowerbound, size = 1.5, color='blue') + 
  geom_vline(xintercept =  upperbound, size = 1.5, color='blue')

GRE_Test <- Test_Data$GRE.Score

# Count observations between bounds
count <- sum(GRE_Test >= lowerbound & GRE_Test <= upperbound)

print(count)





#######
#TOEFL Score
#######

# Consider data genarative model as normal with mean 320 and standard deviation 6

#Data-generating parameters
mu = 110
sigma_2 =36

#sample size 
nsamp = 1000

set.seed(1234) 
# X when n equal to 100
x_sample = rnorm(nsamp, mean = mu, sd = sqrt(sigma_2))
rm()

##############################################
#Prior Type 1

# when prior Normal with mean 100 and variance 25



#Hyperparameters for normal prior on mean 
mu0 = 100
tau0 = 25

#Posterior parameters
mu_n = (tau0*mean(x_sample)*nsamp + mu0) / (nsamp*tau0  + 1)
sigman_2 = tau0 /(nsamp * tau0  + 1)




set.seed(1234)
posterior_samples = rnorm(1000, mean = mu_n, sd = sqrt(sigman_2))
rm()

set.seed(1234)
#Monte Carlo 
posterior_check_sample_MC = rep(NA, 1000)
for (kk in 1:1000){
  posterior_check_sample_MC[kk] = rnorm(1,mean = rnorm(1, mean = mu_n, sd = sqrt(sigman_2)), sd =sqrt(sigman_2))
}

rm()


checkdata = data.frame(x = c(posterior_samples,posterior_check_sample_MC),
                       ind = rep(c('Extact posterior','MC posterior'),each = 1000))
ggplot(checkdata,aes(x=x, color=ind)) + 
  geom_histogram(fill='white',alpha=0.5)+
  ggtitle(" comparision the exact posterior density curve with the empirical distribution")



###############################
# Posterior predictive distribution
#Also follows normal distribution with 
# mean = Posterior mean
# Variance = Posterior variance + Likelihood variance



#Hyperparameters for normal posterior predictive are
mu1 = mu_n
tau1 = sigman_2+sigma_2



set.seed(1234)
posterior_pred_samples = rnorm(1000, mean = mu1, sd = sqrt(tau1))
rm()

posterior_pred_samples <- data.frame(posterior_pred_samples)

lowerbound = qnorm(0.025,mu1,sqrt(tau1))
upperbound = qnorm(0.975,mu1,sqrt(tau1))

library(ggplot2)
test_TOEFL <- tibble(x = Test_Data$TOEFL.Score, y = 0)

p <- ggplot(posterior_pred_samples, aes(x=posterior_pred_samples)) + 
  geom_density() +
  geom_jitter(data = test_TOEFL, aes(x,y), height = 0.01)


p +
  geom_vline(xintercept =  lowerbound, size = 1.5, color='blue') + 
  geom_vline(xintercept =  upperbound, size = 1.5, color='blue')

TOEFL_Test <- Test_Data$TOEFL.Score

# Count observations between bounds
count <- sum(TOEFL_Test >= lowerbound & TOEFL_Test <= upperbound)

print(count)



##############################################
#Prior Type 2

# Using flat prior with mu and sigma proportional to 1

#Posterior parameters
mu_n = mu
sigman_2 = sigma_2




set.seed(1234)
posterior_samples = rnorm(1000, mean = mu_n, sd = sqrt(sigman_2))
rm()

set.seed(1234)
#Monte Carlo 
posterior_check_sample_MC = rep(NA, 1000)
for (kk in 1:1000){
  posterior_check_sample_MC[kk] = rnorm(1,mean = rnorm(1, mean = mu_n, sd = sqrt(sigman_2)), sd =sqrt(sigman_2))
}

rm()


checkdata = data.frame(x = c(posterior_samples,posterior_check_sample_MC),
                       ind = rep(c('Extact posterior','MC posterior'),each = 1000))
ggplot(checkdata,aes(x=x, color=ind)) + 
  geom_histogram(fill='white',alpha=0.5)+
  ggtitle(" comparision the exact posterior density curve with the empirical distribution")



###############################
# Posterior predictive distribution
#Also follows normal distribution with 
# mean = Posterior mean
# Variance = Posterior variance + Likelihood variance



#Hyperparameters for normal posterior predictive are
mu1 = mu_n
tau1 = sigman_2+sigma_2



set.seed(1234)
posterior_pred_samples = rnorm(1000, mean = mu1, sd = sqrt(tau1))
rm()

posterior_pred_samples <- data.frame(posterior_pred_samples)

lowerbound = qnorm(0.025,mu1,sqrt(tau1))
upperbound = qnorm(0.975,mu1,sqrt(tau1))

library(ggplot2)
test_TOEFL <- tibble(x = Test_Data$TOEFL.Score, y = 0)

p <- ggplot(posterior_pred_samples, aes(x=posterior_pred_samples)) + 
  geom_density() +
  geom_jitter(data = test_TOEFL, aes(x,y), height = 0.01)


p +
  geom_vline(xintercept =  lowerbound, size = 1.5, color='blue') + 
  geom_vline(xintercept =  upperbound, size = 1.5, color='blue')

TOEFL_Test <- Test_Data$TOEFL.Score

# Count observations between bounds
count <- sum(TOEFL_Test >= lowerbound & TOEFL_Test <= upperbound)

print(count)


rm(list = ls())
data <- adm_data$CGPA

# Likelihood function (Normal)
likelihood_normal <- function(data, mu, sigma) {
  sum(dnorm(data, mean = mu, sd = sigma, log = TRUE))
}

# Prior function (Normal)
prior_normal <- function(mu, sigma, mu_prior_mean, mu_prior_sd, sigma_prior_mean, sigma_prior_sd) {
  dnorm(mu, mean = mu_prior_mean, sd = mu_prior_sd, log = TRUE) + 
    dnorm(sigma, mean = sigma_prior_mean, sd = sigma_prior_sd, log = TRUE)
}

# Metropolis-Hastings algorithm
metropolis_hastings_normal <- function(data, iterations, mu_init, sigma_init, proposal_sd_mu, proposal_sd_sigma, mu_prior_mean, mu_prior_sd, sigma_prior_mean, sigma_prior_sd) {
  mu <- numeric(iterations)
  sigma <- numeric(iterations)
  
  mu[1] <- mu_init
  sigma[1] <- sigma_init
  
  for (i in 2:iterations) {
    # Proposal step
    proposed_mu <- rnorm(1, mu[i-1], proposal_sd_mu)
    proposed_sigma <- rnorm(1, sigma[i-1], proposal_sd_sigma)
    
    # Check if proposed parameters are within reasonable bounds
    if (proposed_sigma <= 0) {
      mu[i] <- mu[i-1]
      sigma[i] <- sigma[i-1]
      next
    }
    
    # Compute likelihood and prior for proposed parameters
    likelihood_proposed <- sum(dnorm(data, mean = proposed_mu, sd = proposed_sigma, log = TRUE))
    prior_proposed <- sum(dnorm(proposed_mu, mean = mu_prior_mean, sd = mu_prior_sd, log = TRUE)) + 
      sum(dnorm(proposed_sigma, mean = sigma_prior_mean, sd = sigma_prior_sd, log = TRUE))
    
    # Compute likelihood and prior for current parameters
    likelihood_current <- sum(dnorm(data, mean = mu[i-1], sd = sigma[i-1], log = TRUE))
    prior_current <- sum(dnorm(mu[i-1], mean = mu_prior_mean, sd = mu_prior_sd, log = TRUE)) + 
      sum(dnorm(sigma[i-1], mean = sigma_prior_mean, sd = sigma_prior_sd, log = TRUE))
    
    # Compute acceptance probability
    acceptance_prob <- exp(likelihood_proposed + prior_proposed - likelihood_current - prior_current)
    
    # Accept or reject
    if (!is.finite(acceptance_prob) || runif(1) < acceptance_prob) {
      mu[i] <- proposed_mu
      sigma[i] <- proposed_sigma
    } else {
      mu[i] <- mu[i-1]
      sigma[i] <- sigma[i-1]
    }
  }
  
  return(data.frame(mu = mu, sigma = sigma))
}



# Set initial values
mu_init <- mean(data)
sigma_init <- sd(data)

# Set proposal standard deviations
proposal_sd_mu <- 0.1
proposal_sd_sigma <- 0.1

# Set prior parameters
mu_prior_mean <- 8
mu_prior_sd <- 1
sigma_prior_mean <- 1
sigma_prior_sd <- 1

# Number of iterations
iterations <- 10000

# Run Metropolis-Hastings algorithm
results <- metropolis_hastings_normal(data, iterations, mu_init, sigma_init, proposal_sd_mu, proposal_sd_sigma, mu_prior_mean, mu_prior_sd, sigma_prior_mean, sigma_prior_sd)

# Plot posterior distributions
par(mfrow=c(2,1))
hist(results$mu, main="Posterior Distribution of Mu", xlab="Mu", breaks=30)
hist(results$sigma, main="Posterior Distribution of Sigma", xlab="Sigma", breaks=30)

curve(dnorm(x, mean(results$mu), sd(results$mu)), from = min(results$mu), to = max(results$mu), n = 1000, main = "Posterior Distribution of Mu", xlab = "Mu", ylab = "Density", col = "blue", lwd = 2)
curve(dnorm(x, mean(results$sigma), sd(results$sigma)), from = min(results$sigma), to = max(results$sigma), n = 1000, main = "Posterior Distribution of Sigma", xlab = "Sigma", ylab = "Density", col = "blue", lwd = 2)

# Trace plot function
trace_plot <- function(samples, parameter_name) {
  plot(samples, type = "l", col = "blue", xlab = "Iteration", ylab = parameter_name, main = paste("Trace Plot of", parameter_name))
}

# Autocorrelation plot function
autocorrelation_plot <- function(samples, parameter_name) {
  acf_result <- acf(samples, main = paste("Autocorrelation Plot of", parameter_name))
}

par(mfrow=c(2,1))
trace_plot(results$mu, "Mu")
trace_plot(results$sigma, "Sigma")

# Plot autocorrelation plots
par(mfrow=c(2,1))
autocorrelation_plot(results$mu, "Mu")
autocorrelation_plot(results$sigma, "Sigma")

effective_sample_size <- function(samples) {
  acf_result <- acf(samples, plot = FALSE)
  rho <- acf_result$acf[-1]
  n <- length(samples)
  ESS <- n / (1 + 2 * sum(rho))
  return(ESS)
}

# Acceptance rate function
acceptance_rate <- function(accepted_moves, total_moves) {
  return(accepted_moves / total_moves)
}

# Mixing rate function (autocorrelation decay)
mixing_rate <- function(samples) {
  acf_result <- acf(samples, plot = FALSE)
  rho <- acf_result$acf[-1]
  return(max(abs(rho)))
}

# Compute metrics
mu_ess <- effective_sample_size(results$mu)
sigma_ess <- effective_sample_size(results$sigma)
mu_acceptance <- acceptance_rate(sum(!is.na(diff(results$mu))), iterations - 1)
sigma_acceptance <- acceptance_rate(sum(!is.na(diff(results$sigma))), iterations - 1)
mu_mixing <- mixing_rate(results$mu)
sigma_mixing <- mixing_rate(results$sigma)

# Print results
cat("Effective Sample Size (Mu):", mu_ess, "\n")
cat("Effective Sample Size (Sigma):", sigma_ess, "\n")
cat("Acceptance Rate (Mu):", mu_acceptance, "\n")
cat("Acceptance Rate (Sigma):", sigma_acceptance, "\n")
cat("Mixing Rate (Mu):", mu_mixing, "\n")
cat("Mixing Rate (Sigma):", sigma_mixing, "\n")




# Function to perform posterior predictive checks on new data
posterior_predictive_check <- function(new_data, mu_samples, sigma_samples) {
  predictions <- matrix(nrow = length(mu_samples), ncol = length(new_data))
  
  for (i in 1:length(mu_samples)) {
    for (j in 1:length(new_data)) {
      predictions[i, j] <- rnorm(1, mu_samples[i], sigma_samples[i])
    }
  }
  
  return(predictions)
}


# Extract sampled parameters from the Metropolis-Hastings results
mu_samples <- results$mu  # Assuming 'results' is your Metropolis-Hastings results object
sigma_samples <- results$sigma

# Perform posterior predictive check on new data
posterior_predictive_samples <- posterior_predictive_check(new_data, mu_samples, sigma_samples)

# Calculate how many variables fit in the posterior predictive model
fit_variables <- apply(posterior_predictive_samples, 2, function(x) sum(x >= min(data) & x <= max(data)))  # Assuming 'data' is your original dataset

# Print the count of variables fitting in the posterior predictive model
cat("Number of variables fitting in the posterior predictive model:", sum(fit_variables > 0), "\n")


rm(list = ls())
data <- adm_data$`GRE Score`

# Likelihood function (Normal)
likelihood_normal <- function(data, mu, sigma) {
  sum(dnorm(data, mean = mu, sd = sigma, log = TRUE))
}

# Prior function (Normal)
prior_normal <- function(mu, sigma, mu_prior_mean, mu_prior_sd, sigma_prior_mean, sigma_prior_sd) {
  dnorm(mu, mean = mu_prior_mean, sd = mu_prior_sd, log = TRUE) + 
    dnorm(sigma, mean = sigma_prior_mean, sd = sigma_prior_sd, log = TRUE)
}

# Metropolis-Hastings algorithm
# Metropolis-Hastings algorithm
metropolis_hastings_normal <- function(data, iterations, mu_init, sigma_init, proposal_sd_mu, proposal_sd_sigma, mu_prior_mean, mu_prior_sd, sigma_prior_mean, sigma_prior_sd) {
  mu <- numeric(iterations)
  sigma <- numeric(iterations)
  
  mu[1] <- mu_init
  sigma[1] <- sigma_init
  
  for (i in 2:iterations) {
    # Proposal step
    proposed_mu <- rnorm(1, mu[i-1], proposal_sd_mu)
    proposed_sigma <- rnorm(1, sigma[i-1], proposal_sd_sigma)
    
    # Check if proposed parameters are within reasonable bounds
    if (proposed_sigma <= 0) {
      mu[i] <- mu[i-1]
      sigma[i] <- sigma[i-1]
      next
    }
    
    # Compute likelihood and prior for proposed parameters
    likelihood_proposed <- sum(dnorm(data, mean = proposed_mu, sd = proposed_sigma, log = TRUE))
    prior_proposed <- sum(dnorm(proposed_mu, mean = mu_prior_mean, sd = mu_prior_sd, log = TRUE)) + 
      sum(dnorm(proposed_sigma, mean = sigma_prior_mean, sd = sigma_prior_sd, log = TRUE))
    
    # Compute likelihood and prior for current parameters
    likelihood_current <- sum(dnorm(data, mean = mu[i-1], sd = sigma[i-1], log = TRUE))
    prior_current <- sum(dnorm(mu[i-1], mean = mu_prior_mean, sd = mu_prior_sd, log = TRUE)) + 
      sum(dnorm(sigma[i-1], mean = sigma_prior_mean, sd = sigma_prior_sd, log = TRUE))
    
    # Compute acceptance probability
    acceptance_prob <- exp(likelihood_proposed + prior_proposed - likelihood_current - prior_current)
    
    # Accept or reject
    if (!is.finite(acceptance_prob) || runif(1) < acceptance_prob) {
      mu[i] <- proposed_mu
      sigma[i] <- proposed_sigma
    } else {
      mu[i] <- mu[i-1]
      sigma[i] <- sigma[i-1]
    }
  }
  
  return(data.frame(mu = mu, sigma = sigma))
}



# Set initial values
mu_init <- mean(data)
sigma_init <- sd(data)

# Set proposal standard deviations
proposal_sd_mu <- 0.1
proposal_sd_sigma <- 0.1

# Set prior parameters
mu_prior_mean <- 300
mu_prior_sd <- 1
sigma_prior_mean <- 10
sigma_prior_sd <- 1

# Number of iterations
iterations <- 10000

# Run Metropolis-Hastings algorithm
results <- metropolis_hastings_normal(data, iterations, mu_init, sigma_init, proposal_sd_mu, proposal_sd_sigma, mu_prior_mean, mu_prior_sd, sigma_prior_mean, sigma_prior_sd)

# Plot posterior distributions
par(mfrow=c(2,1))
hist(results$mu, main="Posterior Distribution of Mu", xlab="Mu", breaks=30)
hist(results$sigma, main="Posterior Distribution of Sigma", xlab="Sigma", breaks=30)
curve(dnorm(x, mean(results$mu), sd(results$mu)), from = min(results$mu), to = max(results$mu), n = 1000, main = "Posterior Distribution of Mu", xlab = "Mu", ylab = "Density", col = "blue", lwd = 2)
curve(dnorm(x, mean(results$sigma), sd(results$sigma)), from = min(results$sigma), to = max(results$sigma), n = 1000, main = "Posterior Distribution of Sigma", xlab = "Sigma", ylab = "Density", col = "blue", lwd = 2)

trace_plot <- function(samples, parameter_name) {
  plot(samples, type = "l", col = "blue", xlab = "Iteration", ylab = parameter_name, main = paste("Trace Plot of", parameter_name))
}

# Autocorrelation plot function
autocorrelation_plot <- function(samples, parameter_name) {
  acf_result <- acf(samples, main = paste("Autocorrelation Plot of", parameter_name))
}

par(mfrow=c(2,1))
trace_plot(results$mu, "Mu")
trace_plot(results$sigma, "Sigma")

# Plot autocorrelation plots
par(mfrow=c(2,1))
autocorrelation_plot(results$mu, "Mu")
autocorrelation_plot(results$sigma, "Sigma")


# Function to perform posterior predictive checks on new data
posterior_predictive_check <- function(new_data, mu_samples, sigma_samples) {
  predictions <- matrix(nrow = length(mu_samples), ncol = length(new_data))
  
  for (i in 1:length(mu_samples)) {
    for (j in 1:length(new_data)) {
      predictions[i, j] <- rnorm(1, mu_samples[i], sigma_samples[i])
    }
  }
  
  return(predictions)
}
# Assuming you have 100 variables for which you want to perform posterior predictive check

# Extract sampled parameters from the Metropolis-Hastings results
mu_samples <- results$mu  # Assuming 'results' is your Metropolis-Hastings results object
sigma_samples <- results$sigma

# Perform posterior predictive check on new data
posterior_predictive_samples <- posterior_predictive_check(new_data, mu_samples, sigma_samples)

# Calculate how many variables fit in the posterior predictive model
fit_variables <- apply(posterior_predictive_samples, 2, function(x) sum(x >= min(data) & x <= max(data)))  # Assuming 'data' is your original dataset

# Print the count of variables fitting in the posterior predictive model
cat("Number of variables fitting in the posterior predictive model:", sum(fit_variables > 0), "\n")


rm(list = ls())
data <- adm_data$`TOEFL Score`

# Likelihood function (Normal)
likelihood_normal <- function(data, mu, sigma) {
  sum(dnorm(data, mean = mu, sd = sigma, log = TRUE))
}

# Prior function (Normal)
prior_normal <- function(mu, sigma, mu_prior_mean, mu_prior_sd, sigma_prior_mean, sigma_prior_sd) {
  dnorm(mu, mean = mu_prior_mean, sd = mu_prior_sd, log = TRUE) + 
    dnorm(sigma, mean = sigma_prior_mean, sd = sigma_prior_sd, log = TRUE)
}

# Metropolis-Hastings algorithm
# Metropolis-Hastings algorithm
metropolis_hastings_normal <- function(data, iterations, mu_init, sigma_init, proposal_sd_mu, proposal_sd_sigma, mu_prior_mean, mu_prior_sd, sigma_prior_mean, sigma_prior_sd) {
  mu <- numeric(iterations)
  sigma <- numeric(iterations)
  
  mu[1] <- mu_init
  sigma[1] <- sigma_init
  
  for (i in 2:iterations) {
    # Proposal step
    proposed_mu <- rnorm(1, mu[i-1], proposal_sd_mu)
    proposed_sigma <- rnorm(1, sigma[i-1], proposal_sd_sigma)
    
    # Check if proposed parameters are within reasonable bounds
    if (proposed_sigma <= 0) {
      mu[i] <- mu[i-1]
      sigma[i] <- sigma[i-1]
      next
    }
    
    # Compute likelihood and prior for proposed parameters
    likelihood_proposed <- sum(dnorm(data, mean = proposed_mu, sd = proposed_sigma, log = TRUE))
    prior_proposed <- sum(dnorm(proposed_mu, mean = mu_prior_mean, sd = mu_prior_sd, log = TRUE)) + 
      sum(dnorm(proposed_sigma, mean = sigma_prior_mean, sd = sigma_prior_sd, log = TRUE))
    
    # Compute likelihood and prior for current parameters
    likelihood_current <- sum(dnorm(data, mean = mu[i-1], sd = sigma[i-1], log = TRUE))
    prior_current <- sum(dnorm(mu[i-1], mean = mu_prior_mean, sd = mu_prior_sd, log = TRUE)) + 
      sum(dnorm(sigma[i-1], mean = sigma_prior_mean, sd = sigma_prior_sd, log = TRUE))
    
    # Compute acceptance probability
    acceptance_prob <- exp(likelihood_proposed + prior_proposed - likelihood_current - prior_current)
    
    # Accept or reject
    if (!is.finite(acceptance_prob) || runif(1) < acceptance_prob) {
      mu[i] <- proposed_mu
      sigma[i] <- proposed_sigma
    } else {
      mu[i] <- mu[i-1]
      sigma[i] <- sigma[i-1]
    }
  }
  
  return(data.frame(mu = mu, sigma = sigma))
}



# Set initial values
mu_init <- mean(data)
sigma_init <- sd(data)

# Set proposal standard deviations
proposal_sd_mu <- 0.1
proposal_sd_sigma <- 0.1

# Set prior parameters
mu_prior_mean <- 100
mu_prior_sd <- 1
sigma_prior_mean <- 5
sigma_prior_sd <- 1

# Number of iterations
iterations <- 10000

# Run Metropolis-Hastings algorithm
results <- metropolis_hastings_normal(data, iterations, mu_init, sigma_init, proposal_sd_mu, proposal_sd_sigma, mu_prior_mean, mu_prior_sd, sigma_prior_mean, sigma_prior_sd)

# Plot posterior distributions
par(mfrow=c(2,1))
hist(results$mu, main="Posterior Distribution of Mu", xlab="Mu", breaks=30)
hist(results$sigma, main="Posterior Distribution of Sigma", xlab="Sigma", breaks=30)

par(mfrow=c(2,1))
curve(dnorm(x, mean(results$mu), sd(results$mu)), from = min(results$mu), to = max(results$mu), n = 1000, main = "Posterior Distribution of Mu", xlab = "Mu", ylab = "Density", col = "blue", lwd = 2)
curve(dnorm(x, mean(results$sigma), sd(results$sigma)), from = min(results$sigma), to = max(results$sigma), n = 1000, main = "Posterior Distribution of Sigma", xlab = "Sigma", ylab = "Density", col = "blue", lwd = 2)

trace_plot <- function(samples, parameter_name) {
  plot(samples, type = "l", col = "blue", xlab = "Iteration", ylab = parameter_name, main = paste("Trace Plot of", parameter_name))
}

# Autocorrelation plot function
autocorrelation_plot <- function(samples, parameter_name) {
  acf_result <- acf(samples, main = paste("Autocorrelation Plot of", parameter_name))
}

par(mfrow=c(2,1))
trace_plot(results$mu, "Mu")
trace_plot(results$sigma, "Sigma")

# Plot autocorrelation plots
par(mfrow=c(2,1))
autocorrelation_plot(results$mu, "Mu")
autocorrelation_plot(results$sigma, "Sigma")


# Function to perform posterior predictive checks on new data
posterior_predictive_check <- function(new_data, mu_samples, sigma_samples) {
  predictions <- matrix(nrow = length(mu_samples), ncol = length(new_data))
  
  for (i in 1:length(mu_samples)) {
    for (j in 1:length(new_data)) {
      predictions[i, j] <- rnorm(1, mu_samples[i], sigma_samples[i])
    }
  }
  
  return(predictions)
}


# Extract sampled parameters from the Metropolis-Hastings results
mu_samples <- results$mu  # Assuming 'results' is your Metropolis-Hastings results object
sigma_samples <- results$sigma

# Perform posterior predictive check on new data
posterior_predictive_samples <- posterior_predictive_check(new_data, mu_samples, sigma_samples)

# Calculate how many variables fit in the posterior predictive model
fit_variables <- apply(posterior_predictive_samples, 2, function(x) sum(x >= min(data) & x <= max(data)))  # Assuming 'data' is your original dataset

# Print the count of variables fitting in the posterior predictive model
cat("Number of variables fitting in the posterior predictive model:", sum(fit_variables > 0), "\n")

posterior_predictive_samples <- unlist(results$posterior_predictive_samples)
density_values <- density(posterior_predictive_samples)
plot(density_values, type = "l", col = "blue", lwd = 2, main = "Posterior Predictive Distribution", xlab = "Value", ylab = "Density")

rm(list = ls())
data <- adm_data$CGPA
gibbs.normal <- function(y,mu0,tau0,a,b,Niter=10^3){
  
  n <- length(y)
  
  # preparing the vectors to store the sampled values
  mu.sample <- tau.sample <- numeric(Niter)
  
  # assigning starting values
  mu <- mean(y)
  tau <- 1/var(y)
  
  for(i in 1:Niter){
    
    mu <- mu.sample[i] <- rnorm(1,
                                (sum(y)*tau+mu0*tau0)/(n*tau+tau0),
                                1/sqrt(n*tau+tau0))
    
    tau <- tau.sample[i] <- rgamma(1,
                                   a+n/2, 
                                   b + .5*sum((y-mu.sample[i])^2))
  } 
  
  return(list(mu=mu.sample,tau=tau.sample))
}

set.seed(12345)
# for men:
sample.m <- gibbs.normal(data,
                         mu0=8,tau0=1,a=0.01,b=0.01,Niter=10^3)

# Plot posterior distributions
par(mfrow=c(2,1))
hist(sample.m$mu, main="Posterior Distribution of Mu", xlab="Mu", breaks=30)
hist(sample.m$tau, main="Posterior Distribution of Tau", xlab="Tau", breaks=30)

curve(dnorm(x, mean(sample.m$mu), sd(sample.m$mu)), from = min(sample.m$mu), to = max(sample.m$mu), n = 1000, main = "Posterior Distribution of Mu", xlab = "Mu", ylab = "Density", col = "blue", lwd = 2)
curve(dnorm(x, mean(sample.m$tau), sd(sample.m$tau)), from = min(sample.m$tau), to = max(sample.m$tau), n = 1000, main = "Posterior Distribution of Tau", xlab = "Tau", ylab = "Density", col = "blue", lwd = 2)

# Trace plot function
trace_plot <- function(samples, parameter_name) {
  plot(samples, type = "l", col = "blue", xlab = "Iteration", ylab = parameter_name, main = paste("Trace Plot of", parameter_name))
}


par(mfrow=c(2,1))
trace_plot(sample.m$mu, "Mu")
trace_plot(sample.m$tau, "tau")

rm(list = ls())
data <- adm_data$`GRE Score`
gibbs.normal <- function(y,mu0,tau0,a,b,Niter=10^3){
  
  n <- length(y)
  
  # preparing the vectors to store the sampled values
  mu.sample <- tau.sample <- numeric(Niter)
  
  # assigning starting values
  mu <- mean(y)
  tau <- 1/var(y)
  
  for(i in 1:Niter){
    
    mu <- mu.sample[i] <- rnorm(1,
                                (sum(y)*tau+mu0*tau0)/(n*tau+tau0),
                                1/sqrt(n*tau+tau0))
    
    tau <- tau.sample[i] <- rgamma(1,
                                   a+n/2, 
                                   b + .5*sum((y-mu.sample[i])^2))
  } 
  
  return(list(mu=mu.sample,tau=tau.sample))
}

set.seed(12345)
# for men:
sample.m <- gibbs.normal(data,
                         mu0=300,tau0=1,a=0.01,b=0.01,Niter=10^3)

# Plot posterior distributions
par(mfrow=c(2,1))
hist(sample.m$mu, main="Posterior Distribution of Mu", xlab="Mu", breaks=30)
hist(sample.m$tau, main="Posterior Distribution of Tau", xlab="Tau", breaks=30)

curve(dnorm(x, mean(sample.m$mu), sd(sample.m$mu)), from = min(sample.m$mu), to = max(sample.m$mu), n = 1000, main = "Posterior Distribution of Mu", xlab = "Mu", ylab = "Density", col = "blue", lwd = 2)
curve(dnorm(x, mean(sample.m$tau), sd(sample.m$tau)), from = min(sample.m$tau), to = max(sample.m$tau), n = 1000, main = "Posterior Distribution of Tau", xlab = "Tau", ylab = "Density", col = "blue", lwd = 2)

# Trace plot function
trace_plot <- function(samples, parameter_name) {
  plot(samples, type = "l", col = "blue", xlab = "Iteration", ylab = parameter_name, main = paste("Trace Plot of", parameter_name))
}


par(mfrow=c(2,1))
trace_plot(sample.m$mu, "Mu")
trace_plot(sample.m$tau, "tau")


rm(list = ls())
data <- adm_data$`TOEFL Score`
gibbs.normal <- function(y,mu0,tau0,a,b,Niter=10^3){
  
  n <- length(y)
  
  # preparing the vectors to store the sampled values
  mu.sample <- tau.sample <- numeric(Niter)
  
  # assigning starting values
  mu <- mean(y)
  tau <- 1/var(y)
  
  for(i in 1:Niter){
    
    mu <- mu.sample[i] <- rnorm(1,
                                (sum(y)*tau+mu0*tau0)/(n*tau+tau0),
                                1/sqrt(n*tau+tau0))
    
    tau <- tau.sample[i] <- rgamma(1,
                                   a+n/2, 
                                   b + .5*sum((y-mu.sample[i])^2))
  } 
  
  return(list(mu=mu.sample,tau=tau.sample))
}

set.seed(12345)
# for men:
sample.m <- gibbs.normal(data,
                         mu0=100,tau0=1,a=0.01,b=0.01,Niter=10^3)

# Plot posterior distributions
par(mfrow=c(2,1))
hist(sample.m$mu, main="Posterior Distribution of Mu", xlab="Mu", breaks=30)
hist(sample.m$tau, main="Posterior Distribution of Tau", xlab="Tau", breaks=30)

curve(dnorm(x, mean(sample.m$mu), sd(sample.m$mu)), from = min(sample.m$mu), to = max(sample.m$mu), n = 1000, main = "Posterior Distribution of Mu", xlab = "Mu", ylab = "Density", col = "blue", lwd = 2)
curve(dnorm(x, mean(sample.m$tau), sd(sample.m$tau)), from = min(sample.m$tau), to = max(sample.m$tau), n = 1000, main = "Posterior Distribution of Tau", xlab = "Tau", ylab = "Density", col = "blue", lwd = 2)

# Trace plot function
trace_plot <- function(samples, parameter_name) {
  plot(samples, type = "l", col = "blue", xlab = "Iteration", ylab = parameter_name, main = paste("Trace Plot of", parameter_name))
}


par(mfrow=c(2,1))
trace_plot(sample.m$mu, "Mu")
trace_plot(sample.m$tau, "tau")
