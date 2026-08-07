# packages
library(tidyverse)
library(coda)
library(rjags)
library(jagsUI)

save.output <- T

source("loading_data.R")

# ATTENUATA starts here:

# age/maturity data for attenuata
AM.data.ATT <- attenuata %>%
  select(Age, isMature_bin) %>%
  drop_na()

# mean and sd of age
mean.age.ATT <- mean(AM.data.ATT$Age)
sd.age.ATT <- sd(AM.data.ATT$Age)

# preparing data for jags by naming constants and vectors
jags.data.AM.ATT <- list(
  N = nrow(AM.data.ATT),
  X = as.vector(AM.data.ATT$Age),
  y = AM.data.ATT$isMature_bin
)

# MCMC
MCMC.AM.ATT <- list(
  n.samples = 150000,
  n.thin = 100, 
  n.burnin = 100000,
  n.chains = 5
)

jags.params.AM.ATT <- c("alpha", "beta")

# non-hierarchical Bayesian LR
NH.AM.ATT <- jagsUI::jags(
  data = jags.data.AM.ATT,
  inits = NULL,
  parameters.to.save = jags.params.AM.ATT,
  model.file = "models/model_logistic_regression_1sp.jags",
  n.chains = MCMC.AM.ATT$n.chains,
  n.burnin = MCMC.AM.ATT$n.burnin,
  n.thin = MCMC.AM.ATT$n.thin,
  n.iter = MCMC.AM.ATT$n.samples,
  DIC = TRUE,
  parallel = TRUE
)

if (save.output)
  saveRDS(NH.AM.ATT,
          file = "RData/NH.AM.ATT.rds")

# extract posterior samples
all.samples.ATT <- as.matrix(NH.AM.ATT$samples)

alpha.samples.ATT <- all.samples.ATT[, "alpha"]
beta.samples.ATT <- all.samples.ATT[, "beta"]

# age at which an individual has a 50% probability of being mature
p.50.AM.ATT <- (-alpha.samples.ATT / beta.samples.ATT)

# summarize posterior estimates
summary(p.50.AM.ATT)

quantile(
  p.50.AM.ATT,
  probs = c(0.025, 0.5, 0.975)
)



# COERULEOALBA starts here:

# age/maturity data for coeruleoalba
AM.data.COE <- coeruleoalba %>%
  select(Age, isMature_bin) %>%
  drop_na()

# mean and sd of age
mean.age.COE <- mean(AM.data.COE$Age)
sd.age.COE <- sd(AM.data.COE$Age)

# preparing data for jags by naming constants and vectors
jags.data.AM.COE <- list(
  N = nrow(AM.data.COE),
  X = as.vector(AM.data.COE$Age),
  y = AM.data.COE$isMature_bin
)

# MCMC
MCMC.AM.COE <- list(
  n.samples = 150000,
  n.thin = 100, 
  n.burnin = 100000,
  n.chains = 5
)

jags.params.AM.COE <- c("alpha", "beta")

# non-hierarchical Bayesian LR
NH.AM.COE <- jagsUI::jags(
  data = jags.data.AM.COE,
  inits = NULL,
  parameters.to.save = jags.params.AM.COE,
  model.file = "models/model_logistic_regression_1sp.jags",
  n.chains = MCMC.AM.COE$n.chains,
  n.burnin = MCMC.AM.COE$n.burnin,
  n.thin = MCMC.AM.COE$n.thin,
  n.iter = MCMC.AM.COE$n.samples,
  DIC = TRUE,
  parallel = TRUE
)

if (save.output)
  saveRDS(NH.AM.COE,
          file = "RData/NH.AM.COE")

# extract posterior samples
all.samples.COE <- as.matrix(NH.AM.COE$samples)

alpha.samples.COE <- all.samples.COE[, "alpha"]
beta.samples.COE <- all.samples.COE[, "beta"]

# age at which an individual has a 50% probability of being mature
p.50.AM.COE <- (-alpha.samples.COE / beta.samples.COE)

# summarize posterior estimates
summary(p.50.AM.COE)

quantile(
  p.50.AM.COE,
  probs = c(0.025, 0.5, 0.975)
)


# LONGIROSTRIS starts here

# age/maturity data for longirostris
AM.data.LON <- longirostris %>%
  select(Age, isMature_bin) %>%
  drop_na()

# mean and sd of age
mean.age.LON <- mean(AM.data.LON$Age)
sd.age.LON <- sd(AM.data.LON$Age)

# preparing data for jags by naming constants and vectors
jags.data.AM.LON <- list(
  N = nrow(AM.data.LON),
  X = as.vector(AM.data.LON$Age),
  y = AM.data.LON$isMature_bin
)

# MCMC
MCMC.AM.LON <- list(
  n.samples = 150000,
  n.thin = 100, 
  n.burnin = 100000,
  n.chains = 5
)

jags.params.AM.LON <- c("alpha", "beta")

# non-hierarchical Bayesian LR
NH.AM.LON <- jagsUI::jags(
  data = jags.data.AM.LON,
  inits = NULL,
  parameters.to.save = jags.params.AM.LON,
  model.file = "models/model_logistic_regression_1sp.jags",
  n.chains = MCMC.AM.LON$n.chains,
  n.burnin = MCMC.AM.LON$n.burnin,
  n.thin = MCMC.AM.LON$n.thin,
  n.iter = MCMC.AM.LON$n.samples,
  DIC = TRUE,
  parallel = TRUE
)

if (save.output)
  saveRDS(NH.AM.LON,
          file = "RData/NH.AM.LON")

# extract posterior samples
all.samples.LON <- as.matrix(NH.AM.LON$samples)

alpha.samples.LON <- all.samples.LON[, "alpha"]
beta.samples.LON <- all.samples.LON[, "beta"]

# age at which an individual has a 50% probability of being mature
p.50.AM.LON <- (-alpha.samples.LON / beta.samples.LON)

# summarize posterior estimates
summary(p.50.AM.LON)

quantile(
  p.50.AM.LON,
  probs = c(0.025, 0.5, 0.975)
)