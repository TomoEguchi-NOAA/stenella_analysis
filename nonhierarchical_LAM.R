# packages
library(tidyverse)
library(coda)
library(rjags)
library(jagsUI)

save.output <- T

source("loading_data.R")

# ATTENUATA starts here:

# totallength/maturity data for attenuata
LAM.data.ATT <- attenuata %>%
  select(TotalLength, isMature_bin) %>%
  drop_na()

# mean and sd of total length
mean.TotalLength.ATT <- mean(LAM.data.ATT$TotalLength)
sd.TotalLength.ATT <- sd(LAM.data.ATT$TotalLength)

# preparing data for jags by naming constants and vectors
jags.data.LAM.ATT <- list(
  N = nrow(LAM.data.ATT),
  X = as.vector(LAM.data.ATT$TotalLength),
  y = LAM.data.ATT$isMature_bin
)

# MCMC
MCMC.LAM.ATT <- list(
  n.samples = 150000,
  n.thin = 100, 
  n.burnin = 100000,
  n.chains = 5
)

jags.params.LAM.ATT <- c("alpha", "beta")

# non-hierarchical Bayesian LR
NH.LAM.ATT <- jagsUI::jags(
  data = jags.data.LAM.ATT,
  inits = NULL,
  parameters.to.save = jags.params.LAM.ATT,
  model.file = "models/model_logistic_regression_1sp.jags",
  n.chains = MCMC.LAM.ATT$n.chains,
  n.burnin = MCMC.LAM.ATT$n.burnin,
  n.thin = MCMC.LAM.ATT$n.thin,
  n.iter = MCMC.LAM.ATT$n.samples,
  DIC = TRUE,
  parallel = TRUE
)

if (save.output)
  saveRDS(NH.LAM.ATT,
          file = "RData/NH.LAM.ATT.rds")

# extract posterior samples
all.samples.LAM.ATT <- as.matrix(NH.LAM.ATT$samples)

alpha.samples.LAM.ATT <- all.samples.LAM.ATT[, "alpha"]
beta.samples.LAM.ATT <- all.samples.LAM.ATT[, "beta"]

# age at which an individual has a 50% probability of being mature
p.50.LAM.ATT <- (-alpha.samples.LAM.ATT / beta.samples.LAM.ATT)

# summarize posterior estimates
summary(p.50.LAM.ATT)

quantile(
  p.50.LAM.ATT,
  probs = c(0.025, 0.5, 0.975)
)



# COERULEOALBA starts here:

# totallength/maturity data for coeruleoalba
LAM.data.COE <- coeruleoalba %>%
  select(TotalLength, isMature_bin) %>%
  drop_na()

# mean and sd of total length
mean.TotalLength.COE <- mean(LAM.data.COE$TotalLength)
sd.TotalLength.COE <- sd(LAM.data.COE$TotalLength)

# preparing data for jags by naming constants and vectors
jags.data.LAM.COE <- list(
  N = nrow(LAM.data.COE),
  X = as.vector(LAM.data.COE$TotalLength),
  y = LAM.data.COE$isMature_bin
)

# MCMC
MCMC.LAM.COE <- list(
  n.samples = 150000,
  n.thin = 100, 
  n.burnin = 100000,
  n.chains = 5
)

jags.params.LAM.COE <- c("alpha", "beta")

# non-hierarchical Bayesian LR
NH.LAM.COE <- jagsUI::jags(
  data = jags.data.LAM.COE,
  inits = NULL,
  parameters.to.save = jags.params.LAM.COE,
  model.file = "models/model_logistic_regression_1sp.jags",
  n.chains = MCMC.LAM.COE$n.chains,
  n.burnin = MCMC.LAM.COE$n.burnin,
  n.thin = MCMC.LAM.COE$n.thin,
  n.iter = MCMC.LAM.COE$n.samples,
  DIC = TRUE,
  parallel = TRUE
)

if (save.output)
  saveRDS(NH.LAM.COE,
          file = "RData/NH.LAM.COE.rds")

# extract posterior samples
all.samples.LAM.COE <- as.matrix(NH.LAM.COE$samples)

alpha.samples.LAM.COE <- all.samples.LAM.COE[, "alpha"]
beta.samples.LAM.COE <- all.samples.LAM.COE[, "beta"]

# age at which an individual has a 50% probability of being mature
p.50.LAM.COE <- (-alpha.samples.LAM.COE / beta.samples.LAM.COE)

# summarize posterior estimates
summary(p.50.LAM.COE)

quantile(
  p.50.LAM.COE,
  probs = c(0.025, 0.5, 0.975)
)


# longirostris starts here:

# totallength/maturity data for longirostris
LAM.data.LON <- longirostris %>%
  select(TotalLength, isMature_bin) %>%
  drop_na()

# mean and sd of total length
mean.TotalLength.LON <- mean(LAM.data.LON$TotalLength)
sd.TotalLength.LON <- sd(LAM.data.LON$TotalLength)

# preparing data for jags by naming constants and vectors
jags.data.LAM.LON <- list(
  N = nrow(LAM.data.LON),
  X = as.vector(LAM.data.LON$TotalLength),
  y = LAM.data.LON$isMature_bin
)

# MCMC
MCMC.LAM.LON <- list(
  n.samples = 150000,
  n.thin = 100, 
  n.burnin = 100000,
  n.chains = 5
)

jags.params.LAM.LON <- c("alpha", "beta")

# non-hierarchical Bayesian LR
NH.LAM.LON <- jagsUI::jags(
  data = jags.data.LAM.LON,
  inits = NULL,
  parameters.to.save = jags.params.LAM.LON,
  model.file = "models/model_logistic_regression_1sp.jags",
  n.chains = MCMC.LAM.LON$n.chains,
  n.burnin = MCMC.LAM.LON$n.burnin,
  n.thin = MCMC.LAM.LON$n.thin,
  n.iter = MCMC.LAM.LON$n.samples,
  DIC = TRUE,
  parallel = TRUE
)

if (save.output)
  saveRDS(NH.LAM.LON,
          file = "RData/NH.LAM.LON.rds")

# extract posterior samples
all.samples.LAM.LON <- as.matrix(NH.LAM.LON$samples)

alpha.samples.LAM.LON <- all.samples.LAM.LON[, "alpha"]
beta.samples.LAM.LON <- all.samples.LAM.LON[, "beta"]

# age at which an individual has a 50% probability of being mature
p.50.LAM.LON <- (-alpha.samples.LAM.LON / beta.samples.LAM.LON)

# summarize posterior estimates
summary(p.50.LAM.LON)

quantile(
  p.50.LAM.LON,
  probs = c(0.025, 0.5, 0.975)
)

