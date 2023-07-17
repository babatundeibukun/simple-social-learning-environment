packages <- c('tidyverse', "vembedr", 'formatR')
#invisible(lapply(packages, install.packages, character.only = TRUE)) #Install packages if any are missing
invisible(lapply(packages, require, character.only = TRUE)) #Load all packages
knitr::opts_chunk$set(warning = FALSE, message = FALSE) 
set.seed(1234) #set seed for reproducibility

#generative model of the bandit task 
K = 2 #number of options
meanVec <- runif(n=K, min = -10, max = 10) #Payoff means, which we sample from uniform distribution between -10 to 10
sigmaVec <- rep(1, K) #Variability of payoffs (as a standard deviation), which we assume is the same for all options. 

banditGenerator <- function(k) {#k is an integer or k vector of integers, selecting one of the 1:K arms of the bandit
  payoff <- rnorm(n = length(k), mean = meanVec[k], sd = sigmaVec[k])
  return (payoff)
}

actionSequence <- sample(1:K, size = 25, replace = TRUE) # select 25 random actions
payoffs <- banditGenerator(actionSequence)

df <- data.frame(action = actionSequence, payoff = payoffs)
knitr::kable(df)


source('codeSnippets/geom_flat_violin.R') 
# Use samples to approximate the reward distribution
sampleDF <- data.frame()
nsamples = 10000 #Samples used to approximate the normal distribution
plotsamples = 25 #samples to plot as dots

rewardSamples <- c(sapply(1:K, FUN=function(k) banditGenerator(rep(k, nsamples))))#use an apply function to simulate multiple samples of each arm
sampleDF <- data.frame(option = rep(1:K, each = nsamples), payoff=rewardSamples, nsample = rep(1:nsamples, K)) #create a dataframe for plotting

ggplot(sampleDF, aes(x = factor(option), y = payoff, fill = factor(option)))+
  geom_flat_violin(position = position_nudge(x = 0.3, y = 0), alpha = .5) +
  geom_jitter(data = subset(sampleDF, nsample<=plotsamples), aes(y = payoff, color = factor(option)),width = 0.2 )+  #Plot only a subset of points show how well a limited number of samples approximates the true distribution
  ylab('Payoff')+
  xlab('Option')+
  theme_classic() +
  scale_fill_viridis_d()+
  scale_color_viridis_d()+
  ggtitle('Payoff conditions')+
  theme(legend.position ='none')