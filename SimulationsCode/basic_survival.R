#'basic_survival
#'
#'Generates survival probabilities for all individuals in the population based on draws from a Gaussian distribution on a logit scale.
#'
#'@param indiv_data current indiv_data dataframe
#'@param mps Mean survival probability per demographic timestep (between 0 and 1)
#'@param lvps Variance in survival probability on a logit scale
#'@details Generates survival probabilities for all individuals in the population based on draws from a Gaussian distribution on a logit scale.
#'@return The new indiv_data dataframe with newly calculated survival probabilities added
#'@export

basic_survival<-function(indiv_data,mps,lvps){
  lmps<-car::logit(mps,adjust=0.001)
  indiv_data$survival<-inv.logit(rnorm(nrow(indiv_data),lmps,lvps))
  return(indiv_data)
}