#Read in the arguments listed at the command line
args<-(commandArgs(TRUE))
i<-args

pars1<-readRDS("parameters1.RDS")

#load R packages
library(genNetDem)
library(igraph)
library(asnipe)
library(sna)
library(tnet)
library(car)
library(boot)
library(Matrix)
library(MASS)
library(MCMCvis)
library(nimble)

##############################
##############################

source("SimulationFunction.R")

##############################
##############################

path <- Sys.getenv('PATH')
newPath <- paste("C:\\rtools40\\bin;C:\\rtools40\\mingw_64\\bin;C:\\rtools40\\usr\\bin;",
                 path, sep = "")
Sys.setenv(PATH = newPath)


nim_mod <- nimbleCode({
  beta[1] ~ dnorm(mean=0,sd=10) #prior female
  beta[2] ~ dnorm(mean=0,sd=10) #prior male
  beta[3] ~ dnorm(mean=0,sd=10) #prior betweenness effect
  p ~ dunif(0, 1) # prior detection

  #prior for missing betweenness values
  for(i in 1:nrmc){
    clo2[miss_clo2[i,1],miss_clo2[i,2]] ~ dnorm(mean=me_t[miss_clo2[i,1]],sd=sd_t[miss_clo2[i,1]])
  }

  # likelihood
  omega[1,1] <- 1 - p    # Pr(alive t -> non-detected t)
  omega[1,2] <- p        # Pr(alive t -> detected t)
  omega[2,1] <- 1        # Pr(dead t -> non-detected t)
  omega[2,2] <- 0        # Pr(dead t -> detected t)
  for (i in 1:N){
    for(j in 1:(T-1)){
      logit(phi[i,j]) <- beta[sex[i]]+beta[3]*clo2[i,j]
      gamma[1,1,i,j] <- phi[i,j]      # Pr(alive t -> alive t+1)
      gamma[1,2,i,j] <- 1 - phi[i,j]  # Pr(alive t -> dead t+1)
      gamma[2,1,i,j] <- 0           # Pr(dead t -> alive t+1)
      gamma[2,2,i,j] <- 1           # Pr(dead t -> dead t+1)
    }
  }
  delta[1] <- 1          # Pr(alive t = 1) = 1
  delta[2] <- 0          # Pr(dead t = 1) = 0
  for (i in 1:N){
    z[i,1] ~ dcat(delta[1:2])
    for (j in 2:T){
      z[i,j] ~ dcat(gamma[z[i,j-1], 1:2, i,j-1])
      y[i,j] ~ dcat(omega[z[i,j], 1:2])
    }
  }
})

##############################
##############################

nim_mod2 <- nimbleCode({
  beta[1] ~ dnorm(mean=0,sd=10) #prior female
  beta[2] ~ dnorm(mean=0,sd=10) #prior male
  beta[3] ~ dnorm(mean=0,sd=10) #prior betweenness effect
  p ~ dunif(0, 1) # prior detection

  #prior for missing betweenness values
  for(i in 1:nrmc){
    clo2[miss_clo2[i,1],miss_clo2[i,2]] ~ dnorm(mean=me_t[miss_clo2[i,2]],sd=sd_t[miss_clo2[i,2]])
  }

  # likelihood
  omega[1,1] <- 1 - p    # Pr(alive t -> non-detected t)
  omega[1,2] <- p        # Pr(alive t -> detected t)
  omega[2,1] <- 1        # Pr(dead t -> non-detected t)
  omega[2,2] <- 0        # Pr(dead t -> detected t)
  for (i in 1:N){
    for(j in 1:(T-1)){
      logit(phi[i,j]) <- beta[sex[i]]+beta[3]*clo2[i,j]
      gamma[1,1,i,j] <- phi[i,j]      # Pr(alive t -> alive t+1)
      gamma[1,2,i,j] <- 1 - phi[i,j]  # Pr(alive t -> dead t+1)
      gamma[2,1,i,j] <- 0           # Pr(dead t -> alive t+1)
      gamma[2,2,i,j] <- 1           # Pr(dead t -> dead t+1)
    }
  }
  delta[1] <- 1          # Pr(alive t = 1) = 1
  delta[2] <- 0          # Pr(dead t = 1) = 0
  for (i in 1:N){
    z[i,1] ~ dcat(delta[1:2])
    for (j in 2:T){
      z[i,j] ~ dcat(gamma[z[i,j-1], 1:2, i,j-1])
      y[i,j] ~ dcat(omega[z[i,j], 1:2])
    }
  }
})

##############################
##############################

#SAMP_RES_L<-FULL_RES_L<-SAMP_RES_C<-FULL_RES_C<-array(NA,dim=c(4,7,nrow(pars1)))
SAMP_RES_L<-FULL_RES_L<-SAMP_RES_C<-FULL_RES_C<-matrix(NA,nr=500,nc=4)

##############################
##############################


p_wr_i<-pars1[i,1]
pcg<-pars1[i,2]
p_wr_i<-pars1[i,1]
net_vars<-as.character(pars1[i,3])
ifelse(net_vars=="strength",net_vars2<-"strength",net_vars2<-"betweenness")
ifelse(net_vars=="strength",net_packages<-"igraph",net_packages<-"tnet")
net_effs<-pars1[i,4]
max_cor<-pars1[i,5]
reps<-pars1[i,6]

results<-SimRun(p_wr_i=p_wr_i,
                  pcg=pcg,
                  net_vars=net_vars,
                  net_vars2=net_vars2,
                  net_packages=net_packages,
                  net_effs=net_effs,
                  max_cor=max_cor,
                  reps=reps,
                  nim_mod=nim_mod,
                  nim_mod2=nim_mod2)

eval(parse(text=paste0("saveRDS(results,'results/res",i,".RDS')")))
