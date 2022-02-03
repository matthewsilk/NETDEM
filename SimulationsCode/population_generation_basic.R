
##Function for basic population generation
#Integrates information on number of individuals, groups and group locations

# n = number of individuals in the population
# ng = number of groups in the population

# Groups are distributed uniformly across a 2d space 
# (bounded by 0 and 1 on x and y axes)

population_generation_basic<-function(n,ng){
  #Number of individuals in the population
  n<-100
  
  #Individual codes
  indivs<-seq(1,n,1)
  
  #Number of groups
  ng<-100
  
  #Sort individuals into groups
  groups<-vector()
  if(n==ng){
    groups<-indivs
  }
  if(n>ng){
    print("invalid number of groups")
  }
  if(n<ng){
    groups<-sample(seq(1,ng,1),n,replace=TRUE)
  }
  
  #Locations
  x<-runif(ng,0,1)
  y<-runif(ng,0,1)
  
  #Check locations
  plot(x,y)
  
  #Create dataframe for individuals
  indiv_data<-data.frame(indivs,groups,x,y)
  
  #Create distance matrix between locations
  dist_mat<-as.matrix(dist(cbind(indiv_data$x,indiv_data$y),upper=TRUE))
  
  output<-list(indiv_data,dist_mat)
  
  return(output)
  
}

