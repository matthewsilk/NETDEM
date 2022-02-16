##This example script is designed to test the implementation of
##the indiv_info_gen() function. 
##Run it subsequent to example_script1.R

source("indiv_info_gen.R")

indiv_info<-data.frame(indiv_data$indivs)
indiv_info<-indiv_info_gen(indiv_info,ii_tag=NULL,indiv_data=indiv_data,trait="sex",trait_type="fac",level_names=c("M","F"))
indiv_info<-indiv_info_gen(indiv_info[[1]],ii_tag=indiv_info[[2]],indiv_data,trait="size",trait_type="cov",x_dist="norm")

new_info<-timestep_demographics(indiv_data=indiv_data,full_indiv_data=full_indiv_data,
                                mps=0.95,lvps=0.5)
indiv_data<-new_info[[1]]
                                

indiv_info<-indiv_info_add(indiv_info[[1]],ii_tag=indiv_info[[2]],indiv_data=indiv_data)
