
create.age.prop<-function(age_prop_opt,age_prop_opt_list)
{ # This function is used to generate age_prop table
switch(age_prop_opt,
       "1"={ # option1: use natural mortality z to calculate the age proportion
         N0<-10^10 # arbitary number
         age_num<-N0*(exp(-age_prop_opt_list$z*age_prop_opt_list$age_bins))
         age_prop_temp<-age_num/sum(age_num)
         nfleet<-length(age_prop_opt_list$fleet_seq)
         year_temp<-rep(age_prop_opt_list$year_bins,nfleet)
         fleet_temp<-rep(age_prop_opt_list$fleet_seq,each=length(age_prop_opt_list$year_bins))
         age_prop_mat<-matrix(rep(age_prop_temp,each=length(year_temp)), nrow = length(year_temp))
         age_prop<-cbind(year_temp,fleet_temp,age_prop_mat)
         colnames(age_prop)<-c("year","fleet",paste("age",age_prop_opt_list$age_bins,sep=""))
         # write.table(age_prop,file="age_prop_opt1.csv",sep=",",row.names=F)
         # or age_prop<-read.my.csv("age_prop_opt1.csv",header=F,sep=",")
       },
       "2"={ #option2: read from excel sheet
         # generate an example of "age_prop_opt2.csv" from opt1, add randomness
         # age_prop<-read.my.csv("age_prop_opt1.csv")
         # rnum<-matrix(runif(nrow(age_prop)*ncol(age_prop),-0.01,0.01), nrow(age_prop), ncol(age_prop))
         # age_prop_new<-age_prop*(rnum+1)
         # age_prop_new[,1]<-age_prop[,1]
         # age_prop_new[,2]<-age_prop[,2]
         # write.table(age_prop_new,file="age_prop_opt2.csv",sep=",",row.names=F)
         # age_prop<-read.my.csv("age_prop_opt2.csv") })
         age_prop<-read.my.csv(age_prop_opt_list$filename) } )#end switch
         return(age_prop)

} #end function
