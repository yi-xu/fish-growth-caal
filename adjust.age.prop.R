adjust.age.prop <-
function(age_select_opt,age_prop,age_select_filename=NULL)
{ # This function is used to adjust age_prop if age selectivity is provided(option2) 
  
  age_select<-age_prop*0+1 #default table is a table of all 1
  
  if(age_select_opt=="2")
  { # generate an example of linear age selectivity table "age_select.csv"
    # for (i in 1:10)  age_select[,(i+2)]<-0.1*(i-1)  # generate linear selectivity table (e.g.)
    # write.table(age_select,file="age_select.csv",row.names=F,sep=",") # write age selectivity table
    # example: age_select<-read.my.csv("age_select.csv") # read age selectivity table
    age_select<-read.my.csv(age_select_filename) # read age selectivity table
  }
  age_prop_adjust<-age_prop*age_select
  return(age_prop_adjust)
}
