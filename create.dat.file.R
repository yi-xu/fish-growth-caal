create.dat.file <-
function(filename,datlist)
{
  # This function is used to create a .dat file to be used in the CAAL
  # .dat file need to be created before doing estimation
  # A data list "datlist" need to be created before use this function
  # see "test_with_function.R" file for all the data needed
  # created by Yi Xu (NOAA-Southwest Fisheries Science Center)
  # Contact email: yi.xu@noaa.gov
  
  file.name<-paste(filename,".dat",sep="")
  
  write("# growth model option 1=von bertlanffy 2=Gompertz 3=Inverse logistic 4=Richards ", file =file.name, append = F) 
  write(datlist$gmopt, file =file.name, append = T) 
  write("# growth parameters lower and upper bounds and estimate phase", file =file.name, append = T) 
  write(c(datlist$p1_lower,datlist$p1_upper,datlist$p1_phase), file =file.name, append = T) 
  write(c(datlist$p2_lower,datlist$p2_upper,datlist$p2_phase), file =file.name, append = T) 
  write(c(datlist$p3_lower,datlist$p3_upper,datlist$p3_phase), file =file.name, append = T) 
  write(c(datlist$p4_lower,datlist$p4_upper,datlist$p4_phase), file =file.name, append = T) 
  
  write("# estimate variance option1==estimate const cv 2==estimate const sd 3==cv1 cv2 at age1 age2", file =file.name, append = T) 
  write(datlist$varopt, file =file.name, append = T) 
  if(datlist$varopt==1) write(c(datlist$cv_lower,datlist$cv_upper,datlist$cv_phase),file=file.name,append = T)
  if(datlist$varopt==2) write(c(datlist$sd_lower,datlist$sd_upper,datlist$sd_phase),file=file.name,append = T)
  if(datlist$varopt==3) {
    write(c(datlist$cva1,datlist$cva2), file =file.name, append = T) 
    write(c(datlist$cv1_lower,datlist$cv1_upper,datlist$cv1_phase), file =file.name, append = T)
    write(c(datlist$cv2_lower,datlist$cv2_upper,datlist$cv2_phase), file =file.name, append = T)}
  
  write("# first length/last length/length increment (rows)", file =file.name, append = T) 
  write(c(datlist$first_length,datlist$last_length,datlist$increment_length), file =file.name, append = T) 
  write("# first age/last age/age increment (columns)", file =file.name, append = T) 
  write(c(datlist$first_age,datlist$last_age,datlist$increment_age), file =file.name, append = T) 
  write("# first year/last year/year increment (third dimension)", file =file.name, append = T)
  write(c(datlist$first_year,datlist$last_year,datlist$increment_year), file =file.name, append = T) 
  write("# numbers of fleet", file =file.name, append = T)
  write(datlist$nfleet, file =file.name, append = T) 
  write("# lines of age proportion", file =file.name, append = T)
  write(datlist$nage_prop, file =file.name, append = T)
  
  write("# age proportion by year and by fleet", file =file.name, append = T)
  write("# year fleet    age proportion from age 0 to age max", file =file.name, append = T)
  write.table(datlist$age_prop, file =file.name, append = T,col.names=F,row.names=F)
  
  write("# lines of caal", file =file.name, append = T)
  write(datlist$ncaal, file =file.name, append = T)
  
  write("# fish length data organized with rows corresponding to lengths and columns corresponding to ages", file =file.name, append = T)
  write("# Year  fleet  lengthbin age 0 to age max", file =file.name, append = T)
  write.table(datlist$caal_mat, file =file.name, append = T,col.names=F,row.names=F)
  
  write("999 # end of the file", file =file.name, append = T)
  
}
