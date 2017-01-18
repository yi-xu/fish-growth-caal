create.pin.file <-
function(filename,pinlist)
{
  # This function is used to create a .pin file to be used in the CAAL
  # .pin file includes the initial values of all the estimated parameters
  # .pin file need to be created before doing any estimation
  # created by Yi Xu (NOAA-Southwest Fisheries Science Center)
  # Contact email: yi.xu@noaa.gov
  file.name<-paste(filename,".pin",sep="")
  
  write("#p1", file =file.name, append = F) 
  write(pinlist$p1.init, file =file.name, append = T) 
  write("#p2", file =file.name, append = T) 
  write(pinlist$p2.init, file =file.name, append = T) 
  write("#p3", file =file.name, append = T) 
  write(pinlist$p3.init, file =file.name, append = T) 
  write("#p4 dummy if not Richards model", file =file.name, append = T) 
  write(pinlist$p4.init, file =file.name, append = T) 
  if(pinlist$varopt==1) {
    write("#cv", file =file.name, append = T) 
    write(pinlist$cv.init, file =file.name, append = T) }
  if(pinlist$varopt==2) {
    write("#sd", file =file.name, append = T) 
    write(pinlist$sd.init, file =file.name, append = T) }
  if(pinlist$varopt==3) {
    write("#cv1", file =file.name, append = T) 
    write(pinlist$cv1.init, file =file.name, append = T) 
    write("#cv2", file =file.name, append = T) 
    write(pinlist$cv2.init, file =file.name, append = T) }
  
}
