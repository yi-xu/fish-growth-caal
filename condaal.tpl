//><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>//
// This product is developed by Yi Xu (yi.xu@noaa.gov)     		 //
// NOAA - Southwest Fisheries Science Center		    		 //
// Date:02/17/2016											     //
// Purpose:Fit VB growth model using Conditional age at length 	 //		
//><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>//

DATA_SECTION
	
	init_int gmopt //growth model option 1==Traditional VB 2==Gompertz 3==Inverse logistic 4==Richards 
	
    init_number p1_lower //linf_lower
	init_number p1_upper //linf_upper
	init_number p2_lower //vbk_lower  
	init_number p2_upper //vbk_upper
	init_number p3_lower //t0_lower
	init_number p3_upper //t0_upper	
	init_number p4_lower //p_lower for Richards only, dummy otherwise
	init_number p4_upper //p_upper for Richards only, dummy otherwise
 
	init_int varopt //variance option 1==estimate constant cv 2==estimate constant sd 3==linear interpolation between age1 & age2
	!!if (varopt==3) {
	init_number cva1
	init_number cva2
	!!}
	init_number first_length //first length 
	init_number last_length //last length
	init_number increment_length //length increment /if==1 1cm/==2 every 2cm/ ==0.5 every half cm/
	init_number first_age
	init_number last_age
	init_number increment_age // age increment /if==1 annual/==2 every 2 years/==0.5 every 6months/==0.25 every 3months
	init_number first_year
	init_number last_year
	init_number increment_year // year increment /same as age
	init_number nfleet  //number of fleets
	
	number nlength 
	!!nlength=int((last_length-first_length)/increment_length)+1;
	number nage
	!!nage=int((last_age-first_age)/increment_age)+1;
	number nyear
	!!nyear=int((last_year-first_year)/increment_year)+1;

	!!cout<< "nlength " << nlength << endl;	
	!!cout<< "nage " << nage << endl;
	!!cout<< "nyear " << nyear << endl;	
    !!cout<< "nfleet " << nfleet << endl;			
	vector lenbin(1,nlength)
	vector agebin(1,nage)
	vector yearbin(1,nyear)
	!!for(int i=1;i<=nlength;i++) lenbin(i)=floor((first_length+(i-1)*increment_length)*100+0.5)/100; //define length bins round to 2 decimal: floor(num*100+0.5)/100;
	!!cout<< "lenbin" << lenbin << endl;	
	!!for(int j=1;j<=nage;j++) agebin(j)=floor((first_age+(j-1)*increment_age)*100+0.5)/100; //define age bins 
	!!cout<< "agebin " << agebin << endl;
    !!for(int k=1;k<=nyear;k++) yearbin(k)=floor((first_year+(k-1)*increment_year)*100+0.5)/100; //define year bins 
	!!cout<< "yearbin " << yearbin << endl;	
	init_matrix ageprop_temp(1,nyear*nfleet,1,nage+2) //temporary age proportion, first column is not used

	//!!cout<< "ageprop_temp " << ageprop_temp << endl;	
	init_3darray condaal_temp(1,nyear*nfleet,1,nlength,1,nage+3) //temporary 3d pop matrix,3 additional lines for year/fleet/lengthbin
	//!!cout<< "condaal_temp " << condaal_temp << endl;	
	init_number endfileflag
	!!cout<<"If this number is 999, reach the end of the .dat file!! "<<endfileflag<<endl;
	int i
	int j
	int k
	int m
	int mk_temp
	int k_temp
	int m_temp
	int i_temp

		
	vector poplenbin(1,nlength+1)		
	//!!poplenbin.fill_seqadd(first_length,1);

	!!for (int i=1;i<=nlength;i++) poplenbin(i)=lenbin(i);
	!!poplenbin(nlength+1)=lenbin(nlength)+increment_length; //assign last length bin based on flex length binning
	3darray ageprop(1,nage,1,nyear,1,nfleet) // change orders!!!
	4darray condaal(1,nlength,1,nage,1,nyear,1,nfleet)
	//matrix obsmeanlen(1,nlength,1,nage)
	
	!! for (int m=1;m<=nfleet;m++) //reformat the age proportion matrix
	!!	{     for(int k=1;k<=nyear;k++)
	!!			{	mk_temp=(m-1)*nyear+k;
	!!				k_temp=int((ageprop_temp(mk_temp,1)-first_year)/increment_year+0.5)+1; //corresponding year
	!!			    m_temp=ageprop_temp(mk_temp,2); //corresponding fleet
	//!!cout<< "testxx" << mk_temp<<endl;
	//!!cout<<ageprop_temp(mk_temp,1)<<"kyear"<<k_temp<<endl;
	//!!cout<<ageprop_temp(mk_temp,2)<<"mfleet"<<m_temp<<endl;
	!!				for(int j=1;j<=nage;j++)
	!!			        { ageprop(j,k_temp,m_temp)=ageprop_temp(mk_temp,j+2);}}}
    //!!cout<< "testxx2" << ageprop <<endl;
	
	!!for (int m=1;m<=nfleet;m++) //reformat the condaal matrix
	!!    {    for(int k=1;k<=nyear;k++) 
	!!       	{	mk_temp=(m-1)*nyear+k; 
	!!	            for (int i=1;i<=nlength;i++) 
	!!					{ k_temp=int((condaal_temp(mk_temp,i,1)-first_year)/increment_year+0.5)+1; //nth year
	!!					  m_temp=condaal_temp(mk_temp,i,2);	//fleet number
	!!					  i_temp=int((condaal_temp(mk_temp,i,3)-first_length)/increment_length+0.5)+1; //lengthbin
	//						!!cout<< "testxx" << mk_temp<<k_temp<<m_temp<<i_temp<<endl;
	!!					  for(int j=1;j<=nage;j++)  
	!!                    { condaal(i_temp,j,k_temp,m_temp)=condaal_temp(mk_temp,i_temp,j+3); }}}}
	//!!cout<< "the test conAatL matrix is " << conaal << endl;		
	!!cout<< "gmopt " << gmopt << endl;
	!!cout<< "p1_lower " << p1_lower << endl;
	!!cout<< "p1_upper " << p1_upper << endl;
	!!cout<< "p2_lower " << p2_lower << endl;
	!!cout<< "p2_upper " << p2_upper << endl;
	!!cout<< "p3_lower " << p3_lower << endl;
	!!cout<< "p3_upper " << p3_upper << endl;
	!!cout<< "first_length " << first_length << endl;
	!!cout<< "last_length " << last_length << endl;
	!!cout<< "nlength " << nlength << endl;	
	!!cout<< "first_age " << first_age << endl;
	!!cout<< "last_age " << last_age << endl;
	!!cout<< "nage " << nage << endl;
    !!cout<< "first_year " << first_year << endl;
	!!cout<< "last_year " << last_year << endl;
	!!cout<< "nyear " << nyear << endl;	
	
    !! cout << "Done Data Section" << endl;
	
	matrix covar_mat(1,4,1,5);
    !!set_covariance_matrix(covar_mat);
    
PARAMETER_SECTION
	
	init_bounded_number p1(p1_lower,p1_upper,2);	//linf if gmopt=1
	init_bounded_number p2(p2_lower,p2_upper,1);	//K if gmopt=1
	init_bounded_number p3(p3_lower,p3_upper,3);	//t0 if gmopt=1
	init_bounded_number p4(p4_lower,p4_upper,gmopt*100-400+3);    //dummy if gmopt=1,2,3 and estimate at phase 3 when Richards
	
	!! if (varopt<=2) { //varopt==1 this is cv; varopt==2 this is sd
	init_number var(3); 
	!! }
	!! else { //varopt==3 cv1 and cv2 are estimated 
	init_number cv1(3);
	init_number cv2(3);
	!! }
	vector meanlen(1,nage);
	vector sigma(1,nage);
	vector cv(1,nage);
	matrix cdf1(1,nlength,1,nage);
	matrix cdf2(1,nlength,1,nage);
	matrix pdf(1,nlength,1,nage);
	4darray popProp(1,nlength,1,nage,1,nyear,1,nfleet);
	3darray sumpop(1,nlength,1,nyear,1,nfleet);
	matrix sumpopmat(1,nlength,1,nage);
	4darray RescalepopProp(1,nlength,1,nage,1,nyear,1,nfleet);
	4darray logRescalepopProp(1,nlength,1,nage,1,nyear,1,nfleet);
	3darray p_hat(1,nyear,1,nlength,1,nage+3)	//this variable is for report only
	number likesum;
	objective_function_value nll;//must define the objective function	
     !! cout << "Done Parameters Section" << endl;
PROCEDURE_SECTION	
	calculation();	
	calobjectivefunction();	
	
FUNCTION calculation
	//calcuate predicted mean length at age 
	for(int j=1;j<=nage;j++) 
	 {
	 switch (gmopt)
	 {
	  case 1:	 { //traditional VB growth model
	  meanlen(j) = p1*(1.-mfexp(-p2*((j-1)*increment_age-p3)));
	  //linf*(1.-mfexp(-vbk*((j-1)*increment_age-t0))); 
	  break;	 }

	  case 2:    { //Gompertz growth model
	  meanlen(j) = p1* mfexp(-(1./p2)*mfexp(-p2*((j-1)*increment_age-p3)));
	  //meanlen(j)=linf*mfexp(-(1./k)*mfexp(-k*(t-t0)));
	  break;	 }
	  
	  case 3:    { //Inverse logistic growth model
	  meanlen(j) = p1/(1+p2*exp(-p3*(j-1)*increment_age));
	  //meanlen(j)=alpha/(1+beta*exp(-kt))
	  break;	 }
	  
	  case 4:    { //Richards growth model
	  meanlen(j) = p1*pow((1.0+(1.0/p4)*mfexp(-p2*((j-1)*increment_age-p3))),-p4);
	  //linf*pow((1+(1/p)*exp(-k*(t-t0)),-p);
	  break; 	 }  
	  }
	  // cout<<"meanlen"<<meanlen<<endl;
	   //cout<<"ps"<<p1<<p2<<p3<<p4<<endl;
	   
	  switch (varopt)
	  { case 1: 	  
	  sigma(j) = meanlen(j) * var; 
	  break;
	    case 2:    
	  sigma(j) = var; 
	  break;
	  	case 3: {
		sigma(j) = meanlen(j)*cv1;
		if ((j-1)*increment_age>cva1&(j-1)*increment_age<cva2)
		sigma(j) = sigma(j)+ meanlen(j)*(cv2-cv1)*((j-1)*increment_age-cva1)/(cva2-cva1);
		if ((j-1)*increment_age>=cva2)
		sigma(j) = meanlen(j)*cv2;
	   break;
		}
	  } //end switch varopt
	  
	  cv(j)=sigma(j)/meanlen(j);
	 } //end j loop
	  

	// cout<<"sigma"<<sigma<<endl;// prints sigma 

	 
	 //calculate pdf
	for(int i=1;i<=nlength;i++) 
	 {
	 for(int j=1;j<=nage;j++) 	    
	  {
	  	cdf1(i,j)=cumd_norm((lenbin(i)-meanlen(j))/sigma(j));  
	  	cdf2(i,j)=cumd_norm((poplenbin(i+1)-meanlen(j))/sigma(j));    
	  	pdf(i,j)=cdf2(i,j)-cdf1(i,j);
	  }
	 }   

	for (int m=1;m<=nfleet;m++)
	{	
		for (int k=1;k<=nyear;k++)
		{  
		for(int i=1;i<=nlength;i++) 
		{	  
		sumpop(i,k,m)=0;
		for(int j=1;j<=nage;j++) 
		{
		popProp(i,j,k,m)= pdf(i,j)*ageprop(j,k,m);	//
		if (popProp(i,j,k,m) == 0) 
		  {
		   popProp(i,j,k,m) = 1.e-100;		   
		  } 	     
	  	  sumpop(i,k,m) += popProp(i,j,k,m);		///two dimension sum
		}
	  }
	}	
	}
	//cout<< "sumpop complete" << endl;	
	
	for (int m=1;m<=nfleet;m++)
	{
	for (int k=1;k<=nyear;k++)
	{
		for(int i=1;i<=nlength;i++)		
		{
		
		p_hat(k,i,1)=(k-1)*increment_year+first_year;
		p_hat(k,i,2)=m;
		p_hat(k,i,3)=(i-1)*increment_length;
			for(int j=1;j<=nage;j++) 
			{
		sumpopmat(i,j)=sumpop(i,k,m);
		RescalepopProp(i,j,k,m) = popProp(i,j,k,m)/sumpopmat(i,j);
		logRescalepopProp(i,j,k,m) =log(RescalepopProp(i,j,k,m));
		//cout<<"i"<<i<<"j"<<j<<"k"<<k<<"m"<<m<<"  prop"<<popProp(i,j,k,m)<<"sumpop"<<sumpopmat(i,j)<<" logprop"<<logRescalepopProp(i,j,k,m)<<endl;
		
		p_hat(k,i,j+3)=RescalepopProp(i,j,k,m);
		}
	  }
	}
	}

	//cout<< "popProp complete" <<endl;
FUNCTION calobjectivefunction	//calculate objective function	
	dvar4_array x1(1,nlength,1,nage,1,nyear,1,nfleet);// temporary variable
	nll =0;
	likesum	= 0;
	for (int m=1;m<=nfleet;m++)
	  {
	for(int k=1;k<=nyear;k++) 
	{
	 	for(int i=1;i<=nlength;i++) 
		{
	    for(int j=1;j<=nage;j++) 
		{
	   x1(i,j,k,m) = condaal(i,j,k,m)*logRescalepopProp(i,j,k,m);
	   likesum += x1(i,j,k,m);
	  }
	 }
	}		
   }
	cout<< "nloglike " << -likesum << endl;	    	
	nll = -likesum; 
		
REPORT_SECTION

    report<<"# Estimation of all parameters"<<endl;
	REPORT(p1);// write to the report file using the REPORT function
	REPORT(p2);
	REPORT(p3);
	//REPORT(p4); //dummy if gmopt=1
	report<<"p4 # If using Richards growth curve, p4 is estimated, otherwise p4 is a dummy variable and isn't estimated"<<endl;
	report<<p4<<endl;
	if (varopt==1 ) {	report<<"cv"<<endl;
	report<<var<<endl;}
	if (varopt==2)  {   report<<"sigma"<<endl;
	report<<var<<endl;}
	if (varopt==3 ) {
					REPORT(cv1);
					REPORT(cv2);
					}
	report<<meanlen<<endl;
	REPORT(sigma);
	REPORT(cv);
	report<<"negative-log-likelihood"<<endl;
	report<<nll<<endl;
	//REPORT(covar_mat); this doesn't work in report section
	REPORT(p_hat);
	//save_gradients(gradients);
TOP_OF_MAIN_SECTION

	time(&start);
	arrmblsize = 50000000;
	gradient_structure::set_GRADSTACK_BUFFER_SIZE(1.e7);
	gradient_structure::set_CMPDIF_BUFFER_SIZE(1.e7);
	gradient_structure::set_MAX_NVAR_OFFSET(5000);
	gradient_structure::set_NUM_DEPENDENT_VARIABLES(5000);

GLOBALS_SECTION
	/**
	\def REPORT(object)
	Prints name and value of \a object on ADMB report %ofstream file.
	*/
	#undef REPORT
	#define REPORT(object) report << #object "\n" << object << endl;//create the REPORT object thar configures the way things are going to be reported
	#include <admodel.h>
	#include <time.h>
	#include <math.h>
	#include<contrib.h>//IF you have ADMB-11
	//#include<stats.cxx>//If you have ADMB-10 and make sure stats.cxx is in your working directory
	time_t start,finish;
	long hour,minute,second;
	double elapsed_time;
    
FINAL_SECTION
	time(&finish);
	elapsed_time=difftime(finish,start);
	hour=long(elapsed_time)/3600;
	minute=long(elapsed_time)%3600/60;
	second=(long(elapsed_time)%3600)%60;
	cout<<"*******************************************"<<endl;
	cout<<"--Start time: "<<ctime(&start)<<endl;
	cout<<"--Finish time: "<<ctime(&finish)<<endl;
	cout<<"--Runtime: ";
	cout<<hour<<" hours, "<<minute<<" minutes, "<<second<<" seconds"<<endl;
	cout<<"*******************************************"<<endl;
    
	ofstream covarout("myreport.rep");
    
    covarout<<"### This is the report file for the CONDAAL -Version 1.0. (NOAA-SWFSC) ###"<<endl;
	//covarout<<gmopt<<" # Growth model option 1==Traditional VB 2==Gompertz 3==Inverse logistic 4==Richards "<<endl;
	covarout<<"# Estimation of all parameters"<<endl;
	covarout<<"p1"<<endl;
	covarout<<p1<<endl;
	covarout<<"p2"<<endl;
	covarout<<p2<<endl;
	covarout<<"p3"<<endl;
	covarout<<p3<<endl;
	covarout<<"p4"<<endl;
	covarout<<p4<<" #If using Richards growth curve, p4 is estimated, otherwise p4 is a dummy variable and isn't estimated"<<endl;
	covarout<<"covariance_matrix"<<endl;
	covarout<<covar_mat<<endl;
	//covarout<<varopt<<" # variance option 1==fixed cv 2==fixed sigma 3==fixed cv at two user defined ages, linear interpolation between age1&2"<<endl;
	if (varopt==1 ) {	covarout<<"cv"<<endl;
						covarout<<var<<endl;    }
	if (varopt==2)  {   covarout<<"sigma"<<endl;
	                    covarout<<var<<endl;    }
	if (varopt==3 ) {   covarout<<"cv1"<<endl;
					    covarout<<cv1<<endl;
						covarout<<"cv2"<<endl;
					    covarout<<cv2<<endl;	}
						
	covarout<<"mean_length"<<endl;
	covarout<<meanlen<<endl;
	covarout<<"sigma_at_ages"<<endl;
	covarout<<sigma<<endl;
	covarout<<"cv_at_ages"<<endl;
	covarout<<cv<<endl;
	covarout<<"negative-log-likelihood"<<endl;
	covarout<<nll<<endl;
	covarout<<"max_gradient"<<endl;
	covarout<<objective_function_value::pobjfun->gmax<<endl;
	covarout<<" "<<endl;
	covarout<<"estimated_p_hat"<<endl;
	covarout<<p_hat<<endl;
	covarout<<"# All Echo inputs #"<<endl;
	covarout<<gmopt<<" #gmopt"<<endl;  //growth model option 1==Traditional VB 2==Gompertz 3==Inverse logistic 4==Richards 
	covarout<<p1_lower<<"   "<<p1_upper<<" #p1_lower_upper_bounds"<<endl;
	covarout<<p2_lower<<"   "<<p2_upper<<" #p2_lower_upper_bounds"<<endl;
	covarout<<p3_lower<<"   "<<p3_upper<<" #p3_lower_upper_bounds"<<endl;
	covarout<<p4_lower<<"   "<<p4_upper<<" #p4_lower_upper_bounds//for Richards only, dummy otherwise"<<endl;
    covarout<<varopt<<" #varopt"<<endl;
	if (varopt==3) {	covarout<<cva1<<"   "<<cva2<<" #if varopt==3 define age"<<endl; }
	covarout<<first_length<<"   "<<last_length<<"   "<<increment_length<<" #1st_length last_length length_increment"<<endl;
	covarout<<first_age<<"   "<<last_age<<"   "<<increment_age<<" #1st_age last_age age_increment//if==1 annual/==2 every 2 years/==0.5 every 6months"<<endl;
	covarout<<first_year<<"   "<<last_year<<"   "<<increment_year<<" #1st_year last_year year_increment"<<endl;
	covarout<<nfleet<<" #nfleet"<<endl;
	covarout<<"age_proportion_population"<<endl;
	covarout<<ageprop_temp<<endl;
	covarout<<"age_length_key"<<endl;
	covarout<<condaal_temp<<endl;
	covarout<<endfileflag<<" #end_of_file_if999"<<endl;
	//end of the file

