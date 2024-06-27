##===============================================================================##
##                        SIMULATION SCRIPT
##                          MASTER THESIS
##===============================================================================##
#                                                   

#' Generate a predictor matrix x, and response vector y, following a specified
#'   setup.  Actually, twenty seven of predictors and one response are generated:
#'   one for training, and one for validation.
#'
#' @param n,p  The number of training observations,the number of predictors 
#'   variables; specifically, predictors i and j have population correlation
#' @param nrep   #Number of repetitions for a given setting
#' @return A list with the following components: x, y
#'
#' @author Hubert Azonvidé DOSSA
#                                                   
#

# Set working directory

setwd("E:/Biostatistics documents/Biostatistics_2/Thesis_Master/Scripts thesis/Simulation_Data/R code")


sim.xy = function(n,p){
  if (p==27){
    # For p=29 and q=14
    intercept <-25.73          # The inetercept of the model
    # Let's define the vector of our parameters
    beta <- c(0.04,0.13,0.08,27.27,0.01,-0.03,0.71,-1.68,-0.001,
              -2.13,-0.02,-0.45,-0.05,-0.99,-1.20,-2.60,-0.26,1.28,-1.30,1.85,-0.66,
              1.86,-2.48,0.20,-2.31,2.22,-1.96,20.02,-22.37,3.38)
    # Generate predictors
    Data <-data.frame(Age=round(abs(rnorm(n,43.91,12.51))),
                      FOPR= round(abs(rnorm(n,2.3145,2.4156)),digits = 2),
                      TFarmSize=round(abs(rnorm(n,4.2944,5.3756)),digits = 2),
                      LRS=round(abs(rnorm(n,1.175,1.3318)),digits = 2),
                      # Variables suivant poisson
                      HouseSize= rpois(n,8.5995),      # lambda param tre de la distrib poisson
                      TWF= rpois(n,8.1318),       
                      Experience=rpois(n,13.9568),  
                      LivestockO=rpois(n,0.2757),    
                      # Variables suivant Binomiale 
                      OFIOui=rbinom(n,1,0.2374), 
                      LandOOui=rbinom(n,1,0.7601),   
                      FamilyWFOui=rbinom(n,1,0.8105),
                      HFLOui=rbinom(n,1,0.4868),
                      RAOui=rbinom(n,1,0.8800),
                      TRFOui=rbinom(n,1,0.6570),
                      MAOui=rbinom(n,1,0.6306),
                      RMCropOui=rbinom(n,1,0.58753),
                      UFertilizerOui=rbinom(n,1,0.7793),
                      UPesticidesOui=rbinom(n,1,0.6762),
                      IrrigationOui=rbinom(n,1,0.1272),
                      OSROui=rbinom(n,1,0.1942),
                      GEOui=rbinom(n,1,0.5059),
                      NGOSOui=rbinom(n,1,0.20),
                      MachineryOOui=rbinom(n,1,0.1570),
                      AninamlTOui=rbinom(n,1,0.1507),
                      InterInstOui=rbinom(n,1,0.1504),
                      # Variables multinominales
                      Region=sample(c(1,2,3),n,replace = TRUE,prob = c(0.18,0.52,0.30)),
                      Education=sample(c(1,2,3,4),n,replace = TRUE, prob=c(0.50,0.20,0.19,0.11))
    )
    attach(Data) 
    
    ###################################"
    
    #### Let's write the simulation Model to generate the response variable
    
    Adoption <- numeric(n)
    invlogit <- function(x) {return(exp(x)/(1+exp(x)))}
    for (i in 1:n){
      Adoption[i]<- intercept+beta[1]*Age+beta[2]*(Education[i]==2)+beta[3]*(Education[i]==3)+
        beta[4]*(Education[i]==4)+beta[5]*HouseSize+beta[6]*Experience+beta[7]*OFIOui+
        beta[8]*LandOOui+beta[9]*LivestockO+beta[10]*MachineryOOui+beta[11]*TFarmSize+
        beta[12]*LRS+beta[13]*TWF+beta[14]*FamilyWFOui+
        beta[15]*HFLOui+beta[16]*RAOui+beta[17]*TRFOui+beta[18]*MAOui+beta[19]*RMCropOui+
        beta[20]*UFertilizerOui+beta[21]*UPesticidesOui+beta[22]*AninamlTOui+beta[23]*IrrigationOui+
        beta[24]*FOPR+beta[25]*OSROui+beta[26]*GEOui+beta[27]*NGOSOui+beta[28]*InterInstOui+
        beta[29]*(Region[i]==2)+beta[30]*(Region[i]==3)
    }
    Size<- rep(n,n)
    prop <- invlogit(Adoption)
    Adoption <- rbinom(n,1,prop)
    ##     
    for (a in 1:length(Adoption)){
      while ((0 %in% Adoption)!=TRUE) {
        Adoption <- rbinom(n,1,prop)  
      } 
    }
    Base <- data.frame(Adoption,Data) # constitute the dataset
    ##
  } else if (p==20) {
    # For p=20 and q=07
    intercept <-21.31          # The inetercept of the model
    # Let's define the vector of our parameters
    beta<-c(20.13,0.77,1.10,0.05,-0.79,-0.03,0.11,-0.48,2.09,-2.54,0.22,
            0.03,25.07,-2.03,-20.52,-0.01,0.03,-1.58,-0.22,1.22,-0.03,-0.03,0.01)
    # Generate predictors
    Data <-data.frame(Age=round(abs(rnorm(n,43.91,12.51))),
                      FOPR= round(abs(rnorm(n,2.3145,2.4156)),digits = 2),
                      TFarmSize=round(abs(rnorm(n,4.2944,5.3756)),digits = 2),
                      LRS=round(abs(rnorm(n,1.175,1.3318)),digits = 2),
                      # Variables suivant poisson
                      HouseSize= rpois(n,8.5995), # lambda param tre de la distrib poisson
                      TWF= rpois(n,8.1318),
                      Experience=rpois(n,13.9568),
                      LivestockO=rpois(n,0.2757),
                      # Variables suivant Binomiale
                      OFIOui=rbinom(n,1,0.2374),
                      RAOui=rbinom(n,1,0.8800),
                      TRFOui=rbinom(n,1,0.6570),
                      RMCropOui=rbinom(n,1,0.58753),
                      UFertilizerOui=rbinom(n,1,0.7793),  
                      UPesticidesOui=rbinom(n,1,0.6762),
                      NGOSOui=rbinom(n,1,0.20),
                      MachineryOOui=rbinom(n,1,0.1570),
                      AninamlTOui=rbinom(n,1,0.1507),
                      InterInstOui=rbinom(n,1,0.1504),
                      # Variables multinominales
                      Region=sample(c(1,2,3),n,replace = TRUE,prob = c(0.18,0.52,0.30)),
                      Education=sample(c(1,2,3,4),n,replace = TRUE, prob=c(0.50,0.20,0.19,0.11))
                      
    )
    ###################################"
    attach(Data) 
    
    #### Let's write the simulation Model
    Adoption <- numeric(n)
    invlogit <- function(x) {return(exp(x)/(1+exp(x)))}
    for (i in 1:n){
      Adoption[i]<- intercept+beta[1]*InterInstOui+beta[2]*AninamlTOui+beta[3]*UFertilizerOui+beta[4]*Age+
        beta[5]*RMCropOui+beta[6]*Experience+beta[7]*LivestockO+beta[8]*LRS+beta[9]*TRFOui+
        beta[10]*RAOui+beta[11]*(Education[i]==2)+beta[12]*(Education[i]==3)+beta[13]*(Education[i]==4)+
        beta[14]*NGOSOui+beta[15]*(Region[i]==2)+beta[16]*(Region[i]==3)+beta[17]*FOPR+
        beta[18]*MachineryOOui+beta[19]*UPesticidesOui+beta[20]*OFIOui+
        beta[21]*TWF+beta[22]*HouseSize+beta[23]*TFarmSize
    }
    Size<- rep(n,n)
    prop <- invlogit(Adoption)
    Adoption <- rbinom(n,1,prop)
    ##     
    for (a in 1:length(Adoption)){
      while ((0 %in% Adoption)!=TRUE) {
        Adoption <- rbinom(n,1,prop)
      } 
    }
    Base <- data.frame(Adoption,Data) # constitute the dataset
    ## # constitute the dataset
  } else if (p==10){
    # For p=10 and q=04
    intercept <-18.36          # The inetercept of the model
    # Let's define the vector of our parameters
    beta<-c(0.03,0.19,-0.01,18.58,-0.02,0.00,-18.71,0.71,0.87,-0.04,-0.03)
    # Generate predictors
    Data <-data.frame(Age=round(abs(rnorm(n,43.91,12.51))),
                      LRS=round(abs(rnorm(n,1.175,1.3318)),digits = 2),
                      # Variables suivant poisson
                      HouseSize= rpois(n,8.5995), # lambda param tre de la distrib poisson
                      TWF= rpois(n,8.1318),
                      LivestockO=rpois(n,0.2757),
                      # Variables suivant Binomiale
                      OFIOui=rbinom(n,1,0.2374),
                      MachineryOOui=rbinom(n,1,0.1570),
                      AninamlTOui=rbinom(n,1,0.1507),
                      InterInstOui=rbinom(n,1,0.1504),
                      # Variables multinominales
                      Region=sample(c(1,2,3),n,replace = TRUE,prob = c(0.18,0.52,0.30))
    )
    
    attach(Data)
    
    #### Let's write the simulation Model
    Adoption <- numeric(n)
    invlogit <- function(x){return(exp(x)/(1+exp(x)))}
    for (i in 1:n){
      Adoption[i]<- intercept+beta[1]*Age+beta[2]*AninamlTOui+beta[3]*LivestockO+beta[4]*InterInstOui+
        beta[5]*MachineryOOui+beta[6]*LRS+beta[7]*(Region[i]==2)+beta[8]*(Region[i]==3)+beta[9]*OFIOui+
        beta[10]*TWF+beta[11]*HouseSize
    }
    Size<- rep(n,n)
    prop <- invlogit(Adoption)
    Adoption <- rbinom(n,1,prop)
    ##     
    for (a in 1:length(Adoption)){
      while ((0 %in% Adoption)!=TRUE) {
        Adoption <- rbinom(n,1,prop)
      } 
    }
    Base <- data.frame(Adoption,Data) # constitute the dataset
    ##
  } else {
    print("No program for this number of covariates")
  }
}
#       Loop to repeat the 5000 simulations

nrep <-5000             # nrep is the number of repetition

base.df <- vector(mode = 'list',nrep)
attr(base.df,'names')<-paste('Sim',1: nrep,sep ="") # To name each base of simulate data

for (i in 1:length(base.df)){
  base.df[[i]] <- sim.xy(n,p)   # value of n and p (nbre of observation and covariates)
}

# to save the simulate data as .rda form
save('base.df', file = 'data80.rda') 





















