# For p=20

attach(base.df)
for (i in 1:length(base.df)){           
  base.df[[i]]$Adoption<-as.factor(base.df[[i]]$Adoption)
  base.df[[i]]$Education <-as.factor(base.df[[i]]$Education)
  base.df[[i]]$OFIOui<-as.factor(base.df[[i]]$OFIOui)
  base.df[[i]]$MachineryOOui<-as.factor(base.df[[i]]$MachineryOOui)
  base.df[[i]]$RAOui <-as.factor(base.df[[i]]$RAOui)
  base.df[[i]]$TRFOui<-as.factor(base.df[[i]]$TRFOui)
  base.df[[i]]$UFertilizerOui<-as.factor(base.df[[i]]$UFertilizerOui)
  base.df[[i]]$UPesticidesOui<-as.factor(base.df[[i]]$UPesticidesOui)
  base.df[[i]]$AninamlTOui <-as.factor(base.df[[i]]$AninamlTOui)
  base.df[[i]]$RMCropOui <-as.factor(base.df[[i]]$RMCropOui)
  base.df[[i]]$NGOSOui<-as.factor(base.df[[i]]$NGOSOui)
  base.df[[i]]$InterInstOui<-as.factor(base.df[[i]]$InterInstOui)
  base.df[[i]]$Region<-as.factor(base.df[[i]]$Region)
}
attach(base.df[[i]])
Sim2


heta <- matrix(rep(NA),N,4)
#Thetak <- NULL
Beta <- matrix(rep(NA),N,5)
Se1 <-matrix(rep(NA),N,4)
Se2 <-matrix(rep(NA),N,5)