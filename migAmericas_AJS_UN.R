### code for: Leal, Diego F. "Network Inequalities and International Migration in the Americas." American Journal of Sociology 126(5): 1067-1126
### code written by Diego F. Leal (www.diegoleal.info)
### Last update: 05/21/2021
### Robustness check: TERGM for 1990-2015 UN Data and binarization based on size (see Tables C5, C6, and C7 in the paper)

rm(list=ls())

library(reshape2)         # version 1.4.3
library(network)          # version 1.15
library(ergm)             # version 3.10.4
library(statnet)          # version 2015.11.0
library(sna)              # version 2.4
library(latticeExtra)     # version 0.6-28
library(xergm)            # version 1.3
library(migest)           # version 1.7.4
library(dplyr)            # version 0.8.3
library(Rcpp)             # version 1.0.2
library(tergm)            # version 3.6.1
library(snow)             # version 0.4-3

#session info for full replication
# R version 3.5.1 (2018-07-02)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows >= 8 x64 (build 9200)
# 
# Matrix products: default
# 
# locale:
# [1] LC_COLLATE=English_United States.1252  LC_CTYPE=English_United States.1252   
# [3] LC_MONETARY=English_United States.1252 LC_NUMERIC=C                          
# [5] LC_TIME=English_United States.1252    
# 
# attached base packages:
# [1] stats     graphics  grDevices utils     datasets  methods   base     
# 
# other attached packages:
# [1] snow_0.4-3            Rcpp_1.0.2            dplyr_0.8.3          
# [4] migest_1.7.4          xergm_1.3             latticeExtra_0.6-28  
# [7] RColorBrewer_1.1-2    lattice_0.20-35       statnet_2015.11.0    
# [10] ergm.count_3.4.0      tergm_3.6.1           sna_2.4              
# [13] statnet.common_4.3.0  networkDynamic_0.10.0 ergm_3.10.4          
# [16] network_1.15          reshape2_1.4.3       
# 
# loaded via a namespace (and not attached):
# [1] gtools_3.8.1       tidyselect_1.1.0   lpSolve_5.6.15     purrr_0.3.4       
# [5] splines_3.5.1      tcltk_3.5.1        vctrs_0.3.1        stats4_3.5.1      
# [9] yaml_2.2.0         mgcv_1.8-24        rlang_0.4.6        pillar_1.4.4      
# [13] nloptr_1.2.1       glue_1.3.0         trust_0.1-7        lifecycle_0.2.0   
# [17] plyr_1.8.4         robustbase_0.93-5  stringr_1.4.0      caTools_1.17.1.1  
# [21] coda_0.19-2        permute_0.9-4      parallel_3.5.1     DEoptimR_1.0-8    
# [25] KernSmooth_2.23-15 ROCR_1.0-7         gdata_2.18.0       vegan_2.5-3       
# [29] texreg_1.36.23     lme4_1.1-19        gplots_3.0.1.1     stringi_1.1.7     
# [33] grid_3.5.1         bitops_1.0-6       tools_3.5.1        magrittr_1.5      
# [37] RSiena_1.2-12      tibble_3.0.1       cluster_2.0.7-1    crayon_1.3.4      
# [41] pkgconfig_2.0.2    MASS_7.3-53        ellipsis_0.2.0.1   Matrix_1.2-17     
# [45] assertthat_0.2.1   minqa_1.2.4        rstudioapi_0.11    boot_1.3-20       
# [49] R6_2.3.0           igraph_1.2.4.1     nlme_3.1-141       compiler_3.5.1  


## set key paramters

#threshold for flow sizes 
flow.sizes<-c(10,25,50)

#for each flow size
for (zz in 1:length(flow.sizes))
{  

#set random number generator seed for replicability
set.seed(5)


#set working dicrectory to import UN and other relevant data
setwd("C:/Users/leald/Dropbox/DLeal/USC/papers/MigAmericas/AJS/R&R/new_analyses_R&R/AJS_R&R/Robustness_1990_2015")

#import controls data
data_90_15<-read.csv("clean_node_level_90_15.csv",header=T,sep = ",",as.is=T)

#import the Azose and Raftery estimates (https://www.pnas.org/content/116/1/116.abstract) based on the UN stock data
data_90_95<-read.csv("Flow_period 1.csv",header=F,sep = ",",as.is=T)
data_90_95<-data_90_95[-1,-1]

data_95_00<-read.csv("Flow_period 2.csv",header=F,sep = ",",as.is=T)
data_95_00<-data_95_00[-1,-1]

data_00_05<-read.csv("Flow_period 3.csv",header=F,sep = ",",as.is=T)
data_00_05<-data_00_05[-1,-1]

data_05_10<-read.csv("Flow_period 4.csv",header=F,sep = ",",as.is=T)
data_05_10<-data_05_10[-1,-1]

data_10_15<-read.csv("Flow_period 5.csv",header=F,sep = ",",as.is=T)
data_10_15<-data_10_15[-1,-1]

labels_90_15 <-c("AAB", "ABW",	"ARG",	"BAR",	"BHM",	"BLZ",	"BOL",	"BRA",	"CAN",	"CHL",	"COL",	"COS",	"CUB",
                 "DOM",	"ECU",  "GRN",	"GUA",	"GUY",	"HAI",	"HON",	"JAM",	"MEX",	"NIC",	"PAN",	"PAR",	"PER",	
                 "PRI",	"SAL",	"SLU",	"SUR",	"SVG",	"TRI",	"URU",	"USA",	"VEN")

#keep relevant data only
to.keep.90_15<-as.vector(0)
to.keep.flag <-0

for (re in 1:nrow(data_90_95))
{
  for (rew in 1:length(labels_90_15))
    if(data_90_95[re,1]==labels_90_15[rew])
    {
      to.keep.flag <- to.keep.flag + 1 
      to.keep.90_15[to.keep.flag] <- re
    }
}


list.90_15<-list(data_90_95,data_95_00,data_00_05,data_05_10,data_10_15)


for (i in 1:length(list.90_15))
{
  list.90_15[[i]]<-list.90_15[[i]][,-1]
  list.90_15[[i]]<-list.90_15[[i]][to.keep.90_15,to.keep.90_15]
}


t1<-data.matrix(list.90_15[[1]])
t2<-data.matrix(list.90_15[[2]])
t3<-data.matrix(list.90_15[[3]])
t4<-data.matrix(list.90_15[[4]])
t5<-data.matrix(list.90_15[[5]])

for (i in 1:nrow(t1))
{
  for (j in 1:ncol(t1))
  {
    ifelse(as.numeric(t1[i,j])>flow.sizes[zz],t1[i,j]<-as.numeric(1),t1[i,j]<-as.numeric(0))
  }
}
row.names(t1)<-colnames(t1)<-labels_90_15


for (i in 1:nrow(t2))
{
  for (j in 1:ncol(t2))
  {
    ifelse(as.numeric(t2[i,j])>flow.sizes[zz],t2[i,j]<-as.numeric(1),t2[i,j]<-as.numeric(0))
  }
}
row.names(t2)<-colnames(t2)<-labels_90_15

for (i in 1:nrow(t3))
{
  for (j in 1:ncol(t3))
  {
    ifelse(as.numeric(t3[i,j])>flow.sizes[zz],t3[i,j]<-as.numeric(1),t3[i,j]<-as.numeric(0))
  }
}
row.names(t3)<-colnames(t3)<-labels_90_15

for (i in 1:nrow(t4))
{
  for (j in 1:ncol(t4))
  {
    ifelse(as.numeric(t4[i,j])>flow.sizes[zz],t4[i,j]<-as.numeric(1),t4[i,j]<-as.numeric(0))
  }
}
row.names(t4)<-colnames(t4)<-labels_90_15

for (i in 1:nrow(t5))
{
  for (j in 1:ncol(t5))
  {
    ifelse(as.numeric(t5[i,j])>flow.sizes[zz],t5[i,j]<-as.numeric(1),t5[i,j]<-as.numeric(0))
  }
}
row.names(t5)<-colnames(t5)<-labels_90_15

##transforming data objects into binary (flow) matrices, flows that are < 50 = 0
T1<-data.matrix(t1)
T2<-data.matrix(t2)
T3<-data.matrix(t3)
T4<-data.matrix(t4)
T5<-data.matrix(t5)



#stroring the matrices in a list
FLOWS<-mget(paste0("T",1:5))

#preprocess data using XERGM's built in function
dep2<- preprocess(FLOWS,
                  data_90_15[,"Gross.Domestic.Product..GDP..e90s"],
                  data_90_15[,"Gross.Domestic.Product..GDP..l90s"],
                  data_90_15[,"Gross.Domestic.Product..GDP..e00s"],
                  data_90_15[,"Gross.Domestic.Product..GDP..l00s"],
                  data_90_15[,"Gross.Domestic.Product..GDP..e10s"],
                  data_90_15[,"coups_e90s"],
                  data_90_15[,"coups_l90s"],
                  data_90_15[,"coups_e00s"],
                  data_90_15[,"coups_l00s"],
                  data_90_15[,"coups_e10s"],
                  data_90_15[,"pop_e90s"],
                  data_90_15[,"pop_l90s"],
                  data_90_15[,"pop_e00s"],
                  data_90_15[,"pop_l00s"],
                  data_90_15[,"pop_e10s"],
                  data_90_15[,"language_num"],
                  data_90_15[,"Region_Num"],
                  lag=T,covariate=F,na=NA,na.method="fillmode")

#checking the resutls, the dependnet networks should start at time 2 because the estimation is conditioned on the first network
length(dep2)
sapply(FLOWS,dim)
sapply(dep2,dim)
rownames(dep2[[4]])

#creating a "memory" term for existing and non-existing edges
mem.stability2<- preprocess(FLOWS,
                            lag=T,covariate=T,memory="stability", na=NA,na.method="fillmode")

#checking the resutls. The last time step should be removed and dimensionsshould be adjusted.
length(mem.stability2)
sapply(mem.stability2,dim)
sapply(mem.stability2,dim)
rownames(mem.stability2[[4]])

#covariate: region of the world
region.cov2<-preprocess(data_90_15[,"Region_Num"], FLOWS,
                        lag=F,covariate=T)

#covariate: language
language.cov2<-preprocess(data_90_15[,"language_num"], FLOWS,
                          lag=F,covariate=T)

#single-period delayed reciprocity:
#transpose the flow matrices and create a lagged reciprocity covariate
delrecip2<-lapply(FLOWS,t)
delrecip2<-preprocess(delrecip2,
                      lag=T,covariate=T,
                      na=NA,na.method="fillmode")

#adding, other variables: 

for (i in 1:length(dep2))
{
  dep2[[i]] <-network(dep2[[i]])                        #for the ith network, compute
  odegsqrt2<-sqrt(degree(dep2[[i]],cmode="outdegree"))   #sqrt(outdegree)
  idegsqrt2<-sqrt(degree(dep2[[i]],cmode="indegree"))    #sqrt(indegree)
  dep2[[i]]<-set.vertex.attribute(dep2[[i]],"odegsqrt",odegsqrt2) #add outdegree as a network attribute
  dep2[[i]]<-set.vertex.attribute(dep2[[i]],"idegsqrt",idegsqrt2) #add indegree as a network attribute
  dep2[[i]]<-set.vertex.attribute(dep2[[i]],"region",region.cov2[[i]]) #add region as a network attribute
  dep2[[i]]<-set.vertex.attribute(dep2[[i]],"language",language.cov2[[i]]) #add language as a network attribute
}

#add (lagged) income as a covariate
dep2[[1]]<-set.vertex.attribute(dep2[[1]],"aici",data_90_15[,"Gross.Domestic.Product..GDP..e90s"])   
dep2[[2]]<-set.vertex.attribute(dep2[[2]],"aici",data_90_15[,"Gross.Domestic.Product..GDP..l90s"])
dep2[[3]]<-set.vertex.attribute(dep2[[3]],"aici",data_90_15[,"Gross.Domestic.Product..GDP..e00s"])
dep2[[4]]<-set.vertex.attribute(dep2[[4]],"aici",data_90_15[,"Gross.Domestic.Product..GDP..l00s"])


#add (lagged) conflict as a covariate
dep2[[1]]<-set.vertex.attribute(dep2[[1]],"conflict",data_90_15[,"coups_e90s"])
dep2[[2]]<-set.vertex.attribute(dep2[[2]],"conflict",data_90_15[,"coups_l90s"])
dep2[[3]]<-set.vertex.attribute(dep2[[3]],"conflict",data_90_15[,"coups_e00s"])
dep2[[4]]<-set.vertex.attribute(dep2[[4]],"conflict",data_90_15[,"coups_l00s"])

#add (lagged) conflict as a covariate
dep2[[1]]<-set.vertex.attribute(dep2[[1]],"population",data_90_15[,"pop_e90s"])
dep2[[2]]<-set.vertex.attribute(dep2[[2]],"population",data_90_15[,"pop_l90s"])
dep2[[3]]<-set.vertex.attribute(dep2[[3]],"population",data_90_15[,"pop_e00s"])
dep2[[4]]<-set.vertex.attribute(dep2[[4]],"population",data_90_15[,"pop_l00s"])

#checking the dependent networks
#first extract the networks
dependent2.t2<-dep2$T2
dependent2.t3<-dep2$T3
dependent2.t4<-dep2$T4
dependent2.t5<-dep2$T5

#sumarize the networks
dependent2.t2
dependent2.t3
dependent2.t4
dependent2.t5

#checking some attributes 
get.vertex.attribute(dependent2.t2,attrname="aici")
get.vertex.attribute(dependent2.t3,attrname="conflict")
get.vertex.attribute(dependent2.t4,attrname="population")


## Final/Authoritative Model
## This reproduces C5 in the paper if flow.sizes = 50
## This reproduces C6 in the paper, if flow.sizes = 25
## This reproduces C7 in the paper,  if flow.sizes = 10

model <-btergm(dep2 ~
                  ctriple+
                  ttriple+
                  mutual +
                  edgecov(delrecip2) +
                  edgecov(mem.stability2)+
                  idegree1.5 +       
                  odegree1.5 +       
                  absdiff("odegsqrt")+
                  nodematch("region") +
                  nodematch("language") +
                  nodeicov("aici")+
                  nodeocov("aici")+
                  nodeicov("conflict")+
                  nodeocov("conflict")+
                  edges,
                parallel="snow",ncpus=30,R=250000)


#summary of model
summary(model,level=0.90)  # p-val <.1
summary(model,level=0.95)  # p-val <.05
summary(model,level=0.995) # p-val <.01
summary(model,level=0.999) # p-val <.001


#save results
session.info<-sessionInfo()


setwd("C:/Users/leald/Dropbox/DLeal/USC/papers/MigAmericas/AJS/output")




### save image
save.image(file = paste("All_objects_UN_size_",flow.sizes[zz],".RData",sep=""))

}

