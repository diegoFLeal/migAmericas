### code for: Leal, Diego F. "Network Inequalities and International Migration in the Americas." American Journal of Sociology 126(5): 1067-1126
### code written by Diego F. Leal (www.diegoleal.info)
### Last update: 05/21/2021

## session info for replicability
# R version 3.6.1 (2019-07-05)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows Server x64 (build 14393)
# 
# Matrix products: default
# 
# locale:
# [1] LC_COLLATE=English_United States.1252 
# [2] LC_CTYPE=English_United States.1252   
# [3] LC_MONETARY=English_United States.1252
# [4] LC_NUMERIC=C                          
# [5] LC_TIME=English_United States.1252    
# 
# attached base packages:
# [1] stats     graphics  grDevices utils     datasets  methods   base     
# 
# other attached packages:
# [1] snow_0.4-3            migest_1.7.3          Rcpp_1.0.4.6         
# [4] dplyr_0.8.5           xergm_1.3             latticeExtra_0.6-26  
# [7] lattice_0.20-38       RColorBrewer_1.1-2    texreg_1.36.23       
# [10] statnet_2015.11.0     ergm.count_3.4.0      tergm_3.6.1          
# [13] sna_2.4               statnet.common_4.3.0  networkDynamic_0.10.1
# [16] ergm_3.10.4           network_1.16.0        reshape2_1.4.3       
# [19] devtools_2.3.0        usethis_1.6.1        
# 
# loaded via a namespace (and not attached):
# [1] prettyunits_1.1.1 ps_1.3.0          assertthat_0.2.1  rprojroot_1.3-2  
# [5] digest_0.6.25     R6_2.4.0          plyr_1.8.4        backports_1.1.4  
# [9] stats4_3.6.1      coda_0.19-3       pillar_1.4.1      rlang_0.4.6      
# [13] rstudioapi_0.11   minqa_1.2.4       vegan_2.5-5       callr_3.4.3      
# [17] nloptr_1.2.2.1    Matrix_1.2-17     desc_1.2.0        splines_3.6.1    
# [21] lme4_1.1-23       statmod_1.4.34    stringr_1.4.0     igraph_1.2.4.1   
# [25] compiler_3.6.1    pkgconfig_2.0.2   pkgbuild_1.0.8    mgcv_1.8-28      
# [29] tidyselect_0.2.5  tibble_2.1.1      lpSolve_5.6.13.2  permute_0.9-5    
# [33] fansi_0.4.0       crayon_1.3.4      withr_2.1.2       MASS_7.3-51.4    
# [37] grid_3.6.1        nlme_3.1-140      magrittr_1.5      cli_2.0.2        
# [41] stringi_1.4.3     ROCR_1.0-11       fs_1.4.1          remotes_2.1.1    
# [45] testthat_2.3.2    robustbase_0.93-5 ellipsis_0.3.1    boot_1.3-22      
# [49] trust_0.1-7       tools_3.6.1       glue_1.4.1        DEoptimR_1.0-8   
# [53] purrr_0.3.4       processx_3.4.2    pkgload_1.0.2     parallel_3.6.1   
# [57] cluster_2.1.0     sessioninfo_1.1.1 memoise_1.1.0    

## clear all
rm(list=ls())

library(reshape2)         # version 1.4.3
library(network)          # version 1.16.0
library(ergm)             # version 3.10.4
library(statnet)          # version 2015.11.0
library(sna)              # version 2.4
library(latticeExtra)     # version 0.6-26
library(xergm)            # version 1.3
library(migest)           # version 1.7.3
library(dplyr)            # version 0.8.5
library(Rcpp)             # version 1.0.4.6
library(tergm)            # version 3.6.1
library(snow)             # version 0.4-3

#for full replicability, uncomment these lines of code to make sure the version of key packages is the one used in the paper
# require(devtools)
# remove.packages("xergm")
# remove.packages("latticeExtra")
# remove.packages("statnet")
# remove.packages("migest")
#install_version("xergm", version = "1.3", repos = "http://cran.us.r-project.org")
#install_version("latticeExtra", version = "0.6-26", repos = "http://cran.us.r-project.org")
#install_version("statnet", version = "2015.11.0", repos = "http://cran.us.r-project.org")
#install_version("migest", version = "1.7.3", repos = "http://cran.us.r-project.org")

##### 1. Import data, set key paramaters #####

## set working directory
setwd("C:/Users/leald/Dropbox/DLeal/USC/papers/MigAmericas/AJS/R&R/new_analyses_R&R/AJS_R&R")

## import data
load("all_data_migration_Americas.Rdata")
HDI<-read.csv("HHDI_with_iso.csv",header=T,sep = ",",as.is=T)

## set key paramters

#threshold to binarize ties. Any number > 32 would include all countries in the Americas since there are 32 in the analytic sample
top_destinations <-10000

#total number of countries in the original/preprocessed data set
preprocessed_N   <-231


#set random number generator seed for replicability
set.seed(5)

#set number of dyad census-conditioned graphs to simulate in Table 2 
sim.dyad.census <- 500



##### 2. cleaning and merging World Bank (WB) migrant stock data, CEPII data, and UN demographic data #####

#set working directory
#setwd(paste("C:/Users/leald/Dropbox/DLeal/USC/papers/MigAmericas/AJS/R&R/new_analyses_R&R/AJS_R&R/Models_TERGM_1960_2000/TERGM_1960_2000_top",top_destinations,sep=""))
setwd("C:/Users/leald/Dropbox/DLeal/USC/papers/MigAmericas/AJS/output")
#get rid of unimportant columns
WB.stock<- subset(WB.stock, select=-c(Migration.by.Gender.Code,Migration.by.Gender.Name))

#keep importat rows only 
WB.stock<-WB.stock[1:((preprocessed_N*preprocessed_N)+preprocessed_N),]

#relabel columns
labels.WB.stock<-c("orig","iso_orig","dest","iso_dest","s_1960s","s_1970s","s_1980s","s_1990s","s_2000s")
colnames(WB.stock)<-labels.WB.stock

#replace ".." with NAs
for(i in 5:ncol(WB.stock))
{
  for (j in 1:nrow(WB.stock))
  {
    if(WB.stock[j,i]== "..")
    {WB.stock[j,i]<-NA}
  }
}

#transforming columns with migrant stock data from type ch to type numeric 
WB.stock<-as.data.frame(WB.stock)

for (i in 5:ncol(WB.stock))
{
  WB.stock[,i] <-as.numeric(gsub(" ", "",WB.stock[,i], fixed = TRUE)) 
}  


######################## cleaning UN population data 

#extract column labels
labels.UN.pop<-unlist(UN.pop[16,])

#get rid of unimportant rows
UN.pop<-UN.pop[-(1:16),]

#attach labels to original object
colnames(UN.pop)<-labels.UN.pop
colnames(UN.pop)[3]<-"country_UN"
colnames(UN.pop)[5]<-"code_UN"

#get rid of unimportant columns
UN.pop    <-as.data.frame(UN.pop[,c("country_UN","code_UN","1960","1970","1980","1990","2000")])

#transforming columns with pop data from type ch to type numeric 
for (i in 3:ncol(UN.pop))
{
  UN.pop[,i] <-as.numeric(gsub(" ", "",UN.pop[,i], fixed = TRUE)) 
}  

#raw UN.pop data in thousands, so each cell that contains pop data is * by 3
Z<-UN.pop[,3:ncol(UN.pop)]
Y<-UN.pop[,1:2]
UN.pop<-cbind(Y,Z)

########################### cleaning UN births data 

#extract column labels
labels.UN.births<-unlist(UN.births[16,])

#get rid of unimportant rows
UN.births<-UN.births[-(1:16),]

#attach labels to original object
colnames(UN.births)     <-labels.UN.births
colnames(UN.births)[3]  <-"country_UN"
colnames(UN.births)[5]  <-"code_UN"

#get rid of unimportant columns
UN.births <-as.data.frame(UN.births[,c("country_UN","code_UN","1960-1965",
                                       "1965-1970","1970-1975","1975-1980","1980-1985","1985-1990",
                                       "1990-1995","1995-2000")])

#transforming columns with pop data from type ch to type numeric 
for (i in 3:ncol(UN.births))
{
  UN.births[,i] <-as.numeric(gsub(" ", "",UN.births[,i], fixed = TRUE)) 
}  

#create new columns to store decades-long births
UN.births[,c("b_1960s","b_1970s","b_1980s","b_1990s")]<-9999

#compute decades-long births 
for (i in 1:nrow(UN.births))
{
  UN.births[i,"b_1960s"] <-(UN.births[i,"1960-1965"] + UN.births[i,"1965-1970"]) 
  UN.births[i,"b_1970s"] <-(UN.births[i,"1970-1975"] + UN.births[i,"1975-1980"]) 
  UN.births[i,"b_1980s"] <-(UN.births[i,"1980-1985"] + UN.births[i,"1985-1990"]) 
  UN.births[i,"b_1990s"] <-(UN.births[i,"1990-1995"] + UN.births[i,"1995-2000"]) 
}


#raw UN.pop data in thousands, so each cell that contains pop data is * by 3
Z<-UN.births[,11:ncol(UN.births)]
Y<-UN.births[,1:2]
UN.births<-cbind(Y,Z)


########################### cleaning UN deaths data 

#extract column labels
labels.UN.deaths<-unlist(UN.deaths[16,])

#get rid of unimportant rows
UN.deaths<-UN.deaths[-(1:16),]

#attach labels to original object
colnames(UN.deaths)<-labels.UN.deaths
colnames(UN.deaths)[3]<-"country_UN"
colnames(UN.deaths)[5]<-"code_UN"

#get rid of unimportant columns
UN.deaths <-as.data.frame(UN.deaths[,c("country_UN","code_UN","1960-1965",
                                       "1965-1970","1970-1975","1975-1980","1980-1985","1985-1990",
                                       "1990-1995","1995-2000")])

#transforming columns with pop data from type ch to type numeric 
for (i in 3:ncol(UN.deaths))
{
  UN.deaths[,i] <-as.numeric(gsub(" ", "",UN.deaths[,i], fixed = TRUE)) 
}  

#create new columns to store decade-long deaths
UN.deaths[,c("d_1960s","d_1970s","d_1980s","d_1990s")]<-9999

#compute decade-long deaths 
for (i in 1:nrow(UN.deaths))
{
  UN.deaths[i,"d_1960s"] <-(UN.deaths[i,"1960-1965"] + UN.deaths[i,"1965-1970"]) 
  UN.deaths[i,"d_1970s"] <-(UN.deaths[i,"1970-1975"] + UN.deaths[i,"1975-1980"]) 
  UN.deaths[i,"d_1980s"] <-(UN.deaths[i,"1980-1985"] + UN.deaths[i,"1985-1990"]) 
  UN.deaths[i,"d_1990s"] <-(UN.deaths[i,"1990-1995"] + UN.deaths[i,"1995-2000"]) 
} 

#keep onlyv relevabt columns
Z<-UN.deaths[,11:ncol(UN.deaths)]
Y<-UN.deaths[,1:2]
UN.deaths<-cbind(Y,Z)

########################### cleaning TWN data 

## select important cols and rows, labeling them
TWN.data<-TWN.data[c(2:6),c(2:4)]
colnames(TWN.data)<-c("population","births","deaths")
rownames(TWN.data)<-c("Y1960","Y1970","Y1980","Y1990", "Y2000")

#transforming columns with pop data from type ch to type numeric 
for (i in 1:ncol(TWN.data))
{
  TWN.data[,i] <-as.numeric(gsub( ",", "",TWN.data[,i], fixed = TRUE)) 
}  

######## storing TWN data in the deaths, births, and pop data frames 
# remember: code_UN = 900 (i.e. the first row in the data frames) is a place holder for TWN  

for (i in 3:ncol(UN.births))
{
  (UN.births[1,i]<-TWN.data[i-2,2])
  (UN.deaths[1,i]<-TWN.data[i-2,3])
}

for (i in 3:ncol(UN.pop))
{
  (UN.pop[1,i]<-TWN.data[i-2,1])
}

################ merge demographic data (births, deaths, population) 

UN.deaths  <-subset(UN.deaths, select= -country_UN)
UN.births  <-subset(UN.births, select= -country_UN)
UN.demo    <-merge(UN.pop, UN.deaths, by="code_UN")
UN.demo    <-merge(UN.demo, UN.births, by="code_UN")
UN.demo    <-merge(IDs,UN.demo, by="code_UN",sort=T)
UN.demo    <-UN.demo[with(UN.demo, order(id)), ]
colnames(UN.demo)[9:13]<-c("p_1960","p_1970","p_1980","p_1990","p_2000")


########## keep migrant stock data only for the 192 under analysis 
########## merge migrant stock data & demographic data             

Z                <-as.data.frame(UN.demo[,4:ncol(UN.demo)])
colnames(Z)[3]   <-c("iso_dest")
ALL.data         <-merge(Z,WB.stock)
ALL.data         <-ALL.data[with(ALL.data, order(id)), ]

Z                <-as.data.frame(IDs[,c("id", "iso_abel")])
colnames(Z)      <-c("id_o","iso_orig")
ALL.data         <-merge(Z,ALL.data)
ALL.data         <-ALL.data[with(ALL.data, order(id_o,id)), ]
ALL.data         <-subset(ALL.data, select=-c(id_o))

## compute native population (UN pop ith country - total stock ith country) 
## store native pop in the main diagonal (i.e. where iso_org = iso_dest)    

## sum of all migrants in each country in a given decade
V<-aggregate(ALL.data$s_1960s,by=list(Category=ALL.data$id),FUN=sum)
W<-aggregate(ALL.data$s_1970s,by=list(Category=ALL.data$id),FUN=sum)
X<-aggregate(ALL.data$s_1980s,by=list(Category=ALL.data$id),FUN=sum)
Y<-aggregate(ALL.data$s_1990s,by=list(Category=ALL.data$id),FUN=sum)
Z<-aggregate(ALL.data$s_2000s,by=list(Category=ALL.data$id),FUN=sum)

## store native pop in the main diagonal
U<-1
for (i in 1:nrow(ALL.data))
{
  if (ALL.data[i,"iso_orig"]==ALL.data[i,"iso_dest"])
  {
    ALL.data[i,"s_1960s"] <-ALL.data[i,"p_1960"] - V[U,"x"]
    ALL.data[i,"s_1970s"] <-ALL.data[i,"p_1970"] - W[U,"x"]
    ALL.data[i,"s_1980s"] <-ALL.data[i,"p_1980"] - X[U,"x"]
    ALL.data[i,"s_1990s"] <-ALL.data[i,"p_1990"] - Y[U,"x"]
    ALL.data[i,"s_2000s"] <-ALL.data[i,"p_2000"] - Z[U,"x"]
    U                     <-U + 1
  } 
}

################ save decades-long edgelists (with native pop) 

STOCK.60s <- subset(ALL.data, select=c(iso_orig, iso_dest, s_1960s))
STOCK.70s <- subset(ALL.data, select=c(iso_orig, iso_dest, s_1970s))
STOCK.80s <- subset(ALL.data, select=c(iso_orig, iso_dest, s_1980s))
STOCK.90s <- subset(ALL.data, select=c(iso_orig, iso_dest, s_1990s))
STOCK.00s <- subset(ALL.data, select=c(iso_orig, iso_dest, s_2000s))

############### merge edgelist with CEPII dist data 

Z                    <-as.data.frame(UN.demo[,4:ncol(UN.demo)])
colnames(Z)[4]       <-c("iso_d")
CEPII.dist           <-merge(Z,CEPII.all,by="iso_d")
CEPII.dist           <-CEPII.dist[with(CEPII.dist, order(id)), ]

Z                    <-as.data.frame(IDs[,c("id", "iso_cepii")])
colnames(Z)          <-c("id_o","iso_o")
CEPII.dist           <-merge(Z,CEPII.dist,by="iso_o")
CEPII.dist           <-CEPII.dist[with(CEPII.dist, order(id_o,id)), ]
CEPII.dist           <-subset(CEPII.dist, select=c(id_o, iso_o,id,iso_d,distcap))
colnames(CEPII.dist) <-c("id_o","iso_orig","id_d","iso_dest","distcap") 

##### set up births and deaths by decade in one (long) data frame 

W             <-as.data.frame(UN.demo[,c("iso_abel","b_1960s","d_1960s")])
W$period      <-"1960-1970"            
X             <-as.data.frame(UN.demo[,c("iso_abel","b_1970s","b_1970s")])
X$period      <-"1970-1980"  
Y             <-as.data.frame(UN.demo[,c("iso_abel","b_1980s","d_1980s")])
Y$period      <-"1980-1990"  
Z             <-as.data.frame(UN.demo[,c("iso_abel","b_1990s","b_1990s")])
Z$period      <-"1990-2000"  
colnames(W)   <-colnames(X)<-colnames(Y)<-colnames(Z)<-c("iso3","b","d","period")
df0           <-rbind(W,X,Y,Z)
rownames(df0) <-NULL
df0           <- df0[,c("iso3","period","b","d")]

############## labels 

df1           <-subset(IDs,select=c(iso_abel,country,region,region_code))
colnames(df1) <-c("iso3","country","region","reg")

############## distances: from edgelist to matrix 

dd            <-subset(CEPII.dist,select=c(id_o,id_d,distcap))
colnames(dd)  <-c("iso_abel_o","iso_abel_d","distcap")
dd            <-acast(dd, iso_abel_o ~ iso_abel_d, value.var="distcap")
Z             <-unlist(UN.demo[,"iso_abel"])     
colnames(dd)  <-rownames(dd)  <-Z

############## migrant stock with native pop in long form 

V             <-STOCK.60s
V$decade      <-1960
W             <-STOCK.70s
W$decade      <-1970
X             <-STOCK.80s
X$decade      <-1980
Y             <-STOCK.90s
Y$decade      <-1990
Z             <-STOCK.00s
Z$decade      <-2000
colnames(V)   <-colnames(W)<-colnames(X)<-colnames(Y)<-colnames(Z) <-c("orig","dest","stock","decade")
stw           <-rbind(V,W,X,Y,Z)
rownames(stw) <-NULL
stw           <- stw[,c("decade","dest","orig","stock")]

############## sorting stw so that it is ordered as all other data sets 

Z                 <-as.data.frame(IDs[,c("id", "iso_abel")])
colnames(Z)       <-c("id","dest")
stw               <-merge(Z,stw,by="dest")
stw               <-stw[with(stw, order(decade, id)), ]

Z                 <-as.data.frame(IDs[,c("id", "iso_abel")])
colnames(Z)       <-c("id_o","orig")
stw               <-merge(Z,stw,by="orig")
stw               <-stw[with(stw, order(decade,id,id_o)), ]
stw               <-subset(stw, select=c(decade, orig,dest,stock))
stw               <-stw[,c("decade","dest","orig","stock")]
rownames(stw)     <-NULL

#export data
#write.csv(x=stw,file="Global_stocks_with_native_pop_1960-2000_edgelist_AJS.csv",row.names=TRUE)


##### 3. Esimating migration flows #####

#set up empty data.frame (df2) and array (st) to store results
decades  <- unique(df0$period)
years    <- c("1960", "1970", "1980", "1990", "2000" )
iso      <- df1$iso3
df2      <- expand.grid(orig = iso, dest = iso, pob=iso, period=decades)
df2$flow <- NA

## arrays dims nrows = 192, and five time periods (stw$decade)
st <- array(stw$stock, c(nrow(IDs),nrow(IDs),length(unique(stw$decade))), dimnames=list(pob=iso, por=iso, time=years))

#offset for orig X dest X pob tables
g_dist     <- array(NA, c(nrow(IDs),nrow(IDs),nrow(IDs)), dimnames=list(orig=iso, dest=iso, pob=iso))
g_dist[,,] <- 1/dd

## locate position of the the variables "b_1960s" and "d_1960s", 
## the first variable where births and deaths are recorder in WB.demo, respectively
allColLabels<-colnames(UN.demo[,])

for (i in 1:length(allColLabels))
{
  if (allColLabels[i]=="b_1960s")
  {count_b<-i } 
  if (allColLabels[i]=="d_1960s")
  {count_d<-i } 
}

## Main loop to carry out the estimation using the flows-from-stocks methodology using the migest Package (see Abel 2013 for details)

count_p <-1

s<-Sys.time()                                              ### keep track of time
for(p in decades)                                          ### for each period (1960-1970, 1970-1980, etc)
{  
  t0 <- years[count_p]                                     ### store t0 in the ith decade (e.g. 1960 if period is 1960-1970)
  t1 <- years[count_p + 1]                                 ### store t1 in the ith decade (e.g. 1970 if period is 1960-1970)
  message("t0 = P1 = ", t0)                                ### print t0
  message("t1 = P2 = ", t1)                                ### print t1
  message("births data = ",colnames(UN.demo)[count_b])     ### print name of birth data for the ith decade (see UN.demo object)
  message("deaths data = ",colnames(UN.demo)[count_d])     ### print name of death data for the ith decade (see UN.demo object)
  gf <- ffs(                                               ### use fss function, flows from stocks methodology
    P1 = st[,,t0],                                         ### P1= stock data at time t (e.g. 1960)
    P2 = st[,,t1],                                         ### P2= stock data at t1me + 1 (e.g. 1970)
    b = UN.demo[,count_b],                                 ### births in decade p
    d = UN.demo[,count_d],                                 ### deaths in decade p
    m = g_dist,                                            ### geo distances between all countries
    method = "outside"                                     ### this method reproduces the results in Abel 2013  
  )
  df2$flow[df2$period==p] <- round(c(gf$mu))               ### save estimates in df in column "flow"
  count_b <- count_b + 1                                   ### go to births in next decade
  count_d <- count_d + 1                                   ### go to deaths in next decade
  count_p <- count_p + 1                                   ### go to migrant stock in next decade   
}

#Edgelsit: country to country migration flows by decade
FLOWS.all_edgelist      <-df2 %>% group_by(period, orig, dest) %>% summarise(flow = sum(flow)) %>% mutate(flow = ifelse(orig!=dest, flow, 0))

#Export all the data
#write.csv(x=FLOWS.all_edgelist,file="Global_flows_1960-2000_edgelist_AJS.csv",row.names=TRUE)

#from edgelist to array od matrices
FLOWS.all_matrix        <-array(FLOWS.all_edgelist$flow, c(nrow(IDs),nrow(IDs),length(unique(FLOWS.all_edgelist$period))), dimnames=list(pob=iso, por=iso, time=decades))

#create a list to store to store migration flows within the Americas only
FLOWS.americas_matrix   <-vector("list",nrow(IDs.americas)) ## egonets in matrix format with relationship type and including NAs

for (i in 1:length(decades))
{  
  Z<-as.data.frame(t(FLOWS.all_matrix[,,decades[i]]))     ## extract one matrix                           
  Y<-cbind(subset(IDs,select=iso_abel),Z)                 ## add country labels as a column in the matrix  
  X<-merge(IDs.americas,Y,by="iso_abel",sort=F)           ## merge in order to leave on countries in the Americas
  rownames(X)<-X[,"iso_2"]                                ## give row names the names of the countries in the Americas
  X<-X[,(ncol(IDs.americas)+1):ncol(X)]                   ## remove all columns that do not contain flow data
  Z<-t(X)                                                 ## transpose the matrix and do the same as above to get a rectangular matrix
  Y<-cbind(subset(IDs,select=iso_abel),Z)
  X<-merge(IDs.americas,Y,by="iso_abel",sort=F) 
  rownames(X)<-X[,"iso_2"]
  X<-X[,(ncol(IDs.americas)+1):ncol(X)]
  X<-t(X)
  FLOWS.americas_matrix[[i]] <-X                         ## store the resulting matrix in the list FLOWS.americas_matrix 
}

##save the decades-long migration flow matrices for the Americas in csv format
for(i in 1:length(decades))
{
  net  <-(FLOWS.americas_matrix[[i]])
  #write.csv(x=net,file=paste("Americas_flows",decades[i],"matrix_AJS.csv",sep="_"),row.names=T)
}


##### 4. Thresholding/binarization #####

## this loop binarizes the  original/weighted flow matrices based on a given number of top-destinations per country 

for (d in 1:length(decades))                          ## loop through each flow network
{
  for (a in 1:nrow(FLOWS.americas_matrix[[d]]))        ## loop through each row (i.e. sending country)
  {
    for (b in 1:top_destinations)                       ## select top_destination per sending country
    {
      flow<-max(FLOWS.americas_matrix[[d]][a,])          ## store the bth top destination 
      for (c in 1:nrow(FLOWS.americas_matrix[[d]]))      ## loop thorugh each column  
      {
        if ((FLOWS.americas_matrix[[d]][a,c] == flow))    ## if the cell [a,c] == flow
        {
          FLOWS.americas_matrix[[d]][a,c]<- FLOWS.americas_matrix[[d]][a,c] * -1  ## flag that cell (that top destination) with a -1
        } 
      }
    }
  }
  X<-which(FLOWS.americas_matrix[[d]][,]>0)                            ## get the indices/positions of all cell > 0   
  FLOWS.americas_matrix[[d]]<-replace(FLOWS.americas_matrix[[d]],X,0)  ## make the cells identified above = 0
  X<-which(FLOWS.americas_matrix[[d]][,]<0)                            ## get the indices/positions of all cells < 0 (i.e. the top destinations of each country)
  FLOWS.americas_matrix[[d]]<-replace(FLOWS.americas_matrix[[d]],X,1)  ## make the ceels idientifies above = 1
}

####% Import, clean Income Adjusted Index (AICI) data

AICI             <-AICI[-c(1,3),]   ## get rid of unnecessary cells
AICI[1,2]        <-"country"        ## change some labels
X                <-unlist(AICI[1,]) ## create a list of col labels
X[1]             <-"iso_abel"       ## change the label of the first column for future merging
colnames(AICI)   <-X                ## assign X to AICI
AICI             <-AICI[-c(1),]     ## get rid of unnecessary rows
AICI             <-AICI[,-c(3:13)]  ## get rid of unnecessary cols
X                <-colnames(AICI)   ## extract the col labels of AICI

for (i in 2:length(X))              ## append _AICI to each col label in AICI
{
  Y<-X[i]
  Y<-paste(Y,"AICI",sep="_")
  X[i]<-Y
}
colnames(AICI)   <-X                ## attach the new vector of labels to AICI

### merging AICI data with the original nodel-level data set (i.e. IDs.americas)
IDs.americas<- merge(IDs.americas,AICI,by="iso_abel",sort=F) ## merge countries IDs object (i.e. node-level attributes) and AICI

## get labels that are repeated in the original flow matrices and the node-level attribute data set
ZZ<-intersect(colnames(FLOWS.americas_matrix[[1]]),IDs.americas$iso_2)

## drop columns and row that are not in ZZ
for (i in 1:length(decades))
{
  FLOWS.americas_matrix[[i]]<-FLOWS.americas_matrix[[i]][(rownames(FLOWS.americas_matrix[[i]]) %in% ZZ), (colnames(FLOWS.americas_matrix[[i]]) %in% ZZ)]
}

##### 5. clean and process political conflict data and economic (AICI) data #####

### getting rid of unimporant variables
war <-subset(war,select=c(iso_code,year,civtot))

## relabeling first columns for merging
colnames(war)[1] <-"iso_abel"

## merge war data and node-level attributes
war<-merge(IDs.americas,war,by="iso_abel",sort=F)

## sort the merged object
war<-war[with(war, order(id, year)), ]

## create a vector with the needed years of conflict data
Z<-seq(from=1950,to=1999,by=1)

## get rid of rows (i.e. years) that are in Z
for (i in 1:nrow(war))
{
  war<-war[(war$year %in% Z),]
}

## create a new variable in the data frame to store the conflict data
war$conflict<-NA

## sum the number of conflicts (i.e."civtot) for each country in a given decade
for (b in 1:(nrow(war)/10))  
{
  X                <-10*b                         ## year in which a decade ends
  Y                <-X -9                         ## year in which a decade begins  
  war[b*10,"conflict"] <- sum(war[Y:X,"civtot"]) ## sum conflicts in the decade
}

## get rids of NAs (i.e. years different from those who corresponde to the end of a given decade)
war<-war[complete.cases(war$conflict),]

## order the war object
war<-war[with(war, order(year, id)), ]

## create labels for the war object
Z <- c("conflict_50s", "conflict_60s", "conflict_70s", "conflict_80s", "conflict_90s")

## assignt the nnumber of conflicts to each country
for (i in 1:length(Z))
{
  X <- nrow(IDs.americas) * i                            ## row corresponding to a new country
  Y <- X - (nrow(IDs.americas) - 1)                      ## row corresponding to the same counry as above
  IDs.americas<-cbind(war[Y:X,"conflict"],IDs.americas)  ## cbind war (i.e. conflicts) for the ith decade with the node-level data set (IDs.americas)
  colnames(IDs.americas)[1]<-Z[i]                        ## label the new conflict variable
}

#region, language, adjusted GDP per capita, & political violence 
region         <-IDs.americas$region_num
language       <-IDs.americas$language_num
aici           <-vector("list",nrow(IDs.americas))
conflict       <-vector("list",nrow(IDs.americas))

aici_60        <-as.data.frame(as.numeric((IDs.americas[,"1960_AICI"])))
aici_60        <-aici_60*100
aici_65        <-as.data.frame(as.numeric((IDs.americas[,"1965_AICI"])))
aici_65        <-aici_65*100
aici_70        <-as.data.frame(as.numeric((IDs.americas[,"1970_AICI"])))
aici_70        <-aici_70*100 
aici_75        <-as.data.frame(as.numeric((IDs.americas[,"1975_AICI"])))
aici_75        <-aici_75*100 
aici_80        <-as.data.frame(as.numeric((IDs.americas[,"1980_AICI"])))
aici_80        <-aici_80*100 
aici_85        <-as.data.frame(as.numeric((IDs.americas[,"1985_AICI"])))
aici_85        <-aici_85*100 
aici_90        <-as.data.frame(as.numeric((IDs.americas[,"1990_AICI"])))
aici_90        <-aici_90*100 
aici_95        <-as.data.frame(as.numeric((IDs.americas[,"1995_AICI"])))
aici_95        <-aici_95*100
aici_00        <-as.data.frame(as.numeric((IDs.americas[,"2000_AICI"])))
aici_00        <-aici_00*100

aici_60s       <-((aici_60)+(aici_65)+(aici_70)) / 3 # aici in the 1960s
aici_70s       <-((aici_70)+(aici_75)+(aici_80)) / 3 # aici in the 1970s
aici_80s       <-((aici_80)+(aici_85)+(aici_90)) / 3 # aici in the 1980s
aici_90s       <-((aici_90)+(aici_95)+(aici_00)) / 3 # aici in the 1990s

#put the aici variables in the aici list
aici[[1]]      <-aici_60s
aici[[2]]      <-aici_70s
aici[[3]]      <-aici_80s
aici[[4]]      <-aici_90s

#put the conflict variables in the conflict list
conflict[[1]]  <-IDs.americas$conflict_60s
conflict[[2]]  <-IDs.americas$conflict_70s
conflict[[3]]  <-IDs.americas$conflict_80s
conflict[[4]]  <-IDs.americas$conflict_90s

#create empty lists to store results
results        <-vector("list",length(decades)) 
degeneracy     <-vector("list",length(decades)) 
AllNets        <-vector("list",length(decades))

##### 6. Create network objects, do descriptive SNA (Tables in the article), and do sociograms ##### 

#create the network objects, on object per decade, and set key vertex attributes
for (i in 1:length(decades))
{
  #create network objects 
  net <-as.network.matrix(FLOWS.americas_matrix[[i]],matrix.type = "adjacency",directed=T)
  network.vertex.names(net)<-IDs.americas$iso_2
  odegsqrt<-sqrt(degree(net,cmode="outdegree"))
  idegsqrt<-sqrt(degree(net,cmode="indegree"))
  
  # setting vertex attributes
  set.vertex.attribute(net,"region",region)
  set.vertex.attribute(net,"language",language)
  set.vertex.attribute(net,"odegsqrt",odegsqrt)
  set.vertex.attribute(net,"idegsqrt",idegsqrt)
  set.vertex.attribute(net,"aici",aici[i])
  set.vertex.attribute(net,"conflict",conflict[i])
  
  # save the network in the object AllNets
  AllNets[[i]]<-net
}


#Classic SNA: Describe the networks. This reproduces Table 2 if top_destinations > 32

# density
round(gden(AllNets),3)  

#degree centralization
round(centralization(AllNets,degree,mode="digraph",normalize=T),3)

#indegree centralization
round(centralization(AllNets,degree,cmode="indegree",normalize=T),3)

#dyad census
dyad.census.all<-dyad.census(AllNets)  

#triad census
triad.census.all<-triad.census(AllNets) 

#indegree distributions
indegree1<-(degree(AllNets, g=1, gmode="digraph", cmode="indegree"))
indegree2<-(degree(AllNets, g=2, gmode="digraph", cmode="indegree"))
indegree3<-(degree(AllNets, g=3, gmode="digraph", cmode="indegree"))
indegree4<-(degree(AllNets, g=4, gmode="digraph", cmode="indegree"))

#mean indegree
round(mean((indegree1)),2)
round(mean((indegree2)),2)
round(mean((indegree3)),2)
round(mean((indegree4)),2)

#sd of indegree
round(sd((indegree1)),1)
round(sd((indegree2)),1)
round(sd((indegree3)),1)
round(sd((indegree4)),1)

#outdegree distributions
outdegree1<-(degree(AllNets, g=1, gmode="digraph", cmode="outdegree"))
outdegree2<-(degree(AllNets, g=2, gmode="digraph", cmode="outdegree"))
outdegree3<-(degree(AllNets, g=3, gmode="digraph", cmode="outdegree"))
outdegree4<-(degree(AllNets, g=4, gmode="digraph", cmode="outdegree"))

#mean outdegree
mean((outdegree1))
mean((outdegree2))
mean((outdegree3))
mean((outdegree4))

#sd of outdegree
sd((outdegree1))
sd((outdegree2))
sd((outdegree3))
sd((outdegree4))

###### Plots 


#plot degree distributions
hist(indegree1, xlab="Indegree", main="Indegree distribution, 1960s") 
hist(indegree2, xlab="Indegree", main="Indegree distribution, 1970s") 
hist(indegree3, xlab="Indegree", main="Indegree distribution, 1980s") 
hist(indegree4, xlab="Indegree", main="Indegree distribution, 1990s") 

#ploting the networks (see Appendix A.4)
plotCoordinates   <-vector("list",length(decades))

par(mfrow = c(2,2), mar = c(0,0,1,0))
for (i in 1:length(AllNets)) 
{
  plotCoordinates[[i]]<-plot(network(AllNets[[i]]),main=paste("t =",decades[i]),
                             usearrows = T,edge.col="azure2",vertex.cex = 1.5)
}


#assign labels for sociogram
network.vertex.names(AllNets[[4]]) #check old labels
new.net.labels                 <-c("AR", "BS", "BB", "BZ", "BO", 
                                   "BR", "CA", "CL", "CO", "CR", 
                                   "CU", "DO", "EC", "SV", "GT", 
                                   "GY", "HT", "HN", "JA", "MX", 
                                   "NI", "PA", "PY", "PE", "PR", 
                                   "WL", "WV", "SR", "TT", "US", 
                                   "UY", "VE")
#assign new labels
network.vertex.names(AllNets[[4]]) <- new.net.labels
network.vertex.names(AllNets[[3]]) <- new.net.labels
network.vertex.names(AllNets[[2]]) <- new.net.labels
network.vertex.names(AllNets[[1]]) <- new.net.labels


#assign colors for nodes/countries based on geographic region
new.net.colors                 <-c("ivory3", "khaki", "khaki", "violet", "steelblue1", 
                                   "ivory3", "aquamarine1", "ivory3", "steelblue1", "violet", 
                                   "khaki", "khaki",
                                   "steelblue1", "violet", "violet", "khaki", "khaki", "violet", 
                                   "khaki", "aquamarine1", "violet", "violet", "ivory3", "steelblue1",
                                   "khaki", "khaki", "khaki", "khaki", "khaki", "aquamarine1", 
                                   "ivory3", "steelblue1")


#ploting the networks
plotCoordinates   <-vector("list",length(decades))

par(mfrow = c(2,2), mar = c(0,0,1,0))
for (i in 1:length(AllNets)) 
{
  plotCoordinates[[i]]<-gplot(network(AllNets[[i]]),
                            main=paste("t =",decades[i]),
                            usearrows = T,
                            edge.col= rgb(red=0,green=0,blue=0,alpha=0.25),
                            mode="kamadakawai",
                            label.pos=5,
                            displaylabels = T,
                            label.cex = 0.8,
                            vertex.border="grey",
                            vertex.col=new.net.colors,
                            vertex.cex = ((indegree4)/20)+0.6)
}

##### 7. Classify countries by their position in 030T triads (table 5) (results in the article are based on top 10 destinations) ####

### create a data frame to store the number of instances per decade a given node:
##  a) is in the highly attractive position in the 030T it belongs to
##  b) is in the mildly attractive position in the 030T it belongs to
##  c) is in the minimally attractive position in the 030T it belongs to
##  d) is in 030C triads
triads.data.frame<-data.frame(Country=character(),
                              Position_in_triad=character(),
                              Count=integer(),
                              Triad_type=character(),
                              Decade=character(),
                              stringsAsFactors=FALSE)

#this loop goes through each network and gathers infor about countries' positions in 030T triads
#and 030T triads
for (b in 1:length(AllNets))
{
 net.to.analyze<-AllNets[b]
 for (a in 1:length(new.net.labels))
 {
                                         #initially the counts for all countries are 0
  count.030T.ego.highly    <-0
  count.030T.ego.mildly    <-0
  count.030T.ego.minimally <-0
  count.030C.ego           <-0
  country.ego<-a                          #for the ath country
  for (i in 1:length(new.net.labels))
  {
   for (j in 1:length(new.net.labels))
   {
    for (k in 1:length(new.net.labels))
    {
     if ((i != j) & (j != k) & (i != k) & (j > i) & (k > i) & (k > j))  # go though all triangles
     {
      if ((i == country.ego) || (j ==country.ego) || (k ==country.ego)) # if the ath country is any of those triangles
      {
       if (triad.classify(net.to.analyze,tri=c(i,j,k),mode="digraph") == "030T") #check triad i,j,k is a 030T
       {
        #this if loop locates the position (highly, mildly, or minimally attractive) of the ath country in the context of the i,j,k 030T traid                                             
        this.triad<-(AllNets[[4]][c(i,j,k) , c(i,j,k)])
        ego.label<-new.net.labels[country.ego]
        out.ego<-sum(this.triad[ego.label,])
        if(out.ego == 0)
        {
         count.030T.ego.highly<- count.030T.ego.highly +1  #increase the count of the number of times the ath country is in the highly attarctive position in a 030T triad in the bth decade
        }
        if(out.ego == 1)
        {
         count.030T.ego.mildly<- count.030T.ego.mildly +1 #increase the count of the number of times the ath country is in the mildly attarctive position in a 030T triad in the bth decade
        }
        if(out.ego == 2)
        { 
         count.030T.ego.minimally<- count.030T.ego.minimally +1 #increase the count of the number of times the ath country is in the minimally attarctive position in a 030T triad in the bth decade
        }
       }
        #this if loop locates the position (highly, mildly, or minimally attractive) of the ath country in the context of the i,j,k 030T traid                                             
       if (triad.classify(net.to.analyze,tri=c(i,j,k),mode="digraph") == "030C")
       {
        this.triad<-(AllNets[[4]][c(i,j,k) , c(i,j,k)])
        ego.label<-new.net.labels[country.ego]
        out.ego<-sum(this.triad[ego.label,])
        if(out.ego == 1)
        {
         count.030C.ego<- count.030C.ego +1 #increase the count of the number of times the ath country is in a 030C triad triad in the bth decade
        }
       }
      }
     }
    }
   }
  }
  #put all the info together
  triads.1st<-c(new.net.labels[country.ego],"is highly attractive in",count.030T.ego.highly,"030T triads",decades[b])
  triads.2nd<-c(new.net.labels[country.ego],"is mildly attractive in",count.030T.ego.mildly,"030T triads",decades[b])
  triads.3rd<-c(new.net.labels[country.ego],"is minimally attractive in",count.030T.ego.minimally,"030T triads",decades[b])
  triads.4th<-c(new.net.labels[country.ego],"is in",count.030C.ego,"030C triads",decades[b])
  triads.1to.4<-rbind(triads.1st,triads.2nd,triads.3rd,triads.4th)
  triads.data.frame<-rbind(triads.data.frame,triads.1to.4)
 }
}

## this reproduces Table 5 in the paper
colnames(triads.data.frame)<-c("Country","Position","Count","Triad_Type","Decade")

#all the info regarding countries position's in 030T and 030C triads is exported into one data frame
#write.csv(triads.data.frame,file=paste("positions_in_triads","_top_",top_destinations,"_destinations",".csv",sep=""),row.names = F)

##### 8. Test of Hypothesis 3 (based on Table 5) #####

#test hyhypothesis 3: correlation between economic well-being and structural attractivenness

#extract information reharding countries' positions in 030T triads
highly.att<-subset(triads.data.frame, Position == "is highly attractive in",   select=c(Country,Count))
mildly.att<-subset(triads.data.frame, Position == "is mildly attractive in",   select=c(Count))
minima.att<-subset(triads.data.frame, Position == "is minimally attractive in",select=c(Count))
attract<-cbind(highly.att,mildly.att,minima.att)
colnames(attract)<-c("Country","highly","mildly","minimally")
colnames(aici_60s)<-colnames(aici_70s)<-colnames(aici_80s)<-colnames(aici_90s)<-"All_AICIS"

#extract information regarding countries' GDP
all.aicis<-rbind(aici_60s,aici_70s,aici_80s,aici_90s)
all.hdis<-cbind(IDs.americas[,c("iso_abel")],as.matrix(new.net.labels))
colnames(all.hdis)<-c("iso_code","iso_code_2")

#extract information regardings countries' Historical Index of Human Development (HDI) (Padros de la Escodura 2015; see Appendix A.2 for details).
HDI.labels<-(HDI[2,])
HDI.labels<-unlist(HDI.labels)
colnames(HDI)<-HDI.labels

all.hdis<-merge(all.hdis,HDI,by="iso_code")

all.hdis$hdi_60s  <-NaN
all.hdis$hdi_70s  <-NaN
all.hdis$hdi_80s  <-NaN
all.hdis$hdi_90s  <-NaN

#compute average HDI per deacde. Some warnings come up because three countries (St. Lucia, Suriname, St. Vincent and the Granadines) do not have complete info
for (ttt in 1:nrow(all.hdis))
{
 all.hdis[ttt,"hdi_60s"]  <-(mean(c(as.numeric(all.hdis[ttt,"1960"]), as.numeric(all.hdis[ttt,"1965"]) , as.numeric(all.hdis[ttt,"1970"])),na.rm = T)) * 100
 all.hdis[ttt,"hdi_70s"]  <-(mean(c(as.numeric(all.hdis[ttt,"1970"]), as.numeric(all.hdis[ttt,"1975"]) , as.numeric(all.hdis[ttt,"1980"])),na.rm = T)) * 100
 all.hdis[ttt,"hdi_80s"]  <-(mean(c(as.numeric(all.hdis[ttt,"1980"]), as.numeric(all.hdis[ttt,"1985"]) , as.numeric(all.hdis[ttt,"1990"])),na.rm = T)) * 100
 all.hdis[ttt,"hdi_90s"]  <-(mean(c(as.numeric(all.hdis[ttt,"1990"]), as.numeric(all.hdis[ttt,"1995"]) , as.numeric(all.hdis[ttt,"2000"])),na.rm = T)) * 100
}

#create unique objects for each decade
hdi_60s<-as.data.frame(all.hdis$hdi_60s) 
hdi_70s<-as.data.frame(all.hdis$hdi_70s) 
hdi_80s<-as.data.frame(all.hdis$hdi_80s) 
hdi_90s<-as.data.frame(all.hdis$hdi_90s) 

#relabel objects
colnames(hdi_60s)<-colnames(hdi_70s)<-colnames(hdi_80s)<-colnames(hdi_90s)<-"All_HDIS"

all.hdis<-rbind(hdi_60s,hdi_70s,hdi_80s,hdi_90s)

#create a final data frame with countries' position in the 030T triads, GDP, and HDI
attract<-cbind(highly.att,mildly.att,minima.att,all.aicis,all.hdis)
colnames(attract)<-c("Country","highly","mildly","minimally","aicis","hdis")
attract[,"highly"]<-(as.numeric(as.character(attract[,"highly"])))
attract[,"mildly"]<-(as.numeric(as.character(attract[,"mildly"])))
attract[,"minimally"]<-(as.numeric(as.character(attract[,"minimally"])))

#compute correlation between positions in 030T triads and GDP
cor.highly.aici<-cor(attract[,"highly"],attract[,"aicis"])
cor.mildly.aici<-cor(attract[,"mildly"],attract[,"aicis"])
cor.minima.aici<-cor(attract[,"minimally"],attract[,"aicis"])

#Compute correlations between positions in 030T and HDI
cor.highly.hdi<-cor(attract[,"highly"],attract[,"hdis"],use="complete.obs")
cor.mildly.hdi<-cor(attract[,"mildly"],attract[,"hdis"],use="complete.obs")
cor.minima.hdi<-cor(attract[,"minimally"],attract[,"hdis"],use="complete.obs")

#find the p-value associated with the null of no correlation between GDP and structural attractiveness
cor.test(attract[,"highly"],attract[,"aicis"])
cor.test(attract[,"mildly"],attract[,"aicis"])
cor.test(attract[,"minimally"],attract[,"aicis"])

#find the p-value associated with the null of no correlation between HDI and structural attractiveness
cor.test(attract[,"highly"],attract[,"hdis"])
cor.test(attract[,"mildly"],attract[,"hdis"])
cor.test(attract[,"minimally"],attract[,"hdis"])


##### 9. Bottom part of Table 2: simulated dyad census conditioned-graphs for each network #####

set.seed(5)

#create an object to store results
triads.simu<-as.data.frame(matrix(0,ncol=1,nrow=80))
flag.triads.simu<-0

for (tu in 1:nrow(dyad.census.all)) ## for each time period
{
  #create a list to store the simulatd nets
  g        <-vector("list",sim.dyad.census) 
  
  
  #simulate 500 (i.e., sym.dyad.census) random graphs conditioned on the observed dyad census in a given decade
  for (i in 1:length(g))
  {
    g[[i]]<-as.sociomatrix(rguman(n=1, 
                                  nrow(hdi_60s),    ## number of nodes
                                  mut =  round(dyad.census.all[tu,1]/sum(dyad.census.all[tu,]),2), ## proportion of mutual dyads in the 60s
                                  asym = round(dyad.census.all[tu,2]/sum(dyad.census.all[tu,]),2), ## proprotion of asymmetric dyads in the 60s         
                                  null = round(dyad.census.all[tu,3]/sum(dyad.census.all[tu,]),2), ## proportion of null dyads in the 60s
                                  method = "probability", 
                                  return.as.edgelist = FALSE))
  }
  
  
  #compute triad census for each one of the dyad census-conditioned random graphs and for the obserbed network
  
  triads<-triad.census(AllNets[[tu]], mode="digraph")
  
  for(i in 1:length(g))
  {
    triads.i<-triad.census(g[[i]], mode="digraph")
    triads<-rbind(triads,triads.i)
  }
  
  #labeling the triads object
  colnames(triads)<-c("T-003","T-012","T-102","T-021D","T-021U",   
                      "T-021C", "T-111D", "T-111U", "T-030T","T-030C",
                      "T-201", "T-120D", "T-120U", "T-120C", "T-210",
                      "T-300")
  
  #print means and sd of relevant triadic configurations of simulated networks
  
  #030T triads
  flag.triads.simu<- flag.triads.simu + 1
  triads.simu[flag.triads.simu,1]<-paste("Period ",tu,". Mean, SD, and observed 030T triads. =",sep="")
  flag.triads.simu<- flag.triads.simu + 1
  triads.simu[flag.triads.simu,1]<-round(mean(triads[2:nrow(triads),"T-030T"]),2)
  flag.triads.simu<- flag.triads.simu + 1
  triads.simu[flag.triads.simu,1]<-round(sd(triads[2:nrow(triads),"T-030T"]),1)
  flag.triads.simu<- flag.triads.simu + 1
  triads.simu[flag.triads.simu,1]<-triads[1,"T-030T"] # observed count
  
  #030C triads
  flag.triads.simu<- flag.triads.simu + 1
  triads.simu[flag.triads.simu,1]<-paste("Period ",tu,". Mean, SD, and observed 030C triads. =",sep="")
  flag.triads.simu<- flag.triads.simu + 1
  triads.simu[flag.triads.simu,1]<-round(mean(triads[2:nrow(triads),"T-030C"]),2)
  flag.triads.simu<- flag.triads.simu + 1
  triads.simu[flag.triads.simu,1]<-round(sd(triads[2:nrow(triads),"T-030C"]),1)
  flag.triads.simu<- flag.triads.simu + 1
  triads.simu[flag.triads.simu,1]<-triads[1,"T-030C"] # observed count
  
  #021U triads
  flag.triads.simu<- flag.triads.simu + 1
  triads.simu[flag.triads.simu,1]<-paste("Period ",tu,". Mean, SD, and observed 021U triads. =",sep="")
  flag.triads.simu<- flag.triads.simu + 1
  triads.simu[flag.triads.simu,1]<-round(mean(triads[2:nrow(triads),"T-021U"]),2)
  flag.triads.simu<- flag.triads.simu + 1
  triads.simu[flag.triads.simu,1]<-round(sd(triads[2:nrow(triads),"T-021U"]),1)
  flag.triads.simu<- flag.triads.simu + 1
  triads.simu[flag.triads.simu,1]<-triads[1,"T-021U"] # observed count
  
  #021D triads
  flag.triads.simu<- flag.triads.simu + 1
  triads.simu[flag.triads.simu,1]<-paste("Period ",tu,". Mean, SD, and observed 021D triads. =",sep="")
  flag.triads.simu<- flag.triads.simu + 1
  triads.simu[flag.triads.simu,1]<-round(mean(triads[2:nrow(triads),"T-021D"]),2)
  flag.triads.simu<- flag.triads.simu + 1
  triads.simu[flag.triads.simu,1]<-round(sd(triads[2:nrow(triads),"T-021D"]),1)
  flag.triads.simu<- flag.triads.simu + 1
  triads.simu[flag.triads.simu,1]<-triads[1,"T-021D"] # observed count
  
  #021C triads
  flag.triads.simu<- flag.triads.simu + 1
  triads.simu[flag.triads.simu,1]<-paste("Period ",tu,". Mean, SD, and observed 021C triads. =",sep="")
  flag.triads.simu<- flag.triads.simu + 1
  triads.simu[flag.triads.simu,1]<-round(mean(triads[2:nrow(triads),"T-021C"]),2)
  flag.triads.simu<- flag.triads.simu + 1
  triads.simu[flag.triads.simu,1]<-round(sd(triads[2:nrow(triads),"T-021C"]),1)
  flag.triads.simu<- flag.triads.simu + 1
  triads.simu[flag.triads.simu,1]<-triads[1,"T-021C"] # observed count
}

#export data
#write.csv(triads.simu,file=paste("triads_simulated_counts","_top_",top_destinations,"_destinations",".csv",sep=""),row.names = F)

##### 10. Compute conflict, income, and structural attractiveness variables #####

## create objects containing the networks (i.e., the DVs)
t1<-FLOWS.americas_matrix[[1]]
t2<-FLOWS.americas_matrix[[2]]
t3<-FLOWS.americas_matrix[[3]]
t4<-FLOWS.americas_matrix[[4]]

#stroring the matrices in a list
flows<-mget(paste0("t",1:4))

## create objects containing the confict data 
conflict_60s<-as.numeric(IDs.americas$conflict_60s)
conflict_70s<-as.numeric(IDs.americas$conflict_70s)
conflict_80s<-as.numeric(IDs.americas$conflict_80s)
conflict_90s<-as.numeric(IDs.americas$conflict_90s)

#create objects containing GDP data
aici_60s<-((as.numeric(IDs.americas$`1960_AICI`) * 1000) + (as.numeric(IDs.americas$`1965_AICI`) * 1000) ) / 2
aici_70s<-((as.numeric(IDs.americas$`1970_AICI`) * 1000) + (as.numeric(IDs.americas$`1975_AICI`) * 1000) ) / 2
aici_80s<-((as.numeric(IDs.americas$`1980_AICI`) * 1000) + (as.numeric(IDs.americas$`1985_AICI`) * 1000) ) / 2
aici_90s<-((as.numeric(IDs.americas$`1990_AICI`) * 1000) + (as.numeric(IDs.americas$`1995_AICI`) * 1000) ) / 2


#the following lines of code are developed to create the "type" variable and indicator of the model position a given country occupied in the 030T triads it was part of in a given decade
#this variable is aimed as a robustness check to analyze statys dynamics related to countries attractivess within the TERGM framework (i.e., hypothesis 3 in the article)

# aff postion, count and decade variables to the triads.data.frame
triads.data.frame[,"Position"] <-as.character(triads.data.frame[,"Position"])
triads.data.frame[,"Count"]    <-as.numeric(as.character(triads.data.frame[,"Count"]))
triads.data.frame[,"Decade"]   <-as.character(triads.data.frame[,"Decade"])

#retain info re: countries' positions as highly structurally attractive
subsetted.triads60s.highly<-subset(triads.data.frame, Position=="is highly attractive in" & Decade == "1960-1970",select=c(Count))
subsetted.triads70s.highly<-subset(triads.data.frame, Position=="is highly attractive in" & Decade == "1970-1980",select=c(Count))
subsetted.triads80s.highly<-subset(triads.data.frame, Position=="is highly attractive in" & Decade == "1980-1990",select=c(Count))
subsetted.triads90s.highly<-subset(triads.data.frame, Position=="is highly attractive in" & Decade == "1990-2000",select=c(Count))

#retain info re: countries' positions as mildly structurally attractive
subsetted.triads60s.mildly<-subset(triads.data.frame, Position=="is mildly attractive in" & Decade == "1960-1970",select=c(Count))
subsetted.triads70s.mildly<-subset(triads.data.frame, Position=="is mildly attractive in" & Decade == "1970-1980",select=c(Count))
subsetted.triads80s.mildly<-subset(triads.data.frame, Position=="is mildly attractive in" & Decade == "1980-1990",select=c(Count))
subsetted.triads90s.mildly<-subset(triads.data.frame, Position=="is mildly attractive in" & Decade == "1990-2000",select=c(Count))

#retain info re: countries' positions as minimially structurally attractive
subsetted.triads60s.minimally<-subset(triads.data.frame, Position=="is minimally attractive in" & Decade == "1960-1970",select=c(Count))
subsetted.triads70s.minimally<-subset(triads.data.frame, Position=="is minimally attractive in" & Decade == "1970-1980",select=c(Count))
subsetted.triads80s.minimally<-subset(triads.data.frame, Position=="is minimally attractive in" & Decade == "1980-1990",select=c(Count))
subsetted.triads90s.minimally<-subset(triads.data.frame, Position=="is minimally attractive in" & Decade == "1990-2000",select=c(Count))

#put all info re: countries' structural attractiveness during the 60s in one object
cat.attr_60s<-cbind(subsetted.triads60s.highly,subsetted.triads60s.mildly,subsetted.triads60s.minimally)
cat.attr_60s<-as.data.frame(cat.attr_60s)
cat.attr_60s$type<-""
colnames(cat.attr_60s)<-c("highly","mildly","minimally","type")

#put all info re: countries' structural attractiveness during the 70s in one object
cat.attr_70s<-cbind(subsetted.triads70s.highly,subsetted.triads70s.mildly,subsetted.triads70s.minimally)
cat.attr_70s<-as.data.frame(cat.attr_70s)
cat.attr_70s$type<-""
colnames(cat.attr_70s)<-c("highly","mildly","minimally","type")

#put all info re: countries' structural attractiveness during the 80s in one object
cat.attr_80s<-cbind(subsetted.triads80s.highly,subsetted.triads80s.mildly,subsetted.triads80s.minimally)
cat.attr_80s<-as.data.frame(cat.attr_80s)
cat.attr_80s$type<-""
colnames(cat.attr_80s)<-c("highly","mildly","minimally","type")

#put all info re: countries' structural attractiveness during the 90s in one object
cat.attr_90s<-cbind(subsetted.triads90s.highly,subsetted.triads90s.mildly,subsetted.triads90s.minimally)
cat.attr_90s<-as.data.frame(cat.attr_90s)
cat.attr_90s$type<-""
colnames(cat.attr_90s)<-c("highly","mildly","minimally","type")

#combine all decade-specific structural attractivenss data sets in one list
all.cat.attr<-list(cat.attr_60s,cat.attr_70s,cat.attr_80s,cat.attr_90s)
status.all  <-list()

#create the status variable understood as the modal position of each country in given decade
for (yu in 1:length(all.cat.attr))
{
  for (yi in 1:(nrow(all.cat.attr[[yu]])))
  {
    if ((all.cat.attr[[yu]][yi,"highly"]>=all.cat.attr[[yu]][yi,"mildly"]) & (all.cat.attr[[yu]][yi,"highly"]>=all.cat.attr[[yu]][yi,"minimally"]))
    {
      all.cat.attr[[yu]][yi,"type"] <- 1  #if the modal position of country yi on decade yu is the highly attractive one, assign a 1 to the "type" variable for this country
    }
    
    if ((all.cat.attr[[yu]][yi,"mildly"]>all.cat.attr[[yu]][yi,"highly"]) & (all.cat.attr[[yu]][yi,"mildly"]>=all.cat.attr[[yu]][yi,"minimally"]))
    {
      all.cat.attr[[yu]][yi,"type"] <- 2 #if the modal position of country yi on decade yu is the mildly attractive one, assign a 2 to the "type" variable for this country
    }
    
    if ((all.cat.attr[[yu]][yi,"minimally"]>all.cat.attr[[yu]][yi,"highly"]) & (all.cat.attr[[yu]][yi,"minimally"]>all.cat.attr[[yu]][yi,"mildly"]))
    {
      all.cat.attr[[yu]][yi,"type"] <- 3 #if the modal position of country yi on decade yu is the minimally attractive one, assign a 3 to the "type" variable for this country
    }
  }
  status<-aici_60s  #after going through all countries in a given devase, create the status variable. Make it a copy of aici_60s
  status<-as.numeric(as.vector(all.cat.attr[[yu]]$type)) #replace the informtion in the status variable by the info contained in the type column
  status.all[[yu]]<-status #
}


#create an object with all the relevant node-level covariates, including several different forms to ID countries (e.g., UN IDs. CEPII IDs, etc.)
all_covariates<-cbind(IDs.americas[,"americas_3"],IDs.americas[,"americas_region"],IDs.americas[,"iso_2"],
                    IDs.americas[,"region_num"],IDs.americas[,"language_num"],IDs.americas[,"language"],
                    IDs.americas[,"americas_3"],IDs.americas[,"country_AICI"],IDs.americas[,"iso_abel"],
                    IDs.americas[,"id"],IDs.americas[,"code_UN"],IDs.americas[,"iso_cepii"],
                    aici_60s,aici_70s,aici_80s,aici_90s,
                    conflict_60s,conflict_70s,conflict_80s,conflict_90s,
                    status.all[[1]],
                    status.all[[2]],
                    status.all[[3]],
                    status.all[[4]]
                    )

#add labels to the key covariates
colnames(all_covariates)<-c("americas_3","americas_region","iso_2","region_num","language_num","language",
                            "americas_3","country_AICI","iso_abel","id","code_UN","iso_cepii",
                            "income_60s","income_70s","income_80s","income_90s","conflict_60s","conflict_70s","conflict_80s",
                            "conflict_90s","struct_attrac_60s","struct_attrac_70s","struct_attrac_80s","struct_attrac_90s")

#export node-level covariates
#write.csv(x=all_covariates,file=paste("node_level_covars_AJS_top_",top_destinations,"_destinations",".csv",sep=""),row.names=T)

##### 11. Preprocess data to fit TERGMs #####

#preprocess data using XERGM's built in function
dep<- preprocess(flows,aici_60s,aici_70s,aici_80s,aici_90s,
                 conflict_60s,conflict_70s,conflict_80s,conflict_90s,
                 as.numeric(as.character(cat.attr_60s[,1])),
                 as.numeric(as.character(cat.attr_70s[,1])),
                 as.numeric(as.character(cat.attr_80s[,1])),
                 as.numeric(as.character(cat.attr_90s[,1])),
                 language,region, 
                 lag=T,covariate=F,na=NA,na.method="fillmode")

#checking the resutls, the dependnet networks shouls start at time 2 because
# the estimation is contidioned on the first network
length(dep)
sapply(flows,dim)
sapply(dep,dim)
rownames(dep[[3]])

#creating a "memory" term for existing and non-existing edges
mem.stability<- preprocess(flows,
                           lag=T,covariate=T,memory="stability", na=NA,na.method="fillmode")

#checking the resutls. The last time step should be removed and dimensions
#should be adjusted.
length(mem.stability)
sapply(mem.stability,dim)
sapply(mem.stability,dim)
rownames(mem.stability[[3]])

#covariate: region of the world
region.cov<-preprocess(region, flows,
                       lag=F,covariate=T)

#covariate: language
language.cov<-preprocess(language, flows,
                         lag=F,covariate=T)

#single-period delayed reciprocity:
#transpose the flow matrices and create a lagged reciprocity covariate
delrecip<-lapply(flows,t)
delrecip<-preprocess(delrecip,
                     lag=T,covariate=T,
                     na=NA,na.method="fillmode")

#adding, other variables: 

for (i in 1:length(dep))
{
  dep[[i]] <-network(dep[[i]])                        #for the ith network, compute
  odegsqrt<-sqrt(degree(dep[[i]],cmode="outdegree"))   #sqrt(outdegree)
  odeg.act<-(degree(dep[[i]],cmode="outdegree"))^1.5   #outdegree activity
  idegsqrt<-sqrt(degree(dep[[i]],cmode="indegree"))    #sqrt(indegree)
  dep[[i]]<-set.vertex.attribute(dep[[i]],"odegsqrt",odegsqrt) #add outdegree as a network attribute
  dep[[i]]<-set.vertex.attribute(dep[[i]],"idegsqrt",idegsqrt) #add indegree as a network attribute
  dep[[i]]<-set.vertex.attribute(dep[[i]],"odeg.act",odeg.act) #add outdegree activity as a network attribute  
  dep[[i]]<-set.vertex.attribute(dep[[i]],"region",region.cov[[i]]) #add region as a network attribute
  dep[[i]]<-set.vertex.attribute(dep[[i]],"language",language.cov[[i]]) #add language as a network attribute
}

#add (lagged) income as a covariate
dep[[1]]<-set.vertex.attribute(dep[[1]],"aici",aici_60s)   
dep[[2]]<-set.vertex.attribute(dep[[2]],"aici",aici_70s)
dep[[3]]<-set.vertex.attribute(dep[[3]],"aici",aici_80s)

#add (lagged) conflict as a covariate
dep[[1]]<-set.vertex.attribute(dep[[1]],"conflict",conflict_60s)
dep[[2]]<-set.vertex.attribute(dep[[2]],"conflict",conflict_70s)
dep[[3]]<-set.vertex.attribute(dep[[3]],"conflict",conflict_80s)

#add (lagged) status (structural attractiveness * substative attractiveness)
dep[[1]]<-set.vertex.attribute(dep[[1]],"status",as.numeric(as.character(cat.attr_60s[,1]))*aici_60s)
dep[[2]]<-set.vertex.attribute(dep[[2]],"status",as.numeric(as.character(cat.attr_70s[,1]))*aici_70s)
dep[[3]]<-set.vertex.attribute(dep[[3]],"status",as.numeric(as.character(cat.attr_80s[,1]))*aici_80s)


#checking the dependent networks
#first extract the networks
dependent.t2<-dep$t2
dependent.t3<-dep$t3
dependent.t4<-dep$t4

#sumarize the networks
dependent.t2
dependent.t3
dependent.t4

#checking some attributes 
get.vertex.attribute(dependent.t2,attrname="aici")
get.vertex.attribute(dependent.t3,attrname="conflict")
get.vertex.attribute(dependent.t3,attrname="odeg.act")
get.vertex.attribute(dependent.t4,attrname="odeg.act")
get.vertex.attribute(dependent.t4,attrname="status")

##### 12. fit TERGMs #####

## Final/Authoritative Model: 
## This reproduces table 3 in the paper if top_destinations > 32
## This reproduces table 4 in the paper, panel A if top_destinations = 10
## This reproduces table 4 in the paper, panel B if top_destinations = 15
## This reproduces table 4 in the paper, panel C if top_destinations = 20
model <-btergm(dep ~
                 ctriple+
                 ttriple+
                 mutual +
                 edgecov(delrecip) +
                 edgecov(mem.stability)+
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


## Robustness check: Final/authoritative model + outdegree activity
## This reproduces the results reported in footnote 14 if if top_destinations = 10 
model2 <-btergm(dep ~
                 ctriple+
                 ttriple+
                 mutual +
                 edgecov(delrecip) +
                 edgecov(mem.stability)+
                 idegree1.5 +       
                 odegree1.5 +       
                 nodecov("odeg.act")+
                 absdiff("odegsqrt")+
                 nodematch("region") +
                 nodematch("language") +
                 nodeicov("aici")+
                 nodeocov("aici")+
                 nodeicov("conflict")+
                 nodeocov("conflict")+
                 edges,
               parallel="snow",ncpus=30,R=250000)
#summary TERGM
summary(model2,level=0.90)  # p-val <.1
summary(model2,level=0.95)  # p-val <.05
summary(model2,level=0.995) # p-val <.01
summary(model2,level=0.999) # p-val <.001


## Extra check for hypothesis 3: Final/authoritative model + (structural attractiveness * substantive attractivness)
## This reproduces Table 4 in the paper if top_destinations = 10
model3 <-btergm(dep ~
                 ctriple+
                 ttriple+
                 mutual +
                 edgecov(delrecip) +
                 edgecov(mem.stability)+
                 idegree1.5 +       
                 odegree1.5 +       
                 absdiff("odegsqrt")+
                 nodeicov("status")+
                 nodematch("region") +
                 nodematch("language") +
                 nodeicov("aici")+
                 nodeocov("aici")+
                 nodeicov("conflict")+
                 nodeocov("conflict")+
                 edges,
               parallel="snow",ncpus=30,R=250000)


#summary TERGM
summary(model3,level=0.90)  # p-val <.1
summary(model3,level=0.95)  # p-val <.05
summary(model3,level=0.995) # p-val <.01
summary(model3,level=0.999) # p-val <.001

## Robustness check: Final/authoritative model + 120C triad
## This reproduces Table C1 in the paper if top_destinations > 32
model4 <-btergm(dep ~
                  ctriple+
                  ttriple+
                  mutual +
                  edgecov(delrecip) +
                  edgecov(mem.stability)+
                  idegree1.5 +       
                  odegree1.5 +       
                  triadcensus(levels=13)+
                  absdiff("odegsqrt")+
                  nodematch("region") +
                  nodematch("language") +
                  nodeicov("aici")+
                  nodeocov("aici")+
                  nodeicov("conflict")+
                  nodeocov("conflict")+
                  edges,
                parallel="snow",ncpus=30,R=250000)
#summary TERGM
summary(model4,level=0.90)  # p-val <.1
summary(model4,level=0.95)  # p-val <.05
summary(model4,level=0.995) # p-val <.01
summary(model4,level=0.999) # p-val <.001

##### 13. Goodness of fit and model comparison #####

##compare models with or without outdegree activity?

set.seed(5)

#call the simulate.formula from the ergm package
simulate.formula <- ergm:::simulate.formula

#model without outdegree acivity
gof.tergm1         <-gof(model,nsim=500,MCMC.interval=50,classicgof=F,
                         MCMC.burnin=10000,MCM.samplesize=5000,ncpus=30)

#model with indegree activiy
gof.tergm1_odegact    <-gof(model2,nsim=500,MCMC.interval=50,classicgof=F,
                      MCMC.burnin=10000,MCM.samplesize=5000,ncpus=30)

#compare fit of both models and check for degeneracy, all periods (both degeneracy checks and GOF checks favor the authoritative model)
gof.tergm1         
gof.tergm1_odegact

#plot AUC ROC for all periods (press enter to see both plots)
plot(gof.tergm1)


## ckech goodness-of-fit: classic statnet measures by period
# 1990s

gof.tergmt4       <-gof(model,nsim=500,MCM.interval=50,
                      predict.period =4,
                      MCMC.burnin=10000,MCM.samplesize=5000,ncpus=30,
                      classicgof=T
                      ,simulate(model,sequential=F))

#1980s

gof.tergmt3       <-gof(model,nsim=500,MCM.interval=50,
                        predict.period =3,
                        MCMC.burnin=10000,MCM.samplesize=5000,ncpus=30,
                        classicgof=T
                        ,simulate(model,sequential=F))

#1970s

gof.tergmt2       <-gof(model,nsim=500,MCM.interval=50,
                        predict.period =2,
                        MCMC.burnin=10000,MCM.samplesize=5000,ncpus=30,
                        classicgof=T
                        ,simulate(model,sequential=F))


##### Check all GOF plots (#Press ENTER to see all plots)
plot(gof.tergmt4)  
plot(gof.tergmt3)  
plot(gof.tergmt2)  


#save results
print(Sys.time()-s)                                        ### print how much time did the loop take
session.info<-sessionInfo()

### save image
save.image(file = paste("All_objects_WB_top_",top_destinations,".RData",sep=""))
