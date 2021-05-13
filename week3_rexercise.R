library(readr)        # to import tabular data (e.g. csv)
library(dplyr)        # to manipulate (tabular) data
library(ggplot2)      # to visualize data

#install.packages("gghighlight")
library(gghighlight)

#install.packages("SimilarityMeasures")
library(SimilarityMeasures)

caro <- read_delim("caro60.csv",",") # adjust path
caro

## Task 1: Segmentation
caro <- caro %>%
  mutate(
    nMinus3 = sqrt((lag(E,3)-E)^2+(lag(N,3)-N)^2),   # distance to pos -3 minutes
    nMinus2 = sqrt((lag(E,2)-E)^2+(lag(N,2)-N)^2),   # distance to pos -2 minutes
    nMinus1 = sqrt((lag(E,1)-E)^2+(lag(N,1)-N)^2),   # distance to pos -1 minutes
    nPlus1  = sqrt((E-lead(E,1))^2+(N-lead(N,1))^2), # distance to pos +1 mintues
    nPlus2  = sqrt((E-lead(E,2))^2+(N-lead(N,2))^2),  # distance to pos +2 minutes
    nPlus3  = sqrt((E-lead(E,3))^2+(N-lead(N,3))^2) # distance to pos +3 minutes
    )

## Task 2: Specify and apply threshold d
caro <- caro %>%
  rowwise() %>%
  mutate(
    stepMean = mean(c(nMinus3, nMinus2, nMinus1,nPlus1,nPlus2, nPlus3))
  ) %>%
  ungroup() 

summary(caro)


caro <- caro %>% 
  ungroup() %>%
  mutate(static = stepMean < mean(stepMean, na.rm = TRUE)) # For the steps smaller than stepmean, caro is not moving (static=TRUE=STOP)


## Task 3 Visualize segmented trajectories
p_caro<-ggplot(data=caro, aes(E,N))+
  geom_point(aes(color=static))+
  geom_path()+
  coord_equal()
p_caro


## Task 4: Segment-based analysis
rle_id <- function(vec){
  x <- rle(vec)$lengths
  as.factor(rep(seq_along(x), times=x))
}

caro <- caro%>%
  mutate(segment_id = rle_id(static))
caro


caro_filter <- caro %>%
  filter(!static)

p2_caro<-ggplot(data=caro_filter, aes(E,N,color=segment_id))+
  geom_point()+
  geom_path()+
  coord_equal()+
  theme(legend.position = "bottom")
p2_caro

caro_filter_longsegements<-caro_filter[caro_filter$segment_id %in% names(which(table(caro_filter$segment_id) >6)), ]
p3_caro<-ggplot(data=caro_filter_longsegements, aes(E,N,color=segment_id))+
  geom_point()+
  geom_path()+
  coord_equal()+
  theme(legend.position = "bottom")
p3_caro

# comment: I chose to filter out segments <=6 minutes. I have a bit different segments than the plots in the exercise-description. This might come from different thresholds chosen...


## Task 5 
pedestrian <- read_delim("pedestrian.csv",",") # adjust path
pedestrian
pedestrian$TrajID<-as.factor(pedestrian$TrajID)
pedestrian$TrajID2<-pedestrian$TrajID

p_pedestrian<- ggplot(data=pedestrian, aes(E,N))+
  geom_point(data=pedestrian[,2:5], color="grey")+
  geom_point(aes(color=TrajID))+
  geom_path(aes(color=TrajID))+
  facet_wrap(.~TrajID)+
  coord_equal()
p_pedestrian


## Task 6: calculate similarity
help(package = "SimilarityMeasures")
?SimilarityMeasures
?LCSS()
library(sf)

pedestrian1<-pedestrian%>%
  filter(TrajID=="1")
pedestrian1<-pedestrian1[,2:3]

pedestrian2<-pedestrian%>%
  filter(TrajID=="2")
pedestrian2<-pedestrian2[,2:3]

pedestrian3<-pedestrian%>%
  filter(TrajID=="3")
pedestrian3<-pedestrian3[,2:3]

pedestrian4<-pedestrian%>%
  filter(TrajID=="4")
pedestrian4<-pedestrian4[,2:3]

pedestrian5<-pedestrian%>%
  filter(TrajID=="5")
pedestrian5<-pedestrian5[,2:3]

pedestrian6<-pedestrian%>%
  filter(TrajID=="6")
pedestrian6<-pedestrian6[,2:3]


pedestrian1<-as.matrix(pedestrian1)  
pedestrian2<-as.matrix(pedestrian2)
pedestrian3<-as.matrix(pedestrian3)  
pedestrian4<-as.matrix(pedestrian4)
pedestrian5<-as.matrix(pedestrian5)  
pedestrian6<-as.matrix(pedestrian6)

DTW1<-DTW(pedestrian1,pedestrian1)
DTW2<-DTW(pedestrian1,pedestrian2)
DTW3<-DTW(pedestrian1,pedestrian3) # trajectory 3 is least similar to trajectory one in terms of DTW?
DTW4<-DTW(pedestrian1,pedestrian4)
DTW5<-DTW(pedestrian1,pedestrian5)
DTW6<-DTW(pedestrian1,pedestrian6)

DTW<-c(DTW1,DTW2,DTW3,DTW4,DTW5,DTW6)

barplot(DTW)

ED1<-EditDist(pedestrian1,pedestrian1)
ED2<-EditDist(pedestrian1,pedestrian2)
ED3<-EditDist(pedestrian1,pedestrian3) # trajectory 3 needs the most edits to align with trajectory 1?
ED4<-EditDist(pedestrian1,pedestrian4)
ED5<-EditDist(pedestrian1,pedestrian5)
ED6<-EditDist(pedestrian1,pedestrian6)

F1<-Frechet(pedestrian1,pedestrian1)
F2<-Frechet(pedestrian1,pedestrian2)
F3<-Frechet(pedestrian1,pedestrian3)
F4<-Frechet(pedestrian1,pedestrian4)
F5<-Frechet(pedestrian1,pedestrian5)
F6<-Frechet(pedestrian1,pedestrian6)
## can't really understand the frechet...

LCSS1<-LCSS(pedestrian1,pedestrian1, errorMarg = 50, pointDistance=5)# i think that the longest common distance is equal to the full distance here
LCSS2<-LCSS(pedestrian1,pedestrian2, errorMarg = 50, pointDistance=5)
LCSS3<-LCSS(pedestrian1,pedestrian3, errorMarg = 50, pointDistance=5)
LCSS4<-LCSS(pedestrian1,pedestrian4, errorMarg = 50, pointDistance=5)
LCSS5<-LCSS(pedestrian1,pedestrian5, errorMarg = 50, pointDistance=5)# trajectory 5 would have the longest common distance within the threshold of 5 
LCSS6<-LCSS(pedestrian1,pedestrian6, errorMarg = 50, pointDistance=5)

?LCSS

Plot5<-matrix(c(1,2,3,4,5,6,DTW1,DTW2,DTW3,DTW4,DTW5,DTW6,ED1,ED2,ED3,ED4,ED5,ED6,F1,F2,F3,F4,F5,F6,LCSS1,LCSS2,LCSS3,LCSS4,LCSS5,LCSS6),6)
Plot5<-as.data.frame(Plot5)
Plot5$V1<-as.factor(Plot5$V1)
colnames(Plot5)<-c("no","DTW","EditDist", "Frechet", "LCSS")
Plot5

Plot6<-Plot5%>%
  group_by(no)

library(tidyverse)
Plot6<-Plot5%>%
  gather(key,value,-no)

ggplot(data=Plot6,aes(y=value,x=no,fill=no))+
  facet_wrap(~key, scales="free")+
  geom_bar(stat="identity")

## note: to be honest I did not really understand everything here, especially not Frechet and also not clearly LCSS
## also I hope there is an easier way with less rows to calculate this exercise? I did it like this because it says in the description, that there needs to be one trajectory per matrix.
## ...and I didn't find Alain Both (2018) on moodle nor on google scholar...
## I am looking forward to your solutions ;)
