library(readr)        # to import tabular data (e.g. csv)
library(dplyr)        # to manipulate (tabular) data
library(ggplot2)      # to visualize data

install.packages("gghighlight")
library(gghighlight)

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

# comment: I chose to filter out segments <=6 minutes. I have a bit different segments than the plots in the excercise-descripiton. This might come from different thresholds chosen...


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


