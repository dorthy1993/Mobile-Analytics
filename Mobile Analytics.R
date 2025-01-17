setwd("/Users/DoryChen/Desktop")

library(dplyr)

Geo_fence <- read.csv("Geo-Fence Analytics.csv", header=T)

#Create variable: "imp_large", "cat_entertainment", "cat_social", "cat_tech", "os_ios", "distance"

attach(Geo_fence)

Geo_fence$imp_large <- ifelse(imp_size == "728x90", 1, 0)
Geo_fence$cat_entertainment <-ifelse(app_topcat == "IAB1" | app_topcat == "IAB1-6", 1, 0)
Geo_fence$cat_social <- ifelse(app_topcat == "IAB14", 1, 0)
Geo_fence$cat_tech <- ifelse(app_topcat == "IAB19-6", 1, 0)
Geo_fence$os_ios <- ifelse(device_os == "iOS", 1, 0)

## Create variable distance using Harvesine formula to calculate the distance 
## for a pair of latitude/longitude coordinates.

library(aspace)
Geo_fence$distance <- 6371 * acos(cos(as_radians(device_lat)) * cos(as_radians(geofence_lat)) 
                                  * cos(as_radians(device_lon) - as_radians(geofence_lon)) 
                                  + sin(as_radians(device_lat)) * sin(as_radians(geofence_lat)))

#Create distance group and calculate click-through-rate
attach(Geo_fence)
Geo_fence$distance_group <-ifelse(between(distance, 0, 0.5), 1, 
                                  ifelse(between(distance, 0.5, 1), 2, 
                                         ifelse(between(distance, 1, 2), 3, 
                                                ifelse(between(distance, 2, 4), 4, 
                                                       ifelse(between(distance, 4, 7), 5,
                                                              ifelse(between(distance, 7, 10), 6, 
                                                                     ifelse(distance >10, 7, 0)))))))


library("sqldf")

mean_ctr <- sqldf("select distance, distance_group, avg(didclick) as mean_didclick 
                    from Geo_fence group by distance_group")

barplot(mean_ctr$mean_didclick, mean_ctr$distance_group, 
        xlab="Distance_group", ylab="CTR",
        names.arg=c("<0.5", "0.5-<1.0", "1.0-<2.0", "2.0-<4.0", "4.0-<7.0", "7.0-<10.0","10.0>"),width =500
)

#Create variables "distance_squared", "ln_app_review_vol"
Geo_fence$ln_app_review_vol <- log(app_review_vol)
Geo_fence$distance_squared <- distance^2

# Descriptive Statisitcs

attach(Geo_fence)

statfun<- function(x){
  data.frame(Mean=mean(x),Median=median(x),STDEV=sd(x),Min=min(x),Max=max(x))
}


sum_table<-rbind(statfun(didclick),
                 statfun(distance),
                 statfun(imp_large),
                 statfun(cat_entertainment),
                 statfun(cat_social),
                 statfun(cat_tech),
                 statfun(os_ios),
                 statfun(ln_app_review_vol),
                 statfun(app_review_val)
)
row.names(sum_table)<-c("didclick", "distance", "imp_large", "cat_entertainment", 
                        "cat_social", "cat_tech","os_ios","ln_app_vol","app_val")
print(sum_table)


#Correlations between the variables
library(corrgram)

cor_geofence <- subset(Geo_fence, select = c(didclick, distance, imp_large, cat_entertainment, cat_social, cat_tech, os_ios, ln_app_review_vol, app_review_val))
cor(cor_geofence)
corrgram(cor_geofence, order = TRUE, lower.panel = panel.shade,
         upper.panel = panel.pie, text.panel = panel.txt,
         main = "Correlations of Geo-fense")

#Scatterplot of distance and click-through-rate
library(car)
attach(mean_ctr)

scatterplot(mean_didclick ~ distance, xlab = "distance", 
            ylab = "click_through_rate", main = "distance and click-through-rate")

#We found that the closer mobile device to geofence, the higher the click-through-rate.

#Logistic regression of "didclick"
fit.didclick <- glm(didclick ~ distance + imp_large + cat_entertainment
                    + cat_social + cat_tech + os_ios + ln_app_review_vol + app_review_val,
                    data = Geo_fence, family = binomial())
summary(fit.didclick)
fit.didclick_distance_sqrt <- glm(didclick ~ distance + distance_squared + imp_large + cat_entertainment
                                  + cat_social + cat_tech + os_ios + ln_app_review_vol + app_review_val,
                                  data = Geo_fence, family = binomial())
summary(fit.didclick_distance_sqrt)


fit.didclick_reduced <- glm(didclick ~ distance + distance_squared + imp_large 
                            + cat_tech + os_ios,
                            data = Geo_fence, family = binomial())
summary(fit.didclick_reduced)


