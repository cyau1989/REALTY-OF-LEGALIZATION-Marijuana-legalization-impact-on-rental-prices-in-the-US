#library definitions
library("readr")
library("plm")
library(MatchIt)
library(psych)
library(ggplot2)
library(data.table)
library(geosphere)

#Importing the analytical dataset
input_dataset <- read.csv("~/NUS/Semester 2/Hands on with Consumer Analytics/Final Project/Data/analytical_dataset.csv")
input_dataset<-input_dataset[,-1]

#Importing the city to lat-long file
zip_code_city_mapping <- read_csv("~/NUS/Semester 2/Hands on with Consumer Analytics/Final Project/Data/Housing data/zip_code-city mapping.csv", 
                                  col_types = cols(Decommisioned = col_skip(), 
                                                   EstimatedPopulation = col_skip(), 
                                                   Location = col_skip(), LocationType = col_skip(), 
                                                   TaxReturnsFiled = col_skip(), TotalWages = col_skip(), 
                                                   ZipCodeType = col_skip()))

#checking that the columns are in required format
str(input_dataset)
input_dataset$city.state<-as.character(input_dataset$city.state)
input_dataset$Year<-as.factor(input_dataset$Year)
input_dataset$post_flag<-as.factor(input_dataset$post_flag)
input_dataset$treat_flag<-as.factor(input_dataset$treat_flag)
#Keeping only mainland cities
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}
input_dataset$state<-substrRight(input_dataset$city.state,2)
table(input_dataset$state)
input_dataset<-input_dataset[-which(input_dataset$state %in% c("AK","HI","PR")),]
input_dataset<-input_dataset[,-42]
input_dataset$city.state<-as.factor(input_dataset$city.state)

#Converting the data to plm format
Input_data_plm<-pdata.frame(input_dataset, index = c("Year","city.state"), drop.index = F, row.names = F)


############################################### Checking the effect on rent of thhe houses############################
#Simple linear model
full_sample_plm_no_cntrl_1_nofe<- lm(formula = median_gross_rent~post_flag , data=Input_data_plm)
summary(full_sample_plm_no_cntrl_1_nofe)
#In general the rent seems to be increasing

plm1 <- plm(median_gross_rent ~ post_flag*treat_flag , data = Input_data_plm, index = c("Year","city.state"), model = "within")
summary(plm1)

#Some significant results, however, the model is not very good, as can be seen from the Rsquare

#Now introducing control variables
controls<-c(
  "Population",
  "Violent.crime",
  "Property.crime",
  #"median_units_in_str",
  "median_age_building",
  "median_number_rooms",
  "median_occupancy_years",
  "median_smoc_mortgage",
  "median_smoc_no_mortgage",
  "total.housing.units",
  "occupied.housing.units",
  #"vacant.housing.units",
  #"units.with.gas",
  #"units.with.bottled.tank.lp.gas",
  "units.with.electricity",
  #"units.with.fuel.oil.kerosene",
  #"units.with.coal.or.coke",
  #"units.with.wood",
  #"units.with.solar.energy",
  #"units.with.other.fuel",
  "units.without.fuel",
  #"units.without.complete.kitchen",
  #"units.without.telephone",
  "Adj_Gross_Income_tot",
  #"Income_tax_tot.thousands.",
  "average_household_income",
  "percent_unemployment",
  #"Male_Population",
  #"Female_Population",
  #"White_Population",
  #"African_American",
  #"Other_races",
  #"Median.Age..2010.",
  "number_of_schools")
var<-c(controls,"Year","city.state","treat_flag","post_flag","median_gross_rent")
index<-as.numeric(which(colnames(Input_data_plm) %in% var))
data_for_reg<-Input_data_plm[,index]
f <- as.formula(paste("median_gross_rent ~ treat_flag*post_flag+", paste(var[!var %in% c("median_gross_rent","treat_flag","post_flag","Year","city.state")], collapse = " + ")))
plm2 <- plm(data = data_for_reg,formula = f,  index = c("city.state","Year"), model = "within")
summary(plm2)
#Here, it can be seen that the model is significant, and there seems to be significant change in the value among the treatment group relative to control

#So, now trying to get a matched sample to work with
#Using 2011 data to match
data_for_match<-Input_data_plm[Input_data_plm$Year=="2011",]

f1<-as.formula(paste("treat_flag ~ ", paste(c(controls[!controls %in% c("median_gross_rent","treat_flag","post_flag","Year","city.state")],"Male_Population","White_Population","African_American","Other_races","Median.Age..2010."), collapse = " + ")))

match <- matchit(formula = f1,data = data_for_match, method = "nearest", replace=T, ratio=5, distance = "logit")

#Now checking that the treatment and controls are more similar than earlier
data_matched=match.data(match)

stats_pre<-describeBy(data_for_match, group=data_for_match$treat_flag,mat=FALSE,type=3,digits=5) # statistics before matching
stats_post<-describeBy(data_matched, group=data_matched$treat_flag,mat=FALSE,type=3,digits=5) # statistics after matching

pre_matching<-as.data.frame(cbind(colnames(data_for_match)[stats_pre$`0`$vars],stats_pre$`0`$mean, stats_pre$`1`$mean))
colnames(pre_matching)<-c("variable", "mean_cntrol", "mean_treatment")
post_matching<-as.data.frame(cbind(colnames(data_matched)[stats_post$`0`$vars],stats_post$`0`$mean, stats_post$`1`$mean))
colnames(post_matching)<-c("variable", "mean_cntrol", "mean_treatment")
pre_matching<-pre_matching[pre_matching$variable %in% controls,]
post_matching<-post_matching[post_matching$variable %in% controls,]

matching_compare<-merge(x=post_matching, y=pre_matching, by=c("variable", "mean_treatment"))
colnames(matching_compare)[3:4]<-c("mean_control.post", "mean_control.pre")

pre_matching_data<-data_for_match[,colnames(data_for_match) %in% controls]
post_matching_data<-data_matched[,colnames(data_matched) %in% controls]

results<-expand.grid(variable=colnames(pre_matching_data), pvalue.pre=0,pvalue.post=0)
for(i in 1:ncol(pre_matching_data))
{
x<-t.test(as.numeric(pre_matching_data[,i]),as.numeric(pre_matching_data[,i]),conf.level = 0.95,alternative="two.sided", paired=F, var.equal = T)
y<-t.test(as.numeric(post_matching_data[,i]),as.numeric(post_matching_data[,i]),conf.level = 0.95,alternative="two.sided", paired=F, var.equal = T)
results[,2]<-x$p.value
results[,3]<-y$p.value
}

#re-running the regression on the matched samples
data_for_reg_2<-merge(x=data_for_reg,y=data_matched[,c("city.state","weights")],by="city.state")
plm3 <- plm(data = data_for_reg_2,formula = f,  index = c("city.state","Year"), model = "within", weights=data_for_reg_2$weights)
summary(plm3)

stargazer::stargazer(plm3, type="text")
#Using PCA on the control variables and rerunning
data_for_pca<-Input_data_plm[,index]
pca<-prcomp(data_for_pca[,-c(1,5,11,20,21)], scale. = T)
# proportion of variance explained
pca$sdev
plot(cumsum(pca$sdev^2) / sum(pca$sdev^2), type="b", ylim=c(0,1))
pca_num<-which(cumsum(pca$sdev^2) / sum(pca$sdev^2)>=0.8)[1]
#8 PC are enough to explain around 80% variation in the controls
princ_comp<-data.frame(pca$x[,1:pca_num])

data_for_reg_3<-as.data.frame(cbind(data_for_reg[,c(1,5,11,20,21)],princ_comp))
data_for_reg_3<-merge(x=data_for_reg_3,y=data_matched[,c("city.state","weights")],by="city.state")
plm4 <- plm(data = data_for_reg_3,formula =median_gross_rent~treat_flag*post_flag + PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8 ,  index = c("Year","city.state"), model = "within", weights=data_for_reg_3$weights)
summary(plm4)


#Using clustering within the cities to find if certain cities saw an effect more than others, as an alternative to PSM
#Finding number of clusters required based on when adding an one more cluster doesnt reduce Within SS (less concentrated groups)

data_for_clustering<-data_for_match[which(data_for_match$treat_flag==1),]
pca_clust<-prcomp(data_for_clustering[,-c(1,5,6,13,37,34,15,16,40,41)], scale. = T)
plot(cumsum(pca_clust$sdev^2) / sum(pca_clust$sdev^2), type="b", ylim=c(0,1))
pca_clust_num<-which(cumsum(pca_clust$sdev^2) / sum(pca_clust$sdev^2)>=0.8)[1]
#12 PC are enough to explain around 80% variation in the controls
princ_comp_clust<-data.frame(pca_clust$x[,1:pca_clust_num])

wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i, nstart = 50)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}

wssplot(princ_comp_clust, nc=10) 
#Now building 4 clusters using the principal component derived previously 
set.seed(1234)
km1<-kmeans(x=princ_comp_clust, centers=3, nstart=50)
ggplot(data=princ_comp_clust, aes(x=PC1, y=PC2)) + geom_point(aes(color=factor(km1$cluster)), size=4) + theme_bw()

#Now, profiling the clusters
clustered_data<-as.data.frame(cbind(data_for_clustering,km1$cluster))
colnames(clustered_data)[ncol(clustered_data)]<-"cluster"
clustered_data_2<-clustered_data[,which(colnames(clustered_data) %in% c(controls,"cluster"))]
cluster_summ<-cbind(aggregate(clustered_data_2[,-17],by=list(clustered_data_2$cluster),FUN=mean),km1$size)
colnames(cluster_summ)[1]<-"cluster"
colnames(cluster_summ)[18]<-"cluster_size"
demo_summ_cluster<-aggregate(x=data_for_clustering[,c("Male_Population","White_Population","African_American","Other_races","Median.Age..2010."),], by=list(km1$cluster), FUN=mean)
cluster_summ<-as.data.frame(cbind(cluster_summ,demo_summ_cluster[,-1]))

x<-as.data.frame(match$match.matrix)
x1<-as.data.frame(cbind(data_for_match, as.numeric(rownames(data_for_match))))
x2<-as.data.frame(cbind(data_matched, as.numeric(rownames(data_matched))))
colnames(x2)[ncol(x2)]<-"rowname"
colnames(x1)[ncol(x1)]<-"rowname"
x<-as.data.frame(cbind(x, rownames(x)))
colnames(x)[ncol(x)]<-"rowname"
x<-merge(x=x,y=x1[,c(40,42)],by="rowname")[,-1]
colnames(x)[ncol(x)]<-"city.state.treatment"

comb_matched<-rbind(x,x,x,x,x)
comb_matched$control.rownum<-as.character(ifelse(as.numeric(rownames(comb_matched)<=167),as.character(comb_matched[,1]),ifelse(as.numeric(rownames(comb_matched)<=167*2),as.character(comb_matched[,2]),ifelse(as.numeric(rownames(comb_matched)<=167*3),as.character(comb_matched[,3]),ifelse(as.numeric(rownames(comb_matched)<=167*4),as.character(comb_matched[,4]),as.character(comb_matched[,5]))))))
comb_matched<-comb_matched[,c(6,7)]
comb_matched1<-merge(x=comb_matched,y=x1[,c(40,42)],by.x="control.rownum", by.y = "rowname")[,-1]
colnames(comb_matched1)[ncol(comb_matched1)]<-"city.state.control"
comb_matched1<-merge(comb_matched1,clustered_data[,c(40,42)],by.x="city.state.treatment", by.y="city.state")

data_for_reg_4_1<-unique(merge(data_matched,comb_matched1[,c(1,3)], by.x="city.state",by.y="city.state.treatment"))
data_for_reg_4_2<-unique(merge(data_matched,comb_matched1[,c(2,3)], by.x="city.state",by.y="city.state.control"))

data_for_reg_4<-rbind(data_for_reg_4_1,data_for_reg_4_2)[,c(1,44)]
#Now rerunning the regression within each cluster
data_for_reg_4_f<-merge(x=data_for_reg,y=data_for_reg_4,by="city.state")

for(i in 1:3)
{
  data.reg.temp<-data_for_reg_4_f[data_for_reg_4_f$cluster==i,]
  plm.temp<-plm(data = data.reg.temp,formula = f,  index = c("city.state","Year"), model = "within")
  assign(paste("plm_cluster", i, sep = '_'), plm.temp)
}
summary(plm_cluster_1)
summary(plm_cluster_2)
summary(plm_cluster_3)

plm.cluster_all<-plm(data = data_for_reg_4_f,formula = median_gross_rent ~ treat_flag * post_flag *as.factor(cluster) + Population + Violent.crime + 
                       Property.crime + median_age_building + median_number_rooms + 
                       median_occupancy_years + median_smoc_mortgage + median_smoc_no_mortgage + 
                       total.housing.units + occupied.housing.units + units.with.electricity + 
                       units.without.fuel + Adj_Gross_Income_tot + average_household_income + 
                       percent_unemployment + number_of_schools,  index = c("Year","city.state"), model = "within")
summary(plm.cluster_all)

################################################# Robustness check##########################################
Input_data_clean_placebo<-data_for_reg_2
placebo_results<-expand.grid(iteration=1:1000,interaction_beta=0,p_value_interaction=0,pre.period=c(2,3))
rand_pre_period<-c(2,3)
for (i in 1:1000)
{
  Input_data_clean_placebo<-data_for_reg_2
  for (j in rand_pre_period)
  {
  Input_data_clean_placebo$post_flag<-as.factor(as.numeric(as.numeric(Input_data_clean_placebo$Year)>j))
  all_cities<-data.frame(unique(Input_data_clean_placebo$city.state))
  colnames(all_cities)[1]<-"city.state"
  rand_folds<-sample.int(n=nrow(all_cities),size=167)
  all_cities$treat_flag<-0
  all_cities[rand_folds,"treat_flag"]<-1
  all_cities$treat_flag<-as.factor(all_cities$treat_flag)
  colnames(all_cities)[2]<-"treat_flag"
  Input_data_clean_placebo<-merge(x = all_cities, y = Input_data_clean_placebo[,-which(colnames(Input_data_clean_placebo)=="treat_flag")], by = "city.state", all = TRUE)
  
  placebo_iteration<- plm(formula = f, index = c("Year","city.state"), data=Input_data_clean_placebo, model="within")
  summary(placebo_iteration)
  
  interaction_beta<-placebo_iteration$coefficients[length(placebo_iteration$coefficients)]
  p_value_interaction<-coef(summary(placebo_iteration))[length(placebo_iteration$coefficients),4]
  placebo_results[placebo_results$iteration==i & placebo_results$pre.period==j,2]<-interaction_beta
  placebo_results[placebo_results$iteration==i & placebo_results$pre.period==j,3]<-p_value_interaction
  }
}
write.csv(data_for_reg_3,"treatment_control_all.csv")

#plotting the control and treatment cities on a map
library(ggmap)
qmplot(Long, Lat, data = treatment_control_all, colour = treat_flag, size = I(1), darken = 0.3)+scale_color_manual(labels = c("Control Cities", "Treatment Cities"), values = c("red", "greenyellow"))


#Checking the effect on WA and CO separately
comb_matched1$state_treat<-substrRight(as.character(comb_matched1$city.state.treatment),2)
comb_matched_CO<-comb_matched1[which(comb_matched1$state_treat=="CO"),-3]
input_dataset_CO<-input_dataset[which(input_dataset$city.state %in% c(as.character(comb_matched_CO$city.state.treatment),as.character(comb_matched_CO$city.state.control))),]
Input_data_plm_CO<-pdata.frame(input_dataset_CO, index = c("Year","city.state"), drop.index = F, row.names = F)
plm_CO_1 <- plm(data = Input_data_plm_CO,formula = f,  index = c("city.state","Year"), model = "within", weights=Input_data_plm_CO$weights)
summary(plm_CO_1)





#Checking the effect on WA and CO separately
comb_matched1$state_treat<-substrRight(as.character(comb_matched1$city.state.treatment),2)
comb_matched_WA<-comb_matched1[which(comb_matched1$state_treat=="WA"),-3]
input_dataset_WA<-input_dataset[which(input_dataset$city.state %in% c(as.character(comb_matched_WA$city.state.treatment),as.character(comb_matched_WA$city.state.control))),]
Input_data_plm_WA<-pdata.frame(input_dataset_WA, index = c("Year","city.state"), drop.index = F, row.names = F)
plm_WA_1 <- plm(data = Input_data_plm_WA,formula = f,  index = c("city.state","Year"), model = "within", weights=Input_data_plm_WA$weights)
summary(plm_WA_1)


###Checking the effect of some moderators
#Income
f_income <- as.formula(paste("median_gross_rent ~ treat_flag*post_flag*average_household_income +", paste(var[!var %in% c("median_gross_rent","treat_flag","post_flag","Year","city.state")], collapse = " + ")))
plm_avg_income <- plm(data = data_for_reg_2,formula = f_income,  index = c("city.state","Year"), model = "within", weights=data_for_reg_2$weights)
summary(plm_avg_income)

#Population
f_Population <- as.formula(paste("median_gross_rent ~ treat_flag*post_flag*Population +", paste(var[!var %in% c("median_gross_rent","treat_flag","post_flag","Year","city.state")], collapse = " + ")))
plm_Population <- plm(data = data_for_reg_2,formula = f_Population,  index = c("city.state","Year"), model = "within", weights=data_for_reg_2$weights)
summary(plm_Population)

#Violent Crime
f_violent_crime <- as.formula(paste("median_gross_rent ~ treat_flag*post_flag*Violent.crime +", paste(var[!var %in% c("median_gross_rent","treat_flag","post_flag","Year","city.state")], collapse = " + ")))
plm_violent_crime <- plm(data = data_for_reg_2,formula = f_violent_crime,  index = c("city.state","Year"), model = "within", weights=data_for_reg_2$weights)
summary(plm_violent_crime)

#Property Crime
f_Property.crime <- as.formula(paste("median_gross_rent ~ treat_flag*post_flag*Property.crime +", paste(var[!var %in% c("median_gross_rent","treat_flag","post_flag","Year","city.state")], collapse = " + ")))
plm_Property.crime <- plm(data = data_for_reg_2,formula = f_Property.crime,  index = c("city.state","Year"), model = "within", weights=data_for_reg_2$weights)
summary(plm_Property.crime)

#Number of schools
f_number_of_schools <- as.formula(paste("median_gross_rent ~ treat_flag*post_flag*number_of_schools +", paste(var[!var %in% c("median_gross_rent","treat_flag","post_flag","Year","city.state")], collapse = " + ")))
plm_number_of_schools <- plm(data = data_for_reg_2,formula = f_number_of_schools,  index = c("city.state","Year"), model = "within", weights=data_for_reg_2$weights)
summary(plm_number_of_schools)

#Total housing units - Significant
f_total.housing.units <- as.formula(paste("median_gross_rent ~ treat_flag*post_flag*total.housing.units +", paste(var[!var %in% c("median_gross_rent","treat_flag","post_flag","Year","city.state")], collapse = " + ")))
plm_total_housing_units <- plm(data = data_for_reg_2,formula = f_total.housing.units,  index = c("city.state","Year"), model = "within", weights=data_for_reg_2$weights)
summary(plm_total_housing_units)

#Median number of rooms - Significant
f_median_number_rooms <- as.formula(paste("median_gross_rent ~ treat_flag*post_flag*median_number_rooms +", paste(var[!var %in% c("median_gross_rent","treat_flag","post_flag","Year","city.state")], collapse = " + ")))
plm_median_number_rooms <- plm(data = data_for_reg_2,formula = f_median_number_rooms,  index = c("city.state","Year"), model = "within", weights=data_for_reg_2$weights)
summary(plm_median_number_rooms)

#Adj_Gross_Income_tot
f_Adj_Gross_Income_tot <- as.formula(paste("median_gross_rent ~ treat_flag*post_flag*Adj_Gross_Income_tot +", paste(var[!var %in% c("median_gross_rent","treat_flag","post_flag","Year","city.state")], collapse = " + ")))
plm_Adj_Gross_Income_tot <- plm(data = data_for_reg_2,formula = f_Adj_Gross_Income_tot,  index = c("city.state","Year"), model = "within", weights=data_for_reg_2$weights)
summary(plm_Adj_Gross_Income_tot)

#median_age_building
f_median_age_building <- as.formula(paste("median_gross_rent ~ treat_flag*post_flag*median_age_building +", paste(var[!var %in% c("median_gross_rent","treat_flag","post_flag","Year","city.state")], collapse = " + ")))
plm_median_age_building <- plm(data = data_for_reg_2,formula = f_median_age_building,  index = c("city.state","Year"), model = "within", weights=data_for_reg_2$weights)
summary(plm_median_age_building)


#percent_unemployment
f_percent_unemployment <- as.formula(paste("median_gross_rent ~ treat_flag*post_flag*percent_unemployment +", paste(var[!var %in% c("median_gross_rent","treat_flag","post_flag","Year","city.state")], collapse = " + ")))
plm_percent_unemployment <- plm(data = data_for_reg_2,formula = f_percent_unemployment,  index = c("city.state","Year"), model = "within", weights=data_for_reg_2$weights)
summary(plm_percent_unemployment)


#####Spatial regression using rental price of the city closest as control

#First, getting the lattitude and longitude information for each city
zip_code_city_mapping_2<-unique(zip_code_city_mapping[!duplicated(c(zip_code_city_mapping$City,zip_code_city_mapping$State)),-1])
zip_code_city_mapping_2$city.state<-paste(zip_code_city_mapping_2$City,", ",zip_code_city_mapping_2$State)
city_list<-data.frame(unique(data_for_reg_2$city.state))

city_list_reqd<-data.frame(city_list[city_list$unique.data_for_reg_2.city.state. %in% zip_code_city_mapping_2$city.state,])
colnames(city_list_reqd)<-"city.state"
city_list_reqd_lat_long<-merge(x=city_list_reqd,y=zip_code_city_mapping_2,by="city.state")
city_list_reqd_lat_long<-city_list_reqd_lat_long[,-c(2,3)]

#Now taking a cross product of all these cities to find those closest to each one of them
city_lst_cj<-CJ(x=city_list_reqd_lat_long$city.state,y=city_list_reqd_lat_long$city.state)
city_lst_cj<-city_lst_cj[city_lst_cj$x!=city_lst_cj$y,]

city_lst_cj_lat_long<-merge(x=city_lst_cj, y=city_list_reqd_lat_long, by.x="x", by.y="city.state")
colnames(city_lst_cj_lat_long)<-c("city_from", "city_to", "lat_from", "long_from")
city_lst_cj_lat_long<-merge(x=city_lst_cj_lat_long, y=city_list_reqd_lat_long, by.x="city_to", by.y="city.state")
colnames(city_lst_cj_lat_long)[5:6]<-c("lat_to", "long_to")

#Now using the haversine distance formula to calculate the distance between each of these points
city_lst_cj_lat_long$dist<-distHaversine(p1=cbind(city_lst_cj_lat_long$long_from,city_lst_cj_lat_long$lat_from), p2=cbind(city_lst_cj_lat_long$long_to,city_lst_cj_lat_long$lat_to))/1000


#Now selecting the nearest city for each of the "From" city
city_lst_cj_lat_long<-city_lst_cj_lat_long[order(city_lst_cj_lat_long$city_from,city_lst_cj_lat_long$dist),]
city_nearest<-city_lst_cj_lat_long[!duplicated(city_lst_cj_lat_long$city_from),c(1,2)]
colnames(city_nearest)<-c("nearest_city", "city")

#Now, finding the rental price across the 4 years
data_for_spatial_regression<-merge(x=Input_data_plm,y=city_nearest, by.x="city.state", by.y="city")
data_for_spatial_regression_2<-merge(x=Input_data_plm[,c(1,13,40)],y=data_for_spatial_regression, by.x=c("city.state","Year"), by.y=c("nearest_city","Year"))
colnames(data_for_spatial_regression_2)[c(3,16)]<-c("nearest_city_median_gross_rent", "median_gross_rent")
df_dups<-data_for_spatial_regression_2[,c("city.state","Year")]
data_for_spatial_regression_3<-data_for_spatial_regression_2[!duplicated(df_dups),]

f_spatial_regression<-as.formula(paste("median_gross_rent ~ treat_flag*post_flag+nearest_city_median_gross_rent+", paste(var[!var %in% c("median_gross_rent","treat_flag","post_flag","Year","city.state")], collapse = " + ")))
plm_spatial<-plm(data=na.omit(data_for_spatial_regression_3), formula=f_spatial_regression,index = c("city.state","Year"), model = "within")
summary(plm_spatial)
stargazer::stargazer(plm_spatial, type="text")
###Checking to see if we only keep neightbouring states as control universe
input_dataset$state<-substrRight(as.character(input_dataset$city.state),2)
Input_data_neighbouring_reg<-input_dataset[input_dataset$state %in% c("NM", "UT", "AZ", "TX", "OK", "KS", "CO", "NE", "WY", "WA", "ID", "OR"),]
plm_neighbour<-plm(data=na.omit(Input_data_neighbouring_reg), formula=f,index = c("city.state","Year"), model = "within")
summary(plm_neighbour)

Input_data_neighbouring_reg_CO<-input_dataset[input_dataset$state %in% c("NM", "UT", "AZ", "TX", "OK", "KS", "CO", "NE", "WY"),]
plm_neighbour_CO<-plm(data=na.omit(Input_data_neighbouring_reg_CO), formula=f,index = c("city.state","Year"), model = "within")
summary(plm_neighbour_CO)

stargazer::stargazer(plm_neighbour_WA, type="text")

Input_data_neighbouring_reg_WA<-input_dataset[input_dataset$state %in% c("WA", "ID", "OR"),]
plm_neighbour_WA<-plm(data=na.omit(Input_data_neighbouring_reg_WA), formula=f,index = c("city.state","Year"), model = "within")
summary(plm_neighbour_WA)
