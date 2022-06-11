library("lubridate")
library("tidyverse")
`%!in%` <- Negate(`%in%`)

getmode <- function(v) 
{
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

frequency_of <- function(input_column)
{
  all=c()
  for(i in 1:length(input_column))
  {
    all[i]=length(input_column[input_column==input_column[i]])
  }
  return(all)
}

frequency_of_A_in_B <- function(input_column_A, reference_column_B)
{
  all=c()
  for(i in 1:length(input_column_A))
  {
    all[i]=length(reference_column_B[reference_column_B==input_column_A[i]])
  }
  return(all)
}

data_directory = "C:/Users/User/Desktop/Google Analystics/C8/Case Study/Data/"
setwd(data_directory)
data_files = list.files()

data_table = read.csv(paste(data_directory, data_files[1], sep=""))
list_column_names = colnames(data_table)
size_column_names = length(list_column_names)

for(i in 2:length(data_files))
{
  t = read.csv(paste(data_directory, data_files[i], sep=""))
  data_table = bind_rows(data_table, t)
  list_column_names = c(list_column_names, colnames(t))
  size_column_names = c(size_column_names, length(colnames(t)))
  rm(t)
  cat("\f")
  cat(paste("Progress: ", toString(round(100*((i-1)/(length(data_files)-1)),2)), "%", sep=""))
}

unique_size_column_names = unique(size_column_names)
unique_list_column_names = unique(list_column_names)

if(length(unique_list_column_names)*unique_size_column_names == (length(colnames(data_table)))^2)
{
  cat("\f")
  cat("Column names are consistent in each data file. No further consideration required in this regard :)")
}else {cat("Column names are not consistent across the files, they need to be modified")}

###check data types of columns--------------------------------------------------
str(data_table)
#The started_at and ended_at are currently as characters. We convert to datetime:
data_table$started_at = as_datetime(data_table$started_at)
data_table$ended_at = as_datetime(data_table$ended_at)

###Create some useful columns---------------------------------------------------
#Compute the ride duration in seconds
data_table$ride_duration = as.numeric(data_table$ended_at - data_table$started_at)
#Create calender info from the started_at date
data_table$hour = hour(data_table$started_at)
data_table$weekday = weekdays(data_table$started_at)
data_table$month = month(data_table$started_at)
#Compute/Estimate the average ride VELOCITY (not speed) in kmph. At around 40° North, 1°~= 85 km. 
data_table$velocity = (85*((data_table$start_lat-data_table$end_lat)^2+(data_table$start_lng-data_table$end_lng)^2)^0.5)/(data_table$ride_duration/3600)
data_table[data_table$ride_duration==0,"velocity"]=0

###We now try cleaning and standardizing the data.------------------------------
#We try creating a lookup table to standardize the data
temp_table = data_table[!is.na(data_table$end_lat),]
temp_table1 = temp_table %>% select(start_station_name, start_station_id, start_lat, start_lng)
temp_table1 = temp_table1[temp_table1$start_station_name !="" & temp_table1$start_station_id !="" & temp_table1$start_lat !="" & temp_table1$start_lng !="",]
colnames(temp_table1) = c("station_name", "id", "lat", "lng")
temp_table2 = temp_table %>% select(end_station_name, end_station_id, end_lat, end_lng)
temp_table2 = temp_table2[temp_table2$end_station_name !="" & temp_table2$end_station_id !="" & temp_table2$end_lat !="" & temp_table2$end_lng !="",]
colnames(temp_table2) = c("station_name", "id", "lat", "lng")
rm(temp_table)
temp_table = bind_rows(temp_table1, temp_table2)
rm(temp_table1,temp_table2)

#>convinced that the stations resulting in dataframe t should be excluded from data_table
t = temp_table %>% group_by(id) %>% summarize(mode_station_name = getmode(station_name))
t$first_id = substr(t$id, 1, 1)
t$freq_first_id = frequency_of(t$first_id)
t = t[t$first_id %!in% c("1", "2", "3", "4", "5" , "6", "7", "8" ,"9"), ]
t$second_id = substr(t$id, 1, 2)
t$freq_second_id = frequency_of(t$second_id)
t = t[t$second_id %!in% c("KA", "TA"),]
t$freq_main_df_start = frequency_of_A_in_B(t$id, data_table$start_station_id)
t$freq_main_df_end = frequency_of_A_in_B(t$id, data_table$end_station_id)
t = t[t$second_id %!in% c("SL", "RP", "E0", "WL", "KP", "LF", "LP", "RN"),]

data_table = data_table %>% select(-ride_id, -started_at, -ended_at)
data_table = data_table[data_table$start_station_id %!in% t$id,] # 5860776 to 5853903
data_table = data_table[data_table$end_station_id %!in% t$id,] # 5853903 to 5847891
temp_table = temp_table[temp_table$id %!in% t$id,]
temp_table = temp_table %>% group_by(id) %>% summarize(mode_station_name = getmode(station_name), mean_lat = mean(lat), mean_lng = mean(lng))

iterate_list = which(data_table$start_station_id == "" & data_table$start_lat != "" & data_table$start_lng != "") #822694
deno = dim(data_table)[1]
for(i in iterate_list)
{
  filtered_data = temp_table[which.min((temp_table$mean_lat - data_table[i,"start_lat"])^2 + (temp_table$mean_lng - data_table[i,"start_lng"])^2),]
  data_table[i,"start_station_name"] = filtered_data$mode_station_name
  data_table[i,"start_station_id"] = filtered_data$id

  cat("\f")
  cat(paste(toString(100*i/deno), "%"))
}

iterate_list = which(data_table$end_station_id == "" & data_table$end_lat != "" & data_table$end_lng != "") #872437
deno = dim(data_table)[1]
for(i in iterate_list)
{
  filtered_data = temp_table[which.min((temp_table$mean_lat - data_table[i,"end_lat"])^2 + (temp_table$mean_lng - data_table[i,"end_lng"])^2),]
  data_table[i,"end_station_name"] = filtered_data$mode_station_name
  data_table[i,"end_station_id"] = filtered_data$id
  
  cat("\f")
  cat(paste(toString(100*i/deno), "%"))
}

#We now check if all start and end have a station id
which(data_table$start_station_id=="") #none are empty in the start - so all start have an id
which(data_table$end_station_id=="") #there appears to be 5032 entries without any information that could identify the end position - could these have been stolen bikes? we remove these entries:
data_table = data_table[data_table$end_station_id!="",] # 5847891 to 5842859

#Now that we have a both station id's for all entries, we standardize all entries:
for(i in 1:dim(temp_table)[1])
{
  inds = which(data_table$start_station_id==temp_table[i,]$id)
  data_table[inds, "start_station_name"] = temp_table[i,"mode_station_name"]
  data_table[inds, "start_lat"] = temp_table[i,"mean_lat"]
  data_table[inds, "start_lng"] = temp_table[i,"mean_lng"]
  
  inde = which(data_table$end_station_id==temp_table[i,]$id)
  data_table[inde, "end_station_name"] = temp_table[i,"mode_station_name"]
  data_table[inde, "end_lat"] = temp_table[i,"mean_lat"]
  data_table[inde, "end_lng"] = temp_table[i,"mean_lng"]
  
  
  cat("\f")
  cat(paste(toString(100*i/dim(temp_table)[1]), "%"))
}

#recalculate the velocity
data_table$velocity = (85*((data_table$start_lat-data_table$end_lat)^2+(data_table$start_lng-data_table$end_lng)^2)^0.5)/(data_table$ride_duration/3600)
data_table[data_table$ride_duration==0,"velocity"]=0

#We now look at the summary to determine far fetched entries
summary(data_table)
#The ride duration show some considerable outliers. Based on the rates described on divvybikes.com, we remove rides below 1 second and rides above 4 hours (14400 seconds)
data_table = data_table[data_table$ride_duration>=1 & data_table$ride_duration<=14400,] #from 5842859 to 5830125
summary(data_table)
#The velocity of some rides are unrealistic, we only consider those under 50kmph:
data_table = data_table[data_table$velocity<=50,] #from 5830125 to 5825327

###Check unique entries of categorical columns that seem reasonable for breakdown analytics
print(paste("Rideable_Types: ", toString(unique(data_table$rideable_type))))
print(paste("Member_Casual: ", toString(unique(data_table$member_casual))))

###We create some more categories for breakdown analytics, using ride_duration in steps of 0.5 hours and velocity in steps on 10kmph
data_table$ride_duration_steps = ceiling(data_table$ride_duration/1800)/2
data_table$velocity_steps =ceiling(data_table$velocity/10)*10
data_table$count = 1

data_table$month_c = as.character(data_table$month)
data_table[data_table$month<10,"month_c"] = paste("0", data_table[data_table$month<10,"month_c"], sep="")
data_table[data_table$month>=6,"month_c"] = paste("2021", data_table[data_table$month>=6,"month_c"], sep="")
data_table[data_table$month<6,"month_c"] = paste("2022", data_table[data_table$month<6,"month_c"], sep="")

data_table$velocity_steps_c = as.character(data_table$velocity_steps)
data_table[data_table$velocity_steps==10 | data_table$velocity_steps==0 ,"velocity_steps_c"] = "0-10"
data_table[data_table$velocity_steps==20,"velocity_steps_c"] = "10-20"
data_table[data_table$velocity_steps==30,"velocity_steps_c"] = "20-30"
data_table[data_table$velocity_steps==40,"velocity_steps_c"] = "30-40"
data_table[data_table$velocity_steps==50,"velocity_steps_c"] = "40-50"

data_table$ride_duration_steps_c = ""
data_table[data_table$ride_duration_steps==0.5,"ride_duration_steps_c"] = "0-30"
data_table[data_table$ride_duration_steps==1,"ride_duration_steps_c"] = "30-60"
data_table[data_table$ride_duration_steps==1.5,"ride_duration_steps_c"] = "60-90"
data_table[data_table$ride_duration_steps==2,"ride_duration_steps_c"] = "90-120"
data_table[data_table$ride_duration_steps==2.5,"ride_duration_steps_c"] = "120-150"
data_table[data_table$ride_duration_steps==3,"ride_duration_steps_c"] = "150-180"
data_table[data_table$ride_duration_steps==3.5,"ride_duration_steps_c"] = "180-210"
data_table[data_table$ride_duration_steps==4,"ride_duration_steps_c"] = "210-240"

customer_type_ratio = dim(data_table[data_table$member_casual=="member",])[1]/dim(data_table)[1]


###Visuals:

########## Weekdays
data_table$weekday <- factor(data_table$weekday, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

ggplot(data_table, aes(x = weekday, y = count, fill = member_casual)) + 
  geom_bar(position = "fill", stat = "identity", width=0.6) + 
  scale_y_continuous(labels = scales::percent_format()) + 
  geom_hline(yintercept=customer_type_ratio, linetype=2) +
  labs(title = "Weekday Proportioned Breakdown", x = "Day of the Week", y = "Proportion", fill = "Customer Type") + 
  annotate(geom = "text", x = "Saturday", y = customer_type_ratio+0.04, label = "Customer Type Ratio")

ggplot(data_table) + 
  geom_bar(aes(x = weekday, fill = member_casual), width=0.6) + 
  labs(title = "Weekday Count Breakdown", x = "Day of the Week", fill = "Customer Type")


########## Hourly
ggplot(data_table, aes(x = hour, y = count, fill = member_casual)) + 
  geom_bar(position = "fill", stat = "identity", width=0.6) + 
  scale_y_continuous(labels = scales::percent_format()) + 
  geom_hline(yintercept=customer_type_ratio, linetype=2) +
  scale_x_continuous(breaks = pretty(data_table$hour, n = 9)) +
  labs(title = "Hourly Proportioned Breakdown", x = "Hour of the Day", y = "Proportion", fill = "Customer Type") + 
  annotate(geom = "text", x =20, y = customer_type_ratio+0.04, label = "Customer Type Ratio")

ggplot(data_table, ) + 
  geom_bar(aes(x = hour, fill = member_casual), width=0.6) + 
  scale_x_continuous(breaks = pretty(data_table$hour, n = 9)) +
  labs(title = "Hourly Count Breakdown", x = "Hour of the Day", fill = "Customer Type")


########## Monthly
ggplot(data_table, aes(x = month_c, y = count, fill = member_casual)) + 
  geom_bar(position = "fill", stat = "identity", width=0.6) + 
  scale_y_continuous(labels = scales::percent_format()) + 
  geom_hline(yintercept=customer_type_ratio, linetype=2) +
  labs(title = "Monthly Proportioned Breakdown", x = "Month (yyyymm)", y = "Proportion", fill = "Customer Type") + 
  annotate(geom = "text", x ="202203", y = customer_type_ratio+0.04, label = "Customer Type Ratio")

ggplot(data_table, ) + 
  geom_bar(aes(x = month_c, fill = member_casual), width=0.6) + 
  labs(title = "Monthly Count Breakdown", x = "Month (yyyymm)", fill = "Customer Type")


########## Velocity
ggplot(data_table, aes(x = velocity_steps_c, y = count, fill = member_casual)) + 
  geom_bar(position = "fill", stat = "identity", width=0.6) + 
  scale_y_continuous(labels = scales::percent_format()) + 
  geom_hline(yintercept=customer_type_ratio, linetype=2) +
  labs(title = "Velocity Proportioned In Bins Of 10 kmph", x = "Velocity (kmph)", y = "Proportion", fill = "Customer Type") + 
  annotate(geom = "text", x ="30-40", y = customer_type_ratio+0.04, label = "Customer Type Ratio")

ggplot(data_table, ) + 
  geom_bar(aes(x = velocity_steps_c, fill = member_casual), width=0.6) + 
  labs(title = "Velocity In Bins Of 10 kmph", x = "Velocity (kmph)", fill = "Customer Type")


########## Duration
data_table$ride_duration_steps_c <- factor(data_table$ride_duration_steps_c, levels = c("0-30", "30-60", "60-90", "90-120", "120-150", "150-180", "180-210", "210-240"))

ggplot(data_table, aes(x = ride_duration_steps_c, y = count, fill = member_casual)) + 
  geom_bar(position = "fill", stat = "identity", width=0.6) + 
  scale_y_continuous(labels = scales::percent_format()) + 
  geom_hline(yintercept=customer_type_ratio, linetype=2) +
  labs(title = "Ride Duration Proportioned In Bins Of 30 minutes", x = "Duration (minutes)", y = "Proportion", fill = "Customer Type") + 
  annotate(geom = "text", x ="150-180", y = customer_type_ratio+0.04, label = "Customer Type Ratio")

ggplot(data_table, ) + 
  geom_bar(aes(x = ride_duration_steps_c, fill = member_casual), width=0.6) + 
  labs(title = "Ride Duration In Bins Of 30 minutes", x = "Duration (minutes)", fill = "Customer Type")


########## Ride Type
ggplot(data_table, aes(x = rideable_type, y = count, fill = member_casual)) + 
  geom_bar(position = "fill", stat = "identity", width=0.6) + 
  scale_y_continuous(labels = scales::percent_format()) + 
  geom_hline(yintercept=customer_type_ratio, linetype=2) +
  labs(title = "Type of Ride Proportioned Breakdown", x = "Type of Ride", y = "Proportion", fill = "Customer Type") + 
  annotate(geom = "text", x ="docked_bike", y = customer_type_ratio+0.04, label = "Customer Type Ratio")

ggplot(data_table, ) + 
  geom_bar(aes(x = rideable_type, fill = member_casual), width=0.6) + 
  labs(title = "Type of Ride Breakdown", x = "Type of Ride", fill = "Customer Type")



###Lets create some breakdown tables:

data_table %>% group_by(member_casual, rideable_type) %>% summarize(avg_duration_minutes = mean(ride_duration)/60)

data_table %>% group_by(member_casual, weekday) %>% summarize(avg_duration_minutes = mean(ride_duration)/60)

data_table %>% group_by(member_casual, month) %>% summarize(avg_duration_minutes = mean(ride_duration)/60)

data_table %>% group_by(member_casual) %>% summarize(mode_start_station = getmode(start_station_name), mode_end_station = getmode(end_station_name))

data_table %>% group_by(member_casual, rideable_type) %>% summarize(mode_start_station = getmode(start_station_name), mode_end_station = getmode(end_station_name))

data_table %>% group_by(member_casual, rideable_type, month) %>% summarize(mode_start_station = getmode(start_station_name), mode_end_station = getmode(end_station_name))

data_table %>% group_by(member_casual, rideable_type, weekday) %>% summarize(mode_start_station = getmode(start_station_name), mode_end_station = getmode(end_station_name))

export_map_vis = data_table %>% group_by(member_casual, start_station_id) %>% summarize(lat = mean(start_lat), lng = mean(start_lng), count = sum(count))


write.csv(export_map_vis, "C:/Users/User/Desktop/Google Analystics/C8/Case Study/mapping.csv")









