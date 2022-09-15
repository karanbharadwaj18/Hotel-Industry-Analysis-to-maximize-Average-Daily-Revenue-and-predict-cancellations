dev.off() # Clear the graph window
cat('\014')   # Clear the console
rm(list=ls()) # Clear user objects from the environment

# Set working directory  
setwd("C:/Users/kastu/Desktop/Syracuse/Spring 21/IST687 DS/Project") # Change to the folder containing your homework data files

#------ Packages--------

library("tidyverse", "readr")
library(xlsx)
library(readxl)



#-----Reading the data set into the R environment-----
city_hotel<- read_excel('C:/Users/kastu/Desktop/Syracuse/Spring 21/IST687 DS/Project/H2-City.xlsx')
hotel1 <- read_excel('C:/Users/kastu/Desktop/Syracuse/Spring 21/IST687 DS/Project/H2-City.xlsx')
str(city_hotel) # 14 number columns, 12 chracter columns, 2 date type columns

na_city<- sum(is.na(city_hotel))
na_city #39270, this gives the total number of NA's that are present in the dataset.

#Renaming column names with a space:
# city_hotel$[Arrival Date] <- str_replace_all(city_hotel$`Arrival Date`,c(" "="_"))
# View(city_hotel)


#Q) which column has these na values and how do we fix them?
# After observing the dataset and through trial and error method of analyzing the staysInWeekendNights attribute in step, it was found that arrival date had NA values.
arrival_na <- is.na(city_hotel$ArrivalDate)
sum_arrival_na <- sum(arrival_na)
sum_arrival_na # 39270, which matches the total na value that was given for the whole dataset.


# Verifying if any other column has na values:
x <- city_hotel[,-3] # generating a table without the arrivaldate column
sum_x <- sum(is.na(x))
sum_x #0, implying no other na values are present.


#Analysis on Columns:
#Finding the least and maximum number of weekend nights customers stayed in.
range_weekend <- range(city_hotel$StaysInWeekendNights)
range_weekend # The 0 , 16
range_week <- range(city_hotel$StaysInWeekNights)
range_week #0 , 41
index_max_siwn <- which.max(city_hotel$StaysInWeekendNights)
index_max_siwn #61735, this is the same for stayinweeknights
#View(city_hotel[index_max_siwn, ])

#Changing children attribute from character to numeric
 city_hotel$Children <- as.numeric(city_hotel$Children)

# Converting country code into country names
# install.packages(countrycode)
library(countrycode)
city_hotel$Country <- countrycode(city_hotel$Country, origin='iso3c',destination='country.name')
table(city_hotel$Country)
View(data.frame(table(city_hotel$Country)))
city_hotel$Country[city_hotel$Country == 'CN'] <- 'Canada'
city_hotel$Country[city_hotel$Country == 'TMP'] <- 'East Timor'
city_hotel$Country[city_hotel$Country == 'United States'] <- 'USA'
city_hotel$Country[city_hotel$Country == 'United States Minor Outlying Islands (the)'] <- 'USA'
city_hotel[-(city_hotel$Country == 'Null'),] #Removing null values
#View(city_hotel)
#View(data.frame(table(city_hotel$Country)))

#Countries and their frequencies for hotels
country_table <- table(city_hotel$Country)
View(data.frame(country_table))
df_country_freq <- data.frame(country_table)
df_country_freq

names(df_country_freq)[names(df_country_freq)=="Var1"] <- "Country"
names(df_country_freq)[names(df_country_freq)=="Freq"] <- "Count"
View(df_country_freq)
# View(city_hotel)

#---- Generating a wordcloud to show which countries people came frequently from
#install.packages("wordcloud") 
#install.packages("RColorBrewer")
library("wordcloud")
library("RColorBrewer") 

wordcloud(words = df_country_freq$Country, freq = df_country_freq$Count, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

# generate a map for better visualization
library(ggplot2)
library(ggmap)
world_map <- map_data("world")

View(world_map)
#str(world_map)
world_map <- merge(world_map,df_country_freq, by.x ="region", by.y="Country")
str(world_map)
world_map <- world_map[order(world_map$group,world_map$order), ]
ggplot() + geom_map(data=world_map,map = world_map, aes(x=long,y=lat, map_id=region, fill = Count)) + coord_map(projection = "mercator") +scale_fill_gradient(high = "red", low ="blue")+ scale_fill_continuous(low = "#9A7787", high = "RED",breaks =c(0,5000,10000,15000,20000,25000,30500)) 
#Shows more people from portugal stayed in the hotel, to attract more people or to satisfy the local customers, ads/offers can be given to people residing there.

world_map, aes(x=long, y=lat, group = group)+geom_polygon(aes(fill = Count),color = "black")
world <- map_data("world")
ggplot() +
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region)
  ) 



#creating number of days stayed.
city_hotel$DaysStayed <- city_hotel$StaysInWeekendNights + city_hotel$StaysInWeekNights # Creating a new varaible for number of days stayed
#View(city_hotel)
city_hotel$ReservationStatusDate <- as.Date(city_hotel$ReservationStatusDate, format = '%d-%m-%Y') #Converting the Reservation Status date column to date type
city_hotel$newArrival <- as.Date(city_hotel$ReservationStatusDate - city_hotel$DaysStayed) #Generating a new column for arrival date from the reservation status date and number of days stayed.
tab1 <- format(city_hotel$newArrival, "%m") # Getting month of the arrival date
tab1 <- data.frame(table(tab1)) # generating a table of months to get their total counts and converting it into a dataframe for further analysis
names(tab1)[names(tab1)=="tab1"] <- "Month" #Renaming the Column name in the table
View(tab1)
#tab1[match(tab1$Month,month.abb),]
View(city_hotel)

#Creating a plot for the month and number of customers
plot_month <- ggplot(data= tab1, aes(x = Month,y=Freq, group =1))+ ylab ('Number of Customers') + ggtitle(" Customers by Month ") + geom_line()+ geom_point(shape = 21, color="black", fill="#69b3a2", size=2 ) + scale_x_discrete(labels=month.abb) + scale_y_continuous(breaks = seq(5000,8000, by = 500))
plot_month <- plot_month +  theme(
  plot.title = element_text(color="Black", size=12, face="bold.italic"),
  axis.title.x = element_text( size=8, face="bold"),
  axis.title.y = element_text( size=8, face="bold"))
plot_month


# Creating a seasons variable based on the new derived arrival date.
city_hotel <- mutate(city_hotel, Season = case_when( format(city_hotel$newArrival, "%m") == '03' ~ "Spring",
                                                     format(city_hotel$newArrival, "%m") == '04' ~ "Spring",
                                                     format(city_hotel$newArrival, "%m") == '05' ~ "Spring",
                                                     format(city_hotel$newArrival, "%m") == '06' ~ "Summer",
                                                     format(city_hotel$newArrival, "%m") == '07' ~ "Summer",
                                                     format(city_hotel$newArrival, "%m") == '08' ~ "Summer",
                                                     format(city_hotel$newArrival, "%m") == '09' ~ "Fall",
                                                     format(city_hotel$newArrival, "%m") == '10' ~ "Fall",
                                                     format(city_hotel$newArrival, "%m") == '11' ~ "Fall",
                                                     format(city_hotel$newArrival, "%m") == '12' ~ "Winter",
                                                     format(city_hotel$newArrival, "%m") == '01' ~ "Winter",
                                                     format(city_hotel$newArrival, "%m") == '02' ~ "Winter"
                                                     ))
#View(city_hotel)

#Plot for Seasons based on number of customers.
tab_season <- data.frame(table(city_hotel$Season)) #Generating a table of seasons with total number of customers
#tab_season <- data.frame(tab_season)
#View(tab_season)
names(tab_season)[names(tab_season)=="Var1"] <- "Season"  # renaming the tabular column

#Creating a plot for Seasons and number of customers
season_plot <- ggplot(data= tab_season,aes(x=Season,y=Freq, color = Season, group =1)) +  geom_line(color = "Grey")+ geom_point(shape = 21, color="black", aes(fill=Season), size=3 ) +theme(legend.position = "none")
season_plot <- season_plot + ggtitle("Trend by Season") + ylab(" Number of Customers") 
season_plot <- season_plot + theme(
  plot.title = element_text(color="Black", size=12, face="bold.italic"),
  axis.title.x = element_text( size=8, face="bold"),
  axis.title.y = element_text( size=8, face="bold"))
season_plot


#analyze after excluding cancellations.
# Creating a new dataframe by excluding cancellations , including no-shows
hotel_nocancel <- city_hotel[(city_hotel$ReservationStatus == 'Check-Out' | city_hotel$ReservationStatus == 'No-Show'), ] 
# Creating a seasons variable based on the new derived arrival date.
hotel_nocancel <- mutate(hotel_nocancel, Season = case_when( format(hotel_nocancel$newArrival, "%m") == '03' ~ "Spring",
                                                     format(hotel_nocancel$newArrival, "%m") == '04' ~ "Spring",
                                                     format(hotel_nocancel$newArrival, "%m") == '05' ~ "Spring",
                                                     format(hotel_nocancel$newArrival, "%m") == '06' ~ "Summer",
                                                     format(hotel_nocancel$newArrival, "%m") == '07' ~ "Summer",
                                                     format(hotel_nocancel$newArrival, "%m") == '08' ~ "Summer",
                                                     format(hotel_nocancel$newArrival, "%m") == '09' ~ "Fall",
                                                     format(hotel_nocancel$newArrival, "%m") == '10' ~ "Fall",
                                                     format(hotel_nocancel$newArrival, "%m") == '11' ~ "Fall",
                                                     format(hotel_nocancel$newArrival, "%m") == '12' ~ "Winter",
                                                     format(hotel_nocancel$newArrival, "%m") == '01' ~ "Winter",
                                                     format(hotel_nocancel$newArrival, "%m") == '02' ~ "Winter"
))
#View(hotel_nocancel)

#Plot for seasons based on number of customers who checked or didnt show up.
tab2_season <- data.frame(table(hotel_nocancel$Season))
#View(tab2_season)
names(tab2_season)[names(tab2_season)=="Var1"] <- "Season" #Renaming tabular column

#Creating a plot for Seasons and number of customers
season_plot2 <- ggplot(data= tab2_season,aes(x=Season,y=Freq, color = Season, group =1)) +  geom_line(color = "Grey")+ geom_point(shape = 21, color="black", aes(fill=Season), size=3 ) +theme(legend.position = "none")
season_plot2 <- season_plot2 + ggtitle("Trend by Season") + ylab(" Number of Customers") 
season_plot2 <- season_plot2 + theme(
  plot.title = element_text(color="Black", size=12, face="bold.italic"),
  axis.title.x = element_text( size=8, face="bold"),
  axis.title.y = element_text( size=8, face="bold"))
season_plot2




#Creating visitor type variable based on total number of people
View(city_hotel)
str(city_hotel)
city_hotel$TotalPeople <-  city_hotel$Adults + city_hotel$Children + city_hotel$Babies
city_hotel$TotalChildren <- city_hotel$Babies + city_hotel$Children
str(city_hotel)
city_hotel<- mutate(city_hotel, VisitorType = case_when( TotalPeople  == 1~ "Single",
                                                         TotalPeople == 2 & Adults == 2 ~ "Couple",
                                                         TotalPeople > 2 ~ "Family",
                                                         TotalPeople >= 2 & Adults >= 1 & TotalChildren >= 1 ~ "Family" ,
                                                         TotalPeople == 0 ~ ""
))
                                                             

#Creating average revenue per stay attribute
city_hotel$AvgRevPerStay <- city_hotel$DaysStayed * city_hotel$ADR
View(city_hotel)


#Generating ADR box plots by customer type
 
boxplot_adr_customertype <- ggplot(data = city_hotel, aes(x = CustomerType, y= ADR, fill = CustomerType)) +geom_boxplot(outlier.colour="black", outlier.shape=1, outlier.size=1)+ scale_fill_brewer(palette="RdBu") + 
                            ggtitle("Average Daily Rate by Customer Type") + scale_y_continuous(limits = c(0,500),breaks = seq(0,500, by = 50)) + theme(legend.position = "none", plot.title = element_text(color="Black", size=11, face="bold.italic"), axis.title.x = element_text( size=9),
                                                                                                                                                     axis.title.y = element_text( size=9))
boxplot_adr_customertype

#Generating ADR box plot by Room Type
boxplot_adr_roomtype <- ggplot(data = city_hotel, aes(x = ReservedRoomType, y= ADR, fill = ReservedRoomType)) +geom_boxplot(outlier.colour="black", outlier.shape=1, outlier.size=1)+ scale_fill_brewer(palette="Spectral") + 
  ggtitle("Average Daily Rate by Room Type")+scale_y_continuous(limits = c(0,500),breaks = seq(0,500, by = 50)) +theme(legend.position = "none", plot.title = element_text(color="Black", size=11, face="bold.italic"), axis.title.x = element_text( size=9),
                                                                                                                           axis.title.y = element_text( size=9))
boxplot_adr_roomtype


#Generating ADR box plot by meal plan
boxplot_adr_mealtype <- ggplot(data = city_hotel, aes(x = Meal, y= ADR, fill = Meal)) +geom_boxplot(outlier.colour="black", outlier.shape=1, outlier.size=1)+ scale_fill_brewer(palette="PuOr",labels=c("Bed & Breakfast", "Full Board", "Half Board", "Unidentified")) + 
  ggtitle("Average Daily Rate by Meal Type")+scale_y_continuous(limits = c(0,500),breaks = seq(0,500, by = 50)) +theme(plot.title = element_text(color="Black", size=11, face="bold.italic"), axis.title.x = element_text( size=9),
                                                                                                                           axis.title.y = element_text( size=9)) 
boxplot_adr_mealtype


# How many times did people get a different room assigned
room_plot <- ggplot(data = city_hotel,aes(x= ReservedRoomType, y = AssignedRoomType)) + geom_jitter(shape = 21,size = 1, color = "#FF6701") + ggtitle("Room Type : Reserved vs Assigned") + xlab("Reserved Room") + ylab("Assigned Room") + 
            theme(plot.title = element_text(color="Black", size=11, face="bold.italic"), axis.title.x = element_text( size=9), axis.title.y = element_text( size=9))
room_plot

room_reserved_cancel_plot <- ggplot(data = city_hotel,aes(x= ReservedRoomType, y = IsCanceled)) + geom_jitter(shape = 21,size = 1, color = "#7393B3") + ggtitle("Cancelations based on Room Type Reserved") + xlab("Reserved Room") + ylab("Cancelation") + scale_y_discrete(labels = c("0" = "Not Canceled","1"="Canceled")) +
  theme(plot.title = element_text(color="Black", size=11, face="bold.italic"), axis.title.x = element_text( size=9), axis.title.y = element_text( size=9))
room_reserved_cancel_plot
# People who booked room type A and D are most likely to cancel. The bookings for P were definitely canceled. Under case of overbooking for one type of category, other rooms which are more likely to be canceled can be assigned.

room_assigned_Cancel_plot <- ggplot(data = city_hotel,aes(x= AssignedRoomType, y = IsCanceled)) + geom_jitter(shape = 21,size = 1, color = "#7393B3") + ggtitle("Cancelations based on Assigned Type Reserved") + xlab("Assigned Room") + ylab("Cancelation") + scale_y_discrete(labels = c("0" = "Not Canceled","1"="Canceled")) +
  theme(plot.title = element_text(color="Black", size=11, face="bold.italic"), axis.title.x = element_text( size=9), axis.title.y = element_text( size=9))
room_assigned_Cancel_plot


#-----Linear Models-----


# ADR based on room type, meal plan, number of people.
df <- data.frame(city_hotel$ADR,city_hotel$AssignedRoomType,city_hotel$Meal,city_hotel$TotalPeople)
head(df)

#Linear Model for ADR based on Assigned Room type
lm_room <- lm(data = city_hotel, ADR ~ AssignedRoomType)
lm_room
summary(lm_room)
r <- data.frame(AssignedRoomType = 'G')
pred_room <- predict(lm_room, r) #Predicting the ADR value for the roomtype assigned.
pred_room #184.474

#Linear Model for ADR based on Meal type chosen
lm_meal <- lm(data = city_hotel,ADR ~ Meal)
lm_meal
summary(lm_meal)
m <- data.frame(Meal = "HB")
pred_meal <- predict(lm_meal,m)
pred_meal #120.0553

#Linear Model for ADR based on total number of people per booking.
lm_people <- lm(data = city_hotel,ADR ~ TotalPeople)
lm_people
summary(lm_people)
p <- data.frame(TotalPeople = 5)
pred_people <- predict(lm_people,p)
pred_people #187.7978

#Linear Model for ADR based on Reserved roomtype and total number of people
lm_room_people <- lm(data= city_hotel,ADR ~ ReservedRoomType + TotalPeople)
lm_room_people
summary(lm_room_people)
rp <- data.frame(ReservedRoomType = "G", TotalPeople = 5)
pred_room_people <- predict(lm_room_people,rp)
pred_room_people #227.402

#Linear Model for ADR based on Reserved Room Type and Meal plan chosen.
lm_room_meal <- lm(data= city_hotel,ADR ~ ReservedRoomType + Meal )
lm_room_meal
summary(lm_room_meal)
rm <- data.frame(ReservedRoomType = "G", Meal = "HB")
pred_room_meal <- predict(lm_room_meal,rm)
pred_room_meal #220.0447

#Linear Model for ADR based on meal plan chosen and type of meal
lm_people_meal <- lm(data= city_hotel,ADR ~ TotalPeople + Meal)
lm_people_meal
summary(lm_people_meal)
pm <- data.frame(TotalPeople= 5,Meal = "HB")
pred_people_meal <- predict(lm_people_meal,pm)
pred_people_meal #200.5209

#Linear Model for ADR based on Reserved Roomtype, total number of people, meal chosen
lm_people_meal_room <- lm(ADR ~ ., data=city_hotel)

lm_people_meal_room
summary(lm_people_meal_room)
lm_sample <- data.frame(ReservedRoomType = "G", TotalPeople = 5, Meal ="HB")
pred_linear <- predict(lm_people_meal_room,lm_sample)
pred_linear #243.4406


#Season,Customer Type
lm_season <- lm(ADR~Season, data= city_hotel)
lm_season9
summary(lm_season)
se <- data.frame(Season = "Summer")
pred_season <- predict(lm_season, se)
pred_season #winter :94.51, Summer : 113.7119

#deposit time, reservation status, lead time
#creating a column for Deposit type numeric

city_hotel<- mutate(city_hotel, DepositFactor = case_when( DepositType  == 'No Deposit'~ 1,
                                                         DepositType == 'Non Refund' ~ 2,
                                                         DepositType == 'Refundable' ~ 3
))

#Creating a column for Reservation to numeric levels
city_hotel<- mutate(city_hotel, ReservationFactor = case_when( ReservationStatus == 'Canceled'~ 1,
                                                         ReservationStatus == 'Check-out' ~ 2,
                                                         ReservationStatus == 'No-Show' ~ 3
))

#city_hotel$IsCanceled <- as.numeric(city_hotel$IsCanceled)
View(city_hotel)
lm_lead <- lm(IsCanceled ~ LeadTime + DepositFactor +, data= city_hotel)
summary(lm_lead)
v <- data.frame(LeadTime = 500, DepositFactor = 1)
pred_deposit_lead <- predict(lm_lead,v)
pred_deposit_lead
ggplot(data=city_hotel) + aes(x= LeadTime, y=IsCanceled) + geom_point() + geom_smooth(method="lm", se=FALSE) 











#=========================Linear Model ============================
#IsCanceled,LeadTime,ArrivalDate, ReservationStatusDate, StayInWeekNights, Adults, Previous bookings not canceled, Booking changes
hotel1$TotalPeople <- city_hotel$TotalPeople
View(hotel1)
lm1 <- lm(data = city_hotel, ADR ~ .)
summary(lm1)




#Generating apriori rules for who is likely to cancel.
library(arules)
library(arulesViz)
city_hotel$IsCanceled <- as.factor(city_hotel$IsCanceled)
city_hotel$DepositType <- as.factor(city_hotel$DepositType)
city_hotel$Meal <- as.factor(city_hotel$Meal)
city_hotel$CustomerType <- as.factor(city_hotel$CustomerType)
city_hotel$Season <- as.factor(city_hotel$Season)
city_hotel$Country <- as.factor(city_hotel$Country)
city_hotel$ReservedRoomType <- as.factor(city_hotel$ReservedRoomType)

city_sample <- city_hotel[ , c("IsCanceled" , "DepositType" , "Meal" , "CustomerType" , "Season","Country", "ReservedRoomType")]
city_sample_transactions <- as(city_sample,"transactions")
itemLabels(city_sample_transactions)
rules_cancel <- apriori(city_sample_transactions, parameter = list(support = 0.05,conf = 0.5),
                        control = list(verbose = FALSE),
                          appearance = list(default = "lhs", rhs = ( "IsCanceled=1" )))
inspect(rules_cancel)
itemLabels(rules_cancel)
itemFrequencyPlot(city_sample_transactions,cex.names = 0.7)
plot(rules_cancel,method = "graph",engine = "htmlwidget")



#===== Checking seasonality for only checkout dates

sample_hotel <- data.frame(city_hotel$newArrival,city_hotel$ReservationStatus)
View(sample_hotel)
sample_hotel<- sample_hotel[(sample_hotel$city_hotel.ReservationStatus != 'Canceled' & sample_hotel$city_hotel.ReservationStatus != 'No-Show'), ]

#Extracting arrival months only for checked-out customers
tab8 <- format(sample_hotel$city_hotel.newArrival, "%m") # Getting month of the arrival date
tab8 <- data.frame(table(tab8)) # generating a table of months to get their total counts and converting it into a dataframe for further analysis
names(tab8)[names(tab8)== "tab8"] <- "Month" #Renaming the Column name in the table
View(tab8)
#Creating a plot for the month and number of customers that checked out.
p <- ggplot(data = tab8, aes( x= Month, y= Freq, group = 1)) + ggtitle("Check-in")+geom_line(data = tab8, aes( x= Month, y= Freq, group =1)) + geom_point(color = "green", data = tab8, aes( x= Month, y= Freq, group =1)) + scale_x_discrete(labels=month.abb) 
#+ scale_y_continuous(breaks = seq(1000,6000, by = 500))
p


#When were most cancellations done.
sample_cancel <- data.frame(city_hotel$ReservationStatusDate,city_hotel$ReservationStatus, city_hotel$IsCanceled)
sample_cancel <- sample_cancel[(sample_cancel$city_hotel.ReservationStatus != 'Check-out'), ]

cancel_table <- format(sample_cancel$city_hotel.ReservationStatusDate,"%m")
cancel_table <- data.frame(table(cancel_table))
View(cancel_table)
names(cancel_table)[names(cancel_table)=="cancel_table"] <- "Month"
names(cancel_table)[names(cancel_table)=="Freq"] <- "Freq_cancel"
c <- ggplot() +ggtitle("cancelations")+ geom_line(data = cancel_table, aes( x= Month, y= Freq_cancel ,group = 1)) + geom_point(color = "red",data = cancel_table, aes( x= Month, y= Freq_cancel ,group = 1)) + scale_y_continuous(breaks = seq(1000,15000, by = 500))
c

sample_table <- table(cancel_table,tab8)
dff <- data.frame(sample_table)
View(dff)
b <- merge(cancel_table,tab8, by = "Month")
View(b)
plot <- ggplot(data =b, aes(x=Month)) + geom_line(data = b, aes( y= Freq, group = 1), color = "red")+geom_point(data = b, aes( y= Freq, group = 1)) + geom_line(data = b, aes(y= Freq_cancel ,group = 1))+geom_point(data = b, aes(y= Freq_cancel ,group = 1)) + scale_x_discrete(labels=month.abb)
plot






#=========SVM========
library(kernlab)
library(caret)
hotel_train_list <- createDataPartition(city_hotel$IsCanceled, p = .40,list = FALSE)
hotel_train_set <- city_hotel[hotel_train_list,]
View(hotel_train_set)
str(hotel_train_set)
# hotel_test_set <- city_hotel[-hotel_train_list,]
# jh <- data.frametable(hotel_train_set$VisitorType)
svmOut <- ksvm(IsCanceled ~ PreviousCancellations + ReservedRoomType, data= hotel_train_set,kernel ="rbfdot", kpar="automatic" , C=5, cross= 3,prob.model= T)   

#plot(svmOut,hotel_train_set)
svm_pred <- predict(svmOut, newdata = hotel_test_set)
confusionMatrix(svm_pred,hotel_test_set$IsCanceled)

svm_pred
svmOut
summary(svmOut)

install.packages("e1071")
library(e1071)


