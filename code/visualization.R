library(data.table)
library(ggplot2)
library(cowplot)
library(dplyr)
library(tibbletime)
library(data.table)
library(tidyr)
library(lubridate)
library(tidyverse)
library(gridExtra) # multiple plots in 1
library(magick) # attach dope image for visual
library(scales) # show the colors
library(ggrepel)
library(ggpubr)
setwd("~/Desktop/5330- Data Mining  2/santander")
train <- fread("train_tidy.csv")
test<-fread("tidy_test.csv")

#================Visualization
my_color <- c("#FA6E4F", "#F2CF59", "#FB8E7E", "#C5D7C0", "#8EC9BB", "#F8CA9D")

my_theme <- theme(
  text = element_text(color = "grey35"),
  plot.title = element_text(hjust=0.5),
  axis.title = element_text(size = 15),
  axis.text = element_text(size = 11),
  axis.line = element_line(size = 1.2, color = "grey35"),
  legend.box.background = element_rect(color = "grey75", size = 1),
  legend.box.margin = margin(t = 5, r = 5, b = 5, l = 5),
  legend.title = element_text(face = "bold"))


#=============================train - change to desried mode ===================
train$fecha_dato <- as.POSIXct(strptime(train$fecha_dato, format = "%Y-%m-%d"))
train$ind_empleado<-as.factor(train$ind_empleado)
train$pais_residencia<-as.factor(train$pais_residencia)
train$sexo<-as.factor(train$sexo)
train$fecha_alta <- as.POSIXct(strptime(train$fecha_alta, format = "%Y-%m-%d"))
train$ind_nuevo<-as.factor(train$ind_nuevo)
train$indrel<-as.factor(train$indrel)
train$indrel_1mes<-as.factor(train$indrel_1mes)



#=========================test - change variables to desired mode ===========================================
test$fecha_dato <- as.POSIXct(strptime(test$fecha_dato, format = "%Y-%m-%d"))
test$ind_empleado<-as.factor(test$ind_empleado)
test$pais_residencia<-as.factor(test$pais_residencia)
test$sexo<-as.factor(test$sexo)
test$fecha_alta <- as.POSIXct(strptime(test$fecha_alta, format = "%Y-%m-%d"))
test$ind_nuevo<-as.factor(test$ind_nuevo)
test$indrel<-as.factor(test$indrel)
test$indrel_1mes<-as.factor(test$indrel_1mes)


#=================================Data Visualization ==============================
#==================age =================
#=========Age with responses 
#create age intervals 
train$age_interval<-as.character(train$age)
train$age_interval[(train$age >=18)&(train$age<=30)] <- '18_30' 
train$age_interval[(train$age >=31)&(train$age<=40)] <- '31_40' 
train$age_interval[(train$age >=41)&(train$age<=50)] <- '41_50' 
train$age_interval[(train$age >=51)&(train$age<=60)] <- '51_60' 
train$age_interval[(train$age >=61)&(train$age<=70)] <- '61_70' 
train$age_interval[(train$age >=71)&(train$age<=100)] <- '71_100' 
table(train$age_interval)
#create a new dataset to plot 
count_ones <- function(x) {
   return(table(x)["1"])
}
new_set <- aggregate(train[ ,c(24:47, 52)], by = list(train$age_interval), FUN = count_ones)
age_product_count<-new_set[,-26]

#plot- age with response 
data_age<- t(age_product_count[,-1])
write.csv(data_age, file = "data_age.csv")
data_age <- read.csv("~/Desktop/5330- Data Mining  2/santander/data_age.csv")

#18-30
p1<-ggplot(data=data_age, aes(x = data_age$X, y=data_age$V1)) +
   geom_bar(stat = "identity") +
   theme_minimal()+
   my_theme+
   labs(x = "Finanical Product", y = "Total Number of Products", title = "Age: 18-30") + 
   coord_flip()
#31-40
p2<-ggplot(data=data_age, aes(x = data_age$X, y=data_age$V2)) +
   geom_bar(stat = "identity") +
   theme_minimal()+
   my_theme+
   labs(x = "Finanical Product", y = "Total Number of Products", title = "Age: 31-40") + 
   coord_flip()
#41-50
p3<-ggplot(data=data_age, aes(x = data_age$X, y=data_age$V3)) +
   geom_bar(stat = "identity") +
   theme_minimal()+
   my_theme+
   labs(x = "Finanical Product", y = "Total Number of Products", title = "Age: 41-50") + 
   coord_flip()
#51-60
p4<-ggplot(data=data_age, aes(x = data_age$X, y=data_age$V4)) +
   geom_bar(stat = "identity") +
   theme_minimal()+
   my_theme+
   labs(x = "Finanical Product", y = "Total Number of Products", title = "Age: 51-60") + 
   coord_flip()
#61-70
p5<-ggplot(data=data_age, aes(x = data_age$X, y=data_age$V5)) +
   geom_bar(stat = "identity") +
   theme_minimal()+
   my_theme+
   labs(x = "Finanical Product", y = "Total Number of Products", title = "Age: 61-70") + 
   coord_flip()
#71-100
p6<-ggplot(data=data_age, aes(x = data_age$X, y=data_age$V6)) +
   geom_bar(stat = "identity") +
   theme_minimal()+
   my_theme+
   labs(x = "Finanical Product", y = "Total Number of Products", title = "Age: 71-100") + 
   coord_flip()

ggarrange(p1, p2,p3,p4,p5,p6, ncol = 2, nrow = 3)

#=======Age Distribution
#====train
age_train<-ggplot(data=train,aes(x=age)) + 
   geom_bar(alpha=0.75) +
   ggtitle("Age Distribution in Training Dataset") + 
   theme_minimal()+
   my_theme+
   labs(x = "Age", y= "Number of Customers") 
#====test
age_test<-ggplot(data=test,aes(x=age)) + 
   geom_bar(alpha=0.75) +
   ggtitle("Age Distribution in Testing Dataset") + 
   theme_minimal()+
   my_theme+
   labs(x = "Age", y= "Number of Customers")
ggarrange(age_train,age_test, ncol = 2, nrow = 1)

#==================antiguedad =================
#=======Anitiguedad Distribution
#====train
Antiguedad_train<-ggplot(data=train,aes(x=antiguedad)) + 
   geom_bar(alpha=0.75) +
   ggtitle("Antiguedad Distribution in Training Dataset") + 
   theme_minimal()+
   my_theme+
   labs(x = "Antiguedad - Customer Seniority in Months", y= "Number of Customers") 
#====test
Antiguedad_test<-ggplot(data=test,aes(x=antiguedad)) + 
   geom_bar(alpha=0.75) +
   ggtitle("Antiguedad Distribution in Testing Dataset") + 
   theme_minimal()+
   my_theme+
   labs(x = "Antiguedad - Customer Seniority in Months", y= "Number of Customers")
ggarrange(Antiguedad_train,Antiguedad_test, ncol = 2, nrow = 1)
#=========Antiguedad with responses 
table(test$antiguedad)
#create antidad intervals 
train$anti_interval<-as.character(train$antiguedad)
train$anti_interval[(train$antiguedad >=0)&(train$antiguedad<=60)] <- '0_60_month' 
train$anti_interval[(train$antiguedad >=61)&(train$antiguedad<=120)] <- '61_120_month' 
train$anti_interval[(train$antiguedad >=121)&(train$antiguedad<=180)] <- '121_180_month' 
train$anti_interval[(train$antiguedad >=181)&(train$antiguedad<=257)] <- '181_257_month' 
#create new dataset to plot
anti_product_count <- aggregate(train[ ,c(24:47, 53)], by = list(train$anti_interval), FUN = count_ones)
anti_product_count<-anti_product_count[,-26]
data_anti<- t(anti_product_count[,-1])
write.csv(data_anti, file = "data_anti.csv")
data_anti <- read.csv("~/Desktop/5330- Data Mining  2/santander/data_anti.csv")
#0-60 months
anti1<-ggplot(data=data_anti, aes(x = data_anti$X, y=data_anti$V1)) +
   geom_bar(stat = "identity") +
   theme_minimal()+
   my_theme+
   labs(x = "Finanical Product", y = "Total Number of Products", title = "Customer Seniority: 0-60 Months") + 
   coord_flip()
#61-120months
anti2<-ggplot(data=data_anti, aes(x = data_anti$X, y=data_anti$V2)) +
   geom_bar(stat = "identity") +
   theme_minimal()+
   my_theme+
   labs(x = "Finanical Product", y = "Total Number of Products", title = "Customer Seniority: 61-120 Months") + 
   coord_flip()
#121-180 months
anti3<-ggplot(data=data_anti, aes(x = data_anti$X, y=data_anti$V3)) +
   geom_bar(stat = "identity") +
   theme_minimal()+
   my_theme+
   labs(x = "Finanical Product", y = "Total Number of Products", title = "Customer Seniority: 121-180 Months") + 
   coord_flip()
#181-257 months
anti4<-ggplot(data=data_anti, aes(x = data_anti$X, y=data_anti$V4)) +
   geom_bar(stat = "identity") +
   theme_minimal()+
   my_theme+
   labs(x = "Finanical Product", y = "Total Number of Products", title = "Customer Seniority: 180-257") + 
   coord_flip()
ggarrange(anti1, anti2,anti3,anti4,ncol = 2, nrow = 2)


#==================fecha_alta =================
summary(train$fecha_alta)
#====train
fecha_alta_train<-ggplot(data=train,aes(x=fecha_alta)) + 
   geom_bar(alpha=0.75) +
   ggtitle("Fecha_alta Distribution in Training Dataset") + 
   theme_minimal()+
   my_theme+
   labs(x = "Fecha-alta  - The date when customer became the first holder of a contract", y= "Number of Customers") 
#====test
fecha_alta_test<-ggplot(data=test,aes(x=fecha_alta)) + 
   geom_bar(alpha=0.75) +
   ggtitle("Fecha_alta Distribution in Testing Dataset") + 
   theme_minimal()+
   my_theme+
   labs(x = "Fecha-alta  - The date when customer became the first holder of a contract", y= "Number of Customers")
ggarrange(fecha_alta_train, fecha_alta_test, ncol = 2, nrow = 1)
#=========fecha_alta with responses 
#create antidad intervals 
train$alta_interval<-train$fecha_alta
train$alta_interval[(train$fecha_alta >="1995-01-01")&(train$fecha_alta<="2000-12-31")] <- '1995-01-01'
train$alta_interval[(train$fecha_alta >="2001-01-01")&(train$fecha_alta<="2005-12-31")] <- '2001-01-01'
train$alta_interval[(train$fecha_alta >="2006-01-01")&(train$fecha_alta<="2010-12-31")] <- '2006-01-01'
train$alta_interval[(train$fecha_alta >="2011-01-01")&(train$fecha_alta<="2016-12-31")] <- '2011-01-01'
train$alta_interval<-as.character(train$alta_interval)
train$alta_interval[train$alta_interval=="1995-01-01"] <-'1995-2000'
train$alta_interval[train$alta_interval=="2001-01-01"] <-'2001-2005'
train$alta_interval[train$alta_interval=="2006-01-01"] <-'2006-2010'
train$alta_interval[train$alta_interval=="2011-01-01"] <-'2011-2016'
#create new dataset to plot
alta_product_count <- aggregate(train[ ,c(24:47, 54)], by = list(train$alta_interval), FUN = count_ones)
alta_product_count<-alta_product_count[,-26]
data_alta<- t(alta_product_count[,-1])
write.csv(data_alta, file = "data_alta.csv")
data_alta <- read.csv("~/Desktop/5330- Data Mining  2/santander/data_alta.csv")
#1995-2000
alta1<-ggplot(data=data_alta, aes(x = data_alta$X, y=data_alta$V1)) +
   geom_bar(stat = "identity") +
   theme_minimal()+
   my_theme+
   labs(x = "Finanical Product", y = "Total Number of Products", title = "Fecha_alta: 1995-2000") + 
   coord_flip()
#2001-2005
alta2<-ggplot(data=data_alta, aes(x = data_alta$X, y=data_alta$V2)) +
   geom_bar(stat = "identity") +
   theme_minimal()+
   my_theme+
   labs(x = "Finanical Product", y = "Total Number of Products", title = "Fecha_alta: 2000-2005") + 
   coord_flip()
#2006-2010
alta3<-ggplot(data=data_alta, aes(x = data_alta$X, y=data_alta$V3)) +
   geom_bar(stat = "identity") +
   theme_minimal()+
   my_theme+
   labs(x = "Finanical Product", y = "Total Number of Products", title = "Fecha_alta: 2006-2010") + 
   coord_flip()
#2011-2016
alta4<-ggplot(data=data_alta, aes(x = data_alta$X, y=data_alta$V4)) +
   geom_bar(stat = "identity") +
   theme_minimal()+
   my_theme+
   labs(x = "Finanical Product", y = "Total Number of Products", title = "Fecha_alta: 2011-2016") + 
   coord_flip()
ggarrange(alta1, alta2,alta3,alta4,ncol = 2, nrow = 2)


# WoldMap
#Reference: https://www.kaggle.com/donyoe/santander-quick-first-view
library(rworldmap)
#train
customer_by_country <- train[,.N,by=pais_residencia] # Number of customers by country
fr <- joinCountryData2Map(dF = customer_by_country,joinCode = "ISO2",nameJoinColumn = "pais_residencia",verbose=F) # Prepare data to plot
map_train<-mapCountryData(mapToPlot = fr,nameColumnToPlot = "N",catMethod = "logFixedWidth",
               oceanCol = "steelblue",missingCountryCol = "white",
               mapTitle = "Number of customers by country in Training Dataset", addLegend=FALSE)
#test
customer_by_country_test <- test[,.N,by=pais_residencia] # Number of customers by country
fr_test <- joinCountryData2Map(dF = customer_by_country_test,joinCode = "ISO2",nameJoinColumn = "pais_residencia",verbose=F) # Prepare data to plot
map_test<-mapCountryData(mapToPlot = fr_test,nameColumnToPlot = "N",catMethod = "logFixedWidth",
               oceanCol = "steelblue",missingCountryCol = "white",
               mapTitle = "Number of customers by country in Testing Datset", addLegend=FALSE)


#==================sex =================
train$sex<-as.character(train$sexo)
train$sex[train$sexo=="H"] <-'Female'
train$sex[train$sexo=="V"] <-'Male'
test$sex<-as.character(test$sexo)
test$sex[test$sexo=="H"] <-'Female'
test$sex[test$sexo=="V"] <-'Male'
#====train
sex_train<-ggplot(data=train,aes(x=sex)) + 
   geom_bar(alpha=0.75) +
   ggtitle("Sex Distribution in Training Dataset") + 
   theme_minimal()+
   my_theme+
   labs(x = "Sex", y= "Number of Customers") 
#====test
sex_test<-ggplot(data=test,aes(x=sex)) + 
   geom_bar(alpha=0.75) +
   ggtitle("Sex Distribution in Testing Dataset") + 
   theme_minimal()+
   my_theme+
   labs(x = "Sex", y= "Number of Customers")
ggarrange(sex_train, sex_test, ncol = 2, nrow = 1)

#==================ind_empleado =================
#====train
emp_train<-ggplot(data=train,aes(x=ind_empleado)) + 
   geom_bar(alpha=0.75) +
   ggtitle("Employee Index Distribution in Training Dataset") + 
   theme_minimal()+
   my_theme+
   labs(x = "Employee Index", y= "Number of Customers") 
#====test
emp_test<-ggplot(data=test,aes(x=ind_empleado)) + 
   geom_bar(alpha=0.75) +
   ggtitle("Employee Distribution in Testing Dataset") + 
   theme_minimal()+
   my_theme+
   labs(x = "Employee Index", y= "Number of Customers")
ggarrange(emp_train, emp_test, ncol = 2, nrow = 1)
table(train$ind_empleado)
table(test$ind_empleado)


#=========pattern
#Reference:https://www.kaggle.com/apryor6/detailed-cleaning-visualization
#==create new datasets 
#train 2015
train_2015<-train[train$year_dato == 2015]
train_2015<-filter(train_2015, (month_dato == 1)|(month_dato == 2)|(month_dato == 3)|(month_dato == 4)|(month_dato == 5))

#train 2016
train_2016<-train[train$year_dato == 2016]


#=====train_2015 create dataset for plot
train_2015$total.services <- rowSums(train_2015[,24:47],na.rm=TRUE)

train_2015 <- train_2015 %>% arrange(fecha_dato)
train_2015$month.id <- as.numeric(factor((train_2015$fecha_dato)))
train_2015$month.next.id <- train_2015$month.id + 1

status.change <- function(x){
   if ( length(x) == 1 ) { # if only one entry exists, I'll assume they are a new customer and therefore are adding services
      label = ifelse(x==1,"Added","Maintained")
   } else {
      diffs <- diff(x) # difference month-by-month
      diffs <- c(0,diffs) # first occurrence will be considered Maintained, which is a little lazy. A better way would be to check if the earliest date was the same as the earliest we have in the dataset and consider those separately. Entries with earliest dates later than that have joined and should be labeled as "Added"
      label <- rep("Maintained", length(x))
      label <- ifelse(diffs==1,"Added",
                      ifelse(diffs==-1,"Dropped",
                             "Maintained"))
   }
   label
}

train_2015[,24:47] <- lapply(train_2015[,24:47], function(x) return(ave(x,train_2015$ncodpers, FUN=status.change)))

interesting <- rowSums(train_2015[,24:47]!="Maintained")
train_2015  <- train_2015[interesting>0,]
train_2015  <- train_2015  %>%
   gather(key=feature,
          value=status,
          ind_ahor_fin_ult1:ind_recibo_ult1)
train_2015 <- filter(train_2015 ,status!="Maintained")
#plot train_2015
month.counts <- table(unique(train_2015$month.id)%%5)
cur.names <- names(month.counts)
names(month.counts) <- cur.names
month.counts<- data.frame(month.counts) %>%
   rename(month_dato=Var1,month.count=Freq) %>% mutate(month_dato=as.numeric(month_dato))

train_2015 %>% 
   group_by(month_dato,feature,status) %>%
   summarise(counts=n())%>%
   ungroup() %>%
   inner_join(month.counts,by="month_dato") %>%
   
   mutate(counts=counts/month.count) %>%
   ggplot(aes(y=counts,x=factor(month.abb[month_dato],levels=month.abb[seq(12,1,-1)]))) +
   geom_bar(aes(fill=status), stat="identity") +
   facet_wrap(facets=~feature,ncol = 6) +
   coord_flip() +
   my_theme + 
   ylab("Count") +
   xlab("") + 
   ggtitle("Service Changes by Month (Jan - May) in 2015") +
   theme(axis.text    = element_text(size=10),
         legend.text  = element_text(size=14),
         legend.title = element_blank()      ,
         strip.text   = element_text(face="bold")) +
   scale_fill_manual(values=c("#FA6E4F", "#F2CF59"))



#=====train_2016 create dataset for plot
train_2016$total.services <- rowSums(train_2016[,24:47],na.rm=TRUE)

train_2016 <- train_2016 %>% arrange(fecha_dato)
train_2016$month.id <- as.numeric(factor((train_2016$fecha_dato)))
train_2016$month.next.id <- train_2016$month.id + 1

status.change <- function(x){
   if ( length(x) == 1 ) { # if only one entry exists, I'll assume they are a new customer and therefore are adding services
      label = ifelse(x==1,"Added","Maintained")
   } else {
      diffs <- diff(x) # difference month-by-month
      diffs <- c(0,diffs) # first occurrence will be considered Maintained, which is a little lazy. A better way would be to check if the earliest date was the same as the earliest we have in the dataset and consider those separately. Entries with earliest dates later than that have joined and should be labeled as "Added"
      label <- rep("Maintained", length(x))
      label <- ifelse(diffs==1,"Added",
                      ifelse(diffs==-1,"Dropped",
                             "Maintained"))
   }
   label
}

train_2016[,24:47] <- lapply(train_2016[,24:47], function(x) return(ave(x,train_2016$ncodpers, FUN=status.change)))

interesting_2016 <- rowSums(train_2016[,24:47]!="Maintained")
train_2016  <- train_2016[interesting_2016>0,]
train_2016  <- train_2016  %>%
   gather(key=feature,
          value=status,
          ind_ahor_fin_ult1:ind_recibo_ult1)
train_2016 <- filter(train_2016 ,status!="Maintained")
#plot train_2015
month.counts <- table(unique(train_2016$month.id)%%5)
cur.names <- names(month.counts)
names(month.counts) <- cur.names
month.counts<- data.frame(month.counts) %>%
   rename(month_dato=Var1,month.count=Freq) %>% mutate(month_dato=as.numeric(month_dato))

train_2016 %>% 
   group_by(month_dato,feature,status) %>%
   summarise(counts=n())%>%
   ungroup() %>%
   inner_join(month.counts,by="month_dato") %>%
   
   mutate(counts=counts/month.count) %>%
   ggplot(aes(y=counts,x=factor(month.abb[month_dato],levels=month.abb[seq(12,1,-1)]))) +
   geom_bar(aes(fill=status), stat="identity") +
   facet_wrap(facets=~feature,ncol = 6) +
   coord_flip() +
   my_theme + 
   ylab("Count") +
   xlab("") + 
   ggtitle("Service Changes by Month (Jan - May) in 2016") +
   theme(axis.text    = element_text(size=10),
         legend.text  = element_text(size=14),
         legend.title = element_blank()      ,
         strip.text   = element_text(face="bold")) +
   scale_fill_manual(values=c("#FA6E4F", "#F2CF59"))

