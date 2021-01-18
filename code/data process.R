setwd("/Users/guu/Documents/MATH/University of Virginia/STAT 5330 Data Mining/Project")

#test<-read.csv("test_ver2.csv",na.strings = c("",NA,"NA"))

#train2<-read.csv("train_ver2.csv")
#test<-read.csv("test_ver2.csv")


#sapply(train3, function(x)sum(is.na(x)))
#sapply(test, function(x)sum(is.na(x)))

library(data.table)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)

#for (i in 1:48) {
#  n<-sum(is.na(train[,i]))
#  print(n)
#}



#----------------------------------------
train3 <- fread("train_ver2.csv",nrows=-1,na.strings = c("",NA,"NA"))
sapply(train3, function(x)sum(is.na(x)))


#unique.id <- unique(train3$ncodpers)
##sample 350000 people to be our new train3
#limit.people <- 3.5e5
#unique.id <- unique.id[sample(length(unique.id), limit.people)]
#train3 <- train3[train3$ncodpers %in% unique.id]
#str(train3)

##convert fecha_dato and fecha_alta to date
train3$fecha_dato <- as.POSIXct(strptime(train3$fecha_dato, format = "%Y-%m-%d"))
train3$fecha_alta <- as.POSIXct(strptime(train3$fecha_alta, format = "%Y-%m-%d"))

##############Lets deal with missing values###############################
###At least the customers with NA ind_nuevo seem to be new customers. Replave NA with 1
train3$ind_nuevo[is.na(train3$ind_nuevo)] <- 1

##substitude missing fecha_alta with median of other new customers
train3$fecha_alta[is.na(train3$fecha_alta)] <- median(train3$fecha_alta[train3$ind_nuevo == 1], na.rm = TRUE)

##create a month column
train3$month_dato <- month(train3$fecha_dato)
train3$month_alta <- month(train3$fecha_alta)
##create a year column
train3$year_dato <- year(train3$fecha_dato)
train3$year_alta <- year(train3$fecha_alta)

##substitude the outliers(<18 or >100) into the median of their nearest age group
train3$age[train3$age <18] <- median(train3$age[(train3$age >=18) & (train3$age <= 30)], na.rm = TRUE) 
train3$age[train3$age >100] <- median(train3$age[(train3$age > 30) & (train3$age <= 100)], na.rm = TRUE) 
##substitude NAs with median age of sample
train3$age[is.na(train3$age)] <- median(train3$age, na.rm = TRUE)

##substitude  NA to 0. -999999 to 6
train3$antiguedad[is.na(train3$antiguedad)]<- 0
train3$antiguedad[(train3$antiguedad == -999999)]<- 6

##fill in missing value with 1
train3$indrel[is.na(train3$indrel)]<- 1

###deal with ind_actividad_cliente
train3$ind_actividad_cliente[is.na(train3$ind_actividad_cliente)] <- median(train3$ind_actividad_cliente[train3$ind_nuevo ==1],na.rm=TRUE)

###deal with nomprov, NA as UNKOWN
train3$nomprov[is.na(train3$nomprov)] <- "UNKNOWN" 


##fill renta with province median income
new.incomes <-train3 %>%
  select(nomprov) %>%
  merge(train3 %>%
          group_by(nomprov) %>%
          summarise(med.income=median(renta,na.rm=TRUE)),by="nomprov") %>%
  select(nomprov,med.income) %>%
  arrange(nomprov)
train3 <- arrange(train3,nomprov)
train3$renta[is.na(train3$renta)] <- new.incomes$med.income[is.na(train3$renta)]
rm(new.incomes)

train3$renta[is.na(train3$renta)] <- median(train3$renta,na.rm=TRUE)
train3 <- arrange(train3,fecha_dato)

train3$ind_nomina_ult1[is.na(train3$ind_nomina_ult1)] <- 0
train3$ind_nom_pens_ult1[is.na(train3$ind_nom_pens_ult1)] <- 0


sapply(train3, function(x)sum(is.na(x)))

train3$indfall[is.na(train3$indfall)]                 <- "N"

train3$tiprel_1mes[is.na(train3$tiprel_1mes)]         <- "I"
train3$indrel_1mes[is.na(train3$indrel_1mes)]         <- "1"
train3$indrel_1mes[train3$indrel_1mes=="P"]        <- "5" # change to just numbers because it currently contains letters and numbers

table(train3$indrel_1mes)




#ATTENTION!
train3$indrel_1mes                             <- as.factor(as.integer(train3$indrel_1mes))
table(train3$indrel_1mes)

train3$pais_residencia[is.na(train3$pais_residencia)] <- "UNKNOWN"
train3$canal_entrada[is.na(train3$canal_entrada)]     <- "UNKNOWN"
train3$sexo[is.na(train3$sexo)]                       <- "UNKNOWN"
train3$ult_fec_cli_1t[is.na(train3$ult_fec_cli_1t)]   <- "UNKNOWN"
train3$ind_empleado[is.na(train3$ind_empleado)]       <- "UNKNOWN"
train3$indext[is.na(train3$indext)]                   <- "UNKNOWN"
train3$indresi[is.na(train3$indresi)]                 <- "UNKNOWN"
train3$conyuemp[is.na(train3$conyuemp)]               <- "UNKNOWN"
train3$segmento[is.na(train3$segmento)]               <- "UNKNOWN"

str(train3)

train <- subset(train3, select = -c(tipodom,cod_prov))
summary(train)

write.csv(train, "tidy.csv")





