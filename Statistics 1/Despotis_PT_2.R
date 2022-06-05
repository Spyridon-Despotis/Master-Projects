

###Exercise 1-----


Drugs1 <- read.csv("Drugs.txt")
Drugs <- subset( Drugs1, select = -FLAG_CODES )

###Exercise 2------


length(unique(Drugs$LOCATION, incomparables = FALSE))


location_time_table <- table(Drugs$LOCATION)
location_time_table <- sort(location_time_table)

location_time_table  <- as.data.frame(location_time_table)
location_time_table <- setNames(location_time_table, c("country","freq"))
location_time_table[order(location_time_table$freq ),]


###Exercise 3----


quantile(location_time_table$freq)

q75=quantile(location_time_table$freq,.75)

ss <- subset(location_time_table,  subset=freq >=q75, select = country)

Drugs2=subset(Drugs, subset= LOCATION %in% ss$country)



###Exercise 4----

library(tidyverse)
drugs3=Drugs2 %>% pivot_longer(-c(TIME,LOCATION))


library(ggplot2)
ggplot(drugs3) +
 aes(x = TIME, y = value, colour = LOCATION) +
 geom_line(size = 0.5) +
 
 theme_minimal() +
 theme(legend.position = "bottom") +
 facet_wrap(vars(name), 
 scales = "free_y")



###Exercise 5----

  
# probabilities for every metric


    bel<- Drugs[Drugs$LOCATION == 'BEL',]

    metrics=names(bel)[3:6]
    
    FiveYeProbs=c()
    
    YearlyProbs=c()
    
    j=1
    
    for (i in metrics){
      
    dat=unlist(subset(bel,select=i))
    
    x= sum(diff(dat)>0)
    
    prop=x/length(diff(dat))
    
    YearlyProbs[j]=prop
    
    FiveYeProbs[j]= sum(dbinom(c(4,5),5,prop))
    
    j=j+1
  }
  
  Bel_list=list(
    Data=bel,
    Years=range(bel$TIME),
    Data.points=nrow(bel),
    YearlyProbs=YearlyProbs,
    FiveYeProbs=FiveYeProbs
    
  )

  
  # Exercise 6 ------
  
  myfun=function(DATA=NULL, METRIC="PC_GDP", nofY=5){
    
    XX=nrow(DATA)
    
    if( XX>10){
   
    minYear=  min(DATA$TIME)
    maxYear= max(DATA$TIME)
 
 countrycode= unique(DATA$LOCATION)

 # select column and convert to vector
 dat=unlist(subset(DATA,select=METRIC))

 x= sum(diff(dat)>0)
 
 prop=x/length(diff(dat))
 
 # probability that X=nofY-1 or X= nofY
 estimated_probability= round(sum(dbinom(c(nofY-1,nofY),nofY,prop)),3)
      
 print(paste("Based on",XX, "datapoints from years" , minYear, "to" , maxYear , ", the probability that"  , countrycode, "will increase its drug expenditure, in terms of", METRIC,", in at  least ", nofY-1, "years in the period" ,maxYear+1, "to" , maxYear+1+nofY, "is" ,estimated_probability))
 
    }else{
print("Unable to calculate probability (n<10)")
    }
  }

  # examples
  
myfun(DATA=Drugs[Drugs$LOCATION=="DEU",],METRIC="PC_GDP", nofY=10)

myfun(DATA=Drugs[Drugs$LOCATION=="BEL",],METRIC="PC_HEALTHXP", nofY=8)  

myfun(DATA=Drugs[Drugs$LOCATION=="NLD",],METRIC="USD_CAP", nofY=3)  

myfun(DATA=Drugs[Drugs$LOCATION=="NOR",],METRIC="TOTAL_SPEND", nofY=4) 

myfun(DATA=Drugs[Drugs$LOCATION=="FRA",][1:15,],METRIC="USD_CAP", nofY=4) 

myfun(DATA=Drugs[Drugs$LOCATION=="FRA",][1:10,],METRIC="TOTAL_SPEND", nofY=4) 
