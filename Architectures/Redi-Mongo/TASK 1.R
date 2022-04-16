setwd("E:/Thanasis/Documents/Msc/Big Data Systems and Architectures/Redis-Mongo Assignment-v1.3")

# Libraries
library("redux")
library(ggplot2)
library(dplyr)
library(ggpubr)
library(plyr)

# Connect to the local instance of REDIS
r <- redux::hiredis(
  redux::redis_config(
    host = "127.0.0.1", 
    port = "6379"))

#Read dataset 1
df1 = read.csv("emails_sent.csv") 

#Read dataset 2
df2 = read.csv("modified_listings.csv")


# -------1.1 # How many users modified their listing on January?----------

for(i in 1:nrow(df2)){
  if((df2$ModifiedListing[i] == 1) & (df2$MonthID[i] == 1)) {
    r$SETBIT ("ModificationsJanuary", df2$UserID[i], "1")
  }
}
out1 = r$BITCOUNT("ModificationsJanuary")
out1

# -------1.2 # How many users did NOT modify their listing on January?----

r$BITOP("NOT","NoModificationsJanuary","ModificationsJanuary")
out2 = r$BITCOUNT("NoModificationsJanuary")
out2

total_users = length(unique(df2$UserID))
isTRUE(out1 + out2 == total_users)
out1 + out2
total_users


# -------1.3 # How many users received at least one e-mail per month?----------

for(i in 1:nrow(df1)){
  if (df1$MonthID[i] == 1){
    r$SETBIT("EmailsJanuary", df1$UserID[i] , "1")
  } 
  else if (df1$MonthID[i] == 2){
    r$SETBIT("EmailsFebruary", df1$UserID[i] , "1")
  } else {
    r$SETBIT("EmailsMarch", df1$UserID[i] , "1")
  }
}

r$BITOP("AND","out3", c("EmailsJanuary","EmailsFebruary","EmailsMarch"))
result3 = r$BITCOUNT("out3")
result3
# -------1.4 # How many users received an e-mail on January and March but NOT on February?-------

r$BITOP("NOT", "NotEmailsFebruary", "EmailsFebruary")
r$BITCOUNT("NotEmailsFebruary") 
r$BITOP("AND", "out4" , c(c("EmailsJanuary","NotEmailsFebruary","EmailsMarch")))
out4 = r$BITCOUNT("out4")


# -------1.5 # How many users received an e-mail on January that they did not open but they updated their listing anyway?-------

data = merge(df1, df2, by = "UserID")

for(i in 1:nrow(data)) { 
  
  if((data$EmailOpened[i] == 0) & 
     (data$MonthID.x[i] == 1) &
     (data$ModifiedListing[i] == 1) &
     (data$MonthID.y[i] == 1 )) {
    
    r$SETBIT("EmailsNotOpenedUpdatedJanuary", data$UserID[i] , "1")
    
  } 
}

out5 = r$BITCOUNT("EmailsNotOpenedUpdatedJanuary")
out5

# -------1.6-------
# How many users received an e-mail on January that they did not open
# but they updated their listing anyway on January OR they received an
# e-mail on February that they did not open but they updated their listing
# anyway on February OR they received an e-mail on March that they did not 
# open but they updated their listing anyway on March?

for(i in 1:nrow(data)) { 
  
  if((data$EmailOpened[i] == 0) & 
     (data$MonthID.x[i] == 2) &
     (data$ModifiedListing[i] == 1) &
     (data$MonthID.y[i] == 2 )) {
    
    r$SETBIT("EmailsNotOpenedUpdatedFebruary", data$UserID[i] , "1")
    
  } else if ((data$EmailOpened[i] == 0) & 
             (data$MonthID.x[i] == 3) &
             (data$ModifiedListing[i] == 1) &
             (data$MonthID.y[i] == 3 )) {
    
    r$SETBIT("EmailsNotOpenedUpdatedMarch", data$UserID[i] , "1")
    
  }
}

r$BITOP("OR", "out6", c("EmailsNotOpenedUpdatedJanuary",
                           "EmailsNotOpenedUpdatedFebruary",
                           "EmailsNotOpenedUpdatedMarch")) 
out6 = r$BITCOUNT("out6")

# -------1.7 -------
# Does it make any sense to keep sending e-mails with
# recommendations to sellers? Does this strategy really work?
# How would you describe this in terms a business person would understand?


# People that read the email and updated their listing. 
for(i in 1:nrow(data)){
  if ((data$EmailOpened[i]==1)
      &(data$MonthID.x[i]==1)
      &(data$MonthID.y[i]==1)
      &(data$ModifiedListing[i]==1)){
    
    r$SETBIT("EmailsOpenedUpdatedJanuary", data$UserID[i],"1")
    
  }else if ((data$EmailOpened[i]==1)
            &(data$MonthID.x[i]==2)
            &(data$MonthID.y[i]==2)
            &(data$ModifiedListing[i]==1)){
    
    r$SETBIT("EmailsOpenedUpdatedFebruary", data$UserID[i],"1")
    
  }else if ((data$EmailOpened[i]==1)
            &(data$MonthID.x[i]==3)
            &(data$MonthID.y[i]==3)
            &(data$ModifiedListing[i]==1)){
    
    r$SETBIT("EmailsOpenedUpdatedMarch", data$UserID[i],"1")
  }
}

jan_OpenedUpdated = r$BITCOUNT("EmailsOpenedUpdatedJanuary")
feb_OpenedUpdated = r$BITCOUNT("EmailsOpenedUpdatedFebruary")
mar_OpenedUpdated = r$BITCOUNT("EmailsOpenedUpdatedMarch")

#People that did not read their email and did not update.

for(i in 1:nrow(data)){
  
  if ((data$EmailOpened[i]==0)
      &(data$MonthID.x[i]==1)
      &(data$MonthID.y[i]==1)
      &(data$ModifiedListing[i]==0)){
    
    r$SETBIT("EmailsNotOpenedNotUpdatedJanuary", data$UserID[i],"1")
    
  }else if ((data$EmailOpened[i]==0)
            &(data$MonthID.x[i]==2)
            &(data$MonthID.y[i]==2)
            &(data$ModifiedListing[i]==0)){
    
    r$SETBIT("EmailsNotOpenedNotUpdatedFebruary", data$UserID[i],"1")
    
  }else if ((data$EmailOpened[i]==0)
            &(data$MonthID.x[i]==3)
            &(data$MonthID.y[i]==3)
            &(data$ModifiedListing[i]==0)){
    
    r$SETBIT("EmailsNotOpenedNotUpdatedMarch", data$UserID[i],"1")
  }
}

jan_NotOpenedNotUpdated = r$BITCOUNT("EmailsNotOpenedNotUpdatedJanuary")
feb_NotOpenedNotUpdated = r$BITCOUNT("EmailsNotOpenedNotUpdatedFebruary")
mar_NotOpenedNotUpdated = r$BITCOUNT("EmailsNotOpenedNotUpdatedMarch")

# 3. People that read the email and did not update 

for(i in 1:nrow(data)){
  
  if ((data$EmailOpened[i]==1)
      &(data$MonthID.x[i]==1)
      &(data$MonthID.y[i]==1)
      &(data$ModifiedListing[i]==0)){
    
    r$SETBIT("EmailsOpenedNotUpdatedJanuary", data$UserID[i],"1")
    
  }else if ((data$EmailOpened[i]==1)
            &(data$MonthID.x[i]==2)
            &(data$MonthID.y[i]==2)
            &(data$ModifiedListing[i]==0)){
    
    r$SETBIT("EmailsOpenedNotUpdatedFebruary", data$UserID[i],"1")
    
  }else if ((data$EmailOpened[i]==1)
            &(data$MonthID.x[i]==3)
            &(data$MonthID.y[i]==3)
            &(data$ModifiedListing[i]==0)){
    
    r$SETBIT("EmailsOpenedNotUpdatedMarch", data$UserID[i],"1")
  }
}

jan_OpenedNotUpdated = r$BITCOUNT("EmailsOpenedNotUpdatedJanuary")
feb_OpenedNotUpdated = r$BITCOUNT("EmailsOpenedNotUpdatedFebruary")
mar_OpenedNotUpdated = r$BITCOUNT("EmailsOpenedNotUpdatedMarch")

jan_NotOpenedUpdated = r$BITCOUNT("EmailsNotOpenedUpdatedJanuary")
feb_NotOpenedUpdated = r$BITCOUNT("EmailsNotOpenedUpdatedFebruary")
mar_NotOpenedUpdated = r$BITCOUNT("EmailsNotOpenedUpdatedMarch")


actions = rep(c("EmailOpenedUpdated",
                   "EmailNotOpenedNotUpdated",
                   "EmailOpenedNotUpdated",
                   "EmailNotOpenedUpdated"),3)

dfplot1 <- data.frame(
  action= actions[1:4],
  value= c(sum(jan_OpenedUpdated,feb_OpenedUpdated,mar_OpenedUpdated),
           sum(jan_NotOpenedNotUpdated,feb_NotOpenedNotUpdated,mar_NotOpenedNotUpdated),
           sum(jan_OpenedNotUpdated,feb_OpenedNotUpdated,mar_OpenedNotUpdated),
           sum(jan_NotOpenedUpdated,feb_NotOpenedUpdated,mar_NotOpenedUpdated))
)

dfplot1 <- dfplot1 %>% 
  arrange(desc(action)) %>%
  mutate(prop = value / sum(dfplot1$value) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop )

fig1<-ggplot(dfplot1, aes(x="", y=prop, fill=action)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() +
  geom_text(aes(label = paste0("",
                               scales::percent(value / sum(value)),
                               "")),
            position = position_stack(vjust = 0.5))+
  ggtitle("Percentage of each user's behavior")+
  scale_fill_brewer(palette="Set1")


positive_pcnt_jan = (jan_OpenedUpdated / sum(jan_OpenedUpdated,jan_NotOpenedNotUpdated,jan_OpenedNotUpdated,jan_NotOpenedUpdated)) *100
positive_pcnt_feb = (feb_OpenedUpdated / sum(feb_OpenedUpdated,feb_NotOpenedNotUpdated,feb_OpenedNotUpdated,feb_NotOpenedUpdated)) *100
positive_pcnt_mar = (mar_OpenedUpdated / sum(mar_OpenedUpdated,mar_NotOpenedNotUpdated,mar_OpenedNotUpdated,mar_NotOpenedUpdated)) *100

months = rep(c(rep("January" , 2),rep("February" , 2),rep("March" , 2)))
condition = rep(c("Positive Response" , "Negative Response" ), 3)
values = c(jan_OpenedUpdated,sum(jan_NotOpenedNotUpdated,jan_OpenedNotUpdated,jan_NotOpenedUpdated),
           feb_OpenedUpdated,sum(feb_NotOpenedNotUpdated,feb_OpenedNotUpdated,feb_NotOpenedUpdated),
           mar_OpenedUpdated,sum(mar_NotOpenedNotUpdated,mar_OpenedNotUpdated,mar_NotOpenedUpdated))

dfplot2 = data.frame(months,condition,values)
dfplot2$months <- factor(dfplot2$months, levels=month.name)
dfplot2 = ddply(dfplot2, .(months), transform, percent = values/sum(values) * 100)


fig2<-ggplot(dfplot2, aes(fill=condition, y=values, x=months)) + 
  geom_bar(position="stack", stat="identity") +
  geom_text(aes(label = paste0(round(percent,2),"%")), 
            position = position_stack(vjust = 0.5), size = 4)+
  ggtitle("Number of positive and negative responses per month")+
  scale_fill_brewer(palette="Set1")+
  ylab("Number of emails")

fig1
fig2
