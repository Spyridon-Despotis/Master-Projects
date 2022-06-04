library(robotstxt)
library(rvest)

# The code starts with data gathering from 28 pages from the website metacritic.com
# After collecting the data, its proceeds with data cleansing, data type convertions and creation of dataframes
# Once the data are prepared, it transformes data to usuful graphs to provide deeper insights



# Check whether scraping is allowed from this webpage (returns TRUE)
# ATTENTION: PUT THE WHOLE URL IN ONE LINE WHEN RUNNING THE CODE


paths_allowed("https://www.metacritic.com/publication/washington-post?filter=movies&num_items=100&sort_options=date&page=0")

# Define character element "main.page", to be used recursively for defining
# multiple pages from metacritic.com
# ATTENTION: PUT THE WHOLE URL IN ONE LINE WHEN RUNNING THE CODE


main.page <- "https://www.metacritic.com/publication/washington-post?filter=movies&num_items=100&sort_options=date&page="
main.page

for (i in 0:27){ 
  
  # This is a "for" loop.
  # This means that all the lines until the closure of }
  # will be repeated for different values of object i
  # thus, on the first run i=0, second run i=1,... last run i=27
  # for each step, define...
  
  
  # Element "step.page", is created which includes  
  # the function "paste", which concatenates the element "main.page" that contains the main URL, 
  # with the different values from i object and gives us results from the different sub-pages (0-27)
  
  step.page <- paste(main.page,i,sep="")
  
  
  webdata <-read_html(step.page)# OK
  
  
  # Vector title is created which includes all movie titles from step.page i (first i=0, second i=1,... last i=27)
  # html_nodes function gathered all data from HTML attribute review_product
  # function html_text extracted only the text from HTML data
  
  title <-c(webdata %>% html_nodes("div.review_product") %>% html_nodes("a") %>%
              html_text())
  
  
  # Vector metascore is created which includes all metascores from step.page i movies,
  # html_nodes function gathered all data from HTML attributes
  # html_text function  extracted only the text from HTML data
  
  metascore <- c(webdata %>% html_nodes("li.review_product_score.brief_metascore") %>%
                   html_nodes("span.metascore_w") %>% html_text())
  class(metascore)
  
  
  # Character element critic is created which includes all critic scores from step.page i movies,
  # html_nodes function gathered all data from HTML attributes
  # html_text function extracted only the text from HTML data
  
  critic <- c(webdata %>% html_nodes("li.review_product_score.brief_critscore") %>%
                html_nodes("span.metascore_w") %>% html_text())
  
  
  
  # Character element date is created which includes all the movies reviews post dates, 
  # in step.page i 
  # html_nodes function gathered all data from HTML attributes
  # html_text function extracted only the text from HTML data
  
  date <- c(webdata %>% html_nodes("li.review_action.post_date") %>% html_text())
  
  
  if (length(date)<100 ){for (j in length(date):100){ date[j] <- date[length(date)]}} #OK
  
  
  # Character element "a" is created 
  # which contains from the string date the characters in positions 12 and 13
  
  a <- substr(date,12,13)
  
  # Character element "b" is created 
  # which contains from the string date the characters from positions 8 up to 10
  
  b <- substr(date,8,10)
  
  
  # Character element "d" is created which contains from the string date
  # the characters from positions 16 up to 19
  
  d <- substr(date,16,19)
  
  
  lct <- Sys.getlocale("LC_TIME"); Sys.setlocale("LC_TIME", "C") #OK
  
  
  # Define character element "date2", which contains
  # the pasted characters of a,b,d separated with "/"
  # Setting 1 as parameter of the MARGIN argument in apply function
  # applies cbind function to every row
  
  date2 <- apply(cbind(a,b,d),1,paste,collapse="/")
  
  # Vector "date2" with class: Date, 
  # Contains the character "date2" converted to numeric with format yyyy-mm-dd
  

  date3 <- as.Date(date2,"%d/%b/%Y")
  
  Sys.setlocale("LC_TIME", lct) #OK
  
  
  
  # The data frame "df" is created and contains characters as
  # columes: title,metascore,critic and date
  
  df = data.frame(title,metascore,critic,date3)
  
  
  # The function colnames, adds new names to the dataset columns
  # and renames date3 to date
  
  colnames(df) <- c("title", "metascore", "critic","date")
  mode(colnames)
  
  # The column metascore is converted to numeric from character
  
  df$metascore <- as.numeric(as.character(df$metascore))
  class(df$metascore)
  
  
  # The column critic is converted to numeric from character
  
  df$critic <- as.numeric(as.character(df$critic))
  
  # The vector df  removes from dataframe df, all rows containing any NA's
  
  df <- df[complete.cases(df), ] 
  
  
  
  if (i==0){ #OK
    df.tot <- df} #OK
  
  
  # This is an "if" Statement
  # This means that all the lines until the closure of }
  # will be executed if the i>0 is TRUE 
  # But if it's FALSE, nothing happens.
  
  
  if (i>0){
    
    
    # Dataframe df.tot is updated, by binding the rows of the df (corresponding to i)
   
    
    df.tot <- rbind(df.tot,df) }
  
}

# column title of the df.tot data frame, is converted to character

df.tot$title <- as.character(df.tot$title)


################----------------------------------

###Exercise 2

str(df.tot)


# The dataframe contains 2776 rows and 4 columns.
# Column title is of class character
# Comumns metascore and critic are of class numeric
# Column data is of class Date

# 'data.frame':	2776 obs. of  4 variables:
#   $ title    : chr  "The Last Duel" "Mass" "Halloween Kills" "Bergman Island" ...
# $ metascore: num  68 80 42 81 86 69 68 47 60 73 ...
# $ critic   : num  75 100 37 75 100 75 75 75 37 37 ...
# $ date     : Date, format: "2021-10-14" "2021-10-13" .


###Exercise 3

df.tot$perc.meta<-rank(df.tot$metascore)/length(df.tot$metascore)
df.tot$perc.critic<-rank(df.tot$critic)/length(df.tot$critic)

df.tot$year <- substr(df.tot$date,1,4)
df.tot$ratio = df.tot$metascore  / df.tot$critic 

View(df.tot)

###Exercise 4

# The film with the highest metascore is the "Boyhood"
df.tot$title[which.max(df.tot$metascore)]


###Exercise 5

df.tot$year <- as.numeric(as.character(df.tot$year))
boxplot(df.tot$perc.meta ~ df.tot$year)
abline(h = 0.5, col = 'red')

# The majority of the movie metascores from all years, were above the mean = 0.5
# The worst year was in 2013, in which the movies had the lowest metascores from all years,this year has the highest negative difference from the mean
# The best year was in 2020, in which the movies had the highest metascores from all years,this year we have the highest positive difference from the mean


###Exercise 6

# Infinity ratio values (equal to score / critics score) were produced 
# because in some cases the critic value was 0. 

df.tot2 <- subset(df.tot, !is.infinite(ratio))


###Exercise 7

mtrx=as.matrix(subset(df.tot2,  select = c(metascore, critic)))

# average of metascore and critic by movie
a1=apply(mtrx,1, mean)


# overall average of metascore and critic by columns
a2=apply(mtrx,2, mean)

###Exercise 8

# plot for metasxore>50 (red)
plot(
  x = df.tot2$date[df.tot2$metascore > 50],
  y = df.tot2$perc.meta[df.tot2$metascore > 50] ,
  main = "Metascores percentiles",
  xlab = "Year",
  ylab = "metascore percentiles",
  col = "red",
   ylim = c(0,1),
   las = 1 ##labels to be perpendicular to the axis
)

# add points for metascore<50 (blue)

points(
  x = df.tot2$date[df.tot2$metascore <= 50],
  y = df.tot2$perc.meta[df.tot2$metascore <= 50] ,
  col = "blue",
  type = "p"
)

# legend

legend(
  x = "topright",
  legend = c("metascore>50", "metascore<50"),
  col = c("red", "blue"),
  pch = 1,
  cex = 0.6
)

# perc.meta at metascore=50
hh=unique(df.tot2$perc.meta[df.tot2$metascore==50])
abline(h=hh, lty="dashed",lwd=3)


###Exercise 9

# 78% of the movies have metascore bigger than 50 
# 22% of the movies have metascore less than 50


