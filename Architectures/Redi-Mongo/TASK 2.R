library(jsonlite)
library(mongolite)
library(dplyr)

# Create a dataframe containing all the paths
file = "E:/Thanasis/Documents/Msc/Big Data Systems and Architectures/Redis-Mongo Assignment-v1.3/BIKES/files_list.txt"
my_data <- read.table(file,header = TRUE, sep = "\n", fileEncoding="UTF-16LE")
my_data$files_list.txt = gsub(pattern = "\\\\", replacement = "/", x = my_data$files_list.txt)
my_data['files_list.txt'] <- lapply(my_data, function(x) paste('E:/Thanasis/Documents/Msc/Big Data Systems and Architectures/Redis-Mongo Assignment-v1.3/BIKES/', sep = '', x))

# Create a connection to mongodb
m <- mongo(collection = "Full_Dataset", db = "Bikes", url = "mongodb://localhost")
# Read the files from the dataframe and import them in the database
for(i in 1:length(my_data$files_list.txt)){
  m$insert(fromJSON(readLines(my_data$files_list.txt[i], warn = F, encoding = "UTF-8")))
}

full_df = m$find("{}")

#Take the dataframes
query_df = full_df[,"query"]
ad_data_df = full_df[,"ad_data"]
ad_seller_df = full_df[,"ad_seller"]
meta_data_df = full_df[,"metadata"]
# Take Lists
list_df = full_df[,c("title","ad_id","extras","description")]

#The query_df and meta_data_df have 2 columns with the same name (type)
colnames(meta_data_df) = c("type2","brand","model")
#Combine all the dataframes in one
df = cbind(query_df, ad_data_df,ad_seller_df,meta_data_df,list_df)

#Cleaning

df$Price <- as.numeric(gsub("\\D+","",df$Price))

df$Mileage <- gsub(" km", "",df$Mileage)
df$Mileage <- gsub("," , "",df$Mileage)
df$Mileage = as.numeric(df$Mileage)

df$Registration = gsub(".*/","",df$Registration)
df$Registration = as.numeric(df$Registration)

# Drop the previous collection
m$drop()
# Insert a new collection
t <- mongo(collection = "Cleaned_Dataset", db = "Bikes", url = "mongodb://localhost")
t$insert(df)

# 2.2 -- How many bikes are there for sale?
total_bikes = t$aggregate('[{"$group" : {"_id" : "$type2", "count": {"$sum":1} }}]')
total_bikes

#-----# - Average Price of a motorcycle?
# 2.3 # - Number of listing used to calculate this avg?
#-----# - Is this number the same as 2.2 ?
t$aggregate(
  '[{
	"$match": {
		"Price": {
			"$gt":100
		}}},
  {"$group" : {"_id" : "AveragePrice", "avg_price":{"$avg": "$Price"}}}]'
) 

total_listings = t$find(
  query  = '{ "Price" : { "$gt" : 100 } }',
  fields = '{ "Price" : true }'
)
nrow(total_listings)

nrow(df)

#-----#
# 2.4 # - What is the maximum and minimum price of a motorcycle currently available in the market
#-----#

t$aggregate('[
        {"$match" : {"Price" : {"$gt" : 100}, "type2" : "Bikes"}},
        {"$group": { "_id": null,"max": { "$max": "$Price" }, "min": { "$min": "$Price" } }}
        ]')


#-----#
# 2.5 # - # How many listings have a price that is identified as "Negotiable"?
#-----#

t$aggregate('[
  {"$match": {"model" : { "$regex" : "Negotiable", "$options" : "i"}}},
  {"$group": {"_id": "null","negotiable_ads": {"$sum": 1}}}
  ]')





