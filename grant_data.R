
#Grant Nav database years 2017-2018

rm(list = ls())

library(dplyr)
library(ggplot2)
library(tidyr)
library(data.table)
library(readr)
library(stringr)
library(tm)

setwd("/Users/ryannazareth/Documents/360_Giving_Viz_challenge/Data")

data <- read.csv("Question_1/grantnav-20180723151603.csv")

# the dataset below is for only user-led organisations 
#data <- read.csv("Question_2/user-led_2007-2017.csv")
head(data, 5)
names(data)  # list of column names
filtered_data <- data[c(2:3, 6, 8, 17,31,34, 72:73)] # get rid of unnecessary columns 
head(filtered_data, 10)
names(filtered_data) <- c("Title","Description","Amount_Awarded", "Award_Date", "Recipient_Org", "Funding_Org", "Grant_Programme_Title", "Recipient_Region" ,    
                          "Recipient_District"  )

filtered_data$Award_Date <- format(as.Date(str_sub(filtered_data$Award_Date,1,10)), "%d-%m-%Y") # reformat the dates
lapply(filtered_data, class) #check the class
filtered_data$Amount_Awarded <- as.integer(filtered_data$Amount_Awarded) # Reformat to integer

# Add another column to identify which regions are London
for(i in 1:dim(filtered_data)[1]){
  if (filtered_data$Recipient_Region[i]=="London"){
    filtered_data$is_London[i] <- "Y"
  }
  else{
    filtered_data$is_London[i] <- "N"
  }
}

# Cleaning the district names to remove the stopwords below - makes setting geo locations easier on map

remove <- c("London Boro", "District", "City", "City of ", "County of ", "County ", "North East ", 
            "North ", "West ", "South ", "East ", "South West ", "South East ", "North West ")

filtered_data$Recipient_District <- gsub(paste(remove, collapse ="|"), "", filtered_data$Recipient_District)
filtered_data <- subset(filtered_data, select = -c(Recipient_Region, Grant_Programme_Title)) #Remove the Recipient Region and Grant Programme Title columns as we do not need it anymore

save(filtered_data, file = "data.Rda")
# Themes - we will look at 4 themes and filter the data accordingly

#Lets filter each theme separately to see the split between number of grants for each

elderly <- dplyr::filter(filtered_data, grepl('elderly|older people|dementia', Description)) 
elderly$Theme <- "elderly"
sports <- dplyr::filter(filtered_data, grepl('sports|fitness', Description))
sports$Theme <- "sports"
environment <- dplyr::filter(filtered_data, grepl('environment', Description))
environment$Theme <- "environment"
science<- dplyr::filter(filtered_data, grepl('science', Description))
science$Theme <- "science"
transport<- dplyr::filter(filtered_data, grepl('transport', Description))
transport$Theme <- "transport"
loneliness <- dplyr::filter(filtered_data, grepl('loneliness|isolation', Description))
loneliness$Theme <- "loneliness"
jobs <- dplyr::filter(filtered_data, grepl('jobs', Description))
jobs$Theme <- "jobs"
education <- dplyr::filter(filtered_data, grepl('education', Description))
education$Theme <- "education"

number_grants <- c(dim(elderly)[1], dim(science)[1],dim(environment)[1], dim(transport)[1], 
             dim(loneliness)[1], dim(jobs)[1], dim(education)[1])
theme <- c("elderly", "science", "environment", "transport", "loneliness", "jobs", "education")
themes <- data.frame(theme, number_grants)
g <- ggplot(data= themes , aes(theme) )                                                                              
g + geom_bar(aes(weight = number_grants, fill = theme)) + labs(y = "Number of grants", Title = "Number of grants allocated to different themes") + coord_flip()

#Now lets decide which themes we want to filter the original dataset with:
#Ive gone for 3 to give me 14K grants in total.

#We can easily bind the dataframes together, depending on which themes we want
# and then filter out duplicated rows .e.g. if one grant crosses over between categories

filter_themes <-bind_rows(elderly, science, sports) 
filter_themes <- distinct(filter_themes, Description, .keep_all = TRUE)

#filter_themes <- dplyr::filter(filtered_data, grepl('elderly|older people|dementia|sports|fitness|science', Description))

write.csv(filter_themes, 'cleaned.csv', row.names=FALSE)
