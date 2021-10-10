#This year, I built a wine lounge in a spare room in my house. Although I do love a good red blend and the aesthetic of a wine lounge, I wouldn't consider myself a wine connesuir. I want to learn more about good wine leveraging ~130K reviews published to WineEnthusiast.

# Load the necessary packages for data analysis
library(tidyverse)

library(tidyr)

library(readr)

library(ggplot2)

library(janitor)

library(dplyr)

# Read the data
wine <- read.csv("winemag-data-130k-v2.csv")

# Remove rows with NA values for prices
wine2 <- na.omit(wine)

# Remove rows with null values for province
wine3 <- wine2[!(is.na(wine2$province) | wine2$province==""), ]

# Examine the data frame column names
colnames(wine3)

#Preview the data
glimpse(wine3)

# View the entire dataset
View(wine3)

# Compute average price per province
aveprice <-
  wine3 %>% 
  group_by(province) %>% 
  drop_na() %>% 
  summarize(mean_price = mean(price))

# Determine the provinces with the top highest prices in descending order
arrange(aveprice, -mean_price)

# An issue we find here is that the province with the highest average price (Colares) only has two reviews. 
# Let's find out about highest average prices for those provinces with at least 10 reviews.
province10ormore <-
  wine3 %>% 
  group_by(province) %>% 
  filter(n() >= 10)

avepriceatleastten <-
  province10ormore %>% 
  group_by(province) %>% 
  drop_na() %>% 
  summarize(mean_price = mean(price))

# Determine the provinces with the top highest prices in descending order (provinces with at least 10 reviews)
arrange(avepriceatleastten, -mean_price)


# Does the number of points predict the price of wine? If so, how strong is the correlation?

ggplot(data=wine3) + 
  geom_point(mapping=aes(x=points,y=price),color="orange") +
  geom_smooth(mapping=aes(x=points,y=price),color="purple") +
  labs(title="Wine Ratings: Price vs. Points", subtitle="Wine ratings and their relationship with the price of wine", caption="Data published by WineEnthusiast")


# Which provinces have the highest average points? My wine lounge deserves nothing but the best because I worked hard on it!

pointsbyprovince <-
  wine3 %>% 
  group_by(province) %>% 
  drop_na() %>% 
  summarize(average_points = mean(points))

arrange(pointsbyprovince, -average_points)

topprovinces <- c("Südburgenland", "Madeira", "Mittelrhein", "Puente Alto", "Wachau", "England", "Santa Cruz", "Leithaberg", "Kamptal", "Traisental")


# Let's take a closer look at the varieties within the top points by province.

wine3 %>%
  filter(province %in% topprovinces) %>%
  ggplot(aes(x = province, y = points, fill=variety)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45)) +
  labs(title="Total Variety Composition of Top Provinces by Total Points",caption="Data published by WineEnthusiast")


# I love a good red blend. Where are the best red blends from according to WineEnthusiast reviews?
redblendprovinces <-
  wine3 %>% 
  group_by(province) %>% 
  drop_na() %>% 
  filter(variety=="Red Blend") %>% 
  summarize(average_points = mean(points))

arrange(redblendprovinces, -average_points)
