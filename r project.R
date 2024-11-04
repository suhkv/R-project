  #_________*********_____________**********__________*************
  # R-Language project:
  # 
  # The aim of this project is to analyse the dataset and apply various function's on the dataset.
  # With purpose to find some trend or pattern from the dataset which could be helpful for private organization,
  # artists, listeners.

  # data URL= https://www.kaggle.com/datasets/maharshipandya/-spotify-tracks-dataset

  # Why using R language?
  # Most commonly, the R language is used for data analysis and statistical computing. It's
  # also an effective tool for machine learning algorithms. R is especially relevant for data 
  # science professionals due to its data cleaning, importing, and visualization capabilities.
  # This project is about various ascpects analysis of the spotify songs. 
  # for example there are songs which are full of bass,loudness, speechness etc, and there are
  # some songs which are listened at soft volume.
  

  # By considering this various aspect in the data set we will analyse that what kind of songs is that. 
  # we will analyse 
  # 1.) trends
  # 2.) popularity
  # 3.) associative property
  # 4.) songs type by analysing it attributes
  # We will apply various function on dataset.
  # 
  # outcome = By analysing dataset we will try to predict trend, 
  # description of previous data,
  # we will do data mining on dataset.

  # Package Name	    Description
  # ggplot2	          tool for data visualisation
  # dplyr	            used for data manipulation
  # tidyr	            used to tidy data, meaning that every column is a variable, every row is an observation
  #                   and every cell is a single value
  # readr	            used to allow data import of rectangular data, e.g. .csv files

library(tidyr)
library(dplyr)
library(ggplot2)
library(readxl)
library(shiny)

#   dataset link :  https://www.kaggle.com/datasets/maharshipandya/-spotify-tracks-dataset

# 1. Loading the Data

# We first load the dataset using the read.csv function after unzipping the file.
# This step involves importing the data into R for analysis.
# 


a<-unzip("C:/Users/my/Downloads/dataset.csv.zip", exdir = "C:/Users/my/Downloads/dataset.csv")
a
data<- read.csv("C:/Users/my/Downloads/dataset.csv/dataset.csv")
data


#   2. Understanding the Data Structure
#    After loading the data, it is important to understand its structure.
#   We will view the first few rows, column names, and summary statistics.

# View basic structure and first few rows of the dataset
head(data, 4)
str(data)
summary(data)
mean(data$X)
median(data$X)
sd(data$X)
summary(data)
is.na(data)
dim(data)
View(data)
rownames(data)
colnames(data)
data['track_id']
data[,c('artists','album_name','energy')]

# This provides insight into the types of variables in the dataset (e.g., numerical, categorical),
# the presence of any missing values, and summary statistics like means and ranges.


#   3. Basic Data Cleaning
#   Data cleaning often involves handling missing values, renaming columns, or changing data types.

data_clean <- na.omit(data)
data$column_name[is.na(data$column_name)] <- mean(data$column_name, na.rm = TRUE)
data



#   4. Filtering and Selecting Data
#   You can filter rows or select specific columns for analysis.


# Filtering specific columns of interest
filtered <- data[, c("track_id", "album_name", "energy", "popularity")]
filterd_data <- data[,c("track_id","album_name")]
filterd_data
filtered
for(i in data){
  print(i)
}


#   5. Data Visualization
#   Visualize data using the ggplot2 package, which allows you to create various plots.
      

# A histogram is a chart that plots the distribution of a numeric 
# variable's values as a series of bars. Each bar typically covers a range 
# of numeric values called a bin or class; a bar's height indicates the frequency
# of data points with a value within the corresponding bin.

# 1. Histogram for Popularity
ggplot(data, aes(x = popularity)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
  labs(title = "Distribution of Popularity", x = "Popularity", y = "Count") +
  theme_minimal()

# 2. Histogram for Duration (in milliseconds)
ggplot(data, aes(x = duration_ms)) +
  geom_histogram(binwidth = 50000, fill = "lightgreen", color = "black") +
  labs(title = "Distribution of Duration of Songs", x = "Duration (ms)", y = "Count") +
  theme_minimal()

# 3. Histogram for Energy
ggplot(data, aes(x = energy)) +
  geom_histogram(binwidth = 0.05, fill = "orange", color = "black") +
  labs(title = "Distribution of Energy", x = "Energy", y = "Count") +
  theme_minimal()

# 4. Histogram for Loudness
ggplot(data, aes(x = loudness)) +
  geom_histogram(binwidth = 1, fill = "purple", color = "black") +
  labs(title = "Distribution of Loudness", x = "Loudness (dB)", y = "Count") +
  theme_minimal()

# 5. Histogram for Tempo
ggplot(data, aes(x = tempo)) +
  geom_histogram(binwidth = 5, fill = "red", color = "black") +
  labs(title = "Distribution of Tempo", x = "Tempo (BPM)", y = "Count") +
  theme_minimal()




# Create an overlapping histogram for Energy based on explicit content
ggplot(data, aes(x = energy, fill = as.factor(explicit))) +
  geom_histogram(position = "identity", alpha = 0.5, binwidth = 0.05) +
  labs(title = "Overlapping Histogram of Energy by Explicit Content",
       x = "Energy",
       y = "Count",
       fill = "Explicit Content") +
  theme_minimal() +
  scale_fill_manual(values = c("blue", "red"), labels = c("No", "Yes"))

# Overlapping histogram for Loudness
ggplot(data, aes(x = loudness, fill = as.factor(explicit))) +
  geom_histogram(position = "identity", alpha = 0.5, binwidth = 1) +
  labs(title = "Overlapping Histogram of Loudness by Explicit Content",
       x = "Loudness (dB)",
       y = "Count",
       fill = "Explicit Content") +
  theme_minimal() +
  scale_fill_manual(values = c("lightgreen", "orange"), labels = c("No", "Yes"))


# Boxplot for popularity
ggplot(data, aes(y = popularity)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 16, outlier.size = 2) +
  labs(title = "Boxplot of Popularity", y = "Popularity") +
  theme_minimal()



# Scatter plot for energy vs. popularity
energy <- ggplot(data, aes(x = energy, y = popularity)) +
  geom_point(alpha = 0.6, color = "blue") + # Scatter points
  labs(title = "Scatter Plot of Energy vs. Popularity",
       x = "Energy",
       y = "Popularity") +
  theme_minimal()
# Scatter plot for danceability vs. energy
dancei <- ggplot(data, aes(x = danceability, y = energy)) +
  geom_point(alpha = 0.6, color = "green") +
  labs(title = "Scatter Plot of Danceability vs. Energy",
       x = "Danceability",
       y = "Energy") +
  theme_minimal()


# A list in R programming is a generic object consisting of an ordered collection of objects. Lists are one-dimensional
# , heterogeneous data structures.
# The list can be a list of vectors, a list of matrices, a list of characters, a list of functions, and so on.

lst <- list(data$speechiness,c(data$artists,data$album_name,data$explicit))
lst
data$gym_col <- data$energy + data$danceability
data


#  The aggregate() function in R can be used to calculate summary statistics for a dataset.
# 
#  This function uses the following basic syntax:
#   
#  aggregate(x, by, FUN)
# 
#  where:
#   
#  x: A variable to aggregate
#  by: A list of variables to group by
#  FUN: The summary statistic to compute
#  Input is dataframe



count_acoustic <- 0
for (i in 1:nrow(df)){
  
  if (df$time_signature[i] == 4 & df$track_genre[i] == 'acoustic')
    {count_acoustic <- count_acoustic + 1}
}
print(count_acoustic)

# Aggregate to find mean popularity by energy levels
aggregate_popularity_by_energy <- aggregate(popularity ~ energy, data = data, FUN = mean)
print(aggregate_popularity_by_energy)








