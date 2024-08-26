#R workshop 2024
#Exploratory data analysis
#Author: Joshua Ajowele
#Date: 25 August 2024

#install and load required package
install.packages("tidyverse")

#load package
library(tidyverse)


#load iris data
data("iris")

#check variables in iris data
head(iris)

#How many individuals of each species were measured?
#How many species were considered?
ggplot(data=iris, mapping=aes(x=Species, fill=Species))+
  geom_bar()
#so, how many individuals and how many species?

#What is the distribution of Sepal_Length?
ggplot(data=iris, mapping=aes(x=Sepal.Length))+
  geom_histogram()

#What is the distribution of Sepal_Length by species?
ggplot(data=iris, mapping=aes(x=Sepal.Length, fill=Species))+
  geom_histogram()
#specify a binwidth
ggplot(data=iris, mapping=aes(x=Sepal.Length, fill=Species))+
  geom_histogram(binwidth = 0.2)
#What species do you think has the shortest sepal length?

#Exercise 1: Show the distribution of Sepal Width for each species
ggplot(data=iris, mapping=aes(x=Sepal.Width, fill=Species))+
  geom_histogram(binwidth = 0.2)
#what species do you think has the highest width?

#much easier to view multiple category 
#with frequency polygon than histogram
#use petal length
ggplot(data=iris, mapping=aes(x=Petal.Length, col=Species))+
  geom_freqpoly(binwidth = 0.2)
#what species do you think has the longest petal?

#Is there a relationship between sepal length and sepal width?
ggplot(data=iris, mapping=aes(x=Sepal.Width,y=Sepal.Length))+
  geom_point()+
  geom_smooth(method = lm)
#Is there a relationship? Describe the relationship.

#Exercise 2:
#Is there a relationship between sepal length and sepal width within each species?
ggplot(data=iris, mapping=aes(x=Sepal.Width,y=Sepal.Length, col=Species))+
  geom_point()+
  geom_smooth(method = lm)

#Is there a petal length by width relationship?
ggplot(data=iris, mapping=aes(x=Petal.Width,y=Petal.Length))+
  geom_point()+
  geom_smooth(method = lm)

#Do we have the same pattern among species?
ggplot(data=iris, mapping=aes(x=Petal.Width,y=Petal.Length, col=Species))+
  geom_point()+
  geom_smooth(method = lm)

#any relationship between sepal and petal length?
ggplot(data=iris, mapping=aes(x=Sepal.Length,y=Petal.Length, col=Species))+
  geom_point()+
  geom_smooth(method = lm)

#any relationship between sepal and petal width?
ggplot(data=iris, mapping=aes(x=Sepal.Width,y=Petal.Width, col=Species))+
  geom_point(size=3)+
  geom_smooth(method = lm, linewidth=2)+
  #selecting colours manually
  scale_color_manual(values=c("Purple","Pink","Green"))+
  #specifying x and y axis
  xlab("Sepal Width")+
  ylab("Petal Width")+
  #removing grid lines from the background
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank())

#making a multipanel figure
ggplot(data=iris, mapping=aes(x=Sepal.Width,y=Petal.Width))+
  geom_point(size=3, col="Purple")+
  geom_smooth(method = lm, linewidth=2, col="Pink")+
  facet_wrap(~Species)

#make axes different for each species
ggplot(data=iris, mapping=aes(x=Sepal.Width,y=Petal.Width))+
  geom_point(size=3, col="Purple")+
  geom_smooth(method = lm, linewidth=2, col="Pink")+
  facet_wrap(~Species,scales = "free")

#assigning figures to object
sepal_petal_fig<-ggplot(data=iris, mapping=aes(x=Sepal.Width,y=Petal.Width))+
  geom_point(size=3, col="Purple")+
  geom_smooth(method = lm, linewidth=2, col="Pink")+
  facet_wrap(~Species,scales = "free")
print(sepal_petal_fig)


#Further studies
#R for Data Science
#https://r4ds.hadley.nz/eda



#rename Iris data

iris_data<-iris%>%
  #select specific columns
  select(Petal.Width, Species)
head(iris_data)

#Exercise: show only the sepal width and petal length
iris_length<-iris%>%
  #select sepal width and petal length
  select(Sepal.Width, Petal.Length)
head(iris_length)
#another way to select
iris_length<-iris%>%
  #select sepal width and petal length
  select(2,3)
head(iris_length)

#pick setosa from Iris 
iris_setosa<-iris%>%
  filter(Species=="setosa")
head(iris_setosa)

#Exercise 2: Pick virginica from iris using filter

#find the mean of the sepal length and width for each species
#without losing any datapoint

iris_mean<-iris%>%
  #select columns needed
  select(1,2,5)%>%
  #group to calculate mean
  group_by(Species)%>%
  mutate(sepal_length_mean=mean(Sepal.Length, na.rm=T),
         Sepal_Width_mean=mean(Sepal.Width, na.rm=T))
head(iris_mean)  
 
#calculate mean and remove the initial data 
iris_mean2<-iris%>%
  #select columns needed
  select(1,2,5)%>%
  #group to calculate mean
  group_by(Species)%>%
  summarise(sepal_length_mean=mean(Sepal.Length, na.rm=T),
         Sepal_Width_mean=mean(Sepal.Width, na.rm=T))
head(iris_mean2)
  
  
  


