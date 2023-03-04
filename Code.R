# The goal of the project is to find out the traits of a community who are in poverty or at the the risk of poverty. For this it is important to determine the influence of certain factors, such as age, sex, education level etc on the salary of an individual in the USA.
#The inferences from the project can be used by government agencies to formulate policies to improve lives of persons experiencing poverty or who are at the risk of poverty.
library(ggplot2)
library(tidyverse)
library(dplyr)
library(RColorBrewer)
library(maps)
library(mapproj)
sal <- read.csv("/Users/home/Desktop/R-project/adult.csv")#reading the census data 

#Assigning meaningful column names 
colnames(sal) <- c('age', 'workclass','fnlwgt','education','education in years','marital status', 'occupation', 'relationship','race','sex','capital gain','capital loss','hours per week','country','salary')
head(sal)
str(sal)
#removing all rows which contain '?' for any column 
sal1<-sal[!(sal$workclass==" ?" | sal$education==" ?"),]
sal2 <- sal1[!(sal1$`marital status`==" ?" | sal1$occupation==" ?"),]
sal3 <- sal2[!(sal2$race==" ?" | sal2$sex==" ?"),]
sal4 <- sal3[!(sal3$country==" ?" | sal3$salary==" ?"),]
sal5 <- sal4[!(sal4$age==" ?" | sal4$`hours per week`==" ?"),]
str(sal5)
#dropping unnecessary columns which do not contribute to the project
drop12 <- c("fnlwgt", "relationship","capital gain","capital loss")
sal5 = sal5[,!(names(sal5) %in% drop12)]
str(sal5)
head(sal5)
occurences1<-table(unlist(sal5))
occurences1
unique(sal5$country)#checking id '?' values are still there
unique(sal5$salary)
#reading file which contains latitude and longitude information of each country
lat <- read.csv("/Users/home/Desktop/R-project/A1.csv")
str(lat)

#merging dataframes states and idf based on common column name 'abb'
#removing trailing and leading spaces to merge dataframes correctly 
sal5$country1<- trimws(sal5$country, which = c("both"))
lat$country1 <- trimws(lat$country, which = c("both"))
salnew <- merge(sal5, lat, by="country1")
#choosing only those rows where salary <=50K
salnew1 <-  filter(salnew, salary == " <=50K")
head(salnew1)
#Checking for missing values

#Question 1: What is the country of origin of individuals earning less than $50,000 per annum?
library(leaflet)
library(rgdal)
salnew1 %>% leaflet() %>% addProviderTiles(providers$Esri.WorldImagery, group = "World Imagery") %>% addProviderTiles(providers$Stamen.TonerLite) %>% addLayersControl(baseGroups = c("Toner Lite", "World Imagery")) %>% addMarkers(label = salnew1$country1,clusterOptions = markerClusterOptions(),popup = ifelse(salnew1$salary==" <=50K","At risk of poverty","Not at poverty risk"))
#It is observed from the map that most persons earning <=50K, who are in or at the risk of poverty in the United states are originally from the US i.e they were born in the US. Therefore it is unlikely that a person can fall into poverty because an immigrant status. It is to be noted that a person can have an ethnicity which is unrelated to his country of of birth.


#Question 2: What is the ethnicity of the majority of persons born in the US and earning <=50K per annum
#choosing only those rows where native country is United States 
library(googleVis)
salnew2 <-  filter(salnew1, country1 == "United-States")
head(salnew2)
str(salnew2)
agg_tbl <- salnew2 %>% group_by(race) %>% summarise(total_count=n(), .groups = 'drop')#finding count of persons belonging to each race 
agg_tbl
race <- agg_tbl %>% as.data.frame()
race
Pie2 <- gvisPieChart(race, options=list(title='Ethnic distribution within U.S born population earning <=$50k per annum'))
plot(Pie2)
#it is observed that most of the population is White, followed by Black and Asian Pacific Islander ethnicity.
#I drill down further to find the majority in the population interms of gender 
#choosing columns where race is 'White'

unique(salnew3$sex)#only two distinct genders, Male and Female we found in dataframe
salnew3 <-  filter(salnew2, race == " White")
head(salnew3)
agg_tbl1 <- salnew3 %>% group_by(sex,age) %>% summarise(total_count=n(), .groups = 'drop')
agg_tbl1
sex <- agg_tbl1 %>% as.data.frame()
#plotting a scatterplot to understand the distribution of persons earning less than $50k/year, U.S born  and of White ethnicity 
gender <- ggplot(data = sex) + geom_jitter(aes(x= age , y= total_count, colour = sex))+ xlab("Age in Years")+ ylab("Count of persons")+ labs( title ="Poverty outcome of population: U.S born, White ethnic group based on age and gender", caption="Created by Sushmitha Mohana")
gender
#We observe from the above graph that, count of persons in poverty or at the risk of poverty decreases with age, irrespective of gender.
#More White U.S born Men between ages ~24 to 38 are prone to poverty than Women of the same age

#Question3 : What are the top 3 professions of this population(U.S born -White- Male- earning less than $50k/year)?
# I have answered this question with a tree map made using gvis package
# Before that we filter out all sex =female, rows 
profess <- filter(salnew3, sex == " Male")
head(profess)
profess1 <- profess %>% group_by(occupation) %>% summarise(n()) %>% top_n(3)#creating Tibble
profess1 <- profess1 %>% as.data.frame()#converting tibble into dataframe 
colnames(profess1) <- c('ocupation','number of persons in the occupation')
profess1#examining the number of persons in the top three occupations 
profess2 =data.frame(number= c("Craft Repair", "Sales", "Executive Manager"), `number of persons`=c(2451,1252,1030))
profess2
Bar1 <- gvisBarChart(profess2, xvar="number", yvar=c("number.of.persons"),  options=list(title="Top 3 professions of Men: Born in the U.S,from a White ethnic background and earning <=$50K/year",titleTextStyle="{color:'red',fontName:'Courier',fontSize:16}", curveType='function'))
plot(Bar1)#created a simple bar chart representation for the same using gvis package
#It is found that the count of Men in Craft Repair, Sales and Managerial executive occupations were the highest in this population.
#It is uncanny that Executive managers are in the top 3 of this list because the salary range for Higher Executive Manager positions  is ~77K/year and upwards. Lower lever Executive mangers make <50K/year. We do not have enough information from the data to decide in which Level these Excecutive Managers belong. The same applies to Sales professionals as well.
#Therefore we can exclude occupation while understanding the features of the population segment 

#Question4: what is the marital status and number of years of education
library(plotly)
library(gcookbook)
test <- salnew3 %>%  filter(sex==" Male") 
head(test)
edu <- test %>% group_by(`marital status`, `education in years`) %>% summarise(n())# grouping based on marital status and education in years 
edu1 <- edu %>% as.data.frame()
head(edu1)
colnames(edu1) <- c('marital status','education (in years)','count of persons')
p <-ggplot(edu1,aes(`education (in years)`,fill=`marital status`))+geom_bar(aes(weight=`count of persons`),position="stack")+ggtitle("Number of White Men with earning <=$50K/year by years of education received and Marital status")+ scale_fill_discrete(labels = c("Divorced", "Married with Armed Forces spouse", "Married with Civilian Spouse","Married but spouse absent", "NeverMarried","Separated","Widowed" ))  #+geom_text(position="stack",aes(`education (in years)`,`count of persons`,label=`marital status`),size=5)
ggplotly(p)
#through the interactive plot I observed that U.S born white men possessing between 5-10 years of formal education excluding preschool-grade 3and repititon of a grade, and who are married to a Civilian spouse are at the highest risk of being in / are already in poverty. This concludes that U.S born White men, whose maximum education level is a High School diploma and who are married to a Civilian Spouse are at the highest risk of being in /already in poverty 
GTM <- gvisMerge(Bar1, Pie2, horizontal=TRUE,
                 tableOptions="cellspacing=200")
plot(GTM) #merging gvis objects 













