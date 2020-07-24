library(tidyverse)
library(lubridate)
data1 <- read.csv("C:/Users/SALEM/Downloads/datasets_529942_1182360_sars_2003_complete_dataset_clean.csv")
View(data1) # First Sars dataset
columns1 <- read_csv("C:/Users/SALEM/Downloads/datasets_529942_1182360_sars_2003_complete_dataset_clean.csv") #Showing column names of the first sars dataset

data2 <- read.csv("C:/Users/SALEM/Downloads/datasets_529942_1182360_summary_data_clean.csv")
View(data2) #Second Sars dataset
columns2 <- read_csv("C:/Users/SALEM/Downloads/datasets_529942_1182360_summary_data_clean.csv") #Showing column names of the second sars dataset
cleaningdata2 <- data2 %>% filter( !is.na(Median.age), !is.na(Number.of.Imported.cases), !is.na(Percentage.of.Imported.cases)) #Data cleaning by getting rid of NA values
View(cleaningdata2)

china <- filter(data1, Country == "China") #Filtered data so that only rows that have China as a country show up in the dataset
View(china)
ggplot(data = china, aes(x = Date, y = Cumulative.number.of.case.s., group = 1)) + geom_line() + geom_point() + theme(axis.text.x = element_text(angle = 90)) #Line graph of the total number of cases for each day in china
ggplot(data = china, aes(x = Date, y = Number.of.deaths, group = 1)) + geom_line() + geom_point() + theme(axis.text.x = element_text(angle = 90)) #Line graph of the total number of deaths for each day in china
ggplot(data = china, aes(x = Date, y = Number.recovered, group = 1)) + geom_line() + geom_point() + theme(axis.text.x = element_text(angle = 90)) #Line graph of the total number of people who recovered from the virus in china

USA <- filter(data1, Country == "United States") #Filtered data so that only rows that have United States as a country show up in the dataset
View(USA)
ggplot(data = USA, aes(x = Date, y = Cumulative.number.of.case.s., group = 1)) + geom_line() + geom_point() + theme(axis.text.x = element_text(angle = 90)) #Line graph of the total number of cases for each day in the United States
ggplot(data = USA, aes(x = Date, y = Number.of.deaths, group = 1)) + geom_line() + geom_point() + theme(axis.text.x = element_text(angle = 90)) #Line graph of the total number of deaths for each day in the United States
ggplot(data = USA, aes(x = Date, y = Number.recovered, group = 1)) + geom_line() + geom_point() + theme(axis.text.x = element_text(angle = 90)) #Line graph of the total number of people who recovered from the virus in the United States       

meanage <- summarise(data2, mean = mean(Median.age, na.rm = TRUE)) #Mean median age of the people in the sars dataset
View(meanage)
meanHCW <- summarise(data2, mean2 = mean(Number.of.HCW.affected)) #Average of the number of health care workers affected by sars

chinaUSA <- union(USA, china) #Union of the two USA and china dataset together
View(chinaUSA)

min(chinaUSA$Date) 
betterdates <- as.Date(chinaUSA$Date, origin = "2003-03-19") #Putting the dates in a date format
View(betterdates)
min(betterdates) #Minimum date in the dataset
newvariable1 <- mutate(chinaUSA, newdateformat = betterdates) #Added a new variable
View(newvariable1)

chinaUSA2 <- select(newvariable1, Country:newdateformat) #Selected the columns we want
View(chinaUSA2)
chinaUSA3 <- chinaUSA2[, c(5, 1, 2, 3, 4)] #Columns reordered
View(chinaUSA3)
ggplot(data = chinaUSA3, aes(x = newdateformat, y = Cumulative.number.of.case.s., group = Country, colour = Country)) + geom_line() + geom_point() + theme(axis.text.x = element_text(angle = 90)) #Line graph of the total number of cases for each day in the United States and china
ggplot(data = chinaUSA3, aes(x = newdateformat, y = Number.of.deaths, group = Country, colour = Country)) + geom_line() + geom_point() + theme(axis.text.x = element_text(angle = 90)) #Line graph of the total number of deaths in the United States and china
ggplot(data = chinaUSA3, aes(x = newdateformat, y = Number.recovered, group = Country, colour = Country)) + geom_line() + geom_point() + theme(axis.text.x = element_text(angle = 90)) #Line graph of the total number of recoveries in the United States and china
ggplot(data = chinaUSA3) + geom_histogram(mapping = aes(x = Cumulative.number.of.case.s.), binwidth = 0.5) #Histogram showing how frequent the cases are

chinaUSA4 <- chinaUSA3 %>% filter(newdateformat >= "2003-05-01") #Filtered the dataset so that only rows with a date after may 1st, 2003 show up
ggplot(data = chinaUSA4, aes(x = newdateformat, y = Cumulative.number.of.case.s., group = Country, colour = Country)) + geom_line() + geom_point() + theme(axis.text.x = element_text(angle = 90)) #Line graph of the total number of cases for each day in the United States and china
ggplot(data = chinaUSA4, aes(x = newdateformat, y = Number.of.deaths, group = Country, colour = Country)) + geom_line() + geom_point() + theme(axis.text.x = element_text(angle = 90)) #Line graph of the total number of deaths in the United States and china
ggplot(data = chinaUSA4, aes(x = newdateformat, y = Number.recovered, group = Country, colour = Country)) + geom_line() + geom_point() + theme(axis.text.x = element_text(angle = 90)) #Line graph of the total number of recoveries in the United States and china

summarise(data2, cases1 = mean(Cumulative.male.cases),  cases2 = mean(Cumulative.female.cases), cases3 = mean(Cumulative.total.cases)) #Averages for the cumulative male, female, and total cases
summarise(data2, age = mean(Number.of.HCW.affected, na.rm = TRUE)) #Average of the number of health care workers affected by SARS

ggplot(data = data2) + geom_bar(mapping = aes(x = Country.Region, y = Cumulative.total.cases, fill = Country.Region), stat = "identity") + theme(axis.text.x = element_text(angle = 90)) #Bar graph showing the number of cumulative SARS cases for each country
ggplot(data = data2) + geom_bar(mapping = aes(x = Country.Region, y = Cumulative.male.cases, fill = Country.Region), stat = "identity") + theme(axis.text.x = element_text(angle = 90)) #Bar graph showing the number of male cumulative SARS cases for each country
ggplot(data = data2) + geom_bar(mapping = aes(x = Country.Region, y = Cumulative.female.cases, fill = Country.Region), stat = "identity") + theme(axis.text.x = element_text(angle = 90)) #Bar graph showing the number of female cumulative SARS cases for each country
ggplot(data = data2) + geom_bar(mapping = aes(x = Country.Region, y = No..of.deaths, fill = Country.Region), stat = "identity") + theme(axis.text.x = element_text(angle = 90)) #Bar graph showing the number of deaths for each country

data3 <- select(cleaningdata2, Country.Region:No..of.deaths, Median.age:Age.range, Number.of.HCW.affected) #selecting columns that are important
View(data3)

means <-  tribble(
  ~cases,                                   ~Averages,
  "Cumulative male cases",                   147.3103,
  "Cumulative female cases",                 130.2759,
  "Cumulative total cases",                  279.1724,
  "Number of Health Care Workers affected",  58.82759
) #Table for the averages for the cumulative male and female casses, total cases, and number of healthcare workers affected by SARS
ggplot(data = means) + geom_bar(mapping = aes(x = cases, y = Averages), stat = "identity")+ theme(axis.text.x = element_text(angle = 90)) #Bar graph with the averages of the cumulative male and female cases, total cases, and number of health care workers affected by SARS

numberofcases <-  group_by(data1, Date) #Groups everything by date
numberofcases2 <- summarise(numberofcases, count = sum(Cumulative.number.of.case.s.)) #Showing the number of cases for each day
View(numberofcases2)

casesoverall <- tribble(
  ~Date,                        ~Count,
  "2003-03-17",                    167,
  "2003-03-18",                    219,
  "2003-03-19",                    264,
  "2003-03-20",                    306,
  "2003-03-21",                    350,
  "2003-03-22",                    386,
  "2003-03-24",                    456,
  "2003-03-25",                    487,
  "2003-03-26",                    1323,
  "2003-03-27",                    1408,
  "2003-03-28",                    1485,
  "2003-03-29",                    1550,
  "2003-03-31",                    1622,
  "2003-04-01",                    1804,
  "2003-04-02",                    2223,
  "2003-04-03",                    2270,
  "2003-04-04",                    2353,
  "2003-04-05",                    2416,
  "2003-04-07",                    2601,
  "2003-04-08",                    2671,
  "2003-04-09",                    2722,
  "2003-04-10",                    2781,
  "2003-04-11",                    2890,
  "2003-04-12",                    2960,
  "2003-04-14",                    3169,
  "2003-04-15",                    3235,
  "2003-04-16",                    3293,
  "2003-04-17",                    3389
) #Table showing the number of cumulative cases each day
ggplot(data = casesoverall, aes(x = Date, y = Count, group = 1)) + geom_line() + geom_point() + theme(axis.text.x = element_text(angle = 90)) #Line graph showing the number of cumulative cases for each day
ggplot(data = casesoverall) + geom_bar(mapping = aes(x = Date, y = Count, fill = Date), stat = "identity") + theme(axis.text.x = element_text(angle = 90)) #Bar graph showing the number of cumulative cases each day

separate <- data1 %>% separate(Date, into = c("Year", "Month", "Day")) #Separating the date column into three columns which are the year, month and day
View(separate)
byyear <- group_by(separate, Year) #Grouping by year
summarise(byyear, totalcases = mean(Cumulative.number.of.case.s.)) #Average of the cumulative number of cases in 2003
bymonth <- group_by(separate, Month) #Grouping by month
summarise(bymonth, totalcases = sum(Cumulative.number.of.case.s.)) #Sum of the cumulative number of cases in each month
monthlycases <-  tribble(
  ~Month,           ~totalcases,
  03,                10023,
  04,                90042,
  05,                203272,
  06,                177118,
  07,                75905
) #Table of the number of cases monthly 
View(monthlycases)
ggplot(data = monthlycases) + geom_bar(mapping = aes(x = Month, y = totalcases, fill = Month), stat = "identity") #Bar graph showing the number of cumulative cases monthly
ggplot(data = monthlycases, aes(x = Month, y = totalcases, group = 1)) + geom_line() + geom_point() + theme(axis.text.x = element_text(angle = 90)) #Line graph showing the number of cumulative cases for each month
ggplot(data = separate) + geom_histogram(mapping = aes(x = Cumulative.number.of.case.s.), binwidth = 0.5) + coord_cartesian(xlim = c(0, 100)) #Histogram showing how frequent the cumulative cases are


numberofdeaths <- data1 %>% count(cut_width(Number.of.deaths, 5))#Range of the number of deaths in the dataset
View(numberofdeaths)

boxwhiskerplot1 <- filter(separate, Country == "China" | Country == "United States" | Country == "United Kingdom" | Country == "Japan" | Country == "Viet Nam") #Filtering dataset to five countries
View(boxwhiskerplot1)
ggplot(data = boxwhiskerplot1, mapping = aes(x = Country, y = Cumulative.number.of.case.s.)) + geom_boxplot() + coord_flip() + theme(axis.text.x = element_text(angle = 90)) #Box and whisker plot for the cumulative number of cases
ggplot(data = boxwhiskerplot1, mapping = aes(x = Country, y = Number.of.deaths)) + geom_boxplot() + coord_flip() + theme(axis.text.x = element_text(angle = 90)) #Box and whisker plot for the number of people who recovered
ggplot(data = boxwhiskerplot1, mapping = aes(x = Country, y = Number.recovered)) + geom_boxplot() + coord_flip() + theme(axis.text.x = element_text(angle = 90)) #Box and whisker plot for the number of deaths

fivecountries <- filter(data1, Country == "China" | Country == "United States" | Country == "United Kingdom" | Country == "Japan" | Country == "Viet Nam") #Filtering dataset to four countries
View(fivecountries)
betterdates2 <- as.Date(fivecountries$Date, origin = "2003-03-19") #Putting the dates in a date format
View(betterdates2)
newvariable2 <- mutate(fivecountries, newdateformat2 = betterdates2) #Added a new variable
ggplot(data = newvariable2, aes(x = newdateformat2, y = Cumulative.number.of.case.s., group = Country, colour = Country)) + geom_line() + geom_point() + theme(axis.text.x = element_text(angle = 90)) #Line graph of the total number of cases for each day in the five countries
