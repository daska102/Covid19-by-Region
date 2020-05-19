library (readr)
library(ggplot2)
library(dplyr)
library(sf)
library(maps)
library(ggthemes)
library(tidyverse)
library(wesanderson)


#reading in population data
populations <- read_csv("SCPRC-EST2019-18+POP-RES.csv")


#getting raw data from john hopkins covid 19 dataset
urlfile="https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"
jhCovid<-read_csv(url(urlfile))
states<- map_data("state")


#restructuring dataset from timeseries
johnHopkinsConfirmedCases <- jhCovid %>%
  pivot_longer(matches("/20"),
               names_to="dates",
               values_to = "case_count")%>%
  group_by(Province_State) %>%
  mutate(date_count = row_number()) %>%
  ungroup()


#adding a row to calculate new cases by day per region
jhuWithCounts <- johnHopkinsConfirmedCases %>%
  group_by(Lat, Long_) %>%
  mutate(count = n(), todaysCases = case_count[count] - case_count[count - 1], case_sum = cumsum(case_count))


#removing duplicates of so that each lat and long only have one entry in dataset, keeps only the last date entry
#will use this for map
jhuNoDupes <- jhuWithCounts[!duplicated(jhuWithCounts[c("Lat","Long_")], fromLast = T), ]


#unused for visualizations or infographic, but I used this to check that my calcultions were accurate
byStateCounts <- jhuWithCounts %>%
  group_by(Province_State, dates) %>%
  mutate(stateCases = sum(case_count), dailyCaseCountSum = sum(case_sum))
byStateCounts <- byStateCounts[!duplicated(byStateCounts[c("Province_State", "dates")], fromLast = T), ]
byStateCounts <-  byStateCounts[order(-byStateCounts$stateCases),] 


#gathering data per region: west, central, and east
westStates <- c("Washington", "Oregon", "California", "Idaho", "Nevada", "Arizona", "Montana", "Utah", "New Mexico",
                    "Colorado", "Wyoming")

centralStates <- c("North Dakota", "South Dakota", "Minnesota", "Wisconsin", "Michigan" , "Indiana" , "Illinois", "Iowa",
                 "Nebraska", "Kansas", "Missouri", "Arkansas", "Oklahoma", "Texas", "Louisiana")

eastStates <- c("Ohio", "Pennsylvania", "Kentucky" , "Tennessee", "Indiana", "Virginia", "West Virginia", "Maryland", 
                "Delaware" , "New Jersey", "Connecticut", "Rhode Island",
                "New York", "Massachusetts", "Vermont", "New Hampshire", "Maine", "Mississippi", "Alabama", 
                "Georgia", "North Carolina", "South Carolina", "Florida", "District of Columbia")


#creates a subset of all states in a regoin and sums their 2019 populations
getRegionPopulation <- function(regionArray) {
  regionPopSubset <- subset(populations, is.element(populations$NAME , regionArray))
  pop <- sum(regionPopSubset$POPESTIMATE2019)
  return(pop)
}


#getting populations for infographic
wpop <- getRegionPopulation(westStates)
epop <- getRegionPopulation(eastStates)
cpop <- getRegionPopulation(centralStates)


#creates a new dataframe for each region
createRegionalDF <- function(regionArray, dateToGatherInformationFrom) {
  regionSubset <- subset(jhuWithCounts,  is.element(jhuWithCounts$Province_State , regionArray))
  regionSubset$DATES <- as.Date(regionSubset$dates, "%m/%d/%y")
  
  #groups each region by dates and adds a column with sum of cases per day
  regionSubset <- regionSubset %>%
    group_by(DATES) %>%
    mutate(regionalCases = sum(case_count))
  
  #gets rid of duplicate dates
  regionSubset <- regionSubset[!duplicated(regionSubset[c("dates")], fromLast = T), ]
  
  #only collects dates after selected date
  regionSubset <- subset(regionSubset, DATES > as.Date(dateToGatherInformationFrom))
  
  #creates a column which holds new cases per day
  regionSubset$newCaseCount <- regionSubset$regionalCases - lag(regionSubset$regionalCases, default = first(regionSubset$regionalCases))
  
  #adds a per 100,000 population column for total cases in the region
  #uses the getRegionPopulation function
  regionSubset$per100000 <- regionSubset$regionalCases * 100000 / getRegionPopulation(regionArray)
  return(regionSubset)
}


#creating separate dataframes for regions
westStateDF <- createRegionalDF(westStates, "2020-03-15")
centralStateDF <- createRegionalDF(centralStates, "2020-03-15")
eastStateDF <- createRegionalDF(eastStates, "2020-03-15")


#plot of US Map with all current cases + new cases in last 24 hours
ggplot() + 
  geom_polygon( data=states
          , aes(x=long, y=lat, group=group), color="#01d9e1", fill= wes_palette("BottleRocket2")[5] ) +  
  theme_map() +
  ggtitle("U.S. Cases of Covid 19") +
  geom_sf() +
  geom_point(data = jhuNoDupes, aes( x = Long_, y = Lat, size = ifelse(case_count==0, NA, case_count)), 
             shape = 21, alpha = .8, fill = wes_palette("Zissou1")[1],stroke = 0) +
  geom_point(data = jhuNoDupes, aes(x = Long_, y = Lat, size = ifelse(todaysCases==0, NA, todaysCases)), 
             shape = 21, alpha = .8, stroke = 0, fill = wes_palette("Zissou1")[5]) +
  scale_size(guide = FALSE, range = c(1.3, 12)) +
  geom_count() +
  coord_sf(xlim = c(-125,-60), ylim= c(24,50), expand = TRUE) + 
  theme(title= element_text(hjust = 0.5, vjust=1, face="bold")) + 
  scale_fill_discrete(name = "Case Type", labels = c("New Cases", "Total Confirmed Cases"))


#overlapping bar plot of count of new cases by day per region
ggplot() +
  theme_set(theme_minimal()) +
  theme(  axis.text=element_text(size=10, face="bold"), axis.title=element_text(size=10, face="bold"), plot.title =element_text(size=13, face="bold"))+
  geom_bar(data = eastStateDF, stat="identity", aes(x = DATES, y = newCaseCount), fill = "#e6dba3")+
  geom_bar(data = centralStateDF, stat="identity", aes(x = DATES, y = newCaseCount), fill = "#a0bd98") +
  geom_bar(data = westStateDF, stat="identity", aes(x = DATES, y = newCaseCount), fill = "#dfbfd3") +
  ggtitle("New Covid Cases per Day")+
  xlab("") + ylab("New Cases")


#line graph showing total confurmed cases by region, per 100,000 pop
ggplot() +
  theme_set(theme_minimal()) +
  theme(  axis.text=element_text(size=10, face="bold"), axis.title=element_text(size=10, face="bold"), plot.title =element_text(size=13, face="bold"))+
  geom_line(data = centralStateDF, aes(x = DATES, y =  per100000), color= "#a0bd98", size = 3.5)  + 
  geom_line(data = westStateDF, aes(x = DATES, y = per100000), color= "#dfbfd3", size = 3.5)  +
  geom_line(data = eastStateDF, aes(x = DATES, y = per100000), color= "#e6dba3", size = 3.5) +
  ggtitle("Total Case Count Per 100,000 Population Size") +
  xlab("") + ylab("Total Case Count")

