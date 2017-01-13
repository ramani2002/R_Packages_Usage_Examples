
# Read in the tab delimited file into an object. Note the full path of the file.
UN_VotingData <- read.table("C:/Users/ramani/Documents/UN Voting Data for DataCamp Class/RawVotingdata.txt", sep = '\t', header = T)

# Display the object
UN_VotingData

# Change the order of columns into a new object. The comma indicates all rows, the values inside the curly brackets
# are the column indexes in the original dataset
UN_VotingData2 <- UN_VotingData[,c(3,2,4,1)]

# rcid is Roll Call ID, session is the year long session in UN's history, vote is the country's choice 1 = yes, 2 = abstain,
# 3 = no, 9 = not present, 9 = country not yet a UN member, ccode is the country code to uniquely specify the country
UN_VotingData2

summary(UN_VotingData2)

str(UN_VotingData2)

dim(UN_VotingData2)

# Since we only care about yes, no, and abstain votes, we pipe the dataset through the filter function
# In the filter function, we define a condition
# make sure to attach both the dplyr and magrittr packages before running the pipe statement!
library(magrittr)
library(dplyr)
# Notice how the output of pipe is passed to a new object....
UN_VotingData3 <- UN_VotingData2 %>% filter(UN_VotingData2$vote <= 3)
# check the size of the new dataset
dim(UN_VotingData3)

# The session variable is a bit tough to interpret. If the first UN session was held in 1945, then each session is a
# count after 1945. We use the MUTATE pipe to do this
UN_VotingData4 <- UN_VotingData3 %>% mutate(year = session + 1945)
UN_VotingData4

# Now convert the ccodes into country names. To do this load the COUNTRYCODE package and used chained pipes
# install.packages("countrycode", repos='http://cran.us.r-project.org')
library(countrycode)

votes_processed <- UN_VotingData %>%
  filter(vote <= 3) %>%
  mutate(year = session + 1945) %>%
  mutate(country = countrycode(ccode, "cown", "country.name"))
votes_processed
dim(votes_processed)

# In the above chained pipes, I have done all previous steps at the same time

summary(votes_processed)
class(votes_processed)

# Once more we change the order of columns
votes_processed <- votes_processed[,c(3,2,4,1,5,6)]
votes_processed

# Now we use SUMMARIZE to begin aggregating the data. The ground rule is that if a country votes YES on a majority
# of resolutions we assume it votes along international consensus. If NO, the alternative assumption.
# We can use SUMMARIZE to calculcate the number of rows as:
votes_processed %>% summarize(total = n())
# the output of the above statement is the total number of rows. This output may be passed to an object if desired.
# n() is a special function within SUMMARIZE meaning 'the number of rows.' So the above statement gives the total
# number of rows in the dataframe.

# Now we want to calculate the percent yes votes.
votes_processed %>% summarize(total = n(), percent_yes = (mean(vote == 1)*100))

# the above output tells us that 79.6 percent of votes have been yes-es in the history of the UN. But has this
# percentage changed over time?
# we use groupby() - before the summarize operation - Groupby() tells SUMMARIZE to create one row for each subgroup
# instead of one overall row
# since we want to check the change of yes votes over time, we group by year first
votes_processed %>% group_by(year) %>% summarize(total = n(), percent_yes = (mean(vote == 1)*100))

# How many countries have voted yes over the lifetime of the UN?
# Group_by country, summarize, and save the output in the by_country object and print out this object
by_country <- votes_processed %>%
  group_by(country) %>% summarize(total = n(), percent_yes = (mean(vote == 1)*100))
by_country

# to get a better idea of the votes, we sort the data using another DPLYR 'verb' ARRANGE()
by_country %>% arrange(percent_yes) # the default is ascending order
# the list can be sorted in descending order by invoking the desc() function inside arrange()
by_country %>% arrange(desc(percent_yes))

# Now we remove countries that have fewer than 100 votes
by_country2 <- by_country %>% arrange(percent_yes) %>% filter(total >= 100)


