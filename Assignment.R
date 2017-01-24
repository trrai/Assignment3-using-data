#################
#### PART 1 #####
#################

#Create vector names with the names of the 6 TAs in the course
names<-c("Dylan Babbs", "Iman Baghi", "Andrea Chen", "Jack Fox", "Evan James Frawley", "Chris Ross")

#Create a vector math.grades with 6 hypothetical grades (0 - 100) in a math course (that correspond to the 6 names above)
math.grades<-c(85,89,92,88,92,99)

#Create a vector spanish.grades with 6 hypothetical grades (0 - 100) in a Spanish course (that correspond to the 6 names above).
spanish.grades<-c(82,84,90,91,92,98)

#Create a data frame variable tas by combining the three vectors.
tas<-data.frame(names,math.grades,spanish.grades)

#Print a sentence "The TA data frame has X rows and Y cols: COL_NAMES", replacing X with the number of rows, Y with the number of columns, and COL_NAMES with a comma-separated list of the column names.
X<- nrow(tas)
Y<-ncol(tas)
COL_NAMES<-paste(colnames(tas), collapse = ", ")
print(paste("The TA data frame has", X, "rows and", Y, "cols: ", COL_NAMES))

#You can print out the names of the columns... but giving the rows names would help! Add a name for each row which is the name of the TA (e.g., using your names variable).
row.names(tas)<-names
#Print out the row of the data frame with your TA's grades.
print(tas[tas$names == "Chris Ross", ])

#Add a new column grade.diff to your data frame, which has values equal to each TA's math grade minus the TA's Spanish grade.
tas$grade.diff<-math.grades-spanish.grades

#Add another column better.at.math as a boolean (TRUE/FALSE) variable that indicates that a TA got a better grade in math.
tas$better.at.math<-math.grades>spanish.grades

#Create a variable num.better.at.math that is the count of how many TAs are better at math. Print out this variable.
num.better.at.math<-length(tas[tas$better.at.math == TRUE, "better.at.math"])

#Write your data frame to a new .csv file inside your data/ directory with the filename grade_data.csv. Do not include the row names you added!
write.csv(tas, file="data/grade_data.csv", row.names = FALSE)


#################
#### PART 2 #####
#################

#Read in the life_expectancy.csv file found in the data/ directory. Store it into a variable called life.expectancy. You may also want to use RStudio to View() it to learn about its rows and columns.
life.expectancy<-read.csv(file="data/life_expectancy.csv", stringsAsFactors = FALSE)

#Add a column called change to the data frame that is the change in life expectancy from 1960 to 2013.
life.expectancy$change<-life.expectancy$le_2013 - life.expectancy$le_1960

#Create a variable num.small.gain that has the number of countries whose life expectancy has improved fewer than 5 years between 1960 and 2013. Print out this variable
num.small.gain<-length(life.expectancy[life.expectancy$change<5, "country"])
print(num.small.gain)

#Create a variable most.improved that is the name of the country with the largest gain in life expectancy. Print out this variable.
most.improved<-life.expectancy[life.expectancy$change == max(life.expectancy$change), "country"]
print(most.improved)

#Define a function CountryChange that takes in a country's name as an argument, and returns that country's change in life expectancy from 1960 to 2013.
CountryChange<- function(country){
  return(life.expectancy[life.expectancy$country == country, "change"])
}

#Use your CountryChange function to print out the change in life expectancy from 1960 to 2013 in Haiti.
CountryChange("Haiti")

#Define a function LowestLifeExpInRegion that takes in a region as an argument, and returns the name of the country with the lowest life expectancy in 2013 (in that region)
LowestLifeExpInRegion<- function(region){
  region.countries.life.expectancies<-life.expectancy[life.expectancy$region == region, "le_2013"]
  lowest.life.exp.country<- life.expectancy[life.expectancy$region == region & 
                                              life.expectancy$le_2013 == min(region.countries.life.expectancies), "country"]
  return(lowest.life.exp.country)
}
#Use your LowestLifeExpInRegion to print out the country with the lowest life expectancy in 2013 in South Asia.
LowestLifeExpInRegion("South Asia")

#Define a function CompareCountries that takes in two country names as arguments, and returns a data frame representing a table of those two countrys life expectancies in 2013 and the change since 1960
CompareCountries<- function(country1, country2){
  country.one.le_2013<- c(life.expectancy[life.expectancy$country == country1, "le_2013"])
  country.two.le_2013<- c(life.expectancy[life.expectancy$country == country2, "le_2013"])
  country.one.change<- CountryChange(country1)
  country.two.change<- CountryChange(country2)
  countries<- c(country1,country2)
  countries.life.expectancies<- c(country.one.le_2013, country.two.le_2013)
  countries.change<- c(country.one.change, country.two.change)
  return(data.frame(countries, countries.life.expectancies, countries.change))
}

# Use your CompareCountries function to compare the life expectancy changes of the United States and Cuba, and store the data frame in a variable us.vs.cuba.
us.vs.cuba<-CompareCountries("United States", "Cuba")

#What do you think might explain this result? Maybe something about their health care system (Links to an external site.)?




#################
#### PART 3 #####
#################
#Load the Titanic data set built into R. You may also want to use RStudio to View() it to learn about its rows and columns.
data("Titanic")

#This data set actually loads in a format called a table (Links to an external site.), which is slightly different than a data frame. Use the is.data.frame() function to confirm this.
is.data.frame(Titanic)

#You should convert the Titanic variable into a data frame; you can use the data.frame() function or as.data.frame() Be sure to not treat strings as factors!
titanic.data<-data.frame(Titanic)

#Create a variable children that are the rows of the data frame with information about children on the Titanic.
children<- titanic.data[titanic.data$Age == "Child", ]
View(titanic.data)
#Create a variable children.num that is the total number of children on the Titanic. 
children.num<- sum(titanic.data[titanic.data$Age == "Child", "Freq"])
print(children.num)

#Output the row in the data frame which has the largest absolute number of losses (people who did not survive). 
did.not.survive<-titanic.data[titanic.data$Survived == "No", ]
most.lost<-did.not.survive[did.not.survive$Freq == max(did.not.survive$Freq), ]
print(most.lost)

#Define a function called SurvivalRate that takes in a ticket class (e.g., "1st", "2nd") as an argument. This function should return a sentence describing the total survival rate of men vs. "women and children" in that ticketing class. For example: "Of Crew class, 87% of women and children survived and 22% of men survived."
SurvivalRate<- function(class){
  #MEN SURVIVAL RATE CALCULATIONS
  men.died<-titanic.data[titanic.data$Sex=="Male" & titanic.data$Age == "Adult" & titanic.data$Survived == "No"
                          & titanic.data$Class == class, "Freq"]
  
  men.survived<- titanic.data[titanic.data$Sex=="Male" & titanic.data$Age == "Adult" & titanic.data$Survived == "Yes"
                               & titanic.data$Class == class, "Freq"]
  men.total<- men.died + men.survived
  men.survival.rate<- round((men.survived/men.total) * 100)

  #WOMEN + CHILDREN SURVIVAL RATE CALCULATIONS
  women.died<-titanic.data[titanic.data$Sex=="Female" & titanic.data$Age == "Adult" & titanic.data$Survived == "No"
                          & titanic.data$Class == class, "Freq"]
  
  women.survived<- titanic.data[titanic.data$Sex=="Female" & titanic.data$Age == "Adult" & titanic.data$Survived == "Yes"
                               & titanic.data$Class == class, "Freq"]
  women.total<- women.died + women.survived
  
  children.died<-titanic.data[titanic.data$Age == "Child" & titanic.data$Survived == "No" & titanic.data$Class == class, "Freq"]
  
  children.survived<- titanic.data[titanic.data$Age == "Child" & titanic.data$Survived == "Yes" & titanic.data$Class == class, "Freq"]
  
  #Since the children are male and female, we need to combine the numbers for genders
  children.total.died<- children.died[1] + children.died[2] 
  children.total.survived<- children.survived[1] + children.survived[2]
  children.total<- children.total.died + children.total.survived
  
  women.and.children.survival.rate<- round((women.survived + children.total.survived)/(women.total + children.total) * 100)
  
  sentence<- paste0("Of the ", class, " class, ", women.and.children.survival.rate, "% of women and children survived and ", 
                    men.survival.rate, "% of men survived.")
  return(sentence)
}



#Call your SurvivalRate() function on each of the ticketing classes (1st, 2nd, and 3rd), printing out the results.
SurvivalRate("1st")
SurvivalRate("2nd")
SurvivalRate("3rd")
