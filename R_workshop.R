getwd()
setwd("C:/Users/lawre/Documents/R/R workshop")
law <- 2+10
bay <- 5+5
law
rm(law, bay)
install.packages("sp")#for spatial analysis
install.packages("tidyverse")
library(vegan)
#installing from github
install.packages("devtools")
library(tidyverse)
#installing multiple packages and calling out
filter()


Hammed <- c(1:100)
#array data type
a <- array(c(2,3,1), dim = c(2,3,3))
a

#factor data type
apple <- c("Red", "green", "yellow", "Red", "green",
           "green")
Facapple <- factor(apple)
Facapple

#building function
#function that print Happy birthday message and add date
HappyBirthday <- function(Name, Birthday) {
  Sentence <- paste0(
    "Hi",
    Name,
    "! Because today is ",
    Birthday,
    " it is your",
    " very special day. Happy birthday man!"
  )
  print(Sentence)
}

HappyBirthday("Lawrence", "Nov 20., 2022")

#create a function for m=x-mean/s
stand <- function(x) {
  m = mean(x)
  std = sd(x)
  output = (x-m)/std
  output
}
set.seed(100)#wont change the output of random generation until after 100the output
test_data <- rnorm(n =10, mean=4, sd=25)
test_data
stand(x=test_data)

#conditional statement
a = 2
b = 4
if (a>b) {
  c = a*b
  print("a is larger than b")
} else {
  c = a + 4 * b
  print("a is less than or equal to b")
}

#iteration(loop)
book_titles <- c("book1","book2","book3","book4","book5")
for (i in book_titles) {
  print(i)
}

a =12:16
for (i in 1:5) {
  a[i] = a[i]*2
  print (a[i])
}

#Day2 Data wrangling and transformation
library(tidyverse)
install.packages("palmerpenguins")
library(palmerpenguins)
head(penguins)
penguinsData <- penguins
penguinsData
glimpse(penguinsData)
#to select few coloumns
Newdata1 <- select(penguinsData, species, island, sex, year)
Newdata1

select(penguinsData, bill_length_mm:body_mass_g)

#Filter function
bodymasspen <- filter(penguinsData, body_mass_g > 3600)
bodymasspen

penisland <- filter (penguinsData, island=="Dream")
penisland
#Arrange

arrangedpen <- arrange(penguinsData, year, body_mass_g)
arrangedpen

#pipping
pipedpen <- penguinsData %>%
  select(year, species, island, body_mass_g) %>%
  arrange(year)
pipedpen

#distinct function(allow to see the levels of data)
distinct(penguinsData, species)#how maany distinct island
distinct(penguinsData, year)
distinct(penguinsData, island)

penguin_data_A <- select(penguinsData, species:body_mass_g, year)
penguin_data_A
penguin_data_B <- select(penguinsData, sex:year)

#joining the two data set
penguin_combo <- full_join(penguin_data_A, penguin_data_B, by = "year")
penguin_combo
#others are left_join anf right_join

#pivot wider and pivot longer
penguins_wider <- pivot_wider(penguinsData, names_from = sex, values_from = bill_depth_mm)

penguins_longer <- pivot_longer(penguins_wider, cols = male:"NA", names_to = "sex",
                                values_to = "bill_depth_mm")

#Exploratory data analysis

summarisepen <- penguinsData %>% 
  drop_na(body_mass_g) %>% 
  group_by(species) %>% 
  summarise(MeanBodyMass_Kg = mean(body_mass_g/1000))
summarisepen
penguinsData
#mutate function
Law <- penguinsData %>% 
  drop_na (body_mass_g, sex) %>% 
  group_by(species, sex) %>% 
  summarise(
    meanbodymass_Kg = mean(body_mass_g/1000),
    minbodymass_kg = min(body_mass_g/1000),
    maxbodymass_kg = max(body_mass_g/1000)
  ) %>% 
  mutate(species_mean = mean(meanbodymass_Kg))
Law
#plotting in ggplot

install.packages("gapminder")
library(gapminder)
str(gapminder)
glimpse(gapminder)
GapData <- gapminder
GapData

ggplot(GapData, aes(x=continent)) +
  geom_bar()
bargrap <- ggplot(GapData, aes(x=continent, fill =continent)) +
  geom_bar(aes(y=..count../12)) +#count the number of countries in leve two(the two dots)
  labs(y="Number of Countries") +
  guides(fill=FALSE)
bargraph + coord_flip()
bargraph + coord_polar()

#density graph

ggplot(GapData, aes(x=lifeExp)) +
  geom_density()

ggplot(GapData, aes(x=lifeExp, fill=continent)) +
  geom_density(alpha = 0.2)

#boxplots
ggplot(GapData, aes(x=continent, y=lifeExp, fill=continent)) +
  geom_boxplot()

GapData %>% 
  group_by(continent, year) %>% 
  summarise(Median_lifeExp=median(lifeExp)) %>% 
  ggplot(aes(x=year, y=Median_lifeExp, colour=continent)) +
  geom_line() +
  geom_point()


scargraph <- ggplot(GapData, aes(x=gdpPercap, y=lifeExp))
jpeg("life Expectancy.jpeg", width = 300, height = 350)
scargraph + geom_point(aes(colour=continent)) +
  geom_smooth() + 
  theme_classic()
dev.off()
setwd("C:/Users/lawre/Documents/R/R workshop")

install.packages("swirl")
library(swirl)
swirl()
Lawrence








