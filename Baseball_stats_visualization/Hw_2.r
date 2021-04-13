# Guided Exercise 02
# Robert Alward
# 2/22/2021


### 0. Loading Data and Libraries

library(tidyverse)
library(ggplot2)

player_data <- read.csv("Master.csv")
pitching_data <- read.csv("Pitching.csv")
salary_data <- read.csv("Salaries.csv")
inflation_index <- read.csv("inflation.csv")

### 1. Data Cleaning

salary_data$yearID <- as.factor(salary_data$yearID)

### 2. Exploratory Graphs for Salary

# 2.1 General Salary Graph
ggplot(salary_data) +
  geom_boxplot(aes(x=yearID, y=salary))

# 2.2 Group By Graph

summary_salary <- summarize(group_by(salary_data, yearID), 
                            Q1 = quantile(salary,.25,na.rm=T),
                            median = median(salary,na.rm=T),
                            Q3 = quantile(salary,.75,na.rm=T),
                            min=min(salary,na.rm=T),
                            max=max(salary,na.rm=T))

summary_salary

# Note: Initial summary graph
ggplot(summary_salary)+
  geom_boxplot(aes(x=yearID, y=salary))

summary_salary$yearID <- as.numeric(as.character(summary_salary$yearID))

# Note: Edited summary graph with correct titles
ggplot(summary_salary) +
  geom_line(aes(x=yearID, y=median))

# 2.3 Salary Graph: Median, Q1, and Q2
ggplot(summary_salary) +
  geom_line(aes(x=yearID, y=median)) +
  geom_line(aes(x=yearID, y=Q1)) +
  geom_line(aes(x=yearID, y=Q3))

# 2.4 Ribbon Graph

ggplot(summary_salary) +
  geom_line(aes(x=yearID, y=median)) +
  geom_ribbon(aes(x=yearID, ymin=Q1, ymax=Q3), fill="lightblue") +
  geom_line(aes(x=yearID, y=median),color="red")


# 2.5 Extra Aggregate Salary Data
salary_data$salary <- as.numeric(salary_data$salary)

summary_salary_2 <- summarize(group_by(salary_data, yearID),
                              median=median(salary,na.rm=T), 
                              count=n(), 
                              unique_count=n_distinct(playerID), 
                              million_plus_salaries=sum(salary >= 1000000), 
                              million_plus_salaries_proportion=mean(salary >= 1000000), 
                              total_salary = sum(salary), 
                              top_salary=max(salary), 
                              bottom_salary=min(salary))


### 3. Exploratory Graphs for ERA (earned run averages)

str(pitching_data)

pitching_data$yearID <- as.factor(pitching_data$yearID)

# 3.1: Plot #1
ggplot(pitching_data)+
  geom_boxplot(aes(x=yearID, y=ERA))


# 3.2: Plot #2
summary_ERA <- summarize(group_by(pitching_data, yearID), 
                            Q1 = quantile(ERA,.25,na.rm=T),
                            median = median(ERA,na.rm=T),
                            Q3 = quantile(ERA,.75,na.rm=T),
                            min=min(ERA,na.rm=T),
                            max=max(ERA,na.rm=T))


summary_ERA$yearID <- as.numeric(as.character(summary_ERA$yearID))

ggplot(summary_ERA) +
  geom_line(aes(x=yearID,y=median))

# 3.3: Plot #3

ggplot(summary_ERA) +
  geom_line(aes(x=yearID, y=median)) +
  geom_ribbon(aes(x=yearID, ymin=Q1, ymax=Q3), fill="lightgreen") +
  geom_line(aes(x=yearID, y=median),color="blue")

# 3.4: Plot #4 Proportions of Pitchers With Low and High ERAs by Year

pitching_data$ERA <- as.numeric(pitching_data$ERA)

head(pitching_data)

pitching_data_G <- filter(pitching_data, G >= 10)

pitching_data_G$yearID <- as.numeric(as.character(pitching_data_G$yearID))

summary_ERA_2 <- summarize(group_by(pitching_data_G, yearID),
                              six_plus_pitchers=mean(ERA >= 6,na.rm=T), 
                              three_under_pitchers=mean(ERA <= 3,na.rm=T))

ggplot(summary_ERA_2) + 
  geom_line(aes(x=yearID,y=(six_plus_pitchers),color= "Six or Higher")) +
  geom_line(aes(x=yearID,y=(three_under_pitchers),color= "Three or Under" )) +
  scale_color_manual(values=c("Three or Under" = "darkblue", "Six or Higher" = "red"),name = ("ERA")) +
  labs(title="Proportion of Pitchers (pitching at least 10 games) \n With Low and High ERAs by Year",
         x="Year",y="Proportion") +
  theme_classic()

### 4. Combining Data Sets: Salary Data

# 4.1 Joins

salary_data_keyed <- mutate(salary_data, row_num = row_number())

names(inflation_index)[1] <- "yearID"

head(inflation_index)
tail(inflation_index)

summary_salary_inner <- inner_join(summary_salary, inflation_index, by="yearID")
head(summary_salary_inner)
tail(summary_salary_inner)

summary_salary_left <- left_join(summary_salary, inflation_index, by="yearID")
head(summary_salary_left)
tail(summary_salary_left)

summary_salary_right <- right_join(summary_salary, inflation_index, by="yearID")
head(summary_salary_right)
tail(summary_salary_right)

summary_salary_full <- full_join(summary_salary, inflation_index, by="yearID")
head(summary_salary_full)
tail(summary_salary_full)

summary_salary_left[summary_salary_left$yearID==2015,"inflation2015"]<-1

# 4.2 Inflation adjusted graphs: Salary

summary_salary <- mutate(summary_salary_left, 
                         median_inflation_adjusted = median*inflation2015,
                         Q1_inflation_adjusted = Q1*inflation2015, 
                         Q3_inflation_adjusted = Q3*inflation2015, 
                         min_inflation_adjusted = min*inflation2015,
                         max_inflation_adjusted = max*inflation2015)

head(summary_salary)

ggplot(summary_salary)+
  geom_ribbon(aes(x=yearID, ymin=Q1_inflation_adjusted, 
                                       ymax=Q3_inflation_adjusted),fill="lightblue")+
  geom_line(aes(x=yearID, y=median_inflation_adjusted),color="red")


ggplot(summary_salary)+
  geom_ribbon(aes(x=yearID, ymin=Q1_inflation_adjusted, ymax=Q3_inflation_adjusted,
                  fill="Middle 50% of Earners"))+
  geom_line(aes(x=yearID, y=median_inflation_adjusted),color="red")+
  geom_ribbon(aes(x=yearID,ymin=min_inflation_adjusted,ymax=Q1_inflation_adjusted,
                  fill="Bottom 25% of Earners"))+
  scale_y_continuous(labels = scales::dollar)+
  labs(y="Annual Salary  \n (Adjusted for Inflation)",
       x="Year",title="Salaries of Lower 75% of Earners in Major League Baseball")+
  scale_fill_manual(name="",
                    values=c("Middle 50% of Earners"="lightblue", "Bottom 25% of Earners" = "darkblue"))+
  theme_minimal()

### 5. Salaries of Middle 50% earners

# 5.1 Data Organization Work

### Birth Place Data
player_data

# Filtering out US and Foreign Born players
USA_born <- filter(player_data, birthCountry == "USA")
Foreign_born <- filter(player_data, birthCountry != "USA")

#Joining the birthplace data with salary data
USA_born_right <- inner_join(salary_data, USA_born, by="playerID")
Foreign_born_right <- inner_join(salary_data, Foreign_born, by="playerID")

# Adding variables to identify US or Not US born players
USA_born_right$birth_place <- "Born in USA" 
Foreign_born_right$birth_place <- "Born outside USA"

# Binding the data backtogether
complete_birth_data <- rbind(USA_born_right,Foreign_born_right)
head(complete_birth_data)

# Creating Summary Data for the salary combined with birth place data
summary_birth <- summarize(group_by(complete_birth_data, yearID, birth_place), 
                           Q1 = quantile(salary,.25,na.rm=T),
                           median=median(salary,na.rm=T),
                           Q3 = quantile(salary,.75,na.rm=T), 
                           min=min(salary,na.rm=T), 
                           max=max(salary,na.rm=T))

summary_birth$yearID <- as.numeric(as.character(summary_birth$yearID))
head(summary_birth)


# Adding in the inflation factor
names(inflation_index)[1] <- "yearID"
inflation_index$yearID <- as.numeric(as.character(inflation_index$yearID))
head(inflation_index)

summary_birth_left <- left_join(summary_birth,inflation_index, by = "yearID")
summary_birth_left[summary_birth_left$yearID==2015,"inflation2015"]<-1
tail(summary_birth_left)

summary_birth <- mutate(summary_birth_left, 
                         median_inflation_adjusted = median*inflation2015,
                         Q1_inflation_adjusted = Q1*inflation2015, 
                         Q3_inflation_adjusted = Q3*inflation2015, 
                         min_inflation_adjusted = min*inflation2015,
                         max_inflation_adjusted = max*inflation2015)
head(summary_birth)

## 5.2 Graph Creation 

ggplot(summary_birth)+
  geom_ribbon(aes(x=yearID, ymin=Q1_inflation_adjusted, 
                  ymax=Q3_inflation_adjusted,fill=birth_place ),alpha = 0.4)+
  geom_line(aes(x=yearID, y=median_inflation_adjusted, color= birth_place ),size=1.2) +
  scale_fill_discrete(name = "Middle 50% of Earners")+
  scale_color_discrete(name = "Median Salary") + 
  scale_y_continuous(labels = scales::dollar)+
  labs(y="Annual Salary  \n (Adjusted for Inflation)", 
       x="Year",
       title="Salaries of Middle 50% of Earners in Major League Baseball") +
  theme_minimal()
