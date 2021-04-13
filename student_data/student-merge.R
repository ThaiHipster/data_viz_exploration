library(ggplot2)
library(tidyverse)


################################################################################
####                     1. Reading In Data                                #####
################################################################################

#### 1.1 Loading in Data

# 1.1.a Loading math and portugese sets seperately
mat=read.table("student-mat.csv",sep=";",header=TRUE)
por=read.table("student-por.csv",sep=";",header=TRUE)

# 1.1.b Creating Merged Dataset
combo=merge(mat,por,by=c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet"))
print(head(combo))

#### 1.2 Variable Exploration: 
mat_small <- mat[,c("studytime","activities","goout","romantic","romantic",
                    "absences","failures","Dalc","Walc","age","G3")]
mat_small$studytime <- as.factor(mat_small$studytime)

por_small <- por[,c("studytime","activities","goout","freetime","romantic",
                    "absences","failures","Dalc","Walc","age","G3")]
por_small$studytime <- as.factor(por_small$studytime)

str(por_small)
str(mat_small)

#### 1.3 Math Scores Mean Translation
mat_studytime_1 <- filter(mat_small, studytime == 1)
mat_studytime_2 <- filter(mat_small, studytime == 2)
mat_studytime_3 <- filter(mat_small, studytime == 3)
mat_studytime_4 <- filter(mat_small, studytime == 4)

# 1.3.a Dataset: Study Time, Number of Instance, Mean Test Scores
mat_mean <- count(mat_small,studytime)
mat_mean$mean[1] <- mean(mat_studytime_1$G3)
mat_mean$mean[2] <- mean(mat_studytime_2$G3)
mat_mean$mean[3] <- mean(mat_studytime_3$G3)
mat_mean$mean[4] <- mean(mat_studytime_4$G3)

mat_mean_2 = aggregate(mat_small,
                by = list(mat_small$studytime),
                FUN = mean)

#### 1.4 Portugese Scores Mean Translation
por_studytime_1 <- filter(por_small, studytime == 1)
por_studytime_2 <- filter(por_small, studytime == 2)
por_studytime_3 <- filter(por_small, studytime == 3)
por_studytime_4 <- filter(por_small, studytime == 4)


por_mean <- count(por_small,studytime)
por_mean$mean[1] <- mean(por_studytime_1$G3)
por_mean$mean[2] <- mean(por_studytime_2$G3)
por_mean$mean[3] <- mean(por_studytime_3$G3)
por_mean$mean[4] <- mean(por_studytime_4$G3)

################################################################################
####                     2. Trial Graphs                                   #####
################################################################################

#### 2.1 Exploration Bar ------------------------------------------------------
ggplot(mat_small)+
  geom_bar(aes(x=studytime, fill=studytime)) # computed variable: count

#### 2.2 Exploration Math Mean  -----------------------------------------------
ggplot(data=mat_mean) +
  geom_segment(aes(x=studytime, xend=studytime,y=0,yend=mean, color=studytime))+
  geom_point(aes(x=studytime,y=mean,color=studytime),size=2)+
  coord_flip()

#### 2.3 Exploration Portuguese Mean
ggplot(data=por_mean) +
  geom_segment(aes(x=studytime, xend=studytime,y=0,yend=mean, color=studytime))+
  geom_point(aes(x=studytime,y=mean,color=studytime),size=2)+
  coord_flip()



################################################################################
####                     3. Edited Datasets                                #####
################################################################################

#### 3.0 Summary -------------------------------------------------------------
str(por_small)
str(mat_small)


#### 3.1 Activities ---------------------------------------------------------

#Selecting the different data for activities, grades, and study time
str(mat_small)
mat_act <- mat_small[,c(1,2,11)] 
por_act <- por_small[,c(1,2,11)]

mat_act_mean = aggregate(mat_act,
                         by = list(mat_act$studytime,mat_act$activities),
                         FUN = mean)
mat_act_mean <- mat_act_mean[,c(-3,-4)]
names(mat_act_mean) <- c("studytime","activites","math_grade")

por_act_mean = aggregate(por_act,
                by = list(por_act$studytime,por_act$activities),
                FUN = mean)
por_act_mean <- por_act_mean[,c("Group.1","Group.2","G3")]
names(por_act_mean) <- c("studytime","activities","portugese_grade")

#### 3.2 Go Out ?  ---------------------------------------------------------
str(mat_small)
mat_goout <- mat_small[,c(1,3,11)]
por_goout <- por_small[,c(1,3,11)]

mat_goout_mean = aggregate(mat_goout,
                         by = list(mat_goout$studytime,mat_goout$goout),
                         FUN = mean)
mat_goout_mean <- mat_goout_mean[,c(-3,-4)]
names(mat_goout_mean) <- c("studytime","goout","math_grade")

por_goout_mean = aggregate(por_goout,
                         by = list(por_goout$studytime,por_goout$goout),
                         FUN = mean)
por_goout_mean <- por_goout_mean[,c("Group.1","Group.2","G3")]
names(por_goout_mean) <- c("studytime","goout","portugese_grade")
por_goout_mean$goout <- as.factor(por_goout_mean$goout)
print(por_goout_mean)
str(por_goout_mean)

#### 3.3 Freetime  ---------------------------------------------------------

str(mat_small)
mat_freetime <- mat_small[,c(1,4,11)]
por_freetime <- por_small[,c(1,4,11)]

mat_freetime_mean = aggregate(mat_freetime,
                           by = list(mat_freetime$studytime,mat_freetime$freetime),
                           FUN = mean)
mat_freetime_mean <- mat_freetime_mean[,c(-3,-4)]
names(mat_freetime_mean) <- c("studytime","freetime","math_grade")

por_freetime_mean = aggregate(por_freetime,
                           by = list(por_freetime$studytime,por_freetime$freetime),
                           FUN = mean)
por_freetime_mean <- por_freetime_mean[,c("Group.1","Group.2","G3")]
names(por_freetime_mean) <- c("studytime","freetime","portugese_grade")
print(por_freetime_mean)
por_freetime_mean$freetime <- as.factor(por_freetime_mean$freetime)

#### 3.4 Romantic  ---------------------------------------------------------

str(mat_small)
mat_romantic <- mat_small[,c(1,5,11)]
por_romantic <- por_small[,c(1,5,11)]

mat_romantic_mean = aggregate(mat_romantic,
                              by = list(mat_romantic$studytime,mat_romantic$romantic),
                              FUN = mean)
mat_romantic_mean <- mat_romantic_mean[,c(-3,-4)]
names(mat_romantic_mean) <- c("studytime","romantic","math_grade")

por_romantic_mean = aggregate(por_romantic,
                              by = list(por_romantic$studytime,por_romantic$romantic),
                              FUN = mean)
por_romantic_mean <- por_romantic_mean[,c("Group.1","Group.2","G3")]
names(por_romantic_mean) <- c("studytime","romantic","portugese_grade")
print(por_romantic_mean)
#### 3.5 Absences  ---------------------------------------------------------

low_absence<- c(0:10)
mid_absence <- c(10:30)
hi_absence <- c(30:75)

str(mat_small)

mat_absence <- mat_small[,c(1,6,11)]
por_absence <- por_small[,c(1,6,11)]

mat_absence <- mutate(mat_absence, abs_level=ifelse(absences  %in%  low_absence, "low_absence",
                                             ifelse(absences  %in%  mid_absence, "mid_absence",
                                             ifelse(absences  %in%  hi_absence, "hi_absence","NA"))))

por_absence <- mutate(por_absence, abs_level=ifelse(absences  %in% low_absence, "low_absence",
                                             ifelse(absences  %in% mid_absence, "mid_absence",
                                              ifelse(absences %in% hi_absence, "hi_absence","NA"))))

mat_absence_mean = aggregate(mat_absence,
                              by = list(mat_absence$studytime,mat_absence$abs_level),
                              FUN = mean)

mat_absence_mean <- mat_absence_mean[,c(-3,-4)]
names(mat_absence_mean) <- c("studytime","absence","math_grade")

por_absence_mean = aggregate(por_absence,
                              by = list(por_absence$studytime,por_absence$abs_level),
                              FUN = mean)
por_absence_mean <- por_absence_mean[,c("Group.1","Group.2","G3")]
names(por_absence_mean) <- c("studytime","absence","portugese_grade")


#### 3.6 Failures  ---------------------------------------------------------
str(mat_small)

mat_failures <- mat_small[,c(1,7,11)]
por_failures <- por_small[,c(1,7,11)]

mat_failures_mean = aggregate(mat_failures,
                              by = list(mat_failures$studytime,mat_failures$failures),
                              FUN = mean)
mat_failures_mean <- mat_failures_mean[,c(-3,-4)]
names(mat_failures_mean) <- c("studytime","failures","math_grade")

por_failures_mean = aggregate(por_failures,
                              by = list(por_failures$studytime,por_failures$failures),
                              FUN = mean)
por_failures_mean <- por_failures_mean[,c("Group.1","Group.2","G3")]
names(por_failures_mean) <- c("studytime","failures","portugese_grade")
por_failures_mean$failures <- as.factor(por_failures_mean$failures)

#### 3.7 Alcohol  ---------------------------------------------------------
low_alc<- c("2","3")
mid_alc <- c("4", "5", "6")
hi_alc <- c("7", "8", "9", "10")

str(mat_small)

mat_alcohol <- mat_small
mat_alcohol$alc <- mat_alcohol$Dalc+mat_alcohol$Walc
mat_alc <- mat_alcohol[,c(1,11,12)]

por_alcohol <- por_small
por_alcohol$alc <- por_alcohol$Dalc+por_alcohol$Walc
por_alc <- por_alcohol[,c(1,11,12)]

mat_alc <- mutate(mat_alc, alc_level=ifelse(alc %in% low_alc, "low_alc",
                                                 ifelse(alc %in% mid_alc, "mid_alc",
                                                        ifelse(alc %in% hi_alc, "hi_alc","NA"))))

por_alc <- mutate(por_alc, alc_level=ifelse(alc %in% low_alc, "low_alc",
                                            ifelse(alc %in% mid_alc, "mid_alc",
                                                   ifelse(alc %in% hi_alc, "hi_alc","NA"))))


mat_alc_mean = aggregate(mat_alc,
                              by = list(mat_alc$studytime,mat_alc$alc_level),
                              FUN = mean)
mat_alc_mean <- mat_alc_mean[,c(-4,-5)]
names(mat_alc_mean) <- c("studytime","alc_level","math_grade")

por_alc_mean = aggregate(por_alc,
                              by = list(por_alc$studytime,por_alc$alc_level),
                              FUN = mean)
por_alc_mean <- por_alc_mean[,c("Group.1","Group.2","G3")]
names(por_alc_mean) <- c("studytime","alc_level","portugese_grade")
print(por_alc_mean)



################################################################################
####                     4. Aggregate Plots                                #####
################################################################################

#### 4.1 Activities  ---------------------------------------------------------
por_act_mean

ggplot(data=por_act_mean, aes(x=studytime, y=portugese_grade, group=activities)) +
  geom_segment(aes(x=studytime, xend=studytime,y=0,yend=portugese_grade))+
  geom_point(aes(x=studytime,y=portugese_grade, shape = activities, color=activities, fill=activities ),size=3)+
  labs(x="Studytime in Hours",y="Mean Grades",
       title="Mean Grades based on Study Time Based on Activities") +
  coord_flip()

#### 4.2 Go Out  ---------------------------------------------------------
por_goout_mean

ggplot(data=por_goout_mean) +
  geom_segment(aes(x=studytime, xend=studytime,y=0,yend=portugese_grade))+
  geom_point(aes(x=studytime,y=portugese_grade,shape=goout, color=goout, fill=goout ), size=3)+
  labs(x="Studytime in Hours",y="Mean Grades",
       title="Mean Grades based on Study Time based Going Out Levels")+
  coord_flip() +
  theme_classic()

#### 4.3 Freetime # this needs work---------------------------------------
por_freetime_mean

ggplot(data=por_freetime_mean) +
  geom_segment(aes(x=studytime, xend=studytime,y=0,yend=portugese_grade))+
  geom_point(aes(x=studytime,y=portugese_grade,shape=freetime, color=freetime, fill=freetime ),size=3)+
  labs(x="Studytime in Hours",y="Mean Grades",
       title="Mean Grades based on Study Time with Freetime")+
  coord_flip() +
  theme_classic()

#### 4.4 Romantic  ---------------------------------------------------------
por_romantic_mean

ggplot(data=por_romantic_mean) +
  geom_segment(aes(x=studytime, xend=studytime,y=0,yend=portugese_grade))+
  geom_point(aes(x=studytime,y=portugese_grade,shape = romantic, color=romantic, fill=romantic ),size=4)+
  scale_color_manual(values = c("#ff93c9", "#b0a2ff"))+
  labs(x="Studytime in Hours",y="Mean Grades",
       title="Mean Grades based on Study Time with Absence")+
  coord_flip() +
  theme_classic()

#### 4.5 Absences  ---------------------------------------------------------
por_absence_mean

ggplot(data=por_absence_mean) +
  geom_segment(aes(x=studytime, xend=studytime,y=0,yend=portugese_grade))+
  geom_point(aes(x=studytime,y=portugese_grade,shape = absence, color=absence, fill=absence ),size=3)+
  scale_color_manual(values = c("#ff9c08", "#ff18aa","#30148c"))+
  labs(x="Studytime in Hours",y="Mean Grades",
       title="Mean Grades based on Study Time with Absences")+
  coord_flip() +
  theme_classic()

#### 4.6 Failures ### Keep working on this  -------------------------------
print(por_failures_mean)

ggplot(data=por_failures_mean) +
  geom_segment(aes(x=studytime, xend=studytime,y=0,yend=portugese_grade))+
  geom_point(aes(x=studytime,y=portugese_grade,shape=failures, color=failures, fill=failures),size=3)+
  labs(x="Studytime in Hours",y="Mean Grades",
       title="Mean Grades based on Study Time with Failures")+
  coord_flip() +
  theme_classic()

#### 4.7 Alcohol  ---------------------------------------------------------
print(por_alc_mean)

ggplot(data=por_alc_mean) +
  geom_segment(aes(x=studytime, xend=studytime,y=0,yend=portugese_grade))+
  geom_point(aes(x=studytime,y=portugese_grade,shape=alc_level, color=alc_level, fill=alc_level),size=3)+
  labs(x="Studytime in Hours",y="Mean Grades",
       title="Mean Grades based on Study Time with Alcohol Consumption")+
  coord_flip() +
  theme_classic()

################################################################################
####                     5. Aggregate  Plots                               #####
################################################################################

ggplot(mat_small)+
  geom_bar(aes(x=studytime, fill=studytime))

ggplot(mat_small)+
  geom_bar(aes(x=studytime, fill=studytime))+ 
  scale_fill_manual(values = c("#00AFBB", "#c1ffbd", "#ff856f","#ff93c9"))

por_small
str(mat_small)

#4.1 Activities  ------------------------------------------------------------
mat_act_yes <- filter(mat_small, activities == "yes")
mat_act_no  <- filter(mat_small, activities == "no")

#4.2 Go Out   --------------------------------------------------------------
por_goout_mean


mat_goout_1 <- filter(mat_small, goout == 1)
mat_goout_2 <- filter(mat_small, goout == 2) 
mat_goout_3 <- filter(mat_small, goout == 3) 
mat_goout_4 <- filter(mat_small, goout == 4) 
mat_goout_5 <- filter(mat_small, goout == 5) 

ggplot(mat_goout_5)+
  geom_bar(aes(x=studytime, fill=studytime))


