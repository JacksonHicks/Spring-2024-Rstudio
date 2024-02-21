97+65*(84-21/7)
x <- 8
x
x + 3
x * 10
4x #This is an error - you need an *
xx   #This is an error - xx does not exist
X      #This is an error - R is 100% case-sensitive
x <- 2*x + 6
grades <- c(94, 83, 99, 72, 91, 100, 80, 67, 78, 90, 92)
grades
mean(grades)
median(grades)
min(grades)
sd(grades) #This function is a sample SD.
IQR(grades)

#Wanna see a really cool function?
summary(grades)

hist(grades)

#To import a dataset, please install the readxl package
library(readxl)
med <- read_excel("C:/Users/mfranke1/Downloads/Medical Insurance Charge.xlsx")
View(med)

#Summary of bmi variable in med dataset:
summary(med$bmi)
summary(med[,5])

#Freq. Table for Sex
table(med$sex)

#Relative Freq. Table for Sex
prop.table(table(med$sex))
round(prop.table(table(med$sex)),digits=3)

#Interesting rounding:
round(summary(med$charges),digits=-3)

##############################

hist(med$age, main="Figure 1: Histogram of AGE",
     xlab="Age of Patient", ylim=c(0,200), xlim=c(10,70),
     col=c("olivedrab","orangered","plum"))

boxplot(med$age, horizontal = T)

table(med$life_stage)

#To change the order of categories:

anything <- ordered(med$life_stage, 
                    c("Young Adult","Adult","Middle Age Adult","Senior"))

table(anything)
prop.table(table(anything))

barplot(table(med$charges))
barplot(prop.table(table(anything)))

#The 4 contingency table:
#counts
table(anything, med$region)

#proportion of total
prop.table(table(anything, med$region))
round(prop.table(table(anything, med$region)),digits=3)

#proportion of row
prop.table(table(mde$age, med$region),1)
round(prop.table(table(med$age, med$region),1),digits=3)

#proportion of column
prop.table(table(anything, med$region),2)
round(prop.table(table(anything, med$region),2),digits=3)

###
A <- (table(anything, med$region))
A <- addmargins(A, c(1,2),sum)
A
 
########################

#Using ggplot
library(ggplot2)
library(ggthemes)

ggplot(med, aes(x=charges)) +
  geom_histogram(binwidth=5000, center=5000/2, 
                 color="white", fill="gray8") +
  labs(title = "Figure 3: Histogram of Medical Charges",
       x = "Medical Charges", y = "Number of Patients") +
  theme_base()

ggplot(med, aes(x=age)) +
  geom_histogram(binwidth=5, center=5/2, 
                 color="red", fill="green") +
  labs(title = "Figure 4: Histogram of Age",
       x = "Age of Patients", y = "Number of Patients") +
  theme_base()

#Sample Standard deviation
sd(med$age)

#How to find the standard deviation of a population:
sd_pop <- function(x,d){
  n <- length(x)
  print(round(sd(x)*sqrt(n-1)/sqrt(n),digits=d))
  rm(n)
}
sd_pop(med$age,2)
sd_pop(grades,3)
sd(grades)

#Making a categorical variable from a numerical one:

summary(med$charges)

#Because 7000 is between Q1 and med - first cutoff
#Because 14000 is between med and Q3 - second cutoff

#3 categories:  Under 7000, 7000-14000, over 14000

med$crgcat <- ifelse(med$charges<7000, "Group 1", 
                     ifelse(med$charges<=14000, "Group 2",
                            "Group 3"))

#There will always be one fewer ifelse than the number of
#groups created

table(med$crgcat)

################################

library(ggthemes)
library(ggplot2)

ggplot(med, aes(x=sex)) +
  geom_bar(fill=c("goldenrod","seagreen")) +
  ggtitle("figure 12:...") +
  theme_calc()

ggplot(med, aes(x=age)) +
  geom_bar(fill="hotpink") +
  ggtitle("Figure 247.98: Bar Chart of LifeStage Variable") +
  labs(x="Life Stage", y="Number of Patients") +
  theme_economist()
#This is barplot - not ggplot
barplot(table(anything))

table(anything)
frequencies <- as.data.frame(table(anything))
View(frequencies)
names(frequencies) <- c("age_group","count")
View(frequencies)

ggplot(frequencies, aes(x=age_group,y=count)) +
  geom_bar(stat="identity",fill="hotpink") +
  ggtitle("Figure 247.98: Bar Chart of LifeStage Variable") +
  labs(x="Age Group", y="Number of Patients") + 
  geom_text(aes(label=count),vjust=2,size=5) +
  theme_classic()

#Pie
ggplot(frequency, aes(x="", y=count, 
                        fill=factor(age_group))) +
  geom_bar(width=1, stat="identity") +
  geom_text(aes(label=paste(round(count/sum(count)*100,
                                  1), "%")), position=position_stack(vjust=0.5)) +
  theme_classic() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) + 
  labs(fill = "Age Group", x=NULL, y=NULL, 
       title = "Figure 11.91...") + 
  scale_fill_manual(values = 
                      c("lightblue","tomato1","goldenrod","darkorchid")) +
  coord_polar("y")

#Stratified Analysis

aggregate(med$charges~med$smoker,FUN=mean)
aggregate(med$charges~med$smoker,FUN=summary)

#############################

#Stacked Bar Chart
counts <- table(anything, med$smoker)
counts_df  <- as.data.frame(counts) 
names(counts_df) <- c("Age_Group","Smoke","Count") 

ggplot(data = counts_df, aes(x = Age_Group, y = Count, 
                             fill = Smoke, label = Count)) +
  geom_bar(stat = "identity",show.legend = TRUE) +
  geom_text(size = 4.5, position = position_stack(vjust = 0.25))  +
  theme_stata() + 
  theme(legend.position = "bottom") + 
  scale_fill_manual(values = c("lightblue","tomato1","goldenrod","darkorchid")) +
  ggtitle("Figure A: Count of Age Group by Smoking") +
  labs(subtitle = "Showing the Beauty of GGPLOT")

#Switch

ggplot(data = counts_df, aes(fill = Age_Group, y = Count, 
                             x = Smoke, label = Count)) +
  geom_bar(stat = "identity",show.legend = TRUE) +
  geom_text(size = 4.5, position = position_stack(vjust = 0.25))  +
  theme_stata() + 
  theme(legend.position = "bottom") + 
  scale_fill_manual(values = c("lightblue","tomato1","goldenrod","darkorchid")) +
  ggtitle("Figure A: Count of Smoking by Age Group") +
  labs(subtitle = "Showing the Beauty of GGPLOT")

#100% Stacked Bar Chart
counts <- round(prop.table(table(anything, med$smoker),1), digits=3)
counts_df  <- as.data.frame(counts) 
names(counts_df) <- c("Age_Group","Smoke","Proportion")

ggplot(data = counts_df, aes(x = Age_Group, y = Proportion, 
                             fill = Smoke, label = Proportion)) +
  geom_bar(stat = "identity",show.legend = TRUE) +
  geom_text(size = 5, position = position_stack(vjust = 0.2))  +
  theme_stata() + 
  theme(legend.position = "bottom") + 
  scale_fill_manual(values = c("lightblue","tomato1","goldenrod","darkorchid")) +
  ggtitle("Figure B: Proportion of Age Group by Smoking") +
  labs(subtitle = "Showing the Beauty of GGPLOT")

#Switch
counts <- round(prop.table(table(anything, med$smoker),2), digits=7)
counts_df  <- as.data.frame(counts) 
names(counts_df) <- c("Age_Group","Smoke","Proportion")

ggplot(data = counts_df, aes(fill = Age_Group, y = Proportion, 
                             x = Smoke, label = Proportion)) +
  geom_bar(stat = "identity",show.legend = TRUE) +
  geom_text(size = 5, position = position_stack(vjust = 0.2))  +
  theme_stata() + 
  theme(legend.position = "bottom") + 
  scale_fill_manual(values = c("lightblue","tomato1","goldenrod","darkorchid")) +
  ggtitle("Figure B: Proportion of Age Group by Smoking") +
  labs(subtitle = "Showing the Beauty of GGPLOT")

#Scatterplots:

plot(med$age,med$charges)
abline(lm(med$charges~med$age))

cor(med$age,med$charges)

boxplot(med$age,horizontal=T)

#Side-by-side Boxplots

boxplot(med$age~med$smoker,horizontal=T,
        col=c("red","yellow"))

#What is the distribution for age in the NW region?

summary(med$age[med$region=="northwest"])
length(med$age[med$region=="northwest"])

#Exclude the northeast:

summary(med$age[med$region!="northeast"])
length(med$age[med$region!="northeast"])

#  The | symbol stands for or
#  The & symbol stands for and

summary(med$age[med$region!="northeast" & med$region!="southeast"])
summary(med$age[med$region=="northeast" | med$region=="southeast"])

hist(med$age[med$region == "southwest" & med$sex == "male"])

NWmed <- med[med$region=="northwest",]
YAmed <- med[med$life_stage=="Young Adult",]
Cmed <- med[med$children>2,]
summary(Cmed$children)

Cmed$life_stage[Cmed$life_stage=="Young Adult"] <- "YA"
Cmed$life_stage[Cmed$life_stage=="YA"] <- "5"
Cmed$life_stage[Cmed$life_stage==5] <- "YA"


# Load ggplot2
library(ggplot2)

# Create Data
data <- data.frame(
  group=frequency[1:2],
  value=c(24,17)
)

# Basic piechart
ggplot(data, aes(x="", y=value, fill=group)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+
  labs(fill = "AGES", x=NULL, y=NULL, 
       title = "Ages of Baseball Players") #REMEBER TO COME BACK HERE LATER TO FIGURE OUT HOW TO CHNAGE THE LEGEND TO FIT THE CATEGORIES

