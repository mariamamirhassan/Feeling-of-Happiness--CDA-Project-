library(readxl)
library(tidyverse)
library(dplyr)
library(vcd)
library(vcdExtra)
library(nnet)
library(knitr)
library(nnet)
library(coin)
library(kableExtra)
library(formattable)
library(gamlss)
library(MESS)

#install.packages("gamlss")
#install.packages("kableExtra")
#install.packages("coin")


#Read the data , reduce the names of variables
data <- read_excel("data.xlsx",.name_repair = ~ str_sub(.x,start = 1,end = 7)) %>% select(starts_with("Q"))
sum(is.na(data))



#Renaming Variables, and extracting them
data_subset= data %>%
  rename(happy=`Q46: Fe`,
         comp= `Q109: C`,
         sex=`Q260: S`,
         age=`Q262: A`,
         edu=`Q275: H`,
         employ=`Q279: E`,
         rel_per=`Q173: R`,
         rel_den=`Q289: R`,
         social_class=`Q287: S`,
         people_trust=`Q57: Mo`) %>%
  select(happy,comp,sex,age,edu,employ,rel_per,rel_den,social_class,people_trust)
  
colnames(data_subset)


#write.csv(data_subset, file = "data_subset.csv", row.names = FALSE)
data2=read.csv("data_subset.csv")

attach(data2)
#############################################################################
#Sex and Happiness
data2$sex=as.factor(sex)
data2$happy=as.factor(happy)
levels(data2$happy)


# create contingency tables of 2-way table
table_1=table(sex,happy)
contingency_table_with_totals_1 <- addmargins(table_1, margin = 1:2)

#Good format table 
# Create a basic table
basic_table <- kable(contingency_table_with_totals_1, "html", align = "c", caption = "Table (1): Two-Way Contingency Table for Happiness & Sex") %>%
  kable_styling(full_width = FALSE)

# Add alternating row colors and header formatting
styled_table <- basic_table %>%
  row_spec(0, bold = TRUE) %>%
  row_spec(1:nrow(contingency_table_with_totals_1), background = ifelse(row(contingency_table_with_totals_1) %% 2 == 0, "#f7f7f7", "white")) %>%
  column_spec(1, bold = TRUE)

styled_table

#Conditional table
table1_prop <- prop.table(table_1) 
prob_table_with_totals_1 <- addmargins(prop.table(table_1), margin = 1:2)

# Create a basic table
basic_table <- kable(prob_table_with_totals_1, "html", align = "c", caption = "Table (2): Two-Way Contingency (proportions) Table for Happiness & Sex") %>%
  kable_styling(full_width = FALSE)

# Add alternating row colors and header formatting
styled_table <- basic_table %>%
  row_spec(0, bold = TRUE) %>%
  row_spec(1:nrow(prob_table_with_totals_1), background = ifelse(row(prob_table_with_totals_1) %% 2 == 0, "#f7f7f7", "white")) %>%
  column_spec(1, bold = TRUE)

styled_table

#Tests
gkgamma(table_1)
miu4<-chisq.test(table_1);GKgamma(table_1);miu4$expected

##########################################################################
#Age and Happiness
data2$age[data2$age<= 29] <- 1L
data2$age[data2$age>=30 &data2$age<=49] <- 2L
data2$age[data2$age>=50] <- 3L
data2$age <- as.factor(data2$age)
levels(data2$age)
levels(data2$age)=c("Less Than 29", "30-49","Greater Than 50")

attach(data2)
# create contingency tables of 2-way table
table_2=table(age,happy)
contingency_table_with_totals_2 <- addmargins(table_2, margin = 1:2)

#Good format table 
# Create a basic table
basic_table <- kable(contingency_table_with_totals_2, "html", align = "c", caption = "Table (1): Two-Way Contingency Table for Happiness & Age") %>%
  kable_styling(full_width = FALSE)

# Add alternating row colors and header formatting
styled_table <- basic_table %>%
  row_spec(0, bold = TRUE) %>%
  row_spec(1:nrow(contingency_table_with_totals_2), background = ifelse(row(contingency_table_with_totals_2) %% 2 == 0, "#f7f7f7", "white")) %>%
  column_spec(1, bold = TRUE)

styled_table

#Conditional table
table1_prop <- prop.table(table_2) 
prob_table_with_totals_2 <- addmargins(prop.table(table_2), margin = 1:2)

# Create a basic table
basic_table <- kable(prob_table_with_totals_2, "html", align = "c", caption = "Table (2): Two-Way Contingency (proportions) Table for Happiness & Sex") %>%
  kable_styling(full_width = FALSE)

# Add alternating row colors and header formatting
styled_table <- basic_table %>%
  row_spec(0, bold = TRUE) %>%
  row_spec(1:nrow(prob_table_with_totals_2), background = ifelse(row(prob_table_with_totals_2) %% 2 == 0, "#f7f7f7", "white")) %>%
  column_spec(1, bold = TRUE)

styled_table

#Tests
gkgamma(table_2)
miu4<-chisq.test(table_2);GKgamma(table_2);miu4$expected

##################################################################################

#Compettion and Happiness
data2$comp[data2$comp == "Competition is good"]=1
data2$comp[data2$comp == "Competition is harmful"]=10
data2$comp <- as.factor(data2$comp)

# Assume 'data2$comp' is your factor column
data2$comp <- factor(data2$comp, levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "Don't know"))

levels(data2$comp)



attach(data2)

#create contingency tables of 2-way table
table_3=table(comp,happy)
contingency_table_with_totals_3 <- addmargins(table_3, margin = 1:2)

#Good format table 
# Create a basic table
basic_table <- kable(contingency_table_with_totals_3, "html", align = "c", caption = "Table (1): Two-Way Contingency Table for Happiness & Competition") %>%
  kable_styling(full_width = FALSE)

# Add alternating row colors and header formatting
styled_table <- basic_table %>%
  row_spec(0, bold = TRUE) %>%
  row_spec(1:nrow(contingency_table_with_totals_3), background = ifelse(row(contingency_table_with_totals_3) %% 2 == 0, "#f7f7f7", "white")) %>%
  column_spec(1, bold = TRUE)

styled_table

#Conditional table
table1_prop <- prop.table(table_3) 
prob_table_with_totals_3 <- addmargins(prop.table(table_3), margin = 1:2)

# Create a basic table
basic_table <- kable(prob_table_with_totals_3, "html", align = "c", caption = "Table (2): Two-Way Contingency (proportions) Table for Happiness & Competition") %>%
  kable_styling(full_width = FALSE)

# Add alternating row colors and header formatting
styled_table <- basic_table %>%
  row_spec(0, bold = TRUE) %>%
  row_spec(1:nrow(prob_table_with_totals_3), background = ifelse(row(prob_table_with_totals_3) %% 2 == 0, "#f7f7f7", "white")) %>%
  column_spec(1, bold = TRUE)

styled_table

#Tests
gkgamma(table_3)
miu4<-chisq.test(table_3);GKgamma(table_3);miu4$expected

############################################################################################

#Education  and Happiness

data2$edu <- as.factor(data2$edu)

# Assume 'data2$comp' is your factor column
data2$comp <- factor(data2$comp, levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "Don't know"))

levels(data2$edu)

data2$edu <- factor(data2$edu, levels = c(
  "Early childhood education (ISCED 0) / no education",
  "Primary education (ISCED 1)",
  "Lower secondary education (ISCED 2)",
  "Upper secondary education (ISCED 3)",
  "Post-secondary non-tertiary education (ISCED 4)",
  "Short-cycle tertiary education (ISCED 5)",
  "Bachelor or equivalent (ISCED 6)",
  "Master or equivalent (ISCED 7)",
  "Doctoral or equivalent (ISCED 8)"
))


attach(data2)

#create contingency tables of 2-way table
table_4=table(edu,happy)
contingency_table_with_totals_4 <- addmargins(table_4, margin = 1:2)

#Good format table 
# Create a basic table
basic_table <- kable(contingency_table_with_totals_4, "html", align = "c", caption = "Table (1): Two-Way Contingency Table for Happiness & Education") %>%
  kable_styling(full_width = FALSE)

# Add alternating row colors and header formatting
styled_table <- basic_table %>%
  row_spec(0, bold = TRUE) %>%
  row_spec(1:nrow(contingency_table_with_totals_4), background = ifelse(row(contingency_table_with_totals_4) %% 2 == 0, "#f7f7f7", "white")) %>%
  column_spec(1, bold = TRUE)

styled_table

#Conditional table
table1_prop <- prop.table(table_4) 
prob_table_with_totals_4 <- addmargins(prop.table(table_4), margin = 1:2)

# Create a basic table
basic_table <- kable(prob_table_with_totals_4, "html", align = "c", caption = "Table (2): Two-Way Contingency (proportions) Table for Happiness & Education") %>%
  kable_styling(full_width = FALSE)

# Add alternating row colors and header formatting
styled_table <- basic_table %>%
  row_spec(0, bold = TRUE) %>%
  row_spec(1:nrow(prob_table_with_totals_4), background = ifelse(row(prob_table_with_totals_4) %% 2 == 0, "#f7f7f7", "white")) %>%
  column_spec(1, bold = TRUE)

styled_table

#Tests
gkgamma(table_4)
miu4<-chisq.test(table_3);GKgamma(table_4);miu4$expected

##################################################################################################

#Social Class and Happiness

data2$happy=as.factor(happy)
levels(data2$happy)



data2$social_class <- as.factor(data2$social_class)


levels(data2$social_class)

data2$social_class <- factor(data2$social_class, levels = c(
  "Don't know", "Lower class", "Lower middle class",
  "Working class","Upper middle class" ,"Upper class"
))



levels(data2$social_class)


attach(data2)

#create contingency tables of 2-way table
table_5=table(social_class,happy)
contingency_table_with_totals_5 <- addmargins(table_5, margin = 1:2)

#Good format table 
# Create a basic table
basic_table <- kable(contingency_table_with_totals_5, "html", align = "c", caption = "Table (1): Two-Way Contingency Table for Happiness & Social Class") %>%
  kable_styling(full_width = FALSE)

# Add alternating row colors and header formatting
styled_table <- basic_table %>%
  row_spec(0, bold = TRUE) %>%
  row_spec(1:nrow(contingency_table_with_totals_5), background = ifelse(row(contingency_table_with_totals_5) %% 2 == 0, "#f7f7f7", "white")) %>%
  column_spec(1, bold = TRUE)

styled_table

#Conditional table
table1_prop <- prop.table(table_5) 
prob_table_with_totals_5 <- addmargins(prop.table(table_5), margin = 1:2)

# Create a basic table
basic_table <- kable(prob_table_with_totals_5, "html", align = "c", caption = "Table (2): Two-Way Contingency (proportions) Table for Happiness & Social Class") %>%
  kable_styling(full_width = FALSE)

# Add alternating row colors and header formatting
styled_table <- basic_table %>%
  row_spec(0, bold = TRUE) %>%
  row_spec(1:nrow(prob_table_with_totals_5), background = ifelse(row(prob_table_with_totals_5) %% 2 == 0, "#f7f7f7", "white")) %>%
  column_spec(1, bold = TRUE)

styled_table

#Tests
gkgamma(table_5)
miu4<-chisq.test(table_5);GKgamma(table_5);miu4$expected

####################################################################################3333
#Rel den and Happiness

data2$happy=as.factor(happy)
levels(data2$happy)



data2$rel_den <- as.factor(data2$rel_den)


levels(data2$rel_den)


attach(data2)

#create contingency tables of 2-way table
table_6=table(rel_den,happy)
contingency_table_with_totals_6 <- addmargins(table_6, margin = 1:2)

#Good format table 
# Create a basic table
basic_table <- kable(contingency_table_with_totals_6, "html", align = "c", caption = "Table (1): Two-Way Contingency Table for Happiness & Religious denominations") %>%
  kable_styling(full_width = FALSE)

# Add alternating row colors and header formatting
styled_table <- basic_table %>%
  row_spec(0, bold = TRUE) %>%
  row_spec(1:nrow(contingency_table_with_totals_6), background = ifelse(row(contingency_table_with_totals_6) %% 2 == 0, "#f7f7f7", "white")) %>%
  column_spec(1, bold = TRUE)

styled_table

#Conditional table
table1_prop <- prop.table(table_6) 
prob_table_with_totals_6 <- addmargins(prop.table(table_6), margin = 1:2)

# Create a basic table
basic_table <- kable(prob_table_with_totals_6, "html", align = "c", caption = "Table (2): Two-Way Contingency (proportions) Table for Happiness & Religious denominations") %>%
  kable_styling(full_width = FALSE)

# Add alternating row colors and header formatting
styled_table <- basic_table %>%
  row_spec(0, bold = TRUE) %>%
  row_spec(1:nrow(prob_table_with_totals_6), background = ifelse(row(prob_table_with_totals_6) %% 2 == 0, "#f7f7f7", "white")) %>%
  column_spec(1, bold = TRUE)

styled_table

#Tests
gkgamma(table_6)
miu4<-chisq.test(table_6);GKgamma(table_6);miu4$expected
#####################################################################################3

#People trust and Happiness

data2$happy=as.factor(happy)
levels(data2$happy)



data2$people_trust <- as.factor(data2$people_trust)


levels(data2$people_trust)


attach(data2)

#create contingency tables of 2-way table
table_7=table(people_trust,happy)
contingency_table_with_totals_7 <- addmargins(table_7, margin = 1:2)

#Good format table 
# Create a basic table
basic_table <- kable(contingency_table_with_totals_7, "html", align = "c", caption = "Table (1): Two-Way Contingency Table for Happiness & People Trust") %>%
  kable_styling(full_width = FALSE)

# Add alternating row colors and header formatting
styled_table <- basic_table %>%
  row_spec(0, bold = TRUE) %>%
  row_spec(1:nrow(contingency_table_with_totals_7), background = ifelse(row(contingency_table_with_totals_7) %% 2 == 0, "#f7f7f7", "white")) %>%
  column_spec(1, bold = TRUE)

styled_table

#Conditional table
table1_prop <- prop.table(table_7) 
prob_table_with_totals_7 <- addmargins(prop.table(table_7), margin = 1:2)

# Create a basic table
basic_table <- kable(prob_table_with_totals_7, "html", align = "c", caption = "Table (2): Two-Way Contingency (proportions) Table for Happiness & People Trust") %>%
  kable_styling(full_width = FALSE)

# Add alternating row colors and header formatting
styled_table <- basic_table %>%
  row_spec(0, bold = TRUE) %>%
  row_spec(1:nrow(prob_table_with_totals_7), background = ifelse(row(prob_table_with_totals_7) %% 2 == 0, "#f7f7f7", "white")) %>%
  column_spec(1, bold = TRUE)

styled_table

#Tests
gkgamma(table_7)
miu4<-chisq.test(table_7);GKgamma(table_6);miu4$expected
miu4<-chisq.test(table_7)
#Monto Carlo
chisq.test(table_7, simulate.p.value=TRUE, B=10000)



###########################################################################################
#Rel per and Happiness

data2$happy=as.factor(happy)
levels(data2$happy)



data2$rel_per<- as.factor(data2$rel_per)


data2$rel_per <- factor(data2$rel_per, levels = c(
  "An atheist", "Not a religious person", "Don't know", "A religious person"
))
levels(data2$rel_per)


attach(data2)

#create contingency tables of 2-way table
table_8=table(rel_per,happy)
contingency_table_with_totals_8 <- addmargins(table_8, margin = 1:2)

#Good format table 
# Create a basic table
basic_table <- kable(contingency_table_with_totals_8, "html", align = "c", caption = "Table (1): Two-Way Contingency Table for Happiness & Religious person") %>%
  kable_styling(full_width = FALSE)

# Add alternating row colors and header formatting
styled_table <- basic_table %>%
  row_spec(0, bold = TRUE) %>%
  row_spec(1:nrow(contingency_table_with_totals_8), background = ifelse(row(contingency_table_with_totals_8) %% 2 == 0, "#f7f7f7", "white")) %>%
  column_spec(1, bold = TRUE)

styled_table

#Conditional table
table1_prop <- prop.table(table_8) 
prob_table_with_totals_8 <- addmargins(prop.table(table_8), margin = 1:2)
# Create a basic table
basic_table <- kable(prob_table_with_totals_8, "html", align = "c", caption = "Table (2): Two-Way Contingency (proportions) Table for Happiness & Religious person") %>%
  kable_styling(full_width = FALSE)

# Add alternating row colors and header formatting
styled_table <- basic_table %>%
  row_spec(0, bold = TRUE) %>%
  row_spec(1:nrow(prob_table_with_totals_8), background = ifelse(row(prob_table_with_totals_8) %% 2 == 0, "#f7f7f7", "white")) %>%
  column_spec(1, bold = TRUE)

styled_table

#Tests
gkgamma(table_8)
miu4<-chisq.test(table_8);GKgamma(table_6);miu4$expected
miu4<-chisq.test(table_8)
miu4

#Monto Carlo
chisq.test(table_8, simulate.p.value=TRUE, B=10000)


assocstats(table_8)
###########################################################################33
library(descr)
library(kableExtra)
library(infotheo)
#install.packages("samplesizeCMH")
library(samplesizeCMH)
#install.packages("stats")
library(stats)
three_way<-table(data$Q47_recode,data$Y, data$Q287_recode)
three_way
mantelhaen.test(three_way)
odds.ratio(three_way)

three_way_ftable<-ftable(three_way)
three_way_ftable
X2<-chisq.test(three_way_ftable)
X2$expected
install.packages("infotheo")
library(infotheo)

####################################33
three_way_table <- xtabs(~ age + social_class + happy, data = data2)
three_way_ftable <- ftable(three_way_table)

three_way_ftable

mantelhaen.test(three_way_table)
