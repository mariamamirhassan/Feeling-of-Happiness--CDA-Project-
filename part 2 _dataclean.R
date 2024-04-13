#Cleaning the Data again CDA Project

data2=read.csv("data_subset.csv")
attach(data2)
#Sex and Happiness
data2$sex=as.factor(sex)
data2$happy=as.factor(happy)
levels(data2$happy)

#Compettion and Happiness
data2$comp[data2$comp == "Competition is good"]=1
data2$comp[data2$comp == "Competition is harmful"]=10
data2$comp <- as.factor(data2$comp)


# Assume 'data2$comp' is your factor column
data2$comp <- factor(data2$comp, levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "Don't know"))

levels(data2$comp)


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


#Social Class and Happiness

data2$happy=as.factor(happy)
levels(data2$happy)



data2$social_class <- as.factor(data2$social_class)


levels(data2$social_class)

data2$social_class <- factor(data2$social_class, levels = c(
  "Don't know", "Lower class", "Lower middle class",
  "Working class","Upper middle class" ,"Upper class"
))

#Rel den and Happiness

data2$happy=as.factor(happy)
levels(data2$happy)



data2$rel_den <- as.factor(data2$rel_den)


levels(data2$rel_den)


attach(data2)


#People trust and Happiness

data2$happy=as.factor(happy)
levels(data2$happy)



data2$people_trust <- as.factor(data2$people_trust)


levels(data2$people_trust)


attach(data2)


#Rel per and Happiness

data2$happy=as.factor(happy)
levels(data2$happy)



data2$rel_per<- as.factor(data2$rel_per)


data2$rel_per <- factor(data2$rel_per, levels = c(
  "An atheist", "Not a religious person", "Don't know", "A religious person"
))
levels(data2$rel_per)


attach(data2)





write.csv(data2, file = "data_clean.csv", row.names = FALSE)
