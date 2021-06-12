#######
### Starter Script for Case 1: 
### Reminder: make sure all the files and 
### scripts are in the working directory
###
### List of files:
### natalityNew.Rda
### DataAnalyticsFunctions.R
###
### Keep in mind that:
### Questions 1, 3 and 4 can be solved after the Cleaning up of the data below.
### Question 2, I provided some initial code to create a list of p-values (and labels)
### Questions 5 and 6 do not need additional data analysis
###########################################

### Initialization with Cleaning up of Data ####
###
### Load auxiliary R file
source("DataAnalyticsFunctions.R")
######################################################
### Load infant weight data
load("natalityNew.Rda")
### This creates a data frame called "d" 
### This data set has 198377 observations
### 19 variables but we need to clean up a little
### Lets see a summary of the data
summary(d)
######################################################
### The following code will clean up the data for you
###  
###
### tri.none is all zeros so we will remove it. 
### birmon is all 6 so we will remove it.
### Also, there are redundancies (and identifier number). 
### We will remove the following varaibles: "id","birmon","tri.none","novisit"
### to do that lets create a vector with those names
drops <- c("id","birmon","tri.none","novisit")
### names(d) has all the column names. We want to REMOVE the names in drops
### names(d) %in% drops tells you which entry of names(d) is in drops
names(d) %in% drops
### this is a value of true/false for each column
### since we want "not in" we reverse that with the command "!"
!( names(d) %in% drops )
### we can use the logical vector above to select the columns
### the following command creates a dataframe (called DATA) 
### which has all the columns from d not in drops
DATA <- d[,!(names(d) %in% drops)]
summary(DATA)
######################################################
### End of data clean up
###
######################################################
###
### You can start with Questions 1, 3, 4 
### (recall that 5 and 6 do note need additional analysis)





##Question 1, starting with a correlation matrix and then
##simple regressions to see relationship between variables

##Correlation matrix, installing corrplot package, ggplot2 package

install.packages('ggplot2')
library(ggplot2)
install.packages('corrplot')
library(corrplot)

corr <- cor(DATA)

corrplot(corr, method = "number", number.cex = .5, number.digits = 2)

##the numbers are kind of hard to see, so for visualization purposes, this may
##be a little be easier

corrplot(corr, method = 'circle')

##some interesting things in this correlation matrix, tri1 is highly correlated
##with tri2 and tri3, because if you did not have your first prenatal appointment
##in the first trimester, it was likely in the second, and if not, it was in the 3rd.
##Careful when further model-building to only include two of these dummy variables,
##if all 3 are included, there will be perfect multicollinearity.


##The useful information we can from this correlation matrix is the 
##highest correlations (aside from above) appear to be 
##mothers age -- married, mothers age--college education, and married--college education.




##Simple Regressions to see significance of each relationship mentioned above.


##RELATIONSHIP 1, MOTHERS AGE & MARRIED

ageMarried <- lm(mom.age ~ married, data = DATA)
summary(ageMarried)
##The r2 is relatively high compared to other relationships we tested.
## (we took them out of the code but they were btweight & moms age, 
##btweight & smoke, and btweight with a log transformation on moms age)
##The regression shows that the average age of non-married women with kids
## is 24.12, and if married 28.75. Below is a graph to show this:

r1 <- ggplot(DATA, aes(factor(married), mom.age))

r1 + geom_boxplot() + geom_jitter( width = .1, size = .05, color = 'Blue') 







##RELATIONSHIP 2, MOTHERS AGE & EDUCATION 
##First, we should create a factored term for education and put it into the
##dataset, so we can see the age by education level, not just age and college 
##education. 



educationLevel <- c()
for(i in 1:198377){
  if(DATA[i, 10] == 1){
    educationLevel<-c(educationLevel, 3)
  } else if(DATA[i, 9] == 1){
    educationLevel <-c(educationLevel, 2)
  }else if(DATA[i, 8]==1){
    educationLevel <-c(educationLevel, 1)
  } else {
    educationLevel <- c(educationLevel, 0)
  }
}

##THE CODE ABOVE WILL TAKE A MINUTE OR TWO TO RUN DONT STOP IT 
## UNTIL 'educationLevel' in the Values section on the right says 
##Large numeric, 198377. Now, the code below turns the variable into a factor
## and adds it to the main DATA set.


educationFactor <- factor(educationLevel)


DATA <- cbind(DATA, educationFactor)


educationModel <- lm(mom.age ~ educationFactor, data = DATA)
summary(educationModel)
##In line with other regressions, the average age of mothers is 24. This
## is expected to increase by 2.07 years with a hs education,
## 3.92 years with some college education, and 6.99 years with a college education
##This has the highest r2 of the 3, with .18 .




##The code below creates a graph based on mother's education by age.

r2 <- ggplot(DATA,aes(educationFactor,mom.age))

r2 + geom_boxplot()

##This shows that education level does rise with age, and implies that
##more educated women wait longer to have children.






##RELATIONSHIP 3


marriageEducation <- lm(married ~ educationFactor, data = DATA)
summary(marriageEducation)
##This shows basically the same two as the other regressions.
## The higher a woman's education level, the more likely she is to be married.
## The graph for this relationship is below 

r3 <- ggplot(DATA, aes(factor(married),educationLevel ))

r3 + geom_boxplot()




######################################################
###
### Organizational help for Question 2 
### 
### This creates a matrix with only the 10 binary variables.
MatrixComp <- as.matrix( cbind( DATA$boy, DATA$tri1, DATA$tri2, DATA$tri3, DATA$black, DATA$married, DATA$ed.hs, DATA$ed.smcol, DATA$ed.col, DATA$smoke ))  
### Here is the associated LAbels (for convenience).
LabelsTmp <- c( "boy", "tri1", "tri2", "tri3", "black", "married", "ed.hs", "ed.smcol", "ed.col","smoke")
### Number of columns (should be 10).
NumCol <- ncol(MatrixComp)
### Next we compute the p-values for each pair.
pvals <- rep(0, NumCol*(NumCol-1)/2) 
### Also will collect the pair label.
ListLabels <- rep("", NumCol*(NumCol-1)/2) 
k <- 0
for (i in 1:(NumCol-1) ){
  for ( j in (i+1):NumCol ){
    k <- k+1
    ### Creates the entries of the contingency table.
    m00 <- sum( (MatrixComp[,i] == 0) & (MatrixComp[,j] == 0) ) 
    m01 <- sum( (MatrixComp[,i] == 0) & (MatrixComp[,j] == 1) ) 
    m10 <- sum( (MatrixComp[,i] == 1) & (MatrixComp[,j] == 0) ) 
    m11 <- sum( (MatrixComp[,i] == 1) & (MatrixComp[,j] == 1) ) 
    ### Construct the contingency table.
    ContingencyMatrix <- as.table(rbind(c(m00, m01), c(m10, m11)))
    ### Perform the Pearson chi squares test for independent of factors.
    ### Store the p-value of the test.
    pvals[k] <- chisq.test(ContingencyMatrix)$p.value  
    ### create the Label.
    ListLabels[k] <- paste(LabelsTmp[i],LabelsTmp[j], sep=" and ")  
  }  
}
###############################################################
### Now you have:
### a list of p-values; and
### a list of labels to help you identify which are the variables.

### pvals: is a vector with 45 p-values each associated with an independence test
### ListLabels: is a vector with the labels of the 2 variables used on each the independence test
###
### for example  
### the third test pertains to the pair 
ListLabels[3]
### which p-value is
pvals[3]
################################################################
### You can proceed to Question 2 
################################################################




##The following code applies the bonferonni corrections to the pvalues
##from the chi-square test of independence.
pvalsCorrected <- bonferroni <- p.adjust(pvals, method = "bonferroni", n= 45)


##This code checks the original pvals and the corrected ones to see if they 
##are independent or not.
pval1 <- pvals > .05
pval2 <- pvalsCorrected > .05


##This puts the information into an easy to read table so we can tell
##if there are any discrepancies between the two sets of p-values.
q2DATA2 <- cbind(ListLabels, pvals, pvalsCorrected, pval1, pval2)
q2DATA2


##QUESTION 2 ANSWER
##The test above shows that 4 & 5 (boy & black) & (boy & married)
##changed from dependent to independent. We think the bonferroni 
##correction yielded the correct result as race likely has no effect
##on the gender of the child, and neither does the marital status. 



##QUESTION 3 
##We worked through many different regression models to determine 
##which variables were statistically significant in increasing the weight
##of a baby.


boyheavier <- lm(weight ~ boy  + smoke + cigsper
                 + m.wtgain + mom.age + mom.age2 + married + black
                 , data = DATA)

summary(boyheavier)

###age2 function shows that at the age of 33.378 babies begin getting lighter
##but still has a positive effect up until the age of about 67.









##QUESTION 4
##This code regresses all of the variables in Exhibit 1 (exluding tri3).
##The education levels do not create perfect multicollinearity because
##there were records in the sample in which a mother did not complete
##any level of education. So we did not need to include an 
##omitted reference category, as mothers who did not complete any
##education serve this purpose.
q4model <- lm(weight ~ black + married + 
                boy + tri1 + tri2 + educationFactor + mom.age
              + mom.age2
              + smoke + cigsper + m.wtgain, data = DATA )
summary(q4model)





##This code block takes the coefficients in the model and puts them into a list.
pvalsq4 <- summary(q4model)$coefficients[,4]

##This code block adds the labels to make it easier to tell within each category
##if there is a discrepancy between the two variables.

labelsq4 <- c('Intercept','black','married','boy','tri1','tri2',
              'HS education','some college', 'college education',
              'mom.age', 'mom.age 2', 'smoke', 'cigsper', 'mom weightgain')

q4DATA <- cbind(labelsq4, pvalsq4)

##This code corrects the p-values from from the regression in question 4 and adds 
##them to the q4 data in order to make it easier to read and tell if there
##are any discrepancies.
pvalsCorrected4 <- bonferroni <- p.adjust(pvalsq4, method = "bonferroni", n= 14)

q4DATA <- cbind(q4DATA, pvalsCorrected4)

q4DATA 


##All variables are statistically significant at the .05 level in both instances, 
##so there are no discrepancies between the original p-values and their corrected form.