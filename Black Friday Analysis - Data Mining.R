
############################ DATA LOADING ########################
#******Load the Blackfriday file
BlackF.raw.df <- read.csv("BlackFriday.csv", header=TRUE)
dim(BlackF.raw.df)
str(BlackF.raw.df)



####################### DATA EXPLORATION ########################## 


#**** summary ****#

summary(BlackF.raw.df)
# From the summary of data we can observe that Product_Category_2 
#and Product_Category_3 have 166986 and 373299 values missing respectively.

# Moreover we can also see that the data is not deviated highly in any of the features 
# implying No outliers in our data.

#********* Gender ***********#

par(mfcol = c(2,1))
options(scipen=999) 
counts <- table(BlackF.raw.df$Gender)
pie(counts)

data.for.plot <- aggregate(BlackF.raw.df$Purchase, by = list(BlackF.raw.df$Gender), 
                           FUN = mean)
names(data.for.plot) <- c("Gender", "Purchase")
data.for.plot
mytable <- table((data.for.plot$Purchase/sum(data.for.plot$Purchase))*100)

mycols <- c("#0073C2FF", "#EFC000FF")
library(ggplot2)
ggplot(data.for.plot, aes(x = "", y = Purchase, fill = Gender)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  scale_fill_manual(values = mycols) +
  theme_void()

# Initially while looking at the customer ratio based on gender we can see that the number of male clients
#recorded exceeds the number of female clients recorded by almost 4 times. 
#Later looking how much each gender spent in regards to eachself we can observe that they are almost even
#implying equal proportion of sales regarding gender.


#******** Age ********#

par(mfcol = c(1,2))
options(scipen=999)

# age group distribution

counts <- table(BlackF.raw.df$Age)
barplot(counts, main="Age group distribution", 
        xlab="Age",ylab = "Count")


data.for.plot <- aggregate(BlackF.raw.df$Purchase, by = list(BlackF.raw.df$Age),
                           FUN = sum)
names(data.for.plot) <- c("Age", "Purchase")
data.for.plot
barplot(height = data.for.plot$Purchase,
        names.arg = data.for.plot$Age,
        xlab = "Age", ylab = "Purchase", main = " Purchase", cex.axis = par("cex.axis"))

par(mfcol = c(1,1))


# It's quite apparent that the largest age group amongst the customers is 26-35 meaning that  that, 
# the 26-35 age group is the most popular. But does this mean that the amount of money spent amongst 
# the age groups is also high in this age group?
# We cannot answer the above question by just considering the number of customers belonging to the age group
# because other factors like the product and its quantity can also affect the total purchases of age.
# So now considering the total purchases made by each age group we get the graph below.

# Interestingly,Our data clearly shows that the amount of money made from each age group 
#correlates proportionally with the amount of customers within the age groups. This can be 
#valuable information for the store, as it might want to add more products geared towards this 
#age group in the future, or perhaps work on marketing different items to increase a broader 
#diversity in the age groups of their customers.


#****** Occupation ********#


# Analysing the relationship between occupation and Purchase through bar graph, in order to create new factor levels for it that will give insights
# about a customer's financial status based on its occupation, as occupation is masked in its current form and it doesn't make much sense.

options(scipen=999)
data.for.plot <- aggregate(BlackF.raw.df$Purchase, by = list(BlackF.raw.df$Occupation), 
                           FUN = mean)
names(data.for.plot) <- c("Occupation", "Purchase")
data.for.plot
barplot(height = data.for.plot$Purchase, 
        names.arg = data.for.plot$Occupation, 
        xlab = "Occupation", ylab = "Average_Purchase", main = "Average Purchase Distribution ", cex.axis = par("cex.axis"))

#Now let's explore the Occupation category. The Occupation number is the ID number of 
#occupation type of each customer. We can see that around 21 different occupations exist
#The Occupation number could represent different professions of customers: for example, 
#number 1 could be an engineer, number 2 - a doctor, number 3 an artist, etc.
#But which category belonging to which group has been masked, making it difficult to 
#interpret the purchase amount based on occupation.
# On the other hand, from the above results we can see that 
#Although there are some occupations which have higher representations, it seems that the 
#amount each user spends on average is more or less the same for all occupations.
# Therefore since the data doesnt provide us much information regarding purchase amount of a customer
# it is likely to be eliminated from the model.


#********** Stay_in_current city years ****#

str(BlackF.raw.df)
data.for.plot <- aggregate(BlackF.raw.df$Purchase, by = list(BlackF.raw.df$Stay_In_Current_City_Years),
                           FUN = sum)
names(data.for.plot) <- c("Stay_In_Current_City_Years", "Purchase")
data.for.plot
barplot(height = data.for.plot$Purchase,
        names.arg = data.for.plot$Stay_In_Current_City_Years,
        xlab = "Stay_In_Current_City_Years", ylab = "Purchase", main = " Purchase", cex.axis = par("cex.axis"))

#The tendency looks like the longest someone is living in that city the less prone they are 
#to buy new things. Hence, if someone is new in town and needs a great number of new things
#for their house that they'll take advantage of the low prices in Black Friday to purchase 
#all the things needed.



#******** City Category ********#
options(scipen=999)


counts <- table(BlackF.raw.df$City_Category)
barplot(counts, main="City_Category distribution", 
        xlab="City_Category",ylab = "Count")


# From the obtained graph we can observe that more number of purchase records have been 
# of customers who stay in City B implying more shoppers from City B where as customers from
# City A and City C are evenly distributed(approximately).


###################################### DATA CLEANING & PRE PROCESSING #########################################

#************************* 1. Imputing missing values

# # REchecking for missing values

sapply(BlackF.raw.df,function(x) sum(is.na(x)))

#install.packages("Amelia")
library(Amelia)
missmap(BlackF.raw.df, main = "Missing values vs observed")

# missing values in the dataset in Product_Category_2 and Product_Category_3

# imputing missing values as 0 because in the 2 columns where values are missing there is no entry as 0. As in all the observations where any of 
# Product_Category_2 or Product_Category_3 has missing values there is a non null entry in Product_Category_1. Hence, missing values mean that 
# Customer did not buy any product of that category.

BlackF.raw.df[is.na(BlackF.raw.df)] <- 0
sapply(BlackF.raw.df,function(x) sum(is.na(x)))

# No missing values in the dataset
str(BlackF.raw.df)


#***************************2.Remove Customer ID and product ID

summary(BlackF.raw.df)
#From the summary of our data, we can observe that,
#User ID and Product ID are just unique ID's represnting a customer and product respectively.
# removing User_ID and Product_ID 
BlackF.df<-BlackF.raw.df[,-c(1,2)]
str(BlackF.df)

#*************************** 3.Removing Occupation
# As described above, we are gonna eliminate occupation for building our models.
BlackF.df<-BlackF.df[,-3]
str(BlackF.df)


#*************************** 4. Converting Age to Continuous 
#Why converting and why average only ??? Aish!
BlackF.df$Age<-ifelse (BlackF.df$Age == '0-17', 8.5,
                             ifelse(BlackF.df$Age == '18-25',21.5,
                                    ifelse(BlackF.df$Age == '26-35',30.5,
                                           ifelse(BlackF.df$Age == '36-45',40.5,
                                                  ifelse(BlackF.df$Age == '46-50',48,
                                                         ifelse(BlackF.df$Age == '51-55',53,
                                                                ifelse(BlackF.df$Age == '55+' ,55,0)))))))


summary(BlackF.df)
str(BlackF.df)

#**************************** 5.Changing Marital status to factor type

#From the class of Marital status we can see that though it is a categorical feature
# it is identified as an integer value. Therefore we are going to change its class type.

BlackF.df$Marital_Status<-as.factor(BlackF.df$Marital_Status)


#***************************** 6. Converting Stay in current city years into continuous

#why to continuous and why +4 to 4???????  Aish!

BlackF.df$Stay_In_Current_City_Years<-ifelse(BlackF.df$Stay_In_Current_City_Years == 0,0,
                                                   ifelse(BlackF.df$Stay_In_Current_City_Years == 1,1,
                                                          ifelse(BlackF.df$Stay_In_Current_City_Years == 2,2,
                                                                 ifelse(BlackF.df$Stay_In_Current_City_Years == 3,3,
                                                                        ifelse(BlackF.df$Stay_In_Current_City_Years == '4+' ,4,0)))))
str(BlackF.df)
#summary(BlackF.df)



################################## RELATIONSHIP OF FEATURES ###################################

#**********relationship with numerical predictors. 


numeric.var<-c(2,4,6,7,8,9)
BlackF.df_N <-BlackF.df[,numeric.var]
str(BlackF.df_N)
#install.packages("corrplot")
library('corrplot')
M <- cor(BlackF.df_N)
M
corrplot(M, method = "circle",diag = FALSE)

# From the above plot we can see that purchase amount is highly correlated with what category
#the product belongs to making Product Category(especially Product_category_1 and 
#Product_category_2 ) as important numerical predictors for our model further.
#Moreover, it can also be infered that the features are exempted from multicollinearity.


#====================================================================================================================================================================#
#====================================================================================================================================================================#

################################ MULTIPLE LINEAR REGRESSION #####################################

BlackF.df_reg <- BlackF.df
str(BlackF.df_reg)


#**************** Creating dummies for categorical variable:

# Categorical Variables - Gender, City_Category , Marital Status

library("dummies")                                                
BlackF.df_reg <- dummy.data.frame(BlackF.df_reg, sep = ".")
BlackF.df_reg<-BlackF.df_reg[,-c(2,6,9)] # keeping n-1 dummies
colnames(BlackF.df_reg)



#**************** Data Partitioning(Train/Valid/Test):
set.seed(1)

train.rows <- sample(rownames(BlackF.df_reg), dim(BlackF.df_reg)[1]*0.5)  # partitioning into training (50%), validation (30%), test (20%)
train.data <- BlackF.df_reg[train.rows, ]                                    # randomly sample 50% of the rows for training
dim(train.data)

valid.rows <- sample(setdiff(rownames(BlackF.df_reg), train.rows),           # sample 30% of the rows into the validation set
                     dim(BlackF.df_reg)[1]*0.3)
valid.data <- BlackF.df_reg[valid.rows, ]
dim(valid.data)

test.rows <- setdiff(rownames(BlackF.df_reg), union(train.rows, valid.rows)) # assign the remaining 20% rows (serve as test)
test.data <- BlackF.df_reg[test.rows, ]
dim(test.data)


#************************** Exhaustive search (Selecting best subset of predictors):

install.packages("leaps")
library(leaps)
search <- regsubsets(Purchase ~ ., data = train.data, nbest = 1, nvmax = dim(train.data)[2],
                     method = "exhaustive")
sum <- summary(search)

# show models
sum$which

# show metrics
sum$adjr2

#From the obtained results we can observe a significant increase of adjusted r2 until we add 5th variable
#and then a drop from the next addition of variable in adjusted R2.So now we build models based on the 
#three close number of variable to the best r2 model and evaluate on valid data so that the 
#model doesnt overfit our data.
#Therefore,Choosing models with 5,6,7 predictors based on the adj R square.


#************************** Building Models :

###     Model 1  with 5 predictors(Adjusted R-squared:0.13501581) 

reg1 <- lm(Purchase~Gender.F+City_Category.A+City_Category.B+Product_Category_1+Product_Category_3, data = train.data) 
options(scipen=999)
summary(reg1)

library(forecast)
tr.res1 <- data.frame(train.data$Purchase, reg1$fitted.values, reg1$residuals)
head(tr.res1)

pred1 <- predict(reg1, newdata = valid.data)
vl.res1 <- data.frame(valid.data$Purchase, pred1, residuals = valid.data$Purchase - pred1)
head(vl.res1)

accuracy(pred1, valid.data$Purchase)

# RMSE : 4639.693



###     Model 2  with 6 predictors(Adjusted R-squared:0.13584238) 

reg2 <- lm(Purchase~Gender.F+Age+City_Category.A+City_Category.B+Product_Category_1+Product_Category_3, data = train.data) 
options(scipen=999)
summary(reg2)

library(forecast)
tr.res2 <- data.frame(train.data$Purchase, reg2$fitted.values, reg2$residuals)
head(tr.res2)

pred2 <- predict(reg2, newdata = valid.data)
vl.res2 <- data.frame(valid.data$Purchase, pred2, residuals = valid.data$Purchase - pred2)
head(vl.res2)

accuracy(pred2, valid.data$Purchase)

# RMSE : 4637.758



### Model 3  with 7 predictors(Adjusted R-squared:0.13593739)

reg3 <- lm(Purchase~Gender.F+Age+City_Category.A+City_Category.B+Product_Category_1+Product_Category_2+Product_Category_3, data = train.data) 
options(scipen=999)
summary(reg3)

library(forecast)
tr.res3 <- data.frame(train.data$Purchase, reg3$fitted.values, reg3$residuals)
head(tr.res3)

pred3 <- predict(reg3, newdata = valid.data)
vl.res3 <- data.frame(valid.data$Purchase, pred3, residuals = valid.data$Purchase - pred3)
head(vl.res3)

accuracy(pred3, valid.data$Purchase)

# RMSE : 4637.501

#From the above results, though the RMSE of Model 2 and Model 3 are quite closer we prefer model 3 because the added variable Product_Category_3 
#would play an important role when a product is only categorized in Electronics as it cannot be catgorized as 
# Clothing or Home Goods(Product Category 1 and Product Category 2) and moreover adding this varibale also causes less Error(Lower RMSE).
#Therefore,we can conclude that Model 3 is the best model
#

#Hence using Model - 3 (a multiple regression model) we can predict Purchase amount of a customer on black friday.


#***************************** Applying the model on Test Data(Unseen Data):

pred_test <- predict(reg3, newdata = test.data)
vl.res_test <- data.frame(test.data$Purchase, pred_test, residuals = test.data$Purchase - pred_test)
head(vl.res_test)

accuracy(pred_test, test.data$Purchase)

# RMSE = 4630.611  


#====================================================================================================================================================================#
#====================================================================================================================================================================#

################################ CLUSTERING #####################################

BlackF_clust.df<-BlackF.df
str(BlackF_clust.df)

#************** Encoding the factor variables Gender & City_Category into Binary and numeric respectively.
#Gender: 0- Female , 1-Male
#City_Category: 1 - City A , 2- City B , City C

BlackF_clust.df$Gender<-as.factor(ifelse(BlackF_clust.df$Gender == 'F', 0 , 1))
str(BlackF_clust.df)

BlackF_clust.df$City_Category<-as.factor(ifelse(BlackF_clust.df$City_Category == 'A', 1 , ifelse(BlackF_clust.df$City_Category == 'B',2,3)))
summary(BlackF_clust.df)


#***************** Scaling Data:

#scaling numrical data to the range of 0-1

library(scales)
BlackF_clust.df[,2]<-rescale(BlackF_clust.df[,2])
BlackF_clust.df[,4]<-rescale(BlackF_clust.df[,4])
BlackF_clust.df[,6] <-rescale(BlackF_clust.df[,6])
BlackF_clust.df[,7] <-rescale(BlackF_clust.df[,7])
BlackF_clust.df[,8]<-rescale(BlackF_clust.df[,8])
BlackF_clust.df[,9]<-rescale(BlackF_clust.df[,9])

summary(BlackF_clust.df)


#******************* Clustering using - K-protoype :

set.seed(1)

install.packages("clustMixType")
library(clustMixType)

#-------- Check for the optimal number of clusters given the data
mydata <- BlackF_clust.df
wss<-vector()
for (i in 1:10){ wss[i] <- sum(kproto(mydata, i)$withinss)}
plot(1:10, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares",
     main="Assessing the Optimal Number of Clusters with the Elbow Method",
     pch=20, cex=1)

#--------- apply k prototypes with optimal k(number of clusters)
kpres <- kproto(mydata, 4)
BlackF_clust.df$cluster = kpres$cluster
head(BlackF_clust.df)

#--------- Visualizing cluster distribution of numerical data

mydata_num <- mydata[,-c(1,3,5,10)]
install.packages("pheatmap")
library(pheatmap)
for (i in c(1:4)) {
  print(i)
  pheatmap(mydata_num[names(kpres$cluster[kpres$cluster == i]), ]) }
  
#----------- Visualizing cluster distribution of categorical data
head(mydata)
mydata_cat<-BlackF_clust.df[,c(1,3,5,10)]
clprofiles(kpres,mydata_cat)





#====================================================================================================================================================================#
#====================================================================================================================================================================#

######################################## APRIORI ALGORITHM #####################################


install.packages("tidyverse")
library(tidyverse)
install.packages("arulesViz")
library(arulesViz)
install.packages("arules")
library(arules)
install.packages("dplyr")
library(dplyr)


#**************************** Data Preprocessing

# Getting the dataset into the correct format
customers_products = BlackF.raw.df %>%
  select(User_ID, Product_ID) %>%   # Selecting the columns we will need
  group_by(User_ID) %>%             # Grouping by "User_ID"          
  arrange(User_ID) %>%              # Arranging by "User_ID" 
  mutate(id = row_number()) %>%     # Defining a key column for each "Product_ID" and its corresponding "User_ID" (Must do this for spread() to work properly)
  spread(User_ID, Product_ID) %>%   # Converting our dataset from tall to wide format, and grouping "Product_IDs" to their corresponding "User_ID"
  t()                               # Transposing the dataset from columns of "User_ID" to rows of "User_ID"

# Now we can remove the Id row we created earlier for spread() to work correctly.
customers_products = customers_products[-1,]

#Now, in order for the Apriori algorithm to work correctly, we need to convert the customers_products 
#table into a sparse matrix. Unfortunately, Apriori doesn't take strings or text as input, but rather 
#1 + 0. (Binary Format) This means that we must allocate a column for each individual product and then 
#if a User_ID contains that product, it will be marked as a 1. On the other hand, if the User_ID does not
#contain that Product_ID, it wil be marked with a 0.

#In order to do so, we need to use the arules library as described above and import the table as a 
# .csv file. From there, we can use the arules function, "read.transactions()" to get our sparse matrix.

write.csv(customers_products, file = 'customers_products.csv')

customersProducts = read.transactions('customers_products.csv', sep = ',', rm.duplicates = TRUE) # remove duplicates with rm.duplicates

# lets take a look at our newly created sparse matrix.

summary(customersProducts)

#Here, we can see that there are 5892 rows (elements/itemsets/transactions) and 10539 columns (items) 
#in our sparse matrix. With this sumary function, we get a density of 0.008768598 in our matrix. The 
#density tells us that we have 0.9% non-zero values (1) in our sparse matrix and 99.1% zero (0) values.

#The "element (itemset/transaction) length distribution" gives us a distribution of the number of items 
#in a customers (User) basket and underneath it we can see more information including the quartile and 
#mean information. In this case, we see a mean of 92.41, which means that on average, each customer 
#purchased 92.41 items. In this case, since we are aware of a few customers who purchased over ~1000 
#items, it may be useful to use the median value of 54.00 items instead since the mean can be heavily 
#affected by outlier values.

#To get a clearer picture of the items, lets create an item frequency plot which is included in the 
#arules package.

itemFrequencyPlot(customersProducts, topN = 25)    # topN is limiting to the top 25 products

#***********************************  training the association rule model **************************

#Our first step will be to set our parameters. The first parameters we will set are the support and 
#confidence. The support value is derived from the frequency of a specific item within the dataset. 
#When we set our support value, we are setting a minimum number of transactions necessary for our rules
#to take effect.

# Support: Our support value wil be the minimum number of transactions necessary divided by the total 
# number of transactions.

#As described by summary(customersProducts), we have a total number of unique customer transactions of 5892.
#From our dataset, lets assume that we want to choose a product which was purchased by at least 50 
#different customers.
#With these two values established, we can compute the support value with simple division.(50/5892) = .008486083

#The second parameter we will take into consideration will be the confidence. The confidence value 
#determines how often a rule is to be found true. In other words, the minimum strength of any rule is a 
#limit we place when setting our minimum confidence value.

#The default confidence value in the apriori() function is 0.80 or 80%, so we can begin with that number
#and then adjust the parameters to applicable results.

#With more domain knowledge, and with Product_IDs referencing items with recognizable names, the 
#Confidence value can be easily changed to see different, and more relevant, results.

rules = apriori(data = customersProducts,
                parameter = list(support = 0.008, confidence = 0.80, maxtime = 0)) # maxtime = 0 will allow our algorithim to run until completion with no time limit
# 7 rules created

inspect(sort(rules, by = 'lift'))

#The first value, lhs, corresponds to a grouping of items which the algorithm has pulled from the dataset.
#The second value, rhs, corresponds to the value predicted by apriori to be purchased with items in the "lhs" category.
#The third value, support is the number of transactions including that specific set of items divided by the total number of transactions. (As described earlier when we chose the parameters for Apriori.)
#The fourth value, confidence is the % chance in which a rule will be upheld.
#The fifth value, lift gives us the independance/dependence of a rule. It takes the confidence value and its relationship to the entire dataset into account.
#The sixth and final value, count is the number of times a rule occured during the implementation of Apriori on our data.
plot(rules, method = 'graph')

