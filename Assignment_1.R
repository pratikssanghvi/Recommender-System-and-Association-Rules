
###############################################

## Pratik Sanghvi                ##
## Statistical Data Mining 2     ##
## Assignment 1                  ##

#########################################################################
## Question 1
#########################################################################

install.packages("recommenderlab")
library(recommenderlab)

#setwd("C:/UB_Spring_Semester/Statistical_Data_Mining_2/Assignment_1")

data("MovieLense")

movlen <- MovieLense

class(movlen)

head(movlen)

dim(movlen)

x11()
image(movlen[1:200,1:200], main= "Raw Ratings")

# normalize

movlen_norm <- normalize(movlen)

x11()
image(movlen_norm[1:200,1:200], main= "Normalized Ratings")


getRatingMatrix(movlen)[1:3,1:3]

x11()
hist(getRatings(movlen), main= "Histogram for user ratings",
     breaks= 20)

x11()
hist(rowCounts(movlen), breaks = 100, 
     main = "Number of Ratings Given by Users")

x11()
hist(colCounts(movlen), breaks = 100, 
     main = "Number of Ratings Per Movie")

#####

#?recommenderRegistry

mov_rec <- Recommender(movlen, method= "UBCF")

#?getModel

names(getModel(mov_rec))

###########################
# predicting the top 5 rated movies for users

top_mov_rec_pred <- predict(mov_rec, movlen, n = 5)

top5_mov_rec <- bestN(top_mov_rec_pred, n=5)

top5_movies_rec <- as(top5_mov_rec, "list")

top5_movies_rec[1:5]

write.table(top5_movies_rec,"Top_5_rated_movies.csv", sep=",")

############################

###########################
# predicting the ratings for all movies not rated by users with 
# NA for ones that have been rated

top_mov_ratings_with_na <- predict(mov_rec, movlen, 
                                type= "ratings")

top_movies_with_na <- as(top_mov_ratings_with_na, "matrix")

top_movies_with_na[1:5,1:10]

write.table(top_movies_with_na,"Top_movies_with_na.csv", sep=",")

###########################

###########################
# predicting ratings for all movies not rated by users without NA
# values for ones that have been rated

top_mov_ratings_without_na <- predict(mov_rec, movlen, 
                                type= "ratingMatrix")

top_movies_without_na <- as(top_mov_ratings_without_na, "matrix")

top_movies_without_na[1:5,1:10]

write.table(top_movies_without_na,"Top_movies_without_na.csv", sep=",")

###########################

###################################################

# part 2

?evaluationScheme
?evaluate

eval<- evaluationScheme(movlen,method = "cross-validation", 
                        given = 15, train=0.5, goodRating=4, k=5)

# Building a recommender model using user based collaborative filtering

userbased_model<- Recommender(getData(eval,"train"), "UBCF")
userbased_model

P1<- predict(userbased_model, getData(eval, "known"), type="ratings")

P2 <- as(P1, "matrix")

P2[1:5,1:5]

ERROR<- rbind(UBCF = calcPredictionAccuracy(P1, getData(eval,"unknown")))
ERROR

#########################################################################


#########################################################################
## Question 2
#########################################################################

rm(list = ls())

install.packages("recommenderlab")
library(recommenderlab)

demog <- read.delim("question_2.csv", header = TRUE, sep = ",")

demog

demog_mat <- data.matrix(demog)

class(demog_mat)

demog_rrm <- as(demog_mat,"realRatingMatrix")

class(demog_rrm)

getRatingMatrix(demog_rrm)

demog_reco_ubcf <- Recommender(demog_rrm, method = "UBCF", 
                          param= list(method= "Pearson"))

demog_pred_ubcf <- predict(demog_reco_ubcf, demog_rrm[2], type="ratings")

as(demog_pred_ubcf,"matrix")

demog_reco_ibcf <- Recommender(demog_rrm, method = "IBCF", 
                               param= list(method= "Cosine"))

demog_pred_ibcf <- predict(demog_reco_ibcf, demog_rrm[2], type="ratings")

as(demog_pred_ibcf,"matrix")


########################################################################

########################################################################
## Question 3
########################################################################

#install.packages("ElemStatLearn")
#install.packages("MASS")
#install.packages("arules")

library(ElemStatLearn)
library(MASS)
library(arules)
#?Boston

#############################################
## Part A
#############################################

head(Boston)

boston <- Boston  

cor(boston)

# removing the variable chas as it doesnt have correlation with dis

boston$chas <- NULL

str(boston)

x11()
hist(boston$crim, main = "Histogram for variable crim")
summary(boston$crim)

boston$crim <- ordered(cut(boston[["crim"]], 
                               c(0,0.08, 0.25, 3.67, 88.97)),
                           labels= c("safe","moderate","cautious",
                                     "dangerous"))

x11()
hist(boston$dis, main = "Histogram for variable dis")
summary(boston$dis)

boston$dis <- ordered(cut(boston[["dis"]], 
                              c(0, 3, 6, 9, 12)),
                          labels= c("Nearby","Reachable","Far",
                                    "Very Distant"))

#boston$dis

x11()
hist(boston$ptratio, main = "Histogram for variable ptratio")
summary(boston$ptratio)

boston$ptratio <- ordered(cut(boston[["ptratio"]], 
                          c(0, 17, 19, 20, 22)),
                      labels= c("low","Moderate","High",
                                "Very High"))

boston$ptratio

x11()
hist(boston$black, main = "Histogram for variable ptratio")
summary(boston$black)

boston$black <- ordered(cut(boston[["black"]], 
                              c(0, 200, 300, 350, 397)),
                          labels= c("low","Moderate","High",
                                    "Very High"))

boston$black

x11()
hist(boston$lstat, main = "Histogram for variable lstat")
summary(boston$lstat)

boston$lstat <- ordered(cut(boston[["lstat"]], 
                            c(0, 9, 18, 27, 38)),
                        labels= c("low","Moderate","High",
                                  "Very High"))

boston$lstat

x11()
hist(boston$medv, main = "Histogram for variable medv")
summary(boston$medv)

boston$medv <- ordered(cut(boston[["medv"]], 
                            c(0, 13, 25, 37, 50)),
                        labels= c("low","Moderate","High",
                                  "Very High"))

boston$medv

x11()
hist(boston$tax, main = "Histogram for variable tax")
summary(boston$tax)

boston$tax <- ordered(cut(boston[["tax"]], 
                           c(0, 250, 350, 550, 720)),
                       labels= c("low_tax_rate","Moderate_tax_rate",
                                 "High_tax_rate", "Very_High_tax_rate"))

boston$tax

x11()
hist(boston$rad)
summary(boston$rad)

boston$rad <- ordered(cut(boston[["rad"]], 
                           c(0, 6, 12, 18, 24)),
                       labels= c("Low","Moderate","High"))

x11()
hist(boston$rm)
summary(boston$rm)

boston$rm <- ordered(cut(boston[["rm"]], 
                          c(0, 4, 6, 9)),
                      labels= c("Low","Moderate","High"))

boston$rm

x11()
hist(boston$age)
summary(boston$age)

boston$age <- ordered(cut(boston[["age"]], 
                         c(0, 25, 50, 75, 100)),
                     labels= c("Low","Moderate","High", "Very High"))

boston$age

x11()
hist(boston$nox)
summary(boston$nox)

boston$nox <- ordered(cut(boston[["nox"]], 
                          c(0, 0.4, 0.6, 0.8, 1)),
                      labels= c("Low","Moderate","High", "Very High"))

boston$nox

x11()
hist(boston$zn)
summary(boston$zn)

boston$zn <- ordered(cut(boston[["zn"]], 
                          c(-1, 25, 50, 75, 100)),
                      labels= c("Low","Moderate","High", "Very High"))

boston$zn

x11()
hist(boston$indus)
summary(boston$indus)

boston$indus <- ordered(cut(boston[["indus"]], 
                         c(0, 7, 14, 21, 28)),
                     labels= c("Low","Moderate","High", "Very High"))

boston$indus

# converting into a binary incidence matrix

bost <- as(boston, "transactions")

summary(bost)

##########################################
## Part b ##
##########################################


x11()
itemFrequencyPlot(bost, support= 0.1, cex.names = 0.8, 
                  main= "Item Frequency Plot")

bost_apriori <- apriori(bost, parameter = list(support=0.1, 
                                               confidence=0.6))


######################################
## Part c ##
######################################

summary(bost_apriori)

rule_subset <- subset(bost_apriori,
                      subset= lhs %ain% c("dis=Reachable","crim=safe"))

rule_subset

inspect(sort(rule_subset, by="confidence"))


######################################
## Part d ##
######################################

rule_lowptratio <- subset(bost_apriori,
                      subset= lhs %ain% c("ptratio=low"))

rule_lowptratio

inspect(head(sort(rule_lowptratio, by="confidence")))
 

######################################
## Part e ##
######################################

boston_e <- Boston

head(boston_e)

?lm

bos_lm <- lm(boston_e$ptratio~., data = boston_e)

summary(bos_lm)


#########################################################################


########################################################################
## Question 4
########################################################################

rm(list= ls())

#install.packages("rpart")
#install.packages("ElemStatLearn")
library(rpart)
library(ElemStatLearn)

#head(marketing)

demo_data <- marketing

dim(demo_data)

class <- sample(1, size=8993, replace = TRUE)

demo_data <- data.frame(demo_data, class)

#class(demo_data)
tail(demo_data)

# check for total missing values

sum(is.na(demo_data))

# replacing the missing values with median values

for (i in 1:ncol(demo_data))
{
  demo_data[is.na(demo_data[,i]),i] <- median(demo_data[,i], na.rm = TRUE)
  
}

sum(is.na(demo_data))

reference_data <- demo_data[sample(nrow(demo_data)),]
#reference_data <- reference_data[sample(nrow(reference_data)),]

for (i in (1:14))
{
  reference_data[[i]] <- sample(demo_data[[i]])
  
}


head(demo_data)
head(reference_data)

reference_data$class <- 0

head(reference_data)
head(demo_data)

# combining the datasets

data <- rbind(demo_data, reference_data)

tail(data)

data$class <- as.factor(data$class)

model.control <- rpart.control(minsplit = 2, minbucket = 1, xval = 10,
                               cp = 0)

?rpart
class_tree <- rpart(data$class~., data = data, method = "class",
                    control = model.control)

names(class_tree)

class_tree$cptable

class_pred <- predict(class_tree, data[,-c(15)])

class_pred

# cannot plot classification tree as there is no split

#x11()
#plot(class_tree, branch = 0.8, uniform = T,
#     compress= T, main = "Full Tree")
#text(class_tree, cex = 0.5)

########################################################################






