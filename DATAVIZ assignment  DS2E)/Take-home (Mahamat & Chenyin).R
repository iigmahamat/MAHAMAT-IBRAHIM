rm(list = ls())
getwd()
setwd("D:/Study/Marco")

library(readr)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(ggpubr)
library(VIM)
library(data.table)
library(psych)
library(mvtnorm)
library(caret)
library(PRROC)
library(caTools)
library(pROC)
library(maptools)
library(sp)
library(rgdal)
library(ggmap)
library(aod)
library(ROCR)
require(foreign)
require(nnet)
require(kohonen)
require(ipred)
require(e1071)
require(mlbench) 
require(rpart)
require(party)
require(ROCR)

an13 <- read_csv("D:/Study/Marco/an13.csv")
data1 <- read.csv("D:/Study/Marco/data1.csv")
in13 <- read.csv("D:/Study/Marco/in13.csv")


# DATA PREPARATION #


#### 1. Describe the most interesting variables by plotting distributions, correlations, co-occurrence. ####

# Birth year & sex distribution

a <- data.frame(sex = an13$sesso,birth_year = an13$data_nascita)
male <- filter(a, sex == "M")
b <- group_by(male, birth_year)
male <- summarise(b, number = n())
male$sex <- "Male"
female <- filter(a, sex == "F")
c <- group_by(female, birth_year)
female <- summarise(c, number = n())
female$sex <- "Female"
d <- bind_rows(male, female)
d <- filter(d,!is.na(d))
d <- d[-which(d$birth_year > 2013),]

p1=ggplot(d,aes(x = birth_year,weight = number, fill = sex)) +geom_bar(position="stack")+
  labs(title="Birth year & gender distribution",y="Number of people",x="Birth Year")+
  theme(plot.title = element_text(size=20,face = "bold"),axis.text.y=element_text(size=15),
        axis.text.x=element_text(size=20),
        axis.title.y=element_text(size=15,face="bold"),
        axis.title.x=element_text(size=15,face="bold"))
p1

# User number & price paid distribution

a <- data.frame(CodCliente = data1$codcliente,churn = data1$si2014)
b <- data.frame(CodCliente = an13$codcliente,price_paid = an13$importo,type_of_price_reduction = an13$riduzione)
c <- group_by(in13, CodCliente)
c <- summarise(c, frequency = n())
d <- merge(a, b, by = "CodCliente")
d <- merge(c, d, by = "CodCliente")
d <- group_by(d, price_paid)
d <- summarise(d, mean_frequency = mean(frequency), churn_percentage = 100-mean(churn)*100, 
               people_number = n())
d$price_paid<-factor(d$price_paid)
d$percent<-paste(round(100*d$people_number/sum(d$people_number), 2), "%")

x <- d$people_number/sum(d$people_number)*100
y <- signif(x,digits = 3)
lbls<-paste(d$price_paid,"€    (",d$percent,")")
pie(x,lbls,main="User number & price paid distribution",col = rainbow(length(lbls)))

# Churning & number of visit correlation (Group by price paid)

p2 <- ggplot(d, aes(x=mean_frequency, y=churn_percentage))+ 
  geom_point(aes(col=price_paid, size=20))+
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=FALSE)+
  labs(title="Churning & number of visit correlation (Group by price paid)",y="Mean percentage of churning",
       x="Mean number of visit")+
  theme(plot.title = element_text(size=20,face = "bold"),axis.text.y=element_text(size=15),
        axis.text.x=element_text(size=20),
        axis.title.y=element_text(size=15,face="bold"),
        axis.title.x=element_text(size=15,face="bold"))
p2


#### 2. Do you spot some problem with variables? Are there any specific problems you should take care about? ####

# Cleaning for data1

summary(data1)
data1 <- filter(data1,!duplicated(data1[1]))
data1 <- filter(data1,!duplicated(data1$codcliente))
aggr(data1,prop=FALSE,numbers=TRUE)

# Cleaning for in13

summary(in13)
in13 <- filter(in13,!duplicated(in13[1]))
aggr(in13,prop=FALSE,numbers=TRUE)

# Cleaning for an13

summary(an13)
an13 <- filter(an13,!duplicated(an13[1]))
an13 <- filter(an13,!duplicated(an13$codcliente))
aggr(an13,prop=FALSE,numbers=TRUE)

a <- group_by(an13, data_nascita)
a <- summarise(a, times = n())
an13 <- filter(an13,!is.na(an13$data_nascita))
an13 <- an13[-which(an13$data_nascita > 2013),]

a <- group_by(an13, cap)
a <- summarise(a, times = n())
an13 <- filter(an13,!is.na(an13$cap))
an13 <- an13[-which(str_detect(an13$cap,'[a-zA-Z]')),]


#### 3. Analyse the pattern of missing values. Is there any variable you should drop from the analysis? ####

an13 <- filter(an13,!is.na(an13$sesso))
an13 <- an13[-which(colnames(an13) %in% c("professione"))]
data1 <- data1[-which((data1$si2014 == 1) & (is.na(data1$abb14))),]

aggr(data1,prop=FALSE,numbers=TRUE)
aggr(in13,prop=FALSE,numbers=TRUE)
aggr(an13,prop=FALSE,numbers=TRUE)


#### 4. Can you cluster the observations? Is there a cluster with most churners? ####

A <- data.frame(CodCliente = an13$codcliente, Price_paid = an13$importo, 
                Gender = an13$sesso, Birth_year = an13$data_nascita)
B <- data.frame(CodCliente = data1$codcliente, Churning = data1$si2014)
C <- group_by(in13, CodCliente)
C <- summarise(C, Number_of_visit = n())
D <- merge(A, B, by = "CodCliente")
D <- merge(C, D, by = "CodCliente")

male <- filter(D, Gender == "M")
male$Gender <- 1
female <- filter(D, Gender == "F")
female$Gender <- 0
D <- bind_rows(male, female)
D <- D[order(D$CodCliente), ]
str(D)
coolBlueHotRed <- function(n, alpha = 1) {rainbow(n, end=4/6, alpha=alpha)[n:1]}
pretty_palette <- c("#1f77b4", '#ff7f0e', '#2ca02c', '#d62728', '#9467bd', '#8c564b', '#e377c2')

data_train <- D[, c(2,3,4,5,6)]
data_train_matrix <- as.matrix(scale(data_train))

som_grid <- somgrid(xdim = 10, ydim = 10, topo="rectangular")
som_model <- som(data_train_matrix, 
                 grid=som_grid, 
                 rlen=500, 
                 alpha=c(0.05,0.01), 
                 keep.data = TRUE)

plot(som_model, type="changes")
plot(som_model, type="count")
plot(som_model, type="dist.neighbours")
plot(som_model, type="codes")
plot(som_model, type = "property", property = as.data.frame(som_model$codes)[,2], 
     main=names(som_model$data)[4], palette.name=coolBlueHotRed)
plot(som_model, type = "property", property = A[,2], main=names(som_model$data)[4], palette.name=coolBlueHotRed)

var <- 5
var_unscaled <- aggregate(as.numeric(data_train[,var]), by=list(som_model$unit.classif), FUN=mean, simplify=TRUE)[,2] 
plot(som_model, type = "property", property=var_unscaled, main=names(data_train)[var], palette.name=coolBlueHotRed)
var <- 1
var_unscaled <- aggregate(as.numeric(data_train[,var]), by=list(som_model$unit.classif), FUN=mean, simplify=TRUE)[,2] 
plot(som_model, type = "property", property=var_unscaled, main=names(data_train)[var], palette.name=coolBlueHotRed)
var <- 2
var_unscaled <- aggregate(as.numeric(data_train[,var]), by=list(som_model$unit.classif), FUN=mean, simplify=TRUE)[,2] 
plot(som_model, type = "property", property=var_unscaled, main=names(data_train)[var], palette.name=coolBlueHotRed)
var <- 3
var_unscaled <- aggregate(as.numeric(data_train[,var]), by=list(som_model$unit.classif), FUN=mean, simplify=TRUE)[,2] 
plot(som_model, type = "property", property=var_unscaled, main=names(data_train)[var], palette.name=coolBlueHotRed)
var <- 4
var_unscaled <- aggregate(as.numeric(data_train[,var]), by=list(som_model$unit.classif), FUN=mean, simplify=TRUE)[,2] 
plot(som_model, type = "property", property=var_unscaled, main=names(data_train)[var], palette.name=coolBlueHotRed)


#### 5. Draw a geographical maps of the distribution of card holders, percentage of churners, and average revenues. ####

A <- data.frame(IT_CAP = an13$cap, CodCliente = an13$codcliente, Price_paid = an13$importo)
B <- data.frame(CodCliente = data1$codcliente, Churn = data1$si2014)
C <- merge(A, B, by = "CodCliente")
C <- group_by(C, IT_CAP)
D <- summarise(C, Card_holders = n())
E <- summarise(C, Percentage_of_churners = 100-mean(Churn)*100)
F <- summarise(C, Average_revenues = mean(Price_paid))
G <- merge(D, E, by = "IT_CAP")
G <- merge(F, G, by = "IT_CAP")

shp <- readOGR(dsn = "CAP_NordOvest_Shp", layer="cap_NO")
head(shp@data)
shp@data <- merge(shp@data, G, by = "IT_CAP", all.x = TRUE)

# Map of the distribution of card holders

spplot(shp, c("Card_holders"))

# Map of the distribution of the percentage of churners

spplot(shp, c("Percentage_of_churners"))

# Map of the distribution of average revenues

spplot(shp, c("Average_revenues"))


# CHURN and MARKETING CAMPAIGN #


####  1. Is there an impact of age and gender on the probability of churning? #### 
# Identify a model to answer the question and show your answer in a simple graphical way. 

A <- data.frame(CodCliente = an13$codcliente, Gender = an13$sesso, Age_2014 = 2014 - an13$data_nascita)
B <- data.frame(CodCliente = data1$codcliente, Probability_of_Churning = 1-data1$si2014)
C <- merge(A, B, by = "CodCliente")
male <- filter(C, Gender == "M")
male$Gender <- 1
female <- filter(C, Gender == "F")
female$Gender <- 0
D <- bind_rows(male, female)
D <- D[order(D$CodCliente), ]

# Regression normal

fit1 <- lm(Probability_of_Churning~Age_2014+Gender, data=D)
summary(fit1)

# Logit

D$Gender <-factor(D$Gender)
fit2 <- glm(Probability_of_Churning~Age_2014+Gender, data=D, family = "binomial")
summary(fit2)
exp(coef(fit2))

newdata1 <- with(D, data.frame(Age_2014 = mean(Age_2014), Gender = factor(0:1)))
newdata1$GenderP <- predict(fit2, newdata = newdata1, type = "response")
newdata1

newdata2 <- with(D, data.frame(Age_2014 = rep(seq(from = 0, to = 120, length.out = 10),10), 
                               Gender = factor(rep(0:1, each = 100))))
newdata2$Probability_of_Churning <-predict(fit2, newdata = newdata2, type = "response")
ggplot(newdata2, aes(x = Age_2014, y = Probability_of_Churning)) + geom_line(aes(colour = Gender), size=1)

# Odd ratio

data <- as.data.frame(exp(coef(fit2))[2:3])
data$y <- rownames(data)
colnames(data)<- c("x", "y")

p1 <- ggplot(data, aes(x=x, y=y))+
  geom_segment( aes(y=y, yend=y, x=0, xend=x ), color="orange", size=2)+
  geom_point( color= "grey", size=3)+
  xlab("") +
  ylab("Impact") +
  ggtitle("Is there an impact of age and gender on the probability of churning?")+
  geom_vline(xintercept = 1, linetype="dotted", color = "blue", size=1.5)+ 
  annotate("text", x=1.2, y=2, label="positive impact",color="orange", size=4 , angle=90, fontface="bold", hjust=0) 
p1


#### 2. Which models could you use to predict churners? #### 
# Run at least three prediction models and show the ROC curves for them. Compute the predicted probability on the 
# test-set and show its distribution. Draw on the same graph two distributions, the predicted probability distribution
# for churners and non churners.

A <- data.frame(CodCliente = data1$codcliente, Churner = 1 - data1$si2014)
B <- data.frame(CodCliente = in13$CodCliente, Should_pay = in13$importo)
B <- group_by(B, CodCliente)
B <- summarise(B, Frequency = n(), Should_pay = sum(Should_pay))
C <- data.frame(CodCliente = an13$codcliente, Pay_for_card = an13$importo, Type_of_reduction = an13$riduzione, 
                Type_of_payment = an13$tipo_pag, Gender = an13$sesso, Age_2014 = 2014 - an13$data_nascita)
D <- merge(A, B, by = "CodCliente")
D <- merge(C, D, by = "CodCliente")
D<- na.omit(D)
D$Churner <- as.factor(D$Churner)
D$Type_of_reduction <- as.factor(D$Type_of_reduction)
D$Type_of_payment <- as.factor(D$Type_of_payment)
D$Gender <- as.factor(D$Gender)
D <- D[-which(colnames(D) %in% c("CodCliente"))]

# Model 1 Logit

ind <- sample(2, nrow(D), replace = TRUE, prob=c(0.7, 0.3))
trainset = D[ind == 1,]
testset = D[ind == 2,]

fit1 <- glm(Churner ~ Pay_for_card + Type_of_reduction + Type_of_payment + Gender + Age_2014 + 
              Frequency + Should_pay, data = trainset, family = "binomial")
testset$probability_lg <- predict(fit1,testset,type='response')

testset$predclass_lg <- ifelse(testset$probability_lg > 0.5,1,0)
table(testset$Churner, testset$predclass_lg)

p1 <- ggplot(testset, aes(probability_lg, color= as.factor(Churner))) + geom_density(kernel="gaussian") + 
  xlab("Fitted results") + ylab("Density")
p1

# Model 2 Recursive partition

x.rp <- rpart(Churner ~ ., data=trainset)
testset$x.rp.prob <- predict(x.rp, type="prob", newdata=testset)
testset$x.rp.pred <- predict(x.rp, type="class", newdata=testset)
testset$probability_rp <- testset$x.rp.prob[,2]

testset$predclass_rp <- ifelse(testset$probability_rp > 0.5,1,0)
table(testset$Churner,testset$predclass_rp)

p2 <- ggplot(testset, aes(probability_rp, color= as.factor(Churner))) + geom_density(kernel="gaussian") + 
  xlab("Fitted results") + ylab("Density")
p2

# Model 3 Conditional tree

trainset[sapply(trainset, is.character)] <- lapply(trainset[sapply(trainset, is.character)], as.factor)
testset[sapply(testset, is.character)] <- lapply(testset[sapply(testset, is.character)], as.factor)

x.ct <- ctree(Churner ~ ., data=trainset)
testset$x.ct.prob <-  1- unlist(treeresponse(x.ct, testset), use.names=F)[seq(1,nrow(testset)*2,2)]
testset$x.ct.pred <- predict(x.ct, newdata=testset)
plot(x.ct, main="Decision tree created using ctree")
testset$probability_ct <- testset$x.ct.prob

testset$predclass_ct <- ifelse(testset$probability_ct > 0.5,1,0)
table(testset$Churner,testset$predclass_ct)

p3 <- ggplot(testset, aes(probability_ct, color= as.factor(Churner))) + geom_density(kernel="gaussian") + 
  xlab("Fitted results") + ylab("Density")
p3

# ROC curves for model 1,2,3

x.lg.prob.rocr <- prediction(testset$probability_lg, testset$Churner)
x.lg.perf <- performance(x.lg.prob.rocr, "tpr","fpr")
plot(x.lg.perf, col=2, main="ROC curves comparing classification performance of 3 models")

x.rp.prob.rocr <- prediction(testset$probability_rp, testset$Churner)
x.rp.perf <- performance(x.rp.prob.rocr, "tpr","fpr")
plot(x.rp.perf, col=3, add=TRUE)

x.ct.prob.rocr <- prediction(testset$probability_ct, testset$Churner)
x.ct.perf <- performance(x.ct.prob.rocr, "tpr","fpr")
plot(x.ct.perf, col=4, add=TRUE)


#### 3. Consider a marketing campaign addressing directly single customer. #### 
# We know that each contact costs ALPHA euros. We know from past marketing campaign, that it decreases churn 
# probability per consumer by 10%. Order the consumers on the test-set according on their expected profits for the 
# card association WITHOUT marketing campaign using the predicted probability of different models. Compare them with 
# the real profit curve. Which model are you going to use?

# Logit profit curve

testset$lg.prof <- testset$probability_lg * 0 + (1-testset$probability_lg) * 
  (testset$Pay_for_card - (testset$Frequency * 5))

newdata1 <- testset[order(-testset$lg.prof),] 
newdata1$cumprof<-cumsum(newdata1$lg.prof)
plot(newdata1$cumprof, ylab="Cumulative Profit", xlab="Observations", col=2, type="l",
     main="Real profit comparing expected profits of 3 models on the testset", ylim=c(-70000,160000))

# Rp profit curve

testset$rp.prof <- testset$probability_rp * 0 + (1-testset$probability_rp) * 
  (testset$Pay_for_card - (testset$Frequency * 5))

newdata2 <- testset[order(-testset$rp.prof),] 
newdata2$cumprof<-cumsum(newdata2$rp.prof)
lines(newdata2$cumprof, col=3)

# Ct profit curve

testset$ct.prof <- testset$probability_ct * 0 + (1-testset$probability_ct) * 
  (testset$Pay_for_card - (testset$Frequency * 5))

newdata3 <- testset[order(-testset$ct.prof),] 
newdata3$cumprof<-cumsum(newdata3$ct.prof)
lines(newdata3$cumprof, col=4)

# Real profit curve

testset$Churner <- as.numeric(as.character(testset$Churner))
testset$Churner_real <- 1-testset$Churner
testset$real.prof <- testset$Churner_real * (testset$Pay_for_card - (testset$Frequency * 5))

newdata4 <- testset[order(-testset$real.prof),] 
newdata4$cumprof<-cumsum(newdata4$real.prof)
lines(newdata4$cumprof, col=1)


#### 4. Simulate the profits in 2014, assuming that consumers behavior is the same and draw the expected profit curves. ####
# Given that you contact consumers by expected profits, what it is the percentage of consumers you are going to
# contact? Show graphically what happens for different value of ALPHA.

# Ct profit curve without the marketing campaign

testset$ct.prof <- testset$probability_ct * 0 + (1-testset$probability_ct) * 
  (testset$Pay_for_card - (testset$Frequency * 5))

newdata1 <- testset[order(-testset$ct.prof),] 
newdata1$cumprof<-cumsum(newdata1$ct.prof)
plot(newdata1$cumprof, ylab="Cumulative Profit", xlab="Observations", col=1, type="l", 
     main="Expected profit with marketing campaign according to different ALPHA (CT Model)", ylim=c(-70000,160000))

# When Alpha is 0.1

Alpha = 0.1
testset$ct.prof.MC1 <- (testset$probability_ct * 0.9) * 0 + (1-testset$probability_ct * 0.9) * 
  (testset$Pay_for_card - (testset$Frequency * 5)) - Alpha

newdata2 <- testset[order(-testset$ct.prof.MC1),] 
newdata2$cumprof<-cumsum(newdata2$ct.prof.MC1)
lines(newdata2$cumprof, col=2)

# When Alpha is 0.5

Alpha = 0.5
testset$ct.prof.MC2 <- (testset$probability_ct * 0.9) * 0 + (1-testset$probability_ct * 0.9) * 
  (testset$Pay_for_card - (testset$Frequency * 5)) - Alpha

newdata3 <- testset[order(-testset$ct.prof.MC2),] 
newdata3$cumprof<-cumsum(newdata3$ct.prof.MC2)
lines(newdata3$cumprof, col=3)

# When Alpha is 1

Alpha = 1
testset$ct.prof.MC3 <- (testset$probability_ct * 0.9) * 0 + (1-testset$probability_ct * 0.9) * 
  (testset$Pay_for_card - (testset$Frequency * 5)) - Alpha

newdata4 <- testset[order(-testset$ct.prof.MC3),] 
newdata4$cumprof<-cumsum(newdata3$ct.prof.MC3)
lines(newdata4$cumprof, col=4)
