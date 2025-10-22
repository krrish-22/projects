rm(list = ls())

setwd("~/Desktop/krrish porfolio/project 1/R programming")

#GENERAL
#load libraries
library(ggplot2)
library("reshape2")
library(tidyverse)

#reading dataset
insurance_dataset<-read.csv("insurance_policies_data (R)(2).csv",header=TRUE)

#descriptive statistics
summary(insurance_dataset)
sum(insurance_dataset$Claim.Freq > 0) # policies with at least 1 claim. result = 14086
mean(insurance_dataset$Claim.Freq) # average claim frequency. result = 0.5002528

# Claim severity (only for those with claims)
mean(insurance_dataset$Claim.Amount[insurance_dataset$Claim.Freq > 0]) #result = 5059.693
median(insurance_dataset$Claim.Amount[insurance_dataset$Claim.Freq > 0]) #result = 5127.271

# Histogram of claim amounts
ggplot(insurance_dataset, aes(x = Claim.Amount)) +
  geom_histogram(bins = 50, fill = "steelblue", color = "white") +
  scale_x_continuous(trans="log1p") + # log scale if skewed
  labs(title="Distribution of Claim Amounts", x="Claim Amount", y="Count")

# Claim frequency by coverage zone
ggplot(insurance_dataset, aes(x = Coverage.Zone, y = Claim.Freq)) +
  geom_boxplot(fill="tomato") +
  labs(title="Claim Frequency by Coverage Zone")

#logistic regression model = to find the probability that a customer will file at least one claim in a policy year
insurance_dataset$Has.Claim<-ifelse(insurance_dataset$Claim.Freq > 0, 1, 0)

logit_model<-glm(Has.Claim ~ age + Gender + Education + Coverage.Zone,
                   data = insurance_dataset, family = binomial) # Logistic regression model
summary(logit_model)

#poisson regression model - to find out how often claims are expected to occur
poisson_model<-glm(Claim.Freq ~ age + Gender + Education + Coverage.Zone,
                     data = insurance_dataset, family = poisson)
summary(poisson_model)

#gamma model
claims_only<-subset(insurance_dataset,Claim.Amount>0) #Only keep rows with claims
gamma_model<-glm(Claim.Amount ~ age + Gender + Education + Coverage.Zone,
                   data = claims_only, family = Gamma(link="log"))
summary(gamma_model)


