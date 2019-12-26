library(Lahman)
library(dplyr)
library(neuralnet)

str(kidney)



# PCA Heart 
set.seed(100)
sample = sample(2, nrow(heart), replace = T, prob = c(.7,.3))
train = heart[sample ==1,]
test = heart[sample ==2,]

View(heart)
pca = prcomp(heart[,-2],
             scale. = T,
             center = T,)



attributes(pca)

# average of variables
pca$center

#pca 11-13 dont pay an imporantance in the variablity 
summary(pca)

print(pca)


# Nueral Network

library(readr)
heart <- read_csv("~/Desktop/heart.csv")
str(heart)
install.packages("neuralnet")
library(neuralnet)

# min max 
heart$cp = (heart$cp - min(heart$cp))/(max(heart$cp) - min(heart$cp))

heart$trestbps = (heart$trestbp - min(heart$trestbp))/
  (max(heart$trestbp) - min(heart$trestbp))

heart$chol = (heart$chol - min(heart$chol))/(max(heart$chol) - min(heart$chol))

heart$restecg = (heart$restecg - min(heart$restecg))/
  (max(heart$restecg) - min(heart$restecg))

heart$thalach  = (heart$thalach  - min(heart$thalach))/
  (max(heart$thalach) - min(heart$thalach))


heart$oldpeak  = (heart$oldpeak  - min(heart$oldpeak))/
  (max(heart$oldpeak) - min(heart$oldpeak))

heart$slope  = (heart$slope - min(heart$slope))/
  (max(heart$slope) - min(heart$slope))

heart$ca  = (heart$ca - min(heart$ca))/
  (max(heart$ca) - min(heart$ca))

heart$thal  = (heart$thal - min(heart$thal))/
  (max(heart$thal) - min(heart$thal))


# Part 

set.seed(100)
sample = sample(2, nrow(heart), replace = T, prob = c(.7,.3))
train = heart[sample ==1,]
test = heart[sample ==2,]

# Heart problems Neural Network 

library(neuralnet)

net = neuralnet(sex ~.,
                data = train,
                hidden = 1, 
                err.fct = "ce",
                linear.output = F)

plot(net)

# Predict 
# getting out the repsone varaible in the prediction 
output = compute(net, train[,-2])
# prob its a male or female 
head(output$net.result)
head(train[1,])


# confusion matrix 
p1 = output$net.result
predictt = ifelse(p1>0.5, 1, 0)
table(predictt, train$sex)

# test 
output2 = compute(net, test[,-2])
p2 = output2$net.result
predictt2 = ifelse(p2>0.5, 1, 0)
table(predictt2, test$sex)



# net work with more neurons 
# 5 was the max my laptop could handle 
net2 = neuralnet(sex ~.,
                data = train,
                hidden = 5, 
                err.fct = "ce",
                linear.output = F)

plot(net2)

# respine 

output3 = compute(net2, train[,-2])

p4 = output3$net.result
predictt4 = ifelse(p4>0.5, 1, 0)
table(predictt4, train$sex)






# two hidden layers 

net3 = neuralnet(sex ~.,
                 data = train,
                 hidden = c(3,2), 
                 err.fct = "ce",
                 linear.output = F)

plot(net3)


