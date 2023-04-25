library(tidyverse)
library(glmnet)
library(pls)

### Loading and Filtering Data
#
# I created two versions of the variables to test:
# One with the individual yes/no questions and one with the category scores
#
# I focused on expenditures and the response variable and used 2020 for the year
# That can be easily changed, though, for uniformity

data <- read_csv("../data/data2.csv")

dataV1 <- select(data, -economy_code, -iso_code, -wbl_index,
                 -mobility, -workplace, -pay, -marriage, -parenthood,
                 -entrepreneurship, -assets, -total, -overnight, 
                 -oneday, -cruise, -country)
dataV1 <- filter(dataV1, year == 2020, !is.na(expenditures), !is.na(region))


dataV2 <- select(data, region, income_group, year, mobility, 
                 workplace, pay, marriage, parenthood, entrepreneurship, 
                 assets, expenditures)
dataV2 <- filter(dataV2, year == 2020, !is.na(expenditures), !is.na(region))


### Ridge and Lasso Regression
### Version 1
regV1 <- lm(expenditures ~ ., data = dataV1)
x1 <- model.matrix(regV1)
x1 <- x1[, -1]
y1 <- dataV1$expenditures

## Ridge
ridge1 <- glmnet(x1, y1, alpha = 0)
plot(ridge1, label = True, xvar = "lambda")

set.seed(123)
ridge1CV <- cv.glmnet(x1, y1, alpha = 0)
plot(ridge1CV)
coef(ridge1CV) |> round(2)
ridge1CV
## MSE: 6.983 x 10^9

## Lasso
lasso1 <- glmnet(x1, y1)
plot(lasso1, label = True, xvar = "lambda")

set.seed(123)
lasso1CV <- cv.glmnet(x1, y1)
plot(lasso1CV) #1se lambda got rid of all variables
coef(lasso1CV, s = "lambda.min") |> round(2)
lasso1CV
## MSE: 1.231 x 10^10

### Version 2
regV2 <- lm(expenditures ~ ., data = dataV2)
x2 <- model.matrix(regV2)
x2 <- x2[, -1]
y2 <- dataV2$expenditures

## Ridge
ridge2 <- glmnet(x2, y2, alpha = 0)
plot(ridge2, label = True, xvar = "lambda")

set.seed(123)
ridge2CV <- cv.glmnet(x2, y2, alpha = 0)
plot(ridge2CV)
coef(ridge2CV) |> round(2)
ridge2CV
## MSE: 9.854 x 10^9

## Lasso
lasso2 <- glmnet(x2, y2)
plot(lasso2, label = True, xvar = "lambda")

set.seed(123)
lasso2CV <- cv.glmnet(x2, y2)
plot(lasso2CV)
coef(lasso2CV) |> round(2)
lasso2CV
## MSE: 9.150 x 10^10


### PCA Regression
### Version 1
pc1 <- prcomp(x1)
screeplot(pc1)
pcreg1 <- pcr(expenditures ~ ., data = dataV1, validation = "CV")
summary(pcreg1)
## MSE: 6.986 x 10^16

### Version 2
pc2 <- prcomp(x2)
screeplot(pc2)
pcreg2 <- pcr(expenditures ~ ., data = dataV2, validation = "CV")
summary(pcreg2)
## MSE: 5.464 x 10^6