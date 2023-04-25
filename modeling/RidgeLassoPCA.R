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


### New Version of Data
#
# I tried doing a version of the data focusing on the change in expenditures
# as well as the change in the index measures

### Loading and Filtering Data
data <- read_csv("../data/data2.csv")

data <- select(data, region, country, income_group, year, mobility, 
               workplace, pay, marriage, parenthood, entrepreneurship, 
               assets, expenditures)
data <- filter(data, year > 2000, !is.na(expenditures))
data <- pivot_wider(data, id_cols = c(country, region, income_group), 
                    names_from = year, 
                    values_from = mobility:expenditures)

data <- mutate(data, expenditures_old = case_when(
  !is.na(expenditures_2001) ~ expenditures_2001,
  !is.na(expenditures_2002) ~ expenditures_2002, 
  !is.na(expenditures_2003) ~ expenditures_2003, 
  !is.na(expenditures_2004) ~ expenditures_2004, 
  !is.na(expenditures_2005) ~ expenditures_2005
)
)

data <- mutate(data, expenditures_new = case_when(
  !is.na(expenditures_2019) ~ expenditures_2019,
  !is.na(expenditures_2018) ~ expenditures_2018, 
  !is.na(expenditures_2017) ~ expenditures_2017, 
  !is.na(expenditures_2016) ~ expenditures_2016, 
  !is.na(expenditures_2015) ~ expenditures_2015,
  !is.na(expenditures_2014) ~ expenditures_2014,
  !is.na(expenditures_2013) ~ expenditures_2013,
  !is.na(expenditures_2012) ~ expenditures_2012,
)
)

data <- select(data, region, country, income_group, mobility_2001, mobility_2019,
               workplace_2001, workplace_2019, pay_2001, pay_2019, marriage_2001,
               marriage_2019, parenthood_2001, parenthood_2019, entrepreneurship_2001,
               entrepreneurship_2019, assets_2001, assets_2019, expenditures_old,
               expenditures_new)
data <- drop_na(data)


data <- mutate(data, mobility = mobility_2019 - mobility_2001,
               workplace = workplace_2019 - workplace_2001,
               pay = pay_2019 - pay_2001,
               marriage = marriage_2019 - marriage_2001,
               parenthood = parenthood_2019 - parenthood_2001,
               entrepreneurship = entrepreneurship_2019 - entrepreneurship_2001,
               assets = assets_2019 - assets_2001,
               expenditures = expenditures_new - expenditures_old)

data <- select(data, region, country, income_group, mobility, 
               workplace, pay, marriage, parenthood, entrepreneurship, assets,
               expenditures)

## Ridge Regression
reg <- lm(expenditures ~ ., data = data)
x3 <- model.matrix(reg)
x3 <- x3[, -1]
y3 <- data$expenditures

ridge3 <- glmnet(x3, y3, alpha = 0)
plot(ridge3, label = TRUE, xvar = "lambda")

set.seed(123)
ridge3CV <- cv.glmnet(x3, y3, alpha = 0)
plot(ridge3CV)
coef(ridge3CV) |> round(2)
ridge3CV

## Lasso Regression
lasso3 <- glmnet(x3, y3)
plot(lasso3, label = TRUE, xvar = "lambda")

set.seed(123)
lasso3CV <- cv.glmnet(x3, y3)
plot(lasso3CV)
coef(lasso3CV) |> round(2)
lasso3CV

## PCA Regression
pc3 <- prcomp(x3)
screeplot(pc3)
pcreg3 <- pcr(expenditures ~ ., data = data, validation = "CV")
summary(pcreg3)
