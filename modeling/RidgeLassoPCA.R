library(tidyverse)
library(glmnet)
library(pls)

data <- read_csv("../data/clean_data.csv")
data <- select(data, region, income_group, year, mobility, 
                 workplace, pay, marriage, parenthood, entrepreneurship, 
                 assets, pension, total)
data <- filter(data, year == 2018, !is.na(total), !is.na(region))


### RIDGE AND LASSO
## Setting up matrices
reg <- lm(total ~ ., data = data)
x2 <- model.matrix(reg)
x2 <- x2
y2 <- data$total

## Ridge
ridge <- glmnet(x2, y2, alpha = 0)
plot(ridge, label = True, xvar = "lambda")

set.seed(123)
ridgeCV <- cv.glmnet(x2, y2, alpha = 0)
plot(ridgeCV)
coef(ridgeCV) |> round(2)
ridgeCV

## Lasso
lasso <- glmnet(x2, y2)
plot(lasso, label = True, xvar = "lambda")

set.seed(123)
lassoCV <- cv.glmnet(x2, y2)
plot(lassoCV)
coef(lassoCV, s = "lambda.min") |> round(2)
lassoCV


### PCA
pc <- prcomp(x2)
screeplot(pc)
set.seed(123)
pcreg <- pcr(total ~ ., data = data, validation = "CV")
summary(pcreg)

### Code repeated for four different years by switching out year in filtering

