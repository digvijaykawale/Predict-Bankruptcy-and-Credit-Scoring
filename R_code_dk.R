# Logistic Regression 

## Creating Index Column

index <- seq(1,500, 1)

set.seed(13437586)
x1 <- runif(500, min = 0, max = 1)
x2 <- rep(c(1,0), 250)


## Actual Probabilities using the given formula

p_actual <- (exp(-1.1 + 5*x1 - 0.4*x2))/(1 + exp(-1.1 + 5*x1 - 0.4*x2))

df <- data.frame(x1 = x1, x2 = x2, p_actual = p_actual)

## Generating the Response variable using the above probabilities 

df$y_actual <- rbinom(500, 1, df$p_actual)


## Fitting a General Model 

model_1 <- glm(y_actual ~ x1 + x2, data = df, family = binomial)
summary(model_1)


df$p_predict <- predict(model_1, type = "response")

df$y_predict <- ifelse(df$p_predict < 0.5, 0, 1)

xtabs(~y_actual + y_predict, df)



library(pROC)
roc_obj <- roc(df$y_actual, df$y_predict)


##############################################################################################

#Probit Regression 

y_probit_vector <- (-1.1 + 5*x1 - 0.4*x2)


p_probit_actual <- pnorm(y_probit_vector)

y_1_actual <- rbinom(500, 1, p_probit_actual)

df_1 <- data.frame(y_1_actual = y_1_actual, x1 = x1, x2 = x2)

## Fitting a General Model 

model_2 <- glm(y_1_actual ~ x1 + x2, data = df_1, family = binomial("probit"))

summary(model_2)

df_1$p_probit_predict <- predict(model_2, type = "response")

df_1$y_1_predict <- ifelse(df_1$p_probit_predict < 0.5, 0, 1)

xtabs(~y_1_actual + y_1_predict, df_1)


roc_obj_1 <- roc(df_1$y_1_actual, df_1$y_1_predict)















