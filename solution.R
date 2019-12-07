#### Read packages
library(readr)
library(dplyr)
library(purrr)
library(mice)
library(assertr)
library(ggplot2)
library(relaimpo)

#### Read data
training_data <-
  read_csv('/Users/in-rmoitra/Downloads/Data Analysis Exercise/trainingData.csv')

#### Clean data ####
summary(training_data)
summary(training_data$loan_purpose)

map_df(training_data, function(x)
  sum(is.na(x)))

## Handle missing values ##
# Monthly expenses -> Mean
training_data <- training_data %>%
  mutate('monthly_expenses' = if_else(
    is.na(monthly_expenses),
    mean(training_data$monthly_expenses, na.rm = TRUE),
    monthly_expenses
  ))
summary(training_data)

# Home ownership -> Assume no home ownership ( Risk minimization )
table(training_data$home_ownership)
training_data <- training_data %>%
  mutate('home_ownership' = if_else(is.na(home_ownership), 0, home_ownership))
summary(training_data)

# Sanitary availability -> Assume no sanitary availability ( Risk minimization )
table(training_data$sanitary_availability)
training_data <- training_data %>%
  mutate('sanitary_availability' = if_else((
    is.na(sanitary_availability) |
      sanitary_availability == -1
  ), 0, sanitary_availability))
summary(training_data)

# Water availability -> Assume partial water availability ( Risk minimization by using middle ground )
table(training_data$water_availabity)
training_data <- training_data %>%
  mutate('water_availabity' = if_else((
    is.na(water_availabity) |
      water_availabity == -1
  ), 0.5, water_availabity))
summary(training_data)

# Social class -> Exclude rows
table(training_data$social_class)
training_data <- training_data %>%
  filter(!is.na(social_class))

# Primary business -> Exclude rows
table(training_data$primary_business)
training_data <- training_data %>%
  filter(!is.na(primary_business))

# Assert that there are no missing values and stop execution if any
training_data %>%
  verify(sum(is.na(training_data)) == 0)

## Handle outliers
summary(training_data)

# Age -> Filter outliers
summary(training_data$age)
training_data <- training_data %>%
  filter(age < 130)
ggplot(training_data, aes(x = age)) +
  geom_density()

# Occupants count
max_threshold_occupants_count = quantile(training_data$occupants_count, probs = 0.99965)
training_data <- training_data %>%
  filter(occupants_count < max_threshold_occupants_count)

# House area
max_threshold_house_area <-
  quantile(training_data$house_area, probs = 0.99965)
sum(training_data$house_area > max_threshold_house_area)

training_data <- training_data %>%
  filter(house_area < max_threshold_house_area)

## Collapse levels for categorical variables having large number of levels

# Social class
frequency_social_class <- training_data %>%
  group_by(social_class) %>%
  summarise(
    'count' = n(),
    freq = count / nrow(training_data),
    above_thresh = freq > 0.05
  )

training_data <- training_data %>%
  left_join(frequency_social_class, by = 'social_class') %>%
  mutate('social_class' = if_else(above_thresh == TRUE, social_class, "Other")) %>%
  dplyr::select(-c(count, freq, above_thresh))

# Loan Purpose
frequency_loan_purpose <- training_data %>%
  group_by(loan_purpose) %>%
  summarise(
    'count' = n(),
    freq = count / nrow(training_data),
    above_thresh = freq > 0.05
  )

training_data <- training_data %>%
  left_join(frequency_loan_purpose, by = 'loan_purpose') %>%
  mutate('loan_purpose' = if_else(above_thresh == TRUE, loan_purpose, "Other")) %>%
  dplyr::select(-c(count, freq, above_thresh))

## Change applicable columns to categorical data ##
training_data$sex <- as.factor(training_data$sex)
training_data$social_class <- as.factor(training_data$social_class)
training_data$primary_business <-
  as.factor(training_data$primary_business)
training_data$secondary_business <-
  as.factor(training_data$secondary_business)
training_data$loan_purpose <- as.factor(training_data$loan_purpose)
training_data$home_ownership <-
  as.factor(training_data$home_ownership)
training_data$sanitary_availability <-
  as.factor(training_data$sanitary_availability)
training_data$water_availabity <-
  as.factor(training_data$water_availabity)

#### Train test split ####

## 75% of the sample size
smp_size <- floor(0.75 * nrow(training_data))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(training_data)), size = smp_size)

train <- training_data[train_ind,]
test <- training_data[-train_ind,]

#### Feature selection ####
fit2 = lm(
  loan_amount ~ age
  + sex
  + social_class
  + annual_income
  + monthly_expenses
  + home_ownership
  + occupants_count
  + house_area
  + sanitary_availability
  + water_availabity
  + loan_purpose
  + loan_tenure
  + loan_installments
  ,
  data = train
)

# calculate relative importance
relImportance <- calc.relimp(fit2, type = "lmg", rela = F)
cat('Relative Importances: \n')
sort(round(relImportance$lmg, 3), decreasing = TRUE)

#### Fit model using relevant features ####
final_linear_model = lm(
  loan_amount ~
    water_availabity +
    loan_tenure +
    social_class +
    annual_income +
    loan_purpose +
    occupants_count +
    sanitary_availability, 
  data = train
)
summary(final_linear_model)

predictions <- predict(final_linear_model, newdata = test)
test_rmse <- rmse(predictions, test$loan_amount)

test$predictions <- predictions

#### Visualize relationships ####
plot(loan_amount~water_availabity, train)
plot(loan_amount~loan_tenure, train)
plot(loan_amount~social_class, train)
plot(loan_amount~annual_income, train)
plot(loan_amount~loan_purpose, train)
plot(loan_amount~occupants_count, train)
plot(loan_amount~sanitary_availability, train)

# Check fit for loan tenure
fit_tenure = lm(loan_amount ~ poly(loan_tenure, 4), data = train)
points(train$loan_tenure,fitted(fit_tenure),col="red",pch=20)
summary(fit_tenure)
