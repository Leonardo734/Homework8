# Homework8
Homework8





---
title: "Lab 7"
output: html_document
date: "2024-7-11"
---

Question 1: 

Group memebers: Arifa Begum, Maria Camalia Vargas, Nene Diallo, Sun Wu Kim, and Leonardo Alcaide. 

```{r Question 2}
# Load libraries and dataset
library(ggplot2)
library(tidyverse)
library(haven)

load("/Users/jaydenkim/Desktop/Econometrics/ACS_2021_couples.RData")

# Recode race and Hispanic origin variables
acs2021_couples$RACE <- fct_recode(as.factor(acs2021_couples$RACE),
                                   "White" = "1",
                                   "Black" = "2",
                                   "American Indian or Alaska Native" = "3",
                                   "Chinese" = "4",
                                   "Japanese" = "5",
                                   "Other Asian or Pacific Islander" = "6",
                                   "Other race" = "7",
                                   "two races" = "8",
                                   "three races" = "9")
acs2021_couples$h_race <- fct_recode(as.factor(acs2021_couples$h_race),
                                     "White" = "1",
                                     "Black" = "2",
                                     "American Indian or Alaska Native" = "3",
                                     "Chinese" = "4",
                                     "Japanese" = "5",
                                     "Other Asian or Pacific Islander" = "6",
                                     "Other race" = "7",
                                     "two races" = "8",
                                     "three races" = "9")
acs2021_couples$HISPAN <- fct_recode(as.factor(acs2021_couples$HISPAN),
                                     "Not Hispanic" = "0",
                                     "Mexican" = "1",
                                     "Puerto Rican" = "2",
                                     "Cuban" = "3",
                                     "Other" = "4")
acs2021_couples$h_hispan <- fct_recode(as.factor(acs2021_couples$h_hispan),
                                       "Not Hispanic" = "0",
                                       "Mexican" = "1",
                                       "Puerto Rican" = "2",
                                       "Cuban" = "3",
                                       "Other" = "4")

# Filter data for female-male couples and create age difference indicator
trad_data <- acs2021_couples %>% filter((SEX == "Female") & (h_sex == "Male"))
trad_data$he_more_than_5yrs_than_her <- as.numeric(trad_data$age_diff < -5)

# Display table for age difference categories
table(trad_data$he_more_than_5yrs_than_her, cut(trad_data$age_diff, c(-100, -10, -5, 0, 5, 10, 100)))

# Map education levels to numeric values for both partners
trad_data$educ_numeric <- fct_recode(trad_data$EDUC,
                                     "0" = "N/A or no schooling",
                                     "2" = "Nursery school to grade 4",
                                     "6.5" = "Grade 5, 6, 7, or 8",
                                     "9" = "Grade 9",
                                     "10" = "Grade 10",
                                     "11" = "Grade 11",
                                     "12" = "Grade 12",
                                     "13" = "1 year of college",
                                     "14" = "2 years of college",
                                     "15" = "3 years of college",
                                     "16" = "4 years of college",
                                     "17" = "5+ years of college")
trad_data$educ_numeric <- as.numeric(levels(trad_data$educ_numeric))[trad_data$educ_numeric]

trad_data$h_educ_numeric <- fct_recode(trad_data$h_educ,
                                       "0" = "N/A or no schooling",
                                       "2" = "Nursery school to grade 4",
                                       "6.5" = "Grade 5, 6, 7, or 8",
                                       "9" = "Grade 9",
                                       "10" = "Grade 10",
                                       "11" = "Grade 11",
                                       "12" = "Grade 12",
                                       "13" = "1 year of college",
                                       "14" = "2 years of college",
                                       "15" = "3 years of college",
                                       "16" = "4 years of college",
                                       "17" = "5+ years of college")
trad_data$h_educ_numeric <- as.numeric(levels(trad_data$h_educ_numeric))[trad_data$h_educ_numeric]

# Filter rows with NA values in relevant variables
trad_data <- trad_data %>%
  filter(!is.na(educ_numeric), !is.na(h_educ_numeric), !is.na(AGE), !is.na(he_more_than_5yrs_than_her))

# Run regression models
ols_out1 <- lm(he_more_than_5yrs_than_her ~ educ_hs + educ_somecoll + educ_college + educ_advdeg + AGE, data = trad_data)
summary(ols_out1)

ols_out2 <- lm(he_more_than_5yrs_than_her ~ educ_numeric + h_educ_numeric + AGE, data = trad_data)
summary(ols_out2)

ols_out3 <- lm(he_more_than_5yrs_than_her ~ EDUC + h_educ + AGE, data = trad_data)
summary(ols_out3)

ols_out4 <- lm(he_more_than_5yrs_than_her ~ educ_numeric + h_educ_numeric + AGE + STATEFIP, data = trad_data)
summary(ols_out4)

ols_out5 <- lm(he_more_than_5yrs_than_her ~ educ_numeric + h_educ_numeric + AGE + REGION, data = trad_data)
summary(ols_out5)

# Additional model with family size and children
model_us <- lm(he_more_than_5yrs_than_her ~ educ_numeric + h_educ_numeric + AGE + FAMSIZE + NCHILD, data = trad_data)
summary(model_us)

library(ggthemes)

# Plot for AGE and NCHILD
ggplot(data = trad_data, mapping = aes(x = AGE, y = NCHILD)) + 
  geom_smooth(method = "lm") + 
  theme_economist() + 
  labs(title = 'Age & Number of Children')

# Model with REGION
shub_niggurath <- lm(he_more_than_5yrs_than_her ~ EDUC + h_educ + AGE + NCHILD + REGION, data = trad_data)
summary(shub_niggurath)

# Plot for MORTGAGE and NCHILD
ggplot(data = acs2021_couples, mapping = aes(x = MORTGAGE, y = NCHILD)) + 
  geom_smooth(method = "lm") + 
  theme_economist() + 
  labs(title = 'Mortgage and Number of Children')

# Hypothesis Test: Likelihood of children living with older vs. younger couples
# Null Hypothesis: Older couples are just as likely to have children living with them as younger couples. (NCHILD = Age)
# Alternative Hypothesis: Older couples are less likely to have children living with them. (NCHILD < Age)

# Confidence Level: 95%
# p-value: Given as 1.11e-02
# This is a left-tailed test
# Decision rule: Reject if p-value < alpha (0.05)

p_value <- 1.11e-02
alpha <- 0.05

if (p_value < alpha) {
  result <- "Reject the null hypothesis. Older couples are less likely to have children living with them than younger couples."
} else {
  result <- "Fail to reject the null hypothesis. Older couples are just as likely to have children living with them as younger couples."
}

result

```

```{r Question 1 Portion 2}
# Load necessary libraries
library(ggplot2)
library(tidyverse)
library(haven)
library(dplyr)

# Load the dataset
load("/Users/jaydenkim/Desktop/Econometrics/ACS_2021_couples.RData")

# Recode education levels to numeric for both partners
acs2021_couples$educ_numeric <- fct_recode(acs2021_couples$EDUC,
                                           "0" = "N/A or no schooling",
                                           "2" = "Nursery school to grade 4",
                                           "6.5" = "Grade 5, 6, 7, or 8",
                                           "9" = "Grade 9",
                                           "10" = "Grade 10",
                                           "11" = "Grade 11",
                                           "12" = "Grade 12",
                                           "13" = "1 year of college",
                                           "14" = "2 years of college",
                                           "15" = "3 years of college",
                                           "16" = "4 years of college",
                                           "17" = "5+ years of college")
acs2021_couples$educ_numeric <- as.numeric(levels(acs2021_couples$educ_numeric))[acs2021_couples$educ_numeric]

acs2021_couples$h_educ_numeric <- fct_recode(acs2021_couples$h_educ,
                                             "0" = "N/A or no schooling",
                                             "2" = "Nursery school to grade 4",
                                             "6.5" = "Grade 5, 6, 7, or 8",
                                             "9" = "Grade 9",
                                             "10" = "Grade 10",
                                             "11" = "Grade 11",
                                             "12" = "Grade 12",
                                             "13" = "1 year of college",
                                             "14" = "2 years of college",
                                             "15" = "3 years of college",
                                             "16" = "4 years of college",
                                             "17" = "5+ years of college")
acs2021_couples$h_educ_numeric <- as.numeric(levels(acs2021_couples$h_educ_numeric))[acs2021_couples$h_educ_numeric]

# Recode race and Hispanic origin variables
acs2021_couples$RACE <- fct_recode(as.factor(acs2021_couples$RACE),
                                   "White" = "1", "Black" = "2",
                                   "American Indian or Alaska Native" = "3",
                                   "Chinese" = "4", "Japanese" = "5",
                                   "Other Asian or Pacific Islander" = "6",
                                   "Other race" = "7", "two races" = "8",
                                   "three races" = "9")
acs2021_couples$h_race <- fct_recode(as.factor(acs2021_couples$h_race),
                                     "White" = "1", "Black" = "2",
                                     "American Indian or Alaska Native" = "3",
                                     "Chinese" = "4", "Japanese" = "5",
                                     "Other Asian or Pacific Islander" = "6",
                                     "Other race" = "7", "two races" = "8",
                                     "three races" = "9")
acs2021_couples$HISPAN <- fct_recode(as.factor(acs2021_couples$HISPAN),
                                     "Not Hispanic" = "0", "Mexican" = "1",
                                     "Puerto Rican" = "2", "Cuban" = "3",
                                     "Other" = "4")
acs2021_couples$h_hispan <- fct_recode(as.factor(acs2021_couples$h_hispan),
                                       "Not Hispanic" = "0", "Mexican" = "1",
                                       "Puerto Rican" = "2", "Cuban" = "3",
                                       "Other" = "4")

# Filter data for female-male couples and create indicator for age difference
trad_data <- acs2021_couples %>% filter((SEX == "Female") & (h_sex == "Male"))
trad_data$he_more_than_5yrs_than_her <- as.numeric(trad_data$age_diff < -5)

# Run regression models
ols_out1 <- lm(he_more_than_5yrs_than_her ~ educ_hs + educ_somecoll + educ_college + educ_advdeg + AGE, data = trad_data)
summary(ols_out1)

ols_out2 <- lm(he_more_than_5yrs_than_her ~ educ_numeric + h_educ_numeric + AGE, data = trad_data)
summary(ols_out2)

ols_out3 <- lm(he_more_than_5yrs_than_her ~ EDUC + h_educ + AGE, data = trad_data)
summary(ols_out3)

ols_out4 <- lm(he_more_than_5yrs_than_her ~ educ_numeric + h_educ_numeric + AGE + STATEFIP, data = trad_data)
summary(ols_out4)

model_us <- lm(he_more_than_5yrs_than_her ~ educ_numeric + h_educ_numeric + AGE + FAMSIZE + NCHILD, data = trad_data)
summary(model_us)

# Visualizations

# Average Age Difference by Family Size
summary_data <- trad_data %>%
  group_by(FAMSIZE) %>%
  summarise(avg_age_diff = mean(he_more_than_5yrs_than_her, na.rm = TRUE))
colors <- rep(c("purple", "pink"), length.out = nrow(summary_data))
ggplot(summary_data, aes(x = as.factor(FAMSIZE), y = avg_age_diff, fill = as.factor(FAMSIZE))) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = colors) +
  labs(title = "Average Age Difference by Family Size",
       x = "Family Size", y = "Average Age Difference (Years)") +
  theme_minimal() +
  theme(legend.position = "none")

# Predicted Probability by AGE
trad_data$predicted_prob <- predict(model_us, newdata = trad_data, type = "response")
ggplot(trad_data, aes(x = AGE, y = predicted_prob)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", color = "blue") +
  labs(title = "Predicted Probability that Male Partner is More Than 5 Years Older",
       x = "Age of Female Partner", y = "Predicted Probability") +
  theme_minimal()

# Box plot of predicted probabilities by male partner's education level
ggplot(trad_data, aes(x = as.factor(h_educ_numeric), y = predicted_prob)) +
  geom_boxplot(aes(fill = as.factor(h_educ_numeric)), outlier.shape = NA) +
  labs(title = "Predicted Probability by Male Partner's Education Level",
       x = "Education Level of Male Partner", y = "Predicted Probability") +
  theme_minimal()

# Box plot of predicted probabilities by female partner's education level
ggplot(trad_data, aes(x = as.factor(educ_numeric), y = predicted_prob)) +
  geom_boxplot(aes(fill = as.factor(educ_numeric)), outlier.shape = NA) +
  labs(title = "Predicted Probability by Female Partner's Education Level",
       x = "Education Level of Female Partner", y = "Predicted Probability") +
  theme_minimal()

# Hypothesis Test: Likelihood of children living with older vs. younger couples
# Null Hypothesis: Older couples are just as likely to have children living with them as younger couples. (NCHILD = Age)
# Alternative Hypothesis: Older couples are less likely to have children living with them. (NCHILD < Age)

# Confidence Level: 95%
# p-value: Given as 1.11e-02
# This is a left-tailed test
# Decision rule: Reject if p-value < alpha (0.05)

p_value <- 1.11e-02
alpha <- 0.05

if (p_value < alpha) {
  result <- "Reject the null hypothesis. Older couples are less likely to have children living with them than younger couples."
} else {
  result <- "Fail to reject the null hypothesis. Older couples are just as likely to have children living with them as younger couples."
}

result
```

```{r Question 1 Portion 3}

library(ggplot2)
library(tidyverse)
library(haven)
load("/Users/jaydenkim/Desktop/Econometrics/ACS_2021_couples.RData")
#view(acs2021_couples)
acs2021_couples_sample <- acs2021_couples[sample(nrow(acs2021_couples), 500), ]
#write.csv(acs2021_couples, "/Users/jaydenkim/Desktop/Econometrics/ACS_2021_couples.RData)

```

```{r}
acs2021_couples$educ_numeric <- fct_recode(acs2021_couples$EDUC,
                                           "0" = "N/A or no schooling",
                                           "2" = "Nursery school to grade 4",
                                           "6.5" = "Grade 5, 6, 7, or 8",
                                           "9" = "Grade 9",
                                           "10" = "Grade 10",
                                           "11" = "Grade 11",
                                           "12" = "Grade 12",
                                           "13" = "1 year of college",
                                           "14" = "2 years of college",
                                           "15" = "3 years of college",
                                           "16" = "4 years of college",
                                           "17" = "5+ years of college")
acs2021_couples$educ_numeric <- as.numeric(levels(acs2021_couples$educ_numeric))[acs2021_couples$educ_numeric]
acs2021_couples$h_educ_numeric <- fct_recode(acs2021_couples$h_educ,
                                           "0" = "N/A or no schooling",
                                           "2" = "Nursery school to grade 4",
                                           "6.5" = "Grade 5, 6, 7, or 8",
                                           "9" = "Grade 9",
                                           "10" = "Grade 10",
                                           "11" = "Grade 11",
                                           "12" = "Grade 12",
                                           "13" = "1 year of college",
                                           "14" = "2 years of college",
                                           "15" = "3 years of college",
                                           "16" = "4 years of college",
                                           "17" = "5+ years of college")
acs2021_couples$h_educ_numeric <- as.numeric(levels(acs2021_couples$h_educ_numeric))[acs2021_couples$h_educ_numeric]
acs2021_couples$educ_diff <- acs2021_couples$educ_numeric - acs2021_couples$h_educ_numeric
```

```{r}
acs2021_couples$RACE <- fct_recode(as.factor(acs2021_couples$RACE),
                                      "White" = "1",
                                      "Black" = "2",
                                      "American Indian or Alaska Native" = "3",
                                      "Chinese" = "4",
                                      "Japanese" = "5",
                                      "Other Asian or Pacific Islander" = "6",
                                      "Other race" = "7",
                                      "two races" = "8",
                                      "three races" = "9")
                                      
acs2021_couples$h_race <- fct_recode(as.factor(acs2021_couples$h_race),
                                   "White" = "1",
                                   "Black" = "2",
                                   "American Indian or Alaska Native" = "3",
                                   "Chinese" = "4",
                                   "Japanese" = "5",
                                   "Other Asian or Pacific Islander" = "6",
                                   "Other race" = "7",
                                   "two races" = "8",
                                   "three races" = "9")
acs2021_couples$HISPAN <- fct_recode(as.factor(acs2021_couples$HISPAN),
                                     "Not Hispanic" = "0",
                                     "Mexican" = "1",
                                     "Puerto Rican" = "2",
                                     "Cuban" = "3",
                                     "Other" = "4")
acs2021_couples$h_hispan <- fct_recode(as.factor(acs2021_couples$h_hispan),
                                     "Not Hispanic" = "0",
                                     "Mexican" = "1",
                                     "Puerto Rican" = "2",
                                     "Cuban" = "3",
                                     "Other" = "4")
trad_data <- acs2021_couples %>% filter( (SEX == "Female") & (h_sex == "Male") )
trad_data$he_more_than_5yrs_than_her <- as.numeric(trad_data$age_diff < -5)
table(trad_data$he_more_than_5yrs_than_her,cut(trad_data$age_diff,c(-100,-10, -5, 0, 5, 10, 100)))
ols_out1 <- lm(he_more_than_5yrs_than_her ~ educ_hs + educ_somecoll + educ_college + educ_advdeg + AGE, data = trad_data)
summary(ols_out1)
ols_out2 <- lm(he_more_than_5yrs_than_her ~ educ_numeric + h_educ_numeric + AGE, data = trad_data)
summary(ols_out2)
ols_out3 <- lm(he_more_than_5yrs_than_her ~ EDUC + h_educ + AGE, data = trad_data)
summary(ols_out3)
ols_out4 <-Im(he_more_than_Syrs_than_her ~ educnumeric + h_educ+ AGE, data - trad_data)
summary(ols_out4)
ols_out5 <- lm(he_more_than_5yrs_than_her ~ educ_numeric + h_educ_numeric + AGE + REGION, data = trad_data)
summary(ols_out5)
model_us <- Im(he_more_than_Syrs_than_her ~ educnumeric + h_educ_numeric + AGE +
FAMSIZE + NCHILD, data - trad_data)
summary(model_us)



Question 3.
Lissoni, Francesco, and Ernest Miguelez. “Migration and Innovation: Learning from Patent and Inventor Data.” The Journal of Economic Perspectives, vol. 38, no. 1, 2024, pp. 27–54. JSTOR, https://www.jstor.org/stable/27282173. Accessed 6 Nov. 2024. 

 This article goes over the relationship between innovation, immigrant workers and patents. The article primarily goes over the skilled workers who have a college degree and majored in stems. Most of the data is from the US Patent and Trademark Office which may not be accessible to me. The Econometrics techniques used are linear regression when determining the patents of each country, bar graphs when comparing native and immigrant workers, and points when displaying which stem major to a percentage of immigrants major.  

Costa, Daniel. “Temporary Migrant Workers or Immigrants? The Question for U.S. Labor Migration.” RSF: The Russell Sage Foundation Journal of the Social Sciences, vol. 6, no. 3, 2020, pp. 18–44. JSTOR, https://doi.org/10.7758/rsf.2020.6.3.02. Accessed 7 Nov. 2024. 

 

This article goes over the concept of temporary residents and how they are affected by labor regulation. It covers all different classification of immigrant, both legal and illegal, and what protections do they receive from US labor laws. The article then covers how bills to extend the rights of temporary workers have failed to political pressure. The data primary comes from the US Department of State which keeps track of immigrants and their classification so this information should be accessible. The Econometrics techniques by showing a linear regression for very classification for immigrants and linear graph for why they may be granted temporary status.  
