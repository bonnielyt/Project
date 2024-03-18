---
title: "Project report"
author: "Bonnie Leung"
date: "2024-02-19"
output: html_document
---
```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE)
```

# Mice's visual cortex experiment

# Abstract

This experiment tests mice's neuron activity inside the different parts of the brain; this is done by giving the mice choices to choose left or right side.

The following is how the experiment outcomes being determined:

- If left contrast > right contrast, it is success if the mice turn the wheel to the right

- If right contrast > left contrast, it is success if the mice turn the wheel to the left. 

- If both left and right contrasts are zero, it is success if the mice hold the wheel still. 

- When left and right contrasts are equal but non-zero, left or right will be randomly chose. 

This experiment displays that nearly all neurons responded when the mice made a move. The neurons that encode visual stimuli and upcoming choices are found in the neocortex, basal ganglia, and midbrain. The neurons that are responsible for making decisions were rare and emerged at the same time. The midbrain neurons were activated before contralateral choices, and the forebrain could prefer either side. The study also shows that the researcher can predict if the mice are being engaged before the experiment starts; if they are engaged, they will have enhanced subcortical but suppressed neocortical activity. 

# Introduction

In this report, I will be building a predictive model to predict how many successes and failures the mice gained, and display what type and how many neurons got activated when the mice is succeeding or failing, and show the connection between the neurons and their decision. There are five variables (feedback_type (success or failure), contrast_left (left stimulus contrast), contrast_right (right stimulus contrast), time (center of the time bins for spks), spks (number of spikes of neurons), and brain_area (the brain area that the neurons live)) for each trial in this experiment and I will be using them to help me to build the model.

# Exploratory analysis

# Variables in a session

```{r}
setwd("/Users/bonniel/Desktop/sessions")
knitr::opts_knit$set(root.dir = "/Users/bonniel/Desktop/sessions")
session <- list()
for(i in 1:18){
  session[[i]] <- readRDS(paste('session', i, '.rds', sep = ''))
}
print(names(session[[i]]))
summary(session[[18]])
```

# (i) Describe the data structures across sessions

# Date and names they did the experiment

```{r}
setwd("/Users/bonniel/Desktop/sessions")
session <- list()
for(i in 1:18){
  session[[i]] <- readRDS(paste('session', i, '.rds', sep = ''))
  print(session[[i]]$date_exp)
  print(session[[i]]$mouse_name)
}
```

# What's in a trial?

```{r}
dim(session[[1]]$spks[[1]])
```
Spks has 734 rows and 40 columns in session 1, trial 1.

```{r}
length(session[[1]]$brain_area)
```
Brain_area has 734 elements.

```{r}
session[[1]]$spks[[1]][6,]
```
1 represent if there is a spike and 0 represent there are no spike in session 1, trial 1.

# What is the mean of the contrast left and right 

```{r}
contrast_left_mean <- mean(session[[i]]$contrast_left)
print(contrast_left_mean)

contrast_right_mean <- mean(session[[i]]$contrast_right)
print(contrast_right_mean)
```

Contrast left and contrast right mean have similar outcomes, meaning the stimuli conditions are similar between two contrasts.

# Connecting the neuron spike with brain region

```{r}
session[[3]]$spks[[6]][2,8]
session[[3]]$brain_area[2]
```

This shows that in session 3, trial 6, the 2 neuron from area VISam does not have a spike at time bin 8.

# (ii) Explore the neural activities during each trial
  
```{r}
library(dplyr)
library(tibble)

get_trial_data <- function(session_id, trial_id) {
  spikes <- session[[session_id]]$spks[[trial_id]]
  if (any(is.na(spikes))) {
    print("value missing")
  }

  trial_tibble <- tibble(
    neuron_spike = rowSums(spikes),
    brain_area = session[[session_id]]$brain_area,
    trial_id = trial_id, 
    contrast_left = session[[session_id]]$contrast_left[trial_id],
    contrast_right = session[[session_id]]$contrast_right[trial_id],
    feedback_type = session[[session_id]]$feedback_type[trial_id]
  )
  
  aggregated_tibble <- trial_tibble %>%
    group_by(brain_area, trial_id) %>%
    summarize(
      region_sum_spike = sum(neuron_spike),
      region_count = n(),
      region_mean_spike = mean(neuron_spike),
      .groups = 'drop'
    ) %>%
    
    mutate(
      contrast_left = unique(trial_tibble$contrast_left),
      contrast_right = unique(trial_tibble$contrast_right),
      feedback_type = unique(trial_tibble$feedback_type)
    )
  
  return(aggregated_tibble)
}
```

```{r}
trial_tibble_1_2 <- get_trial_data(1,2)
trial_tibble_1_2 
```


```{r}
library(dplyr)
library(tibble)

get_session_data <- function(session_id) {
  all_trials_data <- lapply(seq_along(session[[session_id]]$spks), function(trial_id) {
    get_trial_data(session_id, trial_id)
  })
  
  session_data <- bind_rows(all_trials_data)
  
  session_data <- session_data %>%
    mutate(
      mouse_name = session[[session_id]]$mouse_name, 
      date_exp = session[[session_id]]$date_exp,     
      session_id = session_id                        
    )
  
  return(session_data)
}
```

# The data below is the neuron activity across the first 3 sessions.

```{r}
session_1 <- get_session_data(1)
head(session_1)
session_2 <- get_session_data(2)
head(session_2)
session_3 <- get_session_data(3)
head(session_3)
session_4 <- get_session_data(4)

session_5 <- get_session_data(5)

session_6 <- get_session_data(6)

session_7 <- get_session_data(7)

session_8 <- get_session_data(8)

session_9 <- get_session_data(9)

session_10 <- get_session_data(10)

session_11 <- get_session_data(11)

session_12 <- get_session_data(12)

session_13 <- get_session_data(13)

session_14 <- get_session_data(14)

session_15 <- get_session_data(15)

session_16 <- get_session_data(16)

session_17 <- get_session_data(17)

session_18 <- get_session_data(18)

```

# (iii) Explore the changes across trials.

# Success rate change across trials

```{r}
library(ggplot2)
library(dplyr)

session_data_list <- list(session_1, session_2, session_3, session_4, session_5, session_6, session_7, session_8, session_9, session_10, session_11, session_12, session_13, session_14, session_15, session_16, session_17, session_18)

all_session_data <- bind_rows(session_data_list) 
success_rate <- all_session_data %>%
  mutate(trial_bin = ceiling(trial_id / 25)) %>%
  group_by(session_id, trial_bin) %>%
  summarise(success = mean(feedback_type == 1), .groups = 'drop')

ggplot(success_rate, aes(x = trial_bin, y = success)) +
  geom_bar(stat = 'identity') + 
  facet_wrap(~ session_id, scales = 'free_x', ncol = 6) +
  theme_minimal() +
  labs(x = "Trial Group", y = "Success Rate", title = "Success Rate Change Over Time Across Trials") +
  scale_y_continuous(breaks = seq(0, 1, by = 0.25)) 

all_session_data <- bind_rows(session_data_list) 
success_rate <- all_session_data %>%
  mutate(trial_bin = ceiling(trial_id / 25)) %>%
  group_by(mouse_name, trial_bin) %>%
  summarise(success = mean(feedback_type == 1), .groups = 'drop')
ggplot(success_rate, aes(x = trial_bin, y = success)) +
  geom_bar(stat = 'identity') + 
  facet_wrap(~ mouse_name, scales = 'free_x', ncol = 2) +
  theme_minimal() +
  labs(x = "Trial Group", y = "Success Rate", title = "Success Rate Change Over Time Across Trials") +
  scale_y_continuous(breaks = seq(0, 1, by = 0.25)) 
```

# Overall neuron spike rate change over time

```{r}
library(dplyr)
library(ggplot2)

combined_data <- bind_rows(session_1, session_2, session_3, session_4, session_5, session_6, session_7, session_8, session_9, session_10, session_11, session_12, session_13, session_14, session_15, session_16, session_17, session_18)
mean_spike_rate <- combined_data %>%
  group_by(session_id, trial_id) %>%
  summarize(mean_spike = mean(region_mean_spike, na.rm = TRUE)) %>%
  ungroup()

sessionplot <- ggplot(mean_spike_rate, aes(x = trial_id, y = mean_spike)) +
  geom_line() +
  geom_smooth(color = "blue", se = FALSE) +
  facet_wrap(~ session_id, nrow = 3) + 
  theme_minimal() +
  labs(x = "Trial ID", y = "Mean Spike Rate")

print(sessionplot)

mean_spike_rate <- combined_data %>%
  group_by(mouse_name, trial_id) %>%
  summarize(mean_spike = mean(region_mean_spike, na.rm = TRUE)) %>%
  ungroup()
mouseplot <- ggplot(mean_spike_rate, aes(x = trial_id, y = mean_spike)) +
  geom_line() +
  geom_smooth(color = "blue", se = FALSE) +
  facet_wrap(~ mouse_name, nrow = 3) +  
  theme_minimal() +
  labs(x = "Trial ID", y = "Mean Spike Rate")
print(mouseplot)
```

# Dimension Reduction through PCA for spks

```{r}
library(tidyverse)
library(ggplot2)
combined_data <- bind_rows(session_1, session_2, session_3, session_4, session_5, session_6, session_7, session_8, session_9, session_10, session_11, session_12, session_13, session_14, session_15, session_16, session_17, session_18)
pca_results <- prcomp(combined_data[,c("region_count", "region_sum_spike", "region_mean_spike")], scale. = TRUE)
pca_data <- as.data.frame(pca_results$x)
pca_data$session_id <- combined_data$session_id
ggplot(pca_data, aes(x = PC1, y = PC2, color = as.factor(session_id))) +
  geom_point() +
  theme_minimal() +
  labs(color = 'session_id', title = 'PCA: PC1 vs PC2', x = 'PC1', y = 'PC2')
```
For mice:

```{r}
pca_results <- prcomp(combined_data[,c("region_count", "region_sum_spike", "region_mean_spike")], scale. = TRUE)
pca_data <- as.data.frame(pca_results$x)
pca_data$mouse_name <- combined_data$mouse_name
ggplot(pca_data, aes(x = PC1, y = PC2, color = as.factor(mouse_name))) +
  geom_point() +
  theme_minimal() +
  labs(color = 'mouse_name', title = 'PCA: PC1 vs PC2', x = 'PC1', y = 'PC2')
```

The two plots appear similarly. It has a positive correlation since all vectors points the same direction.

# (iv) Explore homogeneity and heterogeneity across sessions and mice

# How does the contrast difference affect the success rate?
```{r}
library(dplyr)

all_session_data <- all_session_data %>%
  mutate(contrast_diff = contrast_right - contrast_left)

success_rate_by_contrast_diff <- all_session_data %>%
  group_by(contrast_diff) %>%
  summarise(success_rate = mean(feedback_type == 1, na.rm = TRUE)) %>%
  arrange(contrast_diff)

filtered_results <- success_rate_by_contrast_diff %>%
  filter(contrast_diff %in% c(0, 0.25, 0.5, 0.75, 1))

# Display the results
print(filtered_results)
```



# Does the success rate difference among mice caused by the different distributions of contrast difference?

```{r}
all_session_data$mouse_name <- as.factor(all_session_data$mouse_name)
all_session_data$contrast_diff <- as.factor(all_session_data$contrast_diff)

anova_result <- aov(feedback_type == 1 ~ mouse_name * contrast_diff, data = all_session_data)

summary(anova_result)

```

# The number of neuron's region by session and mice

```{r}
library(dplyr)

region_count_per_session <- all_session_data %>%
  group_by(session_id) %>%
  summarise(number_of_regions = n_distinct(brain_area)) %>%
  arrange(session_id) 

print(region_count_per_session)
```

```{r}
library(dplyr)

distinct_regions_per_mouse <- all_session_data %>%
  group_by(mouse_name) %>%
  summarise(distinct_regions = n_distinct(brain_area)) %>%
  ungroup() 

print(distinct_regions_per_mouse)
```

# Average spike rate by sessions and mice

```{r}
library(dplyr)

all_session_data <- bind_rows(session_data_list) 

average_spike_rate_per_session <- all_session_data %>%
  group_by(session_id) %>%
  summarize(average_spike_rate = mean(region_mean_spike, na.rm = TRUE), .groups = 'drop')

print(average_spike_rate_per_session)
```

```{r}
library(dplyr)

all_session_data <- bind_rows(session_data_list) 

average_spike_rate_by_mouse <- all_session_data %>%
  group_by(mouse_name) %>%
  summarize(average_spike_rate = mean(region_mean_spike, na.rm = TRUE), .groups = 'drop')

print(average_spike_rate_by_mouse)
```

# Brain area by sesssion and mice

```{r}
library(dplyr)

brain_area_count_per_session <- all_session_data %>%
  group_by(session_id) %>%
  summarise(number_of_brain_areas = n_distinct(brain_area)) %>%
  ungroup() 


print(brain_area_count_per_session)
```

```{r}
library(dplyr)

brain_area_count_per_mouse <- all_session_data %>%
  group_by(mouse_name) %>%
  summarise(number_of_brain_areas = n_distinct(brain_area)) %>%
  ungroup() 

print(brain_area_count_per_mouse)
```

# Success rate by sessions and mice

```{r}
library(dplyr)

success_rate_per_session <- all_session_data %>%
  group_by(session_id) %>%
  summarise(success_rate = mean(feedback_type == 1, na.rm = TRUE))
  
print(success_rate_per_session)
```

```{r}
library(dplyr)

success_rate_per_mouse <- all_session_data %>%
  group_by(mouse_name) %>%
  summarise(success_rate = mean(feedback_type == 1, na.rm = TRUE)) %>%
  ungroup()

print(success_rate_per_mouse)
```

# What are the brain areas with neurons recorded in each session?
```{r}
library(dplyr)

brain_areas_per_session <- all_session_data %>%
  group_by(session_id, brain_area) %>%
  summarise(neuron_count = n(), .groups = 'drop')
library(ggplot2)

ggplot(brain_areas_per_session, aes(x = factor(session_id), y = brain_area)) +
  geom_point() +
  theme_minimal() +
  labs(x = "Session ID", y = "Brain Area", title = "Brain Areas with Neurons Recorded by Session") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
```

# Data integration

# Extract the shared patters across sessions

```{r}
library(tibble)
full_functional_tibble <- all_session_data

number_of_time_bins <- 40  

for (i in 1:number_of_time_bins) {
  bin_name <- paste0("bin", i)
  full_functional_tibble[[bin_name]] <- runif(nrow(full_functional_tibble), min = 0, max = 1)
}

full_functional_tibble <- full_functional_tibble %>%
  mutate(contrast_diff = abs(contrast_right - contrast_left))
binename <- paste0("bin", 1:number_of_time_bins)
predictive_feature <- c("session_id","trial_id","contrast_right","contrast_left", "contrast_diff", binename)

aggregated_data <- full_functional_tibble %>%
  group_by(session_id, trial_id) %>%
  summarize(
    across(starts_with("bin"), mean, na.rm = TRUE),  
    contrast_right = first(contrast_right),          
    contrast_left = first(contrast_left),            
    contrast_diff = first(contrast_diff)
  )
aggregated_data <- aggregated_data %>%
  group_by(session_id) %>%
  mutate(trial_id = row_number()) %>%
  ungroup()
head(aggregated_data[predictive_feature])
```

# Prediction model

# Train the model on 80% trials and test it on the rest
```{r}
options(repos = c(CRAN = "https://cloud.r-project.org/"))

library(xgboost)
library(caret)
library(dplyr)

label <- session_2$feedback_type
label <- ifelse(label == -1, 0, 1)

predictive_dat <- session_2

features_df <- session_2[, setdiff(colnames(session_2), "feedback_type")]
features_df <- data.frame(lapply(features_df, function(x) if(is.character(x)) as.numeric(as.factor(x)) else x))
X <- as.matrix(features_df)
set.seed(123) 

trainIndex <- createDataPartition(label, p = .8, 
                                  list = FALSE, 
                                  times = 1)
train_df <- predictive_dat[trainIndex, ]
train_X <- X[trainIndex,]
test_df <- predictive_dat[-trainIndex, ]
test_X <- X[-trainIndex,]

train_label <- label[trainIndex]
test_label <- label[-trainIndex]

if (!is.numeric(train_X)) {
  train_X <- as.matrix(as.numeric(as.matrix(train_X)))
}
numeric_columns <- sapply(session_2, is.numeric)
numeric_columns["feedback_type"] <- FALSE
xgb_model <- xgboost(data = train_X, label = train_label, objective = "binary:logistic", nrounds=10)
```

```{r}
predictions <- predict(xgb_model, newdata = test_X)
predicted_labels <- as.numeric(ifelse(predictions > 0.5, 1, 0))
accuracy <- mean(predicted_labels == test_label)
accuracy
```

```{r}
conf_matrix <- confusionMatrix(as.factor(predicted_labels), as.factor(test_label))
conf_matrix$table
```

```{r}
install.packages("pROC")
library(pROC)
auroc <- roc(test_label, predictions)
auroc
```

# Test the model's performance on 100 random trials from Session 1

```{r}
setwd("/Users/bonniel/Desktop/test")
knitr::opts_knit$set(root.dir = "/Users/bonniel/Desktop/test")
test <- list()
for(i in 1:2){
  test[[i]] <- readRDS(paste('test', i, '.rds', sep = ''))
}
test1_data <- readRDS("/Users/bonniel/Desktop/test/test1.rds")
test2_data <- readRDS("/Users/bonniel/Desktop/test/test2.rds")
test1_data$brain_area <- test1_data$brain_area[1:100]
test1 <- as_tibble(test1_data)
test2_data$brain_area <- test2_data$brain_area[1:100]
test2 <- as_tibble(test1_data)
```

# For test 1

```{r}
library(xgboost)
library(caret)
library(dplyr)

test1data <- as.matrix(test1$feedback_type)
dtest1 <- xgb.DMatrix(data = test1data)
predictions <- predict(xgb_model, newdata = dtest1)
predicted_labels <- as.numeric(predictions > 0.5)
accuracy <- mean(predicted_labels == test_label)
accuracy
```

# For test 2

```{r}
library(xgboost)
library(caret)
library(dplyr)

test2 <- as.matrix(test2$feedback_type)
str(test2)
dtest2 <- xgb.DMatrix(data = test2)
predictions <- predict(xgb_model, newdata = dtest1)
predicted_labels <- as.numeric(predictions > 0.5)
accuracy <- mean(predicted_labels == test_label)
accuracy
```

# Discussion

Both prediction are 0.5976, which is not high enough. My goal is to hit 0.8. This project shows that the neuron inside the mice brain does highly affect their decision. We can also see that neurons appear in different part of the brain during each sessions. Through the data I provided in Part 1; for example, average_spike_rate by session, we notice that they vary a lot where they can be very different through sessions. 


# Reference
OpenAI assisted me with the coding of the graphs in part 1, exploratory data analysis. Also guide me on how to find the specific variable like full_functional_tibble in part 2, data integration. They also helped me fix the code when I got error message in Part 3. 

