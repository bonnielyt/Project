```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE)
```
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

```{r}
length(session[[1]]$brain_area)
```

```{r}
session[[1]]$spks[[1]][6,]
```

# What is the mean of the contrast left and right 

```{r}
contrast_left_mean <- mean(session[[i]]$contrast_left)
print(contrast_left_mean)

contrast_right_mean <- mean(session[[i]]$contrast_right)
print(contrast_right_mean)
```

# Connecting the neuron spike with brain region

```{r}
session[[3]]$spks[[6]][2,8]
session[[3]]$brain_area[2]
```

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
