
library(shiny)

## load in libraries ###########################################################
library(tidyverse)
library(VGAM)

## make functions ##############################################################
abr_columns <- function(data, valid, column) {
  
  # Tidy eval capture of column name
  column_quo <- ensym(column)
  
  # Regex pattern with word boundaries
  pattern <- str_c("\\b(", str_c(valid, collapse = "|"), ")\\b")
  
  # Perform transformations
  data %>%
    mutate(
      # Convert to lowercase for matching
      temp_col = str_to_lower(!!column_quo),
      
      # Step 1: If more than one valid value appears, set to "other"
      temp_col = if_else(str_count(temp_col, pattern) > 1, "other", temp_col),
      
      # Step 2: If there's at least one valid value, extract and keep it
      temp_col = if_else(str_detect(temp_col, pattern), str_extract(temp_col, pattern), temp_col),
      
      # Step 3: If no valid value was found at all, set to "other"
      temp_col = if_else(str_detect(temp_col, pattern), temp_col, "other"),
      
      # Final result replaces original column
      !!column_quo := temp_col
    ) %>%
    select(-temp_col)  # clean up temp column
}

get_prediction <- function(model, data) {
  
  probs <- predict(model, data, type = "response")
  predicts <- apply(probs, 1, which.max)
  accuracy <- mean(reduced_data$refined_class[-index] == predicts)
  return(round(accuracy, 4)*100)
  
}

mse <- function(model, data) {
  
  probs <- predict(model, data, type = "response")
  predicts <- apply(probs, 1, which.max)
  mse <- sqrt(mean((reduced_data$refined_class[-index] - predicts)^2))
  return(round(mse, 2))
  
}

race_transform <- function(race) {
  new_race <- ifelse(race == "human", "Human",
                     ifelse(race == "elf", "Elf",
                            ifelse(race %in% c("tiefling", "dragonborn"), "Monster",
                                   ifelse(race %in% c("gnome", "dwarf", "halfling"), "Short",
                                          ifelse(race %in% c("half-orc", "half-elf"), "Hybrid", 
                                                 ifelse(race == "other", "Other", NA))))))
  return(new_race)
}

background_transform <- function(background) {
  new_background <- ifelse(background %in% c("charlatan", "criminal"), "Shady",
                           ifelse(background %in% c("sailor", "guild artisan", "entertainer", "soldier"), "Industrial",
                                  ifelse(background %in% c("hermit", "urchin", "outlander"), "Loner",
                                         ifelse(background %in% c("noble", "folk hero"), "Famous",
                                                ifelse(background %in% c("sage", "acolyte"), "Magical", 
                                                       ifelse(background == "other", "Other", NA))))))
  return(new_background)
}

class_transform <- function(class) {
  new_class <- ifelse(class %in% c("rogue", "barbarian", "fighter"), 1,
                      ifelse(class %in% c("monk", "ranger", "paladin"), 2, 
                             ifelse(class %in% c("bard", "cleric", "druid", "warlock"), 3,
                                    ifelse(class %in% c("sorcerer", "wizard"), 4, NA))))
  return(new_class)
}

## start some sweet analysis ###################################################

## read in polished data
data <- read.csv("final_data.csv")
data <- data[data$level > 0, ]
data <- data[data$hp >= 0, ]

## due to computation difficulties, lets summarize this data
data$refined_class <- ifelse(data$class %in% c("rogue", "barbarian", "fighter"), 1,
                             ifelse(data$class %in% c("monk", "ranger", "paladin"), 2, 
                                    ifelse(data$class %in% c("bard", "cleric", "druid", "warlock"), 3,
                                           ifelse(data$class %in% c("sorcerer", "wizard"), 4, NA))))
data$refined_race <- ifelse(data$race == "human", "Human",
                            ifelse(data$race == "elf", "Elf",
                                   ifelse(data$race %in% c("tiefling", "dragonborn"), "Monster",
                                          ifelse(data$race %in% c("gnome", "dwarf", "halfling"), "Short",
                                                 ifelse(data$race %in% c("half-orc", "half-elf"), "Hybrid", 
                                                        ifelse(data$race == "other", "Other", NA))))))
data$refined_background <- ifelse(data$background %in% c("charlatan", "criminal"), "Shady",
                                  ifelse(data$background %in% c("sailor", "guild artisan", "entertainer", "soldier"), "Industrial",
                                         ifelse(data$background %in% c("hermit", "urchin", "outlander"), "Loner",
                                                ifelse(data$background %in% c("noble", "folk hero"), "Famous",
                                                       ifelse(data$background %in% c("sage", "acolyte"), "Magical", 
                                                              ifelse(data$background == "other", "Other", NA))))))

## now drop anything in an other category to have less data
reduced_data <- data[data$race != "other" & data$background != "other", ]
set.seed(2007)
index <- sample(1:nrow(reduced_data), size = 20000)
new_data <- reduced_data[index, ]
new_data$refined_background <- as.factor(new_data$refined_background)
new_data$refined_race <- as.factor(new_data$refined_race)
test_data <- reduced_data[-index, c(1:7,11,13:14)]

## continuation categories
contratio_fit <- vglm(refined_class ~ hp + strength + dex +
                        const + intelligence + wisdom + 
                        charisma + level +
                        refined_race + refined_background, 
                      sratio,
                      data = new_data)



saveRDS(contratio_fit, "contratio_model.RDS")












