library(tidyverse)
library(Rtsne)
library(mlr3)
library(mlr3pipelines)
library(fairness)
library(iml)
library(counterfactuals)
library(randomForest)
library(R6)
library(miesmuschel)
library(checkmate)
library(paradox)
library(data.table)
theme_set(theme_bw(18))


dataset = fairness::compas
colnames(dataset)[which(names(dataset) == "Female")] <- "Gender"
dataset <- subset(dataset, select = -c(8, 9))
print(head(dataset))

df_data = as.data.frame(dataset)
df_data["type"] = "original data"
df_data[900L, ]$type = "x_interest"

rf = randomForest(Two_yr_Recidivism ~ ., data = dataset[-900L, ])
predictor = iml::Predictor$new(rf, type = "prob")
fairness_obj = FairnessTest$new(predictor, df = dataset, column = "ethnicity", row_num = 900L, desired_class = "Caucasian")
cf_data = fairness_obj$get_difference()
cf_data = as.data.frame(cf_data)
cf_data["type"] = "counterfactuals"
cf_data <- subset(cf_data, select = -c(8, 9))

df_merged = rbind(cf_data, df_data)
df_merged <- df_merged %>% distinct()

task = TaskClassif$new("task", df_merged, "Two_yr_Recidivism")
poe = po("encode")
df_data = as.data.frame(poe$train(list(task))[[1]]$data())

df_data <- df_data %>% 
  drop_na() %>%
  mutate(ID=row_number())

recidivism_meta <- df_data %>%
  select(ID, type, Two_yr_Recidivism)

set.seed(142)
tSNE_fit <- df_data %>%
  select(where(is.numeric)) %>%
  column_to_rownames("ID") %>%
  scale() %>%
  Rtsne(check_duplicates = FALSE)


tSNE_df <- tSNE_fit$Y %>%
  as.data.frame() %>%
  rename(tSNE1="V1",
         tSNE2="V2") %>%
  mutate(ID=row_number())

tSNE_df <- tSNE_df %>%
  inner_join(recidivism_meta, by="ID")

tSNE_df %>% head()

library(ggplot2)
tSNE_df %>%
  ggplot2::ggplot(aes(x = tSNE1,
             y = tSNE2,
             color = type,
             shape = type,))+ geom_point()+ theme(legend.position="bottom")
