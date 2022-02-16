MOC_rf_adult = read.csv("/Users/rifatmehreenamin/Desktop/Thesis/MOC-for-CounterfactualFairness/results/MOC_rf_adult.csv")
MOC_lr_adult = read.csv("/Users/rifatmehreenamin/Desktop/Thesis/MOC-for-CounterfactualFairness/results/MOC_lr_adult.csv")
NICE_rf_adult = read.csv("/Users/rifatmehreenamin/Desktop/Thesis/MOC-for-CounterfactualFairness/results/NICE_rf_adult.csv")
NICE_lr_adult = read.csv("/Users/rifatmehreenamin/Desktop/Thesis/MOC-for-CounterfactualFairness/results/NICE_lr_adult.csv")
whatif_rf_adult = read.csv("/Users/rifatmehreenamin/Desktop/Thesis/MOC-for-CounterfactualFairness/results/whatif_rf_adult.csv")
whatif_lr_adult = read.csv("/Users/rifatmehreenamin/Desktop/Thesis/MOC-for-CounterfactualFairness/results/whatif_lr_adult.csv")

MOC_rf_adult$model = "randomForest"
MOC_lr_adult$model = "log_reg"
NICE_rf_adult$model = "randomForest"
NICE_lr_adult$model = "log_reg"
whatif_rf_adult$model = "randomForest"
whatif_lr_adult$model = "log_reg"

MOC_rf_adult$method = "MOCCF"
MOC_lr_adult$method = "MOCCF"
NICE_rf_adult$method = "NICE"
NICE_lr_adult$method = "NICE"
whatif_rf_adult$method = "WhatIf"
whatif_lr_adult$method = "WhatIf"


cal_data_adult = rbind(MOC_rf_adult, MOC_lr_adult, NICE_rf_adult, NICE_lr_adult, whatif_rf_adult, whatif_lr_adult)
cal_data_adult %>% ggplot(aes(x=method, y=MPD, fill=model)) + geom_boxplot()+ ylab("mean prediction difference") +ylim(-0.5,1) +coord_fixed()+ theme_bw(base_size = 30)

# ggplot(cal_data_adult, aes(x = factor(instance_id), y = total_cf, fill = method)) + geom_col(width = 0.8, position = "dodge") +ylim(0,50) + xlab("instance id") + ylab("number of counterfactuals") + theme_bw(base_size = 30)
# 
# moc_t = sum(MOC_rf_adult$execution_time.s.)
# nice_t = sum(NICE_rf_adult$execution_time.s.)
# whatif_t = sum(whatif_rf_adult$execution_time.s.)
# 
# v = c(moc_t, nice_t, whatif_t)
# method = c("MOCCF", "NICE", "WhatIf")
# 
# data <- data.frame(
#   name=method ,
#   value=v
# )
# 
# p <- ggplot(data, aes(x = method, y = v)) + geom_col(width = 0.8, position = "dodge") +ylim(0,600) + ylab("time (s)") + theme_bw(base_size = 20)
# #p <- ggplot(data, aes(x = method, y = v, fill = method)) + geom_col(width = 0.3, position = "dodge") +ylim(0,15) + xlab("") + ylab("average time (s)") + theme_bw(base_size = 20)
# p + coord_flip()


