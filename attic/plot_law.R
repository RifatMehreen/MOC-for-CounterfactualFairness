MOC_rf_law = read.csv("/Users/rifatmehreenamin/Desktop/Thesis/MOC-for-CounterfactualFairness/results/MOC_rf_law.csv")
MOC_lr_law = read.csv("/Users/rifatmehreenamin/Desktop/Thesis/MOC-for-CounterfactualFairness/results/MOC_lr_law.csv")
NICE_rf_law = read.csv("/Users/rifatmehreenamin/Desktop/Thesis/MOC-for-CounterfactualFairness/results/NICE_rf_law.csv")
NICE_lr_law= read.csv("/Users/rifatmehreenamin/Desktop/Thesis/MOC-for-CounterfactualFairness/results/NICE_lr_law.csv")
whatif_rf_law = read.csv("/Users/rifatmehreenamin/Desktop/Thesis/MOC-for-CounterfactualFairness/results/whatif_rf_law.csv")
whatif_lr_law = read.csv("/Users/rifatmehreenamin/Desktop/Thesis/MOC-for-CounterfactualFairness/results/whatif_lr_law.csv")

MOC_rf_law$model = "randomForest"
MOC_lr_law$model = "log_reg"
NICE_rf_law$model = "randomForest"
NICE_lr_law$model = "log_reg"
whatif_rf_law$model = "randomForest"
whatif_lr_law$model = "log_reg"

MOC_rf_law$method = "MOCCF"
MOC_lr_law$method = "MOCCF"
NICE_rf_law$method = "NICE"
NICE_lr_law$method = "NICE"
whatif_rf_law$method = "WhatIf"
whatif_lr_law$method = "WhatIf"

cal_data_law = rbind(MOC_rf_law, MOC_lr_law, NICE_rf_law, NICE_lr_law, whatif_rf_law, whatif_lr_law)
#cal_data_law %>% ggplot(aes(x=method, y=MPD, fill=model)) + geom_boxplot() +ylim(-0.5,1)+ylab("mean prediction difference") +coord_fixed()+ theme_bw(base_size = 30)

#ggplot(cal_data_law, aes(x = factor(instance_id), y = total_cf, fill = method)) + geom_col(width = 0.8, position = "dodge") +ylim(0,155) + xlab("instance id") + ylab("number of counterfactuals") + theme_bw(base_size = 30)


# moc_t = (sum(MOC_rf_law$execution_time.s.))/sum(MOC_rf_law$total_cf)
# nice_t = (sum(NICE_rf_law$execution_time.s.))/sum(NICE_rf_law$total_cf)
# whatif_t = (sum(whatif_rf_law$execution_time.s.))/sum(whatif_rf_law$total_cf)
# 
# v = c(moc_t, nice_t, whatif_t)
# method = c("MOCCF", "NICE", "WhatIf")
# 
# data <- data.frame(
#   name=method ,
#   value=v
# )
# 
# #p <- ggplot(data, aes(x = method, y = v)) + geom_col(width = 0.8, position = "dodge") +ylim(0,400) + ylab("time (s)") + theme_bw(base_size = 20)
# p <- ggplot(data, aes(x = method, y = v, fill = method)) + geom_col(width = 0.3, position = "dodge") +ylim(0,55) + xlab("") + ylab("average time (s)") + theme_bw(base_size = 20)
# p + coord_flip()

