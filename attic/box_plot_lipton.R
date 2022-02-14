MOC_rf_lipton = read.csv("/Users/rifatmehreenamin/Desktop/Thesis/MOC-for-CounterfactualFairness/results/MOC_rf_lipton.csv")
MOC_lr_lipton = read.csv("/Users/rifatmehreenamin/Desktop/Thesis/MOC-for-CounterfactualFairness/results/MOC_lr_lipton.csv")
NICE_rf_lipton = read.csv("/Users/rifatmehreenamin/Desktop/Thesis/MOC-for-CounterfactualFairness/results/NICE_rf_lipton.csv")
NICE_lr_lipton= read.csv("/Users/rifatmehreenamin/Desktop/Thesis/MOC-for-CounterfactualFairness/results/NICE_lr_lipton.csv")
whatif_rf_lipton = read.csv("/Users/rifatmehreenamin/Desktop/Thesis/MOC-for-CounterfactualFairness/results/whatif_rf_lipton.csv")
whatif_lr_lipton = read.csv("/Users/rifatmehreenamin/Desktop/Thesis/MOC-for-CounterfactualFairness/results/whatif_lr_lipton.csv")

MOC_rf_lipton$model = "randomForest"
MOC_lr_lipton$model = "log_reg"
NICE_rf_lipton$model = "randomForest"
NICE_lr_lipton$model = "log_reg"
whatif_rf_lipton$model = "randomForest"
whatif_lr_lipton$model = "log_reg"

MOC_rf_lipton$method = "MOCCF"
MOC_lr_lipton$method = "MOCCF"
NICE_rf_lipton$method = "NICE"
NICE_lr_lipton$method = "NICE"
whatif_rf_lipton$method = "WhatIf"
whatif_lr_lipton$method = "WhatIf"

cal_data_lipton = rbind(MOC_rf_lipton, MOC_lr_lipton, NICE_rf_lipton, NICE_lr_lipton, whatif_rf_lipton, whatif_lr_lipton)
cal_data_lipton %>% ggplot(aes(x=method, y=MPD, fill=model)) + ylab("mean prediction difference")+ geom_boxplot() +ylim(-0.5,1) + coord_fixed()+ theme_bw(base_size = 30)

# ggplot(cal_data_lipton, aes(x = factor(instance_id), y = total_cf, fill = method)) + geom_col(width = 0.8, position = "dodge") +ylim(0,80) + xlab("instance id") + ylab("number of counterfactuals") + theme_bw(base_size = 30)
# 
# moc_t = (sum(MOC_rf_lipton$execution_time.s.))/sum(MOC_rf_lipton$total_cf)
# nice_t = (sum(NICE_rf_lipton$execution_time.s.))/sum(NICE_rf_lipton$total_cf)
# whatif_t = (sum(whatif_rf_lipton$execution_time.s.))/sum(whatif_rf_lipton$total_cf)
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
# p <- ggplot(data, aes(x = method, y = v, fill = method)) + geom_col(width = 0.3, position = "dodge") +ylim(0,15) + xlab("") + ylab("average time (s)") + theme_bw(base_size = 20)
# p + coord_flip()
# 
