MOC_rf_compas = read.csv("/Users/rifatmehreenamin/Desktop/Thesis/MOC-for-CounterfactualFairness/results/MOC_rf_compas.csv")
MOC_lr_compas = read.csv("/Users/rifatmehreenamin/Desktop/Thesis/MOC-for-CounterfactualFairness/results/MOC_lr_compas.csv")
NICE_rf_compas = read.csv("/Users/rifatmehreenamin/Desktop/Thesis/MOC-for-CounterfactualFairness/results/NICE_rf_compas.csv")
NICE_lr_compas= read.csv("/Users/rifatmehreenamin/Desktop/Thesis/MOC-for-CounterfactualFairness/results/NICE_lr_compas.csv")
whatif_rf_compas = read.csv("/Users/rifatmehreenamin/Desktop/Thesis/MOC-for-CounterfactualFairness/results/whatif_rf_compas.csv")
whatif_lr_compas = read.csv("/Users/rifatmehreenamin/Desktop/Thesis/MOC-for-CounterfactualFairness/results/whatif_lr_compas.csv")

MOC_rf_compas$model = "randomForest"
MOC_lr_compas$model = "log_reg"
NICE_rf_compas$model = "randomForest"
NICE_lr_compas$model = "log_reg"
whatif_rf_compas$model = "randomForest"
whatif_lr_compas$model = "log_reg"

MOC_rf_compas$method = "MOCCF"
MOC_lr_compas$method = "MOCCF"
NICE_rf_compas$method = "NICE"
NICE_lr_compas$method = "NICE"
whatif_rf_compas$method = "WhatIf"
whatif_lr_compas$method = "WhatIf"

cal_data_compas = rbind(MOC_rf_compas, MOC_lr_compas, NICE_rf_compas, NICE_lr_compas, whatif_rf_compas, whatif_lr_compas)
cal_data_compas %>% ggplot(aes(x=method, y=MPD, fill=model)) + geom_boxplot()+ ylab("mean prediction difference") + ylim(-0.5,1) +coord_fixed()+ theme_bw(base_size = 30)

# 
# ggplot(cal_data_compas, aes(x = factor(instance_id), y = total_cf, fill = method)) + geom_col(width = 0.8, position = "dodge")+ ylim(0,30) + xlab("instance id") + ylab("number of counterfactuals") + theme_bw(base_size = 30)
# 
# moc_t = (sum(MOC_rf_compas$execution_time.s.))
# nice_t = (sum(NICE_rf_compas$execution_time.s.))
# whatif_t = (sum(whatif_rf_compas$execution_time.s.))
# 
# v = c(moc_t, nice_t, whatif_t)
# method = c("MOCCF", "NICE", "WhatIf")
# 
# data <- data.frame(
#   name=method ,
#   value=v
# )
# 
# #p <- ggplot(data, aes(x = method, y = v, fill = method)) + geom_col(width = 0.3, position = "dodge") +ylim(0,60) + xlab("") + ylab("average time (s)") + theme_bw(base_size = 20)
# p <- ggplot(data, aes(x = method, y = v)) + geom_col(width = 0.8, position = "dodge") +ylim(0,600) + ylab("time (s)") + theme_bw(base_size = 20)
# p + coord_flip()
# # 
