data = read.csv("/Users/rifatmehreenamin/Desktop/Thesis/MBE_values.csv")

data_MOCCF = data[which(data$method=="MOCCF"), ]
data_NICE = data[which(data$method=="NICE"), ]
data_whatif = data[which(data$method=="whatif"), ]

data_compas = data[which(data$dataset=="compas"), ]
data_law = data[which(data$dataset=="law"), ]
data_adult = data[which(data$dataset=="adult"), ]
data_lipton = data[which(data$dataset=="lipton"), ]





ggplot(data_compas, aes(x = model, y = MBE, fill = method)) + geom_col(width = 0.5, position = "dodge") +ylim(0,1) + xlab("") + ylab("MBE") + theme_bw(base_size = 30)

ggplot(data_law, aes(x = model, y = MBE, fill = method)) + geom_col(width = 0.5, position = "dodge") +ylim(0,1) + xlab("") + ylab("MBE") + theme_bw(base_size = 30)

ggplot(data_adult, aes(x = model, y = MBE, fill = method)) + geom_col(width = 0.5, position = "dodge") +ylim(0,1) + xlab("") + ylab("MBE") + theme_bw(base_size = 30)


ggplot(data_lipton, aes(x = model, y = MBE, fill = method)) + geom_col(width = 0.5, position = "dodge") +ylim(0,1) + xlab("") + ylab("MBE") + theme_bw(base_size = 30)
