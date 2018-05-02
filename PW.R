# 加载相关package
library(data.table)
library(ggplot2)


# 导入Trucksim输出数据
setwd(dir = "E:/R/MaWan/01ZongPoFenXi/PW")  # 设置工作目录
kPWFileList <- list.files(pattern = "*.txt")  # 分析数据文件名
kPWDataName <- gsub(".txt", "", kPWFileList)  # 导入数据集命名
df.pw <- data.frame()  # 创建用于合并数据的数据集

for (i in 1:length(kPWDataName)) {  # 导入数据
  
  kColName <- c("Station", "Speed")
  
  tmp.file2data <- fread(kPWFileList[i],
                         header = T,
                         sep = "auto",
                         stringsAsFactors = FALSE,
                         data.table = FALSE,
                         skip = "Station",
                         col.names = kColName)
  
  # 添加轴数
  tmp.file2data$Axles <- substr(kPWDataName[i], 1, 2)
  
  # 添加坡度
  tmp.file2data$Grade <- substr(kPWDataName[i], 7, 9)
  
  df.pw <- rbind(tmp.file2data, df.pw)
}


# 1.冲坡速度80，比功率6.0，坡度5.0，不同轴数vs坡长----
df.expectedspeed80pw6.0grade5.0 <- df.pw[df.pw$Grade == "5.0",]

plot.expectedspeed80pw6.0grade5.0 <- ggplot(df.expectedspeed80pw6.0grade5.0,
                                            aes(x = Station, y = Speed)) +
  geom_line(aes(colour = factor(Axles)), size = 1.2) +
  geom_hline(yintercept = 80, linetype = "dashed", color = "red", size = 1.0) +
  scale_x_continuous(limits = c(0, 5000), breaks = seq(0, 5000, 500)) +
  scale_y_continuous(limits = c(0, 85), breaks = seq(0, 85, 10)) +
  scale_colour_hue("轴数") +
  labs(title = "冲坡速度80km/h, 比功率6.0kW/T，坡度5.0%", x = "位置", y = "速度, km/h") +
  theme(legend.title = element_text(face = "bold", size = 10),
        legend.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 11),
        axis.text = element_text(face = "bold", size = 10),
        plot.title = element_text(hjust = 0.5))

plot.expectedspeed80pw6.0grade5.0


# 冲破速度80，比功率6.0，坡度5.0%，不同轴数货车稳定速度----
tmpdf.lastspeedgrade5.0 <- subset(df.expectedspeed80pw6.0grade5.0,
                                  df.expectedspeed80pw6.0grade5.0$Station %/% 1 == 4500)

df.lastspeedgrade5.0 <- tmpdf.lastspeedgrade5.0[!duplicated(tmpdf.lastspeedgrade5.0$Axles),]

df.lastspeedgrade5.0


# 2.冲坡速度60，比功率6.0，坡度5.5，不同轴数vs坡长----
df.expectedspeed60pw6.0grade5.5 <- df.pw[df.pw$Grade == "5.5",]

plot.expectedspeed60pw6.0grade5.5 <- ggplot(df.expectedspeed60pw6.0grade5.5,
                                            aes(x = Station, y = Speed)) +
  geom_line(aes(colour = factor(Axles)), size = 1.2) +
  geom_hline(yintercept = 60, linetype = "dashed", color = "red", size = 1.0) +
  scale_x_continuous(limits = c(0, 5000), breaks = seq(0, 5000, 500)) +
  scale_y_continuous(limits = c(0, 65), breaks = seq(0, 65, 10)) +
  scale_colour_hue("轴数") +
  labs(title = "冲坡速度60km/h, 比功率6.0kW/T，坡度5.5%", x = "位置", y = "速度, km/h") +
  theme(legend.title = element_text(face = "bold", size = 10),
        legend.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 11),
        axis.text = element_text(face = "bold", size = 10))

plot.expectedspeed60pw6.0grade5.5


# 冲破速度60，比功率6.0，坡度5.5%，不同轴数货车稳定速度----
tmpdf.lastspeedgrade5.5 <- subset(df.expectedspeed60pw6.0grade5.5,
                                  df.expectedspeed60pw6.0grade5.5$Station %/% 1 == 4500)

df.lastspeedgrade5.5 <- tmpdf.lastspeedgrade5.5[!duplicated(tmpdf.lastspeedgrade5.5$Axles),]

df.lastspeedgrade5.5

