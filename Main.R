#-----------------------Code Description---------------------------------------#
# Notes:
# ver1.0, data: 20170220, by ZhangWH
# ver2.0, data: 20170308, by MaoY
# ver3.0, data: 20170704, by MaoY
#              修正了车辆跑偏和不能达到稳定速度等问题。
#
# Description:
# 深圳妈湾项目研究内容1：纵坡设计参数与典型货运车辆动力性能特征调研研究
#     上坡工况分析。
#     研究深圳妈湾隧道内纵坡指标，分析Trucksim数据，数据来源于Trucksim仿真的不同
# 冲坡速度、以及不同比功率车型的车辆在不同坡度纵坡上平衡稳定速度的仿真实验数据。
# 分析的目的是揭示初始速度（冲坡速度）、坡度、车辆性能（主要是比功率）之间的相互
# 关系。
#------------------------------------------------------------------------------#


# 加载相关package
library(data.table)
library(ggplot2)
library(stats)

# 标准调用函数
source("E:/R/MaWan/01ZongPoFenXi/Functions.R", encoding = "utf-8")

# 导入Trucksim输出数据
setwd(dir = "E:/R/MaWan/01ZongPoFenXi/UphillData")  # 设置工作目录
kFileList <- list.files(pattern = "*.txt")  # 分析数据文件名
kDataName <- gsub(".txt", "", kFileList)  # 导入数据集命名
df.dataall <- data.frame()  # 创建用于合并数据的数据集

for(i in 1:length(kDataName)){  # 导入数据
  
  kColName <- c("Station", "Speed")
  
  tmp.file2data <- fread(kFileList[i],
                         header = T,
                         sep = "auto",
                         stringsAsFactors = FALSE,
                         data.table = FALSE,
                         skip = "Station",
                         col.names = kColName)
  
  # 按照桩号排序
  # tmp.file2data <- Order.dis(tmp.file2data, step = 1)
  
  #增加平滑后的速度项
  # tmp.speedloess <- loess(Speed~Station,
  #                         data = tmp.file2data,
  #                         span = 0.1,
  #                         degree = 2) #loess 数据平滑
  
  # tmp.file2data$SpeedLoess <- tmp.speedloess$fitted #平滑后的结果
  
  # 添加期望速度（初始速度or冲坡速度）
  tmp.file2data$ExpectedSpeed <- substr(kDataName[i], 0, 2)
  # 添加坡度
  tmp.file2data$Grade <- substr(kDataName[i], 4, 6)
  # 添加车辆比功率
  tmp.file2data$Powerweight <- substr(kDataName[i], 8, 10)
  
  df.dataall <- rbind(tmp.file2data, df.dataall)
}


# 1.固定冲坡速度，固定比功率，不同坡度vs坡长------------------------------------
# 1.1 冲坡速度80，比功率4.5，不同坡度vs坡长----
df.expectedspeed80pw4.5 <- df.dataall[df.dataall$ExpectedSpeed == "80" &
                                        df.dataall$Powerweight == "4.5",]

plot.expectedspeed80pw4.5 <- ggplot(df.expectedspeed80pw4.5,
                                    aes(x = Station, y = Speed))+
  geom_line(aes(colour = factor(Grade)), size = 1.2)+
  geom_hline(yintercept = 80, linetype = "dashed", color = "red", size = 1.0)+
  scale_x_continuous(limits = c(0, 5000), breaks = seq(0, 5000, 500))+
  scale_y_continuous(limits = c(0, 85), breaks = seq(0, 85, 10))+
  scale_colour_hue("坡度")+
  labs(title = "冲坡速度80km/h, 比功率4.5kW/T", x = "位置", y = "速度, km/h")+
  theme(legend.title = element_text(face = "bold", size = 10),
        legend.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 11),
        axis.text = element_text(face = "bold", size = 10))

plot.expectedspeed80pw4.5


# 1.2 冲坡速度80，比功率6.0，不同坡度vs坡长----
df.expectedspeed80pw6.0 <- df.dataall[df.dataall$ExpectedSpeed == "80" &
                                        df.dataall$Powerweight == "6.0",]

plot.expectedspeed80pw6.0 <- ggplot(df.expectedspeed80pw6.0,
                                    aes(x = Station, y = Speed))+
  geom_line(aes(colour = factor(Grade)), size = 1.2)+
  geom_hline(yintercept = 80, linetype = "dashed", color = "red", size = 1.0)+
  scale_x_continuous(limits = c(0, 5000), breaks = seq(0, 5000, 500))+
  scale_y_continuous(limits = c(0, 85), breaks = seq(0, 85, 10))+
  scale_colour_hue("坡度")+
  labs(title = "冲坡速度80km/h, 比功率6.0kW/T", x = "位置", y = "速度, km/h")+
  theme(legend.title = element_text(face = "bold", size = 10),
        legend.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 11),
        axis.text = element_text(face = "bold", size = 10))

plot.expectedspeed80pw6.0


# 1.3 冲坡速度80，比功率8.0，不同坡度vs坡长----
df.expectedspeed80pw8.0 <- df.dataall[df.dataall$ExpectedSpeed == "80" &
                                        df.dataall$Powerweight == "8.0",]

plot.expectedspeed80pw8.0 <- ggplot(df.expectedspeed80pw8.0,
                                    aes(x = Station, y = Speed))+
  geom_line(aes(colour = factor(Grade)), size = 1.2)+
  geom_hline(yintercept = 80, linetype = "dashed", color = "red", size = 1.0)+
  scale_x_continuous(limits = c(0, 5000), breaks = seq(0, 5000, 500))+
  scale_y_continuous(limits = c(0, 85), breaks = seq(0, 85, 10))+
  scale_colour_hue("坡度")+
  labs(title = "冲坡速度80km/h, 比功率8.0kW/T", x = "位置", y = "速度, km/h")+
  theme(legend.title = element_text(face = "bold", size = 10),
        legend.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 11),
        axis.text = element_text(face = "bold", size = 10))

plot.expectedspeed80pw8.0


# 1.4 冲坡速度80，比功率9.3，不同坡度vs坡长----
df.expectedspeed80pw9.3 <- df.dataall[df.dataall$ExpectedSpeed == "80" &
                                        df.dataall$Powerweight == "9.3",]

plot.expectedspeed80pw9.3 <- ggplot(df.expectedspeed80pw9.3,
                                    aes(x = Station, y = Speed))+
  geom_line(aes(colour = factor(Grade)), size = 1.2)+
  geom_hline(yintercept = 80, linetype = "dashed", color = "red", size = 1.0)+
  scale_x_continuous(limits = c(0, 5000), breaks = seq(0, 5000, 500))+
  scale_y_continuous(limits = c(0, 85), breaks = seq(0, 85, 10))+
  scale_colour_hue("坡度")+
  labs(title = "冲坡速度80km/h, 比功率9.3kW/T", x = "位置", y = "速度, km/h")+
  theme(legend.title = element_text(face = "bold", size = 10),
        legend.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 11),
        axis.text = element_text(face = "bold", size = 10))

plot.expectedspeed80pw9.3


# 1.5 冲坡速度60，比功率4.5，不同坡度vs坡长----
df.expectedspeed60pw4.5 <- df.dataall[df.dataall$ExpectedSpeed == "60" &
                                        df.dataall$Powerweight == "4.5",]

plot.expectedspeed60pw4.5 <- ggplot(df.expectedspeed60pw4.5,
                                    aes(x = Station, y = Speed))+
  geom_line(aes(colour = factor(Grade)), size = 1.2)+
  geom_hline(yintercept = 60, linetype = "dashed", color = "red", size = 1.0)+
  scale_x_continuous(limits = c(0, 5000), breaks = seq(0, 5000, 500))+
  scale_y_continuous(limits = c(0, 65), breaks = seq(0, 65, 10))+
  scale_colour_hue("坡度")+
  labs(title = "冲坡速度60km/h, 比功率4.5kW/T", x = "位置", y = "速度, km/h")+
  theme(legend.title = element_text(face = "bold", size = 10),
        legend.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 11),
        axis.text = element_text(face = "bold", size = 10))

plot.expectedspeed60pw4.5


# 1.6 冲坡速度60，比功率6.0，不同坡度vs坡长----
df.expectedspeed60pw6.0 <- df.dataall[df.dataall$ExpectedSpeed == "60" &
                                        df.dataall$Powerweight == "6.0",]

plot.expectedspeed60pw6.0 <- ggplot(df.expectedspeed60pw6.0,
                                    aes(x = Station, y = Speed))+
  geom_line(aes(colour = factor(Grade)), size = 1.2)+
  geom_hline(yintercept = 60, linetype = "dashed", color = "red", size = 1.0)+
  scale_x_continuous(limits = c(0, 5000), breaks = seq(0, 5000, 500))+
  scale_y_continuous(limits = c(0, 65), breaks = seq(0, 65, 10))+
  scale_colour_hue("坡度")+
  labs(title = "冲坡速度60km/h, 比功率6.0kW/T", x = "位置", y = "速度, km/h")+
  theme(legend.title = element_text(face = "bold", size = 10),
        legend.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 11),
        axis.text = element_text(face = "bold", size = 10))

plot.expectedspeed60pw6.0


# 1.7 冲坡速度60，比功率8.0，不同坡度vs坡长----
df.expectedspeed60pw8.0 <- df.dataall[df.dataall$ExpectedSpeed == "60" &
                                        df.dataall$Powerweight == "8.0",]

plot.expectedspeed60pw8.0 <- ggplot(df.expectedspeed60pw8.0,
                                    aes(x = Station, y = Speed))+
  geom_line(aes(colour = factor(Grade)), size = 1.2)+
  geom_hline(yintercept = 60, linetype = "dashed", color = "red", size = 1.0)+
  scale_x_continuous(limits = c(0, 5000), breaks = seq(0, 5000, 500))+
  scale_y_continuous(limits = c(0, 65), breaks = seq(0, 65, 10))+
  scale_colour_hue("坡度")+
  labs(title = "冲坡速度60km/h, 比功率8.0kW/T", x = "位置", y = "速度, km/h")+
  theme(legend.title = element_text(face = "bold", size = 10),
        legend.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 11),
        axis.text = element_text(face = "bold", size = 10))

plot.expectedspeed60pw8.0


# 1.8 冲坡速度60，比功率9.3，不同坡度vs坡长----
df.expectedspeed60pw9.3 <- df.dataall[df.dataall$ExpectedSpeed == "60" &
                                        df.dataall$Powerweight == "9.3",]

plot.expectedspeed60pw9.3 <- ggplot(df.expectedspeed60pw9.3,
                                    aes(x = Station, y = Speed))+
  geom_line(aes(colour = factor(Grade)), size = 1.2)+
  geom_hline(yintercept = 60, linetype = "dashed", color = "red", size = 1.0)+
  scale_x_continuous(limits = c(0, 5000), breaks = seq(0, 5000, 500))+
  scale_y_continuous(limits = c(0, 65), breaks = seq(0, 65, 10))+
  scale_colour_hue("坡度")+
  labs(title = "冲坡速度60km/h, 比功率9.3kW/T", x = "位置", y = "速度, km/h")+
  theme(legend.title = element_text(face = "bold", size = 10),
        legend.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 11),
        axis.text = element_text(face = "bold", size = 10))

plot.expectedspeed60pw9.3


# 1.9 冲坡速度40，比功率4.5，不同坡度vs坡长----
df.expectedspeed40pw4.5 <- df.dataall[df.dataall$ExpectedSpeed == "40" &
                                        df.dataall$Powerweight == "4.5",]

plot.expectedspeed40pw4.5 <- ggplot(df.expectedspeed40pw4.5,
                                    aes(x = Station, y = Speed))+
  geom_line(aes(colour = factor(Grade)), size = 1.2)+
  geom_hline(yintercept = 40, linetype = "dashed", color = "red", size = 1.0)+
  scale_x_continuous(limits = c(0, 5000), breaks = seq(0, 5000, 500))+
  scale_y_continuous(limits = c(0, 45), breaks = seq(0, 45, 10))+
  scale_colour_hue("坡度")+
  labs(title = "冲坡速度40km/h, 比功率4.5kW/T", x = "位置", y = "速度, km/h")+
  theme(legend.title = element_text(face = "bold", size = 10),
        legend.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 11),
        axis.text = element_text(face = "bold", size = 10))

plot.expectedspeed40pw4.5


# 1.10 冲坡速度40，比功率6.0，不同坡度vs坡长----
df.expectedspeed40pw6.0 <- df.dataall[df.dataall$ExpectedSpeed == "40" &
                                        df.dataall$Powerweight == "6.0",]

plot.expectedspeed40pw6.0 <- ggplot(df.expectedspeed40pw6.0,
                                    aes(x = Station, y = Speed))+
  geom_line(aes(colour = factor(Grade)), size = 1.2)+
  geom_hline(yintercept = 40, linetype = "dashed", color = "red", size = 1.0)+
  scale_x_continuous(limits = c(0, 5000), breaks = seq(0, 5000, 500))+
  scale_y_continuous(limits = c(0, 45), breaks = seq(0, 45, 10))+
  scale_colour_hue("坡度")+
  labs(title = "冲坡速度40km/h, 比功率6.0kW/T", x = "位置", y = "速度, km/h")+
  theme(legend.title = element_text(face = "bold", size = 10),
        legend.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 11),
        axis.text = element_text(face = "bold", size = 10))

plot.expectedspeed40pw6.0


# 1.11 冲坡速度40，比功率8.0，不同坡度vs坡长----
df.expectedspeed40pw8.0 <- df.dataall[df.dataall$ExpectedSpeed == "40" &
                                        df.dataall$Powerweight == "8.0",]

plot.expectedspeed40pw8.0 <- ggplot(df.expectedspeed40pw8.0,
                                    aes(x = Station, y = Speed))+
  geom_line(aes(colour = factor(Grade)), size = 1.2)+
  geom_hline(yintercept = 40, linetype = "dashed", color = "red", size = 1.0)+
  scale_x_continuous(limits = c(0, 5000), breaks = seq(0, 5000, 500))+
  scale_y_continuous(limits = c(0, 45), breaks = seq(0, 45, 10))+
  scale_colour_hue("坡度")+
  labs(title = "冲坡速度40km/h, 比功率8.0kW/T", x = "位置", y = "速度, km/h")+
  theme(legend.title = element_text(face = "bold", size = 10),
        legend.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 11),
        axis.text = element_text(face = "bold", size = 10))

plot.expectedspeed40pw8.0


# 1.12 冲坡速度40，比功率9.3，不同坡度vs坡长----
df.expectedspeed40pw9.3 <- df.dataall[df.dataall$ExpectedSpeed == "40" &
                                        df.dataall$Powerweight == "9.3",]

plot.expectedspeed40pw9.3 <- ggplot(df.expectedspeed40pw9.3,
                                    aes(x = Station, y = Speed))+
  geom_line(aes(colour = factor(Grade)), size = 1.2)+
  geom_hline(yintercept = 40, linetype = "dashed", color = "red", size = 1.0)+
  scale_x_continuous(limits = c(0, 5000), breaks = seq(0, 5000, 500))+
  scale_y_continuous(limits = c(0, 45), breaks = seq(0, 45, 10))+
  scale_colour_hue("坡度")+
  labs(title = "冲坡速度40km/h, 比功率9.3kW/T", x = "位置", y = "速度, km/h")+
  theme(legend.title = element_text(face = "bold", size = 10),
        legend.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 11),
        axis.text = element_text(face = "bold", size = 10))

plot.expectedspeed40pw9.3


# 2.固定冲坡速度，固定坡度，比功率vs坡长------------------------------------
# 2.1 冲坡速度80，坡度2.5，不同比功率vs坡长----
df.expectedspeed80grade2.5 <- df.dataall[df.dataall$ExpectedSpeed == "80" &
                                           df.dataall$Grade == "2.5",]

plot.expectedspeed80grade2.5 <- ggplot(df.expectedspeed80grade2.5,
                                       aes(x = Station, y = Speed))+
  geom_line(aes(colour = factor(Powerweight)), size = 1.2)+
  geom_hline(yintercept = 80, linetype = "dashed", color = "red", size = 1.0)+
  scale_x_continuous(limits = c(0, 5000), breaks = seq(0, 5000, 500))+
  scale_y_continuous(limits = c(0, 85), breaks = seq(0, 85, 10))+
  scale_colour_hue("比功率")+
  labs(title = "冲坡速度80km/h, 坡度2.5%", x = "位置", y = "速度, km/h")+
  theme(legend.title = element_text(face = "bold", size = 10),
        legend.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 11),
        axis.text = element_text(face = "bold", size = 10))

plot.expectedspeed80grade2.5


# 2.2 冲坡速度80，坡度3.0，不同比功率vs坡长----
df.expectedspeed80grade3.0 <- df.dataall[df.dataall$ExpectedSpeed == "80" &
                                           df.dataall$Grade == "3.0",]

plot.expectedspeed80grade3.0 <- ggplot(df.expectedspeed80grade3.0,
                                       aes(x = Station, y = Speed))+
  geom_line(aes(colour = factor(Powerweight)), size = 1.2)+
  geom_hline(yintercept = 80, linetype = "dashed", color = "red", size = 1.0)+
  scale_x_continuous(limits = c(0, 5000), breaks = seq(0, 5000, 500))+
  scale_y_continuous(limits = c(0, 85), breaks = seq(0, 85, 10))+
  scale_colour_hue("比功率")+
  labs(title = "冲坡速度80km/h, 坡度3.0%", x = "位置", y = "速度, km/h")+
  theme(legend.title = element_text(face = "bold", size = 10),
        legend.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 11),
        axis.text = element_text(face = "bold", size = 10))

plot.expectedspeed80grade3.0


# 2.3 冲坡速度80，坡度3.5，不同比功率vs坡长----
df.expectedspeed80grade3.5 <- df.dataall[df.dataall$ExpectedSpeed == "80" &
                                           df.dataall$Grade == "3.5",]

plot.expectedspeed80grade3.5 <- ggplot(df.expectedspeed80grade3.5,
                                       aes(x = Station, y = Speed))+
  geom_line(aes(colour = factor(Powerweight)), size = 1.2)+
  geom_hline(yintercept = 80, linetype = "dashed", color = "red", size = 1.0)+
  scale_x_continuous(limits = c(0, 5000), breaks = seq(0, 5000, 500))+
  scale_y_continuous(limits = c(0, 85), breaks = seq(0, 85, 10))+
  scale_colour_hue("比功率")+
  labs(title = "冲坡速度80km/h, 坡度3.5%", x = "位置", y = "速度, km/h")+
  theme(legend.title = element_text(face = "bold", size = 10),
        legend.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 11),
        axis.text = element_text(face = "bold", size = 10))

plot.expectedspeed80grade3.5


# 2.4 冲坡速度80，坡度4.0，不同比功率vs坡长----
df.expectedspeed80grade4.0 <- df.dataall[df.dataall$ExpectedSpeed == "80" &
                                           df.dataall$Grade == "4.0",]

plot.expectedspeed80grade4.0 <- ggplot(df.expectedspeed80grade4.0,
                                       aes(x = Station, y = Speed))+
  geom_line(aes(colour = factor(Powerweight)), size = 1.2)+
  geom_hline(yintercept = 80, linetype = "dashed", color = "red", size = 1.0)+
  scale_x_continuous(limits = c(0, 5000), breaks = seq(0, 5000, 500))+
  scale_y_continuous(limits = c(0, 85), breaks = seq(0, 85, 10))+
  scale_colour_hue("比功率")+
  labs(title = "冲坡速度80km/h, 坡度4.0%", x = "位置", y = "速度, km/h")+
  theme(legend.title = element_text(face = "bold", size = 10),
        legend.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 11),
        axis.text = element_text(face = "bold", size = 10))

plot.expectedspeed80grade4.0


# 2.5 冲坡速度80，坡度4.5，不同比功率vs坡长----
df.expectedspeed80grade4.5 <- df.dataall[df.dataall$ExpectedSpeed == "80" &
                                           df.dataall$Grade == "4.5",]

plot.expectedspeed80grade4.5 <- ggplot(df.expectedspeed80grade4.5,
                                       aes(x = Station, y = Speed))+
  geom_line(aes(colour = factor(Powerweight)), size = 1.2)+
  geom_hline(yintercept = 80, linetype = "dashed", color = "red", size = 1.0)+
  scale_x_continuous(limits = c(0, 5000), breaks = seq(0, 5000, 500))+
  scale_y_continuous(limits = c(0, 85), breaks = seq(0, 85, 10))+
  scale_colour_hue("比功率")+
  labs(title = "冲坡速度80km/h, 坡度4.5%", x = "位置", y = "速度, km/h")+
  theme(legend.title = element_text(face = "bold", size = 10),
        legend.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 11),
        axis.text = element_text(face = "bold", size = 10))

plot.expectedspeed80grade4.5


# 2.6 冲坡速度80，坡度5.0，不同比功率vs坡长----
df.expectedspeed80grade5.0 <- df.dataall[df.dataall$ExpectedSpeed == "80" &
                                           df.dataall$Grade == "5.0",]

plot.expectedspeed80grade5.0 <- ggplot(df.expectedspeed80grade5.0,
                                       aes(x = Station, y = Speed))+
  geom_line(aes(colour = factor(Powerweight)), size = 1.2)+
  geom_hline(yintercept = 80, linetype = "dashed", color = "red", size = 1.0)+
  scale_x_continuous(limits = c(0, 5000), breaks = seq(0, 5000, 500))+
  scale_y_continuous(limits = c(0, 85), breaks = seq(0, 85, 10))+
  scale_colour_hue("比功率")+
  labs(title = "冲坡速度80km/h, 坡度5.0%", x = "位置", y = "速度, km/h")+
  theme(legend.title = element_text(face = "bold", size = 10),
        legend.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 11),
        axis.text = element_text(face = "bold", size = 10))

plot.expectedspeed80grade5.0


# 2.7 冲坡速度80，坡度5.5，不同比功率vs坡长----
df.expectedspeed80grade5.5 <- df.dataall[df.dataall$ExpectedSpeed == "80" &
                                           df.dataall$Grade == "5.5",]

plot.expectedspeed80grade5.5 <- ggplot(df.expectedspeed80grade5.5,
                                       aes(x = Station, y = Speed))+
  geom_line(aes(colour = factor(Powerweight)), size = 1.2)+
  geom_hline(yintercept = 80, linetype = "dashed", color = "red", size = 1.0)+
  scale_x_continuous(limits = c(0, 5000), breaks = seq(0, 5000, 500))+
  scale_y_continuous(limits = c(0, 85), breaks = seq(0, 85, 10))+
  scale_colour_hue("比功率")+
  labs(title = "冲坡速度80km/h, 坡度5.5%", x = "位置", y = "速度, km/h")+
  theme(legend.title = element_text(face = "bold", size = 10),
        legend.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 11),
        axis.text = element_text(face = "bold", size = 10))

plot.expectedspeed80grade5.5


# 2.8 冲坡速度80，坡度6.0，不同比功率vs坡长----
df.expectedspeed80grade6.0 <- df.dataall[df.dataall$ExpectedSpeed == "80" &
                                           df.dataall$Grade == "6.0",]

plot.expectedspeed80grade6.0 <- ggplot(df.expectedspeed80grade6.0,
                                       aes(x = Station, y = Speed))+
  geom_line(aes(colour = factor(Powerweight)), size = 1.2)+
  geom_hline(yintercept = 80, linetype = "dashed", color = "red", size = 1.0)+
  scale_x_continuous(limits = c(0, 5000), breaks = seq(0, 5000, 500))+
  scale_y_continuous(limits = c(0, 85), breaks = seq(0, 85, 10))+
  scale_colour_hue("比功率")+
  labs(title = "冲坡速度80km/h, 坡度6.0%", x = "位置", y = "速度, km/h")+
  theme(legend.title = element_text(face = "bold", size = 10),
        legend.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 11),
        axis.text = element_text(face = "bold", size = 10))

plot.expectedspeed80grade6.0


# 2.9 冲坡速度60，坡度2.5，不同比功率vs坡长----
df.expectedspeed60grade2.5 <- df.dataall[df.dataall$ExpectedSpeed == "60" &
                                           df.dataall$Grade == "2.5",]

plot.expectedspeed60grade2.5 <- ggplot(df.expectedspeed60grade2.5,
                                       aes(x = Station, y = Speed))+
  geom_line(aes(colour = factor(Powerweight)), size = 1.2)+
  geom_hline(yintercept = 60, linetype = "dashed", color = "red", size = 1.0)+
  scale_x_continuous(limits = c(0, 5000), breaks = seq(0, 5000, 500))+
  scale_y_continuous(limits = c(0, 65), breaks = seq(0, 65, 10))+
  scale_colour_hue("比功率")+
  labs(title = "冲坡速度60km/h, 坡度2.5%", x = "位置", y = "速度, km/h")+
  theme(legend.title = element_text(face = "bold", size = 10),
        legend.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 11),
        axis.text = element_text(face = "bold", size = 10))

plot.expectedspeed60grade2.5


# 2.10 冲坡速度60，坡度3.0，不同比功率vs坡长----
df.expectedspeed60grade3.0 <- df.dataall[df.dataall$ExpectedSpeed == "60" &
                                           df.dataall$Grade == "3.0",]

plot.expectedspeed60grade3.0 <- ggplot(df.expectedspeed60grade3.0,
                                       aes(x = Station, y = Speed))+
  geom_line(aes(colour = factor(Powerweight)), size = 1.2)+
  geom_hline(yintercept = 60, linetype = "dashed", color = "red", size = 1.0)+
  scale_x_continuous(limits = c(0, 5000), breaks = seq(0, 5000, 500))+
  scale_y_continuous(limits = c(0, 65), breaks = seq(0, 65, 10))+
  scale_colour_hue("比功率")+
  labs(title = "冲坡速度60km/h, 坡度3.0%", x = "位置", y = "速度, km/h")+
  theme(legend.title = element_text(face = "bold", size = 10),
        legend.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 11),
        axis.text = element_text(face = "bold", size = 10))

plot.expectedspeed60grade3.0


# 2.11 冲坡速度60，坡度3.5，不同比功率vs坡长----
df.expectedspeed60grade3.5 <- df.dataall[df.dataall$ExpectedSpeed == "60" &
                                           df.dataall$Grade == "3.5",]

plot.expectedspeed60grade3.5 <- ggplot(df.expectedspeed60grade3.5,
                                       aes(x = Station, y = Speed))+
  geom_line(aes(colour = factor(Powerweight)), size = 1.2)+
  geom_hline(yintercept = 60, linetype = "dashed", color = "red", size = 1.0)+
  scale_x_continuous(limits = c(0, 5000), breaks = seq(0, 5000, 500))+
  scale_y_continuous(limits = c(0, 65), breaks = seq(0, 65, 10))+
  scale_colour_hue("比功率")+
  labs(title = "冲坡速度60km/h, 坡度3.5%", x = "位置", y = "速度, km/h")+
  theme(legend.title = element_text(face = "bold", size = 10),
        legend.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 11),
        axis.text = element_text(face = "bold", size = 10))

plot.expectedspeed60grade3.5


# 2.12 冲坡速度60，坡度4.0，不同比功率vs坡长----
df.expectedspeed60grade4.0 <- df.dataall[df.dataall$ExpectedSpeed == "60" &
                                           df.dataall$Grade == "4.0",]

plot.expectedspeed60grade4.0 <- ggplot(df.expectedspeed60grade4.0,
                                       aes(x = Station, y = Speed))+
  geom_line(aes(colour = factor(Powerweight)), size = 1.2)+
  geom_hline(yintercept = 60, linetype = "dashed", color = "red", size = 1.0)+
  scale_x_continuous(limits = c(0, 5000), breaks = seq(0, 5000, 500))+
  scale_y_continuous(limits = c(0, 65), breaks = seq(0, 65, 10))+
  scale_colour_hue("比功率")+
  labs(title = "冲坡速度60km/h, 坡度4.0%", x = "位置", y = "速度, km/h")+
  theme(legend.title = element_text(face = "bold", size = 10),
        legend.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 11),
        axis.text = element_text(face = "bold", size = 10))

plot.expectedspeed60grade4.0


# 2.13 冲坡速度60，坡度4.5，不同比功率vs坡长----
df.expectedspeed60grade4.5 <- df.dataall[df.dataall$ExpectedSpeed == "60" &
                                           df.dataall$Grade == "4.5",]

plot.expectedspeed60grade4.5 <- ggplot(df.expectedspeed60grade4.5,
                                       aes(x = Station, y = Speed))+
  geom_line(aes(colour = factor(Powerweight)), size = 1.2)+
  geom_hline(yintercept = 60, linetype = "dashed", color = "red", size = 1.0)+
  scale_x_continuous(limits = c(0, 5000), breaks = seq(0, 5000, 500))+
  scale_y_continuous(limits = c(0, 65), breaks = seq(0, 65, 10))+
  scale_colour_hue("比功率")+
  labs(title = "冲坡速度60km/h, 坡度4.5%", x = "位置", y = "速度, km/h")+
  theme(legend.title = element_text(face = "bold", size = 10),
        legend.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 11),
        axis.text = element_text(face = "bold", size = 10))

plot.expectedspeed60grade4.5


# 2.14 冲坡速度60，坡度5.0，不同比功率vs坡长----
df.expectedspeed60grade5.0 <- df.dataall[df.dataall$ExpectedSpeed == "60" &
                                           df.dataall$Grade == "5.0",]

plot.expectedspeed60grade5.0 <- ggplot(df.expectedspeed60grade5.0,
                                       aes(x = Station, y = Speed))+
  geom_line(aes(colour = factor(Powerweight)), size = 1.2)+
  geom_hline(yintercept = 60, linetype = "dashed", color = "red", size = 1.0)+
  scale_x_continuous(limits = c(0, 5000), breaks = seq(0, 5000, 500))+
  scale_y_continuous(limits = c(0, 65), breaks = seq(0, 65, 10))+
  scale_colour_hue("比功率")+
  labs(title = "冲坡速度60km/h, 坡度5.0%", x = "位置", y = "速度, km/h")+
  theme(legend.title = element_text(face = "bold", size = 10),
        legend.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 11),
        axis.text = element_text(face = "bold", size = 10))

plot.expectedspeed60grade5.0


# 2.15 冲坡速度60，坡度5.5，不同比功率vs坡长----
df.expectedspeed60grade5.5 <- df.dataall[df.dataall$ExpectedSpeed == "60" &
                                           df.dataall$Grade == "5.5",]

plot.expectedspeed60grade5.5 <- ggplot(df.expectedspeed60grade5.5,
                                       aes(x = Station, y = Speed))+
  geom_line(aes(colour = factor(Powerweight)), size = 1.2)+
  geom_hline(yintercept = 60, linetype = "dashed", color = "red", size = 1.0)+
  scale_x_continuous(limits = c(0, 5000), breaks = seq(0, 5000, 500))+
  scale_y_continuous(limits = c(0, 65), breaks = seq(0, 65, 10))+
  scale_colour_hue("比功率")+
  labs(title = "冲坡速度60km/h, 坡度5.5%", x = "位置", y = "速度, km/h")+
  theme(legend.title = element_text(face = "bold", size = 10),
        legend.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 11),
        axis.text = element_text(face = "bold", size = 10))

plot.expectedspeed60grade5.5


# 2.16 冲坡速度60，坡度6.0，不同比功率vs坡长----
df.expectedspeed60grade6.0 <- df.dataall[df.dataall$ExpectedSpeed == "60" &
                                           df.dataall$Grade == "6.0",]

plot.expectedspeed60grade6.0 <- ggplot(df.expectedspeed60grade6.0,
                                       aes(x = Station, y = Speed))+
  geom_line(aes(colour = factor(Powerweight)), size = 1.2)+
  geom_hline(yintercept = 60, linetype = "dashed", color = "red", size = 1.0)+
  scale_x_continuous(limits = c(0, 5000), breaks = seq(0, 5000, 500))+
  scale_y_continuous(limits = c(0, 65), breaks = seq(0, 65, 10))+
  scale_colour_hue("比功率")+
  labs(title = "冲坡速度60km/h, 坡度6.0%", x = "位置", y = "速度, km/h")+
  theme(legend.title = element_text(face = "bold", size = 10),
        legend.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 11),
        axis.text = element_text(face = "bold", size = 10))

plot.expectedspeed60grade6.0


# 2.17 冲坡速度40，坡度2.5，不同比功率vs坡长----
df.expectedspeed40grade2.5 <- df.dataall[df.dataall$ExpectedSpeed == "40" &
                                           df.dataall$Grade == "2.5",]

plot.expectedspeed40grade2.5 <- ggplot(df.expectedspeed40grade2.5,
                                       aes(x = Station, y = Speed))+
  geom_line(aes(colour = factor(Powerweight)), size = 1.2)+
  geom_hline(yintercept = 40, linetype = "dashed", color = "red", size = 1.0)+
  scale_x_continuous(limits = c(0, 5000), breaks = seq(0, 5000, 500))+
  scale_y_continuous(limits = c(0, 45), breaks = seq(0, 45, 10))+
  scale_colour_hue("坡度")+
  labs(title = "冲坡速度40km/h, 坡度2.5%", x = "位置", y = "速度, km/h")+
  theme(legend.title = element_text(face = "bold", size = 10),
        legend.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 11),
        axis.text = element_text(face = "bold", size = 10))

plot.expectedspeed40grade2.5


# 2.18 冲坡速度40，坡度3.0，不同比功率vs坡长----
df.expectedspeed40grade3.0 <- df.dataall[df.dataall$ExpectedSpeed == "40" &
                                           df.dataall$Grade == "3.0",]

plot.expectedspeed40grade3.0 <- ggplot(df.expectedspeed40grade3.0,
                                       aes(x = Station, y = Speed))+
  geom_line(aes(colour = factor(Powerweight)), size = 1.2)+
  geom_hline(yintercept = 40, linetype = "dashed", color = "red", size = 1.0)+
  scale_x_continuous(limits = c(0, 5000), breaks = seq(0, 5000, 500))+
  scale_y_continuous(limits = c(0, 45), breaks = seq(0, 45, 10))+
  scale_colour_hue("坡度")+
  labs(title = "冲坡速度40km/h, 坡度3.0%", x = "位置", y = "速度, km/h")+
  theme(legend.title = element_text(face = "bold", size = 10),
        legend.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 11),
        axis.text = element_text(face = "bold", size = 10))

plot.expectedspeed40grade3.0


# 2.19 冲坡速度40，坡度3.5，不同比功率vs坡长----
df.expectedspeed40grade3.5 <- df.dataall[df.dataall$ExpectedSpeed == "40" &
                                           df.dataall$Grade == "3.5",]

plot.expectedspeed40grade3.5 <- ggplot(df.expectedspeed40grade3.5,
                                       aes(x = Station, y = Speed))+
  geom_line(aes(colour = factor(Powerweight)), size = 1.2)+
  geom_hline(yintercept = 40, linetype = "dashed", color = "red", size = 1.0)+
  scale_x_continuous(limits = c(0, 5000), breaks = seq(0, 5000, 500))+
  scale_y_continuous(limits = c(0, 45), breaks = seq(0, 45, 10))+
  scale_colour_hue("坡度")+
  labs(title = "冲坡速度40km/h, 坡度3.5%", x = "位置", y = "速度, km/h")+
  theme(legend.title = element_text(face = "bold", size = 10),
        legend.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 11),
        axis.text = element_text(face = "bold", size = 10))

plot.expectedspeed40grade3.5


# 2.20 冲坡速度40，坡度4.0，不同比功率vs坡长----
df.expectedspeed40grade4.0 <- df.dataall[df.dataall$ExpectedSpeed == "40" &
                                           df.dataall$Grade == "4.0",]

plot.expectedspeed40grade4.0 <- ggplot(df.expectedspeed40grade4.0,
                                       aes(x = Station, y = Speed))+
  geom_line(aes(colour = factor(Powerweight)), size = 1.2)+
  geom_hline(yintercept = 40, linetype = "dashed", color = "red", size = 1.0)+
  scale_x_continuous(limits = c(0, 5000), breaks = seq(0, 5000, 500))+
  scale_y_continuous(limits = c(0, 45), breaks = seq(0, 45, 10))+
  scale_colour_hue("坡度")+
  labs(title = "冲坡速度40km/h, 坡度4.0%", x = "位置", y = "速度, km/h")+
  theme(legend.title = element_text(face = "bold", size = 10),
        legend.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 11),
        axis.text = element_text(face = "bold", size = 10))

plot.expectedspeed40grade4.0


# 2.21 冲坡速度40，坡度4.5，不同比功率vs坡长----
df.expectedspeed40grade4.5 <- df.dataall[df.dataall$ExpectedSpeed == "40" &
                                           df.dataall$Grade == "4.5",]

plot.expectedspeed40grade4.5 <- ggplot(df.expectedspeed40grade4.5,
                                       aes(x = Station, y = Speed))+
  geom_line(aes(colour = factor(Powerweight)), size = 1.2)+
  geom_hline(yintercept = 40, linetype = "dashed", color = "red", size = 1.0)+
  scale_x_continuous(limits = c(0, 5000), breaks = seq(0, 5000, 500))+
  scale_y_continuous(limits = c(0, 45), breaks = seq(0, 45, 10))+
  scale_colour_hue("坡度")+
  labs(title = "冲坡速度40km/h, 坡度4.5%", x = "位置", y = "速度, km/h")+
  theme(legend.title = element_text(face = "bold", size = 10),
        legend.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 11),
        axis.text = element_text(face = "bold", size = 10))

plot.expectedspeed40grade4.5


# 2.22 冲坡速度40，坡度5.0，不同比功率vs坡长----
df.expectedspeed40grade5.0 <- df.dataall[df.dataall$ExpectedSpeed == "40" &
                                           df.dataall$Grade == "5.0",]

plot.expectedspeed40grade5.0 <- ggplot(df.expectedspeed40grade5.0,
                                       aes(x = Station, y = Speed))+
  geom_line(aes(colour = factor(Powerweight)), size = 1.2)+
  geom_hline(yintercept = 40, linetype = "dashed", color = "red", size = 1.0)+
  scale_x_continuous(limits = c(0, 5000), breaks = seq(0, 5000, 500))+
  scale_y_continuous(limits = c(0, 45), breaks = seq(0, 45, 10))+
  scale_colour_hue("坡度")+
  labs(title = "冲坡速度40km/h, 坡度5.0%", x = "位置", y = "速度, km/h")+
  theme(legend.title = element_text(face = "bold", size = 10),
        legend.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 11),
        axis.text = element_text(face = "bold", size = 10))

plot.expectedspeed40grade5.0


# 2.23 冲坡速度40，坡度5.5，不同比功率vs坡长----
df.expectedspeed40grade5.5 <- df.dataall[df.dataall$ExpectedSpeed == "40" &
                                           df.dataall$Grade == "5.5",]

plot.expectedspeed40grade5.5 <- ggplot(df.expectedspeed40grade5.5,
                                       aes(x = Station, y = Speed))+
  geom_line(aes(colour = factor(Powerweight)), size = 1.2)+
  geom_hline(yintercept = 40, linetype = "dashed", color = "red", size = 1.0)+
  scale_x_continuous(limits = c(0, 5000), breaks = seq(0, 5000, 500))+
  scale_y_continuous(limits = c(0, 45), breaks = seq(0, 45, 10))+
  scale_colour_hue("坡度")+
  labs(title = "冲坡速度40km/h, 坡度5.5%", x = "位置", y = "速度, km/h")+
  theme(legend.title = element_text(face = "bold", size = 10),
        legend.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 11),
        axis.text = element_text(face = "bold", size = 10))

plot.expectedspeed40grade5.5


# 2.24 冲坡速度40，坡度6.0，不同比功率vs坡长----
df.expectedspeed40grade6.0 <- df.dataall[df.dataall$ExpectedSpeed == "40" &
                                           df.dataall$Grade == "6.0",]

plot.expectedspeed40grade6.0 <- ggplot(df.expectedspeed40grade6.0,
                                       aes(x = Station, y = Speed))+
  geom_line(aes(colour = factor(Powerweight)), size = 1.2)+
  geom_hline(yintercept = 40, linetype = "dashed", color = "red", size = 1.0)+
  scale_x_continuous(limits = c(0, 5000), breaks = seq(0, 5000, 500))+
  scale_y_continuous(limits = c(0, 45), breaks = seq(0, 45, 10))+
  scale_colour_hue("坡度")+
  labs(title = "冲坡速度40km/h, 坡度6.0%", x = "位置", y = "速度, km/h")+
  theme(legend.title = element_text(face = "bold", size = 10),
        legend.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 11),
        axis.text = element_text(face = "bold", size = 10))

plot.expectedspeed40grade6.0


##########
CalcGradeSpeed(df.expectedspeed80pw4.5[df.expectedspeed80pw4.5$Grade == "4.0", ])
CalcGradeSpeed(df.expectedspeed80pw6.0[df.expectedspeed80pw6.0$Grade == "4.0", ])
CalcGradeSpeed(df.expectedspeed80pw8.0[df.expectedspeed80pw8.0$Grade == "4.0", ])
CalcGradeSpeed(df.expectedspeed80pw9.3[df.expectedspeed80pw9.3$Grade == "4.0", ])

CalcGradeSpeed(df.expectedspeed80pw4.5[df.expectedspeed80pw4.5$Grade == "5.0", ])
CalcGradeSpeed(df.expectedspeed80pw6.0[df.expectedspeed80pw6.0$Grade == "5.0", ])
CalcGradeSpeed(df.expectedspeed80pw8.0[df.expectedspeed80pw8.0$Grade == "5.0", ])
CalcGradeSpeed(df.expectedspeed80pw9.3[df.expectedspeed80pw9.3$Grade == "5.0", ])


CalcGradeSpeed(df.expectedspeed60pw4.5[df.expectedspeed60pw4.5$Grade == "5.0", ])
CalcGradeSpeed(df.expectedspeed60pw6.0[df.expectedspeed60pw6.0$Grade == "5.0", ])
CalcGradeSpeed(df.expectedspeed60pw8.0[df.expectedspeed60pw8.0$Grade == "5.0", ])
CalcGradeSpeed(df.expectedspeed60pw9.3[df.expectedspeed60pw9.3$Grade == "5.0", ])

CalcGradeSpeed(df.expectedspeed60pw4.5[df.expectedspeed60pw4.5$Grade == "6.0", ])
CalcGradeSpeed(df.expectedspeed60pw6.0[df.expectedspeed60pw6.0$Grade == "6.0", ])
CalcGradeSpeed(df.expectedspeed60pw8.0[df.expectedspeed60pw8.0$Grade == "6.0", ])
CalcGradeSpeed(df.expectedspeed60pw9.3[df.expectedspeed60pw9.3$Grade == "6.0", ])


CalcGradeSpeed(df.expectedspeed40pw4.5[df.expectedspeed40pw4.5$Grade == "6.0", ])
CalcGradeSpeed(df.expectedspeed40pw6.0[df.expectedspeed40pw6.0$Grade == "6.0", ])
CalcGradeSpeed(df.expectedspeed40pw8.0[df.expectedspeed40pw8.0$Grade == "6.0", ])
CalcGradeSpeed(df.expectedspeed40pw9.3[df.expectedspeed40pw9.3$Grade == "6.0", ])

CalcGradeSpeed(df.expectedspeed40pw4.5[df.expectedspeed40pw4.5$Grade == "7.0", ])
CalcGradeSpeed(df.expectedspeed40pw6.0[df.expectedspeed40pw6.0$Grade == "7.0", ])
CalcGradeSpeed(df.expectedspeed40pw8.0[df.expectedspeed40pw8.0$Grade == "7.0", ])
CalcGradeSpeed(df.expectedspeed40pw9.3[df.expectedspeed40pw9.3$Grade == "7.0", ])


df.expectedspeed80grade5.0[df.expectedspeed80grade5.0$Station%/%1-600==0,]
df.expectedspeed60grade6.0[df.expectedspeed60grade6.0$Station%/%1-400==0,]

























