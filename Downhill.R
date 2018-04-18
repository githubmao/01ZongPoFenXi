#-----------------------Code Description---------------------------------------#
# Notes:
# ver1.0, data: 20170714, by MaoY
#
# Description:
# 深圳妈湾项目研究内容1：纵坡设计参数与典型货运车辆动力性能特征调研研究
#     下坡工况分析
#     数据来源于PIARC Grade_analysis分析软件。
# 稳定速度、轴数（总质量按对应轴数最高限重）、制动鼓温度之间的相互关系。
#------------------------------------------------------------------------------#


# 加载相关package
library(data.table)
library(ggplot2)


# 导入数据
setwd(dir = "E:/R/MaWan/01ZongPoFenXi/DownhillData")  # 设置工作目录
kDownhillFileList <- list.files(pattern = "*.txt")  # 分析数据文件名
kDownhillDataName <- gsub(".txt", "", kDownhillFileList)  # 导入数据集命名
df.downhill <- data.frame()  # 创建用于合并数据的数据集

for(i in 1:length(kDownhillDataName)){  # 导入数据
  
  kColName <- c("Distance",
                "SegmentLength",
                "Grade",
                "Speed",
                "Braking",
                "NormalTemp",
                "EmrgTemp")
  
  tmp.file2data <- fread(kDownhillFileList[i],
                         header = T,
                         sep = "auto",
                         stringsAsFactors = FALSE,
                         data.table = FALSE,
                         skip = "(m)",
                         col.names = kColName)
  
  # 添加下坡稳定速度（初始速度or稳定速度）
  tmp.file2data$ExpectedSpeed <- substr(kDownhillDataName[i], 1, 2)
  # 添加坡度
  tmp.file2data$Grade <- paste("-", substr(kDownhillDataName[i], 4, 6), sep = "")
  # 添加车辆总质量
  tmp.file2data$Mass <- substr(kDownhillDataName[i], 8, 9)
  
  df.downhill <- rbind(tmp.file2data, df.downhill)
}


# 1.固定下坡速度，固定轴数（总质量），不同坡度vs坡长----------------------------
# 1.1 下坡速度80，总质量49（6轴），不同坡度vs坡长----
df.speed80mass49 <- df.downhill[df.downhill$Speed == 80 &
                                  df.downhill$Mass == "49",]

plot.speed80mass49 <- ggplot(df.speed80mass49,
                             aes(x = Distance, y = NormalTemp))+
  geom_line(aes(colour = factor(Grade)), size = 1.2)+
  geom_hline(yintercept = 200, linetype = "dashed", color = "orange", size = 1.0)+
  geom_hline(yintercept = 260, linetype = "dashed", color = "red", size = 1.0)+
  scale_x_continuous(limits = c(0, 5000), breaks = seq(0, 5000, 500))+
  scale_y_continuous(limits = c(0, 300), breaks = seq(0, 300, 50))+
  scale_colour_hue("坡度")+
  labs(title = "下坡速度80km/h, 6轴49t货车", x = "位置", y = "温度,℃")+
  theme(legend.title = element_text(face = "bold", size = 10),
        legend.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 11),
        axis.text = element_text(face = "bold", size = 10))

plot.speed80mass49


# 1.2 下坡速度80，总质量43（5轴），不同坡度vs坡长----
df.speed80mass43 <- df.downhill[df.downhill$Speed == 80 &
                                  df.downhill$Mass == "43",]

plot.speed80mass43 <- ggplot(df.speed80mass43,
                             aes(x = Distance, y = NormalTemp))+
  geom_line(aes(colour = factor(Grade)), size = 1.2)+
  geom_hline(yintercept = 200, linetype = "dashed", color = "orange", size = 1.0)+
  geom_hline(yintercept = 260, linetype = "dashed", color = "red", size = 1.0)+
  scale_x_continuous(limits = c(0, 5000), breaks = seq(0, 5000, 500))+
  scale_y_continuous(limits = c(0, 300), breaks = seq(0, 300, 50))+
  scale_colour_hue("坡度")+
  labs(title = "下坡速度80km/h, 5轴43t货车", x = "位置", y = "温度,℃")+
  theme(legend.title = element_text(face = "bold", size = 10),
        legend.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 11),
        axis.text = element_text(face = "bold", size = 10))

plot.speed80mass43


# 1.3 下坡速度80，总质量36（4轴），不同坡度vs坡长----
df.speed80mass36 <- df.downhill[df.downhill$Speed == 80 &
                                  df.downhill$Mass == "36",]

plot.speed80mass36 <- ggplot(df.speed80mass36,
                             aes(x = Distance, y = NormalTemp))+
  geom_line(aes(colour = factor(Grade)), size = 1.2)+
  geom_hline(yintercept = 200, linetype = "dashed", color = "orange", size = 1.0)+
  geom_hline(yintercept = 260, linetype = "dashed", color = "red", size = 1.0)+
  scale_x_continuous(limits = c(0, 5000), breaks = seq(0, 5000, 500))+
  scale_y_continuous(limits = c(0, 300), breaks = seq(0, 300, 50))+
  scale_colour_hue("坡度")+
  labs(title = "下坡速度80km/h, 4轴36t货车", x = "位置", y = "温度,℃")+
  theme(legend.title = element_text(face = "bold", size = 10),
        legend.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 11),
        axis.text = element_text(face = "bold", size = 10))

plot.speed80mass36


# 1.4 下坡速度80，总质量27（3轴），不同坡度vs坡长----
df.speed80mass27 <- df.downhill[df.downhill$Speed == 80 &
                                  df.downhill$Mass == "27",]

plot.speed80mass27 <- ggplot(df.speed80mass27,
                             aes(x = Distance, y = NormalTemp))+
  geom_line(aes(colour = factor(Grade)), size = 1.2)+
  geom_hline(yintercept = 200, linetype = "dashed", color = "orange", size = 1.0)+
  geom_hline(yintercept = 260, linetype = "dashed", color = "red", size = 1.0)+
  scale_x_continuous(limits = c(0, 5000), breaks = seq(0, 5000, 500))+
  scale_y_continuous(limits = c(0, 300), breaks = seq(0, 300, 50))+
  scale_colour_hue("坡度")+
  labs(title = "下坡速度80km/h, 3轴27t货车", x = "位置", y = "温度,℃")+
  theme(legend.title = element_text(face = "bold", size = 10),
        legend.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 11),
        axis.text = element_text(face = "bold", size = 10))

plot.speed80mass27


# 1.5 下坡速度60，总质量49（6轴），不同坡度vs坡长----
df.speed60mass49 <- df.downhill[df.downhill$Speed == 60 &
                                  df.downhill$Mass == "49",]

plot.speed60mass49 <- ggplot(df.speed60mass49,
                             aes(x = Distance, y = NormalTemp))+
  geom_line(aes(colour = factor(Grade)), size = 1.2)+
  geom_hline(yintercept = 200, linetype = "dashed", color = "orange", size = 1.0)+
  geom_hline(yintercept = 260, linetype = "dashed", color = "red", size = 1.0)+
  scale_x_continuous(limits = c(0, 5000), breaks = seq(0, 5000, 500))+
  scale_y_continuous(limits = c(0, 300), breaks = seq(0, 300, 50))+
  scale_colour_hue("坡度")+
  labs(title = "下坡速度60km/h, 6轴49t货车", x = "位置", y = "温度,℃")+
  theme(legend.title = element_text(face = "bold", size = 10),
        legend.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 11),
        axis.text = element_text(face = "bold", size = 10))

plot.speed60mass49


# 1.6 下坡速度60，总质量43（5轴），不同坡度vs坡长----
df.speed60mass43 <- df.downhill[df.downhill$Speed == 60 &
                                  df.downhill$Mass == "43",]

plot.speed60mass43<- ggplot(df.speed60mass43,
                             aes(x = Distance, y = NormalTemp))+
  geom_line(aes(colour = factor(Grade)), size = 1.2)+
  geom_hline(yintercept = 200, linetype = "dashed", color = "orange", size = 1.0)+
  geom_hline(yintercept = 260, linetype = "dashed", color = "red", size = 1.0)+
  scale_x_continuous(limits = c(0, 5000), breaks = seq(0, 5000, 500))+
  scale_y_continuous(limits = c(0, 300), breaks = seq(0, 300, 50))+
  scale_colour_hue("坡度")+
  labs(title = "下坡速度60km/h, 5轴43t货车", x = "位置", y = "温度,℃")+
  theme(legend.title = element_text(face = "bold", size = 10),
        legend.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 11),
        axis.text = element_text(face = "bold", size = 10))

plot.speed60mass43


# 1.7 下坡速度60，总质量36（4轴），不同坡度vs坡长----
df.speed60mass36 <- df.downhill[df.downhill$Speed == 60 &
                                  df.downhill$Mass == "36",]

plot.speed60mass36<- ggplot(df.speed60mass36,
                            aes(x = Distance, y = NormalTemp))+
  geom_line(aes(colour = factor(Grade)), size = 1.2)+
  geom_hline(yintercept = 200, linetype = "dashed", color = "orange", size = 1.0)+
  geom_hline(yintercept = 260, linetype = "dashed", color = "red", size = 1.0)+
  scale_x_continuous(limits = c(0, 5000), breaks = seq(0, 5000, 500))+
  scale_y_continuous(limits = c(0, 300), breaks = seq(0, 300, 50))+
  scale_colour_hue("坡度")+
  labs(title = "下坡速度60km/h, 4轴36t货车", x = "位置", y = "温度,℃")+
  theme(legend.title = element_text(face = "bold", size = 10),
        legend.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 11),
        axis.text = element_text(face = "bold", size = 10))

plot.speed60mass36


# 1.8 下坡速度60，总质量27（3轴），不同坡度vs坡长----
df.speed60mass27 <- df.downhill[df.downhill$Speed == 60 &
                                  df.downhill$Mass == "27",]

plot.speed60mass27<- ggplot(df.speed60mass27,
                            aes(x = Distance, y = NormalTemp))+
  geom_line(aes(colour = factor(Grade)), size = 1.2)+
  geom_hline(yintercept = 200, linetype = "dashed", color = "orange", size = 1.0)+
  geom_hline(yintercept = 260, linetype = "dashed", color = "red", size = 1.0)+
  scale_x_continuous(limits = c(0, 5000), breaks = seq(0, 5000, 500))+
  scale_y_continuous(limits = c(0, 300), breaks = seq(0, 300, 50))+
  scale_colour_hue("坡度")+
  labs(title = "下坡速度60km/h, 3轴27t货车", x = "位置", y = "温度,℃")+
  theme(legend.title = element_text(face = "bold", size = 10),
        legend.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 11),
        axis.text = element_text(face = "bold", size = 10))

plot.speed60mass27


# 1.9 下坡速度40，总质量49（6轴），不同坡度vs坡长----
df.speed40mass49 <- df.downhill[df.downhill$Speed == 40 &
                                  df.downhill$Mass == "49",]

plot.speed40mass49 <- ggplot(df.speed40mass49,
                             aes(x = Distance, y = NormalTemp))+
  geom_line(aes(colour = factor(Grade)), size = 1.2)+
  geom_hline(yintercept = 200, linetype = "dashed", color = "orange", size = 1.0)+
  geom_hline(yintercept = 260, linetype = "dashed", color = "red", size = 1.0)+
  scale_x_continuous(limits = c(0, 5000), breaks = seq(0, 5000, 500))+
  scale_y_continuous(limits = c(0, 300), breaks = seq(0, 300, 50))+
  scale_colour_hue("坡度")+
  labs(title = "下坡速度40km/h, 6轴49t货车", x = "位置", y = "温度,℃")+
  theme(legend.title = element_text(face = "bold", size = 10),
        legend.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 11),
        axis.text = element_text(face = "bold", size = 10))

plot.speed40mass49


# 1.10 下坡速度40，总质量43（5轴），不同坡度vs坡长----
df.speed40mass43 <- df.downhill[df.downhill$Speed == 40 &
                                  df.downhill$Mass == "43",]

plot.speed40mass43 <- ggplot(df.speed40mass43,
                             aes(x = Distance, y = NormalTemp))+
  geom_line(aes(colour = factor(Grade)), size = 1.2)+
  geom_hline(yintercept = 200, linetype = "dashed", color = "orange", size = 1.0)+
  geom_hline(yintercept = 260, linetype = "dashed", color = "red", size = 1.0)+
  scale_x_continuous(limits = c(0, 5000), breaks = seq(0, 5000, 500))+
  scale_y_continuous(limits = c(0, 300), breaks = seq(0, 300, 50))+
  scale_colour_hue("坡度")+
  labs(title = "下坡速度40km/h, 5轴43t货车", x = "位置", y = "温度,℃")+
  theme(legend.title = element_text(face = "bold", size = 10),
        legend.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 11),
        axis.text = element_text(face = "bold", size = 10))

plot.speed40mass43


# 1.11 下坡速度40，总质量36（4轴），不同坡度vs坡长----
df.speed40mass36 <- df.downhill[df.downhill$Speed == 40 &
                                  df.downhill$Mass == "36",]

plot.speed40mass36 <- ggplot(df.speed40mass36,
                             aes(x = Distance, y = NormalTemp))+
  geom_line(aes(colour = factor(Grade)), size = 1.2)+
  geom_hline(yintercept = 200, linetype = "dashed", color = "orange", size = 1.0)+
  geom_hline(yintercept = 260, linetype = "dashed", color = "red", size = 1.0)+
  scale_x_continuous(limits = c(0, 5000), breaks = seq(0, 5000, 500))+
  scale_y_continuous(limits = c(0, 300), breaks = seq(0, 300, 50))+
  scale_colour_hue("坡度")+
  labs(title = "下坡速度40km/h, 4轴36t货车", x = "位置", y = "温度,℃")+
  theme(legend.title = element_text(face = "bold", size = 10),
        legend.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 11),
        axis.text = element_text(face = "bold", size = 10))

plot.speed40mass36


# 1.12 下坡速度40，总质量27（3轴），不同坡度vs坡长----
df.speed40mass27 <- df.downhill[df.downhill$Speed == 40 &
                                  df.downhill$Mass == "27",]

plot.speed40mass27 <- ggplot(df.speed40mass27,
                             aes(x = Distance, y = NormalTemp))+
  geom_line(aes(colour = factor(Grade)), size = 1.2)+
  geom_hline(yintercept = 200, linetype = "dashed", color = "orange", size = 1.0)+
  geom_hline(yintercept = 260, linetype = "dashed", color = "red", size = 1.0)+
  scale_x_continuous(limits = c(0, 5000), breaks = seq(0, 5000, 500))+
  scale_y_continuous(limits = c(0, 300), breaks = seq(0, 300, 50))+
  scale_colour_hue("坡度")+
  labs(title = "下坡速度40km/h, 3轴27t货车", x = "位置", y = "温度,℃")+
  theme(legend.title = element_text(face = "bold", size = 10),
        legend.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 11),
        axis.text = element_text(face = "bold", size = 10))

plot.speed40mass27


# 2.固定下坡速度，固定坡度，轴数（总质量）vs坡长--------------------------------
# 2.1 下坡速度80，固定坡度-2.5，不同轴数（总质量）vs坡长----
df.speed80grade2.5 <- df.downhill[df.downhill$Speed == 80 &
                                  df.downhill$Grade == "-2.5",]

plot.speed80grade2.5 <- ggplot(df.speed80grade2.5,
                             aes(x = Distance, y = NormalTemp))+
  geom_line(aes(colour = factor(Mass)), size = 1.2)+
  geom_hline(yintercept = 200, linetype = "dashed", color = "orange", size = 1.0)+
  geom_hline(yintercept = 260, linetype = "dashed", color = "red", size = 1.0)+
  scale_x_continuous(limits = c(0, 5000), breaks = seq(0, 5000, 500))+
  scale_y_continuous(limits = c(0, 300), breaks = seq(0, 300, 50))+
  scale_colour_hue(name="轴数，总质量", labels=c("3轴27t", "4轴36t", "5轴43t", "6轴49t"))+
  labs(title = "下坡速度80km/h, 坡度2.5%", x = "位置", y = "温度,℃")+
  theme(legend.title = element_text(face = "bold", size = 10),
        legend.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 11),
        axis.text = element_text(face = "bold", size = 10))

plot.speed80grade2.5


# 2.2 下坡速度80，固定坡度-3.0，不同轴数（总质量）vs坡长----
df.speed80grade3.0 <- df.downhill[df.downhill$Speed == 80 &
                                    df.downhill$Grade == "-3.0",]

plot.speed80grade3.0 <- ggplot(df.speed80grade3.0,
                               aes(x = Distance, y = NormalTemp))+
  geom_line(aes(colour = factor(Mass)), size = 1.2)+
  geom_hline(yintercept = 200, linetype = "dashed", color = "orange", size = 1.0)+
  geom_hline(yintercept = 260, linetype = "dashed", color = "red", size = 1.0)+
  scale_x_continuous(limits = c(0, 5000), breaks = seq(0, 5000, 500))+
  scale_y_continuous(limits = c(0, 300), breaks = seq(0, 300, 50))+
  scale_colour_hue(name="轴数，总质量", labels=c("3轴27t", "4轴36t", "5轴43t", "6轴49t"))+
  labs(title = "下坡速度80km/h, 坡度3.0%", x = "位置", y = "温度,℃")+
  theme(legend.title = element_text(face = "bold", size = 10),
        legend.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 11),
        axis.text = element_text(face = "bold", size = 10))

plot.speed80grade3.0


# 2.3 下坡速度80，固定坡度-3.5，不同轴数（总质量）vs坡长----
df.speed80grade3.5 <- df.downhill[df.downhill$Speed == 80 &
                                    df.downhill$Grade == "-3.5",]

plot.speed80grade3.5 <- ggplot(df.speed80grade3.5,
                               aes(x = Distance, y = NormalTemp))+
  geom_line(aes(colour = factor(Mass)), size = 1.2)+
  geom_hline(yintercept = 200, linetype = "dashed", color = "orange", size = 1.0)+
  geom_hline(yintercept = 260, linetype = "dashed", color = "red", size = 1.0)+
  scale_x_continuous(limits = c(0, 5000), breaks = seq(0, 5000, 500))+
  scale_y_continuous(limits = c(0, 300), breaks = seq(0, 300, 50))+
  scale_colour_hue(name="轴数，总质量", labels=c("3轴27t", "4轴36t", "5轴43t", "6轴49t"))+
  labs(title = "下坡速度80km/h, 坡度3.5%", x = "位置", y = "温度,℃")+
  theme(legend.title = element_text(face = "bold", size = 10),
        legend.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 11),
        axis.text = element_text(face = "bold", size = 10))

plot.speed80grade3.5


# 2.4 下坡速度80，固定坡度-4.0，不同轴数（总质量）vs坡长----
df.speed80grade4.0 <- df.downhill[df.downhill$Speed == 80 &
                                    df.downhill$Grade == "-4.0",]

plot.speed80grade4.0 <- ggplot(df.speed80grade4.0,
                               aes(x = Distance, y = NormalTemp))+
  geom_line(aes(colour = factor(Mass)), size = 1.2)+
  geom_hline(yintercept = 200, linetype = "dashed", color = "orange", size = 1.0)+
  geom_hline(yintercept = 260, linetype = "dashed", color = "red", size = 1.0)+
  scale_x_continuous(limits = c(0, 5000), breaks = seq(0, 5000, 500))+
  scale_y_continuous(limits = c(0, 300), breaks = seq(0, 300, 50))+
  scale_colour_hue(name="轴数，总质量", labels=c("3轴27t", "4轴36t", "5轴43t", "6轴49t"))+
  labs(title = "下坡速度80km/h, 坡度4.0%", x = "位置", y = "温度,℃")+
  theme(legend.title = element_text(face = "bold", size = 10),
        legend.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 11),
        axis.text = element_text(face = "bold", size = 10))

plot.speed80grade4.0


# 2.5 下坡速度80，固定坡度-4.5，不同轴数（总质量）vs坡长----
df.speed80grade4.5 <- df.downhill[df.downhill$Speed == 80 &
                                    df.downhill$Grade == "-4.5",]

plot.speed80grade4.5 <- ggplot(df.speed80grade4.5,
                               aes(x = Distance, y = NormalTemp))+
  geom_line(aes(colour = factor(Mass)), size = 1.2)+
  geom_hline(yintercept = 200, linetype = "dashed", color = "orange", size = 1.0)+
  geom_hline(yintercept = 260, linetype = "dashed", color = "red", size = 1.0)+
  scale_x_continuous(limits = c(0, 5000), breaks = seq(0, 5000, 500))+
  scale_y_continuous(limits = c(0, 300), breaks = seq(0, 300, 50))+
  scale_colour_hue(name="轴数，总质量", labels=c("3轴27t", "4轴36t", "5轴43t", "6轴49t"))+
  labs(title = "下坡速度80km/h, 坡度4.5%", x = "位置", y = "温度,℃")+
  theme(legend.title = element_text(face = "bold", size = 10),
        legend.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 11),
        axis.text = element_text(face = "bold", size = 10))

plot.speed80grade4.5


# 2.6 下坡速度80，固定坡度-5.0，不同轴数（总质量）vs坡长----
df.speed80grade5.0 <- df.downhill[df.downhill$Speed == 80 &
                                    df.downhill$Grade == "-5.0",]

plot.speed80grade5.0 <- ggplot(df.speed80grade5.0,
                               aes(x = Distance, y = NormalTemp))+
  geom_line(aes(colour = factor(Mass)), size = 1.2)+
  geom_hline(yintercept = 200, linetype = "dashed", color = "orange", size = 1.0)+
  geom_hline(yintercept = 260, linetype = "dashed", color = "red", size = 1.0)+
  scale_x_continuous(limits = c(0, 5000), breaks = seq(0, 5000, 500))+
  scale_y_continuous(limits = c(0, 300), breaks = seq(0, 300, 50))+
  scale_colour_hue(name="轴数，总质量", labels=c("3轴27t", "4轴36t", "5轴43t", "6轴49t"))+
  labs(title = "下坡速度80km/h, 坡度5.0%", x = "位置", y = "温度,℃")+
  theme(legend.title = element_text(face = "bold", size = 10),
        legend.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 11),
        axis.text = element_text(face = "bold", size = 10))

plot.speed80grade5.0


# 2.7 下坡速度80，固定坡度-5.5，不同轴数（总质量）vs坡长----
df.speed80grade5.5 <- df.downhill[df.downhill$Speed == 80 &
                                    df.downhill$Grade == "-5.5",]

plot.speed80grade5.5 <- ggplot(df.speed80grade5.5,
                               aes(x = Distance, y = NormalTemp))+
  geom_line(aes(colour = factor(Mass)), size = 1.2)+
  geom_hline(yintercept = 200, linetype = "dashed", color = "orange", size = 1.0)+
  geom_hline(yintercept = 260, linetype = "dashed", color = "red", size = 1.0)+
  scale_x_continuous(limits = c(0, 5000), breaks = seq(0, 5000, 500))+
  scale_y_continuous(limits = c(0, 300), breaks = seq(0, 300, 50))+
  scale_colour_hue(name="轴数，总质量", labels=c("3轴27t", "4轴36t", "5轴43t", "6轴49t"))+
  labs(title = "下坡速度80km/h, 坡度5.5%", x = "位置", y = "温度,℃")+
  theme(legend.title = element_text(face = "bold", size = 10),
        legend.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 11),
        axis.text = element_text(face = "bold", size = 10))

plot.speed80grade5.5


# 2.8 下坡速度80，固定坡度-6.0，不同轴数（总质量）vs坡长----
df.speed80grade6.0 <- df.downhill[df.downhill$Speed == 80 &
                                    df.downhill$Grade == "-6.0",]

plot.speed80grade6.0 <- ggplot(df.speed80grade6.0,
                               aes(x = Distance, y = NormalTemp))+
  geom_line(aes(colour = factor(Mass)), size = 1.2)+
  geom_hline(yintercept = 200, linetype = "dashed", color = "orange", size = 1.0)+
  geom_hline(yintercept = 260, linetype = "dashed", color = "red", size = 1.0)+
  scale_x_continuous(limits = c(0, 5000), breaks = seq(0, 5000, 500))+
  scale_y_continuous(limits = c(0, 300), breaks = seq(0, 300, 50))+
  scale_colour_hue(name="轴数，总质量", labels=c("3轴27t", "4轴36t", "5轴43t", "6轴49t"))+
  labs(title = "下坡速度80km/h, 坡度6.0%", x = "位置", y = "温度,℃")+
  theme(legend.title = element_text(face = "bold", size = 10),
        legend.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 11),
        axis.text = element_text(face = "bold", size = 10))

plot.speed80grade6.0


# 2.9 下坡速度60，固定坡度-2.5，不同轴数（总质量）vs坡长----
df.speed60grade2.5 <- df.downhill[df.downhill$Speed == 60 &
                                    df.downhill$Grade == "-2.5",]

plot.speed60grade2.5 <- ggplot(df.speed60grade2.5,
                               aes(x = Distance, y = NormalTemp))+
  geom_line(aes(colour = factor(Mass)), size = 1.2)+
  geom_hline(yintercept = 200, linetype = "dashed", color = "orange", size = 1.0)+
  geom_hline(yintercept = 260, linetype = "dashed", color = "red", size = 1.0)+
  scale_x_continuous(limits = c(0, 5000), breaks = seq(0, 5000, 500))+
  scale_y_continuous(limits = c(0, 300), breaks = seq(0, 300, 50))+
  scale_colour_hue(name="轴数，总质量", labels=c("3轴27t", "4轴36t", "5轴43t", "6轴49t"))+
  labs(title = "下坡速度60km/h, 坡度2.5%", x = "位置", y = "温度,℃")+
  theme(legend.title = element_text(face = "bold", size = 10),
        legend.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 11),
        axis.text = element_text(face = "bold", size = 10))

plot.speed60grade2.5


# 2.10 下坡速度60，固定坡度-3.0，不同轴数（总质量）vs坡长----
df.speed60grade3.0 <- df.downhill[df.downhill$Speed == 60 &
                                    df.downhill$Grade == "-3.0",]

plot.speed60grade3.0 <- ggplot(df.speed60grade3.0,
                               aes(x = Distance, y = NormalTemp))+
  geom_line(aes(colour = factor(Mass)), size = 1.2)+
  geom_hline(yintercept = 200, linetype = "dashed", color = "orange", size = 1.0)+
  geom_hline(yintercept = 260, linetype = "dashed", color = "red", size = 1.0)+
  scale_x_continuous(limits = c(0, 5000), breaks = seq(0, 5000, 500))+
  scale_y_continuous(limits = c(0, 300), breaks = seq(0, 300, 50))+
  scale_colour_hue(name="轴数，总质量", labels=c("3轴27t", "4轴36t", "5轴43t", "6轴49t"))+
  labs(title = "下坡速度60km/h, 坡度3.0%", x = "位置", y = "温度,℃")+
  theme(legend.title = element_text(face = "bold", size = 10),
        legend.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 11),
        axis.text = element_text(face = "bold", size = 10))

plot.speed60grade3.0


# 2.11 下坡速度60，固定坡度-3.5，不同轴数（总质量）vs坡长----
df.speed60grade3.5 <- df.downhill[df.downhill$Speed == 60 &
                                    df.downhill$Grade == "-3.5",]

plot.speed60grade3.5 <- ggplot(df.speed60grade3.5,
                               aes(x = Distance, y = NormalTemp))+
  geom_line(aes(colour = factor(Mass)), size = 1.2)+
  geom_hline(yintercept = 200, linetype = "dashed", color = "orange", size = 1.0)+
  geom_hline(yintercept = 260, linetype = "dashed", color = "red", size = 1.0)+
  scale_x_continuous(limits = c(0, 5000), breaks = seq(0, 5000, 500))+
  scale_y_continuous(limits = c(0, 300), breaks = seq(0, 300, 50))+
  scale_colour_hue(name="轴数，总质量", labels=c("3轴27t", "4轴36t", "5轴43t", "6轴49t"))+
  labs(title = "下坡速度60km/h, 坡度3.5%", x = "位置", y = "温度,℃")+
  theme(legend.title = element_text(face = "bold", size = 10),
        legend.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 11),
        axis.text = element_text(face = "bold", size = 10))

plot.speed60grade3.5


# 2.12 下坡速度60，固定坡度-4.0，不同轴数（总质量）vs坡长----
df.speed60grade4.0 <- df.downhill[df.downhill$Speed == 60 &
                                    df.downhill$Grade == "-4.0",]

plot.speed60grade4.0 <- ggplot(df.speed60grade4.0,
                               aes(x = Distance, y = NormalTemp))+
  geom_line(aes(colour = factor(Mass)), size = 1.2)+
  geom_hline(yintercept = 200, linetype = "dashed", color = "orange", size = 1.0)+
  geom_hline(yintercept = 260, linetype = "dashed", color = "red", size = 1.0)+
  scale_x_continuous(limits = c(0, 5000), breaks = seq(0, 5000, 500))+
  scale_y_continuous(limits = c(0, 300), breaks = seq(0, 300, 50))+
  scale_colour_hue(name="轴数，总质量", labels=c("3轴27t", "4轴36t", "5轴43t", "6轴49t"))+
  labs(title = "下坡速度60km/h, 坡度4.0%", x = "位置", y = "温度,℃")+
  theme(legend.title = element_text(face = "bold", size = 10),
        legend.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 11),
        axis.text = element_text(face = "bold", size = 10))

plot.speed60grade4.0


# 2.13 下坡速度60，固定坡度-4.5，不同轴数（总质量）vs坡长----
df.speed60grade4.5 <- df.downhill[df.downhill$Speed == 60 &
                                    df.downhill$Grade == "-4.5",]

plot.speed60grade4.5 <- ggplot(df.speed60grade4.5,
                               aes(x = Distance, y = NormalTemp))+
  geom_line(aes(colour = factor(Mass)), size = 1.2)+
  geom_hline(yintercept = 200, linetype = "dashed", color = "orange", size = 1.0)+
  geom_hline(yintercept = 260, linetype = "dashed", color = "red", size = 1.0)+
  scale_x_continuous(limits = c(0, 5000), breaks = seq(0, 5000, 500))+
  scale_y_continuous(limits = c(0, 300), breaks = seq(0, 300, 50))+
  scale_colour_hue(name="轴数，总质量", labels=c("3轴27t", "4轴36t", "5轴43t", "6轴49t"))+
  labs(title = "下坡速度60km/h, 坡度4.5%", x = "位置", y = "温度,℃")+
  theme(legend.title = element_text(face = "bold", size = 10),
        legend.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 11),
        axis.text = element_text(face = "bold", size = 10))

plot.speed60grade4.5


# 2.14 下坡速度60，固定坡度-5.0，不同轴数（总质量）vs坡长----
df.speed60grade5.0 <- df.downhill[df.downhill$Speed == 60 &
                                    df.downhill$Grade == "-5.0",]

plot.speed60grade5.0 <- ggplot(df.speed60grade5.0,
                               aes(x = Distance, y = NormalTemp))+
  geom_line(aes(colour = factor(Mass)), size = 1.2)+
  geom_hline(yintercept = 200, linetype = "dashed", color = "orange", size = 1.0)+
  geom_hline(yintercept = 260, linetype = "dashed", color = "red", size = 1.0)+
  scale_x_continuous(limits = c(0, 5000), breaks = seq(0, 5000, 500))+
  scale_y_continuous(limits = c(0, 300), breaks = seq(0, 300, 50))+
  scale_colour_hue(name="轴数，总质量", labels=c("3轴27t", "4轴36t", "5轴43t", "6轴49t"))+
  labs(title = "下坡速度60km/h, 坡度5.0%", x = "位置", y = "温度,℃")+
  theme(legend.title = element_text(face = "bold", size = 10),
        legend.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 11),
        axis.text = element_text(face = "bold", size = 10))

plot.speed60grade5.0


# 2.15 下坡速度60，固定坡度-5.5，不同轴数（总质量）vs坡长----
df.speed60grade5.5 <- df.downhill[df.downhill$Speed == 60 &
                                    df.downhill$Grade == "-5.5",]

plot.speed60grade5.5 <- ggplot(df.speed60grade5.5,
                               aes(x = Distance, y = NormalTemp))+
  geom_line(aes(colour = factor(Mass)), size = 1.2)+
  geom_hline(yintercept = 200, linetype = "dashed", color = "orange", size = 1.0)+
  geom_hline(yintercept = 260, linetype = "dashed", color = "red", size = 1.0)+
  scale_x_continuous(limits = c(0, 5000), breaks = seq(0, 5000, 500))+
  scale_y_continuous(limits = c(0, 300), breaks = seq(0, 300, 50))+
  scale_colour_hue(name="轴数，总质量", labels=c("3轴27t", "4轴36t", "5轴43t", "6轴49t"))+
  labs(title = "下坡速度60km/h, 坡度5.5%", x = "位置", y = "温度,℃")+
  theme(legend.title = element_text(face = "bold", size = 10),
        legend.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 11),
        axis.text = element_text(face = "bold", size = 10))

plot.speed60grade5.5


# 2.16 下坡速度60，固定坡度-6.0，不同轴数（总质量）vs坡长----
df.speed60grade6.0 <- df.downhill[df.downhill$Speed == 60 &
                                    df.downhill$Grade == "-6.0",]

plot.speed60grade6.0 <- ggplot(df.speed60grade6.0,
                               aes(x = Distance, y = NormalTemp))+
  geom_line(aes(colour = factor(Mass)), size = 1.2)+
  geom_hline(yintercept = 200, linetype = "dashed", color = "orange", size = 1.0)+
  geom_hline(yintercept = 260, linetype = "dashed", color = "red", size = 1.0)+
  scale_x_continuous(limits = c(0, 5000), breaks = seq(0, 5000, 500))+
  scale_y_continuous(limits = c(0, 300), breaks = seq(0, 300, 50))+
  scale_colour_hue(name="轴数，总质量", labels=c("3轴27t", "4轴36t", "5轴43t", "6轴49t"))+
  labs(title = "下坡速度60km/h, 坡度6.0%", x = "位置", y = "温度,℃")+
  theme(legend.title = element_text(face = "bold", size = 10),
        legend.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 11),
        axis.text = element_text(face = "bold", size = 10))

plot.speed60grade6.0


# 2.17 下坡速度40，固定坡度-2.5，不同轴数（总质量）vs坡长----
df.speed40grade2.5 <- df.downhill[df.downhill$Speed == 40 &
                                    df.downhill$Grade == "-2.5",]

plot.speed40grade2.5 <- ggplot(df.speed40grade2.5,
                               aes(x = Distance, y = NormalTemp))+
  geom_line(aes(colour = factor(Mass)), size = 1.2)+
  geom_hline(yintercept = 200, linetype = "dashed", color = "orange", size = 1.0)+
  geom_hline(yintercept = 260, linetype = "dashed", color = "red", size = 1.0)+
  scale_x_continuous(limits = c(0, 5000), breaks = seq(0, 5000, 500))+
  scale_y_continuous(limits = c(0, 300), breaks = seq(0, 300, 50))+
  scale_colour_hue(name="轴数，总质量", labels=c("3轴27t", "4轴36t", "5轴43t", "6轴49t"))+
  labs(title = "下坡速度40km/h, 坡度2.5%", x = "位置", y = "温度,℃")+
  theme(legend.title = element_text(face = "bold", size = 10),
        legend.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 11),
        axis.text = element_text(face = "bold", size = 10))

plot.speed40grade2.5


# 2.18 下坡速度40，固定坡度-3.0，不同轴数（总质量）vs坡长----
df.speed40grade3.0 <- df.downhill[df.downhill$Speed == 40 &
                                    df.downhill$Grade == "-3.0",]

plot.speed40grade3.0 <- ggplot(df.speed40grade3.0,
                               aes(x = Distance, y = NormalTemp))+
  geom_line(aes(colour = factor(Mass)), size = 1.2)+
  geom_hline(yintercept = 200, linetype = "dashed", color = "orange", size = 1.0)+
  geom_hline(yintercept = 260, linetype = "dashed", color = "red", size = 1.0)+
  scale_x_continuous(limits = c(0, 5000), breaks = seq(0, 5000, 500))+
  scale_y_continuous(limits = c(0, 300), breaks = seq(0, 300, 50))+
  scale_colour_hue(name="轴数，总质量", labels=c("3轴27t", "4轴36t", "5轴43t", "6轴49t"))+
  labs(title = "下坡速度40km/h, 坡度3.0%", x = "位置", y = "温度,℃")+
  theme(legend.title = element_text(face = "bold", size = 10),
        legend.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 11),
        axis.text = element_text(face = "bold", size = 10))

plot.speed40grade3.0


# 2.19 下坡速度40，固定坡度-3.5，不同轴数（总质量）vs坡长----
df.speed40grade3.5 <- df.downhill[df.downhill$Speed == 40 &
                                    df.downhill$Grade == "-3.5",]

plot.speed40grade3.5 <- ggplot(df.speed40grade3.5,
                               aes(x = Distance, y = NormalTemp))+
  geom_line(aes(colour = factor(Mass)), size = 1.2)+
  geom_hline(yintercept = 200, linetype = "dashed", color = "orange", size = 1.0)+
  geom_hline(yintercept = 260, linetype = "dashed", color = "red", size = 1.0)+
  scale_x_continuous(limits = c(0, 5000), breaks = seq(0, 5000, 500))+
  scale_y_continuous(limits = c(0, 300), breaks = seq(0, 300, 50))+
  scale_colour_hue(name="轴数，总质量", labels=c("3轴27t", "4轴36t", "5轴43t", "6轴49t"))+
  labs(title = "下坡速度40km/h, 坡度3.5%", x = "位置", y = "温度,℃")+
  theme(legend.title = element_text(face = "bold", size = 10),
        legend.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 11),
        axis.text = element_text(face = "bold", size = 10))

plot.speed40grade3.5


# 2.20 下坡速度40，固定坡度-4.0，不同轴数（总质量）vs坡长----
df.speed40grade4.0 <- df.downhill[df.downhill$Speed == 40 &
                                    df.downhill$Grade == "-4.0",]

plot.speed40grade4.0 <- ggplot(df.speed40grade4.0,
                               aes(x = Distance, y = NormalTemp))+
  geom_line(aes(colour = factor(Mass)), size = 1.2)+
  geom_hline(yintercept = 200, linetype = "dashed", color = "orange", size = 1.0)+
  geom_hline(yintercept = 260, linetype = "dashed", color = "red", size = 1.0)+
  scale_x_continuous(limits = c(0, 5000), breaks = seq(0, 5000, 500))+
  scale_y_continuous(limits = c(0, 300), breaks = seq(0, 300, 50))+
  scale_colour_hue(name="轴数，总质量", labels=c("3轴27t", "4轴36t", "5轴43t", "6轴49t"))+
  labs(title = "下坡速度40km/h, 坡度4.0%", x = "位置", y = "温度,℃")+
  theme(legend.title = element_text(face = "bold", size = 10),
        legend.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 11),
        axis.text = element_text(face = "bold", size = 10))

plot.speed40grade4.0


# 2.21 下坡速度40，固定坡度-4.5，不同轴数（总质量）vs坡长----
df.speed40grade4.5 <- df.downhill[df.downhill$Speed == 40 &
                                    df.downhill$Grade == "-4.5",]

plot.speed40grade4.5 <- ggplot(df.speed40grade4.5,
                               aes(x = Distance, y = NormalTemp))+
  geom_line(aes(colour = factor(Mass)), size = 1.2)+
  geom_hline(yintercept = 200, linetype = "dashed", color = "orange", size = 1.0)+
  geom_hline(yintercept = 260, linetype = "dashed", color = "red", size = 1.0)+
  scale_x_continuous(limits = c(0, 5000), breaks = seq(0, 5000, 500))+
  scale_y_continuous(limits = c(0, 300), breaks = seq(0, 300, 50))+
  scale_colour_hue(name="轴数，总质量", labels=c("3轴27t", "4轴36t", "5轴43t", "6轴49t"))+
  labs(title = "下坡速度40km/h, 坡度4.5%", x = "位置", y = "温度,℃")+
  theme(legend.title = element_text(face = "bold", size = 10),
        legend.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 11),
        axis.text = element_text(face = "bold", size = 10))

plot.speed40grade4.5


# 2.22 下坡速度40，固定坡度-5.0，不同轴数（总质量）vs坡长----
df.speed40grade5.0 <- df.downhill[df.downhill$Speed == 40 &
                                    df.downhill$Grade == "-5.0",]

plot.speed40grade5.0 <- ggplot(df.speed40grade5.0,
                               aes(x = Distance, y = NormalTemp))+
  geom_line(aes(colour = factor(Mass)), size = 1.2)+
  geom_hline(yintercept = 200, linetype = "dashed", color = "orange", size = 1.0)+
  geom_hline(yintercept = 260, linetype = "dashed", color = "red", size = 1.0)+
  scale_x_continuous(limits = c(0, 5000), breaks = seq(0, 5000, 500))+
  scale_y_continuous(limits = c(0, 300), breaks = seq(0, 300, 50))+
  scale_colour_hue(name="轴数，总质量", labels=c("3轴27t", "4轴36t", "5轴43t", "6轴49t"))+
  labs(title = "下坡速度40km/h, 坡度5.0%", x = "位置", y = "温度,℃")+
  theme(legend.title = element_text(face = "bold", size = 10),
        legend.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 11),
        axis.text = element_text(face = "bold", size = 10))

plot.speed40grade5.0


# 2.23 下坡速度40，固定坡度-5.5，不同轴数（总质量）vs坡长----
df.speed40grade5.5 <- df.downhill[df.downhill$Speed == 40 &
                                    df.downhill$Grade == "-5.5",]

plot.speed40grade5.5 <- ggplot(df.speed40grade5.5,
                               aes(x = Distance, y = NormalTemp))+
  geom_line(aes(colour = factor(Mass)), size = 1.2)+
  geom_hline(yintercept = 200, linetype = "dashed", color = "orange", size = 1.0)+
  geom_hline(yintercept = 260, linetype = "dashed", color = "red", size = 1.0)+
  scale_x_continuous(limits = c(0, 5000), breaks = seq(0, 5000, 500))+
  scale_y_continuous(limits = c(0, 300), breaks = seq(0, 300, 50))+
  scale_colour_hue(name="轴数，总质量", labels=c("3轴27t", "4轴36t", "5轴43t", "6轴49t"))+
  labs(title = "下坡速度40km/h, 坡度5.5%", x = "位置", y = "温度,℃")+
  theme(legend.title = element_text(face = "bold", size = 10),
        legend.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 11),
        axis.text = element_text(face = "bold", size = 10))

plot.speed40grade5.5


# 2.24 下坡速度40，固定坡度-6.0，不同轴数（总质量）vs坡长----
df.speed40grade6.0 <- df.downhill[df.downhill$Speed == 40 &
                                    df.downhill$Grade == "-6.0",]

plot.speed40grade6.0 <- ggplot(df.speed40grade6.0,
                               aes(x = Distance, y = NormalTemp))+
  geom_line(aes(colour = factor(Mass)), size = 1.2)+
  geom_hline(yintercept = 200, linetype = "dashed", color = "orange", size = 1.0)+
  geom_hline(yintercept = 260, linetype = "dashed", color = "red", size = 1.0)+
  scale_x_continuous(limits = c(0, 5000), breaks = seq(0, 5000, 500))+
  scale_y_continuous(limits = c(0, 300), breaks = seq(0, 300, 50))+
  scale_colour_hue(name="轴数，总质量", labels=c("3轴27t", "4轴36t", "5轴43t", "6轴49t"))+
  labs(title = "下坡速度40km/h, 坡度6.0%", x = "位置", y = "温度,℃")+
  theme(legend.title = element_text(face = "bold", size = 10),
        legend.text = element_text(face = "bold", size = 10),
        axis.title = element_text(face = "bold", size = 11),
        axis.text = element_text(face = "bold", size = 10))

plot.speed40grade6.0


df.speed60mass49[df.speed60mass49$NormalTemp>=200 &
                   df.speed60mass49$NormalTemp<=210,]



