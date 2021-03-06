
## Data Wrangling

library(readxl)
getwd()
IDinfo <- read_excel("Data/IDinfo.xls")
discrete <- read_excel("Data/discrete.xls")

AP_GRF_stance_N <- read.csv("Data/AP_GRF_stance_N.csv", header = FALSE)
ML_GRF_stance_N <- read.csv("Data/ML_GRF_stance_N.csv", header = FALSE)
V_GRF_stance_N <- read.csv("Data/V_GRF_stance_N.csv", header = FALSE)

COPx <- read.csv("Data/COPx.csv", header = FALSE)
COPx_stance <- read.csv("Data/COPx_stance.csv", header = FALSE)
COPy <- read.csv("Data/COPy.csv", header = FALSE)
COPy_stance <- read.csv("Data/COPy_stance.csv", header = FALSE)

GRFx <- read.csv("Data/GRFx.csv", header = FALSE)
GRFy <- read.csv("Data/GRFy.csv", header = FALSE)
GRFz <- read.csv("Data/GRFz.csv", header = FALSE)
Mx <- read.csv("Data/Mx.csv", header = FALSE)
My <- read.csv("Data/My.csv", header = FALSE)
Mz <- read.csv("Data/Mz.csv", header = FALSE)

dimension <- data.frame(cbind(dim(AP_GRF_stance_N), dim(ML_GRF_stance_N), dim(V_GRF_stance_N),
                              dim(COPx), dim(COPx_stance), dim(COPy), dim(COPy_stance),
                              dim(GRFx), dim(GRFy), dim(GRFz), dim(Mx), dim(My), dim(Mz)))

# GRF = ground reaction force 地面反作用力
# COP = center of pressure 压力中心
# AP = anterior-posterior 前后
# ML = medial-lateral 内侧-外侧
# V = vertical 垂直
# Stance = time when foot is in contact with the ground (heel strike to toe-off of the same foot)
# 脚和地面接触的时间 （同一只脚的脚跟触地到脚尖）







