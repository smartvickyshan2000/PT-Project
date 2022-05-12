library(ggplot2)
library(dbplyr)
library(dplyr)
library(randomcoloR)
GRFx_id <- cbind(IDinfo,GRFx)
GRFy_id <- cbind(IDinfo,GRFy)
GRFz_id <- cbind(IDinfo,GRFz)
GRFx_id_left <- GRFx_id %>% filter(KNEE== 'LEFT') 
GRFx_id_right <- GRFx_id %>% filter(KNEE== 'RIGHT') 
GRFy_id_left <- GRFy_id %>% filter(KNEE== 'LEFT')
GRFy_id_right <- GRFy_id %>% filter(KNEE== 'RIGHT') 
GRFz_id_left <- GRFz_id %>% filter(KNEE== 'LEFT') 
GRFz_id_right <- GRFz_id %>% filter(KNEE== 'RIGHT')

COPx_id <- cbind(IDinfo,COPx)
COPy_id <- cbind(IDinfo,COPy)
discrete_id <- cbind(IDinfo,discrete)

GRFx_id_left_avg_id <- aggregate(GRFx_id_left[, 5:2994], list(GRFx_id_left$ID), mean)
GRFx_id_right_avg_id <- aggregate(GRFx_id_right[, 5:2994], list(GRFx_id_right$ID), mean)
GRFy_id_left_avg_id <- aggregate(GRFy_id_left[, 5:2994], list(GRFy_id_left$ID), mean)
GRFy_id_right_avg_id <- aggregate(GRFy_id_right[, 5:2994], list(GRFy_id_right$ID), mean)
GRFz_id_left_avg_id <- aggregate(GRFz_id_left[, 5:2994], list(GRFz_id_left$ID), mean)
GRFz_id_right_avg_id <- aggregate(GRFz_id_right[, 5:2994], list(GRFx_id_right$ID), mean)

library(reshape)
dfm <- melt(GRFx_id_left_avg_id, id.vars=c("Group.1"))

ggplot(dfm, aes(x=as.numeric(variable), y=value, colour=Group.1)) + geom_line()

for (j in 1:5000){
  y = c()
  for (i in 2:ncol(GRFx_id_left_avg_id)){
    y[i] <- GRFx_id_left_avg_id[j,i]
  }
  plot(y,col=randomColor(1),type='l',ylim=c(-500,500))
  par(new=TRUE)
}

for (j in 1:500){
  y = c()
  for (i in 2:ncol(GRFy_id_left_avg_id)){
    y[i] <- GRFy_id_left_avg_id[j,i]
  }
  plot(y,col=randomColor(1),type='l',ylim=c(-200,200))
  par(new=TRUE)
}

for (j in 1:500){
  y = c()
  for (i in 2:ncol(GRFy_id_left_avg_id)){
    y[i] <- GRFz_id_left_avg_id[j,i]
  }
  plot(y,col=randomColor(1),type='l',ylim=c(0,2000))
  par(new=TRUE)
}

for (j in 1:500){
  y = c()
  for (i in 2:ncol(GRFx_id_right_avg_id)){
    y[i] <- GRFx_id_right_avg_id[j,i]
  }
  plot(y,col=randomColor(1),type='l',ylim=c(-500,500))
  par(new=TRUE)
}

for (j in 1:500){
  y = c()
  for (i in 2:ncol(GRFy_id_right_avg_id)){
    y[i] <- GRFy_id_right_avg_id[j,i]
  }
  plot(y,col=randomColor(1),type='l',ylim=c(-200,200))
  par(new=TRUE)
}

for (j in 1:500){
  y = c()
  for (i in 2:ncol(GRFy_id_left_avg_id)){
    y[i] <- GRFz_id_right_avg_id[j,i]
  }
  plot(y,col=randomColor(1),type='l',ylim=c(0,2000))
  par(new=TRUE)
}

