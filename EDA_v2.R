library(fda)
library(ggplot2)
library(tidyverse)
library(plotly)

#### For V_GRF_stance_N
leg_ID <- 1:nrow(IDinfo)
x <- vector(mode = "list", length = nrow(IDinfo))
t <- vector(mode = "list", length = nrow(IDinfo))


for (i in 1:nrow(IDinfo)){
  t[i] <- list(c(seq(1,100,by=1)))
  x[i] <- list(c(unlist(V_GRF_stance_N[i,])))
}


df_1 <- tibble(leg_ID,t,x)
names(df_1) <- c("ID", "Time", "Curve")
head(df_1)

df_3 <- df_1 %>% select(!c(ID,Curve)) %>% unnest_longer(Time) 
df_2 <- df_1 %>% select(!c(ID,Time)) %>% unnest_longer(Curve)
ID <- sort(rep(1:nrow(IDinfo),100))
df_l <- cbind(ID,df_3,df_2)
p <- ggplot(df_l, aes(x = Time, y = Curve, group = ID, col = ID)) +
  geom_line()
p

knots    = c(seq(0,100,5)) #Location of knots
n_knots   = length(knots) #Number of knots
n_order   = 4 # order of basis functions: for cubic b-splines: order = 3 + 1
n_basis   = length(knots) + n_order - 2;
basis = create.bspline.basis(rangeval = c(0,100), n_basis)
plot(basis)

argvals <- matrix(df_l$Time, nrow = 100, ncol =nrow(IDinfo) )
y_mat <- matrix(df_l$Curve, nrow = 100, ncol = nrow(IDinfo))

W.obj <- Data2fd(argvals = argvals, y = y_mat, basisobj = basis, lambda = 0.5)
W_mean <- mean.fd(W.obj)
W_sd <- std.fd(W.obj)
# Create objects for the standard upper and lower standard deviation
SE_u <- fd(basisobj = basis)
SE_l <- fd(basisobj = basis)
# Fill in the sd values
SE_u$coefs <- W_mean$coefs +  1.96 * W_sd$coefs/sqrt(nrow(IDinfo)) 
SE_l$coefs <- W_mean$coefs -  1.96 * W_sd$coefs/sqrt(nrow(IDinfo))

plot(W.obj, xlab="Time", ylab="", lty = 1)
## [1] "done"
title(main = "Smoothed Curves")
lines(SE_u, lwd = 3, lty = 3)
lines(SE_l, lwd = 3, lty = 3)
lines(W_mean,  lwd = 4)

#### For AP_GRF_stance_N

x_ap <- vector(mode = "list", length = nrow(IDinfo))
t_ap <- vector(mode = "list", length = nrow(IDinfo))
for (i in 1:nrow(IDinfo)){
  t_ap[i] <- list(c(seq(1,100,by=1)))
  x_ap[i] <- list(c(unlist(AP_GRF_stance_N[i,])))
}


df_1_ap <- tibble(leg_ID,t_ap,x_ap)
names(df_1_ap) <- c("ID", "Time", "Curve")
head(df_1)

df_3_ap <- df_1_ap %>% select(!c(ID,Curve)) %>% unnest_longer(Time) 
df_2_ap <- df_1_ap %>% select(!c(ID,Time)) %>% unnest_longer(Curve)
ID <- sort(rep(1:nrow(IDinfo),100))
df_l_ap <- cbind(ID,df_3_ap,df_2_ap)
p <- ggplot(df_l_ap, aes(x = Time, y = Curve, group = ID, col = ID)) +
  geom_line()
p

knots    = c(seq(0,100,5)) #Location of knots
n_knots   = length(knots) #Number of knots
n_order   = 4 # order of basis functions: for cubic b-splines: order = 3 + 1
n_basis   = length(knots) + n_order - 2;
basis = create.bspline.basis(rangeval = c(0,100), n_basis)
plot(basis)

argvals_ap <- matrix(df_l_ap$Time, nrow = 100, ncol =nrow(IDinfo) )
y_mat_ap <- matrix(df_l_ap$Curve, nrow = 100, ncol = nrow(IDinfo))

W.obj <- Data2fd(argvals = argvals_ap, y = y_mat_ap, basisobj = basis, lambda = 0.5)
W_mean <- mean.fd(W.obj)
W_sd <- std.fd(W.obj)
# Create objects for the standard upper and lower standard deviation
SE_u <- fd(basisobj = basis)
SE_l <- fd(basisobj = basis)
# Fill in the sd values
SE_u$coefs <- W_mean$coefs +  1.96 * W_sd$coefs/sqrt(nrow(IDinfo)) 
SE_l$coefs <- W_mean$coefs -  1.96 * W_sd$coefs/sqrt(nrow(IDinfo))

plot(W.obj, xlab="Time", ylab="", lty = 1)
lines(W_mean,  lwd = 4)
## [1] "done"
title(main = "Smoothed Curves")
lines(SE_u, lwd = 3, lty = 3)
lines(SE_l, lwd = 3, lty = 3)
lines(W_mean,  lwd = 4)


###### ML_GRF_stance_N

x_ml <- vector(mode = "list", length = nrow(IDinfo))
t_ml <- vector(mode = "list", length = nrow(IDinfo))
for (i in 1:nrow(IDinfo)){
  t_ml[i] <- list(c(seq(1,100,by=1)))
  x_ml[i] <- list(c(unlist(ML_GRF_stance_N[i,])))
}


df_1_ml <- tibble(leg_ID,t_ml,x_ml)
names(df_1_ml) <- c("ID", "Time", "Curve")
head(df_1)

df_3_ml <- df_1_ml %>% select(!c(ID,Curve)) %>% unnest_longer(Time) 
df_2_ml <- df_1_ml %>% select(!c(ID,Time)) %>% unnest_longer(Curve)
ID <- sort(rep(1:nrow(IDinfo),100))
df_l_ml <- cbind(ID,df_3_ml,df_2_ml)
p <- ggplot(df_l_ml, aes(x = Time, y = Curve, group = ID, col = ID)) +
  geom_line()
p

knots    = c(seq(0,100,5)) #Location of knots
n_knots   = length(knots) #Number of knots
n_order   = 4 # order of basis functions: for cubic b-splines: order = 3 + 1
n_basis   = length(knots) + n_order - 2;
basis = create.bspline.basis(rangeval = c(0,100), n_basis)
plot(basis)

argvals_ml <- matrix(df_l_ml$Time, nrow = 100, ncol =nrow(IDinfo) )
y_mat_ml <- matrix(df_l_ml$Curve, nrow = 100, ncol = nrow(IDinfo))

W.obj <- Data2fd(argvals = argvals_ml, y = y_mat_ml, basisobj = basis, lambda = 0.5)
W_mean <- mean.fd(W.obj)
W_sd <- std.fd(W.obj)
# Create objects for the standard upper and lower standard deviation
SE_u <- fd(basisobj = basis)
SE_l <- fd(basisobj = basis)
# Fill in the sd values
SE_u$coefs <- W_mean$coefs +  1.96 * W_sd$coefs/sqrt(nrow(IDinfo)) 
SE_l$coefs <- W_mean$coefs -  1.96 * W_sd$coefs/sqrt(nrow(IDinfo))

plot(W.obj, xlab="Time", ylab="", lty = 1)
lines(W_mean,  lwd = 4)
## [1] "done"
title(main = "Smoothed Curves")
lines(SE_u, lwd = 3, lty = 3)
lines(SE_l, lwd = 3, lty = 3)
lines(W_mean,  lwd = 4)
