library(Rssa)

s <- list()
r <- list()
j <- (as.complex(-1))^(1/3) # define the complex number 
for (i in (1:15696)) {
  test_v <- t(V_GRF_stance_N[i,])
  test_ap <- t(AP_GRF_stance_N[i,])
  test_ml <- t(ML_GRF_stance_N[i,])
  s[[i]] <- ssa(test_v + 1*j^(3/2)*test_ap + 1*j*test_ml,kind = "cssa", svd.method = "svd")
  r[[i]] <- reconstruct(s[[i]], groups = list(Trend = 1:2))
}

j <- (as.complex(-1))^(1/3)
s <- ssa(test_v + 1*j*test_ap + 1*(j^2)*test_ml,kind = "cssa", svd.method = "svd")
r <- reconstruct(s, groups = list(Trend = 1:2))
plot(r, plot.method = "xyplot", layout = c(2, 3))
print(rforecast(s, groups = list(Trend = 1:2), len = 2)[1:2])
plot(s, type = "vectors", idx = 1:8)
plot(r, add.residuals = FALSE,
     plot.method = "xyplot",
     superpose = TRUE, auto.key = list(columns = 3))


test_v_1 <- t(V_GRF_stance_N[1,])
test_ap_1 <- t(AP_GRF_stance_N[1,])
test_ml_1 <- t(ML_GRF_stance_N[1,])
s_1 <- ssa(test_v_1 + 1*j^(3/2)*test_ap_1 + 1*j*test_ml_1,kind = "cssa", svd.method = "svd")
r_1 <- reconstruct(s_1, groups = list(Trend = 1:2))
plot(r_1, plot.method = "xyplot", layout = c(2, 3))
plot(s_1, type = "vectors", idx = 1:8)

test_v <- t(V_GRF_stance_N[1,])
test_ap <- t(AP_GRF_stance_N[1,])
test_ml <- t(ML_GRF_stance_N[1,])
s_1 <- ssa(test_v + 1*j*test_ap + 1*(j^2)*test_ml,kind = "cssa", svd.method = "svd")
r_1 <- reconstruct(s_1, groups = list(Trend = 1:2))
plot(r_1, plot.method = "xyplot", layout = c(2, 3))
plot(s_1, type = "vectors", idx = 1:8)

# Mean of 96% can be described by the combination of the first and second trend. 
k <- c()
for (i in 1:15696) {
  c <- contributions(s[[i]])*100
  k[i] <- round(c[1],2)
}
mse <- c()
for (i in 1:15696) {
  c <- wnorm(residuals(r[[i]]))
  mse[i] <- round(c,3)
}



require(Rfssa)
load_github_data("https://github.com/haghbinh/Rfssa/blob/master/data/Montana.RData")
Temp <- Montana$Temp
NDVI <- Montana$NDVI
d_temp <- 11
d_NDVI <- 13
## Define functional time series
Y <- fts(
  list(Temp / sd(Temp), NDVI), list(
    list(d_temp, "bspline"),
    list(d_NDVI, d_NDVI, "bspline", "bspline")
  ),
  list(c(0, 23), list(c(1, 33), c(1, 33)))
)
# Plot the first 100 observations
plot(Y,
     xlabels = c("Time", "Lon."), ylabels = c("Temperature (\u00B0C)", "Lat."),
     zlabels = c("", "NDVI"), mains = c("Temperature Curves", "NDVI Images")
)


Y <- fts(
  list(t(as.matrix(V_GRF_stance_N)),t(as.matrix(AP_GRF_stance_N)),t(as.matrix(ML_GRF_stance_N))), list(
    list(23, "bspline"),
    list(23, "bspline"),
    list(23, "bspline")
  ),
  list(c(1,1000), c(1,1000), c(1,1000))
)
L <- 50
U <- fssa(Y, L)
Q <- freconstruct(U = U, groups = list(c(1:2))) # reconstruct based on the first and second compontent
saveRDS(Q, file="fname.RData")
plot(Q[[1]])