library(ggplot2)
T = 1
rate = 0.1
spot = 100
kappa = 0.5
eta = 1
theta = 0.04
rho = -0.75
v_0 = 0.0625
KUp = 1.2*spot
KLow = 0.8*spot
N = 250
K = c(0.9, 1, 1.1)
lowValues = c(0.6, 0.8, 1)
upValues = c(0.8, 1.0, 1.2)


### theta ###

values = seq(0.05, 0.6 , by = 0.05)
values = values^2

vanilla = matrix(NA, nrow  = length(values), ncol = 3)
vanilla[,1] = values
vanilla[,3] = rep(K[1], length(values))

vanilla2 = matrix(NA, nrow  = length(values), ncol = 3)
vanilla2[,1] = values
vanilla2[,3] = rep(K[2], length(values))

vanilla3 = matrix(NA, nrow  = length(values), ncol = 3)
vanilla3[,1] = values
vanilla3[,3] = rep(K[3], length(values))

ran = matrix(NA, nrow = length(values), ncol = 3)
ran[,1] = values
ran[,3] = rep(paste("(", lowValues[1], ", ", upValues[1], ")", sep = ""), length(values))

ran2 = matrix(NA, nrow = length(values), ncol = 3)
ran2[,1] = values
ran2[,3] = rep(paste("(", lowValues[2], ", ", upValues[2], ")", sep = ""), length(values))

ran3 = matrix(NA, nrow = length(values), ncol = 3)
ran3[,1] = values
ran3[,3] = rep(paste("(", lowValues[3], ", ", upValues[3], ")", sep = ""), length(values))


for (i in 1:length(values)){
  ran[i,2] = sRAN_analytic(S = spot, KUp = upValues[1]*spot, KLow = lowValues[1]*spot, numTimeSteps = N, T = T,
                               coupon = 10, r = rate, v0 = v_0, vT = values[i], rho = rho, k = kappa, sigma = eta)
  
  ran2[i,2] = sRAN_analytic(S = spot, KUp = upValues[2]*spot, KLow = lowValues[2]*spot, numTimeSteps = N, T = T,
                           coupon = 10, r = rate, v0 = v_0, vT = values[i], rho = rho, k = kappa, sigma = eta)
  
  ran3[i,2] = sRAN_analytic(S = spot, KUp = upValues[3]*spot, KLow = lowValues[3]*spot, numTimeSteps = N, T = T,
                           coupon = 10, r = rate, v0 = v_0, vT = values[i], rho = rho, k = kappa, sigma = eta)
   
  vanilla[i,2] = NMOF::callHestoncf(S = spot, X = K[1]*spot, tau = T, r = rate, q = 0, v0 = v_0, vT = values[i],
                                rho = rho, k = kappa, sigma = eta, implVol = FALSE)

  vanilla2[i,2] = NMOF::callHestoncf(S = spot, X = K[2]*spot, tau = T, r = rate, q = 0, v0 = v_0, vT = values[i],
                                    rho = rho, k = kappa, sigma = eta, implVol = FALSE)
  
  vanilla3[i,2] = NMOF::callHestoncf(S = spot, X = K[3]*spot, tau = T, r = rate, q = 0, v0 = v_0, vT = values[i],
                                     rho = rho, k = kappa, sigma = eta, implVol = FALSE)
}

comb_vanilla = as.data.frame(rbind(vanilla, vanilla2, vanilla3))
comb_vanilla[,1] = as.numeric(comb_vanilla[,1])
comb_vanilla[,2] = as.numeric(comb_vanilla[,2])
comb_vanilla[,3] = as.factor(comb_vanilla[,3])

plot12 <- ggplot(data = comb_vanilla, aes(x = sqrt(V1), y = V2, col = V3)) +
  geom_point(shape = 18, size = 5) +
  geom_line(lwd = 1.5) +
  labs(title = "vanilla call", y = "price", x = "theta", col = "strike") +
  theme_Publication() +
  scale_colour_Publication()


comb_ran = as.data.frame(rbind(ran, ran2, ran3))
comb_ran[,1] = as.numeric(comb_ran[,1])
comb_ran[,2] = as.numeric(comb_ran[,2])
comb_ran[,3] = as.factor(comb_ran[,3])


plot11 <- ggplot(data = comb_ran, aes(x = sqrt(V1), y = V2, col = V3)) +
  geom_point(shape = 18, size = 5) +
  geom_line(lwd = 1.5) +
  labs(title = "range-accrual", y = "price", x = "theta", col = "barriers") +
  theme_Publication() +
  scale_colour_Publication()

### kappa ###

T = 1
rate = 0.1
spot = 100
kappa = 0.5
eta = 1
theta = 0.04
rho = -0.75
v_0 = 0.0625
KUp = 1.2*spot
KLow = 0.8*spot
N = 250

values = seq(0.2, 3 , by = 0.2)


vanilla = matrix(NA, nrow  = length(values), ncol = 3)
vanilla[,1] = values
vanilla[,3] = rep("0.01", length(values))
vanilla2 = matrix(NA, nrow  = length(values), ncol = 3)
vanilla2[,1] = values
vanilla2[,3] = rep("0.16", length(values))

ran = matrix(NA, nrow = length(values), ncol = 3)
ran[,1] = values
ran[,3] = rep("0.01", length(values))
ran2 = matrix(NA, nrow = length(values), ncol = 3)
ran2[,1] = values
ran2[,3] = rep("0.16", length(values))

high = 0.4^2
low = 0.1^2

for (i in 1:length(values)){
  ran[i,2] = sRAN_analytic(S = spot, KUp = KUp, KLow = KLow, numTimeSteps = N, T = T,
                           coupon = 10, r = rate, v0 = low, vT = theta, rho = rho, k = values[i], sigma = eta)
  ran2[i,2] = sRAN_analytic(S = spot, KUp = KUp, KLow = KLow, numTimeSteps = N, T = T,
                           coupon = 10, r = rate, v0 = high, vT = theta, rho = rho, k = values[i], sigma = eta)
  
  
  vanilla[i,2] = NMOF::callHestoncf(S = spot, X = spot, tau = T, r = rate, q = 0, v0 = low, vT = theta,
                                    rho = rho, k = values[i], sigma = eta, implVol = FALSE)
  vanilla2[i,2] = NMOF::callHestoncf(S = spot, X = spot, tau = T, r = rate, q = 0, v0 = high, vT = theta,
                                    rho = rho, k = values[i], sigma = eta, implVol = FALSE)
}

comb_vanilla = as.data.frame(rbind(vanilla, vanilla2))
comb_vanilla[,1] = as.numeric(comb_vanilla[,1])
comb_vanilla[,2] = as.numeric(comb_vanilla[,2])
comb_vanilla[,3] = as.factor(comb_vanilla[,3])
comb_ran = as.data.frame(rbind(ran, ran2))
comb_ran[,1] = as.numeric(comb_ran[,1])
comb_ran[,2] = as.numeric(comb_ran[,2])
comb_ran[,3] = as.factor(comb_ran[,3])


plot22 <- ggplot(data = comb_vanilla, aes(x = V1, y = V2, col = V3)) +
  geom_point(shape = 18, size = 5) +
  geom_line(lwd = 1.5) +
  labs(title = "vanilla call", y = "price", x = "kappa", col = "initial variance") +
  theme_Publication() +
  scale_colour_Publication()

plot21 <- ggplot(data = comb_ran, aes(x = V1, y = V2, col = V3)) +
  geom_point(shape = 18, size = 5) +
  geom_line(lwd = 1.5) +
  labs(title = "range-accrual", y = "price", x = "kappa", col = "initial variance") +
  theme_Publication() +
  scale_colour_Publication()



### eta ###

T = 1
rate = 0.1
spot = 100
kappa = 0.5
eta = 1
theta = 0.04
rho = -0.75
v_0 = 0.0625
KUp = 1.2*spot
KLow = 0.8*spot
N = 250
K = c(0.9, 1, 1.1)
lowValues = c(0.6, 0.8, 1.0)
upValues = c(0.8, 1.0, 1.2)


values = seq(0.1, 1.5 , by = 0.1)


vanilla = matrix(NA, nrow  = length(values), ncol = 3)
vanilla[,1] = values
vanilla[,3] = rep(K[1], length(values))

vanilla2 = matrix(NA, nrow  = length(values), ncol = 3)
vanilla2[,1] = values
vanilla2[,3] = rep(K[2], length(values))

vanilla3 = matrix(NA, nrow  = length(values), ncol = 3)
vanilla3[,1] = values
vanilla3[,3] = rep(K[3], length(values))

ran = matrix(NA, nrow = length(values), ncol = 3)
ran[,1] = values
ran[,3] = rep(paste("(", lowValues[1], ", ", upValues[1], ")", sep = ""), length(values))

ran2 = matrix(NA, nrow = length(values), ncol = 3)
ran2[,1] = values
ran2[,3] = rep(paste("(", lowValues[2], ", ", upValues[2], ")", sep = ""), length(values))

ran3 = matrix(NA, nrow = length(values), ncol = 3)
ran3[,1] = values
ran3[,3] = rep(paste("(", lowValues[3], ", ", upValues[3], ")", sep = ""), length(values))


for (i in 1:length(values)){
  ran[i,2] = sRAN_analytic(S = spot, KUp = upValues[1]*spot, KLow = lowValues[1]*spot, numTimeSteps = N, T = T,
                           coupon = 10, r = rate, v0 = v_0, vT = theta, rho = rho, k = kappa, sigma = values[i])
  
  ran2[i,2] = sRAN_analytic(S = spot, KUp = upValues[2]*spot, KLow = lowValues[2]*spot, numTimeSteps = N, T = T,
                            coupon = 10, r = rate, v0 = v_0, vT = theta, rho = rho, k = kappa, sigma = values[i])
  
  ran3[i,2] = sRAN_analytic(S = spot, KUp = upValues[3]*spot, KLow = lowValues[3]*spot, numTimeSteps = N, T = T,
                            coupon = 10, r = rate, v0 = v_0, vT = theta, rho = rho, k = kappa, sigma = values[i])
  
  vanilla[i,2] = NMOF::callHestoncf(S = spot, X = K[1]*spot, tau = T, r = rate, q = 0, v0 = v_0, vT = theta,
                                    rho = rho, k = kappa, sigma = values[i], implVol = FALSE)
  
  vanilla2[i,2] = NMOF::callHestoncf(S = spot, X = K[2]*spot, tau = T, r = rate, q = 0, v0 = v_0, vT = theta,
                                     rho = rho, k = kappa, sigma = values[i], implVol = FALSE)
  
  vanilla3[i,2] = NMOF::callHestoncf(S = spot, X = K[3]*spot, tau = T, r = rate, q = 0, v0 = v_0, vT = theta,
                                     rho = rho, k = kappa, sigma = values[i], implVol = FALSE)
}

comb_vanilla = as.data.frame(rbind(vanilla, vanilla2, vanilla3))
comb_vanilla[,1] = as.numeric(comb_vanilla[,1])
comb_vanilla[,2] = as.numeric(comb_vanilla[,2])
comb_vanilla[,3] = as.factor(comb_vanilla[,3])

plot32 <- ggplot(data = comb_vanilla, aes(x = V1, y = V2, col = V3)) +
  geom_point(shape = 18, size = 5) +
  geom_line(lwd = 1.5) +
  labs(title = "", y = "price", x = "eta", col = "strike") +
  theme_Publication() +
  scale_colour_Publication()


comb_ran = as.data.frame(rbind(ran, ran2, ran3))
comb_ran[,1] = as.numeric(comb_ran[,1])
comb_ran[,2] = as.numeric(comb_ran[,2])
comb_ran[,3] = as.factor(comb_ran[,3])


plot31 <- ggplot(data = comb_ran, aes(x = V1, y = V2, col = V3)) +
  geom_point(shape = 18, size = 5) +
  geom_line(lwd = 1.5) +
  labs(title = "", y = "price", x = "eta", col = "barriers") +
  theme_Publication() +
  scale_colour_Publication()






### rho ###

T = 1
rate = 0.1
spot = 100
kappa = 0.5
eta = 1
theta = 0.04
rho = -0.75
v_0 = 0.0625
KUp = 1.2*spot
KLow = 0.8*spot
N = 250
K = c(0.9, 1, 1.1)
lowValues = c(0.6, 0.8, 1.0)
upValues = c(0.8, 1.0, 1.2)


values = seq(-0.9, 0.9 , by = 0.1)


vanilla = matrix(NA, nrow  = length(values), ncol = 3)
vanilla[,1] = values
vanilla[,3] = rep(K[1], length(values))

vanilla2 = matrix(NA, nrow  = length(values), ncol = 3)
vanilla2[,1] = values
vanilla2[,3] = rep(K[2], length(values))

vanilla3 = matrix(NA, nrow  = length(values), ncol = 3)
vanilla3[,1] = values
vanilla3[,3] = rep(K[3], length(values))

ran = matrix(NA, nrow = length(values), ncol = 3)
ran[,1] = values
ran[,3] = rep(paste("(", lowValues[1], ", ", upValues[1], ")", sep = ""), length(values))

ran2 = matrix(NA, nrow = length(values), ncol = 3)
ran2[,1] = values
ran2[,3] = rep(paste("(", lowValues[2], ", ", upValues[2], ")", sep = ""), length(values))

ran3 = matrix(NA, nrow = length(values), ncol = 3)
ran3[,1] = values
ran3[,3] = rep(paste("(", lowValues[3], ", ", upValues[3], ")", sep = ""), length(values))


for (i in 1:length(values)){
  ran[i,2] = sRAN_analytic(S = spot, KUp = upValues[1]*spot, KLow = lowValues[1]*spot, numTimeSteps = N, T = T,
                           coupon = 10, r = rate, v0 = v_0, vT = theta, rho = values[i], k = kappa, sigma = eta)
  
  ran2[i,2] = sRAN_analytic(S = spot, KUp = upValues[2]*spot, KLow = lowValues[2]*spot, numTimeSteps = N, T = T,
                            coupon = 10, r = rate, v0 = v_0, vT = theta, rho = values[i], k = kappa, sigma = eta)
  
  ran3[i,2] = sRAN_analytic(S = spot, KUp = upValues[3]*spot, KLow = lowValues[3]*spot, numTimeSteps = N, T = T,
                            coupon = 10, r = rate, v0 = v_0, vT = theta, rho = values[i], k = kappa, sigma = eta)
  
  vanilla[i,2] = NMOF::callHestoncf(S = spot, X = K[1]*spot, tau = T, r = rate, q = 0, v0 = v_0, vT = theta,
                                    rho = values[i], k = kappa, sigma = eta, implVol = FALSE)
  
  vanilla2[i,2] = NMOF::callHestoncf(S = spot, X = K[2]*spot, tau = T, r = rate, q = 0, v0 = v_0, vT = theta,
                                     rho = values[i], k = kappa, sigma = eta, implVol = FALSE)
  
  vanilla3[i,2] = NMOF::callHestoncf(S = spot, X = K[3]*spot, tau = T, r = rate, q = 0, v0 = v_0, vT = theta,
                                     rho = values[i], k = kappa, sigma = eta, implVol = FALSE)
}

comb_vanilla = as.data.frame(rbind(vanilla, vanilla2, vanilla3))
comb_vanilla[,1] = as.numeric(comb_vanilla[,1])
comb_vanilla[,2] = as.numeric(comb_vanilla[,2])
comb_vanilla[,3] = as.factor(comb_vanilla[,3])

plot42 <- ggplot(data = comb_vanilla, aes(x = V1, y = V2, col = V3)) +
  geom_point(shape = 18, size = 5) +
  geom_line(lwd = 1.5) +
  labs(title = "", y = "price", x = "rho", col = "strike") +
  theme_Publication() +
  scale_colour_Publication() +
  scale_x_continuous(name = "rho", seq(-0.9, 0.9, by = 0.2))


comb_ran = as.data.frame(rbind(ran, ran2, ran3))
comb_ran[,1] = as.numeric(comb_ran[,1])
comb_ran[,2] = as.numeric(comb_ran[,2])
comb_ran[,3] = as.factor(comb_ran[,3])


plot41 <- ggplot(data = comb_ran, aes(x = V1, y = V2, col = V3)) +
  geom_point(shape = 18, size = 5) +
  geom_line(lwd = 1.5) +
  labs(title = "", y = "price", x = "rho", col = "barriers") +
  theme_Publication() +
  scale_colour_Publication() +
  scale_x_continuous(name = "rho", seq(-0.9, 0.9, by = 0.2))



### v_0 ###

T = 1
rate = 0.1
spot = 100
kappa = 0.5
eta = 1
theta = 0.04
rho = -0.75
v_0 = 0.0625
KUp = 1.2*spot
KLow = 0.8*spot
N = 250
K = c(0.9, 1, 1.1)
lowValues = c(0.6, 0.8, 1.0)
upValues = c(0.8, 1.0, 1.2)


values = seq(0.1, 0.6 , by = 0.05)
values = values^2


vanilla = matrix(NA, nrow  = length(values), ncol = 3)
vanilla[,1] = values
vanilla[,3] = rep(K[1], length(values))

vanilla2 = matrix(NA, nrow  = length(values), ncol = 3)
vanilla2[,1] = values
vanilla2[,3] = rep(K[2], length(values))

vanilla3 = matrix(NA, nrow  = length(values), ncol = 3)
vanilla3[,1] = values
vanilla3[,3] = rep(K[3], length(values))

ran = matrix(NA, nrow = length(values), ncol = 3)
ran[,1] = values
ran[,3] = rep(paste("(", lowValues[1], ", ", upValues[1], ")", sep = ""), length(values))

ran2 = matrix(NA, nrow = length(values), ncol = 3)
ran2[,1] = values
ran2[,3] = rep(paste("(", lowValues[2], ", ", upValues[2], ")", sep = ""), length(values))

ran3 = matrix(NA, nrow = length(values), ncol = 3)
ran3[,1] = values
ran3[,3] = rep(paste("(", lowValues[3], ", ", upValues[3], ")", sep = ""), length(values))





for (i in 1:length(values)){
  ran[i,2] = sRAN_analytic(S = spot, KUp = upValues[1]*spot, KLow = lowValues[1]*spot, numTimeSteps = N, T = T,
                           coupon = 10, r = rate, v0 = values[i], vT = theta, rho = rho, k = kappa, sigma = eta)
  
  ran2[i,2] = sRAN_analytic(S = spot, KUp = upValues[2]*spot, KLow = lowValues[2]*spot, numTimeSteps = N, T = T,
                            coupon = 10, r = rate, v0 = values[i], vT = theta, rho = rho, k = kappa, sigma = eta)
  
  ran3[i,2] = sRAN_analytic(S = spot, KUp = upValues[3]*spot, KLow = lowValues[3]*spot, numTimeSteps = N, T = T,
                            coupon = 10, r = rate, v0 = values[i], vT = theta, rho = rho, k = kappa, sigma = eta)
  
  vanilla[i,2] = NMOF::callHestoncf(S = spot, X = K[1]*spot, tau = T, r = rate, q = 0, v0 = values[i], vT = theta,
                                    rho = rho, k = kappa, sigma = eta, implVol = FALSE)
  
  vanilla2[i,2] = NMOF::callHestoncf(S = spot, X = K[2]*spot, tau = T, r = rate, q = 0, v0 = values[i], vT = theta,
                                     rho = rho, k = kappa, sigma = eta, implVol = FALSE)
  
  vanilla3[i,2] = NMOF::callHestoncf(S = spot, X = K[3]*spot, tau = T, r = rate, q = 0, v0 = values[i], vT = theta,
                                     rho = rho, k = kappa, sigma = eta, implVol = FALSE)
}

comb_vanilla = as.data.frame(rbind(vanilla, vanilla2, vanilla3))
comb_vanilla[,1] = as.numeric(comb_vanilla[,1])
comb_vanilla[,2] = as.numeric(comb_vanilla[,2])
comb_vanilla[,3] = as.factor(comb_vanilla[,3])

plot52 <- ggplot(data = comb_vanilla, aes(x = sqrt(V1), y = V2, col = V3)) +
  geom_point(shape = 18, size = 5) +
  geom_line(lwd = 1.5) +
  labs(title = "vanilla call", y = "price", x = "v_0", col = "strike") +
  theme_Publication() +
  scale_colour_Publication()


comb_ran = as.data.frame(rbind(ran, ran2, ran3))
comb_ran[,1] = as.numeric(comb_ran[,1])
comb_ran[,2] = as.numeric(comb_ran[,2])
comb_ran[,3] = as.factor(comb_ran[,3])


plot51 <- ggplot(data = comb_ran, aes(x = sqrt(V1), y = V2, col = V3)) +
  geom_point(shape = 18, size = 5) +
  geom_line(lwd = 1.5) +
  labs(title = "range-accrual", y = "price", x = "v_0", col = "barriers") +
  theme_Publication() +
  scale_colour_Publication()


final1 <- gridExtra::grid.arrange(plot51, plot52, nrow = 1, ncol = 2)
final2 <- gridExtra::grid.arrange(plot21, plot22, nrow = 1, ncol = 2)

final3 <- gridExtra::grid.arrange(plot11, plot12, plot31, plot32, plot41, plot42, nrow = 3, ncol = 2)

final4 <- gridExtra::grid.arrange(plot31, plot32, nrow = 1, ncol = 2)

final5 <- gridExtra::grid.arrange(plot11, plot12, plot41, plot42, nrow = 2, ncol = 2)

