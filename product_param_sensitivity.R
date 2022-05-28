
### setup
library(ggplot2)

rho = -0.77264229
v_0 = 0.03991306
theta = 0.14145378
kappa = 1.00383780
eta = 1.00302961

sigma = c(0.2092002, 0.2422952, 0.2533971)
sigma = sigma[1:2]

spot = 4478.28
rate = 0.01338

### N

periods = seq(10, 300, by = 10)
results1 = matrix(NA, nrow = length(periods), ncol = 3)
results1[,1] = periods
results1[,3] = "Heston"

results2 = matrix(NA, nrow = length(periods), ncol = 3)
results2[,1] = periods
results2[,3] = "TDBS"

#többi param

N = 10
KUp = 1.1*spot
KLow = 0.9*spot


for (i in 1:length(periods)){
  results1[i,2] = sRAN_analytic(S = spot, KUp = KUp, KLow = KLow, numTimeSteps = periods[i], T = 1,
                               coupon = 10, r = rate, v0 = v_0, vT = theta, rho = rho, k = kappa, sigma = eta)
  
  results2[i,2] = sRAN_analytic_TDBS(S = spot, KUp = KUp, KLow = KLow, numTimeSteps = periods[i], T = 1,
                                    coupon = 10, r = rate, sigma = sigma, timesteps = c(0,0.5))
}

# plot(periods, results[,2], ylim = c(5,6))
# points(periods, results[,3], col = "red")

long_results = as.data.frame(rbind(results1, results2))
colnames(long_results) = c("N", "price", "model")
long_results[,1] = as.numeric(long_results[,1])
long_results[,2] = as.numeric(long_results[,2])

long_results[,3] = as.factor(long_results[,3])

ggplot(long_results) +
  geom_point(aes(x = N, y = price, col = model), shape = 18, size = 4) +
  geom_line(aes(x = N, y = price, col = model), lwd = 1) +
  xlab("Number of observation periods") +
  theme_Publication() +
  scale_colour_Publication()



###maturity
sigma =  c(0.2092002, 0.2422952, 0.2533971)

periods = seq(0.2, 1.60, by = 0.10)
results1 = matrix(NA, nrow = length(periods), ncol = 3)
results1[,1] = periods
results1[,3] = "Heston"

results2 = matrix(NA, nrow = length(periods), ncol = 3)
results2[,1] = periods
results2[,3] = "TDBS"

#többi param

sRAN_analytic_TDBS(S = spot, KUp = KUp, KLow = KLow, numTimeSteps = N, T = 1.1,
                   coupon = 10, r = rate, sigma = sigma, timesteps = c(0,0.5, 1))

N = 250
KUp = 1.1*spot
KLow = 0.9*spot

for (i in 1:length(periods)){
  results1[i,2] = sRAN_analytic(S = spot, KUp = KUp, KLow = KLow, numTimeSteps = N, T = periods[i],
                                coupon = 10, r = rate, v0 = v_0, vT = theta, rho = rho, k = kappa, sigma = eta)
  
  results2[i,2] = sRAN_analytic_TDBS(S = spot, KUp = KUp, KLow = KLow, numTimeSteps = N, T = periods[i],
                                     coupon = 10, r = rate, sigma = sigma, timesteps = c(0,0.5, 1))
}



long_results = as.data.frame(rbind(results1, results2))
colnames(long_results) = c("T", "price", "model")
long_results[,1] = as.numeric(long_results[,1])
long_results[,2] = as.numeric(long_results[,2])

long_results[,3] = as.factor(long_results[,3])

ggplot(long_results) +
  geom_point(aes(x = T, y = price, col = model), shape = 18, size = 4) +
  geom_line(aes(x = T, y = price, col = model), lwd = 1) +
  theme_Publication() +
  scale_colour_Publication() +
  xlab("Time to maturity")


##barriers

sigma =  c(0.2092002, 0.2422952, 0.2533971)

barrier = seq(0.1, 1.05, by = 0.05)
results1 = matrix(NA, nrow = length(barrier), ncol = 3)
results1[,1] = barrier
results1[,3] = "Heston"

results2 = matrix(NA, nrow = length(barrier), ncol = 3)
results2[,1] = barrier
results2[,3] = "TDBS"



N = 250
T = 1
KUp = 1.1*spot
KLow = barrier*spot

for (i in 1:length(periods)){
  results1[i,2] = sRAN_analytic(S = spot, KUp = KUp, KLow = KLow[i], numTimeSteps = N, T = T,
                                coupon = 10, r = rate, v0 = v_0, vT = theta, rho = rho, k = kappa, sigma = eta)
  
  results2[i,2] = sRAN_analytic_TDBS(S = spot, KUp = KUp, KLow = KLow[i], numTimeSteps = N, T = T,
                                     coupon = 10, r = rate, sigma = sigma, timesteps = c(0,0.5, 1))
}



long_results = as.data.frame(rbind(results1, results2))
colnames(long_results) = c("lowBar", "price", "model")
long_results[,1] = as.numeric(long_results[,1])
long_results[,2] = as.numeric(long_results[,2])

long_results[,3] = as.factor(long_results[,3])


bar1 <- ggplot(long_results) +
  geom_point(aes(x = lowBar, y = price, col = model), shape = 18, size = 4) +
  geom_line(aes(x = lowBar, y = price, col = model), lwd = 1) +
  theme_Publication() +
  scale_colour_Publication() +
  xlab("Lower barrier")


## upper

sigma =  c(0.2092002, 0.2422952, 0.2533971)

barrier = seq(0.95, 2, by = 0.05)
results1 = matrix(NA, nrow = length(barrier), ncol = 3)
results1[,1] = barrier
results1[,3] = "Heston"

results2 = matrix(NA, nrow = length(barrier), ncol = 3)
results2[,1] = barrier
results2[,3] = "TDBS"



N = 250
T = 1
KUp = barrier*spot
KLow = 0.9*spot

for (i in 1:length(periods)){
  results1[i,2] = sRAN_analytic(S = spot, KUp = KUp[i], KLow = KLow, numTimeSteps = N, T = T,
                                coupon = 10, r = rate, v0 = v_0, vT = theta, rho = rho, k = kappa, sigma = eta)
  
  results2[i,2] = sRAN_analytic_TDBS(S = spot, KUp = KUp[i], KLow = KLow, numTimeSteps = N, T = T,
                                     coupon = 10, r = rate, sigma = sigma, timesteps = c(0,0.5, 1))
}



long_results = as.data.frame(rbind(results1, results2))
colnames(long_results) = c("upBar", "price", "model")
long_results[,1] = as.numeric(long_results[,1])
long_results[,2] = as.numeric(long_results[,2])

long_results[,3] = as.factor(long_results[,3])


bar2 <- ggplot(long_results) +
  geom_point(aes(x = upBar, y = price, col = model), shape = 18, size = 4) +
  geom_line(aes(x = upBar, y = price, col = model), lwd = 1) +
  theme_Publication() +
  scale_colour_Publication() +
  xlab("Upper barrier")

final <- gridExtra::grid.arrange(bar1, bar2, nrow = 1)



# check the distribution

numPaths = 500000
T = 0.5
r = rate
S_0 = spot
dt = 1/64
V_0 = 0.03991306
sig = 0.2092002


heston_paths = Heston_paths_AE(numPaths, T, r, S_0, dt, kappa, eta, theta, rho, V_0)
distr = heston_paths[33,]/spot
tdbs_sigma = sqrt(sig)
Wiener = rnorm(numPaths)*sqrt(0.5)
tdbs_distr = spot * exp((rate - tdbs_sigma^2/2)*0.5 + tdbs_sigma*Wiener)
tdbs_distr = tdbs_distr/spot

plot(density(distr))
lines(density(tdbs_distr))

moments::skewness(as.numeric(tdbs_distr[,1]))
moments::skewness(as.numeric(distr[,1]))

distr = cbind(distr, rep("Heston", numPaths))
tdbs_distr = cbind(tdbs_distr, rep("TDBS", numPaths))
plot_df = as.data.frame(rbind(distr, tdbs_distr))
plot_df[,1] = as.numeric(plot_df[,1])
plot_df[,2] = as.factor(plot_df[,2])
colnames(plot_df) = c("distr", "model")

ggplot(plot_df, aes(x = distr)) +
  geom_density(aes(col = model), lwd = 1.5) +
  xlim(c(0,2)) +
  xlab("Normalised stock price") +
  theme_Publication() +
  scale_colour_Publication()




numPaths = 500000
T = 0.5
r = rate
S_0 = spot
dt = 1/64
V_0 = 0.03991306
sig = 0.2092002


heston_paths = Heston_paths_AE(numPaths, T, r, S_0, dt, kappa, 0.01, theta, rho, V_0)
distr = heston_paths[33,]/spot

heston_paths = Heston_paths_AE(numPaths, T, r, S_0, dt, kappa, 0.25, theta, rho, V_0)
distr2 = heston_paths[33,]/spot

heston_paths = Heston_paths_AE(numPaths, T, r, S_0, dt, kappa, 0.75, theta, rho, V_0)
distr3 = heston_paths[33,]/spot

heston_paths = Heston_paths_AE(numPaths, T, r, S_0, dt, kappa, 1, theta, rho, V_0)
distr4 = heston_paths[33,]/spot


plot(density(distr4))
lines(density(distr))
lines(density(distr2))
lines(density(distr3))

moments::skewness(as.numeric(tdbs_distr[,1]))
moments::skewness(as.numeric(distr[,1]))


#infinite upper barrier


sigma =  c(0.2092002, 0.2422952, 0.2533971)

barrier = seq(0.5, 1.9, by = 0.05)
results1 = matrix(NA, nrow = length(barrier), ncol = 3)
results1[,1] = barrier
results1[,3] = "Heston"

results2 = matrix(NA, nrow = length(barrier), ncol = 3)
results2[,1] = barrier
results2[,3] = "TDBS"



N = 250
T = 1
KUp = 5*spot
KLow = barrier*spot

for (i in 1:length(barrier)){
  results1[i,2] = sRAN_analytic(S = spot, KUp = KUp, KLow = KLow[i], numTimeSteps = N, T = T,
                                coupon = 10, r = rate, v0 = v_0, vT = theta, rho = rho, k = kappa, sigma = eta)
  
  results2[i,2] = sRAN_analytic_TDBS(S = spot, KUp = KUp, KLow = KLow[i], numTimeSteps = N, T = T,
                                     coupon = 10, r = rate, sigma = sigma, timesteps = c(0,0.5, 1))
}



long_results = as.data.frame(rbind(results1, results2))
colnames(long_results) = c("lowBar", "price", "model")
long_results[,1] = as.numeric(long_results[,1])
long_results[,2] = as.numeric(long_results[,2])

long_results[,3] = as.factor(long_results[,3])


bar1 <- ggplot(long_results) +
  geom_point(aes(x = lowBar, y = price, col = model), shape = 18, size = 4) +
  geom_line(aes(x = lowBar, y = price, col = model), lwd = 1) +
  theme_Publication() +
  scale_colour_Publication() +
  xlab("Lower barrier")




