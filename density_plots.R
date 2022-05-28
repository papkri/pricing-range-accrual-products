
rate = 0.1
spot = 100
kappa = 0.5
eta = 1
theta = 0.04
rho = -0.75
v_0 = 0.0625

dt = 1/64
numPaths = 500000
T = 0.5
r = rate
S_0 = spot



# v_0
variances = c(0.15, 0.25, 0.4)
variances = variances^2

heston_paths1 = Heston_paths_AE(numPaths, T, r, S_0, dt, kappa, eta, theta, rho, variances[1])
heston_paths2 = Heston_paths_AE(numPaths, T, r, S_0, dt, kappa, eta, theta, rho, variances[2])
heston_paths3 = Heston_paths_AE(numPaths, T, r, S_0, dt, kappa, eta, theta, rho, variances[3])
distr1 = heston_paths1[33,]/spot
distr2 = heston_paths2[33,]/spot
distr3 = heston_paths3[33,]/spot


plot(density(distr1))
lines(density(distr2))
lines(density(distr3))



distr1 = cbind(distr1, rep(variances[1], numPaths))
distr2 = cbind(distr2, rep(variances[2], numPaths))
distr3 = cbind(distr3, rep(variances[3], numPaths))


plot_df = as.data.frame(rbind(distr1, distr2, distr3))
plot_df[,1] = as.numeric(plot_df[,1])
plot_df[,2] = as.factor(plot_df[,2])
colnames(plot_df) = c("distr", "v0")

ggplot(plot_df, aes(x = distr)) +
  geom_density(aes(col = v0), lwd = 1.5) +
  xlab("Normalised stock price") +
  theme_Publication() +
  xlim(c(0.5, 1.5)) +
  scale_colour_Publication()




# kappa

rate = 0.1
spot = 100
kappa = 0.5
eta = 1
theta = 0.04
rho = -0.75
v_0 = 0.4^2

dt = 1/64
numPaths = 500000
kappas = c(1, 2, 3)


heston_paths1 = Heston_paths_AE(numPaths, T, r, S_0, dt, kappas[1], eta, theta, rho, v_0)
heston_paths2 = Heston_paths_AE(numPaths, T, r, S_0, dt, kappas[2], eta, theta, rho, v_0)
heston_paths3 = Heston_paths_AE(numPaths, T, r, S_0, dt, kappas[3], eta, theta, rho, v_0)
distr1 = heston_paths1[33,]/spot
distr2 = heston_paths2[33,]/spot
distr3 = heston_paths3[33,]/spot


plot(density(distr1))
lines(density(distr2))
lines(density(distr3))



distr1 = cbind(distr1, rep(kappas[1], numPaths))
distr2 = cbind(distr2, rep(kappas[2], numPaths))
distr3 = cbind(distr3, rep(kappas[3], numPaths))


plot_df = as.data.frame(rbind(distr1, distr2, distr3))
plot_df[,1] = as.numeric(plot_df[,1])
plot_df[,2] = as.factor(plot_df[,2])
colnames(plot_df) = c("distr", "kappa")

ggplot(plot_df, aes(x = distr)) +
  geom_density(aes(col = kappa), lwd = 1.5) +
  xlab("Normalised stock price") +
  theme_Publication() +
  xlim(c(0.5, 1.5)) +
  scale_colour_Publication()


## eta

S_0 = 100
rate = 0.1
spot = 100
kappa = 0.5
eta = 1
theta = 0.04
rho = -0.75
v_0 = 0.4^2
kappa = 0.5

dt = 1/64
numPaths = 500000
etas = c(0.5, 1, 1.5)


heston_paths1 = Heston_paths_AE(numPaths, T, r, S_0, dt, kappa, etas[1], theta, rho, v_0)
heston_paths2 = Heston_paths_AE(numPaths, T, r, S_0, dt, kappa, etas[2], theta, rho, v_0)
heston_paths3 = Heston_paths_AE(numPaths, T, r, S_0, dt, kappa, etas[3], theta, rho, v_0)
distr1 = heston_paths1[33,]/spot
distr2 = heston_paths2[33,]/spot
distr3 = heston_paths3[33,]/spot


plot(density(distr1))
lines(density(distr2))
lines(density(distr3))



distr1 = cbind(distr1, rep(etas[1], numPaths))
distr2 = cbind(distr2, rep(etas[2], numPaths))
distr3 = cbind(distr3, rep(etas[3], numPaths))


plot_df = as.data.frame(rbind(distr1, distr2, distr3))
plot_df[,1] = as.numeric(plot_df[,1])
plot_df[,2] = as.factor(plot_df[,2])
colnames(plot_df) = c("distr", "eta")

ggplot(plot_df, aes(x = distr)) +
  geom_density(aes(col = eta), lwd = 1.5) +
  xlab("Normalised stock price") +
  theme_Publication() +
  xlim(c(0, 2)) +
  scale_colour_Publication()



##rho

S_0 = 100
rate = 0.1
spot = 100
kappa = 0.5
eta = 1
theta = 0.04
rho = -0.75
v_0 = 0.4^2
kappa = 0.5
eta = 1

dt = 1/64
numPaths = 500000
rhos = c(-0.7, 0, 0.7)


heston_paths1 = Heston_paths_AE(numPaths, T, r, S_0, dt, kappa, eta, theta, rhos[1], v_0)
heston_paths2 = Heston_paths_AE(numPaths, T, r, S_0, dt, kappa, eta, theta, rhos[2], v_0)
heston_paths3 = Heston_paths_AE(numPaths, T, r, S_0, dt, kappa, eta, theta, rhos[3], v_0)
distr1 = heston_paths1[33,]/spot
distr2 = heston_paths2[33,]/spot
distr3 = heston_paths3[33,]/spot


plot(density(distr1))
lines(density(distr2))
lines(density(distr3))



distr1 = cbind(distr1, rep(rhos[1], numPaths))
distr2 = cbind(distr2, rep(rhos[2], numPaths))
distr3 = cbind(distr3, rep(rhos[3], numPaths))


plot_df = as.data.frame(rbind(distr1, distr2, distr3))
plot_df[,1] = as.numeric(plot_df[,1])
plot_df[,2] = as.factor(plot_df[,2])
colnames(plot_df) = c("distr", "rho")

ggplot(plot_df, aes(x = distr)) +
  geom_density(aes(col = rho), lwd = 1.5) +
  xlab("Normalised stock price") +
  theme_Publication() +
  xlim(c(0, 2)) +
  scale_colour_Publication()

