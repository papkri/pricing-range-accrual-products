
df = cbind(prices$Strike, prices$IV, prices$Maturity)
A = 1/abs( (prices$Ask - prices$Bid)/((prices$Ask + prices$Bid)/2))
B = 1/( (prices$Ask - prices$Bid)/((prices$Ask + prices$Bid)/2))^2
C = 1/sqrt( (prices$Ask - prices$Bid)/((prices$Ask + prices$Bid)/2))
A = (A - min(A))/(max(A) - min(A))
B = (B - min(B))/(max(B) - min(B))
C = (C - min(C))/(max(C) - min(C))
D = rep(1, nrow(df))



spot = 4478.28
rate = 0.01338
fit1 = Heston_calibrator_multiMat(input = df, r = rate, S_0 = spot, type = "IV", weights = A)
fit2 = Heston_calibrator_multiMat(input = df, r = rate, S_0 = spot, type = "IV", weights = B)
fit3 = Heston_calibrator_multiMat(input = df, r = rate, S_0 = spot, type = "IV", weights = C)
fit4 = Heston_calibrator_multiMat(input = df, r = rate, S_0 = spot, type = "IV", weights = D)


params1 = fit1$par
params2 = fit2$par
params3 = fit3$par
params4 = fit4$par

parameters = list()
parameters[[1]] = params1
parameters[[2]] = params2
parameters[[3]] = params3
parameters[[4]] = params4


modell_IV = matrix(NA, nrow = nrow(df), ncol = 4)

for (i in 1:nrow(df)){
  for (j in 1:4){
    pars = parameters[[j]]
    rho = pars[1]
    v_0 = pars[2]
    theta = pars[3]
    kappa = pars[4]
    eta = pars[5]
    
    price = NMOF::callHestoncf(S = spot, X = prices$Strike[i], tau = prices$Maturity[i], r = rate, q = 0, v0 = v_0, vT = theta, rho = rho, k = kappa,
                               sigma = eta, implVol = FALSE)
    
    modell_IV[i,j] =  IV_calculator(price = price, S = spot, r = rate, T = prices$Maturity[i], K = prices$Strike[i])$minimum
  }
}

true_IVS = cbind(prices$IV, prices$IV, prices$IV, prices$IV)

RMSE = (modell_IV - true_IVS)^2
RMSE_results = rep(NA, 4)
for (i in 1:4){
  RMSE_results[i] = sqrt(sum(RMSE[,i])/nrow(RMSE))
}



AARE = abs(modell_IV  - true_IVS)/true_IVS
AARE_results = apply(AARE, 2, mean)

MARE_results = apply(AARE, 2, max)


library(dplyr)
final_results = matrix(NA, 3, 4)
final_results[1,] = RMSE_results
final_results[2,] = AARE_results
final_results[3,] = MARE_results
colnames(final_results) = c("A", "B", "C", "D")
rownames(final_results) = c("RMSE", "AARE", "MARE")
final_results = round(final_results, 5)
final_results %>% kableExtra::kbl() %>% kableExtra::kable_styling(full_width = F)



#IV plot
library(ggplot2)



prices = cbind(prices, modell_IV[,4])
names(prices)[names(prices) == 'modell_IV[, 4]'] <- 'modell_IV'
error = abs(1 - prices$IV/prices$modell_IV)
prices = cbind(prices, error)
prices = prices[c(-130, -133, -134),]


mat1 = prices[prices$Maturity == prices$Maturity[1], ]
mat2 = prices[prices$Maturity == prices$Maturity[80], ]
mat3 = prices[prices$Maturity == prices$Maturity[200], ]


plot11 <- ggplot(mat1) +
  geom_point(aes(Strike, IV), size = 2) +
  geom_line(aes(Strike, modell_IV), lwd = 1.5, col = "steelblue") +
  theme_Publication()

plot12 <- ggplot(mat1)+
  geom_bar(aes(x=Strike, y = error), size = 1, stat = "identity", col = "steelblue") +
  theme_Publication()



plot21 <- ggplot(mat2) +
  geom_point(aes(Strike, IV), size = 2) +
  geom_line(aes(Strike, modell_IV), lwd = 1.5, col = "steelblue") +
  theme_Publication()

plot22 <- ggplot(mat2)+
  geom_bar(aes(x=Strike, y = error), size = 1, stat = "identity", col = "steelblue") +
  theme_Publication()

plot31 <- ggplot(mat3) +
  geom_point(aes(Strike, IV), size = 2) +
  geom_line(aes(Strike, modell_IV), lwd = 1.5, col = "steelblue") +
  theme_Publication()

plot32 <- ggplot(mat3)+
  geom_col(aes(x=Strike, y = error), stat = "identity") +
  theme_Publication()


par(mfrow = c(3,2))
plot(mat1$Strike, mat1$IV, main = "0.5 years", xlab = "strike", ylab = "IV")
lines(mat1$Strike, mat1$modell_IV, col = "steelblue", lwd = 2.5)
barplot(height = mat1$error, names.arg = mat1$Strike, main = "0.5 years", xlab = "strike", ylab = "error", ylim = c(0, 0.15))




plot(mat2$Strike, mat2$IV, main = "1 year", xlab = "strike", ylab = "IV")
lines(mat2$Strike, mat2$modell_IV, col = "steelblue", lwd = 2.5)
barplot(height = mat2$error, names.arg = mat2$Strike, main = "1 year", xlab = "strike", ylab = "error", ylim = c(0, 0.15))



plot(mat3$Strike, mat3$IV, main = "1.65 years", xlab = "strike", ylab = "IV")
lines(mat3$Strike, mat3$modell_IV, col = "steelblue", lwd = 2.5)
barplot(height = mat3$error, names.arg = mat3$Strike, main = "1.65 years", xlab = "strike", ylab = "error", ylim = c(0, 0.15))


#### TDBS
spot = 4478.28
rate = 0.01338

prices = prices[-139,]
prices = prices[c(-130, -133, -134),]
mat1 = prices[prices$Maturity == prices$Maturity[1], ]
mat2 = prices[prices$Maturity == prices$Maturity[80], ]
mat3 = prices[prices$Maturity == prices$Maturity[200], ]

atm1 = mat1[abs(mat1$Strike - spot) == min(abs(mat1$Strike - spot)),]
atm = atm1$Strike




times = c(0, prices$Maturity[1], prices$Maturity[80], prices$Maturity[200])
IVS = c(0, mat1$IV[mat1$Strike == atm], mat2$IV[mat2$Strike == atm], mat3$IV[mat3$Strike == atm])^2
sigma = rep(NA, 3)

for (i in 1:(length(times)-1)){
  maturity = times[i+1]
  sigma[i] = (times[i+1]*IVS[i+1] - times[i]*IVS[i])/(times[i+1] - times[i])
}


sigma = sqrt(sigma)



#how good of a fit?
model_IV_BS = rep(NA, nrow(prices))
iv_matrix = cbind(times[2:4], IVS[2:4]^0.5)
for(i in 1:nrow(prices)){
  model_IV_BS[i] = iv_matrix[iv_matrix[,1]== prices$Maturity[i], 2]
}


RMSE = (model_IV_BS - prices$IV)^2
RMSE_results = sqrt(mean(RMSE))


AARE = abs(model_IV_BS  - prices$IV)/prices$IV
AARE_results = mean(AARE)

MARE_results = max(AARE)


library(dplyr)
final_results = matrix(NA, 3, 1)
final_results[1,1] = RMSE_results
final_results[2,1] = AARE_results
final_results[3,1] = MARE_results
rownames(final_results) = c("RMSE", "AARE", "MARE")
final_results = round(final_results, 5)
final_results %>% kableExtra::kbl() %>% kableExtra::kable_styling(full_width = F)

#plot the fit
par(mfrow = c(1,2))
prices = cbind(prices, model_IV_BS)
error = abs(1 - prices$IV/model_IV_BS)
prices = cbind(prices, error)

mat1 = prices[prices$Maturity == prices$Maturity[1], ]
plot(mat1$Strike, mat1$IV, main = "0.5 year")
lines(mat1$Strike, mat1$model_IV_BS)
barplot(height = mat1$error, names.arg = mat1$Strike, main = "0.5 year")



mat2 = prices[prices$Maturity == prices$Maturity[80], ]
plot(mat2$Strike, mat2$IV, main = "1 year")
lines(mat2$Strike, mat2$model_IV_BS)
barplot(height = mat2$error, names = mat2$Strike, main = "1 year")


mat3 = prices[prices$Maturity == prices$Maturity[200], ]
plot(mat3$Strike, mat3$IV, main = "1.65 year")
lines(mat3$Strike, mat3$model_IV_BS)
barplot(height = mat3$error, names = mat3$Strike, main = "1.65 year")


par(mfrow = c(1,3))
barplot(height = mat1$error, names.arg = mat1$Strike, main = "0.5 years", xlab = "strike", ylab = "error", ylim = c(0, max(prices$error)))
barplot(height = mat2$error, names = mat2$Strike, main = "1 year", xlab = "strike", ylab = "error", ylim = c(0, max(prices$error)))
barplot(height = mat3$error, names = mat3$Strike, main = "1.65 years", xlab = "strike", ylab = "error", ylim = c(0, max(prices$error)))



### 2 maturities ###


prices = prices[-139,]

prices = prices[prices$Maturity != prices$Maturity[1],]


df = cbind(prices$Strike, prices$IV, prices$Maturity)
A = 1/abs( (prices$Ask - prices$Bid)/((prices$Ask + prices$Bid)/2))
B = 1/( (prices$Ask - prices$Bid)/((prices$Ask + prices$Bid)/2))^2
C = 1/sqrt( (prices$Ask - prices$Bid)/((prices$Ask + prices$Bid)/2))
A = (A - min(A))/(max(A) - min(A))
B = (B - min(B))/(max(B) - min(B))
C = (C - min(C))/(max(C) - min(C))
D = rep(1, nrow(df))


spot = 4478.28
rate = 0.01338
fit1 = Heston_calibrator_multiMat(input = df, r = rate, S_0 = spot, type = "IV", weights = A)
fit2 = Heston_calibrator_multiMat(input = df, r = rate, S_0 = spot, type = "IV", weights = B)
fit3 = Heston_calibrator_multiMat(input = df, r = rate, S_0 = spot, type = "IV", weights = C)
fit4 = Heston_calibrator_multiMat(input = df, r = rate, S_0 = spot, type = "IV", weights = D)

params1 = fit1$par
params2 = fit2$par
params3 = fit3$par
params4 = fit4$par

parameters = list()
parameters[[1]] = params1
parameters[[2]] = params2
parameters[[3]] = params3
parameters[[4]] = params4

modell_IV = matrix(NA, nrow = nrow(df), ncol = 4)

for (i in 1:nrow(df)){
  for (j in 1:4){
    pars = parameters[[j]]
    rho = pars[1]
    v_0 = pars[2]
    theta = pars[3]
    kappa = pars[4]
    eta = pars[5]
    
    price = NMOF::callHestoncf(S = spot, X = prices$Strike[i], tau = prices$Maturity[i], r = rate, q = 0, v0 = v_0, vT = theta, rho = rho, k = kappa,
                               sigma = eta, implVol = FALSE)
    
    modell_IV[i,j] =  IV_calculator(price = price, S = spot, r = rate, T = prices$Maturity[i], K = prices$Strike[i])$minimum
  }
}

true_IVS = cbind(prices$IV, prices$IV, prices$IV, prices$IV)

RMSE = (modell_IV - true_IVS)^2
RMSE_results = rep(NA, 4)
for (i in 1:4){
  RMSE_results[i] = sqrt(sum(RMSE[,i])/nrow(RMSE))
}



AARE = abs(modell_IV  - true_IVS)/true_IVS
AARE_results = apply(AARE, 2, mean)

MARE_results = apply(AARE, 2, max)


library(dplyr)
final_results = matrix(NA, 3, 4)
final_results[1,] = RMSE_results
final_results[2,] = AARE_results
final_results[3,] = MARE_results
colnames(final_results) = c("A", "B", "C", "D")
rownames(final_results) = c("RMSE", "AARE", "MARE")
final_results = round(final_results, 5)
final_results %>% kableExtra::kbl() %>% kableExtra::kable_styling(full_width = F)




########### graphing ##################


prices = cbind(prices, modell_IV[,4])
names(prices)[names(prices) == 'modell_IV[, 4]'] <- 'modell_IV'
error = abs(1 - prices$IV/prices$modell_IV)
prices = cbind(prices, error)

mat1 = prices[prices$Maturity == prices$Maturity[1], ]
par(mfrow = c(1,2))
plot(mat1$Strike, mat1$IV, main = "1 year", xlab = "strike", ylab = "IV")
lines(mat1$Strike, mat1$modell_IV, col = "steelblue", lwd = 2)
barplot(height = mat1$error, names.arg = mat1$Strike, main = "0.5 year", xlab = "strike", ylab = "error", ylim = c(0, 0.15))


mat2 = prices[prices$Maturity == prices$Maturity[130], ]
plot(mat2$Strike, mat2$IV, main = "1.65 year", xlab = "strike", ylab = "IV")
lines(mat2$Strike, mat2$modell_IV, col = "steelblue", lwd = 2)
barplot(height = mat2$error, names.arg = mat2$Strike, main = "1 year", xlab = "strike", ylab = "error", ylim = c(0, 0.15))






####### other 2 maturities



prices = prices[-139,]

prices = prices[prices$Maturity != prices$Maturity[200],]


df = cbind(prices$Strike, prices$IV, prices$Maturity)
A = 1/abs( (prices$Ask - prices$Bid)/((prices$Ask + prices$Bid)/2))
B = 1/( (prices$Ask - prices$Bid)/((prices$Ask + prices$Bid)/2))^2
C = 1/sqrt( (prices$Ask - prices$Bid)/((prices$Ask + prices$Bid)/2))
A = (A - min(A))/(max(A) - min(A))
B = (B - min(B))/(max(B) - min(B))
C = (C - min(C))/(max(C) - min(C))
D = rep(1, nrow(df))


spot = 4478.28
rate = 0.01338
fit1 = Heston_calibrator_multiMat(input = df, r = rate, S_0 = spot, type = "IV", weights = A)
fit2 = Heston_calibrator_multiMat(input = df, r = rate, S_0 = spot, type = "IV", weights = B)
fit3 = Heston_calibrator_multiMat(input = df, r = rate, S_0 = spot, type = "IV", weights = C)
fit4 = Heston_calibrator_multiMat(input = df, r = rate, S_0 = spot, type = "IV", weights = D)

params1 = fit1$par
params2 = fit2$par
params3 = fit3$par
params4 = fit4$par

parameters = list()
parameters[[1]] = params1
parameters[[2]] = params2
parameters[[3]] = params3
parameters[[4]] = params4

modell_IV = matrix(NA, nrow = nrow(df), ncol = 4)

for (i in 1:nrow(df)){
  for (j in 1:4){
    pars = parameters[[j]]
    rho = pars[1]
    v_0 = pars[2]
    theta = pars[3]
    kappa = pars[4]
    eta = pars[5]
    
    price = NMOF::callHestoncf(S = spot, X = prices$Strike[i], tau = prices$Maturity[i], r = rate, q = 0, v0 = v_0, vT = theta, rho = rho, k = kappa,
                               sigma = eta, implVol = FALSE)
    
    modell_IV[i,j] =  IV_calculator(price = price, S = spot, r = rate, T = prices$Maturity[i], K = prices$Strike[i])$minimum
  }
}

true_IVS = cbind(prices$IV, prices$IV, prices$IV, prices$IV)

RMSE = (modell_IV - true_IVS)^2
RMSE_results = rep(NA, 4)
for (i in 1:4){
  RMSE_results[i] = sqrt(sum(RMSE[,i])/nrow(RMSE))
}



AARE = abs(modell_IV  - true_IVS)/true_IVS
AARE_results = apply(AARE, 2, mean)

MARE_results = apply(AARE, 2, max)


library(dplyr)
final_results = matrix(NA, 3, 4)
final_results[1,] = RMSE_results
final_results[2,] = AARE_results
final_results[3,] = MARE_results
colnames(final_results) = c("A", "B", "C", "D")
rownames(final_results) = c("RMSE", "AARE", "MARE")
final_results = round(final_results, 5)
final_results %>% kableExtra::kbl() %>% kableExtra::kable_styling(full_width = F)




########### graphing ##################


prices = cbind(prices, modell_IV[,4])
names(prices)[names(prices) == 'modell_IV[, 4]'] <- 'modell_IV'
error = abs(1 - prices$IV/prices$modell_IV)
prices = cbind(prices, error)

mat1 = prices[prices$Maturity == prices$Maturity[1], ]
par(mfrow = c(1,2))
plot(mat1$Strike, mat1$IV, main = "0.5 year", xlab = "strike", ylab = "IV")
lines(mat1$Strike, mat1$modell_IV, col = "steelblue", lwd = 2)
barplot(height = mat1$error, names.arg = mat1$Strike, main = "0.5 year", xlab = "strike", ylab = "error", ylim = c(0, 0.15))


mat2 = prices[prices$Maturity == prices$Maturity[120], ]
plot(mat2$Strike, mat2$IV, main = "1.65 year", xlab = "strike", ylab = "IV")
lines(mat2$Strike, mat2$modell_IV, col = "steelblue", lwd = 2)
barplot(height = mat2$error, names.arg = mat2$Strike, main = "1 year", xlab = "strike", ylab = "error", ylim = c(0, 0.15))




#### 1 maturity


prices <- readxl::read_excel(
  "C:/Users/tofi4/My Drive/EGYETEM/mester/szakdoga/analysis/calibration/combined_calibration_data.xlsx")
prices = prices[-139,]

prices = prices[prices$Maturity == prices$Maturity[1],]


df = cbind(prices$Strike, prices$IV, prices$Maturity)
A = 1/abs( (prices$Ask - prices$Bid)/((prices$Ask + prices$Bid)/2))
B = 1/( (prices$Ask - prices$Bid)/((prices$Ask + prices$Bid)/2))^2
C = 1/sqrt( (prices$Ask - prices$Bid)/((prices$Ask + prices$Bid)/2))
A = (A - min(A))/(max(A) - min(A))
B = (B - min(B))/(max(B) - min(B))
C = (C - min(C))/(max(C) - min(C))
D = rep(1, nrow(df))


spot = 4478.28
rate = 0.01338
fit1 = Heston_calibrator_multiMat(input = df, r = rate, S_0 = spot, type = "IV", weights = A)
fit2 = Heston_calibrator_multiMat(input = df, r = rate, S_0 = spot, type = "IV", weights = B)
fit3 = Heston_calibrator_multiMat(input = df, r = rate, S_0 = spot, type = "IV", weights = C)
fit4 = Heston_calibrator_multiMat(input = df, r = rate, S_0 = spot, type = "IV", weights = D)

params1 = fit1$par
params2 = fit2$par
params3 = fit3$par
params4 = fit4$par

parameters = list()
parameters[[1]] = params1
parameters[[2]] = params2
parameters[[3]] = params3
parameters[[4]] = params4

modell_IV = matrix(NA, nrow = nrow(df), ncol = 4)

for (i in 1:nrow(df)){
  for (j in 1:4){
    pars = parameters[[j]]
    rho = pars[1]
    v_0 = pars[2]
    theta = pars[3]
    kappa = pars[4]
    eta = pars[5]
    
    price = NMOF::callHestoncf(S = spot, X = prices$Strike[i], tau = prices$Maturity[i], r = rate, q = 0, v0 = v_0, vT = theta, rho = rho, k = kappa,
                               sigma = eta, implVol = FALSE)
    
    modell_IV[i,j] =  IV_calculator(price = price, S = spot, r = rate, T = prices$Maturity[i], K = prices$Strike[i])$minimum
  }
}

true_IVS = cbind(prices$IV, prices$IV, prices$IV, prices$IV)

RMSE = (modell_IV - true_IVS)^2
RMSE_results = rep(NA, 4)
for (i in 1:4){
  RMSE_results[i] = sqrt(sum(RMSE[,i])/nrow(RMSE))
}



AARE = abs(modell_IV  - true_IVS)/true_IVS
AARE_results = apply(AARE, 2, mean)

MARE_results = apply(AARE, 2, max)


library(dplyr)
final_results = matrix(NA, 3, 4)
final_results[1,] = RMSE_results
final_results[2,] = AARE_results
final_results[3,] = MARE_results
colnames(final_results) = c("A", "B", "C", "D")
rownames(final_results) = c("RMSE", "AARE", "MARE")
final_results = round(final_results, 5)
final_results %>% kableExtra::kbl() %>% kableExtra::kable_styling(full_width = F)




########### graphing ##################


prices = cbind(prices, modell_IV[,4])
names(prices)[names(prices) == 'modell_IV[, 4]'] <- 'modell_IV'
error = abs(1 - prices$IV/prices$modell_IV)
prices = cbind(prices, error)

mat1 = prices[prices$Maturity == prices$Maturity[1], ]
par(mfrow = c(1,2))
plot(mat1$Strike, mat1$IV, main = "0.5 year", xlab = "strike", ylab = "IV")
lines(mat1$Strike, mat1$modell_IV, col = "steelblue", lwd = 2)
barplot(height = mat1$error, names.arg = mat1$Strike, main = "0.5 year", xlab = "strike", ylab = "error", ylim = c(0, 0.15))


mat2 = prices[prices$Maturity == prices$Maturity[120], ]
plot(mat2$Strike, mat2$IV, main = "1.65 year", xlab = "strike", ylab = "IV")
lines(mat2$Strike, mat2$modell_IV, col = "steelblue", lwd = 2)
barplot(height = mat2$error, names.arg = mat2$Strike, main = "1 year", xlab = "strike", ylab = "error", ylim = c(0, 0.15))





prices = prices[c(-130, -133, -134),]