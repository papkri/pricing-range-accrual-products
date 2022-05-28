#' next step of a CIR sample
#'
#' generates the next step for the CIR variance process given the vector of previous variances.
#' Uses the exact simulation of the CIR process, sampling from the noncentral Chi-squared distribution
#' @param kappa spead of mean reversion
#' @param eta volatility of variance
#' @param theta long run average variance
#' @param s previous time
#' @param t current time
#' @param v_s Vector of previous variances
#' @keywords CIR
#' @export
#' @examples
#' CIR_sample(kappa = 1, eta = 0.7, theta = 0.04, 0, t = 0.5, 0.03)



CIR_sample <- function(kappa, eta, theta, s, t, v_s){
  
  if(eta == 0){
    beta = kappa
    alpha = theta/kappa
    
    sample = exp(-beta*(t-s))*v_s + (alpha/beta)*(1-exp(-beta*(t-s)))

    
  } else {
    delta = (4 * kappa * theta) / (eta^2)
    c = (eta^2) / (4 * kappa) * (1 - exp(-kappa * (t - s)))
    lambda = 4*kappa*v_s * exp(-kappa*(t-s))/(eta*eta*(1.0-exp(-kappa*(t-s))))
    sample = c *rchisq(lambda, df = delta, ncp = lambda)

  }
  return(sample)
}



#' Generate Heston price paths
#'
#' Generates Heston price paths with almost exact simulation for the variance process and Euler discretizeation
#' for the asset price. The resulting matrix will have 1 column for each path
#' @param numPaths number of paths to generate
#' @param T maturity
#' @param r risk-free interest rate
#' @param S_0 initial asset price
#' @param dt size of timestep
#' @param kappa spead of mean reversion
#' @param eta volatility of variance
#' @param theta long run average variance
#' @param rho correlation between the two driving wiener processes
#' @param V_0 initial variance
#' @keywords Heston
#' @export
#' @examples
#' Heston_paths_AE(numPaths = 1000, T = 1, r = 0.05, S_0 = 100, dt = 1/100,kappa = 1, eta = 0.7,
#' theta = 0.04, rho = -0.7, V_0 = 0.04)


Heston_paths_AE <- function(numPaths, T, r, S_0, dt, kappa, eta, theta, rho, V_0){

  how_many_steps = T/dt
  time = seq(0, T, by = dt)

  X = matrix(NA, nrow = how_many_steps + 1, ncol = numPaths)
  X[1, ] = log(S_0)

  X_ant = matrix(NA, nrow = how_many_steps + 1, ncol = numPaths)
  X_ant[1, ] = log(S_0)

  V = matrix(NA, nrow = how_many_steps + 1, ncol = numPaths)
  V[1, ] = V_0

  k0 = (r -rho/eta*kappa*theta)*dt
  k1 = (rho*kappa/eta -0.5)*dt - rho/eta
  k2 = rho / eta

  for (i in 1:how_many_steps) {
    Z = rnorm(n = numPaths)
    V[i + 1, ] = CIR_sample(kappa, eta, theta, time[i], time[i+1], V[i, ])
    X[i + 1, ] = X[i, ] + k0 + k1*V[i, ] + k2 *V[i+1, ] + sqrt((1.0-rho^2)*V[i, ])*(sqrt(dt) * Z)
  }
  
  S = exp(X)
  rownames(S) = time

  return(S)
}


#' call option in time-dependent BS model
#'
#' Calculates the price of a vanilla option in the time dependent volatility Black-Scholes model.
#' The function assumes that volatility is a piecewise constant function
#' @param S spot asset price
#' @param K strike of the option
#' @param sigma vector of volatilities
#' @param timestep vector of times when volatility changes
#' @param r risk-free interest rate
#' @param q dividend yield
#' @param tau time to maturity
#' @param type "call" or "put"
#'
#' @keywords BSTD
#' @export
#' @examples
#' BSTD_vanilla(S = 100, K = 100, sigma = c(0.3,0.5,0.6), timesteps = c(0,0.5,2/3),
#' r = 0.05, q = 0, tau = 1, type = "call")


BSTD_vanilla <- function(S, K, sigma, timesteps, r, q, tau, type){

  b = r - q
  variance = 0

  for (i in 1:length(timesteps)){
    if (i == length(timesteps)) {
      variance = vol + sigma[i]^2 * (tau - timesteps[i])
    } else {
      variance = vol + sigma[i]^2 * (timesteps[i+1] - timesteps[i])
    }
  }

  sigma =  sqrt((1/tau) * variance)
  d1 <- (log(S / K) + (b + sigma ^ 2 / 2) * tau) / (sigma * sqrt(tau))
  d2 <- d1 - sigma * sqrt(tau)

  if(type == "call"){
    price <- S * exp((b - r) * tau) * pnorm(d1) - K * exp(-r * tau) * pnorm(d2)
  }else if (type == "put"){
    price <-  (K * exp(-r * tau) * pnorm(-d2) - S * exp((b - r) * tau) * pnorm(-d1))
  }

  return(price)
}

#' digital option in time-dep BS model
#'
#' Calculates the price of a digitaloption in the time dependent volatility Black-Scholes model.
#' The function assumes that volatility is a piecewise constant function
#' @param S spot asset price
#' @param K strike of the option
#' @param sigma vector of volatilities
#' @param timestep vector of times when volatility changes
#' @param r risk-free interest rate
#' @param q dividend yield
#' @param tau time to maturity
#' @param type "call" or "put"
#'
#' @keywords BSTD
#' @export
#' @examples
#' BSTD_digital(S = 100, K = 100, sigma = c(0.3,0.5,0.6), timesteps = c(0,0.5,2/3),
#' r = 0.05, q = 0, tau = 1, type = "call")

BSTD_digital <- function(S, K, sigma, timesteps, r, q, tau, type){

  b = r - q
  variance = 0

  for (i in 1:length(timesteps)){

    if (i == length(timesteps)) {
      variance = variance + sigma[i]^2 * (tau - timesteps[i])
    } else {
      variance = variance + sigma[i]^2 * (timesteps[i+1] - timesteps[i])
    }
  }

  sigma =  sqrt((1/tau) * variance)
  d1 <- (log(S / K) + (b + sigma ^ 2 / 2) * tau) / (sigma * sqrt(tau))
  d2 <- d1 - sigma * sqrt(tau)

  if(type == "call"){
    price <-  exp(-r * tau) * pnorm(d2)
  }else if (type == "put"){
    price <-  exp(-r * tau) * pnorm(-d2)
  }

  return(price)
}

#### analytical formula for the TDBS range accrual

sRAN_analytic_TDBS <- function(S, KUp, KLow, numTimeSteps, T, coupon, r, sigma, timesteps){
  
  time = seq(0, T, by = T/numTimeSteps)
  payout = 0
  
  for (i in 1:numTimeSteps){
    
    current_timesteps = timesteps[timesteps<time[i+1]]
    current_sigma = sigma[timesteps<time[i+1]]
    digital_up = BSTD_digital(S, KUp, current_sigma, current_timesteps, r, q = 0, tau = time[i+1], type = "call")
    digital_low = BSTD_digital(S, KLow, current_sigma, current_timesteps, r, q = 0, tau = time[i+1], type = "call")

    payout = payout + exp(r * time[i+1]) * (digital_low - digital_up)
  }
  
  price = exp(-r*T) * (coupon/numTimeSteps) * payout
  
  return(price)
}



#' Analytical formula for Heston digital option
#'
#' Calculates the price of a digital option (pays 1 unit if the asset price is above strike) in the Heston model.
#' The option price is calculated with numerical integration of the formula.
#' @param S spot asset price
#' @param X strike of the option
#' @param tau time to maturity
#' @param r risk-free interest rate
#' @param q dividend yield
#' @param V0 initial variance
#' @param VT long run variance (theta in Heston's paper)
#' @param rho correlation between the two driving wiener processes
#' @param k speed of mean reversion (kappa in Heston's paper)
#' @param sigma volatility of variance
#' @param implVol compute equivalent BSM volatility?
#'
#' @keywords digital
#' @export
#' @examples
#' callHestoncf_digital(S_0 = 100, K = 100, T = 1, r = 0.04, q = 0, V_0 = 0.04,
#'  theta = 0.03, rho = -0.7, kappa = 1, eta = 0.7, implVol = FALSE)


callHestoncf_digital <- function(S, X, tau, r, q, v0, vT, rho, k, sigma,
                                 implVol = FALSE, ...) {

  if (sigma < 0.01)
    sigma <- 0.01

  P2 <- function(om,S,X,tau,r,q,v0,vT,rho,k,sigma) {
    p <- Re(exp(-1i * log(X) * om) *
              cfHeston(om  ,S,tau,r,q,v0,vT,rho,k,sigma) /
              (1i * om))
    return(p)
  }
  
  cfHeston <- function(om,S,tau,r,q,v0,vT,rho,k,sigma) {
    d <- sqrt((rho * sigma * 1i * om - k)^2 + sigma^2 *
                (1i * om + om ^ 2))
    g <- (k - rho * sigma * 1i * om - d) /
      (k - rho * sigma * 1i * om + d)
    cf1 <- 1i * om * (log(S) + (r - q) * tau)
    cf2 <- vT*k/(sigma^2)*((k - rho * sigma * 1i * om - d) *
                             tau - 2 * log((1 - g * exp(-d * tau)) / (1 - g)))
    cf3 <- v0 / sigma^2 * (k - rho * sigma * 1i * om - d) *
      (1 - exp(-d * tau)) / (1 - g * exp(-d * tau))
    cf  <- exp(cf1 + cf2 + cf3)
    cf
  }

  vP2 <- 0.5 + 1/pi * integrate(P2, lower = 0, upper = Inf,
                                S, X, tau, r, q, v0, vT, rho, k, sigma, ...)$value
  result <-  exp(-r * tau) * vP2

  ## implied BSM vol
  if (implVol) {
    diffPrice <- function(vol,call,S,X,tau,r,q){
      d1 <- (log(S/X)+(r - q + vol^2/2)*tau)/(vol*sqrt(tau))
      d2 <- d1 - vol*sqrt(tau)
      callBSM <- S * exp(-q * tau) * pnorm(d1) -
        X * exp(-r * tau) * pnorm(d2)
      call - callBSM
    }
    impliedVol <- uniroot(diffPrice, interval = c(0.0001, 2),
                          call = result, S = S, X = X,
                          tau = tau, r = r, q = q)[[1L]]
    result <- list(value = result, impliedVol = impliedVol)
  }
  return(result)
}



#' Analytical formula for Heston RAN
#'
#' Calculates the price of a single period RAN in the Heston model
#' @param S spot asset price
#' @param Kup Vector of upper barriers (lenght must be equal to numCouponPeriods)
#' @param KLow Vector of lower barriers (lenght must be equal to numCouponPeriods)
#' @param numTimeSteps how many coupon determination periods are there?
#' @param T time to maturity
#' @param coupon max coupon to pay out
#' @param r risk-free interest rate
#' @param V0 initial variance
#' @param VT long run variance (theta in Heston's paper)
#' @param rho correlation between the two driving wiener processes
#' @param k speed of mean reversion (kappa in Heston's paper)
#' @param sigma volatility of variance
#'
#' @keywords sRAN
#' @export
#' @examples
#' sRAN_analytic(S = 100, KUp = 900, KLow = 1, numTimeSteps = 64, T = 1, coupon = 10, r = 0.1, v0 = 0.04, vT = 0.04,
#'                      rho = -0.7, k = 0.5, sigma = 0.7)

sRAN_analytic <- function(S, KUp, KLow, numTimeSteps, T, coupon, r, v0, vT, rho, k, sigma){

  time = seq(0, T, by = T/numTimeSteps)
  payout = 0
  testo = rep(NA, numTimeSteps)

  for (i in 1:numTimeSteps){
    digital_up = callHestoncf_digital(S, KUp, time[i+1], r, 0, v0, vT, rho, k, sigma, implVol = F)
    digital_low = callHestoncf_digital(S, KLow, time[i+1], r, 0, v0, vT, rho, k, sigma, implVol = F)
    testo[i] = digital_low


    payout = payout + exp(r * time[i+1]) * (digital_low - digital_up)
  }

  price = exp(-r*T) * (coupon/numTimeSteps) * payout

  return(price)

}


#' payoff of a RAN
#'
#' Calculates the price of a range-accrual note given an input of price paths
#' @param paths matrix of price trajectories. Each path should be a column and rownames should represent the time
#' @param coupon Maximum coupon of a period (RAN will pay out a percentage of this)
#' @param T time to maturity
#' @param numCouponPeriods How many coupon determination periods are there?
#' @param KUp Vector of upper barriers (lenght must be equal to numCouponPeriods)
#' @param KLow Vector of lower barriers (lenght must be equal to numCouponPeriods)
#' @param r risk-free interest rate

#' @keywords payoff
#' @export
#' @examples
#' RAN_price(paths = test_matrix, T = 1, numCouponPeriods = 2, upBarrier = 110, lowBarrier = 100, coupon = 8, r = 0.1)



RAN_price <- function(paths, coupon, T, numCouponPeriods, KUp, KLow, r) {

  numPaths = ncol(paths)
  firstCoupon = T/numCouponPeriods
  couponPaymentTime = seq(firstCoupon, T, by = firstCoupon)
  stepsInPeriod = matrix(NA, nrow = numCouponPeriods, ncol = numPaths)


  for (i in 1:numCouponPeriods){
    pathsInPeriod  = paths[(rownames(paths) > (couponPaymentTime[i] - firstCoupon)) & (rownames(paths) <= couponPaymentTime[i]), ]


    inRange = matrix(NA, nrow = nrow(pathsInPeriod), ncol = ncol(pathsInPeriod))

    for (j in 1:(nrow(pathsInPeriod))) {
      inRange[j, ] = (pathsInPeriod[j , ] >= KLow) & ((pathsInPeriod[j, ] <= KUp))
    }

    stepsInPeriod[i, ] = coupon * apply(inRange, 2, mean) * exp(-r * couponPaymentTime[i])

    payoffs = apply(stepsInPeriod, 2, sum)


  }

  return(mean(payoffs))
}


RAN_price_KOBar <- function(paths, coupon, T, numCouponPeriods, KUp, KLow, r, barrier) {
  ### given a matrix of price trajectories, it calculates the price of a RAN
  
  # paths:            matrix of price trajectories
  # coupon:           Maximum coupon of a period (RAN will pay out a percentage of this)
  # T:                Maturity
  # numCouponPeriods: How many coupon determination periods are there?
  # KUp:        Vector of upper barriers (lenght must be equal to numCouponPeriods)
  # KLow:       Vector of lower barriers
  # r:                constant, risk-free interest rate
  
  
  numPaths = ncol(paths)
  inrange = matrix(NA, nrow = nrow(paths), ncol = ncol(paths))


  
  
    for (i in 1:nrow(paths)){
      inrange[i,] = (paths[i , ] >= KLow) & ((paths[i, ] <= KUp))
    }
  
    payoffs = apply(inrange, 2, mean)
    payoffs = payoffs*coupon*exp(-r*T)
    
    maximums = apply(paths, 2, max)
    kicked = (maximums <= barrier)
    payoffs = kicked*payoffs
  return(mean(payoffs))
}


### BS price calculator
#S      ~ spot
#K      ~ strike
#sigma  ~ vol
#r      ~ risk free rate
#q      ~ dividend
#t      ~ time to maturity
#type   ~ option type

gBSM <- function(S, K, sigma, r, q, t, type){
  
  b <- r - q
  d1 <- (log(S / K) + (b + sigma ^ 2 / 2) * t) / (sigma * sqrt(t))
  d2 <- d1 - sigma * sqrt(t)
  
  if(type == "call"){
    price <- S * exp((b - r) * t) * pnorm(d1) - K * exp(-r * t) * pnorm(d2)
  }else if (type == "put"){
    price <-  (K * exp(-r * t) * pnorm(-d2) - S * exp((b - r) * t) * pnorm(-d1))
  }
  
  return(price)
}


### auxiliary function for IV_calculator
volOptimFun <- function(sigma, price, S, K, r, q, t, type){
  return(abs(price - gBSM(S, K, sigma, r, q, t, type)))
}



### Calculates the IV assosicated with a given option price, spot, strike, maturity, and interest rate
IV_calculator <- function(price, S, r, T, K){

  IV = optimize(volOptimFun, interval = c(0,3), price = price, S = S, K = K, r = r, q = 0,
                t = T, type = "call")
  return(IV)
  
}


### Calibrator function for the Heston model (multiple maturities)

# input:  ~ market data, column1 must be the vector of strikes, column2 has to be the market prices and column3 are maturities
# r:      ~ risk free interest rate
# S_0:    ~ spot underlying price
# T:      ~ maturity
# type:   ~ What type of data do we calibrate to. Either "price" or "IV"
Heston_calibrator_multiMat <- function(input, r, S_0, type, weights){
  
  if (type == "price"){
    
    error_fun <- function(params){
      rho = params[1]
      v_0 = params[2]
      theta = params[3]
      kappa = params[4]
      eta = params[5]
      
      model_prices = rep(NA, nrow(input))
      
      for (i in 1:nrow(input)){
        model_prices[i] = NMOF::callHestoncf(S = S_0, X = input[i,1], tau = input[i,3], r = r, q = 0, v0 = v_0, vT = theta, rho = rho, k = kappa, sigma = eta)
      }
      
      squared_diff = weights*(model_prices - input[,2])^2
      
      return(sum(squared_diff))
    }
    
  } else if (type == "IV"){
    
    error_fun <- function(params){
      rho = params[1]
      v_0 = params[2]
      theta = params[3]
      kappa = params[4]
      eta = params[5]
      
      model_prices = rep(NA, nrow(input))
      
      for (i in 1:nrow(input)){
        price = NMOF::callHestoncf(S = S_0, X = input[i,1], tau = input[i,3], r = r, q = 0, v0 = v_0, vT = theta, rho = rho, k = kappa, sigma = eta)
        model_prices[i] = IV_calculator(price = price, S = S_0, r = r, T = input[i,3], K = input[i,1])$minimum
      }
      
      squared_diff = weights*(model_prices - input[,2])^2
      
      return(sum(squared_diff))
    }
    
  }
  
  optim_result = optim(par = c(-0.7, 0.2^2, 0.1, 1, 1), fn = error_fun, method = "L-BFGS-B",
                       lower = c(-0.99, 0.01, 0.01, 0.01, 0.01), upper = c(0.99, 100, 100, 100, 20))
  return(optim_result)
}


### themes for graphs
theme_Publication <- function(base_size=14, base_family="sans") {
  library(grid)
  library(ggthemes)
  (theme_foundation(base_size=base_size, base_family=base_family)
    + theme(plot.title = element_text(face = "bold",
                                      size = rel(1.2), hjust = 0.5),
            text = element_text(),
            panel.background = element_rect(colour = NA),
            plot.background = element_rect(colour = NA),
            panel.border = element_rect(colour = NA),
            axis.title = element_text(face = "bold",size = rel(1)),
            axis.title.y = element_text(angle=90,vjust =2),
            axis.title.x = element_text(vjust = -0.2),
            axis.text = element_text(),
            axis.line = element_line(colour="black"),
            axis.ticks = element_line(),
            panel.grid.major = element_line(colour="#f0f0f0"),
            panel.grid.minor = element_blank(),
            legend.key = element_rect(colour = NA),
            legend.position = "bottom",
            legend.direction = "horizontal",
            #legend.key.size= unit(0.2, "cm"),
            #legend.margin = unit(0, "cm"),
            #legend.title = element_text(face="italic"),
            plot.margin=unit(c(10,5,5,5),"mm"),
            strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
            strip.text = element_text(face="bold")
    ))

}

scale_fill_Publication <- function(...){
  library(scales)
  discrete_scale("fill","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)

}

scale_colour_Publication <- function(...){
  library(scales)
  discrete_scale("colour","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)

}
