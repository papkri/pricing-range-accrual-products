# pricing-range-accrual-products

This repository contains the code used for my MSc thesis titled Pricing Range Accrual Products.

Two different market models were used: The time-dependent Black-Scholes model, and the Heston model

The implementation was done in the R programming language.

The file functions.R contain all the main functions for:

  -calculating the price of a range accrual in the two market models analytically
  
  -simulating the Heston model with the "almost-exact" simulaiton
  
  -calibrating the Heston model
 
 Other files were used for creating the figures in the thesis. Any time when the dataframe "prices" is used, it is the combined_calibration_data.xslsx
