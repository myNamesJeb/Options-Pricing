# OCaml Options Pricing Comparison
## Project Overview
This project implements and compares **three popular options pricing algorithms** against historical data for **Apple Inc. (AAPL)** over the past five years. The goal is to determine which pricing model most accurately predicts real market option prices.

The project is written in **OCaml**, leveraging its strong type system and functional programming features to model financial instruments safely and efficiently.

---

## Objectives
- Implement three options pricing algorithms:
  1. **Black–Scholes Model**
  2. **Binomial Tree Model**
  3. **Monte Carlo Simulation**
- Compare each algorithm against historical Apple stock and option price data over a **five-year period**.
- Analyze accuracy, performance, and practical usability of each method.

---

## Features
- Load historical stock and options data (csv).
- Calculate theoretical option prices for each method.
 Compute accuracy metrics:
  - Mean Absolute Error (MAE)
  - Root Mean Squared Error (RMSE)
  - Percent error distribution
- Generate visual comparisons (CLI output).
- Also learn NVIM with NVchad
---

## Prerequisites
- OCaml 5.x  
- Dune build system  
- CSV data parsing library (e.g., `csv`)  
- Optional: plotting library for visual analysis  

---

progress marks:
- [ ] Framework Down:
    [ ] incorperate the algorithms with the main.ml - 2 weeks
    [ ] measure error of the option pricing - 2 weeks
    [ ] visualize output - 1 week

- [ ] Parsing Stock Data:
    [x] import .csv - 1 week
    [x] parse stock data - 1 week 
    [ ] streamline getting date ranges for option pricing in main.ml - 1-1.2 weeks
    
- [x] algo1 - 1 week
- [x] algo2 - 1 week
- [ ] algo3 - 1 week


## Journal:


09/23/2025:
Added better data visuals and added 6 month predictions

09/25/2025:
had some difficulty with a function (Calculate_price) where it was not liking me putting a let () = [aka a generic func.] be a named func with args, it was throwing bad errors, but i figured out it was just because i missed an in (which is like missing a ; a bit)

09/29/2025:
had some issues with calling Calculate_price, but fixed by making two nondescript functions, and also implemented some minor error calculation and better formatting for test data output
-- sorry for not updating readme in commits, was doing `git add .` from options_pricing, not this dir -- 

30/09/2025:
did initial tests with Binomial Tree and calculated:

----------------------------------------
Average BUYER call profit = 1.27
Average BUYER put profit  = 2.43
----------------------------------------
Total Premiums Collected (Company Income)   = 49697.14
Total Payoffs Paid Out (Company Expense)    = 58331.48
Net Company Profit (Income - Expense)       = -8634.34

which just means that Binomial Tree isnt that great, and lost 8.63k


License


MIT License

Author

Wilderf
