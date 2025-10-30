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
    [x] incorperate the algorithms with the main.ml - 2 weeks
    [ ] measure error of the option pricing - 2 weeks
    [ ] visualize output - 1 week

- [ ] Parsing Stock Data:
    [x] import .csv - 1 week
    [x] parse stock data - 1 week 
    [ ] streamline getting date ranges for option pricing in main.ml - 1-1.2 weeks
    
- [x] algo1 - 1 week
- [x] algo2 - 1 week
- [x] algo3 - 1 week


## Journal:




10/30/2025:
made yuuuuuge changes, added implemet for **Forbes Pricing** (a more institutional pricing model)

The Forbes Pricing model estimates fair values for options by blending Black-Scholes math with adjustments 
that reflect how institutions actually trade. It calculates volatility not as a single fixed number, but 
as a “surface” that changes with strike price and time to maturity, making the prices more realistic. The 
model then computes call and put values, adjusting them for spreads, liquidity, and hedging costs to mimic 
real market conditions. It also outputs option “Greeks,” which describe how sensitive the option price is 
to changes in the market. In short, it gives prices and risk measures that are closer to what professional 
desks would quote rather than purely theoretical values.

it signifigantly outperforms pure Black-Scholes, and for that matter, the other 2 pricing models, which, i learned far to late; ARE MEANT TO MIMIC EACHOTHER??!?!? WHY??? "hmmm lets make a pricing model different than Black Scholes"
"great idea!" "LETS MAKE IT ACT EXXXACCCTTLYYLYYY LIKE BLACK SCHOLES!!! BUT DIFFERENT MATH! ALMOST EXACT SAME OUTPUT!!"
"sounds like a great idea!" LIKE WHY?? THEN THEY DID IT TWICE!?



09/23/2025:
Added better data visuals and added 6 month predictions

09/25/2025:
had some difficulty with a function (Calculate_price) where it was not liking me putting a let () = [aka a generic func.] be a named func with args, it was throwing bad errors, but i figured out it was just because i missed an in (which is like missing a ; a bit)

09/29/2025:
had some issues with calling Calculate_price, but fixed by making two nondescript functions, and also implemented some minor error calculation and better formatting for test data output
-- sorry for not updating readme in commits, was doing `git add .` from options_pricing, not this dir -- 

09/30/2025:
did initial tests with Binomial Tree and calculated:



----------------------------------------
Average BUYER call profit = 1.27
Average BUYER put profit  = 2.43
----------------------------------------
Total Premiums Collected (Company Income)   = 49697.14
Total Payoffs Paid Out (Company Expense)    = 58331.48
Net Company Profit (Income - Expense)       = -8634.34

which just means that Binomial Tree isnt that great, and lost 8.63k

06/10/2025:
Rewrote main.ml to be cleaner, it looks much better, cleaned some parts of the bin tree or black Scholes

License


MIT License

Author

Wilderf
