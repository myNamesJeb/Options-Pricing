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
- Compute accuracy metrics:
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
    1. incorperate the algorithms with the main.ml - 2 weeks
    2. measure error of the option pricing - 2 weeks
    3. visualize output - 1 week

- [ ] Parsing Stock Data:
    1. import .csv - 1 week
    2. parse stock data - 1 week 
    3. streamline getting date ranges for option pricing in main.ml - 1-1.2 weeks
    
- [x] algo1 - 1 week
- [x] algo2 - 1 week
- [ ] algo3 - 1 week


## Journal:


09/23/2025:
Added better data visuals and added 6 month predictions


License


MIT License

Author

Wilderf
