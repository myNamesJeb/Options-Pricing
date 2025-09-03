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

## Project Structure

ocaml-options-pricing/
├── src/
│ ├── black_scholes.ml
│ ├── binomial_tree.ml
│ ├── monte_carlo.ml
│ └── main.ml
├── data/
│ └── apple_historical.csv
├── README.md
└── dune


- **src/**: OCaml implementation of pricing models and main driver code.  
- **data/**: Historical stock and option price data.  
- **dune**: OCaml build configuration.  

---

## Prerequisites
- OCaml 5.x  
- Dune build system  
- CSV data parsing library (e.g., `csv`)  
- Optional: plotting library for visual analysis  

---

progress marks:
- [ ] Framework Down
- [ ] Parsing Stock Data
- [ ] algo1 
- [ ] algo2
- [ ] algo3



License

MIT License
Author

Wilderf
