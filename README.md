# Portfolio_Analysis

Portfolio Analysis in R

Version: 1.0.0
Stage: Pre-Alpha/Alpha
Author: Akhilesh B

NOTE: Although, past performance does NOT guarantee future returns, the tool is helpful to learn and build a portfolio and compare it's performance to a Benchmark.

Objective:
The project is focused at finding an optimum combination of securities and build an ideal mean-variance portfolio.
The script relies on PerformanceAnalytics and PortfolioAnalytics package to find the ideal combination, using built in solvers.
The ideal portfolio with recommended weights is compared to the perfomance of a benchmark (S&P500 for now) over a user defined period.

Input: The end user can select anything between 3-5 securities/ETFs as input along with a start date for return calculation

Processing: The script takes all possible combinations of given securities, i.e.5C3+5C4+5C5 = 16 combinations would be possible if user selects 5 securities.
Calculates return for each individual security in the portfolio, recommends ideal weights for each of them, with the objective of maximizing returns and minimizing risk.
A weight constraint is put with minimum of 0.05 % and maximum of 0.5% weight.
Recommended weights are then used to calculate Portfolio Returns and Standard Deviation from the start date upto the latest closing date of the market.

Output: 1) The script generates Top 5 portfolios with their Returns, Std Deviation, Sharpe ratio and weights of individual securities
        2) Multiple risk measures of the portfolio
        3) Graphical Comparision of Portfolio Returns with Benchmark Returns with wealth index on the Y Axis.

EndNote: The Project is in pre-alpha stage, there may be bugs here and there. Script uses default source (Yahoo Finance for now) assigned by quantmod package to fetch the scrip data. Security may or may not be available on the source used.

