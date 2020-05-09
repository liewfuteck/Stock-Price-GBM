# Stock Price GBM
For this project, we use a Geometric Brownian Motion Model to simulate stock prices for 25 companies (5 companies across 5 industries).
We take the stock price data across a 6 month period as training data, taking the mean of stock price data during this period and zero drift as a baseline. We then improve the model by updating our parameters mu and sigma, using the Geometric Brownian Motion model framework to ascertain if our simulated stock price 30 days later falls within the 95% confidence interval. Finally, we consider a different approach, where we simulate based on the last n days and continually update our 95% confidence interval.

For full report on methodology, analysis and results, see "Main Report".

## List of Datasets (Company Names + Stock Symbol)
### Technology
Microsoft (MSFT)

Apple (AAPL)

Alphabet (GOOG)

Amazon (AMZN)

Facebook (FB)

### Finance
Bank of America (BAC)

Berkshire Hathaway (BRK)

JP Morgan (JPM)

Mastercard (MA)

Visa (V)

### Healthcare
Abbott (ABT)

Johnson & Johnson (JNJ)

Merck & Co. (MRK)

Novartis (NVS)

Pfizer (PFE)

### Consumer Services
Coca-Cola (KO)

Nike (NKE)

Proctor & Gamble (PG)

PepsiCo (PEP)

Philip Morris (PM)

### Telecommunications
AT&T (T)

BCE Inc (BCE)

China Mobile (CHL)

T-Mobile (TMUS)

Verizon Communications (VZ)
