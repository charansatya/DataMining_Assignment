Build Neural Network and CART model on same datasheets:
Steps involved should be:

a) Data Import (Target variable is "Attrition" column)
b) Split the data in Dev & Hold Out sample (70:30)
c) Perform Exploratory Data Analysis
d) Identify columns which are of no use. drop those columns
e) Write Hypothesis and validate the Hypothesis
f) Build Neural Network Model (Development sample)
g) Validate NN model on Hold Out. If need be improvise
h) Build CART Model
i) Validate CART Model
j) Compare NN with CART
k) Combine NN and CART into Ensemble Model
l) Check whether Ensemble Model Performance outperforms the individual CART & NN model