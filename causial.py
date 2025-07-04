

## #The goal of this script is to estimate the average treatment effect (ATE) and individual treatment effect (ITE) between multiple variables by using the Causal Forest method.

# Specifically, the script uses the CausalForestDML model combined with RandomForestRegressor to model the causal inference problem and explore the causal relationship between different variables

# The analysis objectives of the script include:
# 1. Estimate the effect of temperature (Temp) on phosphate (PO4) concentration (Temp -> PO4).
# 2. Estimate the effect of PO4 on %Small (PO4 -> %Small).
# 3. Further analyze the effect of temperature on %Small (Temp -> %Small).
# 4. Finally, causal inference is performed by combining the dual effects of PO4 and temperature on %Small (PO4 -> Temp).

# In summary, the script aims to reveal the causal effects between different variables in order to better understand and predict the impact of environmental factors on species distribution or behavior in ecosystems.

# Each part includes the estimation of ATE and visualizes the individual treatment effects for detailed result analysis and interpretation.

# Import libraries
from econml.dml import CausalForestDML
from sklearn.ensemble import RandomForestRegressor
from sklearn.model_selection import train_test_split
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

# Load data
data = pd.read_csv("percentage_small_1215.csv")

# Handle missing values
data = data.dropna()

# Define variables
Y = data['Percentage_of_small'].values
T = data['Temp'].values
W = data['PO4'].values.reshape(-1, 1)
X = data[['silica', 'DO_sat', 'NO2_N', 'Secchi_depth', 'Total_N', 'salinity', 'Total_P']].values

# Split into train/test sets
X_train, X_test, T_train, T_test, Y_train, Y_test, W_train, W_test = train_test_split(
    X, T, Y, W, test_size=0.2, random_state=42
)

# Initialize causal forest model
causal_forest = CausalForestDML(
    model_t=RandomForestRegressor(n_estimators=100, min_samples_leaf=5, random_state=42),
    model_y=RandomForestRegressor(n_estimators=100, min_samples_leaf=5, random_state=42),
    discrete_treatment=False,
    cv=3,
    random_state=42
)

# Fit model
causal_forest.fit(Y_train, T_train, X=X_train, W=W_train)

# Estimate Average Treatment Effect (ATE)
ate_temp_to_po4 = causal_forest.effect(X_test, T0=0, T1=1)

# Print results
print("ATE (Temp -> PO4):", np.mean(ate_temp_to_po4))
print("ATE std:", np.std(ate_temp_to_po4))

# Visualize Individual Treatment Effects (ITE)
plt.hist(ate_temp_to_po4, bins=30, color="skyblue", edgecolor="k")
plt.title("Individual Treatment Effects (Temp -> PO4)")
plt.xlabel("Treatment Effect")
plt.ylabel("Frequency")
plt.show()

# Define new variables
Y = data['Percentage_of_small'].values
T = data['PO4'].values
X = data[['silica', 'DO_sat', 'NO2_N', 'Secchi_depth', 'Total_N', 'salinity', 'Total_P']].values

# Split data
X_train, X_test, T_train, T_test, Y_train, Y_test = train_test_split(
    X, T, Y, test_size=0.2, random_state=42
)

# Initialize model
causal_forest_po4 = CausalForestDML(
    model_t=RandomForestRegressor(n_estimators=100, min_samples_leaf=5, random_state=42),
    model_y=RandomForestRegressor(n_estimators=100, min_samples_leaf=5, random_state=42),
    discrete_treatment=False,
    cv=3,
    random_state=42
)

# Fit model
causal_forest_po4.fit(Y_train, T_train, X=X_train, W=None)

# Calculate ATE
T0 = np.percentile(T_train, 10)
T1 = np.percentile(T_train, 90)
ate_po4_to_small = causal_forest_po4.effect(X_test, T0=T0, T1=T1)

# Print results
print("ATE (PO4 -> % of small):", np.mean(ate_po4_to_small))
print("ATE std:", np.std(ate_po4_to_small))

# Visualize ITE
plt.hist(ate_po4_to_small, bins=30, color="skyblue", edgecolor="k")
plt.title("Individual Treatment Effects (PO4 -> % of small)")
plt.xlabel("Treatment Effect")
plt.ylabel("Frequency")
plt.show()

# Define variables
Y = data['Percentage_of_small'].values
T = data['Temp'].values
X = data[['PO4', 'silica', 'DO_sat', 'NO2_N', 'Secchi_depth', 'Total_N', 'salinity', 'Total_P']].values

# Split data
X_train, X_test, T_train, T_test, Y_train, Y_test = train_test_split(
    X, T, Y, test_size=0.2, random_state=42
)

# Initialize model
causal_forest_temp = CausalForestDML(
    model_t=RandomForestRegressor(n_estimators=100, min_samples_leaf=5, random_state=42),
    model_y=RandomForestRegressor(n_estimators=100, min_samples_leaf=5, random_state=42),
    discrete_treatment=False,
    cv=3,
    random_state=42
)

# Fit model
causal_forest_temp.fit(Y_train, T_train, X=X_train, W=None)

# Calculate ATE
T0 = np.percentile(T_train, 10)
T1 = np.percentile(T_train, 90)
ate_temp_to_small = causal_forest_temp.effect(X_test, T0=T0, T1=T1)

# Print results
print("ATE (Temp -> % of small):", np.mean(ate_temp_to_small))
print("ATE std:", np.std(ate_temp_to_small))

# Visualize ITE
plt.hist(ate_temp_to_small, bins=30, color="skyblue", edgecolor="k")
plt.title("Individual Treatment Effects (Temp -> % of small)")
plt.xlabel("Treatment Effect")
plt.ylabel("Frequency")
plt.show()

# Define variables
Y = data['Percentage_of_small'].values
T = data['PO4'].values
W = data['Temp'].values.reshape(-1, 1)
X = data[['silica', 'DO_sat', 'NO2_N', 'Secchi_depth', 'Total_N', 'salinity', 'Total_P']].values

# Split data
X_train, X_test, T_train, T_test, Y_train, Y_test, W_train, W_test = train_test_split(
    X, T, Y, W, test_size=0.2, random_state=42
)

# Initialize model
causal_forest_PO4_Temp = CausalForestDML(
    model_t=RandomForestRegressor(n_estimators=100, min_samples_leaf=5, random_state=42),
    model_y=RandomForestRegressor(n_estimators=100, min_samples_leaf=5, random_state=42),
    discrete_treatment=False,
    cv=3,
    random_state=42
)

# Fit model
causal_forest_PO4_Temp.fit(Y_train, T_train, X=X_train, W=W_train)

# Estimate ATE
ate_po4_to_temp = causal_forest_PO4_Temp.effect(X_test, T0=0, T1=1)

# Print results
print("ATE (PO4-> Temp):", np.mean(ate_po4_to_temp))
print("ATE std:", np.std(ate_po4_to_temp))

# Visualize ITE
plt.hist(ate_po4_to_temp, bins=30, color="skyblue", edgecolor="k")
plt.title("Individual Treatment Effects (PO4-> Temp)")
plt.xlabel("Treatment Effect")
plt.ylabel("Frequency")
plt.show()