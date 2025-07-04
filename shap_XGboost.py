
## The goal of this script is to train an XGBoost regression model and analyze its feature importance, using SHAP values ​​for model interpretation.

# The main steps are as follows:
# 1. Data loading and preprocessing: Load data from a CSV file, replace missing values ​​and perform logarithmic transformation.
# 2. Outlier detection: Use One-Class SVM to detect and filter out outliers in the data.
# 3. Data segmentation and model training: Split the data into training and test sets, and train the XGBoost model.
# 4. SHAP value calculation and visualization: Calculate the feature importance of the model through SHAP values, and generate different types of visualization charts, including:
# - Feature importance summary plot
# - Feature importance bar plot
# - SHAP scatter plot, using density color to represent the density of data points
# 5. Result output: Save all generated visualization charts to the specified directory

# This script helps explain the prediction results of the XGBoost model through SHAP analysis, intuitively showing the impact of each feature on the model output, making it easier to understand the model's decision-making process.


import pandas as pd
import numpy as np
from xgboost import XGBRegressor
from sklearn.model_selection import train_test_split
from sklearn.svm import OneClassSVM
import shap
import matplotlib.pyplot as plt
from scipy.stats import gaussian_kde
import os

# Load and preprocess data
data = pd.read_csv('data.csv')
data = data.replace(0, np.nan).dropna()

# Prepare target variable
target = 'BioV'
y = np.log(data[target])

# Define columns for log transformation
log_transform_columns = [
    'NH3', 'Total_P', 'Total_N', 'Total_KN', 'TIN', 'silica',
    'PO4', 'NO2_N', 'NO3_N', 'Faecal', 'E_coli', 'NH3_N', 'DIN'
]

# Apply log transformations
for col in log_transform_columns:
    data[f'{col}_log10'] = np.log10(data[col].where(data[col] > 0))

# Define feature set
exclude_columns = log_transform_columns + [
    'Sample', 'Station', 'Waterzone', 'Species', 'Chl_a', 'BioV', 'SS'
]
features = [col for col in data.columns if col not in exclude_columns]
X = data[features]

# Detect outliers using One-Class SVM
svm = OneClassSVM(kernel='rbf', gamma='auto', nu=0.1)
svm.fit(X)
data['is_outlier'] = svm.predict(X).apply(lambda x: 1 if x == -1 else 0)

# Filter out outliers
clean_data = data[data['is_outlier'] == 0]
X_clean = clean_data[features]
y_clean = clean_data[target]

# Split data and train model
X_train, X_test, y_train, y_test = train_test_split(X_clean, y_clean, test_size=0.2, random_state=42)
model = XGBRegressor(random_state=42, n_jobs=-1)
model.fit(X_train, y_train)

# Compute SHAP values
explainer = shap.TreeExplainer(model)
shap_values = explainer.shap_values(X_test)
if isinstance(shap_values, list):
    shap_values = shap_values[0]
explanation = shap.Explanation(values=shap_values,
                               base_values=explainer.expected_value,
                               data=X_test)

# Prepare output directory
output_folder = '.../test/shap_result'
os.makedirs(output_folder, exist_ok=True)

# Generate summary plot
plt.figure(figsize=(12, 8))
shap.summary_plot(shap_values, X_test, feature_names=features, show=False)
plt.xlim(-1e+4, 1.1e+4)
plt.savefig(os.path.join(output_folder, 'summary_plot_size.pdf'), bbox_inches='tight')
plt.close()

# Generate bar plot
plt.figure(figsize=(12, 8))
shap.summary_plot(shap_values, X_test, feature_names=features, plot_type="bar", show=False)
plt.savefig(os.path.join(output_folder, 'bar_plot_size.pdf'), bbox_inches='tight')
plt.close()

# Generate SHAP scatter plot with density
scatter_feature_x = 'Temp'
if scatter_feature_x in X_test.columns:
    x_values = X_test[scatter_feature_x]
    y_values = shap_values[:, features.index(scatter_feature_x)] * 3

    # Calculate point density
    xy = np.vstack([x_values, y_values])
    density = gaussian_kde(xy)(xy)

    # Create density-colored scatter plot
    plt.figure(figsize=(12, 8))
    scatter = plt.scatter(
        x_values,
        y_values,
        c=density,
        cmap='coolwarm',
        s=20,
        alpha=0.8
    )
    plt.colorbar(scatter, label='Point Density')
    plt.title(f'SHAP Scatter Plot: {scatter_feature_x}', fontsize=14)
    plt.xlabel(scatter_feature_x, fontsize=12)
    plt.ylabel(f'SHAP value for {scatter_feature_x}', fontsize=12)
    plt.ylim(-10000, 20000)
    plt.grid(True, linestyle='--', alpha=0.5)
    plt.savefig(os.path.join(output_folder, f'scatter_plot_{scatter_feature_x}_density.pdf'), bbox_inches='tight')
    plt.show()
else:
    print(f"Feature '{scatter_feature_x}' not found in test data")