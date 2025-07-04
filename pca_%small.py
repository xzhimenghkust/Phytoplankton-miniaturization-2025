
## The goal of this script is to perform principal component analysis (PCA) on the features in the environmental dataset to achieve data dimensionality reduction and analyze the contribution of each feature in the principal component.

# The specific steps are as follows:
# 1. Data loading and preprocessing: Load data from CSV files, handle missing values, replace 0 with NaN and delete rows containing missing values.
# 2. Logarithmic transformation: Logarithmically transform specific columns (such as NH3, Total_P, Total_N, etc.) so that the data is more in line with the normal distribution.
# 3. Feature selection: Exclude the target variable (%Small) and other specific columns as feature data.
# 4. Feature standardization: Standardize the feature data so that each feature has the same scale.
# 5. PCA analysis: Perform principal component analysis on the standardized data, automatically determine the number of principal components, and output the variance explained ratio of each principal component.
# 6. Feature importance analysis: Calculate the contribution of each feature in all principal components and rank them according to their contribution.

# Therefore, the goal is to reduce the dimensionality of the data and reveal the importance of different features in the principal components to provide support for further analysis and modeling.

import pandas as pd
import numpy as np
from sklearn.preprocessing import StandardScaler
from sklearn.decomposition import PCA

# Load dataset
data = pd.read_csv('percentage_small_1215.csv')
data = data.replace(0, np.nan).dropna()  # Handle missing values

# Define target variable
target = 'Percentage_of_small'
y = data[target]

# Columns requiring log transformation
log_transform_columns = [
    'NH3', 'Total_P', 'Total_N', 'Total_KN', 'TIN', 'silica',
    'PO4', 'NO2_N', 'NO3_N', 'Faecal', 'E_coli', 'NH3_N', 'DIN'
]

# Create log-transformed columns
for col in log_transform_columns:
    data[f'{col}_log10'] = np.log10(data[col].where(data[col] > 0))

# Columns to exclude from features
exclude_columns = [
    'Chl_a', 'Percentage_of_small', 'Phaeo_pigments', 'BOD5'
]

# Prepare feature matrix
features = [col for col in data.columns if col not in exclude_columns]
X = data[features]

# Standardize features
scaler = StandardScaler()
scaled_x = scaler.fit_transform(X)

# Initialize and fit PCA
pca = PCA(n_components='mle')
pca.fit(scaled_x)

# Print explained variance ratios
var_ratio = pca.explained_variance_ratio_
for idx, val in enumerate(var_ratio, 1):
    print("Principal component %d: %.2f%%" % (idx, val * 100))
print("Total explained variance: %.2f%%" % np.sum(var_ratio * 100))

# Analyze feature importance in components
feature_importance = np.abs(pca.components_)
feature_importance_sum = np.sum(feature_importance, axis=0)

# Create feature ranking dataframe
ranking_df = pd.DataFrame({'Feature': features, 'Contribution': feature_importance_sum})
ranking_df = ranking_df.sort_values(by='Contribution')

print("\nFeature importance ranking:")
print(ranking_df)