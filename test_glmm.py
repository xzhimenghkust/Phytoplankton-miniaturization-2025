

##The goal of this script is to analyze and model environmental data using principal component analysis (PCA) and generalized linear mixed models (GLMM).

# The main steps are as follows:
# 1. Data loading and preprocessing: Load data from CSV files and perform logarithmic transformation on specific variables (such as BioV, DIN, PO4, etc.) to ensure normality of the data.
# 2. Principal component analysis (PCA): Perform PCA analysis on standardized features, calculate and output the explained variance proportion of each principal component, and help understand the main sources of variation in the data.
# 3. GLMM modeling: Based on the specified fixed effect variables (such as temperature, PO4) and species as random effects, use generalized linear mixed models for fitting to study the effects of these variables on biovolume (BioV).
# 4. Species-level analysis: Perform GLMM fitting for each species separately, analyze the effect of PO4 on BioV, and draw the corresponding scatter plots and fitting curves to show the fitting results and significance level (p value) of each species.

# PCA analysis helps understand the principal component composition of the data, and GLMM analysis reveals the relationship between environmental factors and species, providing a deeper understanding of bioV.

import pandas as pd
import numpy as np
from statsmodels.formula.api import mixedlm
from sklearn.preprocessing import StandardScaler
from sklearn.decomposition import PCA
import matplotlib.pyplot as plt

# read data
data = pd.read_csv('data.csv')
spp_order = pd.read_csv('spp25.csv')

# Take the logarithm of the response variable (to ensure non-negative values)
data['Log_BioV'] = np.log(data['BioV'].clip(lower=1e-6))
data['Log10_DIN'] = np.log10(data['DIN'].clip(lower=1e-6))
data['Log10_PO4'] = np.log10(data['PO4'].clip(lower=1e-6))
species_order = spp_order['Species'].tolist()

data['Species'] = pd.Categorical(data['Species'], categories=species_order, ordered=True)
data = data.sort_values('Species')

print(data.head())

# PCA analysis part
data_last_column = data.shape[1]
target = 'Log_BioV'  # Update the target variable to logarithm
features = data.columns.drop(['BioV', 'Log_BioV', 'Sample', 'Station', 'Waterzone', 'Species', 'Log10_DIN',
                              'Log10_PO4'])

Y = data.values[:, 4]
x = data.values[:, 5:-3]

print(x)

scaler = StandardScaler()
scaled_x = scaler.fit_transform(x)

# Create a PCA object and fit the data
pca = PCA(n_components='mle')
pca.fit(scaled_x)

var_ratio = pca.explained_variance_ratio_
for idx, val in enumerate(var_ratio, 1):
    print("Principal component %d: %.2f%%" % (idx, val * 100))
print("Total: %.2f%%" % np.sum(var_ratio * 100))

print(pca.components_)
feature_importance = np.abs(pca.components_)
feature_importance_sum = np.sum(feature_importance, axis=0)
print("\nImportance (contribution) of original features:")
ranking_df = pd.DataFrame({'feature': features, 'importance': feature_importance_sum})
ranking_df = ranking_df.sort_values(by='importance')
print(ranking_df)

# GLMM modeling part
fixed_effects = ['Temp', 'DIN', 'PO4']   # Specify fixed effect variables

# Check if the data contains the required columns
required_columns = fixed_effects + ['Species', target]
missing_columns = [col for col in required_columns if col not in data.columns]
if missing_columns:
    raise ValueError(f"The dataset is missing the following required columns: {missing_columns}")

# Constructing the GLMM model formula
formula = f"{target} ~ {' + '.join(fixed_effects)}"
print(f"Model formula: {formula}")

# constructing the GLMM model formula
model = mixedlm(formula, data, groups=data["Species"])  # Use Species as a random effect group
result = model.fit()
print(result.summary())
model_result = {}

rows, cols = 5, 5
fig, axes = plt.subplots(rows, cols, figsize=(20, 20))
axes = axes.flatten()
y_min = data['Log_BioV'].min()
y_max = data['Log_BioV'].max()

for i, species in enumerate(species_order):
    species_data = data[data['Species'] == species]

    formula_each = "Log_BioV ~ Log10_PO4"
    model_each = mixedlm(formula_each, species_data, groups=species_data["Species"])
    each_result = model_each.fit()

    model_result[species] = each_result
    print(f"Species: {species}")
    print(each_result.summary())

    intercept = each_result.params["Intercept"]
    slope = each_result.params["Log10_PO4"]
    p_value = each_result.pvalues["Log10_PO4"]

    ax = axes[i]
    ax.scatter(species_data["Log10_PO4"], species_data["Log_BioV"], marker='o', s=50, alpha=0.5, facecolors='none',
               edgecolors='#1874CD', label="Data")
    x_vals = np.linspace(species_data["Log10_PO4"].min(), species_data["Log10_PO4"].max(), 100)
    y_vals = intercept + slope * x_vals
    ax.plot(x_vals, y_vals, color="red", label="Fit", linewidth=2.5)

    # Unify the y-axis range
    ax.set_ylim(y_min, y_max)

    ax.set_title(f"Species: {species}")
    ax.set_xlabel("Log10_PO4")
    ax.set_ylabel("Log(BioV)")
    ax.text(0.05, 0.95, f"P = {p_value:.3e}", transform=ax.transAxes,
            fontsize=10, verticalalignment='top', color="blue")  # show p values

# Delete redundant subframes
for j in range(len(species_order), len(axes)):
    fig.delaxes(axes[j])

plt.tight_layout()
#plt.savefig("glmm_species_PO4_fig.pdf", dpi=300)
plt.show()

