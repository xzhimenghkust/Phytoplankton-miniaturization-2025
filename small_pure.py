
##The goal of this script is to analyze the effect of phosphate (PO4) concentration on the percentage of %Small, and use the mixed effects model to fit and analyze the data of each observation station.

# The main steps are as follows:
# 1. Data loading and preprocessing: load data from CSV files, logarithmically transform PO4 values, and clean the data (remove missing values ​​and unreasonable target variables).
# 2. Mixed effects model fitting: for the data of each observation station, use the mixed effects model to analyze the relationship between PO4 concentration and %Small.
# 3. Visualization: generate a separate scatter plot and fitted regression curve for each observation station, and annotate the significance level (p value) of the regression coefficient.
# 4. Output results: draw the regression results for each observation station, and save all figures as PDF files.

# This script aims to reveal the effect of PO4 concentration on %Small through separate analysis of multiple sites, and draw conclusions based on statistical significance.


import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from statsmodels.regression.mixed_linear_model import MixedLM

# Load data
data = pd.read_csv('percentage_small_1222.csv')
data['PO4'] = np.log10(data['PO4'])  # Log-transform PO4 values

# Preprocess data
data.rename(columns={data.columns[0]: "Station"}, inplace=True)
cleaned_data = data.dropna()
cleaned_data = cleaned_data[
    (cleaned_data['Percentage_of_small'] >= 0) &
    (cleaned_data['Percentage_of_small'] <= 1)
    ]

# Get unique stations
stations = cleaned_data['Station'].unique()

# Create plot grid
fig, axes = plt.subplots(nrows=5, ncols=5, figsize=(20, 20), constrained_layout=True)
axes = axes.flatten()  # Flatten for easier iteration

# Analyze each station separately
for i, station in enumerate(stations):
    station_data = cleaned_data[cleaned_data['Station'] == station]

    # Fit mixed effects model
    model = MixedLM.from_formula(
        "Percentage_of_small ~ PO4",
        groups="Station",
        data=station_data
    ).fit()

    # Generate prediction range
    po4_range = np.linspace(station_data['PO4'].min(), station_data['PO4'].max(), 100)
    predictions = model.predict(exog=dict(PO4=po4_range))

    # Get statistical significance
    p_value = model.pvalues["PO4"]

    # Plot results
    ax = axes[i]
    ax.scatter(
        station_data['PO4'],
        station_data['Percentage_of_small'],
        marker='o', s=50, alpha=0.5,
        facecolors='none', edgecolors='#1874CD'
    )
    ax.plot(po4_range, predictions, color='red', linewidth=2)

    # Configure plot
    ax.set_title(f"Station: {station}", fontsize=10)
    ax.set_xlabel("log(PO4)")
    ax.set_ylabel("Small Organism %")
    ax.grid(alpha=0.3)
    ax.set_xlim(data['PO4'].min(), data['PO4'].max())

    # Add statistical annotation
    sig_text = "p < 0.001" if p_value < 0.001 else f"p = {p_value:.3f}"
    ax.text(0.05, 0.95, sig_text, transform=ax.transAxes,
            fontsize=9, verticalalignment='top', color="blue")

# Hide unused subplots
for j in range(i + 1, len(axes)):
    axes[j].axis('off')

# Add main title and save
plt.suptitle("Effect of Phosphate (PO4) on Small Organism Percentage by Station",
             fontsize=16, fontweight='bold')
plt.savefig("phosphate_effect_on_organisms.pdf", dpi=300, bbox_inches='tight')
plt.show()