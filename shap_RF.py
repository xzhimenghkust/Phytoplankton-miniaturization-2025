

##The goal of this script is to model the data using a random forest regression model and explain the feature importance of the model predictions through SHAP analysis.

# The main steps are as follows:
# 1. Data loading and preprocessing: Load data from CSV files, handle missing values ​​and perform necessary logarithmic transformations.
# 2. Outlier detection: Use the Isolation Forest algorithm to detect and remove outliers in the data to clean up the dataset.
# 3. Feature engineering: Select features and apply STL decomposition to seasonally adjust the temperature data.
# 4. Training and evaluation: Divide the data into training and test sets, train the random forest regression model, and calculate the mean square error (MSE) for evaluation.
# 5. SHAP analysis: Use SHAP (Shapley Additive Explanations) to calculate the feature importance of the model and generate different types of visualization charts, including:
# - Feature importance summary plot
# - Feature importance bar plot
# - Dependence plot of the interaction between features and other features
# 6. Result output: Save the generated SHAP visualization chart to the specified directory.

# This script helps explain the prediction results of the random forest regression model through SHAP analysis, intuitively showing the impact of each feature on the model output, making it easier to understand the decision-making process of the model.


import pandas as pd
import numpy as np
from sklearn.model_selection import train_test_split
from sklearn.ensemble import RandomForestRegressor, IsolationForest
from sklearn.metrics import mean_squared_error
import shap
import matplotlib.pyplot as plt
from statsmodels.tsa.seasonal import STL
import os

# Isolation Forest Outlier Detection
def isolation_forest_outlier_detection(X, contamination=0.05):
    """
    Detecting outliers using Isolation Forest
    :param X: input feature dataframe
    :param contamination: proportion of outliers in the data
    :return: cleaned dataframe with outliers removed
    """
    iso_forest = IsolationForest(n_estimators=100, contamination=contamination, random_state=42, n_jobs=-1)
    predictions = iso_forest.fit_predict(X)
    # Remove outliers (label -1 indicates outliers)
    mask = predictions != -1
    return mask


if __name__ == '__main__':
    # Load data
    data = pd.read_csv('percentage_small_1215.csv')
    data = data.replace(0, np.nan).dropna()

    # Define the target variable
    target = 'Percentage_of_small'
    y = data[target]


    # Select Features (Exclude Specified Columns)
    exclude_columns = ['Chl_a', 'Percentage_of_small', 'Phaeo_pigments', 'BOD5']
    features = [col for col in data.columns if col not in exclude_columns]
    X = data[features]
    stl = STL(X['Temp'], period=12)
    result = stl.fit()
    X['Temp'] = result.trend


    # Apply Isolation Forest to detect and remove outliers
    mask = isolation_forest_outlier_detection(X, contamination=0.05)  # Assume 5% of data are outliers
    clean_data = data[mask]

    # Cleaned data
    X_clean = clean_data[features]
    y_clean = clean_data[target]

    # Divide training set and test set
    X_train, X_test, y_train, y_test = train_test_split(X_clean, y_clean, test_size=0.2, random_state=42)

    # Create and train a RandomForest model
    rf_model = RandomForestRegressor(
        n_estimators=150,
        max_depth=6,
        random_state=42
    )
    rf_model.fit(X_train, y_train)

    # Evaluate the model
    y_pred = rf_model.predict(X_test)
    mse = mean_squared_error(y_test, y_pred)
    print(f"Mean Squared Error (MSE): {mse}")

    # Using TreeExplainer Analysis
    explainer = shap.TreeExplainer(rf_model)
    shap_values = explainer.shap_values(X_test)

    # Output folder
    output_folder = '.../test/shap_result'
    os.makedirs(output_folder, exist_ok=True)

    # Summary Plot
    plt.figure(figsize=(12, 8))
    shap.summary_plot(shap_values, X_test, feature_names=features, show=False)
    plt.savefig(os.path.join(output_folder, 'summary_plot_small.pdf'), bbox_inches='tight')
    plt.close()

    # Bar Plot
    plt.figure(figsize=(12, 8))
    shap.summary_plot(shap_values, X_test, feature_names=features, plot_type="bar", show=False)
    plt.savefig(os.path.join(output_folder, 'bar_plot_small.pdf'), bbox_inches='tight')
    plt.close()

    # Dependence Plot (SHAP Value vs PO4 with interaction effect from Temp)
    plt.figure(figsize=(12, 8))

    # Corrected shap.dependence_plot call
    shap.dependence_plot(
        'Temp',
        shap_values,
        X_test,
        interaction_index='PO4',
        show=False
    )
    plt.ylim(-0.2,0.2)
    # Save the dependence plot
    plt.savefig(os.path.join(output_folder, 'shap_dependence_plot_po4_temp.pdf'), bbox_inches='tight')
    plt.close()

