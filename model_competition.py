
##The goal of this script is to train and evaluate multiple regression models and compare their performance in predicting the transformed value of the percentage of %Small.

# The script includes the following main steps:
# 1. Data preprocessing: load the data and apply smoothing to the target variable, transform the target variable using log transformation, handle missing values ​​and standardize features.
# 2. Feature selection: select the most relevant features for model training through analysis of variance (f-regression).
# 3. Model definition and training: define multiple regression models (ridge regression, random forest, XGBoost, support vector machine (SVM) and artificial neural network (ANN)) and train them.
# 4. Performance evaluation: evaluate the performance of each model using MAE, RMSE, R2 and other indicators and compare them.
# 5. Result visualization: generate a bar chart to show the evaluation indicators of different models to help intuitively understand the performance of each model.

# In summary, the script can provide users with the effects of multiple regression methods in data prediction and help choose the best model.

import pandas as pd
import numpy as np
from sklearn.model_selection import train_test_split
from sklearn.linear_model import Ridge
from sklearn.ensemble import RandomForestRegressor
from xgboost import XGBRegressor
from sklearn.svm import SVR
from sklearn.preprocessing import StandardScaler, MinMaxScaler
from sklearn.feature_selection import SelectKBest, f_regression
from sklearn.metrics import mean_absolute_error, mean_squared_error, r2_score
from tensorflow.keras.models import Sequential
from tensorflow.keras.layers import Dense, Dropout
from tensorflow.keras.callbacks import EarlyStopping
import matplotlib.pyplot as plt
import warnings

# Suppress warnings for cleaner output
warnings.filterwarnings("ignore")


def preprocess_data(data, target_col, exclude_cols):
    """
    Prepare dataset: handle missing values, apply transformations, and scale features.
    Returns:
        X_scaled: Standardized feature matrix
        y_scaled: MinMax-scaled target values
        features: List of feature names
        scaler_y: Target scaler for inverse transformations
    """
    # Validate target column exists
    if target_col not in data.columns:
        raise ValueError(f"Target column '{target_col}' not found in dataset")

    # Handle missing values
    data = data.replace(0, np.nan).dropna()

    # Define features and target
    features = [col for col in data.columns if col not in exclude_cols + [target_col]]
    if not features:
        raise ValueError("No features available after column exclusion")

    X = data[features]
    y = data[target_col]

    # Standardize features
    scaler_x = StandardScaler()
    X_scaled = scaler_x.fit_transform(X)

    # Scale target variable
    scaler_y = MinMaxScaler()
    y_scaled = scaler_y.fit_transform(y.values.reshape(-1, 1)).flatten()

    return X_scaled, y_scaled, features, scaler_y


def train_and_evaluate(model, X_train, X_test, y_train, y_test, model_name, results, scaler_y=None):
    """
    Train and evaluate a regression model, storing performance metrics.
    Handles both sklearn and Keras models.
    """
    # Handle Keras models
    if 'keras' in str(type(model)):
        model.fit(X_train, y_train, verbose=0)
        y_pred = model.predict(X_test).flatten()
    # Handle sklearn models
    else:
        model.fit(X_train, y_train)
        y_pred = model.predict(X_test)

    # Inverse transform if scaler provided
    if scaler_y:
        y_pred = scaler_y.inverse_transform(y_pred.reshape(-1, 1)).flatten()
        y_test = scaler_y.inverse_transform(y_test.reshape(-1, 1)).flatten()

    # Calculate metrics
    mae = mean_absolute_error(y_test, y_pred)
    rmse = np.sqrt(mean_squared_error(y_test, y_pred))
    r2 = r2_score(y_test, y_pred)

    # Store results
    results[model_name] = {'MAE': mae, 'RMSE': rmse, 'R2': r2}
    print(f"{model_name}: MAE={mae:.3f}, RMSE={rmse:.3f}, R2={r2:.3f}")


def smooth_target(data, target_col, window_size=3):
    """
    Apply moving average smoothing to target variable.
    Returns dataframe with added smoothed column.
    """
    smoothed_col = f"{target_col}_smoothed"
    data[smoothed_col] = (
        data[target_col]
        .rolling(window=window_size, center=True, min_periods=1)
        .mean()
    )
    return data


def build_ann(input_dim):
    """Construct and compile a neural network regression model"""
    model = Sequential([
        Dense(64, activation='relu', input_dim=input_dim),
        Dropout(0.3),
        Dense(32, activation='relu'),
        Dense(1)
    ])
    model.compile(optimizer='adam', loss='mse')
    return model


def visualize_performance(results):
    """Create bar chart comparing model performance metrics"""
    results_df = pd.DataFrame(results).T
    results_df.index.name = 'Model'

    # Plot metrics
    results_df.plot(kind='bar', figsize=(10, 6))
    plt.title('Model Performance Comparison')
    plt.ylabel('Score')
    plt.xticks(rotation=45)
    plt.tight_layout()
    plt.show()

    return results_df


if __name__ == "__main__":
    # Load and prepare data
    data = pd.read_csv('percentage_small_1215.csv')

    # Apply smoothing to target variable
    data = smooth_target(data, 'Percentage_of_small', window_size=5)

    # Transform target variable
    data['Percentage_transformed'] = np.log1p(data['Percentage_of_small_smoothed'])

    # Preprocess data
    exclude_cols = ['BOD5', 'Chl_a', 'Percentage_of_small', 'Percentage_of_small_smoothed']
    X, y, features, target_scaler = preprocess_data(
        data,
        target_col='Percentage_transformed',
        exclude_cols=exclude_cols
    )

    # Feature selection
    selector = SelectKBest(score_func=f_regression, k=10)
    X_selected = selector.fit_transform(X, y)
    selected_features = [features[i] for i in selector.get_support(indices=True)]
    print(f"Selected features: {selected_features}")

    # Train-test split
    X_train, X_test, y_train, y_test = train_test_split(
        X_selected, y, test_size=0.2, random_state=42
    )

    # Initialize results storage
    results = {}

    # Define models
    models = {
        "Ridge Regression": Ridge(alpha=1.0),
        "Random Forest": RandomForestRegressor(
            n_estimators=150, max_depth=10, random_state=42, n_jobs=-1
        ),
        "XGBoost": XGBRegressor(
            n_estimators=150, max_depth=10, learning_rate=0.05, random_state=42, n_jobs=-1
        ),
        "SVM": SVR(kernel='rbf', C=5.0, epsilon=0.05)
    }

    # Add ANN model
    ann = build_ann(X_train.shape[1])
    early_stop = EarlyStopping(monitor='val_loss', patience=5, restore_best_weights=True)
    ann.fit(
        X_train, y_train,
        validation_data=(X_test, y_test),
        epochs=50,
        batch_size=16,
        callbacks=[early_stop],
        verbose=0
    )
    models["ANN"] = ann

    # Train and evaluate all models
    for name, model in models.items():
        train_and_evaluate(
            model, X_train, X_test, y_train, y_test,
            name, results, scaler_y=target_scaler
        )

    # Visualize and report results
    results_df = visualize_performance(results)
    print("\nModel Performance Summary:")
    print(results_df)