
## The goal of this script is to visualize the relationship between phosphate (PO4) concentration, temperature (Temp), and %Small in an environmental dataset.

# The specific steps are as follows:
# 1. Data loading and preprocessing: Load data from a CSV file and remove rows with missing values ​​to ensure data integrity.
# 2. Data selection: Extract three variables, PO4, Temp, and %Small, for subsequent visualization analysis.
# 3. Create a grid: Based on the PO4 and temperature values, create a grid for interpolation to ensure smooth distribution of data in space.
# 4. Interpolation calculation: Use `griddata` to interpolate to calculate the value of %Small on the grid.
# 5. 3D visualization: Use a 3D scatter plot to show the relationship between PO4 concentration, temperature, and %Small, and color the data points according to the value of the percentage of organisms to observe potential patterns and trends.


# This script provides an intuitive 3D visualization to help analyze the effects of different environmental factors (such as PO4 concentration and temperature) on %Small and reveal potential relationships and trends.


import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from scipy.interpolate import griddata
from mpl_toolkits.mplot3d import Axes3D

# Load dataset
data = pd.read_csv('percentage_small_1215.csv')

# Prepare data for visualization
valid_data = data[['PO4', 'Temp', 'Percentage_of_small']].dropna()
po4 = valid_data['PO4']
temp = valid_data['Temp']
percent_small = valid_data['Percentage_of_small']

# Create grid for surface interpolation
po4_grid = np.linspace(po4.min(), po4.max(), 50)
temp_grid = np.linspace(temp.min(), temp.max(), 50)
po4_mesh, temp_mesh = np.meshgrid(po4_grid, temp_grid)

# Interpolate values on grid
percent_interp = griddata(
    (po4, temp),
    percent_small,
    (po4_mesh, temp_mesh),
    method='linear'
)

# Create 3D visualization
fig = plt.figure(figsize=(12, 8))
ax = fig.add_subplot(111, projection='3d')

# Plot data points
scatter = ax.scatter(
    po4,
    temp,
    percent_small,
    c=percent_small,
    cmap='coolwarm',
    s=15,
    alpha=0.6
)

# Configure plot
ax.set_xlabel('PO4 Concentration')
ax.set_ylabel('Temperature (°C)')
ax.set_zlabel('Percentage of Small Organisms')
plt.title("Relationship Between PO4, Temperature, and Organism Size Distribution")

plt.show()