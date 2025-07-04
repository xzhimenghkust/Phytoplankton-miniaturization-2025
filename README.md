# Phytoplankton-miniaturization-2025

This depository contains all the codes and dataset included in our manuscript entitled "Decadal scale phytoplankton miniaturization in subtropical coastal waters" (Xu et al., 2025).
Since the MS hasn't been published, we are still updating the files in this folder.
For any related questions, please contact us by email: zxube@connect.ust.hk




Scripts for machine learning models (by Python):
pca_%small.py => to perform principal component analysis (PCA) on the features in the environmental dataset to achieve data dimensionality reduction and analyze the contribution of each feature in the principal component.

model_competition.py => to train and evaluate multiple regression models and compare their performance in predicting the transformed value of %Small.

shap_RF => to model the data using a random forest regression model and explain the feature importance of the model predictions through SHAP analysis. 

shap_XGBoost.py => to train an XGBoost regression model and analyze its feature importance, using SHAP values ​​for model interpretation.

causial.py => to estimate the average treatment effect (ATE) and individual treatment effect (ITE) between multiple variables by using the Causal Forest method (i.e., between phosphate, temperature and %Small).

inter_eff.py => to visualize the relationship between phosphate, temperature, and %Small in an environmental dataset.

small_pure.py => to analyze the effect of phosphate on %Small, and use the mixed effects model to fit and analyze the data of each observation station.

test_glmm.py =>


