# Liver-Disease-Research-Project

# Overview

This project focuses on analyzing the dynamics of liver disease and predicting its severity using advanced statistical and machine learning models. By exploring relationships between demographic, biochemical, and medical data, this project aims to uncover key factors contributing to liver disease and create a robust prediction model.

The dataset, provided as part of the STAT515 coursework, includes information about enzyme levels, biochemical indicators, age, and gender. This project combines statistical techniques like ANOVA with predictive modeling approaches, achieving high accuracy and interpretability.

Objective
	1.	Investigate how demographic factors like age and gender influence liver disease.
	2.	Analyze trends and variance in biochemical responses using statistical methods.
	3.	Build predictive models to classify and predict the severity of liver disease.

# Dataset

Features
	•	Age: Age of the patient.
	•	Gender: Gender of the patient (Male/Female).
	•	Enzyme Levels: Includes ALT, AST, ALP, and other liver enzymes.
	•	Biochemical Responses: Measures like bilirubin, albumin, and total proteins.
	•	Target Variable: Binary or multiclass target indicating the presence or severity of liver disease.


# Methodology

1. Exploratory Data Analysis (EDA)
	•	Descriptive Statistics:
	•	Summary statistics for age, enzyme levels, and biochemical indicators.
	•	Trend Analysis:
	•	Examined age and gender distributions and their correlation with liver disease.
	•	Visualizations:
	•	Generated boxplots, histograms, and scatterplots to understand feature distributions.

2. Statistical Analysis
	•	ANOVA:
	•	Conducted analysis of variance to determine if enzyme levels differ significantly across disease severity levels.
	•	Regression Analysis:
	•	Built linear and multiple regression models to quantify the relationship between features and liver enzyme levels.

3. Predictive Modeling
	•	Algorithms Used:
	•	Random Forest
	•	Multinomial Logistic Regression
	•	Evaluation Metrics:
	•	Area Under the Curve (AUC), Precision, Recall, F1-Score
	•	Performance:
	•	Achieved high accuracy, with AUC scores up to 0.99 for Random Forest models.

# Results

Key Insights
	1.	Demographics:
	•	Older age groups showed higher enzyme levels, indicating increased liver dysfunction.
	•	Gender differences were significant in certain enzyme levels, with males generally exhibiting higher levels.
	2.	Biochemical Trends:
	•	High bilirubin levels were strongly correlated with severe liver disease.
	•	Albumin levels showed an inverse relationship with disease severity.

Model Performance

Model	Accuracy	AUC
Random Forest	0.97	0.99
Multinomial Logistic Regression	0.95	0.98

# Tools and Technologies
	•	Programming Languages: R
	•	Libraries Used: caret, randomForest, ggplot2, dplyr
	•	Statistical Techniques: ANOVA, Regression Analysis

# Future Work
	1.	Expand the dataset to include additional features like patient history and lifestyle factors.
	2.	Incorporate deep learning models for enhanced prediction accuracy.
	3.	Explore SHAP values for feature interpretability in complex models.
