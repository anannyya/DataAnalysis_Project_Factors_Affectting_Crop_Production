# 🌾 Crop Yield Prediction for Central and Southern Asia 🌾

![FAO Logo](https://www.fao.org/fileadmin/templates/family-farming-decade/images/fao-logo-en.png)
![World Bank Logo](https://upload.wikimedia.org/wikipedia/commons/0/04/World_Bank_logo.svg)

## Project Overview 📊

This project focuses on understanding and predicting crop yields for wheat, rice, and potatoes in Central and Southern Asia. The analysis leverages economic data from the World Bank and environmental data from the Climate Change Knowledge Portal to provide insights and models that can help improve food security in the region.

## Data Collection and Preprocessing 📂

### Collected Data

1. **Economic Factors** from the World Development Index (World Bank)
2. **Crop Data** from the Food and Agriculture Organization (FAO)
3. **Temperature Data** from the Climate Change Knowledge Portal (CCKP)
4. **Precipitation Data** from the Climate Change Knowledge Portal (CCKP)

The data spans from 1960 to 2021 and covers 12 countries: India, Pakistan, Sri Lanka, Maldives, Bangladesh, Tajikistan, Turkmenistan, Uzbekistan, Kyrgyz Republic, Kazakhstan, and Nepal.

### Data Preprocessing 🛠️

1. **Missing Value Treatment**: Removed attributes with more than 30% missing values.
2. **Interpolation**: Used to estimate missing values for economic factors and crop yields.
3. **Outlier Removal**: Applied the IQR method to eliminate outliers.

## Project Structure 🗂️

- **Collected Data**: Contains raw data files.
- **Pre-processed Data**: Includes cleaned and interpolated CSV files.
- **Crop_production.R**: Main R script for data analysis and model training.
- **Poster.pdf**: Project poster summarizing the key findings.
- **Visualization**: Folder containing all the charts and visualizations.

## Analysis and Modeling 🧪

### Exploratory Data Analysis (EDA) 🔍

Performed EDA to understand the relationships and distributions of the variables. Key steps included visualizing trends, identifying outliers, and calculating correlations.

### Feature Engineering 🧩

Created new features and selected the most significant ones using statistical tests and domain knowledge.

### Modeling 📈

Trained and validated multiple regression models to predict crop yields:

1. **Multiple Linear Regression**
2. **Decision Tree**
3. **Random Forest**
4. **Polynomial Regression**

### Model Performance 🌟

- **Random Forest** was found to be the most accurate model with high predictive performance across all crops.

## Results and Insights 💡

- **Economic Factors**: GDP per capita and agricultural investments showed strong correlations with crop yields.
- **Environmental Factors**: Precipitation and temperature significantly impacted yield variations.
- **Model Accuracy**: The Random Forest model outperformed other models, achieving high accuracy in predicting yields for wheat, rice, and potatoes.
- **Policy Recommendations**: The findings suggest that improving economic conditions and managing environmental factors can significantly enhance crop production.

## How to Use 🚀

1. Clone the repository:
    ```bash
    [git clone https://github.com/anannyya/FAO-Crop-Yield-Prediction.git
    ```
2. Run the R script for analysis:
    ```bash
    Rscript Crop_production.R
    ```

## Conclusion

This project aligns with the UN's Zero Hunger goal by providing actionable insights to improve food security in Central and Southern Asia. The combination of economic and environmental data offers a comprehensive understanding of the factors affecting crop yields.


![UN Zero Hunger Logo](https://www.un.org/sites/un2.un.org/files/sdg_2_zero_hunger.png)
