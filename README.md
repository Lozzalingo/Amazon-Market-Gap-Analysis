# Product Market Gap Analysis

## Overview
This project identifies market gaps in Amazon's **Toys and Games** category using product ratings data. By leveraging machine learning techniques, it predicts customer preferences and highlights product opportunities for entrepreneurs and sellers. 

## Objectives
- Analyse customer reviews and product ratings to identify untapped market opportunities.
- Develop predictive models to recommend products based on user behaviour.
- Use latent factors to group products into meaningful categories for better insights.

---

## Features
1. **Data Exploration**
   - Analysed 1.7 million rows of Amazon review data.
   - Cleaned and prepared data by removing missing titles and formatting fields for analysis.

2. **Predictive Modelling**
   - Implemented models, including:
     - Linear Regression (baseline comparisons)
     - Collaborative Filtering (user-based and item-based)
     - Matrix Factorisation
   - Evaluated models using Root Mean Squared Error (RMSE).

3. **Recommendation System**
   - Generated user-specific product recommendations.
   - Ranked products based on opportunity scores derived from purchase and recommendation frequency.

4. **Category Analysis**
   - Grouped products into latent factors
   - Used t-SNE grouping for visiualisation.
   - Used Llama3 to generate detailed, real-world product categories from most highly associated recommendation titles.

5. **Insights**
   - Highlighted top-performing products with potential market gaps.
   - Provided weighted scoring for product recommendations based on ranking, frequency, and saturation.

---

## Key Findings
- Identified products like **"Schleich Big Spell Set"** and **"Douglas Fox Plumpie"** as high-opportunity items.
- Revealed product clusters with significant potential for innovation or better marketing strategies.
- Highlighted limitations of Amazon's homogeneous rating system and sparsity challenges in the dataset.

---

## Requirements
- **Programming Language**: R
- **Libraries**: 
  - `Matrix.utils` (for sparse matrix handling)
  - `recommenderlab` (for recommendation engines)
  - `IRLBA` (for dimensionality reduction)
- **Data**: Amazon Toys and Games reviews dataset (2018)

---

## How to Use
1. **Data Preprocessing**:
   - Load and clean the dataset to remove inconsistencies.
   - Transform the data into a sparse matrix format for analysis.

2. **Model Training**:
   - Run linear regression and collaborative filtering models to benchmark performance.
   - Fine-tune matrix factorisation methods for latent factor extraction.

3. **Make Predictions**:
   - Generate user-specific recommendations and rank products by potential.

4. **Explore Insights**:
   - Analyse latent factors and product categories for actionable business insights.

---

## Future Improvements
- Incorporate real-time data to identify emerging trends.
- Address data biases caused by paid and fake reviews.
- Enhance the recommendation algorithm by considering multi-criteria rating scales.

---

## Author
**Laurence Stephan**  
Published: 6 December 2024  

Feel free to contribute to this project or raise issues for discussion.
