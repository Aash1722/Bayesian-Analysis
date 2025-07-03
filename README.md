# Bayesian Analysis of University Admission Data

---

## 📌 Project Overview

This project applies **Bayesian inference** to model and analyze university admission data. Using data on GRE scores, TOEFL scores, and CGPA from applicants, we investigate the suitability of the **normal distribution** as a data-generating model and compare different **prior distributions** to derive posterior and posterior predictive distributions. The work is centered around probabilistic modeling of academic attributes, with credible intervals used to evaluate predictive accuracy.

---

## 🎯 Objective

- To identify the most suitable **data-generating model** for predicting university admission attributes.
- To **compare posterior predictive performance** using different prior assumptions.
- To quantify uncertainty using **95% credible intervals** and assess how well predictions cover test data.

---

## 📊 Dataset Description

- **Source**: Kaggle (University Admission Dataset)
- **Sample Size**: 400 students
- **Selected Features**:
  - `GRE Score` (out of 340) – Continuous
  - `TOEFL Score` (out of 120) – Continuous
  - `CGPA` (out of 10) – Continuous

---

## 🧠 Methodology

### 📌 Preprocessing
- Checked for missing values – none found
- Split into 75% training and 25% testing data
- Stratified sampling used to preserve distribution

### 📊 Descriptive Analysis
- Computed mean, median, standard deviation for selected features
- Used **density plots** to confirm approximate symmetry in distributions

### 📈 Bayesian Modeling – Approach 1
- Assumed normal likelihood:  
  `X₁:ₙ | μ ~ N(μ, σ²)`  
- Compared two types of priors:  
  1. Informative prior: `μ ~ N(μ₁, σ₁²)`  
  2. Non-informative flat prior: `P(μ) ∝ 1`

- Posterior and posterior predictive distributions derived analytically
- Calculated **95% credible intervals** for test predictions
- Counted number of test values that fell within the credible intervals

---

## 📊 Results

### 🔍 Summary Statistics

| Feature      | Mean   | Median | Std. Dev |
|--------------|--------|--------|----------|
| CGPA         | 8.625  | 8.640  | 0.584    |
| GRE Score    | 317.2  | 317.5  | 11.408   |
| TOEFL Score  | 107.7  | 108.0  | 6.014    |

All distributions appear symmetric based on density plots.

### 📈 Predictive Performance

- Posterior predictive distributions were plotted for each feature under multiple prior configurations.
- Across all prior settings, the number of test samples inside the **95% credible interval** remained consistent.
- This suggests that a **normal distribution is a good fit** for modeling GRE, TOEFL, and CGPA.

---

## 📦 Repository Structure

bayesian-admission-analysis/
├── data/ # Dataset file (CSV)
├── notebooks/ # Jupyter Notebooks or Rmd files for analysis
├── results/ # Graphs and posterior plots
├── report/ # Final PDF report
├── README.md # Project summary (this file)

---

## 📚 References

- Kaggle Dataset: [University Admission](https://www.kaggle.com/mohansacharya/graduate-admissions)
- Bayesian Theory and Methods:
  - Gelman et al. (Bayesian Data Analysis)
  - Hoff, P. D. (A First Course in Bayesian Statistical Methods)
  - Lecture notes and course content from STAT 619

---

## 📈 Key Takeaways

- **Flat priors** (uninformative) and **informative priors** yielded similar predictive coverage.
- **Normal distribution** was consistently adequate for modeling admission metrics.
- Bayesian methods provided interpretable predictions and credible intervals with theoretical support.

---

## 🧪 Tools Used

- Python (NumPy, SciPy, Matplotlib, Pandas)
- Jupyter Notebook / R Markdown
- LaTeX (for typesetting equations and report)


