# 🧬 ODNOM Diagnostics: Multiclass Elastic Net

*Clinical Diagnostic & Biomarker Analysis Platform (Optimized for Flow Cytometry)*

**ODNOM Diagnostics** is an advanced, production-ready R Shiny application tailored for clinical biomarker discovery and multiclass patient classification. It seamlessly integrates automated feature engineering, regularized multinomial logistic regression, conformal prediction safety guardrails, and spatial anomaly detection into a single, cohesive workflow.

---

## ✨ Core Capabilities

*   **Leakage-Free Partitioning:** Enforces a strict 80/20 split, completely isolating the training cohort (80%) from the calibration cohort (20%).
*   **Automated Feature Engineering:** Autonomously generates complex, non-linear derived features such as ratios, differences, products, and log-ratios.
*   **Dynamic Intra-Fold ANOVA Filter:** Performs rigorous feature selection driven by a False Discovery Rate (FDR < 0.05), applied strictly within cross-validation folds to prevent data leakage.
*   **Elastic Net Multinomial Regression:** Utilizes simultaneous L1 (Lasso) and L2 (Ridge) penalization to optimize multiclass classification.
*   **Split Conformal Prediction:** Quantifies uncertainty in real-time by leveraging the isolated 20% calibration set to issue clinical safety warnings.
*   **Spatial Anomaly Detection:** Combines leakage-free UMAP projections with HDBSCAN density clustering and Isolation Forest outlier detection to visually map patient heterogeneity.

---

## 🚀 Installation & Quick Start

### Prerequisites
Ensure that **R (≥ 4.0.0)** and **RStudio** are installed on your system.

### 1. Clone the Repository
```bash
git clone https://github.com/your-username/ODNOM-Diagnostics.git
cd ODNOM-Diagnostics
```

### 2. Launch the Application
Open `app.R` in RStudio and click **Run App**, or execute the following command directly in your R console:
```R
# Automatically installs required packages and launches the app
shiny::runApp()
```

---

## 📊 Data Requirements & Formatting

Your input dataset must be provided in `.csv` or `.xlsx` format and adhere to the following structural rules:

*   **Row/Column Structure:** Each row must represent a unique patient, while each column represents a specific clinical variable or biomarker.
*   **Patient Identifier:** The file must contain a column dedicated to unique patient IDs (e.g., `Patient_ID`).
*   **Target/Diagnosis Column:** A categorical column defining the target clinical classes (e.g., *Healthy*, *Leukemia*, *Lymphoma*) is required. 
    *   *Note:* Any diagnostic class containing fewer than 5 patients will be automatically filtered out to ensure the statistical validity of the 5-fold cross-validation.
*   **Biomarker Columns:** All biomarker inputs must be strictly numeric. Any missing values (empty cells) will be automatically handled via median imputation by the application.

---

## 🧠 Algorithmic Architecture & Pipeline

The ODNOM platform operates on a strictly disciplined machine learning pipeline designed to eliminate data leakage and ensure clinical robustness.

### Pipeline Flowchart
```text
[Initial Patient Cohort: 100%]
   │
   ├──> 20% Calibration Vault (Strictly Isolated)
   │
   └──> 80% Training Set
          │
          ├── 1. Nested Cross-Validation (5 Folds)
          │      ↳ Selects optimal Alpha (α) & Lambda (λ) by averaging fold scores
          │
          ├── 2. Out-Of-Fold (OOF) Evaluation
          │      ↳ Generates ROC/AUC metrics from blind hold-out fold tests
          │
          └── 3. Final Model Retraining
                 ↳ Retrains a single model on the FULL 80% set using optimal α/λ
                 ↳ Permanently freezes Z-scores & linear coefficients
          │
          V
[Conformal Prediction Integration]
Applies the completely frozen model to the isolated 20% Calibration Vault
↳ Extracts the 95th percentile error threshold to serve as a clinical guardrail
```

### Step-by-Step Methodology

*   **Hyperparameter Tuning (α, λ):** Evaluated through a rigorous 5-fold cross-validation process. The optimal hyperparameter pair is selected by maximizing the average performance score across all 5 validation folds.
*   **Final Retraining & Freezing:** The algorithm trains one definitive model on the entire 80% training partition using the optimal hyperparameters discovered in the previous step. The resulting linear coefficients and global Z-scores are then permanently frozen.
*   **Out-Of-Fold (OOF) Metrics:** Performance visualizations, including ROC and Precision-Recall curves, are plotted strictly using blind predictions aggregated across the 5 validation folds.
*   **Conformal Safety Guardrail:** The fully frozen model is applied to the previously untouched 20% Calibration Set to determine the 95th percentile error threshold (q̂).
*   **New Patient Diagnostics:** When analyzing a new patient, the frozen model generates direct multiclass probabilities. If the prediction's uncertainty score (1 - max(P)) exceeds the established threshold q̂, the system automatically triggers an **Uncertainty Warning**.

---

## 🛡️ License, Citation & Disclaimer

**License:**  
This project is licensed and distributed under the **GNU General Public License (GPL)**.

**Citation Requirement:**  
If you utilize this code, methodology, or application in your research, publications, or projects, you are required to explicitly cite **Dr. Quentin AMIOT** and **Lucy LOCHER** as the authors.

**Disclaimer:**  
**Strictly for Research and Educational Purposes.** 
The software and the models generated by this tool must undergo rigorous, independent external validation prior to any clinical or diagnostic application.
