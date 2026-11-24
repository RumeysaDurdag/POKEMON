# Pokémon Legendary Classification

An R-based machine-learning coursework project using Pokémon attributes to classify whether a Pokémon is legendary.

## Objective
Build a classification workflow from tabular Pokémon data, prepare incomplete categorical and numeric fields, train a decision tree, and generate predictions for a separate test dataset.

## Workflow
1. Load the training and test datasets.
2. Inspect data structure and missing values.
3. Handle missing names and secondary types.
4. Convert selected variables to categorical factors.
5. Impute selected feature columns with the missForest package.
6. Split the labeled training data into training and validation partitions.
7. Train a classification decision tree with rpart.
8. Evaluate validation predictions with a confusion matrix.
9. Generate predictions for the separate test dataset and export them to competition-412.csv.

## Tools
- **Language:** R
- **Data manipulation:** dplyr, tidyverse
- **Imputation:** missForest
- **Modeling:** rpart, rpart.plot
- **Evaluation:** caret

## Repository contents
| File | Description |
|---|---|
| kaggle_competition.R | Data preparation, imputation, decision-tree training, evaluation, and prediction |
| train.csv | Labeled training data |
| test.csv | Unlabeled test data |
| competition-412.csv | Exported prediction file |

## Run locally
1. Install R and the packages listed above.
2. Clone this repository and open the project directory in RStudio.
3. Run kaggle_competition.R from the repository root.

The script expects train.csv and test.csv in the working directory. The project is an academic exercise; the reported workflow should be rerun and its validation results reviewed before using it as a benchmark.

## Potential improvements
- Use a stratified split and a fixed random seed for reproducible validation.
- Compare the decision tree with baseline and ensemble classifiers.
- Evaluate precision, recall, F1-score, and ROC-AUC in addition to the confusion matrix.
- Fit preprocessing steps on training data only and apply the fitted transformations to validation and test data.
