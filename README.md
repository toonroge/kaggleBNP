These are just some ideas. Feel free to change/update.

### STAGE 1:

Set up a shiny app for a decent exploratory analysis.
This app should proof useful during the rest of the competition.
It should be possible to reuse this app for other competitions.
Elements that should be included:  

* Exploratory numeric features
* Exploratory categorical features
* Analysis of correlations and relations between features (nice analysis on https://www.kaggle.com/c/bnp-paribas-cardif-claims-management/forums/t/19240/analysis-of-duplicate-variables-correlated-variables-large-post)
* Analysis of missing values
* One way analysis of numeric on the response
* One way analysis of categorical on the response
* Effect of missing values on the response
* PCA or clustering??

### STAGE 2:

Based on the exploratory analysis should be able to create some features:  

* Use ridge regression on hold-out sample, next predict on whole dataset = easy and good way to convert categorical features in numeric
* + test other techniques Owen Zhang (check his ppt)
* One hot encoding...
* flagging outliers (anomaly detection?)
* PCA results
* clustering results
* missing values

### STAGE 3:

Fit different models, not necessarily the best models but different models.
Simple decision trees, ridge, gam, lasso, randomforest, extermely randomized tree, knn, xgb, glm ...
With different parameters...

### STAGE 4:

Analyse the correlations between the models + their performance,
build charts to compare model performance,
check if some models perform better on other subspaces of the data as others
This stage might generate new ideas ...

### STAGE 5

Based on the previous interpretations, leverage different models in ensembles and see if and why we are able to inprove our predictions  

* We can add in some of the features we created before.
* Which models perform best on the outlier data?
* Which models perform best on data with lots of missing values?
* Some models better for some clusters as other models?
* Add this new features + model predictions (+ original variables) in new blend,
probably models that are good in finding interactions...