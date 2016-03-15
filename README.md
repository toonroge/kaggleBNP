progress + ideas BNPParibasKaggle


### STAGE 1 EXPLORATORY ANALYSIS:

* https://toonroge.shinyapps.io/BNPParibasKaggle/
* Can't publish edits Selim, because it uses Axaml
* There are a lot of missing values, a lot of observations have only 44pct variables non-missing
* A lot of variables are heavily correlated (even (almost) linear dependency)
* Variable 50 is very strongly related to the target
* Some categorical variables have very high cardinality (example v22)
* Analysis of correlations and relations between features (nice analysis on https://www.kaggle.com/c/bnp-paribas-cardif-claims-management/forums/t/19240/analysis-of-duplicate-variables-correlated-variables-large-post)
* We can still add PCA/MCA


### STAGE 2 CREATE DB:

I already created a feature set:  

* https://docs.google.com/spreadsheets/d/1_F366lhjT60n4Rlu9kUlygV-H5OFdIJ4wJ6PxbUrDBg/edit?usp=sharing
* https://www.dropbox.com/sh/pqnkx38t8o4q6q6/AADctVZjtnB1ej2nQG5R6mREa?dl=0
* script is data_preparation.R

I think there are other options still to test here:  

* Autoencoding, PCA, MCA, PCA on clusters of correlated features ...
* Check if we can get more info out of missing values (observations with same missing pattern)
* flagging outliers
* ...


### STAGE 3 FIT MODEL:
 
Apparently the key to a good kaggle performance is (besides feature engineering) to make ensembles with different models, on different feature sets: http://mlwave.com/kaggle-ensembling-guide/

We can make different kind of models and store out-of-fold predictions (see stacking in the link above).
We can then use the predictions of these models as meta-features.

Since the key is to make different models I started with 2 regression models:  

* toon_baggingofGAMS_2.R (around 0.474)
* toon_elasticnet_01.R (around 0.471)

Other (stronger) models should be made + probably try to improve the feature engineering part:
I think interesting ideas might be:
* XGBoost (with different interaction depths)
* Extremely randomized trees (https://www.kaggle.com/chabir/bnp-paribas-cardif-claims-management/extratreesclassifier-score-0-45-v5/code)
* knn
* ...


### STAGE 4 ANALYSE DIFFERENCES AND COMBINE:

I think we can make some correlation graphs + double lift plot, to analyze differences of different models.
(Make small shiny app)


### STAGE 5 ENSEMBLE

Based on the previous interpretations, leverage different models in ensembles and see if and why we are able to inprove our predictions  

* Add predictions as meta-features + original features + new features (PCA, clustering, flagging outliers ...)

Goal is to find models performing better on certain subsets of the data as others and exploiting this.
