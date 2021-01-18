# Santander-Bank-Product-Analysis-and-Recommendation
This is a final project of Data Mining course @Uva. Santander Bank offers an array of financial services and products for customers. Target is to find methods to improve recommendation system. Dataset has 1.4 million samples, for 956 thousand customers in Santander Bank worldwide from January 2015 to May 2016. 

There are two main fearues we achieved:

#### User Analysis: 

Analyzed user behaviors patterns and evaluated existing product features by data visualization, exploring out customer preference and segment for specific products.

#### Data Modeling: 

Applied XGBoost and SVD based on preprocessing data, obtaining the most important feature. And Mean Average Precision @7 for SVD is ~0.001 higher than XGBoost.

## Data

Training and test data can be found at [here](https://www.kaggle.com/c/santander-product-recommendation/data).

### Orinigal data

* Total items: 1.4 million
* Total users: 956 thousand
* Total variables: 48