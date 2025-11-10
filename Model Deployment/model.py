import pandas as pd
import numpy as np
from sklearn import linear_model
#from sklearn.externals import joblib
import joblib
data = pd.read_csv("C:/Users/DS-31/Downloads/IRIS.csv")
print(data.head())
X = data[['sepal_length', 'sepal_width', 'petal_length', 'petal_width']]
Y = data['species']
clf = linear_model.SGDClassifier(max_iter=1000, tol=1e-3)
clf.fit(X, Y)
joblib.dump(clf, 'model.pkl')
