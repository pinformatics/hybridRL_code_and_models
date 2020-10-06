import pandas as pd
from gensim.models.doc2vec import Doc2Vec
from scipy.spatial.distance import cosine

fname_model = Doc2Vec.load("fname_epochs_640_vectorSize_30_window_2.model")
lname_model = Doc2Vec.load("lname_epochs_640_vectorSize_30_window_2.model")

data = pd.read_csv('all_NC_test.csv', encoding= 'unicode_escape')

data['fname_a'] = data['fname_a'].str.lower()
data['fname_b'] = data['fname_b'].str.lower()
data['lname_a'] = data['lname_a'].str.lower()
data['lname_b'] = data['lname_b'].str.lower()

data['fname2vec'] = 0.0
data['lname2vec'] = 0.0

for i in range(len(data)):
    print(i)
    data['fname2vec'][i] = cosine(fname_model.infer_vector(list(data['fname_a'][i])), fname_model.infer_vector(list(data['fname_b'][i])))
    data['lname2vec'][i] = cosine(lname_model.infer_vector(list(data['lname_a'][i])), lname_model.infer_vector(list(data['lname_b'][i])))
data.to_csv('all_NC_test_n2v.csv') 

