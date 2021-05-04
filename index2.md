---
title: Supplementary Python script for Successful mobilisation strategies: Increasing low-cost participation to engender civil society in hybrid regimes
output:
  html_document:
    keep_md: true
---
This Python script shows how the topic of e-petitions were extracted. Before approaching the topic modeling, we need to clean the texts of e-petitions.
# Text cleaning

```py
# defying the functions
import re
import string
import numpy as np
import pandas as pd
    
# Russian stopwords
# !pip install nltk
import nltk
nltk.download("stopwords")
from nltk.corpus import stopwords
russian_stopwords = stopwords.words("russian") # loading Russian stopwords
print(russian_stopwords)

def remove_stop_words(sentence):
    return ' '.join([word for word in sentence.split() if word not in russian_stopwords])

def remove_short(sentence):
    return ' '.join([word for word in sentence.split() if len(word) >= 3])
    
def remove_digits(sentence):
    return ' '.join([i for i in sentence.split() if not i.isdigit()])
    
def preprocess(all_texts):
    all_texts = list(map(lambda x: x.lower(), all_texts))#lowercase
    all_texts = list(map(lambda x: x.translate(str.maketrans('', '', string.punctuation)), all_texts))# remove punctuation
    all_texts = list(map(lambda x: x.strip(), all_texts))#remove extra spaces
    all_texts = list(map(lambda x: remove_stop_words(x), all_texts))# removing stopwords
    all_texts = list(map(lambda x: remove_digits(x), all_texts))#removing digits
    all_texts = list(map(lambda x: re.sub("[0-9]+", "", x), all_texts))#removing digits
    all_texts = list(map(lambda x: re.sub("[a-zA-Z]+", "", x), all_texts))#removing english
    all_texts = list(map(lambda x: re.sub("«", "", x), all_texts))#removing digits
    all_texts = list(map(lambda x: re.sub("»", "", x), all_texts))#removing english
    all_texts = list(map(lambda x: re.sub("  ", " ", x), all_texts))#removing english
    all_texts = list(map(lambda x: remove_short(x), all_texts))#removing short words
    return all_texts 
```
```py
# the following function creates the objects needed for the topic modeling, i.e., the corpus and id2word
#!pip3 install rnnmorph
from rnnmorph.predictor import RNNMorphPredictor
predictor = RNNMorphPredictor(language="ru")

import gensim.corpora as corpora

def create_list_of_words(all_texts):
    all_texts= preprocess(all_texts)# applying the function of preprocessing to the text
    l_text2=[]
    for doc in all_texts:
          list1=doc.split(" ")
          l_text2.append(list1)
    
    # choosing only the nouns
    corp=[]
    for j in range(0,len(l_text2)):
        if len(l_text2[j])>1:
            forms = predictor.predict(l_text2[j])
            text=[]
            for i in range(len(forms)):
                if forms[i].pos=="NOUN":
                    text.append(forms[i].normal_form)
            corp.append(text)
        else:
            corp.append([''])
    
    # Create Dictionary
    id2word = corpora.Dictionary(corp)

    # Create Corpus
    texts = corp

    # Term Document Frequency
    corpus = [id2word.doc2bow(text) for text in texts]
    
    return corp,id2word,texts,corpus
```

```py
#now, we can clean both datasets
#ROI
df_roi=pd.read_json('roi_data.json')
res1,id2word1,texts1,corpus1=create_list_of_words(df_roi.Text)

#Change.org
df_ch=pd.read_json('ch_data.json')
res2,id2word2,texts2,corpus2=create_list_of_words(df_ch.Text)
```
