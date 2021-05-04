---
title: Supplementary Python script for Successful mobilisation strategies: Increasing low-cost participation to engender civil society in hybrid regimes
output:
  html_document:
    keep_md: true
---

```py
# defying the functions
import re
import string
    
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
