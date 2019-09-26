

import requests
import re
r = requests.get('https://www.aeaweb.org/issues/196')

from bs4 import BeautifulSoup  
soup = BeautifulSoup(r.text, 'html.parser')

results = soup.find_all('h3', attrs={'class':'title'})
results = soup.find_all('article', attrs={'class':'journal-article symposia-title'})


records = []
for result in results:
    session_title = result.text
    end_session = False
    for sibling in result.next_siblings:
        try:
            classes = sibling['class']
            match = False
            for obj in classes:
                if re.search('art_', obj):
                    match = True
                if re.search('symposia-title', obj):
                    end_session = True
        except TypeError:
            match = False
        if match is True:
            output = sibling.text.strip().split("\n\n")
            title = output[0]
            try: 
                authors = re.split(", |and ", output[1])
            except IndexError:
                authors = []
        if end_session is  True:
            break
    records.append((session_title, title, authors))  
        
print(records)
                                  
import pandas as pd  
df = pd.DataFrame(records, columns=['session_title','paper_title', 'author'])  
df.to_csv('eee.csv', index=False, encoding='utf-8') 
