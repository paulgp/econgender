
import requests  
r = requests.get('https://www.aeaweb.org/issues/338')

from bs4 import BeautifulSoup  
soup = BeautifulSoup(r.text, 'html.parser')

results = soup.find_all('h3', attrs={'class':'title'})

new = results[5:103]

records = []  
for result in new:  
    paper_name = result.find('a').text
    author_name = result.find('span').text
    records.append((paper_name,author_name))

print(records)


import pandas as pd  
df = pd.DataFrame(records, columns=['paper_title', 'author'])  
df.to_csv('2014_P&P_Papers.csv', index=False, encoding='utf-8') 
