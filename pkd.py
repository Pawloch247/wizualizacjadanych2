import pandas as pd
import json


pkd_json = ''
with open('pkd_mod.json', 'r', encoding='utf8') as pkd:
	pkd_json = pkd.read()

pkd = json.loads(pkd_json)


pkd_main = {}

for group in pkd:
	pkd_main[group] = pkd[group]['title']

df = pd.DataFrame(pkd_main.items(), columns=['PKDMainSection', 'PKDMainSectionTitle'])

df.to_csv('data/pkd_main.csv', index=False)
