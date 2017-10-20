import pandas as pd
import requests
from credentials import *
from sqlalchemy import create_engine
from sqlalchemy.types import Integer

engine = create_engine('postgresql://{}:{}@{}:{}/{}'.format(user, password, host, port, dbname))


def get_json(year, offset_num):
	'''
	Call City of Chicago API for Crimes committed for given year and offset number

	Outputs: list of dictionaries
	'''
	url = "https://data.cityofchicago.org/resource/6zsd-86xi.json?&year={}&$offset={}".format(year, str(offset_num))
	r = requests.get(url)
	return r.json()

def process_year(year):
	'''
	Load crimes into db for given year
	'''
	print('processing year ', year)
	results = [1]
	offset_num = 0
	while len(results) > 0:
		results = get_json(year, offset_num)
		if len(results) > 0:
			df = pd.DataFrame(results)
			print('processing offset ', offset_num, ', df shape ', df.shape)
			del df['location']
			df.to_sql('crimes', engine, if_exists='append', index=False)
			offset_num += 1000

for year in range(2011, 2018):
	process_year(year)

