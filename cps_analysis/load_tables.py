import pandas as pd
from credentials import *
from sqlalchemy import create_engine
import psycopg2

engine = create_engine('postgresql://{}:{}@{}:{}/{}'.format(user, password, host, port, dbname))

# load profiles csv to db
profile = pd.read_csv('../cps_data/Profile_1617.csv')
try: 
	profile.to_sql('schools_1617', engine, index=False, if_exists = 'fail')
except:
	pass


# pull spatially-joined schools+crimes table from db
conn = psycopg2.connect("dbname={} user={} host={} password={} port={}".format(dbname, user, host, password, port))
cur = conn.cursor()
s = "select * from schools_crimes;"
cur.execute(s)
results = cur.fetchall()
colnames = [desc[0] for desc in cur.description]
cur.close()
schools_crimes = pd.DataFrame(results, columns=colnames)
schools_crimes.to_csv('schools_crimes.csv')