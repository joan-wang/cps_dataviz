# crimes table: set up geom for joining
ALTER TABLE crimes
ALTER COLUMN longitude TYPE numeric USING longitude::numeric,
ALTER COLUMN latitude TYPE numeric USING latitude::numeric;
ALTER TABLE crimes ADD COLUMN geom geometry(Point, 4269);
UPDATE crimes SET geom = ST_SetSRID(ST_MakePoint(crimes.longitude, crimes.latitude), 4269);
CREATE INDEX ON crimes (id);
CREATE INDEX spatial_index_crimes ON crimes USING GIST (geom);

# schools table: set up geom for joining
ALTER TABLE schools_1617 ADD COLUMN geom geometry(Point, 4269);
UPDATE schools_1617 SET geom = ST_SetSRID(ST_MakePoint(schools_1617."School_Longitude", schools_1617."School_Latitude"), 4269);
CREATE INDEX ON schools_1617 ("School_ID");
CREATE INDEX spatial_index_schools ON schools_1617 USING GIST (geom);

# join schools with number of crimes within 1 mile radius
# approximately 1609.34 meters in a mille
CREATE TABLE schools_crimes AS 
SELECT s."School_ID", count(crimes.id) as num_crimes_16
FROM schools_1617 as s
LEFT JOIN crimes 
ON ST_DWithin(ST_Transform(s.geom, 2163), ST_Transform(crimes.geom, 2163), 1609.34) 
WHERE crimes.year = '2016'
GROUP BY s."School_ID";

