-- creating the table to import the florida insurance data
CREATE TABLE florida(
"policyID" INTEGER,
"statecode" CHAR,
"county" CHAR,
"eq_site_limit" REAL,
"hu_site_limit" REAL,
fl_site_limit" REAL,
"fr_site_limit" REAL,
"tiv_2011" REAL,
"tiv_2012" REAL,
"eq_site_deductible" INTEGER,
"hu_site_deductible" INTEGER,
"fl_site_deductible" INTEGER,
"fr_site_deductible" INTEGER,
"point_latitude" REAL,
"point_longitude" REAL,
"line" CHAR,
"construction" CHAR,
"point_granularity" INTEGER
);

--reading the data into the table that I created
.mode csv
.import DScourseS20/ProblemSets/PS3 florida;

--printing the first 10 rows of the data set
SELECT * FROM florida LIMIT 10;

--listing unique county variables
SELECT county, COUNT(*) FROM florida GROUP BY county;

--calculatig the average property appreciation from 2011 to 2012
SELECT AVG(tiv_2012 - tiv_2011) FROM florida;

--showing frequency of each type of construction material
SELECT construction, COUNT(*) FROM florida GROUP BY construction;

