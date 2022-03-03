# NESS_SMA

Note (2022.03.03):
getSMACalibrators.py (notice the capital C, sorry!) is the Python version of the IDL script. 
Usage:
First, follow these instructions to generate the raw calibrator file from the SMA website:
```
1. Go to sma1.sma.hawaii.edu/callist/callist.html and copy the entire html table into a CSV file using
    some utility like the CopyTables extension for Firefox.
2. Open up the CSV and change the headers so there's one name per column. The ones I chose are:
    Common Name, Source Name, RAJ2000, DEJ2000, Band, Last Obs. Date, Obs., Flux Density (Jy) 30.0 day avg, Light curves
3. Find all instances of ± and replace with \pm.
4. Find all instances of mi and replace with mi.
5. Save as SMA_Calibrator_List_Raw.csv.
```
Once this is done:
```
from getSMACalibrators import *
out = getSMACalibrators(ID, RA, DEC[, radius_deg])
```
ID, RA, DEC are the arrays containing the unique identifiers, RAs, and DECs of your SMA targets. The `out` astropy table will contain three columns: the `ID` column that was input, and `cal_1` and `cal_2`. `cal_1` is the nearest calibrator within `radius_deg`. `cal_2` is the brightest calibrator within the same radius, only if it is different from `cal_1` (the column is otherwise masked).
You can write this to file using
```
out.write('SMA_calibrators.csv', format = 'csv', overwrite = True)
```
See the documentation in getSMACalibrators for details.



IDL scripts used to generate SMA target lists.
NOTE: you will have to update your IDLUTILS folder with scripts from [here](http://www.sdss3.org/dr8/software/idlutils_doc.php) if you encounter errors related to the BSORT or DJS_ANGLE_MATCH modules.

How to:
1) First, execute ```getSMAcalibrators.pro``` (perhaps in a different folder, or backup the previous output file if any). See the documentation for this script for how to, using ```doc_library, 'getSMAcalibrators'```.
The function calls ```REPSTR```, ```FORPRINT```, ```DJS_ANGLE_MATCH```, and ```FILE_TEST```. These should be part of your regular IDL distribution or included libraries (e.g., ```astron.dir/pro/misc/forprint.pro``` and ```idlutils/pro/coord/djs_angle_match.pro```). If not, you can install those libraries by searching for them.

2) Run the DOALL module in doall.pro.
Things that (might) need updating:
1. QA file (see existing QA sheet for format, etc.).

2. List of existing ALMA observations (see existing NESS_SMA_exObs.csv).

3. List of existing SMA observations. At the moment, the array is defined in the module ```prune_using_existing_SMA_obs``` as
```
psc = ['IRAS 03507+1115', 'IRAS 10131+3049', 'IRAS 19486+3247', 'IRAS 23558+5106'].
```
Just append any new PSCs that have been observed to this array.

4. List of non-AGB stars. At the moment, the array is defined in the module ```prune_non_AGB``` as
```
psc = ['IRAS 17441-2411', 'IRAS 19244+1115', 'IRAS 07209-2540', 'IRAS 05524+0723', 'IRAS 03030+5532']
```
Just append any new non-AGB stars to this array.

5. Duplicate observations need to be updated in the module ```getNdup``` (see existing file NESS_combined_column_table_Peter.csv).
