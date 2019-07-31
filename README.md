# NESS_SMA
IDL scripts used to generate SMA target lists.
NOTE: you will have to update your IDLUTILS folder with scripts from [here](http://www.sdss3.org/dr8/software/idlutils_doc.php) if you encounter errors related to the BSORT or DJS_ANGLE_MATCH modules.

How to: Run the DOALL module in doall.pro.

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
