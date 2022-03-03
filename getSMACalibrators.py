"""
1. Go to sma1.sma.hawaii.edu/callist/callist.html and copy the entire html table into a CSV file using
    some utility like the CopyTables extension for Firefox.
2. Open up the CSV and change the headers so there's one name per column. The ones I chose are:
    Common Name, Source Name, RAJ2000, DEJ2000, Band, Last Obs. Date, Obs., Flux Density (Jy) 30.0 day avg, Light curves
3. Find all instances of ± and replace with \pm.
4. Find all instances of mi and replace with mi.
4. Save as SMA_Calibrator_List_Raw.csv.
"""

from astropy.table import Table, hstack, MaskedColumn, join, join_skycoord
import numpy as np
from astropy.coordinates import SkyCoord
from astropy import units
import os

def makeSMACalibratorList(rawFile = 'SMA_Calibrator_List_Raw.csv', \
                          outFile = 'SMA_Calibrator_List'):
    try:
        t = Table.read(rawFile, format = 'csv')
    except:
        FileNotFoundError("The raw calibrator list was not found in this folder!")

    for col in ['Common Name', 'Source Name']:
        t[col] = [c if c is np.ma.masked else c.replace('cen a', 'cen_a') for c in t[col]]

    oldcols = ['Common Name', 'Source Name', 'RAJ2000', 'DEJ2000', 'Band', 'Last Obs. Date', \
               'Obs.', 'Flux Density (Jy) 30.0 day avg', 'Light curves']
    newcols = ['CommonName', 'SourceName', 'RAJ2000', 'DEJ2000', 'Band', 'ObsDate_1mm', \
               'Obs_1mm', 'FluxJy_1mm', 'LightCurve_1mm']
    descrip = ['--', '--', '--', '--', '--', '--', '30 day avg', '']
    t.rename_columns(oldcols, newcols)
    for c, d in zip(newcols, descrip):
        t[c].description = d

    k850 = np.nonzero(['850mi' in b for b in t['Band']])[0]
    k_1 = np.array([k - 1 for k in k850])
    nk850 = len(k850)
    v = Table()
    cols = ['ObsDate_1mm', 'Obs_1mm', 'FluxJy_1mm', 'LightCurve_1mm']
    for c in cols:
        v[c.replace('1mm', '850mi')] = np.repeat(len(t[c][0]) * ' ', len(t))
        v[c.replace('1mm', '850mi')][k_1] = t[c][k850]

    tv = hstack([t, v])
    k1mm = np.nonzero(['1mm' in b for b in t['Band']])[0]
    tv = tv[k1mm].copy()
    tv.remove_column('Band')

    # Fluxes and uncertainties are written into separate float columns
    for b in ['1mm', '850mi']:
        f, _, df = np.array([[np.nan, '', np.nan] if f.strip() == '' else f.split() for f in tv['FluxJy_' + b]]).T
        f = [float(ff) for ff in f]
        df = [float(ff) for ff in df]
        tv.remove_column('FluxJy_' + b)
        tv['FluxJy_' + b] = f
        tv['dFluxJy_' + b] = df

    # RA and DEC are converted to degrees and stored as floats
    coords = SkyCoord(tv['RAJ2000'], tv['DEJ2000'], unit = (units.hourangle, units.deg), frame = 'icrs')
    tv.remove_columns(['RAJ2000', 'DEJ2000'])
    tv['RAJ2000'] = coords.ra
    tv['DEJ2000'] = coords.dec

    for c in tv.colnames:
        if ('Flux' not in c) and ('J2000' not in c):
            tv[c] = MaskedColumn(data = tv[c], mask = [d.strip() == '' for d in tv[c]])

    tv.write(outFile + '.vot', format = 'votable', overwrite = True)
    tv.write(outFile + '.csv', format = 'csv', overwrite = True)

def getSMACalibrators(ID, RA, DEC, radius_deg = 15., calibratorList = 'SMA_Calibrator_List.vot'):
    """
    Find the nearest calibration within the radius (in degrees) specified by `radius_deg`.
    Arguments:
    ----------
    ID -- array of unique identifiers of sources for which calibrators are sought.
    RA, DEC -- RA and DEC of sources in degrees.
    radius_deg -- radius in degrees within which to look for calibrators.
    calibratorList -- VOTable containing list of calibrators. If not found, the code tries to
            generate it by calling makeSMACalibratorList.
    Outputs:
    --------
    out -- an astropy table containing information for the nearest calibrator within `radius_deg`
            if no calibrator could be found within the specified radius, the corresponding CommonName
            and SourceName columns are BOTH masked.
    """
    if ~os.path.isfile(calibratorList):
        print("Calibrator file {} does not exist in this folder. Trying to generate from raw file.".format(calibratorList))
        makeSMACalibratorList()

    data_coord = SkyCoord(RA, DEC, unit = 'deg')
    data = Table([ID, data_coord], names = ('ID', 'sc'))

    cal = Table.read(calibratorList, format = 'votable')
    cal_coord = SkyCoord(cal['RAJ2000'], cal['DEJ2000'], unit = 'deg')
    cal['sc'] = cal_coord

    print("Looking for calibrators within a radius_deg of {} deg.".format(radius_deg))
    dc = join(data, cal, join_funcs = {'sc': join_skycoord(radius_deg * units.deg)})
    dc['angDist'] = dc['sc_1'].separation(dc['sc_2'])
    # Nearest calibrator
    k = np.lexsort([dc['angDist'], dc['ID']])
    kk = dc[k].group_by('ID').groups.indices[:-1]
    dc_near = dc[k][kk]
    # Brightest calibrator at 1 mm
    k = np.lexsort([dc['angDist'], -dc['FluxJy_1mm']])
    kk = dc[k].group_by('ID').groups.indices[:-1]
    dc_bright = dc[k][kk]

    dc = Table([dc_near['ID'], dc_near['SourceName']], names = ('ID', 'cal_1'))
    dc['cal_2'] = MaskedColumn(data = dc_bright['SourceName'], mask = dc_bright['SourceName'] == dc_near['SourceName'])

    out = join(Table([ID], names = ('ID', )), dc, keys = 'ID', join_type = 'left')

    return out
