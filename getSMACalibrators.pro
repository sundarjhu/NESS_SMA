;+
; FUNCTION getSMACalibrators, inra, indec, directory = dir, radius1 = rad1, radius2 = rad2
;   Given a pair of arrays INRA and INDEC containing the RA and DEC in decimal degrees for a list of sources,
;       return the IAU names of up to two SMA calibrators for each source within specified radii.
;   OPTIONAL INPUTS:
;       DIRECTORY: folder where files are written into and read from.
;       RADIUS1 and RADIUS2: search radii in degrees such that RADIUS2 > RADIUS1.
;           DEFAULT VALUES: RADIUS1 = 10 deg, RADIUS2 = 15 deg.
;   PROCEDURE:
;       1) Set search radiusi to RAD1.
;       2) Choose nearest match regardless of brightness as first calibrator.
;       3) Choose brightest source that isn't (2) as second calibrator.
;       4) Repeat with larger search radius RAD2 if not all sources have both matches.
;   EXAMPLE CALL TO THE FUNCTION:
;       readcol,'sourcelist.txt',sourcename,RA,DEC,f='a,d,d',/silent 
;       cal1 = replicate('', n) & cal2 = cal1
;       cals = getcalib(RA, DEC)
;       ;;;;....follow instructions...
;       cal1 = cals[*,0] & cal2 = cals[*,1] ;Arrays for the first and second calibrator respectively
;-
PRO makeSMACalibratorList
 print,'--------First, do the following-------'
 print,'1. Go to sma1.sma.hawaii.edu/callist/callist.html and copy the entire html table.'
 print,'2. Paste into a new document in any text editor.'
 print,'3. Remove any header lines and replace all "±" with "pm" and all "µm" with "mic".'
 print,'4. Save file as SMA_Calibrator_List_Raw.txt IN THIS FOLDER.'
 junk = '' & read,junk,prompt='When done, press enter: '


;First, mess with the raw data
 readcol,'SMA_Calibrator_List_Raw.txt',rows,f='a',/sil,del='$',str='#',ski=2
 rows = repstr(repstr(strtrim(rows,2),'plot/',''),'data','')
 ;Change the name of cen a to cen_a
 rows = repstr(rows,'cen a','cen_a')
 ;Rows starting with "850mi" need to be merged with previous entry
 k = where(strmid(strtrim(rows,2),0,5) eq '850mi', comp=nk)
 ;Append the 850 micron information to the preceding row (which has the 1mm info)
 rows[k-1] = rows[k-1] + ' ' + rows[k]
 ;Append a bunch of dashes to the 1mm rows that don't have corresponding 850 micron info.
 h = where(strmatch(rows[nk],'*850mic*'), comp=nh)
 rows[nk[nh]] = rows[nk[nh]] + '-- -- -- -- -- -- -- --'
 ;Now, only rows[nk] matters

 ;Split into columns and process the relevant ones
 junk = strarrsplit(rows[nk],' ',/ex)
 ;Columns to be joined. Col5 will contain the collapsed Date field.
 leave = [0, 1, 2, 3, 5, 8, 9, 11, 13, 16, 17, 19]

 junk[*,2] = strtrim(string(15*tenv(junk[*,2])),2) ;RA (deg)
 junk[*,3] = strtrim(string(tenv(junk[*,3])),2) ;DEC (deg)
 junk[*,5] = strjoin(transpose(junk[*,5:7]),'-') ;Date [dd-monthname-yyyy]
 junk[*,13] = strjoin(transpose(junk[*,13:15]),'-') ;Date [dd-monthname-yyyy]

 k = where(strtrim(junk[*,17],2) eq '--')

 ;Recast as CSV and output
 rows = strjoin(transpose(junk[*,leave]),', ')
 com = 'CommonName, IAUNameJ2000, RAdeg, DECdeg, LastObsDate1mm, Obs1mm, FnuJy1mm, dFnuJy1mm, LastObsdate850, Obs850, FnuJy850, dFnuJy850'
 forprint,t='SMA_Calibrator_List.csv',strtrim(rows,2),com=com,/sil

 print,'Processed calibration list saved in SMA_Calibrator_List.csv.'
END

FUNCTION getSMACalibrators, inra, indec, directory = dir, radius1 = rad1, radius2 = rad2
 if n_elements(dir) eq 0 then dir = './'
 if n_elements(rad1) eq 0 then rad1 = 10.
 if n_elements(rad2) eq 0 then rad2 = 15.

 ;If raw calibrator list does not exist, generate it.
 if ~file_test('SMA_Calibrator_List_Raw.txt') then begin
  junk = '' & read,junk,prompt='SMA_Calibrator_List_Raw.txt does not exist in folder '+strtrim(dir,2)+', generating...'
  makeSMACalibratorList
 endif

 readcol,'SMA_Calibrator_List.csv',comname,iauname,ra,dec,date1mm,obs1mm,fnu1mm,dfnu1mm,date850,obs850,fnu850,dfnu850,$
         f='a,a,d,d,a,a,d,d,a,a,d,d',/sil,del=','

 ndata = n_elements(inra) & ncal = n_elements(comname)

 print,'Looking for calibrators within a radius of '+strtrim(string(rad1),2)+' degrees.'
 ;Given NCAL = len(calibrators), find up to NCAL matches within DTH for each source.
 ntot = djs_angle_match(inra,indec,ra,dec,dth=rad1,mindx=mindx,mc=mc,mdist=mdist,mmax=ncal)
 nearest = replicate(-1, ndata) & brightest = replicate(-1, ndata)
 for i = 0, ndata-1 do begin
         if max(mindx[*,i]) ne -1 then begin
                 h = where(mindx[*,i] ne -1)
                 sdist = bsort(mdist[h,i]) ;nearest
                 sflux = bsort(fnu1mm[mindx[h,1]],/rev) ;brightest
                 nearest[i] = mindx[h[sdist[0]],i]
                 ;If there IS only one match, can't do very much about it.
                 if n_elements(sflux) gt 1 and sflux[0] eq sdist[0] then brightest[i] = mindx[h[sflux[1]],i] else brightest[i] = mindx[h[sflux[0]],i]
         endif
 endfor

 print,'Looking for calibrators within a larger radius of '+strtrim(string(rad2),2)+' degrees.'
 ;Given NCAL = len(calibrators), find up to NCAL matches within DTH for each source.
 ntot = djs_angle_match(inra,indec,ra,dec,dth=rad2,mindx=mindx,mc=mc,mdist=mdist,mmax=ncal)
 for i = 0, ndata-1 do begin
         if max(mindx[*,i]) ne -1 then begin
                 h = where(mindx[*,i] ne -1)
                 sdist = bsort(mdist[h,i]) ;nearest
                 sflux = bsort(fnu1mm[mindx[h,1]],/rev) ;brightest
                 if nearest[i] eq -1 then nearest[i] = mindx[h[sdist[0]],i]
                 ;Try to update the brightest neighbour if possible
                 if brightest[i] eq -1 or brightest[i] eq nearest[i] then begin
                         if n_elements(sflux) gt 1 and sflux[0] eq sdist[0] then brightest[i] = mindx[h[sflux[1]],i] else brightest[i] = mindx[h[sflux[0]],i]
                 endif
         endif
 endfor

 cal1 = iauname[nearest] & cal2 = iauname[brightest] & k = where(nearest eq brightest) & if k[0] ne -1 then cal2[k] = '--'
 return,[[cal1],[cal2]]
END
