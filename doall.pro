@getSMACalibrators
FUNCTION convxxmmss,x
;Return in XX:MM:SS.s format
 ;First, preserve sign
 xx = abs(x) & sign = xx/x
 y = sixtyv(xx)
 y1 = strtrim(string(fix(y[0,*])),2) & y2 = strtrim(string(fix(y[1,*])),2) & y3 = strtrim(y[2,*],2)
 k = where(strlen(y1) eq 1) & if k[0] ne -1 then y1[k] = '0' + y1[k]
 k = where(strlen(y2) eq 1) & if k[0] ne -1 then y2[k] = '0' + y2[k]
 k = where(sign lt 0.) & if k[0] ne -1 then y1[k] = '-' + y1[k]
 yy = strtrim(y1,2)+':'+strtrim(y2,2)+':'+strtrim(y3,2)
 return,transpose(yy)
END

PRO fixQAsheet, infile
 ;saves output to infile_fixed.csv
 outfile = repstr(infile, '.csv', '_fixed.csv')

 readcol, infile, header, f = 'a', /sil, del = '$', num = 1
 readcol, infile, lines, f = 'a', /sil, del = '$', ski = 1
 lines = strtrim(lines, 2)
 k = where(strmatch(lines, '*,'), comp = nk) & if k[0] ne -1 then lines[k] = lines[k] + '-'
 lines = repstr(lines, ',,', ',-,')
 lines = repstr(lines, ',,', ',-,')
 forprint, t = outfile, lines, com = header, /sil

 readcol, outfile, header, num = 1, del = '$', /sil, f = 'a'
 readcol, outfile, source, receiver, molecule, checkedby, detection, fitsuccess, fitpeak, fitv_inf, $
         rms1, rms2, rms4, comments, f = 'a, a, a, a, a, a, a, a, a, a, a, a', /sil, del = ',', ski = 1
 source = repstr(strtrim(source, 2), '_', ' ')
 floats = ['fitpeak', 'fitv_inf', 'rms1', 'rms2', 'rms4']
 for i = 0, n_elements(floats) - 1 do begin
         junk = execute('k = where(strtrim(' + strtrim(floats[i], 2) + ', 2) eq "-")')
         junk = execute('if k[0] ne -1 then ' + strtrim(floats[i], 2) + '[k] = "NaN"')
 endfor
 lines = strtrim(source, 2) + ',' + strtrim(receiver, 2) + ',' + strtrim(molecule, 2) + ',' + strtrim(checkedby, 2) + ',' + $
         strtrim(detection, 2) + ',' + strtrim(fitsuccess, 2) + ',' + strtrim(fitpeak, 2) + ',' + strtrim(fitv_inf, 2) + ',' + $
         strtrim(rms1, 2) + ',' + strtrim(rms2, 2) + ',' + strtrim(rms4, 2) + ',' + strtrim(comments, 2)
 forprint, t = outfile, com = header, lines, /sil
END

;PRO sourcelist_plus_JCMTCO, t, k
PRO prune_using_JCMTCO21_flux_limit, t, k, fpeaks
 ;Returns the indices K of sources that are above the flux limit, and 
 ;  the peak fluxes in the four lines, in order: FCO21, 13FCO21, FCO32, 13FCO32, in the array FPEAKS
 ;Use the CO peak fluxes (12 and 13) from the QA table even if the fit quality was terrible.
 ;Have to also read in the full NESS table to get the distances
 t0 = read_csv('NESS+Gaia.csv',h=t0hdr) & t0 = str2arrstr(t0)
 match2, strtrim(t.field01, 2), strtrim(t0.field01, 2), a, b
 dkpc = replicate(!values.d_nan, n_elements(t)) & dkpc[a] = t0[b].field23

 readcol, 'NESS_JCMT_QA_sheet4_fixed.csv', source, receiver, molecule, fitpeak, f = 'a, a, a, x, x, x, d', /sil, del = ','
 
 fco21 = replicate(!values.d_nan, n_elements(t)) & f13co21 = fco21 & fco32 = fco21 & f13co32 = fco21

 kh12 = where(strmatch(receiver, '*HARP*') and ~strmatch(molecule, '*13*'))
 match2, strtrim(t.field01, 2), strtrim(source[kh12], 2), a, b, na, nb & if a[0] ne -1 then fco21[a] = fitpeak[kh12[b]]
 kh13 = where(strmatch(receiver, '*HARP*') and strmatch(molecule, '*13*'))
 match2, strtrim(t.field01, 2), strtrim(source[kh13], 2), a, b, na, nb & if a[0] ne -1 then f13co21[a] = fitpeak[kh13[b]]
 kr12 = where(strmatch(receiver, '*RXA*') and ~strmatch(molecule, '*13*')) 
 match2, strtrim(t.field01, 2), strtrim(source[kr12], 2), a, b, na, nb & if a[0] ne -1 then fco32[a] = fitpeak[kr12[b]]
 kr13 = where(strmatch(receiver, '*RXA*') and strmatch(molecule, '*13*')) 
 match2, strtrim(t.field01, 2), strtrim(source[kr13], 2), a, b, na, nb & if a[0] ne -1 then f13co32[a] = fitpeak[kr13[b]]

 h = histogram(alog10(fco21), nbins = 8, loc = hloc, /nan)
 plot, 10^hloc, h, /xl, chars = 2, xtit = textoidl('Peak CO (2-1) T_A^* (K)'), psym = 10, ysty = 3

 k1 = where(finite(f13co21), nk1)
 print, 'Number of sources with finite 13CO(2-1) fluxes: ' + strtrim(string(nk1), 2)
 fmin21with13 = min(fco21[k1], /nan)
 print, 'Minimum CO(2-1) T*_A at which 13CO(2-1) is detected: ' + strtrim(string(fmin21with13), 2) + ' K.'
 k = where(fco21 gt min(fmin21with13, /nan), nk)
 print, 'Number of sources with 12CO(2-1) higher than faintest source that has 13CO(2-1): ' + strtrim(string(nk), 2)
 fpeaks = [[fco21], [f13co21], [fco32], [f13co32]]
END

PRO prune_using_existing_ALMA_obs, t, k
;Peter generate a table listing sources that have existing ALMA observations. Remove these from the sample.
 readcol, 'NESS_SMA_exObs.csv', psc, f = 'a', /sil, del = ',', ski = 1
 match2, strtrim(t.field01, 2), strtrim(psc, 2), a, b, k
 print, 'Number of sources in remaining sample that have pre-existing ALMA/SMA observations: ' + $
         strtrim(string(n_elements(a)), 2) + '.'
END

PRO prune_using_existing_SMA_obs, t, k
;Sources that either Hyosun or Tomek said have data in both bands
 psc = ['IRAS 03507+1115', 'IRAS 10131+3049', 'IRAS 19486+3247', 'IRAS 23558+5106' ]
 match2, strtrim(t.field01, 2), strtrim(psc, 2), a, b, k
END

PRO prune_non_AGB, t, k
;List of sources that are not AGB stars
 psc = ['IRAS 17441-2411', 'IRAS 19244+1115', 'IRAS 07209-2540', 'IRAS 05524+0723', 'IRAS 03030+5532']
 match2, strtrim(t.field01, 2), strtrim(psc, 2), a, b, k
END

FUNCTION grouping, t, raminmaxmeanmed, decminmaxmeanmed
;Group into bins of width dra. 
;THE FOLLOWING IS NOT IMPLEMENTED HERE.
; Then, if there are bins with less than NMAX sources, collapse them
;   into the nearest neighbour bin and update the bin names and indices for all objects involved.

 ra = t.field02/15d & dec = t.field03

 dra = 3 ;Max width of bin in h
 nmin = 10 ;min number of targets per bin
 
 binindex = floor(double(ra/dra) + 1/2.)
 binvalue = strtrim(string(fix(dra*(binindex - 0.5))), 2) + '-' + strtrim(string(fix(dra*(binindex + 0.5))), 2)

 k = where(binindex eq 0 or binindex*dra ge 24)
 if k[0] ne -1 then begin
         binvalue[k] = strtrim(string(fix(24 - dra/2.)), 2) + '-' + strtrim(string(fix(dra/2.)), 2)
         binindex[k] = 0
 endif

 uniqnums, binindex, uniqvals;, /nopr
 nvals = n_elements(uniqvals)

 raminmaxmeanmed = cmreplicate(!values.d_nan, [nvals, 4]) & decminmaxmeanmed = raminmaxmeanmed
 for i = 0, nvals - 1 do begin
  k = where(binindex eq uniqvals[i])
  raminmaxmeanmed[i, 0:3] = [min(ra[k], /nan, max = max), max, mean(/nan, ra[k]), median(/ev, /do, ra[k])]
  decminmaxmeanmed[i, 0:3] = [min(dec[k], /nan, max = max), max, mean(/nan, dec[k]), median(/ev, /do, dec[k])]
 endfor

 set_plot,'ps' & device,file='grouping.eps',/inches,/color
 niceplot2
 plot,ra,dec,psym=2,xtit='RA (h)',ytit='DEC (deg)',chars=1.5,chart=2,xth=2,yth=2,th=2
 for i = 0, n_elements(startra) - 1 do oplot, [startra[i], startra[i]], [-1000, 1000], th = 2
 niceplot2,/restore
 device,/close & set_plot,'ps'

 return, binvalue
END

PRO getNdup,psc,sma21,sma32;,fcont1mm,fco21
;Given an array of PSCs, find the corresponding number of duplications from Peter's full CSV.
;   Also return the continuum flux at 1 mm in Jy and the CO(2-1) peak flux in Jy.

 t = read_csv('NESS_combined_column_table_Peter.csv',h=thdr) & t = str2arrstr(t)
 match2,strtrim(psc,2),strtrim(t.field01,2),a,b & ks = bsort(a) & t = t[b[ks]]
 k = where(strmatch(thdr,'*Ndup(2-1)*')) & junk = execute('sma21 = t.field'+strtrim(string(k[0]+1),2))
 k = where(strmatch(thdr,'*Ndup(3-2)*')) & junk = execute('sma32 = t.field'+strtrim(string(k[0]+1),2))
 ;k = where(strmatch(thdr,'*F_cont*')) & junk = execute('fcont1mm = t.field'+strtrim(string(k[0]+1),2))
 ;k = where(strmatch(thdr,'*F_CO*')) & junk = execute('fco21 = t.field'+strtrim(string(k[0]+1),2))
END

PRO get_need21_and_need32, t, need21, need32
 tt = read_csv('NESS_SMA_newObs.csv', h = tthdr) & tt = str2arrstr(tt) & tt_tags = tag_names(tt)
 u21 = where(strmatch(tthdr, '*Alma*ACA*CO(2-1)*')) & junk = execute('alma21flag = tt.' + strtrim(tt_tags[u21[0]], 2))
 u32 = where(strmatch(tthdr, '*Alma*ACA*CO(3-2)*')) & junk = execute('alma32flag = tt.' + strtrim(tt_tags[u32[0]], 2))

 ;Do we need further observations? This field is TRUE if the corresponding field in TT is FALSE.
 need21 = replicate('TRUE', n_elements(t)) & need32 = need21
 match2, strtrim(t.field01, 2), strtrim(tt.field01, 2), a, b, na, nb
 k = where(strtrim(strupcase(alma21flag[b]), 2) eq 'TRUE') & if k[0] ne -1 then need21[a[k]] = 'FALSE'
 k = where(strtrim(strupcase(alma32flag[b]), 2) eq 'TRUE') & if k[0] ne -1 then need32[a[k]] = 'FALSE'
 k1 = where(strmatch(need21, '*TRUE*'), nk1) & k2 = where(strmatch(need32, '*TRUE*'), nk2)
 print, 'Number of sources that need SMA 2-1 observations: ' + strtrim(string(nk1), 2)
 print, 'Number of sources that need SMA 3-2 observations: ' + strtrim(string(nk2), 2)
END

PRO make_COpeakfluxstats_table, fpeaks, rms
 ;RMS for 2-1 and 3-2
 if n_elements(rms) eq 0 then rms = [80d-3, 210d-3]

 ;Change filename to reflect current proposal cycle
 filename = 'NESS_SMA_LP_2018B_COpeakfluxstats.csv'
 print, 'Min, max, and median peak fluxes in the various CO lines being printed to ' + filename
 conv = 17/0.6 ;temperature to Jy conversion assuming 60% efficiency
 forprint, t = filename, /sil, com = 'line, minJy, maxJy, medianJy', strtrim(['CO21', '13CO21', 'CO32', '13CO32'], 2)$
         + ',' + sigfiground(conv*min(fpeaks, dim = 1, /nan, max = max), 3, /note) + ','$
         + ',' + sigfiground(conv*max, 3, /note) + ',' + sigfiground(median(fpeaks, dim = 1, /ev, /do)*conv, 3, /note)

 ;To do: also calculate the rms values
 ;
 ;

END

PRO dogrouping, t, bins
;Note: this function returns the RA in hour, no need to convert again!
 bins = grouping(t, raminmaxmeanmed, decminmaxmeanmed)

 uniqnums, bins, uniqbins, /nopr
 ;Write mean/median RA/DEC to file
 ks = bsort(fix((strarrsplit(uniqbins,'-',/ex))[*,0]))

 ;Return everything in XX:MM:SS.sss format
 medianrahms = convxxmmss(raminmaxmeanmed[*, 3]) & meanrahms = convxxmmss(raminmaxmeanmed[*,2])
 maxrahms = convxxmmss(raminmaxmeanmed[*,1]) & minrahms = convxxmmss(raminmaxmeanmed[*,0])
 mediandedms = convxxmmss(decminmaxmeanmed[*, 3]) & meandedms = convxxmmss(decminmaxmeanmed[*,2])
 maxdedms = convxxmmss(decminmaxmeanmed[*,1]) & mindedms = convxxmmss(decminmaxmeanmed[*,0])

 forprint,t='NESS_LP_grouping_meanmedianRADEC.csv',com='#uniqbins, medianra, mediandec, meanra, meandec, minra, mindec, maxra, maxdec',/sil,$
         strarrpad(strtrim(uniqbins[ks],2),6,/af)+','+$
         strarrpad(strtrim(string(medianrahms[ks]),2),18,/af)+','+strarrpad(strtrim(string(mediandedms[ks]),2),18,/af)+','+$
         strarrpad(strtrim(string(meanrahms[ks]),2),18,/af)+','+strarrpad(strtrim(string(meandedms[ks]),2),18,/af)+','+$
         strarrpad(strtrim(string(minrahms[ks]),2),18,/af)+','+strarrpad(strtrim(string(mindedms[ks]),2),18,/af)+','+$
         strarrpad(strtrim(string(maxrahms[ks]),2),18,/af)+','+strarrpad(strtrim(string(maxdedms[ks]),2),18,/af)

 junk = strarrsplit(bins,'-',/ex) & ks = bsort(fix(junk[*,0]))
END

PRO doall
 QAfile = 'NESS_JCMT_QA_sheet4.csv'
 fixQAsheet, QAfile ;requires an input file of the form NESS_JCMT_QA_sheet4.csv 
 ;Generate source list for SMA LP from the full NESS LP list
 t0 = read_csv('NESS_SMA_LP.csv', h = t0hdr) & t0 = str2arrstr(t0)

 ;All sorts of pruning
 prune_using_JCMTCO21_flux_limit, t0, k, fpeaks
 t1 = t0[k] & fpeaks1 = fpeaks[k, *]
 readcol, 'NESS_SMA_LP_chemtype.csv', del = ',', psc1, chemtype, f = 'a, a', /sil
 match2,strtrim(t1.field01, 2), strtrim(psc1, 2), a, b & ks = bsort(a)
 print, 'Breakdown of chemical types for this "full" sample of ' + strtrim(string(n_elements(k)), 2) + ' sources : '
 uniqnums, chemtype[b[ks]]

 prune_using_existing_ALMA_obs, t1, k
 t2 = t1[k] & fpeaks2 = fpeaks1[k, *]
 prune_using_existing_SMA_obs, t2, k
 t3 = t2[k] & fpeaks3 = fpeaks2[k, *]
 prune_non_AGB, t3, k
 t4 = t3[k] & fpeaks4 = fpeaks3[k, *]

 ;Get flags to indicate which bands in the remainder need observing
 get_need21_and_need32, t4, need21, need32

 ;Generate TeX table of CO peak flux stats
 make_COpeakfluxstats_table, fpeaks4

 ;---------The following was part of maketable
 psc=strtrim(t4.field01,2) & simname=strtrim(t4.field04,2)
 getNdup, psc, sma21, sma32
 cals = getSMACalibrators(t4.field02, t4.field03) 

 dogrouping, t4, bins

 psc = repstr(psc,'IRAS ','') & psc = strtrim(psc,2)
 psclen = max(strlen(strtrim(psc,2))) & simlen = max(strlen(strtrim(simname,2)))
 smalen = max([strlen(strtrim(string(sma21),2)),strlen(strtrim(string(sma32),2))])

 ;Get chemical types
 readcol, 'NESS_SMA_LP_chemtype.csv', del = ',', psc1, chemtype, f = 'a, a', /sil
 match2, strtrim(t4.field01, 2), strtrim(psc1, 2), a, b & ks = bsort(a) & b = b[ks]
 chemtype = chemtype[b]

 uniqnums, chemtype

 ;Newer version of ROWS, includes chemical type and which band has ALMA observations, if any.
 rows = strarrpad(strtrim(psc,2),psclen+1,/af)+' & '+strarrpad(strtrim(simname,2),simlen+1,/af)+' & '+$
         strtrim(chemtype, 2) + ' & ' + strtrim(need21, 2) + ' & ' + strtrim(need32,2) + ' & ' + $
         strtrim(bins,2)+' & '+strtrim(string(cals[*,0]),2)+' & '+strtrim(string(cals[*,1]),2)+' \\'

 ;rows = rows[ks] ;sort by bin

 forprint,t='NESS_LP_table.tex',/sil,/nocom,rows
 ;---------Done stuff part of maketable
END

