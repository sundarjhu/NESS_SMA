PRO match2,xx,yy,mx,my,nmx,nmy
;Updated 2014-01-23 -- error message slightly less cryptic
;Written February 8 2013 by Sundar Srinivasan
;Given two arrays X and Y, output MX and MY such that
;	XX[MX] = YY[MY]
;	MX contains an entry for EACH element in XX that has a match in YY. However, MY contains only the locations
;		of the LAST occurrence of that value in YY (similar to the UNIQ command).
;	NMX and NMY contain the locations of all the elements in XX and YY that don't have matches.
;	NOTE: MATCH2 is a many-to-one mapping, so if there are duplicate elements in YY that are matched to an
;		element in XX, MATCH2 will only document the last occurrence in MY. However, the duplicate
;		element will not be erroneously recorded in NMY.
;CALLS: match2misc.pro in PRO/idlutils/pro/misc

 match2misc,xx,yy,a,b
 mx=where(a gt -1,comp=nmx)
 if mx[0] ne -1 then begin
  my=a[mx] & nmy=where(b eq -1)
 endif else begin
  print,'No matches were found?? MATCH2 may have failed.'
 endelse
END
