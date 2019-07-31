FUNCTION STRARRSPLIT,ARRAY,DELIMITER,EXTRACT=EXTRACT,STRUCTURE=STRUCTURE,MAX_ELEMENTS=MAXELEMENTS
;Updated 2015-04-13 -- Added MAX_ELEMENTS keyword
;Updated December 4 2011 -- Added STRUCTURE keyword
;Written June 6 2011 by Sundar Srinivasan
;Wraparound to STRSPLIT, works on an array of strings.
;If keyword STRUCTURE is set, return a structure with fields named 'COL1' etc.
 str='' & if keyword_set(extract) then str+=',/extract'
 n=n_elements(array)
 if n_elements(maxelements) ne 0 then begin
  m=maxelements
 endif else begin
  junk=execute('a=strsplit(array[0],delimiter'+str+')')
  m=n_elements(a)
 endelse
 array2=strarr(n,m)
 for i=0l,n-1 do junk=execute('junk=strsplit(array[i],delimiter'+str+') & njunk=n_elements(junk) & if njunk gt 0 then array2[i,0:njunk-1]=junk')
 if keyword_set(structure) then begin
  string='s={' & for i=0,m-1 do string+='col'+makestr(i+1)+':array2[*,'+makestr(i)+'],'
  string+='junk:0}'
  junk=execute(string)
  array2=s
 endif
 return,array2
END
