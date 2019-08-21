FUNCTION STRARRMID,ARRAY,STARTPOS,NUMPOS,NUMEND=NUMEND,REVERSE=REVERSE
;Updated June 21 2012 to accept STARTPOS and NUMPOS as arrays
;Written May 16 2012
;Wraparound to STRMID, works on arrays of strings.
;If keyword NUMEND is specified, NUMPOS is ignored and the string segment is automatically terminated at the end of the string.
;The keyword REVERSE is used in a fashion similar to STRMID.
 n=n_elements(array) & out=strarr(n)
 if n_elements(startpos) eq 1 then startpos1=replicate(startpos,n) else startpos1=startpos
 if keyword_set(numend) then begin
  numpos1=strlen(array)-1
 endif else begin
  if n_elements(numpos) eq 1 then numpos1=replicate(numpos,n) else numpos1=numpos
 endelse
 str1='' & if keyword_set(reverse) then str1=',/reverse'

 for i=0l,n-1 do junk=execute("out[i]=strmid(array[i],"+makestr(startpos1[i])+","+makestr(numpos1[i])+str1+")")

 return,out
END
