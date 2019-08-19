FUNCTION makestr,invar,pad=pad
;Updated June 9 2011
;Added PAD keyword
;	If set, MAKESTR returns a string array with STRLEN equal to STRLEN of the largest original string.
;Written March 13 2008
;Convert a non-string into a string using string and strtrim
 string=strtrim(string(invar),2)
 if keyword_set(pad) then begin
  length=max(strlen(string))-strlen(string) & k=where(length gt 0)
  if k[0] ne -1 then for i=0L,nel(k)-1 do string[k[i]]+=strjoin(replicate(' ',length[k[i]]))
 endif
 return,string
END
