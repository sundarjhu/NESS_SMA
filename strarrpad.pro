;FUNCTION strarrpad,strarr,len,after_=after,fill_=fill
FUNCTION strarrpad,strarr,len,_extra=_extra
;Updated 2016-07-07: added _EXTRA keyword. Using this keyword, you can pass the FILL=CHARACTER and /AFTER keywords.
;Written July 26 2012
;Pass array to STRPAD
 outstr=strarr 
 for i=0L,n_elements(strarr)-1 do outstr[i]=strpad(strtrim(strarr[i],2),len,_extra=_extra)
 return,outstr
END
