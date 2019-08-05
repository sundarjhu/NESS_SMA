FUNCTION str2arrstr,s,overwrite=overwrite,onlytags=onlytags
;Updated July 20 2012 -- Added ONLYTAGS keyword
;Convert a structure S into an array of structures.
;If keyword OVERWRITE is set, the function returns 0 and overwrites S.
;Example: s={number:indgen(12),array:findgen(12)}
;	  s2=str2arrstr(s)
;optional keyword ONLYTAGS -- selectively pick indices of TAG_NAMES
;Example: s2=str2arrstr(s,onlytags=[0]}
 tags=tag_names(s)
 if keyword_set(onlytags) then tags=tags[onlytags]
 ntags=n_elements(tags) & junk=execute("n=n_elements(s."+tags[0]+")")
 tagflag=replicate(1,ntags)
 for i=0,ntags-1 do junk=execute("tagflag[i]=(size(s."+makestr(tags[i])+"))[0]") 
 k1=where(tagflag eq 1,comp=k2)
 tagstr1=replicate('',ntags) & tagstr2=tagstr1 & tagstr3=tagstr1
 if k2[0] ne -1 then begin
  tagstr1[k2]='transpose(' & tagstr2[k2]=',*' & tagstr3[k2]=')'
 endif

 str="s2=replicate({"+tags[k1[0]]+":s."+tags[k1[0]]+"[0]"
 if ntags gt 0 then for i=1,ntags-1 do str+=","+tags[i]+":"+tagstr1[i]+"s."+tags[i]+"[0"+tagstr2[i]+']'+tagstr3[i]
 str+="},n)"
 junk=execute(str)

 for j=0l,n-1 do for i=0,ntags-1 do $
	junk=execute("s2["+makestr(j)+"]."+tags[i]+"="+tagstr1[i]+"s."+tags[i]+"["+makestr(j)+tagstr2[i]+"]"+tagstr3[i]) 
 
 if keyword_set(overwrite) then begin
  s=s2 & return,0
 endif else begin
  return,s2
 endelse
END
