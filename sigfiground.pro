FUNCTION sigfiground,var,nsig,notexp=notexp,tex=tex,latex=latex,nan=nanstring
;Updated 2015-09-17: added NAN keyword, which allows user to replace any NaNs with the contents of NANSTRING (defaults to 'NaN').
;Updated 2015-08-12: added LATEX keyword, which allows output ready to be ingested into a TeX document.
;Rewritten 2015-04-08: Uses arrays instead of FOR loops
;Updated November 19 2011: Added TEXTOIDL and NOTEXP keywords
;Written March 8 2010
;VAR is an array of numbers, NSIG is a scalar
 if keyword_set(note) and max(var) gt 1e6 then begin
  print,'Input number must be smaller than 1e6 if NOTEXP keyword is set!'
  print,'Try again!'
 endif else begin
  s=size(var) & na=s[s[0]+2] & if s[s[0]+1] ne 7 then a=strtrim(string(var),2) else a=var
  pow1=replicate(0,na) & exppos=strpos(strupcase(a),'E')
  k=where(exppos ne -1)
  if k[0] ne -1 then begin
   pow1[k]=fix(strarrmid(a[k],exppos[k]+1,/numend))
   a[k]=strarrmid(a[k],0,exppos[k])
  endif
  sign=replicate('',na) & left=sign & right=sign
  k=where(strmatch(a,'-*'))
  if k[0] ne -1 then begin
   sign[k]='-'
  endif
  aa=double(a)*10d^pow1 & pow=floor(alog10(aa)) & decpos=strpos(a,'.')
  t=where(decpos eq 0) & if t[0] ne -1 then a[t]='0'+a[t]
  k=where(strpos(a,'.') gt -1 and strpos(a,'.') ne strlen(a)-1,comp=nk)
  if k[0] ne -1 then begin
   junk=strarrsplit(a[k],'.',/ex) & left[k]=strtrim(junk[*,0],2) & right[k]=strtrim(junk[*,1],2)
  endif
  if nk[0] ne -1 then begin
   junk=strarrsplit(a[nk],'.',/ex) & left[nk]=strtrim(junk,2) & right[nk]='' 
  endif
  strlen=strlen(a) & lstrlen=strlen(left) & rstrlen=strlen-lstrlen-1
  out=round(double(strarrmid(strtrim(string(round(double(left+right))),2),0,nsig)+'.'+strarrmid(strtrim(string(round(double(left+right))),2),nsig,/numend)))
  out=strtrim(string(out),2) & strlenout=strlen(out)
  if keyword_set(notexp) then begin
   leadingzeros=replicate('',na) & trailingzeros=replicate('',na)
   toofew=where(nsig le pow)
   ;if toofew[0] ne -1 then for i=0,n_elements(toofew)-1 do trailingzeros[toofew[i]]=strjoin(replicate('0',pow[toofew[i]]+1-nsig),'')
   if toofew[0] ne -1 then trailingzeros[toofew]=strarrmid(strtrim(string(10l^(pow[toofew]+1-nsig)),2),1,/numend)
   neg=where(pow lt 0) & if neg[0] ne -1 then leadingzeros[neg]='0.'
   ;ltm1=where(pow lt -1) & if ltm1[0] ne -1 then for i=0,n_elements(ltm1)-1 do leadingzeros[ltm1[i]]+=strjoin(replicate('0',abs(pow[ltm1[i]])-1),'')
   ltm1=where(pow lt -1) & if ltm1[0] ne -1 then leadingzeros[ltm1]+=strarrmid(strtrim(string(10l^(abs(pow[ltm1])-1)),2),1,/numend)
   out=leadingzeros+out+trailingzeros
   int=where(nsig gt pow and pow ge 0) & if int[0] ne -1 then out[int]=strarrmid(out[int],0,1+pow[int])+'.'+strarrmid(out[int],1+pow[int],nsig-(1+pow[int])) 
   cleanup=where(strmatch(out,'*.')) & if cleanup[0] ne -1 then out[cleanup]=strarrmid(out[cleanup],0,strlen(out[cleanup])-1) 
   cleanup=where(strmatch(out,'.*')) & if cleanup[0] ne -1 then out[cleanup]='0.'+out[cleanup]
   out=sign+out
  endif else begin
   out=sign+strarrmid(out,0,1)+'.'+strarrmid(out,1,/numend)
   out1=[[out],[strtrim(string(pow),2)]]
   cleanup=where(strmatch(out1,'*.'))
   if cleanup[0] ne -1 then begin
    decpos=strpos(out1[cleanup],'.')
    out1[cleanup]=strarrmid(out1[cleanup],0,decpos)
   endif
   latexout=out1[*,0]+'\times '+'10^{'+out1[*,1]+'}'
   ;if keyword_set(tex) then out=textoidl(out1[*,0]+'\times'+'10^{'+out1[*,1]+'}') else out=out1[*,0]+'E'+out1[*,1]
   if keyword_set(tex) then out=textoidl(latexout) else out=out1[*,0]+'E'+out1[*,1]
   if keyword_set(latex) then out=latexout
  endelse

  ;Final check: are there any NaNs?
  k=where(finite(var,/nan))
  if k[0] ne -1 then begin
   if n_elements(nanstring) eq 0 then nanstring='NaN'
   print,'**************	SIGFIGROUND WARNING!'
   print,'		NaNs found. Replacing with '+strtrim(nanstring,2)
   out[k]=strtrim(nanstring,2)
  endif 
  return,out
 endelse
END
