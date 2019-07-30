PRO uniqnums,invar,uniqvals,u,r,instances,noprint=noprint,sort=sort_
;Updated 2017-11-20
;   - Added SORT keyword. Set to either 'ASC' or 'DESC'
;   - SERIOUS bug fix: histogram(r,...) --> histogram(r[*,1],...)
;Rewritten Feb 25 2013
;Returns the number of instances of each unique value of INVAR.
;	Can also return all the instances in the variables U and R, generated as part of the call to UNIQ2.
;	UNIQVALS is an array containing the unique values in INVAR
;	U has the locations of the LAST occurrence of each unique value
;	R is a 2-D array with the first element corresponding to the location of repetitions, and the
;		second element equal to one of the locations already contained in U.
;EXAMPLE:
;	> A = [1,1,5,1,3,2] & UNIQNUMS,A,UNIQVALS,U,R ;;output below:
;	----------------------------------------------------
;	Unique value      #(Instances)      % of total      
;	----------------------------------------------------
;	 2                1                 16.66           
;	 3                1                 16.66           
;	 5                1                 16.66           
;	 1                3                 50.00           
;	 Total            6                 100.0           
;	> PRINT,UNIQVALS
;	2       3       5       1
;	> PRINT,U
;	5           4           2           3
;	> PRINT,R
;       0           1 ;;;Locations in A of repeats
;       3           3 ;;;Locations in A of corresponding unique elements
 u=uniq2(invar,r)
 if n_elements(r) ne 0 then begin
  h=histogram(r[*,1],loc=loc,min=min(u),max=max(u))
  ;Remember that the histogram above only counts the total number of REPEAT elements,
  ;	so add 1 to each H value below.
  match2,u,loc,a,b & u=u[a] & loc=loc[b] & h=h[b]+1
  ;Sort the results based on number of repeats (default: ascending)
  if n_elements(sort_) eq 0 then sort_='asc'
  if strtrim(sort_,2) eq 'asc' then ks=bsort(h) else ks=bsort(h,/rev)
  ;s=sort(h) & u=u[s] & loc=loc[s] & h=h[s]
  u=u[ks] & loc=loc[ks] & h=h[ks] ;& r=r[ks,*]
  len1=max(strlen(invar[u])) & len2=max(strlen(makestr(h))) & len3=max(strlen(makestr(100*h/total(h)))) 
 endif else begin
  nu=n_elements(u) & len1=max(strlen(invar[u])) & len2=1 & len3=max(strlen(replicate(strtrim(string(100./nu),2),nu)))
 endelse
 str1='Unique value' & max1=max([len1,strlen(str1)])+6
 str2='#(Instances)' & max2=max([len2,strlen(str2)])+6
 str3='% of total'   & max3=max([len3,strlen(str3)])+6
 if ~keyword_set(noprint) then begin
  print,strjoin(replicate('-',max1+max2+max3),'')
  print,strpad(str1,max1)+strpad(str2,max2)+strpad(str3,max3)
  print,strjoin(replicate('-',max1+max2+max3),'')
  if n_elements(r) ne 0 then begin
   forprint,strarrpad(strtrim(invar[u],2),max1,/after)+strarrpad(strtrim(h,2),max2)+strarrpad(strtrim(sigfiground(100*h/total(h),5,/note),2),max3)          
   forprint,strarrpad('Total',max1,/after)+strarrpad(strtrim(long(total(h)),2),max2)+strarrpad('100.0',max3)
  endif else begin
   forprint,strarrpad(strtrim(invar[u],2),max1,/after)+strarrpad(replicate('1',nu),max2)+strarrpad(strtrim(sigfiground(replicate(strtrim(string(100./nu),2),nu),5,/note),2),max3)
   forprint,strarrpad('Total',max1,/after)+strarrpad(strtrim(string(nu),2),max2)+strarrpad('100.0',max3)
  endelse
 endif
 uniqvals=invar[u]
 instances=h
END
