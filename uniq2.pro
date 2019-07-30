FUNCTION uniq2,array,reps
;Updated 2014-01-23 - minor cleanup
;Written April 2 2013 by Sundar Srinivasan
;Calls: MATCH2
	n=n_elements(array)
	u=uniq(array,bsort(array))
	nu=n_elements(u)
	match2,array,array[u],a,b,na,nb
	if na[0] ne -1 or nb[0] ne -1 then print,"---------- Something's wrong! See line 7 in UNIQ2"
	if n eq nu then begin
	 r=-1 
	endif else begin
	 match2,a,u[b],a1,b1,na1,nb1
	 reps=[[a[na1]],[u[b[na1]]]]
	endelse
	return,u
END
