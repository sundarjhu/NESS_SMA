pro niceplot2,restore=restore,tex=tex
common niceplot,pticklen,pthick,xthick,ythick,charsize,font

if n_elements(pticklen) eq 0 then begin
    pticklen = !p.ticklen
    pthick = !p.thick
    xthick = !x.thick
    ythick = !y.thick
    charsize = !p.charsize
    font = !p.font
endif

if keyword_set(restore) then begin
    !p.ticklen = pticklen
    !p.thick = pthick
    !x.thick = xthick
    !y.thick = ythick
    !p.charsize = charsize
    !p.font = font
endif else begin
    !p.ticklen = 0.03
    !p.thick = 2
    !x.thick = 4
    !y.thick = 4
    !p.charsize = 1.4
    if ~keyword_set(tex) then !p.font = 1
endelse

end
