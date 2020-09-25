
; --- template for population file ---
restore, '..\Files\INPUT\ANCILLARY\pop_templ.sav'

; --- population country totals ---
pop = read_ascii( '..\Files\INPUT\ANCILLARY\MORTALITY\BASEMORT2018\POP_1990-2100_UN2017_AGEGRP.csv', TEMPLATE=POP_TEMPL )

; --- generate indices of list with unique country codes and names from POP file ---
clist   = uniq( pop.cntr_id )
ncntr   = n_elements( clist )
cntr_id = pop.cntr_id[ clist ]

; --- read the grid ---
restore, '..\Files\INPUT\ANCILLARY\FASST_REGION_MASK\0.5x0.5_INDIV_COUNTRY_MASK.SAV'

dims = size( CNTRMASK_MEDRES.CNTRYMASKMED, /DIMENSIONS )
z    = dims[ 0 ]
x    = dims[ 1 ]
y    = dims[ 2 ]

; --- one sheet per layer ---
; ---print, '[[begin]]'
; ---for icntr = 0, z - 1 do begin
; ---   cmask = reform( CNTRMASK_MEDRES.CNTRYMASKMED[ icntr, *, * ] )
; ---   print, cmask
; ---   print, '-----'
; ---end
; ---print, '[[end]]'

; --- merge all layers ---
merge = intarr( x, y ) * 0
for icntr = 0, z - 1 do begin
   id    = cntr_id[ icntr ]
   cmask = reform( CNTRMASK_MEDRES.CNTRYMASKMED[ icntr, *, * ] )
   for y1 = 0, y - 1 do begin
      for x1 = 0, x - 1 do begin
         c = cmask[ x1, y1 ]
         if c ne 0 then begin
            d = merge[ x1, y1 ]
            if d eq 0 then begin
               merge[ x1, y1 ]  =  id
            endif else begin
               print, format = '("Attempt to store value: ", I, " in cell: ", I, ", ", I, " - but it s already filled with: ", I )', c, x1, y1, d
            endelse
         endif
      endfor
   endfor
endfor

; --- write out all merged layers as csv ---
merge = reverse( merge, 2 )
for y1 = 0, y - 1 do begin
   print, format='(720( I, ";" ))', merge[ *, y1 ]
end

end
