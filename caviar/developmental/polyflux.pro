Function polyflux, poly, image, inx, iny, f, errbar=errbar, $
                   radscan_np=radscan_np, mask=mask, median=median

;   define refined image dimensions 
;   # pixels is f^2 time the number of input image pixels

    nx    = inx*f
    ny    = iny*f

;   convert poly units (normalized) to image units

    ix  = nx*poly[*,0] + .5*f 
    iy  = ny*poly[*,1] + .5*f

;   find image pixels inside polygon 
; im contains each pixel inside the polygon, repeated the appropriate number
; of times.  Pixels entirely within the polygon are repeated f^2 times.

    p   = polyfillv(ix,iy,nx,ny)
    radscan_np = 0
    errbar = 0
    if p[0] eq -1 then return, -1

    xx  = (p mod nx)/f
    yy  = (p   / nx)/f
    im  = xx + yy*inx 

    ; Exclude pixels identically equal to 0 (generally bad data).
    foo = where( image[im] eq 0, count )
    if count eq n_elements(im) then return, -1 else if count gt 0 then im = vec_remove( im, foo )

    if keyword_set(mask) then begin
      foo = where( mask[im] gt 0, radscan_np )
      radscan_np = radscan_np / float(f)^2
      if radscan_np eq 0 then return, -1 else im = im[foo]
    endif else radscan_np = n_elements(im) / float(f)^2
    ;; It might be better to use (idea 2/17/06)
    ;radscan_np = n_elements(im[ uniq(im,sort(im)) ])
    if radscan_np gt 1 then errbar = stddev(image[im])
    if keyword_set(median) then begin
      return, median(image[im])
    endif else begin
      return, mean(image[im])
    endelse

; Ideas.  Make a nx-by-ny array (make_array?) and array[p]=1.  Then rebin.
; Or make a inx-by-iny array and for j=0,n_elements(xx)-1 do array[xx,yy]=array[xx,yy]+1
; Concatenate all arrays for all bins, then set to zero all at once the cosmic ray locations.

end


