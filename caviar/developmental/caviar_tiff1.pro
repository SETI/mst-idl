if not keyword_set(tifffile) then tifffile = image_name+'.tiff'
outim = tvrd(true=tiffcolor)
if keyword_set(tiffcolor) then write_tiff, tifffile, red=reverse(reform(outim[0,*,*]),2), green=reverse(reform(outim[1,*,*]),2), blue=reverse(reform(outim[2,*,*]),2), planarconfig=2 else write_tiff, tifffile, reverse( outim, 2 )
