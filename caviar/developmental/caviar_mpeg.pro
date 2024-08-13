;if not keyword_set(mpegfile) then mpegfile = image_name+'.mpg'
if not keyword_set(mpegdim) then mpegdim = [ 1024, 1024 ]
mpegid = mpeg_open( mpegdim, quality=mpeg_quality )
if not keyword_exists(mpegstart) then mpegstart = 0
if not keyword_exists(mpegend) then mpegend = n_elements(filenames) - 1
for j=mpegstart,mpegend do begin
  im = read_vicar( filenames[j] )
  mpeg_put, mpegid, color=mpegcolor, frame=j, image=im, /order
endfor
mpeg_save, mpegid, filename=mpegfile
mpeg_close, mpegid

end
