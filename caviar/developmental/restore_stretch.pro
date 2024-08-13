slash = rstrpos( image_name, '/' )
image_dir = strmid( image_name, 0, slash+1 )
savefile = image_dir + 'stretch.sav'
if keyword_set(findfile(savefile)) then begin

  print, 'Restoring brightness information from '+savefile
  restore, savefile
  jimg = (where( strmid(image_name,slash+1,11) eq strmid(filenames,0,11), count ))[0]
  if count eq 0 then begin
    print, image_name+' not found in list of filenames.  No stretching performed.'
    jimg = -1
  endif else if count gt 1 then begin
    print, image_name+' found multiple times in list of filenames.  No stretching performed.'
    jimg = -1
  endif

endif

if keyword_set(usehistogram) then goto, dohist
if keyword_set(manualstretch) and keyword_exists(stmin) and $
                                      keyword_exists(stmax) then begin
  _rawim = rawim > stmin < stmax 
endif else if keyword_set(jimg) then if jimg ne -1 then begin
  _rawim = rawim > stretchmin[jimg] < stretchmax[jimg]
endif

if not keyword_set(_rawim) and not keyword_set(nohistogram) then begin
  dohist:
  run_histogram, rawim, stmin, stmax, locations=locations, many=many, fail=fail, fullhist=fullhist, threshold=histogram_threshold, hist=hist
  if not keyword_set(fail) then _rawim = rawim > stmin < stmax
endif
if not keyword_set(_rawim) then _rawim = rawim

end
