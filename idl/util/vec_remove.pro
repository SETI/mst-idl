function vec_remove, v, i

; For a 1-dimensional vector, v, removes the ith element
; i can now be a vector, with all elements of i being elements of v
; to be removed.

sz = size(v)
if sz[0] ne 1 then begin
  print, '% VEC_REMOVE: v is not a 1-dimensional vector'
  return, -1
endif

if (size(i))[0] eq 0 then if i eq -1 then return, v

if max(i) ge sz[1] then begin
  print, '% VEC_REMOVE: max(i) is greater than n_elements(v)-1'
  return, -1
endif

if min(i) lt 0 then begin
  print, '% VEC_REMOVE: min(i) is less than 0'
  return, -1
endif

szi = size(i)
if szi[0] gt 1 then begin
  print, '% VEC_REMOVE: i has more than one dimension'
  return, -1
endif

if sz[1] gt 32767 then j=lonarr(sz[1]) else j=intarr(sz[1])
j[i] = 1
return, v[where(j eq 0)]

end
