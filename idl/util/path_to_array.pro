pro path_to_array, path

p=!PATH
length=strlen(p)

colon=strpos(p,':',0)
path=strmid(p,0,colon)

lcolon=colon+1
colon=strpos(p,':',lcolon)
while colon ne -1 do begin
  path=[path,strmid(p,lcolon,colon-lcolon)]
  lcolon=colon+1
  colon=strpos(p,':',lcolon)
endwhile
path=[path,strmid(p,lcolon,length-lcolon)]

end