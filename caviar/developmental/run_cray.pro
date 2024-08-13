@caviar
.run cray_auto
image_name=filenames[jjj]
.run save_cray
jjj=jjj+1 & print,jjj & image_name=strmid(filenames[jjj],0,13)+'.IMG'
