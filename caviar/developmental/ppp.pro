; Protocol for downloading new imaging sequences:
; I like to browse all images on the IMDB with the following keywords checked:
;   Instrument ID
;   Image Mid Time
;   Exposure Duration
;   Filter Wheel 1
;   Filter Wheel 2
;   Gain
;   Instrument Mode ID
;   Data Conversion Type
;   Inst Comprs Type
;   Shutter State ID
;   Observation ID
;   Description
;   Detector Temperature
; Make sure to check all 4 boxes (Keyword columns, Image thumb, Autonav thumb, 
; Generate Spreadsheet) and increase the Results per page.  Then, when you've 
; identified images you want to download, note the Observation ID and then click
; on the "Full Query Mode" button to go back to the query page with the same 
; settings you put in before.  Specify the Observation ID and re-do the search. 
; "Download Spreadsheet", "Save Page As..." and then Back.  Click the "Get All 
; Images" box and then "Download Selected Images".  
;
; Put all of the results in the appropriate folder and unpack the tar.gz .  
; In IDL, ".run ppp" to run this script.
; 
; Then go to the Cisscal directory and batch calibrate all of the files.  Make 
;   sure that the dark current and the DN to I/F settings are correct.
; Finally, return to the image directory and type ".run update_stretch" in IDL.


csvfile = findfile('*csv')
if n_elements(csvfile) ne 1 then stop, 'csvfile:  ', csvfile
openr, 1, csvfile
if csvfile eq 'spreadsheet.csv' then outfile='spreadsheet1.csv' else outfile='spreadsheet.csv'
openw, 2, outfile
a = ''

print, 'Replacing tabs with backslashes in '+csvfile
tab = ''
while not eof(1) do begin
  b = ''
  readf, 1, a
  if not keyword_set(tab) then tab = strmid( a, 10, 1 )
  slash = [ strsplit( a, tab ), strlen(a)+1 ]
  for jjj=0,n_elements(slash)-2 do begin
    if jjj ne 0 then b = b + '\'
    b = b + strmid( a, slash[jjj], slash[jjj+1]-slash[jjj]-1 )
  endfor
  printf, 2, b
endwhile

close, 1
close, 2
print, 'Removing old csvfile:'
spawn, 'rm '+csvfile
if csvfile eq 'spreadsheet.csv' then spawn, 'mv spreadsheet1.csv spreadsheet.csv'

print, 'Generating spreadsheet.sav'
data = read_csv( 'spreadsheet.csv', /silent )
legend = data[*,0]
data = data[*,1:n_elements(data[0,*])-1]
data = data[*,sort(data[0,*])]
exposure_duration = long(data[3,*])
save, legend, data, exposure_duration, filename='spreadsheet.sav'

print, 'Creating files.lis'
if keyword_set(findfile('files.lis')) then spawn, 'rm files.lis'
spawn, 'ls *IMG > files.lis'

print, 'Now go to the Cisscal directory and batch calibrate all of the files.'
print, '  Make sure that the dark current and the DN to I/F settings are correct.'
print, 'Finally, return to the image directory and type ".run update_stretch" in IDL.'

end
