;This is an altered version of edgefit_auto made in 2010/2011 by
;Breanna M. Byington to accomodate fitting four gap edges in 065. Due to
;the setup of run_edgefit, without more complex programming changes
;only two edges can be fit at once, and then the user must exit and
;re-enter idl to fit the remaining two.

restore,'stretch.sav'
image_number =''
read,image_number,prompt='Image Number (0-'+strtrim(n_elements(filenames)-1,2)+') : '
image_number = fix(image_number)
image_name = filenames(image_number)

if not keyword_exists(nosmooth) then nosmooth = 0
if not keyword_set(screenx) then screenx = 1190

newnacmat=2
nostars=1
norings=1
@caviar

mainmenu:
reply1=''
while reply1 eq '' do begin
    print, 'Start with [H]erschel or [J]effreys  or K[u]iper gap?'
    read, reply1
    case reply1 of
        'h': begin
            kk=0
            herschel=1
            goto, herschel
        end
        'j': begin
            kk=2
            jeffreys=1
            goto, jeffreys
        end
        'u': begin
            kk=4
            kuiper=1
            goto, kuiper
        end
        else: reply1=''
    endcase
endwhile

herschel:
if keyword_set(herschel) then begin
    mnrad = 118260.0
    mxrad = 118300.0
    gap = 'H'
    inout = ''
    goto, next
endif

russell:
if keyword_set(russell) then begin
 kk=kk+1
    mnrad = 118600.0
    mxrad = 118650.0
    gap = 'S'
    inout = ''
    goto, next
endif

jeffreys:
if keyword_set(jeffreys) then begin
    mnrad = 118940.0
    mxrad = 118990.0
    gap = 'J'
    inout = ''
    goto, next
endif

kuiper:
if keyword_set(kuiper) then begin
kk=kk+1
    mnrad = 119370.0
    mxrad = 119440.0
    gap = 'U'
    inout = ''
    goto, next
endif

next:
mnlon = 0
mxlon = 0
@r
rwin = !d.window

period = rstrpos( image_name, '.' )
filestem = strmid( image_name, 0, period )

for j=0,n_elements(inout)-1 do begin
  if j eq 1 and keyword_set(encke) then begin
    mnrad = 133705.
    mxrad = 133780.
    bke = 0
    @r
    rwin = !d.window
  endif
  print, 'Fit ' + gap + inout[j]
  @run_edgefit
  edge = j+1
  savefile = filestem + '.edge' + gap + inout[j]
  redge_cmat = cmat
  save, redge, redge_sigma, redge_cmat, filename=savefile
  dummy = redge_to_linsam_encke( image_name = image_name, gap=gap, edge=edge )
  if j ne n_elements(inout)-1 then wdelete, 8 else begin
    wdelete, rwin
    wdelete, 8
    ;wdelete, 1
  endelse
  if j eq 0 then begin
    window, 5
    !p.multi = [0,1,2]
  endif else begin
    wset, 5
    !p.multi = [1,1,2]
  endelse
  plot_nosci, redge[0,*], redge[1,*], /xs, /ys, $
              xtit='Longitude (!Uo!N)', ytit='Radius (km)'
  if j ne n_elements(inout)-1 then wset, rwin
endfor

print, strtrim(Image_Number,2) + ' / ' + strtrim(n_elements(filenames),2)
if (kk eq 0) then begin
    kuiper=0
    jeffreys=0
    russell=1
    herschel=0
    goto, russell
endif

if (kk eq 1) then begin
    kuiper=0
    jeffreys=1
    russell=0
    herschel=0
    goto, ending
endif

if (kk eq 2) then begin
    kuiper=1
    jeffreys=0
    russell=0
    herschel=0
    goto, kuiper
endif

ending:
print, 'You should now exit idl before making more edge profiles.'

end
