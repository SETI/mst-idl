FUNCTION UnwrapBah,  badimg,  jumpthresh=jumpthresh,   $
	mesas=mesas, jumps=jumps
	
; Fix 8LSB images ruined by byte overflow
; Apply this to the raw data out of a Cassini Vicar
; Do not calibrate, subtract bias, etc!
;
; INPUT
;	badimg: raw 8LSB data from vicar file
;
;	jumpthresh: sets how big jump must be to counts as a rollover
;          50-100 is good range; default is 64(?)
; 
; OUTPUT
;	mesas1, mesas2 are test outputs, only for development
; 

; RETURN
;	a supposedly valid 12-bit image 
;
; contact: daren wilson, CICLOPS,   darenw@ciclops.org
; 



; ---- FIND THE IMAGE SIZE - ASSUME width=height ----
img = reform(badimg)
w = (size(img))[1]
ones = replicate(1., w)   ; handy for later



; ---- DETECT THE MISSING LINES ----
missing=img eq 0    ; found all zeros
; missing[] is polluted with accidental zeros in image, perhaps as wrapped DN=256*n
; use 1D median on each row to preserve only solid runs of missing pixels
; and then snuff the crud appearing along left side of image due to incomplete fitlering
; (for the NAC, this affects the evil dark strip area)
for y=0,w-1 do missing[*,y] = median(missing[*,y], 7)
missing[0:6,*]=0
; find leftmost pixel in each run of missing pixels
missingstarts =   missing and not shift(missing,1,0)


; ==== DETECT WRAPPED-AROUND REGIONS ====
; Find suspected rollover transitions where DN jumps by large amount
; dns[], ups[] are where rollovers occur, indicate at pixel to right of jump
; we examine DN values, not adjacent differences - seems to work better, but i can't prove it
if not keyword_set(jumpthresh) then jumpthresh=64
ups = (img gt 255-jumpthresh) and (shift(img,1,0) lt jumpthresh)
dns = (img lt jumpthresh) and (shift(img,1,0) gt 255-jumpthresh)
jumps = ( fix(dns)-fix(ups) ) *(1-missingstarts) 
; jumps marks borders of regions to fix.   +1=entering rolledover area, -1=leaving


; ==== CONSTRUCT CORRECTIVE MESAS ====
; First guess at corrective mesas by integrating jumps along x
; If we are very lucky we just add 256*mesas to raw data to make fixed image
; but in real life, jumps[] contains a lot of bogus info
mesas = total( jumps, 1, /cum)

; Second version of corrective mesas by destreaking our first guess
; This fixes most of the ugliness caused by flaky data in jumps[]
; This one-lines is similar in concept to 2hz noise removal
mesas = mesas - ones # min(mesas, dim=1)



; =====  FIX THE IMAGE WITH FIRST-TRY MESAS ============
; Use these improved mesas to fix the image 
; Corrective mesa is not applied to missing pixels; these remain zero
fixed = img + (1-missing)*mesas*256.

; Fill in missing lines, so we can detect streaks w/o lots of false alarms
; For this step,  minimum-neighbor is better than averaging the pixels above & below
; (averaging would create values +/-128 rather than +/-256 rel to neighbor pixels)
filled = fixed*(1-missing) + missing*(shift(fixed,0,1) < shift(fixed,0,-1))



; ===== SECOND REPAIR ======
; Still under influence of some bogus jumps  resulting in horiz bright streaks
; (maybe dark streaks too?)
; This works only if streaks are isolated, only a few pixels thick, not densely spaced
streaks = filled gt (120+smooth(filled,21))
; use median filter to snuff out anything not a solid horizontal streak
for y=0,w-1 do   streaks[*,y]=median(streaks[*,y], 15)

; Remove streaks from the mesas, then re-fix and re-fill the image
mesas = (1-missing)*(mesas -streaks )
fixed = img + mesas*256.
filled = fixed*(1-missing) + missing* ( shift(fixed,0,1) < shift(fixed,0,-1) )




; ===== THIRD REPAIR ======
; Some test images show remaing bright streaks, so perform streak repair once more
; This time, because of high confidence of good data above and below each row
; of missing pixels, we take the average rather than the minimum.  
streaks = filled gt (120+smooth(filled,21, /edge))
for y=0,w-1 do   streaks[*,y]=median(streaks[*,y], 15)
; this time, when the median filter misses blips at either side, they'll get fixed
fixed = filled - streaks*256.
filled = fixed*(1-missing) + missing*0.5*(shift(fixed,0,1) + shift(fixed,0,-1))

return, fix(filled)

END

