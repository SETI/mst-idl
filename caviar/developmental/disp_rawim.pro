; If you have already loaded an image called rawim, and you want to display
; it the way @disp does, run @disp_rawim
@restore_stretch1
_rawim = _rawim - min(_rawim)
im=_rawim*248/max(_rawim)
im=byte(im)
@disp
