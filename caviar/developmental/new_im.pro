function new_im, _rawim, stmin=stmin, stmax=stmax

_rawim = _rawim - min(_rawim)
im=_rawim*248/max(_rawim)
im=byte(im)

return, im

end
