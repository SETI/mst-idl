function keyword_exists, var

; Returns positive if var exists, even if it is zero.  By contrast, 
; keyword_set returns positive only if var exists *and* is non-zero.

return, fix( (size(var))[1] ne 0 )

end