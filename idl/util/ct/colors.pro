; The colors from the original ct routines:

; Identify the colors by the number subtracted from the table size in 8-bit
clrs = [ '', 'white', 'purple', 'yellow', 'blue', 'green', 'red', 'cyan' ]

; The 24-bit color indices
ind24 = [ 0l, 16777215, 16711935, 65535, 16711680, 65280, 255, 16776960 ]

; Displaying these numbers in binary, it is clear that there are three blocks
; of 8 bits.  From left to right: blue, green, red.  To turn on a particular
; color, all 8 bits in its block are set to 1.
for i=0,7 do print, f='(A25)', dec_to_bin( ind24[i], /returnstring )
;                        0
; 111111111111111111111111  white  = B + G + R
; 111111110000000011111111  purple = B +     R
;         1111111111111111  yellow =     G + R
; 111111110000000000000000  blue   = B
;         1111111100000000  green  =     G
;                 11111111  red    =         R
; 111111111111111100000000  cyan   = B + G

; To make orange, take red plus half of green
print, bin_to_dec( '1000000011111111' )
;       33023.000


end
