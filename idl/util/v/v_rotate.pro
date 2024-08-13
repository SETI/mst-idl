;=============================================================================
;+
; NAME:
;       v_rotate
;
;
; PURPOSE:
;       Rotates the (N,3) column vectors, v, about the (N,3) column vectors,
;       n, by the angles theta.  The sin and cos of theta are given in
;       order to improve performance.
;
;
; CATEGORY:
;       UTIL/V
;
;
; CALLING SEQUENCE:
;       result = v_rotate(v, n, sin_theta, cos_theta)
;
;
; ARGUMENTS:
;  INPUT:
;               v:      An array of N column vectors
;
;               n:      An array of N column vectors
;
;       sin_theta:      Sine of rotation angle theta
;
;       cos_theta:      Cosine of rotation angle theta
;
;
;  OUTPUT:
;       NONE
;
; KEYWORDS:
;           outer:	Forces each vector to be transformed once for each
;			value of theta, even if there happens to be the same
;			number of thetas as vectors.
;
; RETURN:
;       If the arguments have dimensions v(N,3), n(N,3) and sin_theta(M),
;       cos_theta(M) then the result has dimensions (N,3,M).  However, if
; 	N=M and keyword "outer" is not set, then each theta is assigned to 
;	only one vector, and the result has dimensions (N,3).
;
;
; RESTRICTIONS:
;       v and n must have exactly the same dimensions.
;       sin_theta and cos_theta must be 1-dimensional arrays of any length
;       as long as the lengths are the same.  Note that if only one theta
;       is specified, the arguments must be given as [sin_theta], [cos_theta]
;       instead of as scalars.
;	** NOTE:  This routine rotates the coordinate axes, not the vector
;	itself.  In other words, if you simply want to rotate one vector
;	about another, rather than transforming coordinates, then specify
;	the opposite direction from what you want. **
;
; STATUS:
;       Completed.
;
;
; MODIFICATION HISTORY:
;       Written by:     Spitale
;	Modified by: 	Tiscareno, 6/2001
;
;-
;=============================================================================
function v_rotate, v, n, sin_theta, cos_theta, outer=outer

; In Einstein notation, this routine gives the output vprime as 
; vprime[i] = v[i]*cos(theta) + n[i]*n[j]*v[j]*(1-cos(theta) + 
;		epsilon[i,j,k]*v[j]*n[k]*sin(theta)

; If the number of thetas is the same as the number of vectors, then assign
; one theta to each vector.
 sz = size(v)
 if n_elements(sin_theta) eq sz[1] and not keyword_set(outer) then begin
   return, v*rebin(cos_theta,sz[1],sz[2]) + $
	n*rebin(v_inner(n,v),sz[1],sz[2])*(1-rebin(cos_theta,sz[1],sz[2])) + $
	v_cross(v,n)*rebin(sin_theta,sz[1],sz[2])
 endif

; Otherwise, transform each vector once for each theta (outer product).
; Keyword "outer" forces this option.
 n_dot_v_x_1_cos_theta = total(n*v, 2)#(1-cos_theta)
 s=size(v)

 r = dblarr(s[1], s[2], n_elements(sin_theta), /nozero)
 MM = make_array(n_elements(sin_theta), val=1.0)
 r[*,0,*] = v[*,0]#cos_theta + (n[*,0]#MM)*n_dot_v_x_1_cos_theta + $
                                   (v[*,1]*n[*,2] - v[*,2]*n[*,1])#sin_theta
 r[*,1,*] = v[*,1]#cos_theta + (n[*,1]#MM)*n_dot_v_x_1_cos_theta + $
                                   (v[*,2]*n[*,0] - v[*,0]*n[*,2])#sin_theta
 r[*,2,*] = v[*,2]#cos_theta + (n[*,2]#MM)*n_dot_v_x_1_cos_theta + $
                                   (v[*,0]*n[*,1] - v[*,1]*n[*,0])#sin_theta

 return, r
end
;===========================================================================
