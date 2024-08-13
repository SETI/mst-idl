pro fsine, x, a, f, pder

  f = a[0] + a[1] * sin( a[2]*x + a[3] )
  pder = [ [ replicate( 1, n_elements(x) ) ], $
           [ sin( a[2]*x + a[3] ) ], $
           [ a[1] * x * cos( a[2]*x + a[3] ) ], $
           [ a[1] * cos( a[2]*x + a[3] ) ] ]

end
