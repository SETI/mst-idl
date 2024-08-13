; Compensate for the inability of arrow to plot in polar coordinates.  

pro arrowpolar, r0, theta0, r1, theta1, data=data, normal=normal, hsize=hsize, color=color, hthick=hthick, solid=solid, thick=thick

arrow, r0*cos(theta0), r0*sin(theta0), r1*cos(theta1), r1*sin(theta1), data=data, normal=normal, hsize=hsize, color=color, hthick=hthick, solid=solid, thick=thick

end
