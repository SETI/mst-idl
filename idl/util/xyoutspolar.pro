; Compensate for the inability of xyouts to plot in polar coordinates.  

pro xyoutspolar, r, theta, string, align=align, charsize=charsize, charthick=charthick, text_axes=text_axes, width=width, clip=clip, color=color, data=data, device=device, normal=normal, font=font, orient=orient, noclip=noclip, t3d=t3d, z=z

xyouts, r*cos(theta), r*sin(theta), string, align=align, charsize=charsize, charthick=charthick, text_axes=text_axes, width=width, clip=clip, color=color, data=data, device=device, normal=normal, font=font, orient=orient, noclip=noclip, t3d=t3d, z=z

end
