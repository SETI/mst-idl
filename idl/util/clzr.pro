pro clzr,fname
; this routine closes any open postscript file and if fname is given
; it will send that file to the line printer

common device,old

npar=n_params()

device,/close
set_plot,old


if (npar eq 1) then begin
   fnname = fname+'.ps'
   spawn,'lpr '+fnname
   print,fnname,' has been sent to the laser printer'
endif


end

