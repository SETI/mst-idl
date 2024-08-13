pro icy_setup,xdum
;
;  set path for SPICE / ICY interface dlm
;
;dlm_register,'/usr/local/CTV_NAIF/ICY/icy.dlm'
dlm_register,getenv("ICY_PATH")+'icy.dlm'
;
end
