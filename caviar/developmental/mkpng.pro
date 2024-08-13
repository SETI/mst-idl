
	pro mkpng

	spawn,'ls *.IMG',list

	num_images=n_elements(list)

	for i=0,num_images-1 do begin

                ; Load the COMMON blocks CalibrateGuiCommon 
                ; and CalibrateGuiOpts. 
                ; Also load the current version of Cisscal 
                ; in the variable CisscalVers
                @cisscal_common_caviar
                DebugFlag = 0

                ImageObj = OBJ_NEW('CassImg')
		ImageObj->ReadVic,list[i]
		im=ImageObj->Image()
		im=byte(im)
		label=ImageObj->LabelArray()

		fileout=strmid(list[i],0,14)+'PNG'

		write_png,fileout,im,/order

	endfor

	return
	end
