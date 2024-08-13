

OPT_FTN_LIBS =  -L/save/usr/lib/gcc-lib/i386-redhat-linux/3.3.2/ -lg2c -lm

C_FLAGS =  -c -fPIC

LD_FLAGS = -shared

F_FLAGS = -c -fPIC -fno-second-underscore



# Declare the makefile "all" target, used to test the C only routines.

main:   icyN0057_ucac2_subroutines_c_wrappers.o ucac2_subroutines.o open_zfile.o
	ld $(LD_FLAGS) -o ucac2_subroutines.so  icyN0057_ucac2_subroutines_c_wrappers.o  \
	 ucac2_subroutines.o open_zfile.o $(OPT_FTN_LIBS)


ucac2_subroutines.o: 
	f77 -c -fPIC -fno-second-underscore ucac2_subroutines.f

open_zfile.o: 
	f77 -c -fPIC -fno-second-underscore open_zfile.f

ucac2_subroutines_c_wrappers.o: 
	gcc -c -fPIC ucac2_subroutines_c_wrappers.c



