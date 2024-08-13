/*******************************************************************
**$File ucac2_subroutines_c_wrappers.c

*********************************************************************/

/* OSF setup */

#include "c_type_def.h"



/*
** Declare the string structure used in IDL
*/

typedef struct {                /* Define string descriptor */
  unsigned short slen;          /* Length of string, 0 for null */
  short stype;                  /* type of string, static or dynamic */
  char *s;                      /* Addr of string */
} STRING;

#include <string.h>
#include <stdio.h>



/*
** Define a C macro that will return the length of an IDL String
*/

#define STR_LEN(__str)    ((idl_long)(__str)->slen)

/*
****************************************************************************
**$Procedure qmul_u2access
*/
idl_int qmul_u2access(argc, argv)
    idl_int   argc;
    idl_byte  *argv[];

{
  STRING * pathu;
  double * newep;
  double * ra;
  double * dec;
  double * w;
  double * mag1;
  double * mag2;
  idl_long ** output;
  idl_long * nss;

  idl_byte * temp1;

/*
** Quick error checking
*/

  if(argc != 9)
    return(1);

/*
** Do the casts
*/

  pathu=(STRING*)argv[0];
  newep=(double*)argv[1];
  ra=(double*)argv[2];
  dec=(double*)argv[3];
  w=(double*)argv[4];
  mag1=(double*)argv[5];
  mag2=(double*)argv[6];
  output=(idl_long**)argv[7]; 
  nss=(idl_long*)argv[8];

  temp1=strdup(pathu->s);


/*
** Make the call
*/

  qmul_u2access_(temp1,newep,ra,dec,w,mag1,mag2,output,nss,STR_LEN(pathu));

  free((idl_byte*)temp1);
  return(0);

}



/*
****************************************************************************
**$Procedure open_zfile
*/
idl_int open_zfile(argc, argv)
    idl_int   argc;
    idl_byte  *argv[];

{
  STRING * pathz;
  idl_long * un;
  idl_long * zn;
  idl_long * only_rd; 

  idl_byte * temp1;

/*
** Quick error checking
*/

  if(argc != 4)
    return(1);

/*
** Do the casts
*/

  pathz=(STRING*)argv[0];
  un=(idl_long*)argv[1]; 
  zn=(idl_long*)argv[2];
  only_rd=(idl_long*)argv[3];

  temp1=strdup(pathz->s);


/*
** Make the call
*/

  open_zfile_(temp1,un,zn,only_rd,STR_LEN(pathz));

  free((idl_byte*)temp1);
  return(0);

}

