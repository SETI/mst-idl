

;=============================================================================
;+
; $Id: gethost.pro,v 1.1 2008/08/27 18:04:31 nathan Exp $
; gethost
;
; PURPOSE
;`
;  Determines the host machine type.
;
;'
; CALLING SEQUENCE :
;
;       result=gethost()
;
;
; ARGUMENTS
;  INPUT : NONE
;
;  OUTPUT : NONE
;
;
;
; KEYWORDS 
;  INPUT : NONE
;
;  OUTPUT : status - If no errors occur, status will be zero, otherwise
;                    it will be a string giving an error message.
;
;
;
; RETURN : String indicating the host machine type.
;
;
;
; KNOWN BUGS : NONE
;
;
;
; ORIGINAL AUTHOR : J. Spitale ; 5/96
;-
; $Log: gethost.pro,v $
; Revision 1.1  2008/08/27 18:04:31  nathan
; added cvs keywords
;
;=============================================================================

;===========================================================================
; gethost
;
;===========================================================================
function gethost, status=status, silent=silent

 status=0

 arch=strupcase(!version.arch)
if(NOT keyword_set(silent)) then help,arch
if(NOT keyword_set(silent)) then help,!version.os

;;	http://www-mipl.jpl.nasa.gov/vicar/vic_file_fmt.html
;;	ALLIANT: Alliant FX series computer. 
;;	CRAY: Cray (port is incomplete, and Cray format is not yet supported). 
;;	DECSTATN: DECstation (any DEC MIPS-based RISC machine) running Ultrix. 
;;	HP-700: HP 9000 Series 700 workstation. 
;;	MAC-AUX: Macintosh running A/UX. 
;;	MAC-MPW: Macintosh running native mode with Mac Programmers Workbench. 
;;	SGI: Silicon Graphics workstation. 
;;	SUN-3: Sun 3, any model. 
;;	SUN-4: Sun 4 or SPARCstation, or clone such as Solbourne. 
;;	TEK: Tektronix workstation. 
;;	VAX-VMS: VAX running VMS.
 if(strpos(arch, 'MIPSEB') NE -1) then return, 'SGI'
 if(strpos(arch, 'SPARC') NE -1) then return, 'SUN-SOLR'
 if(strpos(arch, 'VAX') NE -1) then return, 'VAX_VMS'
 if(strpos(arch, 'ALPHA') NE -1) then return, 'DECSTATN'
 if(strpos(arch, 'X86') NE -1 AND strpos(strupcase(!version.os), 'LINUX') NE -1) then return, 'X86-LINUX'
 if(strpos(arch, 'X86') NE -1 AND strpos(strupcase(!version.os), 'WIN32') NE -1) then return, 'X86-NT'
 if(strpos(arch, 'PPC') NE -1) then return, 'PPC'		; PowerPC Macintosh
 if(strpos(arch, 'I386') NE -1) then return, 'I386'		; Intel Macintosh
 if(strpos(arch, 'X86_64') NE -1) then return, 'I386'		; Intel Macintosh

 status='Unrecogonized architecture.'
 return, ''
end
;===========================================================================
