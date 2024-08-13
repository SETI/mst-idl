
	double precision newep,ra,dec,w,mag1,mag2
	character pathu*(255)
	character*(255) command_line
	integer output(30000,4),nss,last,one,two,three,four
	integer five,six,seven,eight,lcpathu

	call getcml(command_line)

	one=index(command_line,',')
	two=index(command_line(one+1:),',')+one
	three=index(command_line(two+1:),',')+two
	four=index(command_line(three+1:),',')+three
	five=index(command_line(four+1:),',')+four
	six=index(command_line(five+1:),',')+five
	seven=index(command_line(six+1:),',')+six
	eight=index(command_line(seven+1:),',')+seven

	read(command_line(one+1:two-1),'(a)')pathu
	read(command_line(two+1:three-1),*)newep
	read(command_line(three+1:four-1),*)ra
	read(command_line(four+1:five-1),*)dec
	read(command_line(five+1:six-1),*)w
	read(command_line(six+1:seven-1),*)mag1
	read(command_line(seven+1:eight-1),*)mag2

	lcpathu=lastnb(pathu)

	call  qmul_u2access(pathu(1:lcpathu),newep,ra,dec,w,mag1,mag2,output,nss)

	do i=1,nss
		write(*,'(i8,1x,i11,1x,i11,1x,i5)')output(i,1),output(i,2),output(i,3),output(i,4)
	enddo

	stop
	end


c********************************************************************************************

	subroutine qmul_u2access(pathu,newep,ra,dec,w,mag1,mag2,output,nss)
C
C  pgf77 -o u2access u2access.f u2sub.f sorti.f
C
c  130104 mwe QMUL pathu,fnxu, fnout, answer changed from 40 character
c  character variables to 50 character variable to allow for long pathnames
c  in fnxu  
C  030417 NZ create
C  030418 NZ finish, test,debug
C  030422 NZ extend interactive options
C  030423 NZ star implement pos., err. update by newep
C  030424 NZ finish pos.update
C  030427 NZ back to 23 items, ep0=1975, zmax, open_zfile
C  030429 NZ bfx flag
C  030428 NZ no "READONLY" in OPEN statements 
C  030529 NZ width of box RA, Dec
C  030530 NZ wrap around 0/24h also for box, fix subr.
C  030601-2 NZ finalize output formats, tests, index DA
C
C  - top level program to access UCAC2 data
C  - extract all items for stars
C      in selected areas (RA, Dec box)
C  - formatted (ASCII) output table, various format options
C
C  use Fortran unit 13 to read index and zone files
C  use Fortran unit 20 for output file (or screen dump)

      IMPLICIT NONE
      INTEGER    zmax, dimj, dima
      PARAMETER (zmax= 288       ! maximal zone number
     .          ,dimj= 25 + 4    ! add updated position and errors
     .          ,dima=  30000)   ! max.numb. stars in box

      INTEGER  alls(dima,dimj)   ! all data all selected star
      INTEGER  nx (zmax,240)     ! index = accumul.numb.stars f(zone,RA bin)
      INTEGER  fmt

      INTEGER  i,j,jp,is,zn, nsr,nss,errc, uo, rah,ram,dcd,dcm, un
	integer output(dima,4)
      	double precision   ra1,ra2, dc1,dc2, ra0,dc0,ras,dcs, mag1,mag2 
	double precision newep,w,ra,dec
      CHARACTER*(80) fnout, answer, fnxu
      CHARACTER*(*) pathu
      CHARACTER*(1)  a1
      LOGICAL      bf, bfx, was

* default

	fnout = 'u2.tab' 
 	un    = 13                 ! Fortran unit number for zone files
c      	mag1  =  1.0d0
c      	mag2  = 13.0d0
      	fmt   = 2
      	is    = 0
      	was   = .FALSE.    ! width in arcsec, else degree

* check for byte flip
      CALL chk_byte_flip (pathu,un,bf)

* read index file
      jp = INDEX (pathu,' ') - 1

* new read of index file, works better accross platforms:
c      fnxu = pathu(1:jp) // 'u2index.da'
      fnxu = pathu // 'u2index.da'
 
      OPEN (un,FILE=fnxu,ACCESS='direct',RECL=960) ! 240 * 4 byte
      DO zn=1,288
        READ (un,REC=zn,ERR=901) (nx(zn,j),j=1,240)
      ENDDO

      CLOSE(un)
c      WRITE (*,'(a)') 'index read successfully'
      DO zn=1,3
	continue
c        WRITE (*,'(a,6i6)') 'zn, nx...=',zn,(nx(zn,j),j=1,5)
      ENDDO

      CALL nx_byte_flip (nx,zmax,bfx) ! check for byte flip, apply if needed
 
* prepare table output
      IF (fnout.EQ.'s') THEN
        uo = 6
      ELSE
        uo = 20
        OPEN (uo,FILE=fnout,status='unknown')
      ENDIF

* loop boxes 
 101  continue
 
        CALL calc_box (ra,dec,w,ra1,ra2,dc1,dc2)

c	write(*,*) ra1,ra2,dc1,dc2

c        WRITE (*,'(f10.6,1x,f10.6,1x,f10.5,1x,f10.5)') ra1,ra2,dc1,dc2

* output info
c      WRITE (uo,'(/a, f10.3)') 'epoch      = ',newep
c      WRITE (uo,'( a, i10  )') 'sort item  = ',is 
c      WRITE (uo,'( a, i10  )') 'output fmt = ',fmt
c      WRITE (uo,'( a,2f10.2)') 'mag range  = ',mag1,mag2
c      WRITE (uo,'( a,f10.6,f10.5)') 'min RA, DE = ',ra1,dc1 
c      WRITE (uo,'( a,f10.6,f10.5)') 'max RA, DE = ',ra2,dc2

	ra1=ra1/15.0d0
	ra2=ra2/15.0d0 

* get stars 
      CALL get_stars (bf,ra1,ra2,dc1,dc2,mag1,mag2
     .               ,newep,pathu,nx,zmax,dima,dimj
     .               ,alls,nss,nsr,errc)

c      WRITE (*,'(a,3i7)') 'stars read, slected, errors = ',nsr,nss,errc

* sort
      IF (is.GE.1.AND.is.LE.dimj) THEN
c        WRITE (*,'(a,i3)') 'start sort by column = ',is
        CALL sorti (alls,is,dimj,1,nss,dima,dimj)
      ENDIF

* output
      CALL put_stars (alls,dima,dimj,nss,fmt,uo)
c      WRITE (*,'(a,i6,a)') 'output complete',nss,' stars'

	call populate_output(nss,dima,dimj,alls,output)

	goto 99


* the end
 901	continue  
c	WRITE (*,'(a)') 'error in reading index file'
      STOP

  99  continue
 
      END  ! subr. <u2access>

************************************************************************

      SUBROUTINE calc_box (ra,dec,w,ra1,ra2,dc1,dc2)
C
c	   w      = width of box in degrees
c	   ra     = right ascension decimal degrees
c	   dec	  = declination decimal degrees
C  output: ra1,ra2= range in RA (decimal degrees)
C          dc1,dc2= range in Declination (decimal degrees)

      IMPLICIT NONE
      double precision  ra,dec,w, ra1,ra2,dc1,dc2
      double precision  degrad, hwr,hwd

      degrad = DATAN (1.0d0) / (4.5d1)    ! degree to radian

      hwr=0.5d0*w/cos(dec*degrad)
      hwd =0.5*w

      ra1 = ra - hwr
      ra2 = ra + hwr
      IF (ra1.LT. 0.0d0) ra1 = ra1 + 360.0d0
      IF (ra2.GT.360.0d0) ra2 = ra2 - 360.0d0

      dc1 = dec - hwd
      dc2 = dec + hwd

      END  ! subr. <calc_box>

************************************************************************

      SUBROUTINE get_stars (bf,ra1,ra2,dc1,dc2,mag1,mag2
     .                     ,newep,pathu,nx,zmax,dima,dimj
     .                     ,alls,nss,nsr,errc)
C
C  get all items from all stars within specified RA,Dec box
C
C  input : bf       = .TRUE. if flip of bytes is required
C          ra1,ra2  = RA range (hours, decimal)
C          dc1,dc2  = declination range (degrees, decimal)
C          mag1,mag2= magnitude rane (UCAC 1/100 mag)
C          newep    = requested epoch for positions (year)
C          pathu    = path name for UCAC2 zone files (max. 40 char.)
C          nx       = index array = accumulated numb.stars f(zone, RA bin)
C          zmax     = largest zone number (0.5 deg steps from South Pole)
C          dima     = max. number of stars to retrieve
C          dimj     = number of items per star incl. updated pos.+error
C
C  output: alls     = all items for all retrieved stars
C          nss      = number of stars selected = within specified box
C          nsr      = number of star records read
C          errc     = number of errors while reading zone files

      IMPLICIT NONE
      LOGICAL  bf
      REAL*8   ra1,ra2,dc1,dc2, mag1,mag2, newep
      CHARACTER*(*) pathu
      INTEGER  zmax,dima,dimj, nb, nsr,nss,errc
      INTEGER  nx (zmax,240)     ! accumul.numb.stars = f(zone,RA bin)
      INTEGER  alls(dima,dimj)   ! all integer data each star

      INTEGER  idat(25)          ! data items individual star

      INTEGER  i,j,ju,zn,rr,uz, d1m,d2m,z1,z2,nz, mi1,mi2
     .        ,r1m(2),r2m(2),i1(2),i2(2),nr, j1,j2, recn,n0
     .        ,ran,dcn, sxn,syn
 
      LOGICAL  errflg, only_rd

* prepare
      only_rd = .TRUE.           ! no write to zone files
      mi1 = IDNINT (mag1 * 1.0d2)
      mi2 = IDNINT (mag2 * 1.0d2)

* calculate range 
      CALL get_zone_range (dc1,dc2,zmax,d1m,d2m,z1,z2,nz)

      IF (nz.LT.1) THEN
        nss = 0
        RETURN                   ! exit, declination invalid
      ENDIF

      CALL get_ra_range (ra1,ra2,r1m,r2m,i1,i2,nr)

* initialize
      uz = 13    ! unit number for zone files
      nsr = 0
      nss = 0 
      errc= 0
      ju = INDEX (pathu,' ') - 1

* loop all zones
      DO zn = z1,z2
c        WRITE (*,'(a,i3)') 'open file for zone = ',zn 
        CALL open_zfile (pathu,uz,zn,only_rd)

        IF (zn.EQ.1) THEN
          n0 = 0              ! no stars in "previous" zone
        ELSE
          n0 = nx (zn-1,240)  ! = numb.of stars up to end of previous zone
        ENDIF

* rr    = 1 or 2     = numb. of RA ranges, normal=1, else crossover 24/0 hour
* i1,i2 = 1,...,240  = bin number (0.1 hour) along RA
* j1,j2 = 1,... many = record number on zone file (1 record = 1 star)

        DO rr = 1,nr          ! 1 or 2 RA ranges possible
          IF (i1(rr).EQ.1) THEN
            j1 = 1            ! start with first star on file
          ELSE
            j1 = nx (zn,i1(rr)-1) - n0 + 1   ! i1-1 = previous bin
          ENDIF

          j2 = nx (zn,i2(rr)) - n0 

CC        WRITE (*,'(a,i3,i9,3i5,2i9)') 
CC   .     'zn, n0, rr,i1,i2, j1,j2 = ',zn,n0,rr,i1(rr),i2(rr),j1,j2

          DO recn= j1,j2
            CALL read_u2line (uz,recn,bf,idat,errflg) 

            IF (errflg) THEN
              errc = errc + 1  ! count read errors

            ELSE
              nsr = nsr + 1    ! count number of stars read
              IF (idat(3).GE.mi1.AND.idat(3).LE.mi2) THEN   ! mag range
              IF (idat(1).GE.r1m(rr).AND.idat(1).LE.r2m(rr).AND.
     .            idat(2).GE.d1m.AND.idat(2).LE.d2m) THEN
                nss = nss + 1
                IF (nss.GT.dima) THEN
		continue
c                  WRITE (*,'(/a,i8)') 'WARNING: too many stars, take first dima =',dima
                  RETURN
                ENDIF

                DO j=1,25     ! current version: only 23 items on file
                  alls (nss,j) = idat(j)    ! still keep data structure
                ENDDO
* here generate on the fly, instead read from file:
                alls (nss,24) = n0 + recn   ! official UCAC2 star ID

                CALL apply_pm (idat,newep, ran,dcn,sxn,syn)
                alls (nss,26) = ran
                alls (nss,27) = dcn
                alls (nss,28) = sxn
                alls (nss,29) = syn

              ENDIF    ! within box
              ENDIF    ! within mag range
            ENDIF      ! case read o.k. or error

          ENDDO        ! all stars within a zone
        ENDDO          ! 1 or 2 RA ranges 
      ENDDO            ! all zones

      END   ! subr. <get_stars>

************************************************************************

      SUBROUTINE put_stars (alls,dima,dimj,nss,fmt,uo)
C
C  input : alls  = array with selected stars
C          dima  = max.numb. of stars
C          dimj  = number of items per star
C          nss   = number of selected stars
C          fmt   = format modus for output (= 1 to 4 here)
C          uo    = Fortran unit number for output file
C  output : items to file with unit number uo (can be screen)

      IMPLICIT NONE
      INTEGER  dima,dimj,nss, fmt,uo
      INTEGER  alls(dima,dimj)

      INTEGER  i,j
      REAL*8   mag, epr,epd, pmr,pmrc,pmd, degrad, cosdec
     .        ,ran,dcn, magj, magh, magk, epmr,epmd, qra,qde
      CHARACTER*13 cra,cdc

c	write(*,*)' '

      degrad = DATAN (1.0d0) / (4.5d1)   ! degree to radian

* header line

c        WRITE (uo,'(/3a)') '   RA (deg)    DE (deg)'
c     .   ,' U2mag eRA eDE     epRA     epDC    pm RA'
c     .   ,' pmRAcD   pmDE epmR epmD UCAC2 ID'


* table with data
      DO i=1,nss
          ran = alls(i,26) / 3.6d6       ! updated mas -> degree
          dcn = alls(i,27) / 3.6d6       ! updated mas -> degree
          mag = alls(i, 3) * 1.0d-2
          epr = alls(i,10) * 1.0d-3 + 1975.0d0
          epd = alls(i,11) * 1.0d-3 + 1975.0d0
          cosdec = DCOS (dcn * degrad)
          pmr = alls(i,12) * 0.1d0       ! 0.1 mas/yr -> mas/yr
          pmrc= pmr * cosdec
          pmd = alls(i,13) * 0.1d0
          epmr= alls(i,14) * 0.1d0
          epmd= alls(i,15) * 0.1d0

c          WRITE (uo,'(f11.7,f12.7,f6.2,2i4,2f9.3
c     .               ,f9.1,2f7.1,2f5.1,i9.8)') 
c     .      ran,dcn,mag, alls(i,28),alls(i,29), epr,epd
c     .     ,pmr,pmrc,pmd, epmr,epmd, alls(i,24)

c          WRITE (*,'(f11.7,f12.7,f6.2,2i4,2f9.3
c     .               ,f9.1,2f7.1,2f5.1,i9.8)') 
c     .      ran,dcn,mag, alls(i,28),alls(i,29), epr,epd
c     .     ,pmr,pmrc,pmd, epmr,epmd, alls(i,24)



      ENDDO   ! all stars
    
      END   ! subr. <put_stars>

************************************************************************

      SUBROUTINE pos_up (ra,dc,pmra,pmdc,newep, ran,dcn)
C
C  input : ra, dc    = RA, Dec (mas) at J2000 epoch
C          pmra,pmdc = proper motion RA, Dec (0.1 mas/yr)
C                      no cos(Dec) for pmra, i.e. large values near pole
C          newep     = new epoch (year)
C  output: ran,dcn   = RA, Dec (mas) at new epoch

      IMPLICIT NONE
      INTEGER  ra,dc,pmra,pmdc,ran,dcn
      REAL*8   newep, dt, dra, ddc

      dt  = newep - 2000.0d0
      dra = DFLOAT(pmra) * 0.1d0 * dt
      ddc = DFLOAT(pmdc) * 0.1d0 * dt
 
      ran = ra + IDNINT (dra)
      IF (ran.GT.1296000000) ran = ran - 1296000000   ! 24 hour in mas
      IF (ran.LT.         0) ran = ran + 1296000000   ! restrict to 0..24 range

      dcn = dc + IDNINT (ddc)     ! assume no jump over pole

CC    WRITE (20,'(a,f9.3,2f9.1)') 'pos_up: dt,dra,ddc = ',dt,dra,ddc

      END    ! subr. <pos_up>

************************************************************************

      SUBROUTINE pos_error (sigx,sigy,cepx,cepy,spmx,spmy,newep
     .                     ,sxn,syn)
C
C     x = RA * cosDec,  y = Dec  component
C
C input : sigx, sigy = position error (mas) at central epoch
C         cepx, cepy = central epoch (1/1000 yr from 1975)
C         spmx, spmy = error of proper motion component (0.1 mas/yr)
C         newep      = requested epoch for position error (year)
C output: sxn, syn   = position error at new epoch (mas)

      IMPLICIT NONE
      INTEGER  sigx,sigy, spmx,spmy, cepx,cepy, sxn,syn
      REAL*8   newep, dtx,dty

      dtx = newep - DFLOAT(cepx) * 1.0d-3 - 1975.0d0
      dty = newep - DFLOAT(cepy) * 1.0d-3 - 1975.0d0

      sxn = IDNINT (DSQRT (sigx**2 + (spmx * 0.1d0 * dtx)**2))
      syn = IDNINT (DSQRT (sigy**2 + (spmy * 0.1d0 * dty)**2))

CC    WRITE (20,'(a,2f9.3)') 'pos_error: dtx,dty = ',dtx,dty

      END   ! subr. <pos_error>

************************************************************************

      SUBROUTINE apply_pm (idat,newep, ran,dcn,sxn,syn)
C
C input : idat = integer vector with original UCAC2 data for 1 star
C         newep= requested new epoch (year)
C output: ran, dcn = new position, updated for proper motion (mas)
C         sxn, syn = error of new position (at new epoch) (mas)

      IMPLICIT NONE
      INTEGER  idat(25), ran,dcn,sxn,syn
      REAL*8   newep

      CALL pos_up (idat(1),idat(2),idat(12),idat(13),newep, ran,dcn)

      CALL pos_error (idat( 4),idat( 5), idat(10),idat(11)
     .               ,idat(14),idat(15), newep, sxn,syn)

      END   ! subr. <apply_pm>

************************************************************************
*  subroutines used for UCAC2 release utility programs
*
*  open_zfile    : open direct access, unformatted zone file
*  read_u2line   : read single record = all items for a star
*  cat_id        : identify proper motion catalogs from flag
*  flip2         : flip 2 byte integer
*  flip4         : flip 4 byte integer
*  valid_range   : restrict R*8 data item to given min,max 
*  get_zone_range: declination range --> required zone numbers
*  get_ra_ragne  : RA range --> required index for 0.1h bins
*  chk_byte_flip : read first record of z001, is a byte flip required?
*  nx_byte_flip  : check / apply byte flip on index array
*  count_id      : count proper motion catalogs from flag
*  as2hms        : convert arcsec (RA,Dec) into hms format
*
*  030528 change "rflg" to "epos", remove "READONLY" option in OPEN
*  030529 fix RA range 24/0h
*
************************************************************************

************************************************************************

      SUBROUTINE read_u2line (un,recn,bf,idat,errflg)
C
C  read a single record of UCAC2 data = 1 star
C  input:
C    un   = unit number of file  (assumed to be open)
C    recn = record number on that file
C    bf   = .TRUE. if byte flip required
C  output:
C    idat = integer*4 vector of 23 items (see readme2.txt)
C    errflg = true  if error occured (like end of file)
C             else false

      IMPLICIT NONE
      INTEGER  un,recn, idat(25)  ! item #24,25 = star ID options
      LOGICAL  bf, errflg 

      INTEGER   ra2000, dc2000, pmx,pmy, id2m, u2id,r11
      INTEGER*2 mag, cepx,cepy, j2m,h2m,k2m
      BYTE      sigx,sigy,nobs,epos,ncat,cflg    ! INTEGER*1
     .         ,spmx,spmy, rx,ry, ph,cc          ! signed integer

      errflg = .FALSE.     ! default

CC    READ (un,REC=recn,ERR=99) ra2000,dc2000
CC   .  ,mag,sigx,sigy, nobs,rflg,ncat,cflg
CC   .  ,cepx,cepy, pmx,pmy, spmx,spmy, rx,ry
CC   .  ,id2m, j2m,h2m,k2m, ph,cc, u2id,r11   ! incl. ID numbers

      READ (un,REC=recn,ERR=99) ra2000,dc2000
     .  ,mag,sigx,sigy, nobs,epos,ncat,cflg
     .  ,cepx,cepy, pmx,pmy, spmx,spmy, rx,ry
     .  ,id2m, j2m,h2m,k2m, ph,cc

      IF (bf) THEN
        CALL flip4 (ra2000)
        CALL flip4 (dc2000)
        CALL flip2 (mag)
        CALL flip2 (cepx)
        CALL flip2 (cepy)
        CALL flip4 (pmx)
        CALL flip4 (pmy)
        CALL flip4 (id2m)
        CALL flip2 (j2m)
        CALL flip2 (h2m)
        CALL flip2 (k2m)
CC      CALL flip4 (u2id)
CC      CALL flip4 (r11)
      ENDIF

* note: first assign I*1 to idat(I*4), 
*       then add 127 to avoid overflow

      idat ( 1) = ra2000
      idat ( 2) = dc2000
      idat ( 3) = mag
      idat ( 4) = sigx
      idat ( 4) = idat ( 4) + 127
      idat ( 5) = sigy
      idat ( 5) = idat ( 5) + 127
      idat ( 6) = nobs
      idat ( 7) = epos 
      idat ( 7) = idat ( 7) + 127
      idat ( 8) = ncat
      idat ( 9) = cflg
      idat (10) = cepx
      idat (11) = cepy
      idat (12) = pmx
      idat (13) = pmy
      idat (14) = spmx
      idat (14) = idat (14) + 127
      idat (15) = spmy
      idat (15) = idat (15) + 127
      idat (16) = rx
      idat (16) = idat (16) + 127
      idat (17) = ry
      idat (17) = idat (17) + 127
      idat (18) = id2m
      idat (19) = j2m
      idat (20) = h2m
      idat (21) = k2m
      idat (22) = ph 
      idat (22) = idat (22) + 127
      idat (23) = cc
      idat (23) = idat (23) + 127
CC    idat (24) = u2id  ! option for test runs
CC    idat (25) = r11   ! including cross references
      idat (24) = 0     ! here don't use item 24,25
      idat (25) = 0     ! but keep data structure
      RETURN

 99   errflg = .TRUE.
      END   ! subr. <read_u2line>

************************************************************************

      SUBROUTINE cat_id (cflg,icat)
C
C  input : cflg   = combined flag for catalog ID's
C  output: icat(7)= 1 if catalog (1 to 7) is included, else 0

      IMPLICIT NONE
      INTEGER  cflg, icat(7), cc, j

      cc = cflg

      DO j=1,7
        icat(j) = 0
      ENDDO

      IF (cc.GE.64) THEN         ! USNO-A2
        icat(7) = 1
        cc = cc - 64
      ENDIF

      IF (cc.GE.32) THEN         ! NLTT
        icat(6) = 1
        cc = cc - 32
      ENDIF

      IF (cc.GE.16) THEN         ! Hipparcos
        icat(5) = 1
        cc = cc - 16
      ENDIF

      IF (cc.GE.8) THEN          ! AGK2 
        icat(4) = 1
        cc = cc - 8
      ENDIF

      IF (cc.GE.4) THEN          ! Tycho-2
        icat(3) = 1
        cc = cc - 4
      ENDIF

      IF (cc.GE.2) THEN          ! AC2000
        icat(2) = 1
        cc = cc - 2
      ENDIF

      IF (cc.EQ.1) icat(1) = 1   ! YS

      END  ! subr. <cat_id>

************************************************************************

      SUBROUTINE flip2 (i2)
C
C input:  Integer*2 value i2
C output: same with byte fliped

      IMPLICIT NONE
      INTEGER*2  i2, in, out
      BYTE       a(2), b(2)
      EQUIVALENCE (in,a)
      EQUIVALENCE (out,b)

      in = i2
      b(1) = a(2)
      b(2) = a(1)
      i2 = out

      END    ! subr. <flip2>
 
************************************************************************

      SUBROUTINE flip4 (i4)
C
C input:  Integer*4 value i4
C output: same with byte fliped

      IMPLICIT NONE
      INTEGER*4  i4, in, out
      BYTE       a(4), b(4)
      EQUIVALENCE (in,a)
      EQUIVALENCE (out,b)

      in = i4
      b(1) = a(4)
      b(2) = a(3)
      b(3) = a(2)
      b(4) = a(1)
      i4 = out

      END    ! subr. <flip4>

************************************************************************

      SUBROUTINE valid_range (data,dmin,dmax)
C
      IMPLICIT NONE
      REAL*8   data, dmin,dmax

      IF (data.LT.dmin) data = dmin
      IF (data.GT.dmax) data = dmax

      END    ! subr. <valid_range>

************************************************************************

      SUBROUTINE get_zone_range (dc1,dc2,zmax, d1m,d2m,z1,z2,nz)
C
C input:  dc1,dc2 = declination range (degree)
C         zmax    = largest zone number available
C output: d1m,d2m = declination range in mas
C         z1, z2  = req. range of zone numbers (0.5 deg steps)
C         nz      = number of zones, or 0 if out of range

      IMPLICIT NONE
      REAL*8  dc1,dc2
      INTEGER zmax, d1m,d2m, z1,z2, nz
      REAL*8  dcx 

      IF (dc1.LT.-90.0d0.AND.dc2.LT.-90.0d0) THEN
        nz = 0
        z1 = 1
        z2 = 0
        RETURN
      ENDIF

      CALL valid_range (dc1,-90.0d0,90.0d0)
      CALL valid_range (dc2,-90.0d0,90.0d0)

      IF (dc1.GT.dc2) THEN     ! flip range
        dcx = dc1
        dc1 = dc2
        dc2 = dcx
      ENDIF

      d1m = IDNINT (dc1 * 3.6d6)    ! declination (mas)
      d2m = IDNINT (dc2 * 3.6d6)

      z1 = (d1m + 324000000) / 1800000 + 1
      z2 = (d2m + 323999999) / 1800000 + 1

      IF (z2.GT.zmax) z2 = zmax

      IF (z1.GT.zmax) THEN      ! out of available zone range
        z1 = zmax + 1
        nz = 0  
      ELSE
        nz = z2 - z1 + 1
      ENDIF

      END   !  subr. <get_zone_range>

************************************************************************

      SUBROUTINE get_ra_range (ra1,ra2, ralo,rahi,i1,i2,nr)
C
C  input:  ra1,ra2   = RA range (hour)
C  output: ralo,rahi = range of RA in mas (1 or 2)
C          i1, i2    = range in index for 0.1 h boxes
C          nr        = number of ranges = 1 or 2
C    2 ranges possible, if ra1 > ra2  (e.g. 23.0, 1.0)
C    assume cross over 24/0 hour in RA --> 2 ranges
C    (like  23.0 ... 24.0  and  0.0 ... 1.0 hour for output)

      IMPLICIT NONE
      REAL*8  ra1,ra2
      INTEGER ralo(2),rahi(2), i1(2),i2(2), nr
      INTEGER r1m,r2m
      REAL*8  rax

      CALL valid_range (ra1, 0.0d0,24.0d0)
      CALL valid_range (ra2, 0.0d0,24.0d0)

      r1m = IDNINT (ra1 * 5.4d7)   ! RA in mas
      r2m = IDNINT (ra2 * 5.4d7)

      IF (r1m.LE.r2m) THEN         ! normal case
        nr = 1
        i1(1) =  r1m    / 5400000 + 1
        i2(1) = (r2m-1) / 5400000 + 1
        i1(2) = 1
        i2(2) = 0
        ralo(1) = r1m
        rahi(1) = r2m
        ralo(2) = 0
        rahi(2) = 0

      ELSE                         ! cross over 24/0
        nr = 2
        i1(1) =  r1m    / 5400000 + 1
        i2(1) = 240
        i1(2) =   1
        i2(2) = (r2m-1) / 5400000 + 1
        ralo(1) = r1m
        rahi(1) = 1296000000       ! 24 hour in mas
        ralo(2) = 0
        rahi(2) = r2m
      ENDIF

      END   ! subr. <get_ra_range>

************************************************************************

      SUBROUTINE chk_byte_flip (pathz,un,bf)
C
C  input : pathz  = path for zone files
C          un     = Fortran unit number for zone file
C  output: bf     = .TRUE. if byte flip is required

      IMPLICIT NONE
      CHARACTER*(*) pathz
      INTEGER       un, zn, idat(25)
      LOGICAL       bf, bft, errflg, only_rd
      CHARACTER*40 fnz, a1*1
      INTEGER*2    mag

      only_rd = .TRUE.
      zn = 1

c      WRITE (*,'(/a)') 'open first zone, read first record'

      CALL open_zfile (pathz,un,zn,only_rd)

      bft = .FALSE.                 ! first test with no byte flip

      CALL read_u2line (un,1,bft,idat,errflg)
      CLOSE (un)

c      WRITE (*,'(a,i6)') 'mag of first star = ',idat(3)

      IF (idat(3).EQ.1591) THEN     ! magnitude of first star
        bf = .FALSE.
c        WRITE (*,'(/a)') '-- no byte flip required'

      ELSE
        mag = idat(3)
        CALL flip2 (mag)
        IF (mag.EQ.1591) THEN
c          WRITE (*,'(/a)') '** byte flip is required, will do'
          bf = .TRUE.
        ELSE
c          WRITE (*,'(/a)') '** WARNING: byte flip test inconclusive'
          bf = .FALSE.
        ENDIF
      ENDIF

      END   ! subr. <chk_byte_flip>

************************************************************************

      SUBROUTINE nx_byte_flip (nx,zmax,bf)
C
C input : nx  = array with index
C         zmax= dimension of nx, max. number of zones
C output: nx  = same with byte flip applied (if required)
C         bf  = .TRUE. if byte flip was applied

      INTEGER zmax
      INTEGER nx (zmax,240)
      LOGICAL bf
      INTEGER zn,j, i4
      CHARACTER*1 a1

      IF (nx(1,1).EQ.2) THEN
c        WRITE (*,'(a)') 'index array: no byte flip required'
        bf = .FALSE.

      ELSEIF (nx(1,1).EQ.33554432) THEN
c        WRITE (*,'(a)') 'index array: byte flip is required'
        bf = .TRUE. 
        DO zn= 1,zmax
        DO j = 1,240
          CALL flip4 (nx(zn,j))
        ENDDO
        ENDDO
c        WRITE (*,'(a)') 'index array: byte flip done' 
 
      ELSE
c        WRITE (*,'(a)') 
c     .   'WARNING: index array: byte flip inconclusive'
        bf = .FALSE.
      ENDIF

      END   ! subr. <nx_byte_flip>

************************************************************************

      SUBROUTINE count_id (cflg,ncat)
C
C  input : cflg   = combined flag for catalog ID's
C  output: ncat(7)= count for 7 catalogs coded in cflg

      IMPLICIT NONE
      INTEGER  cflg, ncat(7), cc

      cc = cflg

      IF (cc.GE.64) THEN         ! USNO-A2
        ncat(7) = ncat(7) + 1
        cc = cc - 64
      ENDIF

      IF (cc.GE.32) THEN         ! NLTT
        ncat(6) = ncat(6) + 1
        cc = cc - 32
      ENDIF

      IF (cc.GE.16) THEN         ! Hipparcos
        ncat(5) = ncat(5) + 1
        cc = cc - 16
      ENDIF

      IF (cc.GE.8) THEN          ! AGK2 
        ncat(4) = ncat(4) + 1
        cc = cc - 8
      ENDIF

      IF (cc.GE.4) THEN          ! Tycho-2
        ncat(3) = ncat(3) + 1
        cc = cc - 4
      ENDIF

      IF (cc.GE.2) THEN          ! AC2000
        ncat(2) = ncat(2) + 1
        cc = cc - 2
      ENDIF

      IF (cc.EQ.1) ncat(1) = ncat(1) + 1   ! YS

      END  ! subr. <count_id>

************************************************************************

      SUBROUTINE as2hms (ra,dk,crekt,cdekl)
C
C convert R*8 RA, DC (arcsec) to hms, dms strings
C
C 940725 NZ update to CHARACTER*13 to 1/1000 arcsec

        IMPLICIT REAL*8 (A-H,L-Z)

        REAL*8     RA, DK                            ! added 2-3-93
        INTEGER*4  IRASTD, IRAMIN, IDKGRD,IDKMIN     ! added 2-3-93
        CHARACTER*1  CVZ
        CHARACTER*13 CREKT,CDEKL


C     03. TRANSFORMATIONEN

        IF (RA.GT.1296000.D0)  RA = RA - 1296000.D0
        IF (RA.LT.      0.D0)  RA = RA + 1296000.D0
        IF (RA.GT.1296000.D0.OR.RA.LT.0.D0)  THEN
		continue
c          WRITE (90,'(1X//1X,A,F13.3/)')  'RA > 24 hours or < 0 :',RA
          RETURN
        END IF
        RASTD = RA/(3600.D0*15.D0)
        IRASTD= IDINT(RASTD)
        RAREST= RASTD-DFLOAT(IRASTD)
        IRAMIN= IDINT(RAREST*60.D0)
        RASEC = RAREST*3600.D0-DFLOAT(IRAMIN)*60.D0
        IF (DABS(RASEC-60.D0).LT.0.001D0)  THEN
          RASEC = 0.D0
          IRAMIN= IRAMIN+1
          IF (IRAMIN.EQ.60)  THEN
            IRAMIN= 0
            IRASTD= IRASTD+1
            IF (IRASTD.EQ.24)  IRASTD= 0
          END IF
        END IF

        IF (DABS(DK).GT.324000.D0)  THEN
		continue
c          WRITE (90,'(1X//1X,A,F13.3/)')  'abs (DC)  > 90 deg :',DK
          RETURN
        END IF
        DKGRD = DK/3600.D0
        CVZ= '+'
        IF (DK.LT.0.D0)  THEN
          CVZ= '-'
          DKGRD= -DKGRD
        END IF
        IDKGRD= IDINT(DKGRD)
        DKREST= DKGRD-DFLOAT(IDKGRD)
        IDKMIN= IDINT(DKREST*60.D0)
        DKSEC = DKREST*3600.D0-DFLOAT(IDKMIN)*60.D0
        IF (DABS(DKSEC-60.D0).LT.0.01D0)  THEN
          DKSEC = 0.D0
          IDKMIN= IDKMIN+1
          IF (IDKMIN.EQ.60)  THEN
            IDKMIN= 0
            IDKGRD= IDKGRD+1
          END IF
        END IF

c        WRITE (CREKT,'(   I2.2,1X,I2.2,1X,F7.4)')
c     A    IRASTD,IRAMIN,RASEC
        IF (CREKT(7:7).EQ.' ')  CREKT(7:7)= '0'
c        WRITE (CDEKL,'(A1,I2.2,1X,I2.2,1X,F6.3)')
c     A    CVZ,IDKGRD,IDKMIN,DKSEC
        IF (CDEKL(8:8).EQ.' ')  CDEKL(8:8)= '0'

        RETURN
      END
************************************************************************

      SUBROUTINE SORTI (A,IS,IW,IV,N,KX,KY)
C   
C  sort of 2-dimensional Integer array 
C
C  input: A    = array
C         IS   = column index to sort by
C         IW   = number of columns to sort 
C         IV,N = lower, upper limit on line numbers to sort 
C         KX,KY= dimension of array A (lines,columns) 
C  output:  A  = same array after sort
                                                                                
      DIMENSION A(KX,KY),IL(30),IU(30),T(30),TT(30) 
      INTEGER A,T,TT 
      M=1  
      I=IV 
      J=N  
      II=I 
      GOTO114 
  111 IJ=0.01+0.5*(I+J)  
      DO115IX=1,IW 
  115 T(IX)=A(IJ,IX) 
      K=I 
      L=J 
      IF(A(I,IS).GT.T(IS))THEN 
      DO116IX=1,IW 
      A(IJ,IX)=A(I,IX) 
      A(I,IX)=T(IX)
  116 T(IX)=A(IJ,IX) 
      END IF 
      IF(A(J,IS).LT.T(IS))THEN
      DO117IX=1,IW
      A(IJ,IX)=A(J,IX)
      A(J,IX)=T(IX) 
  117 T(IX)=A(IJ,IX) 
      IF(A(I,IS).GT.T(IS))THEN 
      DO118IX=1,IW 
      A(IJ,IX)=A(I,IX) 
      A(I,IX)=T(IX) 
  118 T(IX)=A(IJ,IX) 
      END IF 
      END IF
  112 L=L-1  
      IF(A(L,IS).GT.T(IS))GOTO112 
      DO119IX=1,IW 
  119 TT(IX)=A(L,IX) 
  113 K=K+1 
      IF(A(K,IS).LT.T(IS))GOTO113 
      IF(K.LE.L)THEN 
      DO120IX=1,IW 
      A(L,IX)=A(K,IX) 
  120 A(K,IX)=TT(IX) 
      GOTO112 
      END IF 
      IF((L-I).GT.(J-K))THEN 
      IL(M)=I 
      IU(M)=L 
      I=K 
      ELSE
      IL(M)=K  
      IU(M)=J 
      J=L  
      END IF 
      M=M+1 
  114 IF(J-I.GT.10)GOTO 111
      IF(I.EQ.II)THEN 
      IF(I.LT.J)GOTO111 
      END IF 
      NI=I+1 
      DO121IZ=NI,J  
      I=IZ 
      DO122IX=1,IW  
  122 T(IX)=A(IZ,IX) 
      K=I-1 
      IF(A(K,IS).GT.T(IS))THEN 
  123 DO124IX=1,IW 
  124 A(K+1,IX)=A(K,IX) 
      K=K-1 
      IF(A(K,IS).GT.T(IS))GOTO123 
      DO125IX=1,IW 
  125 A(K+1,IX)=T(IX)
      END IF 
  121 CONTINUE 
      M=M-1 
      IF(M.GE.1)THEN 
      I=IL(M) 
      J=IU(M) 
      GOTO114 
      END IF  
      RETURN 
      END    ! subr. <sorti>

c*************************************

	subroutine populate_output(nss,dima,dimj,alls,output)

	integer nss,dima,dimj

	integer alls(dima,dimj),output(dima,4)

	do i=1,nss

		output(i,1)=alls(i,24)
		output(i,2)=alls(i,26)
		output(i,3)=alls(i,27)
		output(i,4)=alls(i,3)
	enddo
       
	END    ! subr. <populate_output>

************************************************************************

      SUBROUTINE open_zfile (pathz,un,zn,only_rd)
C
C  input : pathz = path name for zone files
C          un    = Fortran file unit number
C          zn    = zone number = 1, 288
C          only_rd = .TRUE. if only read access

      IMPLICIT NONE
      CHARACTER*(*) pathz
      INTEGER  un,zn, jp
      LOGICAL  only_rd, ifex
      CHARACTER*80 fnzone, answer

      IF (zn.LT.1.OR.zn.GT.288) THEN
c        WRITE (*,'(a,i5)') '<open_zf> invalid zone number = ',zn
        STOP
      ENDIF

      IF (only_rd) THEN               ! read file, check existence
 51     jp = INDEX (pathz,' ') - 1

        WRITE (fnzone,'(a,a,i3.3)') pathz,'z',zn

        INQUIRE (FILE=fnzone,EXIST=ifex)

        IF (.NOT.ifex) THEN
          WRITE (*,'(/a)') 'can not find the file:'
          WRITE (*,'(a)') fnzone
          WRITE (*,'(a)') 'please use correct CD or enter new path:'
          WRITE (*,'(a)') '(exit with "x")'
          READ (*,'(a)') answer
          IF (answer.NE.' ') pathz = answer
          IF (pathz(1:1).EQ.'x'.AND.pathz(2:2).EQ.' ') THEN
            STOP
          ELSE
            GOTO 51
          ENDIF
        ENDIF        ! file does not exist

CC      OPEN (un,FILE=fnzone,ACCESS='direct',RECL=44,READONLY)
        OPEN (un,FILE=fnzone,ACCESS='direct',RECL=44)

      ELSE
        WRITE (fnzone,'(a,a,i3.3)') pathz,'z',zn
        OPEN (un,FILE=fnzone,ACCESS='direct',RECL=44)
      ENDIF ! read or write access

      END   ! subr. <open_zfile>

************************************************************************
C$Procedure      GETCML ( Get the command line )
 
      SUBROUTINE GETCML ( LINE )
 
C$ Abstract
C
C     Return command line arguments in a single string.
C
C$ Disclaimer
C
C     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
C     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
C     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
C     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
C     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
C     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
C     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
C     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
C     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
C     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
C
C     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
C     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
C     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
C     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
C     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
C     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
C
C     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
C     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
C     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
C     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
C
C$ Required_Reading
C
C     None.
C
C$ Keywords
C
C     UTILITY
C
C$ Declarations
 
 
      CHARACTER*(*)         LINE
 
C$ Brief_I/O
C
C     Variable  I/O  Description
C     --------  ---  --------------------------------------------------
C     LINE       O   The command line arguments.
C
C$ Detailed_Input
C
C     None.
C
C$ Detailed_Output
C
C     LINE      is the string containing the command line arguments.
C               The actual command is not contained in the string.
C
C
C$ Parameters
C
C     None.
C
C$ Exceptions
C
C     Error free.
C     This routine does not participate in the SPICELIB error handling
C     mechanism.
C
C     Any other exceptions are unknown at this time.
C
C$ Files
C
C     None.
C
C$ Particulars
C
C     The source code for platforms for which we have no documentation
C     was created as a result of porting the Sun OS/Sun Fortran version
C     of the routine to detemine if it executed successfully on the
C     target platform. This is how we generated the code for the Sun
C     Solaris/Sun Fortran, Silicon Graphics IRIX/SGI Fortran, and
C     DEC Alpha OSF-1/DEC Fortran platforms.
C
C$ Examples
C
C     If inputs is a Fortran program, and the following command is
C     executed from a command line,
C
C       % inputs this is the command line input
C
C     the following string will be returned by GETCML:
C
C       this is the command line input
C
C     The Mac Classic version creates a window into which the user
C     inputs the command line arguments (if any).
C
C$ Restrictions
C
C     None.
C
C$ Literature_References
C
C     1. "FORTRAN/9000 Reference HP 9000 Series 700 Computers,"
C        First Edition, June 1991, Hewlett Packard Company,
C        pages 9-29, 9-30, 18-2 through 18-5.
C
C     2. "Language Guide," accompaniment to Microsoft FORTRAN
C        PowerStation Compiler, version 1.0, 1993, pages 296-297,
C        401-402.
C
C     3. "Absoft FORTRAN 77 Compatibility Libraries,"
C        Absoft Corporation, 1991-1992, pages 3-10 and 3-14.
C
C     4. "Lahey F77L3-EM/32 FORTRAN Programmer's Reference," Lahey
C        Computer Systems, Inc., Revision C, January 1992, page 35.
C
C$ Author_and_Institution
C
C     K.R. Gehringer (JPL)
C     H.A. Neilan    (JPL)
C     M.J. Spencer   (JPL)
C
C$ Version
C
C-    SPICELIB Version 6.2.0, 03-JAN-2005 (BVS)
C
C        Updated for PC-CYGWIN.
C
C-    SPICELIB Version 6.1.1, 17-JUL-2002 (BVS)
C
C        Added MAC-OSX environments.
C
C-    SPICELIB Version 6.1.0, 14-NOV-2001 (EDW)
C
C        Add code to create an input window for arguments
C        under Mac Absoft (Classic) compiler. The enviroment
C        ID is MACPPC.
C
C-    SPICELIB Version 6.0.0, 14-OCT-1999 (WLT)
C
C        The VMS environment was modified to return the command
C        line after applying LCASE.
C
C-    SPICELIB Version 5.0.3, 08-OCT-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitely given.  New
C        environments are WIN-NT
C
C-    SPICELIB Version 5.0.2, 28-JUL-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitely given.  New
C        environments are PC-DIGITAL, SGI-O32 and SGI-N32.
C
C-    SPICELIB Version 5.0.1, 18-MAR-1999 (WLT)
C
C        The environment lines were expanded so that the supported
C        environments are now explicitely given.  Previously,
C        environments such as SUN-SUNOS and SUN-SOLARIS were implied
C        by the environment label SUN.
C
C-    SPICELIB Version 5.0.0, 24-NOV-1998 (WLT)
C
C        Added references for the MAC.
C
C-    SPICELIB Version 4.0.0, 05-APR-1998 (NJB)
C
C        Added references for the PC-LINUX environment.  Added SAVE
C        statements for the variable FIRST.
C
C-    SPICELIB Version 3.2.0, 18-AUG-1995 (KRG) (HAN)
C
C        Added code for the PC-Lahey environment. Increased the size
C        of ARGLEN to 512 as a result of truncation problems on some
C        Unix platforms.
C
C-    SPICELIB Version 3.1.0, 5-JAN-1995 (HAN)
C
C        Removed Sun Solaris environment since it is now the same
C        as the Sun OS 4.1.x environment.
C        Removed DEC Alpha/OpenVMS environment since it is now the
C        same as the VAX environment.
C
C-    SPICELIB Version 3.0.0, 8-JUL-1994 (HAN)
C
C        Added code for the NeXT/Absoft Fortran, HP-UX/HP Fortran,
C        Silicon Graphics (SGI) IRIX/SGI Fortran,
C        Sun Solaris/Sun Fortran, and DEC Alpha OSF-1/DEC Fortran
C        environments.
C
C-    SPICELIB Version 2.0.0, 15-MAR-1994 (HAN) (MJS)
C
C        Added code for the HP and the PC/Microsoft Powerstation
C        environments.
C
C-    SPICELIB Version 1.0.0, 19-NOV-1992 (HAN)
C
C-&
 
C$ Index_Entries
C
C   get command line arguments
C
C-&
 
 
 
C
C     Other functions
C
      INTEGER               IARGC
 
C
C     Local Variables
C
      INTEGER               ARGLEN
      PARAMETER           ( ARGLEN = 512 )
 
      CHARACTER*(ARGLEN)    ARGMNT
 
      INTEGER               HOWMNY
      INTEGER               I
 
      LOGICAL               FIRST
 
      SAVE                  FIRST
 
      DATA                  FIRST  / .TRUE. /
 
C
C     Call the FORTRAN library function iargc to determine how many
C     words are on the command line. Then, get the arguments one at a
C     time and construct the output string.
C
      HOWMNY = IARGC()
      I      = 1
      LINE   = ' '
 
      IF ( HOWMNY .EQ. 0 ) THEN
         RETURN
      END IF
 
      DO WHILE ( I .LE. HOWMNY )
 
         CALL GETARG ( I, ARGMNT )
 
         IF ( FIRST ) THEN
            CALL SUFFIX ( ARGMNT, 0, LINE )
            FIRST = .FALSE.
         ELSE
            CALL SUFFIX ( ARGMNT, 1, LINE )
         END IF
 
         I = I + 1
 
      END DO
 
      RETURN
      END

C$Procedure      SUFFIX (Suffix a character string)
 
      SUBROUTINE SUFFIX ( SUFF, SPACES, STRING )
 
C$ Abstract
C
C      Add a suffix to a character string.
C
C$ Disclaimer
C
C     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
C     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
C     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
C     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
C     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
C     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
C     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
C     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
C     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
C     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
C
C     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
C     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
C     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
C     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
C     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
C     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
C
C     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
C     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
C     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
C     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
C
C$ Required_Reading
C
C     None.
C
C$ Keywords
C
C      ASSIGNMENT,  CHARACTER,  STRING
C
C$ Declarations
 
      CHARACTER*(*)    SUFF
      INTEGER          SPACES
      CHARACTER*(*)    STRING
 
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C      SUFF       I   Suffix.
C      SPACES     I   Number of spaces separating prefix and suffix.
C      STRING    I/O  Prefix on input, string on output.
C
C$ Detailed_Input
C
C      SUFF        is the suffix to be added to the string.
C                  Leading blanks are significant. (A blank
C                  suffix is interpreted as a null suffix.)
C
C      SPACES      is the number of spaces (blanks) in the output
C                  string separating the last non-blank character
C                  of the prefix from the first (blank or non-blank)
C                  character of the suffix. Typically, this will be
C                  zero or one. If not positive, SPACES defaults to
C                  zero.
C
C      STRING      on input is the prefix to which the suffix is
C                  to be added. Leading blanks are significant.
C                  Trailing blanks are ignored.
C
C$ Detailed_Output
C
C      STRING      on output is the suffixed string. If STRING
C                  is not large enough to contain the output string,
C                  the output string is truncated on the right.
C
C                  STRING may NOT overwrite SUFF.
C
C$ Parameters
C
C     None.
C
C$ Particulars
C
C      The suffix is added to the right of the last non-blank character
C      of the prefix. (Any necessary truncation is done automatically.)
C
C$ Examples
C
C      The following examples illustrate the use of SUFFIX.
C
C            SUFF         STRING (input)   SPACES    STRING (output)
C            ----------   --------------   ------    ---------------
C            'abc     '   'def    '             0    'defabc '
C            'abc     '   'def    '             1    'def abc'
C            'abc     '   ' def   '             0    ' defabc'
C            'abc     '   ' def   '             1    ' def ab'
C            ' abc    '   'def    '             0    'def abc'
C            ' abc    '   'def    '             1    'def  ab'
C            ' abc    '   ' def   '            -1    ' def ab'
C            '        '   'def    '             0    'def    '
C            '        '   'def    '             1    'def    '
C            ' abc    '   '       '             0    ' abc   '
C            ' abc    '   '       '             1    '  abc  '
C
C$ Restrictions
C
C      SUFF and STRING must be distinct.
C
C$ Exceptions
C
C      Error free.
C
C$ Files
C
C      None.
C
C$ Author_and_Institution
C
C      W.L. Taber      (JPL)
C      I.M. Underwood  (JPL)
C
C$ Literature_References
C
C      None.
C
C$ Version
C
C-     SPICELIB Version 1.0.1, 10-MAR-1992 (WLT)
C
C         Comment section for permuted index source lines was added
C         following the header.
C
C-     SPICELIB Version 1.0.0, 31-JAN-1990 (WLT) (IMU)
C
C-&
 
C$ Index_Entries
C
C     suffix a character_string
C
C-&
 
 
 
 
C
C     SPICELIB functions
C
      INTEGER          LASTNB
 
C
C     Local variables
C
      INTEGER          L
      INTEGER          SLEN
      INTEGER          END
 
 
 
C
C     SLEN is the allocated length of the string. L is the location of
C     the last non-blank character of the prefix.
C
      SLEN = LEN    ( STRING )
      L    = LASTNB ( STRING )
 
C
C     Put the suffix at the end of the string. The spaces will fill
C     themselves in.
C
      END = L + MAX ( SPACES, 0 )
 
      IF ( END .LT. SLEN ) THEN
         STRING(END+1: ) = SUFF
      END IF
 
      RETURN
      END

C$Procedure             LASTNB ( Last non-blank character )
 
      INTEGER FUNCTION  LASTNB ( STRING )
 
C$ Abstract
C
C      Return the index of the last non-blank character in
C      a character string.
C
C$ Disclaimer
C
C     THIS SOFTWARE AND ANY RELATED MATERIALS WERE CREATED BY THE
C     CALIFORNIA INSTITUTE OF TECHNOLOGY (CALTECH) UNDER A U.S.
C     GOVERNMENT CONTRACT WITH THE NATIONAL AERONAUTICS AND SPACE
C     ADMINISTRATION (NASA). THE SOFTWARE IS TECHNOLOGY AND SOFTWARE
C     PUBLICLY AVAILABLE UNDER U.S. EXPORT LAWS AND IS PROVIDED "AS-IS"
C     TO THE RECIPIENT WITHOUT WARRANTY OF ANY KIND, INCLUDING ANY
C     WARRANTIES OF PERFORMANCE OR MERCHANTABILITY OR FITNESS FOR A
C     PARTICULAR USE OR PURPOSE (AS SET FORTH IN UNITED STATES UCC
C     SECTIONS 2312-2313) OR FOR ANY PURPOSE WHATSOEVER, FOR THE
C     SOFTWARE AND RELATED MATERIALS, HOWEVER USED.
C
C     IN NO EVENT SHALL CALTECH, ITS JET PROPULSION LABORATORY, OR NASA
C     BE LIABLE FOR ANY DAMAGES AND/OR COSTS, INCLUDING, BUT NOT
C     LIMITED TO, INCIDENTAL OR CONSEQUENTIAL DAMAGES OF ANY KIND,
C     INCLUDING ECONOMIC DAMAGE OR INJURY TO PROPERTY AND LOST PROFITS,
C     REGARDLESS OF WHETHER CALTECH, JPL, OR NASA BE ADVISED, HAVE
C     REASON TO KNOW, OR, IN FACT, SHALL KNOW OF THE POSSIBILITY.
C
C     RECIPIENT BEARS ALL RISK RELATING TO QUALITY AND PERFORMANCE OF
C     THE SOFTWARE AND ANY RELATED MATERIALS, AND AGREES TO INDEMNIFY
C     CALTECH AND NASA FOR ALL THIRD-PARTY CLAIMS RESULTING FROM THE
C     ACTIONS OF RECIPIENT IN THE USE OF THE SOFTWARE.
C
C$ Required_Reading
C
C     None.
C
C$ Keywords
C
C      ASCII,  CHARACTER,  SEARCH
C
C$ Declarations
 
      CHARACTER*(*)    STRING
 
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C      STRING     I   Input character string.
C      LASTNB     O   Index of the last non-blank character in STRING.
C
C$ Detailed_Input
C
C      STRING      is the input character string.
C
C$ Detailed_Output
C
C      LASTNB      is the index of the last non-blank character
C                  in the input string. If there are no non-blank
C                  characters in the string, LASTNB is zero.
C
C$ Parameters
C
C     None.
C
C$ Particulars
C
C      If the string is blank, return zero. Otherwise, step through
C      the string one character at a time until something other than
C      a blank is found. Return the index of that something within
C      the string.
C
C$ Examples
C
C      The following examples illustrate the use of LASTNB.
C
C            LASTNB ( 'ABCDE'              )   = 5
C            LASTNB ( 'AN EXAMPLE'         )   = 10
C            LASTNB ( 'AN EXAMPLE        ' )   = 10
C            LASTNB ( '                  ' )   = 0
C
C$ Restrictions
C
C      None.
C
C$ Exceptions
C
C      Error free.
C
C$ Files
C
C      None.
C
C$ Author_and_Institution
C
C      K.R. Gehringer  (JPL)
C      I.M. Underwood  (JPL)
C
C$ Literature_References
C
C      None.
C
C$ Version
C
C-     SPICELIB Version 2.0.0, 12-MAR-1996 (KRG)
C
C         Modified the comparison to use integer values and the ICHAR()
C         function. This improves the performance of the subroutine.
C
C-     SPICELIB Version 1.0.2, 10-MAR-1992 (WLT)
C
C         Comment section for permuted index source lines was added
C         following the header.
C
C-     SPICELIB Version 1.0.1,  7-DEC-1990 (IMU)
C
C         Corrected a misprint in the description of LASTNB.
C
C-     SPICELIB Version 1.0.0, 31-JAN-1990 (IMU)
C
C-&
 
C$ Index_Entries
C
C     last non-blank character
C
C-&

C
C     Local parameters
C
      INTEGER               ISPACE
      PARAMETER           ( ISPACE = 32 )

C
C     Local variables
C
      INTEGER          I

C
C     Just like it says in the header.
C
      IF ( STRING .EQ. ' ' ) THEN
         LASTNB = 0
 
      ELSE
         DO I = LEN (STRING), 1, -1
 
            IF ( ICHAR(STRING(I:I)) .NE. ISPACE ) THEN
               LASTNB = I
               RETURN
            END IF
 
         END DO
      END IF
 
      RETURN
      END

	