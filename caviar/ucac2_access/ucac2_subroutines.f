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
      WRITE (*,'(a)') 'index read successfully'
      DO zn=1,3
        WRITE (*,'(a,6i6)') 'zn, nx...=',zn,(nx(zn,j),j=1,5)
      ENDDO

      CALL nx_byte_flip (nx,zmax,bfx) ! check for byte flip, apply if needed
 
* prepare table output
      IF (fnout.EQ.'s') THEN
        uo = 6
      ELSE
        uo = 20
        OPEN (uo,FILE=fnout)
      ENDIF

* loop boxes 
 101  continue
 
        CALL calc_box (ra,dec,w,ra1,ra2,dc1,dc2)

	write(*,*) ra1,ra2,dc1,dc2

        WRITE (*,'(f10.6,1x,f10.6,1x,f10.5,1x,f10.5)') ra1,ra2,dc1,dc2

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

      WRITE (*,'(a,3i7)') 
     .   'stars read, slected, errors = ',nsr,nss,errc

* sort
      IF (is.GE.1.AND.is.LE.dimj) THEN
        WRITE (*,'(a,i3)') 'start sort by column = ',is
        CALL sorti (alls,is,dimj,1,nss,dima,dimj)
      ENDIF

* output
      CALL put_stars (alls,dima,dimj,nss,fmt,uo)
      WRITE (*,'(a,i6,a)') 'output complete',nss,' stars'

	call populate_output(nss,dima,dimj,alls,output)

	goto 99


* the end
 901  WRITE (*,'(a)') 'error in reading index file'
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
        WRITE (*,'(a,i3)') 'open file for zone = ',zn 
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
                  WRITE (*,'(/a,i8)') 
     .             'WARNING: too many stars, take first dima =',dima
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

	write(*,*)' '

      degrad = DATAN (1.0d0) / (4.5d1)   ! degree to radian

* header line

        WRITE (uo,'(/3a)') '   RA (deg)    DE (deg)'
     .   ,' U2mag eRA eDE     epRA     epDC    pm RA'
     .   ,' pmRAcD   pmDE epmR epmD UCAC2 ID'


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

          WRITE (uo,'(f11.7,f12.7,f6.2,2i4,2f9.3
     .               ,f9.1,2f7.1,2f5.1,i9.8)') 
     .      ran,dcn,mag, alls(i,28),alls(i,29), epr,epd
     .     ,pmr,pmrc,pmd, epmr,epmd, alls(i,24)

          WRITE (*,'(f11.7,f12.7,f6.2,2i4,2f9.3
     .               ,f9.1,2f7.1,2f5.1,i9.8)') 
     .      ran,dcn,mag, alls(i,28),alls(i,29), epr,epd
     .     ,pmr,pmrc,pmd, epmr,epmd, alls(i,24)



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

      WRITE (*,'(/a)') 'open first zone, read first record'

      CALL open_zfile (pathz,un,zn,only_rd)

      bft = .FALSE.                 ! first test with no byte flip

      CALL read_u2line (un,1,bft,idat,errflg)
      CLOSE (un)

      WRITE (*,'(a,i6)') 'mag of first star = ',idat(3)

      IF (idat(3).EQ.1591) THEN     ! magnitude of first star
        bf = .FALSE.
        WRITE (*,'(/a)') '-- no byte flip required'

      ELSE
        mag = idat(3)
        CALL flip2 (mag)
        IF (mag.EQ.1591) THEN
          WRITE (*,'(/a)') '** byte flip is required, will do'
          bf = .TRUE.
        ELSE
          WRITE (*,'(/a)') '** WARNING: byte flip test inconclusive'
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
        WRITE (*,'(a)') 'index array: no byte flip required'
        bf = .FALSE.

      ELSEIF (nx(1,1).EQ.33554432) THEN
        WRITE (*,'(a)') 'index array: byte flip is required'
        bf = .TRUE. 
        DO zn= 1,zmax
        DO j = 1,240
          CALL flip4 (nx(zn,j))
        ENDDO
        ENDDO
        WRITE (*,'(a)') 'index array: byte flip done' 
 
      ELSE
        WRITE (*,'(a)') 
     .   'WARNING: index array: byte flip inconclusive'
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
          WRITE (90,'(1X//1X,A,F13.3/)')  'RA > 24 hours or < 0 :',RA
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
          WRITE (90,'(1X//1X,A,F13.3/)')  'abs (DC)  > 90 deg :',DK
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

        WRITE (CREKT,'(   I2.2,1X,I2.2,1X,F7.4)')
     A    IRASTD,IRAMIN,RASEC
        IF (CREKT(7:7).EQ.' ')  CREKT(7:7)= '0'
        WRITE (CDEKL,'(A1,I2.2,1X,I2.2,1X,F6.3)')
     A    CVZ,IDKGRD,IDKMIN,DKSEC
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
	