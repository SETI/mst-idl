C$Procedure      STELAB     ( Stellar Aberration )
 
      SUBROUTINE STELAB ( POBJ, VOBS, APPOBJ )
 
C$ Abstract
C
C      Correct the apparent position of an object for stellar
C      aberration.
C
C$ Copyright
C
C     Copyright (1995), California Institute of Technology.
C     U.S. Government sponsorship acknowledged.
C
C$ Required_Reading
C
C     None.
C
C$ Keywords
C
C      EPHEMERIS
C
C$ Declarations
 
      DOUBLE PRECISION   POBJ   ( 3 )
      DOUBLE PRECISION   VOBS   ( 3 )
      DOUBLE PRECISION   APPOBJ ( 3 )
 
C$ Brief_I/O
C
C      VARIABLE  I/O  DESCRIPTION
C      --------  ---  --------------------------------------------------
C      POBJ       I   Position of an object with respect to the
C                     observer.
C      VOBS       I   Velocity of the observer with respect to the
C                     Solar System barycenter.
C      APPOBJ     O   Apparent position of the object with respect to
C                     the observer, corrected for stellar aberration.
C
C$ Detailed_Input
C
C      POBJ        is the position (x, y, z, km) of an object with
C                  respect to the observer, possibly corrected for
C                  light time.
C
C      VOBS        is the velocity (dx/dt, dy/dt, dz/dt, km/sec)
C                  of the observer with respect to the Solar System
C                  barycenter.
C
C$ Detailed_Output
C
C      APPOBJ      is the apparent position of the object relative
C                  to the observer, corrected for stellar aberration.
C
C                  APPOBJ may overwrite POBJ.
C
C$ Parameters
C
C     None.
C
C$ Particulars
C
C      Let r be the vector from the observer to the object, and v be
C          -                                                    -
C      the velocity of the observer with respect to the Solar System
C      barycenter. Let w be the angle between them. The aberration
C      angle phi is given by
C
C           sin(phi) = v sin(w) / c
C
C      Let h be the vector given by the cross product
C          -
C
C            h = r X v
C            -   -   -
C
C      Rotate r by phi radians about h to obtain the apparent position
C             -                      -
C      of the object.
C
C$ Examples
C
C      In the following example, STELAB is used to correct the position
C      of a target body for stellar aberration.
C
C
C          (Previous subroutine calls have loaded the SPK file and
C           the leapseconds kernel file.)
C
C      C
C      C    Get the state of the observer with respect to the solar
C      C    system barycenter.
C      C
C           CALL SPKSSB ( IDOBS,  ET, 'J2000', SOBS )
C
C      C
C      C    Get the light-time corrected state of the target body as
C      C    seen by the observer.
C      C
C           CALL SPKAPP ( IDTARG, ET, 'J2000',  SOBS,  'LT', STARG, LT )
C
C      C
C      C    Apply the correction for stellar aberration to the
C      C    light-time corrected state of the target body.
C      C
C           CALL STELAB ( STARG(1), SOBS(4), STARG )
C
C
C      Note that this example is somewhat contrived. The correction
C      flag 'LT+S' could have been used (in place of 'LT') to do the
C      correction automatically in SPKAPP.
C
C$ Restrictions
C
C      None.
C
C$ Exceptions
C
C     1) If the velocity of the observer is greater than or equal
C        to the speed of light, the error SPICE(VALUEOUTOFRANGE)
C        is signalled.
C
C$ Files
C
C      None.
C
C$ Author_and_Institution
C
C      H.A. Neilan     (JPL)
C      W.L. Taber      (JPL)
C      I.M. Underwood  (JPL)
C
C$ Literature_References
C
C      1) W.M. Owen, Jr., JPL IOM #314.8-524, "The Treatment of
C         Aberration in Optical Navigation", 8 February 1985.
C
C$ Version
C
C-     SPICELIB Version 1.1.0, 8-FEB-1999 (WLT)
C
C         The example was corrected so that SOBS(4) is passed
C         into STELAB instead of STARG(4).
C
C-     SPICELIB Version 1.0.2, 10-MAR-1992 (WLT)
C
C         Comment section for permuted index source lines was added
C         following the header.
C
C-     SPICELIB Version 1.0.1, 8-AUG-1990 (HAN)
C
C         Examples section of the header was updated to replace
C         calls to the GEF ephemeris readers by calls to the
C         new SPK ephemeris reader.
C
C-     SPICELIB Version 1.0.0, 31-JAN-1990 (IMU) (WLT)
C
C-&
 
C$ Index_Entries
C
C     stellar aberration
C
C-&
 
 
C$ Revisions
C
C-     Beta Version 2.1.0, 9-MAR-1989 (HAN)
C
C         Declaration of the variable LIGHT was removed from the code.
C         The variable was declared but never used.
C
C-     Beta Version 2.0.0, 28-DEC-1988 (HAN)
C
C         Error handling was added to check the velocity of the
C         observer. If the velocity of the observer is greater
C         than or equal to the speed of light, the error
C         SPICE(VALUEOUTOFRANGE) is signalled.
C
C-&
 
 
 
C
C     SPICELIB functions
C
      DOUBLE PRECISION      CLIGHT
      DOUBLE PRECISION      VDOT
      DOUBLE PRECISION      VNORM
      LOGICAL               RETURN
 
 
C
C     Local variables
C
 
      DOUBLE PRECISION      ONEBYC
      DOUBLE PRECISION      U      ( 3 )
      DOUBLE PRECISION      VBYC   ( 3 )
      DOUBLE PRECISION      LENSQR
      DOUBLE PRECISION      H      ( 3 )
      DOUBLE PRECISION      SINPHI
      DOUBLE PRECISION      PHI
 
 
C
C     Standard SPICE error handling.
C
      IF ( RETURN () ) THEN
         RETURN
      ELSE
         CALL CHKIN ( 'STELAB' )
      END IF
 
 
C
C     We are not going to compute the aberrated vector in exactly the
C     way described in the particulars section.  We can combine some
C     steps and we take some precautions to prevent floating point
C     overflows.
C
C
C     Get a unit vector that points in the direction of the object
C     ( u_obj ).
C
      CALL VHAT ( POBJ, U )
 
C
C     Get the velocity vector scaled with respect to the speed of light
C     ( v/c ).
C
      ONEBYC = 1.0D0 / CLIGHT()
 
      CALL VSCL  ( ONEBYC, VOBS, VBYC )
 
C
C     If the square of the length of the velocity vector is greater than
C     or equal to one, the speed of the observer is greater than or
C     equal to the speed of light. The observer speed is definitely out
C     of range. Signal an error and check out.
C
      LENSQR = VDOT ( VBYC, VBYC )
 
      IF ( LENSQR .GE. 1.0D0 ) THEN
 
         CALL SETMSG ( 'Velocity components of observer were:  '      //
     .                 'dx/dt = *, dy/dt = *, dz/dt = *.'              )
         CALL ERRDP  ( '*', VOBS (1)            )
         CALL ERRDP  ( '*', VOBS (2)            )
         CALL ERRDP  ( '*', VOBS (3)            )
         CALL SIGERR ( 'SPICE(VALUEOUTOFRANGE)' )
         CALL CHKOUT ( 'STELAB'                 )
         RETURN
 
      END IF
 
C
C     Compute u_obj x (v/c)
C
      CALL VCRSS ( U, VBYC, H )
 
 
C
C     If the magnitude of the vector H is zero, the observer is moving
C     along the line of sight to the object, and no correction is
C     required. Otherwise, rotate the position of the object by phi
C     radians about H to obtain the apparent position.
C
      SINPHI  = VNORM ( H )
 
      IF ( SINPHI .NE. 0.D0 ) THEN
 
         PHI = DASIN ( SINPHI )
         CALL  VROTV ( POBJ, H, PHI, APPOBJ )
 
      ELSE
 
         CALL MOVED ( POBJ, 3, APPOBJ )
 
      END IF
 
 
      CALL CHKOUT ( 'STELAB' )
      RETURN
      END
