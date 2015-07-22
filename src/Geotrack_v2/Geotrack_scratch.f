C**********************************************************************/
C
C**********************************************************************/

C     Problem Identifier
C     Format: 20A4 = 20 * 4 = 80 (Characters)
      COMMON /CFBBBI/ TITLE(20)

C     NS = Number of Layers <= 5
C     NL = ?? (Number of lines ????)
C     PSI = ???
      COMMON /SCFBCI/ NS,PSI,NL

C     LACOPT = 0 (Compute only Influence Coefficient)
C              1 (Solve entire problem)

      COMMON/BLOAD/KZ,NBRT,KR,ARG,XSIDE,LACOPT

      COMMON /ONE/ COEF(110,6,5)

C     NITER = Number of Iterations used to allow soil moduli to converge
C             number of calculations in program = NITER+1
C     INTERF = Option for obtaining stresses and displacements at
C              locations other than that at depths used to determine
C              stress dependent moduli
C              0 (If information is not required at depths other than
C                 those used for moduli iterations or if NITER = 0)
C              1 (If information is required)
C     NOTENS = Option for limiting maximum incremental tie/soil tensile
C              forces
C              0 (Do not limit)
C              1 (Limit maximum incremental tensile reactions to static
C                 weights assigned to the tie segments)
C     GAMMA(I) = Soil unit weight for layer I
C     KNOT(I)  = Coefficient of lateral earth pressure for layer I
C     KONE(I), KTWO(I) = Coefficients for calculation of stress dependent
C                        soil modulus
C     NP = ???
c     NSTEPS= ???
C     HHH = ???
C     SIGMAV = ???
C     THETAI = ???
      COMMON /TWO/KONE(6),KTWO(6),NSTEPS,NITER,GAMMA(6),KNOT(6),HHH(6),
     *SIGMAV(6),THETAI(6),THETAF(6),INTERF,NP,NOTENS

C     TIEL  = Tie Length
C     TIESP = Tie Spacing
C     NSEG  = ???
C     NTIES = ???
      COMMON /THREE/ TIEL,NSEG,TIESP,NTIES,NLAYER(3),STA(10)

C     KTYPE(I) = Type of stress dependent relationship to use for soil
C                layer I
C     NAXLES = Number of axles to be applied to the track <= 4
C     LP(I) = Tie number at which load I is applied
C     PWHL(I) = Wheel load applied to tie I
C     MINTOUT = First tie at which output is desired
C     MAXTOUT = Last tie at which output is desired
C     MINSEG  = First tie segment at which output is desired
C     MAXSEG  = Last tie segment at which output is desired
C     ITSTD = Option for printing rail deflection, rail seat load, tie
C             deflections, tie bending moments, rail bending moments,
C             and soil stresses beneath the tie
C             0 - Do not print; 1 - Print
C     ITPRIN = Option for calculation of principal stresses and
C              direction cosines for the soil stresses beneath the ties
C              0 - Do not calculate; 1 - Calculate
C     ITRIAX = Option for calculation of equivalent triaxial states for
C              soil stresses
C              0 - Do not calculate; 1 - Calculate
C     ICSTD  = ??
C     ICPRIN = ??
C     ICTRIX = ??
      COMMON/FOUR/NAXLES,LP(4),MINTOUT,MAXTOUT,MAXCOUT,MINSEG,MAXSEG,
     1   ICTRIX,ICPRIN,ICSTD,ITSTD,ITRIAX,ITPRIN,PWHL(4),KTYPE(9)

      COMMON /OFF1/ OFFW(5),OFFX(5),OFFY(5),OFFZ(5),OFFXY(5),OFFXZ(5),
     *              OFFYZ(5),ANGNEW(60)

C     NOTHER = Option for computing stresses at locations adjacent to
C              track structure.
C              0 (Locations adjacent to track structure will not be
C                 specified)
C              1 (Locations adjacent to track structure will be
C                 specified)
      COMMON /OFF2/ NEWFLAG,NOTHER,OFFSET

C     TIEWD = Tie Width
C     TA = Cross-sectional area of tie
C     TE = Young's modulus of tie
C     TI = Moment of Inertia of tie about major axis
C     TWT = Tie Weight
C     MI = Number of tie segments having centers between the rails
C     WI = Rail Spacing
C     RA = Rail cross-sectional area
C     RE = Young's modulus for rail
C     RI = Moment of Inertia for rail about major axis
C     RWT = Rail weight
C     STIF1 = Rail fastener or tie pad stiffness
C     PR = ???

      COMMON /INPUT/ TA,TE,TI,MI,WI,RA,RE,RI,STIF1,PR,TWT,RWT,TIEWD

C     IOPT = Option for printing out soil influence coefficients
C            0 - Do not print; 1 - Print
C     CRIBOPT = ??????????
C     ITOPT = Option for printing out computed values for each iteration
C             0 - Print result from final iteration only
C             1 - Print result from all iterations
      COMMON /OUTPUT/ IOPT, CRIBOPT, ITOPT

C     E(I)    = Resilient Young's modulus for layer I
C     VRAT(I) = Poisson's Ratio for layer I
      COMMON /SCFCCI/ E(7), VRAT(7)

C     HH(I) = Depth of layer I; HH(I) for last layer must be 0
C     WGT = ??
C     IZ = ??
C     IR = ??
C     RR = ??
C     ZZ = ??
      COMMON /SCFBBI/ HH(7), WGT, IZ, IR, RR(110), ZZ(20)

C     II = ??
C     IIZ = ??
      COMMON /FBBBI/ II, IIZ

C     N = ??
C     H(I) = ??
      COMMON /CBBBC/ H(6), N

      COMMON  A(396,7), B(396,7), C(396,7), D(396,7), T1, T2, T1M,
     1 AJ(396), L, NLINE, RJ0(396), RJ1(396), TZZ, Z, COM, CMU, CSR,CST,
     1CSZ,CTR

      COMMON/CONTA/NCNTCT(30),NUMCON

      COMMON /GEO/ AY(11,10,6)
      COMMON /CBPC/ AR,AZ(396),I,R
      COMMON /PBC/ BZ(100),PM(6,4,4)

      COMMON /PC/ ITN,NTEST,SF
      COMMON /PCC/ K
      COMMON /FBCBI/ NOUTP
      COMMON /CBBCC/ P

      COMMON /FRNUM/ BUFR,NC,ISPEC
      COMMON /FRIO/ NCARD,INUNIT,OUTUNT,IPRINT

      REAL*8 KONE, KTWO, KNOT
C**********************************************************************/
C     Opening an input file 'INFILE.DAT'
C**********************************************************************/
      OPEN(UNIT=2,FILE='INFILE.DAT',STATUS='OLD',FORM='FORMATTED')
      OPEN(UNIT=1,STATUS='SCRATCH',FORM='FORMATTED')


C     Reading the title from the input file
      READ(2,310) (TITLE(I), I=1,20)
  310 FORMAT(20A4)

C     Reading number of layers and different options
      READ(2,*) NS, LACOPT, NITER, INTERF, NOTENS, NOTHER
      READ(2,*) IOPT, CRIBOPT, ITOPT

      READ(2,*) (E(I), VRAT(I), HH(I), GAMMA(I), KNOT(I), KTYPE(I),
     1KONE(I), KTWO(I), I=1,NS)

      READ(2,*) TIEL, TIESP, TIEWD
      READ(2,*) TA, TE, TI, TWT
      READ(2,*) MI, WI
      READ(2,*) RA, RE, RI, RWT
      READ(2,*) STIF1
      READ(2,*) NAXLES

      DO 261 I=1,NAXLES
      READ(2,*) LP(I), PWHL(I)
  261 CONTINUE

      READ(2,*) MINTOUT, MAXTOUT, MAXCOUT, MINSEG, MAXSEG
      READ(2,*) ITSTD, ITPRIN, ITRIAX

      IF(CRIBOPT.EQ.1) READ(2,*) ICSTD, ICPRIN, ICTRIX

C**********************************************************************/
C
C**********************************************************************/
      NEWFLAG = 0
      LAST    = 0
C     *** Modified: variable name IPRNT to IPRINT ***
C     IPRINT  = ON

      NL = NS
      IZ = NS+1
      NP = (IZ-1) * NS
      WGT = 1.0

C     Converting CUBIC INCHES TO CUBIC FEET :::: DIVIDING BY 1728
      DO 259 I=1,NS
      GAMMA(I) = GAMMA(I)/1728.
  259 CONTINUE

      NSEG  = 10
      NTIES = 11
C     *** Modified: converting real to integer (cast)
      NTIES   = INT(NTIES/2.+1)
      NPOINTS = NTIES*2-1
      SEGL    = TIEL/NSEG
      PSI     = 1.0/(TIEWD*SEGL)
      PATM    = 14.7

      PW1 = PWHL(1)
      PW2 = PWHL(2)
      PW3 = PWHL(3)
      PW4 = PWHL(4)

      LP1 = LP(1)
      LP2 = LP(2)
      LP3 = LP(3)
      LP4 = LP(4)

      REI = RE*RI/1000.
      TEI = TE*TI/1000.

      DO 264 I=1,4
      PWHL(I) = PWHL(I)*1000.
  264 CONTINUE

      PR     = PWHL(1)
      STATIC = (TWT+(2.*RWT*TIESP/36.))/(TIEL*TIEWD)
      IR     = NPOINTS*NSEG
      XINCR  = SEGL
      YINCR  = TIESP/2.0
      II     = IR+1
      IIZ    = IZ+1
      L1     = 0
      X      = 0

      write(*,*) 'Debug: NSEG    = ', NSEG
      write(*,*) 'Debug: NPOINTS = ', NPOINTS
      write(*,*) 'Debug: KTYPE   = ', KTYPE
      write(*,*) 'Debug: IOPT    = ', IOPT

      DO 1090 J=1,NSEG
      IF(J.EQ.1) GO TO 1020
      X = X + XINCR
 1020 CONTINUE
      Y = 0.
      DO 1050 I=1,NPOINTS
      L1 = L1+1
      IF(L1.EQ.1) GO TO 1040
      IF(I.EQ.1)  GO TO 1030
      Y = Y+YINCR
 1030 RR(L1) = SQRT(X**2+Y**2)
      GO TO 1050
 1040 RR(L1) = 0.
 1050 CONTINUE
 1090 CONTINUE

C**********************************************************************/
C     Printing all the read variables
C     Replacing 3 with * to write to console
C**********************************************************************/

      WRITE(*,3001)
 3001 FORMAT(1H1,/,50X,'PROGRAM GEOTRACK II',/,40X,
     1      'NONLINEAR ELASTIC MULTI-LAYER TRACK ANALYSIS',/)

      WRITE(*,300) (TITLE(I),I=1,20)
  300 FORMAT(40X,20A4)

      WRITE(*,312) TIEL, NSEG, TIESP, TIEWD, TA, TWT, TEI, WI, RA, RWT,
     1             REI, STIF1, NAXLES
  312 FORMAT(//,40X,'TIE LENGTH ........................ ',F5.1,
     12X,'IN.',//,
     140X,'NO. OF SEGMENTS PER TIE ........... ',I2,//,
     140X,'TIE SPACING .......................',F5.1,2X,'IN.',//,
     240X,'TIE WIDTH .........................',F5.1,2X,'IN.',//,
     340X,'TIE AREA ..........................',F5.1,2X,'IN.SQ.',//,
     340X,'TIE WEIGHT ........................',F6.1,2X,'LBS.',//,
     440X,'TIE EI ............................',F9.1,2X,'K-IN.SQ.',//,
     540X,'RAIL GAUGE ........................',F6.2,2X,'IN.',//,
     640X,'RAIL AREA .........................',F6.2,2X,'IN.SQ.',//,
     640X,'RAIL WEIGHT ....................... ',F5.1,2X,'LBS./YARD',//,
     740X,'RAIL EI ...........................',F10.1,2X,'K-IN.SQ.',//,
     840X,'RAIL FASTENER STIFFNESS ...........',F10.1,2X,
     8'LB. PER IN.',//,
     940X,'NO. OF AXLE LOADS .................',I2,/)

      WRITE(*,3121) LP1, LP2, LP3, LP4, PW1, PW2, PW3, PW4, NS, LACOPT,
     1              NITER, INTERF, NOTHER, NOTENS, MINTOUT, MAXTOUT,
     1              MINSEG, MAXSEG, ITSTD, ITPRIN, ITRIAX
 3121 FORMAT(/,
     140X,'AXLE LOADS ON TIE NOS. ............',I2,2X,I2,2X,I2,
     1    2X,I2,//,
     240X,'WHEEL LOAD/AXLE (KIPS) ............ ',F5.2,2X,
     2    F5.2,2X,F5.2,2X,F5.2,//,
     340X,'NO. OF SOIL LAYERS ................',I2,//,
     440X,'OPTION FOR COEFFICIENTS ...........',I2,//,
     540X,'NO. OF ITERATIONS .................',I2,//,
     640X,'CALCULATE AT OTHER DEPTH POINTS ...',I2,//,
     640X,'CALCULATE AT OFFSET LOCATIONS .....',I2,//,
     640X,'CONSIDER TIE/BALLAST SEPARATION ...',I2,//,
     640X,'FIRST TIE OUTPUT ..................',I2,//,
     740X,'LAST TIE OUTPUT ...................',I2,//,
     840X,'FIRST TIE SEGMENT OUTPUT ..........',I2,//,
     840X,'LAST TIE SEGMENT OUTPUT ...........',I2,//,
     840X,'PRINT STANDARD SOIL OUTPUT ........',I2,//,
     840X,'PRINT PRINCIPLE STRESSES ..........',I2,//,
     940X,'PRINT EQUIVALENT TRIAXIAL DATA ....',I2,//)

      WRITE(*,358)
  358 FORMAT(//32X,'LAYER     E(I)-K/IN.SQ.   VRAT(I)    DEPTH-IN     GA
     1MMA-PCF  KNOT'/)

      WRITE(*,302)(I, E(I), VRAT(I), HH(I), GAMMA(I), KNOT(I), I=1,NS)
  302 FORMAT(33X,I2,7X,F10.2,1X,F10.2,3X,2F10.2,1X,F10.2)

      WRITE(*,359)
  359 FORMAT(//9X,'LAYER',7X,'KTYPE',26X,'MODULUS DESCRIPTION',/)

C     Type of stress dependent relationship for the soil layer
      DO 360 I=1,NS
      IF(KTYPE(I).EQ.0)WRITE(*,3590) I, KTYPE(I)
      IF(KTYPE(I).EQ.1)WRITE(*,3591) I, KTYPE(I), KONE(I), KTWO(I)
      IF(KTYPE(I).EQ.2)WRITE(*,3592) I, KTYPE(I), KONE(I), KTWO(I)
      IF(KTYPE(I).EQ.3)WRITE(*,3593) I, KTYPE(I), KONE(I), KTWO(I)
      IF(KTYPE(I).EQ.4)WRITE(*,3594) I, KTYPE(I)
      IF(KTYPE(I).EQ.5)WRITE(*,3595) I, KTYPE(I)
      IF(KTYPE(I).EQ.6)WRITE(*,3596) I, KTYPE(I)
      IF(KTYPE(I).EQ.7)WRITE(*,3597) I, KTYPE(I)

      IF(KTYPE(I).EQ.8)WRITE(*,3598) I, KTYPE(I)
  360 CONTINUE
 3590 FORMAT(10X,I2,10X,I2,10X,'CONSTANT')
 3591 FORMAT(10X,I2,10X,I2,10X,'LOG E - LOG BULK STRESS  K1 =',F8.2,
     1       5X,'K2 = ',F8.4)
 3592 FORMAT(10X,I2,10X,I2,10X,'E - LOG BULK STRESS      K1 =',F8.2,
     1       5X,'K2 = ',F8.4)
 3593 FORMAT(10X,I2,10X,I2,10X,'E - BULK STRESS          K1 =',F8.2,
     1       5X,'K2 = ',F8.4)
 3594 FORMAT(10X,I2,10X,I2,10X,
     1'EXPERIMENTAL SHEAR STRESS REVERSAL')
 3595 FORMAT(10X,I2,10X,I2,10X,
     1'E - DEVIATOR STRESS FOR STIFF COHESIVE SUBGRADE')
 3596 FORMAT(10X,I2,10X,I2,10X,
     1'E - DEVIATOR STRESS FOR MEDIUM COHESIVE SUBGRADE')
 3597 FORMAT(10X,I2,10X,I2,10X,
     1'E - DEVIATOR STRESS FOR SOFT COHESIVE SUBGRADE')
 3598 FORMAT(10X,I2,10X,I2,10X,
     1'E - DEVIATOR STRESS FOR VERY SOFT COHESIVE SUBGRADE')

      IF(IOPT.EQ.0) GO TO 330
      WRITE(*,314)
  314 FORMAT(//40X,'RADIUS FROM ORIGIN',5X,'IN.'/)

      WRITE(*,313)(RR(I), I=1,IR)
  313 FORMAT(11(5X,F6.2))

  330 WRITE(*,317)
  317 FORMAT(//40X,'DIFFERENT DEPTHS AT WHICH MODULI ARE COMPUTED'/)
      READ(2,*) (ZZ(I),I=2,IZ)
      ZZ(1) = 0.

      DO 316 I=1,NS
      KOZ = I+1
      WRITE(*,315) I, ZZ(KOZ)
  315 FORMAT(/40X,'Z(',I1,') = 'F6.2,' IN.')
  316 CONTINUE


      N = NS-1
      HHH(1) = HH(1)
      IF(N.LT.2) GO TO 1011
      DO 1010 I=2,N
 1010 HHH(I) = HHH(I-1) + HH(I)
 1011 CONTINUE

      SIGMAV(1) = GAMMA(1)*ZZ(2) + STATIC
      IZ1 = IZ-1
      DO 2020 I=2,IZ1
C     ???????? ZZ(I+1) EFFECT OF POINTS BENEATH??????
 2020 SIGMAV(I) = SIGMAV(I-1) + GAMMA(I-1)*(HHH(I-1)-ZZ(I))+
     *GAMMA(I)*(ZZ(I+1)-HHH(I-1))
      DO 3030 I=1,IZ1
 3030 THETAI(I) = REAL(SIGMAV(I)+KNOT(I)*SIGMAV(I)+KNOT(I)*SIGMAV(I))

C     *** Modified: REWIND 1 statement deleted from code
C                   Replaced '&' with '*' when calling subroutines
      NSTEPS = 0
      IF(NITER.EQ.0) GO TO 991
      DO 990 I=1,NITER
      NSTEPS = I-1
      REWIND 1
      CALL SCERJ(*99)
      write(*,*) 'call to calcul (1)'
      CALL CALCUL(NPUN)
      write(*,*) 'after calcul (1)'
  990 CONTINUE
      GO TO 992
  991 CALL SCERJ(*99)
C      CALL CALCUL(NPUN)
  992 IF((INTERF.EQ.1).OR.(NOTHER.EQ.1)) GO TO 993
      GO TO 99
  993 IF((INTERF.EQ.1).AND.(NOTHER.EQ.1)) GO TO 9940
      IF((INTERF.EQ.1).AND.(NOTHER.EQ.0)) GO TO 9940
      IF((INTERF.EQ.0).AND.(NOTHER.EQ.1)) GO TO 9942
      GO TO 99
 9940 READ(2,*,END=99) NPT
      IF(NPT.EQ.9999) GO TO 9942
      ZZ(1) = 0.
      DO 9941 I=1,NPT
      KOZ = I+1
      READ(2,*) ZZ(KOZ),KNOT(I)
 9941 CONTINUE
      IZ = NPT + 1
      NL = NPT
      NP = (IZ-1) * NSEG
      REWIND 1
      CALL SCERJ(*99)
C      CALL CALCUL(NPUN)

      GO TO 9940
 9942 READ(2,*,END=99) OFFSET, NPT
      NEWFLAG = 1
      ZZ(1) = 0.
      IZ = NPT + 1
      READ(2,*) (ZZ(I),I=2,IZ)
      NL = NPT
      NP = (IZ-1) * NSEG
      REWIND 1
      CALL SCERJ(*99)
C      CALL CALCUL(NPUN)

      GO TO 9942
   99 STOP
C**********************************************************************/
      END

C***********************************************************************
      SUBROUTINE SCERJ(*)
C***********************************************************************
      COMMON /SCFCCI/ E(7), VRAT(7)
      COMMON /SCFBBI/ HH(7), WGT, IZ, IR, RR(110), ZZ(20)
      COMMON /SCFBCI/ NS, PSI, NL
C***********************************************************************
      LOGICAL LOG, B1
C***********************************************************************
      B1=.TRUE.
      LOG=.TRUE.
C-----WGT
      IF (WGT.GT.0.0) GO TO 1
      WRITE(*,2) WGT
    2 FORMAT (1H0,'INPUT ERROR, THE VALUE OF WGT IS LESS THAN ZERO, WGT
     1= ',E10.4)
      LOG=.FALSE.
    1 IF (WGT.LT.0.100E08)  GO TO 3
      WRITE(*,4) WGT
    4 FORMAT (1H0,'INPUT ERROR, THE VALUE OF WGT IS GREATER THAN 0.100E0
     18, WGT =',E10.4)
      LOG=.FALSE.
C-----PSI
    3 IF (PSI.GT.0.0) GO TO 5
      WRITE(*,6) PSI
    6 FORMAT (1H0,'INPUT ERROR, PSI IS LESS THAN ZERO, PSI =',E10.4)
      LOG=.FALSE.
    5 IF (PSI.LT.0.100E05) GO TO 7
      WRITE(*,8) PSI
    8 FORMAT (1H0,'INPUT ERROR, PSI IS GREATER THAN 10,000.0, PSI =',E10
     1.4)
      LOG=.FALSE.
C-----NS
    7 IF (NS.GT.0) GO TO 9
      WRITE(*,10) NS
   10 FORMAT (1H0,'INPUT ERROR, NS IS LESS THAN ZERO, NS =',I4)
      LOG=.FALSE.
    9 IF (NS.LE.5) GO TO 11
      WRITE(*,12) NS
   12 FORMAT (1H0,'INPUT ERROR, NS IS GREATER THAN  5, NS =',I4)
      LOG=.FALSE.
C-----IR
   11 IF (IR.GT.0) GO TO 13
      WRITE(*,14) IR
   14 FORMAT (1H0,'INPUT ERROR, IR IS LESS THAN ZERO, IR =',I4)
      LOG=.FALSE.
   13 IF (IR.LE.110) GO TO 15
      WRITE(*,16) IR
   16 FORMAT (1H0,'INPUT ERROR, IR IS GREATER THAN 110,IR=',I4)
      LOG=.FALSE.
C-----IZ
   15 IF (IZ.GT.0) GO TO 17
      WRITE(*,18) IZ
   18 FORMAT (1H0,'INPUT ERROR, IZ IS LESS THAN ZERO, IZ =',I4)
      LOG=.FALSE.
   17 IF (IZ.LT.100) GO TO 19
      WRITE(*,20) IZ
   20 FORMAT (1H0,'INPUT ERROR, IZ IS GREATER THAN 100, IZ =',I4)
      LOG=.FALSE.
C-----E(I) &VRAT(I)
   19 IF(NS.GT.15) GO TO 21
      IF (NS.LT.1) GO TO 21
      DO 22 I=1,NS
      IF (E(I).GT.100) GO TO 23
C*******************************************************************
C PARENTHESIS
      WRITE(*,24)I,I,E(I)
   24 FORMAT (1H0,'INPUT ERROR, E(',I2,') IS LESS THAN 100, E(',I2,') ='
     1,E10.4)
      LOG=.FALSE.
   23 IF (E(I).LT.0.100E30)  GO TO 25
C*******************************************************************
C PARENTHESIS
      WRITE(*,26) I,I,E(I)
   26 FORMAT (1H0,'INPUT ERROR, E(',I2,') IS GREATER THAN 0.100E30, E(',
     1I2,') =',E10.4)
      LOG=.FALSE.
   25 IF (VRAT(I).GT.0.05) GO TO 27
C*******************************************************************
C PARENTHESIS
      WRITE(*,28) I,I,VRAT(I)
   28 FORMAT (1H0,'INPUT ERROR, U(',I2,') IS LESS THAN 0.05, U(',I2,') =
     1',E10.4)
      LOG=.FALSE.
   27 IF (VRAT(I).LE.0.50) GO TO 22
C*******************************************************************
C PARENTHESIS
      WRITE(*,30) I,I,VRAT(I)
   30 FORMAT (1H0,'INPUT ERROR, U(',I2,') IS GREATER THAN 0.50, U(',I2,'
     1) =',E10.4)
      LOG=.FALSE.
   22 CONTINUE
   21 CONTINUE
C-----RR(I)
      IF (IR.LE.0) GO TO 31
      IF (IR.GT.99) GO TO 31
      DO 32 I=1,IR
      IF (RR(I).GT.-0.001) GO TO 33
C*******************************************************************
C PARENTHESIS
      WRITE(*,34)I,I,RR(I)
   34 FORMAT (1H0,'INPUT ERROR, R(',I2,') IS LESS THAN ZERO, R(',I2,') =
     1',E10.4)
      LOG=.FALSE.
   33 IF (RR(I).LT.1000.0) GO TO 32
C*******************************************************************
C PARENTHESIS
      WRITE(*,36) I,I,RR(I)
   36 FORMAT (1H0,'INPUT ERROR, R(',I2,') IS GREATER THAN 1000, R(',I2,'
     1) =',E10.4)
      LOG=.FALSE.
   32 CONTINUE
   31 CONTINUE
C-----ZZ(I)
      IF (IZ.LE.0) GO TO 40
      IF (IZ.GT.99) GO TO 40
      DO 41 I=1,IZ
      IF (ZZ(I).GT.-0.001) GO TO 42
C*******************************************************************
C PARENTHESIS
      WRITE(*,43) I,I,ZZ(I)
   43 FORMAT (1H0,'INPUT ERROR, Z(',I2,') IS LESS THAN ZERO,Z(',I2,') ='
     1,E10.4)
      LOG=.FALSE.
   42 IF (ZZ(I).LT.1000.0) GO TO 41
C*******************************************************************
C PARENTHESIS
      WRITE(*,45) I,I,ZZ(I)
   45 FORMAT (1H0,'INPUT ERROR, Z(',I2,') IS GREATER THAN 1000, Z(',I2,'
     1) =',E10.4)
      LOG=.FALSE.
   41 CONTINUE
   40 CONTINUE
C-----HH(I)
      N=NS-1
      IF (N.LE.0) GO TO 80
      IF (N.GT.14) GO TO 80
      DO 81 I=1,N
      IF (HH(I).GT.0.0) GO TO 82
C*******************************************************************
C PARENTHESIS
      WRITE(*,83) I,I,HH(I)
   83 FORMAT (1H0,'INPUT ERROR, H',I2,' IS LESS THAN ZERO, H',I2,' =',E1
     10.4)
      LOG=.FALSE.
   82 IF (HH(I).LT.1000.0) GO TO 81
C*******************************************************************
C PARENTHESIS
      WRITE(*,84) I,I,HH(I)
   84 FORMAT (1H0,'INPUT ERROR, H',I2,' IS GREATER THAN 1000, H',I2,' ='
     1,E10.4)
      LOG=.FALSE.
   81 CONTINUE
   80 CONTINUE
      IF(LOG .AND. B1)RETURN
      RETURN 1
      END





C***********************************************************************
      SUBROUTINE CALCUL(NPUN)
C***********************************************************************
      COMMON  A(396,7),B(396,7),C(396,7),D(396,7),T1,T2,T1M,AJ(396),L,
     1 NLINE,RJ0(396),RJ1(396),TZZ,Z,COM,CMU,CSR,CST,CSZ,CTR
      COMMON /SCFCCI/ E(7),VRAT(7)
      COMMON/GEO/ AY(11,10,6)
      COMMON /ONE/ COEF(110,6,5)
      COMMON /PBC/ BZ(100),PM(6,4,4)
      COMMON /SCFBBI/ HH(7),IR,IZ,RR(110),WGT,ZZ(20)
      COMMON/BLOAD/KZ,NBRT,KR,ARG,XSIDE,LACOPT
      COMMON/CBPC/AR,AZ(396),I1,R
      COMMON /PC/ ITN,NTEST,SF
      COMMON /CBBBC/ H(6),N
      COMMON /OUTPUT/ IOPT,CRIBOPT,ITOPT
      COMMON /SCFBCI/ NS,PSI,NL
      COMMON /CBBCC/ P
      COMMON/CONTA/NCNTCT(30),NUMCON
      COMMON/CFBBBI/TITLE(20)
      COMMON/SIX/ MINSEG,MAXSEG,MINTOUT
      COMMON /TWO/KONE(6),KTWO(6),NSTEPS,NITER,GAMMA(6),KNOT(6),HHH(6),
     *SIGMAV(6),THETAI(6),THETAF(6),INTERF,NP,NOTENS
      COMMON /OFF1/ OFFW(5),OFFX(5),OFFY(5),OFFZ(5),OFFXY(5),OFFXZ(5),
     *              OFFYZ(5),ANGNEW(60)
      COMMON /OFF2/ NEWFLAG,NOTHER,OFFSET
      COMMON /THREE/ TIEL,NSEG,TIESP,NTIES,NLAYER(3),STA(10)
      DIMENSION CHECKR(50)
      REAL*8 KONE,KTWO,KNOT
C***********************************************************************
      INTEGER TITLE,CHECKR,CRIBOPT
      DOUBLE PRECISION CTR
C***********************************************************************
C      DATA FCP017 /'....'/,FCP016/'****'/
C NEED TO FIX THIS!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      DATA NPAGE/0/
C***********************************************************************
      CHARACTER*4 FCP017, FCP016, ASTER, PERD
      FCP017 = '....'
      FCP016 = '****'

      ITN4 = ITN*4
      ASTER = FCP016
      PERD = FCP017
      N = NS-1
      DDDD = 0.31830989E0*WGT/PSI
      AR = SQRT(DDDD)
      NLINE = 17+NS
      NPAGE = NPAGE+1

      IF(IOPT.EQ.1) WRITE(*,350)(ASTER,I=1,5),(TITLE(I),I=1,20),
     1(ASTER,I=1,4),NPAGE
  350 FORMAT (1H1//1H0,5A4,1X,20A4,1X,4A4,6H  PAGE,I2)
      IF(IOPT.EQ.1) WRITE(*,351)WGT,PSI,AR,(I,E(I),VRAT(I),HH(I)
     2,I=1,N)
  351 FORMAT (1H0,40X,26HTHE PROBLEM PARAMETERS ARE/1H0,20X,13HTOTAL LOA
     1D...,8X,F10.2,5H  LBS/1H0,20X,15HTIRE PRESSURE..,5X,F10.2,5H  PSI/
     11H0,20X,13HLOAD RADIUS..,7X,F10.2,5H  IN./1H /(1H ,20X,5HLAYER,I3,
     114H  HAS MODULUS ,E10.3,18H   POISSONS RATIO ,F5.3,17H   AND THICK
     1NESS ,F6.2,4H IN.))
      IF(IOPT.EQ.1) WRITE(*,354)NS,E(NS),VRAT(NS)
  354 FORMAT (1H ,20X,5HLAYER,I3,14H  HAS MODULUS ,E10.3,18H   POISSONS
     1RATIO ,F5.3,24H   AND IS SEMI-INFINITE.)
      IF(IOPT.EQ.1) WRITE(*,352)(PERD,J=1,27)
  352 FORMAT (1H0,34X,15HS T R E S S E S,26X,12HDISPLACEMENT,15X,13HS T
     1R A I N S/13X,15A4,3X,3A4,3X,9A4/4H   R,5X,1HZ,6X,8HVERTICAL,3X,
     110HTANGENTIAL,3X,6HRADIAL,6X,5HSHEAR,7X,4HBULK,11X,8HVERTICAL,7X,
     16HRADIAL,6X,5HSHEAR,7X,4HBULK/1H )

      WRITE(*,*) 'Debug: NEWFLAG: ', NEWFLAG
      WRITE(*,*) 'Debug: CRIBOPT: ', CRIBOPT

C**   ADJUST LAYER DEPTHS **
      H(1) = HH(1)
      DO 25 I=2,N
   25 H(I) = H(I-1) + HH(I)
      IF(NEWFLAG.EQ.1) GO TO 800
      IF((CRIBOPT.EQ.1).AND.(NSTEPS.GE.NITER)) GO TO 30

C     DEFINE AN ARRAY WHOSE ELEMENTS EQUAL THE CRIB RADIUS NUMBERS
      III = 1
      DO 2 KRADNO=2,101,11
      KTEMP = KRADNO
      KSTOP = KTEMP+8
      DO 1 LL=KTEMP,KSTOP,2
      CHECKR(III) = LL
      III = III+1
    1 CONTINUE
    2 CONTINUE
      GO TO 30
  800 IR = 60
      SEGL = TIEL/10
      DO 801 INEW=1,6
      Y = (INEW-1)*TIESP
      DO 801 JNEW=1,10
      X = OFFSET-(TIEL/2.)+((JNEW-1)*SEGL)+(SEGL/2.)
      KNEW = (INEW-1)*10+JNEW
      RR(KNEW) = SQRT(X**2+Y**2)
      ANGNEW(KNEW) = ATAN(Y/X)
  801 CONTINUE
   30 IRT = 0

C  ** START ON A NEW R **
  100 IRT = IRT + 1
      IF(NEWFLAG.EQ.1) GO TO 101
      IF((CRIBOPT.EQ.1).AND.(NSTEPS.GE.NITER)) GO TO 101
C     CHECT TO SEE IF THE RADIUS BEING SOLVED FOR CORRESPONDS TO
C     A CRIB RADIUS NUMBER.  IF IT DOES, DO NOT SOLVE FOR THE
C     COEFFICIENTS AND WRITE ZEROS FOR THE CRIB AREA COEFFICIENTS.
      DO 3 LL=1,50
      ISPOT = CHECKR(LL)
C      WRITE(*,*) 'Debug: IRT = ', IRT
C      WRITE(*,*) 'Debug: ISPOT = ', ISPOT
      IF(IRT.EQ.ISPOT) GO TO 4
    3 CONTINUE
      GO TO 101
    4 CSZ=0.
      CST=0.
      CTR=0.
      CSR=0.
      COM=0.
      DO 5 III=1,IZ
C      WRITE(*,*) '**** Write to scratch file ****'
      WRITE(1,511) CSZ,CST,CTR,CSR,COM
C      WRITE(*,*) '**** End write to scratch file ****'
    5 CONTINUE
      GO TO 100
  101 KZ = 0
      IF (IRT-IR)1050,1050,10
 1050 R = RR(IRT)
      DO 31 I=1,IZ
      DO 31 J=1,N
      TZ = ABS(H(J)-ZZ(I))
      IF (TZ-.0001)32,32,31
   32 ZZ(I) = -H(J)
   31 CONTINUE
C     WRITE(*,355)
      NLINE = NLINE+1
  355 FORMAT (1H )
C      write(*,*) 'calculate the partition'
C  ** CALCULATE THE PARTITION **
      CALL PART
C      write(*,*) 'calculate the coefficients'
C  ** CALCULATE THE COEFFICIENTS **
      DO 125 I=1,ITN4
      P = AZ(I)
  107 CALL COEE(I)
  109 IF (R)115,115,110
  110 PR=P*R
      CALL BESSEL(0,PR,Y)
      RJ0(I)=Y
      CALL BESSEL(1,PR,Y)
      RJ1(I)=Y
  115 PA=P*AR
      CALL BESSEL(1,PA,Y)
      AJ(I)=Y
  125 CONTINUE
  195 IZT=0
C  ** START ON A NEW Z **
  200 IZT=IZT+1
      KZ=KZ+1
      IF (IZT-IZ)205,205,100
  205 Z=ABS(ZZ(IZT))
      IF (NLINE-54)207,206,206
  206 NPAGE=NPAGE+1
      NLINE=8
      IF(IOPT.EQ.1) WRITE(*,350)(ASTER,I=1,5),(TITLE(I),I=1,20)
     3,(ASTER,I=1,4),NPAGE
      IF(IOPT.EQ.1) WRITE(*,352)(PERD,J=1,27)
  207 CONTINUE
C  ** FIND THE LAYER CONTAINING Z **
      TZZ=0.0
      DO 210 J1=1,N
      J=NS-J1
      IF (Z-H(J))210,215,215
  210 CONTINUE
      L=1
      GO TO 34
  215 L=J+1
      IF (ZZ(IZT))33,34,34
   33 L=J
      TZZ=1.0
   34 CONTINUE
C      write(*,*) 'call to calcin'
C      CALL CALCIN
      IF (TZZ)36,36,35
      write(*,*) 'reached line 787'
   35 ZZ(IZT) = -ZZ(IZT)
      IZT=IZT-1
   36 CONTINUE
      IF (NPUN)200,200,220
  220 NZP=IZT-1
      NRP=IRT-1
      GO TO 200
   10 CONTINUE
      write(*,*) 'reached line 795'
C     ***************
C     AFTER ALL THE COEFFICIENTS HAVE BEEN CALCULATED, READ THE
C     COEFFICIENT MATRIX AND PRINT THE COEFFICIENT MATRIX
      REWIND 1
      READ(1,511) (((COEF(IRR,ID,L),L=1,5),ID=1,IZ),IRR=1,IR)
  511 FORMAT (5E12.5)
      IF(IOPT.EQ.0) GO TO 405
      WRITE(*,403)
      DO 402 IRR=1,IR
      DO 401 ID=1,IZ
C*******************************************************************
C PARENTHESIS
      WRITE(*,404) IRR,ID,(COEF(IRR,ID,L),L=1,5)
  401 CONTINUE
  402 CONTINUE
  403 FORMAT(1H1,3X,'CALCULATED',2X,'CALCULATED',/,
     1      5X,'RADIUS',6X,'DEPTH',6X,'SIGMA',8X,'SIGMA',
     2      9X,'TAU',8X,'SIGMA',6X,'VERTICAL',/,
     3      6X,'NO.',9X,'NO.',9X,'Z',10X,'THETA',7X,
     4      'R-THETA',8X,'R',10X,'DISP.'//)
  404 FORMAT (6X,I3,10X,I1,6X,1PE10.3,4(3X,1PE10.3))
C     ***************
  405 IPERM=0
      write(*,*) 'reached 817'
      IF(LACOPT.EQ.2) IPERM=1
      write(*,*) 'line 890: IPERM = ', IPERM
C 3531 IF(LACOPT.GT.0) CALL LAC(IPERM)
      write(*,*) 'line 892'
      RETURN
      END






C**********************************Q************************************
       SUBROUTINE PART
C**********************************Q************************************
      COMMON/CBPC/AR,AZ(396),I1,R
      COMMON /PBC/ BZ(100),PM(6,4,4)
      COMMON /PC/ ITN,NTEST,SF
      COMMON /PCC/ K
C***********************************************************************
      IF (BZ(2))1,1,4
C         ** COMPUTE ZEROS OF J1(X) AND J0(X). SET UP GAUSS CONSTANTS **
    1 BZ(1)=0.0
      BZ(2)=1.0
      BZ(3)=2.4048
      BZ(4)=3.8317
      BZ(5)=5.5201
      BZ(6)=7.0156
      K=ITN+1
      DO 2 I=7,K,2
      T=I/2
      TD=4.0*T-1.0
    2 BZ(I)=3.1415927E0*(T-0.25+0.050661/TD-0.053041/TD**3+0.262051/TD**
     15)
      DO 3 I=8,ITN,2
      T=(I-2)/2
      TD=4.0*T+1.0
    3 BZ(I)=3.1415927E0*(T+0.25-0.151982/TD+0.015399/TD**3-0.245270/TD**
     15)
      G1=0.86113631E0
      G2=0.33998104E0
    4 ZF=AR
      NTEST=2
      IF (R)8,8,9
    9 CONTINUE
      NTEST=AR/R+.0001
      IF (NTEST)6,6,5
    6 CONTINUE
      NTEST=R/AR+0.0001
      ZF=R
    5 CONTINUE
      NTEST=NTEST+1
      IF (NTEST-10)8,8,7
    7 CONTINUE
      NTEST=10
    8 CONTINUE
C             ** COMPUTE POINTS FOR LEGENDRE-GAUSS INTEGRATION **
   15 K=1
      ZF=2.0*ZF
      SZ2=0.0
      DO 28 I=1,ITN
      SZ1=SZ2
      SZ2=BZ(I+1)/ZF
      SF=SZ2-SZ1
      PM(1,1,1)=SZ2+SZ1
      SG1=SF*G1
      SG2=SF*G2
      AZ(K)=PM(1,1,1)-SG1
      AZ(K+1)=PM(1,1,1)-SG2
      AZ(K+2)=PM(1,1,1)+SG2
      AZ(K+3)=PM(1,1,1)+SG1
      K=K+4
   28 CONTINUE
   40 RETURN
      END







C***********************************************************************
      SUBROUTINE COEE(KIN)
C***********************************************************************
      DIMENSION X(7,4,4),FM(4),SC(6)
      COMMON  A(396,7),B(396,7),C(396,7),D(396,7),T1,T2,T1M,AJ(396),L,
     1 NLINE,RJ0(396),RJ1(396),TZZ,Z,COM,CMU,CSR,CST,CSZ,CTR
      COMMON /SCFCCI/ E(7),VRAT(7)
      COMMON /PBC/ BZ(100),PM(6,4,4)
      COMMON /CBBBC/ H(6),N
      COMMON/PCC/K11
      COMMON /CBBCC/ P
      COMMON /SCFBCI/ NS,PSI,NL
      DOUBLE PRECISION CTR
C***********************************************************************
C        UNDER CERTAIN CONDITIONS THIS SUBROUTINE WILL UNDERFLOW FOR A F
C        DATA SETS OF A UNIQUE TYPE; HENCE THIS VERSION HAS INTERNAL CHE
C        THAT MACHINE LIMITATIONS ON UNDERFLOW ARE NOT REACHED. THE VALU
C        TOL AND ETOL MAY BE ADJUSTED BY THE USERS TO HIS SPECIFICATIONS
C        HOWEVER IT IS NOTED THAT MACHINE RESULTS ARE NOT EFFECTED FOR
C        TOLERANCES AS HIGH AS E-30
C
C        TWO VERISIONS OF THIS SUBROUTINE ARE PROVIDED WITH THIS DEC-ONE
C        UNDERFLOW PREVENTED AND ONE WITHOUT- IT SHOULD BE NOTED THAT EX
C        TIME IS NEARLY DOUBLED BY USING THE ROUTINE PREVENTING UNDERFLO
C
C        QUESTIONS CONCERNING THE VALIDITY OF THIS MAY BE CLARIFIED BY
C        CONSULTATION WITH DR. E. BARENBERG,U OF I.
C        SOME MACHINES POSESS SOFWARE CAPABLE OF MASKING UNDERFLOW; IN W
C        CASE THESE CHECKS MAY BE REMOVED AND THE MACHINE TRUNCATION SUB
C***********************************************************************
      DATA JJ/0/
C        TOLERANCES ARE SET TO 1.0E-35 FOR NORMAL USUAGE.
      ETOL=-100.0
      TOL=EXP(ETOL)
C        TRUNV IS THE VALUE A VARIBLE IS TO ASSUME IF UNDERFLOW OCCURS
      TRUNV=1.0E-35
C     IF(JJ.GT.1) GO TO 300
C     WRITE(*,301) ETOL,TOL,TRUNV
C 301 FORMAT('0','ETOL=',E12.5,5X,'TOL=',E12.5,5X,'TRUNV=',E12.5)
C 300 JJ=JJ+1
C***********************************************************************
C        SI IS THE SIGN OF A GIVEN VARIABLE.
      LC=KIN
CS-MX              SET UP MATRIX X=DI*MI*KI*K*M*D
C     COMPUTE THE MATRICES X(K)
      IF (LC-1)3,3,2
    3 ISW=1
    2 IF (ISW)99,1,1
    1 DO 10 K=1,N
      T1=E(K)*(1.0+VRAT(K+1))/(E(K+1)*(1.0+VRAT(K)))
      T1M=T1-1.0
      PH=P*H(K)
      PH2=PH*2.0
      VK2=2.0*VRAT(K)
      VKP2=2.0*VRAT(K+1)
      VK4=2.0*VK2
      VKP4=2.0*VKP2
      VKK8=8.0*VRAT(K)*VRAT(K+1)
C
      X(K,1,1)=VK4-3.0-T1
      X(K,2,1)=0.0
      X(K,3,1)=T1M*(PH2-VK4+1.0)
      X(K,4,1)=-2.0*T1M*P
C
      T3=PH2*(VK2-1.0)
      T4=VKK8+1.0-3.0*VKP2
      T5=PH2*(VKP2-1.0)
      T6=VKK8+1.0-3.0*VK2
C
      X(K,1,2)=(T3+T4-T1*(T5+T6))/P
      X(K,2,2)=T1*(VKP4-3.0)-1.0
      X(K,4,2)=T1M*(1.0-PH2-VKP4)
C
      X(K,3,4)=(T3-T4-T1*(T5-T6))/P
C
      T3=PH2*PH-VKK8+1.0
      T4=PH2*(VK2-VKP2)
C
      X(K,1,4)=(T3+T4+VKP2-T1*(T3+T4+VK2))/P
      X(K,3,2)=(-T3+T4-VKP2+T1*(T3-T4+VK2))/P
C
      X(K,1,3)=T1M*(1.0-PH2-VK4)
      X(K,2,3)=2.0*T1M*P
      X(K,3,3)=VK4-3.0-T1
      X(K,4,3)=0.0
C
      X(K,2,4)=T1M*(PH2-VKP4+1.0)
      X(K,4,4)=T1*(VKP4-3.0)-1.0
   10 CONTINUE
C     COMPUTE THE PRODUCT MATRICES PM
      SC(N)=4.0*(VRAT(N)-1.0)
      IF (N-2) 13,11,11
   11 DO 12 K1=2,N
      M=NS-K1
      SC(M)=SC(M+1)*4.0*(VRAT(M)-1.0)
   12 CONTINUE
   13 CONTINUE
C
      DO 26 K1=1,N
      K=NS-K1
      IF (K-N)23,21,21
   21 DO 22 M=1,4
      DO 22 J=1,4
   22 PM(K,M,J)=X(K,M,J)
      GO TO 26
   23 TEXP=P*(H(K)-H(K+1))
      IF(TEXP.LT.ETOL) TEXP=ETOL
      IF(TEXP.LT.ETOL) WRITE(*,210) TEXP
  210 FORMAT(1X,'TEXP=',E12.5)
      T1=EXP(TEXP)
      T2=1.0/T1
      DO 20 M=1,4
      DO 20 J=1,4
   20 PM(K,M,J)=0.0
      DO 25 M=1,4
      DO 25 J=1,4
      IF(PM(K+1,1,J).EQ.0.0) GO TO 50
      SI=PM(K+1,1,J)/ABS(PM(K+1,1,J))
      IF(ABS(PM(K+1,1,J)).LT.TOL) PM(K+1,1,J)=TRUNV*SI
      IF(ABS(PM(K+1,1,J)).LT.TOL) WRITE(*,211) PM(K+1,1,J)
  211 FORMAT(1X,'PM(K+1,1,J)=',E12.5)
   50 IF(PM(K+1,2,J).EQ.0.0) GO TO 51
      SI=PM(K+1,2,J)/ABS(PM(K+1,2,J))
      IF(ABS(PM(K+1,2,J)).LT.TOL) PM(K+1,2,J)=TRUNV*SI
      IF(ABS(PM(K+1,2,J)).LT.TOL) WRITE(*,211) PM(K+1,2,J)
   51 IF(PM(K+1,3,J).EQ.0.0) GO TO 52
      SI=PM(K+1,3,J)/ABS(PM(K+1,3,J))
      IF(ABS(PM(K+1,3,J)).LT.TOL) PM(K+1,3,J)=TRUNV*SI
      IF(ABS(PM(K+1,3,J)).LT.TOL) WRITE(*,211) PM(K+1,3,J)
   52 IF(PM(K+1,4,J).EQ.0.0) GO TO 53
      SI=PM(K+1,4,J)/ABS(PM(K+1,4,J))
      IF(ABS(PM(K+1,4,J)).LT.TOL) PM(K+1,4,J)=TRUNV*SI
      IF(ABS(PM(K+1,4,J)).LT.TOL) WRITE(*,211) PM(K+1,4,J)
   53 T6=(X(K,M,1)*PM(K+1,1,J)+X(K,M,2)*PM(K+1,2,J))*T1+(X(K,M,3)*PM(K+1
     1,3,J)+X(K,M,4)*PM(K+1,4,J))*T2
      IF (ABS(T6)-1.0E35)24,24,99
   24 PM(K,M,J)=T6
   25 CONTINUE
   26 CONTINUE
      DO 30 K1=1,N
      EHNP=-P*(H(N)+H(K1))
      IF(EHNP.LT.ETOL) EHNP=ETOL
      T1=EXP(EHNP)
      EHNM=-P*(H(N)-H(K1))
      IF(EHNM.LT.ETOL) EHNM=ETOL
      T2=EXP(EHNM)
      DO 31 M=1,2
      DO 31 J=3,4
      IF(PM(K1,M,J).EQ.0.0) GO TO 54
      SI=PM(K1,M,J)/ABS(PM(K1,M,J))
      IF(ABS(PM(K1,M,J)).LT.TOL) PM(K1,M,J)=TRUNV*SI
      IF(ABS(PM(K1,M,J)).LT.TOL) WRITE(*,201) PM(K1,M,J)
  201 FORMAT(1X,'PM(K1,M,J)=',E12.5)
   54 PM(K1,M,J)=T1*PM(K1,M,J)
      IF(PM(K1,M+2,J).EQ.0.0) GO TO 55
      SI=PM(K1,M+2,J)/ABS(PM(K1,M+2,J))
      IF(ABS(PM(K1,M+2,J)).LT.TOL) PM(K1,M+2,J)=TRUNV*SI
      IF(ABS(PM(K1,M+2,J)).LT.TOL) WRITE(*,200) PM(K1,M+2,J)
  200 FORMAT(1X,'PM(K1,M+2,J)=',E12.5)
   55 PM(K1,M+2,J)=T2*PM(K1,M+2,J)
   31 CONTINUE
   30 CONTINUE
C     SOLVE FOR C(NS) AND D(NS)
      T3=2.0*VRAT(1)
      T4=T3-1.0
      FM(1)=P*PM(1,1,3)+T3*PM(1,2,3)+P*PM(1,3,3)-T3*PM(1,4,3)
      FM(2)=P*PM(1,1,3)+T4*PM(1,2,3)-P*PM(1,3,3)+T4*PM(1,4,3)
      FM(3)=P*PM(1,1,4)+T3*PM(1,2,4)+P*PM(1,3,4)-T3*PM(1,4,4)
      FM(4)=P*PM(1,1,4)+T4*PM(1,2,4)-P*PM(1,3,4)+T4*PM(1,4,4)
      DFAC=SC(1)/((FM(1)*FM(4)-FM(3)*FM(2))*P*P)
      A(LC,NS)=0.0
      B(LC,NS)=0.0
      C(LC,NS)=-FM(3)*DFAC
      D(LC,NS)=FM(1)*DFAC
C     BACKSOLVE FOR THE OTHER A,B,C,D
      DO 91 K1=1,N
      IF(PM(K1,1,3).EQ.0.0) GO TO 56
      SI=PM(K1,1,3)/ABS(PM(K1,1,3))
      IF(ABS(PM(K1,1,3)).LT.TOL) PM(K1,1,3)=TRUNV*SI
      IF(ABS(PM(K1,1,3)).LT.TOL) WRITE(*,202) PM(K1,1,3)
  202 FORMAT(1X,'PM(K1,1,3)=',E12.5)
   56 IF(PM(K1,1,4).EQ.0.0) GO TO 61
      SI=PM(K1,1,4)/ABS(PM(K1,1,4))
      IF(ABS(PM(K1,1,4)).LT.TOL) PM(K1,1,4)=TRUNV*SI
      IF(ABS(PM(K1,1,4)).LT.TOL) WRITE(*,203) PM(K1,1,4)
  203 FORMAT(1X,'PM(K1,1,4)=',E12.5)
   61 A(LC,K1)=(PM(K1,1,3)*C(LC,NS)+PM(K1,1,4)*D(LC,NS))/SC(K1)
      IF(PM(K1,2,3).EQ.0.0) GO TO 57
      SI=PM(K1,2,3)/ABS(PM(K1,2,3))
      IF(ABS(PM(K1,2,3)).LT.TOL) PM(K1,2,3)=TRUNV*SI
      IF(ABS(PM(K1,2,3)).LT.TOL) WRITE(*,204) PM(K1,2,3)
  204 FORMAT(1X,'PM(K1,2,3)=',E12.5)
   57 IF(PM(K1,2,4).EQ.0.0) GO TO 64
      SI=PM(K1,2,4)/ABS(PM(K1,2,4))
      IF(ABS(PM(K1,2,4)).LT.TOL) PM(K1,2,4)=TRUNV*SI
      IF(ABS(PM(K1,2,4)).LT.TOL) WRITE(*,205) PM(K1,2,4)
  205 FORMAT(1X,'PM(K1,2,4)=',E12.5)
   64 B(LC,K1)=(PM(K1,2,3)*C(LC,NS)+PM(K1,2,4)*D(LC,NS))/SC(K1)
      IF(PM(K1,3,3).EQ.0.0) GO TO 58
      SI=PM(K1,3,3)/ABS(PM(K1,3,3))
      IF(ABS(PM(K1,3,3)).LT.TOL) PM(K1,3,3)=TRUNV*SI
      IF(ABS(PM(K1,3,3)).LT.TOL) WRITE(*,206) PM(K1,3,3)
  206 FORMAT(1X,'PM(K1,3,3)=',E12.5)
   58 IF(PM(K1,3,4).EQ.0.0) GO TO 66
      SI=PM(K1,3,4)/ABS(PM(K1,3,4))
      IF(ABS(PM(K1,3,4)).LT.TOL) PM(K1,3,4)=TRUNV*SI
      IF(ABS(PM(K1,3,4)).LT.TOL) WRITE(*,207) PM(K1,3,4)
  207 FORMAT(1X,'PM(K1,3,4)=',E12.5)
   66 C(LC,K1)=(PM(K1,3,3)*C(LC,NS)+PM(K1,3,4)*D(LC,NS))/SC(K1)
      IF(PM(K1,4,3).EQ.0.0) GO TO 59
      SI=PM(K1,4,3)/ABS(PM(K1,4,3))
      IF(ABS(PM(K1,4,3)).LT.TOL) PM(K1,4,3)=TRUNV*SI
      IF(ABS(PM(K1,4,3)).LT.TOL) WRITE(*,208) PM(K1,4,3)
  208 FORMAT(1X,'PM(K1,4,3)=',E12.5)
   59 IF(PM(K1,4,4).EQ.0.0) GO TO 91
      SI=PM(K1,4,4)/ABS(PM(K1,4,4))
      IF(ABS(PM(K1,4,4)).LT.TOL) PM(K1,4,4)=TRUNV*SI
      IF(ABS(PM(K1,4,4)).LT.TOL) WRITE(*,209) PM(K1,4,4)
  209 FORMAT(1X,'PM(K1,4,4)=',E12.5)
   91 D(LC,K1)=(PM(K1,4,3)*C(LC,NS)+PM(K1,4,4)*D(LC,NS))/SC(K1)
  100 RETURN
   99 ISW=-1
      DO 98 K1=1,N
      A(LC,K1)=0.0
      B(LC,K1)=0.0
      C(LC,K1)=0.0
   98 D(LC,K1)=0.0
      GO TO 100
      END










C*******************************************************************
SUBROUTINE BESSEL(NI,XI,Y)
C*******************************************************************
      DIMENSION PZ(6),QZ(6)
      DIMENSION P1(6),Q1(6)
      DIMENSION D(20)
C
C
      DATA PZ(1)/0.0/
      IF (PZ(1))9,1,9
    1 PZ(1)=1.0
      PZ(2)=-1.125E-4
      PZ(3)=2.8710938E-7
      PZ(4)=-2.3449658E-9
      PZ(5)=3.9806841E-11
      PZ(6)=-1.1536133E-12
C
      QZ(1)=-5.0E-3
      QZ(2)=4.6875E-6
      QZ(3)=-2.3255859E-8
      QZ(4)=2.8307087E-10
      QZ(5)=-6.3912096E-12
      QZ(6)=2.3124704E-13
C
C
      P1(1)=1.0
      P1(2)=1.875E-4
      P1(3)=-3.6914063E-7
      P1(4)=2.7713232E-9
      P1(5)=-4.5114421E-11
      P1(6)=1.2750463E-12
C
      Q1(1)=1.5E-2
      Q1(2)=-6.5625E-6
      Q1(3)=2.8423828E-8
      Q1(4)=-3.2662024E-10
      Q1(5)=7.1431166E-12
      Q1(6)=-2.5327056E-13
C
C
      PI=3.1415927E0
      PI2=2.0*PI
C
C
    9 N=NI
      X=XI
      IF (X-7.0)10,10,160
C
   10 X2=X/2.0
      FAC=-X2*X2
      IF (N)11,11,14
   11 C=1.0
      Y=C
      DO 13 I=1,34
      T=I
      C=FAC*C/(T*T)
      TEST=ABS(C)-10.0**(-8)
      IF (TEST)17,17,12
   12 Y=Y+C
   13 CONTINUE
   14 C=X2
      Y=C
      DO 16 I=1,34
      T=I
      C=FAC*C/(T*(T+1.0))
      TEST=ABS(C)-10.0**(-8)
      IF (TEST)17,17,15
   15 Y=Y+C
   16 CONTINUE
   17 RETURN
  160 IF (N)161,161,164
C
C
  161 DO 162 I=1,6
      D(I)=PZ(I)
      D(I+10)=QZ(I)
  162 CONTINUE
      GO TO 163
C
  164 DO 165 I=1,6
      D(I)=P1(I)
      D(I+10)=Q1(I)
  165 CONTINUE
  163 CONTINUE
      T1=25.0/X
      T2=T1*T1
      P=D(6)*T2+D(5)
      DO 170 I=1,4
      J=5-I
      P=P*T2+D(J)
  170 CONTINUE
      Q=D(16)*T2+D(15)
      DO 171 I=1,4
      J=5-I
      Q=Q*T2+D(J+10)
  171 CONTINUE
      Q=Q*T1
C
      T4=SQRT(X*PI)
      T6=SIN(X)
      T7=COS(X)
C
      IF (N)180,180,185
C
  180 T5=((P-Q)*T6+(P+Q)*T7)/T4
      GO TO 99
  185 T5=((P+Q)*T6-(P-Q)*T7)/T4
   99 Y=T5
      RETURN
      END




C***********************************************************************
      SUBROUTINE CALCIN
C***********************************************************************
      DIMENSION TEST(11),W(4)
      COMMON  A(396,7),B(396,7),C(396,7),D(396,7),T1,T2,T1M,AJ(396),L,
     1 NLINE,RJ0(396),RJ1(396),TZZ,Z,COM,CMU,CSR,CST,CSZ,CTR
      COMMON /THREE/ TIEL,NSEG,TIESP,NTIES,NLAYER(3),STA(10)
      COMMON/BLOAD/KZ,NBRT,KR,ARG,XSIDE,LACOPT
      COMMON /ONE/ COEF(110,6,5)
      COMMON /SCFCCI/ E(7),VRAT(7)
      COMMON/CBPC/AR,AZ(396),I1,R
      COMMON /PC/ ITN,NTEST,SF
      COMMON /PCC/ K
      COMMON /FBCBI/ NOUTP
      COMMON /OUTPUT/ IOPT, CRIBOPT, ITOPT
      COMMON /CBBCC/ P
      COMMON /SCFBCI/ NS,PSI,NL
      DOUBLE PRECISION CTR
C***********************************************************************
      DATA W(1)/0.0/
C**************Q********************************************************
      IF (W(1))1,1,2
    1 W(1)=0.34785485E0
      W(2)=0.65214515E0
      W(3)=W(2)
      W(4)=W(1)
    2 VL=2.0*VRAT(L)
      EL=(1.0+VRAT(L))/E(L)
      VL1=1.0-VL
      CSZ=0.0
      CST=0.0
      CSR=0.0
      CTR=0.0
      COM=0.0
      CMU=0.0
      NTS1=NTEST+1
      ITS=1
      JT=0
      ARP=AR
      IF (NOUTP)4,4,5
    4 ARP=ARP*PSI
    5 CONTINUE

   10 DO 40 I=1,ITN
C     INITIALIZE THE SUB-INTEGRALS
      RSZ=0.0
      RST=0.0
      RSR=0.0
      RTR=0.0
      ROM=0.0
      RMU=0.0
C     COMPUTE THE SUB-INTEGRALS
      K=4*(I-1)
      DO 30 J=1,4
      J1=K+J
      P=AZ(J1)
C------------
C     MODIFIED TO ACCOUNT FOR MACHINE LIMITATIONS.  IF P*Z > 87.
C     THEN EXP(P*Z) > E38 AND ARITHMETIC OVERFLOW WILL OCCUR
C     OLD EXPRESSION FOR EP WAS:  EP=EXP(P*Z)
C-------------
      PZEXP=P*Z
      IF(PZEXP.GT.87.) PZEXP=87.
      IF(PZEXP.LT.-87.) PZEXP=-87.
      EP=EXP(PZEXP)
      T1=B(J1,L)*EP
      T2=D(J1,L)/EP
      T1P=T1+T2
      T1M=T1-T2
      T1=(A(J1,L)+B(J1,L)*Z)*EP
      T2=(C(J1,L)+D(J1,L)*Z)/EP
      T2P=P*(T1+T2)
      T2M=P*(T1-T2)
      WA=AJ(J1)*W(J)
      IF (R)20,20,15
   15 BJ1=RJ1(J1)*P
      BJ0=RJ0(J1)*P
      RSZ=RSZ+WA*P*BJ0*(VL1*T1P-T2M)
      ROM=ROM+WA*EL*BJ0*(2.0*VL1*T1M-T2P)
      RTR=RTR+WA*P*BJ1*(VL*T1M+T2P)
      RMU=RMU+WA*EL*BJ1*(T1P+T2M)
      RSR=RSR+WA*(P*BJ0*((1.0+VL)*T1P+T2M)-BJ1*(T1P+T2M)/R)
      RST=RST+WA*(VL*P*BJ0*T1P+BJ1*(T1P+T2M)/R)
      GO TO 30
C     SPECIAL ROUTINE FOR R = ZERO
   20 PP=P*P
      RSZ=RSZ+WA*PP*(VL1*T1P-T2M)
      ROM=ROM+WA*EL*P*(2.0*VL1*T1M-T2P)
      RST=RST+WA*PP*((VL+0.5)*T1P+0.5*T2M)
      RSR=RST
   30 CONTINUE
C
      SF=(AZ(K+4)-AZ(K+1))/1.7222726E0
      CSZ=CSZ+RSZ*SF
      CST=CST+RST*SF
      CSR=CSR+RSR*SF
      CTR=CTR+RTR*SF
      COM=COM+ROM*SF
      CMU=CMU+RMU*SF
      RSZ=2.0*RSZ*AR*SF
      TESTH=ABS(RSZ)-10.0**(-4)
      IF (ITS -NTS1)31,32,32
   31 CONTINUE
      TEST(ITS)=TESTH
      ITS=ITS+1
      GO TO 40
   32 CONTINUE
      TEST(NTS1)=TESTH
      DO 33 J=1,NTEST
      IF (TESTH-TEST(J))35,36,36
   35 CONTINUE
      TESTH=TEST(J)
   36 CONTINUE
      TEST(J)=TEST(J+1)
   33 CONTINUE
      IF (TESTH)50,50,40
   40 CONTINUE
      JT=1
   50 CSZ=CSZ*ARP
      CST=CST*ARP
      CTR=CTR*ARP
      CSR=CSR*ARP
      COM=COM*ARP
C     *************
C     WRITE THE INFLUENCE COEFFICIENTS FOR THE PARTICULAR RADIUS
C     AND DEPTH ONTO TAPE 1
      WRITE(1,511) CSZ,CST,CTR,CSR,COM
  511 FORMAT (5E12.5)
C     **************
      CMU=CMU*ARP
      BSTS=CSZ+CST+CSR
      BST=BSTS*(1.0-2.0*VRAT(L))/E(L)
      IF (TZZ)72,72,71
   71 Z=-Z
   72 CONTINUE
      RDS=(CSR-VRAT(L)*(CSZ+CST))/E(L)
      SST=2.0*(1.0+VRAT(L))*CTR/E(L)
      IF(IOPT.EQ.1) WRITE(*,315)R,Z,CSZ,CST,CSR,CTR,BSTS
     1,COM,RDS,SST,BST
  315 FORMAT (1H ,F5.1,F6.1,1X,1P5E12.3,3H * ,E12.3,3H * ,3E12.3)
      NLINE=NLINE+1
      IF(IOPT.EQ.0) GO TO 99
      IF (JT)99,99,60
   60 WRITE(*,316)
  316 FORMAT(1H ,123X,4HSLOW)
      write(*,*) 'end of calcin'
   99 RETURN
      END
C**********************************************************************/
C
C**********************************************************************/
