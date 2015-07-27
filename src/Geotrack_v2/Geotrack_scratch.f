C**********************************************************************/
C     Author: Sadichchha Sharma
C     Date:   07-20-2015
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
      COMMON /CBPC/ AR,AZ(396),I1,R
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
      CALL CALCUL(NPUN)
  990 CONTINUE
      GO TO 992
  991 CALL SCERJ(*99)
      CALL CALCUL(NPUN)
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
      CALL CALCUL(NPUN)

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
      CALL CALCUL(NPUN)

      GO TO 9942
   99 STOP
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
      COMMON  A(396,7), B(396,7), C(396,7), D(396,7), T1, T2, T1M,
     1 AJ(396), L, NLINE, RJ0(396), RJ1(396), TZZ, Z, COM, CMU, CSR,CST,
     1CSZ,CTR
      COMMON /SCFCCI/ E(7),VRAT(7)
      COMMON /GEO/ AY(11,10,6)
      COMMON /ONE/ COEF(110,6,5)
      COMMON /PBC/ BZ(100),PM(6,4,4)
      COMMON /SCFBBI/ HH(7), WGT, IZ, IR, RR(110), ZZ(20)
      COMMON /BLOAD/KZ,NBRT,KR,ARG,XSIDE,LACOPT
C     I1 Changed to I
      COMMON /CBPC/AR,AZ(396),I1,R
      COMMON /PC/ ITN,NTEST,SF
      COMMON /CBBBC/ H(6),N
      COMMON /OUTPUT/ IOPT,CRIBOPT,ITOPT
      COMMON /SCFBCI/ NS,PSI,NL
      COMMON /CBBCC/ P
      COMMON /CONTA/NCNTCT(30),NUMCON
      COMMON /CFBBBI/TITLE(20)
      COMMON /SIX/ MINSEG,MAXSEG,MINTOUT
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

C     WRITE(*,*) 'Debug: NEWFLAG: ', NEWFLAG
C      WRITE(*,*) 'Debug: CRIBOPT: ', CRIBOPT

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
C      WRITE(*,*) 'IZ=', IZ
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
      CALL CALCIN
      IF (TZZ)36,36,35
   35 ZZ(IZT) = -ZZ(IZT)
      IZT=IZT-1
   36 CONTINUE
      IF (NPUN)200,200,220
  220 NZP=IZT-1
      NRP=IRT-1
      GO TO 200
   10 CONTINUE
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
      IF(LACOPT.EQ.2) IPERM=1
 3531 IF(LACOPT.GT.0) CALL LAC(IPERM)
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
   99 RETURN
      END








C***********************************************************************
      SUBROUTINE LAC(IPERM)
C
C     ******************************************************************
C
C     BALLAST-SUBGRADE MODEL
C                        DISTRIBUTION OF WHEEL LOADS ONTO BALLAST
C
C     ******************************************************************
C     ******************************************************************
      DIMENSION X(15,15),BP(11,10),BULKSTR(17,5),XL(10),F(10),XCOR(30)
      DIMENSION RESPW(6,5,5),RESPX(6,5,5),RESPY(6,5,5),
     1          RESPZ(6,5,5),RESPXY(6,5,5),RESPXZ(6,5,5),
     2          RESPYZ(6,5,5),C(10)
      DIMENSION CRBPW(10,5,5),CRBPX(10,5,5),CRBPY(10,5,5),
     1          CRBPZ(10,5,5),CRBPXY(10,5,5),CRBPXZ(10,5,5),
     2          CRBPYZ(10,5,5),CRWS(16,5,5),CRXS(16,5,5),CRYS(16,5,5),
     3          CRZS(16,5,5),CRXYS(16,5,5),CRXZS(16,5,5),CRYZS(16,5,5)
      COMMON/FIVE/ BPS(17,5),YIS(3,17),EOLD(6),DD(5),SV(5),
     1         RWS(17,5,5),RXS(17,5,5),RYS(17,5,5),RZS(17,5,5),
     2         RXYS(17,5,5),RXZS(17,5,5),RYZS(17,5,5),TDFL(17,5)
      COMMON/FOUR/NAXLES,LP(4),MINTOUT,MAXTOUT,MAXCOUT,MINSEG,MAXSEG,
     1   ICTRIX,ICPRIN,ICSTD,ITSTD,ITRIAX,ITPRIN,PWHL(4),KTYPE(9)
C     ******************************************************************
      DIMENSION S(50,50),V(50),T(15,15),TT(15),RT(15)
      DIMENSION YI(4,20),R(15,15),RM(15),RP(15),RN(15)
      COMMON /ONE/ COEF(110,6,5)
      DIMENSION RO(6),DS(9),EIGVEC(3,3),WK(3)
      DIMENSION LM(17),TCM(17),TM(17,5),SM(17,3),RSM(17)
      DIMENSION STIF(2,15)
      DIMENSION RAD(110)
      COMMON /TWO/KONE(6),KTWO(6),NSTEPS,NITER,GAMMA(6),KNOT(6),HHH(6),
     *SIGMAV(6),THETAI(6),THETAF(6),INTERF,NP,NLAST
      COMMON /SCFCCI/ E(7),VRAT(7)
      COMMON /INPUT/ TA,TE,TI,MI,WI,RA,RE,RI,STIF1,PR,TWT,RWT,TIEWD
      COMMON/GEO/ AY(11,10,6)
      COMMON /SCFBBI/ HH(7),IR,IZ,RR(110),WGT,ZZ(20)
      COMMON/CONTA/NCNTCT(30),NUMCON
      COMMON /SCFBCI/ NS,PSI,NL
      COMMON/CFBBBI/TITLE(20)
      COMMON/THREE/TIEL,NSEG,TIESP,NTIES,NLAYER(3),STA(10)
      COMMON/OUTPUT/IOPT,CRIBOPT,ITOPT
      COMMON /OFF1/OFFW(5),OFFX(5),OFFY(5),OFFZ(5),OFFXY(5),OFFXZ(5),
     *             OFFYZ(5),ANGNEW(60)
      COMMON /OFF2/ NEWFLAG,NOTHER,OFFSET
      DOUBLE PRECISION SX1,SX1SYM,SY1,SY1SYM,SZ1,SZ1SYM,
     1       SXY,SXYSYM,SXZ,SXZSYM,SYZ,SYZSYM
      REAL*8 KONE,KTWO,KNOT,LM,J2
      INTEGER CRIBOPT
C     DATA TIE/'TIE'/,RAIL/'RAIL'/
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
C     DATA TIE/0/,RAIL/0/
      MDIM=50
      NDIM=15
C
C  *** ENTER THE TITLE OF THIS PROGRAM ***
C

      IF(NSTEPS.GE.1) GO TO 1515
      IF(IOPT.EQ.0) GO TO 53
      WRITE(*,20)TITLE
  20  FORMAT(1H1,10X,'%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
     %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%',/,11X,'%',81X,'%',/,11X,'%',81X
     %,'%',/11X,'%',5X,20A4,4X,'%',/,11X,'%',81X,'%',/,11X,'%',30X,/,
     %21X,'%',/,11X,'%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
     %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%',////)
C
C  *** ENTER LOADING AS WELL AS RAIL AND TIE PROPERTIES  ***
C
C     IF SYMMETRY IN  TIE THEN ENTER 'TIE'  AFTER PXL
C     IF SYMMETRY IN RAIL THEN ENTER 'RAIL' AFTER QXL
      IF(IOPT.EQ.0) GO TO 53
      WRITE(*,40)
  40  FORMAT(40X,'INPUT DATA FOR THIS CASE',/,32X,3('-------------'),//)
  50  FORMAT(25X,' PR- RIGHT RAIL LOAD FOR THIS CASE         =',F13.3,/,
     *       25X,'PXR- DISTANCE OF PR FROM TIE NO. ONE       =',F13.3,/,
     *       25X,' PL- LEFT RAIL LOAD FOR THIS CASE          =',F13.3,/,
     *       25X,'PXL- DISTANCE OF PL FROM TIE NO. ONE       =',F13.3,/,
     *       25X,' QR- RIGHT RAIL MOMENT FOR THIS CASE       =',F13.3,/,
     *       25X,'QXR- DISTANCE OF QR FROM TIE NO. ONE       =',F13.3,/,
     *       25X,' QL- LEFT RAIL MOMENT FOR THIS CASE        =',F13.3,/,
     *       25X,'QXL- DISTANCE OF QL FROM TIE NO. ONE       =',F13.3,/,
     *       25X,'  N- NO. OF TIES                           =',I5,/,
     *       25X,' TS- LONGITUDINAL TIE SPACING               ',/,
     *       25X,'                  ( CENTER TO CENTER )     =',F13.3,/,
     *       25X,'  M- NO. OF DIVISIONS IN A TIE             =',I5,/,
     *       25X,' MO- NO. OF TIE DIVISIONS OUTSIDE OF RAILS =',I5,/,
     *       25X,' MI- NO. OF TIE DIVISIONS INSIDE OF RAILS  =',I5,/,
     *       25X,' WO- LENGTH OF TIE OUTSIDE OF RAILS        =',F13.5,/,
     *       25X,' WI- LENGTH OF TIE INSIDE OF RAILS         =',F13.5)
  52  FORMAT(25X,' RA- CROSS-SECTIONAL AREA OF RAIL          =',F13.3,/,
     *       25X,' RE- YOUNGS MODULUS OF RAIL                =',F13.3,/,
     *       25X,' RI- MOMENT INERTIA OF RAIL                 ',/,
     *       25X,'                  ( ABOUT MAJOR AXIS )     =',F13.3,/,
     *       25X,' TA- CROSS-SECTIONAL AREA OF TIE           =',F13.3,/,
     *       25X,' TE- YOUNGS MODULUS OF TIE                 =',F13.3,/,
     *       25X,' TI- MOMENT INERTIA OF TIE                  ',/,
     *       25X,'                  ( ABOUT MAJOR AXIS )     =',F13.3,/,
     *       25X,' ND- NO. OF FINITE ELEMENTS IN              ',/,
     *       25X,'                     ONE DIVISION OF A TIE =',I5,/,
     *       25X,' NE- STARTING NODE NUMBER FROM              ',/,
     *       25X,'                     ONE END OF A TIE      =',I5,/,
     *       25X,' ZM MAX. LENGTH ALONG Z-AXIS               ',/,
     *       25X,'              AT WHICH OUTPUT IS DESIRED   =',F13.3,/,
     *       25X,' NP- NO. OF OUTPUT ELEMENTS DESIRED        =',I5,/,
     *       25X,' NZ- NO. OF SECTION ALONG Z-AXIS           =',I5,/)
C
   53 PL=PR
      PXR=5.0*TIESP
      PXL=PXR
      TID=3HTIE
      QR=0
      QXR=0
      QXL=0
      QL=0
      RID=4HRAIL
      MO=10-MI
      WO=TIEL-WI
      N=NTIES*2-1
      TS=TIESP
      M=NSEG
      ND=4
      NE=6
      ZM=5.0*TIESP
      NZ=11
      DO 75 I=1,2
       DO 75 J=1,N
      STIF(I,J)=STIF1
   75 CONTINUE
 1515 CONTINUE
      M=MO+MI
      IF(IOPT.EQ.0) GO TO 83
      IF(NSTEPS.GE.1) GO TO 83
C
      WRITE(*,50)PR,PXR,PL,PXL,QR,QXR,QL,QXL,N,TS,M,MO,MI,WO,WI
      WRITE(*,52)RA,RE,RI,TA,TE,TI,ND,NE,ZM,NP,NZ
C
   83 IF(TID.EQ.TIE) M=M/2
      IF(TID.EQ.TIE) MO=MO/2
      IF(TID.EQ.TIE) MI=MI/2
      IF(TID.EQ.TIE) WI=WI/2
      IF(TID.EQ.TIE) WO=WO/2
      IF (RID.EQ.RAIL) N=N/2+1
      IF(IOPT.EQ.0) GO TO 91
      IF(NSTEPS.GE.1) GO TO 91
      IF (RID.EQ.RAIL) WRITE(*,85)
   85 FORMAT(35X,'***   SYMMETRICAL IN RAIL   ***')
      IF (TID.EQ.TIE) WRITE(*,90)
   90 FORMAT(35X,'***   SYMMETRICAL IN TIE    ***')
   91 IF(IPERM.EQ.1) GO TO 501
      MN=M*N
      NTE=MN+3*N
      IF(TID.NE.TIE) NTE=MN+6*N
      KK=MN-M
      NN=N-1
      CO=WO/MO
      CI=WI/MI
      WL=WI*2.
      IF(TID.NE.TIE) WL=WI
C
      IF(NEWFLAG.EQ.1) GO TO 501
C     DEFINE THE TIE FLEXIBILITY COEFS. USING THE REVISED STIE
C     SUBROUTINE.
C
      IF(TID.EQ.TIE) CALL STIE(M,WL,TIEL,NSEG,TI,TE,T,NDIM)
      IF(TID.NE.TIE) CALL UNTIE(M,MO,MI,CO,CI,WL,TI,TE,T,TT,NDIM)
C
C
C     SET UP THE SOIL COEFF. SYMMETRIC MATRIX
C
      ITMARK=0
  105 DO 25 I=1,NDIM
      RM(I)=0.0
      RP(I)=0.0
      DO 25 J=1,NDIM
      R(I,J)=0.0
   25 X(I,J)=0.0
      DO 30 I=1,MDIM
      V(I)=0.
      DO 30 J=1,MDIM
   30 S(I,J)=0.
      CALL SOIL(M,N,MN,S,TS,RID,ND,NE,NZ,MDIM,IZ)
      DO 100 LL=1,N
      DO 100 I=1,M
      DO 100 J=1,M
      L=LL-1
  100 S(L*M+I,L*M+J)=S(L*M+I,L*M+J)-T(I,J)
      IF(NLAST.EQ.0) GO TO 246
      IF(NSTEPS.LT.NITER) GO TO 246
      ITMARK=ITMARK+1
      DO 245 I=1,NUMCON
      J=NCNTCT(I)
      S(J,J)=S(J,J)*1.0E10
      V(J)=S(J,J)*XCOR(J)
  245 CONTINUE
  246 CONTINUE
      DO 110 J=1,N
      DO 110 I=1,M
      K=J-1
      S(K*M+I,MN+2*N+J)=-1.0
      IF(TID.EQ.TIE) GO TO 110
      S(K*M+I,MN+J)=-TT(I)
      S(K*M+I,MN+2*N+J)=-1.0+TT(I)
  110 CONTINUE
C
C     SPRING FORCE EQULIBRIUM EQUATIONS
C
      DO 131 I=1,N
      S(MN+2*N+I,MN+I)=-1.0
      S(MN+2*N+I,MN+N+I)=-1.0/STIF(2,I)
      S(MN+2*N+I,MN+2*N+I)=1.0
  131 CONTINUE
C
C
C     SET UP THE TIE COEFF.
C
      DO 120 I=1,N
      S(MN+I,MN+N+I)=-1.0
      IF(TID.EQ.TIE) GO TO 120
      S(MN+I,MN+3*N+I)=-1.0
      S(MN+2*N+I,MN+3*N+I)=-1.0
  120 CONTINUE
      DO 130 I=1,N
      DO 130 J=1,M
      K=I-1
      IF(TID.NE.TIE) S(MN+2*N+I,K*M+J)=TT(J)
  130 S(MN+I,K*M+J)=1.
C
C     LOAD IN RAIL COEFF.
C
      IF (NN.EQ.0) GO TO 180
      IF (RID.EQ.RAIL) CALL SRAIL(NN,TS,TOL,RE,RI,RA,R,NDIM)
      IF (RID.NE.RAIL)
     * CALL USRAIL(NN,TS,TOL,RE,RI,RA,R,QXR,RM,PXR,RP,NDIM)
      IF(RID.NE.RAIL.AND.TID.NE.TIE)
     * CALL USRAIL(NN,TS,TOL,RE,RI,RA,R,QXL,RN,PXL,RT,NDIM)
      DO 150 I=1,NN
      S(MN+N+I,MN+1)=-1.0
      S(MN+N+I,MN+I+1)=1.0
      IF(TID.EQ.TIE) GO TO 140
      S(MN+3*N+I,MN+2*N+1)=-1.0
      S(MN+3*N+I,MN+2*N+1+I)=1.0
  140 CONTINUE
      DO 150 J=1,NN
      S(MN+N+I,MN+N+1+J)=R(I,J)
      IF(TID.NE.TIE) S(MN+3*N+I,MN+3*N+1+J)=R(I,J)
  150 CONTINUE
      IF (RID.NE.RAIL) GO TO 200
      DO 160 I=1,NN
      IF(TID.NE.TIE) S(NTE,MN+3*N+I)=1.0
  160 S(MN+2*N,MN+N+I)=1.
      DO 170 I=1,NN
      IF(TID.NE.TIE)
     *V(MN+3*N+I)=PL*(I*TS)*(3.*TOL*TOL-4.*(I*TS)**2)/(48.*RE*RI)*(-1.)
  170 V(MN+N+I)=PR*(I*TS)*(3.*TOL*TOL-4.*(I*TS)**2)/(48.*RE*RI)*(-1.)
  180 V(MN+N+N)=PR/2.0
      IF(TID.EQ.TIE) GO TO 190
      V(NTE)=PL/2.0
      S(MN+2*N,MN+2*N)=0.5
  190 S(MN+2*N,MN+2*N)=0.5
      GO TO 240
  200 CONTINUE
      IF(NN.EQ.0) GO TO 220
      DO 210 I=1,NN
      TSI=TS*I
      TSL=TS*I
      TSI=TOL-TSL
      S(MN+N+I,MN+1)=-TSI/TOL
      S(MN+N+I,MN+N)=-1.0+TSI/TOL
      V(MN+N+I)=RP(I)*PR
      V(MN+N+I)=RM(I)*QR+V(MN+N+I)
      IF(TID.EQ.TIE) GO TO 210
      S(MN+3*N+I,MN+2*N+1)=-TSI/TOL
      S(MN+3*N*I,MN+3*N)=-1.0+TSI/TOL
      V(MN+3*N+I)=RT(I)*PL
      V(MN+3*N+I)=RN(I)*QL+V(MN+3*N+I)
  210 CONTINUE
  220 CONTINUE
      DO 230 I=1,N
      S(MN+2*N-1,MN+N+I)=1.0
      S(MN+2*N,MN+N+I)=(I-1)*TS
      IF(TID.EQ.TIE) GO TO 230
      S(NTE-1,MN+3*N+I)=1.0
      S(NTE,MN+3*N+I)=(I-1)*TS
  230 CONTINUE
      V(MN+2*N-1)=PR
      V(MN+2*N)=QR+PR*PXR
      IF(TID.EQ.TIE) GO TO 240
      V(NTE-1)=PL
      V(NTE)=QL+PL*PXL
  240 CONTINUE
C
      CALL SLVEQ(S,V,NTE,MDIM)
C     CALCULATE STRUCTURAL WEIGHTS
C
      TTWT=TWT/NSEG
      RRWT=RWT*TIESP/36.
      SEGL=TIEL/NSEG
      HALF=SEGL/2.
      W=WO-HALF
      DO 241 I=1,10
      J=I-1
      XL(I)=ABS(W-J*SEGL)
  241 CONTINUE
      SUM=0.
      DO 242 I=1,10
      SUM=SUM+(XL(1)/XL(I))
  242 CONTINUE
      F(1)=RRWT/SUM
      DO 243 I=1,10
      F(I)=F(1)*(XL(1)/XL(I))
  243 CONTINUE
      DO 244 I=1,5
      J=11-I
      F(I)=F(I)+F(J)+TTWT
  244 CONTINUE
C
C
      J=0
      SUM=0.0
      DO 251 I=1,MN
      IF(I.LE.30) K=5
      IF(I.LE.25) K=4
      IF(I.LE.20) K=3
      IF(I.LE.15) K=2
      IF(I.LE.10) K=1
      IF(I.LE.5) K=0
      K=I-5*K
      FS=-1.*F(K)
      IF(V(I).GT.FS) GO TO 251
      EXCESS=V(I)+F(K)
      SUM=SUM+EXCESS
      J=J+1
      NCNTCT(J)=I
      XCOR(J)=FS
  251 CONTINUE
      NUMCON=J
      IF(ITMARK.EQ.0) GO TO 106
      IF(ITMARK.EQ.1) WRITE(*,8050)
 8050 FORMAT(//,19X,
     1      'STATIC WEIGHTS ASSIGNED TO TIE SEGMENTS (LBS.)',
     2      5X,'ITERATION',4X,'TOTAL EXCESS',/,
     3      19X,'SEG 1',5X,'SEG 2',5X,'SEG 3',5X,'SEG 4',
     4      5X,'SEG 5',19X,'TENSION',/)
C*******************************************************************
C PARENTHESIS
      WRITE(*,8060) (F(I),I=1,5),ITMARK,SUM
 8060 FORMAT(14X,5(4X,F6.2),9X,I2,10X,F8.2)
      IF(NLAST.EQ.0) GO TO 106
      IF(NSTEPS.LT.NITER) GO TO 106
      IF(ITMARK.GT.10) GO TO 106
      CHECK=0.001*PL
      IF(ABS(SUM).GT.CHECK) GO TO 105
  106 CONTINUE
      DO 15 L=1,5
      J=11-I
   15 F(J)=F(L)
      DO 14 K=MINSEG,MAXSEG
   14 STA(K)=F(K)/(SEGL*TIEWD)
      DO 250 I=1,N
      DO 250 J=1,M
      II=(I-1)*M+J
      X(I,J)=V(II)
      YI(1,I)=V(MN+I)
      YI(2,I)=V(MN+N+I)
      YI(3,I)=V(MN+2*N+I)
      IF(TID.EQ.TIE) GO TO 250
      YI(3,I)=V(MN+2*N+I)
      YI(4,I)=V(MN+3*N+I)
  250 CONTINUE
      IF(ITSTD.EQ.0) GO TO 501
      IF((NSTEPS.LT.NITER).AND.(ITOPT.EQ.0)) GO TO 2901
      WRITE(*,260)NSTEPS
  260 FORMAT(1H1,/' ITERATION=',I2/45X,'----------------------------'
     -'-------',/,49X,'  DEFLECTIONS AND REACTIONS  ',/,51X,
     1'NEG. DEFL. IS DOWNWARD',/,51X,'NEG. REACTION IS TENSION',/,46X,
     1'-----------------------------------',///,50X,
     1'---- SINGLE AXLE ----',///)
      WRITE(*,270)
  270 FORMAT(1X,3HTIE,5X,10HDEFLECTION,7X,8HREACTION,5X,10HDEFLECTION,
     $25X,21HTIE AND SOIL REACTION,/,2X,2HNO, 8X,4HRAIL,9X,9HRAIL SEAT,
     $        9X,3HTIE        ,10X,5HDIV 1,10X,5HDIV 2,10X,5HDIV 3,10X,
     $5HDIV 4,10X,5HDIV 5,/)
C***********************************************************************
C     NO PRINTING
      DO 290 I=1,N
      NMOD=7-I
      WRITE(*,280) I,YI(1,NMOD),YI(2,NMOD),YI(3,NMOD),
     1             (X(NMOD,J),J=1,5)
  280 FORMAT(1X,I3,8(2X,E13.5))
  290 CONTINUE
 2901 TPRM=0.0
      TL=WI+WO
      DIVL=TL/M
      DIST=(WO-DIVL*MO)
      IF(RID.NE.RAIL)GO TO 999
      DO 1002 J=1,N
      LM(J)=YI(2,J)*TS*(N-J)
      TCM(J)=0.0
      DO 2001 I=1,M
      TM(J,I)=X(J,I)*(M+.5-I)*DIVL
 2001 TCM(J)=TCM(J)+TM(J,I)
      TCM(J)=TCM(J)-YI(2,J)*WI
      RSM(J)=0.0
      DO 3000 K=1,MO
      SM(J,K)=X(J,K)*((MO+.5-K)*DIVL+DIST)
 3000 RSM(J)=RSM(J)+SM(J,K)
 1002 TPRM=TPRM+LM(J)
      IF((NSTEPS.LT.NITER).AND.(ITOPT.EQ.0)) GO TO 404
      DO 1101 I=1,6
      DO 1101 J=1,5
      TDFL(I,J)=0.
      DO 1101 K=1,5
      TDFL(I,J)=TDFL(I,J)+X(I,K)*T(K,J)
 1101 CONTINUE
      DO 1102 I=1,6
      DO 1102 J=1,5
      TDFL(I,J)=TDFL(I,J)+YI(1,I)+(YI(2,I)/STIF1)
 1102 CONTINUE
 1004 WRITE(*,403)
  403 FORMAT(//,2X,'TIE',7X,'TIE CENTER',7X,'TIE RAIL-SEAT',32X,
     1'TIE SEGMENT DEFLECTION',/,2X,'NO.',5X,'BENDING MOMENT',
     25X,'BENDING MOMENT',13X,'DIV 1',10X,'DIV 2',10X,'DIV 3',
     310X,'DIV 4',10X,'DIV 5',/)
      DO 4000 J=1,N
      JMOD=7-J
 4000 WRITE(*,401) J,TCM(JMOD),RSM(JMOD),(TDFL(JMOD,K),K=1,5)
  401 FORMAT(2X,I3,5X,E13.5,5X,E13.5,10X,5(2X,E13.5))
      WRITE(*,402)TPRM
  402 FORMAT(//,2X,'PEAK RAIL BENDING MOMENT=',E12.5)
C     ********************Check this formula***************************
      U=((((PR/YI(1,6))**4)/(RE*RI))**(1./3.))/4000.
      WRITE(*,405) U
  405 FORMAT(//,2X,'TRACK MODULUS U = ',F8.3,' KIPS/IN./IN.')
  404 GO TO 291
  999 N=N/2
      DO 5001 J=1,N
      LM(J)=YI(2,J)*(PXR-(J-1)*TS)
      TCM(J)=0.0
      DO 6000 I=1,M
      TM(J,I)=X(J,I)*(M+.5-I)*DIVL
 6000 TCM(J)=TCM(J)+TM(J,I)
      TCM(J)=TCM(J)-YI(2,J)*WI
      RSM(J)=0.0
      DO 7000 K=1,MO
      SM(J,K)=X(J,K)*((MO+.5-K)*DIVL+DIST)
 7000 RSM(J)=RSM(J)+SM(J,K)
 5001 TPRM=TPRM+LM(J)
      GO TO 1004
  291 CONTINUE
      IF(TID.EQ.TIE) GO TO 320
      WRITE(*,300)
  300 FORMAT(1X,/////,1X,'TIE    DEFLECTION AT INTERFACE     REACTION BE
     *TWEEN                 REACTION BETWEEN TIE AND SOIL',/,2X,'NO.
     *OF LEFT RAIL AND TIE        LEFT RAIL AND TIE     DIVISION 6     D
     *IVISION 7     DIVISION 8     DIVISION 9     DIVISION 10',/)
      DO 310 I=1,N
      WRITE(*,280)I,YI(3,I),YI(4,I),(X(I,J),J=6,M)
  310 CONTINUE
  320 CONTINUE
C     **************
C     INITIALIZE THE RESPONSE ARRAYS
  501 CONTINUE
      DO 340 III=1,6
      DO 340 JJJ=1,5
      DO 340 KKK=1,5
      RESPW(III,JJJ,KKK)=0.
      RESPX(III,JJJ,KKK)=0.
      RESPY(III,JJJ,KKK)=0.
      RESPZ(III,JJJ,KKK)=0.
      RESPXY(III,JJJ,KKK)=0.
      RESPXZ(III,JJJ,KKK)=0.
      RESPYZ(III,JJJ,KKK)=0.
  340 CONTINUE
      IF(IPERM.EQ.0) GO TO 503
      DO 502 I=1,6
      READ(2,*) (X(I,J),J=1,5)
  502 CONTINUE
C
C     DEFINE THE TIE/SOIL REACTIONS DUE TO TIE SYMMETRY.
C
  503 DO 510 I=1,6
      X(I,10)=X(I,1)
      X(I,9)=X(I,2)
      X(I,8)=X(I,3)
      X(I,7)=X(I,4)
      X(I,6)=X(I,5)
  510 CONTINUE
C
C     CHANGE THE TIE/SOIL REACTION NUMBERING SYSTEM SO THAT TIE NO. 1 IS
C     NOW THE CENTER TIE.
C
      DO 530 I=1,6
      II=7-I
      DO 520 J=1,10
      BP(I,J)=X(II,J)
  520 CONTINUE
  530 CONTINUE
C
C     SET THE TIE/SOIL REACTIONS ON THE 'DUMMY' TIES EQUAL TO ZERO.
C
      DO 540 I=7,11
      DO 540 J=1,10
      BP(I,J)=0.
  540 CONTINUE
      IF((IOPT.EQ.0).AND.(IPERM.EQ.0)) GO TO 5431
C
C     WRITE OUT THE NEWLY NUMBERED TIES AND TIE/SOIL REACTIONS( BP(I,J)'S )
C
      WRITE(*,542)
      DO 541 I=1,11
C*******************************************************************
C PARENTHESIS
      WRITE(*,543) I,(BP(I,J),J=1,10)
  541 CONTINUE
  542 FORMAT(1H1,1X,'TIE',3X,'SEG 1',7X,'SEG 2',7X,'SEG 3',
     1       7X,'SEG 4',7X,'SEG 5',7X,'SEG 6',7X,'SEG 7',
     2       7X,'SEG 8',7X,'SEG 9',7X,'SEG 10',/)
  543 FORMAT(1X,I2,10(2X,E10.3))
 5431 PI=3.1415927
      SEGL=TIEL/NSEG
C
      IF(NEWFLAG.EQ.1) GO TO 4240
C
C     DEFINE AN ARRAY WHOSE ELEMENTS EQUAL THE RADIAL DISTANCES TO ALL
C     TIE SEGMENTS FROM SEGMENT NO. 1 OF TIE NO. 1
C
      DO 545 IX=1,10
      XINCR=(IX-1)*SEGL
      IRADNO=((IX-1)*11)+1
      DO 544 IY=1,6
      YINCR=(IY-1)*TIESP
      RAD(IRADNO)=SQRT(XINCR**2+YINCR**2)
      IRADNO=IRADNO+2
  544 CONTINUE
  545 CONTINUE
 4240 IF(NEWFLAG.EQ.0) GO TO 6999
C     CALCULATE FOR THE OFFSET POSITIONS
C
C
      DO 4001 I=1,5
      OFFW(I)=0.
      OFFX(I)=0.
      OFFY(I)=0.
      OFFZ(I)=0.
      OFFXY(I)=0.
      OFFXZ(I)=0.
      OFFYZ(I)=0.
 4001 CONTINUE
      DO 4243 I=1,6
      DO 4244 J=1,10
      K=(I-1)*10+J
      DO 4245 KZ=2,IZ
      KKZ=KZ-1
      CSZ=COEF(K,KZ,1)
      CST=COEF(K,KZ,2)
      CTR=COEF(K,KZ,3)
      CSR=COEF(K,KZ,4)
      COM=COEF(K,KZ,5)
      IF(I.EQ.1) GO TO 4246
      GO TO 4247
 4246 CSZSYM=0.
      CSTSYM=0.
      CTRSYM=0.
      CSRSYM=0.
      COMSYM=0.
      GO TO 4248
 4247 CSZSYM=CSZ
      CSTSYM=CST
      CTRSYM=CTR
      CSRSYM=CSR
      COMSYM=COM
 4248 IF(I.GT.1) GO TO 4249
      THETA=PI
      THETAP=PI
      GO TO 4250
 4249 THETA=PI-ANGNEW(K)
      THETAP=PI+ANGNEW(K)
 4250 SX1=CSR*COS(THETA)*COS(THETA)+CST*SIN(THETA)*SIN(THETA)
      SX1SYM=CSRSYM*COS(THETAP)*COS(THETAP)+
     1       CSTSYM*SIN(THETAP)*SIN(THETAP)
      SY1=CST*COS(THETA)*COS(THETA)+CSR*SIN(THETA)*SIN(THETA)
      SY1SYM=CSTSYM*COS(THETAP)*COS(THETAP)+
     1       CSRSYM*SIN(THETAP)*SIN(THETAP)
      SZ1=CSZ
      SZ1SYM=CSZSYM
      SXY=(CSR-CST)*SIN(THETA)*COS(THETA)
      SXYSYM=(CSRSYM-CSTSYM)*SIN(THETAP)*COS(THETAP)
      SXZ=CTR*COS(THETA)
      SXZSYM=CTRSYM*COS(THETAP)
      SYZ=CTR*SIN(THETA)
      SYZSYM=CTRSYM*SIN(THETAP)
      SU1=COM
      SU1SYM=COMSYM
      OFFW(KKZ)=OFFW(KKZ)+(SU1+SU1SYM)*BP(I,J)
      OFFX(KKZ)=OFFX(KKZ)+(SX1+SX1SYM)*BP(I,J)
      OFFY(KKZ)=OFFY(KKZ)+(SY1+SY1SYM)*BP(I,J)
      OFFZ(KKZ)=OFFZ(KKZ)+(SZ1+SZ1SYM)*BP(I,J)
      OFFXY(KKZ)=OFFXY(KKZ)+(SXY+SXYSYM)*BP(I,J)
      OFFXZ(KKZ)=OFFXZ(KKZ)+(SXZ+SXZSYM)*BP(I,J)
      OFFYZ(KKZ)=OFFYZ(KKZ)+(SYZ+SYZSYM)*BP(I,J)
 4245 CONTINUE
 4244 CONTINUE
 4243 CONTINUE
      GO TO 7345
C     BEGIN THE CALCULATION OF THE DISPLACEMENTS AND STRESS TENSORS.
C
 6999 DO 600 ITIE=1,6
      YMAX=(6-ITIE)*TIESP
      DO 550 IP=1,5
      DO 500 J=1,10
C
C     DEFINE LOCAL COORDINATE ORIGINS AND RELATIVE ANGLES.
C
      XDIST=(J-IP)*SEGL
      DO 450 K=1,6
      YDIST=(K-1)*TIESP
      RTEMP=SQRT(XDIST**2+YDIST**2)
      IF((XDIST.EQ.0.).AND.(YDIST.EQ.0.)) GO TO 420
      IF(XDIST.EQ.0.) GO TO 421
      IF((XDIST.LT.0.).AND.(YDIST.EQ.0.)) GO TO 422
      IF((XDIST.GT.0.).AND.(YDIST.EQ.0.)) GO TO 423
      XABS=ABS(XDIST)
      ANGLER=ATAN(YDIST/XABS)
      IF(XDIST.GT.0.) GO TO 424
      THETA=PI-ANGLER
      THETAP=PI+ANGLER
      GO TO 440
  420 THETA=0.
      THETAP=0.
      GO TO 440
  421 THETA=PI/2.
      THETAP=3*PI/2.
      GO TO 440
  422 THETA=PI
      THETAP=PI
      GO TO 440
  423 THETA=0.
      THETAP=2*PI
      GO TO 440
  424 THETA=ANGLER
      THETAP=2*PI-ANGLER
C
C     PICK THE PROPER RELATIVE INFLUENCE COEFFICIENT VECTOR.
C

  440 DO 330 I=1,110
      RADIUS=RAD(I)
      ILOC=I
      IF(RADIUS.EQ.RTEMP) GO TO 331
  330 CONTINUE
  331 L=ILOC
  332 DO 350 KZ=2,IZ
      KKZ=KZ-1
      CSZ=COEF(L,KZ,1)
      CST=COEF(L,KZ,2)
      CTR=COEF(L,KZ,3)
      CSR=COEF(L,KZ,4)
      COM=COEF(L,KZ,5)
      IF(K.EQ.1) GO TO 341
      GO TO 342
  341 CSZSYM=0.
      CSTSYM=0.
      CTRSYM=0.
      CSRSYM=0.
      COMSYM=0.
      GO TO 343
  342 CSZSYM=CSZ
      CSTSYM=CST
      CTRSYM=CTR
      CSRSYM=CSR
      COMSYM=COM
  343 IF(YDIST.GT.YMAX) GO TO 344
      GO TO 345
  344 CSZ=0.
      CST=0.
      CTR=0.
      CSR=0.
      COM=0.
C
C     TRANSFORM FROM CYLINDRICAL TO CARTESIAN COORDINATES.
C
  345 SX1=CSR*COS(THETA)*COS(THETA)+CST*SIN(THETA)*SIN(THETA)
      SX1SYM=CSRSYM*COS(THETAP)*COS(THETAP)+
     1       CSTSYM*SIN(THETAP)*SIN(THETAP)
      SY1=CST*COS(THETA)*COS(THETA)+CSR*SIN(THETA)*SIN(THETA)
      SY1SYM=CSTSYM*COS(THETAP)*COS(THETAP)+
     1       CSRSYM*SIN(THETAP)*SIN(THETAP)
      SZ1=CSZ
      SZ1SYM=CSZSYM
      SXY=(CSR-CST)*SIN(THETA)*COS(THETA)
      SXYSYM=(CSRSYM-CSTSYM)*SIN(THETAP)*COS(THETAP)
      SXZ=CTR*COS(THETA)
      SXZSYM=CTRSYM*COS(THETAP)
      SYZ=CTR*SIN(THETA)
      SYZSYM=CTRSYM*SIN(THETAP)
      SU1=COM
      SU1SYM=COMSYM
C
C     SUM UP THE INFLUENCE COEFFICIENTS TIMES THE PROPER TIE/SOIL REACTIONS.
C
  346 J1=ITIE-K
      IF(J1.LT.0)J1=-J1
      J1=J1+1
      J2=K+ITIE-1
      RESPW(ITIE,IP,KKZ)=RESPW(ITIE,IP,KKZ)+
     1(SU1SYM*BP(J1,J)+SU1*BP(J2,J))
      RESPX(ITIE,IP,KKZ)=RESPX(ITIE,IP,KKZ)+
     1(SX1SYM*BP(J1,J)+SX1*BP(J2,J))
      RESPY(ITIE,IP,KKZ)=RESPY(ITIE,IP,KKZ)+
     1(SY1SYM*BP(J1,J)+SY1*BP(J2,J))
      RESPZ(ITIE,IP,KKZ)=RESPZ(ITIE,IP,KKZ)+
     1(SZ1SYM*BP(J1,J)+SZ1*BP(J2,J))
      RESPXY(ITIE,IP,KKZ)=RESPXY(ITIE,IP,KKZ)+
     1(SXYSYM*BP(J1,J)+SXY*BP(J2,J))
      RESPXZ(ITIE,IP,KKZ)=RESPXZ(ITIE,IP,KKZ)+
     1(SXZSYM*BP(J1,J)+SXZ*BP(J2,J))
      RESPYZ(ITIE,IP,KKZ)=RESPYZ(ITIE,IP,KKZ)+
     1(SYZSYM*BP(J1,J)+SYZ*BP(J2,J))
  350 CONTINUE
  450 CONTINUE
  500 CONTINUE
  550 CONTINUE
  600 CONTINUE
C      ********************************************
C      BEGIN SUPERPOSITION OF AXLE LOADS
C      ********************************************
C      CALCULATE THE NUMBER OF SUPERPOSITIONS
      NSUPERS=NAXLES-1
      IF(NSUPERS.EQ.0) GO TO 700
      NDIFF=LP(NAXLES)-LP(NSUPERS)
      IF(NDIFF.GT.6) WRITE(*,701)
  701 FORMAT(//,2X,'THE LAST LOADED TIE IS OUTSIDE THE',
     1      ' ASSUMED INFLUENCE DISTANCE OF SIX TIES AWAY',
     2      ' FROM THE PREVIOUS LOADED TIE',/)
      MAXTIE=LP(NAXLES)+5
      IF(MAXTIE.GT.17) MAXTIE=17
      GO TO 702
  700 MAXTIE=6
  702 CONTINUE
C
C      MODIFY TIE NUMBERING SYSTEM FOR RAIL DEFL.,RAIL SEAT LOAD,
C      AND TIE DEFL.
C
      DO 704 I=1,3
      DO 703 J=1,3
      JNEW=7-J
      T1=YI(I,JNEW)
      T2=YI(I,J)
      YI(I,JNEW)=T2
      YI(I,J)=T1
  703 CONTINUE
  704 CONTINUE
C
C      INITIALIZE YIS(I,TIE),BPS(TIE,SEGMENT),ARRAYS(TIE,SEG.Z)
C
      DO 705 I=1,3
      DO 705 J=1,17
      YIS(I,J)=0.
  705 CONTINUE
      DO 706 I=1,17
      DO 706 J=1,5
      BPS(I,J)=0.
  706 CONTINUE
      DO 707 I=1,17
      DO 707 J=1,5
      DO 707 K=1,NS
      RWS(I,J,K)=0.
      RXS(I,J,K)=0.
      RYS(I,J,K)=0.
      RZS(I,J,K)=0.
      RXYS(I,J,K)=0.
      RXZS(I,J,K)=0.
      RYZS(I,J,K)=0.
  707 CONTINUE
C     DO SUPERPOSITION FOR YIS AND BPS ARRAYS
C      THE FIRST SUPERPOSITION IS FOR THE LOAD AT THE CENTER TIE
      DO 708 I=1,3
      DO 708 J=1,6
      YIS(I,J)=YIS(I,J)+YI(I,J)
  708 CONTINUE
      DO 709 I=1,6
      DO 709 J=1,5
      BPS(I,J)=BP(I,J)
  709 CONTINUE
      DO 710 I=1,6
      DO 710 J=1,5
      DO 710 K=1,NL
      RWS(I,J,K)=RESPW(I,J,K)
      RXS(I,J,K)=RESPX(I,J,K)
      RYS(I,J,K)=RESPY(I,J,K)
      RZS(I,J,K)=RESPZ(I,J,K)
      RXYS(I,J,K)=RESPXY(I,J,K)
      RXZS(I,J,K)=RESPXZ(I,J,K)
      RYZS(I,J,K)=RESPYZ(I,J,K)
  710 CONTINUE
      IF(NSUPERS.EQ.0) GO TO 721
C      SUPERIMPOSE RAIL DEFL.,RAIL SEAT LOADS,TIE DEFL.
      DO 713 NN=1,NSUPERS
      NCALC=NN+1
      LOCAX=LP(NCALC)
      FACTOR=PWHL(NCALC)/PWHL(1)
      JMAX=LOCAX+5
      DO 712 I=1,3
      DO 711 J=1,JMAX
      KADD=(IABS(LOCAX-J))+1
      IF(KADD.GT.6) GO TO 711
      YIS(I,J)=YIS(I,J)+FACTOR*YI(I,KADD)
  711 CONTINUE
  712 CONTINUE
  713 CONTINUE
C      SUPERIMPOSE THE TIE/SOIL REACTIONS
      DO 716 NN=1,NSUPERS
      NCALC=NN+1
      LOCAX=LP(NCALC)
      FACTOR=PWHL(NCALC)/PWHL(1)
      IMAX=LOCAX+5
      DO 715 I=1,IMAX
      KADD=(IABS(LOCAX-I))+1
      IF(KADD.GT.6) GO TO 715
      DO 714 J=1,5
      BPS(I,J)=BPS(I,J)+FACTOR*BP(KADD,J)
  714 CONTINUE
  715 CONTINUE
  716 CONTINUE
      TL=WI+WO
      DIVL=TL/M
      DIST=(WO-DIVL*MO)
      DO 7163 J=1,MAXTIE
      TCM(J)=0.
      DO 7161 I=1,M
      TM(J,I)=BPS(J,I)*(M+.5-I)*DIVL
 7161 TCM(J)=TCM(J)+TM(J,I)
      TCM(J)=TCM(J)-YIS(2,J)*WI
      RSM(J)=0.
      DO 7162 K=1,MO
      SM(J,K)=BPS(J,K)*((MO+.5-K)*DIVL+DIST)
 7162 RSM(J)=RSM(J)+SM(J,K)
 7163 CONTINUE
      DO 7166 I=1,MAXTIE
      LM(I)=0.
      K=0
      DO 7164 J=I,MAXTIE
      K=K+1
      LM(I)=LM(I)+YIS(2,J)*TS*(K-1)
 7164 CONTINUE
      DO 7165 J=1,NAXLES
      IDIST=LP(J)-I
      IF(IDIST.LE.0) GO TO 7165
      LM(I)=LM(I)-PWHL(J)*IDIST*TS
 7165 CONTINUE
 7166 CONTINUE
C
C
C      SUPERIMPOSE THE SOIL DEFLECTIONS AND STRESS TENSORS
      DO 720 NN=1,NSUPERS
      NCALC=NN+1
      LOCAX=LP(NCALC)
      FACTOR=PWHL(NCALC)/PWHL(1)
      IMAX=LOCAX+5
      DO 719 I=1,IMAX
      KADD=(IABS(LOCAX-I))+1
      IF(KADD.GT.6) GO TO 719
      DO 718 J=1,5
      DO 717 K=1,NL
      RWS(I,J,K)=RWS(I,J,K)+(FACTOR*RESPW(KADD,J,K))
      RXS(I,J,K)=RXS(I,J,K)+(FACTOR*RESPX(KADD,J,K))
      RYS(I,J,K)=RYS(I,J,K)+(FACTOR*RESPY(KADD,J,K))
      RZS(I,J,K)=RZS(I,J,K)+(FACTOR*RESPZ(KADD,J,K))
      RXYS(I,J,K)=RXYS(I,J,K)+(FACTOR*RESPXY(KADD,J,K))
      RXZS(I,J,K)=RXZS(I,J,K)+(FACTOR*RESPXZ(KADD,J,K))
C      S-YZ COMPONENT MUST CONSIDER SIGN CHANGES
      ISIGN=LOCAX-I
      IF(ISIGN.GT.0) SIGN=-1.
      IF(ISIGN.LE.0) SIGN=1.
      RYZS(I,J,K)=RYZS(I,J,K)+(SIGN*(FACTOR*RESPYZ(KADD,J,K)))
  717 CONTINUE
  718 CONTINUE
  719 CONTINUE
  720 CONTINUE
  721 CONTINUE
C**************************************************************
C**************************************************************
      IF(CRIBOPT.EQ.0) GO TO 117
C
C     INITIALIZE THE RESPONSE ARRAYS FOR CRIBS
C
      DO 3401 III=1,6
      DO 3401 JJJ=1,5
      DO 3401 KKK=1,5
      CRBPW(III,JJJ,KKK)=0.
      CRBPX(III,JJJ,KKK)=0.
      CRBPY(III,JJJ,KKK)=0.
      CRBPZ(III,JJJ,KKK)=0.
      CRBPXY(III,JJJ,KKK)=0.
      CRBPXZ(III,JJJ,KKK)=0.
      CRBPYZ(III,JJJ,KKK)=0.
 3401 CONTINUE
C
C     DEFINE AN ARRAY WHOSE ELEMENTS EQUAL THE RADIAL DISTANCES TO ALL
C     TIE SEGMENTS FROM SEGMENT NO. 1 OF TIE NO. 1
C
      DO 5451 IX=1,10
      XINCR=(IX-1)*SEGL
      IRADNO=((IX-1)*11)+2
      DO 5441 IC=1,5
      YINCR=(IC-1)*TIESP+(TIESP/2.0)
      RAD(IRADNO)=SQRT(XINCR**2+YINCR**2)
      IRADNO=IRADNO+2
 5441 CONTINUE
 5451 CONTINUE
C
C     BEGIN THE CALCULATION OF THE DISPLACEMENTS AND STRESS TENSORS.
C
      DO 6001 ICRB=2,10,2
      YMAX=(11-ICRB)*TIESP/2.0
      DO 5501 IP=1,5
      DO 5011 J=1,10
C
C     DEFINE LOCAL COORDINATE ORIGINS AND RELATIVE ANGLES.
C
      XDIST=(J-IP)*SEGL
      DO 4501 KTIE=3,11,2
      YDIST=(KTIE-2)*TIESP/2.0
      RTEMP=SQRT(XDIST**2+YDIST**2)
      IF((XDIST.EQ.0.).AND.(YDIST.EQ.0.)) GO TO 4201
      IF(XDIST.EQ.0.) GO TO 4211
      IF((XDIST.LT.0.).AND.(YDIST.EQ.0.)) GO TO 4221
      IF((XDIST.GT.0.).AND.(YDIST.EQ.0.)) GO TO 4231
      XABS=ABS(XDIST)
      ANGLER=ATAN(YDIST/XABS)
      IF(XDIST.GT.0.) GO TO 4241
      THETA=PI-ANGLER
      THETAP=PI+ANGLER
      GO TO 4401
 4201 THETA=0.
      THETAP=0.
      GO TO 4401
 4211 THETA=PI/2.
      THETAP=3*PI/2.
      GO TO 4401
 4221 THETA=PI
      THETAP=PI
      GO TO 4401
 4231 THETA=0.
      THETAP=2*PI
      GO TO 4401
 4241 THETA=ANGLER
      THETAP=2*PI-ANGLER
C
C     PICK THE PROPER RELATIVE INFLUENCE COEFFICIENT VECTOR.
C
 4401 DO 3301 I=1,110
      RADIUS=RAD(I)
      ILOC=I
      IF(RADIUS.EQ.RTEMP) GO TO 3311
 3301 CONTINUE
 3311 L=ILOC
 3321 DO 3501 KZ=2,IZ
      KKZ=KZ-1
      CSZ=COEF(L,KZ,1)
      CST=COEF(L,KZ,2)
      CTR=COEF(L,KZ,3)
      CSR=COEF(L,KZ,4)
      COM=COEF(L,KZ,5)
      CSZSYM=CSZ
      CSTSYM=CST
      CTRSYM=CTR
      CSRSYM=CSR
      COMSYM=COM
 3431 IF(YDIST.GT.YMAX) GO TO 3441
      GO TO 3451
 3441 CSZ=0.
      CST=0.
      CTR=0.
      CSR=0.
      COM=0.
C
C     TRANSFORM FROM CYLINDRICAL TO CARTESIAN COORDINATES.
C
 3451 SX1=CSR*COS(THETA)*COS(THETA)+CST*SIN(THETA)*SIN(THETA)
      SX1SYM=CSRSYM*COS(THETAP)*COS(THETAP)+
     1       CSTSYM*SIN(THETAP)*SIN(THETAP)
      SY1=CST*COS(THETA)*COS(THETA)+CSR*SIN(THETA)*SIN(THETA)
      SY1SYM=CSTSYM*COS(THETAP)*COS(THETAP)+
     1       CSRSYM*SIN(THETAP)*SIN(THETAP)
      SZ1=CSZ
      SZ1SYM=CSZSYM
      SXY=(CSR-CST)*SIN(THETA)*COS(THETA)
      SXYSYM=(CSRSYM-CSTSYM)*SIN(THETAP)*COS(THETAP)
      SXZ=CTR*COS(THETA)
      SXZSYM=CTRSYM*COS(THETAP)
      SYZ=CTR*SIN(THETA)
      SYZSYM=CTRSYM*SIN(THETAP)
      SU1=COM
      SU1SYM=COMSYM
C
C     SUM UP THE INFLUENCE COEFFICIENTS TIMES THE PROPER TIE/SOIL REACTION
C
      J2=(ICRB+KTIE-1)/2
      J1=ABS(ICRB-J2)+1
      CRBPW(ICRB,IP,KKZ)=CRBPW(ICRB,IP,KKZ)+
     1(SU1SYM*BP(J1,J)+SU1*BP(J2,J))
      CRBPX(ICRB,IP,KKZ)=CRBPX(ICRB,IP,KKZ)+
     1(SX1SYM*BP(J1,J)+SX1*BP(J2,J))
      CRBPY(ICRB,IP,KKZ)=CRBPY(ICRB,IP,KKZ)+
     1(SY1SYM*BP(J1,J)+SY1*BP(J2,J))
      CRBPZ(ICRB,IP,KKZ)=CRBPZ(ICRB,IP,KKZ)+
     1(SZ1SYM*BP(J1,J)+SZ1*BP(J2,J))
      CRBPXY(ICRB,IP,KKZ)=CRBPXY(ICRB,IP,KKZ)+
     1(SXYSYM*BP(J1,J)+SXY*BP(J2,J))
      CRBPXZ(ICRB,IP,KKZ)=CRBPXZ(ICRB,IP,KKZ)+
     1(SXZSYM*BP(J1,J)+SXZ*BP(J2,J))
      CRBPYZ(ICRB,IP,KKZ)=CRBPYZ(ICRB,IP,KKZ)+
     1(SYZSYM*BP(J1,J)+SYZ*BP(J2,J))
      IF(KKZ.NE.1) GO TO 3501
 3501 CONTINUE
 4501 CONTINUE
 5011 CONTINUE
 5501 CONTINUE
 6001 CONTINUE
C
C   RENUMBER THE CRIB COEFFICIENTS
C
      DO 113 ICRB=1,5
      L=2*ICRB
      DO 113 J=1,5
      DO 113 K=1,NL
      CRBPW(ICRB,J,K)=CRBPW(L,J,K)
      CRBPX(ICRB,J,K)=CRBPX(L,J,K)
      CRBPY(ICRB,J,K)=CRBPY(L,J,K)
      CRBPZ(ICRB,J,K)=CRBPZ(L,J,K)
      CRBPXY(ICRB,J,K)=CRBPXY(L,J,K)
      CRBPXZ(ICRB,J,K)=CRBPXZ(L,J,K)
      CRBPYZ(ICRB,J,K)=CRBPYZ(L,J,K)
  113 CONTINUE
C**************************************************************
C     BEGIN TO SUPERIMPOSE THE CRIB COEFFICIENTS
C**************************************************************
      NSUPERS=NAXLES-1
      IF(NSUPERS.EQ.0) GO TO 7001
      NDIFF=LP(NAXLES)-LP(NSUPERS)
      IF(NDIFF.GT.6) WRITE(*,7011)
 7011 FORMAT(//,2X,'THE LAST LOADED TIE IS OUTSIDE THE',
     1      ' ASSUMED INFLUENCE DISTANCE OF SIX TIES AWAY',
     2      ' FROM THE PREVIOUS LOADED TIE',/)
      MAXTIE=LP(NAXLES)+5
      IF(MAXTIE.GT.17) MAXTIE=17
      GO TO 7021
 7001 MAXTIE=6
 7021 CONTINUE
      DO 7071 I=1,17
      DO 7071 J=1,5
      DO 7071 K=1,NS
      CRWS(I,J,K)=0.
      CRXS(I,J,K)=0.
      CRYS(I,J,K)=0.
      CRZS(I,J,K)=0.
      CRXYS(I,J,K)=0.
      CRXZS(I,J,K)=0.
      CRYZS(I,J,K)=0.
 7071 CONTINUE
      DO 7101 I=1,6
      DO 7101 J=1,5
      DO 7101 K=1,NL
      CRWS(I,J,K)=CRBPW(I,J,K)
      CRXS(I,J,K)=CRBPX(I,J,K)
      CRYS(I,J,K)=CRBPY(I,J,K)
      CRZS(I,J,K)=CRBPZ(I,J,K)
      CRXYS(I,J,K)=CRBPXY(I,J,K)
      CRXZS(I,J,K)=CRBPXZ(I,J,K)
      CRYZS(I,J,K)=CRBPYZ(I,J,K)
 7101 CONTINUE
C********************************************************************
      IF(NSUPERS.EQ.0) GO TO 7211
C********************************************************************
C      SUPERIMPOSE THE SOIL DEFLECTIONS AND STRESS TENSORS FOR CRIBS
C********************************************************************
      DO 7201 NN=1,NSUPERS
      NCALC=NN+1
      LOCAX=LP(NCALC)
      FACTOR=PWHL(NCALC)/PWHL(1)
      IMAX=LOCAX+4
      DO 7191 I=1,IMAX
      KADD=LOCAX-I
      IF(KADD.LE.0) KADD=-KADD+1
      IF(KADD.GT.5) GO TO 7191
      DO 7181 J=1,5
      DO 7171 K=1,NL
      CRWS(I,J,K)=CRWS(I,J,K)+(FACTOR*CRBPW(KADD,J,K))
      CRXS(I,J,K)=CRXS(I,J,K)+(FACTOR*CRBPX(KADD,J,K))
      CRYS(I,J,K)=CRYS(I,J,K)+(FACTOR*CRBPY(KADD,J,K))
      CRZS(I,J,K)=CRZS(I,J,K)+(FACTOR*CRBPZ(KADD,J,K))
      CRXYS(I,J,K)=CRXYS(I,J,K)+(FACTOR*CRBPXY(KADD,J,K))
      CRXZS(I,J,K)=CRXZS(I,J,K)+(FACTOR*CRBPXZ(KADD,J,K))
C      S-YZ COMPONENT MUST CONSIDER SIGN CHANGES
      ISIGN=LOCAX-I
      IF(ISIGN.GT.0) SIGN=-1.
      IF(ISIGN.LE.0) SIGN=1.
      CRYZS(I,J,K)=CRYZS(I,J,K)+(SIGN*(FACTOR*CRBPYZ(KADD,J,K)))
      IF(K.GT.1) GO TO 7171
 7171 CONTINUE
 7181 CONTINUE
 7191 CONTINUE
 7201 CONTINUE
 7211 CONTINUE
C****************************************************************
C****************************************************************
C
C      AT THIS POINT ALL SUPERPOSITIONS ARE COMPLETE
C
  117 LLAST=LP(NAXLES)
      DO 7219 I=1,LLAST
      DO 7219 J=1,5
      BULKSTR(I,J)=0.
 7219 CONTINUE
      IF(NSTEPS.GE.NITER) GO TO 7345
      RSL=0.
      DO 722 I=1,LLAST
      RSL=RSL+YIS(2,I)
  722 CONTINUE
      DO 725 I=1,NL
      DO 724 J=1,LLAST
      TH=0.
      DO 723 K=1,5
      TH=TH+RXS(J,K,I)+RYS(J,K,I)+RZS(J,K,I)
  723 CONTINUE
      TH=TH/5.
      BULKSTR(J,I)=BULKSTR(J,I)+TH*YIS(2,J)
  724 CONTINUE
  725 CONTINUE
      DO 727 I=1,NL
      THETAF(I)=0.
      DO 726 J=1,LLAST
      THETAF(I)=THETAF(I)+BULKSTR(J,I)
  726 CONTINUE
      THETAF(I)=THETAF(I)/RSL
  727 CONTINUE
C      ADD THE INITIAL BULK STRESSES TO THE INCREMENTAL
C      BULK STRESSES. NOTE SIGN CHANGES ON INCREMENTAL
C      BULK STRESSES.
      DO 728 I=1,NL
      THETAF(I)=-1.*THETAF(I)+THETAI(I)
      EOLD(I)=E(I)
  728 CONTINUE
      NLAY=0
      PATM=14.7
      DO 731 I=1,NL
      NLAY=NLAY+1
      IF(THETAF(NLAY).GT.0.) GO TO 729
      E(NLAY)=1000.
      GO TO 730
  729 IF(KTYPE(NLAY).EQ.0) GO TO 7291
      IF(KTYPE(NLAY).EQ.1) GO TO 7292
      IF(KTYPE(NLAY).EQ.2) GO TO 7293
      IF(KTYPE(NLAY).EQ.3) GO TO 7294
      IF(KTYPE(NLAY).EQ.4) CALL NEWMOD(NLAY)
      IF(KTYPE(NLAY).EQ.5) CALL NEWMOD(NLAY)
      IF(KTYPE(NLAY).EQ.6) CALL NEWMOD(NLAY)
      IF(KTYPE(NLAY).EQ.7) CALL NEWMOD(NLAY)
      IF(KTYPE(NLAY).EQ.8) CALL NEWMOD(NLAY)
      GO TO 730
 7291 E(NLAY)=E(I)
      GO TO 730
 7292 E(NLAY)=KONE(I)*PATM*(THETAF(NLAY)/PATM)**KTWO(I)
      GO TO 730
 7293 E(NLAY)=PATM*(KONE(I)+KTWO(I)*(ALOG10(THETAF(NLAY)/PATM)))
      GO TO 730
 7294 E(NLAY)=PATM*(KONE(I)+KTWO(I)*(THETAF(NLAY)/PATM))
  730 CONTINUE
  731 CONTINUE
      IF(NSTEPS.EQ.0) WRITE(*,7321)
 7321 FORMAT(//,40X,'MODULI USED FOR THE ITERATIONS',/)
      WRITE(*,732)
  732 FORMAT(//,51X,'INITIAL',5X,'NEW',/,
     140X,'LAYER',6X,'MODULUS',3X,'MODULUS',/,
     252X,'(PSI)',7X,'(PSI)',/)
      DO 734 I=1,NL
      WRITE(*,733) I,EOLD(I),E(I)
  733 FORMAT(42X,I1,7X,F8.1,3X,F8.1)
  734 CONTINUE
 7345 CONTINUE
      NSTEPS=NSTEPS+1
      IF(TID.EQ.TIE) M=2*M
      IF(TID.EQ.TIE) MO=2*MO
      IF(TID.EQ.TIE) MI=2*MI
      IF(TIE.EQ.TIE) WO=2*WO
      IF(TID.EQ.TIE) WI=2*WI
      IF(RID.EQ.RAIL) N=2*N-1
      IF(NEWFLAG.EQ.1) GO TO 7451
      IF(ITOPT.EQ.1) GO TO 735
      IF(NSTEPS.GT.NITER) GO TO 735
      GO TO 9999
  735 IF(NSUPERS.EQ.0) GO TO 7374
      WRITE(*,736)
  736 FORMAT(1H1,//,20X,'SUPERIMPOSED RESULTS FROM FINAL',
     1' ITERATION',//,20X,'----- DEFLECTIONS AND REACTIONS ----',//)
      WRITE(*,270)
      DO 737 I=MINTOUT,MAXTOUT
      WRITE(*,280) I,YIS(1,I),YIS(2,I),YIS(3,I),
     1            (BPS(I,J),J=1,5)
  737 CONTINUE
C
      DO 7301 I=1,MAXTIE
      DO 7301 J=1,5
      TDFL(I,J)=0.
      DO 7301 K=1,5
      TDFL(I,J)=TDFL(I,J)+BPS(I,K)*T(K,J)
 7301 CONTINUE
      DO 7302 I=1,MAXTIE
      DO 7302 J=1,5
      TDFL(I,J)=TDFL(I,J)+YIS(1,I)+(YIS(2,I)/STIF1)
 7302 CONTINUE
      WRITE(*,7371)
 7371 FORMAT(1H1,//,10X,'TIE CENTER',4X,'TIE RAIL-SEAT',6X,'RAIL',/,
     12X,'TIE',7X,'BENDING',7X,'BENDING',9X,'BENDING',24X,
     2'TIE SEGMENT VERTICAL DEFLECTIONS (IN.)',/,2X,'NO.',7X,
     3'MOMENT',8X,'MOMENT',10X,'MOMENT',10X,'DIV 1',10X,'DIV 2',
     410X,'DIV 3',10X,'DIV 4',10X,'DIV 5',/,11X,'(IN.-LBS.)',
     54X,'(IN.-LBS.)',5X,'(IN.-LBS.)',/)
      DO 7372 I=MINTOUT,MAXTOUT
 7372 WRITE(*,7373) I,TCM(I),RSM(I),LM(I),(TDFL(I,J),J=1,5)
 7373 FORMAT(2X,I3,2X,8(2X,E13.5))
 7374 IF(ITSTD.EQ.0) GO TO 738
      WRITE(*,800)
  800 FORMAT(1H1,//,43X,'SOIL VERTICAL DISPLACEMENTS AND ',
     1      'INCREMENTAL STRESSES',//,
     -43X,'T=TIE NUMBER (1=CENTER TIE)',/,
     -43X,'SEG=SEGMENT NUMBER',/,
     -43X,'Z=DEPTH POINT NUMBER',//,
     -43X,'UNITS ARE INCHES AND PSI.',//,
     -43X,'COMPRESSION IS NEGATIVE',//)
      DO 801 I=1,NL
      INL=I+1
C*******************************************************************
C PARENTHESIS
      WRITE(*,4412) I,ZZ(INL)
  801 CONTINUE
      JJ=IZ-1
      IF(JJ.LT.1) GO TO 602
      DO 807 ITIE=MINTOUT,MAXTOUT
      WRITE(*,802)
  802 FORMAT(1H1,10X,'T',3X,'SEG',2X,'Z',7X,'W',8X,'THETA',
     1      5X,'S-XX',6X,'S-YY',6X,'S-ZZ',6X,'S-XY',6X,'S-XZ',
     2      6X,'S-YZ')
      DO 806 NNSEG=MINSEG,MAXSEG
      DO 805 NDEPTH=1,JJ
      BULK=RXS(ITIE,NNSEG,NDEPTH)+RYS(ITIE,NNSEG,NDEPTH)+
     1     RZS(ITIE,NNSEG,NDEPTH)
      IF(NDEPTH.EQ.1) WRITE(*,803)
  803 FORMAT(/,10X,'----------------------------------------',
     1'-----------------------------------------------------',/)
      WRITE(*,804) ITIE,NNSEG,NDEPTH,RWS(ITIE,NNSEG,NDEPTH),
     4            BULK,
     5            RXS(ITIE,NNSEG,NDEPTH),
     6            RYS(ITIE,NNSEG,NDEPTH),
     7            RZS(ITIE,NNSEG,NDEPTH),
     8            RXYS(ITIE,NNSEG,NDEPTH),
     9            RXZS(ITIE,NNSEG,NDEPTH),
     9            RYZS(ITIE,NNSEG,NDEPTH)
  804 FORMAT(10X,I2,3X,I1,4X,I1,4X,F7.5,7(4X,F6.1))
  805 CONTINUE
  806 CONTINUE
  807 CONTINUE
  738 IF(ITPRIN.EQ.0) GO TO 602
 4410 WRITE(*,441)
      WRITE(*,4411)
      DO 4413 I=1,NL
      INL=I+1
C*******************************************************************
C PARENTHESIS
      WRITE(*,4412) I,ZZ(INL)
 4412 FORMAT(/,43X,'Z(',I1,') = ',F6.2,' IN.')
 4413 CONTINUE
      JJ=IZ-1
      IF(JJ.LT.1) GO TO 602
      DO 445 ITIE=MINTOUT,MAXTOUT
      WRITE(*,442)
      DO 444 NNSEG=MINSEG,MAXSEG
      DO 443 NDEPTH=1,JJ
      N1=3
      MROW=3
      N=3
      MV=0
      RO(1)=RXS(ITIE,NNSEG,NDEPTH)
      RO(2)=RXYS(ITIE,NNSEG,NDEPTH)
      RO(3)=RYS(ITIE,NNSEG,NDEPTH)
      RO(4)=RXZS(ITIE,NNSEG,NDEPTH)
      RO(5)=RYZS(ITIE,NNSEG,NDEPTH)
      RO(6)=RZS(ITIE,NNSEG,NDEPTH)
      write(*,*) RO
      CALL EIGEN(RO,DS,N,MV)
      SIG1=RO(6)
      SIG2=RO(3)
      SIG3=RO(1)
      BULK=SIG1+SIG2+SIG3
      DEV=SIG1-SIG3
      IF(NDEPTH.EQ.1) WRITE(*,447)
  447 FORMAT(/,43X,'------------------------------',
     1'--------------------',/)
      WRITE(*,446) ITIE,NNSEG,NDEPTH,RWS(ITIE,NNSEG,NDEPTH),
     1            SIG1,(DS(I),I=7,9),
     2            SIG2,(DS(I),I=4,6),
     3            SIG3,(DS(I),I=1,3),
     4            BULK,DEV
  443 CONTINUE
  444 CONTINUE
  445 CONTINUE
  441 FORMAT(1H1,42X,'-------------------------',/,46X,'  ELEMENT UNKNOW
     -NS ',/,43X,'-------------------------',///,43X,'T=TIE NUMBER (1=CE
     -NTER TIE)',/,43X,'SEG=SEGMENT NUMBER',/,43X,'Z=DEPTH POINT NUMBER'
     -,/,43X,'W=VERTICAL DISPLACEMENT-IN.',/,43X,'SIG1=MAXIMUM PRINCIPAL
     -STRESS-PSI.',/,43X,'SIG2=INTERMEDIATE PRINCIPAL STRESS-PSI.',/,43X
     -,'SIG3=MINIMUM PRINCIPAL STRESS-PSI.',/,43X,'A1  =DIRECTION COSINE
     -S FOR SIG1.',/,43X,'A2  =DIRECTION COSINES FOR SIG2.',/,43X,'A3  =
     -DIRECTION COSINES FOR SIG3.',/,43X,'THETA=BULK STRESS=SIG1+SIG2+SI
     -G3',/,43X,'DEV=DEVIATORIC STRESS=SIG1-SIG3',//)
 4411 FORMAT(43X,'COMPRESSION IS NEGATIVE',//)
  442 FORMAT (1H1,6X,'T',1X,'SEG',1X,'Z',5X,'W',8X,
     1      'SIG1',4X,'A1X',4X,'A1Y',4X,'A1Z',4X,
     2      'SIG2',4X,'A2X',4X,'A2Y',4X,'A2Z'4X,
     3      'SIG3',4X,'A3X',4X,'A3Y',4X,'A3Z',
     4      5X,'BULK',4X,'DEV',//)
  446 FORMAT(6X,I2,1X,I2,2X,I1,2X,F7.5,1X,3(3X,F5.1,3(2X,F5.2)),4X,
     1       F5.1,2X,F5.1)
  602 CONTINUE
      IF(ITRIAX.EQ.0) GO TO 116
      CALL TRIAXL(RXS,RYS,RZS,RXYS,RXZS,RYZS,MAXTOUT,STA,0)
C***************************************************************
C     OPTIONS FOR CRIBS
C***************************************************************
  116 IF(CRIBOPT.EQ.0) GO TO 9999
      IF(ICSTD.EQ.0) GO TO 118
      WRITE(*,7401)
 7401 FORMAT(1H1,//,43X,'SOIL VERTICAL DISPLACEMENTS AND ',
     1      'INCREMENTAL STRESSES',//,
     -43X,'C=CRIB NUMBER (1=ON THE RIGHT OF CENTER TIE)',/,
     -43X,'SEG=SEGMENT NUMBER',/,
     -43X,'Z=DEPTH POINT NUMBER',//,
     -43X,'UNITS ARE INCHES AND PSI.',//,
     -43X,'COMPRESSION IS NEGATIVE',//)
      DO 740 I=1,NL
      INL=I+1
C*******************************************************************
C PARENTHESIS
      WRITE(*,4412) I,ZZ(INL)
  740 CONTINUE
      JJ=IZ-1
      IF(JJ.LT.1) GO TO 746
      DO 741 ICRB=MINTOUT,MAXCOUT
      WRITE(*,742)
  742 FORMAT(1H1,10X,'C',3X,'SEG',2X,'Z',7X,'W',8X,'THETA',
     1      5X,'S-XX',6X,'S-YY',6X,'S-ZZ',6X,'S-XY',6X,'S-XZ',
     2      6X,'S-YZ')
      DO 741 NNSEG=MINSEG,MAXSEG
      DO 741 NDEPTH=1,JJ
      BULK=CRXS(ICRB,NNSEG,NDEPTH)+CRYS(ICRB,NNSEG,NDEPTH)+
     1     CRZS(ICRB,NNSEG,NDEPTH)
      IF(NDEPTH.EQ.1) WRITE(*,744)
  744 FORMAT(/,10X,'----------------------------------------',
     1'-----------------------------------------------------',/)
      WRITE(*,745) ICRB,NNSEG,NDEPTH,CRWS(ICRB,NNSEG,NDEPTH),
     4            BULK,
     5            CRXS(ICRB,NNSEG,NDEPTH),
     6            CRYS(ICRB,NNSEG,NDEPTH),
     7            CRZS(ICRB,NNSEG,NDEPTH),
     8            CRXYS(ICRB,NNSEG,NDEPTH),
     9            CRXZS(ICRB,NNSEG,NDEPTH),
     9            CRYZS(ICRB,NNSEG,NDEPTH)
  745 FORMAT(10X,I2,3X,I1,4X,I1,4X,F7.5,7(4X,F6.1))
  741 CONTINUE
  118 IF(ICPRIN.EQ.0) GO TO 746
      WRITE(*,747)
      WRITE(*,748)
      DO 749 I=1,NL
      INL=I+1
C*******************************************************************
C PARENTHESIS
      WRITE(*,750) I,ZZ(INL)
  750 FORMAT(/,43X,'Z(',I1,') = ',F6.2,' IN.')
  749 CONTINUE
      JJ=IZ-1
      IF(JJ.LT.1) GO TO 746
      DO 751 ICRB=MINTOUT,MAXCOUT
      WRITE(*,752)
      DO 753 NNSEG=MINSEG,MAXSEG
      DO 754 NDEPTH=1,JJ
      N1=3
      MROW=3
      N=3
      MV=0
      RO(1)=CRXS(ICRB,NNSEG,NDEPTH)
      RO(2)=CRXYS(ICRB,NNSEG,NDEPTH)
      RO(3)=CRYS(ICRB,NNSEG,NDEPTH)
      RO(4)=CRXZS(ICRB,NNSEG,NDEPTH)
      RO(5)=CRYZS(ICRB,NNSEG,NDEPTH)
      RO(6)=CRZS(ICRB,NNSEG,NDEPTH)
      CALL EIGEN(RO,DS,N,MV)
      SIG1=RO(6)
      SIG2=RO(3)
      SIG3=RO(1)
      BULK=SIG1+SIG2+SIG3
      DEV=SIG1-SIG3
      IF(NDEPTH.EQ.1) WRITE(*,755)
  755 FORMAT(/,43X,'------------------------------',
     1'--------------------',/)
      WRITE(*,756) ICRB,NNSEG,NDEPTH,CRWS(ICRB,NNSEG,NDEPTH),
     1            SIG1,(DS(I),I=7,9),
     2            SIG2,(DS(I),I=4,6),
     3            SIG3,(DS(I),I=1,3),
     4            BULK,DEV
  754 CONTINUE
  753 CONTINUE
  751 CONTINUE
  747 FORMAT(1H1,42X,'-------------------------',/,46X,'  ELEMENT UNKNOW
     -NS ',/,43X,'-------------------------',///,43X,'C=CRIB NUMBER (1=
     -ON THE RIGHT OF CENTER TIE)',/,43X,'SEG=SEGMENT NUMBER',
     -/,43X,'Z=DEPTH POINT NUMBER',
     -/,43X,'W=VERTICAL DISPLACEMENT-IN.',/,43X,'SIG1=MAIIMUM PRINCIPAL
     -STRESS-PSI.',/,43X,'SIG2=INTERMEDIATE PRINCIPAL STRESS-PSI.',/,43X
     -,'SIG3=MINIMUM PRINCIPAL STRESS-PSI.',/,43X,'A1  =DIRECTION COSINE
     -S FOR SIG1.',/,43X,'A2  =DIRECTION COSINES FOR SIG2.',/,43X,'A3  =
     -DIRECTION COSINES FOR SIG3.',/,43X,'THETA=BULK STRESS=SIG1+SIG2+SI
     -G3',/,43X,'DEV=DEVIATORIC STRESS=SIG1-SIG3',//)
  748 FORMAT(43X,'COMPRESSION IS NEGATIVE',//)
  752 FORMAT (1H1,6X,'C',1X,'SEG',1X,'Z',5X,'W',8X,
     1      'SIG1',4X,'A1X',4X,'A1Y',4X,'A1Z',4X,
     2      'SIG2',4X,'A2X',4X,'A2Y',4X,'A2Z'4X,
     3      'SIG3',4X,'A3X',4X,'A3Y',4X,'A3Z',
     4      5X,'BULK',4X,'DEV',//)
  756 FORMAT(6X,I2,1X,I2,2X,I1,2X,F7.5,1X,3(3X,F5.1,3(2X,F5.2)),4X,
     1       F5.1,2X,F5.1)
  746 CONTINUE
      IF(ICTRIX.EQ.0) GO TO 9999
      DO 13 I=MINSEG,MAXSEG
   13 C(I)=0.35
      CALL TRIAXL(CRXS,CRYS,CRZS,CRXYS,CRXZS,CRYZS,MAXCOUT,C,1)
 7451 IF(NEWFLAG.EQ.0) GO TO 9999
      OFFT=OFFSET/12.
      WRITE(*,7452) OFFSET,OFFT
 7452 FORMAT(///,10X,'LATERAL OFFSET FROM TRACK CENTERLINE = ',
     1      F5.1,2X,'INCHES = ',2X,F5.2,2X,'FEET',//)
      WRITE(*,7453)
 7453 FORMAT(15X,'DEPTH',8X,'W',18X,'S-XX',6X,'S-YY',6X,'S-ZZ',
     1      6X,'S-XY',6X,'S-XZ',6X,'S-YZ',/
     2      16X,'IN.',8X,'IN.',17X,'PSI',7X,'PSI',7X,'PSI',
     3      7X,'PSI',7X,'PSI',7X,'PSI',/)
      DO 7454 I=1,NL
      INL=I+1
C*******************************************************************
C PARENTHESIS
      WRITE(*,7455)ZZ(INL),OFFW(I),OFFX(I),OFFY(I),OFFZ(I),
     1                      OFFXY(I),OFFXZ(I),OFFYZ(I)
 7455 FORMAT(15X,F5.1,5X,F7.5,15X,F5.1,5X,F5.1,5X,F5.1,5X,F5.1,
     1       5X,F5.1,5X,F5.1)
 7454 CONTINUE
 9999 RETURN
      END



C***********************************************************************
      SUBROUTINE STIE(M,WL,TIEL,NSEG,TI,TE,T,NDIM)
C***********************************************************************
C     THIS SUBROUTINE IS TO FIND THE VERTICAL OR LONGITUDINAL
C     FLEXIBILITY COEFFICIENT OF TIE IN THE SYMMETRICAL CASE.
C     ------------------------------------------------------------------
      DIMENSION T(NDIM,NDIM)
      DR=WL
      SEGL=TIEL/NSEG
      HAFSEG=SEGL/2.
      DOUT=(TIEL-DR)/2.
      DO 200 I=1,M
      DO 100 J=1,M
      IF(I.GT.J) GO TO 100
      DK=DOUT-HAFSEG-(I-1)*SEGL
      DJ=DOUT-HAFSEG-(J-1)*SEGL
      IF(DK.LT.0.) GO TO 30
      IF(DJ.LT.0.) GO TO 20
      IF(DJ.GT.DK) GO TO 10
      T(I,J)=DJ*(3.*DR*DK+DJ*(3.*DK-DJ))/(6.*TE*TI)
      GO TO 100
   10 T(I,J)=((DK*DR*DJ)/2.+(DK*DK)*(DJ-DK/3.)/2.)/(TE*TI)
      GO TO 100
   20 DJ=ABS(DJ)
      T(I,J)=DK*DR*(DR-((DR-DJ)**3/(DR*DR))-DJ**3/(DR*DR))/(-6.*TE*TI)
      GO TO 100
   30 DK=ABS(DK)
      B=DR-DK
      IF(DJ.LT.0.) GO TO 40
      T(I,J)=DK*(DR-DK)*DJ/(-2.*TE*TI)
      GO TO 100
   40 DJ=ABS(DJ)
      X2=DR-DJ
      D1=B*DJ*(DR*DR-B*B-DJ*DJ)/(6.*DR*TE*TI)
      IF(DJ.LE.DK) GO TO 50
      D1=D1+(DJ-DK)**3/(6.*TE*TI)
   50 D2=B*X2*(DR*DR-B*B-X2*X2)/(6.*DR*TE*TI)
      IF(X2.LE.DK) GO TO 60
      D2=D2+(X2-DK)**3/(6.*TE*TI)
   60 T(I,J)=D1+D2
  100 CONTINUE
  200 CONTINUE
      DO 300 I=1,M
      DO 300 J=1,M
      T(J,I)=T(I,J)
  300 CONTINUE
      RETURN
      END
C***********************************************************************
      SUBROUTINE UNTIE(M,MO,MI,CO,CI,WL,TI,TE,T,TT,NDIM)
C***********************************************************************
C     THIS SUBROUTINE IS TO FIND THE VERTICAL OR LONGITUDINAL
C     FLEXIBILITY COEFFICIENT OF TIE IN THE UNSYMMETRICAL CASE.
C     ------------------------------------------------------------------
      DIMENSION T(NDIM,NDIM),TT(NDIM)
      NO=MO/2
      NI=MI/2
      NH=NO+MI
      TEI6=6.*TE*TI
      DO 140 I=1,M
      DO 140 J=1,M
      IF(I.GT.NO) GO TO 70
      XP=(NO-I+0.5)*CO
      IF(J.GT.NO) GO TO 30
      X=(NO-J+0.5)*CO
   10 IF(X.LT.XP) GO TO 20
      TEMX=X
      X=XP
      XP=TEMX
   20 T(I,J)=+X*(2.*WL*XP+X*(3.*XP-X))/TEI6
      GO TO 140
   30 IF(J.GT.NH) GO TO 50
      X=(NH-J+0.5)*CI
   40 T(I,J)=-XP*X*(WL-X*X/WL)/TEI6
      GO TO 140
   50 X=(J-NH-0.5)*CO
   60 T(I,J)=+X*XP*WL/TEI6
      GO TO 140
   70 IF(I.GT.NH) GO TO 110
      XP=(I-NO-0.5)*CI
      IF(J.GT.NO) GO TO 80
      X=(NO-J+0.5)*CO
      T(I,J)=-X*XP*(WL-XP)*(WL+WL-XP)/WL/TEI6
      GO TO 140
   80 IF(J.GT.NH) GO TO 100
      X=(J-NO-0.5)*CI
      IF(X.GT.XP) GO TO 90
      T(I,J)=(X*XP*(WL-XP)*(WL+WL-XP)-X**3*(WL-XP))/WL/TEI6
      GO TO 140
   90 X=WL-X
      T(I,J)=(X*XP*(WL+XP)*(WL-XP)-X**3*XP)/WL/TEI6
      GO TO 140
  100 X=(J-NH-0.5)*CO
      T(I,J)=-X*XP*(WL+XP)*(WL-XP)/WL/TEI6
      GO TO 140
  110 XP=(I-NH-0.5)*CO
      IF(J.GT.NO) GO TO 120
      X=(NO-J+0.5)*CO
      GO TO 60
  120 IF(J.GT.NH) GO TO 130
      X=(J-NO-0.5)*CI
      GO TO 40
  130 X=(J-NH-0.5)*CO
      GO TO 10
  140 CONTINUE
      DO 180 K=1,M
      IF(K.GT.NO) GO TO 150
      XX=(K-0.5)*CO
      GO TO 170
  150 IF(K.GT.NH) GO TO 160
      XX=(K-NO-0.5)*CI+CO*NO
      GO TO 170
  160 XX=(K-NH-0.5)*CO+CO*NO+WL
  170 TT(K)=(CO*NO+WL-XX)/WL
  180 CONTINUE
      RETURN
      END
C***********************************************************************
      SUBROUTINE SRAIL(NN,TS,TOL,RE,RI,RA,R,NDIM)
C***********************************************************************
C     THIS SUBROUTINE IS TO FIND THE VERTICAL OR LATERAL FLEXIBILITY
C     COEFFICIENT OF RAIL IN THE SYMMETRICAL CASE.
C     ------------------------------------------------------------------
      DIMENSION R(NDIM,NDIM)
      TOL=NN*2.*TS
      REI6=6.*RE*RI
      DO 100 I=1,NN
      XP=I*TS
      XPL=TOL-XP
      DO 100 J=1,NN
      XD=J*TS
      XDL=TOL-XD
      D1=XPL*XD*(TOL*TOL-XD*XD-XPL*XPL)/TOL/REI6
      IF (XD.LE.XP) GO TO 10
      D1=D1+(XD-XP)**3/REI6
   10 D2=XPL*XDL*(TOL*TOL-XDL*XDL-XPL*XPL)/TOL/REI6
      IF (XDL.LE.XP) GO TO 20
      D2=D2+(XDL-XP)**3/REI6
   20 R(I,J)=-D1-D2
      IF (J.EQ.NN) R(I,J)=R(I,J)/2
  100 CONTINUE
      RETURN
      END
C***********************************************************************
      SUBROUTINE USRAIL(NN,TS,TOL,RE,RI,RA,R,QXD,RM,PXD,RP,NDIM)
C***********************************************************************
C     THIS SUBROUTINE IS TO FIND THE VERTICAL OR LATERAL FLEXIBILITY
C     COEFFICIENT OF RAIL IN THE UNSYMMETRICAL CASE.
C     ------------------------------------------------------------------
      DIMENSION R(NDIM,NDIM),RM(NDIM),RP(NDIM)
      TOL=NN*TS
      REI6=6.*RE*RI
      PXL=TOL-PXD
      QXL=TOL-QXD
      NN=NN-1
      DO 100 I=1,NN
      XP=I*TS
      XPL=TOL-XP
      DO 20 J=1,NN
      XD=J*TS
      XDL=TOL-XD
      D1=XPL*XD*(TOL*TOL-XD*XD-XPL*XPL)/TOL/REI6
      IF (XD.LE.XP) GO TO 10
      D1=D1+(XD-XP)**3/REI6
   10 R(I,J)=-D1
   20 CONTINUE
      D2=PXL*XP*(TOL*TOL-XP*XP-PXL*PXL)/TOL/REI6
      IF (XP.LE.PXD) GO TO 30
      D2=D2+(XP-PXD)**3/REI6
   30 RP(I)=-D2
      IF (XP.LE.QXD) GO TO 40
      TEMP=QXD
      QXD=QXL
      QXL=TEMP
      XP=-(TOL-XP)
   40 RM(I)=XP*((3*QXD*QXD*QXL+QXD**3-2*QXL**3)/TOL-XP*XP)/TOL/REI6
  100 CONTINUE
      RETURN
      END



C***********************************************************************
      SUBROUTINE SOIL(M,N,MN,S,TS,RID,ND,NE,NZ,MD,IZ)
C***********************************************************************
C     THIS SUBROUTINE IS DESIGNED TO SET UP THE SOIL FLEXIBILITY
C     COEFFICIENT MATRIX FROM PSA AVERAGE RESULTS.
      DIMENSION S(MD,MD),TEM(50,50)
C     ------------------------------------------------------------------
      COMMON/GEO/ AY(11,10,6)
C      DATA RAIL/'RAIL'/
C!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      DATA RAIL/0/
      IF (RID.NE.RAIL) N1=N
      IF (RID.EQ.RAIL) N1=2*N-1
      KK=MN-M
      DO 10 I=1,MD
      DO 10 J=1,MD
   10 TEM(I,J)=0.
      CALL AVRG(AY,IZ)
      DO 20 KZ=1,N
      II=(KZ-1)*M
      DO 20 KP=1,M
      DO 20 KN=1,M
      KI=II+KN
   20 S(KI,KP)=AY(KZ,KN,KP)
C     WRITE(*,81)((S(I,J),J=1,30),I=1,30)
C 81  FORMAT((10E10.3))
      NA=N-1
      IF(NA.EQ.0) RETURN
      IF (RID.NE.RAIL) GO TO 40
      CALL SRMTRX(AY,TEM,N,M)
   40 CONTINUE
      DO 50 L=1,NA
      LX=L-1
      NNN=NA-LX
      DO 50 K=1,NNN
      DO 50 I=1,M
      DO 50 J=1,M
   50 S(LX*M+K*M+I,K*M+J)=S(LX*M+I,J)
      DO 60 I=1,MN
      DO 60 J=1,MN
   60 S(I,J)=S(J,I)
      DO 70 I=1,MN
      DO 70 J=1,KK
   70 S(I,J)=S(I,J)+TEM(I,J)
C     WRITE(*,71)((S(I,J),J=1,30),I=1,30)
C 71  FORMAT((10E10.3))
      RETURN
      END

C******************************************************************
      SUBROUTINE SRMTRX(AY,TEM,N,M)
C******************************************************************
      DIMENSION TEM(50,50), AY(11,10,10)
      NA=N-1
      DO 10 KY=1,NA
      KYY=N-KY+1
      JJ=(KY-1)*5+1
      KK=JJ+M-1
      DO 10 KZ=KYY,N
      KZZ=(N-KZ)+KYY
      II=(KZ-1)*M
      DO 20 KP=JJ,KK
      KPP=KP-JJ+1
      DO 20 KN=1,M
      KI=II+KN
   20 TEM(KI,KP)=AY(KZZ,KN,KPP)
   10 CONTINUE
C     WRITE(*,31)((TEM(I,J),J=1,30),I=1,30)
C 31  FORMAT((10E11.3))
      RETURN
      END


C***********************************************************************
      SUBROUTINE SLVEQ(A,B,N,KDIM)
C***********************************************************************
C     THIS SUBROUTINE SOLVES THE SIMULTANEOUS EQUATIONS
C     ------------------------------------------------------------------
      DIMENSION A(1),B(1)
C        COMPACT THE MATRIX
        KS=N
      DO 10 J=2,N
      DO 10 I=1,N
      KS=KS+1
      L=(J-1)*KDIM+I
   10 A(KS)=A(L)
C        FORWARD SOLUTION
      TOL=0.0
      KS=0
      JJ=-N
      DO 65 J=1,N
      JY=J+1
      JJ=JJ+N+1
      BIGA=0
      IT=JJ-J
      DO 30 I=J,N
C        SEARCH FOR MAXIMUM COEFFICIENT IN COLUMN
      IJ=IT+I
      IF(ABS(BIGA)-ABS(A(IJ))) 20,30,30
   20 BIGA=A(IJ)
      IMAX=I
   30 CONTINUE
C        TEST FOR PIVOT LESS THAN TOLERANCE (SINGULAR MATRIX)
      IF(ABS(BIGA)-TOL) 35,35,40
   35 KS=1
      RETURN
C        INTERCHANGE ROWS IF NECESSARY
   40 I1=J+N*(J-2)
      IT=IMAX-J
      DO 50 K=J,N
      I1=I1+N
      I2=I1+IT
      SAVE=A(I1)
      A(I1)=A(I2)
      A(I2)=SAVE
C        DIVIDE EQUATION BY LEADING COEFFICIENT
   50 A(I1)=A(I1)/BIGA
      SAVE=B(IMAX)
      B(IMAX)=B(J)
      B(J)=SAVE/BIGA
C        ELIMINATE NEXT VARIABLE
      IF(J-N) 55,70,55
   55 IQS=N*(J-1)
      DO 65 IX=JY,N
      IXJ=IQS+IX
      IT=J-IX
         DO 60 JX=JY,N
      IXJX=N*(JX-1)+IX
      JJX=IXJX+IT
   60 A(IXJX)=A(IXJX)-(A(IXJ)*A(JJX))
   65 B(IX)=B(IX)-(B(J)*A(IXJ))
C        BACK SOLUTION
   70 NY=N-1
      IT=N*N
      DO 80 J=1,NY
      IA=IT-J
      IB=N-J
      IC=N
      DO 80 K=1,J
      B(IB)=B(IB)-A(IA)*B(IC)
      IA=IA-N
   80 IC=IC-1
      RETURN
      END
C**********************************************************************
      SUBROUTINE EIGEN (A,R,N,MV)
C**********************************************************************
      DIMENSION A(1),R(1)
C
C     GENERATE IDENTITY MATRIX
C
      IF(MV-1)10,25,10
   10 IQ=-N
      DO 20 J=1,N
      IQ=IQ+N
      DO 20 I=1,N
      IJ=IQ+I
      R(IJ)=0.0
      IF(I-J)20,15,20
   15 R(IJ)=1.0
   20 CONTINUE
C
C     COMPUTE INITIAL AND FINAL NORMS (ANORM AND ANORMX)
C
   25 ANORM=0.0
      DO 35 I=1,N
      DO 35 J=I,N
      IF(I-J)30,35,30
   30 IA=I+(J*J-J)/2
      ANORM=ANORM+A(IA)*A(IA)
   35 CONTINUE
      IF(ANORM)165,165,40
   40 ANORM=1.414*SQRT(ANORM)
      ANRMX=ANORM*1.0E-6/FLOAT(N)
      IND=0
      THR=ANORM
   45 THR=THR/FLOAT(N)
   50 L=1
   55 M=L+1
C      write(*,*) 'compute sin and cos'
C
C     COMPUTE SIN AND COS
C
   60 MQ=(M*M-M)/2
      LQ=(L*L-L)/2
      LM=L+MQ
   62 IF(ABS(A(LM))-THR)130,65,65
   65 IND=1
      LL=L+LQ
      MM=M+MQ
      X=0.5*(A(LL)-A(MM))
   68 Y=-A(LM)/SQRT(A(LM)*A(LM)+X*X)
      IF(X)70,75,75
   70 Y=-Y
   75 SINX=Y/SQRT(2.0*(1.0+(SQRT(1.0-Y*Y))))
      SINX2=SINX*SINX
   78 COSX=SQRT(1.0-SINX2)
      COSX2=COSX*COSX
      SINCS=SINX*COSX
C
C     ROTATE L AND M COLUMNS
C
      ILQ=N*(L-1)
      IMQ=N*(M-1)
      DO 125 I=1,N
      IQ=(I*I-I)/2
      IF(I-L)80,115,80
   80 IF(I-M)85,115,90
   85 IM=I+MQ
      GO TO 95
   90 IM=M+IQ
   95 IF(I-L)100,105,105
  100 IL=I+LQ
      GO TO 110
  105 IL=L+IQ
  110 X=A(IL)*COSX-A(IM)*SINX
      A(IM)=A(IL)*SINX+A(IM)*COSX
      A(IL)=X
  115 IF(MV-1)120,125,120
  120 ILR=ILQ+I
      IMR=IMQ+I
      X=R(ILR)*COSX-R(IMR)*SINX
      R(IMR)=R(ILR)*SINX+R(IMR)*COSX
      R(ILR)=X
  125 CONTINUE
      X=2.0*A(LM)*SINCS
      Y=A(LL)*COSX2+A(MM)*SINX2-X
      X=A(LL)*SINX2+A(MM)*COSX2+X
      A(LM)=(A(LL)-A(MM))*SINCS+A(LM)*(COSX2-SINX2)
      A(LL)=Y
      A(MM)=X
C
C     TEST FOR COMPLETION
C
C     TEST FOR M = LAST COLUMN
C
  130 IF(M-N)135,140,135
  135 M=M+1
      GO TO 60
C
C     TEST FOR L = SECOND FROM LAST COLUMN
C
  140 IF(L-(N-1))145,150,145
  145 L=L+1
      GO TO 55
  150 IF(IND-1)160,155,160
  155 IND=0
      GO TO 50
C
C     COMPARE THRESHOLD WITH FINAL NORM
C
  160 IF(THR-ANRMX)165,165,45
C
C     SORT EIGENVALUES AND EIGENVECTORS
C
  165 IQ=-N
      DO 185 I=1,N
      IQ=IQ+N
      LL=I+(I*I-I)/2
      JQ=N*(I-2)
      DO 185 J=I,N
      JQ=JQ+N
      MM=J+(J*J-J)/2
      IF(A(LL)-A(MM))170,185,185
  170 X=A(LL)
      A(LL)=A(MM)
      A(MM)=X
      IF(MV-1)175,185,175
  175 DO 180 K=1,N
      ILR=IQ+K
      IMR=JQ+K
      X=R(ILR)
      R(ILR)=R(IMR)
  180 R(IMR)=X
  185 CONTINUE
      RETURN
      END





C*****************************************************************
      SUBROUTINE NEWMOD(NLAY)
C*****************************************************************
      COMMON/FIVE/ BPS(17,5),YIS(3,17),EOLD(6),DD(5),SV(5),
     1         RWS(17,5,5),RXS(17,5,5),RYS(17,5,5),RZS(17,5,5),
     2         RXYS(17,5,5),RXZS(17,5,5),RYZS(17,5,5),TDFL(17,5)
      COMMON/FOUR/NAXLES,LP(4),MINTOUT,MAXTOUT,MAXCOUT,MINSEG,MAXSEG,
     1   ICTRIX,ICPRIN,ICSTD,ITSTD,ITRIAX,ITPRIN,PWHL(4),KTYPE(9)
      COMMON/TWO/KONE(6),KTWO(6),NSTEPS,NITER,GAMMA(6),KNOT(6),HHH(6),
     *SIGMAV(6),THETAI(6),THETAF(6),INTERF,NP,NOTENS
      COMMON /SCFCCI/ E(7),VRAT(7)
      COMMON /INPUT/ TA,TE,TI,MI,WI,RA,RE,RI,STIF1,PR,TWT,RWT,TIEWD
      COMMON /SCFBBI/ HH(7),IR,IZ,RR(110),WGT,ZZ(20)
      COMMON/THREE/TIEL,NSEG,TIESP,NTIES,NLAYER(3),STA(10)
      DIMENSION MR(17,5),GV(4),SIGD(17,5)
      REAL*8 KNOT,KONE,KTWO,J2,MR
      LLAST=LP(NAXLES)
      IDEP=NLAY+1
      GV(NLAY)=ZZ(IDEP)*GAMMA(NLAY)
      DO 20 I=1,LLAST
      DO 15 J=1,5
      K=NLAY
      SIGOCT=((-1.*(RXS(I,J,K)+RYS(I,J,K)+RZS(I,J,K)))+
     1      ((GV(K)+STA(J))*(1.+2.*KNOT(K))))/3.
      D1=-1.*RXS(I,J,K)+RYS(I,J,K)
      D2=-1.*RXS(I,J,K)+RZS(I,J,K)-((GV(K)+STA(J))*(1.-KNOT(K)))
      D3=-1.*RYS(I,J,K)+RZS(I,J,K)-((GV(K)+STA(J))*(1.-KNOT(K)))
      D4=RXYS(I,J,K)
      D5=RXZS(I,J,K)
      D6=RYZS(I,J,K)
      J2=((D1**2+D2**2+D3**2)/6.)+(D4**2+D5**2+D6**2)
      TAUOCT=ABS(SQRT(2.*J2/3.))
      S1M=SIGOCT+(SQRT(2.))*TAUOCT
      S3M=SIGOCT-(1./SQRT(2.))*TAUOCT
      IF(KTYPE(NLAY).GT.4) GO TO 2
      PM=(S1M+S3M)/2.
      QM=(S1M-S3M)/2.
      PO=(GV(K)+STA(J))*(1.+KNOT(K))/2.
      QO=(GV(K)+STA(J))*(1.-KNOT(K))/2.
      DQ=QM-QO
      AC=(S3M**0.088)/1.282
      BC=QM*(S3M**(-1.145))/0.0008647
      EC=(BC**AC)*0.000001
      AE=(QO*(S3M**(-1.5183)))/0.0002535
      EE=AE*0.000001
      MR(I,J)=(2.*DQ)/(EC-EE)
      GO TO 15
    2 SIGD(I,J)=S1M-S3M
      IF(KTYPE(NLAY).EQ.5) GO TO 3
      IF(KTYPE(NLAY).EQ.6) GO TO 4
      IF(KTYPE(NLAY).EQ.7) GO TO 5
      IF(KTYPE(NLAY).EQ.8) GO TO 6
    3 ESA=17000.
      SDA=0.
      ESB=17000.
      SDB=2.
      ESC=12400.
      SDC=6.2
      ESD=7600.
      SDD=32.
      GO TO 7
    4 ESA=12400.
      SDA=0.
      ESB=12400.
      SDB=2.
      ESC=7500.
      SDC=6.2
      ESD=4700.
      SDD=23.
      GO TO 7
    5 ESA=7600.
      SDA=0.
      ESB=7600.
      SDB=2.
      ESC=3000.
      SDC=6.2
      ESD=1850.
      SDD=13.
      GO TO 7
    6 ESA=5700.
      SDA=0.
      ESB=5700.
      SDB=2.
      ESC=1000.
      SDC=6.2
      ESD=1000.
      SDD=10.
    7 IF(SIGD(I,J).LT.SDB) GO TO 8
      GO TO 9
    8 MR(I,J)=ESA
      GO TO 15
    9 IF((SIGD(I,J).GE.SDB).AND.(SIGD(I,J).LT.SDC)) GO TO 10
      GO TO 11
   10 DK=(ESB-ESC)/(SDC-SDB)
      DE=(SIGD(I,J)-SDB)*DK
      MR(I,J)=ESB-DE
      GO TO 15
   11 IF((SIGD(I,J).GE.SDC).AND.(SIGD(I,J).LT.SDD)) GO TO 12
      GO TO 13
   12 DK=(ESC-ESD)/(SDD-SDC)
      DE=(SIGD(I,J)-SDC)*DK
      MR(I,J)=ESC-DE
      GO TO 15
   13 IF(SIGD(I,J).GE.SDD) MR(I,J)=ESD
   15 CONTINUE
   20 CONTINUE
      RSL=0.
      DO 30 I=1,LLAST
      RSL=RSL+YIS(2,I)
   30 CONTINUE
      TT=0.
      DO 50 I=1,LLAST
      T=0.
      DO 40 J=1,5
      T=T+MR(I,J)/5.
   40 CONTINUE
      TT=TT+(T*YIS(2,I))
   50 CONTINUE
      E(NLAY)=TT/RSL
      RETURN
      END



C*****************************************************************
      SUBROUTINE TRIAXL(RXS,RYS,RZS,RXYS,RXZS,RYZS,MAXOUT,G,M)
C*****************************************************************
C      CALCULATE EQUIVALENT TRIAXIAL COMPRESSION STATES
C
      COMMON/FOUR/NAXLES,LP(4),MINTOUT,MAXTOUT,MAXCOUT,MINSEG,MAXSEG,
     1   ICTRIX,ICPRIN,ICSTD,ITSTD,ITRIAX,ITPRIN,PWHL(4),KTYPE(9)
      COMMON/TWO/KONE(6),KTWO(6),NSTEPS,NITER,GAMMA(6),KNOT(6),HHH(6),
     *SIGMAV(6),THETAI(6),THETAF(7),INTERF,NP,NLAST
      COMMON/SCFBBI/HH(7),IR,IZ,RR(110),WGT,ZZ(20)
      COMMON/SCFBCI/NS,PSI,NL
      DIMENSION  RXS(17,5,5),RYS(17,5,5),RZS(17,5,5),RXYS(17,5,5),
     1   RXZS(17,5,5),RYZS(17,5,5),G(10)
      DIMENSION DD(7),SV(7),SIGV(7),SIG(7)
      REAL*8 KNOT,KONE,KTWO
      DD(1)=HH(1)
      SV(1)=DD(1)*GAMMA(1)
      NSM1=NS-1
      DO 740 I=2,NSM1
      J=I-1
      DD(I)=DD(J)+HH(I)
      SV(I)=SV(J)+HH(I)*GAMMA(I)
  740 CONTINUE
      IF(NS.EQ.5) GO TO 7402
      DO 7401 I=NS,5
      DD(I)=DD(NSM1)
      SV(I)=SV(NSM1)
      GAMMA(I)=GAMMA(NS)
 7401 CONTINUE
 7402 KK=0
      DO 746 I=2,IZ
      IF(ZZ(I).LE.DD(1)) GO TO 741
      IF(ZZ(I).LE.DD(2)) GO TO 742
      IF(ZZ(I).LE.DD(3)) GO TO 743
      IF(ZZ(I).LE.DD(4)) GO TO 744
      GO TO 745
  741 KK=KK+1
      SIGV(KK)=ZZ(I)*GAMMA(1)
      GO TO 746
  742 KK=KK+1
      SIGV(KK)=SV(1)+(ZZ(I)-DD(1))*GAMMA(2)
      GO TO 746
  743 KK=KK+1
      SIGV(KK)=SV(2)+(ZZ(I)-DD(2))*GAMMA(3)
      GO TO 746
  744 KK=KK+1
      SIGV(KK)=SV(3)+(ZZ(I)-DD(3))*GAMMA(4)
      GO TO 746
  745 KK=KK+1
      SIGV(KK)=SV(4)+(ZZ(I)-DD(4))*GAMMA(5)
  746 CONTINUE
  729 FORMAT(1H1,//,29X,'GEOSTAT.',2X,'INIT.',2X,'INIT.',16X,
     1'EQUIVALENT TRIAXIAL STATES',/,4X,'TIE',1X,'SEG',1X,'POINT',
     21X,'DEPTH',1X,'KNOT',1X,'VERT.STR.',2X,'P',6X,'Q',6X,
     3'SOCT',3X,'TOCT',2X,'SIG 1',2X,'SIG 3',2X,'MAX P',2X,
     4'MAX Q',/,18X,'(IN.)',8X,'(PSI)',3X,'(PSI)',2X,'(PSI)',
     52X,'(PSI)',2X,'(PSI)',2X,'(PSI)',2X,'(PSI)',2X,'(PSI)',
     62X,'(PSI)',/)
  739 FORMAT(1H1,//,29X,'GEOSTAT.',2X,'INIT.',2X,'INIT.',16X,
     1'EQUIVALENT TRIAXIAL STATES',/,3X,'CRIB',1X,'SEG',1X,'POINT',
     21X,'DEPTH',1X,'KNOT',1X,'VERT.STR.',2X,'P',6X,'Q',6X,
     3'SOCT',3X,'TOCT',2X,'SIG 1',2X,'SIG 3',2X,'MAX P',2X,
     4'MAX Q',/,18X,'(IN.)',8X,'(PSI)',3X,'(PSI)',2X,'(PSI)',
     52X,'(PSI)',2X,'(PSI)',2X,'(PSI)',2X,'(PSI)',2X,'(PSI)',
     62X,'(PSI)',/)
      DO 749 I=MINTOUT,MAXOUT
      IF(M.EQ.1) WRITE(*,739)
      IF(M.EQ.0) WRITE(*,729)
      DO 748 J=MINSEG,MAXSEG
      DO 747 K=1,NL
      SIGOCT=((-1.*(RXS(I,J,K)+RYS(I,J,K)+RZS(I,J,K)))+
     1      ((SIGV(K)+G(J))*(1.+2.*KNOT(K))))/3.
      D1=-1.*RXS(I,J,K)+RYS(I,J,K)
      D2=-1.*RXS(I,J,K)+RZS(I,J,K)-((SIGV(K)+G(J))*(1.-KNOT(K)))
      D3=-1.*RYS(I,J,K)+RZS(I,J,K)-((SIGV(K)+G(J))*(1.-KNOT(K)))
      D4=RXYS(I,J,K)
      D5=RXZS(I,J,K)
      D6=RYZS(I,J,K)
      DJ2=((D1**2+D2**2+D3**2)/6.)+(D4**2+D5**2+D6**2)
      TAUOCT=ABS(SQRT(2.*DJ2/3.))
      S1M=SIGOCT+(SQRT(2.))*TAUOCT
      S3M=SIGOCT-(1./SQRT(2.))*TAUOCT
      PM=(S1M+S3M)/2.
      QM=(S1M-S3M)/2.
      PO=(SIGV(K)+G(J))*(1.+KNOT(K))/2.
      QO=(SIGV(K)+G(J))*(1.-KNOT(K))/2.
      KK=K+1
      SIG(K)=SIGV(K)+G(J)
      IF(K.EQ.1) WRITE(*,751)
  751 FORMAT(/,4X,'----------------------------------------',
     1'--------------------------------------------------',/)
      WRITE(*,750) I,J,K,ZZ(KK),KNOT(K),SIG(K),PO,QO,SIGOCT,
     1            TAUOCT,S1M,S3M,PM,QM
  747 CONTINUE
  748 CONTINUE
  749 CONTINUE
  750 FORMAT(4X,I2,2X,I2,4X,I1,2X,F5.1,2X,F5.2,2X,F5.2,3X,
     1       F5.2,7(1X,F6.2))
      RETURN
99    STOP
      END


C***************************************************************
      SUBROUTINE AVRG (AY,IZ)
C***************************************************************
      COMMON/ONE/ COEF(110,6,5)
      DIMENSION AY(11,10,6),UY(11,10,6),AT(11,10,6)
      DO 5 I=1,11
      DO 5 J=1,10
      DO 5 K=1,IZ
      AY(I,J,K)=0.
      UY(I,J,K)=0.
      AT(I,J,K)=0.
    5 CONTINUE
C     THREE LOOPS TO PLACE THE COEFS. INTO EQUIVALENT 'UY' POSITIONS
C     AS WERE ORIGINALLY ON TAPE
      JJ=1
      DO 30 J=1,10
      DO 20 I=1,11
      DO 10 K=1,IZ
      UY(I,J,K)=COEF(JJ,K,5)
   10 CONTINUE
      JJ=JJ+1
   20 CONTINUE
   30 CONTINUE
C      DEFINE SYMMETRY OF VERTICAL DISPLACEMENT COEFFICIENTS AS
C      WAS PREVIOUSLY DONE IN SUBROUTINE STMTRX
      DO 90 JJ=1,10
      DO 50 L=1,10
      NTL=JJ-L
      IF(NTL.LT.0) NTL=-NTL
      J=NTL+1
      DO 40 K=1,IZ
      DO 40 I=1,11
      AY(I,L,K)=UY(I,J,K)
   40 CONTINUE
   50 CONTINUE
      DO 70 K=1,IZ
      DO 70 I=1,11
      DO 60 L=1,5
      LL=11-L
      AY(I,L,K)=AY(I,L,K)+AY(I,LL,K)
   60 CONTINUE
   70 CONTINUE
      IF(JJ.GT.5) GO TO 90
      DO 80 J=1,10
      AT(1,J,JJ)=-AY(1,J,1)
      AT(2,J,JJ)=-AY(3,J,1)
      AT(3,J,JJ)=-AY(5,J,1)
      AT(4,J,JJ)=-AY(7,J,1)
      AT(5,J,JJ)=-AY(9,J,1)
      AT(6,J,JJ)=-AY(11,J,1)
   80 CONTINUE
   90 CONTINUE
      DO 100 I=1,11
      DO 100 J=1,10
      DO 100 K=1,5
      AY(I,J,K)=0.
  100 CONTINUE
      DO 110 I=1,6
      DO 110 J=1,10
      DO 110 K=1,5
      AY(I,J,K)=AT(I,J,K)
  110 CONTINUE
      RETURN
      END

C**********************************************************************/
C
C**********************************************************************/
