      program geotrack_custom

      DOUBLE PRECISION CTR
      COMMON /ONE/A(396,5),B(396,5),C(396,5),D(396,5),T1,T2,T1M,AJ(396),
     *        L,NLINE,RJ0(396),RJ1(396),TZZ,Z,COM,CMU,CSR,CST,CSZ,CTR

      REAL, PARAMETER :: PI  = 3.141592654
      REAL, PARAMETER :: WGT = 1.0

      character(len=60) :: TITLE
      INTEGER, PARAMETER :: MAX_NS     = 5
      INTEGER, PARAMETER :: MAX_NAXLES = 4

      DIMENSION E(MAX_NS)    ! Resilient Young's Modulus, Er for layer I(psi) [100.1 <= E <= 0.1E30]
      DIMENSION VRAT(MAX_NS) ! Poisson's ratio for layer I [0.05 <= VRAT <= 0.499]
      DIMENSION HH(MAX_NS)   ! Depth of layer I [HH <= 1000]
                             ! *Note: HH(I) for last layer (i.e., I = NS) must be 0.0
      DIMENSION GAMMA(MAX_NS)! Soil unit layer for layer I (lbs/ft^3)
      REAL KNOT(MAX_NS)      ! Coefficient of lateral earth pressure for layer I
      DIMENSION KTYPE(MAX_NS)! Type of stress-dependent relationship to use for layer I
      REAL KONE(MAX_NS)      ! Coefficient for calculation of stress-dependent soil modulus
      REAL KTWO(MAX_NS)      ! Coefficient for calculation of stress-dependent soil modulus
                             ! *If KTYPE(I) = 0 or KTYPE(I) >= 5, KONE(I) and KTWO(I) can be input as 0.0
      REAL ZZ(MAX_NS)        ! Depths at which moduli are computed

      INTEGER LP(MAX_NAXLES), CHECKR(50)
      REAL    PWHL(MAX_NAXLES)

      DIMENSION RR(110)
      DIMENSION ANGNEW(60)
      DIMENSION H(MAX_NS+1)
      DIMENSION BZ(100), AZ(396), COEF(110,6,5), PM(6,4,4)
      INTEGER   ITN, K, NTEST
      REAL      R, SF, AR



      TITLE = 'TEST RUN'
      ! Options
      LACOPT = 1 ! Solve for the track response [0,1]
      INTERF = 0 !* Solve for other depth points [0,1]
      NOTENS = 0 ! Limit max incremental  tie/soil tensile forces [0,1]
      NOTHER = 0 !* Solve for offset depths [0,1]

      IOPT  = 1 ! Print soil influence coefficiens [0,1]
      ITOPT = 0 ! Print computed values for each iteration [0,1]

      NS    = 2 ! Number of soil layers, [2 <= NS <= 5]
      NITER = 0 ! Number of iterations to run [~3]
      TIEL  = 102.00 ! Tie Length (IN.)
      TIESP = 19.50  ! Tie Spacing (IN.)
      TIEWD = 9.00   ! Tie Width (IN.)

      TA  = 63.00     ! Cross-sectional area of tie (SQ. IN)
      TE  = 386000000.00 ! Young's Modulus of tie (PSI)
      TI  = 1.00      ! Moment of inertia of tie about major axis (IN.^4)
      TWT = 250.00    ! Tie Weight (LBS)

      MI = 6     ! Number of tie segments having centers between the rails
                 ! *MI = 6 is used for standard U.S. rail spacing
      WI = 59.25 ! Rail spacing (IN.) *Standard rail spacing in U.S. is
                 ! WI = 59.25 in. betweem the centers of the rail heads.

      RA  = 13.35      ! Rail cross-sectional area (SQ. IN.)
      RE  = 2800000000.00 ! Young's modulus for rail (PSI)
      RI  = 1.00       ! Moment of inertia for rail about major axis (IN.^4)
      RWT = 136.00     ! Rail weight (LBS/YARD)

      STIF1 = 7000000.0 ! Rail fastener or tie pad stiffness (LB/IN.)
                        ! For out spikes and plates, STIF =~ 7,000,000 LB/IN.
      NAXLES = 1 ! Number of axles to be applied to the track.
                 ! NAXLES = 1 for single axle loading [NAXLES <= 4]
      OFFSET = 0 ! Offset distance from track center line at which
                 ! stresses will be computed [OFFSET > TIEL/2]
      NPT    = 0 ! Number of depths corresponding to the offset distance
                 ! at which stresses will be computed. [NPT <= 5]

      NEWFLAG = 1
      NOUTP   = 0
!!!!!!!!!!!???????????????????      real, dimension(5) :: E



      E(1)     = 45000.00
      E(2)     = 8000.00

      VRAT(1)  = 0.30
      VRAT(2)  = 0.40

      HH(1)    = 12.00
      HH(2)    = 0.00

      GAMMA(1) = 104.00
      GAMMA(2) = 110.00

      KNOT(1)  = 1.00
      KNOT(2)  = 0.75

      KTYPE(1) = 3
      KTYPE(2) = 1

      KONE(1)  = 1543.000
      KONE(2)  = 877.000

      KTWO(1)  = 425.000
      KTWO(2)  = 1.100

      ! LP(I): Tie number at which load I is applied.
      ! LP(I) must equal 1 for the first entry in this data group

      ! PWHL(I): Wheel load applied to tie I (KIPS)

      ! *Note: The number of entries for LP and PWHL must be equal to
      !        NAXLES. The maximum tie number that can be loaded is tie
      !        number 12. PWHL(I) is actually half f the total axle
      !        load applied to the rail. Since, only symmetrical tie
      !        loadings are considered, PWHL(I) is the wheel load per axle.
      !        Unequal wheel loads can be considered.
      LP(1)   = 1
      PWHL(1) = 32.90

      ! *Note: The possible range for tie output is from tie 1 to tie 17.
      !        The assumed loading influence zone is five ties away from
      !        any loaded point, i.e., if the last loaded tie is tie
      !        number 6, then ties 12 through 17 will be unaffected.
      !        Symmetry of rail loading should be considered to reduce
      !        output.

      !        The possible range for tie segment output is from segment
      !        ! to segment 5.
      MINTOUT = 1 ! First tie at which output is desired
      MAXTOUT = 6 ! Last tie at which output is desired
      MINSEG  = 1 ! First tie segment at which output is desired
      MAXSEG  = 5 ! Last tie segment at which output is desired

      ITSTD  = 1 ! Option for  printing rail deflections, rail seat loads,
                 ! tie deflections, tie bending moments, rail bending
                 ! moments, ad the soil stresses beneath the ties. [0,1]
      ITPRIN = 0 ! Option for calculation of principal stresses and
                 ! direction cosines for the soil stresses beneath the ties [0,1]
      ITRIAX = 1 ! Option for calculation of equivalent triaxial states for
                 ! soil stresses. [0,1]

      ZZ(1) = 6.00
      ZZ(2) = 15.00

      LP1 = LP(1)
      LP2 = LP(2)
      LP3 = LP(3)
      LP4 = LP(4)

      PW1 = PWHL(1)
      PW2 = PWHL(2)
      PW3 = PWHL(3)
      PW4 = PWHL(4)

      ! Default values
      NSEG    = 10
      NTIES   = 11


      ! Calculated values
      REI = RE * RI
      TEI = TE * TI

      NTIES   = INT(NTIES/2.) + 1
      NPOINTS = NTIES * 2 - 1
      SEGL    = TIEL / NSEG
      PSI     = TIEWD * SEGL

      IZ = NS + 1


      DO I = 1,4
        PWHL(I)=PWHL(I) *1000.
      END DO

      IR    = NPOINTS * NSEG
      XINCR = SEGL
      YINCR = TIESP/2.0

      II  = IR + 1
      IIZ = IZ + 1

      L1 = 0
      X  = 0.
      DO J = 1,NSEG
        IF(J.NE.1) X = X + XINCR
        Y  = 0.
        DO I = 1,NPOINTS
            L1 = L1 + 1
            IF(L1.EQ.1) THEN
                RR(L1) = 0.
            ELSE
                IF(I.NE.1) Y = Y + YINCR
                RR(L1) = SQRT(X**2+Y**2)
            END IF
        END DO
      END DO

      ! print the input values
      CALL PRINT_OUTPUT


      ! UNIT 1 IS AN INTERNAL SCRATCH FILE ONTO WHICH THE INFLUENCE
      ! COEFFICIENTS ARE WRITTEN
      OPEN(1, FILE="SCR", STATUS='REPLACE', FORM='FORMATTED')

      !!!!!!!!!! TRYING TO DO THE CALC. STEP NO. 1 !!!!!!!!!!!!!!!!!!!!!

      NTIMES = NITER + 1

      DO NSTEPS = 1, NTIMES
        WRITE(*,3370) NSTEPS
        REWIND 1
        CALL SCERJ
        CALL CALCUL
      END DO




 3370 FORMAT(//,10X,'WORKING ON CALCULATION STEP NUMBER ',I2)

      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


      ! Subroutines used by the main program
      CONTAINS


      !*****************************************************************
      SUBROUTINE PRINT_OUTPUT
      !*****************************************************************

      WRITE(*,*) TITLE

      WRITE(*,3180) TIEL,NSEG,MI,TIESP,TIEWD,TA,TWT,TEI,WI,RA,RWT,REI,
     *   STIF1

      WRITE(*,3190) NAXLES,LP1,LP2,LP3,LP4,PW1,PW2,PW3,PW4,
     *   NS,LACOPT,NITER,INTERF,NOTHER,NOTENS,
     *   MINTOUT,MAXTOUT,MINSEG,MAXSEG,ITSTD,ITPRIN,ITRIAX

      WRITE(*,3200)
      WRITE(*,3210)(I,E(I),VRAT(I),HH(I),GAMMA(I),KNOT(I),I=1,NS)

      WRITE(*,3230)
      DO I = 1, NS
          IF(KTYPE(I).EQ.0) THEN
            WRITE(*,3240) I, KTYPE(I)
          ELSE IF(KTYPE(I).EQ.1) THEN
            WRITE(*,3250) I, KTYPE(I), KONE(I), KTWO(I)
          ELSE IF(KTYPE(I).EQ.2) THEN
            WRITE(*,3260) I, KTYPE(I), KONE(I), KTWO(I)
          ELSE IF(KTYPE(I).EQ.3) THEN
            WRITE(*,3270) I, KTYPE(I), KONE(I), KTWO(I)
          ELSE IF(KTYPE(I).EQ.4) THEN
            WRITE(*,3280) I, KTYPE(I)
          ELSE IF(KTYPE(I).EQ.5) THEN
            WRITE(*,3290) I, KTYPE(I)
          ELSE IF(KTYPE(I).EQ.6) THEN
            WRITE(*,3300) I, KTYPE(I)
          ELSE IF(KTYPE(I).EQ.7) THEN
            WRITE(*,3310) I, KTYPE(I)
          ELSE IF(KTYPE(I).EQ.8) THEN
            WRITE(*,3320) I, KTYPE(I)
          END IF
      END DO

      WRITE(*,3355)
      WRITE(*,3360) (I, ZZ(I), I=1,NS)


 3180 FORMAT(//,2X,
     *8X, 'TIE LENGTH ........................ ',F5.1,2X,'IN.'/,
     *10X,'NUMBER OF SEGMENTS PER TIE ........ ',I2,/,
     *10X,'NUMBER OF SEGMENTS BETWEEN RAILS .. ',I2,/,
     *10X,'TIE SPACING ....................... ',F5.1,2X,'IN.',/,
     *10X,'TIE WIDTH ......................... ',F5.1,2X,'IN.',/,
     *10X,'TIE AREA .......................... ',F5.1,2X,'SQ.IN.',/,
     *10X,'TIE WEIGHT ........................ ',F6.1,2X,'LBS',/,
     *10X,'TIE EI ............................ ',F13.2,2X,'LB-SQ.IN.'/,
     *10X,'RAIL SPACING ...................... ',F6.2,2X,'IN.',/,
     *10X,'RAIL AREA ......................... ',F6.2,2X,'SQ.IN.',/,
     *10X,'RAIL WEIGHT ....................... ',F6.1,2X,'LBS PER YARD',
     */,
     *10X,'RAIL EI ........................... ',F13.2,2X,'LB-SQ.IN.' ,
     *   /,
     *10X,'RAIL FASTENER STIFFNESS ........... ',F10.1,2X,
     *     'LBS PER IN.')

 3190 FORMAT(2X,
     *8X, 'NUMBER OF AXLE LOADS .............. ',I2,/,
     *10X,'AXLE LOADS ARE ON TIE NUMBER(S) ... ',I2,3(2X,I2),/,
     *10X,'WHEEL LOAD PER AXLE (KIPS) ........ ',F5.2,3(2X,F5.2),/,
     *10X,'NUMBER OF SOIL LAYERS ............. ',I2,/,
     *10X,'SOLVE FOR FULL TRACK RESPONSE ..... ',I2,/,
     *10X,'NUMBER OF ITERATIONS .............. ',I2,/,
     *10X,'CALCULATE AT OTHER DEPTH POINTS ... ',I2,/,
     *10X,'CALCULATE AT OFFSET LOCATIONS ..... ',I2,/,
     *10X,'LIMIT TIE/BALLAST TENSION ......... ',I2,/,
     *10X,'FIRST TIE OUTPUT .................. ',I2,/,
     *10X,'LAST TIE OUTPUT ................... ',I2,/,
     *10X,'FIRST TIE SEGMENT OUTPUT .......... ',I2,/,
     *10X,'LAST TIE SEGMENT OUTPUT ........... ',I2,/,
     *10X,'PRINT STANDARD SOIL OUTPUT ........ ',I2,/,
     *10X,'PRINT PRINCIPAL STRESSES .......... ',I2,/,
     *10X,'PRINT EQUIVALENT TRIAXIAL DATA .... ',I2,//)


 3200 FORMAT(/5X,'LAYER',8X,'MODULUS',6X,'VRAT',7X,'DEPTH',6X,'GAMMA',
     *5X,'KNOT',/,19X,'(PSI)',18X,'(IN.)',6X,'(PCF)',/)

 3210 FORMAT(7X,I1,7X,F10.0,6X,F4.2,6X,F6.2,6X,F5.1,5X,F4.2)

 3230 FORMAT(//5X,'LAYER',4X,'KTYPE',16X,'MODULUS DESCRIPTION',/)
 3240 FORMAT(6X,I2,7X,I2,5X,'CONSTANT')
 3250 FORMAT(6X,I2,7X,I2,5X,'LOG E - LOG BULK STRESS K1 = ',F8.2,
     *2X,'K2 = ',F8.3)
 3260 FORMAT(6X,I2,7X,I2,5X,'E - LOG BULK STRESS     K1 = ',F8.2,
     *2X,'K2 = ',F8.3)
 3270 FORMAT(6X,I2,7X,I2,5X,'E - BULK STRESS         K1 = ',F8.2,
     *2X,'K2 = ',F8.3)
 3280 FORMAT(6X,I2,7X,I2,5X,'EXPERIMENTAL SHEAR STRESS REVERSAL')
 3290 FORMAT(6X,I2,7X,I2,5X,'E - DEVIATOR STRESS FOR STIFF SUBGRADE')
 3300 FORMAT(6X,I2,7X,I2,5X,'E - DEVIATOR STRESS FOR MEDIUM SUBGRADE')
 3310 FORMAT(6X,I2,7X,I2,5X,'E - DEVIATOR STRESS FOR SOFT SUBGRADE')
 3320 FORMAT(6X,I2,7X,I2,5X,'E - DEVIATOR STRESS FOR VERY SOFT',
     *' SUBGRADE')

 3355 FORMAT(//,17X,'DIFFERENT DEPTHS AT WHICH MODULI ARE COMPUTED'/)
 3360 FORMAT(32X,'Z(',I1,') = 'F6.2,' IN.')

      END SUBROUTINE PRINT_OUTPUT
      !*****************************************************************


      !*****************************************************************
        SUBROUTINE SCERJ
      !*****************************************************************
        LOGICAL LOG,B1
        B1=.TRUE.
        LOG=.TRUE.

C  CHECK ON THE NUMBER OF LAYERS, NS
C
        IF (NS.GT.0) GO TO 10
        WRITE(3,3000) NS
        WRITE(*,3000) NS
        LOG=.FALSE.
   10 IF (NS.LE.5) GO TO 20
        WRITE(3,3010) NS
        WRITE(*,3010) NS
        LOG=.FALSE.

C  CHECK ON THE LAYER MODULI AND POISSON'S RATIOS, E(I) & VRAT(I)

   20 IF(NS.GT.5) GO TO 70
        IF (NS.LT.1) GO TO 70
        DO 60 I=1,NS
        IF (E(I).GT.100) GO TO 30
        WRITE(3,3020) I,I,E(I)
        WRITE(*,3020) I,I,E(I)
        LOG=.FALSE.
   30 IF (E(I).LT.0.100E30) GO TO 40
        WRITE(3,3030) I,I,E(I)
        WRITE(*,3030) I,I,E(I)
        LOG=.FALSE.
   40 IF (VRAT(I).GT.0.05) GO TO 50
        WRITE(3,3040) I,I,VRAT(I)
        WRITE(*,3040) I,I,VRAT(I)
        LOG=.FALSE.
   50 IF (VRAT(I).LE.0.50) GO TO 60
        WRITE(3,3050) I,I,VRAT(I)
        WRITE(*,3050) I,I,VRAT(I)
        LOG=.FALSE.
   60 CONTINUE
   70  CONTINUE

C  CHECK ON THE LAYER DEPTHS, HH(I)
C
        N = NS-1
        IF (N.LE.0) GO TO 100
        IF (N.GT.14) GO TO 100
        DO 90 I=1,N
        IF (HH(I).GT.0.0) GO TO 80
        WRITE(3,3060) I,I,HH(I)
        WRITE(*,3060) I,I,HH(I)
        LOG=.FALSE.
   80 IF (HH(I).LT.1000.0) GO TO 90
        WRITE(3,3070) I,I,HH(I)
        WRITE(*,3070) I,I,HH(I)
        LOG=.FALSE.
   90 CONTINUE
  100 CONTINUE
        IF(LOG .AND. B1)RETURN
        RETURN

C  FORMAT STATEMENTS
 3000 FORMAT (//,2X,'INPUT ERROR, NS IS LESS THAN ZERO, NS =',I4)
 3010 FORMAT (//,2X,'INPUT ERROR, NS IS GREATER THAN 5, NS =',I4)
 3020 FORMAT (//,2X,'INPUT ERROR, E(',I1,') IS LESS THAN 100.0,
     *               E(',I1,') =',E10.4)
 3030 FORMAT (//,2X,'INPUT ERROR, E(',I1,') IS GREATER THAN 0.100E30,
     *              E(',I1,') =',2E10.4)
 3040 FORMAT (//,2X,'INPUT ERROR, VRAT(',I1,') IS LESS THAN 0.05',
     *' VRAT(',I1,') =',E10.4)
 3050 FORMAT (//,2X,'INPUT ERROR, VRAT(',I1,') IS GREATER THAN 0.50',
     *' VRAT(',I1,') =',E10.4)
 3060 FORMAT (//,2X,'INPUT ERROR, HH(',I1,') IS LESS THAN ZERO, HH(',
     *I1,') =',E10.4)
 3070 FORMAT (//,2X,'INPUT ERROR, HH(',I1,') IS GREATER THAN 1000, HH(',
     *I1,') =',E10.4)
C
C  END OF FORMAT STATEMENTS
C
      END SUBROUTINE SCERJ
      !*****************************************************************

      !*****************************************************************
      SUBROUTINE CALCUL
      !*****************************************************************
      DATA NPAGE /0/

      ITN  = 46 ! default value from original code
      ITN4 = ITN * 4

      N     = NS-1
      AR    = SQRT(PSI/PI)
      NLINE = 17 + NS
      NPAGE = NPAGE + 1

      IF(IOPT.EQ.1) WRITE(*,3000) ('****',I=1,5), (TITLE),
     *                            ('****',I=1,4), NPAGE

      IF(IOPT.EQ.1) WRITE(*,3010) WGT, PSI, AR,
     *                             (I, E(I), VRAT(I), HH(I), I=1,N)

      IF(IOPT.EQ.1) THEN
        WRITE(*,3020) NS, E(NS), VRAT(NS)
        WRITE(*,3030) ('....', I=1,27)
      END IF

      ! Adjust Layer Depths
      H(1) = HH(1)
      DO I=2,N
        H(I) = H(I-1) + HH(I)
      END DO


      IF(NEWFLAG.NE.1) THEN

        ! Define an array whose elements are equal to the
        ! crib radius numbers
        III = 1
        DO KRADNO = 2,101,11
            KTEMP = KRADNO
            KSTOP = KTEMP + 8
            DO LL = KTEMP,KSTOP,2
                CHECKR(III) = LL
                III = III + 1
            END DO
        END DO
      ELSE
        IR = 60
        DO INEW = 1,6
            Y_CALCUL = (INEW-1) * TIESP
            DO JNEW = 1,10
                X_CALCUL = OFFSET-(TIEL/2.)+((JNEW-1)*SEGL)+(SEGL/2.)
                KNEW = (INEW-1) * 10 + JNEW
                RR(KNEW) = SQRT(X_CALCUL**2+Y_CALCUL**2)
                ANGNEW(KNEW) = ATAN(Y_CALCUL/X_CALCUL)
            END DO
        END DO
      END IF

      IRT = 0

      ! Start on a new R

   70 IRT = IRT + 1

      IF(NEWFLAG.EQ.1) GOTO 120

      ! Check to see if the radius being solved for corresponds to a
      ! crib radius number. If it does, do not solve for the
      ! coefficients and write zeros for the crib area coefficients.

      DO LL = 1,50
        ISPOT = CHECKR(LL)
        IF(IRT.EQ.ISPOT) GOTO 90
      END DO

      GOTO 120

   90 CSZ = 0.
      CST = 0.
      CTR = 0.
      CSR = 0.
      COM = 0.

      DO III = 1,IZ
        WRITE(1,5000) CSZ, CST, CTR, CSR, COM
      END DO

      GOTO 70

  120 KZ = 0

      IF (IRT-IR) 1050,1050,1010

 1050 R = RR(IRT)

      DO I = 1,IZ
        DO J = 1,N
            TZ = ABS(H(J)-ZZ(I))
            IF((TZ-.0001).LE.0) ZZ(I) = -H(J)
        END DO
      END DO

      NLINE = NLINE + 1

      ! Calculate the partition
      CALL PART(AR, AZ, R, BZ, ITN, NTEST, SF, K)

      ! Calculate the coefficients
      DO I = 1,ITN4
        P = AZ(I)
        CALL COEE(I,E,VRAT,PM,NS,H,N,P)

        IF(R.GT.0) THEN
            PR = P * R
            CALL BESSEL(0,PR,Y)

            RJ0(I) = Y
            CALL BESSEL(1,PR,Y)

            RJ1(I) = Y
        END IF

        PA = P * AR
        CALL BESSEL(1,PA,Y)
        AJ(I) = Y
      END DO

      IZT = 0

      ! Start on a new Z
  200 IZT = IZT + 1
      KZ  = KZ + 1

      IF((IZT-IZ).GT.0) GOTO 70

      Z = ABS(ZZ(IZT))
      IF((NLINE-54).GE.0) THEN
        NPAGE = NPAGE + 1
        NLINE = 8
        IF(IOPT.EQ.1) WRITE(3,3000) ('****',I=1,5), (TITLE),
     *                              ('****',I=1,4), NPAGE
        IF(IOPT.EQ.1) WRITE(3,3030) ('....',J=1,27)
      END IF

      ! Find the layer containing Z
      TZZ = 0.0

      DO J1 = 1,N
        J = NS - J1
        IF ((Z-H(J)).GE.0) GOTO 215
      END DO

      L = 1
      GOTO 34

  215 L = J + 1
      IF ((ZZ(IZT)).LE.0) THEN
        L   = J
        TZZ = 1.0
      END IF

   34 CONTINUE

      CALL CALCIN

      IF ((TZZ).GT.0) THEN
        ZZ(IZT) = -ZZ(IZT)
        IZT=IZT-1
      END IF

      !!!!!!!!!!!! ******************* !!!!!!!!!!!!!!
      ! MANUAL ENTRY OF NPUN VALUE
      NPUN = 0
      IF (NPUN.GT.0) THEN
        NZP = IZT - 1
        NRP = IRT - 1
      END IF

      GO TO 200

 1010 CONTINUE


      ! AFTER ALL THE COEFFICIENTS HAVE BEEN CALCULATED, READ THE
      ! COEFFICIENT MATRIX AND PRINT THE COEFFICIENT MATRIX
      REWIND 1
      READ(1,5000) (((COEF(IRR,ID,L),L=1,5),ID=1,IZ),IRR=1,IR)

      IF(IOPT.NE.0) THEN
          WRITE(*,3040)
          DO IRR = 1,IR
            DO ID = 1,IZ
                WRITE(*,3050) IRR,ID,(COEF(IRR,ID,L),L=1,5)
            END DO
          END DO
      END IF

      ! IF(LACOPT.GT.0) CALL LAC

      RETURN

      !---------------------FORMAT STATEMENTS---------------------------
 3000 FORMAT(//,5A4,1X,A60,1X,4A4,'  PAGE ',I2,/)
 3010 FORMAT (/,40X,'THE PROBLEM PARAMETERS ARE',/,
     *        20X,'TOTAL LOAD......',5X,F10.2,'  LBS',/,
     *        20X,'TIRE PRESSURE...',5X,F10.2,'  PSI',/,
     *        20X,'LOAD RADIUS.....',5X,F10.2,'  IN.',//,
     *        20X,'LAYER ',I3,'  HAS MODULUS ',E10.3,
     *        '  POISSONS RATIO ',F5.3,'  AND THICKNESS ',F6.2,' IN.')

 3020 FORMAT(20X,'LAYER ',I3,'  HAS MODULUS ', E10.3,'  POISSONS RATIO '
     *,F5.3,'  AND IS SEMI-INFINITE.')

 3030 FORMAT (/,34X,'STRESSES',
     *        26X,'DISPLACEMENT',
     *        15X,'STRAINS',
     *        /, 15A4, 3X, 3A4, 3X, 9A4/'   R', 5X,'Z',
     *        6X, 'VERTICAL',
     *        3X, 'TANGENTIAL',
     *        3X, 'RADIAL',
     *        6X, 'SHEAR',
     *        7X, 'BULK',
     *        11X,'VERTICAL',
     *        7X, 'RADIAL',
     *        6X, 'SHEAR',
     *        7X, 'BULK')

 3040 FORMAT(3X,'CALCULATED',2X,'CALCULATED',/,
     *       5X,'RADIUS',6X,'DEPTH',6X,'SIGMA',8X,'SIGMA',
     *       9X,'TAU',8X,'SIGMA',6X,'VERTICAL',/,
     *       6X,'NO.',9X,'NO.',9X,'Z',10X,'THETA',7X,
     *       'R-THETA',8X,'R',10X,'DISP.'//)

 3050 FORMAT (6X,I3,10X,I1,6X,1PE10.3,4(3X,1PE10.3))

 5000 FORMAT(5E12.5)

      END SUBROUTINE CALCUL
      !*****************************************************************

      !*****************************************************************
      SUBROUTINE PART(AR, AZ, R, BZ, ITN, NTEST, SF, K)
      !*****************************************************************

      DIMENSION BZ(100), AZ(396)
      INTEGER   ITN, K, NTEST
      REAL      R, SF, AR

      IF (BZ(2).LE.0) THEN

        ! COMPUTE ZEROS OF J1(X) AND JO(X). SET UP GAUSS CONSTANTS
        BZ(1) = 0.0
        BZ(2) = 1.0
        BZ(3) = 2.4048
        BZ(4) = 3.8317
        BZ(5) = 5.5201
        BZ(6) = 7.0156

        K = ITN + 1

        DO I = 7,K,2
            T  = I / 2
            TD = 4.0 * T - 1.0
            BZ(I) = PI * (T - 0.25 + 0.050661/TD - 0.053041/TD**3 +
     *              0.262051/TD**5)
        END DO

        DO I=8,ITN,2
            T  = (I-2)/2
            TD = 4.0 * T + 1.0
            BZ(I) = PI * (T + 0.25 - 0.151982/TD + 0.015399/TD**3 -
     *              0.245270/TD**5)
        END DO
      END IF

      !!! WE MOVED THE 4 SO THAT G1 AND G2 HAVE THE VALUES EVERYTIME!!
      G1 = 0.86113631E0
      G2 = 0.33998104E0

      ZF    = AR
      NTEST = 2

      IF(R.GT.0) THEN
        NTEST = INT(AR/R + .0001)
        IF (NTEST.LE.0) THEN
            NTEST = INT(R/AR + 0.0001)
            ZF = R
        END IF
        NTEST = NTEST + 1
        IF((NTEST-10).GT.0) NTEST = 10
      END IF

      ! COMPUTE POINTS FOR LEGENDRE-GAUSS INTEGRATION
      K   = 1
      ZF  = 2.0 * ZF
      SZ2 = 0.0

      DO I = 1,ITN
        SZ1 = SZ2
        SZ2 = BZ(I+1)/ZF
        SF  = SZ2 - SZ1
        PP  = SZ2 + SZ1
        SG1 = SF * G1
        SG2 = SF * G2

        AZ(K)   = PP - SG1
        AZ(K+1) = PP - SG2
        AZ(K+2) = PP + SG2
        AZ(K+3) = PP + SG1

        K = K + 4
      END DO

      END SUBROUTINE PART
      !*****************************************************************


      !*****************************************************************
      SUBROUTINE COEE(KIN,E,VRAT,PM,NS,H,N,P)
      !*****************************************************************
      DOUBLE PRECISION CTR
      COMMON /ONE/A(396,5),B(396,5),C(396,5),D(396,5),T1,T2,T1M,AJ(396),
     *        L,NLINE,RJ0(396),RJ1(396),TZZ,Z,COM,CMU,CSR,CST,CSZ,CTR

      DIMENSION E(5), VRAT(5), H(6), PM(6,4,4)
      INTEGER   NS, N
      REAL      P

      DIMENSION X(7,4,4)
      DIMENSION FM(4)
      DIMENSION SC(6)

      ! TOLERANCES ARE SET TO 1.0E-35 FOR NORMAL USAGE.
      ! TRUNV IS THE VALUE A VARIBLE IS TO ASSUME IF UNDERFLOW OCCURS
      ! SI IS THE SIGN OF A GIVEN VARIABLE.

      ETOL  = -100.0
      TOL   = EXP(ETOL)
      TRUNV = 1.0E-35
      LC    = KIN

      ! S-MX SET UP MATRIX X=DI*MI*KI*K*M*D
      ! COMPUTE THE MATRICES X(K)

      IF ((LC-1).LE.0) ISW = 1

      IF (ISW.LT.0) GOTO 99

      DO K = 1,N
        T1   = E(K) * (1.0+VRAT(K+1)) / (E(K+1)*(1.0+VRAT(K)))
        T1M  = T1 - 1.0
        PH   = P * H(K)
        PH2  = PH * 2.0
        VK2  = 2.0 * VRAT(K)
        VKP2 = 2.0 * VRAT(K+1)
        VK4  = 2.0 * VK2
        VKP4 = 2.0 * VKP2
        VKK8 = 8.0 * VRAT(K) * VRAT(K+1)

        X(K,1,1) = VK4 - 3.0 - T1
        X(K,2,1) = 0.0
        X(K,3,1) = T1M * (PH2 - VK4 + 1.0)
        X(K,4,1) = -2.0 * T1M * P

        T3 = PH2 * (VK2 - 1.0)
        T4 = VKK8 + 1.0 - 3.0 * VKP2
        T5 = PH2 * (VKP2 - 1.0)
        T6 = VKK8 + 1.0 - 3.0 * VK2

        X(K,1,2) = (T3 + T4 - T1 * (T5 + T6)) / P
        X(K,2,2) = T1 * (VKP4 - 3.0) - 1.0
        X(K,4,2) = T1M * (1.0 - PH2 - VKP4)
        X(K,3,4) = (T3 - T4 - T1 * (T5-T6)) / P

        T3 = PH2 * PH - VKK8 + 1.0
        T4 = PH2 * (VK2 - VKP2)

        X(K,1,4) = (T3 + T4 + VKP2 - T1 * (T3+T4+VK2)) / P
        X(K,3,2) = (-T3 + T4 - VKP2 + T1 * (T3-T4+VK2)) / P
        X(K,1,3) = T1M * (1.0 - PH2 - VK4)
        X(K,2,3) = 2.0 * T1M * P
        X(K,3,3) = VK4 - 3.0 - T1
        X(K,4,3) = 0.0
        X(K,2,4) = T1M * (PH2 - VKP4 + 1.0)
        X(K,4,4) = T1 * (VKP4 - 3.0) - 1.0
      END DO


      ! COMPUTE THE PRODUCT MATRICES PM
      SC(N) = 4.0 * (VRAT(N) - 1.0)

      IF ((N-2).GE.0) THEN
        DO K1 = 2,N
            M = NS - K1
            SC(M) = SC(M+1) * 4.0 * (VRAT(M)-1.0)
        END DO
      END IF

      DO K1 = 1,N
        K = NS-K1
        IF ((K-N).GE.0) THEN
            DO M = 1,4
                DO J = 1,4
                    PM(K,M,J)=X(K,M,J)
                END DO
            END DO
        ELSE
            TEXP = P * (H(K) - H(K+1))
            IF(TEXP.LT.ETOL) THEN
                TEXP = ETOL
                WRITE(*,210) TEXP
            END IF

            T1 = EXP(TEXP)
            T2 = 1.0 / T1
            DO M = 1,4
                DO J = 1,4
                    PM(K,M,J) = 0.0
                END DO
            END DO

            DO M = 1,4
                DO J = 1,4
                    IF(PM(K+1,1,J).NE.0.0) THEN
                        SI=PM(K+1,1,J)/ABS(PM(K+1,1,J))
                        IF(ABS(PM(K+1,1,J)).LT.TOL) THEN
                            PM(K+1,1,J) = TRUNV * SI
                            WRITE(*,211) PM(K+1,1,J)
                        END IF

                    END IF

                    IF(PM(K+1,2,J).NE.0.0) THEN
                        SI = PM(K+1,2,J) / ABS(PM(K+1,2,J))
                        IF(ABS(PM(K+1,2,J)).LT.TOL) THEN
                            PM(K+1,2,J) = TRUNV * SI
                            WRITE(*,211) PM(K+1,2,J)
                        END IF
                    END IF

                    IF(PM(K+1,3,J).NE.0.0) THEN
                        SI = PM(K+1,3,J) / ABS(PM(K+1,3,J))
                        IF(ABS(PM(K+1,3,J)).LT.TOL) THEN
                             PM(K+1,3,J) = TRUNV * SI
                             WRITE(3,211) PM(K+1,3,J)
                        END IF
                    END IF

                    IF(PM(K+1,4,J).NE.0.0) THEN
                        SI = PM(K+1,4,J) / ABS(PM(K+1,4,J))
                        IF(ABS(PM(K+1,4,J)).LT.TOL) THEN
                            PM(K+1,4,J) = TRUNV*SI
                            WRITE(*,211) PM(K+1,4,J)
                        END IF
                    END IF

                    T6 = (X(K,M,1) * PM(K+1,1,J) + X(K,M,2) *
     *                   PM(K+1,2,J)) * T1 + (X(K,M,3) * PM(K+1,3,J)
     *                   + X(K,M,4) * PM(K+1,4,J)) * T2

                    IF ((ABS(T6)-1.0E35).GT.0) GOTO 99

                    PM(K,M,J)=T6

                END DO
            END DO
        END IF
      END DO

      DO K1 = 1,N
        EHNP = -P * (H(N) + H(K1))

        IF(EHNP.LT.ETOL) EHNP = ETOL
        T1   = EXP(EHNP)

        EHNM = -P * (H(N) - H(K1))
        IF(EHNM.LT.ETOL) EHNM = ETOL
        T2 = EXP(EHNM)

        DO M = 1,2
            DO J = 3,4
                IF(PM(K1,M,J).NE.0.0) THEN
                    SI = PM(K1,M,J) / ABS(PM(K1,M,J))
                    IF(ABS(PM(K1,M,J)).LT.TOL) THEN
                        PM(K1,M,J) = TRUNV * SI
                        WRITE(*,201) PM(K1,M,J)
                    END IF
                END IF

                PM(K1,M,J) = T1 * PM(K1,M,J)
                IF(PM(K1,M+2,J).NE.0.0) THEN
                    SI = PM(K1,M+2,J) / ABS(PM(K1,M+2,J))
                    IF(ABS(PM(K1,M+2,J)).LT.TOL) THEN
                        PM(K1,M+2,J) = TRUNV * SI
                        WRITE(*,200) PM(K1,M+2,J)
                    END IF
                END IF

                PM(K1,M+2,J) = T2 * PM(K1,M+2,J)
            END DO
          END DO
      END DO

      ! SOLVE FOR C(NS) AND D(NS)

      T3 = 2.0 * VRAT(1)
      T4 = T3 - 1.0

      FM(1) = P*PM(1,1,3)+T3*PM(1,2,3)+P*PM(1,3,3)-T3*PM(1,4,3)
      FM(2) = P*PM(1,1,3)+T4*PM(1,2,3)-P*PM(1,3,3)+T4*PM(1,4,3)
      FM(3) = P*PM(1,1,4)+T3*PM(1,2,4)+P*PM(1,3,4)-T3*PM(1,4,4)
      FM(4) = P*PM(1,1,4)+T4*PM(1,2,4)-P*PM(1,3,4)+T4*PM(1,4,4)

      DFAC  = SC(1)/((FM(1)*FM(4)-FM(3)*FM(2))*P*P)

      A(LC,NS) = 0.0
      B(LC,NS) = 0.0
      C(LC,NS) = -FM(3) * DFAC
      D(LC,NS) = FM(1) * DFAC

      ! BACKSOLVE FOR THE OTHER A,B,C,D
      DO K1 = 1,N
        IF(PM(K1,1,3).NE.0.0) THEN
            SI = PM(K1,1,3) / ABS(PM(K1,1,3))
            IF(ABS(PM(K1,1,3)).LT.TOL) THEN
                PM(K1,1,3) = TRUNV * SI
                WRITE(*,202) PM(K1,1,3)
            END IF
        END IF

        IF(PM(K1,1,4).NE.0.0) THEN
            SI = PM(K1,1,4) / ABS(PM(K1,1,4))
            IF(ABS(PM(K1,1,4)).LT.TOL) THEN
                PM(K1,1,4) = TRUNV * SI
                WRITE(*,203) PM(K1,1,4)
            END IF
        END IF

        A(LC,K1) = (PM(K1,1,3)*C(LC,NS)+PM(K1,1,4)*D(LC,NS))/SC(K1)

        IF(PM(K1,2,3).NE.0.0) THEN
            SI = PM(K1,2,3) / ABS(PM(K1,2,3))
            IF(ABS(PM(K1,2,3)).LT.TOL) THEN
                PM(K1,2,3)=TRUNV*SI
                WRITE(*,204) PM(K1,2,3)
            END IF
        END IF

        IF(PM(K1,2,4).NE.0.0) THEN
            SI = PM(K1,2,4) / ABS(PM(K1,2,4))
            IF(ABS(PM(K1,2,4)).LT.TOL) THEN
                PM(K1,2,4)=TRUNV*SI
                WRITE(*,205) PM(K1,2,4)
            END IF
        END IF

        B(LC,K1) = (PM(K1,2,3)*C(LC,NS)+PM(K1,2,4)*D(LC,NS))/SC(K1)
        IF(PM(K1,3,3).NE.0.0) THEN
            SI = PM(K1,3,3) / ABS(PM(K1,3,3))
            IF(ABS(PM(K1,3,3)).LT.TOL) THEN
                PM(K1,3,3) = TRUNV * SI
                WRITE(*,206) PM(K1,3,3)
            END IF
        END IF

        IF(PM(K1,3,4).NE.0.0) THEN
            SI = PM(K1,3,4) / ABS(PM(K1,3,4))
            IF(ABS(PM(K1,3,4)).LT.TOL) THEN
                PM(K1,3,4) = TRUNV * SI
                WRITE(*,207) PM(K1,3,4)
            END IF
         END IF

        C(LC,K1) = (PM(K1,3,3)*C(LC,NS)+PM(K1,3,4)*D(LC,NS))/SC(K1)
        IF(PM(K1,4,3).NE.0.0) THEN
            SI = PM(K1,4,3) / ABS(PM(K1,4,3))
            IF(ABS(PM(K1,4,3)).LT.TOL) THEN
                PM(K1,4,3) = TRUNV * SI
                WRITE(*,208) PM(K1,4,3)
            END IF
        END IF

        IF(PM(K1,4,4).NE.0.0) THEN
            SI = PM(K1,4,4) / ABS(PM(K1,4,4))
            IF(ABS(PM(K1,4,4)).LT.TOL) THEN
                PM(K1,4,4) = TRUNV * SI
                WRITE(*,209) PM(K1,4,4)

            END IF
        END IF

        D(LC,K1)=(PM(K1,4,3)*C(LC,NS)+PM(K1,4,4)*D(LC,NS))/SC(K1)
      END DO

      RETURN

   99 ISW = -1

      DO K1 = 1,N
        A(LC,K1) = 0.0
        B(LC,K1) = 0.0
        C(LC,K1) = 0.0
        D(LC,K1) = 0.0
      END DO

      RETURN

  200 FORMAT(1X,'PM(K1,M+2,J)=',E12.5)
  201 FORMAT(1X,'PM(K1,M,J)=',E12.5)
  202 FORMAT(1X,'PM(K1,1,3)=',E12.5)
  203 FORMAT(1X,'PM(K1,1,4)=',E12.5)
  204 FORMAT(1X,'PM(K1,2,3)=',E12.5)
  205 FORMAT(1X,'PM(K1,2,4)=',E12.5)
  206 FORMAT(1X,'PM(K1,3,3)=',E12.5)
  207 FORMAT(1X,'PM(K1,3,4)=',E12.5)
  208 FORMAT(1X,'PM(K1,4,3)=',E12.5)
  209 FORMAT(1X,'PM(K1,4,4)=',E12.5)
  210 FORMAT(1X,'TEXP=',E12.5)
  211 FORMAT(1X,'PM(K+1,1,J)=',E12.5)

      END SUBROUTINE COEE
      !*****************************************************************

      !*****************************************************************
      SUBROUTINE CALCIN
      !*****************************************************************
      DOUBLE PRECISION CTR
      COMMON /ONE/ A(396,5),B(396,5),C(396,5),D(396,5),T1,T2,T1M,
     * AJ(396),L,NLINE,RJ0(396),RJ1(396),TZZ,Z,COM,CMU,CSR,CST,CSZ,CTR

      DIMENSION TEST(11), W(4)
      DATA W(1)/0.0/

      IF (W(1).LE.0) THEN
        W(1) = 0.34785485
        W(2) = 0.65214515
        W(3) = W(2)
        W(4) = W(1)
      END IF

      VL  = 2.0 * VRAT(L)
      EL  = (1.0 + VRAT(L)) / E(L)
      VL1 = 1.0 - VL

      CSZ = 0.0
      CST = 0.0
      CSR = 0.0
      CTR = 0.0
      COM = 0.0
      CMU = 0.0

      NTS1 = NTEST + 1
      ITS  = 1
      JT   = 0
      ARP  = AR

      IF (NOUTP.LE.0) ARP = ARP * PSI

      DO I = 1,ITN

      ! INITIALIZE THE SUB-INTEGRALS
        RSZ = 0.0
        RST = 0.0
        RSR = 0.0
        RTR = 0.0
        ROM = 0.0
        RMU = 0.0

      ! COMPUTE THE SUB-INTEGRALS
        K = 4 * (I-1)
        DO J=1,4
          J1 = K + J
          P  = AZ(J1)
      !------------
      ! MODIFIED TO ACCOUNT FOR MACHINE LIMITATIONS.  IF P*Z > 87.
      ! THEN EXP(P*Z) > E38 AND ARITHMETIC OVERFLOW WILL OCCUR
      ! OLD EXPRESSION FOR EP WAS:  EP=EXP(P*Z)
      !-------------
          PZEXP = P * Z

          IF(PZEXP.GT.87.)  PZEXP = 87.
          IF(PZEXP.LT.-87.) PZEXP = -87.

          EP  = EXP(PZEXP)
          T1  = B(J1,L)*EP
          T2  = D(J1,L)/EP
          T1P = T1 + T2
          T1M = T1 - T2

          T1  = (A(J1,L) + B(J1,L)*Z) * EP
          T2  = (C(J1,L) + D(J1,L)*Z) / EP
          T2P = P * (T1+T2)
          T2M = P * (T1-T2)
          WA  = AJ(J1) * W(J)

          IF(R.GT.0) THEN
              BJ1 = RJ1(J1) * P
              BJ0 = RJ0(J1) * P
              RSZ = RSZ + WA * P * BJ0 * (VL1 * T1P - T2M)
              ROM = ROM + WA * EL * BJ0 * (2.0 * VL1 * T1M - T2P)
              RTR = RTR + WA * P * BJ1 * (VL * T1M + T2P)
              RMU = RMU + WA * EL * BJ1 * (T1P + T2M)
              RSR = RSR+WA*(P*BJ0*((1.0+VL)*T1P+T2M)-BJ1*(T1P+T2M)/R)
              RST = RST+WA*(VL*P*BJ0*T1P+BJ1*(T1P+T2M)/R)
          ELSE
          ! SPECIAL ROUTINE FOR R = ZERO
              PP = P * P
              RSZ = RSZ + WA * PP * (VL1 * T1P - T2M)
              ROM = ROM + WA * EL * P * (2.0 * VL1 * T1M - T2P)
              RST = RST + WA * PP * ((VL + 0.5) * T1P + 0.5 * T2M)
              RSR = RST
          END IF
        END DO
        SF  = (AZ(K+4) - AZ(K+1)) / 1.7222726
        CSZ = CSZ + RSZ * SF
        CST = CST + RST * SF
        CSR = CSR + RSR * SF
        CTR = CTR + RTR * SF
        COM = COM + ROM * SF
        CMU = CMU + RMU * SF

        RSZ   = 2.0 * RSZ * AR * SF
        TESTH = ABS(RSZ) - 10.0**(-4)

        IF((ITS -NTS1).LT.0) THEN
            TEST(ITS) = TESTH
            ITS = ITS + 1
        ELSE
            TEST(NTS1) = TESTH
            DO J = 1,NTEST
                IF ((TESTH-TEST(J)).LT.0) TESTH=TEST(J)
                TEST(J) = TEST(J+1)
            END DO
            IF (TESTH.LE.0) GOTO 50
        END IF
      END DO
      JT = 1

      !-------
      ! CSZ = VERTICAL STRESS
      ! CST = TANGENTIAL STRESS
      ! CTR = SHEAR STRESS IN R-Z DIRECTION
      ! CSR = RADIAL STRESS
      ! COM = VERTICAL DEFLECTION
      !-------
   50 CSZ = CSZ * ARP
      CST = CST * ARP
      CTR = CTR * ARP
      CSR = CSR * ARP
      COM = COM * ARP

      !-------
      ! WRITE THE INFLUENCE COEFFICIENTS FOR THE PARTICULAR RADIUS
      ! AND DEPTH ONTO TAPE 1 (SCRATCH FILE)
      !-------
      WRITE(1,511) CSZ, CST, CTR, CSR, COM
  511 FORMAT (5E12.5)


      CMU  = CMU * ARP
      BSTS = CSZ + CST + CSR
      BST  = BSTS * (1.0 - 2.0 * VRAT(L)) / E(L)

      IF(TZZ.GT.0) Z=-Z

      RDS = (CSR - VRAT(L) * (CSZ + CST)) / E(L)
      SST = REAL(2.0 * (1.0 + VRAT(L)) * CTR / E(L))

      IF(IOPT.EQ.1) THEN
        WRITE(*,315) R, Z, CSZ, CST, CSR, CTR,BSTS,COM,RDS,SST,BST
      END IF

      NLINE = NLINE + 1

      IF(IOPT.EQ.0) RETURN

      IF(JT.GT.0) WRITE(*,316)

  315 FORMAT(F5.1,F6.1,1X,1P5E12.3,3H * ,E12.3,3H * ,3E12.3)
  316 FORMAT(20X,'SLOW TO CONVERGE')

      RETURN

      END SUBROUTINE CALCIN
      !*****************************************************************
      SUBROUTINE LAC
      !*****************************************************************

        DIMENSION X(15,15),T(15,15),R(15,15)
        DIMENSION BP(11,10)
        DIMENSION XL(10),F(10)
        DIMENSION XCOR(30)
        DIMENSION RAD(110)
        DIMENSION YIMOD (3,6)
        DIMENSION S(50,50)
        DIMENSION V(50)
        DIMENSION TT(15),RT(15),RM(15),RP(15),RN(15)
        DIMENSION YI(4,20)
        DIMENSION RO(6)
        DIMENSION DS(9)
        DIMENSION EIGVEC(3,3)
        DIMENSION WK(3)
        DIMENSION LM(17),TCM(17),RSM(17)
        DIMENSION TM(17,5)
        DIMENSION SM(17,3)
        DOUBLE PRECISION SX1,SX1SYM,SY1,SY1SYM,SZ1,SZ1SYM,
     *    SXY,SXYSYM,SXZ,SXZSYM,SYZ,SYZSYM
        INTEGER NCNTCT
      MDIM = 50
      NDIM = 15

      IF(NSTEPS.LE.1) THEN
         QR  = 0
         QL  = 0
         QXR = 0
         QXL = 0

         PL  = PR
         PXR = 5.0 * TIESP
         PXL = PXR
         MO  = 10 - MI
         WO  = TIEL - WI
         TS  = TIESP
         M   = NSEG
         N   = NTIES * 2 - 1
         ND  = 4
         NE  = 6
         ZM  = 5.0 * TIESP
         NZ  = 11
      END IF

      M = MO + MI
      ! SINCE SYMMETRY IN THE TIES,
      ! DIVIDE THE VARIABLE DIMENSIONS BY TWO
      M  = M / 2
      MO = MO / 2
      MI = MI / 2
      WI = WI / 2
      WO = WO / 2
      N  = N / 2 + 1

      MN  = M * N
      NTE = MN + 3 * N
      KK  = MN - M
      NN  = N - 1
      CO  = WO / MO
      CI  = WI / MI
      WL  = WI * 2.

      IF((NEWFLAG.EQ.1).OR.(NSTEPS.GT.NITER+1)) GO TO 380

      ! DEFINE THE TIE FLEXIBILITY COEFS.
      ! USING THE REVISED STIE SUBROUTINE.
      ! CALL STIE(M,WL,TIEL,NSEG,TI,TE,T,NDIM)


      ! SET UP THE SOIL COEFF. SYMMETRIC MATRIX

      ITMARK = 0
   20 DO I = 1,NDIM
        RM(I) = 0.0
        RP(I) = 0.0
        DO J = 1,NDIM
            R(I,J) = 0.0
            X(I,J) = 0.0
        END DO
      END DO

      DO I = 1,MDIM
        V(I) = 0.
        DO J = 1,MDIM
            S(I,J) = 0.
        END DO
      END DO

      ! CALL SOIL(M,N,MN,S,MDIM,IZ)

      DO LL = 1,N
        DO I = 1,M
            DO J = 1,M
                L = LL - 1
                S(L*M+I,L*M+J) = S(L*M+I,L*M+J) - T(I,J)
            END DO
        END DO
      END DO


      ! TRY TO ELIMIMINATE THE CONTACT FORCE AT THE CENTER TIE SEGMENTS
      ! FOR THE FIRST TWO TIES
      S(5,5)   = S(5,5)   * 1.0E20
      S(10,10) = S(10,10) * 1.0E20

      V(5)     = -1. * S(5,5)   * 30.
      V(10)    = -1. * S(10,10) * 30.

      IF((NOTENS.EQ.0).OR.(NSTEPS.LT.NITER+1)) GO TO 70

      ITMARK = ITMARK + 1

      ! IF ANY NET TIE/SOIL REACTIONS WERE NEGATIVE (TENSILE) SET
      ! THOSE DIAGONAL ELEMENTS OF THE TIE/SOIL FLEXIBILITY MATRIX TO
      ! A VERY LARGE VALUE AND SET THE CORRESPONDING "UNKNOWN" VECTOR
      ! ELEMENT EQUAL TO THE STATIC TIE SEGMENT WEIGHT TIMES THE
      ! MODIFIED TIE/SOIL FLEXIBILITY COEFFICIENT. THIS FORCES
      ! THAT SOLUTION TO THE STATIC TIE SEGMENT WEIGHT, IN THE "NOTENS"
      ! OPTION.

      DO I = 1,NUMCON
        J      = NCNTCT(I)
        S(J,J) = S(J,J) * 1.0E20
        V(J)   = S(J,J) * XCOR(J)
      END DO

   70 CONTINUE

      DO J = 1,N
        DO I = 1,M
            K = J - 1
            S(K*M+I,MN+2*N+J) = -1.0
        END DO
      END DO


      ! SPRING FORCE EQULIBRIUM EQUATIONS

      DO I = 1,N
        S(MN+2*N+I,MN+I)     = -1.0
        S(MN+2*N+I,MN+N+I)   = -1.0 / STIF1
        S(MN+2*N+I,MN+2*N+I) = 1.0
      END DO

      ! SET UP THE TIE COEFF.
      DO I = 1,N
        S(MN+I,MN+N+I) = -1.0
      END DO

      DO I = 1,N
        DO J = 1,M
            K = I - 1
            S(MN+I,K*M+J) = 1.
         END DO
      END DO


      ! LOAD IN RAIL COEFF.

      ! CALL SRAIL(NN,TS,TOL,RE,RI,R,NDIM)


      DO I = 1,NN
        S(MN+N+I,MN+1)   = -1.0
        S(MN+N+I,MN+I+1) = 1.0
        DO J = 1,NN
            S(MN+N+I,MN+N+1+J) = R(I,J)
        END DO
      END DO

      DO I = 1,NN
        S(MN+2*N,MN+N+I) = 1.
      END DO

      DO I = 1,NN
        V(MN+N+I) = PR*(I*TS) * (3.*TOL*TOL-4.*(I*TS)**2) /
     *              (48.*RE*RI)*(-1.)
      END DO

      V(MN+N+N)        = PR / 2.0
      S(MN+2*N,MN+2*N) = 0.5


      ! SOLVE THE SIMULTANEOUS EQUATIONS (50 X 50 MATRIX)

      ! CALL SLVEQ(S,V,NTE,MDIM)


      ! CALCULATE STRUCTURAL WEIGHTS

      TTWT = TWT / NSEG
      RRWT = RWT * TIESP / 36.
      SEGL = TIEL / NSEG
      HALF = SEGL / 2.
      W    = WO - HALF

      DO I = 1,10
        J = I - 1
        XL(I) = ABS(W-J*SEGL)
      END DO

      SUM = 0.
      DO I = 1,10
        SUM = SUM + (XL(1) / XL(I))
      END DO

      F(1) = RRWT / SUM
      DO I = 1,10
        F(I) = F(1) * (XL(1) / XL(I))
      END DO

      DO I = 1,5
        J = 11 - I
        F(I) = F(I) + F(J) + TTWT
      END DO


      J   = 0
      SUM = 0.0
      DO I=1,MN
        IF(I.LE.30) THEN
            K = 5
        ELSE IF(I.LE.25) THEN
            K = 4
        ELSE IF(I.LE.20) THEN
            K = 3
        ELSE IF(I.LE.15) THEN
            K = 2
        ELSE IF(I.LE.10) THEN
            K = 1
        ELSE IF(I.LE.5) THEN
            K = 0
        END IF

        K  = I - 5 * K
        FS = -1. * F(K)

        IF(V(I).LE.FS) THEN
            EXCESS = V(I) + F(K)
            SUM    = SUM + EXCESS
            J = J + 1
            NCNTCT(J) = I
            XCOR(J) = FS
        END IF
      END DO


      NUMCON = J

      IF(ITMARK.EQ.0) GO TO 220
      IF(ITMARK.EQ.1) WRITE(*,3000)

      WRITE(3,3010) (F(I),I=1,5),ITMARK,SUM

      IF( (NOTENS.EQ.0).OR.(NSTEPS.LT.NITER+1).OR.(ITMARK.GT.9)) THEN
        GO TO 220
      END IF

      CHECK = 0.0005 * PL

      IF(ABS(SUM).GT.CHECK) GO TO 20

  220 CONTINUE

      DO L = 1,5
        J    = 11 - I
        F(J) = F(L)
      END DO

      DO K = MINSEG,MAXSEG
        STA(K) = F(K) / (SEGL*TIEWD)
      END DO

      DO I = 1,N
        DO J = 1,M
            II      = (I-1) * M + J
            X(I,J)  = V(II)
            YI(1,I) = V(MN+I)
            YI(2,I) = V(MN+N+I)
            YI(3,I) = V(MN+2*N+I)
        END DO
      END DO


      IF(ITSTD.EQ.0) GO TO 380

      IF((NSTEPS.LT.NITER+1).AND.(ITOPT.EQ.0)) GO TO 270

      WRITE(*,3020)NSTEPS
      WRITE(*,3030)

      DO I = 1,N
        NMOD = 7 - I
        WRITE(*,3040) I,YI(1,NMOD),YI(2,NMOD),YI(3,NMOD)
      END DO


  270 TPRM = 0.0
      TL   = WI + WO
      D1VL = TL / M
      DIST = (WO - DIVL * MO)

      DO J = 1,N
        LM(J)  = YI(2,J) * TS * (N-J)
        TCM(J) = 0.0
        DO I = 1,M
            TM(J,I) = X(J,I) * (M+.5-I) * DIVL
            TCM(J)  = TCM(J) + TM(J,I)
        END DO

        TCM(J) = TCM(J) - YI(2,J) * WI
        RSM(J) = 0.0
        DO K=1,MO
            SM(J,K) = X(J,K) * ((MO+.5-K) * DIVL + DIST)
            RSM(J)  = RSM(J) + SM(J,K)
        END DO
        TPRM = TPRM + LM(J)
      END DO

      IF((NSTEPS.LT.NITER+1).AND.(ITOPT.EQ.0)) GO TO 370

      DO I = 1,6
        DO J = 1,5
            TDFL(I,J) = 0.
            DO K = 1,5
                TDFL(I,J) = TDFL(I,J) + X(I,K) * T(K,J)
            END DO
        END DO
      END DO

      DO I = 1,6
        DO J = 1,5
            TDFL(I,J) = TDFL(I,J) + YI(1,I) + (YI(2,I) / STIF1)
        END DO
      END DO

      WRITE(*,3050)

      DO J = 1,N
        JMOD = 7 - J
        WRITE(*,3060) J, TCM(JMOD), RSM(JMOD)
      END DO

      WRITE(*,3070)
      DO I = 1,N
        NMOD=7-I
        WRITE(*,3080) I, (X(NMOD,J),J=1,5)
      END DO

      WRITE(*,3090)

      DO J = 1,N
        JMOD = 7 - J
        WRITE(*,3100) J, (TDFL(JMOD,K),K=1,5)
      END DO
C
      WRITE(*,3110) TPRM

      U   = ((((PR/YI(1,6))**4)/(RE*RI))**(1./3.))/4000.
      USI = U * 6.894

      WRITE(*,3120) U, USI

  370 CONTINUE

      ! INITIALIZE THE RESPONSE ARRAYS

  380 CONTINUE

      DO III = 1,6
        DO JJJ = 1,5
            DO KKK = 1,5
                RESPW(III,JJJ,KKK)  = 0.
                RESPX(III,JJJ,KKK)  = 0.
                RESPY(III,JJJ,KKK)  = 0.
                RESPZ(III,JJJ,KKK)  = 0.
                RESPXY(III,JJJ,KKK) = 0.
                RESPXZ(III,JJJ,KKK) = 0.
                RESPYZ(III,JJJ,KKK) = 0.
            END DO
        END DO
      END DO

      ! DEFINE THE TIE/SOIL REACTIONS DUE TO TIE SYMMETRY.

      DO I = 1,6
        X(I,10) = X(I,1)
        X(I,9)  = X(I,2)
        X(I,8)  = X(I,3)
        X(I,7)  = X(I,4)
        X(I,6)  = X(I,5)
      END DO

      ! CHANGE THE TIE/SOIL REACTION NUMBERING SYSTEM SO THAT
      ! TIE NO. 1 IS NOW THE CENTER TIE.

      DO I = 1,6
        II = 7-1
        DO J=1,10
            BP(I,J) = X(II,J)
        END DO
      END DO

      ! SET THE TIE/SOIL REACTIONS ON THE 'DUMMY' TIES EQUAL TO ZERO.
      DO I = 7,11
        DO J = 1,10
            BP(I,J) = 0.
        END DO
      END DO

      PI   = 3.1415927
      SEGL = TIEL / NSEG

      IF(NEWFLAG.EQ.1) GO TO 460

      ! DEFINE AN ARRAY WHOSE ELEMENTS EQUAL THE RADIAL DISTANCES TO ALL
      ! TIE SEGMENTS FROM SEGMENT NO. 1 OF TIE NO. 1

      DO IX = 1,10
        XINCR  = (IX-1) * SEGL
        IRADNO = ((IX-1) * 11) + 1
        DO IY = 1,6
            YINCR       = (IY - 1) * TIESP
            RAD(IRADNO) = SQRT(XINCR**2 + YINCR**2)
            IRADNO      = IRADNO + 2
        END DO
      END DO

  460 IF(NEWFLAG.EQ.0) GO TO 560

      ! CALCULATE FOR THE OFFSET POSITIONS

      DO I = 1,5
        OFFW(I)  = 0.
        OFFX(I)  = 0.
        OFFY(I)  = 0.
        OFFZ(I)  = 0.
        OFFXY(I) = 0.
        OFFXZ(I) = 0.
        OFFYZ(I) = 0.
      END DO

      DO I = 1,6
        DO J = 1,10
            K = (I-1) * 10 + J
            DO KZ = 2,IZ
                KKZ = KZ - 1
                CSZ = COEF(K,KZ,1)
                CST = COEF(K,KZ,2)
                CTR = COEF(K,KZ,3)
                CSR = COEF(K,KZ,4)
                COM = COEF(K,KZ,5)
                IF(I.EQ.1) THEN
                    CSZSYM = 0.
                    CSTSYM = 0.
                    CTRSYM = 0.
                    CSRSYM = 0.
                    COMSYM = 0.
                ELSE
                    CSZSYM = CSZ
                    CSTSYM = CST
                    CTRSYM = CTR
                    CSRSYM = CSR
                    COMSYM = COM
                END IF
  500           IF(I.GT.1) THEN
                    THETA  = PI - ANGNEW(K)
                    THETAP = PI + ANGNEW(K)
                ELSE
                    THETA  = PI
                    THETAP = PI
                END IF

  520           SX1=CSR*COS(THETA)*COS(THETA)+CST*SIN(THETA)*SIN(THETA)
                SX1SYM = CSRSYM*COS(THETAP)*COS(THETAP)+
     *                   CSTSYM*SIN(THETAP)*SIN(THETAP)
                SY1=CST*COS(THETA)*COS(THETA)+CSR*SIN(THETA)*SIN(THETA)
                SY1SYM = CSTSYM*COS(THETAP)*COS(THETAP)+
     *                   CSRSYM*SIN(THETAP)*SIN(THETAP)
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
            END DO
        END DO
      END DO

      GO TO 1060

      ! BEGIN THE CALCULATION OF THE DISPLACEMENTS AND STRESS TENSORS.
  560 DO ITIE = 1,6
        YMAX = (6 - ITIE) * TIESP
        DO IP = 1,5
            DO J = 1,10

      ! DEFINE LOCAL COORDINATE ORIGINS AND RELATIVE ANGLES.
                XDIST = (J-IP) * SEGL
                DO K = 1,6
                    YDIST = (K-1) * TIESP
                    RTEMP=SQRT(XDIST**2+YDIST**2)
                    IF((XDIST.EQ.0.).AND.(YDIST.EQ.0.)) THEN
                        THETA = 0.
                        THETAP = 0.
                    ELSE IF(XDIST.EQ.0.) THEN
                        THETA = PI/2.
                        THETAP = 3 * PI/2.
                    ELSE IF((XDIST.LT.0.).AND.(YDIST.EQ.0.)) THEN
                        THETA = PI
                        THETAP = PI
                    ELSE IF((XDIST.GT.0.).AND.(YDIST.EQ.0.)) THEN
                        THETA = 0.
                        THETAP = 2*PI
                    ELSE
                        XABS=ABS (XDIST)
                        ANGLER=ATAN(YDIST/XABS)
                        IF (.NOT.(XDIST.GT.0.)) THEN
                            THETA=PI-ANGLER
                            THETAP=PI+ANGLER
                        ELSE
                            THETA = ANGLER
                            THETAP = 2*PI - ANGLER
                        END IF
                    END IF
      ! PICK THE PROPER RELATIVE INFLUENCE COEFFICIENT VECTOR.
                    DO I = 1, 110
                        RADIUS=RAD(I)
                        ILOC=I
                        IF(RADIUS.EQ.RTEMP) GO TO 640
                    END DO
  640               L=ILOC
  650               DO KZ=2,IZ
                        KKZ=KZ-1
                        CSZ=COEF(L,KZ,1)
                        CST=COEF(L,KZ,2)
                        CTR=COEF(L,KZ,3)
                        CSR=COEF(L,KZ,4)
                        COM=COEF(L,KZ,5)
                        IF(K.EQ.1) GO TO 660
                        GO TO 670

  660                   CSZSYM=0.
                        CSTSYM=0.
                        CTRSYM=0.
                        CSRSYM=0.
                        COMSYM=0.
                        GO TO 680

  670                   CSZSYM=CSZ
                        CSTSYM=CST
                        CTRSYM=CTR
                        CSRSYM=CSR
                        COMSYM=COM
  680                   IF(YDIST.GT.YMAX) GO TO 690
                        GO TO 700

  690                   CSZ=0.
                        CST=0.
                        CTR=0.
                        CSR=0.
                        COM=0.

      ! TRANSFORM FROM CYLINDRICAL TO CARTESIAN COORDINATES.

  700 SX1 = CSR * COS(THETA) * COS(THETA) + CST * SIN(THETA) *
     *      SIN(THETA)

      SX1SYM = CSRSYM * COS(THETAP) * COS(THETAP) + CSTSYM * SIN(THETAP)
     *         *SIN(THETAP)

      SY1 = CST * COS(THETA) * COS(THETA) + CSR * SIN(THETA) *
     *      SIN(THETA)

      SY1SYM = CSTSYM * COS(THETAP) * COS(THETAP) + CSRSYM * SIN(THETAP)
     *         *SIN(THETAP)

      SZ1 = CSZ
      SZ1SYM = CSZSYM
      SXY = (CSR-CST) * SIN(THETA) * COS(THETA)
      SXYSYM = (CSRSYM-CSTSYM) * SIN(THETAP) * COS(THETAP)
      SXZ = CTR * COS(THETA)
      SXZSYM = CTRSYM * COS(THETAP)
      SYZ = CTR * SIN(THETA)
      SYZSYM = CTRSYM * SIN(THETAP)
      SU1 = COM
      SU1SYM = COMSYM

      ! SUM UP THE INFLUENCE COEFFICIENTS TIMES THE PROPER TIE/SOIL REACTIONS.

  710 J1 = ITIE - K
      IF (J1.LT.0) J1 = -J1
      J1 = J1 + 1
      J2 = K + ITIE - 1
      RESPW(ITIE,IP,KKZ) = RESPW(ITIE,IP,KKZ) + SU1SYM * BP(J1,J) +
     *                     SU1*BP(J2,J)
      RESPX(ITIE,IP,KKZ) = RESPX(ITIE,IP,KKZ) + SX1SYM * BP(J1,J) +
     *                     SX1*BP(J2,J)
      RESPY(ITIE,IP,KKZ) = RESPY(ITIE,IP,KKZ) + SY1SYM * BP(J1,J) +
     *                     SY1*BP(J2,J)
      RESPZ(ITIE,IP,KKZ) = RESPZ(ITIE,IP,KKZ) + SZ1SYM * BP(J1,J) +
     *                     SZ1*BP(J2,J)
      RESPXY(ITIE,IP,KKZ) = RESPXY(ITIE,IP,KKZ) + SXYSYM * BP(J1,J) +
     *                      SXY*BP(J2,J)
      RESPXZ(ITIE,IP,KKZ) = RESPXZ(ITIE,IP,KKZ) + SXZSYM * BP(J1,J) +
     *                      SXZ*BP(J2,J)
      RESPYZ(ITIE,IP,KKZ) = RESPYZ(ITIE,IP,KKZ) + SYZSYM * BP(J1,J) +
     *                      SYZ*BP(J2,J)
      END DO
      END DO
      END DO
      END DO
      END DO


      !*****************************************************************
      END SUBROUTINE LAC
      !*****************************************************************

      end program geotrack_custom




      !*****************************************************************
      SUBROUTINE BESSEL(NI,XI,Y)
      !*****************************************************************

      DOUBLE PRECISION PZ(6), QZ(6), P1(6), Q1(6), D(20)

      DATA PZ/1.0D0,-1.125D-4,2.8710938D-7,-2.3449658D-9,
     *3.9806841D-11,-1.1536133D-12/

      DATA QZ/-5.0D-3,4.6875D-6,-2.3255859D-8,2.8307087D-10,
     *-6.3912096D-12,2.3124704D-13/

      DATA P1/1.0D0,1.875D-4,-3.6914063D-7,2.7713232D-9,
     *-4.5114421D-11,1.2750463D-12/

      DATA Q1/1.5D-2,-6.5625D-6,2.8423828D-8,-3.2662024D-10,
     *7.1431166D-12,-2.5327056D-13/

      DATA PI/3.1415927/

      N = NI
      X = XI

      IF ((X-7.0).GT.0) GOTO 160

      X2  = X / 2.0
      FAC = -X2 * X2

      IF(N.LE.0) THEN
        C = 1.0
        Y = C
        DO I = 1,34
            T = I
            C = FAC * C / (T*T)
            TEST = ABS(C) - 10.0**(-8)
            IF(TEST.LE.0) RETURN
            Y = Y + C
        END DO
      END IF

      C = X2
      Y = C
      DO I = 1,34
        T = I
        C = FAC * C / (T * (T+1.0))
        TEST = ABS(C) - 10.0**(-8)
        IF(TEST.LE.0) RETURN
        Y = Y + C
      END DO

      RETURN

  160 IF(N.LE.0) THEN
        DO I = 1,6
            D(I)    = PZ(I)
            D(I+10) = QZ(I)
        END DO
      ELSE
        DO I = 1,6
            D(I)    = P1(I)
            D(I+10) = Q1(I)
        END DO
      END IF

      T1 = 25.0 / X
      T2 = T1 * T1
      P  = REAL(D(6) * T 2+ D(5))

      DO I = 1,4
        J = 5 - I
        P = REAL(P * T2 + D(J))
      END DO

      Q = REAL(D(16) * T2 + D(15))
      DO I = 1,4
        J = 5 - I
        Q = REAL(Q * T2 + D(J+10))
      END DO

      Q  = Q * T1
      T4 = SQRT(X*PI)
      T6 = SIN(X)
      T7 = COS(X)

      IF(N.LE.0) THEN
        T5=((P-Q)*T6+(P+Q)*T7)/T4
      ELSE
        T5=((P+Q)*T6-(P-Q)*T7)/T4
      END IF

      Y = T5

      RETURN

      END SUBROUTINE BESSEL
      !*****************************************************************

