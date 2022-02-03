C     PROGRAM NO.: F-4
C     FORTRAN IV PROGRAM TO CALCULATE CANADIAN FOREST
C     FIRE WEATHER INDEX FOR A DEC PDP 11 AT P.F.E.S.
C     READS DATA AND PRINTS OUT IN METRIC UNITS.
      DIMENSION LMON(12), EL(12), FL(12)
      WRITE(*,1004)
1004  FORMAT(2X,'PROGRAM NO.: F-4')
100   FORMAT(I2,F4.1,F4.1)
101   FORMAT(F4.1,I4,I4,F4.1)
102   FORMAT(F4.1,F4.1,F5.1,I2,I2)
C
C     READS LENGTH OF MONTHS, AND DAY LENGTH FACTORS
C
      DO 20 J=1,12
      READ(*,100) LMON(J), EL(J), FL(J)
20    CONTINUE
C
C     READS INITIAL VALUES OF FFMC, DMC, DC, STARTING MONTH AND NUMBER
C     OF DAYS OF DATA STARTING MONTH.
C
      READ(*,102) FO,PO,DOT,M,NDAYS
      DO 25 J=M,12
      NN=LMON(J)
1002  FORMAT(10(/),1X,'  DATE  TEMP  RH   WIND  RAIN   FFMC   DMC   DC
     1   ISI   BUI   FWI'/)
      IF(J.EQ.M) GO TO 304
      IDAYS=1
      GO TO 302
304   IDAYS=LMON(J)-NDAYS+1
C
C     READS DAILY WEATHER DATA
C
302   L=0
      DO 25 I=IDAYS,NN
      L=L+1
      READ(*,101,END=2000) T,IH,IW,R
      IF(L.NE.1) GO TO 301
      WRITE(*,1002)
301   TX=T
      H=IH
      W=IW
      RAIN=R
C
C     FINE FUEL MOISTURE CODE
C
      IF(R.GT.0.5) GO TO 10
      R=0.0
      FR=FO
      GO TO 150
10    RA=R
      IF(RA.LE.1.45) GO TO 6
      IF(RA-5.75) 9,9,12
6     F=123.85-(55.6*ALOG(RA+1.016))
      GO TO 13
9     F=57.87-(18.2*ALOG(RA-1.016))
      GO TO 13
12    F=40.69-(8.25*ALOG(RA-1.905))
13    C=8.73*EXP(-0.1117*FO)
      FR=(FO/100.)*F+(1.0-C)
      IF(FR.GE.0.) GO TO 150
      FR=0.0
150   WMO=101.-FR
      ED=0.942*(H**0.679)+(11.*EXP((H-100.)/10.))+0.18*(21.1-T)
     1*(1.-1./EXP(0.115*H))
      IF(WMO-ED) 26,27,28
26    EW=0.618*(H**0.753)+(10.*EXP((H-100.)/10.))+0.18*(21.1-T)
     1*(1.-1./EXP(0.115*H))
      IF(WMO.LT.EW) GO TO 29
27    WM=WMO
      GO TO 30
28    Z=0.424*(1.-(H/100.)**1.7)+(0.0694*(W**0.5))*(1.-(H/100.)**8)
      X=Z*(0.463*(EXP(0.0365*T)))
      WM=ED+(WMO-ED)/10.**X
      GO TO 30
29    WM=EW-(EW-WMO)/1.9953
30    FFM=101.-WM
      IF(FFM.GT.101.) GO TO 32
      IF(FFM) 33,34,34
32    FFM=101.
      GO TO 34
33    FFM=0.0
C
C     DUFF MOISTURE CODE
C
34    IF(T+1.1.GE.0.) GO TO 41
      T=-1.1
41    RK=1.894*(T+1.1)*(100.-H)*(EL(J)*0.0001)
43    IF(R.GT.1.5) GO TO 45
      PR=PO
      GO TO 250
45    RA=R
      RW=0.92*RA-1.27
      WMI=20.0+280./EXP(0.023*PO)
      IF(PO.LE.33.) GO TO 50
      IF(PO-65.) 52,52,53
50    B=100./(0.5+0.3*PO)
      GO TO 55
52    B=14.-1.3*ALOG(PO)
      GO TO 55
53    B=6.2*ALOG(PO)-17.2
55    WMR=WMI+(1000.*RW)/(48.77+B*RW)
      PR=43.43*(5.6348-ALOG(WMR-20.))
250   IF(PR.GE.0.) GO TO 61
      PR=0.0
61    DMC=PR+RK
C
C     DROUGHT CODE
C
      IF(T+2.8.GE.0.) GO TO 65
      T=-2.8
65    PE=(.36*(T+2.8)+FL(J))/2.
      IF(R.LE.2.8) GO TO 300
      RA=R
      RW=0.83*RA-1.27
      SMI=800.*EXP(-DOT/400.)
      DR=DOT-400.*ALOG(1.+((3.937*RW)/SMI))
      IF(DR.GT.0.) GO TO 83
      DR=0.0
83    DC=DR+PE
      GO TO 350
300   DR=DOT
      GO TO 83
350   IF(DC.GE.0.) GO TO 85
      DC=0.0
C
C     INITIAL SPREAD INDEX, BUILDUP INDEX, FIRE WEATHER INDEX
C
85    FM=101.-FFM
      SF=19.1152*EXP(-0.1386*FM)*(1.+FM**4.65/7950000.)
      SI=SF*EXP(0.05039*W)
93    BUI=(0.8*DC*DMC)/(DMC+0.4*DC)
      IF(BUI.GE.DMC) GO TO 95
      P=(DMC-BUI)/DMC
      CC=0.92+(0.0114*DMC)**1.7
      BUI=DMC-(CC*P)
      IF(BUI.LT.0.) BUI=0.
95    IF(BUI.GT.80.) GO TO 60
      BB=0.1*SI*(0.626*BUI**0.809+2.)
      GO TO 91
60    BB=0.1*SI*(1000./(25.+108.64/EXP(0.023*BUI)))
91    IF(BB-1.0.LE.0.) GO TO 98
      SL=2.72*(0.43*ALOG(BB))**0.647
      FWI=EXP(SL)
      GO TO 400
98    FWI=BB
400   IDC=DC+0.5
      IFFM=FFM+0.5
      IDMC=DMC+0.5
      ISI=SI+0.5
      IBUI=BUI+0.5
      IFWI=FWI+0.5
      WRITE(*,1001) J,I,TX,IH,IW,RAIN,IFFM,IDMC,IDC,ISI,IBUI,IFWI
1001  FORMAT(1X,2I3,F6.1,I4,I6,F7.1,6I6)
      FO=FFM
      PO=DMC
      DOT=DC
25    CONTINUE
2000  STOP
      END
      