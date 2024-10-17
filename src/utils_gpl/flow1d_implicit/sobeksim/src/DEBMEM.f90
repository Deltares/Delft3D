SUBROUTINE DEBMEM ( ICODE, LU, ISTEP, CLABEL )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Memory Management Module
!
! Programmer:         A. Hoekstra / S.L. van der Woude
!
! Module:             DEBMEM (DEBug MEMory)
!
! Module description: Give debug information to programmer
!
!                     This subroutine gives debug information to the
!                     user of the memory management subroutines. First a
!                     call must be made with ICODE = 1 to create a dou-
!                     ble administration. All names known are copied
!                     preceded with a dollar sign. The memory buffers
!                     are copied also. Next consecutive calls can be
!                     made with ICODE = 2. Changed variables will then
!                     be reported.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  4 clabel            I  Character text to be reported
!  1 icode             I  Code for memory debugging
!                         1 = Create a shadow administration
!                         2 = Dump changes to file
!  3 istep             I  Current time step number (t(n+1)).
!  2 lu                I  Logical unitnumber associated with file.
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! dequal  EQUAL test of two Double precision variables
! equal   EQUAL test of two real variables
! gtclen  GeT Character variable LENgth
! gtcpnt  GeT Character PoiNTer
! gtdlen  GeT Double precision variable LENgth
! gtdpnt  GeT Double precision variable PoiNTer
! gtilen  GeT Integer variable LENgth
! gtipnt  GeT Integer variable PoiNTer
! gtllen  GeT Logical variable LENgth
! gtlpnt  GeT Logical variable PoiNTer
! gtrlen  GeT Real variable LENgth
! gtrpnt  GeT Real variable PoiNTer
! mkcpnt  MaKe Character PoiNTer
! mkdpnt  MaKe Double precision variable PoiNTer
! mkipnt  MaKe Integer variable PoiNTer
! mklpnt  MaKe Logical variable PoiNTer
! mkrpnt  MaKe Real variable PoiNTer
!=======================================================================
!
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: debmem.pf,v $
! Revision 1.5  1999/03/15  15:52:35  kuipe_j
! tabs removed
!
! Revision 1.4  1996/11/12  15:11:25  kuipe_j
! Declare auxill. arrays later
!
! Revision 1.3  1995/10/18  08:59:46  kuipe_j
! Changes concerning aux. ouput and IVR adjustments
!
! Revision 1.2  1995/05/30  07:03:23  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:08:36  hoeks_a
! Initial check-in
!
! Revision 1.1.1.1  1993/07/21  14:44:01  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Parameters
!
   INTEGER       ICODE, LU, ISTEP
   CHARACTER*(*) CLABEL
!
!     Local variables
!
   INTEGER       LVAR, PNT1, PNT2, I, J, NPNT
   CHARACTER*16  NAME, CNAM
   LOGICAL       CHNGD
!
!     External functions
!
   INTEGER       MKCPNT, MKDPNT, MKIPNT, MKLPNT, MKRPNT
   INTEGER       GTCLEN, GTDLEN, GTILEN, GTLLEN, GTRLEN
   INTEGER       GTCPNT, GTDPNT, GTIPNT, GTLPNT, GTRPNT
   LOGICAL       EQUAL , DEQUAL

   EXTERNAL      MKCPNT, MKDPNT, MKIPNT, MKLPNT, MKRPNT
   EXTERNAL      GTCLEN, GTDLEN, GTILEN, GTLLEN, GTRLEN
   EXTERNAL      GTCPNT, GTDPNT, GTIPNT, GTLPNT, GTRPNT
   EXTERNAL      EQUAL , DEQUAL
!
!     Include files
!
   include '..\include\pointrs.i'
   include '..\include\mempool.i'
!
!     ICODE = 1, create double administration
!
   IF (ICODE .EQ. 1) THEN
!
!        Write memory usage to file
!
      WRITE(LU,*) 'MEMORY USAGE BEFORE COPYING'
      WRITE(LU,*) 'CP ', CADDRS(NCPNTR+1)-1
      WRITE(LU,*) 'DP ', DADDRS(NDPNTR+1)-1
      WRITE(LU,*) 'IP ', IADDRS(NIPNTR+1)-1
      WRITE(LU,*) 'LP ', LADDRS(NLPNTR+1)-1
      WRITE(LU,*) 'RP ', RADDRS(NRPNTR+1)-1
!
!        Create double administration
!
      NPNT = NCPNTR
      IF (NPNT .GT. 0) THEN
         DO 20 I = 1, NPNT
            NAME = CPNTRS (I)
            IF (NAME(1:1) .NE. '$') THEN
               LVAR = GTCLEN (NAME)
               PNT1 = GTCPNT (NAME)
               CNAM = '$' // NAME(1:15)
               PNT2 = MKCPNT (CNAM,LVAR)
               IF (PNT2 .LT. -1) GOTO 1000
               IF (PNT2 .EQ. 0) THEN
                  DO 10 J = 1, LVAR
                     CP(PNT2) = CP(PNT1)
                     PNT1 = PNT1 + 1
                     PNT2 = PNT2 + 1
10                CONTINUE
               ENDIF
            ENDIF
20       CONTINUE
      ENDIF
!
      NPNT = NDPNTR
      IF (NPNT .GT. 0) THEN
         DO 40 I = 1, NPNT
            NAME = DPNTRS (I)
            IF (NAME(1:1) .NE. '$') THEN
               LVAR = GTDLEN (NAME)
               PNT1 = GTDPNT (NAME)
               CNAM = '$' // NAME(1:15)
               PNT2 = MKDPNT (CNAM,LVAR)
               IF (PNT2 .LT. -1) GOTO 1000
               IF (PNT2 .EQ. 0) THEN
                  DO 30 J = 1, LVAR
                     DP(PNT2) = DP(PNT1)
                     PNT1 = PNT1 + 1
                     PNT2 = PNT2 + 1
30                CONTINUE
               ENDIF
            ENDIF
40       CONTINUE
      ENDIF
!
      NPNT = NIPNTR
      IF (NPNT .GT. 0) THEN
         DO 60 I = 1, NPNT
            NAME = IPNTRS (I)
            IF (NAME(1:1) .NE. '$') THEN
               LVAR = GTILEN (NAME)
               PNT1 = GTIPNT (NAME)
               CNAM = '$' // NAME(1:15)
               PNT2 = MKIPNT (CNAM,LVAR)
               IF (PNT2 .LT. -1) GOTO 1000
               IF (PNT2 .EQ. 0)  THEN
                  DO 50 J = 1, LVAR
                     IP(PNT2) = IP(PNT1)
                     PNT1 = PNT1 + 1
                     PNT2 = PNT2 + 1
50                CONTINUE
               ENDIF
            ENDIF
60       CONTINUE
      ENDIF
!
      NPNT = NLPNTR
      IF (NPNT .GT. 0) THEN
         DO 80 I = 1, NPNT
            NAME = LPNTRS (I)
            IF (NAME(1:1) .NE. '$') THEN
               LVAR = GTLLEN (NAME)
               PNT1 = GTLPNT (NAME)
               CNAM = '$' // NAME(1:15)
               PNT2 = MKLPNT (CNAM,LVAR)
               IF (PNT2 .LT. -1) GOTO 1000
               IF (PNT2 .EQ. 0)  THEN
                  DO 70 J = 1, LVAR
                     LP(PNT2) = LP(PNT1)
                     PNT1 = PNT1 + 1
                     PNT2 = PNT2 + 1
70                CONTINUE
               ENDIF
            ENDIF
80       CONTINUE
      ENDIF
!
      NPNT = NRPNTR
      IF (NPNT .GT. 0) THEN
         DO 95 I = 1, NPNT
            NAME = RPNTRS (I)
            IF (NAME(1:1) .NE. '$') THEN
               LVAR = GTRLEN (NAME)
               PNT1 = GTRPNT (NAME)
               CNAM = '$' // NAME(1:15)
               PNT2 = MKRPNT (CNAM,LVAR)
               IF (PNT2 .LT. -1) GOTO 1000
               IF (PNT2 .EQ. 0)  THEN
                  DO 90 J = 1, LVAR
                     RP(PNT2) = RP(PNT1)
                     PNT1 = PNT1 + 1
                     PNT2 = PNT2 + 1
90                CONTINUE
               ENDIF
            ENDIF
95       CONTINUE
      ENDIF
!
   ELSE IF (ICODE .EQ. 2) THEN
!
!        Dump changed variables
!
      WRITE(LU,*) 'CHANGED VARIABLE DUMP ', ISTEP, ' ', CLABEL
!
!        Check for changed variables
!
      IF (NCPNTR .GT. 0) THEN
         DO 120 I = 1, NCPNTR
            NAME = CPNTRS (I)
            LVAR = GTCLEN (NAME)
            PNT1 = GTCPNT (NAME)
            IF (NAME(1:1) .NE. '$') THEN
               CNAM = '$' // NAME(1:15)
               PNT2 = GTCPNT (CNAM)
               IF (PNT2 .LT. 0) GOTO 2000
               CHNGD = .FALSE.
               DO 110 J = 1, LVAR
                  IF (.NOT. CHNGD) THEN
                     CHNGD = CP(PNT1) .NE. CP(PNT2)
                     IF (CHNGD) THEN
                        WRITE(LU,*) CPNTRS(I), ' has been changed'
                     ENDIF
                  ENDIF
                  CP(PNT2) = CP(PNT1)
                  PNT1 = PNT1 + 1
                  PNT2 = PNT2 + 1
110            CONTINUE
            ENDIF
120      CONTINUE
      ENDIF
!
      IF (NDPNTR .GT. 0) THEN
         DO 140 I = 1, NDPNTR
            NAME = DPNTRS (I)
            LVAR = GTDLEN (NAME)
            PNT1 = GTDPNT (NAME)
            IF (NAME(1:1) .NE. '$') THEN
               CNAM = '$' // NAME(1:15)
               PNT2 = GTDPNT (CNAM)
               IF (PNT2 .LT. 0) GOTO 2000
               CHNGD = .FALSE.
               DO 130 J = 1, LVAR
                  IF (.NOT. CHNGD) THEN
                     CHNGD = .NOT. DEQUAL(DP(PNT1),DP(PNT2))
                     IF (CHNGD) THEN
                        WRITE(LU,*) DPNTRS(I), ' has been changed'
                     ENDIF
                  ENDIF
                  DP(PNT2) = DP(PNT1)
                  PNT1 = PNT1 + 1
                  PNT2 = PNT2 + 1
130            CONTINUE
            ENDIF
140      CONTINUE
      ENDIF
!
      IF (NIPNTR .GT. 0) THEN
         DO 160 I = 1, NIPNTR
            NAME = IPNTRS (I)
            LVAR = GTILEN (NAME)
            PNT1 = GTIPNT (NAME)
            IF (NAME(1:1) .NE. '$') THEN
               CNAM = '$' // NAME(1:15)
               PNT2 = GTIPNT (CNAM)
               IF (PNT2 .LT. 0) GOTO 2000
               CHNGD = .FALSE.
               DO 150 J = 1, LVAR
                  IF (.NOT. CHNGD) THEN
                     CHNGD = IP(PNT1) .NE. IP(PNT2)
                     IF (CHNGD) THEN
                        WRITE(LU,*) IPNTRS(I), ' has been changed'
                     ENDIF
                  ENDIF
                  IP(PNT2) = IP(PNT1)
                  PNT1 = PNT1 + 1
                  PNT2 = PNT2 + 1
150            CONTINUE
            ENDIF
160      CONTINUE
      ENDIF
!
      IF (NLPNTR .GT. 0) THEN
         DO 180 I = 1, NLPNTR
            NAME = LPNTRS (I)
            LVAR = GTLLEN (NAME)
            PNT1 = GTLPNT (NAME)
            IF (NAME(1:1) .NE. '$') THEN
               CNAM = '$' // NAME(1:15)
               PNT2 = GTLPNT (CNAM)
               IF (PNT2 .LT. 0) GOTO 2000
               CHNGD = .FALSE.
               DO 170 J = 1, LVAR
                  IF (.NOT. CHNGD) THEN
                     IF (LP(PNT1)) THEN
                        IF (.NOT. LP(PNT2)) THEN
                           CHNGD = .TRUE.
                        ELSE
                           CHNGD = .FALSE.
                        ENDIF
                     ELSE
                        IF (LP(PNT2)) THEN
                           CHNGD = .TRUE.
                        ELSE
                           CHNGD = .FALSE.
                        ENDIF
                     ENDIF
                     IF (CHNGD) THEN
                        WRITE(LU,*) LPNTRS(I), ' has been changed'
                     ENDIF
                  ENDIF
                  LP(PNT2) = LP(PNT1)
                  PNT1 = PNT1 + 1
                  PNT2 = PNT2 + 1
170            CONTINUE
            ENDIF
180      CONTINUE
      ENDIF
!
      IF (NRPNTR .GT. 0) THEN
         DO 200 I = 1, NRPNTR
            NAME = RPNTRS (I)
            LVAR = GTRLEN (NAME)
            PNT1 = GTRPNT (NAME)
            IF (NAME(1:1) .NE. '$') THEN
               CNAM = '$' // NAME(1:15)
               PNT2 = GTRPNT (CNAM)
               IF (PNT2 .LT. 0) GOTO 2000
               CHNGD = .FALSE.
               DO 190 J = 1, LVAR
                  IF (.NOT. CHNGD) THEN
                     CHNGD = .NOT. EQUAL(RP(PNT1),RP(PNT2))
                     IF (CHNGD) THEN
                        WRITE(LU,*) RPNTRS(I), ' has been changed'
                     ENDIF
                  ENDIF
                  RP(PNT2) = RP(PNT1)
                  PNT1 = PNT1 + 1
                  PNT2 = PNT2 + 1
190            CONTINUE
            ENDIF
200      CONTINUE
      ENDIF
   ENDIF
!
   RETURN
!
1000 CONTINUE

   WRITE(LU,*) 'RUNNING OUT OF MEMORY', PNT2, ' ', CNAM
   RETURN

2000 CONTINUE
   WRITE(LU,*) 'UNABLE TO LOCATE ', PNT2, CNAM

END
