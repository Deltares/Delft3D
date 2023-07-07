!!  Copyright (C)  Stichting Deltares, 2012-2023.
!!
!!  This program is free software: you can redistribute it and/or modify
!!  it under the terms of the GNU General Public License version 3,
!!  as published by the Free Software Foundation.
!!
!!  This program is distributed in the hope that it will be useful,
!!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
!!  GNU General Public License for more details.
!!
!!  You should have received a copy of the GNU General Public License
!!  along with this program. If not, see <http://www.gnu.org/licenses/>.
!!
!!  contact: delft3d.support@deltares.nl
!!  Stichting Deltares
!!  P.O. Box 177
!!  2600 MH Delft, The Netherlands
!!
!!  All indications and logos of, and references to registered trademarks
!!  of Stichting Deltares remain the property of Stichting Deltares. All
!!  rights reserved.
      module m_dlwq71

      implicit none

      contains


      SUBROUTINE DLWQ71 ( DISP   , DISPER , AREA   , FLOW   , ALENG  ,
     *                    VELO   , CONC   , BOUND  , IPOINT , NOSYS  ,
     *                    NOTOT  , NOQ1   , NOQ2   , NOQ    , NODISP ,
     *                    NOVELO , IDPNT  , IVPNT  , IOPT   , AMASS2 ,
     *                    ILFLAG , DMPQ   , NDMPQ  , IQDMP  )
!
!     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
!
!     CREATED:    march 1988 by L.Postma
!
!     FUNCTION            : Makes a mass balance with central
!                           differencing in space.
!
!     LOGICAL UNITNUMBERS : none
!
!     SUBROUTINES CALLED  : none
!
!     PARAMETERS          :
!
!     NAME    KIND     LENGTH     FUNCT.  DESCRIPTION
!     ----    -----    ------     ------- -----------
!     DISP    REAL        3       INPUT   dispersion in 3 directions
!     DISPER  REAL   NODISP*NOQ   INPUT   additional dispersion array
!     AREA    REAL       NOQ      INPUT   exchange surface area
!     FLOW    REAL       NOQ      INPUT   flows accross exchange surfs
!     ALENG   REAL      2*NOQ     INPUT   from- and to lengthes
!     VELO    REAL   NOVELO*NOQ   INPUT   additional velocity array
!     CONC    REAL   NOTOT*NOSEG  INPUT   concentrations
!     BOUND   REAL     NOTOT*?    INPUT   boundary concentrations
!     IPOINT  INTEGER   4*NOQ     INPUT   exchange pointers
!     NOSYS   INTEGER     1       INPUT   number of active substances
!     NOTOT   INTEGER     1       INPUT   total number of substances
!     NOQ1    INTEGER     1       INPUT   nr of exchanges in first dir.
!     NOQ2    INTEGER     1       INPUT   nr of exchanges in second dir.
!     NOQ3    INTEGER     1       INPUT   nr of exchanges in third dir.
!     NOQ     INTEGER     1       INPUT   total number of exchanges
!     NODISP  INTEGER     1       INPUT   number  of additional dispers.
!     NOVELO  INTEGER     1       INPUT   number  of additional velos.
!     IDPNT   INTEGER   NOSYS     INPUT   pointer systems to dispersions
!     IVPNT   INTEGER   NOSYS     INPUT   pointer systems to velocities
!     IOPT    INTEGER     1       INPUT   = 0 or 2 DISP at zero flow
!                                         = 1 or 3 no DISP at zero flow
!                                         = 0 or 1 DISP over boundary
!                                         = 2 or 3 no DISP over boundary
!     AMASS2  REAL     NOTOT*5    IN/OUT  mass balance array
!     ILFLAG  INTEGER     1       INPUT   if 0 then 3 length values
!     DMPQ    REAL  NOTOT*NDMPQ*? IN/OUT  mass balance dumped exchange
!                                         if INTOPT > 7
!     NDMPQ   INTEGER     1       INPUT   number of dumped exchanges
!     IQDMP   INTEGER     *       INPUT   pointer dumped exchanges
!
      use timers

      INTEGER    NDMPQ
      INTEGER    IQDMP   (*) , ILFLAG
      real       DISP  (  3) , DISPER(*) , AREA (*) , FLOW  (*) ,
     *           ALENG (  *) , VELO  (*) , CONC (*) , BOUND (*) ,
     *           AMASS2(*)   , DMPQ  (*)
      integer    IPOINT(4,*) , IDPNT(*)  , IVPNT(*)

      integer    i, i3, i4, i5, i6, is, iq, ibflag, ipb, ipq, iopt
      integer    j, k1, k2
      integer    notot, nosys, noq, noq1, noq2, novelo, nodisp

      real       a, q, e, d, v, al, dl, dv, dq, f1, f2

      integer(4) ithandl /0/
      if ( timon ) call timstrt ( "dlwq71", ithandl )
!
!         loop accross the number of exchanges
!
      I4 = 3*NOTOT
      I5 = 4*NOTOT
      I6 = NOSYS*NDMPQ
      IF ( MOD(IOPT,16) .GE. 8  ) THEN
         IBFLAG = 1
      ELSE
         IBFLAG = 0
      ENDIF
!
      DO 60 IQ = 1 , NOQ
!
!         initialisations , check for transport anyhow
!
      I    = IPOINT(1,IQ)
      J    = IPOINT(2,IQ)
      IF ( I .EQ. 0 .OR. J .EQ. 0 ) GOTO 60
!
!     Check if exchange is dump exchange, set IPB
!
      IF ( IBFLAG .EQ. 1 ) THEN
         IF ( IQDMP(IQ) .GT. 0 ) THEN
            IPB = IQDMP(IQ)
            IPQ = (IQDMP(IQ)-1)*NOSYS
         ELSE
            IPB = 0
         ENDIF
      ELSE
         IPB = 0
      ENDIF
      IF ( I .GT. 0 .AND. J .GT. 0 .AND. IPB .EQ. 0 ) GOTO 60
      A    = AREA(IQ)
      Q    = FLOW(IQ)
      IF ( MOD(IOPT,2) .EQ. 1 ) THEN
           IF ( ABS(Q) .LT. 10.0E-25 )  GOTO 60
      ENDIF
      E  = DISP(1)
      AL = ALENG(1)
      IF ( IQ .GT. NOQ1      ) THEN
           E  = DISP (2)
           AL = ALENG(2)
      ENDIF
      IF ( IQ .GT. NOQ1+NOQ2 ) THEN
           E  = DISP (3)
           AL = ALENG(3)
      ENDIF
      IF ( ILFLAG .EQ. 1 ) THEN
           DL = A / (ALENG(2*IQ-1) + ALENG(2*IQ))
           F1 = ALENG(2*IQ  )*DL/A
           F2 = ALENG(2*IQ-1)*DL/A
      ELSE
           DL = A / AL
           F1 = 0.5
           F2 = 0.5
      ENDIF
      E  = E*DL
      IF ( I .LT. 0 ) GOTO 20
      IF ( J .LT. 0 ) GOTO 40
!
!         the regular case
!
      K1 = (I-1)*NOTOT
      K2 = (J-1)*NOTOT
      DO 10 I3=1,NOTOT
      IS = MIN ( I3 , NOSYS )
      D  = E
      V  = Q
      IF ( IDPNT(IS) .GT. 0 ) D = D + DISPER((IQ-1)*NODISP+IDPNT(IS))*DL
      IF ( IVPNT(IS) .GT. 0 ) THEN
           DV = VELO((IQ-1)*NOVELO+IVPNT(IS))*A
           V  = V + DV
      ENDIF
      DQ = (V*F1+D)*CONC(K1+I3) + (V*F2-D)*CONC(K2+I3)
!
!        mass balance
!
         IF ( DQ .GT. 0.0 ) THEN
            DMPQ(IPQ+I3)=DMPQ(IPQ+I3) + DQ
         ELSE
            DMPQ(IPQ+I3+I6)=DMPQ(IPQ+I3+I6) - DQ
         ENDIF
!
   10 CONTINUE
      GOTO 60
!
!        The 'from' element was a boundary. Note the 2 options.
!
   20 IF ( J .LT. 0 ) GOTO 60
      K1 = (-I-1)*NOTOT
      K2 = ( J-1)*NOTOT
      DO 30 I3=1,NOTOT
      IS = MIN ( I3 , NOSYS )
      V  = Q
      IF ( IVPNT(IS) .GT. 0 ) V = V + VELO  ((IQ-1)*NOVELO+IVPNT(IS))*A
      D = 0.0
      IF ( MOD(IOPT,4) .LT.  2 ) THEN
           D  = E
           IF ( IDPNT(IS).GT.0 ) D=D+ DISPER((IQ-1)*NODISP+IDPNT(IS))*DL
      ENDIF
      IF ( MOD(IOPT,8) .GE.  4 ) THEN
           IF ( V .GT. 0.0 ) THEN
                F1 = 1.0
                F2 = 0.0
           ELSE
                F1 = 0.0
                F2 = 1.0
           ENDIF
      ENDIF
      DQ = (V*F1+D)*BOUND(K1+I3) + (V*F2-D)*CONC (K2+I3)
      IF ( DQ .GT. 0.0 ) THEN
           AMASS2(I3+I4) = AMASS2(I3+I4) + DQ
      ELSE
           AMASS2(I3+I5) = AMASS2(I3+I5) - DQ
      ENDIF
      IF ( IPB .GT. 0  ) THEN
         IF ( DQ .GT. 0.0 ) THEN
            DMPQ(IPQ+I3)=DMPQ(IPQ+I3) + DQ
         ELSE
            DMPQ(IPQ+I3+I6)=DMPQ(IPQ+I3+I6) - DQ
         ENDIF
      ENDIF
   30 CONTINUE
      GOTO 60
!
!        The 'to' element was a boundary.
!
   40 IF ( I .EQ. 0 ) GOTO 60
      K1 = ( I-1)*NOTOT
      K2 = (-J-1)*NOTOT
      DO 50 I3=1,NOTOT
      IS = MIN ( I3 , NOSYS )
      V  = Q
      IF ( IVPNT(IS) .GT. 0 ) V = V + VELO  ((IQ-1)*NOVELO+IVPNT(IS))*A
      D = 0.0
      IF ( MOD(IOPT,4)  .LT.  2 ) THEN
           D  = E
           IF ( IDPNT(IS).GT.0 ) D=D+ DISPER((IQ-1)*NODISP+IDPNT(IS))*DL
      ENDIF
      IF ( MOD(IOPT,8) .GE.  4 ) THEN
           IF ( V .GT. 0.0 ) THEN
                F1 = 1.0
                F2 = 0.0
           ELSE
                F1 = 0.0
                F2 = 1.0
           ENDIF
      ENDIF
      DQ = (V*F1+D)*CONC (K1+I3) + (V*F2-D)*BOUND(K2+I3)
      IF ( DQ .GT. 0.0 ) THEN
           AMASS2(I3+I5) = AMASS2(I3+I5) + DQ
      ELSE
           AMASS2(I3+I4) = AMASS2(I3+I4) - DQ
      ENDIF
      IF ( IPB .GT. 0 ) THEN
         IF ( DQ .GT. 0.0 ) THEN
            DMPQ(IPQ+I3)=DMPQ(IPQ+I3) + DQ
         ELSE
            DMPQ(IPQ+I3+I6)=DMPQ(IPQ+I3+I6) - DQ
         ENDIF
      ENDIF
   50 CONTINUE
!
!        end of the loop over exchanges
!
   60 CONTINUE
!
      if ( timon ) call timstop ( ithandl )
      RETURN
      END

      end module m_dlwq71
