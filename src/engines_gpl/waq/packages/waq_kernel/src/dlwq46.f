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
      module m_dlwq46

      implicit none

      contains


      SUBROUTINE DLWQ46 ( DISP   , DISPER , AREA   , FLOW   , ALENG  ,
     *                    VELO   , CONC   , BOUND  , IPOINT , NOSYS  ,
     *                    NOTOT  , NOQ    , NODISP , NOVELO , IDPNT  ,
     *                    IVPNT  , IOPT   , IDT    , ILFLAG , DMPQ   ,
     *                    NDMPQ  , IQDMP  , IBACKW , NOQW    )
!
!     Deltares     SECTOR WATERRESOURCES AND ENVIRONMENT
!
!     CREATED             : october 1991 by J. van Beek
!
!     FUNCTION            : makes mass balance for segments for the past
!                           implicit step with the new concentrations
!                           and the old flow's
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
!     BOUND   REAL     NOSYS*?    INPUT   boundary concentrations
!     IPOINT  INTEGER    4*NOQ    INPUT   exchange pointers
!     NOSYS   INTEGER     1       INPUT   number  of active substances
!     NOTOT   INTEGER     1       INPUT   number  of total substances
!     NOQ     INTEGER     1       INPUT   total number of exchanges
!     NODISP  INTEGER     1       INPUT   number  of additional dispers.
!     NOVELO  INTEGER     1       INPUT   number  of additional velos.
!     IDPNT   INTEGER   NOSYS     INPUT   pointer systems to dispersions
!     IVPNT   INTEGER   NOSYS     INPUT   pointer systems to velocities
!     IOPT    INTEGER     1       INPUT   = 0 or 2 DISP at zero flow
!                                         = 1 or 3 no DISP at zero flow
!                                         = 0 or 1 DISP over boundary
!                                         = 2 or 3 no DISP over boundary
!     IDT     INTEGER     1       INPUT   time step size
!     ILFLAG  INTEGER     1       INPUT   if 0 then 3 length values
!     DMPQ    REAL  NOTOT*NDMPQ*? IN/OUT  mass balance dumped exchange
!                                         if INTOPT > 7
!     NDMPQ   INTEGER     1       INPUT   number of dumped exchanges
!     IQDMP   INTEGER     *       INPUT   pointer dumped exchanges
!     IBACKW  INTEGER     *       INPUT   flag = 0 central differences
!                                              = 1 backward differences
!     NOQW    INTEGER     1       INPUT   number of water exchanges
!
      use timers
      INTEGER    NDMPQ         , IBACKW
      INTEGER    IQDMP   (*)
      real       DISP  (  3)   , DISPER(*) , AREA (*) , FLOW (*) ,
     *           ALENG (  *)   , VELO  (*) , CONC (*) , BOUND(*) ,
     *           DMPQ(*)
      integer noqw                      !  input   number of exchanges waterphase
      integer IDPNT(*), IVPNT(*), IPOINT( 4,* )

      logical disp0q0, disp0bnd, loword !  logical representation of options

      integer(4) ithandl /0/

      integer notot, nosys, noq, novelo, nodisp
      integer i, i3, i6, j, k1, k2
      integer iq, ipb, iopt, ilflag, idt
      real    v, q
      real    f1, f2, g1, g2, d, dl, dq, a, al, e

      if ( timon ) call timstrt ( "dlwq46a", ithandl )

      disp0q0  = btest( iopt , 0 )
      disp0bnd = btest( iopt , 1 )
      loword   = btest( iopt , 2 )
!
!     offsets in mass balance array
!
      I6 = NOSYS*NDMPQ
!
!     loop accross the number of exchanges
!
      DO 60 IQ = 1 , NOQ
!
!        initialisations , check for transport anyhow
!
         I    = IPOINT(1,IQ)
         J    = IPOINT(2,IQ)
         IF ( I .EQ. 0 .OR. J .EQ. 0 ) GOTO 60
         A    = AREA(IQ)
         Q    = FLOW(IQ)
!
!     Check if exchange is dump exchange, set IPB
!
         IF ( IQDMP(IQ) .GT. 0 ) THEN
            IPB = (IQDMP(IQ)-1)*NOSYS
         ELSE
            GOTO 60
         ENDIF
         E  = DISP(1)
         AL = ALENG(1)
         IF ( ILFLAG .EQ. 1 ) THEN
              AL = ALENG(2*IQ-1) + ALENG(2*IQ)
              F1 = ALENG(2*IQ  ) / AL
              F2 = ALENG(2*IQ-1) / AL
         ELSE
              F1 = 0.5
              F2 = 0.5
         ENDIF
         G1 = F1
         G2 = F2
         DL = A / AL
         E  = E*DL
         if ( iq .gt. noqw ) e = 0.0      !  no water diffusion in the bottom
         IF ( I .LT. 0 ) GOTO 20
         IF ( J .LT. 0 ) GOTO 40
!
!            The regular case
!
         K1 = (I-1)*NOTOT
         K2 = (J-1)*NOTOT
         DO 10 I3=1,NOSYS
!
!           dispersion
!
            IF ( IDPNT(I3) .GT. 0 ) THEN
               D = E + DISPER((IQ-1)*NODISP+IDPNT(I3))*DL
            ELSE
               D = E
            ENDIF
!
!           flow
!
            IF ( IVPNT(I3) .GT. 0 ) THEN
               V = Q + VELO((IQ-1)*NOVELO+IVPNT(I3))  *A
            ELSE
               V = Q
            ENDIF
            if ( disp0q0 ) then
               if ( a .le. 0.0 .or. abs(v) .lt. 10.0E-25 ) d = 0.0
            endif
            IF ( IQ .GT. NOQW .OR. IBACKW .EQ. 1 ) THEN  ! in the bottom backward differences
               IF ( V .GT. 0 ) THEN
                  G1 = 1.0
                  G2 = 0.0
               ELSE
                  G1 = 0.0
                  G2 = 1.0
               ENDIF
            ENDIF
!
!           transport
!
            DQ = (V*G1+D)*CONC(K1+I3) + (V*G2-D)*CONC(K2+I3)
!
!           update mass balance array ( internal transport )
!
            IF ( DQ .GT. 0.0 ) THEN
               DMPQ(IPB+I3)=DMPQ(IPB+I3) + DQ*IDT
            ELSE
               DMPQ(IPB+I3+I6)=DMPQ(IPB+I3+I6) - DQ*IDT
            ENDIF
   10    CONTINUE
         GOTO 60
!
!           The 'from' element was a boundary. Note the 2 options.
!
   20    IF ( J .LT. 0 ) GOTO 60
         K1 = (-I-1)*NOSYS
         K2 = ( J-1)*NOTOT
         DO 30 I3=1,NOSYS
!
!           flow
!
            IF ( IVPNT(I3) .GT. 0 ) THEN
               V = Q + VELO((IQ-1)*NOVELO+IVPNT(I3))*A
            ELSE
               V = Q
            ENDIF
            IF ( IQ .GT. NOQW .OR. MOD(IOPT,8) .GE. 4 .OR. IBACKW .EQ. 1 ) THEN ! in the bottom backward differences + etc
               IF ( V .GT. 0 ) THEN
                  G1 = 1.0
                  G2 = 0.0
               ELSE
                  G1 = 0.0
                  G2 = 1.0
               ENDIF
            ENDIF
!
!           dispersion
!
            IF ( MOD(IOPT,4) .LT.  2 ) THEN
               IF ( IDPNT(I3).GT.0 ) THEN
                  D = E + DISPER((IQ-1)*NODISP+IDPNT(I3))*DL
               ELSE
                  D = E
               ENDIF
            ELSE
               D = 0.0
            ENDIF
            if ( disp0q0 ) then
               if ( a .le. 0.0 .or. abs(v) .lt. 10.0E-25 ) d = 0.0
            endif
!
!           tranport
!
            DQ = (V*G1+D)*BOUND(K1+I3) + (V*G2-D)*CONC(K2+I3)
!
!           update mass balance array ( boundaries in / out )
!
            IF ( DQ .GT. 0.0 ) THEN
               DMPQ(IPB+I3)=DMPQ(IPB+I3) + DQ*IDT
            ELSE
               DMPQ(IPB+I3+I6)=DMPQ(IPB+I3+I6) - DQ*IDT
            ENDIF
   30    CONTINUE
         GOTO 60
!
!        The 'to' element was a boundary.
!
   40    K1 = ( I-1)*NOTOT
         K2 = (-J-1)*NOSYS
         DO 50 I3=1,NOSYS
!
!           flow
!
            IF ( IVPNT(I3) .GT. 0 ) THEN
               V = Q + VELO((IQ-1)*NOVELO+IVPNT(I3))*A
            ELSE
               V = Q
            ENDIF
            IF ( IQ .GT. NOQW .OR. MOD(IOPT,8) .GE. 4 .OR. IBACKW .EQ. 1 )THEN ! in the bottom backward differences + etc
               IF ( V .GT. 0 ) THEN
                  G1 = 1.0
                  G2 = 0.0
               ELSE
                  G1 = 0.0
                  G2 = 1.0
               ENDIF
            ENDIF
!
!           dispersion
!
            IF ( MOD(IOPT,4) .LT.  2 ) THEN
               IF ( IDPNT(I3).GT.0 ) THEN
                  D = E + DISPER((IQ-1)*NODISP+IDPNT(I3))*DL
               ELSE
                  D = E
               ENDIF
            ELSE
               D  = 0.0
            ENDIF
            if ( disp0q0 ) then
               if ( a .le. 0.0 .or. abs(v) .lt. 10.0E-25 ) d = 0.0
            endif
!
!           transport
!
            DQ = (V*G1+D)*CONC(K1+I3) + (V*G2-D)*BOUND(K2+I3)
!
!           update mass balance array ( boundaries in / out )
!
            IF ( DQ .GT. 0.0 ) THEN
               DMPQ(IPB+I3)=DMPQ(IPB+I3) + DQ*IDT
            ELSE
               DMPQ(IPB+I3+I6)=DMPQ(IPB+I3+I6) - DQ*IDT
            ENDIF
   50    CONTINUE
!
!        end of the loop over exchanges
!
   60 CONTINUE
!
      if ( timon ) call timstop ( ithandl )
      RETURN
      END

      end module m_dlwq46
