!!  Copyright (C)  Stichting Deltares, 2012-2024.
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
module m_dlwqb5
    use m_waq_precision

    implicit none

contains


    SUBROUTINE DLWQB5 (DISP, DISPER, AREA, FLOW, ALENG, &
            VELO, CONC, BOUND, IPOINT, NOSYS, &
            NOTOT, NOQ1, NOQ2, NOQ, NODISP, &
            NOVELO, IDPNT, IVPNT, integration_id, AMASS2, &
            ILFLAG, DMPQ, NDMPQ, IDT, IQDMP)

        !! Makes a mass balance final to implicit integration methods.
        !! Identical to DLWQ64, but with dimension BOUND(NOSYS,*), with multiplication factor IDT on mass balances and
        !! with loop over active substances only (loops 10, 30 and 50).

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
        !     IPOINT  INTEGER   4*NOQ     INPUT   exchange pointers
        !     NOSYS   INTEGER     1       INPUT   number  of active substances
        !     NOTOT   INTEGER     1       INPUT   number  of total substances
        !     NOQ1    INTEGER     1       INPUT   nr of exchanges in first dir.
        !     NOQ2    INTEGER     1       INPUT   nr of exchanges in second dir.
        !     NOQ3    INTEGER     1       INPUT   nr of exchanges in third dir.
        !     NOQ     INTEGER     1       INPUT   total number of exchanges
        !     NODISP  INTEGER     1       INPUT   number  of additional dispers.
        !     NOVELO  INTEGER     1       INPUT   number  of additional velos.
        !     IDPNT   INTEGER   NOSYS     INPUT   pointer systems to dispersions
        !     IVPNT   INTEGER   NOSYS     INPUT   pointer systems to velocities
        !     integration_id    INTEGER     1       INPUT   = 0 or 2 DISP at zero flow
        !                                         = 1 or 3 no DISP at zero flow
        !                                         = 0 or 1 DISP over boundary
        !                                         = 2 or 3 no DISP over boundary
        !     AMASS2  REAL     NOTOT*5    IN/OUT  mass balance array
        !     ILFLAG  INTEGER     1       INPUT   if 0 then 3 length values
        !     DMPQ    REAL  NOTOT*NDMPQ*? IN/OUT  mass balance dumped exchange
        !                                         if INTOPT > 7
        !     NDMPQ   INTEGER     1       INPUT   number of dumped exchanges
        !     IDT     INTEGER     1       INPUT   timestep (or 1 for steady state)
        !     IQDMP   INTEGER     *       INPUT   pointer dumped exchanges
        !
        use timers

        INTEGER(kind = int_wp) :: NDMPQ
        INTEGER(kind = int_wp) :: IQDMP   (*), IPOINT(4, *), IDPNT(*), IVPNT(*)
        real(kind = real_wp) :: DISP  (3), DISPER(*), AREA (*), FLOW  (*), &
                ALENG (*), VELO  (*), CONC (*), BOUND (*), &
                AMASS2(*), DMPQ(*)
        integer(kind = int_wp) :: NOSYS, NOTOT, NOQ1, NOQ2, NOQ, NODISP, NOVELO
        integer(kind = int_wp) :: integration_id, ILFLAG, IDT

        integer(kind = int_wp) :: i, i3, i4, i5, i6, iq, ipq, is
        integer(kind = int_wp) :: noq12, ibflag, j, ipb, k1, k2

        real(kind = real_wp) :: a, q, e, al, dl, d, v, dq

        integer(kind = int_wp) :: ithandl = 0
        if (timon) call timstrt ("dlwqb5", ithandl)
        !
        !         loop accross the number of exchanges
        !
        I4 = 3 * NOTOT
        I5 = 4 * NOTOT
        I6 = NOSYS * NDMPQ
        NOQ12 = NOQ1 + NOQ2
        IF (MOD(integration_id, 16) >= 8) THEN
            IBFLAG = 1
        ELSE
            IBFLAG = 0
        ENDIF
        !
        DO IQ = 1, NOQ
            !
            !         initialistations, check for transport anyhow
            !
            I = IPOINT(1, IQ)
            J = IPOINT(2, IQ)
            IF (I == 0 .OR. J == 0) GOTO 60
            !
            !     Check if exchange is dump exchange, set IPB
            !
            IF (IBFLAG == 1) THEN
                IF (IQDMP(IQ) > 0) THEN
                    IPB = IQDMP(IQ)
                    IPQ = (IQDMP(IQ) - 1) * NOSYS
                ELSE
                    IPB = 0
                ENDIF
            ELSE
                IPB = 0
            ENDIF
            IF (I > 0 .AND. J > 0 .AND. IPB == 0) GOTO 60
            A = AREA(IQ)
            Q = FLOW(IQ)
            IF (MOD(integration_id, 2) == 1 .AND. IQ <= NOQ12) THEN
                IF (ABS(Q) < 10.0E-25)  GOTO 60
            ENDIF
            E = DISP(1)
            AL = ALENG(1)
            IF (IQ > NOQ1) THEN
                E = DISP (2)
                AL = ALENG(2)
            ENDIF
            IF (IQ > NOQ1 + NOQ2) THEN
                E = DISP (3)
                AL = ALENG(3)
            ENDIF
            IF (ILFLAG == 1) THEN
                DL = A / (ALENG(2 * IQ - 1) + ALENG(2 * IQ))
            ELSE
                DL = A / AL
            ENDIF
            E = E * DL
            IF (I < 0) GOTO 20
            IF (J < 0) GOTO 40
            !
            !         The regular case
            !
            K1 = (I - 1) * NOTOT
            K2 = (J - 1) * NOTOT
            DO I3 = 1, NOSYS
                IS = MIN (I3, NOSYS)
                !
                !        dispersion
                !
                IF (IDPNT(IS) > 0) THEN
                    D = E + DISPER((IQ - 1) * NODISP + IDPNT(IS)) * DL
                ELSE
                    D = E
                ENDIF
                !
                !        flow
                !
                IF (IVPNT(IS) > 0) THEN
                    V = Q + VELO  ((IQ - 1) * NOVELO + IVPNT(IS)) * A
                ELSE
                    V = Q
                ENDIF
                !
                !        transport
                !
                IF (V > 0.0) THEN
                    DQ = ((V + D) * CONC(K1 + I3) - D * CONC(K2 + I3)) * IDT
                ELSE
                    DQ = ((V - D) * CONC(K2 + I3) + D * CONC(K1 + I3)) * IDT
                ENDIF
                !
                !        mass balance
                !
                IF (DQ > 0.0) THEN
                    DMPQ(IPQ + I3) = DMPQ(IPQ + I3) + DQ
                ELSE
                    DMPQ(IPQ + I3 + I6) = DMPQ(IPQ + I3 + I6) - DQ
                ENDIF
                !
            end do
            GOTO 60
            !
            !        The 'from' element was a boundary. Note the 2 options.
            !
            20 IF (J < 0) GOTO 60
            K1 = (-I - 1) * NOSYS
            K2 = (J - 1) * NOTOT
            DO I3 = 1, NOSYS
                IS = MIN (I3, NOSYS)
                V = Q
                D = 0.0
                IF (IVPNT(IS) > 0) V = V + VELO  ((IQ - 1) * NOVELO + IVPNT(IS)) * A
                IF (MOD(integration_id, 4) <  2) THEN
                    D = E
                    IF (IDPNT(IS)>0) D = D + DISPER((IQ - 1) * NODISP + IDPNT(IS)) * DL
                ENDIF
                IF (V > 0.0) THEN
                    DQ = ((V + D) * BOUND(K1 + I3) - D * CONC (K2 + I3)) * IDT
                ELSE
                    DQ = ((V - D) * CONC (K2 + I3) + D * BOUND(K1 + I3)) * IDT
                ENDIF
                IF (DQ > 0.0) THEN
                    AMASS2(I3 + I4) = AMASS2(I3 + I4) + DQ
                ELSE
                    AMASS2(I3 + I5) = AMASS2(I3 + I5) - DQ
                ENDIF
                IF (IPB > 0) THEN
                    IF (DQ > 0.0) THEN
                        DMPQ(IPQ + I3) = DMPQ(IPQ + I3) + DQ
                    ELSE
                        DMPQ(IPQ + I3 + I6) = DMPQ(IPQ + I3 + I6) - DQ
                    ENDIF
                ENDIF
            end do
            GOTO 60
            !
            !        The 'to' element was a boundary.
            !
            40 K1 = (I - 1) * NOTOT
            K2 = (-J - 1) * NOSYS
            DO I3 = 1, NOSYS
                IS = MIN (I3, NOSYS)
                V = Q
                D = 0.0
                IF (IVPNT(IS) > 0) V = V + VELO  ((IQ - 1) * NOVELO + IVPNT(IS)) * A
                IF (MOD(integration_id, 4)  <  2) THEN
                    D = E
                    IF (IDPNT(IS)>0) D = D + DISPER((IQ - 1) * NODISP + IDPNT(IS)) * DL
                ENDIF
                IF (V > 0.0) THEN
                    DQ = ((V + D) * CONC (K1 + I3) - D * BOUND(K2 + I3)) * IDT
                ELSE
                    DQ = ((V - D) * BOUND(K2 + I3) + D * CONC (K1 + I3)) * IDT
                ENDIF
                IF (DQ > 0.0) THEN
                    AMASS2(I3 + I5) = AMASS2(I3 + I5) + DQ
                ELSE
                    AMASS2(I3 + I4) = AMASS2(I3 + I4) - DQ
                ENDIF
                IF (IPB > 0) THEN
                    IF (DQ > 0.0) THEN
                        DMPQ(IPQ + I3) = DMPQ(IPQ + I3) + DQ
                    ELSE
                        DMPQ(IPQ + I3 + I6) = DMPQ(IPQ + I3 + I6) - DQ
                    ENDIF
                ENDIF
            end do
            !
            !        end of the loop over exchanges
            !
            60 CONTINUE
        end do
        !
        if (timon) call timstop (ithandl)
        RETURN
    END

end module m_dlwqb5
