!!  Copyright (C)  Stichting Deltares, 2012-2025.
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
module m_advtra
    use m_waq_precision

    implicit none

contains


    subroutine advtra (process_space_real, fl, ipoint, increm, num_cells, &
            noflux, iexpnt, iknmrk, num_exchanges_u_dir, num_exchanges_v_dir, &
            num_exchanges_z_dir, num_exchanges_bottom_dir)
        !>\file
        !>       Advective transport, velocities and fluxes, of solids in sediment

        !
        !     Description of the module :
        !
        !     Logical Units : -

        !     Modules called : -

        !     Name     Type   Library
        !     ------   -----  ------------

        !     IMPLICIT REAL (A-H,J-Z)
        !
        ! Name    T   L I/O   Description                                    Units
        ! ----    --- -  -    -------------------                            -----
        ! Coll Struct 1  O    Structure with collection of bottom collumn info
        !                  Contains:
        !    type(BotColmn), pointer :: set(:)  ! array with info for all bottom collumns
        !    integer                 :: maxsize ! maximum size of the current array
        !    integer                 :: current_size ! filled up to this size
        ! BotColm Struct 1   O  Structure with bottom collumn info
        !                  Contains:
        !    integer :: fstwatsed  ! first water sediment exchange number
        !    integer :: lstwatsed  ! last  water sediment exchange number
        !    integer :: topsedsed  ! first within collumn exchange number
        !    integer :: botsedsed  ! last exchange of collumn to deeper bnd
        !
        use m_extract_waq_attribute
        USE BottomSet     !  Module with derived types and add function

        !     type ( BotColmnColl ) :: Coll  <= is defined in the module

        REAL(kind = real_wp) :: process_space_real  (*), FL    (*)
        INTEGER(kind = int_wp) :: IPOINT(*), INCREM(*), num_cells, NOFLUX, &
                IEXPNT(4, *), IKNMRK(*), num_exchanges_u_dir, num_exchanges_v_dir, num_exchanges_z_dir, num_exchanges_bottom_dir

        !     Locals

        INTEGER(kind = int_wp) :: IWA1, IWA2, ITOP, IBOT, IOFFSE, IQ, ISEG, &
                IKMRK1, IK
        REAL(kind = real_wp) :: TOTSED, SW, VBUR, DELT, SEEP, PORACT
        INTEGER(kind = int_wp) :: IPDM, IPSFLX, IPZRES, IPVBUR, IPTAU, IPTCRR, IPSURF, &
                IPDEPT, IPMDEP, IPACTH, IPMNTH, IPMXTH, IPSW, IPDELT, &
                IPRFLX, IPPRES, IPTFLX, IPBFLX, IPRVEL, IPSVEL, IPBVEL, &
                IPRVOL, IPSVOL, IPBVOL, IPPORI, IPPORA, &
                IPDVOL, IPDVEL, IPSEEP
        INTEGER(kind = int_wp) :: INDM, INSFLX, INZRES, INVBUR, INTAU, INTCRR, INSURF, &
                INDEPT, INMDEP, INACTH, INMNTH, INMXTH, INSW, INDELT, &
                INRFLX, INPRES, INTFLX, INBFLX, INRVEL, INSVEL, INBVEL, &
                INRVOL, INSVOL, INBVOL, INPORI, INPORA, &
                INDVOL, INDVEL, INSEEP

        integer(kind = int_wp) :: IPSWRE, IPALPH, IPCFLX, INSWRE, INALPH, INCFLX
        !     Include column structure
        !     we define a double column structure, one for downward,
        !     and one for upward transport

        CALL MAKKO2 (IEXPNT, IKNMRK, num_exchanges_u_dir, num_exchanges_v_dir, num_exchanges_z_dir, &
                num_exchanges_bottom_dir)
        !
        IPDM = IPOINT(1)
        IPSFLX = IPOINT(2)
        IPZRES = IPOINT(3)
        IPVBUR = IPOINT(4)
        IPTAU = IPOINT(5)
        IPTCRR = IPOINT(6)
        IPSURF = IPOINT(7)
        IPDEPT = IPOINT(8)
        IPMDEP = IPOINT(9)
        IPACTH = IPOINT(10)
        IPMNTH = IPOINT(11)
        IPMXTH = IPOINT(12)
        IPSW = IPOINT(13)
        IPDELT = IPOINT(14)
        IPPORA = IPOINT(15)
        IPPORI = IPOINT(16)
        IPSEEP = IPOINT(17)
        IPSWRE = IPOINT(18)
        IPALPH = IPOINT(19)
        IPCFLX = IPOINT(20)

        IPRFLX = IPOINT(21)
        IPPRES = IPOINT(22)
        IPTFLX = IPOINT(23)
        IPBFLX = IPOINT(24)

        IPRVEL = IPOINT(25)
        IPSVEL = IPOINT(26)
        IPBVEL = IPOINT(27)
        IPDVEL = IPOINT(28)
        IPRVOL = IPOINT(29)
        IPSVOL = IPOINT(30)
        IPBVOL = IPOINT(31)
        IPDVOL = IPOINT(32)

        INDM = INCREM(1)
        INSFLX = INCREM(2)
        INZRES = INCREM(3)
        INVBUR = INCREM(4)
        INTAU = INCREM(5)
        INTCRR = INCREM(6)
        INSURF = INCREM(7)
        INDEPT = INCREM(8)
        INMDEP = INCREM(9)
        INACTH = INCREM(10)
        INMNTH = INCREM(11)
        INMXTH = INCREM(12)
        INSW = INCREM(13)
        INDELT = INCREM(14)
        INPORA = INCREM(15)
        INPORI = INCREM(16)
        INSEEP = INCREM(17)
        INSWRE = INCREM(18)
        INALPH = INCREM(19)
        INCFLX = INCREM(20)

        INRFLX = INCREM(21)
        INPRES = INCREM(22)
        INTFLX = INCREM(23)
        INBFLX = INCREM(24)

        INRVEL = INCREM(25)
        INSVEL = INCREM(26)
        INBVEL = INCREM(27)
        INDVEL = INCREM(28)
        INRVOL = INCREM(29)
        INSVOL = INCREM(30)
        INBVOL = INCREM(31)
        INDVOL = INCREM(32)
        !
        do ISEG = 1, num_cells

            !     Zero output quantities on segment level

            CALL extract_waq_attribute(1, IKNMRK(ISEG), IKMRK1)
            IF (IKMRK1==3) THEN
                process_space_real (IPCFLX) = 0.0
                process_space_real (IPRFLX) = 0.0
                process_space_real (IPPRES) = 0.0
                process_space_real (IPTFLX) = 0.0
                process_space_real (IPBFLX) = 0.0
            ENDIF

            IPCFLX = IPCFLX + INCFLX
            IPRFLX = IPRFLX + INRFLX
            IPPRES = IPPRES + INPRES
            IPTFLX = IPTFLX + INTFLX
            IPBFLX = IPBFLX + INBFLX
            !
        enddo
        !
        !.....Zero output quantities on exchange level
        do IQ = 1, num_exchanges_u_dir + num_exchanges_v_dir + num_exchanges_z_dir + num_exchanges_bottom_dir
            process_space_real(IPRVEL) = 0.0
            process_space_real(IPSVEL) = 0.0
            process_space_real(IPBVEL) = 0.0
            process_space_real(IPDVEL) = 0.0
            process_space_real(IPRVOL) = 0.0
            process_space_real(IPSVOL) = 0.0
            process_space_real(IPBVOL) = 0.0
            process_space_real(IPDVOL) = 0.0
            IPRVEL = IPRVEL + INRVEL
            IPSVEL = IPSVEL + INSVEL
            IPBVEL = IPBVEL + INBVEL
            IPDVEL = IPDVEL + INDVEL
            IPRVOL = IPRVOL + INRVOL
            IPSVOL = IPSVOL + INSVOL
            IPBVOL = IPBVOL + INBVOL
            IPDVOL = IPDVOL + INDVOL
        enddo

        IPCFLX = IPOINT(20)
        IPRFLX = IPOINT(21)
        IPPRES = IPOINT(22)
        IPTFLX = IPOINT(23)
        IPBFLX = IPOINT(24)

        IPRVEL = IPOINT(25)
        IPSVEL = IPOINT(26)
        IPBVEL = IPOINT(27)
        IPDVEL = IPOINT(28)
        IPRVOL = IPOINT(29)
        IPSVOL = IPOINT(30)
        IPBVOL = IPOINT(31)
        IPDVOL = IPOINT(32)

        !.....Loop over kolommen
        DO IK = 1, Coll%current_size

            !         Select first column of exchanges for DOWNWARD advection

            IWA1 = Coll%set(IK)%fstwatsed
            IWA2 = Coll%set(IK)%lstwatsed
            ITOP = Coll%set(IK)%topsedsed
            IBOT = Coll%set(IK)%botsedsed

            !         Take uniform quantities from top sediment layer

            ISEG = IEXPNT(1, ITOP)
            SW = process_space_real(IPSW + (ISEG - 1) * INSW)
            VBUR = process_space_real(IPVBUR + (ISEG - 1) * INVBUR)
            DELT = process_space_real(IPDELT + (ISEG - 1) * INDELT)
            SEEP = process_space_real(IPSEEP + (ISEG - 1) * INSEEP)

            !         Set sedimentation flux and associated volume velocities

            CALL SEDKOL (IWA1, IWA2, IEXPNT, SW, process_space_real, &
                    IPSFLX, INSFLX, IPDM, INDM, &
                    IPSVEL, INSVEL, IPSVOL, INSVOL, TOTSED)

            !         Set burial fluxes and associated velocities
            !             fixed layers: equal to sedimentation
            !             variable layers: constant velocity if Th > MaxTh

            CALL BURKOL (ITOP, IBOT, IEXPNT, SW, process_space_real, &
                    IPACTH, INACTH, IPMXTH, INMXTH, &
                    IPMNTH, INMNTH, &
                    IPDM, INDM, IPBFLX, INBFLX, DELT, &
                    IPBVEL, INBVEL, IPBVOL, INBVOL, VBUR, &
                    IPPORA, INPORA, IPPORI, INPORI, TOTSED)

            !         Apply digging (negative burial) to get to specified
            !             porosity if layer is not dense enough

            CALL DIGKOL (ITOP, IBOT, IEXPNT, DELT, &
                    process_space_real, IPACTH, INACTH, IPDM, INDM, &
                    IPBFLX, INBFLX, IPBVEL, INBVEL, IPPORA, &
                    INPORA, IPPORI, INPORI)

            !         Add INFILTRATION (defined in m/d!)

            DO IQ = IWA1, IBOT

                IF (SEEP < 0.0) THEN
                    !                 Find upwind porosity!
                    ISEG = IEXPNT(1, IQ)
                    PORACT = process_space_real (IPPORA + (ISEG - 1) * INPORA)
                    process_space_real(IPDVEL + (iq - 1) * indvel) = -SEEP / 86400. / PORACT
                    process_space_real(IPDVOL + (iq - 1) * indvol) = -SEEP / 86400.
                ENDIF
            ENDDO
            !
            !         Set resuspension fluxes and associated velocity
            !         Select second column of exchanges for UPWARD advection

            IOFFSE = IBOT - (IWA1 - 1)
            IWA1 = Coll%set(IK)%fstwatsed + IOFFSE
            IWA2 = Coll%set(IK)%lstwatsed + IOFFSE
            ITOP = Coll%set(IK)%topsedsed + IOFFSE
            IBOT = Coll%set(IK)%botsedsed + IOFFSE
            CALL RESKOL (IWA1, IWA2, IEXPNT, SW, process_space_real, &
                    ITOP, IBOT, IPSURF, INSURF, &
                    IPACTH, INACTH, IPMNTH, INMNTH, &
                    IPDM, INDM, IPRFLX, INRFLX, DELT, &
                    IPRVEL, INRVEL, IPRVOL, INRVOL, &
                    IPZRES, INZRES, IPTAU, INTAU, &
                    IPDEPT, INDEPT, IPMDEP, INMDEP, &
                    IPTCRR, INTCRR, IPTFLX, INTFLX, &
                    IPPRES, INPRES, IPSWRE, INSWRE, &
                    IPALPH, INALPH, IPCFLX, INCFLX)

            !         Add seepage

            DO IQ = IWA1, IBOT

                IF (SEEP > 0.0) THEN
                    !                 Find upwind porosity!
                    IF (IQ == IBOT) THEN
                        ISEG = IEXPNT(1, IQ)
                    ELSE
                        ISEG = IEXPNT(2, IQ)
                    ENDIF
                    PORACT = process_space_real (IPPORA + (ISEG - 1) * INPORA)
                    process_space_real(IPDVEL + (iq - 1) * indvel) = -SEEP / 86400. / PORACT
                    process_space_real(IPDVOL + (iq - 1) * indvol) = -SEEP / 86400.
                ENDIF
            ENDDO

        end do

        RETURN
        !
    END
    SUBROUTINE BURKOL (ITOP, IBOT, IEXPNT, SW, process_space_real, &
            IPACTH, INACTH, IPMXTH, INMXTH, &
            IPMNTH, INMNTH, &
            IPDM, INDM, IPBFLX, INBFLX, DELT, &
            IPBVEL, INBVEL, IPBVOL, INBVOL, VBUR, &
            IPPORA, INPORA, IPPORI, INPORI, TOTSED)
        !***********************************************************************
        !
        !     Description of the module :
        !
        !        General water quality module for DELWAQ:
        !        SETS BURIAL FLUXES AND ASSOCIATED VELOCITIES
        !             fixed layers: equal to sedimentation
        !             variable layers: constant velocity if Th > MaxTh
        !
        !
        ! Name    T      L  I/O Description
        ! ----    ---    -   -  ------------
        ! ITOP    I*4    1   I  first exchange number sediment collumn
        ! IBOT    I*4    1   I  last  exchange number sediment collumn
        ! IEXPNT  I*4 4,NOQT I  exchange pointers
        ! SW      R*4    1   I  Switch, > 0.5 then computation changes volume
        ! process_space_real    R*4    *  I/O real input output array whole process system
        ! IPACTH  I*4    1   I  Pointer in process_space_real where actual layer thickness
        ! INACTH  I*4    1   I  Increment of IPACTH in the process_space_real array
        ! IPMXTH  I*4    1   I  Pointer in process_space_real where maximum layer thickness
        ! INMXTH  I*4    1   I  Increment of IPMXTH in the process_space_real array
        ! IPMNTH  I*4    1   I  Pointer in process_space_real where minimum layer thickmess
        ! INMNTH  I*4    1   I  Increment of IPMNTH in the process_space_real array
        ! IPDM    I*4    1   I  Pointer in process_space_real where dry matter mass starts
        ! INDM    I*4    1   I  Increment of IPDM   in the process_space_real array
        ! IPBFLX  I*4    1   I  Pointer in process_space_real where burial flows
        ! INBFLX  I*4    1   I  Increment of IPBFLX in the process_space_real array
        ! DELT    R*4    1   O  Time step size
        ! IPBVEL  I*4    1   I  Pointer in process_space_real where burial velocity
        ! INBVEL  I*4    1   I  Increment of IPBVEL in the process_space_real array
        ! IPBVOL  I*4    1   I  Pointer in process_space_real where burial volume
        ! INBVOL  I*4    1   I  Increment of IPBVOL in the process_space_real array
        ! VBUR    R*4    1   I  Burial velocity
        ! IPPORA  I*4    1   I  Pointer in process_space_real where actual porosity starts
        ! INPORA  I*4    1   I  Increment of IPPORA in the process_space_real array
        ! IPPORI  I*4    1   I  Pointer in process_space_real where given porosity starts
        ! INPORI  I*4    1   I  Increment of IPPORI in the process_space_real array
        ! TOTSED  R*4    1   O  Summed sedimentation flux
        !
        !
        INTEGER(kind = int_wp) :: ITOP, IBOT, &
                IPACTH, INACTH, IPMXTH, INMXTH, IPMNTH, INMNTH, &
                IPDM, INDM, IPBFLX, INBFLX, &
                IPBVEL, INBVEL, IPBVOL, INBVOL, &
                IPPORA, INPORA, IPPORI, INPORI
        REAL(kind = real_wp) :: TOTSED, SW, VBUR, DELT, process_space_real(*)
        INTEGER(kind = int_wp) :: IEXPNT(4, *)

        !     Local variables

        INTEGER(kind = int_wp) :: IQ, IBODEM
        REAL(kind = real_wp) :: TOTBUR, ACTH, MAXTH, DM, MINTH, &
                PORACT, PORINP, CORBUR, MXRCOR
        PARAMETER (MXRCOR = 1.0)
        !JVB  PARAMETER (MXRCOR = 0.5)

        CORBUR = 0.0
        DO IQ = ITOP, IBOT

            !         SW and VBUR are fixed per column
            IBODEM = IEXPNT(1, IQ)
            ACTH = process_space_real (IPACTH + (IBODEM - 1) * INACTH)
            MAXTH = process_space_real (IPMXTH + (IBODEM - 1) * INMXTH)
            MINTH = process_space_real (IPMNTH + (IBODEM - 1) * INMNTH)
            DM = process_space_real (IPDM + (IBODEM - 1) * INDM)
            PORACT = process_space_real (IPPORA + (IBODEM - 1) * INPORA)
            PORINP = process_space_real (IPPORI + (IBODEM - 1) * INPORI)

            !         Compute burial

            IF (SW < 0.5) THEN

                ! ---     First option (fixed layer thickness)
                !         burial flux equals sedimentation flux
                TOTBUR = TOTSED

            ELSE

                ! ---     Second option (variable layer maximum thickness)
                !         burial flux if layer is too thick, velocity in m/d bulk
                TOTBUR = 0.0
                IF (ACTH > MINTH) THEN
                    TOTBUR = VBUR * DM
                ENDIF
                IF (ACTH > MAXTH) THEN
                    TOTBUR = TOTBUR + MAX (0.0, (ACTH - MAXTH) * DM / DELT)
                ENDIF
            ENDIF

            !         Correction for inhomogeneous porosity
            !         formula tries to maintain PORINP
            !         burial if layer is too dense (porosity too low)
            !         the inverse case is treated as DIGGING (see RESKOL)
            !         numerical parameter determines max.rel.corr. per time step

            IF (PORINP > 0.0001) then
                IF (PORACT < PORINP) THEN
                    CORBUR = CORBUR + ACTH / DELT * DM * (PORINP - PORACT) / (1.0 - PORACT) * MXRCOR
                ENDIF
            ENDIF

            !         Flux from segment

            process_space_real(IPBFLX + (IBODEM - 1) * INBFLX) = TOTBUR + CORBUR

            !         Velocity for use in TRASE2

            IF (DM > 1E-20) &
                    process_space_real(IPBVEL + (IQ - 1) * INBVEL) = (TOTBUR + CORBUR) / DM / 86400.

            !         Velocity for volume change, variable thickness ONLY,
            !         correction flux for porosity EXCLUDED
            IF (SW >= 0.5) THEN
                IF (DM > 1E-20) &
                        process_space_real(IPBVOL + (IQ - 1) * INBVOL) = TOTBUR / DM / 86400.
            ENDIF

        end do

        RETURN
    END

    SUBROUTINE RESKOL (IWA1, IWA2, IEXPNT, SW, process_space_real, &
            ITOP, IBOT, IPSURF, INSURF, &
            IPACTH, INACTH, IPMNTH, INMNTH, &
            IPDM, INDM, IPRFLX, INRFLX, DELT, &
            IPRVEL, INRVEL, IPRVOL, INRVOL, &
            IPZRES, INZRES, IPTAU, INTAU, &
            IPDEPT, INDEPT, IPMDEP, INMDEP, &
            IPTCRR, INTCRR, IPTFLX, INTFLX, &
            IPPRES, INPRES, IPSWRE, INSWRE, &
            IPALPH, INALPH, IPCFLX, INCFLX)
        !***********************************************************************
        !
        !     Description of the module :
        !
        !        General water quality module for DELWAQ:
        !        SETS RESUSPENSION FLUXES AND ASSOCIATED VELOCITIES
        !             Operates on second column of exchanges
        !             for the UPWARD advection
        !
        !
        ! Name    T      L  I/O Description
        ! ----    ---    -   -  ------------
        ! IWA1    I*4    1   I  first exchange number water bottom exchange
        ! IWA2    I*4    1   I  first exchange number water bottom exchange
        ! IEXPNT  I*4 4,NOQT I  exchange pointers
        ! SW      R*4    1   I  Switch, > 0.5 then computation changes volume
        ! process_space_real    R*4    *  I/O real input output array whole process system
        ! ITOP    I*4    1   I  first exchange number sediment collumn
        ! IBOT    I*4    1   I  last  exchange number sediment collumn
        ! IPSURF  I*4    1   I  Pointer in process_space_real where
        ! INSURF  I*4    1   I  Increment of IPSURF in the process_space_real array
        ! IPACTH  I*4    1   I  Pointer in process_space_real where actual layer thickness
        ! INACTH  I*4    1   I  Increment of IPACTH in the process_space_real array
        ! IPMNTH  I*4    1   I  Pointer in process_space_real where minimum layer thickmess
        ! INMNTH  I*4    1   I  Increment of IPMNTH in the process_space_real array
        ! IPDM    I*4    1   I  Pointer in process_space_real where dry matter mass starts
        ! INDM    I*4    1   I  Increment of IPDM   in the process_space_real array
        ! IPRFLX  I*4    1   I  Pointer in process_space_real where burial flows
        ! INRFLX  I*4    1   I  Increment of IPRFLX in the process_space_real array
        ! DELT    R*4    1   O  Time step size
        ! IPRVEL  I*4    1   I  Pointer in process_space_real where burial velocity
        ! INRVEL  I*4    1   I  Increment of IPRVEL in the process_space_real array
        ! IPRVOL  I*4    1   I  Pointer in process_space_real where burial volume
        ! INRVOL  I*4    1   I  Increment of IPRVOL in the process_space_real array
        ! IPDEPT  I*4    1   I  Pointer in process_space_real where
        ! INDEPT  I*4    1   I  Increment of IPDEPT in the process_space_real array
        ! IPMDEP  I*4    1   I  Pointer in process_space_real where
        ! INMDEP  I*4    1   I  Increment of IPMDEP in the process_space_real array
        ! IPTCRR  I*4    1   I  Pointer in process_space_real where
        ! INTCRR  I*4    1   I  Increment of IPTCRR in the process_space_real array
        ! IPTFLX  I*4    1   I  Pointer in process_space_real where
        ! INTFLX  I*4    1   I  Increment of IPTFLX in the process_space_real array
        ! IPPREP  I*4    1   I  Pointer in process_space_real where
        ! INPRES  I*4    1   I  Increment of IPPRES in the process_space_real array
        ! IPSWRE  I*4    1   I  Pointer in process_space_real for swith for resuspension
        ! INSWRE  I*4    1   I  Increment of IPPRES in the process_space_real array
        ! IPALPH  I*4    1   I  Pointer in process_space_real for alpha in Rayleigh distribution
        ! INALPH  I*4    1   I  Increment of IPPRES in the process_space_real array
        ! IPCFLX  I*4    1   I  Pointer in process_space_real for communication TDM flux
        ! INCFLX  I*4    1   I  Increment of IPPRES in the process_space_real array
        !
        !
        INTEGER(kind = int_wp) :: IWA1, IWA2, IPZRES, INZRES, IPTAU, INTAU, &
                IPDEPT, INDEPT, IPMDEP, INMDEP, ITOP, IBOT, IPACTH, &
                INACTH, IPMNTH, INMNTH, IPTCRR, INTCRR, IPDM, INDM, &
                IPSURF, INSURF, IPPRES, INPRES, IPRFLX, INRFLX, &
                IPRVEL, INRVEL, IPTFLX, INTFLX, IPRVOL, INRVOL, &
                IPSWRE, INSWRE, IPALPH, INALPH, IPCFLX, INCFLX
        INTEGER(kind = int_wp) :: IEXPNT(4, *)
        REAL(kind = real_wp) :: process_space_real(*), DELT, SW

        !     Local variables

        INTEGER(kind = int_wp) :: IWATER, IQ, IQ2, IBODEM, IBODE2, IQ3
        LOGICAL    GONE
        REAL(kind = real_wp) :: ZRES, DEPTH, MINDEP, TAU, ACTH, MINTH, &
                FLRES, PRES, TCRR, RESTH, RESMX, SURFW, &
                SURFB, ZRESLE, VELRES, DM, DMTOP
        LOGICAL    SW_PARTHENIADES
        real(kind = real_wp) :: TIME_LEFT, ALPHA

        !     Resuspension submodel for DELWAQ-G, equals Restra, bug fixed ZRESLE
        !     AM (august 2018) ZRESLE is not actually used

        !.....Loop over water_sediment_exchanges in kolom
        !     note: more than 1 sediment water exchange may exist on top of
        !           one colun of sediment layers

        DO IQ = IWA1, IWA2

            !         Water-sediment interface, set physical parameters

            IWATER = IEXPNT(1, IQ)

            DEPTH = process_space_real(IPDEPT + (IWATER - 1) * INDEPT)
            MINDEP = process_space_real(IPMDEP + (IWATER - 1) * INMDEP)
            TAU = process_space_real(IPTAU + (IWATER - 1) * INTAU)
            SURFW = process_space_real(IPSURF + (IWATER - 1) * INSURF)

            IBODEM = IEXPNT(2, IQ)

            DMTOP = process_space_real(IPDM + (IBODEM - 1) * INDM)

            !         No resuspensie if depth is below minimum value
            IF (DEPTH < MINDEP) GOTO 20

            !         Underlying sediment-sediment interfaces: ITOP,IBOT
            !         Compute actual resuspension per layer
            !         GONE is TRUE as long as the layer right above is eroded away
            !         ZRESLE is part of resuspension flux left over after eroding the
            !         layer(s) above

            TIME_LEFT = 1.0
            GONE = .TRUE.
            DO IQ2 = ITOP, IBOT

                IBODEM = IEXPNT(1, IQ2)
                DM = process_space_real(IPDM + (IBODEM - 1) * INDM)
                TCRR = process_space_real(IPTCRR + (IBODEM - 1) * INTCRR)
                SURFB = process_space_real(IPSURF + (IBODEM - 1) * INSURF)
                ACTH = process_space_real(IPACTH + (IBODEM - 1) * INACTH)
                MINTH = process_space_real(IPMNTH + (IBODEM - 1) * INMNTH)
                ZRES = process_space_real(IPZRES + (IBODEM - 1) * INZRES)
                SW_PARTHENIADES = NINT(process_space_real(IPSWRE + (IBODEM - 1) * INSWRE)) == 0
                ALPHA = process_space_real(IPALPH + (IBODEM - 1) * INALPH)

                IF (ACTH <= MINTH) THEN
                    !                 Layer was already gone
                    FLRES = 0.0
                    GONE = .TRUE.
                ELSE
                    !                 Layer (still) exists, compute resuspension probability
                    IF (TAU == -1.0) THEN
                        PRES = 1.0
                    ELSE
                        IF (SW_PARTHENIADES) THEN
                            PRES = MAX (0.0, (TAU / TCRR - 1.0))
                        ELSE
                            IF (TAU < 1.E-10) THEN
                                PRES = 0.0
                            ELSE
                                PRES = EXP(-ALPHA * (TCRR / TAU)**2)
                            ENDIF
                        ENDIF
                    ENDIF
                    RESTH = PRES * ZRES * TIME_LEFT
                    RESMX = MAX (0.0, DM * (ACTH - MINTH) * TIME_LEFT / DELT)
                    IF (RESTH > 1E-20 .AND. RESTH >= RESMX) THEN
                        FLRES = RESMX
                        GONE = .TRUE.
                        TIME_LEFT = TIME_LEFT * (1. - RESMX / RESTH)
                    ELSE
                        FLRES = RESTH
                        GONE = .FALSE.
                        TIME_LEFT = 0.0
                    ENDIF
                ENDIF

                !             Resuspension probability (average for sediment column)
                process_space_real(IPPRES + (IBODEM - 1) * INPRES) = &
                        process_space_real(IPPRES + (IBODEM - 1) * INPRES) + PRES * SURFW / SURFB

                IF (FLRES > 1E-20) THEN

                    !                 Net resuspension flux (average for sediment column)
                    process_space_real(IPRFLX + (IBODEM - 1) * INRFLX) = &
                            process_space_real(IPRFLX + (IBODEM - 1) * INRFLX) + FLRES * SURFW / SURFB

                    !                 Resuspension velocity and volume change for
                    !                 sediment-water interface

                    VELRES = FLRES / DMTOP
                    process_space_real(IPRVEL + (IQ - 1) * INRVEL) = &
                            process_space_real(IPRVEL + (IQ - 1) * INRVEL) - VELRES / 86400.
                    IF (SW >= 0.5) THEN
                        process_space_real(IPRVOL + (IQ - 1) * INRVOL) = &
                                process_space_real(IPRVOL + (IQ - 1) * INRVOL) - VELRES / 86400.
                    ENDIF

                    !                 Set total resuspension fluxes
                    !                 set associated velocities in sediment column
                    !                 Depends on fixed (SW = 0) or variable (SW = 1) layers

                    IF (SW >= 0.5) THEN

                        !                     Variable layer thicknes: only layers on top are affected

                        !                     Total resuspension flux, and the communication flux which must be handled the same way
                        DO IQ3 = ITOP, IQ2
                            IBODE2 = IEXPNT(1, IQ3)
                            process_space_real(IPTFLX + (IBODE2 - 1) * INTFLX) = &
                                    process_space_real(IPTFLX + (IBODE2 - 1) * INTFLX) + FLRES
                            process_space_real(IPCFLX + (IBODE2 - 1) * INCFLX) = &
                                    process_space_real(IPCFLX + (IBODE2 - 1) * INCFLX) + FLRES
                        ENDDO

                        !                     Resuspension velocity and volume change
                        DO IQ3 = ITOP, IQ2 - 1
                            IBODE2 = IEXPNT(2, IQ3)
                            DM = process_space_real(IPDM + (IBODE2 - 1) * INDM)
                            VELRES = FLRES / DM
                            process_space_real(IPRVEL + (IQ3 - 1) * INRVEL) = &
                                    process_space_real(IPRVEL + (IQ3 - 1) * INRVEL) - VELRES / 86400.
                            process_space_real(IPRVOL + (IQ3 - 1) * INRVOL) = &
                                    process_space_real(IPRVOL + (IQ3 - 1) * INRVOL) - VELRES / 86400.
                        ENDDO
                    ELSE

                        !                     Fixed layer thicknes: whole column is affected

                        !                     Total resuspension flux, and the communication flux which must be handled the same way
                        DO IQ3 = ITOP, IBOT
                            IBODE2 = IEXPNT(1, IQ3)
                            process_space_real(IPTFLX + (IBODE2 - 1) * INTFLX) = &
                                    process_space_real(IPTFLX + (IBODE2 - 1) * INTFLX) + FLRES
                            process_space_real(IPCFLX + (IBODE2 - 1) * INCFLX) = &
                                    process_space_real(IPCFLX + (IBODE2 - 1) * INCFLX) + FLRES
                        ENDDO

                        !                     Resuspension velocity, relate to density of lower layer,
                        !                     except at the deep sediment boundary!
                        DO IQ3 = ITOP, IBOT
                            IF (IQ3 == IBOT) THEN
                                IBODE2 = IEXPNT(1, IQ3)
                            ELSE
                                IBODE2 = IEXPNT(2, IQ3)
                            ENDIF
                            DM = process_space_real(IPDM + (IBODE2 - 1) * INDM)
                            VELRES = FLRES / DM
                            process_space_real(IPRVEL + (IQ3 - 1) * INRVEL) = &
                                    process_space_real(IPRVEL + (IQ3 - 1) * INRVEL) - VELRES / 86400.
                        ENDDO
                    ENDIF

                ENDIF

                !             Stop als huidige laag niet helemaal weg is!
                IF (.NOT. GONE) GOTO 20

            end do
            20 CONTINUE
        end do

        RETURN
    END

    SUBROUTINE SEDKOL (IQ1, IQ2, IEXPNT, SW, process_space_real, &
            IPSFLX, INSFLX, IPDM, INDM, &
            IPSVEL, INSVEL, IPSVOL, INSVOL, TOTSED)
        !***********************************************************************
        !
        !     Description of the module :
        !
        !        General water quality module for DELWAQ:
        !        SETS SEDIMENTATION FLUX AND ASSOCIATED VOLUME VELOCITIES
        !
        ! Name    T      L  I/O Description
        ! ----    ---    -   -  ------------
        ! IQ1     I*4    1   I  first exchange number water bottom exchange
        ! IQ2     I*4    1   I  last  exchange number water bottom exchange
        ! IEXPNT  I*4 4,NOQT I  exchange pointers
        ! SW      R*4    1   I  Switch, > 0.5 then computation changed volume
        ! process_space_real    R*4    *  I/O real input output array whole process system
        ! IPSFLX  I*4    1   I  Pointer in process_space_real where flows start
        ! INSFLX  I*4    1   I  Increment of IPSFLX in the process_space_real array
        ! IPDM    I*4    1   I  Pointer in process_space_real where dry matter mass starts
        ! INDM    I*4    1   I  Increment of IPDM   in the process_space_real array
        ! IPSVEL  I*4    1   I  Pointer in process_space_real where sedimentation velocity
        ! INSVEL  I*4    1   I  Increment of IPSVEL in the process_space_real array
        ! IPSVOL  I*4    1   I  Pointer in process_space_real where sedimentation volume
        ! INSVOL  I*4    1   I  Increment of IPSVOL in the process_space_real array
        ! TOTSED  R*4    1   O  Summed sedimentation flux
        !     NB IPSVEL and IPSVOL point in the EXCHANGE space
        !        IPSFLX and IPDM   point in the SEGMENT  space
        !
        !
        INTEGER(kind = int_wp) :: IQ1, IQ2, &
                IPSFLX, INSFLX, IPDM, INDM, &
                IPSVEL, INSVEL, IPSVOL, INSVOL
        INTEGER(kind = int_wp) :: IEXPNT(4, *)
        REAL(kind = real_wp) :: SW, TOTSED, process_space_real(*)

        !     Local variables

        INTEGER(kind = int_wp) :: IQ, IVAN, INAAR
        REAL(kind = real_wp) :: SEDFLX, DM, DVOL

        TOTSED = 0.0
        !     Loop over bodem-water uitwisselingen voor huidige kolom
        DO IQ = IQ1, IQ2
            IVAN = IEXPNT(1, IQ)
            INAAR = IEXPNT(2, IQ)
            !         Totale sedimentatieflux
            SEDFLX = process_space_real(IPSFLX + (IVAN - 1) * INSFLX)
            TOTSED = TOTSED + SEDFLX
            !         Sedimentation velocity
            !         Massa in upwind segment
            DM = process_space_real(IPDM + (IVAN - 1) * INDM)
            DVOL = 0.0
            IF (DM > 0.0) DVOL = SEDFLX / DM / 86400.
            process_space_real (IPSVEL + (IQ - 1) * INSVEL) = DVOL

            IF (SW >= 0.5) THEN
                !             Compute volume change velocity
                !             Massa in DOWNwind segment!!!!!!!!!!!!!!!!!!!!!!
                DM = process_space_real(IPDM + (INAAR - 1) * INDM)
                DVOL = 0.0
                IF (DM > 0.0) DVOL = SEDFLX / DM / 86400.
                process_space_real (IPSVOL + (IQ - 1) * INSVOL) = DVOL
            ENDIF
        enddo

        RETURN
    END
    !
    SUBROUTINE MAKKO2 (IEXPNT, IKNMRK, num_exchanges_u_dir, num_exchanges_v_dir, num_exchanges_z_dir, &
            num_exchanges_bottom_dir)
        !***********************************************************************
        !
        !     Description of the module :
        !
        !        General water quality module for DELWAQ:
        !        SETS COLUMN STRUCTURE OF SEDIMENT
        !
        !        Deze module bouwt de kolomstructuur:
        !        -  boven elke kolom bodemsegmenten kunnen meerdere water
        !           segmenten liggen (som AREAs watersegmenten moet gelijk
        !           zijn aan AREA bodemsegmenten)
        !        De pointertabel bevat in de vierde richting blokjes die
        !        er als volgt uitzien:
        !        watersegment A -> bodemsegment 1  \
        !        watersegment B -> bodemsegment 1  | dit kan onbegrensd herhaald
        !        .... etc                          /
        !        bodemsegment 1 -> bodemsegment 2  \
        !        bodemsegment 2 -> bodemsegment 3  | dit kan ook onbegrensd
        !        .... etc                          | herhaald
        !        bodemsegment N -> rand            /
        !
        !        Bodemsegmenten hebben een eerste kenmerk gelijk aan 2!
        !
        !
        ! Name    T      L  I/O Description
        ! ----    ---    -   -  ------------
        ! IEXPNT  I*4 4,NOQT I  exchange pointers for flows
        ! IKNMRK  I*4  num_cells I  feature array
        ! num_exchanges's   I*4    1   I  number of exchanges num_exchanges_bottom_dir is within bottom
        ! Coll    Struct 1   O  Structure with collection of bottom collumn info
        !                  Contains:
        !    type(BotColmn), pointer :: set(:)  ! array with info for all bottom collumns
        !    integer                 :: maxsize ! maximum size of the current array
        !    integer                 :: current_size ! filled up to this size
        ! BotColm Struct 1   O  Structure with bottom collumn info
        !                  Contains:
        !    integer :: fstwatsed  ! first water sediment exchange number
        !    integer :: lstwatsed  ! last  water sediment exchange number
        !    integer :: topsedsed  ! first within collumn exchange number
        !    integer :: botsedsed  ! last exchange of collumn to deeper bnd
        !
        use m_logger_helper, only : stop_with_error, get_log_unit_number
        use m_extract_waq_attribute
        USE BottomSet     !  Module with derived types and add function

        !     type ( BotColmnColl ) :: Coll  <= is defined in the module
        type (BotColmn) :: set   !  makes code more readable

        INTEGER(kind = int_wp) :: IEXPNT(4, *), IKNMRK(*), num_exchanges_u_dir, num_exchanges_v_dir, num_exchanges_z_dir, num_exchanges_bottom_dir

        !     Local variables

        LOGICAL          KOLOM
        logical, save :: FIRST
        INTEGER(kind = int_wp) :: IK, IQ, ivan, inaar, ikmrkv, ikmrkn, &
                lenkol, nkolom, ist, iw1, iw2, isb
        DATA FIRST / .true. /
        INTEGER(kind = int_wp) :: lunrep, errorcode

        call get_log_unit_number(lunrep)
        errorcode = 0

        !     Check for bottom collumns anyway
        if (num_exchanges_bottom_dir == 0) return

        !     Check for first call
        if (.NOT. FIRST) return
        FIRST = .false.
        Coll%current_size = 0
        Coll%maxsize = 0

        !.....Exchangeloop over de verticale richting

        KOLOM = .FALSE.
        DO IQ = num_exchanges_u_dir + num_exchanges_v_dir + num_exchanges_z_dir + 1, num_exchanges_u_dir + num_exchanges_v_dir + num_exchanges_z_dir + num_exchanges_bottom_dir

            IVAN = IEXPNT(1, IQ)
            INAAR = IEXPNT(2, IQ)

            !        Illegal boundary

            IF (IVAN  < 0) GOTO 9004

            !        Zoek eerste kenmerk van- en naar-segmenten

            IKMRKV = -1
            IF (IVAN  > 0) CALL extract_waq_attribute(1, IKNMRK(IVAN), IKMRKV)
            IKMRKN = -1
            IF (INAAR > 0) CALL extract_waq_attribute(1, IKNMRK(INAAR), IKMRKN)

            !        Bottom-water exchange, the collumn starts

            if ((IKMRKV==1 .AND. IKMRKN==3)  .or. &
                    (IKMRKV==0 .AND. IKMRKN==3)) then
                if (.NOT. KOLOM) then   !  first detected
                    KOLOM = .TRUE.
                    IW1 = IQ
                    IST = 0
                endif
            endif

            !        Bottom-bottom exchange, the collumn continues

            if (IKMRKV==3 .AND. IKMRKN==3) then
                if (.NOT. KOLOM) goto 9002
                if (IW1 <= 0) goto 9003
                if (IST == 0) then
                    IW2 = IQ - 1           !  previous was last water exch
                    IST = IQ             !  this is the first bottom exch
                endif
            endif

            !        Deep sediment boundary

            if (INAAR < 0) then
                if (.NOT. KOLOM) goto 9000
                KOLOM = .false.
                if (IW1 < 0) goto 9001
                if (IST < 0) then   ! this is true
                    IW2 = IQ - 1           ! for 1 bottom layer
                    IST = IQ
                endif
                ISB = IQ                 ! last exchange of collumn
                nkolom = BotColmnCollAdd (Coll, IW1, IW2, IST, ISB)
            endif

        end do

        !     Check and remove copies

        if (mod(nkolom, 2) /= 0) goto 9005
        do ik = 1, nkolom, 2
            set = Coll%set(ik)
            lenkol = set%botsedsed - (set%fstwatsed - 1)
            do iq = set%fstwatsed, set%botsedsed
                if (iexpnt(1, iq) /= iexpnt(1, iq + lenkol)) goto 9006
                if (iexpnt(2, iq) /= iexpnt(2, iq + lenkol)) goto 9006
            enddo
            Coll%set((ik + 1) / 2) = set
        enddo
        Coll%current_size = nkolom / 2
        RETURN
        !
        9000 if (errorcode==0) errorcode = 9000
        9001 if (errorcode==0) errorcode = 9001
        9002 if (errorcode==0) errorcode = 9002
        9003 if (errorcode==0) errorcode = 9003
        9004 if (errorcode==0) errorcode = 9004
        9005 if (errorcode==0) errorcode = 9005
        9006 if (errorcode==0) errorcode = 9006
        write (lunrep, *)'Illegal structure of pointer table MAKKOL: ', errorcode
        write (*, *)'Illegal structure of pointer table MAKKOL: ', errorcode
        call stop_with_error()
        !
    END

    SUBROUTINE DIGKOL (ITOP, IBOT, IEXPNT, DELT, &
            process_space_real, IPACTH, INACTH, IPDM, INDM, &
            IPBFLX, INBFLX, IPBVEL, INBVEL, IPPORA, &
            INPORA, IPPORI, INPORI)
        !
        INTEGER(kind = int_wp) :: ITOP, IBOT, IPACTH, INACTH, &
                IPDM, INDM, IPBFLX, INBFLX, &
                IPBVEL, INBVEL, IPPORA, INPORA, &
                IPPORI, INPORI
        REAL(kind = real_wp) :: DELT, process_space_real(*)
        INTEGER(kind = int_wp) :: IEXPNT(4, *)

        !     Local variables

        INTEGER(kind = int_wp) :: IQ, IBODEM
        REAL(kind = real_wp) :: ACTH, DM, PORACT, PORINP, CORDIG, &
                MXRCOR
        PARAMETER (MXRCOR = 1.0)
        !JVB  PARAMETER (MXRCOR = 0.5)

        CORDIG = 0.0
        DO IQ = ITOP, IBOT

            IBODEM = IEXPNT(1, IQ)
            ACTH = process_space_real (IPACTH + (IBODEM - 1) * INACTH)
            DM = process_space_real (IPDM + (IBODEM - 1) * INDM)
            PORACT = process_space_real (IPPORA + (IBODEM - 1) * INPORA)
            PORINP = process_space_real (IPPORI + (IBODEM - 1) * INPORI)
            PORACT = MIN(PORACT, 0.999)

            !         Correction for inhomogeneous porosity
            !         formula tries to maintain PORINP
            !         digging if layer is not dense enough (porosity too HIGH)
            !         numerical parameter determines max.rel.corr. per time step

            IF (PORINP > 0.0001) THEN
                IF (PORACT > PORINP) THEN
                    CORDIG = CORDIG + ACTH / DELT * DM * (PORINP - PORACT) / (1.0 - PORACT) * MXRCOR
                ENDIF
            ENDIF

            !         Flux from segment

            process_space_real(IPBFLX + (IBODEM - 1) * INBFLX) = process_space_real(IPBFLX + (IBODEM - 1) * INBFLX) + CORDIG

            !         Velocity for use in TRASE2

            IF (DM > 1E-20) process_space_real(IPBVEL + (IQ - 1) * INBVEL) = process_space_real(IPBVEL + (IQ - 1) * INBVEL) + CORDIG / DM / 86400.

        ENDDO

        RETURN
    END

end module m_advtra
