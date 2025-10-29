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
module m_burial_2lb
    use m_waq_precision

    implicit none

contains


    subroutine burial_2lb (process_space_real, fl, ipoint, increm, num_cells, &
            noflux, iexpnt, iknmrk, num_exchanges_u_dir, num_exchanges_v_dir, &
            num_exchanges_z_dir, num_exchanges_bottom_dir)
        use m_extract_waq_attribute

        !>\file
        !>       Compute burial flux from S1 towards S2 (part 6 of the 2-layer bed model)

        !
        !     Description of the module :
        !
        !        General water quality module for DELWAQ:
        !        CALCULATE BURIAL FLUX FROM S1 TOWARDS S2, BURIAL IS ONLY ALLOWED
        !        WHEN THERE IS NET DEPOSITION. NO BURIAL FROM S2 TOWARDS BENEATH
        !
        ! Name    T   L I/O   Description                                    Units
        ! ----    --- -  -    -------------------                            -----
        ! DMS1      R*4 1 I  total amount of dry matter in layer S1       [gDM/m2]
        ! MATTHS1   R*4 1 I  material thickness layer S1                  [m]
        ! WATTHS1   R*4 1 I  water thickness layer S1                     [m]
        ! FSEDIM1   R*4 1 I  sedimentation flux IM1 towards S1            [g/m2/d]
        ! FSEDIM2   R*4 1 I  sedimentation flux IM2 towards S1            [g/m2/d]
        ! FSEDIM3   R*4 1 I  sedimentation flux IM3 towards S1            [g/m2/d]
        ! FRESS1DM  R*4 1 I  total resuspension flux DM from layer S1     [g/m2/d]
        ! FRESS2DM  R*4 1 I  total resuspension flux DM from layer S2     [gDM/m2/d]
        ! RBUR      R*4 1 I  burial rate (from layer S1 to S2)            [-/d]
        ! MAXZHS1   R*4 1 I  maximum material height layer S1             [m]
        ! EQLZHS1   R*4 1 I  equilibrium material height layer S1         [m]
        ! PORS1     R*4 1 I  porosity of sediment layer S1                [-]
        ! PORS2     R*4 1 I  porosity of sediment layer S2                [m3pores/m3bulk]
        ! DELT      R*4 1 I  timestep for processes                       [d]
        ! SURF      R*4 1 I  horizontal surface area of a DELWAQ segment  [m2]
        ! FPOWS1    R*4 1 I  porewater flux in layer S1                   [g/m2/day]
        ! FPOWS2    R*4 1 I  porewater flux in layer S2                   [g/m2/day]
        ! BURS1     R*4 1 O  total burial flux DM from layer S1           [gDM/m2/d]
        ! BURS1     R*4 1 O  total burial flux DM from layer S2           [gDM/m2/d]

        !     Logical Units : -

        !     Modules called : -

        !     Name     Type   Library
        !     ------   -----  ------------

        IMPLICIT REAL (A-H, J-Z)
        IMPLICIT INTEGER (I)

        REAL(kind = real_wp) :: process_space_real  (*), FL    (*)
        INTEGER(kind = int_wp) :: IPOINT(*), INCREM(*), num_cells, NOFLUX, &
                IEXPNT(4, *), IKNMRK(*), num_exchanges_u_dir, num_exchanges_v_dir, num_exchanges_z_dir, num_exchanges_bottom_dir

        INTEGER(kind = int_wp) :: LUNREP
        !
        IP1 = IPOINT(1)
        IP2 = IPOINT(2)
        IP3 = IPOINT(3)
        IP4 = IPOINT(4)
        IP5 = IPOINT(5)
        IP6 = IPOINT(6)
        IP7 = IPOINT(7)
        IP8 = IPOINT(8)
        IP9 = IPOINT(9)
        IP10 = IPOINT(10)
        IP11 = IPOINT(11)
        IP12 = IPOINT(12)
        IP13 = IPOINT(13)
        IP14 = IPOINT(14)
        IP15 = IPOINT(15)
        IP16 = IPOINT(16)
        IP17 = IPOINT(17)
        IP18 = IPOINT(18)
        IP19 = IPOINT(19)
        !
        IN1 = INCREM(1)
        IN2 = INCREM(2)
        IN3 = INCREM(3)
        IN4 = INCREM(4)
        IN5 = INCREM(5)
        IN6 = INCREM(6)
        IN7 = INCREM(7)
        IN8 = INCREM(8)
        IN9 = INCREM(9)
        IN10 = INCREM(10)
        IN11 = INCREM(11)
        IN12 = INCREM(12)
        IN13 = INCREM(13)
        IN14 = INCREM(14)
        IN15 = INCREM(15)
        IN16 = INCREM(16)
        IN17 = INCREM(17)
        IN18 = INCREM(18)
        IN19 = INCREM(19)
        !
        DO ISEG = 1, num_cells

            IF (BTEST(IKNMRK(ISEG), 0)) THEN
                CALL extract_waq_attribute(2, IKNMRK(ISEG), IKMRK2)
                IF ((IKMRK2==0).OR.(IKMRK2==3)) THEN
                    !
                    DMS1     = MAX(0.0, process_space_real(IP1 ))
                    MatThS1  = process_space_real(IP2 )
                    WatThS1  = process_space_real(IP3 )
                    fSedIM1  = process_space_real(IP4 )
                    fSedIM2  = process_space_real(IP5 )
                    fSedIM3  = process_space_real(IP6 )
                    fResS1DM = process_space_real(IP7 )
                    fResS2DM = process_space_real(IP8 )
                    RBUR     = process_space_real(IP9 ) ! unit (-/d)
                    MaxZHS1  = process_space_real(IP10)
                    EqlZHS1  = process_space_real(IP11)
                    PORS1    = process_space_real(IP12)
                    PORS2    = process_space_real(IP13)
                    DELT     = process_space_real(IP14)
                    SURF     = process_space_real(IP15)
                    fPOWS1   = process_space_real(IP16) ! gDM/m2/d
                    fPOWS2   = process_space_real(IP17) ! gDM/m2/d
                
                    !***********************************************************************
                    !**** Processes connected to the BURIAL of dry matter for the 2LB model
                    !***********************************************************************
                
                    ! Dry density of IM in bed (gDM/m3) used for converting height to mass
                    RHOS = 0.260000E+07
                    EqlDMS1 = EqlZHS1 * RHOS
                    MaxDMS1 = MaxZHS1 * RHOS
                    ! Total sedimentation flux
                    fSedTIM = fSedIM1 + fSedIM2 + fSedIM3
                    ! Total dry matter after erosion/deposition
                    DMS1_NEW = (fSedTIM - fResS1DM - fResS2DM) * DELT + DMS1
                
                    ! burial happens when there is net deposition in layer S1
                    IF (DMS1_NEW - DMS1 > 0.0) THEN
                        BURS1_TMP1 = RBUR * MAX(0.0, DMS1_NEW - EqlDMS1) ! gDM/m2/d
                        IF (PORS1 < 0.0) THEN
                            ! empty top layer
                            BURS1_TMP2 = MIN(BURS1_TMP1, (fSedTIM - fResS1DM - fResS2DM)) ! gDM/m2/d
                        ELSE
                            BURS1_TMP2 = MIN(BURS1_TMP1, (fSedTIM - fResS1DM - fResS2DM) + (1.0 - PORS1) * fPOWS1) ! gDM/m2/d
                        ENDIF
                    ELSE
                        BURS1_TMP2 = 0.0
                    ENDIF
                
                    IF ((DMS1_NEW - BURS1_TMP2 * DELT) > MaxDMS1) THEN
                    !   BURS1 = BURS1_TMP + (ZTL_NEW - BURS1_TMP * DELT / RHOS - MaxZHS1) / DELT * RHOS
                        BURS1 = (DMS1_NEW - MaxDMS1) / DELT
                    ELSE
                        BURS1 = BURS1_TMP2
                    ENDIF
                    
                    BURS2 = 0.0 ! no burial from S2 layer
                
                    process_space_real (IP18) = BURS1
                    process_space_real (IP19) = BURS2
                    !
                ENDIF
            ENDIF
            !
            IP1 = IP1 + IN1
            IP2 = IP2 + IN2
            IP3 = IP3 + IN3
            IP4 = IP4 + IN4
            IP5 = IP5 + IN5
            IP6 = IP6 + IN6
            IP7 = IP7 + IN7
            IP8 = IP8 + IN8
            IP9 = IP9 + IN9
            IP10 = IP10 + IN10
            IP11 = IP11 + IN11
            IP12 = IP12 + IN12
            IP13 = IP13 + IN13
            IP14 = IP14 + IN14
            IP15 = IP15 + IN15
            IP16 = IP16 + IN16
            IP17 = IP17 + IN17
            IP18 = IP18 + IN18
            IP19 = IP19 + IN19
            !
        END DO
        !
        RETURN
        !
    END

end module m_burial_2lb
