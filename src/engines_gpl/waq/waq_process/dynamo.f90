!!  Copyright (C)  Stichting Deltares, 2012-2026.
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
module m_dynamo
    use m_waq_precision

    implicit none

contains


    subroutine dynamo (process_space_real, fl, ipoint, increm, num_cells, &
            noflux, iexpnt, iknmrk, num_exchanges_u_dir, num_exchanges_v_dir, &
            num_exchanges_z_dir, num_exchanges_bottom_dir)
        use m_logger_helper

        !>\file
        !>       Nett primary production and mortality DYNAMO algae

        !
        !     Description of the module :
        !
        ! Name    T   L I/O   Description                                   Unit
        ! ----    --- -  -    -------------------                            ---
        ! DL      R*4 1 I daylength for growth saturation green-algae          [
        ! EFF     R*4 1 L average light efficiency green-algae                 [
        ! FNUT    R*4 1 L nutrient limitation function green-algae             [
        ! PPMAX1  R*4 1 I pot. max. pr. prod. rc. green-algae (st.temp)      [1/
        ! process_space_real    R*4 1 L Gross act. pr. prod. rc. green-algae               [1/
        ! TFUNG1  R*4 1 L temp. function for growth processes green            [

        !     Logical Units : -

        !     Modules called : -

        !     Name     Type   Library
        !     ------   -----  ------------

        IMPLICIT REAL    (A-H, J-Z)
        IMPLICIT INTEGER (I)

        REAL(kind = real_wp) :: process_space_real  (*), FL    (*)
        INTEGER(kind = int_wp) :: IPOINT(*), INCREM(*), num_cells, NOFLUX, &
                IEXPNT(4, *), IKNMRK(*), num_exchanges_u_dir, num_exchanges_v_dir, num_exchanges_z_dir, num_exchanges_bottom_dir
        !
        !     Local declaration
        !
		LOGICAL  TMPOPT
		logical LgtOpt      !    False if RadSat, Frad and LnFrad are equal for all cells

        integer(kind = int_wp) :: iseg
        REAL(kind = real_wp) :: ALGMIN
        INTEGER(kind = int_wp) :: NR_MES
        SAVE     NR_MES
        DATA     NR_MES / 0 /
        !
        CALL get_log_unit_number(ILUMON)
		
		ipnt  = ipoint
		iflux = 0

        
		! from TF_green （tfalg.f90）
		IF (increm(1) == 0 .AND. increm(2) == 0 .AND. increm(3) == 0) THEN
			TEMP = process_space_real(ipnt(1))
			TCG = process_space_real(ipnt(2))
			TCM = process_space_real(ipnt(3))
			TEMP20 = TEMP - 20.
            TFG = TCG**TEMP20
            TFM = TCM**TEMP20
            TMPOPT = .FALSE.
        ELSE
            TMPOPT = .TRUE.
        ENDIF
		
		! from RAD_green （RADALG.f90）Light efficiency function green algae DYNAMO	
        LgtOpt = .true.
        if (increm(15) == 0 .and. increm(16) == 0 ) then
            LgtOpt = .false.             !  This is constant for all cells
            Rad = process_space_real(ipnt(15))
            RadSat = process_space_real(ipnt(16))
            !TFGro = process_space_real(ipnt(28))
			TFGro = TFG
            RadSat = TFGro * RadSat      !  Correct RadSat for temperature
            if (RadSat > 1e-20) then
                Frad = Rad / RadSat
                LnFrad = 0.0
                if (Rad > 1E-5) LnFrad = Log (Frad)
            endif
        endif		
		
		
        DO ISEG = 1, num_cells

            IF (BTEST(IKNMRK(ISEG), 0)) THEN
                !
				! from TF_green （tfalg.f90）
				!
                IF (TMPOPT) THEN
                    TEMP = process_space_real(ipnt(1))
                    TCG = process_space_real(ipnt(2))
                    TCM = process_space_real(ipnt(3))
                    TEMP20 = TEMP - 20.
                    !     Algal temp. functions for growth (G) and mortality (M) processes
                    TFG = TCG**TEMP20
                    TFM = TCM**TEMP20
                ENDIF
                !     Uitvoer limiterende factoren
                process_space_real(ipnt(28)) = TFG
                process_space_real(ipnt(29)) = TFM
				
				
                !
				! from DL_green （dlalg.f90）Daylength function for algae DYNAMO
				!				
				DayL = process_space_real(ipnt(4))              ! daylength <0-1> in (d) 
				OptDLGreen = process_space_real(ipnt(5))   		! daylength for growth saturation Greens (d) 

				IF (DayL < 1E-20)  CALL write_error_message ('DayL in DLALG zero')

				!     Actueel licht / licht voor groei verzadiging
				LimDLGreen = MIN (DayL, OptDLGreen) / OptDLGreen  
				process_space_real(ipnt(30)) = LimDLGreen
				
				!
				! from NLgreen （nlalg.f90）Nutrient limiation function for green algae DYNAMO
				!
				AMOPRF = process_space_real(ipnt(6))
                KMDIN  = process_space_real(ipnt(7))
                KMP    = process_space_real(ipnt(8))
                KMSI   = process_space_real(ipnt(9))
                NH4    = process_space_real(ipnt(10))
                NO3    = process_space_real(ipnt(11))
                PO4    = process_space_real(ipnt(12))
                SI     = process_space_real(ipnt(13))

                IF (AMOPRF < 1E-20)  CALL write_error_message ('AMOPRF in NLALG zero')

                !     Calculation of available dissolved N (NO3 corrected with AMOPRF)
                DIN = NO3 / AMOPRF + NH4
                IF ((NO3 < 0.0) .OR. (NH4 < 0.0)) DIN = 0.0

                !     Nutrient limitation functions (MONOD)
                FN = DIN / (DIN + KMDIN)

                IF (PO4 < 0.0) THEN
                    FP = 0.0
                ELSE
                    FP = PO4 / (PO4 + KMP)
                ENDIF

                IF  (KMSI == -1.0) THEN
                    FS = 1.0
                ELSEIF (SI < 0.0)  THEN
                    FS = 0.0
                ELSE
                    FS = SI / (SI + KMSI)
                ENDIF

                FNUT = MIN (FN, FP, FS)
                !@    Uitvoer limiterende factoren
                process_space_real (ipnt(31)) = FN
                process_space_real (ipnt(32)) = FP
                process_space_real (ipnt(33)) = FS
                process_space_real (ipnt(34)) = FNUT
				
                !
				! from RAD_green （RADALG.f90）Light efficiency function green algae DYNAMO	
				!
				
				if (LgtOpt) then
                    Rad    = process_space_real(ipnt(15))
                    RadSat = process_space_real(ipnt(16))
                    !TFGro  = process_space_real(ipnt(28))
					TFGro = TFG
                    RadSat = TFGro * RadSat
                    if (RadSat > 1e-20) then
                        Frad = Rad / RadSat
                        LnFrad = 0.0
                        if (Rad > 1E-5) LnFrad = Log (Frad)
                    endif
                endif

                if (RadSat <= 1e-20) then
                    LimRad = 1.0
                else
                    Depth = process_space_real(ipnt(14))
                    ExtVl = process_space_real(ipnt(17))
                    ExtDpt = ExtVl * Depth
                    if (ExtDpt <= 1.0e-10) then    !  No extinction, e.g. chemostat
                        LimRad = min(Frad, 1.0)
                    else
                        RadBot = Frad * exp(- ExtDpt)
                        if (Frad > 1.0) then       !  Saturation at the surface of the cell
                            if (RadBot > 1.0) then
                                LimRad = 1.0
                            else
                                LimRad = (1.0 + LnFrad - RadBot) / ExtDpt
                            endif
                        else
                            LimRad = (Frad - RadBot) / ExtDpt
                        endif
                    endif
                endif
				process_space_real(ipnt(35)) = LimRad


				EFF = LimRad
				ALG = process_space_real(ipnt(18))
                IF (ALG < 0.0) THEN
                    IF (NR_MES < 25) THEN
                        NR_MES = NR_MES + 1
                        WRITE (ILUMON, *) 'WARNING :negative algae correction', &
                                ' segment=', ISEG, ' conc=', ALG
                    ENDIF
                    IF (NR_MES == 25) THEN
                        NR_MES = NR_MES + 1
                        WRITE(ILUMON, *) ' 25 WARNINGS on negative algae'
                        WRITE(ILUMON, *) ' Further messages on algae surpressed'
                    ENDIF
                    ALG = 0.0
                ENDIF
                
				PPMAX  = process_space_real(ipnt(19))
                MRESP  = process_space_real(ipnt(20))
                GRESP  = process_space_real(ipnt(21))
                MORT0  = process_space_real(ipnt(22))
                MORTS  = process_space_real(ipnt(23))
                SAL1   = process_space_real(ipnt(24))
                SAL2   = process_space_real(ipnt(25))
                SAL    = process_space_real(ipnt(26))
                ALGMIN = process_space_real(ipnt(27))
                ACTMOR = MORT0

                !     Mortality coefficient depends on salinity
                !     Value for low salinity is MORT0
                !     Value for high salinity is MORTS
                !     Linear transition from MORT0 to MORTS
                !        between SAL1 and SAL2

                IF (SAL1 > 0.0 .AND. SAL2 > SAL1) THEN
                    IF (SAL <= SAL1) THEN
                        ACTMOR = MORT0
                    ELSEIF (SAL >= SAL2) THEN
                        ACTMOR = MORTS
                    ELSE
                        ACTMOR = MORT0 + (SAL - SAL1) / (SAL2 - SAL1) * (MORTS - MORT0)
                    ENDIF
                ENDIF

                !     Gross primary production
                PPROD = DL * EFF * FNUT * TFUNG * PPMAX

                !     The respiration does not include excretion!!
                !     The proces formulation used here does not release nutrients due
                !     to respiration, but reduces the uptake of nutrients.
                !     Respiration = maintainance part + growth part
                RESP = MRESP * TFUNM + GRESP * (PPROD - MRESP * TFUNM)

                !     Nett primary production
                FL (1 + IFLUX) = (PPROD - RESP) * ALG

                !     Mortality, including processes as autolysis and zooplankton 'graas
                FL (2 + IFLUX) = ACTMOR * TFUNM * MAX(ALG - ALGMIN, 0.0)

                process_space_real (ipnt(36)) = PPROD - RESP
                process_space_real (ipnt(37)) = ACTMOR * TFUNM
                process_space_real (ipnt(38)) = RESP
                process_space_real (ipnt(39)) = (PPROD - RESP) * ALG
                process_space_real (ipnt(40)) = ACTMOR * TFUNM * MAX(ALG - ALGMIN, 0.0)
				
            ENDIF
            !
            ipnt  = ipnt + increm						!perference.
			iflux = iflux + noflux
        END DO
        RETURN  
    END

end module m_dynamo
