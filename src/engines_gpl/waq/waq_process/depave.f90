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
module m_depave
    use m_waq_precision

    implicit none

contains


    subroutine depave (process_space_real, fl, ipoint, increm, num_cells, &
            noflux, iexpnt, iknmrk, num_exchanges_u_dir, num_exchanges_v_dir, &
            num_exchanges_z_dir, num_exchanges_bottom_dir)
        use m_logger_helper, only : stop_with_error, get_log_unit_number
        !>\file
        !>       Average depth for a Bloom time step (typically a day)

        !
        !     Description of the module :
        !
        !     Logical Units : -

        !     Modules called : -

        !     Name     Type   Library
        !     ------   -----  ------------

        REAL(kind = real_wp) :: process_space_real  (*), FL    (*)
        INTEGER(kind = int_wp) :: IPOINT(*), INCREM(*), num_cells, NOFLUX, &
                IEXPNT(4, *), IKNMRK(*), num_exchanges_u_dir, num_exchanges_v_dir, num_exchanges_z_dir, num_exchanges_bottom_dir

        INTEGER(kind = int_wp) :: LUNREP

        INTEGER(kind = int_wp) :: IP1, IP2, IP3, IP4, IP5, IP6, IP7
        REAL(kind = real_wp) :: DEPTH, ADEPTH, DELT, BLOOM_STEP
        INTEGER(kind = int_wp) :: NSWITS, ISEG
        LOGICAL, SAVE ::  FIRST = .TRUE.
        REAL(kind = real_wp), SAVE :: ELAPSED_TIME = 0.0_real_wp

        IP1 = IPOINT(1)
        IP2 = IPOINT(2)
        IP3 = IPOINT(3)
        IP4 = IPOINT(4)
        IP5 = IPOINT(5)
        IP6 = IPOINT(6)
        IP7 = IPOINT(7)

        !     Check whether certain input parameters are independent of X

        IF (FIRST) THEN
            FIRST = .FALSE.
            IF ((INCREM(1) > 0) .OR. &
                    (INCREM(2) > 0)) THEN
                CALL get_log_unit_number(LUNREP)
                WRITE (LUNREP, *) &
                        ' DEPAVE: INPUT parameters function(x) not ALLOWED'
                WRITE (*, *) &
                        ' DEPAVE: INPUT parameters function(x) not ALLOWED'
                CALL stop_with_error()
            ENDIF
        ENDIF

        !     Retrieve switch for averaging and nr. of steps to be averaged

        NSWITS     = NINT(process_space_real(IP1))
        BLOOM_STEP = NINT(process_space_real(IP2))
        DELT       = NINT(process_space_real(IP3))

        !     Add the current time step to the total elapsed time

        ELAPSED_TIME = ELAPSED_TIME + DELT
        IF (ELAPSED_TIME > BLOOM_STEP) ELAPSED_TIME = ELAPSED_TIME - BLOOM_STEP + DELT

        !     Loop over segments

        DO ISEG = 1, num_cells

            IF (BTEST(IKNMRK(ISEG), 0)) THEN

                DEPTH = process_space_real(IP4)
                ADEPTH = process_space_real(IP5)
                process_space_real(IP7) = ADEPTH

                IF (NSWITS == 0) THEN

                    !                 No averaging: copy depth to average depth

                    process_space_real(IP5) = DEPTH

                ELSE

                    !                 Averaging: FANCY FORMULA!!!!!
                    !                 We calculate the running average, it is
                    !                 automatically (!) reset at the start of a new interval

                    process_space_real(IP6) = (ADEPTH * (ELAPSED_TIME - DELT) + DEPTH * DELT) &
                            / ELAPSED_TIME
                ENDIF
            ENDIF
            !
            IP1 = IP1 + INCREM(1)
            IP2 = IP2 + INCREM(2)
            IP3 = IP3 + INCREM(3)
            IP4 = IP4 + INCREM(4)
            IP5 = IP5 + INCREM(5)
            IP6 = IP6 + INCREM(6)
            IP7 = IP7 + INCREM(7)
            !
        end do
        !
        RETURN
        !
    END

end module m_depave
