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
module m_extina
    use m_waq_precision

    implicit none

contains


    subroutine extina (process_space_real, fl, ipoint, increm, num_cells, &
            noflux, iexpnt, iknmrk, num_exchanges_u_dir, num_exchanges_v_dir, &
            num_exchanges_z_dir, num_exchanges_bottom_dir)
        !>\file
        !>       Extinction of light by algae and POC

        REAL(kind = real_wp) :: process_space_real  (*), FL    (*)
        INTEGER(kind = int_wp) :: IPOINT(*), INCREM(*), num_cells, NOFLUX, &
                IEXPNT(4, *), IKNMRK(*), num_exchanges_u_dir, num_exchanges_v_dir, num_exchanges_z_dir, num_exchanges_bottom_dir
        !
        !     Local declarations
        !
        INTEGER(kind = int_wp) :: NALG, ISWFIX, NIPALG, IFLUX, ISEG, &
                IALG, IP, IFIX
        REAL(kind = dp) :: EXTALG, EXTCF, BIOMAS, DEPTH, SDMIX
        !
        NALG = NINT(process_space_real(IPOINT(1)))
        ISWFIX = NINT(process_space_real(IPOINT(2)))
        IF (ISWFIX == 1) THEN
            NIPALG = 4
        ELSE
            NIPALG = 2
        ENDIF
        IFLUX = 0

        DO ISEG = 1, num_cells

            IF (BTEST(IKNMRK(ISEG), 0)) THEN

                EXTALG = 0.0
                DEPTH = process_space_real (IPOINT(3) + (ISEG - 1) * INCREM(3))
                !
                !     Loop over algae

                DO IALG = 1, NALG

                    IP = 3 + IALG
                    EXTCF = process_space_real (IPOINT(IP) + (ISEG - 1) * INCREM(IP))

                    IP = 3 + NALG + IALG
                    BIOMAS = process_space_real (IPOINT(IP) + (ISEG - 1) * INCREM(IP))

                    IF (ISWFIX == 1) THEN
                        IP = 3 + 2 * NALG + IALG
                        IFIX = NINT(process_space_real (IPOINT(IP) + (ISEG - 1) * INCREM(IP)))
                        IF (IFIX < 0) THEN

                            ! Rooted algae, inlclude only if sdmix positive

                            IP = 3 + 3 * NALG + IALG
                            SDMIX = process_space_real (IPOINT(IP) + (ISEG - 1) * INCREM(IP))
                            IF (SDMIX > 1E-10) THEN
                                BIOMAS = BIOMAS / DEPTH
                            ELSE
                                BIOMAS = 0.0
                            ENDIF
                        ENDIF
                    ENDIF

                    IF (BIOMAS > 0.0) &
                            EXTALG = EXTALG + BIOMAS * EXTCF

                end do

                IP = 3 + NIPALG * NALG + 1
                process_space_real (IPOINT(IP) + (ISEG - 1) * INCREM(IP)) = EXTALG

            ENDIF
            !
            IFLUX = IFLUX + NOFLUX
            !
        end do
        !
        RETURN

    END

end module m_extina
