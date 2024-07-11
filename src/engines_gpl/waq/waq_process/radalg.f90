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
module m_radalg
    use m_waq_precision

    implicit none

contains


    subroutine RADALG     (process_space_real, fl, ipoint, increm, num_cells, &
            noflux, iexpnt, iknmrk, num_exchanges_u_dir, num_exchanges_v_dir, &
            num_exchanges_z_dir, num_exchanges_bottom_dir)
        !>\file
        !>       Light efficiency function DYNAMO algae
        !>
        !>              Routine is called per algae species with species specific coefficients
        !>              Function returns 1.0 if:
        !>              - both light at top of cell and at bottom of cell are larger than saturation
        !>              - saturation value is zero
        !>              If light within the cell becomes below the saturation value, then for
        !>              that light a linear dependency between 0.0 and the saturation value is
        !>              assumed: mu(I) = I/Isat for I < Isat and 1.0 for I > Isat.\n
        !>              This (partly) linear lightcurve is analytically integrated over depth for
        !>              the given extinction coefficient.\n
        !>              If the extinction coefficient or the water depth is zero, like more or less
        !>              in a chemostat, then the function value for the given amount of light
        !>              is returned: either 1.0 or a value on the slope.

        implicit none

        !     Type    Name         I/O Description

        real(kind = real_wp) :: process_space_real(*)     !I/O Process Manager System Array, window of routine to process library
        real(kind = real_wp) :: fl(*)       ! O  Array of fluxes made by this process in mass/volume/time
        integer(kind = int_wp) :: ipoint(6) ! I  Array of pointers in process_space_real to get and store the data
        integer(kind = int_wp) :: increm(6) ! I  Increments in ipoint for segment loop, 0=constant, 1=spatially varying
        integer(kind = int_wp) :: num_cells       ! I  Number of computational elements in the whole model schematisation
        integer(kind = int_wp) :: noflux      ! I  Number of fluxes, increment in the fl array
        integer(kind = int_wp) :: iexpnt(4, *) ! I  From, To, From-1 and To+1 segment numbers of the exchange surfaces
        integer(kind = int_wp) :: iknmrk(*)   ! I  Active-Inactive, Surface-water-bottom, see manual for use
        integer(kind = int_wp) :: num_exchanges_u_dir        ! I  Nr of exchanges in 1st direction (the horizontal dir if irregular mesh)
        integer(kind = int_wp) :: num_exchanges_v_dir        ! I  Nr of exchanges in 2nd direction, num_exchanges_u_dir+num_exchanges_v_dir gives hor. dir. reg. grid
        integer(kind = int_wp) :: num_exchanges_z_dir        ! I  Nr of exchanges in 3rd direction, vertical direction, pos. downward
        integer(kind = int_wp) :: num_exchanges_bottom_dir        ! I  Nr of exchanges in the bottom (bottom layers, specialist use only)
        integer(kind = int_wp) :: ipnt(6)   !    Local work array for the pointering
        integer(kind = int_wp) :: iseg        !    Local loop counter for computational element loop

        !***********************************************************************
        !
        !     Project : STANDAARDISATIE PROCES FORMULES T721.72
        !     Author  : Pascal Boderie
        !     Date    : 921210             Version : 0.01
        !
        !     History :
        !
        !     Date      Author          Description
        !     --------  --------------  --------------------------------------
        !     19921229  Pascal Boderie  Add third algae type, nutrient ratio's
        !                               per species
        !     20140530  Leo Postma      Allow for zero depth, zero extinction
        !                               and/or zero RadSat
        !
        !***********************************************************************

        !     Type    Name         I/O Description                                         Unit

        real(kind = real_wp) :: Depth       ! I  depth of segment                                    (m)
        real(kind = real_wp) :: Rad         ! I  irradiation at the segment upper-boundary           (W/m2)
        real(kind = real_wp) :: RadSat      ! I  total radiation growth saturation for this species  (W/m2)
        real(kind = real_wp) :: ExtVl       ! I  total extinction coefficient visible light          (1/m)
        real(kind = real_wp) :: TFGro       ! I  temperature function growth of this species <0-1>   (-)
        real(kind = real_wp) :: LimRad      ! O  radiation limitation function of this species <0-1> (-)
        logical LgtOpt      !    False if RadSat, Frad and LnFrad are equal for all cells
        real(kind = real_wp) :: Frad        !    Saturation fraction: < 1.0 is under-saturation      (-)
        real(kind = real_wp) :: LnFrad      !    natural logarithm of Frad                           (-)
        real(kind = real_wp) :: ExtDpt      !    product of extinction and depth                     (-)
        real(kind = real_wp) :: RadBot      !    radiation at the bottom of the cell                 (-)

        ipnt = ipoint

        LgtOpt = .true.
        if (increm(2) == 0 .and. increm(3) == 0 .and. increm(5) == 0) then
            LgtOpt = .false.             !  This is constant for all cells
            Rad = process_space_real(ipnt(2))
            RadSat = process_space_real(ipnt(3))
            TFGro = process_space_real(ipnt(5))
            RadSat = TFGro * RadSat      !  Correct RadSat for temperature
            if (RadSat > 1e-20) then
                Frad = Rad / RadSat
                LnFrad = 0.0
                if (Rad > 1E-5) LnFrad = Log (Frad)
            endif
        endif

        do iseg = 1, num_cells

            if (btest(iknmrk(iseg), 0)) then

                if (LgtOpt) then
                    Rad = process_space_real(ipnt(2))
                    RadSat = process_space_real(ipnt(3))
                    TFGro = process_space_real(ipnt(5))
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
                    Depth = process_space_real(ipnt(1))
                    ExtVl = process_space_real(ipnt(4))
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

                process_space_real(ipnt(6)) = LimRad

            endif

            ipnt = ipnt + increm

        end do

        return
    end subroutine

end module m_radalg
