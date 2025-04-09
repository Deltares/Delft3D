!!  Copyright(C) Stichting Deltares, 2012-2024.
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
module m_mpbtmp
   use m_waq_precision

   implicit none

contains

   subroutine MPBTMP(process_space_real, FL, IPOINT, INCREM, num_cells, &
                     NOFLUX, IEXPNT, IKNMRK, num_exchanges_u_dir, num_exchanges_v_dir, &
                     num_exchanges_z_dir, num_exchanges_bottom_dir)
      !     ***********************************************************************
      !          +----------------------------------------+
      !          |    D E L F T   H Y D R A U L I C S     |
      !          +----------------------------------------+
      !     ***********************************************************************
      !
      !          Function : MPB nutrient limitation function
      !
      !     ***********************************************************************

      implicit none

      !          arguments

      real(kind=real_wp) :: process_space_real(*) ! in/out input-output array space to be adressed with IPOINT/INCREM
      real(kind=real_wp) :: FL(*) ! in/out flux array
      integer(kind=int_wp) :: IPOINT(*) ! in     start index input-output parameters in the process_space_real array (segment or exchange number 1)
      integer(kind=int_wp) :: INCREM(*) ! in     increment for each segment-exchange for the input-output parameters in the process_space_real array
      integer(kind=int_wp) :: num_cells ! in     number of segments
      integer(kind=int_wp) :: NOFLUX ! in     total number of fluxes (increment in FL array)
      integer(kind=int_wp) :: IEXPNT(4, *) ! in     exchange pointer table
      integer(kind=int_wp) :: IKNMRK(*) ! in     segment features array
      integer(kind=int_wp) :: num_exchanges_u_dir ! in     number of exchanges in first direction
      integer(kind=int_wp) :: num_exchanges_v_dir ! in     number of exchanges in second direction
      integer(kind=int_wp) :: num_exchanges_z_dir ! in     number of exchanges in third direction
      integer(kind=int_wp) :: num_exchanges_bottom_dir ! in     number of exchanges in fourth direction

      !          from process_space_real array

      real(kind=real_wp) :: TEMP !  1 in  , ambient water temperature                     (oC)
      real(kind=real_wp) :: KTGP !  2 in  , MPB1 temperature coefficient gross production  (-)
      integer(kind=int_wp) :: ITIME !  3 in  , DELWAQ time                                  (scu)
      integer(kind=int_wp) :: IDT !  4 in  , DELWAQ timestep                              (scu)
      integer(kind=int_wp) :: ITSTRT !  5 in  , DELWAQ start time                            (scu)
      integer(kind=int_wp) :: AUXSYS !  6 in  , ratio between days and system clock        (scu/d)
      real(kind=real_wp) :: FTMP !  7 i/o , MPB temperature function                       (-)
      real(kind=real_wp) :: WS !  8 i/o , workspace MPB temperature function             (-)

      !          local decalrations

      integer(kind=int_wp) :: ISEG ! loop counter segment loop
      integer(kind=int_wp), parameter :: NO_POINTER = 10 ! number of input output variables in process_space_real array
      integer(kind=int_wp) :: IP(NO_POINTER) ! index pointer in process_space_real array updated for each segment
      real(kind=real_wp) :: FTMP_NOW ! actual MPB temperature function                         (-)

      !          initialise pointers for process_space_real and FL array

      IP = IPOINT(1:NO_POINTER)

      !          loop over the segments

      do ISEG = 1, num_cells

         !             input, the workspace and ftmp are input-output only the input pointer is used

         TEMP = process_space_real(IP(1))
         KTGP = process_space_real(IP(2))
         ITIME = nint(process_space_real(IP(3)))
         IDT = nint(process_space_real(IP(4)))
         ITSTRT = nint(process_space_real(IP(5)))
         AUXSYS = nint(process_space_real(IP(6)))
         FTMP = process_space_real(IP(7))
         WS = process_space_real(IP(8))

         FTMP_NOW = KTGP**(TEMP - 20.)

         !             update FTMP every day (AUXSYS is one day)

         if (mod(ITIME - ITSTRT, AUXSYS) < IDT) then
            if (ITIME == ITSTRT) then
               FTMP = FTMP_NOW
            else
               FTMP = WS / AUXSYS
            end if
            WS = 0.0
         end if

         !             cummulate in workspace

         WS = WS + FTMP_NOW * IDT

         !             output

         process_space_real(IP(7)) = FTMP
         process_space_real(IP(8)) = WS

         !             update pointering in process_space_real

         IP = IP + INCREM(1:NO_POINTER)

      end do

      return
   end

end module m_mpbtmp
