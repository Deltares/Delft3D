!!  Copyright (C)  Stichting Deltares, 2021-2025.
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

      subroutine agr_atr(input_hyd , ipnt, output_hyd)

      ! function : initialise aggregation, time independent data

      use m_logger_helper
      use m_hydmod
      implicit none

      ! declaration of the arguments

      type(t_hydrodynamics)          :: input_hyd             ! description of the input hydrodynamics
      integer              :: ipnt(input_hyd%num_cells) ! aggregation pointer segments
      type(t_hydrodynamics)          :: output_hyd            ! description of the output hydrodynamics

      ! local declarations

      integer, parameter  :: iatr_miss = -999       ! missing value for attribute
      integer             :: iseg1                  ! segment index input
      integer             :: iseg2                  ! segment index output
      integer             :: lunrep                 ! unit number report file
      integer             :: ik                     ! combined attribute
      integer             :: ik1                    ! combined 1st attribute
      integer             :: ik2                    ! combined 2nd attribute
      integer             :: ik1_o                  ! output 1st attribute
      integer             :: ik2_o                  ! output 2nd attribute
      integer             :: ik1_i                  ! input 1st attribute
      integer             :: ik2_i                  ! input 2nd attribute
      logical             :: surf                   ! aggregated segment contains surface
      logical             :: bottom                 ! aggregated segment contains bottom
      logical             :: surf_o                 ! o segment contains surface
      logical             :: bottom_o               ! o segment contains bottom
      logical             :: surf_i                 ! i segment contains surface
      logical             :: bottom_i               ! i segment contains bottom
      integer             :: ilay                   ! loop counter layers
      integer             :: iseg_u                 ! upper segment index input
      integer             :: iseg_d                 ! lower segment index output
      integer             :: ik1_u                  ! upper 1st attribute
      integer             :: ik2_u                  ! upper 2nd attribute
      integer             :: ik1_d                  ! down 1st attribute
      integer             :: ik2_d                  ! down 2nd attribute
      logical             :: surf_u                 ! u segment contains surface
      logical             :: bottom_u               ! u segment contains bottom
      logical             :: surf_d                 ! d segment contains surface
      logical             :: bottom_d               ! d segment contains bottom

      ! initialise

      do iseg2 = 1 , output_hyd%num_cells
         output_hyd%attributes(iseg2) = iatr_miss
      enddo

      ! copy the values

      output_hyd%atr_type = input_hyd%atr_type
      output_hyd%no_atr   = input_hyd%no_atr

      do iseg1 = 1 , input_hyd%num_cells
         iseg2 = ipnt(iseg1)
         if ( iseg2 .gt. 0 ) then
            if ( output_hyd%attributes(iseg2) .eq. iatr_miss ) then
               output_hyd%attributes(iseg2) = input_hyd%attributes(iseg1)
            else
               if ( output_hyd%attributes(iseg2) .ne. input_hyd%attributes(iseg1) ) then
                  call get_log_unit_number(lunrep)
                  write(lunrep,1000) iseg1,input_hyd%attributes(iseg1),iseg2,output_hyd%attributes(iseg2)
               endif

               ! merge

               ik1_o = mod(output_hyd%attributes(iseg2),10)
               ik1_i = mod(input_hyd%attributes(iseg1),10)
               if ( ik1_o .eq. 1 .or. ik1_i .eq. 1 ) then
                  ik1 = 1
               else
                  ik1 = 0
               endif

               ik2_o = mod(output_hyd%attributes(iseg2),100)/10
               ik2_i = mod(input_hyd%attributes(iseg1),100)/10
               if ( ik1_o .eq. 1 .and. ik1_i .eq. 1 ) then

                  surf_o   = .false.
                  bottom_o = .false.
                  if ( ik2_o .eq. 0 ) surf_o = .true.
                  if ( ik2_o .eq. 1 ) surf_o = .true.
                  if ( ik2_o .eq. 0 ) bottom_o = .true.
                  if ( ik2_o .eq. 3 ) bottom_o = .true.

                  surf_i   = .false.
                  bottom_i = .false.
                  if ( ik2_i .eq. 0 ) surf_i = .true.
                  if ( ik2_i .eq. 1 ) surf_i = .true.
                  if ( ik2_i .eq. 0 ) bottom_i = .true.
                  if ( ik2_i .eq. 3 ) bottom_i = .true.

                  surf   = .false.
                  bottom = .false.
                  if ( surf_o   .or. surf_i   ) surf   = .true.
                  if ( bottom_o .or. bottom_i ) bottom = .true.

                  if ( surf .and. bottom ) then
                     ik2 = 0
                  elseif ( surf ) then
                     ik2 = 1
                  elseif ( bottom ) then
                     ik2 = 3
                  else
                     ik2 = 2
                  endif

               else
                  if ( ik1_o .eq. 1 ) then
                     ik2 = ik2_o
                  else
                     ik2 = ik2_i
                  endif
               endif

               ik = ik2*10+ik1
               output_hyd%attributes(iseg2) = ik

            endif
         endif
      enddo

      ! only one bottom possible

      do iseg2 = 1 , output_hyd%nosegl
         do ilay = 1 , output_hyd%num_layers - 1
            iseg_u = (ilay-1)*output_hyd%nosegl + iseg2
            iseg_d = ilay*output_hyd%nosegl + iseg2
            ik1_u = mod(output_hyd%attributes(iseg_u),10)
            ik1_d = mod(output_hyd%attributes(iseg_d),10)
            ik2_u = mod(output_hyd%attributes(iseg_u),100)/10
            ik2_d = mod(output_hyd%attributes(iseg_d),100)/10

            surf_u   = .false.
            bottom_u = .false.
            if ( ik2_u .eq. 0 ) surf_u = .true.
            if ( ik2_u .eq. 1 ) surf_u = .true.
            if ( ik2_u .eq. 0 ) bottom_u = .true.
            if ( ik2_u .eq. 3 ) bottom_u = .true.

            surf_d   = .false.
            bottom_d = .false.
            if ( ik2_d .eq. 0 ) surf_d = .true.
            if ( ik2_d .eq. 1 ) surf_d = .true.
            if ( ik2_d .eq. 0 ) bottom_d = .true.
            if ( ik2_d .eq. 3 ) bottom_d = .true.

            if ( ik1_u .eq. 1 .and. ik1_d .eq. 1 ) then
               if ( bottom_u ) then
                  if ( surf_u ) then
                     output_hyd%attributes(iseg_u) = 11
                  else
                     output_hyd%attributes(iseg_u) = 21
                  endif
               endif
            endif
         enddo
      enddo

      return
 1000 format (' merging attribute of old segment ',I8,' (',I3,') into new segment ',I8,' (',I3,')')
      end
