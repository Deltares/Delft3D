!!  Copyright (C)  Stichting Deltares, 2021-2024.
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

      subroutine set_aggr_reg(input_hyd, output_hyd, ipnt_h, ipnt_v, ipnt, ipnt_vdf, ipnt_b, nosegbt)

      ! function : set the aggregation pointer for the regular case

      ! (c) DELFT HYDRAULICS

      ! global declarations

      use m_logger_helper, only : stop_with_error
      use m_hydmod
      implicit none

      ! declaration of the arguments

      type(t_hydrodynamics)          :: input_hyd                             ! description of the input hydrodynamics
      type(t_hydrodynamics)          :: output_hyd                            ! description of the output hydrodynamics
      integer              :: ipnt_h(input_hyd%nmax,input_hyd%mmax) ! horizontal aggregation
      integer              :: ipnt_v(input_hyd%kmax)                ! vertical aggregation
      integer              :: ipnt(input_hyd%noseg)                 ! aggregation pointer segments
      integer              :: ipnt_vdf(input_hyd%noseg)             ! aggregation pointer used for minimum vertical diffusion
      integer              :: nosegbt                               ! length aggregation pointer used for boundaries
      integer              :: ipnt_b(nosegbt)                       ! aggregation pointer used for boundaries

      ! local declarations

      integer              :: nosegl_new    ! number of segments per layer in new aggregation
      integer              :: n,m,i         ! loop counters
      integer              :: iseg, isegl   ! segment index
      integer              :: i1, i2        ! segment index
      integer              :: ilay          ! layer index
      integer              :: ilay_new      ! layer index
      integer              :: iplay         ! aggregation pointer for specific layer
      integer              :: nosegb        ! number of old boundaries per layer
      integer              :: nosegb_new    ! number of new boundaries per layer

      nosegl_new = output_hyd%nosegl
      nosegb_new = -minval(ipnt_h)
      nosegb     = nosegbt/input_hyd%nolay
      do i = 1 , input_hyd%noseg
         ipnt(i) = 0
      enddo
      do i = 1 , nosegbt
         ipnt_b(i) = 0
      enddo

      do m = 1 , input_hyd%mmax
         do n = 1 , input_hyd%nmax
            iseg = input_hyd%lgrid(n,m)
            if ( iseg .gt. 0 ) then
               if ( ipnt(iseg) .gt. 0 ) then

                  ! segment is al een keer langs gekomen kijken of het niet gesplitst wordt, verder geen actie

                  if ( ipnt(iseg) .ne. ipnt_h(n,m) ) then
                     write(*,*) 'ERROR: inconsistent aggregation'
                     write(*,*) 'm=:',m
                     write(*,*) 'n=:',n
                     write(*,*) 'iseg=:',iseg
                     write(*,*) 'ipnt=:',ipnt(iseg)
                     write(*,*) 'ipnt_h=:',ipnt_h(n,m)
                     call stop_with_error()
                  endif

               else

                  if ( ipnt_h(n,m) .eq. 0 ) then

                     ! buiten het actieve gebied, geen actie

                  elseif ( ipnt_h(n,m) .lt. 0 ) then

                     ! nieuwe boundary

                     do ilay = 1 , input_hyd%nolay
                        isegl = (ilay-1)*input_hyd%nosegl+iseg
                        iplay = (ipnt_v(ilay)-1)*nosegb_new - ipnt_h(n,m)
                        ipnt(isegl) = -iplay
                     enddo

                  else

                     ! eerste keer dat segment langskomt, zet pointers

                     do ilay = 1 , input_hyd%nolay
                        isegl = (ilay-1)*input_hyd%nosegl+iseg
                        iplay = (ipnt_v(ilay)-1)*nosegl_new + ipnt_h(n,m)
                        ipnt(isegl) = iplay
                     enddo

                  endif

               endif
            elseif ( iseg .lt. 0 ) then
               if ( ipnt_h(n,m) .eq. 0 ) then

                  ! buiten het actieve gebied, geen actie

               elseif ( ipnt_h(n,m) .lt. 0 ) then

                  ! boundary blijft boundary

                  do ilay = 1 , input_hyd%nolay
                     isegl = (ilay-1)*nosegb-iseg
                     iplay = (ipnt_v(ilay)-1)*nosegb_new - ipnt_h(n,m)
                     ipnt_b(isegl) = iplay
                  enddo

               else

                  ! boundary wordt segment, kan niet

                  write(*,*) 'ERROR: boundary becomes segment in aggregation'
                  write(*,*) 'm=:',m
                  write(*,*) 'n=:',n
                  write(*,*) 'iseg=:',iseg
                  write(*,*) 'ipnt_h=:',ipnt_h(n,m)
                  call stop_with_error()

               endif
            endif
         enddo
      enddo

      do ilay = 1 , input_hyd%nolay
         ilay_new = ipnt_v(ilay)
         do i = 1 , input_hyd%nosegl
            i1 = i + (ilay-1)*input_hyd%nosegl
            i2 = i + (ilay_new-1)*input_hyd%nosegl
            ipnt_vdf(i1) = i2
         enddo
      enddo

      return
      end
