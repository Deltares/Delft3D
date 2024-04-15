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

      subroutine set_aggr_pnts(input_hyd, ipnt_h , ipnt_v    , ipnt     , ipnt_vdf, & 
                              ipnt_b   , nosegbt, output_hyd, l_regular, ipnt_tau, & 
                              l_expand , lunrep)

      ! function : set the aggregation pointer

      ! (c) DELFT HYDRAULICS

      ! global declarations

      use m_srstop
      use m_hydmod
      implicit none

      ! declaration of the arguments

      type(t_hydrodynamics)          :: input_hyd                             ! description of the input hydrodynamics
      integer              :: ipnt_h(input_hyd%nmax,input_hyd%mmax) ! horizontal aggregation
      integer              :: ipnt_v(input_hyd%kmax)                ! vertical aggregation
      integer              :: ipnt(input_hyd%noseg)                 ! aggregation pointer segments
      integer              :: ipnt_vdf(input_hyd%noseg)             ! aggregation pointer used for minimum vertical diffusion
      integer              :: nosegbt                               ! length aggregation pointer used for boundaries
      integer              :: ipnt_b(nosegbt)                       ! aggregation pointer used for boundaries
      type(t_hydrodynamics)          :: output_hyd                            ! description of the output hydrodynamics
      logical              :: l_regular                             ! regular aggregartion option
      integer              :: ipnt_tau(input_hyd%noseg)             ! aggregation pointer used for tau
      logical              :: l_expand                              ! expand to full matrix
      integer              :: lunrep                                ! unit number report file

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
      integer              :: isegb         ! boundary number index
      integer              :: ik2           ! second attribute
      logical              :: bottom        ! indicates if the segment has a bottom

      if ( l_regular ) then
         nosegl_new = output_hyd%nosegl
      elseif ( l_expand ) then
         nosegl_new = input_hyd%mmax*input_hyd%nmax
      else
         nosegl_new = maxval(ipnt_h)
      endif
      do i = 1 , input_hyd%noseg
         ipnt(i) = 0
      enddo
      if (input_hyd%geometry .eq. HYD_GEOM_CURVI) then
         nosegb_new = -minval(ipnt_h)
         nosegb     = nosegbt/input_hyd%nolay
         do i = 1 , nosegbt
            ipnt_b(i) = 0
         enddo
      else if (input_hyd%geometry .eq. HYD_GEOM_UNSTRUC) then
         nosegb     = nosegbt/input_hyd%nolay
         nosegb_new = nosegb
          do isegb = 1 , nosegb
              do ilay = 1 , input_hyd%nolay
                  isegl = (ilay-1)*nosegb+isegb
                  iplay = (ipnt_v(ilay)-1)*nosegb_new+isegb
                  ipnt_b(isegl) = iplay
              enddo
          enddo
      endif
      
      do m = 1 , input_hyd%mmax
         do n = 1 , input_hyd%nmax
            iseg = input_hyd%lgrid(n,m)
            if ( iseg .gt. 0 ) then
               if ( ipnt(iseg) .gt. 0 ) then
                  ! This segment number has been seen before, so the data has already been aggregated.
                  ! Check if this segment is aggregated into the same segment too, because it can not be split
                  if ( ipnt(iseg) .ne. ipnt_h(n,m) ) then
                     write(lunrep,*) 'ERROR: inconsistent aggregation: two segments that were aggregated'
                     write(lunrep,*) '       are seperated in the aggregation.'
                     write(lunrep,*) '       check the validity of your dwq-file!'
                     write(lunrep,*) 'm coordinate                                      = ',m
                     write(lunrep,*) 'n coordinate                                      = ',n
                     write(lunrep,*) 'segment number before aggregation                 = ',iseg
                     write(lunrep,*) 'segment number in aggregation                     = ',ipnt_h(n,m)
                     write(lunrep,*) 'the same segment number was already aggregated to = ',ipnt(iseg)
                     write(*,*) 'ERROR: inconsistent aggregation: two segments that were aggregated'
                     write(*,*) '       are seperated in the aggregation.'
                     write(*,*) '       check the validity of your dwq-file!'
                     write(*,*) 'm coordinate                                      = ',m
                     write(*,*) 'n coordinate                                      = ',n
                     write(*,*) 'segment number before aggregation                 = ',iseg
                     write(*,*) 'segment number in aggregation                     = ',ipnt_h(n,m)
                     write(*,*) 'the same segment number was already aggregated to = ',ipnt(iseg)
                     call srstop(1)
                  endif
               else
                  if ( ipnt_h(n,m) .eq. 0 ) then
                     ! outside the active area, no action
                  elseif ( ipnt_h(n,m) .lt. 0 ) then
                     ! new boundary
                     do ilay = 1 , input_hyd%nolay
                        isegl = (ilay-1)*input_hyd%nosegl+iseg
                        iplay = (ipnt_v(ilay)-1)*nosegb_new - ipnt_h(n,m)
                        ipnt(isegl) = -iplay
                     enddo
                  else
                     ! first time that we see this segment, set pointers over all layers
                     do ilay = 1 , input_hyd%nolay
                        isegl = (ilay-1)*input_hyd%nosegl+iseg
                        iplay = (ipnt_v(ilay)-1)*nosegl_new + ipnt_h(n,m)
                        ipnt(isegl) = iplay
                     enddo
                  endif
               endif
            elseif ( iseg .lt. 0 ) then
               if ( ipnt_h(n,m) .eq. 0 ) then
                  ! outside the active area, no action
               elseif ( ipnt_h(n,m) .lt. 0 ) then
                  ! boundary stays boundary
                  do ilay = 1 , input_hyd%nolay
                     isegl = (ilay-1)*nosegb-iseg
                     iplay = (ipnt_v(ilay)-1)*nosegb_new - ipnt_h(n,m)
                     ipnt_b(isegl) = iplay
                  enddo
               else
                  ! boundary becomes a segment, not possible
                  write(lunrep,*) 'ERROR: boundary becomes a segment in the aggregation.'
                  write(lunrep,*) '       check the validity of your dwq-file!'
                  write(lunrep,*) 'm coordinate                                      = ',m
                  write(lunrep,*) 'n coordinate                                      = ',n
                  write(lunrep,*) 'segment number before aggregation                 = ',iseg
                  write(lunrep,*) 'segment number in aggregation                     = ',ipnt_h(n,m)
                  write(*,*) 'ERROR: boundary becomes a segment in the aggregation.'
                  write(*,*) '       check the validity of your dwq-file!'
                  write(*,*) 'm coordinate                                      = ',m
                  write(*,*) 'n coordinate                                      = ',n
                  write(*,*) 'segment number before aggregation                 = ',iseg
                  write(*,*) 'segment number in aggregation                     = ',ipnt_h(n,m)
                  call srstop(1)
               endif
            else
               if ( ipnt_h(n,m) .eq. 0 ) then
                  ! outside the active area, no action
               else
                  ! attamt to aggregeate an inactive segment, not allowed!
                  write(lunrep,*) 'ERROR: a inactive segment becomes active in the aggregation.'
                  write(lunrep,*) '       check the validity of your dwq-file!'
                  write(lunrep,*) 'm coordinate                                      = ',m
                  write(lunrep,*) 'n coordinate                                      = ',n
                  write(lunrep,*) 'segment number before aggregation                 = ',iseg
                  write(lunrep,*) 'segment number in aggregation                     = ',ipnt_h(n,m)
                  write(*,*) 'ERROR: a inactive segment becomes active in the aggregation.'
                  write(*,*) '       check the validity of your dwq-file!'
                  write(*,*) 'm coordinate                                      = ',m
                  write(*,*) 'n coordinate                                      = ',n
                  write(*,*) 'segment number before aggregation                 = ',iseg
                  write(*,*) 'segment number in aggregation                     = ',ipnt_h(n,m)
                  call srstop(1)
               end if      
            endif
         enddo
      enddo

      ! ipnt_vdf basically only layer aggregation to determine the minimum (nearest to exchange? not yet)

      do ilay = 1 , input_hyd%nolay
         ilay_new = ipnt_v(ilay)
         do i = 1 , input_hyd%nosegl
            i1 = i + (ilay-1)*input_hyd%nosegl
            i2 = i + (ilay_new-1)*input_hyd%nosegl
            ipnt_vdf(i1) = i2
         enddo
      enddo

      ! tau, only bottom segments count

      do i = 1 , input_hyd%noseg
         ik2 = mod(input_hyd%attributes(i),100)/10
         bottom = .false.
         if ( ik2 .eq. 0 ) bottom = .true.
         if ( ik2 .eq. 3 ) bottom = .true.
         if ( bottom ) then
            ipnt_tau(i) = ipnt(i)
         else
            ipnt_tau(i) = 0
         endif
      enddo

      return
      end
