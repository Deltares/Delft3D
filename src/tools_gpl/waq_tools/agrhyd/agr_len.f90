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

      subroutine agr_len(input_hyd, output_hyd, ipnt_h, ipnt_q, l_expand, l_lenlen)

      ! function : initialise aggregation, time independent data

      ! (c) DELFT HYDRAULICS

      ! global declarations

      use m_srstop
      use m_monsys
      use m_cli_utils, only : retrieve_command_argument
      use hydmod
      use MessageHandling
      
      implicit none

      ! declaration of the arguments

      type(t_hyd)          :: input_hyd                            ! description of the input hydrodynamics
      type(t_hyd)          :: output_hyd                           ! description of the output hydrodynamics
      integer              :: ipnt_h(input_hyd%nmax,input_hyd%mmax)! aggregation pointer in the horizontal
      integer              :: ipnt_q(input_hyd%noq)                ! aggregation pointer in the horizontal
      logical              :: l_expand                             ! expand to full matrix
      logical              :: l_lenlen                             ! take length from length

      ! local declarations

      character(len=255)  :: message         ! temporary variable for writing log messages.
      logical             :: spherical       ! is the waqgeom spherical?
      real(8)             :: ddistance       ! distance
      real                :: displen         ! dispersion length
      real(8), parameter  :: dearthrad = 6378137.0 ! earth radius
      integer             :: m,n             ! loop counter grid
      integer             :: mmax            ! m dimension of grid
      integer             :: nmax            ! n dimension of grid
      real, allocatable   :: len1(:,:)       ! length in the first direction for the base grid
      real, allocatable   :: len2(:,:)       ! length in the 2nd direction for the base grid
      real, allocatable   :: len1_n(:)       ! length in the first direction for the new segments
      real, allocatable   :: len2_n(:)       ! length in the 2nd direction for the new segments
      integer             :: iq, iq2, iq_n   ! exchange index
      integer             :: ip1, ip2        ! segment index pointers
      integer             :: ig1, ig2        ! grid index pointers
      integer             :: iseg            ! segment index
      integer             :: prev_seg        ! segment index previous segment
      real                :: x1,x2,y1,y2     ! coordinates
      integer             :: lunrep          ! report file
      integer             :: ierr_alloc      ! allocation error indicator
      integer             :: ierr2           ! io errors
      type(t_dlwqfile)    :: file_guu        ! guu quickin file
      type(t_dlwqfile)    :: file_gvv        ! gvv quickin file
      logical             :: lfound
      integer             :: idummy          ! dummy
      real                :: rdummy          ! dummy

      ! some init

      call getmlu(lunrep)

      ! if expand or forced just take over the length

      if ( l_expand .or. l_lenlen ) then
         output_hyd%displen = 1.0
         do iq = 1, input_hyd%noq
            iq_n = ipnt_q(iq)
            if ( iq_n .gt. 0 ) then
               output_hyd%displen(1,iq_n) = input_hyd%displen(1,iq)
               output_hyd%displen(2,iq_n) = input_hyd%displen(2,iq)
            endif
         enddo
         return
      endif

      if (input_hyd%geometry .eq. HYD_GEOM_UNSTRUC) then
         output_hyd%displen = 1.0
         if (output_hyd%crs%epsg_code == 4326) then
            spherical = .true.
            write(message, *) 'The grid has sperical coordinates, agrhyd reckon this while calculating dispersion lenghts.'
            call mess(LEVEL_INFO, trim(message))
         else
            spherical = .false.
            write(message, *) 'The grid has Cartesian coordinates.' 
            call mess(LEVEL_INFO, trim(message))
         endif
         do iq = 1, output_hyd%noq1 
            ip1   = output_hyd%ipoint(1,iq)
            ip2   = output_hyd%ipoint(2,iq)
            if ( ip1 .eq. 0 .or. ip1 .eq. 0) cycle
            if(output_hyd%nolay.gt.1) then
               ig1 = mod(ip1 - 1,output_hyd%nosegl) + 1
               ig2 = mod(ip2 - 1,output_hyd%nosegl) + 1
            else
               ig1 = ip1
               ig2 = ip2
            endif   
            if ( ip1 .lt. 0 ) then
               displen = 0.5*sqrt(output_hyd%surf(ig2))
            else if ( ip2 .lt. 0 ) then
               displen = 0.5*sqrt(output_hyd%surf(ig2))
            else
               call distance(spherical, output_hyd%waqgeom%facex(ig1), output_hyd%waqgeom%facey(ig1), & 
                            output_hyd%waqgeom%facex(ig2), output_hyd%waqgeom%facey(ig2), ddistance, dearthrad)
               displen = 0.5 * ddistance
            endif
            output_hyd%displen(1,iq) = displen
            output_hyd%displen(2,iq) = displen
         enddo
         return
      endif

      ! read or calculate length

      file_gvv=t_dlwqfile(' ',' ',0,FT_ASC,FILE_STAT_UNOPENED)
      file_guu=t_dlwqfile(' ',' ',0,FT_ASC,FILE_STAT_UNOPENED)
      call retrieve_command_argument ( '-guu'  , 3    , lfound, idummy, rdummy, file_guu%name, ierr2)
      if ( lfound ) then
         if ( ierr2.ne. 0 ) then
            file_guu%name = ' '
         endif
      else
         file_guu%name = ' '
      endif
      call retrieve_command_argument ( '-gvv'  , 3    , lfound, idummy, rdummy, file_gvv%name, ierr2)
      if ( lfound ) then
         if ( ierr2.ne. 0 ) then
            file_gvv%name = ' '
         endif
      else
         file_gvv%name = ' '
      endif

      mmax = input_hyd%mmax
      nmax = input_hyd%nmax
      allocate(len1(nmax,mmax),len2(nmax,mmax),stat=ierr_alloc)
      if ( ierr_alloc .ne. 0 ) goto 980

      if ( file_guu%name .ne. ' ' .and. file_gvv%name .ne. ' ' ) then

         ! read length per grid cell (ignore shift in staggered grid at the moment)
         ! the file is a qin file

         call dlwqfile_open(file_guu)
         read(file_guu%unit_nr,*) ((len1(n,m),m=1,mmax),n=1,nmax)
         close(file_guu%unit_nr)
         file_guu%status = FILE_STAT_UNOPENED

         call dlwqfile_open(file_gvv)
         read(file_gvv%unit_nr,*) ((len2(n,m),m=1,mmax),n=1,nmax)
         close(file_gvv%unit_nr)
         file_gvv%status = FILE_STAT_UNOPENED

      else

         ! calculate from cco file

         do m = 2, mmax - 1
            do n = 2, nmax - 1
               if ( input_hyd%lgrid(n,m) .gt. 0 ) then
                  x1 = (input_hyd%xdepth(n-1,m-1) + input_hyd%xdepth(n-1,m))/2.0
                  y1 = (input_hyd%ydepth(n-1,m-1) + input_hyd%ydepth(n-1,m))/2.0
                  x2 = (input_hyd%xdepth(n,m-1) + input_hyd%xdepth(n,m))/2.0
                  y2 = (input_hyd%ydepth(n,m-1) + input_hyd%ydepth(n,m))/2.0
                  len1(n,m) = sqrt((x2-x1)*(x2-x1)+(y2-y1)*(y2-y1))
                  x1 = (input_hyd%xdepth(n,m-1) + input_hyd%xdepth(n-1,m-1))/2.0
                  y1 = (input_hyd%ydepth(n,m-1) + input_hyd%ydepth(n-1,m-1))/2.0
                  x2 = (input_hyd%xdepth(n,m) + input_hyd%xdepth(n-1,m))/2.0
                  y2 = (input_hyd%ydepth(n,m) + input_hyd%ydepth(n-1,m))/2.0
                  len2(n,m) = sqrt((x2-x1)*(x2-x1)+(y2-y1)*(y2-y1))
               endif
            enddo
         enddo

      endif

      ! length per new segments first direction

      allocate(len1_n(output_hyd%nosegl),stat=ierr_alloc)
      if ( ierr_alloc .ne. 0 ) goto 990
      len1_n = 0.0
      do m = 2, mmax - 1
         prev_seg = 0
         do n = 2, nmax - 1
            iseg = ipnt_h(n,m)
            if ( iseg .gt. 0 ) then
               if ( iseg .eq. prev_seg ) then
                  len1_n(iseg) = len1_n(iseg) + len1(n,m)*0.5
               else
                  if ( len1_n(iseg) .eq. 0.0 ) then
                     len1_n(iseg) = len1(n,m)*0.5
                     prev_seg = iseg
                  else
                     prev_seg = 0
                  endif
               endif
            endif
         enddo
      enddo
      deallocate(len1,stat=ierr_alloc)
      if ( ierr_alloc .ne. 0 ) then ; write(*,*) ' error deallocating memory' ; call srstop(1) ; endif

      ! length per new segments first direction

      allocate(len2_n(output_hyd%nosegl),stat=ierr_alloc)
      if ( ierr_alloc .ne. 0 ) goto 990
      len2_n = 0.0
      do n = 2, nmax - 1
         prev_seg = 0
         do m = 2, mmax - 1
            iseg = ipnt_h(n,m)
            if ( iseg .gt. 0 ) then
               if ( iseg .eq. prev_seg ) then
                  len2_n(iseg) = len2_n(iseg) + len2(n,m)*0.5
               else
                  if ( len2_n(iseg) .eq. 0.0 ) then
                     len2_n(iseg) = len2(n,m)*0.5
                     prev_seg = iseg
                  else
                     prev_seg = 0
                  endif
               endif
            endif
         enddo
      enddo
      deallocate(len2,stat=ierr_alloc)
      if ( ierr_alloc .ne. 0 ) then ; write(*,*) ' error deallocating memory' ; call srstop(1) ; endif

      ! length for the pointers

      do iq = 1 , output_hyd%noq1
         ip1   = output_hyd%ipoint(1,iq)
         ip2   = output_hyd%ipoint(2,iq)
         if ( ip1 .gt. 0 ) then
            iseg=mod(ip1-1,output_hyd%nosegl)+1
            output_hyd%displen(1,iq) = len1_n(iseg)
         endif
         if ( ip2 .gt. 0 ) then
            iseg=mod(ip2-1,output_hyd%nosegl)+1
            output_hyd%displen(2,iq) = len1_n(iseg)
         endif
      enddo
      do iq = output_hyd%noq1 + 1 , output_hyd%noq1 + output_hyd%noq2
         ip1   = output_hyd%ipoint(1,iq)
         ip2   = output_hyd%ipoint(2,iq)
         if ( ip1 .gt. 0 ) then
            iseg=mod(ip1-1,output_hyd%nosegl)+1
            output_hyd%displen(1,iq) = len2_n(iseg)
         endif
         if ( ip2 .gt. 0 ) then
            iseg=mod(ip2-1,output_hyd%nosegl)+1
            output_hyd%displen(2,iq) = len2_n(iseg)
         endif
      enddo
      do iq = output_hyd%noq1 + output_hyd%noq2 + 1 , output_hyd%noq
         output_hyd%displen(1,iq) = 1.0
         output_hyd%displen(2,iq) = 1.0
      enddo
      deallocate(len1_n,len2_n,stat=ierr_alloc)
      if ( ierr_alloc .ne. 0 ) then ; write(*,*) ' error deallocating memory' ; call srstop(1) ; endif

      ! length for boundaries equal to length within the grid

      do iq = 1 , output_hyd%noq
         ip1   = output_hyd%ipoint(1,iq)
         ip2   = output_hyd%ipoint(2,iq)
         if ( ip1 .lt. 0 ) then
            output_hyd%displen(1,iq) = output_hyd%displen(2,iq)
         endif
         if ( ip2 .lt. 0 ) then
            output_hyd%displen(2,iq) = output_hyd%displen(1,iq)
         endif
      enddo

      ! minimum dispersion length

      if ( output_hyd%min_disp_len .gt. 0.0 ) then
         do iq = 1 , output_hyd%noq
            output_hyd%displen(1,iq) = max(output_hyd%displen(1,iq),output_hyd%min_disp_len)
            output_hyd%displen(2,iq) = max(output_hyd%displen(2,iq),output_hyd%min_disp_len)
         enddo
      endif

      return
  980 write(lunrep,*) 'error allocating memory:',ierr_alloc
      write(lunrep,*) 'input_hyd%nmax:',input_hyd%nmax
      write(lunrep,*) 'input_hyd%mmax:',input_hyd%mmax
      call srstop(1)
  990 write(lunrep,*) 'error allocating memory:',ierr_alloc
      write(lunrep,*) 'input_hyd%nosegl',input_hyd%nosegl
      call srstop(1)
  995 write(lunrep,*) 'error allocating memory:',ierr_alloc
      write(lunrep,*) 'output_hyd%noq',output_hyd%noq
      call srstop(1)
      end
