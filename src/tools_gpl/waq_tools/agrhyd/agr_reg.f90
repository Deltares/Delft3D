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

      subroutine agr_reg(input_hyd, output_hyd, m_fact, n_fact, m_offset, n_offset, ipnt   )

      use m_srstop
      use m_monsys
      use hydmod
      implicit none

      type(t_hyd)          :: input_hyd                           ! description of the input hydrodynamics
      type(t_hyd)          :: output_hyd                          ! description of the output hydrodynamics
      integer              :: m_fact        ! aggregation factor m direction
      integer              :: n_fact        ! aggregation factor n direction
      integer              :: m_offset      ! offset aggregation m direction
      integer              :: n_offset      ! offset aggregation n direction
      integer              :: ipnt(input_hyd%nmax,input_hyd%mmax) ! aggregation pointer

      ! local declarations

      real                 :: fact_m        ! aggregation factor m direction
      real                 :: fact_n        ! aggregation factor n direction
      integer              :: m_left        ! number of columns to the left
      integer              :: n_left        ! number of rows to the left
      integer              :: m_right       ! number of columns to the right
      integer              :: n_right       ! number of rows to the right
      integer              :: m_left_new    ! number of columns to the left in the new grid
      integer              :: n_left_new    ! number of rows to the left in the new grid
      integer              :: m_right_new   ! number of columns to the right in the new grid
      integer              :: n_right_new   ! number of rows to the right in the new grid
      integer              :: mmax          ! mmax
      integer              :: nmax          ! nmax
      integer              :: mmax_new      ! new mmax after aggregation
      integer              :: nmax_new      ! new nmax after aggregation
      integer              :: m             ! m index
      integer              :: n             ! n index
      integer              :: m_new         ! m index in the new grid
      integer              :: n_new         ! n index in the new grid
      integer              :: iseg          ! segment index
      integer              :: iseg_new      ! segment index in the new grid
      integer              :: m_offset2     ! offset as compared to column 2 mod m_fact
      integer              :: n_offset2     ! offset as compared to column 2 mod n_fact
      logical              :: lfound        ! command line option found
      character            :: cdummy
      real                 :: rdummy
      integer              :: ierr
      integer              :: ierr2
      integer              :: lunrep        ! unit number report file
      logical              :: relocated
      integer              :: iseg_up
      integer              :: iseg_down
      integer              :: iseg_left
      integer              :: iseg_right
      integer              :: nobnd_new
      character(150)       :: fstring
      real   , allocatable :: x_tmp(:,:)    ! temporary x depth coordinate
      real   , allocatable :: y_tmp(:,:)    ! temporary y depth coordinate

      call getmlu(lunrep)

      mmax = input_hyd%mmax
      nmax = input_hyd%nmax
      fact_m = m_fact
      fact_n = n_fact

      ! schuif de offset binnen de buitenste gridlijnen

      if ( m_offset .eq. 1 ) then
         m_offset = m_offset + m_fact
      endif
      if ( m_offset .eq. mmax ) then
         m_offset = m_offset - m_fact
      endif
      if ( m_offset .lt. 1 .or. m_offset .gt. mmax ) then
         write(*,*) ' error m_offset out of range'
         write(*,*) ' m_offset =',m_offset
         call srstop(1)
      endif
      m_offset2 = mod(m_offset-2,m_fact)

      if ( n_offset .eq. 1 ) then
         n_offset = n_offset + n_fact
      endif
      if ( n_offset .eq. nmax ) then
         n_offset = n_offset - n_fact
      endif
      if ( n_offset .lt. 1 .or. n_offset .gt. nmax ) then
         write(*,*) ' error n_offset out of range'
         write(*,*) ' n_offset =',n_offset
         call srstop(1)
      endif
      n_offset2 = mod(n_offset-2,n_fact)

      ! bepaal nieuwe dimensies
      ! buitenste rij blijft altijd bestaan om (in het uiterste geval) ruimte te hebben voor de boundaries

      m_left      = m_offset-2
      m_right     = mmax-m_offset
      m_left_new  = int((m_left-1.)/m_fact+1.0)
      m_right_new = int((m_right-1.)/m_fact+1.0)
      mmax_new    = m_left_new + m_right_new + 2

      n_left      = n_offset-2
      n_right     = nmax-n_offset
      n_left_new  = int((n_left-1.)/n_fact+1.0)
      n_right_new = int((n_right-1.)/n_fact+1.0)
      nmax_new    = n_left_new + n_right_new + 2

      output_hyd%mmax   = mmax_new
      output_hyd%nmax   = nmax_new
      output_hyd%nosegl = mmax_new*nmax_new

      ! alloceer arrays op nieuwe dimensies

      allocate(output_hyd%lgrid(nmax_new,mmax_new))
      allocate(output_hyd%xdepth(nmax_new,mmax_new))
      allocate(output_hyd%ydepth(nmax_new,mmax_new))

      output_hyd%xdepth = 0.0
      output_hyd%ydepth = 0.0

      allocate(x_tmp(nmax_new,mmax_new))
      allocate(y_tmp(nmax_new,mmax_new))

      x_tmp = 0.0
      y_tmp = 0.0

      ! bepaal nieuwe segment nummers (totaal)

      ipnt             = 0
      output_hyd%lgrid = 0

      do m = 1 , mmax
         do n = 1 , nmax
            if ( m .eq. 1 ) then
               m_new = 1
            elseif ( m .eq. mmax ) then
               m_new = mmax_new
            else
               m_new = max(2,ceiling((m-m_offset2-1.)/m_fact)+ceiling(m_offset2/fact_m)+1)
            endif
            if ( n .eq. 1 ) then
               n_new = 1
            elseif ( n .eq. nmax ) then
               n_new = nmax_new
            else
               n_new = max(2,ceiling((n-n_offset2-1.)/n_fact)+ceiling(n_offset2/fact_n)+1)
            endif

            iseg     = input_hyd%lgrid(n,m)
            iseg_new = (m_new-1)*nmax_new + n_new
            if ( iseg .gt. 0 ) then
               output_hyd%lgrid(n_new,m_new) = iseg_new
               ipnt(n,m)                     = iseg_new
            endif

            ! prik cco over

            if ( abs(input_hyd%xdepth(n,m)) .gt. 1.e-20 ) then
               output_hyd%xdepth(n_new,m_new) = input_hyd%xdepth(n,m)
            endif
            if ( abs(input_hyd%ydepth(n,m)) .gt. 1.e-20 ) then
               output_hyd%ydepth(n_new,m_new) = input_hyd%ydepth(n,m)
            endif

         enddo
      enddo

      ! extra sweep back for missing cco values

      do m = mmax, 2 , -1
         do n = nmax, 2 , -1
            if ( m .eq. 1 ) then
               m_new = 1
            elseif ( m .eq. mmax ) then
               m_new = mmax_new
            else
               m_new = max(2,ceiling((m-m_offset2-1.)/m_fact)+ceiling(m_offset2/fact_m)+1)
            endif
            if ( n .eq. 1 ) then
               n_new = 1
            elseif ( n .eq. nmax ) then
               n_new = nmax_new
            else
               n_new = max(2,ceiling((n-n_offset2-1.)/n_fact)+ceiling(n_offset2/fact_n)+1)
            endif

            ! prik cco over in extra array

            if ( abs(input_hyd%xdepth(n,m)) .gt. 1.e-20 ) then
               x_tmp(n_new-1,m_new-1) = input_hyd%xdepth(n,m)
            endif
            if ( abs(input_hyd%ydepth(n,m)) .gt. 1.e-20 ) then
               y_tmp(n_new-1,m_new-1) = input_hyd%ydepth(n,m)
            endif

         enddo
      enddo
      do m_new = 1 , mmax_new
         do n_new = 1 , nmax_new
            if ( abs(output_hyd%xdepth(n_new,m_new)) .lt. 1.e-20 ) then
               output_hyd%xdepth(n_new,m_new) = x_tmp(n_new,m_new)
            endif
            if ( abs(output_hyd%ydepth(n_new,m_new)) .lt. 1.e-20 ) then
               output_hyd%ydepth(n_new,m_new) = y_tmp(n_new,m_new)
            endif
         enddo
      enddo

!     ! extra sweep back for missing cco values
!
!     do m = 2 , mmax
!        do n = 2 , nmax
!           if ( m .eq. 1 ) then
!              m_new = 1
!           elseif ( m .eq. mmax ) then
!              m_new = mmax_new
!           else
!              m_new = max(2,ceiling((m-m_offset2-1.)/m_fact)+ceiling(m_offset2/fact_m)+1)
!           endif
!           if ( n .eq. 1 ) then
!              n_new = 1
!           elseif ( n .eq. nmax ) then
!              n_new = nmax_new
!           else
!              n_new = max(2,ceiling((n-n_offset2-1.)/n_fact)+ceiling(n_offset2/fact_n)+1)
!           endif
!
!           ! prik cco over
!
!           if ( abs(input_hyd%xdepth(n,m)) .gt. 1.e-20 ) then
!              if ( abs(output_hyd%xdepth(n_new-1,m_new-1)) .lt. 1.e-20 ) then
!                 output_hyd%xdepth(n_new-1,m_new-1) = input_hyd%xdepth(n,m)
!              endif
!           endif
!           if ( abs(input_hyd%ydepth(n,m)) .gt. 1.e-20 ) then
!              if ( abs(output_hyd%ydepth(n_new-1,m_new-1)) .lt. 1.e-20 ) then
!                 output_hyd%ydepth(n_new-1,m_new-1) = input_hyd%ydepth(n,m)
!              endif
!           endif
!
!        enddo
!     enddo

      do m = 2 , mmax
         do n = 2 , nmax
            if ( m .eq. 1 ) then
               m_new = 1
            elseif ( m .eq. mmax ) then
               m_new = mmax_new
            else
               m_new = max(2,ceiling((m-m_offset2-1.)/m_fact)+ceiling(m_offset2/fact_m)+1)
            endif
            if ( n .eq. 1 ) then
               n_new = 1
            elseif ( n .eq. nmax ) then
               n_new = nmax_new
            else
               n_new = max(2,ceiling((n-n_offset2-1.)/n_fact)+ceiling(n_offset2/fact_n)+1)
            endif

            ! prik cco over

            if ( abs(input_hyd%xdepth(n,m)) .gt. 1.e-20 ) then
               x_tmp(n_new,m_new-1) = input_hyd%xdepth(n,m)
               x_tmp(n_new-1,m_new) = input_hyd%xdepth(n,m)
            endif
            if ( abs(input_hyd%ydepth(n,m)) .gt. 1.e-20 ) then
               y_tmp(n_new,m_new-1) = input_hyd%ydepth(n,m)
               y_tmp(n_new-1,m_new) = input_hyd%ydepth(n,m)
            endif

         enddo
      enddo
      do m_new = 1 , mmax_new
         do n_new = 1 , nmax_new
            if ( abs(output_hyd%xdepth(n_new,m_new)) .lt. 1.e-20 ) then
               output_hyd%xdepth(n_new,m_new) = x_tmp(n_new,m_new)
            endif
            if ( abs(output_hyd%ydepth(n_new,m_new)) .lt. 1.e-20 ) then
               output_hyd%ydepth(n_new,m_new) = y_tmp(n_new,m_new)
            endif
         enddo
      enddo
      deallocate(x_tmp)
      deallocate(y_tmp)

!     do m = 2 , mmax
!        do n = 2 , nmax
!           if ( m .eq. 1 ) then
!              m_new = 1
!           elseif ( m .eq. mmax ) then
!              m_new = mmax_new
!           else
!              m_new = max(2,ceiling((m-m_offset2-1.)/m_fact)+ceiling(m_offset2/fact_m)+1)
!           endif
!           if ( n .eq. 1 ) then
!              n_new = 1
!           elseif ( n .eq. nmax ) then
!              n_new = nmax_new
!           else
!              n_new = max(2,ceiling((n-n_offset2-1.)/n_fact)+ceiling(n_offset2/fact_n)+1)
!           endif
!
!           ! prik cco over
!
!           if ( abs(input_hyd%xdepth(n,m)) .gt. 1.e-20 ) then
!              if ( abs(output_hyd%xdepth(n_new-1,m_new-1)) .lt. 1.e-20 ) then
!                 output_hyd%xdepth(n_new-1,m_new-1) = input_hyd%xdepth(n,m)
!              endif
!              if ( abs(output_hyd%xdepth(n_new,m_new-1)) .lt. 1.e-20 ) then
!                 output_hyd%xdepth(n_new,m_new-1) = input_hyd%xdepth(n,m)
!              endif
!              if ( abs(output_hyd%xdepth(n_new-1,m_new)) .lt. 1.e-20 ) then
!                 output_hyd%xdepth(n_new-1,m_new) = input_hyd%xdepth(n,m)
!              endif
!           endif
!           if ( abs(input_hyd%ydepth(n,m)) .gt. 1.e-20 ) then
!              if ( abs(output_hyd%ydepth(n_new-1,m_new-1)) .lt. 1.e-20 ) then
!                 output_hyd%ydepth(n_new-1,m_new-1) = input_hyd%ydepth(n,m)
!              endif
!              if ( abs(output_hyd%ydepth(n_new,m_new-1)) .lt. 1.e-20 ) then
!                 output_hyd%ydepth(n_new,m_new-1) = input_hyd%ydepth(n,m)
!              endif
!              if ( abs(output_hyd%ydepth(n_new-1,m_new)) .lt. 1.e-20 ) then
!                 output_hyd%ydepth(n_new-1,m_new) = input_hyd%ydepth(n,m)
!              endif
!           endif
!
!        enddo
!     enddo

      ! boundaries

      nobnd_new = 0
      ierr      = 0
      do m = 1 , mmax
         do n = 1 , nmax
            if ( m .eq. 1 ) then
               m_new = 1
            elseif ( m .eq. mmax ) then
               m_new = mmax_new
            else
               m_new = max(2,ceiling((m-m_offset2-1.)/m_fact)+ceiling(m_offset2/fact_m)+1)
            endif
            if ( n .eq. 1 ) then
               n_new = 1
            elseif ( n .eq. nmax ) then
               n_new = nmax_new
            else
               n_new = max(2,ceiling((n-n_offset2-1.)/n_fact)+ceiling(n_offset2/fact_n)+1)
            endif

            iseg     = input_hyd%lgrid(n,m)
            if ( iseg .lt. 0 ) then
               iseg_new = output_hyd%lgrid(n_new,m_new)
               if ( iseg_new .eq. 0 ) then
                  nobnd_new = nobnd_new + 1
                  output_hyd%lgrid(n_new,m_new) = -nobnd_new
                  ipnt(n,m)                     = -nobnd_new
               elseif ( iseg_new .lt. 0 ) then
                  ipnt(n,m)                     = iseg_new
               elseif ( iseg_new .gt. 0 ) then

                  ! we have to shift the boundary
                  ! (and maybe split but if we split we have a double pointer so do not do that at the moment, but have to check yet the consequence)
                  ! we are by definition not at the edge of the grid because these cells can never be active

                  iseg_up    = input_hyd%lgrid(n,m+1)
                  iseg_down  = input_hyd%lgrid(n,m-1)
                  iseg_left  = input_hyd%lgrid(n-1,m)
                  iseg_right = input_hyd%lgrid(n+1,m)

                  relocated = .false.
                  if ( iseg_up .gt. 0 .and. iseg_down .le. 0 ) then
                     iseg_new = output_hyd%lgrid(n_new,m_new-1)
                     if ( iseg_new .eq. 0 ) then
                        nobnd_new = nobnd_new + 1
                        output_hyd%lgrid(n_new,m_new-1) = -nobnd_new
                        ipnt(n,m)                       = -nobnd_new
                        relocated = .true.
                     elseif ( iseg_new .lt. 0 ) then
                        ipnt(n,m)                       = iseg_new
                        relocated = .true.
                     endif
                  endif

                  if ( iseg_down .gt. 0 .and. iseg_up .le. 0 ) then
                     iseg_new = output_hyd%lgrid(n_new,m_new+1)
                     if ( iseg_new .eq. 0 ) then
                        nobnd_new = nobnd_new + 1
                        output_hyd%lgrid(n_new,m_new+1) = -nobnd_new
                        ipnt(n,m)                       = -nobnd_new
                        relocated = .true.
                     elseif ( iseg_new .lt. 0 ) then
                        ipnt(n,m)                       = iseg_new
                        relocated = .true.
                     endif
                  endif

                  if ( iseg_left .gt. 0 .and. iseg_right .le. 0 ) then
                     iseg_new = output_hyd%lgrid(n_new+1,m_new)
                     if ( iseg_new .eq. 0 ) then
                        nobnd_new = nobnd_new + 1
                        output_hyd%lgrid(n_new+1,m_new) = -nobnd_new
                        ipnt(n,m)                       = -nobnd_new
                        relocated = .true.
                     elseif ( iseg_new .lt. 0 ) then
                        ipnt(n,m)                       = iseg_new
                        relocated = .true.
                     endif
                  endif

                  if ( iseg_right .gt. 0 .and. iseg_left .le. 0 ) then
                     iseg_new = output_hyd%lgrid(n_new-1,m_new)
                     if ( iseg_new .eq. 0 ) then
                        nobnd_new = nobnd_new + 1
                        output_hyd%lgrid(n_new-1,m_new) = -nobnd_new
                        ipnt(n,m)                       = -nobnd_new
                        relocated = .true.
                     elseif ( iseg_new .lt. 0 ) then
                        ipnt(n,m)                       = iseg_new
                        relocated = .true.
                     endif
                  endif

                  if ( .not. relocated ) then
                     ierr = ierr + 1
                     write(lunrep,*) 'ERROR unable to relocate boundary at:'
                     write(lunrep,*) 'm = ',m
                     write(lunrep,*) 'n = ',n
                  endif

               endif
            endif
         enddo
      enddo

      ! write lga table on old grid

!      write(lunrep,'(''     m->'',<mmax>(i4,1x))') (m,m=1,mmax)
!      write(lunrep,'('' n  *** '',<mmax>(''****'',1x),'' ***  n'')')
!      do n = nmax , 1 , -1
!          write(lunrep,'(i3,'' *** '',<mmax>(i4,1x),'' *** '',i3)') n,(ipnt(n,m),m=1,mmax),n
!      enddo
!      write(lunrep,'('' n  *** '',<mmax>(''****'',1x))')
!      write(lunrep,'(''     m->'',<mmax>(i4,1x))') (m,m=1,mmax)

      if ( ierr .ne. 0 ) then
         write(lunrep,*) 'stopped because of errors'
         call srstop(1)
      endif

      return
      end
