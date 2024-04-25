!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2024.                                
!                                                                               
!  This program is free software: you can redistribute it and/or modify         
!  it under the terms of the GNU General Public License as published by         
!  the Free Software Foundation version 3.                                      
!                                                                               
!  This program is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU General Public License for more details.                                 
!                                                                               
!  You should have received a copy of the GNU General Public License            
!  along with this program.  If not, see <http://www.gnu.org/licenses/>.        
!                                                                               
!  contact: delft3d.support@deltares.nl                                         
!  Stichting Deltares                                                           
!  P.O. Box 177                                                                 
!  2600 MH Delft, The Netherlands                                               
!                                                                               
!  All indications and logos of, and references to, "Delft3D" and "Deltares"    
!  are registered trademarks of Stichting Deltares, and remain the property of  
!  Stichting Deltares. All rights reserved.                                     
!                                                                               
!-------------------------------------------------------------------------------
!  
!  

      subroutine write_ddp(hyd)

      ! function : write a ddp file, dd boundary administration for part

      ! global declarations

      use m_srstop
      use m_monsys
      use m_hydmod                   ! module contains everything for the hydrodynamics
      implicit none

      ! declaration of the arguments

      type(t_hydrodynamics)                            :: hyd                   ! description of the hydrodynamics

      ! local declarations

      integer                                :: lunrep                ! unit number report file
      integer                                :: lunddp                ! unit number sources file
      integer                                :: n_domain              ! number of domains
      integer                                :: i_domain              ! loop counter and index
      integer                                :: i_domain1             ! loop counter and index
      integer                                :: i_domain2             ! loop counter and index
      integer, allocatable                   :: m_offset(:)           ! offset in m direction for the domains
      integer                                :: n_dd_bound            ! number of dd_boundaries
      integer                                :: i_dd_bound            ! index number of dd_bound
      type(t_dd_bound),pointer               :: dd_bound              ! description of the overall hydrodynamics
      integer, allocatable                   :: iadmddb(:,:)          ! dd boundary administration
      integer, allocatable                   :: iadmddb_copy(:,:)     ! dd boundary administration copy
      integer                                :: nddb_max              ! number of dd boundary connections
      integer                                :: nddb                  ! number of dd boundary connections
      integer                                :: iddb                  ! index dd boundary connections
      integer                                :: iiddb                 ! index dd boundary connections administration
      integer                                :: ir1                   ! refinement in domain1
      integer                                :: ir2                   ! refinement in domain2
      integer                                :: m_begin1              ! m_begin1
      integer                                :: n_begin1              ! n_begin1
      integer                                :: m_begin2              ! m_begin2
      integer                                :: n_begin2              ! n_begin2
      integer                                :: m                     ! m index
      integer                                :: n                     ! n index
      logical                                :: found                 ! found
      integer                                :: nddb2                 ! nddb2 is nddb after merge
      integer                                :: iddb2                 ! second iddb index

      ! some init

      call getmlu(lunrep)
      n_domain = hyd%domain_coll%current_size
      nddb = 0
      nddb_max = 500
      allocate(iadmddb(22,nddb_max))
      iadmddb = 0

      ! determine offsets in overall grid

      allocate(m_offset(n_domain))
      do i_domain = 1 , n_domain
         if ( i_domain .eq. 1 ) then
            m_offset(i_domain) = 0
         else
            m_offset(i_domain) = m_offset(i_domain-1) + hyd%domain_coll%domain_pnts(i_domain-1)%mmax
         endif
      enddo

      ! check dd_boundaries

      n_dd_bound = hyd%dd_bound_coll%current_size
      do i_dd_bound = 1 , n_dd_bound

         dd_bound => hyd%dd_bound_coll%dd_bound_pnts(i_dd_bound)

         ! look up the domain names

         i_domain1 = hyd%domain_coll%find(dd_bound%name1)
         if ( i_domain .le. 0 ) then
            write(lunrep,*) 'ERROR domain in dd-boundary not found:',trim(dd_bound%name1)
            call srstop(1)
         endif
         dd_bound%i_domain1 = i_domain1
         i_domain2 = hyd%domain_coll%find(dd_bound%name2)
         if ( i_domain .le. 0 ) then
            write(lunrep,*) 'ERROR domain in dd-boundary not found:',trim(dd_bound%name2)
            call srstop(1)
         endif
         dd_bound%i_domain2 = i_domain2

         ! determine refinement and the side of the boundary

         if ( dd_bound%m_begin1 .eq. dd_bound%m_end1 ) then
            if ( hyd%lgrid(dd_bound%n_begin1+1,dd_bound%m_begin1+m_offset(i_domain1)) .gt. 0 ) then
               dd_bound%direction = DD_RIGHT_LEFT
            else
               dd_bound%direction = DD_LEFT_RIGHT
            endif
            if ( dd_bound%n_end1 - dd_bound%n_begin1 .ge. dd_bound%n_end2 - dd_bound%n_begin2 ) then
               ir1 = (dd_bound%n_end1 - dd_bound%n_begin1)/(dd_bound%n_end2 - dd_bound%n_begin2)
               ir2 = 1
            else
               ir1 = 1
               ir2 = (dd_bound%n_end2 - dd_bound%n_begin2)/(dd_bound%n_end1 - dd_bound%n_begin1)
            endif
         else
            if ( hyd%lgrid(dd_bound%n_begin1,dd_bound%m_begin1+1+m_offset(i_domain1)) .gt. 0 ) then
               dd_bound%direction = DD_TOP_BOTTOM
            else
               dd_bound%direction = DD_BOTTOM_TOP
            endif
            if ( dd_bound%m_end1 - dd_bound%m_begin1 .ge. dd_bound%m_end2 - dd_bound%m_begin2 ) then
               ir1 = (dd_bound%m_end1 - dd_bound%m_begin1)/(dd_bound%m_end2 - dd_bound%m_begin2)
               ir2 = 1
            else
               ir1 = 1
               ir2 = (dd_bound%m_end2 - dd_bound%m_begin2)/(dd_bound%m_end1 - dd_bound%m_begin1)
            endif
         endif
         dd_bound%refine1 = ir1
         dd_bound%refine2 = ir2

      enddo

      ! set up the actual connections

      do i_dd_bound = 1 , n_dd_bound

         dd_bound => hyd%dd_bound_coll%dd_bound_pnts(i_dd_bound)
         i_domain1 = dd_bound%i_domain1
         i_domain2 = dd_bound%i_domain2
         ir1 = dd_bound%refine1
         ir2 = dd_bound%refine2

         ! loop over the boundary in the first domain

         if ( dd_bound%direction .eq. DD_RIGHT_LEFT .or. dd_bound%direction .eq. DD_LEFT_RIGHT ) then
            m_begin1 = dd_bound%m_begin1
            n_begin1 = dd_bound%n_begin1 + 1
         else
            m_begin1 = dd_bound%m_begin1 + 1
            n_begin1 = dd_bound%n_begin1
         endif

         do m = m_begin1, dd_bound%m_end1
         do n = n_begin1, dd_bound%n_end1

            nddb = nddb + 1
            if ( nddb .gt. nddb_max ) then
               allocate(iadmddb_copy(22,nddb_max))
               iadmddb_copy = iadmddb
               deallocate(iadmddb)
               nddb_max = nddb_max*2
               allocate(iadmddb(22,nddb_max))
               iadmddb = 0
               iadmddb(:,1:nddb-1) = iadmddb_copy
               deallocate(iadmddb_copy)
            endif

            if ( dd_bound%direction .eq. DD_RIGHT_LEFT ) then
               iadmddb(1,nddb)  = m + 1 + m_offset(i_domain1)
               iadmddb(2,nddb)  = n
               iadmddb(8,nddb)  = dd_bound%m_begin2 + 1 + m_offset(i_domain2)
               iadmddb(9,nddb)  = dd_bound%n_begin2 + 1 + (n-n_begin1)*ir2/ir1
               iadmddb(10,nddb) = ir1
               iadmddb(11,nddb) = ir2
               if ( ir1 .gt. 1 ) then
                  iadmddb(12,nddb) = mod(n-n_begin1,ir1) + 1
               endif
            elseif ( dd_bound%direction .eq. DD_LEFT_RIGHT ) then
               iadmddb(1,nddb)  = m + m_offset(i_domain1)
               iadmddb(2,nddb)  = n
               iadmddb(3,nddb)  = dd_bound%m_begin2 + m_offset(i_domain2)
               iadmddb(4,nddb)  = dd_bound%n_begin2 + 1 + (n-n_begin1)*ir2/ir1
               iadmddb(5,nddb)  = ir1
               iadmddb(6,nddb)  = ir2
               if ( ir1 .gt. 1 ) then
                  iadmddb(8,nddb)  = mod(n-n_begin1,ir1) + 1
               endif
            elseif ( dd_bound%direction .eq. DD_TOP_BOTTOM ) then
               iadmddb(1,nddb)  = m + m_offset(i_domain1)
               iadmddb(2,nddb)  = n + 1
               iadmddb(18,nddb) = dd_bound%m_begin2 + 1 + (m-m_begin1)*ir2/ir1 + m_offset(i_domain2)
               iadmddb(19,nddb) = dd_bound%n_begin2 + 1
               iadmddb(20,nddb) = ir1
               iadmddb(21,nddb) = ir2
               if ( ir1 .gt. 1 ) then
                  iadmddb(22,nddb) = mod(m-m_begin1,ir1) + 1
               endif
            elseif ( dd_bound%direction .eq. DD_BOTTOM_TOP ) then
               iadmddb(1,nddb)  = m + m_offset(i_domain1)
               iadmddb(2,nddb)  = n
               iadmddb(13,nddb) = dd_bound%m_begin2 + 1 + (m-m_begin1)*ir2/ir1 + m_offset(i_domain2)
               iadmddb(14,nddb) = dd_bound%n_begin2
               iadmddb(15,nddb) = ir1
               iadmddb(16,nddb) = ir2
               if ( ir1 .gt. 1 ) then
                  iadmddb(17,nddb) = mod(m-m_begin1,ir1) + 1
               endif
            endif

         enddo
         enddo

         ! loop over the boundary in the second domain

         if ( dd_bound%direction .eq. DD_RIGHT_LEFT .or. dd_bound%direction .eq. DD_LEFT_RIGHT ) then
            m_begin2 = dd_bound%m_begin2
            n_begin2 = dd_bound%n_begin2 + 1
         else
            m_begin2 = dd_bound%m_begin2 + 1
            n_begin2 = dd_bound%n_begin2
         endif

         do m = m_begin2, dd_bound%m_end2
         do n = n_begin2, dd_bound%n_end2

            nddb = nddb + 1
            if ( nddb .gt. nddb_max ) then
               allocate(iadmddb_copy(22,nddb_max))
               iadmddb_copy = iadmddb
               deallocate(iadmddb)
               nddb_max = nddb_max*2
               allocate(iadmddb(22,nddb_max))
               iadmddb = 0
               iadmddb(:,1:nddb-1) = iadmddb_copy
               deallocate(iadmddb_copy)
            endif

            if ( dd_bound%direction .eq. DD_RIGHT_LEFT ) then
               iadmddb(1,nddb)  = m + m_offset(i_domain2)
               iadmddb(2,nddb)  = n
               iadmddb(3,nddb)  = dd_bound%m_begin1 + m_offset(i_domain1)
               iadmddb(4,nddb)  = dd_bound%n_begin1 + 1 + (n-n_begin2)*ir1/ir2
               iadmddb(5,nddb)  = ir2
               iadmddb(6,nddb)  = ir1
               if ( ir2 .gt. 1 ) then
                  iadmddb(7,nddb)  = mod(n-n_begin2,ir2) + 1
               endif
            elseif ( dd_bound%direction .eq. DD_LEFT_RIGHT ) then
               iadmddb(1,nddb)  = m + 1 + m_offset(i_domain2)
               iadmddb(2,nddb)  = n
               iadmddb(8,nddb)  = dd_bound%m_begin1 + 1 + m_offset(i_domain1)
               iadmddb(9,nddb)  = dd_bound%n_begin1 + 1 + (n-n_begin2)*ir1/ir2
               iadmddb(10,nddb) = ir2
               iadmddb(11,nddb) = ir1
               if ( ir2 .gt. 1 ) then
                  iadmddb(12,nddb) = mod(n-n_begin2,ir2) + 1
               endif
            elseif ( dd_bound%direction .eq. DD_TOP_BOTTOM ) then
               iadmddb(1,nddb)  = m + m_offset(i_domain2)
               iadmddb(2,nddb)  = n
               iadmddb(13,nddb) = dd_bound%m_begin1 + 1 + (m-m_begin2)*ir1/ir2 + m_offset(i_domain1)
               iadmddb(14,nddb) = dd_bound%n_begin1
               iadmddb(15,nddb) = ir2
               iadmddb(16,nddb) = ir1
               if ( ir2 .gt. 1 ) then
                  iadmddb(17,nddb) = mod(m-m_begin2,ir2) + 1
               endif
            elseif ( dd_bound%direction .eq. DD_BOTTOM_TOP ) then
               iadmddb(1,nddb)  = m + m_offset(i_domain2)
               iadmddb(2,nddb)  = n + 1
               iadmddb(18,nddb) = dd_bound%m_begin2 + 1 + (m-m_begin2)*ir1/ir2 + m_offset(i_domain1)
               iadmddb(19,nddb) = dd_bound%n_begin2 + 1
               iadmddb(20,nddb) = ir2
               iadmddb(21,nddb) = ir1
               if ( ir2 .gt. 1 ) then
                  iadmddb(22,nddb) = mod(m-m_begin2,ir2) + 1
               endif
            endif

         enddo
         enddo

      enddo

      ! merge admin for the different directions ( by adding the values because one should always contain zeros

      allocate(iadmddb_copy(22,nddb))
      nddb2 = 0
      do iddb = 1, nddb
         found = .false.
         do iddb2 = 1, nddb2
            if ( iadmddb(1,iddb) .eq. iadmddb_copy(1,iddb2) .and. iadmddb(2,iddb) .eq. iadmddb_copy(2,iddb2) ) then
               found = .true.
               iadmddb_copy(3:22,iddb2) = iadmddb_copy(3:22,iddb2) + iadmddb(3:22,iddb)
            endif
         enddo
         if ( .not. found ) then
            nddb2 = nddb2 + 1
            iadmddb_copy(:,nddb2) = iadmddb(:,iddb)
         endif
      enddo
      nddb = nddb2
      iadmddb(:,1:nddb) = iadmddb_copy(:,1:nddb)
      deallocate(iadmddb_copy)

      ! point 0 to 0 pointers towards cell 1 1

      do iddb = 1, nddb
         if ( iadmddb(3,iddb) .eq. 0 ) then
            iadmddb(3,iddb)  = 1
            iadmddb(4,iddb)  = 1
         endif
         if ( iadmddb(8,iddb) .eq. 0 ) then
            iadmddb(8,iddb)  = 1
            iadmddb(9,iddb)  = 1
         endif
         if ( iadmddb(13,iddb) .eq. 0 ) then
            iadmddb(13,iddb)  = 1
            iadmddb(14,iddb)  = 1
         endif
         if ( iadmddb(18,iddb) .eq. 0 ) then
            iadmddb(18,iddb) = 1
            iadmddb(19,iddb) = 1
         endif
      enddo

      ! write

      call hyd%file_ddp%open()
      lunddp = hyd%file_ddp%unit

      write(lunddp,*) nddb
      write(lunddp,*) '# ddcouple output PART ddbound administration'
      do iddb = 1, nddb
         write(lunddp,'(22(i10,1x))') (iadmddb(iiddb,iddb),iiddb=1,22)
      enddo
      deallocate(iadmddb)

      return
      end
