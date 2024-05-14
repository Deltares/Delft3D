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

      subroutine write_src(hyd)
      ! function : write a src file
      use m_logger, only : terminate_execution, get_log_unit_number
      use m_hydmod                   ! module contains everything for the hydrodynamics
      use time_module, only: sec2ddhhmmss
      implicit none

      ! declaration of the arguments

      type(t_hydrodynamics)                            :: hyd                   ! description of the hydrodynamics

      ! local declarations

      integer                                :: lunrep                ! unit number report file
      integer                                :: lunsrc                ! unit number sources file
      integer                                :: nowast                ! number of wasteloads
      integer                                :: noflow                ! number of flows
      integer                                :: nolay                 ! number of layers
      integer                                :: nobrk                 ! number of breakpoints
      real, allocatable                      :: waq_layers_frac(:)    ! frcations of the water column
      integer                                :: i                     ! loop counter
      integer                                :: ilay                  ! waq layer index
      integer                                :: k                     ! hyd layer index
      integer                                :: nok                   ! number of hyd layer per waq layer
      integer                                :: koff                  ! hyd layer offset
      real                                   :: flow                  ! flow for one wasteload
      integer                                :: iwaste                ! wasteload index
      integer                                :: ibrk                  ! breakpoint index
      integer                                :: itime                 ! time in ddddhhmmss format

      call get_log_unit_number(lunrep)

      nowast = hyd%wasteload_coll%current_size
      if ( nowast .le. 0 ) return
      nolay  = hyd%nolay
      nobrk  = hyd%wasteload_data%num_breakpoints

      if ( nowast .ne. hyd%wasteload_data%num_locations ) then
         write(lunrep,*) 'error, number of wasteloads in hyd file does not equal the data files'
         write(lunrep,*) 'number from hyd file:',nowast
         write(lunrep,*) 'number from data    :',hyd%wasteload_data%num_locations
         call terminate_execution(1)
      endif

      if ( nolay .gt. 1 ) then
         allocate(waq_layers_frac(nolay))
         waq_layers_frac = 0.0
         koff = 0
         do ilay = 1 , nolay
            nok = nint(hyd%waq_layers(ilay))
            do k = 1 , nok
               waq_layers_frac(ilay) = waq_layers_frac(ilay) + hyd%hyd_layers(koff+k)
            enddo
            koff = koff + nok
         enddo
         noflow = 0
         do i = 1, nowast
            if ( hyd%wasteload_coll%wasteload_pnts(i)%k .eq. 0 ) then
               noflow = noflow + nolay
            else
               noflow = noflow + 1
            endif
         enddo
      else
         noflow = nowast
         allocate(waq_layers_frac(1))
         waq_layers_frac = 1.0
      endif

      call hyd%file_src%open()
      lunsrc = hyd%file_src%unit

      if ( hyd%wasteload_coll%l_seconds ) then
         write(lunsrc,*) 'SECONDS  ; seconds used as breakpoint time'
      else
         write(lunsrc,*) '         ; ddhhmmss time format used'
      endif
      write(lunsrc,*) '    3    ; time dependent sources'
      write(lunsrc,*) '    1    ; block function'
      write(lunsrc,*) noflow,' ; no. of sources in this block'
      write(lunsrc,'(8(i6,1x))') (i,i=1,noflow)
      write(lunsrc,*) nobrk,' ; number of breakpoints'
      write(lunsrc,*) '1.0 1.0  ; scale factors'

      ! loop over the breakpoints

      do ibrk = 1 , nobrk

         if ( hyd%wasteload_coll%l_seconds ) then
            write(lunsrc,*) hyd%wasteload_data%times(ibrk),' ; breakpoint time'
         else
            itime = sec2ddhhmmss(hyd%wasteload_data%times(ibrk))
            write(lunsrc,'(i11.7,a)') itime,' ; breakpoint time'
         endif

         ! loop over the wasteloads write flow and dummy concentration

         do ilay = 1 , nolay
            do iwaste = 1 , nowast
               if ( ilay .eq. 1 .or. hyd%wasteload_coll%wasteload_pnts(iwaste)%k .eq. 0 ) then
                  if ( hyd%wasteload_coll%wasteload_pnts(iwaste)%k .eq. 0 ) then
                     flow = hyd%wasteload_data%values(1,iwaste,ibrk)*waq_layers_frac(ilay)
                  else
                     flow = hyd%wasteload_data%values(1,iwaste,ibrk)
                  endif
                  write(lunsrc,'(e14.6,a,i6)') flow,' 1.0 ; source:',iwaste
               endif
            enddo
         enddo

      enddo
      deallocate(waq_layers_frac)

      close(hyd%file_src%unit)
      hyd%file_src%status = FILE_STAT_UNOPENED

      return
      end
