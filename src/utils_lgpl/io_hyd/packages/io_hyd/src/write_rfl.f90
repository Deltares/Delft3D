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

      subroutine write_rfl(hyd)

      ! function : write the source in delwaq format

      ! global declarations

      use m_srstop
      use m_monsys
      use hydmod                   ! module contains everything for the hydrodynamics
      use time_module, only: mjd2date
      implicit none

      ! declaration of the arguments

      type(t_hyd)                            :: hyd                   ! description of the hydrodynamics

      ! local declarations

      integer                                :: lunrep                ! unit number report file
      integer                                :: lunrfl                ! unit number sources file
      integer                                :: nowast                ! number of wasteloads
      integer                                :: noflow                ! number of flows
      integer                                :: nolay                 ! number of layers
      integer                                :: nolay_waste           ! number of layers for specific load
      integer                                :: iwaste_lay            ! follow number load
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
      character(len=19)                      :: ctime                 ! time in 2007/01/01-00:00:00 format
      character(len=20)                      :: c20_name              ! time in 2007/01/01-00:00:00 format
      character(len=100)                     :: c100_name             ! time in 2007/01/01-00:00:00 format
      real                                   :: prev_flow             ! previous flow
      character(len=1), parameter            :: quote = ''''

      real*8            :: time
      real*8            :: time_ref
      integer           :: iyear
      integer           :: imonth
      integer           :: iday
      integer           :: ihour
      integer           :: imin
      real*8            :: rsec
      integer           :: isec
      integer           :: success

      call getmlu(lunrep)

      nowast = hyd%wasteload_coll%cursize
      if ( nowast .le. 0 ) return
      nolay  = hyd%nolay
      nobrk  = hyd%wasteload_data%no_brk

      if ( nowast .ne. hyd%wasteload_data%no_loc ) then
         write(lunrep,*) 'error, number of wasteloads in hyd file does not equal the data files'
         write(lunrep,*) 'number from hyd file:',nowast
         write(lunrep,*) 'number from data    :',hyd%wasteload_data%no_loc
         call srstop(1)
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

      call dlwqfile_open(hyd%file_rfl)
      lunrfl = hyd%file_rfl%unit_nr

      ! loop over the wasteloads

      iwaste_lay = 0
      do iwaste = 1 , nowast
         if ( hyd%wasteload_coll%wasteload_pnts(iwaste)%k .eq. 0 ) then
            nolay_waste = nolay
         else
            nolay_waste = 1
         endif
         do ilay = 1 , nolay_waste
            iwaste_lay = iwaste_lay + 1

            ! write header

            if ( iwaste_lay .lt. 10 ) then
               write(c100_name,'(i1,3a)') iwaste_lay,' (',trim(hyd%wasteload_coll%wasteload_pnts(iwaste)%name),')'
            elseif ( iwaste_lay .lt. 100 ) then
               write(c100_name,'(i2,3a)') iwaste_lay,' (',trim(hyd%wasteload_coll%wasteload_pnts(iwaste)%name),')'
            elseif ( iwaste_lay .lt. 1000 ) then
               write(c100_name,'(i3,3a)') iwaste_lay,' (',trim(hyd%wasteload_coll%wasteload_pnts(iwaste)%name),')'
            elseif ( iwaste_lay .lt. 10000 ) then
               write(c100_name,'(i4,3a)') iwaste_lay,' (',trim(hyd%wasteload_coll%wasteload_pnts(iwaste)%name),')'
            else
               write(c100_name,'(i5,3a)') iwaste_lay,' (',trim(hyd%wasteload_coll%wasteload_pnts(iwaste)%name),')'
            endif

            c20_name = c100_name
            write(lunrfl,'(4a)') 'ITEM ',quote,trim(c20_name),quote
            write(lunrfl,'( a)') 'CONCENTRATIONS FLOW Continuity'
            write(lunrfl,'( a)') 'TIME BLOCK'
            write(lunrfl,'( a)') 'DATA'

            prev_flow = -9999999.
            do ibrk = 1 , nobrk

               if ( hyd%wasteload_coll%wasteload_pnts(iwaste)%k .eq. 0 ) then
                  flow = hyd%wasteload_data%values(1,iwaste,ibrk)*waq_layers_frac(ilay)
               else
                  flow = hyd%wasteload_data%values(1,iwaste,ibrk)
               endif

               if ( abs(flow) .lt. 1.e-20 ) then
                  flow = 1.e-20
               endif

               if ( abs(flow-prev_flow) .gt. 1.e-6 .or. ibrk .eq. nobrk ) then
                  time = hyd%time_ref + hyd%wasteload_data%times(ibrk)/86400d0 + 0.1/86400d0 - 2400000.5d0 ! modified julian day
                  success = mjd2date(time,iyear,imonth,iday,ihour,imin,rsec)
                  isec = nint(rsec)
                  write(ctime,2000) iyear ,imonth, iday  ,ihour ,imin ,isec
                  if ( flow .lt. 0.0 ) then
                     write(lunrfl,'(a,'' '',e14.6,a)') ctime,flow,' 0.0'
                  else
                     write(lunrfl,'(a,'' '',e14.6,a)') ctime,flow,' 1.0'
                  endif
                  prev_flow = flow
               endif
            enddo
         enddo
      enddo
      deallocate(waq_layers_frac)

      close(hyd%file_rfl%unit_nr)
      hyd%file_rfl%status = FILE_STAT_UNOPENED

      return
 2000 format (i4.4,'/',i2.2,'/',i2.2,'-',i2.2,':',i2.2,':',i2.2)
      end
