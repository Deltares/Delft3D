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

      subroutine write_cco(file_cco, mmax  , nmax  , xdepth, ydepth, nolay)

      ! function : write a cco file

      ! global declarations

      use m_srstop
      use filmod                   ! module contains everything for the files
      implicit none

      ! declaration of the arguments

      type(t_dlwqfile)                       :: file_cco               ! aggregation-file
      integer                                :: mmax                   ! grid cells m direction
      integer                                :: nmax                   ! grid cells n direction
      real                                   :: xdepth(nmax,mmax)      ! x coordinate depth points
      real                                   :: ydepth(nmax,mmax)      ! y coordinate depth points
      integer                                :: nolay                  ! nolay

      ! local declarations

      real                                   :: x0                     ! x coordinate origin
      real                                   :: y0                     ! y coordinate origin
      real                                   :: alpha                  ! alpha
      integer                                :: npart                  ! npart
      integer                                :: ioerr                  ! error on file
      integer                                :: i                      ! loop counter
      real                                   :: rdum                   ! dummy
      integer       lun
      integer       filtyp
      integer       plform
      character*6   binary
      binary = 'BINARY'
      plform = PL_DOS

      x0    = 0.0
      y0    = 0.0
      alpha = 0.0
      npart = 0
      rdum  = 0.0

      call dlwqfile_open(file_cco)
      lun    = file_cco%unit_nr
      filtyp = file_cco%type

      if ( filtyp .ne. FT_ASC ) then
         write(file_cco%unit_nr,iostat=ioerr) mmax, nmax, x0, y0, alpha, npart, nolay
         if ( ioerr .ne. 0 ) then
            write(*,*) ' error writing cco file header record'
            call srstop(1)
         endif

         do i=1 , 2*npart+9
            write(file_cco%unit_nr,iostat=ioerr) rdum
            if ( ioerr .ne. 0 ) then
               write(*,*) ' error writing cco file dummy records'
               call srstop(1)
            endif
         enddo

         write(file_cco%unit_nr,iostat=ioerr) xdepth
         if ( ioerr .ne. 0 ) then
            write(*,*) ' error writing cco file xdepth'
            call srstop(1)
         endif
         write(file_cco%unit_nr,iostat=ioerr) ydepth
         if ( ioerr .ne. 0 ) then
            write(*,*) ' error writing cco file ydepth'
            call srstop(1)
         endif
      else
         write(file_cco%unit_nr,*,iostat=ioerr) mmax, nmax, x0, y0, alpha, npart, nolay
         if ( ioerr .ne. 0 ) then
            write(*,*) ' error writing cco file header record'
            call srstop(1)
         endif
         write(file_cco%unit_nr,*,iostat=ioerr) xdepth
         if ( ioerr .ne. 0 ) then
            write(*,*) ' error writing cco file xdepth'
            call srstop(1)
         endif
         write(file_cco%unit_nr,*,iostat=ioerr) ydepth
         if ( ioerr .ne. 0 ) then
            write(*,*) ' error writing cco file ydepth'
            call srstop(1)
         endif
      endif

      close(file_cco%unit_nr)
      file_cco%status = FILE_STAT_UNOPENED

      return
      end
