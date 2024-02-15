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

      subroutine read_poi(file_poi, noq   , noq1    , noq2  , noq3  , &
                          ipoint  )

      ! function : read a poi file and check dimensions

      ! global declarations

      use m_srstop
      use m_monsys
      use filmod                   ! module contains everything for the files
      implicit none

      ! declaration of the arguments

      type(t_dlwqfile)                       :: file_poi               ! pointer-file
      integer                                :: noq                    ! noq
      integer                                :: noq1                   ! noq1
      integer                                :: noq2                   ! noq2
      integer                                :: noq3                   ! noq3
      integer                                :: ipoint(4,noq)          ! pointer table

      ! local declarations

      integer                                :: i,j,ip1,ip2            ! indxes in pointer table
      integer                                :: ioerr                  ! error on file
      integer                                :: lunrep                 ! unit number report file

      call getmlu(lunrep)

      call dlwqfile_open(file_poi)

      if ( noq1 .gt. 0 ) then
         read(file_poi%unit_nr,iostat=ioerr) ((ipoint(i,j),i=1,4),j=1,noq1)
         if ( ioerr .ne. 0 ) then
            write(lunrep,*) ' error reading poi file'
            call srstop(1)
         endif
      endif
      if ( noq2 .gt. 0 ) then
         ip1 = noq1 + 1
         ip2 = noq1 + noq2
         read(file_poi%unit_nr,iostat=ioerr) ((ipoint(i,j),i=1,4),j=ip1,ip2)
         if ( ioerr .ne. 0 ) then
            write(lunrep,*) ' error reading poi file'
            call srstop(1)
         endif
      endif
      if ( noq3 .gt. 0 ) then
         ip1 = noq1 + noq2 + 1
         ip2 = noq1 + noq2 + noq3
         read(file_poi%unit_nr,iostat=ioerr) ((ipoint(i,j),i=1,4),j=ip1,ip2)
         if ( ioerr .ne. 0 ) then
            write(lunrep,*) ' error reading poi file'
            call srstop(1)
         endif
      endif

      close(file_poi%unit_nr)
      file_poi%status = FILE_STAT_UNOPENED

      return
      end
