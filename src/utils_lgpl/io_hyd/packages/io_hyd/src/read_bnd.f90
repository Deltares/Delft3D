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
      subroutine read_bnd(file_bnd, openbndsect_coll)

      ! function : read a bnd file and add to the collection

      ! global declarations

      use m_srstop
      use m_monsys
      use m_waq_file                   ! module contains everything for the files
      use m_hydmod                   ! module contains everything for the hydrodynamic description
      use rd_token                 ! tokenized reading

      implicit none

      ! declaration of the arguments

      type(t_file)                       :: file_bnd               ! aggregation-file
      type(t_openbndsect_coll)               :: openbndsect_coll       ! collection of openbndsects


      ! local declarations

      integer                                :: i                      ! loop counter
      integer                                :: lunrep                 ! unit number report file
      integer                                :: int                    ! integer token from input
      real                                   :: reel                   ! real token from input
      integer                                :: ierr                   ! error indication
      integer                                :: ierr_alloc             ! error indication
      integer                                :: no_sect                ! number of sections
      integer                                :: i_sect                 ! index of section
      integer                                :: no_bnd                 ! number of boundaries in section
      integer                                :: i_bnd                  ! index of boundary
      type(t_openbnd_section)                    :: openbndsect            ! single section read
      type(t_open_boundary_line)                     :: openbndlin             ! single open boundary lin
      integer                                :: iret                   ! return value

      call getmlu(lunrep)

      ! open file

      call file_bnd%open()

      ! initialise tokenised reading
      ilun    = 0
      ilun(1) = file_bnd%unit
      lch (1) = file_bnd%name
      npos   = 1000
      cchar  = ';'
      ierr = 0

      ! read

      if ( gettoken( no_sect, ierr) .ne. 0 ) then
         write(lunrep,*) ' error reading boundary file:',trim(file_bnd%name)
         write(lunrep,*) ' expected integer with number of sections'
         goto 200
      endif

      openbndsect%openbndlin_coll%current_size = 0
      openbndsect%openbndlin_coll%maxsize = 0
      openbndsect%openbndlin_coll%openbndlin_pnts => null()
      do i_sect = 1 , no_sect

         if ( gettoken( openbndsect%name, ierr) .ne. 0 ) then
            write(lunrep,*) ' error reading boundary file:',trim(file_bnd%name)
            write(lunrep,*) ' character expected with name of section'
            goto 200
         endif
         if ( gettoken( no_bnd, ierr) .ne. 0 ) then
            write(lunrep,*) ' error reading boundary file:',trim(file_bnd%name)
            write(lunrep,*) ' expected integer with number of boundary in this section'
            goto 200
         endif

         do i_bnd = 1 , no_bnd
            if ( gettoken( openbndlin%ibnd, ierr) .ne. 0 ) then
               write(lunrep,*) ' error reading boundary file:',trim(file_bnd%name)
               write(lunrep,*) ' expected integer with boundary number'
               goto 200
            end if
            if ( gettoken( openbndlin%x1, ierr) .ne. 0 ) then
               write(lunrep,*) ' error reading boundary file:',trim(file_bnd%name)
               write(lunrep,*) ' expected real with first x coordinate'
               goto 200
            end if
            if ( gettoken( openbndlin%y1, ierr) .ne. 0 ) then
               write(lunrep,*) ' error reading boundary file:',trim(file_bnd%name)
               write(lunrep,*) ' expected real with first y coordinate'
               goto 200
            end if
            if ( gettoken( openbndlin%x2, ierr) .ne. 0 ) then
               write(lunrep,*) ' error reading boundary file:',trim(file_bnd%name)
               write(lunrep,*) ' expected real with second x coordinate'
               goto 200
            end if
            if ( gettoken( openbndlin%y2, ierr) .ne. 0 ) then
               write(lunrep,*) ' error reading boundary file:',trim(file_bnd%name)
               write(lunrep,*) ' expected real with second y coordinate'
               goto 200
            end if
            iret = openbndsect%openbndlin_coll%add(openbndlin)
         enddo

         ! add section to collection

         iret = openbndsect_coll%add(openbndsect)
         openbndsect%openbndlin_coll%current_size = 0
         openbndsect%openbndlin_coll%maxsize = 0
         openbndsect%openbndlin_coll%openbndlin_pnts => null()

      enddo
  200 continue
      if ( ierr .ne. 0 ) then
         call srstop(1)
      endif

      close(file_bnd%unit)
      file_bnd%status = FILE_STAT_UNOPENED

      return
      end
