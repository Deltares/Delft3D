!----- AGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2017-2023.                                
!                                                                               
!  This file is part of Delft3D (D-Flow Flexible Mesh component).               
!                                                                               
!  Delft3D is free software: you can redistribute it and/or modify              
!  it under the terms of the GNU Affero General Public License as               
!  published by the Free Software Foundation version 3.                         
!                                                                               
!  Delft3D  is distributed in the hope that it will be useful,                  
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU Affero General Public License for more details.                          
!                                                                               
!  You should have received a copy of the GNU Affero General Public License     
!  along with Delft3D.  If not, see <http://www.gnu.org/licenses/>.             
!                                                                               
!  contact: delft3d.support@deltares.nl                                         
!  Stichting Deltares                                                           
!  P.O. Box 177                                                                 
!  2600 MH Delft, The Netherlands                                               
!                                                                               
!  All indications and logos of, and references to, "Delft3D",                  
!  "D-Flow Flexible Mesh" and "Deltares" are registered trademarks of Stichting 
!  Deltares, and remain the property of Stichting Deltares. All rights reserved.
!                                                                               
!-------------------------------------------------------------------------------

! 
! 

!> read particles release file
subroutine read_particles_release_file(partrelfile)
   use m_particles
   use m_missing
   use m_alloc
   use unstruc_messages
   implicit none

   character(len=255), intent(in) :: partrelfile   !< release particle file
   character(len=1000) :: line
   character(len=1)    :: char
   integer             :: lun, ios, ipart, linenr
   double precision    :: tr, xr, yr, zr

   call oldfil(lun, partrelfile)

   Nrpart = 0
   linenr = 0
   ios = 0

   do while ( ios==0 )
      read(lun, '(a1000)', iostat=ios) line
      linenr = linenr + 1
      if (ios==0) then
         read(line, '(a)', iostat=ios) char
         if (char.ne.'*'.and.char.ne.'#'.and.char.ne.'!') then
            read(line, *, iostat=ios) tr, xr, yr, zr
            if (ios==0) then
               Nrpart = Nrpart + 1
            endif
         endif
      endif
   end do

   if (Nrpart.gt.0) then
      ipart = 0
      linenr = 0
      ios = 0

      rewind (lun)
      call realloc(trpart, Nrpart)
      call realloc(xrpart, Nrpart)
      call realloc(yrpart, Nrpart)
      call realloc(zrpart, Nrpart)

      do while ( ios==0 )
         read(lun, '(a1000)', iostat=ios) line
         linenr = linenr + 1
         if (ios==0) then
            read(line, '(a)', iostat=ios) char
            if (char.ne.'*'.and.char.ne.'#'.and.char.ne.'!') then
               ipart = ipart + 1
               read(line, *, iostat=ios) trpart(ipart), xrpart(ipart), yrpart(ipart), zrpart(ipart)
               if (ios.ne.0 .or. trpart(ipart).eq.dmiss .or. xrpart(ipart).eq.dmiss .or.  yrpart(ipart).eq.dmiss .or. zrpart(ipart).eq.dmiss) then
                  call mess(LEVEL_ERROR, 'error reading particle release file '''//trim(partrelfile)//''' at line', linenr)
               endif
               if (ipart.gt.1) then
                  if (trpart(ipart).lt.trpart(ipart-1)) then
                     call mess(LEVEL_ERROR, 'timing in particle release file '''//trim(partrelfile)//''' is not incremental at line', linenr)
                  endif
               endif
            endif
         endif
      end do
      irpart = 1
      NpartOut = NpartOut + Nrpart
   endif
   close(lun)
end subroutine read_particles_release_file
