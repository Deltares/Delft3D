subroutine chkrefinement(gdp)
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
!!--description-----------------------------------------------------------------
!
!  File 'TMP_refinement' is filled by each mapper (one for each DD boundary),
!  with the refinement factor, both from left to right and from right to left.
!  See mapper_config.cpp.
!  The related ddb-line is added behind the refinement factor with a separating
!  ':'.
!  This subroutine, chkrefinement, checks whether a refinement factor is even
!  and if so writes a warning to the tri-diag file.
!  Each warning appears twice in each tri-diag file.
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    !
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
!
! Global variables
!
!
! Local variables
!
    integer           :: ierr
    integer           :: ipos
    integer           :: refinement
    integer           :: tmpfile
    integer, external :: newlun
    character(50)     :: filnam
    character(300)    :: line
!
!! executable statements -------------------------------------------------------
!
    filnam = 'TMP_refinement'
    if (gdp%gdprognm%nummappers == 0) return
    open(newunit=tmpfile, file=trim(filnam), iostat=ierr)
    do
       read(tmpfile,'(a)', iostat=ierr) line
       if (ierr /= 0) then
          close(tmpfile)
          return
       endif
       if (len_trim(line) <= 0) cycle
       read(line,*, iostat=ierr) refinement
       if (ierr /= 0) cycle
       if (mod(refinement,2) == 0) then
          ipos = index(line, ':')
          if (ipos == 0) then
             !
             ! separating : is missing. Use the first space as separator.
             !
             ipos = index(line, ' ')
          endif
          if (ipos == 0) then
             !
             ! No separating character at all. Write the complete line.
             !
             ipos = 1
          endif
          line = line(ipos+1:)
          call prterr(gdp%gdinout%lundia, 'U190', 'The following line in the ddb-file defines a 1 to even domaindecomposition boundary:')
          write(gdp%gdinout%lundia,'(2a)') '          ', trim(line)
          write(gdp%gdinout%lundia,'(a)')  '            Odd refinement is strongly advised'
       endif
    enddo
end subroutine chkrefinement
