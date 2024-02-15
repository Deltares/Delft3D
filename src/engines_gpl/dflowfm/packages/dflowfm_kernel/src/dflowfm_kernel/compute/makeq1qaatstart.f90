!----- AGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2017-2024.                                
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

 subroutine makeq1qaAtStart()
 use m_flow
 use m_flowgeom
 use unstruc_model, only: md_restartfile
 use unstruc_netcdf
 implicit none

 integer :: L, idfile, idvar, ierr, jaq1, jaqa

 jaq1 = 1
 jaqa = 1

 ! When it is a restart simulation, check if the restart file (rst/map) contains variable 'q1'/'qa'.
 ! If it contains 'q1'/'qa', then 'q1'/'qa' has been read before, and no need to compute it again here.
 if (len_trim(md_restartfile) > 0) then
    ierr = unc_open(md_restartfile, nf90_nowrite, idfile)
    call check_error(ierr, 'file '''//trim(md_restartfile)//'''')

    ierr = nf90_inq_varid(idfile, 'q1', idvar)
    if (ierr == nf90_noerr) then
       jaq1 = 0
    end if

    ierr = nf90_inq_varid(idfile, 'qa', idvar)
    if (ierr == nf90_noerr) then
       jaqa = 0
    end if
 end if

 if (jaq1 == 1) then
    do L = 1,lnx
       if (hu(L) > 0) then
          q1(L) = au(L)*( teta(L)*u1(L) + (1d0-teta(L))*u0(L) )
       else
          q1(L) = 0
       endif
    enddo
 end if

 if (jaqa == 1) then
    do L = 1,lnx
       if (hu(L) > 0) then
          qa(L) = au(L)*u1(L)
       else
          qa(L) = 0
       endif
    enddo
 end if

   end subroutine makeq1qaAtStart
