subroutine prhead(lunprt    ,header    ,nuprpg    ,nuprln    )
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
! NONE
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    implicit none
!
! Global variables
!
    integer, intent(in)            :: lunprt !  Description and declaration in inout.igs
    integer, intent(out)           :: nuprln !  Description and declaration in postpr.igs
    integer         :: nuprpg !  Description and declaration in postpr.igs
    character(131), dimension(10) :: header !  Description and declaration in postpr.igs
!
!
! Local variables
!
    integer                        :: i                    ! Help var. 
!
!
!! executable statements -------------------------------------------------------
!
    !
    !
    !               Date: 01-08-2002
    !         Programmer: H.H. Leepel
    !         CVS header
    !            $Author: Mourits $
    !              $Date: 18-04-03 16:51 $
    !            $Source: /u/FLOW/cvsroot/FLOW/output/prhead.f,v $
    !          $Revision: 1 $
    !
    !   Calling routine :              PRTHIS
    !                                  PRTMAP
    !
    !   Called  routines:              NONE
    !
    !
    !  declarations and specifications
    !
    !
    !
    !-----Update page number in HEADER
    !
    nuprpg = nuprpg + 1
    write (header(2)(82:86), '(i5)') nuprpg
    !
    !-----Write FF character
    !
    write (lunprt, '(a1)') char(12)
    !
    !-----Write HEADER
    !
    do i = 1, 10
       write (lunprt, '(a)') header(i)
    enddo
    write (lunprt, '(a)')
    nuprln = 10
end subroutine prhead
