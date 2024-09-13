subroutine neuMERGED_s1(a               ,b             ,c           ,d                     ,&
                        nmmax           ,nmlb          ,nmub        ,nst         ,icx      ,&
                        icy             ,gdp) 
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011.                                     
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
!  $Id: My_intersec.f90 
!  $HeadURL:
!!--description-----------------------------------------------------------------
!
!   Function:   compute merged agsqs
!
!  Author: Alberto Canestrelli
!
!!--declarations----------------------------------------------------------------
!
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    integer, dimension(:), pointer :: neuMERG
!
! global variables
!
    real(fp), dimension(nmlb:nmub)                       , intent(inout) :: a
    real(fp), dimension(nmlb:nmub)                       , intent(inout) :: b
    real(fp), dimension(nmlb:nmub)                       , intent(inout) :: c
    real(fp), dimension(nmlb:nmub)                       , intent(inout) :: d
    integer                                              , intent(in)    :: nst
    integer                                              , intent(in)    :: nmlb   
    integer                                              , intent(in)    :: nmub
    integer                                              , intent(in)    :: icx   
    integer                                              , intent(in)    :: icy
    integer                                              , intent(in)    :: nmmax
!
! local variables
!
    integer                    :: nm 
!
! executable statements -------------------------------------------------------
! 
    neuMERG => gdp%gdimbound%neuMERG
      do nm=1,nmmax                   
         if (neuMERG(nm)>0) then 
            if (neuMERG(nm)==1) then       ! merging in the ADI direction
             !  nmR = nm + icx
               a(nm) =  0._fp
               b(nm) =  1._fp
               c(nm) = -1._fp
               d(nm) =  0._fp
            elseif (neuMERG(nm)==2) then  ! merging in the ADI direction
             !  nmR = nm - icx
               a(nm) = -1._fp
               b(nm) =  1._fp
               c(nm) =  0._fp
               d(nm) =  0._fp
            endif
         endif
      enddo
!
!
RETURN
end subroutine neuMERGED_s1
