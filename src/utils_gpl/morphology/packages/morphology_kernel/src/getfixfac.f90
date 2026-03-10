subroutine getfixfac(bedcomp   ,nmlb      ,nmub      ,nval      ,nmmax     , &
                   & fixfac    ,ffthresh  ,ithresh)
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2026.                                
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
!    Function: - Update underlayer bookkeeping system for erosion/sedimentation
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision 
    use bedcomposition_module
    !
    implicit none
!
! Arguments
!
    integer                                            , intent(in)  :: nmmax
    integer                                            , intent(in)  :: nmlb
    integer                                            , intent(in)  :: nmub
    integer                                            , intent(in)  :: nval
    type(bedcomp_data)                                 , intent(in)  :: bedcomp
    real(fp)                                           , intent(in)  :: ffthresh
    real(fp), dimension(nmlb:nmub, nval)               , intent(out) :: fixfac
    integer                                            , intent(in)  :: ithresh
!
! Local variables
!
    integer  :: l
    integer  :: nm
    real(fp) :: thresh
    real(fp) :: thick
    real(fp)   , dimension(:)     , pointer :: thtrlyr  
    integer, parameter :: THRESH_BASED_ON_THICKNESS = 2 
!
!! executable statements -------------------------------------------------------
!
    !
    if (ithresh == THRESH_BASED_ON_THICKNESS) then
       thtrlyr     => bedcomp%settings%thtrlyr
       do l = 1, nval
          do nm = nmlb, nmub
             call getthicklayer(bedcomp, nm, 1, thick)
             thresh = max(1.0e-10_fp,thtrlyr(nm))
             fixfac(nm, l) = min(max(thick/thresh, 0.0_fp), 1.0_fp)
          enddo
       enddo
    else !1 = constant value set by `thresh`
       call getalluvthick(bedcomp, fixfac, nmlb, nmub, nval)
       !call getsedthick(bedcomp, fixfac)  <- new call 
       ! The FIXFAC array contains at this stage the sediment thickness!
       thresh = max(1.0e-10_fp,ffthresh)
       do l = 1, nval
          do nm = nmlb, nmub
             fixfac(nm, l) = min(max(fixfac(nm, l)/thresh, 0.0_fp), 1.0_fp)
          enddo
       enddo
    endif    
end subroutine getfixfac
