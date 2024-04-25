subroutine getfixfac(bedcomp   ,nmlb      ,nmub      ,nval      ,nmmax     , &
                   & fixfac    ,ffthresh  )
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
!
! Local variables
!
    integer  :: l
    integer  :: nm
    real(fp) :: ffac
    real(fp) :: thresh
!
!! executable statements -------------------------------------------------------
!
    call getalluvthick(bedcomp, fixfac, nmlb, nmub, nval)
    ! The FIXFAC array contains at this stage the sediment thickness!
    !
    thresh = max(1.0e-10_fp,ffthresh)
    do l = 1, nval
       do nm = max(nmlb,1), min(nmmax,nmub)
          fixfac(nm, l) = min(max(fixfac(nm, l)/thresh, 0.0_fp), 1.0_fp)
       enddo
    enddo
end subroutine getfixfac
