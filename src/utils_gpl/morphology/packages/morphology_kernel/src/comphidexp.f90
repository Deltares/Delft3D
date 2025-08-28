subroutine comphidexp(frac      ,dm        ,nmmax     ,lsedtot   , &
                    & sedd50    ,hidexp    ,ihidexp   ,asklhe    , &
                    & mwwjhe    ,nmlb      ,nmub      )
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2025.                                
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
    !
    implicit none
!
! Local parameters
!
    real(hp), parameter :: log10_19 = 1.27875360095283_hp
    real(fp), parameter :: dmmin    = 1.0e-4_fp                                  ! minimum value of dm
!
! Arguments
!
    integer                                             , intent(in)  :: lsedtot
    integer                                             , intent(in)  :: nmlb
    integer                                             , intent(in)  :: nmub
    integer                                             , intent(in)  :: nmmax   !  Description and declaration in dimens.igs
    real(fp), dimension(lsedtot)                        , intent(in)  :: sedd50  !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(nmlb:nmub, lsedtot)                           :: hidexp
    real(fp), dimension(nmlb:nmub, lsedtot)             , intent(in)  :: frac
    real(fp), dimension(nmlb:nmub)                      , intent(in)  :: dm
    real(fp)                                            , intent(in)  :: asklhe
    real(fp)                                            , intent(in)  :: mwwjhe
    integer                                             , intent(in)  :: ihidexp
!
! Local variables
!
    integer  :: l
    integer  :: ll
    integer  :: nm
    real(fp) :: dd
    real(fp) :: phi
    real(fp) :: pei
    real(fp) :: dmloc    ! local copy of dm limited to value larger than dmmin specified here
!
!! executable statements -------------------------------------------------------
!
    select case(ihidexp)
    case(2) ! Egiazaroff
       !
       do nm = 1, nmmax
          dmloc = max(dm(nm), dmmin)
          do l = 1, lsedtot
             !
             dd           = sedd50(l)/dmloc
             hidexp(nm,l) = (log10_19 / (log10_19 + log10(dd)))**2.0
             !
          enddo
       enddo
       !
    case(3) ! Ashida & Michiue
       !
       do nm = 1, nmmax
          dmloc = max(dm(nm), dmmin)
          do l = 1, lsedtot
             !
             dd = sedd50(l)/dmloc
             !
             if (dd<0.38889) then
                hidexp(nm,l) = 0.8429 / dd
             else
                hidexp(nm,l) = (log10_19 / (log10_19 + log10(dd)))**2.0
             endif
             !
          enddo
       enddo
       !
    case(4) ! Parker, Klingeman, McLean
       ! Soehngen, Kellermann, Loy
       !
       do nm = 1, nmmax
          dmloc = max(dm(nm), dmmin)
          do l = 1, lsedtot
             !
             dd           = sedd50(l)/dmloc
             hidexp(nm,l) = dd**(-asklhe)
             !
          enddo
       enddo
       !
    case(5) ! Wu, Wang, Jia
       !
       do nm = 1, nmmax
          do l = 1, lsedtot
             !
             phi = 0.0
             do ll = 1, lsedtot
                phi = phi + frac(nm,ll) * sedd50(ll) / (sedd50(l) + sedd50(ll))
             enddo
             pei          = 1.0 - phi
             hidexp(nm,l) = (pei/phi)**mwwjhe
             !
          enddo
       enddo
       !
    case default ! (1)
       !
       ! no hiding & exposure, hidexp initialized to 1.0 in erosed
       ! keep these values
       !
    endselect
end subroutine comphidexp
