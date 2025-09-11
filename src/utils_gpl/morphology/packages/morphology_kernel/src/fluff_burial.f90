subroutine fluff_burial(flufflyr, dbodsd, lsed, lsedtot, nmlb, nmub, dt, morfac)
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
!    Function: Update fluff layer.
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use morphology_data_module, only: fluffy_type
    !
    implicit none
    !
    integer                                 , intent(in)    :: lsed
    integer                                 , intent(in)    :: lsedtot
    integer                                 , intent(in)    :: nmlb
    integer                                 , intent(in)    :: nmub
    real(fp)                                , intent(in)    :: dt
    real(fp)                                , intent(in)    :: morfac
    real(fp), dimension(1:lsedtot,nmlb:nmub), intent(inout) :: dbodsd
    type (fluffy_type)                      , intent(inout) :: flufflyr
!
! Local variables
!
    integer  :: l
    integer  :: nm
    real(fp) :: fac
    real(fp) :: dfluff
    real(fp) :: mfltot
    !
    real(fp), dimension(:,:), pointer :: bfluff0
    real(fp), dimension(:,:), pointer :: bfluff1
    real(fp), dimension(:,:), pointer :: mfluff
!
!! executable statements ------------------
!
    if (flufflyr%iflufflyr==1) then
       bfluff0 => flufflyr%bfluff0
       bfluff1 => flufflyr%bfluff1
       mfluff  => flufflyr%mfluff
       !
       do nm = nmlb, nmub
          mfltot = 0.0_fp
          do l = 1, lsed
             mfltot = mfltot + mfluff(l,nm) 
          enddo
          !
          if (mfltot>0.0_fp) then
             do l = 1, lsed
                fac          = mfluff(l,nm)/mfltot
                dfluff       = min(fac*min(mfltot*bfluff1(l,nm), bfluff0(l,nm))*dt,mfluff(l,nm))
                mfluff(l,nm) = mfluff(l,nm) - dfluff
                dbodsd(l,nm) = dbodsd(l,nm) + dfluff*morfac
             enddo
          endif
       enddo
    endif
end subroutine fluff_burial
