subroutine upwbed(su        ,sv        ,suu       ,svv       ,kfu       , &
                & kfv       ,kcs       ,kfsed     ,lsedtot   , &
                & nmmax     ,icx       ,icy       ,sutot     ,svtot     , &
                & gdp       )
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
!    Function: Copy transport rate from cell centres to velocity points
!              using an upwind or central approach.
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use sediment_basics_module, only: has_bedload
    !
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer, dimension(:)                , pointer :: tratyp
    real(fp)                             , pointer :: bed
    type (mornumericstype)               , pointer :: mornum
!
! Global variables
!
    integer                                            , intent(in)  :: lsedtot
    integer                                            , intent(in)  :: icx
    integer                                            , intent(in)  :: icy
    integer                                            , intent(in)  :: nmmax
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in)  :: kcs
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in)  :: kfsed
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in)  :: kfu
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)         , intent(in)  :: kfv
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, lsedtot), intent(in)  :: su
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, lsedtot), intent(in)  :: sv
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, lsedtot), intent(in)  :: sutot
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, lsedtot), intent(in)  :: svtot
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, lsedtot), intent(out) :: suu
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, lsedtot), intent(out) :: svv
!
! Local variables
!
    integer  :: l
    integer  :: nm
    integer  :: nmu
    integer  :: num
    integer  :: nmd
    integer  :: ndm
    integer  :: ndmu
    integer  :: numd
    integer  :: numu
    logical  :: laterallyaveragedbedload
    logical  :: upwindbedload
    real(fp) :: suv1
    real(fp) :: suv2
!
!! executable statements -------------------------------------------------------
!
    tratyp              => gdp%gdsedpar%tratyp
    bed                 => gdp%gdmorpar%bed
    mornum              => gdp%gdmorpar%mornum
    !
    upwindbedload            = mornum%upwindbedload
    laterallyaveragedbedload = mornum%laterallyaveragedbedload
    !
    do l = 1, lsedtot
       ! if the transport of the fraction may include a bedload component
       if (has_bedload(tratyp(l))) then
          do nm = 1, nmmax
             !
             ! Try a scheme that reverts to central if transport directions oppose (G. Lesser) 
             !
             ! set bedload transport at u points
             !
             nmu = nm + icx
             !
             ! if active velocity point with two adjacent active sediment cells
             ! (done to prevent bed-load transport into kfsed=0 cells)
             !
             if ((kfu(nm)*kfsed(nm)*kfsed(nmu)) /= 0) then
                if (laterallyaveragedbedload) then
                   ndm = nm - icy
                   num = nm + icy
                   ndmu = nm + icx - icy
                   numu = nm + icx + icy
                   suv1 = (4.0 * su(nm, l) &
                         & + kfv(nm) * (su(num, l) - su(nm,l)) &
                         & + kfv(ndm) * (su(ndm, l) - su(nm,l)))/4.0
                   suv2 = (4.0*su(nmu,l) &
                         & + kfv(nmu) * (su(numu,l) - su(nmu,l)) &
                         & + kfv(ndmu) * (su(ndmu,l) - su(nmu,l)))/4.0
                else
                   suv1 = su(nm, l)
                   suv2 = su(nmu, l)
                endif
                if (kcs(nmu) == 3) then
                   !
                   ! correction for domain decomposition:
                   !
                   suu(nm, l) = suv1
                elseif (kcs(nm) == 3) then
                   suu(nm, l) = suv2
                elseif (sutot(nm, l)>0.0 .and. sutot(nmu, l)>0.0 .and. upwindbedload) then
                   suu(nm, l) = suv1
                elseif (sutot(nm, l)<0.0 .and. sutot(nmu, l)<0.0 .and. upwindbedload) then
                   suu(nm, l) = suv2
                else
                   suu(nm, l) = (suv1 + suv2)/2.0
                endif
             else
                suu(nm, l) = 0.0
             endif
             !
             ! set bedload transport at v points
             !
             num = nm + icy
             !
             ! if active velocity point with two adjacent active sediment cells
             ! (done to prevent bed-load transport into kfsed=0 cells)
             !
             if ((kfv(nm)*kfsed(nm)*kfsed(num)) /= 0) then
                if (laterallyaveragedbedload) then
                   nmd = nm - icx
                   nmu = nm + icx
                   numd = nm - icx + icy
                   numu = nm + icx + icy
                   suv1 = (4.0 * sv(nm, l) &
                         & + kfu(nm) * (sv(nmu, l) - sv(nm,l)) &
                         & + kfu(nmd) * (sv(nmd, l) - sv(nm,l)))/4.0
                   suv2 = (4.0*sv(num,l) &
                         & + kfu(num) * (sv(numu,l) - sv(num,l)) &
                         & + kfu(numd) * (sv(numd,l) - sv(num,l)))/4.0
                else
                   suv1 = sv(nm, l)
                   suv2 = sv(num, l)
                endif
                if (kcs(num) == 3) then
                   !
                   ! correction for domain decomposition:
                   !
                   svv(nm, l) = suv1
                elseif (kcs(nm) == 3) then
                   svv(nm, l) = suv2
                elseif (svtot(nm, l)>0.0 .and. svtot(num, l)>0.0 .and. upwindbedload) then
                   svv(nm, l) = suv1
                elseif (svtot(nm, l)<0.0 .and. svtot(num, l)<0.0 .and. upwindbedload) then
                   svv(nm, l) = suv2
                else
                   svv(nm, l) = (suv1 + suv2)/2.0
                endif
             else
                svv(nm, l) = 0.0
             endif
          enddo
       endif
    enddo
end subroutine upwbed
