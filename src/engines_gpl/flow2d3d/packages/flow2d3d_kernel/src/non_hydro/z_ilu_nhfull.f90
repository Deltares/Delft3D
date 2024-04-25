subroutine z_ilu_nhfull(aak       ,bbk       ,cck       ,aak2      ,cck2      , &
                      & bbka      ,bbkc      ,kmax      ,icx       , &
                      & icy       ,nmmax     ,kfsz0     ,kfsmin    , kfsmx0   , &
                      & dinv      ,kfs       ,gdp)
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
! Computes ILU decomposition and stores it in dinv
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer , pointer :: m1_nhy
    integer , pointer :: m2_nhy
    integer , pointer :: n1_nhy
    integer , pointer :: n2_nhy
    real(fp), pointer :: milu
!
! Global variables
!
    integer                                         , intent(in)  :: icx
    integer                                         , intent(in)  :: icy
    integer                                         , intent(in)  :: kmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                       :: nmmax  !  Description and declaration in dimens.igs
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)                    :: kfs    !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)                    :: kfsmin !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)                    :: kfsmx0 !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub, kmax), intent(in)  :: kfsz0  !  Description and declaration in esm_alloc_int.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax), intent(in)  :: bbk
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax), intent(in)  :: bbka
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax), intent(in)  :: bbkc
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)              :: aak
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)              :: aak2
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)              :: cck
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)              :: cck2
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)              :: dinv
!
! Local variables
!
    integer  :: ddb
    integer  :: icxy
    integer  :: k
    integer  :: m
    integer  :: mink
    integer  :: ndelta
    integer  :: nm
    integer  :: nmst
    integer  :: nmstart 
    integer  :: nmu
    integer  :: num
    integer  :: nmd
    integer  :: ndm
    real(fp) :: bi
!
!! executable statements -------------------------------------------------------
!
    m1_nhy  => gdp%gdnonhyd%m1_nhy
    m2_nhy  => gdp%gdnonhyd%m2_nhy
    n1_nhy  => gdp%gdnonhyd%n1_nhy
    n2_nhy  => gdp%gdnonhyd%n2_nhy
    milu    => gdp%gdnonhyd%milu
    !
    ddb  = gdp%d%ddbound
    icxy = max(icx, icy)
    !
    ndelta  = n2_nhy - n1_nhy
    nmstart = (n1_nhy+ddb) + (m1_nhy-1+ddb)*icxy
    !
    dinv = 1.0_fp
    !
    if ((milu > 0.00001_fp) .and. (milu < 1.1_fp)) then
       !
       ! Modified ILU, should usually only be done for 0<=milu<=1.
       !
       do m = m1_nhy, m2_nhy
          nmst = nmstart + (m-m1_nhy)*icxy
          do nm = nmst, nmst + ndelta
             if (kfs(nm) == 1) then
                nmd  = nm - icx
                ndm  = nm - icy
                mink = kfsmin(nm)
                if (kfsz0(nm,mink) /= 0) then
                   dinv(nm, mink) = 1.0_fp / (bbk(nm, mink)                                                                          &
                                  &        - aak2(nm,mink) * dinv(ndm,mink) * (cck2(ndm,mink)+milu*(bbkc(ndm,mink)+cck (ndm,mink)))  &
                                  &        - aak (nm,mink) * dinv(nmd,mink) * (cck (nmd,mink)+milu*(bbkc(nmd,mink)+cck2(nmd,mink))) )
                endif
                do k = mink+1, kfsmx0(nm)
                   if (kfsz0(nm,k) /= 0) then
                      dinv(nm,k) = 1.0_fp / (bbk(nm,k)                                                                     &
                                 &           - bbka(nm,k) * dinv(nm,k-1) * (bbkc(nm,k-1)+milu*(cck2(nm,k-1)+cck (nm,k-1))) &
                                 &           - aak2(nm,k) * dinv(ndm,k)  * (cck2(ndm,k) +milu*(bbkc(ndm,k) +cck (ndm,k)))  &
                                 &           - aak (nm,k) * dinv(nmd,k)  * (cck (nmd,k) +milu*(bbkc(nmd,k) +cck2(nmd,k)))  )
                   endif
                enddo
             endif
          enddo
       enddo
    else
       do m = m1_nhy, m2_nhy
          nmst = nmstart + (m-m1_nhy)*icxy
          do nm = nmst, nmst+ndelta
             if (kfs(nm) == 1) then
                nmd  = nm - icx
                ndm  = nm - icy
                mink = kfsmin(nm)
                if (kfsz0(nm,mink) /= 0) then
                   dinv(nm,mink) = 1.0_fp / (bbk(nm,mink)                                   &
                                 &        - aak2(nm,mink) * dinv(ndm,mink) * cck2(ndm,mink) &
                                 &        - aak (nm,mink) * dinv(nmd,mink) * cck (nmd,mink) )
                endif
                do k = mink+1, kfsmx0(nm)
                   if (kfsz0(nm,k) /= 0) then
                      dinv(nm,k) = 1.0_fp / (bbk(nm,k)                                &
                                 &        - bbka(nm,k) * dinv(nm,k-1) * bbkc(nm, k-1) &
                                 &        - aak2(nm,k) * dinv(ndm,k)  * cck2(ndm,k)   &
                                 &        - aak (nm,k) * dinv(nmd,k)  * cck (nmd,k)   )
                   endif
                enddo
             endif
          enddo
       enddo
    endif
    do m = m1_nhy, m2_nhy
       nmst = nmstart + (m-m1_nhy)*icxy
       do nm = nmst, nmst+ndelta
          if (kfs(nm) == 1) then
             do k = kfsmin(nm), kfsmx0(nm)
                if (kfsz0(nm,k) /= 0) then
                   dinv(nm,k) = sqrt(dinv(nm,k))
                endif
             enddo
          endif
       enddo
    enddo
end subroutine z_ilu_nhfull
