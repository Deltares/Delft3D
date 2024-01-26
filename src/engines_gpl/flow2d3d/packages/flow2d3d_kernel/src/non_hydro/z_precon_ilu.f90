subroutine z_precon_ilu(aak       ,bbk       ,cck       ,aak2      ,cck2      , &
                      & bbka      ,bbkc      ,kmax      ,icx       ,icy       , &
                      & nmmax     ,kfsz0     ,rj        ,p1        ,kfs       , &
                      & kfsmin    ,kfsmx0    ,gdp       )
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
! Computes the preconditioning
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
!
! Global variables
!
    integer                                          , intent(in) :: icx
    integer                                          , intent(in) :: icy
    integer                                          , intent(in) :: kmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                       :: nmmax  !  Description and declaration in dimens.igs
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)                    :: kfs    !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)                    :: kfsmin !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)                    :: kfsmx0 !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub, kmax) , intent(in) :: kfsz0  !  Description and declaration in esm_alloc_int.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)              :: aak
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)              :: aak2
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)              :: bbk
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)              :: bbka
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)              :: bbkc
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)              :: cck
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)              :: cck2
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)              :: p1     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax) , intent(in) :: rj
!
! Local variables
!
    integer :: ddb
    integer :: icxy
    integer :: k
    integer :: m
    integer :: maxk
    integer :: mink
    integer :: ndelta
    integer :: nm
    integer :: nmst
    integer :: nmstart
    integer :: nmu
    integer :: num
    integer :: nmd
    integer :: ndm
!
!! executable statements -------------------------------------------------------
!
    m1_nhy  => gdp%gdnonhyd%m1_nhy
    m2_nhy  => gdp%gdnonhyd%m2_nhy
    n1_nhy  => gdp%gdnonhyd%n1_nhy
    n2_nhy  => gdp%gdnonhyd%n2_nhy
    !
    ddb  = gdp%d%ddbound
    icxy = max(icx, icy)
    !
    ndelta  = n2_nhy - n1_nhy
    nmstart = (n1_nhy + ddb) + (m1_nhy - 1 + ddb)*icxy
    do m = m1_nhy, m2_nhy
       nmst = nmstart + (m - m1_nhy)*icxy
       do nm = nmst, nmst+ndelta
          if (kfs(nm) == 1) then
             nmu  = nm + icx
             num  = nm + icy
             nmd  = nm - icx
             ndm  = nm - icy
             mink = kfsmin(nm)
             if (kfsz0(nm,mink) /= 0) then
                p1(nm,mink) = rj(nm,mink) - aak2(nm,mink)*p1(ndm,mink) &
                         &                - aak (nm,mink)*p1(nmd,mink)
             endif
             do k = mink+1, kfsmx0(nm)
                if (kfsz0(nm,k) /= 0) then
                   p1(nm,k) = rj(nm,k) - bbka(nm,k)*p1(nm ,k-1) &
                            &          - aak2(nm,k)*p1(ndm,k  ) &
                            &          - aak (nm,k)*p1(nmd,k  )
                endif
             enddo
          endif
       enddo
    enddo
    do m = m2_nhy, m1_nhy, -1
       nmst = nmstart + (m - m1_nhy)*icxy
       do nm = nmst+ndelta, nmst, -1
          if (kfs(nm) == 1) then
             nmu  = nm + icx
             num  = nm + icy
             nmd  = nm - icx
             ndm  = nm - icy
             maxk = kfsmx0(nm)
             if (kfsz0(nm,maxk) /= 0) then
                p1(nm,maxk) = p1(nm,maxk) - cck2(nm,maxk)*p1(num,maxk) &
                            &             - cck (nm,maxk)*p1(nmu,maxk)
             endif
             do k = maxk-1, kfsmin(nm), -1
                if (kfsz0(nm,k) /= 0) then
                   p1(nm,k) = p1(nm,k) - bbkc(nm,k)*p1(nm ,k+1) &
                            &          - cck2(nm,k)*p1(num,k)   &
                            &          - cck (nm,k)*p1(nmu,k)
                endif
             enddo
          endif
       enddo
    enddo
end subroutine z_precon_ilu
