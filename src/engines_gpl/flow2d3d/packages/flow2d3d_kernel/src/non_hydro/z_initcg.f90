subroutine z_initcg(aak       ,bbk       ,cck       ,aak2      ,cck2      , &
                  & bbka      ,bbkc      ,ddk       ,kmax      ,icx       , &
                  & icy       ,nmmax     ,nst       ,kfsz0     ,dinv      , &
                  & pbbk      ,pbbkc     ,pnhcor    ,rj        ,pj        , &
                  & p1        ,kfs       ,kfsmin    ,kfsmx0    ,gdp       )
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
! Initialisation for CG iterative solver.
! CG-method
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
    integer , pointer :: ifirst
!
! Global variables
!
    integer                                                       :: icx
    integer                                                       :: icy
    integer                                                       :: kmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                       :: nmmax  !  Description and declaration in dimens.igs
    integer                                                       :: nst
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)                    :: kfs    !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)                    :: kfsmin !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub)                    :: kfsmx0 !  Description and declaration in esm_alloc_int.f90
    integer , dimension(gdp%d%nmlb:gdp%d%nmub, kmax)              :: kfsz0  !  Description and declaration in esm_alloc_int.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)              :: aak
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)              :: aak2
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)              :: bbk
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)              :: bbka
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)              :: bbkc
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)              :: cck
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)              :: cck2
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)              :: ddk
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)              :: dinv
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)              :: p1     !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)              :: pbbk
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)              :: pbbkc
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax), intent(out) :: pj
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)              :: pnhcor !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub, kmax)              :: rj
!
! Local variables
!
    integer :: ddb
    integer :: icxy
    integer :: k
    integer :: m
    integer :: ndelta
    integer :: ndm
    integer :: nm
    integer :: nmd
    integer :: nmst
    integer :: nmstart
    integer :: nmu
    integer :: num
!
!! executable statements -------------------------------------------------------
!
    m1_nhy  => gdp%gdnonhyd%m1_nhy
    m2_nhy  => gdp%gdnonhyd%m2_nhy
    n1_nhy  => gdp%gdnonhyd%n1_nhy
    n2_nhy  => gdp%gdnonhyd%n2_nhy
    ifirst  => gdp%gdz_initcg%ifirst
    !
    ddb  = gdp%d%ddbound
    icxy = max(icx, icy)
    !
    ndelta  = n2_nhy - n1_nhy
    nmstart = (n1_nhy + ddb) + (m1_nhy - 1 + ddb)*icxy
    !
    ! scaling of the system of equations A x = b
    ! with  M A M , with M a diagonal matrix with
    ! element 1 / sqrt (D); Thus, at first column scaling,
    ! followed by row scaling
    !
    ! Now the equation M A M y = M b is solved.
    ! At the end of z_solcg, solution x is computed by x = M(-1) y
    !
    ! set dinv to 1.0 initially
    !
    dinv = 1.0_fp
    !
    do m = m1_nhy, m2_nhy
       nmst = nmstart + (m - m1_nhy)*icxy
       do nm = nmst, nmst + ndelta
          do k = kfsmin(nm), kfsmx0(nm)
             if (kfsz0(nm, k)/=0) then
                dinv(nm, k) = 1.0_fp/sqrt(bbk(nm, k))
             endif
          enddo
       enddo
    enddo
    !
    ! column scaling
    !
    do m = m1_nhy, m2_nhy
       nmst = nmstart + (m - m1_nhy)*icxy
       do nm = nmst, nmst + ndelta
          do k = kfsmin(nm), kfsmx0(nm)
             if (kfsz0(nm, k)/=0) then
                ndm = nm - icy
                nmd = nm - icx
                nmu = nm + icx
                num = nm + icy
                aak (nm, k) = aak (nm, k)*dinv(nmd, k)
                bbk (nm, k) = bbk (nm, k)*dinv(nm , k)
                cck (nm, k) = cck (nm, k)*dinv(nmu, k)
                ddk (nm, k) = ddk (nm, k)*dinv(nm , k)
                aak2(nm, k) = aak2(nm, k)*dinv(ndm, k)
                cck2(nm, k) = cck2(nm, k)*dinv(num, k)
             endif
          enddo
          do k = kfsmin(nm)+1, kfsmx0(nm)
             if (kfsz0(nm, k)/=0) then
                bbka(nm, k) = bbka(nm, k)*dinv(nm, k - 1)
             endif
          enddo
          do k = kfsmin(nm), kfsmx0(nm)-1
             if (kfsz0(nm, k)/=0) then
                bbkc(nm, k) = bbkc(nm, k)*dinv(nm, k + 1)
             endif
          enddo
       enddo
    enddo
    !
    ! row scaling
    !
    do m = m1_nhy, m2_nhy
       nmst = nmstart + (m - m1_nhy)*icxy
       do nm = nmst, nmst + ndelta
          do k = kfsmin(nm), kfsmx0(nm)
             if (kfsz0(nm, k)/=0) then
                aak (nm, k) = aak (nm, k)*dinv(nm, k)
                bbk (nm, k) = bbk (nm, k)*dinv(nm, k)
                cck (nm, k) = cck (nm, k)*dinv(nm, k)
                aak2(nm, k) = aak2(nm, k)*dinv(nm, k)
                cck2(nm, k) = cck2(nm, k)*dinv(nm, k)
                bbka(nm, k) = bbka(nm, k)*dinv(nm, k)
                bbkc(nm, k) = bbkc(nm, k)*dinv(nm, k)
             endif
          enddo
       enddo
    enddo
    !
    ! initialize solution pnhcor at the first time step
    !
    if (ifirst==1) then
       do nm = 1, nmmax
          do k = kfsmin(nm), kfsmx0(nm)
             if (kfsz0(nm, k)/=0) then
                pnhcor(nm, k) = 0.0_fp
             endif
          enddo
       enddo
       ifirst = 0
    endif
    !
    ! compute residu (store matrix vector product in array rj)
    !
    call z_matpro(aak       ,bbk       ,cck       ,aak2      ,cck2      , &
                & bbka      ,bbkc      ,kmax      ,icx       ,icy       , &
                & nmmax     ,kfsz0     ,pnhcor    ,rj        ,kfs       , &
                & kfsmin    ,kfsmx0    ,gdp       )
    !
    do m = m1_nhy, m2_nhy
       nmst = nmstart + (m - m1_nhy)*icxy
       do nm = nmst, nmst + ndelta
          do k = 1, kmax
             if (kfsz0(nm, k)/=0) then
                rj(nm, k) = ddk(nm, k) - rj(nm, k)
             else
                rj(nm, k) = 0.0_fp
             endif
          enddo
       enddo
    enddo
    !
    ! compute the LU decomposition (and store in pbbk/pbbkc)
    !
    call z_lu(bbka      ,bbk       ,bbkc      ,kmax      ,icx       , &
            & icy       ,nmmax     ,kfsz0     ,pbbk      ,pbbkc     , &
            & kfs       ,kfsmin    ,kfsmx0    ,gdp       )
    !
    ! compute the preconditioner (and store the result in array p1)
    !
    call z_precon(bbka      ,bbkc      ,pbbk      ,pbbkc     ,kmax      , &
                & icx       ,icy       ,nmmax     ,kfsz0     ,rj        , &
                & p1        ,kfs       ,kfsmin    ,kfsmx0    ,gdp       )
    !
    !  compute initial search direction
    !
    do m = m1_nhy, m2_nhy
       nmst = nmstart + (m - m1_nhy)*icxy
       do nm = nmst, nmst + ndelta
          do k = 1, kmax
             if (kfsz0(nm, k)/=0) then
                pj(nm, k) = p1(nm, k)
             else
                pj(nm, k) = 0.0_fp
             endif
          enddo
       enddo
    enddo
end subroutine z_initcg
