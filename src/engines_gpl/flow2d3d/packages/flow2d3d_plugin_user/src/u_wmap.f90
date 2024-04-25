subroutine u_wmap(lundat    ,header    ,runid     ,itime     ,idate     , &
                & timnow    ,mmax      ,nmax      ,kmax      ,nmaxus    , &
                & nsrc      ,ltur      ,lmaxd     ,lstsci    ,lsal      , &
                & ltem      ,ilay      ,zmodel    ,kfu       ,kfv       , &
                & kcs       ,kfs       ,kfumin    ,kfvmin    ,kfumax    , &
                & kfvmax    ,kfsmin    ,kfsmax    ,mnksrc    ,xz        , &
                & yz        ,dps       ,rho       ,s1        ,u1        , &
                & v1        ,w1        ,wphy      ,r1        ,p1        , &
                & rtur1     ,taubpu    ,taubpv    ,taubsu    ,taubsv    , &
                & vicww     ,dicww     ,vicuv     ,rich      ,umnldf    , &
                & vmnldf    )
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
! Example routine to write vertical eddy viscosity at
! layer=ILAY, water elevation + height at all waterlevel
! points (map) to a user defined output file
!
! Eddy viscosity are averaged to centre of the cell
! Values of KCx: 0= inactive;      1= active; 2= open boundary
! Values of KFx: 0= temporary dry; 1= wet
! WARNING: RBUFF0 and RBUFF1 uses the same memory location
! see call in postpr
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
   !
    implicit none
!
! Global variables
!
    integer                                         , intent(in) :: idate
    integer                                         , intent(in) :: ilay
    integer                                         , intent(in) :: itime
    integer                                         , intent(in) :: kmax !  Description and declaration in esm_alloc_int.f90
    integer                                         , intent(in) :: lmaxd !  Description and declaration in dimens.igs
    integer                                         , intent(in) :: lsal !  Description and declaration in dimens.igs
    integer                                         , intent(in) :: lstsci !  Description and declaration in esm_alloc_int.f90
    integer                                         , intent(in) :: ltem !  Description and declaration in dimens.igs
    integer                                         , intent(in) :: ltur !  Description and declaration in esm_alloc_int.f90
    integer                                         , intent(in) :: lundat
    integer                                         , intent(in) :: mmax !  Description and declaration in esm_alloc_int.f90
    integer                                         , intent(in) :: nmax !  Description and declaration in esm_alloc_int.f90
    integer                                         , intent(in) :: nmaxus !  Description and declaration in esm_alloc_int.f90
    integer                                         , intent(in) :: nsrc !  Description and declaration in esm_alloc_int.f90
    integer, dimension(7, nsrc)                     , intent(in) :: mnksrc !  Description and declaration in esm_alloc_int.f90
    integer, dimension(nmax, -1:mmax + 2)           , intent(in) :: kcs !  Description and declaration in esm_alloc_int.f90
    integer, dimension(nmax, -1:mmax + 2)           , intent(in) :: kfs !  Description and declaration in esm_alloc_int.f90
    integer, dimension(nmax, -1:mmax + 2)           , intent(in) :: kfsmax !  Description and declaration in esm_alloc_int.f90
    integer, dimension(nmax, -1:mmax + 2)           , intent(in) :: kfsmin !  Description and declaration in esm_alloc_int.f90
    integer, dimension(nmax, -1:mmax + 2)           , intent(in) :: kfu !  Description and declaration in esm_alloc_int.f90
    integer, dimension(nmax, -1:mmax + 2)           , intent(in) :: kfumax !  Description and declaration in esm_alloc_int.f90
    integer, dimension(nmax, -1:mmax + 2)           , intent(in) :: kfumin !  Description and declaration in esm_alloc_int.f90
    integer, dimension(nmax, -1:mmax + 2)           , intent(in) :: kfv !  Description and declaration in esm_alloc_int.f90
    integer, dimension(nmax, -1:mmax + 2)           , intent(in) :: kfvmax !  Description and declaration in esm_alloc_int.f90
    integer, dimension(nmax, -1:mmax + 2)           , intent(in) :: kfvmin !  Description and declaration in esm_alloc_int.f90
    logical                                         , intent(in) :: zmodel !  Description and declaration in procs.igs
    real(fp)                                        , intent(in) :: timnow
    real(prec), dimension(nmax, -1:mmax + 2)              , intent(in) :: dps !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(nmax, -1:mmax + 2)              , intent(in) :: s1 !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(nmax, -1:mmax + 2)              , intent(in) :: taubpu !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(nmax, -1:mmax + 2)              , intent(in) :: taubpv !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(nmax, -1:mmax + 2)              , intent(in) :: taubsu !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(nmax, -1:mmax + 2)              , intent(in) :: taubsv !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(nmax, -1:mmax + 2)              , intent(in) :: umnldf !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(nmax, -1:mmax + 2)              , intent(in) :: vmnldf !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(nmax, -1:mmax + 2)              , intent(in) :: xz !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(nmax, -1:mmax + 2)              , intent(in) :: yz !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(nmax, -1:mmax + 2, 0:kmax)      , intent(in) :: dicww !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(nmax, -1:mmax + 2, 0:kmax)      , intent(in) :: rich !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(nmax, -1:mmax + 2, 0:kmax)      , intent(in) :: vicww !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(nmax, -1:mmax + 2, 0:kmax)      , intent(in) :: w1 !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(nmax, -1:mmax + 2, 0:kmax, ltur), intent(in) :: rtur1 !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(nmax, -1:mmax + 2, kmax + 2)    , intent(in) :: vicuv !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(nmax, -1:mmax + 2, kmax)        , intent(in) :: p1 !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(nmax, -1:mmax + 2, kmax)        , intent(in) :: rho !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(nmax, -1:mmax + 2, kmax)        , intent(in) :: u1 !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(nmax, -1:mmax + 2, kmax)        , intent(in) :: v1 !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(nmax, -1:mmax + 2, kmax)        , intent(in) :: wphy !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(nmax, -1:mmax + 2, kmax, lstsci), intent(in) :: r1 !  Description and declaration in esm_alloc_real.f90
    character(*)                                    , intent(in) :: runid
    character(131), dimension(10)                                :: header !  Description and declaration in postpr.igs
!
! Local variables
!
    integer :: i
    integer :: m
    integer :: n
    logical :: lvar
    real(fp):: vw
    real(fp):: wh
    real(fp):: xydef
    real(fp):: zdef
!
!! executable statements -------------------------------------------------------
!
    !
    ! Initialisation
    !
    xydef = 999999.
    zdef = 999.999
    !
    ! Header;
    !
    header(1) = '* MAP output of run:' // runid
    header(2) = '* Simulation results at simulation time: '
    header(3) = '* column    1 : x-coordinates'
    header(4) = '* column    2 : y-coordinates'
    header(5) = '* column    3 : water level (m)'
    header(6) = '* column    4 : water height (m)'
    header(7) = '* column    5 : vert. eddy visc. (m2/s) at layer: '
    write (header(2)(42:), '(i8,1x,i6)') idate, itime
    write (header(7)(51:), '(i5)') ilay
    !
    do i = 1, 7
       write (lundat, '(  a)') header(i)
    enddo
    write (lundat, '(4i6)') mmax*nmaxus, 5, mmax, nmaxus
    !
    ! determine computed values
    ! if kcs = 1 and kfs = 1 then active an wet point; else point
    ! inactive
    !
    do n = 1, nmaxus
       do m = 1, mmax
          wh = s1(n, m) + real(dps(n, m),fp)
          lvar = ((kcs(n, m)==1) .and. (kfs(n, m)==1))
          vw = zdef
          if (ilay>=1 .and. ilay<=kmax .and. lvar) then
             vw = (vicww(n, m, ilay) + vicww(n, m, ilay - 1))/2.
          endif
          !
          ! Write values to output file
          !
          if (lvar) then
             write (lundat, '(2(e13.6,1x),5(1x,g13.6) )') &
                 & xz(n, m), yz(n, m), wh, s1(n, m), vw
          else
             write (lundat, '(2(e13.6,1x),5(1x,g13.6) )') &
                 & (xydef, i = 1, 2), (zdef, i = 1, 3)
          endif
       enddo
    enddo
end subroutine u_wmap
