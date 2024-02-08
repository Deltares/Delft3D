subroutine incrbc(timsec    ,j         ,nmmaxj    ,nmax      ,norow     , &
                & nocol     ,irocol    ,zetaif    ,ctif      ,stif      , &
                & zetabf    ,ctbf      ,stbf      ,zbmnf     ,wenf      , &
                & wenfm     ,wenlm     ,zetail    ,ctil      ,stil      , &
                & zetabl    ,ctbl      ,stbl      ,ctrf      ,ctrl      , &
                & zbmnl     ,wenl      ,cgdghf    ,cgdghl    ,zmeanf    , &
                & umeanf    ,zmeanl    ,umeanl    ,dpu       ,dpv       , &
                & s0        ,u0        ,v0        ,xcor      ,ycor      , &
                & hu        ,hv        ,crbc      ,gvu       ,guv       , &
                & wsu       ,wsv       ,hdt       ,ncmax     ,ampbc     , &
                & ombc      ,phibc     ,thetbc    ,circ2d    ,gdp       )
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
! Determine from the incoming signals the coefficients for the Riemann Boundary 
! conditions in all Riemann-boundary points.
! Use is made of the Riemann-VanDongeren-Svendsen method.
! The coefficients are stored in crbc and are used in CUCBP.
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
    logical                , pointer :: first
!
! Global variables
!
    integer                                :: icx
    integer                                :: icy
    integer                                :: j
    integer                                :: ncmax
    integer                                :: nmax !  Description and declaration in esm_alloc_int.f90
    integer                                :: nmmaxj !  Description and declaration in dimens.igs
    integer                                :: nocol !  Description and declaration in esm_alloc_int.f90
    integer                                :: norow !  Description and declaration in esm_alloc_int.f90
    integer, dimension(5, norow + nocol)   :: irocol !  Description and declaration in esm_alloc_int.f90
    real(fp)                               :: hdt !  Description and declaration in esm_alloc_real.f90
    real(fp)                               :: timsec !  Description and declaration in inttim.igs
    real(fp), dimension(4, norow + nocol)      :: circ2d !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(12, norow + nocol)     :: crbc !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub) :: dpu !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub) :: dpv !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub) :: guv !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub) :: gvu !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub) :: hu !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub) :: hv !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub) :: s0 !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub) :: u0 !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub) :: v0 !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub) :: wsu !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub) :: wsv !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub) :: xcor !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(gdp%d%nmlb:gdp%d%nmub) :: ycor !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(ncmax)                 :: ampbc !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(ncmax)                 :: ombc !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(ncmax)                 :: phibc !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(ncmax)                 :: thetbc !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(norow + nocol)         :: cgdghf !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(norow + nocol)         :: cgdghl !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(norow + nocol)         :: ctbf !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(norow + nocol)         :: ctbl !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(norow + nocol)         :: ctif !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(norow + nocol)         :: ctil !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(norow + nocol)         :: ctrf !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(norow + nocol)         :: ctrl !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(norow + nocol)         :: stbf !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(norow + nocol)         :: stbl !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(norow + nocol)         :: stif !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(norow + nocol)         :: stil !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(norow + nocol)         :: umeanf !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(norow + nocol)         :: umeanl !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(norow + nocol)         :: wenf !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(norow + nocol)         :: wenfm !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(norow + nocol)         :: wenl !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(norow + nocol)         :: wenlm !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(norow + nocol)         :: zbmnf !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(norow + nocol)         :: zbmnl !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(norow + nocol)         :: zetabf !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(norow + nocol)         :: zetabl !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(norow + nocol)         :: zetaif !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(norow + nocol)         :: zetail !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(norow + nocol)         :: zmeanf !  Description and declaration in esm_alloc_real.f90
    real(fp), dimension(norow + nocol)         :: zmeanl !  Description and declaration in esm_alloc_real.f90
!
! Local variables
!
    integer :: nmaxddb
    real(fp):: urf
!
!! executable statements -------------------------------------------------------
!
    ! The first time incwav is called for the rows,
    ! AND the first time incwav is called for the columns,
    ! parameter first must be true (= initial value).
    ! After these two calls, first must be false (and keep that value)
    !
    first    => gdp%gdincwav%first
    !
    urf = 0.01
    nmaxddb = nmax + 2*gdp%d%ddbound
    !
    ! Determine the incoming wave information at left and right boundaries.
    !
    icx = nmaxddb
    icy = 1
    call incwav(timsec    ,j         ,nmmaxj    ,norow     ,icx       , &
              & icy       ,irocol(1, 1)         ,zetaif(1) ,ctif(1)   ,stif(1)   , &
              & zetabf(1) ,ctbf(1)   ,stbf(1)   ,zbmnf(1)  ,wenf(1)   , &
              & wenfm(1)  ,wenlm(1)  ,zetail(1) ,ctil(1)   ,stil(1)   , &
              & zetabl(1) ,ctbl(1)   ,stbl(1)   ,zbmnl(1)  ,wenl(1)   , &
              & cgdghf(1) ,cgdghl(1) ,zmeanf(1) ,umeanf(1) ,zmeanl(1) , &
              & umeanl(1) ,urf       ,dpu       ,s0        ,u0        , &
              & xcor      ,ycor      ,hu        ,ncmax     ,ampbc     , &
              & ombc      ,phibc     ,thetbc    ,first     ,gdp       )
    !
    ! Determine the coefficients to be used for the Riemann boundaries in CUCBP
    ! for left and right boundaries.
    !
    icx = nmaxddb
    icy = 1
    call cofrbc(j         ,nmmaxj    ,norow     ,icx       ,icy       , &
              & urf       ,hu        ,dpu       ,u0        ,v0        , &
              & crbc(1, 1),ctbf(1)   ,ctbl(1)   ,ctif(1)   ,ctil(1)   , &
              & ctrf(1)   ,ctrl(1)   ,stbf(1)   ,stbl(1)   ,stif(1)   , &
              & stil(1)   ,cgdghf(1) ,cgdghl(1) ,zetabf(1) ,zetabl(1) , &
              & zetaif(1) ,zetail(1) ,zmeanf(1) ,zmeanl(1) ,umeanf(1) , &
              & umeanl(1) ,circ2d(1, 1)         ,gvu       ,wsu       ,irocol(1, 1)         , &
              & timsec    ,hdt       ,gdp       )
    !
    ! Determine the incoming wave information at bottom and top boundaries
    ! of the domain.
    !
    icx = 1
    icy = nmaxddb
    call incwav(timsec    ,j         ,nmmaxj    ,nocol     ,icx       , &
              & icy       ,irocol(1, norow + 1) ,zetaif(norow + 1)    ,ctif(norow + 1)      ,stif(norow + 1)      , &
              & zetabf(norow + 1)    ,ctbf(norow + 1)      ,stbf(norow + 1)      ,zbmnf(norow + 1)     ,wenf(norow + 1)      , &
              & wenfm(norow + 1)     ,wenlm(norow + 1)     ,zetail(norow + 1)    ,ctil(norow + 1)      ,stil(norow + 1)      , &
              & zetabl(norow + 1)    ,ctbl(norow + 1)      ,stbl(norow + 1)      ,zbmnl(norow + 1)     ,wenl(norow + 1)      , &
              & cgdghf(norow + 1)    ,cgdghl(norow + 1)    ,zmeanf(norow + 1)    ,umeanf(norow + 1)    ,zmeanl(norow + 1)    , &
              & umeanl(norow + 1)    ,urf       ,dpv       ,s0        ,v0        , &
              & xcor      ,ycor      ,hv        ,ncmax     ,ampbc     , &
              & ombc      ,phibc     ,thetbc    ,first     ,gdp       )
    !
    ! Determine the coefficients to be used for the Riemann boundaries in
    ! CUCBP for bottom and top boundaries.
    !
    icx = 1
    icy = nmaxddb
    call cofrbc(j         ,nmmaxj    ,nocol     ,icx       ,icy       , &
              & urf       ,hv        ,dpv       ,v0        ,u0        , &
              & crbc(1, norow + 1)   ,ctbf(norow + 1)      ,ctbl(norow + 1)      ,ctif(norow + 1)      ,ctil(norow + 1)      , &
              & ctrf(norow + 1)      ,ctrl(norow + 1)      ,stbf(norow + 1)      ,stbl(norow + 1)      ,stif(norow + 1)      , &
              & stil(norow + 1)      ,cgdghf(norow + 1)    ,cgdghl(norow + 1)    ,zetabf(norow + 1)    ,zetabl(norow + 1)    , &
              & zetaif(norow + 1)    ,zetail(norow + 1)    ,zmeanf(norow + 1)    ,zmeanl(norow + 1)    ,umeanf(norow + 1)    , &
              & umeanl(norow + 1)    ,circ2d(1, norow + 1) ,guv       ,wsv       ,irocol(1, norow + 1) , &
              & timsec    ,hdt       ,gdp       )
    !
    first = .false.
end subroutine incrbc
