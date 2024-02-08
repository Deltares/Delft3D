subroutine disper(kmxdt     ,kmxt      ,np        ,nfreqs    ,angle     , &
                & bvmx      ,freqlo    ,frequp    ,kcri      ,ncri      , &
                & sing      ,umean     ,vmean     ,atg       ,btg       , &
                & bv2       ,ctg       ,d2u       ,d2v       ,h0        , &
                & kbed      ,ktop      ,qz        ,r1tg      ,scale     , &
                & utg       ,vtg       ,xkh       ,gdp       )
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
!      Function: Finding extreme levels of critical layers for
!                all allowed
!                IW's propagating in positive (nr=2) or negative
!                (nr=1) direction with respect to given hori-
!                zontal wavenumber magnitude and ANGLE.
!
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
    real(fp) , pointer :: bvmin
!
! Global variables
!
    integer         :: kbed
                                   !!  K-value on TG grid: upper level
                                   !!  stratified layer
    integer         :: kmxdt !  Description and declaration in dimens.igs
    integer         :: kmxt !  Description and declaration in dimens.igs
    integer         :: ktop
                                   !!  K-value on TG grid: lower level
                                   !!  stratified layer
    integer, intent(in)            :: nfreqs !  Description and declaration in dimens.igs
    integer, intent(in)            :: np
                                   !!  Dimension for IWE frequency arrays
                                   !!  MAX (1,NFREQS)
    integer, dimension(2, 2), intent(out) :: kcri
                                   !!  K-value on TG grid of most upper and
                                   !!  lower critical levels of turbulence-
                                   !!  generated IW's
    integer, dimension(2, 2), intent(out) :: ncri
                                   !!   Number of critical layers found per
                                   !!  minimal or maximal K-value as well as
                                   !!  sign of frequency of turbulence-
                                   !!  generated IW's
    logical, dimension(2, 2), intent(out) :: sing
                                   !!  Indicator of singular solutions of TG
                                   !!  equation
    real(fp)        :: angle
                                   !!  Angle [degrees] between horizontal
                                   !!  IW wavenumber vector and x-u-axis
    real(fp), intent(in)               :: bvmx
                                   !!  Maximal buoyancy frequency
    real(fp)        :: h0
                                   !!  Water depth
    real(fp)        :: scale
                                   !!  Length scale for scaling
    real(fp), intent(in)               :: umean !  Description and declaration in esm_alloc_real.f90
    real(fp), intent(in)               :: vmean !  Description and declaration in esm_alloc_real.f90
    real(fp)        :: xkh
                                   !!  Non-dimensional XK
    real(fp), dimension(0:kmxdt) :: atg
    real(fp), dimension(0:kmxdt) :: btg
    real(fp), dimension(0:kmxdt) :: bv2
    real(fp), dimension(0:kmxdt) :: ctg
    real(fp), dimension(0:kmxdt) :: d2u
    real(fp), dimension(0:kmxdt) :: d2v
    real(fp), dimension(0:kmxdt) :: qz
    real(fp), dimension(0:kmxdt) :: r1tg
    real(fp), dimension(0:kmxdt) :: utg
    real(fp), dimension(0:kmxdt) :: vtg
    real(fp), dimension(np, 2), intent(out) :: freqlo
    real(fp), dimension(np, 2), intent(out) :: frequp
!
!
! Local variables
!
    integer                        :: i
    integer                        :: j
    integer                        :: kcrit
    integer                        :: ndir
    integer                        :: nf
    integer                        :: nr
    logical                        :: singul
    logical                        :: tgfind
    logical                        :: top
    real(fp)                       :: crit
    real(fp)                       :: delf
    real(fp)                       :: flo
    real(fp)                       :: frlo
    real(fp)                       :: frup
    real(fp)                       :: fup
    real(fp)                       :: omeg
    real(fp)                       :: omeg1
    real(fp)                       :: umag
!
!
!! executable statements -------------------------------------------------------
!
    !
    !
    bvmin  => gdp%gdiwearr%bvmin
    !
    umag = sqrt(umean**2 + vmean**2)
    if (umag<1.E-12) then
       angle = 0.
    else
       angle = 45.*acos(umean/umag)/atan(1.)
    endif
    if (vmean<0.0) angle = 360. - angle
    !
    do i = 1, 2
       do j = 1, 2
          kcri(i, j) = 0
          ncri(i, j) = 0
          sing(i, j) = .false.
       enddo
    enddo
    !
    ! Inspect two ranges of normalized eigen frequencies:
    ! nr=1: [-bvmx,-delf]
    ! nr=2: [ delf, bvmx]
    !
    delf = bvmx/nfreqs
    !
    do nr = 1, 2
       i = -1
       if (nr==2) i = 1
       do ndir = 1, 2
          if (ndir==1) then
             j = 1
             omeg = i*bvmin
          else
             j = -1
             omeg = i*bvmx
          endif
          !
          nf = 0
          tgfind = .false.
          top = .true.
          !
          call tg(kmxdt     ,kmxt      ,angle     ,atg       ,btg       , &
                & bv2       ,ctg       ,crit      ,d2u       ,d2v       , &
                & h0        ,kbed      ,ktop      ,omeg      ,qz        , &
                & r1tg      ,scale     ,tgfind    ,top       ,utg       , &
                & vtg       ,xkh       ,gdp       )
          !
          !
          ! For FLO and FLUP only the sign is important (zero criteria)
          !
          flo = sign(1._fp, crit)
          frlo = omeg
          ! -->
   50     continue
          nf = nf + 1
          omeg = omeg + i*j*delf
          tgfind = .false.
          top = .true.
          call tg(kmxdt     ,kmxt      ,angle     ,atg       ,btg       , &
                & bv2       ,ctg       ,crit      ,d2u       ,d2v       , &
                & h0        ,kbed      ,ktop      ,omeg      ,qz        , &
                & r1tg      ,scale     ,tgfind    ,top       ,utg       , &
                & vtg       ,xkh       ,gdp       )
          !
          !
          fup = sign(1._fp, crit)
          frup = omeg
          if (nf<=nfreqs) then
             if (fup*flo>0.) then
                flo = fup
                frlo = frup
                goto 50
             ! <--
             endif
             omeg1 = omeg
             omeg = 0.5*(frlo + frup)
             call seliwe(angle     ,kbed      ,ktop      ,kmxt      ,omeg      , &
                       & r1tg      ,utg       ,vtg       ,xkh       ,singul    , &
                       & top       ,kcrit     ,gdp       )
             !
             omeg = omeg1
             if (singul) then
                frequp(nf, nr) = frup
                freqlo(nf, nr) = frlo
                kcri(nr, ndir) = kcrit
                ncri(nr, ndir) = nf
                sing(nr, ndir) = singul
             else
                goto 50
             ! <--
             endif
          endif
       enddo
    enddo
end subroutine disper
