subroutine zbrent(kmxdt     ,kmxt      ,omeg      ,angle     ,frfind    , &
                & tgfind    ,stage     ,x1        ,x2        ,atg       , &
                & btg       ,bv2       ,ctg       ,crit      ,d2u       , &
                & d2v       ,h0        ,kbed      ,ktop      ,qz        , &
                & r1tg      ,scale     ,top       ,utg       ,vtg       , &
                & xkh       ,luniwe    ,gdp       )
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
!    Function: Finds OMEG or ANGLE in interval [x1,x2] for eigen-
!              mode in Taylor-Goldstein equation.
!              ZBRENT is an adapted version from Numerical
!              Recipes (1990).
!              Parameter setting in parameter list are in accor-
!              dance with Num. Recipes.
!              The adaptation concerns adjustment of OMEG, when
!              FRFIND=.true., or ANGLE with OMEG=0 for lee waves
!
!              ZBRENT calls routine TG which computes the root-
!              finding criterion CRIT.
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
    real(fp) , pointer :: tol
    logical  , pointer :: iwedia
!
! Local parameters
!
    integer, parameter :: itmax = 100
    real(fp), parameter :: eps = 1.E-6, apprx = 1.E-20 !  Description and declaration in numeco.igs
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
    integer, intent(in)            :: luniwe
                                   !!  Unit number for diagnostic reports of
                                   !!  this subprogram as well as called
                                   !!  subroutines
    logical, intent(in)            :: frfind
                                   !!  If .true. then find root of TG
                                   !!  equation as function of angular
                                   !!  frequency, else find root as function
                                   !!  of angle of horizontal wave number
                                   !!  vector
    logical         :: tgfind
                                   !!  Indicator for instructing subroutine
                                   !!  TG to yield the eigen mode for given
                                   !!  input
    logical         :: top
                                   !!  Indicator for instructing subroutine
                                   !!  TG to yield the eigen mode by
                                   !!  sweeping from surface to bed, or vice
                                   !!  versa
    real(fp)        :: angle
                                   !!  Angle [degrees] between horizontal
                                   !!  IW wavenumber vector and x-u-axis
    real(fp)        :: crit
                                   !!  Minimal value found after convergence
                                   !!  of root-finding procedure ZBRENT
    real(fp)        :: h0
                                   !!  Water depth
    real(fp)        :: omeg
                                   !!  Angular frequency of IW with respect
                                   !!  to ground (root of TG equation)
    real(fp)        :: scale
                                   !!  Length scale for scaling
    real(fp), intent(in)               :: x1
                                   !!  FREQLO(N,NR)
                                   !!  Depending on FRFIND either lower lim-
                                   !!  its of frequency or angle intervals
                                   !!  that bracket the roots of TG equation
    real(fp), intent(in)               :: x2
                                   !!  FREQUP(N,NR)
                                   !!  Depending on FRFIND either upper lim-
                                   !!  its of frequency or angle intervals
                                   !!  that bracket the roots of TG equation
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
    character(132), intent(in)     :: stage
                                   !!  Text string to define in which part
                                   !!  of the calibration we are
!
!
! Local variables
!
    integer                        :: iter
    real(fp)                       :: a
    real(fp)                       :: b
    real(fp)                       :: c
    real(fp)                       :: d
    real(fp)                       :: e
    real(fp)                       :: fa
    real(fp)                       :: fb
    real(fp)                       :: fc
    real(fp)                       :: p
    real(fp)                       :: q
    real(fp)                       :: r
    real(fp)                       :: s
    real(fp)                       :: tol1
    real(fp)                       :: xm
    character(132)                 :: text
!
!
!! executable statements -------------------------------------------------------
!
    !
    !
    tol     => gdp%gdiwearr%tol
    iwedia  => gdp%gdiwearr%iwedia
    !
    a = x1
    b = x2
    !
    if (frfind) then
       omeg = a
    else
       omeg = 0.0
       angle = a
    endif
    tgfind = .false.
    !
    call tg(kmxdt     ,kmxt      ,angle     ,atg       ,btg       , &
          & bv2       ,ctg       ,crit      ,d2u       ,d2v       , &
          & h0        ,kbed      ,ktop      ,omeg      ,qz        , &
          & r1tg      ,scale     ,tgfind    ,top       ,utg       , &
          & vtg       ,xkh       ,gdp       )
    !
    fa = crit
    !
    if (frfind) then
       omeg = b
    else
       omeg = 0.0
       angle = b
    endif
    tgfind = .false.
    !
    call tg(kmxdt     ,kmxt      ,angle     ,atg       ,btg       , &
          & bv2       ,ctg       ,crit      ,d2u       ,d2v       , &
          & h0        ,kbed      ,ktop      ,omeg      ,qz        , &
          & r1tg      ,scale     ,tgfind    ,top       ,utg       , &
          & vtg       ,xkh       ,gdp       )
    !
    fb = crit
    !
    if ((fa>0. .and. fb>0.) .or. (fa<0. .and. fb<0.)) then
       if (iwedia) then
          write (luniwe, '(2a)') ' ZBRENT:', stage
          text = 'root must be bracketed for zero'
          write (luniwe, '(7x,a)') text
          tgfind = .false.
       endif
    endif
    c = b
    fc = fb
    do iter = 1, itmax
       if ((fb>0. .and. fc>0.) .or. (fb<0. .and. fc<0.)) then
          c = a
          fc = fa
          d = b - a
          e = d
       endif
       if (abs(fc)<abs(fb)) then
          a = b
          b = c
          c = a
          fa = fb
          fb = fc
          fc = fa
       endif
       tol1 = 2.*eps*abs(b) + 0.5*tol
       xm = 0.5*(c - b)
       if (abs(xm)<tol1 .or. abs(fb)<apprx) then
          if (frfind) then
             omeg = b
          else
             omeg = 0.0
             angle = b
          endif
          tgfind = .true.
          goto 9999
       endif
       if (abs(e)>tol1 .and. abs(fa)>abs(fb)) then
          s = fb/fa
          if (abs(a - c)<apprx) then
             p = 2.*xm*s
             q = 1. - s
          else
             q = fa/fc
             r = fb/fc
             p = s*(2.*xm*q*(q - r) - (b - a)*(r - 1.))
             q = (q - 1.)*(r - 1.)*(s - 1.)
          endif
          if (p>0.) q = -q
          p = abs(p)
          if (2.*p<min(3.*xm*q - abs(tol1*q), abs(e*q))) then
             e = d
             d = p/q
          else
             d = xm
             e = d
          endif
       else
          d = xm
          e = d
       endif
       !
       a = b
       fa = fb
       if (abs(d)>tol1) then
          b = b + d
       else
          b = b + sign(tol1, xm)
       endif
       !
       if (frfind) then
          omeg = b
       else
          omeg = 0.0
          angle = b
       endif
       tgfind = .false.
       !
       call tg(kmxdt     ,kmxt      ,angle     ,atg       ,btg       , &
             & bv2       ,ctg       ,crit      ,d2u       ,d2v       , &
             & h0        ,kbed      ,ktop      ,omeg      ,qz        , &
             & r1tg      ,scale     ,tgfind    ,top       ,utg       , &
             & vtg       ,xkh       ,gdp       )
       !
       fb = crit
    !
    enddo
    !
    if (.not.iwedia) goto 9999
    !
    write (luniwe, '(2a)') ' ZBRENT:', stage
    text = 'maximum number of iterations exceeded'
    write (luniwe, '(7x,a,1x,i5)') text, itmax
    if (frfind) then
       omeg = b
    else
       omeg = 0.0
       angle = b
    endif
    tgfind = .true.
    !
 9999 continue
end subroutine zbrent
