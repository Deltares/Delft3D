subroutine soursin_2d(umod      ,ustarc    ,h0        ,h1        , &
                    & ws        ,tsd       ,rsedeq    ,factsd    , &
                    & sour_ex   ,sour_im   ,sink      )
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
!    Function: Computes the sour and sink terms for the 2D case
!              (Galappatti aproach)
!
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use mathconsts
    !
    implicit none
    !

!
! Arguments
!
    real(fp), intent(in)  :: umod
    real(fp), intent(in)  :: ustarc
    real(fp), intent(in)  :: h0
    real(fp), intent(in)  :: h1
    real(fp), intent(in)  :: ws
    real(fp), intent(in)  :: factsd
    real(fp)              :: tsd
    real(fp), intent(in)  :: rsedeq
    real(fp), intent(out) :: sour_ex
    real(fp), intent(out) :: sour_im
    real(fp), intent(out) :: sink
!
! Local variables
!
    real(fp) :: b
    real(fp) :: eps
    real(fp) :: hots
    real(fp) :: u
    real(fp) :: ulog
    real(fp) :: w
    real(fp) :: wsl
    real(fp) :: x
    real(fp) :: x2
    real(fp) :: x3
!
!! executable statements -------------------------------------------------------
!
    !eps       => gdp%gdconst%eps
    eps        = 1e-6_fp
    !
    wsl = max(1.0e-3_fp,ws)
    if (umod > eps .and. ustarc > eps) then
       if (tsd <= 0.0) then
          !
          ! tsd not given by user transport formula
          ! compute it using the Galappatti formulations
          !
          u = ustarc/umod
          !
          ! limit u to prevent overflow in tsd below
          !
          u = min(u, 0.15_fp)
          if (ustarc > wsl) then
             w = wsl/ustarc
          else
             w = 1.0
          endif
          b    = 1.0
          x    = w/b
          x2   = x*x
          x3   = x2*x
          ulog = log(u)
          tsd  = x*exp((  1.547           - 20.12*u )*x3 &
               &     + (326.832 *u**2.2047 -  0.2   )*x2 &
               &     + (  0.1385*ulog      -  6.4061)*x  &
               &     + (  0.5467*u         +  2.1963) )
       else
          !
          ! tsd given by user transport formula
          !
       endif
       hots = wsl/(tsd*factsd)
       sour_ex = rsedeq*hots/h0
       sour_im = (hots-wsl)/h1
       sink    = wsl/h1
    else
       sour_ex = 0.0_fp
       sour_im = 0.0_fp
       sink = wsl/h1
    endif
end subroutine soursin_2d

