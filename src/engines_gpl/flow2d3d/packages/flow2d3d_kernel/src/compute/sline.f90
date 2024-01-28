subroutine sline(dtn       ,md        ,ndim      ,te        ,xd        , &
               & ud        ,inc       )
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
!    Function: - calculates streamline in this grid cell
!              - if drogue leaves grid cell then calculate time left
!                and increment values for next grid cell
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    implicit none
!
! Global variables
!
    integer, intent(in)            :: md
                                   !!  Number of calculated drogue track
                                   !!  streamline positions
    integer, intent(in)            :: ndim
                                   !!  Dimension parameter
    integer, dimension(ndim), intent(out) :: inc
                                   !!  Increment
    real(fp), intent(in)               :: dtn
                                   !!  Maximum time of drogues in 1 cell
    real(fp)        :: te
                                   !!  Time to pass cell
    real(fp), dimension(ndim, 2), intent(in) :: ud
                                   !!  Velocities of cell boundaries
    real(fp), dimension(ndim, md) :: xd
                                   !!  X- & Y-coordinates for one track
                                   !!  MD per drogue
!
!
! Local variables
!
    integer                        :: i                    ! Hulp Var. 
    integer                        :: j                    ! Hulp Var. 
    integer                        :: ldim                 ! .... 
    real(fp)                       :: d0                   ! .... 
    real(fp)                       :: d1                   ! .... 
    real(fp)                       :: eps                  ! Convergence criterium 
    real(fp)                       :: t                    ! .... 
    real(fp)                       :: t0                   ! .... 
    real(fp)                       :: t1                   ! .... 
    real(fp)                       :: ti                   ! .... 
    real(fp)                       :: tinc                 ! Time increment of drogue in one cell 
    real(fp), dimension(3)             :: a                    ! .... 
    real(fp), dimension(3)             :: b                    ! .... 
    real(fp), dimension(3)             :: c                    ! .... 
    real(fp), dimension(3)             :: u0                   ! .... 
!
!
!! executable statements -------------------------------------------------------
!
    !
    !
    !
    !-----calculation of time te.
    !     if streamline stays inside volume then te = dtn  .
    !     if streamline leaves volume then this happens at time te.
    !     in that case : te < dtn  .
    !
    eps = 1.E-12
    te = dtn
    ldim = 0
    !
    do i = 1, ndim
       u0(i) = (1. - xd(i, 1))*ud(i, 1) + xd(i, 1)*ud(i, 2)
       if (abs(u0(i))<eps) then
          ti = dtn
       elseif (abs(ud(i, 1) - ud(i, 2))<eps) then
          !
          !-----------per definition ud(i,1) < 0. or ud(i,1) > 0. else u0(i) = 0.
          !
          if (ud(i, 1)>0.0) then
             ti = (1. - xd(i, 1))/ud(i, 1)
          else
             ti = -xd(i, 1)/ud(i, 1)
          endif
       else
          b(i) = ud(i, 2) - ud(i, 1)
          c(i) = ud(i, 1)/b(i)
          a(i) = xd(i, 1) + c(i)
          !
          !-----------a(i) = 0. if a(i) = xd(i,1) + ud(i,1) / (ud(i,2) - ud(i,1))
          !                             = u0(i) = 0. => per definition impossible
          ! -----------------------------------------------------------------------
          d1 = (1. + c(i))/a(i)
          d0 = c(i)/a(i)
          !
          if (d1>0.0) then
             t1 = log(d1)/b(i)
          else
             t1 = -dtn*sign(1.0_fp, b(i))
          endif
          !
          if (d0>0.0) then
             t0 = log(d0)/b(i)
          else
             t0 = -dtn*sign(1.0_fp, b(i))
          endif
          !
          ti = max(t0, t1)
       endif
       !
       if (ti<te) then
          te = ti
          ldim = i
       endif
    enddo
    !
    !---calculate streamline in md points.
    !
    tinc = te/real(md - 1,fp)
    !
    do i = 1, ndim
       do j = 2, md
          t = real(j - 1,fp)*tinc
          if (abs(u0(i))<eps) then
             xd(i, j) = xd(i, 1)
          elseif (abs(ud(i, 1) - ud(i, 2))<eps) then
             xd(i, j) = xd(i, 1) + ud(i, 1)*t
          else
             xd(i, j) = a(i)*exp(b(i)*t) - c(i)
          endif
       enddo
    enddo
    !
    !---calculate increment to next grid cell
    !
    do i = 1, ndim
       inc(i) = 0
    enddo
    !
    if (ldim>0) then
       if (xd(ldim, md)<0.1) then
          inc(ldim) = -1
       else
          inc(ldim) = 1
       endif
    endif
end subroutine sline
