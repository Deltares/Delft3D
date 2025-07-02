subroutine tran9t(utot      ,d50       ,d90       ,chezy     ,h         , &
                & ust       ,npar      ,par       ,sbot      ,ssus      )
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
! -
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use sed_support_routines, only: shld
    implicit none
!
! Arguments
!
    integer                  , intent(in)    :: npar
    real(fp)                 , intent(in)    :: chezy
    real(fp)                 , intent(in)    :: d50
    real(fp)                 , intent(in)    :: d90
    real(fp)                 , intent(in)    :: h
    real(fp), dimension(npar), intent(in)    :: par
    real(fp)                 , intent(in)    :: ust
    real(fp)                 , intent(in)    :: utot
    !
    real(fp)                 , intent(out)   :: sbot 
    real(fp)                 , intent(out)   :: ssus
!
!
! Local variables
!
    real(fp)                            :: acal
    real(fp)                            :: ag           ! gravity acceleration
    real(fp)                            :: cgrain
    real(fp)                            :: delta        ! relative density of sediment particle
    real(fp)                            :: dfr
    real(fp)                            :: dstar        ! critical dimensionless grain size
    real(fp)                            :: shdd
    real(fp)                            :: shield       ! parameter  shield coefficient
    real(fp)                            :: shldcr
    real(fp)                            :: sk
    real(fp)                            :: t            ! time in seconds
    real(fp)                            :: ust2         ! shear stress velocity ^ 2
    real(fp)                            :: wamu         ! viscosity of water
!
!
!! executable statements -------------------------------------------------------
!
    sbot = 0.
    ssus = 0.
    !
    ag = par(1)
    delta = par(4)
    wamu = par(11)
    acal = par(12)
    !
    t = 0.05*utot**2*ust**3/d50/(ag*delta)**2
    !
    !     bed load transport according to e-f 1976
    !
    !     dry point
    !
    if (chezy<=0.) then
       sbot = 0
       ssus = 0
       goto 333
    endif
    !
    !     compute grain friction cgrain = cg/sqrt(g)
    !     for this formula d90 = d50 of the total bed sediment.
    !
    dfr = h
    sk = 2.5*d90
  222 continue
    cgrain = 6. + 2.5*log(dfr/sk)
    dfr = h*chezy*chezy/ag/cgrain
    if (dfr>h) then
       dfr = h
    elseif ((dfr - h)/h>0.0001) then
       goto 222
    else
    endif
    !
    !     bed load
    !
    ust2 = ag*(utot/cgrain)**2
    shield = ust2/delta/ag/d50
    dstar = d50*(delta*ag/wamu**2)**0.33333333
    shldcr = shld(dstar)
    shdd = shield - shldcr
    if (shdd<=0) then
       sbot = 0.
    else
       sbot = acal*4.8695*d50*ust/(1. + (0.2668/shdd)**4)                       &
            & **0.25*(1. - 0.7*sqrt(shldcr/shield))
    endif
    !
    !     equilibrium concentration
    !
    ssus = (t - sbot)
  333 continue
end subroutine tran9t
