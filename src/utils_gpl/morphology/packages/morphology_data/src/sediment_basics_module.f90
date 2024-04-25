module sediment_basics_module
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
!!--module description----------------------------------------------------------
!
! This module defines some very basic sediment features.
!
!!--module declarations---------------------------------------------------------
use precision

private

!
! public parameters
!
! public SEDTYP_NONCOHESIVE_TOTALLOAD -> SEDTYP_SAND, TRA_BEDLOAD
! public SEDTYP_NONCOHESIVE_SUSPENDED -> SEDTYP_SAND, TRA_COMBINE
! public SEDTYP_COHESIVE -> SEDTYP_CLAY, TRA_ADVDIFF

public SEDTYP_GRAVEL
public SEDTYP_SAND
public SEDTYP_SILT
public SEDTYP_CLAY

public TRA_NONE     ! no transpor method given
public TRA_BEDLOAD  ! transport given by an algebraic expression (typically bed load or total load)
public TRA_ADVDIFF  ! transport determined via advection diffusion equation
public TRA_COMBINE  ! transport determined via algebraic expression plus advection diffusion equation

public lognormal
public ilognormal

public dclay
public dsilt
public dsand
public dgravel

public has_advdiff
public has_bedload
public is_bedload

integer, parameter :: SEDTYP_CLAY   = 1
integer, parameter :: SEDTYP_SILT   = 2
integer, parameter :: SEDTYP_SAND   = 3
integer, parameter :: SEDTYP_GRAVEL = 4

integer, parameter :: TRA_NONE    = 0
integer, parameter :: TRA_BEDLOAD = 1
integer, parameter :: TRA_ADVDIFF = 2
integer, parameter :: TRA_COMBINE = TRA_BEDLOAD + TRA_ADVDIFF

integer, parameter :: TRA_BEDLOAD_BIT = 0 ! the first bit is used for flagging bed load
integer, parameter :: TRA_ADVDIFf_BIT = 1 ! the second bit is used for flagging suspended load

!
! sqrt(2)*erfinv(P/50-1) for P = 1:99
!
real(fp), dimension(99)  :: lognormal
data lognormal &
  & /-2.3263,-2.0537,-1.8808,-1.7507,-1.6449,-1.5548,-1.4758,-1.4051, &
  &  -1.3408,-1.2816,-1.2265,-1.1750,-1.1264,-1.0803,-1.0364,-0.9945, &
  &  -0.9542,-0.9154,-0.8779,-0.8416,-0.8064,-0.7722,-0.7388,-0.7063, &
  &  -0.6745,-0.6433,-0.6128,-0.5828,-0.5534,-0.5244,-0.4959,-0.4677, &
  &  -0.4399,-0.4125,-0.3853,-0.3585,-0.3319,-0.3055,-0.2793,-0.2533, &
  &  -0.2275,-0.2019,-0.1764,-0.1510,-0.1257,-0.1004,-0.0753,-0.0502, &
  &  -0.0251, 0.0000, 0.0251, 0.0502, 0.0753, 0.1004, 0.1257, 0.1510, &
  &   0.1764, 0.2019, 0.2275, 0.2533, 0.2793, 0.3055, 0.3319, 0.3585, &
  &   0.3853, 0.4125, 0.4399, 0.4677, 0.4959, 0.5244, 0.5534, 0.5828, &
  &   0.6128, 0.6433, 0.6745, 0.7063, 0.7388, 0.7722, 0.8064, 0.8416, &
  &   0.8779, 0.9154, 0.9542, 0.9945, 1.0364, 1.0803, 1.1264, 1.1750, &
  &   1.2265, 1.2816, 1.3408, 1.4051, 1.4758, 1.5548, 1.6449, 1.7507, &
  &   1.8808, 2.0537, 2.3263/
integer, dimension(14)   :: ilognormal
data ilognormal /1, 3, 6, 10, 16, 24, 36, 64, 76, 84, 90, 94, 97, 99/

real(fp), parameter :: dclay   =  8.0e-6_fp   !  grain size threshold clay   (vRijn:    8um)
real(fp), parameter :: dsilt   = 32.0e-6_fp   !  grain size threshold silt   (vRijn:   32um)  
real(fp), parameter :: dsand   = 64.0e-6_fp   !  grain size threshold sand   (vRijn:   62um)  
real(fp), parameter :: dgravel =  2.0e-3_fp   !  grain size threshold gravel (vRijn: 2000um)

    contains

!> Check if fraction should include advection-diffusion component.    
function has_advdiff(tratyp) result (bool)
    implicit none
    !
    integer, intent(in) :: tratyp  !< transport type to be checked
    logical             :: bool    !< logical result of tratyp check
    !
    bool = btest(tratyp, TRA_ADVDIFF_BIT)
end function has_advdiff

!> Check if fraction should include bedload component.    
function has_bedload(tratyp) result (bool)
    implicit none
    !
    integer, intent(in) :: tratyp  !< transport type to be checked
    logical             :: bool    !< logical result of tratyp check
    !
    bool = btest(tratyp, TRA_BEDLOAD_BIT)
end function has_bedload

!> Check if fraction is only governed by bedload component.    
function is_bedload(tratyp) result (bool)
    implicit none
    !
    integer, intent(in) :: tratyp  !< transport type to be checked
    logical             :: bool    !< logical result of tratyp check
    !
    bool = tratyp == TRA_BEDLOAD
end function is_bedload

end module sediment_basics_module
