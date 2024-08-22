!----- AGPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2024.                                
!                                                                               
!  This program is free software: you can redistribute it and/or modify         
!  it under the terms of the GNU Affero General Public License as               
!  published by the Free Software Foundation version 3.                         
!                                                                               
!  This program is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU Affero General Public License for more details.                          
!                                                                               
!  You should have received a copy of the GNU Affero General Public License     
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

   function f05 ( x )
!
! F05 is the function for Groundwater Volume
!
  use RRRunoff, only : f05mean, f05sd, dnorm

  implicit none
  double precision f05
  double precision x

  f05 = dnorm (x, f05mean, f05sd) * x

  return
  end function f05



  function f06 ( x )
!
! F06 is the first function for UnsatZone Volume
!
  use Conf_fil, only : Conffil_get_iOut1
  use RRRunoff, only : f06mean, f06sd, dnorm

  implicit none
  double precision f06
  double precision x

  double precision, parameter :: aloc = 0.0D+00
  double precision               abserrloc
  double precision, parameter :: epsabsloc = 1.0D-300
  double precision, parameter :: epsrelloc = 0.0001D+00
  double precision, external ::  f07
  integer ierloc
  integer nevalloc
  double precision resultloc, f06part2
  integer Iout1

  iOut1 = ConfFil_get_iOut1()

  call qags (f07, aloc, x, epsabsloc, epsrelloc, resultloc, abserrloc, nevalloc, ierloc)
  if (ierloc .gt. 0) write(Iout1,'(A,I3,A,2F12.3)') ' Error ', ierloc, ' evaluating qags in function f06, f06mean and x=',f06mean, x
  f06part2 = resultloc
  f06 = dnorm (x, f06mean, f06sd) * f06part2

  return
  end function f06


  function f07 ( u )
!
! F07 is the second function for UnsatZone Volume
!
  use RRRunoff, only : f07alp, f07n

  implicit none
  double precision f07
  double precision u

  f07 = ( (f07alp*u) ** f07n  + 1. )   ** (1./f07n -1.)

  return
  end function f07



  function f08 ( x )
!
! F08 is the function for Surface Volume
!
  use RRRunoff, only : f08mean, f08sd, dnorm

  implicit none
  double precision f08
  double precision x

! write(*,*) ' compute f08 with x f08mean f08sd', x, f08mean, f08sd
  f08 = dnorm (x, f08mean, f08sd) * x * (-1.)
! write(*,*) ' f08 result ',f08

  return
  end function f08


  function f09 ( x )
!
! F09 is the function for River and Overland fluxes
!
  use RRRunoff, only : f09mean, f09sd, dnorm

  implicit none
  double precision f09
  double precision x

  f09 = dnorm (x, f09mean, f09sd) * x

  return
  end function f09


  function f10 ( x )
!
! F10 is the function for Drainage Fluxed
!
  use RRRunoff, only : f10mean, f10sd, f10Ddr, dnorm

  implicit none
  double precision f10
  double precision x

  f10 = dnorm (x, f10mean, f10sd) * (f10Ddr - x)

  return
  end function f10


