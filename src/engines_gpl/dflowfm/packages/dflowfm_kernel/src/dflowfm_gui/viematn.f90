!----- AGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2017-2024.                                
!                                                                               
!  This file is part of Delft3D (D-Flow Flexible Mesh component).               
!                                                                               
!  Delft3D is free software: you can redistribute it and/or modify              
!  it under the terms of the GNU Affero General Public License as               
!  published by the Free Software Foundation version 3.                         
!                                                                               
!  Delft3D  is distributed in the hope that it will be useful,                  
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU Affero General Public License for more details.                          
!                                                                               
!  You should have received a copy of the GNU Affero General Public License     
!  along with Delft3D.  If not, see <http://www.gnu.org/licenses/>.             
!                                                                               
!  contact: delft3d.support@deltares.nl                                         
!  Stichting Deltares                                                           
!  P.O. Box 177                                                                 
!  2600 MH Delft, The Netherlands                                               
!                                                                               
!  All indications and logos of, and references to, "Delft3D",                  
!  "D-Flow Flexible Mesh" and "Deltares" are registered trademarks of Stichting 
!  Deltares, and remain the property of Stichting Deltares. All rights reserved.
!                                                                               
!-------------------------------------------------------------------------------

! 
! 

  SUBROUTINE VIEMATn(the,phi)
  implicit none
  double precision :: cp
  double precision :: ct
  double precision :: deltx
  double precision :: delty
  double precision :: deltz
  double precision :: dscr
  double precision :: r
  double precision :: sp
  double precision :: st
  double precision :: t1
  double precision :: t2
  double precision :: t3
  double precision :: t4
  double precision :: vs
  double precision :: wpqr
  double precision :: x0s
  double precision :: y0s
  double precision :: z
  double precision :: zfac
  double precision :: zupw
  double precision :: the, phi
!
! Maak viewing matrix Vs
! phi (0 -- pi) en the (-pi/2 -- pi/2) : kijkhoekjes
! wpqr                                 : oog-object in wereldcoor
! deltx,delty,deltz                    : kijk door dit punt in were
! zfac (negatief:op z'n kop)           : oprekking verticaal
! Dscr                                 : oog-scherm in wereldcoor
! Vs                                   : Viewing matrix
!
  common /viewmat/ vs(4,4), x0s, y0s
  COMMON /PERSPX/ WPQR,DELTX,DELTY,DELTZ,ZFAC,DSCR,ZUPW
  dimension T1(4,4),T2(4,4),T3(4,4),T4(4,4),R(4,4),Z(4,4)

  T1 = 0
  T2 = 0
  T3 = 0
  T4 = 0
  R = 0
  CT = COS(THE)
  ST = SIN(THE)
  CP = COS(PHI)
  SP = SIN(PHI)
!
  T1(1,1) = 1.
  T1(2,2) = 1.
  T1(3,3) = 1.   ! ZFAC
  T1(4,4) = 1.
  T1(1,4) = -deltx
  T1(2,4) = -delty
  T1(3,4) =  deltz  ! *ZFAC
!
  T2(1,1) = 1.
  T2(2,2) = 1.
  T2(3,3) = 1.
  T2(4,4) = 1.
  T2(1,4) = -wpqr*CT*CP
  T2(3,4) = -wpqr*CT*SP   ! WAS 2
  T2(2,4) = -wpqr*ST      ! WAS 3
!
  T3(1,1) =  CP
  T3(1,3) =  SP
  T3(3,1) = -SP
  T3(3,3) =  CP
  T3(2,2) =  1.
  T3(4,4) =  1.
!
  T4(1,1) =  CT
  T4(1,2) =  ST
  T4(2,1) = -ST
  T4(2,2) =  CT
  T4(3,3) =  1.
  T4(4,4) =  1.
!
  R(1,2)  =  DSCR
  R(2,3)  =  DSCR
  R(3,1)  = -1.
  R(4,4)  =  1.


! nadat alles geinitialiseerd is de viewing transformatie-matr Vs =

  call matm4(R,T4,Z)

  call matm4(Z,T3,Vs)
  call matm4(Vs,T2,Z)
  call matm4(Z,T1,Vs)
  end SUBROUTINE VIEMATn
