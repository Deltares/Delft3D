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

 subroutine setgrainsizes() ! for all fractions:
 USE M_SEDIMENT
 use m_physcoef,       only : ag, rhomean, vonkar, backgroundwatertemperature, vismol
 use MessageHandling
 implicit none
 integer          :: m, j
 double precision :: Ucr, sster, c1, c2, wster, wschk, taucr, taucr1, thetcr, pclay=0d0, fcr=1d0

 double precision :: a = 2.414d-5, b = 247.8d0, c= 140d0, TempK, s

! where was this moved to or why was it removed?
! TempK         = 273d0 + backgroundwatertemperature
! vismol        = A*10**( B / (TempK-C) ) / rhomean
! vismol        = 4.d0/(20.d0 + backgroundwatertemperature)*1d-5 ! Van rijn, 1993

 if (allocated (D90) ) then
     deallocate(D90, rhodelta, sqsgd50, dstar, dstar03, Accr, Awcr)
 endif
 if (mxgr == 0) return
 m = mxgr
 allocate (D90(m), rhodelta(m), sqsgd50(m), dstar(m), dstar03(m), Accr(m), Awcr(m))

 D90           = 2d0*D50
 rhodelta      = (rhosed-rhomean) / rhomean  ! rhodelta = (s-1), s=rhosed/rhomean
 sqsgd50       = sqrt( rhodelta*ag*D50)
 dstar         = D50*( rhodelta*ag/(vismol*vismol) )** (1d0/3d0)
 dstar03       = dstar**(-0.3d0)

 do j = 1,mxgr

    call fdster(dstar(j),taucr,thetcr,pclay,ag,d50(j),rhosed(j),rhomean,FCR) ! vanRijn Tr2004
    ACCR(J) = sqsgd50(j)*sqrt(thetcr)

    Awcr(j) = D50wa(j)*(rhodelta(j)*ag)**D50wb(j)*D50(j)**D50wc(j)

    Sster = D50(J)/(4*vismol)*sqsgd50(J)
    c1    = 1.06d0*tanh(0.064d0*Sster*exp(-7.5d0/Sster**2))
    c2    = 0.22d0*tanh(2.34d0*Sster**(-1.18d0)*exp(-0.0064d0*Sster**2))
    wster = c1+c2*Sster
    Ws(j) = wster*sqsgd50(j)                                         ! van Rijn
    Wschk = 16.17d0*D50(j)*D50(j)/(1.80d-5 + sqrt(12.12*D50(j)**3) ) ! Ferguson,Church 2006) Wikipedia sand fall velocity

    call mess(LEVEL_INFO,' Backgroundwatertemperature (degC) ', real(Backgroundwatertemperature)  )
    call mess(LEVEL_INFO,' Vismol                     (m2/s) ', real(Vismol)  )
    call mess(LEVEL_INFO,' Fraction diameter D50         (m) ', real(D50(j))    )
    call mess(LEVEL_INFO,' Fraction diameter Dstar       ( ) ', real(Dstar(j))  )
    call mess(LEVEL_INFO,' Settling velocity Ws vR     (m/s) ', real(Ws(j))     )
    call mess(LEVEL_INFO,' Settling velocity Ws F,C    (m/s) ', real(Wschk)     )
    call mess(LEVEL_INFO,' Setting time h=5(m)        (days) ', real(5d0/(ws(j)*24*3600) ) )
    call mess(LEVEL_INFO,' Rhosed                    (kg/m3) ', real(rhosed(j)) )

    Ucr = Accr(j)*log(4.d0*1d0/D90(j))
    call mess(LEVEL_INFO,' Ucrc h=1 (m)             (m/s) ', real(UCr)       )
    Ucr = Accr(j)*log(4.d0*5d0/D90(j))
    call mess(LEVEL_INFO,' Ucrc h=5 (m)             (m/s) ', real(UCr)       )
    Ucr = Accr(j)*log(4.d0*20d0/D90(j))
    call mess(LEVEL_INFO,' Ucrc h=20 (m)            (m/s) ', real(UCr)       )
 enddo

 end subroutine setgrainsizes
