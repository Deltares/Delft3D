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

 subroutine setveg()
 use m_flow
 use m_flowgeom
 use m_sferic
 use m_flowtimes
 implicit none

 double precision :: h1,h0, stemcos, stemsin, stemh, PL, Pv, Pm, Pmi, Bp,dp
 double precision :: rhodif, buoym, bendm, Fbe, Fbu, Ti, Tdti, phi, phit, C, Rm, ep, qsa, qds, ds2
 integer          :: kk, k, num, idum = 1, i, L, k1, k2

 if (kmx == 0 .and. growthunidicouv > 0.0) then

     !stemheight = 0.55d0
     !diaveg     = 0.05d0
     if (.not. allocated(supq) ) allocate( supq(ndx) )
     supq  = 0d0
     do L  = 1,lnxi
        k1 = ln(1,L) ; k2 = ln(2,L)
        qds = growthunidicouv*dxi(L)*wu(L)
        ds2 = rnveg(k2) - rnveg(k1)
        qsa = qds*ds2
        supq(k2) =  supq(k2) - qsa
        supq(k1) =  supq(k1) + qsa
     enddo
     do k  = 1, ndxi
        rnveg(k) = max(0d0, rnveg(k) + dts*supq(k)*bai(k))
     enddo
     return
 endif

 num = 1
 if (rhoveg > 0d0) then
    num    = 10
    rhodif = rhomean - rhoveg
 endif

 do kk = 1,ndxi

    if ( diaveg(kk) > 0d0) then

       phi     = phiv(kk)  ! 0d0                                                                ! 1/s   stemphi
       phit    = phivt(kk) ! 0d0                                                                ! 1/s2  stemomega
       Pl      = stemheight(kk)                                                                 ! m       plantlength
       Pl      = min(hs(kk), Pl)

       do i = 1, num

       stemcos = cos(phi) ; stemsin  = sin(phi)

       if (rhoveg > 0d0) then

          Pv      = Pl*diaveg(kk)*diaveg(kk)*0.25d0*pi                                          ! m3      plantvolume
          Pm      = rhoveg*Pv                                                                   ! kg      plantmass
          Pmi     = (1d0/6d0)*Pm*Pl*Pl                                                          ! kg.m2   plant inertia moment
          Tdti    = 2d0*Pmi/dts                                                                 ! kg.m2/s

          Bendm = 0d0
          do k  = kbot(kk), ktop(kk)
             Fbe   = -0.5d0*rhomean*Cdveg*diaveg(k)*(zws(k)-zws(k-1))*ucx(k)*sqrt( ucx(k)*ucx(k) + ucy(k)*ucy(k) )  ! kg.m/s2
             Bendm = Bendm + Fbe*0.5d0*(zws(k)+zws(k-1))*stemcos                                ! kg.m2/s2
          enddo

          Fbu      = ag*rhodif*diaveg(kk)*diaveg(kk)*0.25d0*pi*Pl                               ! kg.m/s2
          Buoym    = Fbu*0.5d0*Pl*stemsin                                                       ! kg.m2/s2

          Bp       = 0.5d0*rhomean*Cdveg*diaveg(kk)*( (0.5*Pl)**4 )
          Bp       = Bp*max(0.1d0, phivt(kk))                                                   ! kg.m2/s

          phit     = ( phit*Tdti + (Bendm - Buoym) ) / ( Tdti + Bp )
          ep       = 0.1
          if (phit >  ep) then
             phit  =  ep
          else if (phit < -ep) then
             phit  = -ep
          endif
          phivt(kk) = phit
          phi       = (phi/dts + phit) / (1d0/dts + cbveg/Tdti)
          phi       = max(-1.5d0, min(phi, 1.5d0) )
          phiv(kk)  = phi

          stemcos   = cos(phi)

       endif
       stemh = Pl*stemcos

       if (kmx > 0) then
          do k  = kbot(kk), ktop(kk)
             h1 = zws(k)   - zws(kbot(kk) -1)
             h0 = zws(k-1) - zws(kbot(kk) -1)
             if ( h1 < stemh ) then
                 diaveg(k) = diaveg(kk)
                 rnveg (k) = rnveg (kk)
             else if (h0 < stemh .and. h1 > stemh ) then
                 diaveg(k) = diaveg(kk)
                 rnveg (k) = rnveg (kk)*( stemh - h0 ) / ( h1 - h0 )
             else
                 diaveg(k) = 0d0
                 rnveg (k) = 0d0
             endif
          enddo
       endif

       enddo

    endif

 enddo

 end subroutine setveg
