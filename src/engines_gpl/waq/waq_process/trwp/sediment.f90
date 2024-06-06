!!  Copyright (C)  Stichting Deltares, 2012-2024.
!!
!!  This program is free software: you can redistribute it and/or modify
!!  it under the terms of the GNU General Public License version 3,
!!  as published by the Free Software Foundation.
!!
!!  This program is distributed in the hope that it will be useful,
!!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
!!  GNU General Public License for more details.
!!
!!  You should have received a copy of the GNU General Public License
!!  along with this program. If not, see <http://www.gnu.org/licenses/>.
!!
!!  contact: delft3d.support@deltares.nl
!!  Stichting Deltares
!!  P.O. Box 177
!!  2600 MH Delft, The Netherlands
!!
!!  All indications and logos of, and references to registered trademarks
!!  of Stichting Deltares remain the property of Stichting Deltares. All
!!  rights reserved.
      module m_waqsediment
      use m_waq_precision


      implicit none

      contains


      subroutine SEDAGG    ( pmsa  , fl    , ipoint, increm, noseg , & 
                            noflux, iexpnt, iknmrk, noq1  , noq2  , & 
                            noq3  , noq4  )
      use m_evaluate_waq_attribute


!>\file
!>       Sedimentation of the aggregated tyre/road wear particles and suspended solids

!
!     Description of the module :
!
!     Assumptions:
!     - All particles we distinguish in this project have a constant
!       sedimentation velocity
!     - Since we are dealing with a one-dimensional model set-up, the
!       sedimentation results in a transformation of the water-based
!       substance into the bottom-based substance. No vertical
!       velocity required
!
! Name            T   Index   Description                                   Units
! ----            --- -       -------------------                            ----
! CWATER          R*4 1   concentration of particles in water                [g/m3]
! SETTLING        R*4 2   settling velocity particles                         [m/d]
! SHEAR_STRESS    R*4 3   bottom shear stress                                  [Pa]
! CRITICAL_STRESS R*4 4   critical shear stress for sedimentation              [Pa]
! DEPTH           R*4 5   depth of the segment                                  [m]
! DELT            R*4 6   time step                                             [d]
! SAFE_FACTOR     R*4 7   safety factor for delimiter                           [-]
!
! FL (1)          R*4 1   sedimentation flux                               [g/m3/d]
!
      implicit none

      real(kind=real_wp) ::pmsa  ( * ) , fl    (*)
      integer(kind=int_wp) ::ipoint( * ) , increm(*) , noseg , noflux, & 
              iexpnt(4,*) , iknmrk(*) , noq1, noq2, noq3, noq4
!
!     local declarations
!

      integer(kind=int_wp) ::iflux, iseg, ikmrk1, ikmrk2, itel, iq, ifrom, ito
      real(kind=real_wp) ::cwater, settling, shear_stress, critical_stress, & 
              delt, depth, safe_factor, depfro, depto
      real(kind=real_wp) ::prob_settling, settling_flux
      
      integer(kind=int_wp) ::ipnt(500)
      integer(kind=int_wp),parameter  ::ip_nTRWP = 1
      integer(kind=int_wp),parameter  ::ip_nIM = 2
      integer(kind=int_wp),parameter  ::ip_Tau = 3
      integer(kind=int_wp),parameter  ::ip_Depth = 4
      integer(kind=int_wp),parameter  ::ip_Delt = 5
      integer(kind=int_wp),parameter  ::ip_SafeFactor = 6
      integer(kind=int_wp),parameter  ::ip_lastsingle = 6
      integer(kind=int_wp) ::ntrwp, itrwp, nspm, ispm, nitem, offset
      

      ntrwp = pmsa(ipoint(ip_ntrwp))
      nspm = pmsa(ipoint(ip_nim  ))
      nitem = ip_lastsingle + 5*ntrwp*nspm
      delt        = pmsa(ipoint(ip_Delt))
      safe_factor = pmsa(ipoint(ip_SafeFactor))
      
      ipnt(1:nitem) = ipoint(1:nitem)

      iflux = 1
      do iseg = 1 , noseg
          call extract_waq_attribute(1,iknmrk(iseg),ikmrk1)
          if (ikmrk1==1) then
          call extract_waq_attribute(2,iknmrk(iseg),ikmrk2)
          if (ikmrk2==0.or.ikmrk2==3) then   ! surface water

              ! input independentt of fractions
              depth          = pmsa(ipnt(ip_Depth))
              shear_stress    = pmsa(ipnt(ip_Tau) )
              
              ! loop over active fractions, IM are inner loop
              itel = 0
              do itrwp = 1,ntrwp
              do ispm = 1,nspm

              itel = itel + 1
              cwater          = pmsa(ipnt(ip_lastsingle             +itel) )
              settling        = pmsa(ipnt(ip_lastsingle+  ntrwp*nspm+itel) )
              critical_stress = pmsa(ipnt(ip_lastsingle+2*ntrwp*nspm+itel) )
              
              !
              ! Probability of settling (limit to avoid instabilities)
              !
              prob_settling = & 
                 max( 0.0, 1.0 - shear_stress / max(critical_stress,1e-6) )
              settling_flux = min( prob_settling * settling / depth, & 
                                  safe_factor / delt ) * cwater
!              pmsa(ipnt(ip_lastsingle+3*ntrwpm*nspmm+itel) ) = settling_flux * depth  ! g/m2/d

              !
              ! Flux to the bottom
              !
              fl(iflux+itel-1) = settling_flux
              enddo
              enddo
          endif
          endif

          !
          ! Increment the pointers
          !
          iflux = iflux + noflux
          ipnt(1:nitem) = ipnt(1:nitem) + increm(1:nitem)

      enddo
      
!.....Exchangeloop over de horizontale richting
      offset = 6+3*ntrwp*nspm
      ipnt(1:nitem) = ipoint(1:nitem)
      do IQ=1,NOQ1+NOQ2
        itel = 0
        do itrwp = 1,ntrwp
        do ispm = 1,nspm
            itel = itel + 1
            pmsa(ipnt(offset+ntrwp*nspm+itel)) = 0.0
        enddo
        enddo
        ipnt(1:nitem) = ipnt(1:nitem) + increm(1:nitem)
      enddo

!.....Exchangeloop over de verticale richting
      DO IQ = NOQ1+NOQ2+1 , NOQ1+NOQ2+NOQ3

         ifrom = IEXPNT(1,IQ)
         ito   = IEXPNT(2,IQ)
         IF ( ifrom > 0 .AND. Ito > 0 ) THEN

!rs             merk op: sedimentatie tussen waterlagen: geen taucr correctie,
!rs             alleen conversie van 1/d naar 1/s. Ten overvloede:
!rs             scu (s) en aux-timer (d) liggen dus vast!

            depfro = PMSA( ipoint(ip_depth) + (ifrom-1) * increm(ip_depth) )
            depto  = PMSA( ipoint(ip_depth) + (ito  -1) * increm(ip_depth) )
            itel = 0
            do itrwp = 1,ntrwp
            do ispm = 1,nspm
                itel = itel + 1
                settling = pmsa(ipnt(offset+itel))
                settling = min( settling , safe_factor * min(depfro,depto) / delt ) 
                pmsa(ipnt(offset+ntrwp*nspm+itel)) = settling /86400.
            enddo
            enddo
         ELSE
            itel = 0
            do itrwp = 1,ntrwp
            do ispm = 1,nspm
                itel = itel + 1
                pmsa(ipnt(offset+ntrwp*nspm+itel)) = 0.0
            enddo
            enddo
         ENDIF
        ipnt(1:nitem) = ipnt(1:nitem) + increm(1:nitem)
      enddo
!
      return
!
      end

      end module m_waqsediment
