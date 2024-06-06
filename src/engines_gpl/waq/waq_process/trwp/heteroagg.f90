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
      module m_heteroagg
      use m_waq_precision


      implicit none

      contains


      subroutine HETAGG   (  pmsa  , fl    , ipoint, increm, noseg , & 
                            noflux, iexpnt, iknmrk, noq1  , noq2  , & 
                            noq3  , noq4  )
      use m_evaluate_waq_attribute


!>\file
!>       Heteroaggregation of tyre/road wear particles to suspended solids

!
!     Description of the module :
!
!     Assumptions:
!     - The tyre/road wear particles and the suspended solids particles
!       have uniform sizes and densities
!     - The aggregation occurs according to the Smoluchowski theory
!     - The concentration of typre/road wear particles is much smaller
!       than that of suspended solids, so that the latter is not influenced
!
!     Note:
!     To keep the code simple, we implement the interaction between
!     single classes (even though in the acual application we need to
!     combine six classes of each, leading to 36 types of combined
!     particles)
!
! Name            T   Index   Description                                   Units
! ----            --- -       -------------------                            ----
! CTYRE           R*4 1   concentration of tyre/road wear particles          [g/m3]
! CSUSP           R*4 2   concentration of suspended solids                  [g/m3]
! DIAMETER_TYRE   R*4 3   diameter of the tyre/road wear particles              [m]
! DIAMETER_SUSP   R*4 4   diameter of the suspended solids                      [m]
! DENSTIY_TYRE    R*4 5   density of the tyre/road wear particles           [kg/m3]
! DENSITY_SUSP    R*4 6   density of the suspended solids                   [kg/m3]
! EFFICIENCY      R*4 7   aggregation efficiency                                [-]
! TEMPERATURE     R*4 8   temperature (Brownian motion)                        [oC]
! SETTLING_TYRE   R*4 9   settling velocity tyre/road wear particles          [m/d]
! SETTLING_SUSP   R*4 10  settling velocity suspended solids                  [m/d]
! FLOW_VELOCITY   R*4 11  flow velocity                                       [m/s]
! CHEZY           R*4 12  Chezy coefficient                               [m^0.5/s]
! DELT            R*4 6   time step                                             [d]
!
! NTYRE           R*4 --  number concentration of tyre/road wear particles   [n/m3]
! NSUSP           R*4 --  number concentration of suspended solids           [n/m3]
!
! FL (1)          R*4 1   aggregation flux                                 [g/m3/d]
!
! nov 2021 Jos van Gils added a loop over the fractions, to avoid long lists of processes and to speed up ...

      implicit none

      real(kind=real_wp) ::pmsa  ( * ) , fl    (*)
      integer(kind=int_wp) ::ipoint( * ) , increm(*) , noseg , noflux, & 
              iexpnt(4,*) , iknmrk(*) , noq1, noq2, noq3, noq4
!
!     local declarations
!
      real(kind=real_wp), parameter  ::pi                  = 3.1415926
      real(kind=real_wp), parameter  ::perday              = 86400.0        ! [s/day], seconds per day
      real(kind=real_wp), parameter  ::gravacc             = 9.8            ! [m/s2]
      real(kind=real_wp), parameter  ::boltzmann           = 1.38064852e-23 ! [J/K]
      real(kind=real_wp), parameter  ::dynamic_viscosity   = 1.0e-3         ! [kg/m.s]
      real(kind=real_wp), parameter  ::kinematic_viscosity = 1.0e-6         ! [m2/s]

      integer(kind=int_wp) ::iflux, iseg, ikmrk1,ikmrk2, itel
      real(kind=real_wp) ::ctyre, ntyre, diameter_tyre, density_tyre, settling_tyre, & 
              csusp, nsusp, diameter_susp, density_susp, settling_susp
      real(kind=real_wp) ::agg_rate, shear
      real(kind=real_wp) ::chezy, depth, efficiency, flow_velocity, temperature, & 
              delt
      real(kind=real_wp) ::volume_tyre, mass_tyre
      real(kind=real_wp) ::volume_susp, mass_susp

      integer(kind=int_wp) ::ipnt(500)
      integer(kind=int_wp),parameter  ::ip_nTRWP = 1
      integer(kind=int_wp),parameter  ::ip_nIM = 2
      integer(kind=int_wp),parameter  ::ip_Efficiency = 3
      integer(kind=int_wp),parameter  ::ip_Temp = 4
      integer(kind=int_wp),parameter  ::ip_Velocity = 5
      integer(kind=int_wp),parameter  ::ip_Chezy = 6
      integer(kind=int_wp),parameter  ::ip_Delt = 7
      integer(kind=int_wp),parameter  ::ip_Depth = 8
      integer(kind=int_wp),parameter  ::ip_lastsingle = 8

      integer(kind=int_wp) ::ntrwp, itrwp, nspm, ispm, nitem

      ntrwp = pmsa(ipoint(ip_ntrwp))
      nspm = pmsa(ipoint(ip_nim  ))
      nitem = ip_lastsingle + 4 * ntrwp + 4 * nspm
!
!  Note: we only need to do this once, no looping over the segments
!  as all particles of the same size class have the same properties
 
      ipnt(1:nitem) = ipoint(1:nitem)
!
      iflux = 1
      do iseg = 1 , noseg
          call extract_waq_attribute(1,iknmrk(iseg),ikmrk1)
          if (ikmrk1==1) then
          call extract_waq_attribute(2,iknmrk(iseg),ikmrk2)
          if (ikmrk2<=4) then   ! surface water
              
              ! input independentt of fractions
              efficiency     = pmsa(ipnt(ip_Efficiency) )
              temperature    = pmsa(ipnt(ip_Temp) ) + 273.0  ! We need the temperature in kelvin
              flow_velocity  = pmsa(ipnt(ip_Velocity))
              chezy          = pmsa(ipnt(ip_Chezy))
              delt           = pmsa(ipnt(ip_Delt))
              depth          = pmsa(ipnt(ip_Depth))
              
              ! loop over active fractions, IM are inner loop
              itel = 0
              do itrwp = 1,ntrwp
              do ispm = 1,nspm
                  
              itel = itel + 1

              ctyre          = pmsa(ipnt(ip_lastsingle               +itrwp) )
              csusp          = pmsa(ipnt(ip_lastsingle+4*ntrwp       +ispm ) )
              diameter_tyre  = pmsa(ipnt(ip_lastsingle+1*ntrwp       +itrwp) )* 1.0e-6 ! From micrometer to meter
              diameter_susp  = pmsa(ipnt(ip_lastsingle+4*ntrwp+1*nspm+ispm ) )* 1.0e-6
              density_tyre   = pmsa(ipnt(ip_lastsingle+2*ntrwp       +itrwp) )* 1.0e6  ! From kg/m3 to mg/m3
              density_susp   = pmsa(ipnt(ip_lastsingle+4*ntrwp+2*nspm+ispm ) )* 1.0e3  ! From kg/m3 to g/m3 (!)
              settling_tyre  = pmsa(ipnt(ip_lastsingle+3*ntrwp       +itrwp) )/ perday ! From m/d to m/s
              settling_susp  = pmsa(ipnt(ip_lastsingle+4*ntrwp+3*nspm+ispm ) )/ perday

              !
              ! calculate the _number_ of particles
              !
              volume_tyre = pi / 6.0 * diameter_tyre ** 3
              volume_susp = pi / 6.0 * diameter_susp ** 3

              mass_tyre = volume_tyre * density_tyre
              mass_susp = volume_susp * density_susp

              ntyre = ctyre / max( mass_tyre, tiny(mass_tyre) )
              nsusp = csusp / max( mass_susp, tiny(mass_susp) )

              !
              ! Contributions to the aggregation rate:
              !
              agg_rate = 0.0

              !
              ! The Brownian motion
              !
              agg_rate = agg_rate + (2.0/3.0) * & 
                            boltzmann * temperature / & 
                            dynamic_viscosity * & 
                            (diameter_tyre + diameter_susp) ** 2 / & 
                            (diameter_tyre * diameter_susp)

              !
              ! The interception process
              ! Note:
              ! The derivation of the equation for the shear is
              ! unclear. The formula is incorrect as far as dimensions
              ! are concerned, unless the constant 0.5 has the unit 1/kg.
              ! The problem of the origin has been reported in the
              ! Cardno report. The problem of the unit has not been
              ! reported, however.
              !
!!            shear = 0.5 * sqrt(gravacc) * abs(flow_velocity) /
!!   &                      (chezy * dynamic_viscosity)

              shear = sqrt( & 
                         4.0/(15.0 * kinematic_viscosity * 0.4 * depth) & 
                         * (sqrt(gravacc) * flow_velocity / chezy)**3 )

              agg_rate = agg_rate + shear / 6.0 * & 
                            (diameter_tyre + diameter_susp) ** 3

              !
              ! The differential settling
              ! (the settling velocity must be in m/s)
              !
              agg_rate = agg_rate + pi / 4.0 * & 
                            (diameter_tyre + diameter_susp) ** 2 * & 
                            abs(settling_tyre - settling_susp)

              !
              ! Nett flux (mass flux, so not the number of
              ! tyre/road wear particles)
              !
              ! Note: limit the rate to at most the mass that is present
              !       in the segment
              ! Note: as each tyre fraction can aggregate to several
              !       sediment fractions, limit to 1/10 of the available
              !       mass
              !
              fl(iflux+itel-1) = min( perday * efficiency * agg_rate * nsusp, & 
                              0.1 / delt ) * ctyre
              enddo
              enddo

          endif
          endif

          !
          ! Increment the pointers
          !
          iflux = iflux + noflux
          ipnt(1:nitem) = ipnt(1:nitem) + increm(1:nitem)
!
      end do
!
      return
!
      end

      end module m_heteroagg
