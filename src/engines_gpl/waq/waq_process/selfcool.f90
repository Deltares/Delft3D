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
      module m_selfcool
      use m_waq_precision


      implicit none

      contains

   
      subroutine HDISS      ( pmsa   , fl     , ipoint , increm, noseg ,                               & 
                             noflux , iexpnt , iknmrk , noq1  , noq2  ,                               & 
                             noq3   , noq4   )
      use m_evaluate_waq_attribute
                                   
!>\file
!>       Heat dissipation from heatbal & applied to excess heat                                                                                                          
!                                                                                                     
      IMPLICIT NONE

!     arguments
      REAL(kind=real_wp) ::PMSA(*)            ! in/out input-output array space to be adressed with IPOINT/INCREM
      REAL(kind=real_wp) ::FL(*)              ! in/out flux array
      INTEGER(kind=int_wp) ::IPOINT(*)          ! in     start index input-output parameters in the PMSA array (segment or exchange number 1)
      INTEGER(kind=int_wp) ::INCREM(*)          ! in     increment for each segment-exchange for the input-output parameters in the PMSA array
      INTEGER(kind=int_wp) ::NOSEG              ! in     number of segments
      INTEGER(kind=int_wp) ::NOFLUX             ! in     total number of fluxes (increment in FL array)
      INTEGER(kind=int_wp) ::IEXPNT(4,*)        ! in     exchange pointer table
      INTEGER(kind=int_wp) ::IKNMRK(*)          ! in     segment features array
      INTEGER(kind=int_wp) ::NOQ1               ! in     number of exchanges in first direction
      INTEGER(kind=int_wp) ::NOQ2               ! in     number of exchanges in second direction
      INTEGER(kind=int_wp) ::NOQ3               ! in     number of exchanges in third direction
      INTEGER(kind=int_wp) ::NOQ4               ! in     number of exchanges in fourth direction

!     from PMSA array
!                                                                                                     
      REAL(kind=real_wp) ::SurTemp            ! I  natural temperature                                (oC)        1
      REAL(kind=real_wp) ::HtTot              ! I  Total heat flux                                    (W/m2)      2
      REAL(kind=real_wp) ::HtTot2             ! I  Total heat flux for Temp+1                         (W/m2)      3
      REAL(kind=real_wp) ::SelfCool           ! O  Self Cooling                                       [W/deg/m2]  4
      REAL(kind=real_wp) ::DELT              ! I  DELWAQ process time step                           [d]         5
      REAL(kind=real_wp) ::Cp
      REAL(kind=real_wp) ::Rho0
      REAL(kind=real_wp) ::RhoWat
      REAL(kind=real_wp) ::Temp
      REAL(kind=real_wp) ::Depth
      REAL(kind=real_wp) ::HeatFlux           ! 1      excess temperature flux                           [oC/d]
    
!                                                                                                     
      INTEGER(kind=int_wp) ::IP1 ,IP2 ,IP3 ,IP4, IP5,IP6, IP7, IP8, IP9
      INTEGER(kind=int_wp) ::IFLUX , ISEG  , IKMRK1, IKMRK2

      IP1  = IPOINT(1 )
      IP2  = IPOINT(2 )
      IP3  = IPOINT(3 )
      IP4  = IPOINT(4 )
      IP5  = IPOINT(5 )
      IP6  = IPOINT(6 )
      IP7  = IPOINT(7 )
      IP8  = IPOINT(8 )
      IP9  = IPOINT(9 )      
      IFLUX = 0
      !                                                                                                     
      do iseg = 1 , noseg
!                                                                                                     
      SurTemp    = PMSA(IP1)
      HtTot      = PMSA(IP2)
      HtTot2     = PMSA(IP3)
      DELT       = PMSA(IP4)
      Cp         = PMSA(IP5)
      Rho0       = PMSA(IP6)
      Temp       = PMSA(IP7)
      Depth      = PMSA(IP8)
      
      RhoWat = Rho0 * (1.0 - 7.17e-6 * (Temp - 4.0) ** 2.0)
      
!
!     Calculate Self-Cooling only for active water segments
!
      IF (BTEST(IKNMRK(ISEG),0)) THEN

!     Calculate Self-Cooling only for top layer segments
!
          CALL extract_waq_attribute(2,IKNMRK(ISEG),IKMRK2)
                IF (IKMRK2==0 .OR. IKMRK2==1) THEN
                   SelfCool = -1. * (HtTot2 - HtTot) 
                    
!                  heat exchange coefficient = 1/d
! 
                    HeatFlux  = - SelfCool * 86400.0 * Surtemp /  (RhoWat * Cp * Depth)
!
                    IF (SurTemp > 0.0) THEN
!                       Limitation of FL(1) to amount of excess temperature present
                        HeatFlux = MAX (- SurTemp/DELT, HeatFlux )
                    ENDIF
                ENDIF   ! end IF over top segments        
                
                ENDIF      ! end IF over active segments
!                  
         FL(1+IFLUX) = HeatFlux 
                                                                                                   
         PMSA(IP9) = SelfCool    
!
         IFLUX = IFLUX + NOFLUX
         IP1   = IP1   + INCREM ( 1  )
         IP2   = IP2   + INCREM ( 2  )
         IP3   = IP3   + INCREM ( 3  )
         IP4   = IP4   + INCREM ( 4  )
         IP5   = IP5   + INCREM ( 5  )  
         IP6   = IP6   + INCREM ( 6  )
         IP7   = IP7   + INCREM ( 7  )
         IP8   = IP8   + INCREM ( 8  )
         IP9   = IP9   + INCREM ( 9  )          
         
!                                                                                                     
!      
      end do ! end DO over all segments
!                                                                                                     
      return
      end

      end module m_selfcool
