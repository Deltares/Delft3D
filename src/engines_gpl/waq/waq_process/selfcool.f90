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

   
      subroutine HDISS      ( process_space_real   , fl     , ipoint , increm, num_cells ,                               &
                             noflux , iexpnt , iknmrk , num_exchanges_u_dir  , num_exchanges_v_dir  ,                               &
                             num_exchanges_z_dir   , num_exchanges_bottom_dir   )
      use m_extract_waq_attribute
                                   
!>\file
!>       Heat dissipation from heatbal & applied to excess heat                                                                                                          
!                                                                                                     
      IMPLICIT NONE

!     arguments
      REAL(kind=real_wp) ::process_space_real(*)            ! in/out input-output array space to be adressed with IPOINT/INCREM
      REAL(kind=real_wp) ::FL(*)              ! in/out flux array
      INTEGER(kind=int_wp) ::IPOINT(*)          ! in     start index input-output parameters in the process_space_real array (segment or exchange number 1)
      INTEGER(kind=int_wp) ::INCREM(*)          ! in     increment for each segment-exchange for the input-output parameters in the process_space_real array
      INTEGER(kind=int_wp) ::num_cells              ! in     number of segments
      INTEGER(kind=int_wp) ::NOFLUX             ! in     total number of fluxes (increment in FL array)
      INTEGER(kind=int_wp) ::IEXPNT(4,*)        ! in     exchange pointer table
      INTEGER(kind=int_wp) ::IKNMRK(*)          ! in     segment features array
      INTEGER(kind=int_wp) ::num_exchanges_u_dir               ! in     number of exchanges in first direction
      INTEGER(kind=int_wp) ::num_exchanges_v_dir               ! in     number of exchanges in second direction
      INTEGER(kind=int_wp) ::num_exchanges_z_dir               ! in     number of exchanges in third direction
      INTEGER(kind=int_wp) ::num_exchanges_bottom_dir               ! in     number of exchanges in fourth direction

!     from process_space_real array
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
      do iseg = 1 , num_cells
!                                                                                                     
      SurTemp    = process_space_real(IP1)
      HtTot      = process_space_real(IP2)
      HtTot2     = process_space_real(IP3)
      DELT       = process_space_real(IP4)
      Cp         = process_space_real(IP5)
      Rho0       = process_space_real(IP6)
      Temp       = process_space_real(IP7)
      Depth      = process_space_real(IP8)
      
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
                                                                                                   
         process_space_real(IP9) = SelfCool
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
