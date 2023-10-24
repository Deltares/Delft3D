!!  Copyright (C)  Stichting Deltares, 2012-2023.
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
      module m_tempermode

      implicit none

      contains


      subroutine tmode  ( pmsa   , fl     , ipoint , increm , noseg  ,
     &                    noflux , iexpnt , iknmrk , noq1   , noq2   ,
     &                    noq3   , noq4   )
      use m_write_error_message

!>\file
!>       Defnines meaning of two modelled statevars ModTemp and NatTemp

      IMPLICIT NONE

!     arguments

      REAL               :: PMSA(*)            ! in/out input-output array space to be adressed with IPOINT/INCREM
      REAL               :: FL(*)              ! in/out flux array
      INTEGER            :: IPOINT(*)          ! in     start index input-output parameters in the PMSA array (segment or exchange number 1)
      INTEGER            :: INCREM(*)          ! in     increment for each segment-exchange for the input-output parameters in the PMSA array
      INTEGER            :: NOSEG              ! in     number of segments
      INTEGER            :: NOFLUX             ! in     total number of fluxes (increment in FL array)
      INTEGER            :: IEXPNT(4,*)        ! in     exchange pointer table
      INTEGER            :: IKNMRK(*)          ! in     segment features array
      INTEGER            :: NOQ1               ! in     number of exchanges in first direction
      INTEGER            :: NOQ2               ! in     number of exchanges in second direction
      INTEGER            :: NOQ3               ! in     number of exchanges in third direction
      INTEGER            :: NOQ4               ! in     number of exchanges in fourth direction

!     from PMSA array

      REAL               :: MTEMP              ! 1  in  Modelled temperature                                [oC]
      REAL               :: TMPNAT             ! 2  in  natural temperature of ambient water                [oC]
      INTEGER            :: ISWTMP             ! 3  in  DELWAQ process time step                             [d]
      REAL               :: TTEMP              ! 4  out Total temperature                                   [oC]
      REAL               :: ETEMP              ! 5  out EXCESS! temperature                                 [oC]

!     local decalrations


      INTEGER  IP1 ,IP2 ,IP3 ,IP4 ,IP5 ,IP6 
      INTEGER  IFLUX , ISEG  , IKMRK2

      IP1  = IPOINT(1 )
      IP2  = IPOINT(2 )
      IP3  = IPOINT(3 )
      IP4  = IPOINT(4 )
      IP5  = IPOINT(5 )
      IP6  = IPOINT(6 )
      

      DO 9000 ISEG = 1 , NOSEG

         MTEMP  = PMSA(IP1)
         TMPNAT = PMSA(IP2)
         ISWTMP = NINT(PMSA(IP3))

!        What is the meaning of modelled temperatures (one or two may be modelled)

!        User defines total and natural
         IF ( ISWTMP .EQ. 0 ) THEN
            TTEMP = MTEMP
            ETEMP = TTEMP - TMPNAT
!        User defines excess and natural            
         ELSEIF ( ISWTMP .EQ. 1 ) THEN
            ETEMP = MTEMP
            TTEMP = ETEMP + TMPNAT
!        User defines excess and total    
         ELSEIF ( ISWTMP .EQ. 2 ) THEN
            ETEMP = MTEMP
            TTEMP = TMPNAT  
         ELSE
            CALL write_error_message ('SwitchTemp has no valid value <0,1,2> in TMODE')
         ENDIF
       
!
!        Output flux, temp, surtemp, heat exchage and temperature increase due to radiation
!

         PMSA (IP4) = TTEMP
         PMSA (IP5) = ETEMP
         PMSA (IP6) = TMPNAT + 1     
!
         IP1   = IP1   + INCREM ( 1  )
         IP2   = IP2   + INCREM ( 2  )
         IP3   = IP3   + INCREM ( 3  )
         IP4   = IP4   + INCREM ( 4  )
         IP5   = IP5   + INCREM ( 5  )
         IP6   = IP6   + INCREM ( 6  )
!
 9000 CONTINUE
!
      RETURN
!
      END

      end module m_tempermode
