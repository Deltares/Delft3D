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

      subroutine refl   ( pmsa   , fl     , ipoint , increm , noseg  ,
     &                    noflux , iexpnt , iknmrk , noq1   , noq2   ,
     &                    noq3   , noq4   )
!>\file
!>       Reflection calculation

!
!     Description of the module :
!
!        Computes fraction of radiation refelection according to BLOOM
!
!        With latitutes over 23. degree north gives the same seasonal result
!        as originaly in BLOOM. Gives the right seasonal reflection at southern
!        hemisphere and fixed reflection near equator (below 23 degrees N/S).
!
! Name    T   L I/O   Description                                    Units
! ----    --- -  -    -------------------                            -----
! TIME    R*4 1 I  DELWAQ time in scu                              [scu]
! TREF    R*4 1 I  Refernce tim in days                              [d]
! RLAT    R*4 1 I  Latitude, north pos., south neg. [ radians ]   [grad]
!     Logical Units : -

!     Modules called : -

!     Name     Type   Library
!     ------   -----  ------------

      implicit none
      real     pmsa  ( * ) , fl    (*)
      integer  ipoint( * ) , increm(*) , noseg , noflux,
     +         iexpnt(4,*) , iknmrk(*) , noq1, noq2, noq3, noq4

      integer ip1,ip2,ip3,ip4,ip5
      integer in1,in2,in3,in4,in5
      real    time, tref, auxsys
      real    latitudeg, daynr, daynrrefl, reflec
      logical varflg
      integer weeknr , iseg

      in1  = increm( 1)
      in2  = increm( 2)
      in3  = increm( 3)
      in4  = increm( 4)
      in5  = increm( 5)

      ip1  = ipoint( 1)
      ip2  = ipoint( 2)
      ip3  = ipoint( 3)
      ip4  = ipoint( 4)
      ip5  = ipoint( 5)
!
      varflg = .true.
      if ( in1 .eq. 0 .and. in2 .eq. 0 .and. in3 .eq. 0 .and.
     +     in4 .eq. 0                                        ) then

!        Only constant inputs, so only single calculation of reflec needed to be set to all segments
         varflg = .false.
!
         time    = pmsa( ip1 )
         latitudeg = pmsa( ip2 )
         tref    = pmsa( ip3 )
         auxsys  = pmsa( ip4 )

!        Conversion time to daynumbers relative to tref
         daynr =  mod (time / auxsys + tref, 365.) !- 1
         
         ! Compute reflection correction
         if (abs(latitudeg).le.23.) then
             reflec = 0.05
         else
             if (latitudeg.gt.0.0) then
                 daynrrefl = daynr
             else
                 daynrrefl = mod(daynr + 365./2.,365.)
             endif
             weeknr = int(daynrrefl/7.)+1
             
!            reflection as it was in setabc from (to be improved/expanded later on)
             reflec = 0.05
             if ((weeknr .le. 17) .or. (weeknr .ge. 32)) reflec=0.06
             if ((weeknr .le. 13) .or. (weeknr .ge. 36)) reflec=0.08
             if ((weeknr .le.  4) .or. (weeknr .ge. 45)) reflec=0.10
         endif
      endif
      do 9000 iseg = 1 , noseg
         if ( varflg ) then
            time    = pmsa( ip1 )
            latitudeg = pmsa( ip2 )
            tref    = pmsa( ip3 )
            auxsys  = pmsa( ip4 )

!           Conversion time to daynumbers relative to tref
            daynr =  mod (time / auxsys + tref, 365.)

            ! Compute reflection correction
            if (abs(latitudeg).le.23.) then
                reflec = 0.05
            else
                if (latitudeg.gt.0.0) then
                    daynrrefl = daynr
                else
                    daynrrefl = mod(daynr + 365./2.,365.)
                endif
                weeknr = int(daynrrefl/7.)+1
                
!               reflection as in setabc (to be replaced)
                reflec = 0.05
                if ((weeknr .le. 17) .or. (weeknr .ge. 32)) reflec=0.06
                if ((weeknr .le. 13) .or. (weeknr .ge. 36)) reflec=0.08
                if ((weeknr .le.  4) .or. (weeknr .ge. 45)) reflec=0.10
            endif
         endif
         
         pmsa (ip5) = reflec

         ip1   = ip1   + in1
         ip2   = ip2   + in2
         ip3   = ip3   + in3
         ip4   = ip4   + in4
         ip5   = ip5   + in5
 9000 continue

      return
      end
