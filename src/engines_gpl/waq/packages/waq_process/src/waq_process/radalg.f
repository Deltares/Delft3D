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

      subroutine RADALG     ( pmsa   , fl     , ipoint , increm, noseg ,
     &                        noflux , iexpnt , iknmrk , noq1  , noq2  ,
     &                        noq3   , noq4   )
!>\file
!>       Light efficiency function DYNAMO algae
!>
!>              Routine is called per algae species with species specific coefficients
!>              Function returns 1.0 if:
!>              - both light at top of cell and at bottom of cell are larger than saturation
!>              - saturation value is zero
!>              If light within the cell becomes below the saturation value, then for
!>              that light a linear dependency between 0.0 and the saturation value is
!>              assumed: mu(I) = I/Isat for I < Isat and 1.0 for I > Isat.\n
!>              This (partly) linear lightcurve is analytically integrated over depth for
!>              the given extinction coefficient.\n
!>              If the extinction coefficient or the water depth is zero, like more or less
!>              in a chemostat, then the function value for the given amount of light
!>              is returned: either 1.0 or a value on the slope.

      implicit none

!     Type    Name         I/O Description

      real(4) pmsa(*)     !I/O Process Manager System Array, window of routine to process library
      real(4) fl(*)       ! O  Array of fluxes made by this process in mass/volume/time
      integer ipoint(  6) ! I  Array of pointers in pmsa to get and store the data
      integer increm(  6) ! I  Increments in ipoint for segment loop, 0=constant, 1=spatially varying
      integer noseg       ! I  Number of computational elements in the whole model schematisation
      integer noflux      ! I  Number of fluxes, increment in the fl array
      integer iexpnt(4,*) ! I  From, To, From-1 and To+1 segment numbers of the exchange surfaces
      integer iknmrk(*)   ! I  Active-Inactive, Surface-water-bottom, see manual for use
      integer noq1        ! I  Nr of exchanges in 1st direction (the horizontal dir if irregular mesh)
      integer noq2        ! I  Nr of exchanges in 2nd direction, noq1+noq2 gives hor. dir. reg. grid
      integer noq3        ! I  Nr of exchanges in 3rd direction, vertical direction, pos. downward
      integer noq4        ! I  Nr of exchanges in the bottom (bottom layers, specialist use only)
      integer ipnt(  6)   !    Local work array for the pointering
      integer iseg        !    Local loop counter for computational element loop

!***********************************************************************
!
!     Project : STANDAARDISATIE PROCES FORMULES T721.72
!     Author  : Pascal Boderie
!     Date    : 921210             Version : 0.01
!
!     History :
!
!     Date      Author          Description
!     --------  --------------  --------------------------------------
!     19921229  Pascal Boderie  Add third algae type, nutrient ratio's
!                               per species
!     20140530  Leo Postma      Allow for zero depth, zero extinction
!                               and/or zero RadSat
!
!***********************************************************************

!     Type    Name         I/O Description                                         Unit

      real(4) Depth       ! I  depth of segment                                    (m)
      real(4) Rad         ! I  irradiation at the segment upper-boundary           (W/m2)
      real(4) RadSat      ! I  total radiation growth saturation for this species  (W/m2)
      real(4) ExtVl       ! I  total extinction coefficient visible light          (1/m)
      real(4) TFGro       ! I  temperature function growth of this species <0-1>   (-)
      real(4) LimRad      ! O  radiation limitation function of this species <0-1> (-)
      logical LgtOpt      !    False if RadSat, Frad and LnFrad are equal for all cells
      real(4) Frad        !    Saturation fraction: < 1.0 is under-saturation      (-)
      real(4) LnFrad      !    natural logarithm of Frad                           (-)
      real(4) ExtDpt      !    product of extinction and depth                     (-)
      real(4) RadBot      !    radiation at the bottom of the cell                 (-)

      ipnt        = ipoint

      LgtOpt = .true.
      if ( increm(2) .eq. 0 .and. increm(3) .eq. 0 .and. increm(5) .eq. 0 ) then
         LgtOpt = .false.             !  This is constant for all cells
         Rad    = pmsa( ipnt(  2) )
         RadSat = pmsa( ipnt(  3) )
         TFGro  = pmsa( ipnt(  5) )
         RadSat = TFGro * RadSat      !  Correct RadSat for temperature
         if ( RadSat .gt. 1e-20 ) then
            Frad   = Rad / RadSat
            LnFrad = 0.0
            if ( Rad .gt. 1E-5 ) LnFrad = Log ( Frad )
         endif
      endif

      do 9000 iseg = 1 , noseg

         if ( btest(iknmrk(iseg),0) ) then

            if ( LgtOpt ) then
               Rad    = pmsa( ipnt(  2) )
               RadSat = pmsa( ipnt(  3) )
               TFGro  = pmsa( ipnt(  5) )
               RadSat = TFGro * RadSat
               if ( RadSat .gt. 1e-20 ) then
                  Frad   = Rad / RadSat
                  LnFrad = 0.0
                  if ( Rad .gt. 1E-5 ) LnFrad = Log ( Frad )
               endif
            endif

            if ( RadSat .le. 1e-20 ) then
               LimRad = 1.0
            else
               Depth = pmsa( ipnt(  1) )
               ExtVl = pmsa( ipnt(  4) )
               ExtDpt = ExtVl * Depth
               if ( ExtDpt .le. 1.0e-10 ) then    !  No extinction, e.g. chemostat
                  LimRad = min( Frad, 1.0 )
               else
                  RadBot = Frad * exp( - ExtDpt )
                  if ( Frad .gt. 1.0 ) then       !  Saturation at the surface of the cell
                     if ( RadBot .gt. 1.0 ) then
                        LimRad = 1.0
                     else
                        LimRad = ( 1.0 + LnFrad - RadBot ) / ExtDpt
                     endif
                  else
                     LimRad = ( Frad - RadBot ) / ExtDpt
                  endif
               endif
            endif

            pmsa( ipnt(  6)   ) = LimRad

         endif

         ipnt        = ipnt        + increm

 9000 continue

      return
      end subroutine
