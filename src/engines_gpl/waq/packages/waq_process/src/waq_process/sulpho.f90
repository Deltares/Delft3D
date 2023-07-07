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

      subroutine SULPHO     ( pmsa   , fl     , ipoint , increm, noseg , &
                              noflux , iexpnt , iknmrk , noq1  , noq2  , &
                              noq3   , noq4   )
!JVB$ ATTRIBUTES DLLEXPORT, ALIAS: 'SULPHO' :: SULPHO
!
!*******************************************************************************
!
      IMPLICIT NONE
!
!     Type    Name         I/O Description
!
      real(4) pmsa(*)     !I/O Process Manager System Array, window of routine to process library
      real(4) fl(*)       ! O  Array of fluxes made by this process in mass/volume/time
      integer ipoint( 11) ! I  Array of pointers in pmsa to get and store the data
      integer increm( 11) ! I  Increments in ipoint for segment loop, 0=constant, 1=spatially varying
      integer noseg       ! I  Number of computational elements in the whole model schematisation
      integer noflux      ! I  Number of fluxes, increment in the fl array
      integer iexpnt(4,*) ! I  From, To, From-1 and To+1 segment numbers of the exchange surfaces
      integer iknmrk(*)   ! I  Active-Inactive, Surface-water-bottom, see manual for use
      integer noq1        ! I  Nr of exchanges in 1st direction (the horizontal dir if irregular mesh)
      integer noq2        ! I  Nr of exchanges in 2nd direction, noq1+noq2 gives hor. dir. reg. grid
      integer noq3        ! I  Nr of exchanges in 3rd direction, vertical direction, pos. downward
      integer noq4        ! I  Nr of exchanges in the bottom (bottom layers, specialist use only)
      integer ipnt( 11)   !    Local work array for the pointering
      integer iseg        !    Local loop counter for computational element loop
!
!*******************************************************************************
!
!     Type    Name         I/O Description                                        Unit
!
      real(4) fes         ! I  iron(II) sulphide                                  (gFe/m3)
      real(4) fes2        ! I  pyrite                                             (gFe/m3)
      real(4) oxy         ! I  Dissolved Oxygen                                   (g/m3)
      real(4) rcfesox20   ! I  specific rate of iron sulphide oxidation           (m3/gO2/d)
      real(4) rcfes2ox20  ! I  specific rate of pyrite oxidation                  (m3/gO2/d)
      real(4) tcfesox     ! I  temperature coeff. for iron sulphide oxidation     (-)
      real(4) temp        ! I  ambient water temperature                          (oC)
      real(4) delt        ! I  timestep for processes                             (d)
      real(4) poros       ! I  volumetric porosity                                (-)
      real(4) ffesox      ! O  rate of iron sulphide oxidation                    (gFe/m3/d)
      real(4) ffes2ox     ! O  rate of of pyrite oxidation                        (gFe/m3/d)
      real(4) dfesox      ! F  rate of iron sulphide oxidation                    (gFe/m3/d)
      real(4) dfes2ox     ! F  rate of of pyrite oxidation                        (gFe/m3/d)
      integer idfesox     !    Pointer to the rate of iron sulphide oxidation
      integer idfes2ox    !    Pointer to the rate of of pyrite oxidation
      real(4) tffesox     ! L  temperature function for iron sulphide oxidation
      real(4) rcfesox     ! L  specific rate of iron sulphide oxidation
      real(4) rcfes2ox    ! L  specific rate of pyrite oxidation

      ! initialise pointering in pmsa

      ipnt        = ipoint
      idfesox     = 1
      idfes2ox    = 2

      do 9000 iseg = 1 , noseg

         fes        = pmsa( ipnt(  1) )
         fes2       = pmsa( ipnt(  2) )
         oxy        = pmsa( ipnt(  3) )
         rcfesox20  = pmsa( ipnt(  4) )
         rcfes2ox20 = pmsa( ipnt(  5) )
         tcfesox    = pmsa( ipnt(  6) )
         temp       = pmsa( ipnt(  7) )
         delt       = pmsa( ipnt(  8) )
         poros      = pmsa( ipnt(  9) )

         ! no oxidation if no oxygen

         if ( oxy .le. 0.0 ) then

            ffesox     = 0.0
            ffes2ox    = 0.0

         else
            ! temperature function

            tffesox    = tcfesox**(temp-20.0)
            rcfesox    = rcfesox20*tffesox
            rcfes2ox   = rcfes2ox20*tffesox

            ! fluxes

            ffesox     = rcfesox*fes*oxy
            ffes2ox    = rcfes2ox*fes2*oxy

            ! maximise fluxes if neccesary

            if ( ffesox .gt. fes/delt ) then
               ffesox = 0.5*fes/delt
            endif
            if ( ffes2ox .gt. fes2/delt ) then
               ffes2ox = 0.5*fes2/delt
            endif

         endif

         dfesox  = ffesox
         dfes2ox = ffes2ox

         ! store flux and pmsa

         fl  ( idfesox     ) = dfesox
         fl  ( idfes2ox    ) = dfes2ox
         pmsa( ipnt( 10)   ) = ffesox
         pmsa( ipnt( 11)   ) = ffes2ox

         idfesox     = idfesox     + noflux
         idfes2ox    = idfes2ox    + noflux
         ipnt        = ipnt        + increm

 9000 continue

      return
      end
