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

      subroutine IRONOX     ( pmsa   , fl     , ipoint , increm, noseg , &
                              noflux , iexpnt , iknmrk , noq1  , noq2  , &
                              noq3   , noq4   )
!JVB$ ATTRIBUTES DLLEXPORT, ALIAS: 'IRONOX' :: IRONOX
!
!*******************************************************************************
!
      IMPLICIT NONE
!
!     Type    Name         I/O Description
!
      real(4) pmsa(*)     !I/O Process Manager System Array, window of routine to process library
      real(4) fl(*)       ! O  Array of fluxes made by this process in mass/volume/time
      integer ipoint( 18) ! I  Array of pointers in pmsa to get and store the data
      integer increm( 18) ! I  Increments in ipoint for segment loop, 0=constant, 1=spatially varying
      integer noseg       ! I  Number of computational elements in the whole model schematisation
      integer noflux      ! I  Number of fluxes, increment in the fl array
      integer iexpnt(4,*) ! I  From, To, From-1 and To+1 segment numbers of the exchange surfaces
      integer iknmrk(*)   ! I  Active-Inactive, Surface-water-bottom, see manual for use
      integer noq1        ! I  Nr of exchanges in 1st direction (the horizontal dir if irregular mesh)
      integer noq2        ! I  Nr of exchanges in 2nd direction, noq1+noq2 gives hor. dir. reg. grid
      integer noq3        ! I  Nr of exchanges in 3rd direction, vertical direction, pos. downward
      integer noq4        ! I  Nr of exchanges in the bottom (bottom layers, specialist use only)
      integer ipnt( 18)   !    Local work array for the pointering
      integer iseg        !    Local loop counter for computational element loop
!
!*******************************************************************************
!
!     Type    Name         I/O Description                                        Unit
!
      real(4) feiid       ! I  total dissolved reducing iron                      (gFe/m3)
      real(4) oxy         ! I  Dissolved Oxygen                                   (g/m3)
      real(4) no3         ! I  Nitrate (NO3)                                      (gN/m3)
      real(4) frfe2dis    ! I  fraction dissolved free iron(II)                   (-)
      real(4) frfe2ohd    ! I  fraction of dissolved FeOH+                        (-)
      real(4) frfe2oh2d   ! I  fraction of dissolved Fe(OH)2                      (-)
      real(4) rci1oxox20  ! I  rate of Fe2+ oxid. with oxygen at 20 oC            (m3/gO2/d)
      real(4) rci2oxox20  ! I  rate of FeOH+ oxid. with oxygen at 20 oC           (m3/gO2/d)
      real(4) rci3oxox20  ! I  rate of Fe(OH)2 oxid. with oxygen T=20             (m3/gO2/d)
      real(4) rci1oxni20  ! I  rate of Fe2+ oxidation with nitrate T=20           (m3/gN/d)
      real(4) rci2oxni20  ! I  rate of FeOH+ oxidation with nitrate T=20          (m3/gN/d)
      real(4) rci3oxni20  ! I  rate of Fe(OH)2 oxid. with nitrate T=20            (m3/gN/d)
      real(4) tciox       ! I  temperature coefficient for iron oxidation         (-)
      real(4) temp        ! I  ambient water temperature                          (oC)
      real(4) delt        ! I  timestep for processes                             (d)
      real(4) poros       ! I  volumetric porosity                                (-)
      real(4) fioo        ! O  rate of iron oxidation with oxygen                 (gFe/m3/d)
      real(4) fion        ! O  rate of iron oxidation with nitrate                (gFe/m3/d)
      real(4) dioo        ! F  rate of iron oxidation with oxygen                 (gFe/m3/d)
      real(4) dion        ! F  rate of iron oxidation with nitrate                (gFe/m3/d)
      integer idioo       !    Pointer to the rate of iron oxidation with oxygen
      integer idion       !    Pointer to the rate of iron oxidation with nitrate
      real(4) tfiox       ! L  temperature function iron oxidation
      real(4) kioo1       ! L  rate of Fe2+ oxid. with oxygen
      real(4) kioo2       ! L  rate of FeOH+ oxid. with oxygen
      real(4) kioo3       ! L  rate of Fe(OH)2 oxid. with oxygen
      real(4) kion1       ! L  rate of Fe2+ oxidation with nitrate
      real(4) kion2       ! L  rate of FeOH+ oxidation with nitrate
      real(4) kion3       ! L  rate of Fe(OH)2 oxid. with nitrate

      ! initialise pointering in pmsa

      ipnt        = ipoint
      idioo       = 1
      idion       = 2

      do 9000 iseg = 1 , noseg

         feiid      = max(pmsa( ipnt(  1) ), 0.0 )
         oxy        = max(pmsa( ipnt(  2) ), 0.0 )
         no3        = max(pmsa( ipnt(  3) ), 0.0 )
         frfe2dis   = pmsa( ipnt(  4) )
         frfe2ohd   = pmsa( ipnt(  5) )
         frfe2oh2d  = pmsa( ipnt(  6) )
         rci1oxox20 = pmsa( ipnt(  7) )
         rci2oxox20 = pmsa( ipnt(  8) )
         rci3oxox20 = pmsa( ipnt(  9) )
         rci1oxni20 = pmsa( ipnt( 10) )
         rci2oxni20 = pmsa( ipnt( 11) )
         rci3oxni20 = pmsa( ipnt( 12) )
         tciox      = pmsa( ipnt( 13) )
         temp       = pmsa( ipnt( 14) )
         delt       = pmsa( ipnt( 15) )
         poros      = pmsa( ipnt( 16) )

         if ( oxy .le. 0.0 ) then
            dioo = 0.0
         else
            tfiox = tciox**(temp-20.)
            kioo1 = rci1oxox20*tfiox
            kioo2 = rci2oxox20*tfiox
            kioo3 = rci3oxox20*tfiox
            dioo  = (kioo1*frfe2dis + kioo2*frfe2ohd + kioo3*frfe2oh2d)*feiid*oxy/poros
         endif

         if ( oxy .le. 0.0 ) then
            dion = 0.0
         else
            tfiox = tciox**(temp-20.)
            kion1 = rci1oxni20*tfiox
            kion2 = rci2oxni20*tfiox
            kion3 = rci3oxni20*tfiox
            dion  = (kion1*frfe2dis + kion2*frfe2ohd + kion3*frfe2oh2d)*feiid*no3/poros
         endif

         ! maximise fluxes if neccesary

         if ( dioo + dion .gt. feiid/delt ) then
            dioo = (dioo/(dioo+dion))*0.5*feiid/delt
            dion = (dion/(dioo+dion))*0.5*feiid/delt
         endif

         fioo       = dioo
         fion       = dion

         fl  ( idioo       ) = dioo
         fl  ( idion       ) = dion
         pmsa( ipnt( 17)   ) = fioo
         pmsa( ipnt( 18)   ) = fion

         idioo       = idioo       + noflux
         idion       = idion       + noflux
         ipnt        = ipnt        + increm

 9000 continue

      return
      end
