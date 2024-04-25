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

      subroutine HYDDFL     ( pmsa   , fl     , ipoint , increm, noseg , &
                              noflux , iexpnt , iknmrk , noq1  , noq2  , &
                              noq3   , noq4   )
!DEC$ ATTRIBUTES DLLEXPORT, ALIAS: 'HYDDFL' :: HYDDFL
!>\file
!>       Hydrodynamic variables for DUPROL processes

!*******************************************************************************
!
      IMPLICIT NONE
!
!     Type    Name          I/O Description
!
      real(4) pmsa(*)      !I/O Process Manager System Array, window of routine to process library
      real(4) fl(*)        ! O  Array of fluxes made by this process in mass/volume/time
      integer ipoint(15)   ! I  Array of pointers in pmsa to get and store the data
      integer increm(15)   ! I  Increments in ipoint for segment loop, 0=constant, 1=spatially varying
      integer noseg        ! I  Number of computational elements in the whole model schematisation
      integer noflux       ! I  Number of fluxes, increment in the fl array
      integer iexpnt(4,*)  ! I  From, To, From-1 and To+1 segment numbers of the exchange surfaces
      integer iknmrk(*)    ! I  Active-Inactive, Surface-water-bottom, see manual for use
      integer noq1         ! I  Nr of exchanges in 1st direction (the horizontal dir if irregular mesh)
      integer noq2         ! I  Nr of exchanges in 2nd direction, noq1+noq2 gives hor. dir. reg. grid
      integer noq3         ! I  Nr of exchanges in 3rd direction, vertical direction, pos. downward
      integer noq4         ! I  Nr of exchanges in the bottom (bottom layers, specialist use only)
      integer ipnt(15)     !    Local work array for the pointering
      integer iseg         !    Local loop counter for computational element loop
!
!*******************************************************************************
!
!     Type    Name          I/O Description                                        Unit
!
      real(4) volume       ! I                                                        m3
      real(4) surf         ! I                                                        m2
      real(4) delt         ! I                                                        d 
      real(4) flow         ! I                                                        m3/s
      real(4) xarea        ! I                                                        m2
      real(4) width        ! I                                                        m
      real(4) vwind        ! I                                                        m/s
      real(4) winddir      ! I                                                        degrees
      real(4) velocity     ! I                                                        m
      real(4) Z            ! O Average depth of segment                               m
      real(4) Q            ! O Flow                                                   m3/s
      real(4) As           ! O Flow area                                              m2
      real(4) dt           ! O Quality time step                                      s
      real(4) dx           ! O Half of the length of section                          m
      real(4) V            ! O Half of the volume of section                          m3
      real(4) Wf           ! O Wind velocity                                          m/s
      real(4) Wd           ! O Wind direction                                         degrees
      integer iq           !    Local loop counter for exchanges loop
      integer ifrom        !    
      integer ito          !
!
!*******************************************************************************
!
      ipnt        = ipoint
      
      do 9002 iseg = 1 , noseg
         volume             = pmsa( ipnt(  1) )
         surf               = pmsa( ipnt(  2) )
         delt               = pmsa( ipnt(  3) )
         width              = pmsa( ipnt(  4) )
         vwind              = pmsa( ipnt(  5) )
         winddir            = pmsa( ipnt(  6) )
         velocity           = pmsa( ipnt(  7) )
                
         IF (surf    .LT. 1E-30) surf = 1E-30
         
         Z  = volume / surf                   
         As = Z * width           
         Q  = As * velocity
         dt = delt * 86400                    
         dx = surf / width / 2                
         V  = volume / 2                      
         Wf = vwind                           
         Wd = winddir                         
         
         pmsa( ipnt(   8) ) = Z
         pmsa( ipnt(   9) ) = Q
         pmsa( ipnt(  10) ) = As
         pmsa( ipnt(  11) ) = dt
         pmsa( ipnt(  12) ) = dx
         pmsa( ipnt(  13) ) = V
         pmsa( ipnt(  14) ) = Wf
         pmsa( ipnt(  15) ) = Wd
         
         ipnt               = ipnt        + increm
9002  continue
!
      return
      end subroutine
