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



      subroutine SOINIT     ( pmsa   , fl     , ipoint , increm, noseg , &
                              noflux , iexpnt , iknmrk , noq1  , noq2  , &
                              noq3   , noq4   )
!DEC$ ATTRIBUTES DLLEXPORT, ALIAS: 'SOINIT' :: SOINIT
!*******************************************************************************
!
      IMPLICIT NONE
!
!     Type    Name         I/O Description
!
      real(4) pmsa(*)     !I/O Process Manager System Array, window of routine to process library
      real(4) fl(*)       ! O  Array of fluxes made by this process in mass/volume/time
      integer ipoint(*)  ! I  Array of pointers in pmsa to get and store the data
      integer increm(*)  ! I  Increments in ipoint for segment loop, 0=constant, 1=spatially varying
      integer noseg       ! I  Number of computational elements in the whole model schematisation
      integer noflux      ! I  Number of fluxes, increment in the fl array
      integer iexpnt(4,*) ! I  From, To, From-1 and To+1 segment numbers of the exchange surfaces
      integer iknmrk(*)   ! I  Active-Inactive, Surface-water-bottom, see manual for use
      integer noq1        ! I  Nr of exchanges in 1st direction (the horizontal dir if irregular mesh)
      integer noq2        ! I  Nr of exchanges in 2nd direction, noq1+noq2 gives hor. dir. reg. grid
      integer noq3        ! I  Nr of exchanges in 3rd direction, vertical direction, pos. downward
      integer noq4        ! I  Nr of exchanges in the bottom (bottom layers, specialist use only)
!
!*******************************************************************************
!     D-EM Preprocessor to initialize Nitrogen coefficients
!     

!
!     Type    Name         I/O Description                                        Unit
!
      integer            :: iseg

    ! PMSA admin 
      integer,parameter   :: lins = 6
      integer,parameter   :: louts = 1
      integer            :: ipnt(lins+louts)    !    Local work array for the pointering

      ! pointers to concrete items
      integer,parameter   :: ip_c1 = 1
      integer,parameter   :: ip_c2 = 2
      integer,parameter   :: ip_c3 = 3
      integer,parameter   :: ip_f1 = 4
      integer,parameter   :: ip_f2 = 5
      integer,parameter   :: ip_f3 = 6
      integer,parameter   :: ip_cout = 7


      ! input and output items
      real :: c1, c2, c3
      real :: f1, f2, f3
      real :: cout
      
      logical first
      data first /.true./
      save

      if (.not.first) return

      ! loop for processing
      ipnt = ipoint(1:lins+louts)

      do iseg = 1 , noseg
            
        c1 = max(pmsa(ipnt(ip_c1)),0.0)
        c2 = max(pmsa(ipnt(ip_c2)),0.0)
        c3 = max(pmsa(ipnt(ip_c3)),0.0)
        f1 = pmsa(ipnt(ip_f1))
        f2 = pmsa(ipnt(ip_f2))
        f3 = pmsa(ipnt(ip_f3))
          
        ! unit conversion, cg/kg to mg/kg
        cout = ( f1*max(c1,0.0) + f2*max(c2,0.0) + f3*max(c3,0.0) ) * 10.0
                 
        pmsa(ipnt(ip_cout )) = cout
          
        ipnt = ipnt + increm(1:lins+louts)
      enddo
      
      first = .false.
      return
      end
