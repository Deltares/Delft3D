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


      subroutine EMEPNI     ( pmsa   , fl     , ipoint , increm, noseg , &
                              noflux , iexpnt , iknmrk , noq1  , noq2  , &
                              noq3   , noq4   )
!DEC$ ATTRIBUTES DLLEXPORT, ALIAS: 'EMEPNI' :: EMEPNI
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
      integer,parameter   :: louts = 3
      integer            :: ipnt(lins+louts)    !    Local work array for the pointering

      ! pointers to concrete items
      integer,parameter   :: ip_dox = 1
      integer,parameter   :: ip_drd = 2
      integer,parameter   :: ip_wox = 3
      integer,parameter   :: ip_wrd = 4
      integer,parameter   :: ip_rai = 5
      integer,parameter   :: ip_sca = 6
      integer,parameter   :: ip_ddp = 7
      integer,parameter   :: ip_wdp = 8
      integer,parameter   :: ip_tot = 9


      ! input and output items
      real :: dox, drd, wox, wrd, rai, sca
      real :: ddp, wdp, tot
      
      logical first
      data first /.true./
      save

      if (.not.first) return

      ! loop for processing
      ipnt = ipoint(1:lins+louts)

      do iseg = 1 , noseg
            
        dox = max(pmsa(ipnt(ip_dox)),0.0)
        drd = max(pmsa(ipnt(ip_drd)),0.0)
        wox = max(pmsa(ipnt(ip_wox)),0.0)
        wrd = max(pmsa(ipnt(ip_wrd)),0.0)
        rai = max(pmsa(ipnt(ip_rai)),1.0)
        sca =     pmsa(ipnt(ip_sca))
          
        !  older models have a conversion from mg/m2/y to g/m2/d done in Hydro MT for drydep only
        !  this is funny for the general version
        ! to be able to keep running these older models there is the scale factor for drydep only
        ddp = (dox + drd)  / 1000. / 365. * sca
        !  from mg/m2/y to g/m3 using rainfall in mm/y
        ! g/m3   mg/m2/y     / milli m3/m2/y
        wdp = (wox + wrd) / rai
        tot = ddp + (wox + wrd) / 1000. / 365.
                 
        pmsa(ipnt(ip_ddp )) = ddp
        pmsa(ipnt(ip_wdp )) = wdp
        pmsa(ipnt(ip_tot )) = tot
          
        ipnt = ipnt + increm(1:lins+louts)
      enddo
      
      first = .false.
      return
      end
