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



      subroutine SETFAT     ( pmsa   , fl     , ipoint , increm, noseg , &
                              noflux , iexpnt , iknmrk , noq1  , noq2  , &
                              noq3   , noq4   )
!DEC$ ATTRIBUTES DLLEXPORT, ALIAS: 'SETFAT' :: SETFAT
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
      integer            :: iseg, iatt1

    ! PMSA admin 
      integer,parameter   :: lins = 11
      integer,parameter   :: louts = 5
      integer            :: ipnt(lins+louts)    !    Local work array for the pointering

      ! pointers to concrete items
      integer,parameter   :: ip_decpav20 = 1
      integer,parameter   :: ip_decunp20 = 2
      integer,parameter   :: ip_decsoi20 = 3
      integer,parameter   :: ip_kdunpa20 = 4
      integer,parameter   :: ip_kdsoi20 = 5
      integer,parameter   :: ip_theta_decpav = 6
      integer,parameter   :: ip_theta_decunp = 7
      integer,parameter   :: ip_theta_decsoi = 8
      integer,parameter   :: ip_theta_kdunpa = 9
      integer,parameter   :: ip_theta_kdsoi = 10
      integer,parameter   :: ip_temp = 11
      
      integer,parameter   :: ip_decpav = 12
      integer,parameter   :: ip_decunp = 13
      integer,parameter   :: ip_decsoi = 14
      integer,parameter   :: ip_kdunpa = 15
      integer,parameter   :: ip_kdsoi = 16


      ! input and output items
      real :: decpav20 
      real :: decunp20 
      real :: decsoi20 
      real :: kdunpa20 
      real :: kdsoi20 
      real :: theta_decpav 
      real :: theta_decunp 
      real :: theta_decsoi 
      real :: theta_kdunpa 
      real :: theta_kdsoi
      real :: temp 
      
      real :: decpav 
      real :: decunp 
      real :: decsoi 
      real :: kdunpa 
      real :: kdsoi

      
      save


    ! loop for processing
    ipnt = ipoint(1:lins+louts)

    do iseg = 1 , noseg

        call dhkmrk(1,iknmrk(iseg),iatt1) ! pick up first attribute
        if (iatt1.gt.0) then
            
          decpav20 = pmsa(ipnt(ip_decpav20))
          decunp20 = pmsa(ipnt(ip_decunp20))
          decsoi20 = pmsa(ipnt(ip_decsoi20))
          kdunpa20 = pmsa(ipnt(ip_kdunpa20))
          kdsoi20 = pmsa(ipnt(ip_kdsoi20))
          theta_decpav = pmsa(ipnt(ip_theta_decpav))
          theta_decunp = pmsa(ipnt(ip_theta_decunp))
          theta_decsoi = pmsa(ipnt(ip_theta_decsoi))
          theta_kdunpa = pmsa(ipnt(ip_theta_kdunpa))
          theta_kdsoi = pmsa(ipnt(ip_theta_kdsoi))
          temp = pmsa(ipnt(ip_temp))
                 
          temp = max(min(temp,30.),0.)
          decpav  =  decpav20 * (theta_decpav**(temp-20.))
          decunp  =  decunp20 * (theta_decunp**(temp-20.))
          decsoi  =  decsoi20 * (theta_decsoi**(temp-20.))
          ! The temperature effect is on the dissolved fraction
          kdunpa  =  kdunpa20 * (theta_kdunpa**(temp-20.))
          kdsoi   =  kdsoi20 * (theta_kdsoi**(temp-20.))
          
          pmsa(ipnt(ip_decpav )) = decpav 
          pmsa(ipnt(ip_decunp )) = decunp 
          pmsa(ipnt(ip_decsoi )) = decsoi 
          pmsa(ipnt(ip_kdunpa )) = kdunpa 
          pmsa(ipnt(ip_kdsoi )) = kdsoi
         
       ! end IF active column
        endif
          
        ipnt = ipnt + increm(1:lins+louts)

    enddo
      
!******************************************************************************* NO PROCESSING in TIME LOOP

      return
      end
