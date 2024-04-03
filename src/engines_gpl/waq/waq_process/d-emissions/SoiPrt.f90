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


      subroutine SOIPRT     ( pmsa   , fl     , ipoint , increm, noseg , &
                              noflux , iexpnt , iknmrk , noq1  , noq2  , &
                              noq3   , noq4   )
!DEC$ ATTRIBUTES DLLEXPORT, ALIAS: 'SOIPRT' :: SOIPRT
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
!     D-EM Preprocessor to set soil partitioning                        
!     

!
!     Type    Name         I/O Description                                        Unit
!
      integer            :: iseg, iatt1

    ! PMSA admin 
      integer,parameter   :: lins = 5
      integer,parameter   :: louts = 1
      integer            :: ipnt(lins+louts)    !    Local work array for the pointering

      ! pointers to concrete items
      
        integer,parameter :: ip_Poros = 1
        integer,parameter :: ip_RhoDM = 2
        integer,parameter :: ip_fOM = 3
        integer,parameter :: ip_Kd = 4 
        integer,parameter :: ip_lKoc = 5
        integer,parameter :: ip_fDis = 6

      ! input and output items
      
        real :: Poros 
        real :: RhoDM, fOM
        real :: Kd, lKoc, Kdcalc
        real :: fDis
                 
      save


    ! loop for processing
    ipnt = ipoint(1:lins+louts)

    do iseg = 1 , noseg

        call dhkmrk(1,iknmrk(iseg),iatt1) ! pick up first attribute
        if (iatt1.gt.0) then
            
            Poros = pmsa(ipnt(ip_Poros ))
            RhoDM = pmsa(ipnt(ip_RhoDM ))
            fOM = pmsa(ipnt(ip_fOM ))
            Kd = pmsa(ipnt(ip_Kd ))
            lKoc = pmsa(ipnt(ip_lKoc ))
            
            ! simple partitioning
            if (Kd.gt.0.0) then
                ! m3/kg
                Kdcalc = Kd
            elseif (lKoc.gt.0.0) then ! convert from log(l/kg)
                Kdcalc = (10.**(lKoc))/1000.*fOM
            else
                Kdcalc = 0.0
            endif
            ! -                m3/kg          kg/m3
            fdis = 1. / ( 1. + Kdcalc*(1.-Poros)*RhoDM )
!
!---- Output of module
!
           pmsa(ipnt(ip_fDis)) = fDis
          
       ! end IF active column
        endif
          
        ipnt = ipnt + increm(1:lins+louts)

    enddo
      
!******************************************************************************* NO PROCESSING in TIME LOOP

      return
      end
