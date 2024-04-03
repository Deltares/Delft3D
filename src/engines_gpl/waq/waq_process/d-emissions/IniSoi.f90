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



      subroutine INISOI     ( pmsa   , fl     , ipoint , increm, noseg , &
                              noflux , iexpnt , iknmrk , noq1  , noq2  , &
                              noq3   , noq4   )
!DEC$ ATTRIBUTES DLLEXPORT, ALIAS: 'INISOI' :: INISOI
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
!     D-EM Preprocessor to initialize top soil (Unp)
!     

!
!     Type    Name         I/O Description                                        Unit
!
      integer            :: iseg, iflux, iatt1

    ! PMSA admin 
      integer,parameter   :: lins = 10
      integer,parameter   :: louts = 1
      integer            :: ipnt(lins+louts)    !    Local work array for the pointering

      ! pointers to concrete items
      integer,parameter   :: ip_delt = 1
      integer,parameter   :: ip_fUnpaved = 2
      integer,parameter   :: ip_Thickness = 3
      integer,parameter   :: ip_Poros = 4
      integer,parameter   :: ip_RhoDM = 5
      integer,parameter   :: ip_QTopSoil = 6
      integer,parameter   :: ip_Area = 7
      integer,parameter   :: ip_FrPas = 8
      integer,parameter   :: ip_Soi = 9
      integer,parameter   :: ip_Sop = 10
      integer,parameter   :: ip_QSim = 11
      


      ! input and output items
      real   ::  Delt
      real   ::  fUnpaved
      real   ::  Thickness
      real   ::  Poros
      real   ::  RhoDM
      real   ::  QTopSoil
      real   ::  Area, FrPas
      real   ::  Soi, Sop
      real   ::  QSim

      real   ::  fluxinit, massinit, soildm

      logical first
      data first /.true./
      
      save
          delt = pmsa(ipoint(ip_delt))

          ! loop for processing
          ipnt = ipoint(1:lins+louts)
          iflux = 0

          do iseg = 1 , noseg

              call dhkmrk(1,iknmrk(iseg),iatt1) ! pick up first attribute
              if (iatt1.gt.0) then
                  
                  fUnpaved = pmsa(ipnt(ip_fUnpaved))
                  Thickness = pmsa(ipnt(ip_Thickness )) / 1000. ! from mm to m
                  Poros = pmsa(ipnt(ip_Poros ))
                  RhoDM = pmsa(ipnt(ip_RhoDM ))
                  QTopSoil = pmsa(ipnt(ip_QTopSoil ))
                  Area = pmsa(ipnt(ip_Area ))
                  FrPas = pmsa(ipnt(ip_FrPas ))
                  Soi = pmsa(ipnt(ip_Soi ))
                  Sop = pmsa(ipnt(ip_Sop ))
                  
                  ! kg      m2   -        m          -      kg/m3
                  soildm = Area*fUnpaved*Thickness*(1.-Poros)*RhoDM
                  if (soildm.gt.1.0) then
                      ! mg/kg       mg    / kg 
                      QSim = 1000. * (Soi+Sop) / soildm
                  else
                      QSim = 0.0
                    endif
                  pmsa(ipnt(ip_QSim )) = QSim
                  
                  if (first) then
                    !  g        mg/kg     g/mg       kg
                    massinit = QTopsoil / 1000. * soildm
                    fluxinit = massinit/delt ! g/s
                    fl (iflux + 1) = fluxinit * (1.0-FrPas)
                    fl (iflux + 2) = fluxinit *      FrPas
                  endif

          
              ! end IF active column
              endif
          
              ipnt = ipnt + increm(1:lins+louts)
              iflux = iflux + noflux

          enddo
      
          first = .false.
!******************************************************************************* NO PROCESSING in TIME LOOP

      return
      end
