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

      subroutine pprlim ( pmsa   , fl     , ipoint , increm , noseg  ,
     &                    noflux , iexpnt , iknmrk , noq1   , noq2   ,
     &                    noq3   , noq4   )
!>\file
!>       Limitation (numerical) on primary production DYNAMO
!>
!>       If more Si is required than available, the growth of Diatoms is reduced.
!>       If more N and/or P is required than available, the growth of both species
!>       is reduced. The reduction has the form of a negative correction flux on C and
!>       produces together with the original flux on C the increase in algae species.
!>       The sum of original and correction C-flux is forwarded through the output
!>       variables fcPPxxx to the process NuUpt_Alg that makes the nutrient reduction
!>       fluxes.

      implicit none

!     Type    Name         I/O Description

      real(4) pmsa(*)     !I/O Process Manager System Array, window of routine to process library
      real(4) fl(*)       ! O  Array of fluxes made by this process in mass/volume/time
      integer ipoint( 14) ! I  Array of pointers in pmsa to get and store the data
      integer increm( 14) ! I  Increments in ipoint for segment loop, 0=constant, 1=spatially varying
      integer noseg       ! I  Number of computational elements in the whole model schematisation
      integer noflux      ! I  Number of fluxes, increment in the fl array
      integer iexpnt(4,*) ! I  From, To, From-1 and To+1 segment numbers of the exchange surfaces
      integer iknmrk(*)   ! I  Active-Inactive, Surface-water-bottom, see manual for use
      integer noq1        ! I  Nr of exchanges in 1st direction (the horizontal dir if irregular mesh)
      integer noq2        ! I  Nr of exchanges in 2nd direction, noq1+noq2 gives hor. dir. reg. grid
      integer noq3        ! I  Nr of exchanges in 3rd direction, vertical direction, pos. downward
      integer noq4        ! I  Nr of exchanges in the bottom (bottom layers, specialist use only)
      integer ipnt( 14)   !    Local work array for the pointering
      integer iseg        !    Local loop counter for computational element loop

!***********************************************************************
!
!     Project : STANDAARDISATIE PROCES FORMULES T721.72
!     Author  : Pascal Boderie
!     Date    : 1992-12-10             Version : 0.01
!
!     History :
!
!     Date        Author          Description
!     ----------  --------------  ------------------------------------
!     2014-05-25  L. Postma       Functionality adapted for robustness
!
!***********************************************************************

!     Type    Name         I/O Description                                        Unit

      real(4) fPPGreen    ! I  net primary production of Greens                   (gC/m3/d)
      real(4) NCRatGreen  ! I  N:C ratio Greens                                   (gN/gC)
      real(4) PCRatGreen  ! I  P:C ratio Greens                                   (gP/gC)
      real(4) fPPDiat     ! I  net primary production of Diatoms                  (gC/m3/d)
      real(4) NCRatDiat   ! I  N:C ratio Diatoms                                  (gN/gC)
      real(4) PCRatDiat   ! I  P:C ratio Diatoms                                  (gP/gC)
      real(4) SCRatDiat   ! I  Si:C ratio Diatoms                                 (gSi/gC)
      real(4) DELT        ! I  timestep for processes                             (d)
      real(4) NH4         ! I  Ammonium (NH4)                                     (gN/m3)
      real(4) NO3         ! I  Nitrate (NO3)                                      (gN/m3)
      real(4) PO4         ! I  Ortho-Phosphate (PO4)                              (gP/m3)
      real(4) Si          ! I  dissolved Silica (Si)                              (gSi/m3)
      real(4) fcPPGreen   ! O  numerical maximum flux Greens                      (gC/m3/d)
      real(4) fcPPDiat    ! O  numerical maximum flux Diatoms                     (gC/m3/d)
      real(4) dcPPGreen   ! F  correction flux Greens growth                      (gC/m3/d)
      real(4) dcPPDiat    ! F  correction flux Diatoms growth                     (gC/m3/d)
      integer IdcPPGreen  !    Pointer to the correction flux Greens growth
      integer IdcPPDiat   !    Pointer to the correction flux Diatoms growth
      real(4) ConmxN      !    Total available nitrogen, minimally 0.0            (gN/m3)
      real(4) ConmxP      !    Total available phosphorus, minimally 0.0          (gP/m3)
      real(4) ConmxS      !    Total available silica, minimally 0.0              (gSi/m3)
      real(4) N_demand    !    Nitrogen needed for this time step                 (gN/m3)
      real(4) P_demand    !    Phosphorus needed for this time step               (gP/m3)
      real(4) Si_demand   !    Silica needed for this time step                   (gSi/m3)
      real(4) N_fact      !    available / demand N                               (-)
      real(4) P_fact      !    available / demand P                               (-)
      real(4) Si_fact     !    available / demand Si                              (-)
      real(4) G_fact      !    available / demand N & P                           (-)

      ipnt        = ipoint
      IdcPPGreen  = 1
      IdcPPDiat   = 2

      do 9000 iseg = 1 , noseg

         if ( btest(iknmrk(iseg),0) ) then

            fPPGreen   = pmsa( ipnt(  1) )
            NCRatGreen = pmsa( ipnt(  2) )
            PCRatGreen = pmsa( ipnt(  3) )
            fPPDiat    = pmsa( ipnt(  4) )
            NCRatDiat  = pmsa( ipnt(  5) )
            PCRatDiat  = pmsa( ipnt(  6) )
            SCRatDiat  = pmsa( ipnt(  7) )
            DELT       = pmsa( ipnt(  8) )
            NH4        = pmsa( ipnt(  9) )
            NO3        = pmsa( ipnt( 10) )
            PO4        = pmsa( ipnt( 11) )
            Si         = pmsa( ipnt( 12) )

            ConmxN = amax1( NO3+NH4, 0.0 )
            ConmxP = amax1( PO4    , 0.0 )
            ConmxS = amax1( Si     , 0.0 )

            N_demand  = ( fPPDiat * NCratDiat + fPPGreen * NCRatGreen ) * DELT
            P_demand  = ( fPPDiat * PCratDiat + fPPGreen * PCRatGreen ) * DELT
            Si_demand = ( fPPDiat * SCratDiat ) * DELT

            N_fact  = 1.0
            P_fact  = 1.0
            Si_fact = 1.0
            if ( N_demand  .gt. ConmxN ) N_fact  = ConmxN / N_demand
            if ( P_demand  .gt. ConmxP ) P_fact  = ConmxP / P_demand
            if ( Si_demand .gt. ConmxS ) Si_fact = ConmxS / Si_demand
            G_fact = MIN ( N_fact, P_fact )

            if ( Si_fact .ge. G_fact ) then

               fcPPGreen = G_fact  * fPPGreen
               fcPPDiat  = G_fact  * fPPDiat

            else

               fcPPDiat  = Si_fact * fPPDiat
               if ( G_fact .eq. 1.0 ) then
                  fcPPGreen = fPPGreen
               else
                  ConmxN = ConmxN - fcPPDiat * NCratDiat * DELT
                  ConmxP = ConmxP - fcPPDiat * PCratDiat * DELT
                  N_demand = fPPGreen * NCRatGreen * DELT
                  P_demand = fPPGreen * PCRatGreen * DELT
                  N_fact  = 1.0
                  P_fact  = 1.0
                  if ( N_demand .gt. ConmxN ) N_fact  = ConmxN / N_demand
                  if ( P_demand .gt. ConmxP ) P_fact  = ConmxP / P_demand
                  fcPPGreen = MIN ( N_fact, P_fact ) * fPPGreen
               endif

            endif

!     CORRECTION ON Nett primary production 1 and 2

            dcPPGreen = fcPPGreen - fPPGreen
            dcPPDiat  = fcPPDiat  - fPPDiat
            fl  ( IdcPPGreen  ) = dcPPGreen
            fl  ( IdcPPDiat   ) = dcPPDiat
            pmsa( ipnt( 13)   ) = fcPPGreen
            pmsa( ipnt( 14)   ) = fcPPDiat

         endif

         IdcPPGreen  = IdcPPGreen  + noflux
         IdcPPDiat   = IdcPPDiat   + noflux
         ipnt        = ipnt        + increm

 9000 continue

      return
      end
