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



subroutine DIFTEM     ( pmsa   , fl     , ipoint , increm, noseg , &                            
                              noflux , iexpnt , iknmrk , noq1  , noq2  , &                            
                              noq3   , noq4   )                                                       
!DEC$ ATTRIBUTES DLLEXPORT, ALIAS: 'DIFTEM' :: DIFTEM                                     
!                                                                                                     
!*******************************************************************************                      
!                                                                                                     
      
      IMPLICIT NONE 
!                                                                                                     
!     Type    Name         I/O Description                                                            
!                                                                                                     
      real(4) pmsa(*)     ! I/O Process Manager System Array, window of routine to process library     
      real(4) fl(*)       ! O  Array of fluxes made by this process in mass/volume/time               
      integer ipoint(*)   ! I  Array of pointers in pmsa to get and store the data                    
      integer increm(*)   ! I  Increments in ipoint for segment loop, 0=constant, 1=spatially varying 
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
!
!     TRANSPORT FOR MULTIMEDIA MODEL
!     Type    Name         I/O Description                                        Unit                
!                                                                                                     
!     support variables
      integer, parameter :: lins = 11  
      integer, parameter :: line = 1
      integer, parameter :: louts = 3
      integer, parameter :: loute = 0
      integer, parameter :: npmsa = lins+line+louts+loute
      integer            :: ipnt(npmsa)

      integer ioq1, ioq2, iseg, iflux
      real    tflux
      
!     input items
      integer, parameter :: ip_noexf1toW = 1
      integer, parameter :: ip_noexf2toW = 2
      integer, parameter :: ip_tempair = 3
      integer, parameter :: ip_volume = 4
      integer, parameter :: ip_cloudf = 5
      integer, parameter :: ip_period1 = 6
      integer, parameter :: ip_wsin1 = 7
      integer, parameter :: ip_period2 = 8
      integer, parameter :: ip_wsin2 = 9
      integer, parameter :: ip_delt = 10
      integer, parameter :: ip_tempmin = 11
      integer, parameter :: ip_flow = 12
      integer, parameter :: ip_cloudp = 13
      integer, parameter :: ip_wsout1 = 14
      integer, parameter :: ip_wsout2 = 15
      
      integer noexf1toW
      integer noexf2toW
      real(4) volume
      real(4) tempair
      real(4) cloudf
      real(4) period1
      real(4) ws1
      real(4) period2
      real(4) ws2
      real(4) delt
      real(4) tempmin
      real(4) flow1, flow2
      real(4) cloudp
      
      logical first
      data first /.true./
      save 
!                                                                                                     
!******************************************************************************* 
!                                                                                                     
    if (first) then
        ! input by definition independent of space and time, these parameters are picked up for the first time and the first cell only and saved
        noexf1toW = nint(pmsa(ipoint(ip_noexf1toW)))
        noexf2toW = nint(pmsa(ipoint(ip_noexf2toW)))
        delt = pmsa(ipoint(ip_delt))
        tempmin = pmsa(ipoint(ip_tempmin))
    endif
    
    ! LOOP OVER SEGMENTS
    ipnt = ipoint(1:npmsa)
    iflux = 0
    do iseg = 1,noseg
        ioq1 = (noexf1toW-1)*noseg + iseg
        ioq2 = (noexf2toW-1)*noseg + iseg
        flow1 = max(pmsa(ipoint(ip_flow)+(ioq1-1)*increm(ip_flow)),0.0) ! m3/s
        flow2 = max(pmsa(ipoint(ip_flow)+(ioq2-1)*increm(ip_flow)),0.0) ! m3/s
        tempair = pmsa(ipnt(ip_tempair))
        volume = pmsa(ipnt(ip_volume))
        
        ! buffer flux1 over period
        period1 = pmsa(ipnt(ip_period1))/delt    ! expressed as timesteps
        ws1     = pmsa(ipnt(ip_wsin1))
        ws1 = ( (period1-1.0)*ws1 + tempair ) / period1
        pmsa(ipnt(ip_wsin1)) = ws1
        
        ! buffer flux2 over period
        period2 = pmsa(ipnt(ip_period2))/delt    ! expressed as timesteps
        ws2     = pmsa(ipnt(ip_wsin2))
        ws2 = ( (period2-1.0)*ws2 + tempair ) / period2
        pmsa(ipnt(ip_wsin2)) = ws2
        
        tflux  = (flow1*86400.*max(ws1,tempmin)+flow2*86400.*max(ws2,tempmin))/volume
!        if (isnan(tflux)) write (*,*) 'DIFTEM: ',iseg
        fl(iflux+1) = tflux
        
        cloudf = pmsa(ipnt(ip_cloudf))
        cloudp = cloudf * 100.
        pmsa(ipnt(ip_cloudp)) = cloudp
        
        ipnt = ipnt + increm(1:npmsa)
        iflux = iflux + noflux
    enddo
          
    first = .false.

    return
    end
