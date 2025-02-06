!----- AGPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2024.                                
!                                                                               
!  This program is free software: you can redistribute it and/or modify         
!  it under the terms of the GNU Affero General Public License as               
!  published by the Free Software Foundation version 3.                         
!                                                                               
!  This program is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU Affero General Public License for more details.                          
!                                                                               
!  You should have received a copy of the GNU Affero General Public License     
!  along with this program.  If not, see <http://www.gnu.org/licenses/>.        
!                                                                               
!  contact: delft3d.support@deltares.nl                                         
!  Stichting Deltares                                                           
!  P.O. Box 177                                                                 
!  2600 MH Delft, The Netherlands                                               
!                                                                               
!  All indications and logos of, and references to, "Delft3D" and "Deltares"    
!  are registered trademarks of Stichting Deltares, and remain the property of  
!  Stichting Deltares. All rights reserved.                                     
!                                                                               
!-------------------------------------------------------------------------------

      function calbar (juldt, outuni)  result(RetVal)
!-----------------------------------------------------------------------
!         D e l f t      H y d r a u l i c s   -   Section    ZWS
!
!             Module: SUBROUTINE CALBAR
!           Function: Calculates new barrier values and stores them
!                     in array VALBAR.
!                     During interpolation it is assumed that requested
!                     date/time is always equal or later than the
!                     previous request.
!        Method used:
!               Date: 02-11-2001
!         Programmer: J. Zeekant
!        August 2004: G. Prinsen; conversion from subroutine to function
!                                 to enable creation of DLL without stops
!-----------------------------------------------------------------------
!   Calling routines:              RTC
!-----------------------------------------------------------------------
!   Called  routines:              write_error_message_rtc
!-----------------------------------------------------------------------
!  Formal parameters:
!  ------------------
!
!   Var. I/O  Type Dimensions
!   -------------------------
!
! JULDT   I  DP*8                  Mod. Julian date/time
! OUTUNI  I   I*4                  Unit of error output.
!-----------------------------------------------------------------------
!    Common variables:
!    -----------------
!
!   Var.      Type Dimensions
!   -------------------------
! BARINT      L*4  NumBarriers           Logical for interpolation method,
!                                  True is interpolate, false is block
! BARJUL     DP*8  NumBarriers*......    Array with Mod. Julian date/time
! BARVAL      R*4  NumBarriers*......    Array with values from data file
! NumBarriers       I*4                  Number of U- and V-Barriers
! NAMBAR     CH*20 NumBarriers           Barrier names received from Flow
!
!-----------------------------------------------------------------------
!    Local variables:
!    ----------------
!
!   Var.      Type Dimensions
!   ----------------------------------
!
! IBAR        I*4                  Loop counter
! RATIO      DP*8                  Interpolation factor
!-----------------------------------------------------------------------
!
!  declarations and specifications
!
      Use LocationDataModule
      Use SyncRtcFlow
      Use MeasureModule
      Use ParameterModule
      Use Readlib_rtc

      implicit none

!  return Value
      integer  :: RetVal 

!---- Formal variables
      integer   i3ddat, i3dtim
      integer   outuni

!---- Local variables
      integer                     :: ibar, i, IYEAR , IMONTH, IDAY  , IHOUR , IMIN  , ISEC
      double precision            :: juldt, ratio
      logical, save               :: lfirst = .true.
      character(len=CharIdLength) :: ID, NM
      
      RetVal = 0

!-----------------------------------------------------------------------
!     Do some initialisations, will only be done during first call
!-----------------------------------------------------------------------
      if (lfirst) then
        Init_loop : do ibar = 1, NumBarriers
!-------  Check if a table is associated with this barrier
          if (barmax(ibar) .eq. 0) cycle          
!-------- Initialise low end of active table period
          barlju(ibar) = barjul(ibar, 1)
          barlvl(ibar) = barval(ibar, 1)
!-------  Check if there are more rows
          if (barmax(ibar) .gt. 1) then
!---------- Set the high end of the active table period
            barhju(ibar) = barjul(ibar, 2)
            barhvl(ibar) = barval(ibar, 2)
            baract(ibar) = 2
          else
!---------- Only one row, copy low end of active table period
            barhju(ibar) = barjul(ibar, 1)
            barhvl(ibar) = barval(ibar, 1)
            baract(ibar) = 1
          endif
        enddo Init_loop
        lfirst = .false.
      endif
!-----------------------------------------------------------------------
!     Check the Julian date
!-----------------------------------------------------------------------
      if (juldt .lt. 0.0) then
        call write_error_message_rtc (946, 0, 'CALBAR', &
                     ' Wrong date requested from D3DFlow ', outuni)
        RetVal = 946
        Return
      endif
!-----------------------------------------------------------------------
!     Initialize the barrier values to no steering signal
!-----------------------------------------------------------------------
      valbar(1,:) = -1.0
      valbar(2,:) = 0.0
!-----------------------------------------------------------------------
!     Now handle all the barriers
!-----------------------------------------------------------------------
      Update_loop: do ibar = 1, NumBarriers
!-----------------------------------------------------------------------
!       First the case that requested time is before lower active time
!       However this only can occur because of rounding off errors
!       and is in fact same as equal to ....
!-----------------------------------------------------------------------
        if (barmax(ibar)==0) then
           ! skip 
        elseif (juldt .lt. barlju(ibar)) then
!
!-------- Backward 'interpolation'
!
          valbar(2, ibar) = barlvl(ibar)
          valbar(1, ibar) = 1.0
!-----------------------------------------------------------------------
!       Now check for the case that requested value is between the
!       active date/time.
!-----------------------------------------------------------------------
        elseif (juldt .ge. barlju(ibar) .and. juldt .le. barhju(ibar)) then
!
!-------- Interpolation or block value
!
          if (barint(ibar)) then
!
!--------- Interpolation
!
            ratio = 1.0 - (juldt - barlju(ibar)) / (barhju(ibar) - barlju(ibar))
            valbar(2, ibar) = ratio * barlvl(ibar) + (1.0 - ratio) * barhvl(ibar)
            valbar(1, ibar) = 1.0
          else
!
!---------- Block value          
!
            valbar(2, ibar) = barlvl(ibar)
            valbar(1, ibar) = 1.0
          endif
!-----------------------------------------------------------------------
!       The requested date is after the active table period.
!-----------------------------------------------------------------------
        else
!
!-------- Look for more data if available
!
          Search_loop: do while (baract(ibar) .lt. barmax(ibar))
!
!---------- Get the next period
!
            baract(ibar) = baract(ibar) + 1

            barlju(ibar) = barhju(ibar)
            barlvl(ibar) = barhvl(ibar)

            barhju(ibar) = barjul(ibar, baract(ibar))
            barhvl(ibar) = barval(ibar, baract(ibar))
!
!---------- Check if already in required period
!
            if (juldt .le. barhju(ibar)) then
!
!------------ Found!
!
              exit Search_loop
            endif
          enddo Search_loop
!
!-------- Check if there is something to interpolate or
!         just extrapolate
!
          if (juldt .le. barhju(ibar)) then
!
!---------- Interpolation or block value
!
            if (barint(ibar)) then
!
!----------- Interpolation
!
              ratio = 1.0 - (juldt - barlju(ibar)) / (barhju(ibar) - barlju(ibar))
              valbar(2, ibar) = ratio * barlvl(ibar) + (1.0 - ratio) * barhvl(ibar)
              valbar(1, ibar) = 1.0
            else
!
!------------ Block value          
!
              valbar(2, ibar) = barlvl(ibar)
              valbar(1, ibar) = 1.0
            endif
          else
!
!---------- Extrapolation
!
            valbar(2, ibar) = barhvl(ibar)
            valbar(1, ibar) = 1.0
          endif

        endif
!
!------ Set barrier levels using RTC decision parameters
!
        do i = NsMsId_SBK+1,NsMsId_D3D
!
!--------- Match barrier name with either RTC measure name or ID
!
           ID = MSSBID(i)
           NM = MSSBDescr(i)
           call LOWERC(ID)
           call LOWERC(NM)
           if (BarrierNames(ibar) == ID) then
              valbar(2, ibar) = MSSBST(i)
              valbar(1, ibar) = 1.0
           elseif (BarrierNames(ibar) == NM) then
              valbar(2, ibar) = MSSBST(i)
              valbar(1, ibar) = 1.0
           endif
        enddo

      enddo Update_loop



      end
