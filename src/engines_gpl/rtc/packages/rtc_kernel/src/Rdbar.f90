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

      function rdbar (filnam, filuni, outuni) result(RetVal)
!-----------------------------------------------------------------------
!         D e l f t      H y d r a u l i c s   -   Section    ZWS
!
!             Module: SUBROUTINE RDBAR
!           Function: Reads barrier height time dat from file created
!                     by TDATOM. The order can be different than
!                     order in D3D-Flow.
!        Method used:
!               Date: 01-11-2001
!         Programmer: J. Zeekant
!         August 2004: from subroutine changed into function by G Prinsen
!-----------------------------------------------------------------------
!   Calling routines:              RTC
!-----------------------------------------------------------------------
!   Called  routines:              ERRMSG
!-----------------------------------------------------------------------
!  Formal parameters:
!  ------------------
!
!   Var. I/O  Type Dimensions
!   -------------------------
!
! FILNAM  I  C*(*)                 Name of input file.
! FILUNI  I   I*4                  Unit number of input file.
! OUTUNI  I   I*4                  Unit of error output.
!-----------------------------------------------------------------------
!    Common variables:
!    -----------------
!
!   Var.      Type Dimensions
!   -------------------------
! BARINT      L*4  NumBarriers           Logical for interpolation method,
!                                  True is interpolate, false is block
! BARJUL     DP*8  NumBarriers*......    Array with Mod. Julian dat/time as result
!                                        from DP function Modified_Julian
! BARVAL      R*4  NumBarriers*......    Array with values from data file
! NumBarriers       I*4                  Number of U- and V-Barriers
! BarrierNames     CH*20 NumBarriers           Barrier names received from Flow
!
!-----------------------------------------------------------------------
!    Local variables:
!    ----------------
!
!   Var.      Type Dimensions
!   --------------------------------
!
! ALLERR      I*4                  Allocation error status (0 = OK)
! BARNAM     CH*20                 Name of barrier read from file
! BUFDAT      I*4                  Read buffer for date
! BUFTIM      I*4                  Read buffer for time
! BUFVAL      R*4                  Read buffer for barrier height value
! CINTER     CH*20                 Interpolation: linear or block.
! CURBAR      I*4                  Current barrier
! I           I*4                  Loop counter
! IOCOND      I*4                  File I/O-Status (0 = OK)
! ITABLE      I*4                  Loop counter for reading data from tables.
! JULDT             DP*8                  Mod. Julian date/time
! Modified_Julian   DP*8                  Function to calculate Mod. Julian day
! LEOF        L*4                  Logical to determine EOF
! LEXIST      L*4                  Logical to determine if input file
!                                  does exist.
! MAXTAB      I*4                  Length of the longest table
! NAMLEN      I*4                  Length of file name
! NRRECS      I*4                  Number of records in table.
! NTABLE      I*4                  Number of tables in file.
!-----------------------------------------------------------------------
!
!  declarations and specifications
!
      Use LocationDataModule
      Use SyncRtcFlow
      Use DH_Alloc
      Use ReadLib

      implicit none

! Return Value 0=ok, <>0 = error
      Integer :: RetVal

!---- Formal variables
      character filnam*(*)
      integer   filuni
      integer   outuni

!---- Local variables
      integer :: namlen, allerr, iocond, maxtab, curbar, nrrecs
      integer :: i
      integer bufdat, buftim
      real    bufval
      logical lexist
      logical leof
      integer itable, ntable
      character*20 barnam, cinter
      double precision juldt, Modified_Julian
      Integer BarHdr
!
      RetVal = 0
!
!---- First check if file name was read from FNM-file
!
      if (filnam .eq. ' ') then
        call ERRMSG (946, 0, 'RDBAR', ' No barrier data file read from FNM-file', outuni)
        RetVal = 946
        Return
      endif

!
!---- Trim file name
!
      call noextspaces(filnam, namlen)
!
!---- Check if file exists
!
      inquire (file=filnam(1:namlen), exist=lexist)
      if (.not. lexist) then
        call ERRMSG (946, 0, 'RDBAR', &
                     ' File '//filnam(1:namlen)//' does not exist', outuni)
        RetVal = 946
        Return
      endif
!
!---- File exists so open it.
!
      open (filuni, file=filnam(1:namlen),form='unformatted', IOSTAT=iocond)
      if (iocond .ne. 0) then
        call ERRMSG (946, 0, 'RDBAR', &
                     ' Error opening file: '//filnam(1:namlen), outuni)
        RetVal = 946
        Return
      endif
!
!---- First a scan to determine the maximum table length,
!     also the availability/correctness of required data
!     is checked.
!
      maxtab = -1
!
!---- Start the scan
!
      rewind(filuni)
      ntable = 0
      leof   = .false.
      Scan_loop: DO
!
!------ Read header
!
        RetVal = BARHDR (filnam, filuni, outuni, barnam, cinter, nrrecs, &
                         leof)
        if (RetVal .ne. 0) Return
!
!------ Check if end of file was found, which means no more tables
!
        if (leof) then
          exit Scan_loop
        endif
!------ Increase number of tables
        ntable = ntable + 1
!
!------ Check number of rows
!
        if (nrrecs .le. 0) then
          call ERRMSG (946, 0, 'RDBAR', &
                       ' Table must have > 0 rows in file: '//filnam(1:namlen)// &
                       ': Barrier: '//BarrierNames(i), OUTUNI)
          RetVal = 946
          Return
        endif
!
!------ Determine max. table length
!
        maxtab = MAX(maxtab, nrrecs)

!
!------ Now skip the table values
!
        skip_loop: do i = 1, nrrecs
          read(filuni, IOSTAT= iocond) bufdat, buftim, bufval
          if (iocond .lt. 0) then
            call ERRMSG (946, 0, 'RDBAR', &
                         ' Premature end of file: '//filnam(1:namlen), outuni)
            RetVal = 946
            Return
          endif
          if (iocond .gt. 0) then
            call ERRMSG (946, 0, 'RDBAR', &
                         ' Error reading file: '//filnam(1:namlen), outuni)
            RetVal = 946
            Return
          endif
        enddo skip_loop

      enddo Scan_loop
!
!---- Allocate the necessary array's
!
      RetVal = ALLOC3DBAR(NumBarriers, maxtab, outuni)
      If (RetVal .ne. 0) return
!
!---- Start reading the actual data, skip the not required
!
      rewind(filuni)
!
      Read_loop: DO itable = 1, ntable
!
!------ Read header
!
        RetVal = BARHDR (filnam, filuni, outuni, barnam, cinter, nrrecs, &
                     leof)
        if (RetVal .ne. 0) Return
!
!------ Check if end of file was found, now this means error!
!
        if (leof) then
          call ERRMSG (946, 0, 'RDBAR', &
                       ' Premature end of file: '//filnam(1:namlen), OUTUNI)
          RetVal = 946
          Return
        endif
!
!------ Get the current barrier, if not found data will be skipped.
!
        curbar = 0
        Getcur_loop: do i = 1, NumBarriers
          if (BarrierNames(i) .eq. barnam) then
            curbar = i
            exit Getcur_loop
          endif
        enddo Getcur_loop
!
!------ Store interpolation method and number of rows if required
!
        if (curbar .gt. 0) then
!-------- Interpolation method
          if (cinter(1:5) .eq. 'block') then
            barint(curbar) = .false.
          else
            barint(curbar) = .true.
          endif
!-------- Number of rows
          barmax(curbar) = nrrecs
        endif
!
!------ Now get the table values and store if required
!
        Data_loop: do i = 1, nrrecs
          read(filuni, IOSTAT= iocond) bufdat, buftim, bufval
          if (iocond .ne. 0) then
            call ERRMSG (946, 0, 'RDBAR', &
                         ' Error reading data from file: '//filnam(1:namlen), outuni)
            RetVal = 946
            Return
          endif
!
!-------- Store data if required
!
          if (curbar .gt. 0) then
!
!---------- Calculate the Double Precision Mod. Julian date
!
            juldt = Modified_Julian(bufdat, buftim)
!
!---------- Check on valid date/time
!
            if (juldt .lt. 0.0) then
              call ERRMSG (946, 0, 'RDBAR', &
                           ' Wronge date/time file: '//filnam(1:namlen)// &
                           ': Barrier: '//BarrierNames(i), OUTUNI)
              RetVal = 946
              Return
            endif
!
!---------- Store Mod. Julian date and barrier height
!
            barjul(curbar, i) = juldt
            barval(curbar, i) = bufval
          endif

        enddo Data_loop

      enddo Read_loop
      close(filuni,status='delete')

      Return
      end function RdBar




      Function BARHDR (filnam, filuni, outuni, barnam, cinter, nrrecs, &
                       leof)  result(RetVal)
!-----------------------------------------------------------------------
!         D e l f t      H y d r a u l i c s   -   Section    ZWS
!
!             Module: SUBROUTINE BARHDR
!           Function: Reads header of barrier table
!        Method used:
!               Date: 01-11-2001
!         Programmer: J. Zeekant
!-----------------------------------------------------------------------
!   Calling routines:              RDBAR
!-----------------------------------------------------------------------
!   Called  routines:              ERRMSG
!-----------------------------------------------------------------------
!  Formal parameters:
!  ------------------
!
!   Var. I/O  Type Dimensions
!   -------------------------
!
! BARNAM   O CH*(*)                Name of barrier read from file
! CINTER   O CH*(*)                Interpolation: linear or block.
! FILNAM  I  C*(*)                 Name of input file.
! FILUNI  I   I*4                  Unit number of input file.
! LEOF     O  L*4                  Logical to determine EOF
! NRRECS   O  I*4                  Number of records in table.
! OUTUNI  I   I*4                  Unit of error output.
!-----------------------------------------------------------------------
!    Common variables:
!    -----------------
!
!   Var.      Type Dimensions
!   -------------------------
!
!-----------------------------------------------------------------------
!    Local variables:
!    ----------------
!
!   Var.      Type Dimensions
!   -------------------------
! IOCOND      I*4                  File I/O-Status (0 = OK)
! NAMLEN      I*4                  Length of file name
!
!-----------------------------------------------------------------------

      Use ReadLib

      implicit none
      
! Return Value 0=ok, <>0 = error
      Integer :: RetVal

!---- Formal variables
      integer   filuni
      integer   outuni
      integer   nrrecs
      logical   leof
      character*(*) filnam, barnam, cinter

!---- Local variables
      integer namlen, iocond

!---- Initialise
      leof = .false.
      RetVal = 0

!
!---- Trim file name
!
      call noextspaces(filnam, namlen)
!
!---- Read name
!
      read(filuni, IOSTAT= iocond) barnam
      if (iocond .gt. 0) then
        call ERRMSG (946, 0, 'BARHDR', &
                     ' Error reading file: '//filnam(1:namlen), OUTUNI)
        RetVal = 946
        Return
      endif
      if (iocond .lt. 0) then
!
!------ No more tables!!
!
        leof = .true.
      else
        leof = .false.
      endif
!
!---- Only read more if there is more
!
      if (.not. leof) then
!
!------ First convert the name into lower case
!
        call SMALL(barnam, LEN(BARNAM))
!
!------ Read Interpolation method
!       Presume written correctly by TDATOM
!
        read(filuni, IOSTAT= iocond) cinter
        if (iocond .lt. 0) then
          call ERRMSG (946, 0, 'BARHDR', &
                       ' Premature end of file: '//filnam(1:namlen), OUTUNI)
        endif
        if (iocond .gt. 0) then
          call ERRMSG (946, 0, 'BARHDR', &
                       ' Error reading file: '//filnam(1:namlen), OUTUNI)
        endif
        if (Iocond .ne. 0) then
           RetVal = 946
           Return
        Endif
!
!------ Read Number of records
!       Presume written correctly by TDATOM
!
        read(filuni, IOSTAT= iocond) nrrecs
        if (iocond .lt. 0) then
          call ERRMSG (946, 0, 'BARHDR', &
                       ' Premature end of file: '//filnam(1:namlen), OUTUNI)
        endif
        if (iocond .gt. 0) then
          call ERRMSG (946, 0, 'BARHDR', &
                       ' Error reading file: '//filnam(1:namlen), OUTUNI)
        endif
        if (Iocond .ne. 0) then
           RetVal = 946
           Return
        Endif

      endif

      Return
      end Function BarHdr
