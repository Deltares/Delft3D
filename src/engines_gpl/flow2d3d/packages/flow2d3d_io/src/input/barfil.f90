subroutine barfil(lundia    ,filbar    ,error     ,mmax      ,nmax      , &
                & nmaxus    ,kmax      ,kspu      ,kspv      ,nsluv     , &
                & mnbar     ,nambar    ,cbuv      ,gdp       )
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2024.                                
!                                                                               
!  This program is free software: you can redistribute it and/or modify         
!  it under the terms of the GNU General Public License as published by         
!  the Free Software Foundation version 3.                                      
!                                                                               
!  This program is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU General Public License for more details.                                 
!                                                                               
!  You should have received a copy of the GNU General Public License            
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
!  
!  
!!--description-----------------------------------------------------------------
!
!    Function: Reads the barrier location definitions and
!              coefficients from the attribute file
! Method used:
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use globaldata
    use string_module
    use dfparall
    use system_utils, only: exifil
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    integer, pointer      :: mfg
    integer, pointer      :: nfg
!
! Local parameters
! 
    integer, parameter :: MAXFLD = 10 
!
! Global variables
!
    integer                                                                   , intent(in)  :: kmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                                                 :: lundia !  Description and declaration in inout.igs
    integer                                                                   , intent(in)  :: mmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                                                 :: nmax   !  Description and declaration in esm_alloc_int.f90
    integer                                                                   , intent(in)  :: nmaxus !  Description and declaration in esm_alloc_int.f90
    integer                                                                                 :: nsluv  !  Description and declaration in dimens.igs
    integer      , dimension(5, nsluv)                                        , intent(out) :: mnbar  !  Description and declaration in esm_alloc_int.f90
    integer      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, 0:kmax)              :: kspu   !  Description and declaration in esm_alloc_int.f90
    integer      , dimension(gdp%d%nlb:gdp%d%nub, gdp%d%mlb:gdp%d%mub, 0:kmax)              :: kspv   !  Description and declaration in esm_alloc_int.f90
    logical                                                                                 :: error  !!  Flag=TRUE if an error is encountered 
    real(fp)     , dimension(4, nsluv)                                        , intent(out) :: cbuv   !  Description and declaration in esm_alloc_real.f90
    character(*)                                                                            :: filbar !!  File name for Barriers/contr.gates
    character(20), dimension(nsluv)                                                         :: nambar !  Description and declaration in esm_alloc_char.f90
!
! Local variables
!
    integer                          :: ibar                 ! Loop variable counting the number of valid barriers
                                                             ! When parallel, the total number of valid barriers in this partition may differ from nsluv.
                                                             ! In that case, nsluv must be updated.
    integer                          :: ibeg                 ! Begin position in the RECORD from where the search for data/record is started 
    integer                          :: idef                 ! Help var. containing default va- lue(s) for integer variable 
    integer                          :: iend                 ! Last position in the RECORD when the searched data/record is finished 
    integer                          :: ier                  ! =  0 -> end of record encountered =  1 -> real value found = -1 -> length or number of data is larger then specified by the calling routine 
    integer                          :: inc                  ! loop counter 
    integer                          :: incx                 ! Increment between M1,M2 
    integer                          :: incy                 ! Increment between N1,N2 
    integer                          :: iocond               ! IO status for reading 
    integer                          :: lenc                 ! Number of char. to read in string 
    integer                          :: lfile                ! Number of non blank characters of file name 
    integer                          :: luntmp               ! Temporary file unit 
    integer                          :: m                    ! loop counter 
    integer                          :: m1                   ! First m-index for barrier 
    integer                          :: m2                   ! Last m-index for barrier 
    integer                          :: maxinc               ! Maximum of (increment between M1,M2 & increment between N1,N2) 
    integer                          :: n                    ! loop counter 
    integer                          :: n1                   ! First n-index for barrier 
    integer                          :: n2                   ! Last n-index for barrier 
    integer, external                :: newlun
    integer                          :: nlook                ! Nr. of values to look for in a record 
    integer                          :: npar                 ! Number of parameters on input line before barrier velocity
    integer                          :: nrflds
    integer                          :: numbarlinesread      ! Nr. of barier lines read. This will always sum up to (the old value of) nsluv.
    integer, dimension(4)            :: ival                 ! Help array (integer) where the data, recently read from the MD-file, are stored temporarily 
    integer, dimension(maxfld)       :: ifield
    integer, dimension(maxfld)       :: itype
    integer, dimension(maxfld)       :: lenchr 
    logical                          :: outside              ! indicating whether a line of thin dams is outside subdomain (.TRUE.) or not (.FALSE.)
    logical                          :: onParbndIsInside
    real(fp)                         :: brlosc               ! Barrier energy loss coefficient 
    real(fp)                         :: gate                 ! Initial gate height for barrier 
    real(fp)                         :: rdef                 ! Help var. containing default va- lue(s) for real variable 
    real(fp), dimension(maxfld)      :: rfield
    character(10), dimension(maxfld) :: cfield 
    character(1)                     :: direct               ! Help string for reading direction 
    character(132)                   :: rec132               ! Standard rec. length in an attribute file (132) 
    character(20)                    :: btype                ! Barrier type
    character(40)                    :: errmsg               ! Text string error messages
    character(300)                   :: msg
!
!
!! executable statements -------------------------------------------------------
!
    mfg => gdp%gdparall%mfg
    nfg => gdp%gdparall%nfg
    !
    ! initialize local parameters
    !
    direct = ' '
    idef   = 0
    rdef   = 0.0_fp
    lenc   = 1
    errmsg = 'barrier '
    !
    ! test file existence
    !
    call remove_leading_spaces(filbar    ,lfile     )
    error = .not.exifil(filbar, lundia)
    if (error) goto 9999
    !
    ! open formatted file, if not formatted IOCOND <> 0
    !
    open (newunit=luntmp, file = filbar(:lfile), form = 'formatted', status = 'old',    &
        & iostat = iocond)
    if (iocond/=0) then
       error = .true.
       call prterr(lundia    ,'G007'    ,filbar(:lfile)       )
       goto 9999
    endif
    !
    ! freeformatted file, skip lines starting with a '*'
    !
    call skipstarlines(luntmp    )
    !
    ! freeformatted file, read input and test iocond
    !
    numbarlinesread = 0
    ibar            = 0
    ! --->
  100  continue
       if (numbarlinesread >= nsluv) goto 200
       read (luntmp, '(a)', iostat = iocond) rec132
       if (iocond /= 0) then
          if (iocond < 0) then
             call prterr(lundia, 'G006', filbar(:lfile))
          else
             call prterr(lundia, 'G007', filbar(:lfile))
          endif
          error = .true.
          goto 300
       endif
       !
       !  Ignore empty lines
       !
       if (rec132==' ') goto 100
       ! <---
       !
       ! Line read with a barrier description
       !
       numbarlinesread = numbarlinesread + 1
       !
       ! scan the line (skipping the name)
       !
       ibeg = 21
       iend = 132
       call scannr(rec132    ,ibeg      ,iend      ,nrflds    ,itype     , &
                 & ifield    ,rfield    ,cfield    ,lenchr    ,maxfld    , &
                 & .true.    ,.false.   ,.false.   )
       !
       ! stop when there are less than 7 parameters behind the barrier name
       ! or when the first parameter is not of type character
       !
       if (nrflds<7 .or. itype(1)/=3) then
          error = .true.
          call prterr(lundia    ,'G007'    ,filbar(1:lfile)      )
          call prterr(lundia    ,'P004'    ,'Invalid number of parameters specified for barrier "'//rec132(1:20)//'": '//trim(rec132(21:)) )
          goto 300
       endif
       !
       ! barrier location is defined in M,N coordinates
       ! parameter 2, 3, 4 and 5 all must be an integer
       !
       if (itype(2)/=1 .or. itype(3)/=1 .or. itype(4)/=1 .or. itype(5)/=1) then
          error = .true.
          call prterr(lundia    ,'G007'    ,filbar(1:lfile)      )
          call prterr(lundia    ,'P004'    ,'Non-integer data specified for M,N coordinates of barrier "'//rec132(1:20)//'": '//trim(rec132(21:)) )
          goto 300
       endif
       !
       ! Put the M,N indices in ival (partition safe) and put a copy in m/n1/2
       ival(1) = ifield(2) - mfg + 1
       ival(2) = ifield(3) - nfg + 1
       ival(3) = ifield(4) - mfg + 1
       ival(4) = ifield(5) - nfg + 1
       m1      = ival(1)
       n1      = ival(2)
       m2      = ival(3)
       n2      = ival(4)
       !
       ! Check if barrier is fully (.TRUE.) outside subdomain/partition
       ! If partly outside, ival is changed, specifying the interior part
       !
       onParbndIsInside = .true.
       call adjlin (ival, outside, gdp%d%mmax, gdp%d%nmaxus, onParbndIsInside)
       if (.not. parll) then
          !
          ! If ival is changed (compared with the copy in m/n1/2) the barrier is partly outside the domain
          ! This is not allowed when running sequential
          !
          if (       ival(1)/=m1 .or. ival(2)/=n1 &
              & .or. ival(3)/=m2 .or. ival(4)/=n2) then
             write(msg,'(3a)') "Barrier """, trim(rec132), """ lies partly outside the computational domain."
             call prterr(lundia, 'U021', trim(msg))
             error = .true.
             goto 300
          endif 
       endif
       if (outside) then
          if (parll) then
             !
             ! A barrier outside this partition is allowed
             ! do not increase ibar and process the next line from the input file
             !
             goto 100
          else
             write(msg,'(3a)') "Barrier """, trim(rec132), """ lies outside the computational domain."
             call prterr(lundia, 'U021', trim(msg))
             error = .true.
             goto 300
          endif
       else
          ibar          = ibar + 1
          m1            = ival(1)
          n1            = ival(2)
          m2            = ival(3)
          n2            = ival(4)
          mnbar(1,ibar) = m1
          mnbar(2,ibar) = n1
          mnbar(3,ibar) = m2
          mnbar(4,ibar) = n2
       endif
       !
       ! define barrier location name
       !
       nambar(ibar) = rec132(:20)
       !
       ! The lower case names are not only used to detect duplicate barrier names,
       ! but the lower case names must be passed to RTC when it is running online.
       !
       call small(nambar(ibar), 20)
       !
       ! There must be a name defined! Would be more logical to move this to top,
       ! but leave it here for the time being (backward consistent).
       !
       if (nambar(ibar) == '') then
          error = .true.
          call prterr(lundia    ,'G007'    ,filbar(1:lfile)      )
          call prterr(lundia    ,'P004'    ,'Empty barrier name specified on line: '//trim(rec132) )
          goto 300
       endif
       !
       ! barrier direction is indicated by U or V string
       !
       direct = cfield(1)(1:1)
       !
       ! Test contents of read coordinate pairs
       !
       call increm(m1        ,n1        ,m2        ,n2        ,incx      , &
                 & incy      ,maxinc    ,error     )
       if (error) then
          !
          ! No multiple of 45 degrees
          !
          errmsg(9:) = nambar(ibar)
          call prterr(lundia    ,'V231'    ,errmsg(:32)          )
          error = .true.
          goto 300
       endif
       !
       ! test for barrier type 'U' or 'V' and fill mask array
       !
       call small(direct, 1)
       m = m1 - incx
       n = n1 - incy
       !
       ! First U-barriers
       !
       if (direct == 'u') then
          mnbar(5, ibar) = 0
          do inc = 1, maxinc + 1
             m = m + incx
             n = n + incy
             !
             ! Check for different structures defined (not allowed)
             !
             if (abs(kspu(n, m, 0)) > 2) then
                errmsg(1:) = nambar(ibar) // ' on other structure'
                call prterr(lundia    ,'V235'    ,errmsg(:40)          )
                error = .true.
                goto 300
             endif
             !
             ! Define structure point masks for barrier as 3D-gate
             !
             kspu(n, m, 0) = 4
          enddo
       !
       ! V-barriers
       !
       elseif (direct == 'v') then
          mnbar(5, ibar) = 1
          do inc = 1, maxinc + 1
             m = m + incx
             n = n + incy
             !
             ! Check for different structures defined (not allowed)
             !
             if (abs(kspv(n, m, 0))>2) then
                errmsg(1:) = nambar(ibar) // ' on other structure'
                call prterr(lundia    ,'V235'    ,errmsg(:40)          )
                error = .true.
                goto 300
             endif
             !
             ! Define structure point masks for barrier as 3D-gate
             !
             kspv(n, m, 0) = 4
          enddo
       else
          errmsg(23:26) = ' (' // direct // ')'
          call prterr(lundia    ,'V233'    ,errmsg(:11)          )
          error = .true.
          goto 300
       endif
       !
       ! switch between old barriers with BRLOSC and GATEHEIGHT versus
       ! new barriers that continue with barrier formulation type:
       ! if parameter 7 is not of type character: old format
       !
       if (itype(7) < 3) then
          !
          ! old barrier format:
          ! read BRLOSC from record, default value not allowed
          ! stop when parameter 6 or 7 is of type character
          !
          if (itype(6)>2 .or. itype(7)>2) then
             error = .true.
             call prterr(lundia    ,'G007'    ,filbar(1:lfile)      )
             goto 300
          endif
          brlosc = rfield(6)
          gate   = rfield(7)
          !
          ! Value for loss coefficient permitted ?
          !
          if (brlosc < 0.0_fp) then
             call prterr(lundia    ,'V234'    ,errmsg(:22)          )
             error = .true.
             goto 300
          endif
          cbuv(1, ibar) = gate
          cbuv(2, ibar) = 1.0_fp
          cbuv(3, ibar) = brlosc
          !
          npar = 7
       else
          !
          ! new barrier format:
          !
          if (itype(6) > 2) then
             error = .true.
             call prterr(lundia    ,'G007'    ,filbar(1:lfile)      )
             goto 300
          endif
          cbuv(1, ibar) = rfield(6) ! gate height
          btype         = cfield(7)
          call small(btype, 20)
          select case(trim(btype))
          case('a')
             if (nrflds<8 .or. itype(8)>2) then
                error = .true.
                call prterr(lundia    ,'G007'    ,filbar(1:lfile)      )
                goto 300
             endif
             cbuv(2, ibar) = 1.0_fp
             cbuv(3, ibar) = rfield(8) ! brlosc
             !
             ! Value for loss coefficient permitted ?
             !
             if (rfield(8) < 0.0_fp) then
                error = .true.
                call prterr(lundia    ,'V234'    ,errmsg(:22)          )
                goto 300
             endif
          case('b')
             if (nrflds<9 .or. itype(8)>2 .or. itype(9)>2) then
                error = .true.
                call prterr(lundia    ,'G007'    ,filbar(1:lfile)      )
                goto 300
             endif
             cbuv(2, ibar) = 2.0_fp
             cbuv(3, ibar) = rfield(8)
             cbuv(4, ibar) = rfield(9)
             !
             ! Value for loss coefficient permitted ?
             !
             if (rfield(8)<0.0_fp .or. (rfield(8)+rfield(9))<0.0_fp) then
                error = .true.
                call prterr(lundia    ,'V234'    ,errmsg(:22)          )
                goto 300
             endif
             !
             npar = 9
          end select
       endif
       !
       ! process next line from the input file
       !
    goto 100
  200 continue
    !
    ! Finished reading input file
    ! When running parallel: nsluv may be smaller for this partition:
    nsluv = ibar
    !
    ! Not twice the same barrier name
    !
    do ibar = 1, nsluv
       do n = 1, ibar - 1
          if (nambar(n) == nambar(ibar)) then
             error = .true.
             call prterr(lundia, 'U021', 'Barrier name already used: ' // nambar(ibar))
          endif
       enddo
    enddo
    !
    ! close file
    !
  300 continue
    close (luntmp)
 9999 continue
end subroutine barfil
