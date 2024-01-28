subroutine updbcq(lunbcq    ,lundia    ,itbct     ,ito       ,nto       , &
                & kcd       ,hydrbc    ,qsim      ,gdp       )
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
!    Function: Reads the QH-rel. hydrodynamic BC FROM FILE
! Method used: QH pair is read and stored in array
!              HYDRBC (K,N,1).
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use globaldata
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
!
! Global variables
!
    integer, intent(in)            :: ito
                                   !!  Index number of open boundary loc.
    integer, intent(in)            :: kcd !  Description and declaration in dimens.igs
    integer, intent(in)            :: lunbcq !  Description and declaration in luntmp.igs
    integer         :: lundia !  Description and declaration in inout.igs
    integer, intent(in)            :: nto !  Description and declaration in esm_alloc_int.f90
    integer, dimension(5, nto) :: itbct !  Description and declaration in esm_alloc_int.f90
    real(fp), intent(in)               :: qsim
    real(fp), dimension(4, nto, kcd) :: hydrbc !  Description and declaration in esm_alloc_real.f90
!
!
! Local variables
!
    integer                        :: iocond               ! Flag for reading errors = 0 No error < 0 End-Of-File reached > 0 Reading error 
!
!
!! executable statements -------------------------------------------------------
!
    !
    !
    !
    !
    !-----Read QH combinations until suitable range is found ...
    !
    if (itbct(5, ito)<=0) then
       !
       !-------Read from the QH table for the first time.
       !       Start reading the first pair of QH values.
       !
       itbct(5, ito) = itbct(3, ito)
       read (lunbcq, '(2g14.6)', rec = itbct(5, ito), iostat = iocond) &
          & hydrbc(1, ito, 1), hydrbc(2, ito, 1)
       if (iocond/=0) goto 8888
       if (itbct(4, ito)>itbct(3, ito)) then
          !
          !---------More than one record in QH table: read second record
          !
          read (lunbcq, '(2g14.6)', rec = itbct(5, ito) + 1, iostat = iocond) &
             & hydrbc (3, ito, 1), hydrbc (4, ito, 1)
          if (iocond/=0) goto 8888
       else
          !
          !---------Just one record in the QH table: constant waterlevel
          !         raise flag for out-of-range: itbct (1,ito) = -1
          !         and stretch the range to contain all discharges
          !
          itbct(1, ito) = -1
          hydrbc(4, ito, 1) = hydrbc(2, ito, 1)
          hydrbc(1, ito, 1) = min(qsim, -1.0E21_fp)
          hydrbc(3, ito, 1) = max(qsim, 1.0E21_fp)
          goto 9999
       endif
    endif
    !
    !---- Loop until QSIM >= HYDRBC(1,ITO,1)
    !
   10 continue
    if (qsim<hydrbc(1, ito, 1)) then
       if (itbct(5, ito)==itbct(3, ito)) then
          !
          !-------- The first record was already read. So, the discharge
          !         drops below the lowest discharge specified in the table:
          !         extend table to fit using assuming a constant waterlevel
          !         below the lowest specified discharge.
          !
          if (itbct(1, ito)==0) then
             !
             !---------- First time below lowest bound. Use the first QH pair
             !           as the upper limit of the range and raise the
             !           out-of-range flag (itbct(1,ito)).
             !
             hydrbc(3, ito, 1) = hydrbc(1, ito, 1)
             hydrbc(4, ito, 1) = hydrbc(2, ito, 1)
             itbct(5, ito) = itbct(3, ito) - 1
             itbct(1, ito) = -1
          endif
          !
          !-------- Stretch lower limit of range to fit
          !
          hydrbc(1, ito, 1) = min(qsim, -1.0E21_fp)
          goto 9999
       else
          !
          !---------This is not the first record, read previous record
          !         Reset the out of range flag (itbct (1,ito)), it might
          !         have been set and jump back to check whether now the
          !         correct range is selected.
          !
          itbct(1, ito) = 0
          hydrbc(3, ito, 1) = hydrbc(1, ito, 1)
          hydrbc(4, ito, 1) = hydrbc(2, ito, 1)
          itbct(5, ito) = itbct(5, ito) - 1
          read (lunbcq, '(2g14.6)', rec = itbct(5, ito), iostat = iocond) &
             & hydrbc (1, ito, 1), hydrbc (2, ito, 1)
          if (iocond/=0) goto 8888
          goto 10
       endif
    endif
    !
    !---- Loop until QSIM <= HYDRBC(3,ITO,1)
    !
   20 continue
    if (qsim>hydrbc(3, ito, 1)) then
       if (itbct(5, ito)==itbct(4, ito)) then
          !
          !-------- This is beyond the last pair of QH values: extend the
          !         table assuming a constant waterlevel above the highest
          !         specified value for the discharge. Stretch upper limit
          !         of the range to fit.
          !
          hydrbc(3, ito, 1) = max(qsim, 1.0E21_fp)
          itbct(1, ito) = -1
       else
          !
          !-------- Move upper limit to lower limit
          !
          hydrbc(1, ito, 1) = hydrbc(3, ito, 1)
          hydrbc(2, ito, 1) = hydrbc(4, ito, 1)
          itbct(5, ito) = itbct(5, ito) + 1
          if (itbct(5, ito)==itbct(4, ito)) then
             !
             !---------- This is the last pair of QH values. So, don't have
             !           anything to read from the BCQ file. Stretch upper limit
             !           of the range to fit.
             !
             hydrbc(3, ito, 1) = max(qsim, 1.0E21_fp)
             itbct(1, ito) = -1
          else
             !
             !---------- This is not the last record
             !           Reset the out of range flag (itbct (1,ito)), it might
             !           have been set and jump back to check whether now the
             !           correct range is selected.
             !
             itbct(1, ito) = 0
             read (lunbcq, '(2g14.6)', rec = itbct(5, ito) + 1, iostat = iocond) &
                & hydrbc (3, ito, 1), hydrbc (4, ito, 1)
             if (iocond/=0) goto 8888
             goto 20
          endif
       endif
    endif
    !
    !---- Normal Exit
    !
    goto 9999
    !
    !---- Error Exit
    !
 8888 continue
    call prterr(lundia    ,'U079'    ,' '       )
    !
    call d3stop(4         ,gdp       )
    !
    !
    !
    !
 9999 continue
end subroutine updbcq
