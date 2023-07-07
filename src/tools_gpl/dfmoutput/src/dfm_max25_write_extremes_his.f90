module write_extremes_his
! ===============================================================================
!
!  Programmer      Huib Tanis, Deltares
!
!  Copied from: URL: https://repos.deltares.nl/repos/simona/bo_omgeving/simona/src/postproc/postgen/modules/write_extremes_his.f90
!  Revision: 7035 $, $Date: 2018-09-11 18:43:01 +0200 (di, 11 sep 2018)
!
!  COPYRIGHT
!
!  Copyright (c) "Rijkswaterstaat".
!  Permission to copy or distribute this software or documentation
!  in hard copy or soft copy granted only by written licence
!  obtained from "Rijkswaterstaat".
!  All rights reserved. No part of this publication may be
!  reproduced, stored in a retrieval system (e.g., in memory, disk,
!  or core) or be transmitted by any means, electronic, mechanical,
!  photocopy, recording, or otherwise, without written permission
!  from the publisher.
!
! *******************************************************************************
!
!  DESCRIPTION
!
!  Write extreme values including so called max13, max25, etc values to file
!
! *******************************************************************************
implicit none

private :: is_above_threshold, write_mean_nloc_2string, write_mean_range_2string
public  :: write_extremes_statloop, write_extremes_stat, write_extremes_header, write_val2string

contains

logical function is_above_threshold(val, threshold)
! Calculate if the value 'val' is above a threshold value 'threshold'
    real, intent(in)           :: val        ! input value
    real, optional, intent(in) :: threshold  ! threshold value (optional)

    is_above_threshold = .true.
    if (present(threshold)) then
       if (val < threshold) is_above_threshold = .false.
    endif
end function is_above_threshold


subroutine write_val2string(val, str, offset, threshold)
! Write a value to string
real, intent(in)                :: val             ! input value
character(len=*), intent(inout) :: str             ! input/output string
integer, intent(in)             :: offset          ! string off set
real, optional, intent(in)      :: threshold       ! threshold value (optional)

   if (is_above_threshold(val, threshold)) then
      if (abs(val) < 999999.999) then
         write(str(offset:), '(f12.4)') val        ! 'normal' print
      else if (abs(val) <= 9999999999.0) then
         write(str(offset:), '(f12.0)') val        ! print format: big values
      else
         write(str(offset:), '(E12.4)') val        ! print format: very big values
      endif
   else
      write(str(offset:), '(a7)') '-'              ! write '-' sign to string instead of the value
   endif
end subroutine write_val2string


subroutine write_mean_nloc_2string(rdata, iloc, nloc, str, offset, threshold)
! Write the mean of a range values ('nloc') around a certain location ('iloc') to a string ('str')
real, intent(in)                :: rdata(:)    ! input array
integer, intent(in)             :: iloc        ! location index
integer, intent(in)             :: nloc        ! number of locations around the location index
character(len=*), intent(inout) :: str         ! string
integer, intent(in)             :: offset      ! begin index of the string to write the value
real, optional, intent(in)      :: threshold   ! threshold value (optional)
! local variables
integer :: nloc_2   ! half of nloc
real    :: rmean    ! mean

   nloc_2 = (nloc - 1) / 2

   if (iloc <= nloc_2 .or. iloc > size(rdata)-nloc_2) then
      ! write '-' sign to string
      write(str(offset:), '(a7)') '-'
      return
   endif

   ! write mean value to string
   rmean = sum(rdata(iloc-nloc_2:iloc+nloc_2)) / real(nloc)
   call write_val2string(rmean, str, offset, threshold)

end subroutine write_mean_nloc_2string


subroutine write_mean_range_2string(rdata, ib, ie, str, offset, threshold)
! Calculate the mean of a vector part (certain range) and write to a string
real, intent(in)                :: rdata(:)    ! input array
integer, intent(in)             :: ib, ie      ! start and end index in an array
character(len=*), intent(inout) :: str         ! string
integer, intent(in)             :: offset      ! begin index of the string to write the value
real, optional, intent(in)      :: threshold   ! threshold value (optional)
! local variable
real    :: rmean    ! mean

   if (ib < 1 .or. ie > size(rdata)) then
      ! write '-' sign to string
      write(str(offset:), '(a7)') '-'
      return
   endif

   ! write mean value to string
   rmean = sum(rdata(ib:ie)) / real(ie - ib + 1)
   call write_val2string(rmean, str, offset, threshold)

end subroutine write_mean_range_2string


subroutine write_extremes_statloop( rdata, i, iunout, stationsname, minmaxlst, threshold)
! Write extremes for history station
real, intent(in)             :: rdata(:)       ! data history station
integer, intent(in)          :: i              ! station number
integer, intent(in)          :: iunout         ! file unit
character(len=*), intent(in) :: stationsname   ! station name
integer, intent(in)          :: minmaxlst(:)   ! integer array for the extreme quantities
real, optional, intent(in)   :: threshold      ! threshold value (optional)
! local variables
integer            :: k              ! offset
integer            :: klen = 12      ! length to next offset
integer            :: j              ! loop counter
integer            :: nhisf          ! number of values in the time series
integer            :: iloc           ! location of the element in an array
integer            :: nloc           ! number of locations
character(len=800) :: str_val        ! string with 'values'
integer            :: ib, ie         ! start and end index in an array
!==============================================================================
   nhisf = size(rdata)
   write(str_val,'(i6)') i
   k = 7

   ! write first value to string
   call write_val2string(rdata(1), str_val, k, threshold)
   k = k + klen

   ! write minimum value to string
   call write_val2string(minval(rdata), str_val, k, threshold)
   k = k + klen

   ! write min.. (min13, min25, etc) to string
   iloc = minloc(rdata, dim=1)
   do j = 1, size(minmaxlst)
      nloc = minmaxlst(j)
      call write_mean_nloc_2string(rdata, iloc, nloc, str_val, k, threshold)
      k = k + klen
   end do

   ! write maximum value to string
   call write_val2string(maxval(rdata), str_val, k, threshold)
   k = k + klen

   ! write max.. (max13, max25, etc) to string
   iloc = maxloc(rdata, dim=1)
   do j = 1, size(minmaxlst)
      nloc = minmaxlst(j)
      call write_mean_nloc_2string(rdata, iloc, nloc, str_val, k, threshold)
      k = k + klen
   end do

   ! write last value to string
   call write_val2string(rdata(nhisf), str_val, k, threshold)
   k = k + klen

   ! write last.. (last13, last25, etc) to string
   do j = 1, size(minmaxlst)
      ib    = nhisf - minmaxlst(j) + 1
      ie    = nhisf
      call write_mean_range_2string(rdata, ib, ie, str_val, k, threshold)
      k = k + klen
   end do

   ! write station name to string
   str_val(k+2:) = stationsname

   ! write string to file
   write(iunout, '(a)') trim(str_val)

end subroutine write_extremes_statloop


subroutine write_extremes_header ( iunout, minmaxlst )
! Write header for extreme quantities
integer, intent(in) :: iunout        ! file unit
integer, intent(in) :: minmaxlst(:)  ! integer array for the extreme quantities
! local variables
character(len=:), allocatable :: str         ! string
character(len=12)             :: str2        ! string
character(len=12)             :: str_blanks = '            '
integer :: j                                 ! loop counter
integer :: nspaces                           ! number of spaces

   allocate(character((6+3*size(minmaxlst))*12) :: str)
   str = 'nummer       first'
   ! header: minimum and min... (min13, min25, etc)
   str = str // '     minimum'
   do j = 1, size(minmaxlst)
      write (str2, *) minmaxlst(j)
      nspaces = index( str2, ' ', back=.true. ) - 3
      str = str // str_blanks(1:nspaces) // 'min' // trim(adjustl(str2))
   end do

   ! header: maximum and max... (max13, max25, etc)
   str = str // '     maximum'
   do j = 1, size(minmaxlst)
      write (str2, *) minmaxlst(j)
      nspaces = index( str2, ' ', back=.true. ) - 3
      str = str // str_blanks(1:nspaces) // 'max' // trim(adjustl(str2))
   end do

   ! header: last and last... (last13, last25, etc)
   str = str // '        last'
   do j = 1, size(minmaxlst)
      write (str2, *) minmaxlst(j)
      nspaces = index( str2, ' ', back=.true. ) - 4
      str = str // str_blanks(1:nspaces) // 'last' // trim(adjustl(str2))
   end do

   str = str // '  naam'

   ! write header to file
   write(iunout,'(a)') str

   ! deallocate
   if (allocated(str)) deallocate(str)

end subroutine write_extremes_header


subroutine write_extremes_stat( rdata, ib, ie, filename, stationsnames, stats_index, minmaxlst, thresholds)
real, intent(in)             :: rdata(:,:)
integer, intent(in)          :: ib, ie, stats_index(:)
character(len=*), intent(in) :: filename, stationsnames(:)
real, optional, intent(in)   :: thresholds(:)      ! array with threshold values (optional)
integer, intent(in)          :: minmaxlst(:)       ! integer array for the extreme quantities
!
! locals
!
integer           :: i          ! loop counter
integer           :: iunout
!
!==============================================================================
   open (newunit=iunout, file=filename)

   call write_extremes_header ( iunout, minmaxlst )

   ! loop over stations
   do i = ib, ie
      ! for each station
      if (present(thresholds)) then
         call write_extremes_statloop( rdata(:,i), i, iunout, trim(stationsnames(stats_index(i))), &
                                      minmaxlst, thresholds(stats_index(i)) )
      else
         call write_extremes_statloop( rdata(:,i), i, iunout, trim(stationsnames(stats_index(i))), &
                                      minmaxlst )
      endif

   enddo

   close (iunout)

end subroutine write_extremes_stat

end module write_extremes_his
