!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2024.
!
!  This file is part of Delft3D (D-Flow Flexible Mesh component).
!
!  Delft3D is free software: you can redistribute it and/or modify
!  it under the terms of the GNU Affero General Public License as
!  published by the Free Software Foundation version 3.
!
!  Delft3D  is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!  GNU Affero General Public License for more details.
!
!  You should have received a copy of the GNU Affero General Public License
!  along with Delft3D.  If not, see <http://www.gnu.org/licenses/>.
!
!  contact: delft3d.support@deltares.nl
!  Stichting Deltares
!  P.O. Box 177
!  2600 MH Delft, The Netherlands
!
!  All indications and logos of, and references to, "Delft3D",
!  "D-Flow Flexible Mesh" and "Deltares" are registered trademarks of Stichting
!  Deltares, and remain the property of Stichting Deltares. All rights reserved.
!
!-------------------------------------------------------------------------------

! 
! 

!> DFM_MAX25_GETDATA - Subroutine to provide max25 filters to dfmoutput.
!!
module dfm_max25_getdata
   use netcdf
   use write_extremes_his
   use read_nc_histories
   use runsum
   implicit none
   private
   public :: fmgetdata, fmgetdata_running_mean

   contains

!> main routine to write max_running_mean output to file
subroutine fmgetdata_running_mean(filename, filename_out, field_name, minmaxlst)
   implicit none
   character(len=*) , intent(in) :: filename      !< input filename (NetCDF)
   character(len=*) , intent(in) :: field_name    !< input field name (e.g. 'waterlevel')
   character(len=*) , intent(in) :: filename_out  !< output filename (ascii)
   character(len=*),  intent(in) :: minmaxlst     !< list with filter lengths (e.g. '13,25')

   type(TRunSum) :: runsum

   integer :: ierr, i, j, k, nStations, nd, ntimes, iunit
   real, allocatable :: hisdata(:,:), maxvalues(:), minvalues(:)
   real(kind=8), allocatable :: onetime(:)
   character(len=:), allocatable :: stations(:)
   integer, allocatable :: list(:)
   character(len=32) :: stations_var
   character(len=12) :: cnum1, cnum2

                           ierr = read_meta_data(filename, nStations)
   if (ierr == nf90_noerr) call find_stations_var(field_name, stations_var, nStations)
   if (ierr == nf90_noerr) ierr = read_station_names(stations, stations_var)
   if (ierr == nf90_noerr) ierr = read_data(hisdata, field_name)
   if (ierr == nf90_noerr) ierr = close_nc_his_file()

   if (ierr /= nf90_noerr) then
      write(*,*) trim(nf90_strerror(ierr))
   else
      open(newunit=iunit, file=filename_out)
      call parse_min_max_list(minmaxlst, list)
      allocate(maxvalues(nStations), minvalues(nStations), onetime(nStations))
      write(iunit,'(2a)') 'quantity = ', field_name
      do k = 1, size(list)

         nd = list(k)
         ntimes = size(hisdata,1)
         call runsum%init(nStations, nd)
         if (nd > ntimes) then
            write(iunit,'(a,i4)') 'Not enough times for filter width =', nd
            cycle
         end if
         do i = 1, ntimes
            onetime = hisdata(i,:)
            call runsum%update(onetime)
            if (i == nd) then
               maxvalues = runsum%state
               minvalues = runsum%state
            else if (i > nd) then
               do j = 1, nStations
                  maxvalues(j) = max(maxvalues(j), runsum%state(j))
                  minvalues(j) = min(minvalues(j), runsum%state(j))
               end do
            end if
         end do

         ! sum -> mean
         maxvalues = maxvalues / real(nd)
         minvalues = minvalues / real(nd)

         ! print values
         write(iunit,*)
         write(iunit,'(a,i4)') 'width =', nd
         write(iunit,'(a)') '   max value    min value   station name'
         do j = 1, nStations
            call write_val2string(maxvalues(j), cnum1, 1)
            call write_val2string(minvalues(j), cnum2, 1)
            write(iunit,'(2(a12,x),2x,a)') cnum1, cnum2, trim(stations(j))
         end do
      end do
      close(iunit)
   endif

end subroutine fmgetdata_running_mean

!> main routine to write max25 output to file
subroutine fmgetdata(filename, filename_out, field_name, minmaxlst)
   implicit none
   character(len=*) , intent(in) :: filename      !< input filename (NetCDF)
   character(len=*) , intent(in) :: field_name    !< input field name (e.g. 'waterlevel')
   character(len=*) , intent(in) :: filename_out  !< output filename (ascii)
   character(len=*),  intent(in) :: minmaxlst     !< list with filter lengths (e.g. '13,25')

   integer :: ierr, i, nStations
   real, allocatable :: hisdata(:,:)
   character(len=:), allocatable :: stations(:)
   integer, allocatable :: stats_index(:), list(:)
   character(len=32) :: stations_var

                           ierr = read_meta_data(filename, nStations)
   if (ierr == nf90_noerr) call find_stations_var(field_name, stations_var, nStations)
   if (ierr == nf90_noerr) ierr = read_station_names(stations, stations_var)
   if (ierr == nf90_noerr) ierr = read_data(hisdata, field_name)
   if (ierr == nf90_noerr) ierr = close_nc_his_file()

   if (ierr /= nf90_noerr) then
      write(*,*) trim(nf90_strerror(ierr))
   else
      allocate(stats_index(nStations))
      do i = 1, nStations
         stats_index(i) = i
      enddo
      call parse_min_max_list(minmaxlst, list)
      call write_extremes_stat( hisdata, 1, size(stations), filename_out, stations, stats_index, list)
   endif

end subroutine fmgetdata

!> convert comma seperated values in a string to an array of integers
subroutine parse_min_max_list(list_string, list)
   character(len=*),     intent(in)  :: list_string  !< string with filter lengths (e.g. '13,25')
   integer, allocatable, intent(out) :: list(:)      !< list with filter lengths

   integer :: size, i

   size = 1
   do i = 1, len(list_string)
      if (list_string(i:i) == ',') size = size + 1
   enddo
   allocate(list(size))
   read(list_string,*) list
end subroutine parse_min_max_list

end module dfm_max25_getdata
