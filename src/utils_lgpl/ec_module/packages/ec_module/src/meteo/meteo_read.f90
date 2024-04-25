module meteo_read
!----- LGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2011-2024.
!
!  This library is free software; you can redistribute it and/or
!  modify it under the terms of the GNU Lesser General Public
!  License as published by the Free Software Foundation version 2.1.
!
!  This library is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
!  Lesser General Public License for more details.
!
!  You should have received a copy of the GNU Lesser General Public
!  License along with this library; if not, see <http://www.gnu.org/licenses/>.
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
! see meteo.f90
!
!!--pseudo code and references--------------------------------------------------
!
! Stef.Hummel@deltares.nl
! Herman.Kernkamp@deltares.nl
! Adri.Mourits@deltares.nl
!
!!--declarations----------------------------------------------------------------
   use precision
   use meteo_data
   use time_module
   use string_module

   implicit none

contains


function readtime(minp, meteoitem, flow_itdate, flow_tzone, tread) result(success)
   !
   ! read the new time belonging to the meteo input field
   !
   implicit none
   !
   integer                 , intent(in)  :: flow_itdate
   integer                 , intent(in)  :: minp
   real(fp)                , intent(in)  :: flow_tzone
   real(fp)                , intent(out) :: tread
   logical                               :: success
   type(tmeteoitem)                      :: meteoitem
   !
   integer                       :: ierr
   integer                       :: il
   integer                       :: ir
   integer                       :: i_since
   integer                       :: i_unit
   integer                       :: flow_julday
   integer                       :: meteo_julday
   integer                       :: day
   integer                       :: hrs
   integer                       :: meteo_itdate
   integer                       :: min
   integer                       :: month
   integer                       :: sec
   integer                       :: time_zone_hrs
   integer                       :: time_zone_min
   integer                       :: year
   real(fp)                      :: day_diff
   real(fp)                      :: min_diff
   real(fp)                      :: meteo_tzone
   real(fp)                      :: time_conv
   real(fp)                      :: tzone_diff
   character(len=1)              :: sign_time_zone
   character(len=:), allocatable :: rec
   character(len=:), allocatable :: time_definition
   character(len=256)            :: iomsg
   !
   if (meteoitem%filetype == uniuvp) then
      success = .true.
      return
   endif
   !
   time_definition = ' '
   do
      rec = ' '
      call GetLine(minp, rec, ierr, iomsg)
      if (ierr /= 0) then
         meteomessage = 'Meteo input: Premature end of file; expecting data at additional time'
         success = .false.
         return
      endif
      il = index(rec, '=') + 1
      ir = index(rec, '#') - 1
      if (ir == -1) then
         ir = len(rec)
      endif
      if (il == 1) then
         !
         ! No equal sign found in line
         !
         if ( rec(1:ir) ==  ' ' ) then
            !
            ! Empty line or line contains only commentary
            !
            cycle
         else
            !
            ! Line contains entry other than a keyword or commentary
            ! NOT ALLOWED: ERROR
            !
            meteomessage = 'Meteo input: wrong entry in meteofile '//trim(meteoitem%filename)//'; expecting keyword or commentary, but found: '//trim(rec)
            success = .false.
            return
         endif
      endif
      !
      ! Make the keyword case insensitive
      !
      call small(rec,il)
      !
      if ( index(rec(1:il-2), 'time') /= 0 )  then
         time_definition = adjustl(rec(il:ir))
         exit
      else
         cycle
      endif
   enddo
   !
   ! Determine the time_unit from the given time_definition
   !
   time_conv = 1.0_fp
   i_since = index(time_definition, 'since') - 1
   if (i_since < 2) then
      write(meteomessage, '(2a)') 'Meteo input: Expecting "since" in time specification, but getting: ', &
          & trim(time_definition)
      success = .false.
      return
   endif
   read( time_definition(1:i_since),'(a)' , iostat = ierr )  meteoitem%time_unit
   if     ( index(meteoitem%time_unit, ' minutes') /= 0 ) then
      time_conv = 1.0_fp
      i_unit    = i_since - 9
   elseif ( index(meteoitem%time_unit, ' min '   ) /= 0 ) then
      time_conv = 1.0_fp
      i_unit    = i_since - 5
   elseif ( index(meteoitem%time_unit, ' m '     ) /= 0 ) then
      time_conv = 1.0_fp
      i_unit    = i_since - 3
   elseif ( index(meteoitem%time_unit, ' hours ' ) /= 0 ) then
      time_conv = 60.0_fp
      i_unit    = i_since - 7
   elseif ( index(meteoitem%time_unit, ' hrs '   ) /= 0 ) then
      time_conv = 60.0_fp
      i_unit    = i_since - 5
   elseif ( index(meteoitem%time_unit, ' h '     ) /= 0 ) then
      time_conv = 60.0_fp
      i_unit    = i_since - 3
   else
      write(meteomessage, '(2a)') 'Meteo input: Incorrect time unit given, expecting minutes or hours, but getting ', &
          & trim(meteoitem%time_unit)
      success = .false.
      return
   endif
   !
   ! Determine the time read, reference time and time zone from the given time_definition
   !
   read( time_definition(1:i_unit), *, iostat = ierr ) tread
   !
   read( time_definition(i_since+ 7:i_since+10),'(i4)' , iostat = ierr ) year
   read( time_definition(i_since+12:i_since+13),'(i2)' , iostat = ierr ) month
   read( time_definition(i_since+15:i_since+16),'(i2)' , iostat = ierr ) day
   read( time_definition(i_since+18:i_since+19),'(i2)' , iostat = ierr ) hrs
   read( time_definition(i_since+21:i_since+22),'(i2)' , iostat = ierr ) min
   read( time_definition(i_since+24:i_since+25),'(i2)' , iostat = ierr ) sec
   read( time_definition(i_since+27:i_since+27), *     , iostat = ierr ) sign_time_zone
   read( time_definition(i_since+27:i_since+29),'(i3)' , iostat = ierr ) time_zone_hrs
   read( time_definition(i_since+31:i_since+32),'(i2)' , iostat = ierr ) time_zone_min
   !
   if (sign_time_zone == '+' .or. sign_time_zone == '-') then
      !
      ! Appears to be correct time input
      !
   else
      meteomessage = 'Meteo input: Did not find correct reference time and time zone in time definition'
      success = .false.
      return
   endif
   if (ierr /= 0) then
      meteomessage = 'Meteo input: Error encountered while extracting reference time from time definition'
      success = .false.
      return
   endif
   !
   meteo_itdate = year*10000 + month*100 + day
   !
   ! Minutes part of meteo time zone may only be 0 or 30 minutes
   !
   if ((time_zone_min) > 0 .and. (time_zone_min - 30) > 0) then
      write(meteomessage, '(a,i2)') 'Meteo input: Incorrect minutes given in meteo time zone, expecting 0 or 30 minutes, but getting ', &
          & time_zone_min
      success = .false.
      return
   endif
   !
   ! Determine complete meteo time zone in hours
   !
   meteo_tzone  = real(time_zone_hrs, fp) + sign(1.0_fp, real(time_zone_hrs, fp))*real(time_zone_min, fp)/60.0_fp
   !
   ! Compare reference times of FLOW and METEO
   !
   meteo_julday = ymd2jul(meteo_itdate)
   flow_julday = ymd2jul(flow_itdate)
   !
   ! Determine difference between time zones of FLOW and METEO
   !
   tzone_diff = flow_tzone - meteo_tzone
   !
   ! Difference in days
   !
   day_diff = real(flow_julday, fp) - real(meteo_julday, fp)
   !
   ! Difference in minutes, including hours, minutes and seconds from meteo reference time
   ! Include difference in time zone between FLOW and meteo
   !
   min_diff = day_diff*1440.0_fp - real(hrs, fp)*60.0_fp - real(min, fp) - real(sec, fp)/60.0_fp
   min_diff = min_diff + tzone_diff*60.0_fp
   !
   ! Conversion of read time to minutes. If already minutes, time_conv = 1.0_fp
   !
   tread    = tread * time_conv
   !
   ! Adjust tread conform the possible difference in reference times
   ! Time is adjusted to FLOW time zone
   !
   tread    = tread - min_diff
   !
   if ( tzone_diff > 0.01_fp .and. message_count < 1 ) then
      write(meteomessage, '(a,f6.2,a)') 'Shifted meteo input by ', -tzone_diff, ' hours, due to difference in FLOW and meteo timezone'
      message_count = 1
   endif
   !
   ! No errors: all time information succesfully read
   !
   success = .true.
   !
end function readtime


function read_equidistant_block(minp, meteoitem, d, mx, nx) result(success)
   !
   ! read datablock in meteo_on_equidistant_grid file
   !
   implicit none
   !
   integer                      :: minp
   integer                      :: mx
   integer                      :: nx
   real(hp), dimension(:,:)     :: d
   logical                      :: success
   type(tmeteoitem)             :: meteoitem
   !
   integer                :: i
   integer                :: j
   character(16)          :: tex
   !
   if ( size(d,1) .ne. mx .or. size(d,2) .ne. nx ) then
        meteomessage = 'READ_EQUIDISTANT_BLOCK: wrong sizes'
        success = .false.
        return
   endif
   do j = meteoitem%numn, 1, -1
      read(minp,*,end = 100, err=101) (d(i,j), i = 1, meteoitem%numm)
      do i = 1, meteoitem%numm
         if (isnan(d(i,j))) goto 201
      enddo
   enddo
   !
   do i = 1,meteoitem%numm
      do j = 1,meteoitem%numn
         if (d(i,j) == meteoitem%nodata_value) then
            d(i,j) = nodata_default
         else
            !
            ! Conversion of pressure to Pa (N/m2). If already Pa, p_conv = 1.0_hp
            !
            d(i,j) = d(i,j) * meteoitem%p_conv
         endif
      enddo
   enddo
   success = .true.
   return
   !
   ! error handling
   !
100 continue
   meteomessage = 'Unexpected end of file in meteo_on_equidistant_grid file'
   success = .false.
   return
101 continue
   write(tex,'(2i8)') i,j
   write(meteomessage,'(2a)') 'Error reading block in meteo_on_equidistant_grid file in colnr, rownr :', trim(tex)
   success = .false.
   return
201 continue
   write(tex,'(2i8)') i,j
   write(meteomessage,'(2a)') 'NaN found in block in meteo_on_equidistant_grid file in colnr, rownr :', trim(tex)
   success = .false.
   return
end function read_equidistant_block


function read_curvilinear_block(minp, d, meteoitem) result(success)
   !
   ! read field in meteo_on_curvilinear_grid file
   !
   implicit none
   !
   integer                 , intent(in)  :: minp
   real(hp), dimension(:,:), intent(out) :: d
   logical                               :: success
   type(tmeteoitem)                      :: meteoitem
   !
   integer                   :: i
   integer                   :: j
   integer                   :: mincr
   integer                   :: nincr
   character(16)             :: tex
   !
   mincr  = sign(1, meteoitem%mlast-meteoitem%mfirst)
   nincr  = sign(1, meteoitem%nlast-meteoitem%nfirst)
   if (      size(d,1) .ne. max(meteoitem%mfirst,meteoitem%mlast) &
      & .or. size(d,2) .ne. max(meteoitem%nfirst,meteoitem%nlast)  ) then
        meteomessage = 'READ_CURVILINEAR_BLOCK: wrong sizes'
        success = .false.
        return
   endif
   if (meteoitem%mrow) then
      do i = meteoitem%mfirst, meteoitem%mlast, mincr
         read(minp,*,end = 100, err=101) (d(i,j), j = meteoitem%nfirst, meteoitem%nlast, nincr)
         do j = meteoitem%nfirst, meteoitem%nlast, nincr
            if (isnan(d(i,j))) goto 201
         enddo
      enddo
   else
      do j = meteoitem%nfirst, meteoitem%nlast, nincr
         read(minp,*,end = 100, err=101) (d(i,j), i = meteoitem%mfirst, meteoitem%mlast, mincr)
         do i = meteoitem%mfirst, meteoitem%mlast, mincr
            if (isnan(d(i,j))) goto 201
         enddo
      enddo
   endif
   !
   do i = meteoitem%mfirst, meteoitem%mlast, mincr
      do j = meteoitem%nfirst, meteoitem%nlast, nincr
         if (d(i,j) == meteoitem%nodata_value) then
            d(i,j) = nodata_default
         else
            if (meteoitem%quantities(1) == 'patm') then
               !
               ! Conversion of pressure to Pa (N/m2). If already Pa, p_conv = 1.0_hp
               !
               d(i,j) = d(i,j) * meteoitem%p_conv
            endif
         endif
      enddo
   enddo
   success = .true.
   return
   !
   ! error handling
   !
100 continue
   meteomessage = 'Unexpected end of file in meteo_on_curvilinear_grid file'
   success = .false.
   return
101 continue
   write(tex,'(2i8)') i,j
   write(meteomessage,'(2a)') 'Error reading block in meteo_on_curvilinear_grid file in colnr, rownr :', trim(tex)
   success = .false.
   return
201 continue
   write(tex,'(2i8)') i,j
   write(meteomessage,'(2a)') 'NaN found in block in meteo_on_curvilinear_grid file in colnr, rownr :', trim(tex)
   success = .false.
   return
end function read_curvilinear_block


function readseries(minp,d,kx,tread) result(success)
   !
   !  Read uniform time serie
   !  number of columns is number of dimensions
   !
   implicit none
   !
   integer                       :: kx
   integer                       :: minp
   real(hp), dimension(:)        :: d
   real(fp)                      :: tread
   logical                       :: success
   !
   integer                   :: k
   integer                   :: istat
   character(:), allocatable :: rec
   !
   if ( size(d,1) .lt. kx ) then
      meteomessage = 'READSERIES: wrong sizes'
      success = .false.
      return
   endif
10  continue
   call GetLine(minp, rec, istat)
   if (istat /= 0) goto 100
   if (rec(1:1) .eq. '*') goto 10
   read(rec,*,err = 101) tread, ( d(k), k = 1, kx )
   do k = 1, kx
      if (isnan(d(k))) goto 201
   enddo
   success = .true.
   return
100 continue
   meteomessage = 'Unexpected end of file in uniform time serie file'
   success = .false.
   return
101 continue
   write(meteomessage,'(2a)') 'Error reading timeseries : ', trim(rec)
   success = .false.
   return
201 continue
   write(meteomessage,'(2a)') 'NaN found in timeseries : ', trim(rec)
   success = .false.
   return
end function readseries


function read_spv_block(minp, meteoitem, d, mx, nx, kx) result(success)
   !
   ! Read block in meteo_on_computational_grid file
   !
   implicit none
   !
   integer                    :: minp
   integer                    :: kx
   integer                    :: mx
   integer                    :: nx
   real(hp), dimension(:,:,:) :: d
   logical                    :: success
   type(tmeteoitem)           :: meteoitem
   !
   integer                    :: i
   integer                    :: j
   integer                    :: k
   !
   if ( size(d,1) .ne. mx .or. size(d,2) .ne. nx .or. size(d,3) .ne. kx ) then
      meteomessage = 'READ_SPV_BLOCK: wrong sizes'
      success = .false.
      return
   endif
   !
   ! Loop over the third dimension kx
   !
   do k = 1, kx
      do j = 1, mx
         read(minp,*,end = 100, err=101) ( d(j,i,k), i = 1, nx )
         do i = 1, nx
            if (isnan(d(j,i,k))) goto 201
         enddo
      enddo
   enddo
   !
   success = .true.
   return
100 continue
   write(meteomessage,'(2a)') 'Unexpected end of file in field_on_computational_grid file ', trim(meteoitem%FILENAME)
   success = .false.
   return
101 continue
   write(meteomessage,'(3a,i0)') 'Error reading ', trim(meteoitem%FILENAME), 'for quanitity', k
   success = .false.
   return
201 continue
   write(meteomessage,'(3a,i0)') 'NaN found in ', trim(meteoitem%FILENAME), 'for quanitity', k
   success = .false.
   return
end function read_spv_block


function read_spiderweb_block(minp, d, mx, nx, meteoitem, x_spw_eye, y_spw_eye, all_nodata) result(success)
   !
   ! Read spiderweb field including the location of the cyclone/spiderweb eye and the pressure drop there
   !
   implicit none
   !
   integer                    :: minp
   integer                    :: mx
   integer                    :: nx
   real(hp), dimension(:,:,:) :: d
   real(fp)                   :: x_spw_eye
   real(fp)                   :: y_spw_eye
   logical                    :: all_nodata
   logical                    :: success
   type(tmeteoitem)           :: meteoitem
   !
   integer                    :: i
   integer                    :: il
   integer                    :: ir
   integer                    :: iread
   integer                    :: j
   integer                    :: istat
   real(fp)                   :: p_drop_spw_eye
   character(:), allocatable  :: rec
   !
   if ( size(d,1) .ne. mx .or. size(d,2) .ne. nx ) then
      meteomessage = 'READ_SPIDERWEB_BLOCK: wrong sizes'
      success = .false.
      return
   endif
   !
   p_drop_spw_eye = 0.0_fp
   !
   do iread = 1,3
      call GetLine(minp, rec, istat)
      if (istat /= 0) goto 100
      il = index(rec, '=') + 1
      ir = index(rec, '#') - 1
      if (ir == -1) then
         ir = len(rec)
      endif
      if     ( index(rec(1:il-2), 'x_spw_eye'      ) /=0)  then
         read( rec(il:ir), *, err=101 )                 x_spw_eye
      elseif ( index(rec(1:il-2), 'y_spw_eye'      ) /=0)  then
         read( rec(il:ir), *, err=101 )                 y_spw_eye
      elseif ( index(rec(1:il-2), 'p_drop_spw_eye' ) /=0)  then
         read( rec(il:ir), *, err=101 )                 p_drop_spw_eye
      endif
   enddo
   !
   ! Wind speed
   !
   do j = 2, nx
      read(minp,*,end = 100, err=201) ( d(i,j,1), i = 1, mx-1 )
      do i = 1, mx-1
        if (isnan(d(i,j,1))) goto 301
      enddo
   enddo
   !
   ! Wind direction
   !
   do j = 2, nx
      read(minp,*,end = 100, err=202) ( d(i,j,2), i = 1, mx-1 )
      do i = 1, mx-1
        if (isnan(d(i,j,2))) goto 302
      enddo
   enddo
   !
   ! Pressure
   !
   do j = 2, nx
      read(minp,*,end = 100, err=203) ( d(i,j,3), i = 1, mx-1 )
      do i = 1, mx-1
        if (isnan(d(i,j,3))) goto 303
      enddo
   enddo
   do i = 1, mx-1
      d(i,1,1) = 0
      d(i,1,2) = 0
      !
      ! Fill central point
      !
      d(i,1,3) = p_drop_spw_eye
   enddo
   !
   ! Conversion of pressure to Pa (N/m2). If already Pa, p_conv = 1.0_hp
   !
   all_nodata = .true.
   do j = 2, nx
      do i = 1, mx-1
         if (d(i,j,1) == meteoitem%nodata_value) then
            d(i,j,1) = nodata_default
         else
            all_nodata = .false.
         endif
         if (d(i,j,2) == meteoitem%nodata_value) then
            d(i,j,2) = nodata_default
         else
            all_nodata = .false.
         endif
         if (d(i,j,3) == meteoitem%nodata_value) then
            d(i,j,3) = nodata_default
         else
            all_nodata = .false.
            d(i,j,3) = d(i,j,3) * meteoitem%p_conv
         endif
      enddo
   enddo
   !
   do j = 1, nx
      !
      ! Fill 360 degrees
      !
      d(mx,j,1) = d(1,j,1)
      d(mx,j,2) = d(1,j,2)
      d(mx,j,3) = d(1,j,3)
   enddo
   !
   if (all_nodata) then
      if (x_spw_eye == meteoitem%nodata_value .or. y_spw_eye == meteoitem%nodata_value) then
         x_spw_eye = nodata_default
         y_spw_eye = nodata_default
      endif
      if (p_drop_spw_eye == meteoitem%nodata_value) then
         p_drop_spw_eye = nodata_default
      endif
   elseif (x_spw_eye == meteoitem%nodata_value .or. &
           y_spw_eye == meteoitem%nodata_value .or. &
           p_drop_spw_eye == meteoitem%nodata_value) then
      meteomessage = 'Missing x_spw_eye, y_spw_eye, or p_drop_spw_eye not allowed'
      success = .false.
      return
   endif
   success = .true.
   return
100 continue
   meteomessage = 'Unexpected end of file in meteo_on_spiderweb_grid file'
   success = .false.
   return
101 continue
   write(meteomessage,'(2a)') 'Error reading x_spw_eye, y_spw_eye, p_drop_spw_eye of cyclone eye : ', trim(rec)
   success = .false.
   return
201 continue
   meteomessage = 'Error reading cyclone wind-u field'
   success = .false.
   return
202 continue
   meteomessage = 'Error reading cyclone wind-v field'
   success = .false.
   return
203 continue
   meteomessage = 'Error reading cyclone pressure drop field'
   success = .false.
   return
301 continue
   meteomessage = 'NaN found in cyclone wind-u field'
   success = .false.
   return
302 continue
   meteomessage = 'NaN found in cyclone wind-v field'
   success = .false.
   return
303 continue
   meteomessage = 'NaN found in cyclone pressure drop field'
   success = .false.
   return
end function read_spiderweb_block

end module meteo_read
