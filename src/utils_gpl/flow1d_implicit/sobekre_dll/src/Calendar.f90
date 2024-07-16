!
! Calendar module
!
! Conversions to and from Julian dates and find day_of_the_week.
!
! Author: Johan Ansink, HKV lijn in water
!
!--------------------------------------------------------------
Module Calendar

  implicit none

contains
!--------------------------------------------------------------
!
! Return the day of the week 
!
! Input
!   julian :: Integer, Optional
!             If present the julian day for which the weekday is
!             required, if absent the day of the week TODAY is
!             returned
! Output
!   weekday :: Integer, optional
!             The day of the week, 0 = sunday
!   day :: Character*(*), optional
!             The name of the day of the week, e.g. 'Sunday'
!             Minimum length = 9
!   ierr :: Integer
!             Error return, 0=correct
!                          -1=invalid Julian day
!                          -2=neither day nor weekday specified
!
  subroutine day_of_week(julian, weekday, day, ierr)

  implicit none
  integer,intent(in),optional  :: julian
  integer,intent(out),optional :: weekday
  integer, intent(out)         :: ierr
  integer                      :: iweekday
  character*(*),intent(out),optional:: day

  ierr = 0
  if(present(julian)) then   
    if(julian < 0) then
      ierr = -1
      return
    endif
    iweekday = mod(julian+1, 7)
  else
    iweekday = date_to_julian(ierr=ierr)
    if(ierr/=0) return
  endif

  if(.not.present(day).and. .not.present(weekday)) then
    ierr=-2
    return
  endif

  if(present(day)) then
    if(iweekday.eq.0) then
      day = 'Sunday'
    else if(iweekday==1) then
      day = 'Monday'
    else if(iweekday==2) then
      day = 'Tuesday'
    else if(iweekday==3) then
      day = 'Wednesday'
    else if(iweekday==4) then
      day = 'Thursday'
    else if(iweekday==5) then
      day = 'Friday'
    else
      day = 'Saturday'
    endif
  endif
  if(present(weekday)) weekday=iweekday

  end subroutine day_of_week
!-------------------------------------------------------------------------
!
! Convert a Julian day to a day/month/year
!
!   Input
!     julian :: Integer
!               The julian day to convert
!   Output
!     day   :: Integer, optional
!               The day of the month
!     month :: Integer, optional
!               The day of the month
!     year  :: Integer, optional
!               The day of the month
!     values :: Integer(:),optional
!               Array, minimum size=3, values(1)=year
!                                      values(2)=month
!                                      values(3)=day
!     ierr :: Integer
!               Error return, 0=correct
!                            -1=invalid Julian day
!                            -2=no output specified
!
  subroutine julian_to_date(julian,day,month,year,values,ierr)

  implicit none
  integer,parameter      :: igreg=2299161
  integer,parameter      :: k = kind(0.0d0)
  integer,intent(in)     :: julian
  integer,intent(out),optional    :: day, month, year
  integer,intent(out),optional    :: values(:)
  integer,intent(out)             :: ierr
  integer                :: ia, ja, jb, jc, jd, je, iday, imonth, iyear
  real(kind=k)           :: xc

  if(julian < 0) then
    ierr = -1
    return
  else
    ierr = 0
  endif
  if(.not.present(values).and..not.present(day).and. &
     .not.present(month).and..not.present(year)) then
    ierr=-2
    return
  endif

  if (julian.ge.igreg) then
    ia = (real(julian-1867216,k)-0.25d0)/36524.25d0
    ja = julian + 1+ia-int(0.25*ia)
  else
    ja = julian
  end if

  jb = ja + 1524
  xc = (real(jb-2439870,k)-122.1d0)/365.25d0
  jc = 6680.0d0 + xc
  jd = 365*jc + int(0.25d0*real(jc,k))
  je = int(real(jb-jd,k)/30.6001d0)

  iday = jb - jd - int(30.6001d0*real(je,k))

  imonth = je - 1
  if (imonth.gt.12) imonth = imonth - 12

  iyear = jc - 4715
  if (imonth.gt.2) iyear = iyear - 1
  if (iyear.le.0) iyear = iyear - 1
!
! Assign output values
!
  if(present(values)) then
    values(1) = iyear
    values(2) = imonth
    values(3) = iday
  endif
  if(present(year)) year=iyear
  if(present(month)) month=imonth
  if(present(day)) day=iday

  end subroutine julian_to_date
!-----------------------------------------------------------
!
! Convert a day/month/year to a Julian day 
!
!  If VALUES and one of DAY, MONTH and YEAR are missing this
!  will return the Julian day for TODAY
!
!   Input
!     day   :: Integer, optional
!               The day of the month
!     month :: Integer, optional
!               The day of the month
!     year  :: Integer, optional
!               The day of the month
!     values :: Integer(:),optional
!               Array, minimum size=3, values(1)=year
!                                      values(2)=month
!                                      values(3)=day
!   Output
!     julian :: Integer, Function Result
!               The julian day to convert
!     ierr :: Integer
!               Error return, 0=correct
!                            -1=invalid year
!                            -2=invalid month
!                            -3=invalid day
!                            -4=invalid date (29th Feb, non leap-year)
!
  function date_to_julian(day, month, year, values, ierr) result(julian)
!
!  gregorian started midday oct 15 1582
!
  implicit none
  integer,parameter :: igreg=15+31* (10+(12*1582))
  integer,parameter :: limit(12) = &
                       (/31,29,31,30,31,30,31,31,30,31,30,31/)
  real(kind=kind(0.0d0))::xi
  integer,intent(in),optional :: day, month, year
  integer,intent(in),optional :: values(:)
  integer:: julian, v(9)

  integer :: iy, ja, jm, jy, ierr
  integer :: iday,imonth,iyear

  if(present(values)) then
    iyear = values(1)
    imonth = values(2)
    iday = values(3)      
  else if(present(day).and.present(month).and.present(year)) then
    iyear = year ; imonth = month ; iday = day
  else
    call date_and_time(values=v)
    iyear = v(1)
    imonth = v(2)
    iday = v(3)      
  endif

  if (iyear==0) then
    ierr=-1
    return
  endif
  if (imonth <= 0 .or. imonth > 12) then
    ierr=-2
    return
  endif
  if (iday > limit(imonth)) then
    ierr=-3
    return
  endif
  if(imonth==2.and.iday==29.and.iyear>1582)then
    if(mod(iyear,4)/=0) then
      ierr=-4
      return
    endif
  endif
  iy = iyear
  if (iyear.lt.0) iy = iy + 1
  if (imonth.gt.2) then
    jy = iy
    jm = imonth + 1
  else
    jy = iy - 1
    jm = imonth + 13
  end if

  julian = iday + int(30.6001d0*real(jm))
  if (jy.lt.0) then
    xi = 365.25d0*real(jy)
    if(int(xi).ne.xi)xi = xi -1
    julian = julian + int(xi)
  else
    julian = julian + 365*jy
    julian = julian + int(0.25*real(jy))
  end if
  julian = julian + 1720995

  if (iday+ (31* (imonth + (12*iy))) >= igreg) then
    ja=jy/100
    julian = julian - ja
    julian = julian + 2
    julian = julian + ja/4
  end if

  end function date_to_julian

end module Calendar

