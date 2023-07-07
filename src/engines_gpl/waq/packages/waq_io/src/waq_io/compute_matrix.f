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

      subroutine compute_matrix ( lunut , data_param, data_loc, waq_param, waq_loc,
     +                            amiss , fdata     , wdata   )

!     Deltares Software Centre

!     function : assign matrix according to computational
!                rules


!     Global declarations

      use dlwq_data      ! for definition and storage of data
      use timers         ! performance timers

      implicit none

!     declaration of arguments

      integer               , intent(in)    :: lunut        ! report file
      type(t_dlwq_item)     , intent(in)    :: data_param   ! list of param items in the data
      type(t_dlwq_item)     , intent(in)    :: data_loc     ! list of loc items in the data
      type(t_dlwq_item)     , intent(in)    :: waq_param    ! list of waq param items to be set
      type(t_dlwq_item)     , intent(in)    :: waq_loc      ! list of waq loc items to be set
      real                  , intent(in)    :: amiss        ! missing value
      type(t_dlwqdata)      , intent(in)    :: fdata        ! data block input
      type(t_dlwqdata)      , intent(out)   :: wdata        ! data block output

!     local declarations

      integer                               :: iorder       ! order of the parameters and locations in the data array
      integer                               :: functype     ! function type
      integer                               :: nobrk        ! number of breakpoints
      integer                               :: ndim1        ! first dimension of values
      integer                               :: ndim2        ! second dimension of values
      integer                               :: ibrk         ! loop counter breakpoints
      integer                               :: iloc         ! loop counter locations
      integer                               :: ipar         ! loop counter parameters
      integer                               :: ipar_out     ! index output parameter
      integer                               :: ip           ! assignment rule and indx pointer for parameter
      integer                               :: ip2          ! index pointer for parameter in data array
      integer                               :: iparo        ! parameter which is subject to current min or max
      logical                               :: miniem       ! is minimum to be applied
      logical                               :: maxiem       ! is maximum to be applied
      logical                               :: close_accum  ! is calculation ready for this output parameter
      real                                  :: accum        ! accumulation of calculation
      real                                  :: aminv        ! minimum value to be applied
      real                                  :: amaxv        ! maximum value to be applied
      integer(4) :: ithndl = 0
      if (timon) call timstrt( "compute_matrix", ithndl )

      ! some initialisation

      iorder   = wdata%iorder
      functype = wdata%functype
      miniem = .false.
      maxiem = .false.
      ndim1  = wdata%no_param
      ndim2  = wdata%no_loc
      nobrk  = fdata%no_brk
      allocate(wdata%times(nobrk),wdata%values(ndim1,ndim2,nobrk))
      wdata%no_brk = nobrk
      wdata%times  = fdata%times

      ! assignment loop

      breakpoint_loop: do ibrk = 1 , nobrk

         ! if harmonics then deal with the phase

         if ( functype .eq. FUNCTYPE_HARMONIC .or.
     +        functype .eq. FUNCTYPE_FOURIER       ) then
!           phase = phase(ibrk)
         endif

         location_loop: do iloc = 1 , data_loc%no_item

            ipar_out = 1
            accum    = 0.0
            parameter_loop: do ipar = 1 , data_param%no_item

               ip   = data_param%ipnt(ipar)
               ip2  = data_param%sequence(ipar)

               ! normal processing

               if ( ip .gt. -900000 ) then
                  if ( ip .gt. 0 ) then
                     accum = fdata%values(ip2,iloc,ibrk)
                  else
                     accum = data_param%constant(ipar)
                  endif
                  wdata%values(ipar_out,iloc,ibrk) = 0.0
                  wdata%values(ipar_out,iloc,ibrk) = 0.0
                  iparo = ip
               endif

               ! ignore value

               if ( ip .le. -1300000000 ) then
                  ip = 0
               endif

               ! a maximum value need to be applied

               if ( ip .le. -1190000000 ) then
                  ip = ip +  1200000000
                  if ( wdata%values(ipar_out,iloc,ibrk) .ne. amiss .and. accum .ne. amiss ) then
                     wdata%values(ipar_out,iloc,ibrk) = wdata%values(ipar_out,iloc,ibrk) + accum
                  else
                     wdata%values(ipar_out,iloc,ibrk) = amiss
                  endif
                  accum = 0.0
                  maxiem = .true.
                  if ( ip .eq. 0 ) then
                     amaxv = fdata%values(ip2,iloc,ibrk)
                  endif
                  if ( ip .lt. 0 ) amaxv = data_param%constant(ipar)
                  if ( ip .gt. 0 ) amaxv = wdata%values(ip,iloc,ibrk)
               endif

               ! a minimum value need to be applied

               if ( ip .le. -1090000000 ) then
                  ip = ip +  1100000000
                  if ( wdata%values(ipar_out,iloc,ibrk) .ne. amiss .and. accum .ne. amiss ) then
                     wdata%values(ipar_out,iloc,ibrk) = wdata%values(ipar_out,iloc,ibrk) + accum
                  else
                     wdata%values(ipar_out,iloc,ibrk) = amiss
                  endif
                  accum = 0.0
                  miniem = .true.
                  if ( ip .eq. 0 ) then
                     aminv = fdata%values(ip2,iloc,ibrk)
                  endif
                  if ( ip .lt. 0 ) aminv = data_param%constant(ipar)
                  if ( ip .gt. 0 ) aminv = wdata%values(ip,iloc,ibrk)
               endif

               ! a minus sign need to be applied

               if ( ip .le. -900000000 ) then
                  ip = ip + 1000000000
                  if ( wdata%values(ipar_out,iloc,ibrk) .ne. amiss .and. accum .ne. amiss ) then
                     wdata%values(ipar_out,iloc,ibrk) = wdata%values(ipar_out,iloc,ibrk) + accum
                  else
                     wdata%values(ipar_out,iloc,ibrk) = amiss
                  endif
                  if ( ip .eq. 0 ) then
                     accum = -fdata%values(ip2,iloc,ibrk)
                  endif
                  if ( ip .lt. 0 ) accum = -data_param%constant(ipar)
                  if ( ip .gt. 0 ) accum = -wdata%values(ip,iloc,ibrk)
                  if ( accum .eq. -amiss ) accum = amiss
               endif

               ! a plus sign need to be applied

               if ( ip .le. -90000000  ) then
                  ip = ip + 100000000
                  if ( wdata%values(ipar_out,iloc,ibrk) .ne. amiss .and. accum .ne. amiss ) then
                     wdata%values(ipar_out,iloc,ibrk) = wdata%values(ipar_out,iloc,ibrk) + accum
                  else
                     wdata%values(ipar_out,iloc,ibrk) = amiss
                  endif
                  if ( ip .eq. 0 ) then
                     accum =  fdata%values(ip2,iloc,ibrk)
                  endif
                  if ( ip .lt. 0 ) accum =  data_param%constant(ipar)
                  if ( ip .gt. 0 ) accum =  wdata%values(ip,iloc,ibrk)
               endif

               ! a division need to be applied

               if ( ip .le. -9000000   ) then
                  ip = ip + 10000000
                  if ( ip .eq. 0 ) then
                     if ( fdata%values(ipar,iloc,ibrk) .ne. amiss .and. accum .ne. amiss ) then
                        accum = accum / fdata%values(ip2,iloc,ibrk)
                     else
                        accum = amiss
                     endif
                  endif
                  if ( ip .lt. 0 ) then
                     if ( data_param%constant(ipar) .ne. amiss .and. accum .ne. amiss ) then
                        accum = accum / data_param%constant(ipar)
                     else
                        accum = amiss
                     endif
                  endif
                  if ( ip .gt. 0 ) then
                     if ( wdata%values(ip,iloc,ibrk) .ne. amiss .and. accum .ne. amiss ) then
                        accum = accum / wdata%values(ip,iloc,ibrk)
                     else
                        accum = amiss
                     endif
                  endif
               endif

               ! a multiplication need to be applied

               if ( ip .le. -900000    ) then
                  ip = ip + 1000000
                  if ( ip .eq. 0 ) then
                     if ( fdata%values(ipar,iloc,ibrk) .ne. amiss .and. accum .ne. amiss ) then
                        accum = accum * fdata%values(ip2,iloc,ibrk)
                     else
                        accum = amiss
                     endif
                  endif
                  if ( ip .lt. 0 ) then
                     if ( data_param%constant(ipar) .ne. amiss .and. accum .ne. amiss ) then
                        accum = accum * data_param%constant(ipar)
                     else
                        accum = amiss
                     endif
                  endif
                  if ( ip .gt. 0 ) then
                     if ( wdata%values(ip,iloc,ibrk) .ne. amiss .and. accum .ne. amiss ) then
                        accum = accum * wdata%values(ip,iloc,ibrk)
                     else
                        accum = amiss
                     endif
                  endif
               endif

               ! close accumulation if last parameter or next one is normal

               if ( ipar .eq. data_param%no_item ) then
                  close_accum = .true.
               else
                  if ( data_param%ipnt(ipar+1) .gt. -900000 ) then
                     close_accum = .true.
                  else
                     close_accum = .false.
                  endif
               endif

               if ( close_accum ) then
                  if ( wdata%values(ipar_out,iloc,ibrk) .ne. amiss .and. accum .ne. amiss ) then
                     wdata%values(ipar_out,iloc,ibrk) = wdata%values(ipar_out,iloc,ibrk) + accum
                  else
                     wdata%values(ipar_out,iloc,ibrk) = amiss
                  endif
                  if ( maxiem ) then
                     if ( wdata%values(ipar_out,iloc,ibrk) .gt. amaxv .and.
     *                    wdata%values(ipar_out,iloc,ibrk) .ne. amiss       ) then
                        write ( lunut , 1000 ) ibrk, iparo, iloc
                        write ( lunut , 1010 ) wdata%values(ipar_out,iloc,ibrk), amaxv
                        wdata%values(ipar_out,iloc,ibrk) = amaxv
                     endif
                  endif
                  if ( miniem ) then
                     if ( wdata%values(ipar_out,iloc,ibrk) .lt. aminv .and.
     *                    wdata%values(ipar_out,iloc,ibrk) .ne. amiss       ) then
                        write ( lunut , 1000 ) ibrk, iparo, iloc
                        write ( lunut , 1020 ) wdata%values(ipar_out,iloc,ibrk), aminv
                        wdata%values(ipar_out,iloc,ibrk) = aminv
                     endif
                  endif
                  ipar_out = ipar_out + 1
                  maxiem = .false.
                  miniem = .false.
                  accum = 0.0
               endif

            enddo parameter_loop

         enddo location_loop

      enddo breakpoint_loop

      if (timon) call timstop( ithndl )
      return
!
 1000 format ( ' warning: processing breakpoint',i6,' for substance',i3,
     *         ' at station',i5)
 1010 format ( ' the value of ',e15.6,' is overwritten by the maximum ',
     *         ' of ',e15.6,' !' )
 1020 format ( ' the value of ',e15.6,' is overwritten by the minimum ',
     *         ' of ',e15.6,' !' )
!
      end
