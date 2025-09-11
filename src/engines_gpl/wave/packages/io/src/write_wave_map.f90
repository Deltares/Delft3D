subroutine write_wave_map(sg, sof, sif, n_swan_grids, wavedata, casl, prevtime, gamma0, output_ice)
!----- GPL ---------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2011-2025.
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
! NONE
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
   use wave_data
   use swan_flow_grid_maps
   !
   implicit none
!
! Local parameters
!
   integer, parameter :: nelmx = 30
!
! Global variables
!
   type(grid) :: sg ! swan grid
   type(output_fields) :: sof ! output fields defined on swan grid
   type(input_fields) :: sif ! input fields defined on swan grid
   integer, intent(in) :: n_swan_grids ! number of swan grids
   type(wave_data_type) :: wavedata
   character(*), intent(in) :: casl ! runid
   logical :: prevtime ! true: the time to be written is the "previous time"
   real, intent(in) :: gamma0 ! JONSWAP peak enhancement factor
   integer, intent(in) :: output_ice ! switch for writing ice quantities
!
! Local variables
!
   integer :: celidt
   integer :: error
   integer :: i
   integer :: ind
   integer :: idum
   integer :: ierror
   integer :: j
   integer :: nelems
   integer :: nelems2
   integer, dimension(6, nelmx) :: elmdms
   integer, dimension(nelmx) :: nbytsg
   integer, dimension(:, :), allocatable :: elmdms2
   integer, dimension(:), allocatable :: nbytsg2
   logical :: wrswch
   real :: xdum
   character(10), dimension(nelmx) :: elmunt
   character(16), dimension(3) :: grpnam
   character(16), dimension(nelmx) :: elmnms
   character(16), dimension(nelmx) :: elmqty
   character(16), dimension(nelmx) :: elmtps
   character(256) :: filnam
   character(64), dimension(nelmx) :: elmdes
   !
   character(10), dimension(:), allocatable :: elmunt2
   character(16), dimension(:), allocatable :: elmnms2
   character(16), dimension(:), allocatable :: elmqty2
   character(16), dimension(:), allocatable :: elmtps2
   character(64), dimension(:), allocatable :: elmdes2
   character(256) :: gridnam
   integer :: it02
   integer :: itlen
   !
   integer, dimension(1) :: idummy ! Help array to read/write Nefis files
   real(hp) :: perfac
   real, dimension(:, :), allocatable :: rbuf
   !
   !     Define data structure; element dimensions are required only
   !     in write-mode.
   !
   data grpnam/'map-series', 'map-series-add', ' '/
   data elmnms/'TIME', 'CODE ', 'HSIGN ', 'DIR ', 'PDIR ', 'PERIOD', 'RTP ', &
       & 'DEPTH', 'VELOC-X', 'VELOC-Y', 'TRANSP-X', 'TRANSP-Y', 'DSPR',       &
       & 'DISSIP ', 'LEAK', 'QB', 'XP', 'YP ', 'UBOT', 'STEEPW', 'WLENGTH',   &
       & 'TPS', 'TM02', 'TMM10', 'DHSIGN', 'DRTM01', 'SETUP', 'FX', 'FY', 'TP'/
   data elmdes/'time',                                                         &
       & 'code for HISWA output grid                                    ',      &
       & 'significant wave height                                       ',      &
       & 'mean wave direction                                           ',      &
       & 'peak wave direction                                           ',      &
       & 'mean wave period                                              ',      &
       & 'relative peak wave period                                     ',      &
       & 'water depth                                                   ',      &
       & 'x-component current velocity                                  ',      &
       & 'y-component current velocity                                  ',      &
       & 'x-component of energy transport vector                        ',      &
       & 'y-component of energy transport vector                        ',      &
       & 'directional spread of the waves                               ',      &
       & 'energy dissipation                                            ',      &
       & 'leakage of energy over sector boundaries                      ',      &
       & 'the fraction of breaking waves                                ',      &
       & 'x coordinate output grid                                      ',      &
       & 'y-coordinate output grid                                      ',      &
       & 'rms value maximum of the orbital velocity near bottom         ',      &
       & 'mean wave steepness                                           ',      &
       & 'mean wave length                                              ',      &
       & 'smoothed peak period                                          ',      &
       & 'mean absolute zero-crossing period                            ',      &
       & 'mean absolute wave period                                     ',      &
       & 'difference in significant wave height (last iterations)       ',      &
       & 'difference in average wave period (last iterations)           ',      &
       & 'set-up due to waves                                           ',      &
       & 'x-component of wave induced force                             ',      &
       & 'y-component of wave induced force                             ',      &
       & 'peak wave period                                              '/
   data elmqty/30*' '/
   data elmunt/'[TSCALE] ', '[ - ]    ', '[ M ]    ', '[ DEG ]  ', '[ DEG ]  ', &
             & '[ SEC ]  ', '[ SEC ]  ',&
             & '[ M ]    ', '[ M/S ]  ', '[ M/S ]  ', '[ W/M  ] ', '[ W/M  ] ', &
             & '[ DEG ]  ', '[N/M/SEC]', '[J/M2/S] ', '[ - ]    ', '[ M ]    ', &
             & '[ M ]    ', '[ M/S ]  ', '[ - ]    ', '[ M ]    ', &
             & '[ SEC ]  ', '[ SEC ]  ', '[ SEC ]  ', '[ M ]    ', '[ SEC ]  ', &
             & '[ M ]    ', '[ N/M2 ] ', '[ N/M2 ] ', '[ SEC ]  '/
   data elmtps/2*'INTEGER', 28*'REAL'/
   data nbytsg/30*4/
!
!! executable statements -------------------------------------------------------
!
   idum = -1
   xdum = -1.0
   !
   if (sg%sferic) then
      elmunt(17) = '[  DEG  ]'
      elmunt(18) = '[  DEG  ]'
   end if
   !
   wrswch = .true.
   !
   ! Allocate and initialise possible additional output parameters
   ! One extra entry in group 2 for the time information
   !
   nelems2 = sof%n_outpars + 1
   !
   if (nelems2 > 1) then
      allocate (elmdms2(6, nelems2))
      allocate (nbytsg2(nelems2))
      allocate (elmunt2(nelems2))
      allocate (elmnms2(nelems2))
      allocate (elmqty2(nelems2))
      allocate (elmtps2(nelems2))
      allocate (elmdes2(nelems2))
      !
      elmqty2 = ' '
      nbytsg2 = 4
      !
      ! The first element contains the time information
      !
      elmtps2(1) = 'INTEGER'
      elmdes2(1) = 'time'
      elmunt2(1) = '[TSCALE]'
      elmnms2(1) = 'TIME'
      call filldm(elmdms2, 1, 1, 1, 0, &
                & 0, 0, 0)
      !
      ! The other elements are 2D arrays
      !
      do i = 2, nelems2
         elmdes2(i) = sof%add_out_names(i - 1)
         elmtps2(i) = 'REAL'
         elmnms2(i) = sof%add_out_names(i - 1)
         elmunt2(i) = ' '
         call filldm(elmdms2, i, 2, sof%mmax, sof%nmax, &
                   & 0, 0, 0)
      end do
   end if
   !
   if (n_swan_grids == 1) then
      write (filnam, '(2a)') 'wavm-', trim(casl)
   else
      gridnam = sg%grid_name
      ind = index(gridnam, '/', back=.true.)
      if (ind > 0) gridnam = gridnam(ind + 1:)
      ind = index(gridnam, '\', back=.true.)
      if (ind > 0) gridnam = gridnam(ind + 1:)
      ind = index(gridnam, '.', back=.true.)
      if (ind > 0) gridnam = gridnam(:ind - 1)
      write (filnam, '(4a)') 'wavm-', trim(casl), '-', trim(gridnam)
   end if
   !
   ! Write parameters to group PARAMS
   !
   it02 = 0
   itlen = 0
   call wrpara(filnam, .true., wavedata%time%tscale, idum, 5, ierror)
   call wrpara(filnam, .true., xdum, wavedata%time%refdate, 6, ierror)
   call wrpara(filnam, .true., xdum, it02, 7, ierror)
   call wrpara(filnam, .true., xdum, itlen, 8, ierror)
   !
   ! Default output parameters
   !
   call filldm(elmdms, 1, 1, 1, 0, &
             & 0, 0, 0)
   do i = 2, 30
      call filldm(elmdms, i, 2, sof%mmax, sof%nmax, &
                & 0, 0, 0)
   end do
   !
   ! Write all elements to file; all
   ! definition and creation of files, data groups, cells and
   ! elements is handled by PUTGET
   !
   nelems = 30
   celidt = wavedata%output%count
   if (prevtime) then
      idummy(1) = wavedata%time%calctimtscale_prev
   else
      idummy(1) = wavedata%time%calctimtscale
   end if
   call putgti(filnam, grpnam(1), nelems, elmnms(1), elmdms(1, 1), &
             & elmqty(1), elmunt(1), elmdes(1), elmtps(1), nbytsg(1), &
             & elmnms(1), celidt, wrswch, error, idummy(1))
   write (*, '(a,i10)') '  Time written ', wavedata%time%timtscale

   call putgti(filnam, grpnam(1), nelems, elmnms(1), elmdms(1, 1), &
             & elmqty(1), elmunt(1), elmdes(1), elmtps(1), nbytsg(1), &
             & elmnms(2), celidt, wrswch, error, sg%kcs)

   call putgtr(filnam, grpnam(1), nelems, elmnms(1), elmdms(1, 1), &
             & elmqty(1), elmunt(1), elmdes(1), elmtps(1), nbytsg(1), &
             & elmnms(3), celidt, wrswch, error, sof%hs)

   call putgtr(filnam, grpnam(1), nelems, elmnms(1), elmdms(1, 1), &
             & elmqty(1), elmunt(1), elmdes(1), elmtps(1), nbytsg(1), &
             & elmnms(4), celidt, wrswch, error, sof%dir)

   call putgtr(filnam, grpnam(1), nelems, elmnms(1), elmdms(1, 1), &
             & elmqty(1), elmunt(1), elmdes(1), elmtps(1), nbytsg(1), &
             & elmnms(5), celidt, wrswch, error, sof%pdir)

   call putgtr(filnam, grpnam(1), nelems, elmnms(1), elmdms(1, 1), &
             & elmqty(1), elmunt(1), elmdes(1), elmtps(1), nbytsg(1), &
             & elmnms(6), celidt, wrswch, error, sof%period)

   call putgtr(filnam, grpnam(1), nelems, elmnms(1), elmdms(1, 1), &
             & elmqty(1), elmunt(1), elmdes(1), elmtps(1), nbytsg(1), &
             & elmnms(7), celidt, wrswch, error, sof%rtp)

   call putgtr(filnam, grpnam(1), nelems, elmnms(1), elmdms(1, 1), &
             & elmqty(1), elmunt(1), elmdes(1), elmtps(1), nbytsg(1), &
             & elmnms(8), celidt, wrswch, error, sof%depth)

   call putgtr(filnam, grpnam(1), nelems, elmnms(1), elmdms(1, 1), &
             & elmqty(1), elmunt(1), elmdes(1), elmtps(1), nbytsg(1), &
             & elmnms(9), celidt, wrswch, error, sof%u)

   call putgtr(filnam, grpnam(1), nelems, elmnms(1), elmdms(1, 1), &
             & elmqty(1), elmunt(1), elmdes(1), elmtps(1), nbytsg(1), &
             & elmnms(10), celidt, wrswch, error, sof%v)

   call putgtr(filnam, grpnam(1), nelems, elmnms(1), elmdms(1, 1), &
             & elmqty(1), elmunt(1), elmdes(1), elmtps(1), nbytsg(1), &
             & elmnms(11), celidt, wrswch, error, sof%mx)

   call putgtr(filnam, grpnam(1), nelems, elmnms(1), elmdms(1, 1), &
             & elmqty(1), elmunt(1), elmdes(1), elmtps(1), nbytsg(1), &
             & elmnms(12), celidt, wrswch, error, sof%my)

   call putgtr(filnam, grpnam(1), nelems, elmnms(1), elmdms(1, 1), &
             & elmqty(1), elmunt(1), elmdes(1), elmtps(1), nbytsg(1), &
             & elmnms(13), celidt, wrswch, error, sof%dspr)

   call putgtr(filnam, grpnam(1), nelems, elmnms(1), elmdms(1, 1), &
             & elmqty(1), elmunt(1), elmdes(1), elmtps(1), nbytsg(1), &
             & elmnms(14), celidt, wrswch, error, sof%dissip)

   call putgtr(filnam, grpnam(1), nelems, elmnms(1), elmdms(1, 1), &
             & elmqty(1), elmunt(1), elmdes(1), elmtps(1), nbytsg(1), &
             & elmnms(15), celidt, wrswch, error, sof%rleak)

   call putgtr(filnam, grpnam(1), nelems, elmnms(1), elmdms(1, 1), &
             & elmqty(1), elmunt(1), elmdes(1), elmtps(1), nbytsg(1), &
             & elmnms(16), celidt, wrswch, error, sof%qb)

   allocate (rbuf(size(sg%x, 1), size(sg%x, 2)))

   ! This statement causes a stack overflow on big models: rbuf = real(sg%x,sp)
   ! So a double do-loop is necessary
   do j = 1, size(sg%x, 2)
      do i = 1, size(sg%x, 1)
         rbuf(i, j) = real(sg%x(i, j), sp)
      end do
   end do
   call putgtr(filnam, grpnam(1), nelems, elmnms(1), elmdms(1, 1), &
             & elmqty(1), elmunt(1), elmdes(1), elmtps(1), nbytsg(1), &
             & elmnms(17), celidt, wrswch, error, rbuf)

   ! This statement causes a stack overflow on big models: rbuf = real(sg%y,sp)
   ! So a double do-loop is necessary
   do j = 1, size(sg%y, 2)
      do i = 1, size(sg%y, 1)
         rbuf(i, j) = real(sg%y(i, j), sp)
      end do
   end do
   call putgtr(filnam, grpnam(1), nelems, elmnms(1), elmdms(1, 1), &
             & elmqty(1), elmunt(1), elmdes(1), elmtps(1), nbytsg(1), &
             & elmnms(18), celidt, wrswch, error, rbuf)

   call putgtr(filnam, grpnam(1), nelems, elmnms(1), elmdms(1, 1), &
             & elmqty(1), elmunt(1), elmdes(1), elmtps(1), nbytsg(1), &
             & elmnms(19), celidt, wrswch, error, sof%ubot)

   call putgtr(filnam, grpnam(1), nelems, elmnms(1), elmdms(1, 1), &
             & elmqty(1), elmunt(1), elmdes(1), elmtps(1), nbytsg(1), &
             & elmnms(20), celidt, wrswch, error, sof%steep)

   call putgtr(filnam, grpnam(1), nelems, elmnms(1), elmdms(1, 1), &
             & elmqty(1), elmunt(1), elmdes(1), elmtps(1), nbytsg(1), &
             & elmnms(21), celidt, wrswch, error, sof%wlen)

   call putgtr(filnam, grpnam(1), nelems, elmnms(1), elmdms(1, 1), &
             & elmqty(1), elmunt(1), elmdes(1), elmtps(1), nbytsg(1), &
             & elmnms(22), celidt, wrswch, error, sof%tps)

   call putgtr(filnam, grpnam(1), nelems, elmnms(1), elmdms(1, 1), &
             & elmqty(1), elmunt(1), elmdes(1), elmtps(1), nbytsg(1), &
             & elmnms(23), celidt, wrswch, error, sof%tm02)

   call putgtr(filnam, grpnam(1), nelems, elmnms(1), elmdms(1, 1), &
             & elmqty(1), elmunt(1), elmdes(1), elmtps(1), nbytsg(1), &
             & elmnms(24), celidt, wrswch, error, sof%tmm10)

   call putgtr(filnam, grpnam(1), nelems, elmnms(1), elmdms(1, 1), &
             & elmqty(1), elmunt(1), elmdes(1), elmtps(1), nbytsg(1), &
             & elmnms(25), celidt, wrswch, error, sof%dhsign)

   call putgtr(filnam, grpnam(1), nelems, elmnms(1), elmdms(1, 1), &
             & elmqty(1), elmunt(1), elmdes(1), elmtps(1), nbytsg(1), &
             & elmnms(26), celidt, wrswch, error, sof%drtm01)

   call putgtr(filnam, grpnam(1), nelems, elmnms(1), elmdms(1, 1), &
             & elmqty(1), elmunt(1), elmdes(1), elmtps(1), nbytsg(1), &
             & elmnms(27), celidt, wrswch, error, sof%setup)

   call putgtr(filnam, grpnam(1), nelems, elmnms(1), elmdms(1, 1), &
             & elmqty(1), elmunt(1), elmdes(1), elmtps(1), nbytsg(1), &
             & elmnms(28), celidt, wrswch, error, sof%fx)

   call putgtr(filnam, grpnam(1), nelems, elmnms(1), elmdms(1, 1), &
             & elmqty(1), elmunt(1), elmdes(1), elmtps(1), nbytsg(1), &
             & elmnms(29), celidt, wrswch, error, sof%fy)
   !
   call jonswap_mean2peak_period_factor(dble(gamma0), perfac, ierror)
   if (ierror < 0) then
      write (*, '(a,f10.5)') 'ERROR: gamma0 = ', gamma0, ' lies outside allowed range [1,20]'
      stop
   end if
   do j = 1, size(sg%x, 2)
      do i = 1, size(sg%x, 1)
         rbuf(i, j) = sof%period(i, j) * real(perfac, sp)
      end do
   end do
   call putgtr(filnam, grpnam(1), nelems, elmnms(1), elmdms(1, 1), &
             & elmqty(1), elmunt(1), elmdes(1), elmtps(1), nbytsg(1), &
             & elmnms(30), celidt, wrswch, error, rbuf)

   deallocate (rbuf, stat=ierror)
   !
   ! Also write the possible additional output parameters to the map file
   !
   if (nelems2 > 1) then
      idummy(1) = wavedata%time%timtscale
      call putgti(filnam, grpnam(2), nelems2, elmnms2(1), elmdms2(1, 1), &
                & elmqty2(1), elmunt2(1), elmdes2(1), elmtps2(1), nbytsg2(1), &
                & elmnms2(1), celidt, wrswch, error, idummy(1))
      do i = 2, nelems2
         call putgtr(filnam, grpnam(2), nelems2, elmnms2(1), elmdms2(1, 1), &
                   & elmqty2(1), elmunt2(1), elmdes2(1), elmtps2(1), nbytsg2(1), &
                   & elmnms2(i), celidt, wrswch, error, sof%add_out_vals(:, :, i - 1))
      end do
      !
      deallocate (elmdms2)
      deallocate (nbytsg2)
      deallocate (elmunt2)
      deallocate (elmnms2)
      deallocate (elmqty2)
      deallocate (elmtps2)
      deallocate (elmdes2)
   end if
   !
   ! The ice field is written into another group on the wave-map file
   !
   if (output_ice > 0) then
      call write_wave_map_ice(sg, sif, n_swan_grids, wavedata, casl)
   end if
   !
   ! The wind field is written into another group on the wave-map file
   ! it is always written!
   !
   call write_wave_map_wind(sg, sof, n_swan_grids, wavedata, casl)
end subroutine write_wave_map
