!!  Copyright (C)  Stichting Deltares, 2021-2024.
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

      subroutine write_hyd(hyd, parallel)
      ! function : write a hydrodynamic description file

      use m_logger_helper
      use m_hydmod
      use m_hyd_keys, only: key, nokey     ! keywords in hydfile
      use ddcouple_version_module, only: getfullversionstring_ddcouple
      use m_date_time_utils_external, only : write_date_time

      implicit none

      ! declaration of the arguments

      type(t_hydrodynamics)         :: hyd                    ! description of the hydrodynamics
      logical             :: parallel               ! parallel option, extra lines are removed


      ! local declarations

      integer             :: ikey                   ! index keyword (first level)
      integer             :: ikey2                  ! index keyword (second level)
      integer             :: lunhyd                 ! unit number hyd file
      integer             :: lunrep                 ! unit number report file
      integer             :: ilay                   ! index layers
      integer             :: iwast                  ! index wasteloads
      character(len=30)   :: wtype                  ! wasteload type
      integer             :: n_domain               ! number of domains
      integer             :: i_domain               ! index in collection
      integer                   :: n_dd_bound             ! number of dd-boundaries
      integer                   :: i_dd_bound             ! index in collection
      type(t_dd_bound),pointer  :: dd_bound               ! one dd_bound description

      character(Len=80) :: version
      character(20)  rundat            !! Current date and time containing a combination of DATE and TIME
      character(21)  datetime          !! Date/time to be filled in the header

      character,parameter :: cs = ' '               ! space
      character,parameter :: cq = ''''              ! quote
      character(len=2),parameter :: cqs = ''' '     ! quote with space
      character(len=2),parameter :: csq = ' '''     ! space with quote

      call get_log_unit_number(lunrep)

      call hyd%file_hyd%open()
      lunhyd = hyd%file_hyd%unit

      call getfullversionstring_ddcouple(version)
      write(lunhyd,'(A,A)') 'file-created-by  '//trim(version)

      call write_date_time(rundat)
      datetime = rundat(1:4)//'-'//rundat(6:7)//'-'//rundat(9:10)//','//rundat(11:19)
      write(lunhyd,'(A,A)') 'file-creation-date  '//datetime

      write(lunhyd,'(a,'' '',a)') key(1), key(61)
      if(hyd%layer_type == HYD_LAYERS_Z) then
         write(lunhyd,'(a,'' '',a,a,f15.6, f15.6)') key(2), key(65), key(82)
      else
         write(lunhyd,'(a,'' '',a)') key(2), key(65)
      endif
      write(lunhyd,'(a,'' '',a)') key(3), key(66)
      write(lunhyd,'(a,'' '',a)') key(4), key(67)
      write(lunhyd,'(a,'' '',a)') key(5), key(68)
      write(lunhyd,'(a)')         key(6)
      write(lunhyd,'(''   '''''',a,'''''''')') trim(hyd%description(1))
      write(lunhyd,'(''   '''''',a,'''''''')') trim(hyd%description(2))
      write(lunhyd,'(''   '''''',a,'''''''')') trim(hyd%description(3))
      write(lunhyd,'(a)')         key(7)
      write(lunhyd,'(a,'' '''''',a,'''''''')') key(8) , hyd%hyd_ref
      write(lunhyd,'(a,'' '''''',a,'''''''')') key(9) , hyd%hyd_start
      write(lunhyd,'(a,'' '''''',a,'''''''')') key(10), hyd%hyd_stop
      write(lunhyd,'(a,'' '''''',a,'''''''')') key(11), hyd%hyd_step
      write(lunhyd,'(a,'' '''''',a,'''''''')') key(12), hyd%cnv_ref
      write(lunhyd,'(a,'' '''''',a,'''''''')') key(13), hyd%cnv_start
      write(lunhyd,'(a,'' '''''',a,'''''''')') key(14), hyd%cnv_stop
      write(lunhyd,'(a,'' '''''',a,'''''''')') key(15), hyd%cnv_step
      write(lunhyd,'(a,'' '',i10)') key(16), hyd%num_columns
      write(lunhyd,'(a,'' '',i10)') key(17), hyd%num_rows
      write(lunhyd,'(a,'' '',i10)') key(18), hyd%num_layers_grid
      write(lunhyd,'(a,'' '',i10)') key(19), hyd%num_layers
      write(lunhyd,'(a,'' '''''',a,'''''''')') key(20), trim(hyd%file_com%name)
      write(lunhyd,'(a,'' '''''',a,'''''''')') key(21), trim(hyd%file_dwq%name)
      write(lunhyd,'(a,'' '''''',a,'''''''')') key(22), trim(hyd%file_lga%name)
      write(lunhyd,'(a,'' '''''',a,'''''''')') key(23), trim(hyd%file_cco%name)
      write(lunhyd,'(a,'' '''''',a,'''''''')') key(24), trim(hyd%file_vol%name)
      write(lunhyd,'(a,'' '''''',a,'''''''')') key(25), trim(hyd%file_are%name)
      write(lunhyd,'(a,'' '''''',a,'''''''')') key(26), trim(hyd%file_flo%name)
      write(lunhyd,'(a,'' '''''',a,'''''''')') key(27), trim(hyd%file_poi%name)
      write(lunhyd,'(a,'' '''''',a,'''''''')') key(28), trim(hyd%file_len%name)
      if ( hyd%sal_present ) then
         write(lunhyd,'(a,'' '''''',a,'''''''')') key(29), trim(hyd%file_sal%name)
      else
         write(lunhyd,'(a,'' '''''',a,'''''''')') key(29), 'none'
      endif
      if ( hyd%tem_present ) then
         write(lunhyd,'(a,'' '''''',a,'''''''')') key(30), trim(hyd%file_tem%name)
      else
         write(lunhyd,'(a,'' '''''',a,'''''''')') key(30), 'none'
      endif
      if ( hyd%vdf_present ) then
         write(lunhyd,'(a,'' '''''',a,'''''''')') key(31), trim(hyd%file_vdf%name)
      else
         write(lunhyd,'(a,'' '''''',a,'''''''')') key(31), 'none'
      endif
      write(lunhyd,'(a,'' '''''',a,'''''''')') key(32), trim(hyd%file_srf%name)
      write(lunhyd,'(a,'' '''''',a,'''''''')') key(33), trim(hyd%file_lgt%name)
      write(lunhyd,'(a,'' '''''',a,'''''''')') key(34), trim(hyd%file_src%name)
      write(lunhyd,'(a,'' '''''',a,'''''''')') key(35), trim(hyd%file_chz%name)
      if ( hyd%tau_present ) then
         write(lunhyd,'(a,'' '''''',a,'''''''')') key(36), trim(hyd%file_tau%name)
      else
         write(lunhyd,'(a,'' '''''',a,'''''''')') key(36), 'none'
      endif
      write(lunhyd,'(a,'' '''''',a,'''''''')') key(37), trim(hyd%file_wlk%name)
      write(lunhyd,'(a,'' '''''',a,'''''''')') key(63), trim(hyd%file_atr%name)
      write(lunhyd,'(a,'' '''''',a,'''''''')') key(64), trim(hyd%file_dps%name)

      ! zbot, ztop (for z-layer only)

      if(hyd%layer_type == HYD_LAYERS_Z .and. hyd%ztop /= -999.0) then
         write(lunhyd,'(a,'' '',f15.6)') key(83), hyd%ztop
         write(lunhyd,'(a,'' '',f15.6)') key(84), hyd%zbot
      endif

      ! hydrodynamic-layers

      write(lunhyd,'(a)') key(48)
      do ilay = 1 , hyd%num_layers_grid
         write(lunhyd,'(''      '',F15.8)') hyd%hyd_layers(ilay)
      enddo
      write(lunhyd,'(a)') key(49)

      ! water-quality-layers

      write(lunhyd,'(a)') key(50)
      do ilay = 1 , hyd%num_layers
         write(lunhyd,'(''      '',i6)') nint(hyd%waq_layers(ilay))
      enddo
      write(lunhyd,'(a)') key(51)

      ! discharges

      write(lunhyd,'(a)') key(52)
      do iwast = 1 , hyd%wasteload_coll%current_size
         if ( hyd%wasteload_coll%wasteload_pnts(iwast)%type .eq. DLWQ_WASTE_NORMAL ) then
            wtype = key(58)
         elseif ( hyd%wasteload_coll%wasteload_pnts(iwast)%type .eq. DLWQ_WASTE_INLET ) then
            wtype = key(59)
         elseif ( hyd%wasteload_coll%wasteload_pnts(iwast)%type .eq. DLWQ_WASTE_OUTLET ) then
            wtype = key(60)
         elseif ( hyd%wasteload_coll%wasteload_pnts(iwast)%type .eq. DLWQ_WASTE_WALK ) then
               wtype = key(77)
         endif

         write(lunhyd,'(3(i6,1x),'''''''',a,'''''' '',a)') hyd%wasteload_coll%wasteload_pnts(iwast)%n, &
                                                           hyd%wasteload_coll%wasteload_pnts(iwast)%m, &
                                                           hyd%wasteload_coll%wasteload_pnts(iwast)%k, &
                                                           trim(hyd%wasteload_coll%wasteload_pnts(iwast)%name), &
                                                           trim(wtype)
      enddo
      write(lunhyd,'(a)') key(53)

      ! domains

      n_domain = hyd%domain_coll%current_size
      if ( n_domain .gt. 0 .and. .not. parallel) then
         write(lunhyd,'(a)') key(54)
         do i_domain = 1 , n_domain
            write(lunhyd,'(3a,i8,a,i8,3a)') &
                                cq,trim(hyd%domain_coll%domain_pnts(i_domain)%name),cqs, &
                                        hyd%domain_coll%domain_pnts(i_domain)%num_columns ,cs , &
                                        hyd%domain_coll%domain_pnts(i_domain)%num_rows ,     &
                               csq,trim(hyd%domain_coll%domain_pnts(i_domain)%aggr) ,cq
         enddo
            write(lunhyd,'(a)') key(55)
      endif

      ! dd-boundaries

      n_dd_bound = hyd%dd_bound_coll%current_size
      if ( n_dd_bound .gt. 0 .and. .not. parallel) then
         write(lunhyd,'(a)') key(56)
         do i_dd_bound = 1 , n_dd_bound
            dd_bound => hyd%dd_bound_coll%dd_bound_pnts(i_dd_bound)
            write(lunhyd,'(3a,4(i8,1x),3a,4(i8,1x))') &
                                cq,trim(dd_bound%name1),cqs, &
                                        dd_bound%m_begin1  , &
                                        dd_bound%n_begin1  , &
                                        dd_bound%m_end1    , &
                                        dd_bound%n_end1    , &
                                cq,trim(dd_bound%name2),cqs, &
                                        dd_bound%m_begin2  , &
                                        dd_bound%n_begin2  , &
                                        dd_bound%m_end2    , &
                                        dd_bound%n_end2
         enddo
         write(lunhyd,'(a)') key(57)
      endif

      return
      end subroutine write_hyd
