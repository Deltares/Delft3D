module xml_data_discharge_def
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
!  $Id: discharge_def.f90 5888 2016-02-24 10:14:54Z mourits $
!  $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20160128_34357_NearField_Coupling/src/engines_gpl/flow2d3d/packages/kernel/src/compute_nearfar/read_xml_discharges/discharge_def.f90 $
!!--description-----------------------------------------------------------------
! NONE
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
   use READ_XML_PRIMITIVES
   use WRITE_XML_PRIMITIVES
   use XMLPARSE
   implicit none
   integer, private :: lurep_
   logical, private :: strict_

type discharge_t
   character(len=80)                                          :: name
   real                                                      :: x_diff
   real                                                      :: y_diff
   real                                                      :: x_amb
   real                                                      :: y_amb
   real                                                      :: x_int
   real                                                      :: y_int
   real                                                      :: z_int
   real                                                      :: q
   real                                                      :: s0
   real                                                      :: t0
end type discharge_t
   type(discharge_t), dimension(:), pointer                  :: discharges => null()
contains
subroutine read_xml_type_discharge_t_array( &
      info, tag, endtag, attribs, noattribs, data, nodata, &
      dvar, has_dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(inout)                 :: tag
   logical, intent(inout)                          :: endtag
   character(len=*), dimension(:,:), intent(inout) :: attribs
   integer, intent(inout)                          :: noattribs
   character(len=*), dimension(:), intent(inout)   :: data
   integer, intent(inout)                          :: nodata
   type(discharge_t), dimension(:), pointer :: dvar
   logical, intent(inout)                       :: has_dvar

   integer                                      :: newsize
   type(discharge_t), dimension(:), pointer :: newvar

   newsize = size(dvar) + 1
   allocate( newvar(1:newsize) )
   if ( associated(dvar) ) then
      newvar(1:newsize-1) = dvar
      deallocate( dvar )
   endif
   dvar => newvar

   call read_xml_type_discharge_t( info, tag, endtag, attribs, noattribs, data, nodata, &
              dvar(newsize), has_dvar )
end subroutine read_xml_type_discharge_t_array

subroutine read_xml_type_discharge_t( info, starttag, endtag, attribs, noattribs, data, nodata, &
              dvar, has_dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(in)                    :: starttag
   logical, intent(inout)                          :: endtag
   character(len=*), dimension(:,:), intent(inout) :: attribs
   integer, intent(inout)                          :: noattribs
   character(len=*), dimension(:), intent(inout)   :: data
   integer, intent(inout)                          :: nodata
   type(discharge_t), intent(inout)  :: dvar
   logical, intent(inout)                       :: has_dvar

   integer                                      :: att_
   integer                                      :: noatt_
   logical                                      :: error
   logical                                      :: endtag_org
   character(len=len(starttag))                 :: tag
   logical                                                   :: has_name
   logical                                                   :: has_x_diff
   logical                                                   :: has_y_diff
   logical                                                   :: has_x_amb
   logical                                                   :: has_y_amb
   logical                                                   :: has_x_int
   logical                                                   :: has_y_int
   logical                                                   :: has_z_int
   logical                                                   :: has_q
   logical                                                   :: has_s0
   logical                                                   :: has_t0
   has_name                             = .false.
   has_x_diff                           = .false.
   has_y_diff                           = .false.
   has_x_amb                            = .false.
   has_y_amb                            = .false.
   has_x_int                            = .false.
   has_y_int                            = .false.
   has_z_int                            = .false.
   has_q                                = .false.
   has_s0                               = .false.
   has_t0                               = .false.
   call init_xml_type_discharge_t(dvar)
   has_dvar = .true.
   error  = .false.
   att_   = 0
   noatt_ = noattribs+1
   endtag_org = endtag
   do
      if ( nodata .ne. 0 ) then
         noattribs = 0
         tag = starttag
      elseif ( att_ .lt. noatt_ .and. noatt_ .gt. 1 ) then
         att_      = att_ + 1
         if ( att_ .le. noatt_-1 ) then
            tag       = attribs(1,att_)
            data(1)   = attribs(2,att_)
            noattribs = 0
            nodata    = 1
            endtag    = .false.
         else
            tag       = starttag
            noattribs = 0
            nodata    = 0
            endtag    = .true.
            cycle
         endif
      else
         if ( endtag_org ) then
            return
         else
            call xml_get( info, tag, endtag, attribs, noattribs, data, nodata )
            if ( xml_error(info) ) then
               write(lurep_,*) 'Error reading input file!'
               error = .true.
               return
            endif
         endif
      endif
      if ( endtag .and. tag .eq. starttag ) then
         exit
      endif
      if ( endtag .and. noattribs .eq. 0 ) then
         if ( xml_ok(info) ) then
            cycle
         else
            exit
         endif
      endif
      select case( tag )
      case('name')
         call read_xml_line( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%name, has_name )
      case('x_diff')
         call read_xml_real( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%x_diff, has_x_diff )
      case('y_diff')
         call read_xml_real( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%y_diff, has_y_diff )
      case('x_amb')
         call read_xml_real( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%x_amb, has_x_amb )
      case('y_amb')
         call read_xml_real( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%y_amb, has_y_amb )
      case('x_int')
         call read_xml_real( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%x_int, has_x_int )
      case('y_int')
         call read_xml_real( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%y_int, has_y_int )
      case('z_int')
         call read_xml_real( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%z_int, has_z_int )
      case('Q')
         call read_xml_real( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%q, has_q )
      case('S0')
         call read_xml_real( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%s0, has_s0 )
      case('T0')
         call read_xml_real( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            dvar%t0, has_t0 )
      case ('comment', '!--')
         ! Simply ignore
      case default
         if ( strict_ ) then
            error = .true.
            call xml_report_errors( info, &
               'Unknown or wrongly placed tag: ' // trim(tag))
         endif
      end select
      nodata = 0
      if ( .not. xml_ok(info) ) exit
   end do
   if ( .not. has_name ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on name')
   endif
   if ( .not. has_x_diff ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on x_diff')
   endif
   if ( .not. has_y_diff ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on y_diff')
   endif
   if ( .not. has_x_amb ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on x_amb')
   endif
   if ( .not. has_y_amb ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on y_amb')
   endif
   if ( .not. has_x_int ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on x_int')
   endif
   if ( .not. has_y_int ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on y_int')
   endif
   if ( .not. has_z_int ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on z_int')
   endif
   if ( .not. has_q ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on q')
   endif
   if ( .not. has_s0 ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on s0')
   endif
   if ( .not. has_t0 ) then
      has_dvar = .false.
      call xml_report_errors(info, 'Missing data on t0')
   endif
end subroutine read_xml_type_discharge_t
subroutine init_xml_type_discharge_t_array( dvar )
   type(discharge_t), dimension(:), pointer :: dvar
   if ( associated( dvar ) ) then
      deallocate( dvar )
   endif
   allocate( dvar(0) )
end subroutine init_xml_type_discharge_t_array
subroutine init_xml_type_discharge_t(dvar)
   type(discharge_t) :: dvar
end subroutine init_xml_type_discharge_t
subroutine write_xml_type_discharge_t_array( &
      info, tag, indent, dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(in)                    :: tag
   integer                                         :: indent
   type(discharge_t), dimension(:)        :: dvar
   integer                                         :: i
   do i = 1,size(dvar)
       call write_xml_type_discharge_t( info, tag, indent, dvar(i) )
   enddo
end subroutine write_xml_type_discharge_t_array

subroutine write_xml_type_discharge_t( &
      info, tag, indent, dvar )
   type(XML_PARSE)                                 :: info
   character(len=*), intent(in)                    :: tag
   integer                                         :: indent
   type(discharge_t)                      :: dvar
   character(len=100)                              :: indentation
   indentation = ' '
   write(info%lun, '(4a)' ) indentation(1:min(indent,100)),&
       '<',trim(tag), '>'
   call write_to_xml_line( info, 'name', indent+3, dvar%name)
   call write_to_xml_real( info, 'x_diff', indent+3, dvar%x_diff)
   call write_to_xml_real( info, 'y_diff', indent+3, dvar%y_diff)
   call write_to_xml_real( info, 'x_amb', indent+3, dvar%x_amb)
   call write_to_xml_real( info, 'y_amb', indent+3, dvar%y_amb)
   call write_to_xml_real( info, 'x_int', indent+3, dvar%x_int)
   call write_to_xml_real( info, 'y_int', indent+3, dvar%y_int)
   call write_to_xml_real( info, 'z_int', indent+3, dvar%z_int)
   call write_to_xml_real( info, 'Q', indent+3, dvar%q)
   call write_to_xml_real( info, 'S0', indent+3, dvar%s0)
   call write_to_xml_real( info, 'T0', indent+3, dvar%t0)
   write(info%lun,'(4a)') indentation(1:min(indent,100)), &
       '</' //trim(tag) // '>'
end subroutine write_xml_type_discharge_t

subroutine read_xml_file_discharge_def(fname, lurep, errout)
   character(len=*), intent(in)           :: fname
   integer, intent(in), optional          :: lurep
   logical, intent(out), optional         :: errout

   type(XML_PARSE)                        :: info
   logical                                :: error
   character(len=80)                      :: tag
   character(len=80)                      :: starttag
   logical                                :: endtag
   character(len=80), dimension(1:2,1:20) :: attribs
   integer                                :: noattribs
   character(len=200), dimension(1:100)   :: data
   integer                                :: nodata
   logical                                                   :: has_discharges
   has_discharges                       = .false.
   allocate(discharges(0))

   call init_xml_file_discharge_def
   call xml_open( info, fname, .true. )
   call xml_options( info, report_errors=.true., ignore_whitespace=.true.)
   lurep_ = 0
   if ( present(lurep) ) then
      lurep_ = lurep
      call xml_options( info, report_lun=lurep )
   endif
   do
      call xml_get( info, starttag, endtag, attribs, noattribs, &
         data, nodata)
      if ( starttag .ne. '!--' ) exit
   enddo
   if ( starttag .ne. "all_discharges" ) then
      call xml_report_errors( info, &
         'XML-file should have root element "all_discharges"')
      error = .true.
      call xml_close(info)
      return
   endif
   strict_ = .false.
   error = .false.
   do
      call xml_get( info, tag, endtag, attribs, noattribs, data, nodata )
      if ( xml_error(info) ) then
         write(lurep_,*) 'Error reading input file!'
         error = .true.
         return
      endif
      if ( endtag .and. tag .eq. starttag ) then
         exit
      endif
      if ( endtag .and. noattribs .eq. 0 ) then
         if ( xml_ok(info) ) then
            cycle
         else
            exit
         endif
      endif
      select case( tag )
      case('discharge')
         call read_xml_type_discharge_t_array( &
            info, tag, endtag, attribs, noattribs, data, nodata, &
            discharges, has_discharges )
      case ('comment', '!--')
         ! Simply ignore
      case default
         if ( strict_ ) then
            error = .true.
            call xml_report_errors( info, &
               'Unknown or wrongly placed tag: ' // trim(tag))
         endif
      end select
      nodata = 0
      if ( .not. xml_ok(info) ) exit
   end do
   if ( .not. has_discharges ) then
      error = .true.
      call xml_report_errors(info, 'Missing data on discharges')
   endif
   if ( present(errout) ) errout = error
   call xml_close(info)
end subroutine

subroutine write_xml_file_discharge_def(fname, lurep)
   character(len=*), intent(in)           :: fname
   integer, intent(in), optional          :: lurep

   type(XML_PARSE)                        :: info
   integer                                :: indent = 0

   call xml_open( info, fname, .false. )
   call xml_options( info, report_errors=.true.)
   if ( present(lurep) ) then
       call xml_options( info, report_lun=lurep)
   endif
   write(info%lun,'(a)') &
      '<all_discharges>'
   call write_xml_type_discharge_t_array( info, 'discharge', indent+3, discharges)
   write(info%lun,'(a)') '</all_discharges>'
   call xml_close(info)
end subroutine

subroutine init_xml_file_discharge_def

end subroutine

end module
