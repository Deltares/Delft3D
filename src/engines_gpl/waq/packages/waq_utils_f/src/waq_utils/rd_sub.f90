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
module m_rd_sub
use m_zoek


implicit none

contains


   subroutine rd_sub(allocated,input_file,nosys,notot,nocons,noout,syname,syunit,coname,covalue,ouname,oudesc,ierr,cerr)

   use m_monsys
   use dlwq_data      ! for definition and storage of data
   use rd_token       ! tokenized reading

   implicit none

   logical           , intent (in   )  :: allocated       !< only store actual data when arrays are allocated
   character(len=256), intent (in   )  :: input_file      !< input filename
   integer  ( 4)     , intent (inout)  :: nosys           !< Number of active systems
   integer  ( 4)     , intent (inout)  :: notot           !< Number of systems
   integer  ( 4)     , intent (inout)  :: nocons          !< Number of constants used
   integer  ( 4)     , intent (inout)  :: noout           !< Number of outputs
   character(20)     , intent (inout)  :: syname(*)       !< substance names
   character(20)     , intent (inout)  :: syunit(*)       !< substance names
   character(20)     , intent (inout)  :: coname(*)       !< constant names
   real              , intent (inout)  :: covalue(*)      !< constant values
   character(20)     , intent (inout)  :: ouname(*)       !< output names
   character(80)     , intent (inout)  :: oudesc(*)       !< output descriptions
   integer           , intent (  out)  :: ierr            !< error status
   character(256)    , intent (  out)  :: cerr            !< error message

   character(20)                       :: ianame(10000)   !< inactive substance names
   character(20)                       :: iaunit(10000)   !< inactive substance names

   integer, parameter                  :: ntag=4
   integer, parameter                  :: natt=8
   integer                             :: isub
   integer                             :: itag
   integer                             :: iatt
   logical                             :: isactive
   character(20)                       :: starttag (ntag)
   character(20)                       :: endtag   (ntag)
   character(20)                       :: attribute(natt)

   character(20)                       :: csub
   character(20)                       :: cpar
   character(20)                       :: cout
   character(20)                       :: cpro
   character(20)                       :: ctag
   character(20)                       :: catt
   character(256)                      :: cstr
   real                                :: rdata

   integer                             :: i
   integer                             :: iostat

   integer   (4)                       :: anint
   real      (4)                       :: areal
   integer   (4)                       :: itype

   data starttag  / 'substance           ','parameter           ', &
                    'output              ','active-processes    ' /
   data endtag    / 'end-substance       ','end-parameter       ', &
                    'end-output          ','end-active-processes' /
   data attribute / 'active              ','inactive            ', &
                    'description         ','concentration-unit  ', &
                    'waste-load-unit     ','unit                ', &
                    'value               ','name                ' /

   call getmlu(lunut)
   ilun    = 0
   lch (1) = input_file
   open (newunit=ilun(1), file=lch(1), status='old', iostat=ierr)
   if(ierr.ne.0) then
      cerr = 'Error reading file: '//trim(lch(1))
      return
   endif
   npos   = 1000
   cchar  = '#'

   nosys  = 0
   notot  = 0
   nocons = 0
   noout  = 0
   cerr = 'Error: Something went wrong during the reading of the substance file.'

   do
      if ( gettoken ( ctag, anint, areal, itype, ierr) .ne. 0 ) exit
      if (itype .ne. 1) then
         ierr = 101
         cerr = 'Error: Expected a keyword, but found a real or integer while reading file: '//trim(lch(1))
         return
      endif

      call zoekns(ctag,ntag,starttag,20,itag)
      select case (itag)

      case (1)

   ! case 1  substance

   !  substance 'name' active/inactive
   !     description        'text'
   !     concentration-unit '(text)'
   !     waste-load-unit    '(text)'
   !  end-substance

         if ( gettoken ( csub, ierr ) .ne. 0 ) exit
         if ( gettoken ( catt, ierr ) .ne. 0 ) exit
         call zoekns(catt,natt,attribute,20,iatt)
         if (iatt.eq.1) then
            nosys = nosys + 1
            if (allocated) syname(nosys) = csub
            isactive = .true.
         else if (iatt.eq.2) then
            notot = notot + 1
            if (allocated) ianame(notot) = csub
            isactive = .false.
         else
            ierr=101
            cerr='Expected attribute ''active'' or ''inactive'' for substance '//trim(csub)//', but read: '//trim(catt)
            exit
         endif

   !  substance attributes

         if ( gettoken ( catt, ierr ) .ne. 0 ) exit
         call zoekns(catt,natt,attribute,20,iatt)
         if (iatt.ne.3) then
            ierr=101
            cerr='Expected attribute ''description'' for substance '//trim(csub)//', but read: '//trim(catt)
            exit
         endif
         if ( gettoken ( cstr, ierr ) .ne. 0 ) exit
         if ( gettoken ( catt, ierr ) .ne. 0 ) exit
         call zoekns(catt,natt,attribute,20,iatt)
         if (iatt.ne.4) then
            ierr=101
            cerr='Expected attribute ''concentration-unit'' for substance '//trim(csub)//', but read: '//trim(catt)
            exit
         endif
         if ( gettoken ( cstr, ierr ) .ne. 0 ) exit
         if (isactive) then
            if (allocated) syunit(nosys) = cstr
         else
            if (allocated) iaunit(notot) = cstr
         endif
         if ( gettoken ( catt, ierr ) .ne. 0 ) exit
         call zoekns(catt,natt,attribute,20,iatt)
         if (iatt.ne.5) then
            ierr=101
            cerr='Expected attribute ''waste-load-unit'' for substance '//trim(csub)//', but read: '//trim(catt)
            exit
         endif
         if ( gettoken ( cstr, ierr ) .ne. 0 ) exit

   !  substance end-tag

         if ( gettoken ( ctag, ierr ) .ne. 0 ) exit
         call zoekns(ctag,ntag,endtag,20,itag)
         if ( itag .ne. 1 ) then
            ierr=101
            cerr='Expected end-tag ''end-substance'' for substance '//trim(csub)//', but read: '//trim(catt)
            exit
         endif

      case (2)

   ! case 2  parameter

   !  parameter 'name'
   !     description   'text'
   !     unit          'text'
   !     value          0.0E+00
   !  end-parameter

         if ( gettoken ( cpar, ierr ) .ne. 0 ) exit
         nocons = nocons + 1
         if (allocated) coname(nocons) = cpar

         if ( gettoken ( catt, ierr ) .ne. 0 ) exit
         call zoekns(catt,natt,attribute,20,iatt)
         if (iatt.ne.3) then
            ierr=101
            cerr='Expected attribute ''description'' for parameter '//trim(cpar)//', but read: '//trim(catt)
            exit
         endif
         if ( gettoken ( cstr, ierr ) .ne. 0 ) exit

         if ( gettoken ( catt, ierr ) .ne. 0 ) exit
         call zoekns(catt,natt,attribute,20,iatt)
         if (iatt.ne.6) then
            ierr=101
            cerr='Expected attribute ''unit'' for parameter '//trim(cpar)//', but read: '//trim(catt)
            exit
         endif
         if ( gettoken ( cstr, ierr ) .ne. 0 ) exit

         if ( gettoken ( catt, ierr ) .ne. 0 ) exit
         call zoekns(catt,natt,attribute,20,iatt)
         if (iatt.ne.7) then
            ierr=101
            cerr='Expected attribute ''value'' for parameter '//trim(cpar)//', but read: '//trim(catt)
            exit
         endif
         if ( gettoken ( rdata, ierr ) .ne. 0 ) then
            cerr = 'Error: Couldn''t read value for parameter '//trim(cpar)//' in the substance file'
            exit
         endif
         if (allocated) covalue(nocons) = rdata
   !  parameter end-tag

         if ( gettoken ( ctag, ierr ) .ne. 0 ) exit
         call zoekns(ctag,ntag,endtag,20,itag)
         if ( itag .ne. 2 ) then
            ierr=101
            cerr='Expected end-tag ''end-parameter'' for parameter '//trim(cpar)//', but read: '//trim(catt)
            exit
         endif

      case(3)

   !  case 3  output

   !  output 'MrtToEColi'
   !     description   'overall mortality rate EColi'
   !  end-output

         if ( gettoken ( cout, ierr ) .ne. 0 ) exit
         noout = noout + 1
         if (allocated) ouname(noout) = cout

         if ( gettoken ( catt, ierr ) .ne. 0 ) exit
         call zoekns(catt,natt,attribute,20,iatt)
         if (iatt.ne.3) then
            ierr=101
            cerr='Expected attribute ''description'' for output '//trim(cout)//', but read: '//trim(catt)
            exit
         endif
         if ( gettoken ( cstr, ierr ) .ne. 0 ) exit
         if (allocated) oudesc(noout) = cstr

   !  output end-tag

         if ( gettoken ( ctag, ierr ) .ne. 0 ) exit
         call zoekns(ctag,ntag,endtag,20,itag)
         if ( itag .ne. 3 ) then
            ierr=101
            cerr='Expected end-tag ''end-output'' for output '//trim(cout)//', but read: '//trim(catt)
            exit
         endif

      case (4)
   !  case 4  active-processes
   !
   !  active-processes
   !     name  'EColiMrt' 'Mortality EColi bacteria'
   !     name  'Extinc_VLG' 'Extinction of visible-light (370-680nm) DLWQ-G'
   !     name  'DynDepth' 'dynamic calculation of the depth'
   !  end-active-processes

   !     first add some default processes
         nocons = nocons + 1
         if (allocated) coname(nocons) = 'ONLY_ACTIVE'
         if (allocated) covalue(nocons)  = 1.0e0
         nocons = nocons + 1
         if (allocated) coname(nocons) = 'ACTIVE_DynDepth'
         if (allocated) covalue(nocons)  = 1.0e0

         do
            if ( gettoken ( catt, ierr ) .ne. 0 ) exit
            call zoekns(catt,natt,attribute,20,iatt)
            if (iatt.ne.8) then
               ctag=catt
               call zoekns(ctag,ntag,endtag,20,itag)
               if ( itag .ne. 4 ) then
                  ierr=102
                  cerr='Expected keyword ''name'' or end-tag ''end-active-processes'' to close active-processes list, but read: '//trim(catt)
                  exit
               endif
               exit
            endif
            if ( gettoken ( cpro, ierr ) .ne. 0 ) exit
            nocons = nocons + 1
            if (allocated) coname(nocons) = 'ACTIVE_'//cpro
            if ( gettoken ( cstr, ierr ) .ne. 0 ) exit
         end do
         exit

   !  case default: no valid tag found!

      case default
         ierr = 100
         cerr = 'expected tag ''substance'', ''parameter'', ''output'' or ''active-processes'', but read: '//ctag
         exit
      end select
   end do

   if ( ierr.eq.3) then
      ! end of file
      ierr = 0
   endif
!  append inactive substances to substances list
   if (allocated) then
      do isub = 1, notot
         syname(nosys+isub) = ianame(isub)
         syunit(nosys+isub) = iaunit(isub)
      end do
   end if
   notot = notot + nosys

   do i = 1, lstack
      close (unit = ilun(i), err=1)
 1    continue
   end do

   return
   end subroutine
end module m_rd_sub
