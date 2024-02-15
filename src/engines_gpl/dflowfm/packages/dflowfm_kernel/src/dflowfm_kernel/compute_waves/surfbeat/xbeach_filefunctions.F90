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
module m_xbeach_filefunctions
!! Contains logging functions and file administration functions
!! Merge of logging_module and filefunctions_module
   use m_xbeach_typesandkinds


   implicit none

   integer,save     :: logfileid
   integer,save     :: errorfileid
   integer,save     :: warningfileid


   interface check_file_length
   module procedure check_file_length_1D
   module procedure check_file_length_2D
   module procedure check_file_length_3D
   end interface check_file_length


   procedure(distributeloginterface), pointer :: distributelog => null()

   abstract interface

   subroutine distributeloginterface(code,message,len)
   implicit none
   integer, intent(in) :: code
   integer, intent(in) :: len
   character(*),intent(in) :: message

   end subroutine distributeloginterface

   end interface

   !
   ! Options for destiantion in writelog
   ! 's' = screen
   ! 'l' = log file
   ! 'e' = error file
   ! 'w' = warning file
   !
   ! Combinations also allowed, f.i.
   ! 'le' = log file and error file
   ! 'el' ditto
   ! 'sel' = screen, log file and error file
   !
   interface writelog
   module procedure writelog_a
   module procedure writelog_aa
   module procedure writelog_ai
   module procedure writelog_ia
   module procedure writelog_aaa
   module procedure writelog_aaaa
   module procedure writelog_aai
   module procedure writelog_aii
   module procedure writelog_aaai
   module procedure writelog_aaia
   module procedure writelog_aia
   module procedure writelog_aiaa
   module procedure writelog_aiaaa
   module procedure writelog_aiai
   module procedure writelog_aiaia
   module procedure writelog_aaiai
   module procedure writelog_aaaiai
   module procedure writelog_aiafa
   module procedure writelog_aiafaf
   module procedure writelog_aiaiai
   module procedure writelog_aiaiaia
   module procedure writelog_aiaiaf
   module procedure writelog_aiaiafa
   module procedure writelog_iiiii
   module procedure writelog_af
   module procedure writelog_aaf
   module procedure writelog_afa
   module procedure writelog_afaf
   module procedure writelog_afafa
   module procedure writelog_aaaf
   module procedure writelog_aafa
   module procedure writelog_afaaa
   module procedure writelog_aafaf
   module procedure writelog_aaafaf
   module procedure writelog_afafafaf
   module procedure writelog_illll
   module procedure writelog_fa
   module procedure writelog_afaiaaa
   end interface writelog


   contains

subroutine start_logfiles(error)
   use m_xbeach_errorhandling
   use m_partitioninfo
   implicit none

   integer         :: error, logerr, warnerr, errerr

   if ( jampi.eq.0 ) then
      open(newunit=logfileid,     file='surfbeatlog.txt',       status='replace', iostat=logerr)
      open(newunit=errorfileid,   file='surfbeaterror.txt',     status='replace', iostat=errerr)
      open(newunit=warningfileid, file='surfbeatwarning.txt',   status='replace', iostat=warnerr)
   else
      open(newunit=logfileid,     file='surfbeatlog'//'_'//sdmn//'.txt',       status='replace', iostat=logerr)
      open(newunit=errorfileid,   file='surfbeaterror'//'_'//sdmn//'.txt',     status='replace', iostat=errerr)
      open(newunit=warningfileid, file='surfbeatwarning'//'_'//sdmn//'.txt',   status='replace', iostat=warnerr)
   end if

   if (logerr > 0 .or. errerr > 0 .or. warnerr > 0) error = 1

       ! newunit returns negative file id's
   if (logfileid > 0 .or. errorfileid > 0 .or. warningfileid > 0) error = 1



   if (error==1) then
      write(*,*) 'Error: not able to open log file. Stopping simulation'
       call xbeach_errorhandler()
   endif

   end subroutine start_logfiles

   subroutine close_logfiles

   close(logfileid                         )
   close(errorfileid,      STATUS='DELETE' )
   close(warningfileid                     )

   end subroutine close_logfiles


   subroutine get_logfileid(lid,eid,wid)

   implicit none
   integer, intent(out)     :: lid,eid,wid

   lid = logfileid
   eid = errorfileid
   wid = warningfileid

   endsubroutine get_logfileid

   subroutine progress_indicator(initialize,curper,dper,dt)

   implicit none

   logical,intent(in)      :: initialize    ! initialize current progress indicator
   real*8,intent(in)       :: curper        ! current percentage done
   real*8,intent(in)       :: dper          ! steps in percentage between output
   real*8,intent(in)       :: dt            ! steps in time (s) between output
   ! whichever reached earlier (dper,dt) will determin output
   ! internal
   real*8,save             :: lastper,lastt
   real*8                  :: tnow
   integer*4               :: count,count_rate,count_max


   if (initialize) then
      lastper = 0.d0
      call system_clock (count,count_rate,count_max)
      lastt = dble(count)/count_rate
   else
      call system_clock (count,count_rate,count_max)
      tnow = dble(count)/count_rate
      if (curper>=lastper+dper .or. tnow>=lastt+dt) then
         call writelog('ls','(f0.1,a)',curper,'% done')
         if (curper>=lastper+dper) then
            lastper = curper-mod(curper,dper)
         else
            lastper = curper
         endif
         lastt = tnow
      endif
   endif
   end subroutine progress_indicator

   subroutine report_file_read_error(filename)
   use m_xbeach_errorhandling

   implicit none

   character(*)    :: filename

   call writelog('lswe','','Error reading file ''',trim(filename),'''')
   call writelog('lswe','','Check file for incorrect decimal format, line breaks and tab characters')
     call xbeach_errorhandler()

   end subroutine report_file_read_error

   subroutine writelog_startup()

   implicit none

   character(len=8)                                :: date
   character(len=10)                               :: time
   character(len=5)                                :: zone

   ! get current working directory (gcc only)
#ifdef HAVE_CONFIG_H
#include "config.h"
   character(slen)                              :: cwd
   call getcwd(cwd)
#endif
   call date_and_time(DATE=date, TIME=time, ZONE=zone)

   call writelog('ls','','**********************************************************')
   call writelog('ls','','       Welcome to DFLOW FM - surfbeat mode                ')
   call writelog('ls','','**********************************************************')
   call writelog('ls','','                                                          ')
   call writelog('ls','','Simulation started: yyyymmdd    hh:mm:ss     time zone (UTC)')
   call writelog('ls','','                    '//date //'  '//time(1:2)//':'//time(3:4)//':'//time(5:6)//'     '//zone)
   call writelog('ls','','                                                          ')
#ifdef HAVE_CONFIG_H
   call writelog('ls','',' running in: ',cwd)
#endif
   call writelog('ls','','General Input Module')

   end subroutine writelog_startup

   subroutine writelog_distribute(destination,display)

   implicit none

   character(*)            :: destination
   character(slen)         :: display
   integer                 :: level

   level = -1
   if (scan(destination,'s')>0) then
      level = 3
   end if
   if (scan(destination,'l')>0) then
      level = 2
      write(6,*)     trim(display)
      write(logfileid,*)     trim(display)
   end if
   if (scan(destination,'w')>0) then
      level = 1
      write(0,*) trim(display)
      write(warningfileid,*) trim(display)
   end if
   if (scan(destination,'e')>0) then
      level = 0
      write(0,*)   trim(display)
      write(errorfileid,*)   trim(display)
   end if
   if (associated(distributelog)) then
      call distributelog(level,trim(display), len(trim(display)))
   endif

   end subroutine writelog_distribute

   subroutine writelog_a(destination,form,message_char)
   implicit none
   character(*),intent(in)    ::  form,message_char
   character(*),intent(in)    ::  destination
   character(slen)            ::  display

   if (form=='') then
      write(display,*)trim(message_char)
   else
      write(display,form)trim(message_char)
   endif

   call writelog_distribute(destination, display)

   end subroutine writelog_a

   subroutine writelog_aa(destination,form,message_char1,message_char2)
   implicit none
   character(*),intent(in)    ::  form,message_char1,message_char2
   character(*),intent(in)       ::  destination
   character(slen)            ::  display

   if (form=='') then
      write(display,*) message_char1(1:min(len(message_char1),len_trim(message_char1)+1)), &
         trim(message_char2)
   else
      write(display,form) message_char1(1:min(len(message_char1),len_trim(message_char1)+1)), &
         trim(message_char2)
   endif

   call writelog_distribute(destination, display)

   end subroutine writelog_aa

   subroutine writelog_ai(destination,form,message_char,message_int)
   implicit none
   character(*),intent(in)    ::  form,message_char
   character(*),intent(in)    ::  destination
   integer,intent(in)         ::  message_int
   character(slen)            ::  display

   if (form=='') then
      write(display,*)message_char(1:min(len(message_char),len_trim(message_char)+1)), &
         message_int
   else
      write(display,form)message_char(1:min(len(message_char),len_trim(message_char)+1)), &
         message_int
   endif

   call writelog_distribute(destination, display)

   end subroutine writelog_ai

   subroutine writelog_ia(destination,form,mint1,mchar1)
   implicit none
   character(*),intent(in)    ::  form,mchar1
   character(*),intent(in)    ::  destination
   integer,intent(in)         ::  mint1
   character(slen)            ::  display

   if (form=='') then
      write(display,*)mint1,trim(mchar1)
   else
      write(display,form)mint1,trim(mchar1)
   endif

   call writelog_distribute(destination, display)

   end subroutine writelog_ia

   subroutine writelog_aaa(destination,form,message_char1,message_char2,message_char3)
   implicit none
   character(*),intent(in)    ::  form,message_char1,message_char2,message_char3
   character(*),intent(in)       ::  destination
   character(slen)            ::  display

   if (form=='') then
      write(display,*)message_char1(1:min(len(message_char1),len_trim(message_char1)+1)), &
         message_char2(1:min(len(message_char2),len_trim(message_char2)+1)), &
         trim(message_char3)
   else
      write(display,form)message_char1(1:min(len(message_char1),len_trim(message_char1)+1)), &
         message_char2(1:min(len(message_char2),len_trim(message_char2)+1)), &
         trim(message_char3)
   endif

   call writelog_distribute(destination, display)

   end subroutine writelog_aaa

   subroutine writelog_aaaa(destination,form,message_char1,message_char2,message_char3,message_char4)
   implicit none
   character(*),intent(in)    ::  form,message_char1,message_char2,message_char3,message_char4
   character(*),intent(in)       ::  destination
   character(slen)            ::  display

   if (form=='') then
      write(display,*)message_char1(1:min(len(message_char1),len_trim(message_char1)+1)), &
         message_char2(1:min(len(message_char2),len_trim(message_char2)+1)), &
         message_char3(1:min(len(message_char3),len_trim(message_char3)+1)), &
         trim(message_char4)
   else
      write(display,form)message_char1(1:min(len(message_char1),len_trim(message_char1)+1)), &
         message_char2(1:min(len(message_char2),len_trim(message_char2)+1)), &
         message_char3(1:min(len(message_char3),len_trim(message_char3)+1)), &
         trim(message_char4)
   endif

   call writelog_distribute(destination, display)

   end subroutine writelog_aaaa


   subroutine writelog_aai(destination,form,message_char1,message_char2,message_int)
   implicit none
   character(*),intent(in)    ::  form,message_char1,message_char2
   character(*),intent(in)    ::  destination
   integer,intent(in)         ::  message_int
   character(slen)            ::  display

   if (form=='') then
      write(display,*)message_char1(1:min(len(message_char1),len_trim(message_char1)+1)), &
         message_char2(1:min(len(message_char2),len_trim(message_char2)+1)), &
         message_int
   else
      write(display,form)message_char1(1:min(len(message_char1),len_trim(message_char1)+1)), &
         message_char2(1:min(len(message_char2),len_trim(message_char2)+1)), &
         message_int
   endif

   call writelog_distribute(destination, display)

   end subroutine writelog_aai

   subroutine writelog_aii(destination,form,message_char1,message_int1,message_int2)
   implicit none
   character(*),intent(in)    ::  form,message_char1
   character(*),intent(in)    ::  destination
   integer,intent(in)         ::  message_int1,message_int2
   character(slen)            ::  display

   if (form=='') then
      write(display,*)message_char1(1:min(len(message_char1),len_trim(message_char1)+1)), &
         message_int1,message_int2
   else
      write(display,form)message_char1(1:min(len(message_char1),len_trim(message_char1)+1)), &
         message_int1,message_int2
   endif

   call writelog_distribute(destination, display)

   end subroutine writelog_aii

   subroutine writelog_aia(destination,form,message_char1b,message_intb,message_char2b)
   implicit none
   character(*),intent(in)    ::  form,message_char1b,message_char2b
   character(*),intent(in)    ::  destination
   integer,intent(in)         ::  message_intb
   character(slen)            ::  display

   if (form=='') then
      write(display,*)message_char1b(1:min(len(message_char1b),len_trim(message_char1b)+1)), &
         message_intb,trim(message_char2b)
   else
      write(display,form)message_char1b(1:min(len(message_char1b),len_trim(message_char1b)+1)), &
         message_intb,trim(message_char2b)
   endif

   call writelog_distribute(destination, display)

   end subroutine writelog_aia

   subroutine writelog_aaai(destination,form,message_char1,message_char2,message_char3,message_int)
   implicit none
   character(*),intent(in)    ::  form,message_char1,message_char2,message_char3
   character(*),intent(in)    ::  destination
   integer,intent(in)         ::  message_int
   character(slen)            ::  display

   if (form=='') then
      write(display,*)message_char1(1:min(len(message_char1),len_trim(message_char1)+1)), &
         message_char2(1:min(len(message_char2),len_trim(message_char2)+1)), &
         message_char3(1:min(len(message_char3),len_trim(message_char3)+1)), &
         message_int
   else
      write(display,form)message_char1(1:min(len(message_char1),len_trim(message_char1)+1)), &
         message_char2(1:min(len(message_char2),len_trim(message_char2)+1)), &
         message_char3(1:min(len(message_char3),len_trim(message_char3)+1)), &
         message_int
   endif

   call writelog_distribute(destination, display)

   end subroutine writelog_aaai

   subroutine writelog_aaia(destination,formb,message_char1b,message_char2b,message_int,message_char3b)
   implicit none
   character(*),intent(in)    ::  formb,message_char1b,message_char2b,message_char3b
   character(*),intent(in)    ::  destination
   integer,intent(in)         ::  message_int
   character(slen)            ::  display

   if (formb=='') then
      write(display,*)message_char1b(1:min(len(message_char1b),len_trim(message_char1b)+1)), &
         message_char2b(1:min(len(message_char2b),len_trim(message_char2b)+1)), &
         message_int,trim(message_char3b)
   else
      write(display,formb)message_char1b(1:min(len(message_char1b),len_trim(message_char1b)+1)), &
         message_char2b(1:min(len(message_char2b),len_trim(message_char2b)+1)), &
         message_int,trim(message_char3b)
   endif

   call writelog_distribute(destination, display)

   end subroutine writelog_aaia

   subroutine writelog_aiaa(destination,form,message_char1b,message_intb,message_char2b,message_char3b)
   implicit none
   character(*),intent(in)    ::  form,message_char1b,message_char2b,message_char3b
   character(*),intent(in)       ::  destination
   integer,intent(in)         ::  message_intb
   character(slen)            ::  display

   if (form=='') then
      write(display,*)message_char1b(1:min(len(message_char1b),len_trim(message_char1b)+1)), &
         message_intb, &
         message_char2b(1:min(len(message_char2b),len_trim(message_char2b)+1)), &
         trim(message_char3b)
   else
      write(display,form)message_char1b(1:min(len(message_char1b),len_trim(message_char1b)+1)), &
         message_intb, &
         message_char2b(1:min(len(message_char2b),len_trim(message_char2b)+1)), &
         trim(message_char3b)
   endif

   call writelog_distribute(destination, display)

   end subroutine writelog_aiaa

   subroutine writelog_aiaaa(destination,form,message_char1b,message_intb,message_char2b,message_char3b,message_char4b)
   implicit none
   character(*),intent(in)    ::  form,message_char1b,message_char2b,message_char3b,message_char4b
   character(*),intent(in)       ::  destination
   integer,intent(in)         ::  message_intb
   character(slen)            ::  display

   if (form=='') then
      write(display,*)message_char1b(1:min(len(message_char1b),len_trim(message_char1b)+1)), &
         message_intb, &
         message_char2b(1:min(len(message_char2b),len_trim(message_char2b)+1)), &
         message_char3b(1:min(len(message_char3b),len_trim(message_char3b)+1)), &
         trim(message_char4b)
   else
      write(display,form)message_char1b(1:min(len(message_char1b),len_trim(message_char1b)+1)), &
         message_intb, &
         message_char2b(1:min(len(message_char2b),len_trim(message_char2b)+1)), &
         message_char3b(1:min(len(message_char3b),len_trim(message_char3b)+1)), &
         trim(message_char4b)
   endif

   call writelog_distribute(destination, display)

   end subroutine writelog_aiaaa


   subroutine writelog_aiai(destination,form,message_char1,message_int1,message_char2,message_int2)
   implicit none
   character(*),intent(in)    ::  form,message_char1,message_char2
   character(*),intent(in)       ::  destination
   integer,intent(in)         ::  message_int1,message_int2
   character(slen)            ::  display

   if (form=='') then
      write(display,*)message_char1(1:min(len(message_char1),len_trim(message_char1)+1)), &
         message_int1, &
         message_char2(1:min(len(message_char2),len_trim(message_char2)+1)), &
         message_int2
   else
      write(display,form)message_char1(1:min(len(message_char1),len_trim(message_char1)+1)), &
         message_int1, &
         message_char2(1:min(len(message_char2),len_trim(message_char2)+1)), &
         message_int2
   endif

   call writelog_distribute(destination, display)

   end subroutine writelog_aiai

   subroutine writelog_aiaia(destination,form,mc1,mi1,mc2,mi2,mc3)
   implicit none
   character(*),intent(in)    ::  form,mc1,mc2,mc3
   character(*),intent(in)       ::  destination
   integer,intent(in)         ::  mi1,mi2
   character(slen)            ::  display

   if (form=='') then
      write(display,*)mc1(1:min(len(mc1),len_trim(mc1)+1)), &
         mi1, &
         mc2(1:min(len(mc2),len_trim(mc2)+1)), &
         mi2, &
         trim(mc3)
   else
      write(display,form)mc1(1:min(len(mc1),len_trim(mc1)+1)), &
         mi1, &
         mc2(1:min(len(mc2),len_trim(mc2)+1)), &
         mi2, &
         trim(mc3)
   endif

   call writelog_distribute(destination, display)

   end subroutine writelog_aiaia

   subroutine writelog_aaiai(destination,form,message_char1,message_char2,message_i1,message_char3,message_i2)
   implicit none
   character(*),intent(in)    ::  form,message_char1,message_char2,message_char3
   character(*),intent(in)       ::  destination
   integer*4,intent(in)          ::  message_i1,message_i2
   character(slen)            ::  display

   if (form=='') then
      write(display,*)message_char1(1:min(len(message_char1),len_trim(message_char1)+1)), &
         message_char2(1:min(len(message_char2),len_trim(message_char2)+1)), &
         message_i1, &
         message_char3(1:min(len(message_char3),len_trim(message_char3)+1)), &
         message_i2
   else
      write(display,form)message_char1(1:min(len(message_char1),len_trim(message_char1)+1)), &
         message_char2(1:min(len(message_char2),len_trim(message_char2)+1)), &
         message_i1, &
         message_char3(1:min(len(message_char3),len_trim(message_char3)+1)), &
         message_i2
   endif

   call writelog_distribute(destination, display)

   end subroutine writelog_aaiai

   subroutine writelog_aaaiai(destination,form,message_char1,message_char2,message_char3,message_i1,message_char4,message_i2)
   implicit none
   character(*),intent(in)    ::  form,message_char1,message_char2,message_char3,message_char4
   character(*),intent(in)       ::  destination
   integer*4,intent(in)          ::  message_i1,message_i2
   character(slen)            ::  display

   if (form=='') then
      write(display,*)message_char1(1:min(len(message_char1),len_trim(message_char1)+1)), &
         message_char2(1:min(len(message_char2),len_trim(message_char2)+1)), &
         message_char3(1:min(len(message_char3),len_trim(message_char3)+1)), &
         message_i1, &
         message_char4(1:min(len(message_char4),len_trim(message_char4)+1)), &
         message_i2
   else
      write(display,form)message_char1(1:min(len(message_char1),len_trim(message_char1)+1)), &
         message_char2(1:min(len(message_char2),len_trim(message_char2)+1)), &
         message_char3(1:min(len(message_char3),len_trim(message_char3)+1)), &
         message_i1, &
         message_char4(1:min(len(message_char4),len_trim(message_char4)+1)), &
         message_i2
   endif

   call writelog_distribute(destination, display)

   end subroutine writelog_aaaiai

   subroutine writelog_aiafa(destination,form,mc1,mi1,mc2,mf1,mc3)
   implicit none
   character(*),intent(in)    ::  form,mc1,mc2,mc3
   character(*),intent(in)       ::  destination
   integer*4,intent(in)          ::  mi1
   real*8,intent(in)             ::  mf1
   character(slen)            ::  display

   if (form=='') then
      write(display,*)mc1(1:min(len(mc1),len_trim(mc1)+1)), &
         mi1, &
         mc2(1:min(len(mc2),len_trim(mc2)+1)), &
         mf1,trim(mc3)
   else
      write(display,form)mc1(1:min(len(mc1),len_trim(mc1)+1)), &
         mi1, &
         mc2(1:min(len(mc2),len_trim(mc2)+1)), &
         mf1,trim(mc3)
   endif

   call writelog_distribute(destination, display)

   end subroutine writelog_aiafa

   subroutine writelog_aiafaf(destination,form,mc1,mi1,mc2,mf1,mc3,mf2)
   implicit none
   character(*),intent(in)    ::  form,mc1,mc2,mc3
   character(*),intent(in)       ::  destination
   integer*4,intent(in)          ::  mi1
   real*8,intent(in)             ::  mf1,mf2
   character(slen)            ::  display

   if (form=='') then
      write(display,*)mc1(1:min(len(mc1),len_trim(mc1)+1)), &
         mi1, &
         mc2(1:min(len(mc2),len_trim(mc2)+1)), &
         mf1, &
         mc3(1:min(len(mc3),len_trim(mc3)+1)), &
         mf2
   else
      write(display,form)mc1(1:min(len(mc1),len_trim(mc1)+1)), &
         mi1, &
         mc2(1:min(len(mc2),len_trim(mc2)+1)), &
         mf1, &
         mc3(1:min(len(mc3),len_trim(mc3)+1)), &
         mf2
   endif

   call writelog_distribute(destination, display)

   end subroutine writelog_aiafaf

   subroutine writelog_aiaiai(destination,form,message_char1,message_i1,message_char2,message_i2,message_char3,message_i3)
   implicit none
   character(*),intent(in)    ::  form,message_char1,message_char2,message_char3
   character(*),intent(in)    ::  destination
   integer*4,intent(in)       ::  message_i1,message_i2,message_i3
   character(slen)            ::  display

   if (form=='') then
      write(display,*)message_char1(1:min(len(message_char1),len_trim(message_char1)+1)), &
         message_i1, &
         message_char2(1:min(len(message_char2),len_trim(message_char2)+1)), &
         message_i2, &
         message_char3(1:min(len(message_char3),len_trim(message_char3)+1)), &
         message_i3
   else
      write(display,form)message_char1(1:min(len(message_char1),len_trim(message_char1)+1)), &
         message_i1, &
         message_char2(1:min(len(message_char2),len_trim(message_char2)+1)), &
         message_i2, &
         message_char3(1:min(len(message_char3),len_trim(message_char3)+1)), &
         message_i3
   endif

   call writelog_distribute(destination, display)

   end subroutine writelog_aiaiai

   subroutine writelog_aiaiaia(destination,form,mc1,message_i1,mc2,message_i2,mc3,message_i3, &
      mc4)
   implicit none
   character(*),intent(in)    ::  form,mc1,mc2,mc3,mc4
   character(*),intent(in)    ::  destination
   integer*4,intent(in)       ::  message_i1,message_i2,message_i3
   character(slen)            ::  display

   if (form=='') then
      write(display,*)mc1(1:min(len(mc1),len_trim(mc1)+1)), &
         message_i1, &
         mc2(1:min(len(mc2),len_trim(mc2)+1)), &
         message_i2, &
         mc3(1:min(len(mc3),len_trim(mc3)+1)), &
         message_i3,trim(mc4)
   else
      write(display,form)mc1(1:min(len(mc1),len_trim(mc1)+1)), &
         message_i1, &
         mc2(1:min(len(mc2),len_trim(mc2)+1)), &
         message_i2, &
         mc3(1:min(len(mc3),len_trim(mc3)+1)), &
         message_i3,trim(mc4)
   endif

   call writelog_distribute(destination, display)

   end subroutine writelog_aiaiaia

   subroutine writelog_aiaiaf(destination,form,mc1,mi1,mc2,mi2,mc3,mf1)
   implicit none
   character(*),intent(in)    ::  form,mc1,mc2,mc3
   character(*),intent(in)    ::  destination
   integer*4,intent(in)       ::  mi1,mi2
   real*8,intent(in)          ::  mf1
   character(slen)            ::  display

   if (form=='') then
      write(display,*)mc1(1:min(len(mc1),len_trim(mc1)+1)), &
         mi1, &
         mc2(1:min(len(mc2),len_trim(mc2)+1)), &
         mi2, &
         mc3(1:min(len(mc3),len_trim(mc3)+1)), &
         mf1
   else
      write(display,form)mc1(1:min(len(mc1),len_trim(mc1)+1)), &
         mi1, &
         mc2(1:min(len(mc2),len_trim(mc2)+1)), &
         mi2, &
         mc3(1:min(len(mc3),len_trim(mc3)+1)), &
         mf1
   endif

   call writelog_distribute(destination, display)

   end subroutine writelog_aiaiaf

   subroutine writelog_aiaiafa(destination,form,mc1,mi1,mc2,mi2,mc3,mf1,mc4)
   implicit none
   character(*),intent(in)    ::  form,mc1,mc2,mc3,mc4
   character(*),intent(in)    ::  destination
   integer*4,intent(in)       ::  mi1,mi2
   real*8,intent(in)          ::  mf1
   character(slen)            ::  display

   if (form=='') then
      write(display,*)mc1(1:min(len(mc1),len_trim(mc1)+1)), &
         mi1, &
         mc2(1:min(len(mc2),len_trim(mc2)+1)), &
         mi2, &
         mc3(1:min(len(mc3),len_trim(mc3)+1)), &
         mf1,trim(mc4)
   else
      write(display,form)mc1(1:min(len(mc1),len_trim(mc1)+1)), &
         mi1, &
         mc2(1:min(len(mc2),len_trim(mc2)+1)), &
         mi2, &
         mc3(1:min(len(mc3),len_trim(mc3)+1)), &
         mf1,trim(mc4)
   endif

   call writelog_distribute(destination, display)

   end subroutine writelog_aiaiafa

   subroutine writelog_iiiii(destination,form,mi1,mi2,mi3,mi4,mi5)
   implicit none
   character(*),intent(in)    ::  form
   character(*),intent(in)    ::  destination
   integer*4,intent(in)       ::  mi1,mi2,mi3,mi4,mi5
   character(slen)            ::  display

   if (form=='') then
      write(display,*)mi1,mi2,mi3,mi4,mi5
   else
      write(display,form)mi1,mi2,mi3,mi4,mi5
   endif

   call writelog_distribute(destination, display)

   end subroutine writelog_iiiii

   subroutine writelog_af(destination,form,message_char1,message_f1)
   implicit none
   character(*),intent(in)    ::  form, message_char1
   character(*),intent(in)    ::  destination
   real*8,intent(in)          ::  message_f1
   character(slen)            ::  display

   if (form=='') then
      write(display,*)message_char1(1:min(len(message_char1),len_trim(message_char1)+1)), &
         message_f1
   else
      write(display,form)message_char1(1:min(len(message_char1),len_trim(message_char1)+1)), &
         message_f1
   endif

   call writelog_distribute(destination, display)

   end subroutine writelog_af

   subroutine writelog_aaf(destination,form,message_char1,message_char2,message_f1)
   implicit none
   character(*),intent(in)    ::  form,message_char1,message_char2
   character(*),intent(in)       ::  destination
   real*8,intent(in)          ::  message_f1
   character(slen)            ::  display

   if (form=='') then
      write(display,*)message_char1(1:min(len(message_char1),len_trim(message_char1)+1)), &
         message_char2(1:min(len(message_char2),len_trim(message_char2)+1)), &
         message_f1
   else
      write(display,form)message_char1(1:min(len(message_char1),len_trim(message_char1)+1)), &
         message_char2(1:min(len(message_char2),len_trim(message_char2)+1)), &
         message_f1
   endif

   call writelog_distribute(destination, display)

   end subroutine writelog_aaf

   subroutine writelog_afa(destination,form,mc1,mf1,mc2)
   implicit none
   character(*),intent(in)    ::  form,mc1,mc2
   character(*),intent(in)       ::  destination
   real*8,intent(in)          ::  mf1
   character(slen)            ::  display

   if (form=='') then
      write(display,*)mc1(1:min(len(mc1),len_trim(mc1)+1)), &
         mf1,trim(mc2)
   else
      write(display,form)mc1(1:min(len(mc1),len_trim(mc1)+1)), &
         mf1,trim(mc2)
   endif

   call writelog_distribute(destination, display)

   end subroutine writelog_afa

   subroutine writelog_afaf(destination,form,message_char1,message_f1,message_char2,message_f2)
   implicit none
   character(*),intent(in)    ::  form,message_char1,message_char2
   character(*),intent(in)       ::  destination
   real*8,intent(in)          ::  message_f1,message_f2
   character(slen)            ::  display

   if (form=='') then
      write(display,*)message_char1(1:min(len(message_char1),len_trim(message_char1)+1)), &
         message_f1, &
         message_char2(1:min(len(message_char2),len_trim(message_char2)+1)), &
         message_f2
   else
      write(display,form)message_char1(1:min(len(message_char1),len_trim(message_char1)+1)), &
         message_f1, &
         message_char2(1:min(len(message_char2),len_trim(message_char2)+1)), &
         message_f2
   endif

   call writelog_distribute(destination, display)

   end subroutine writelog_afaf

   subroutine writelog_afafa(destination,form,mc1,mf1,mc2,mf2,mc3)
   implicit none
   character(*),intent(in)    ::  form,mc1,mc2,mc3
   character(*),intent(in)       ::  destination
   real*8,intent(in)          ::  mf1,mf2
   character(slen)            ::  display

   if (form=='') then
      write(display,*)mc1(1:min(len(mc1),len_trim(mc1)+1)), &
         mf1, &
         mc2(1:min(len(mc2),len_trim(mc2)+1)), &
         mf2,trim(mc3)
   else
      write(display,form)mc1(1:min(len(mc1),len_trim(mc1)+1)), &
         mf1, &
         mc2(1:min(len(mc2),len_trim(mc2)+1)), &
         mf2,trim(mc3)
   endif

   call writelog_distribute(destination, display)

   end subroutine writelog_afafa

   subroutine writelog_aaaf(destination,form,message_char1,message_char2,message_char3,message_f1)
   implicit none
   character(*),intent(in)    ::  form,message_char1,message_char2,message_char3
   character(*),intent(in)       ::  destination
   real*8,intent(in)          ::  message_f1
   character(slen)            ::  display

   if (form=='') then
      write(display,*)message_char1(1:min(len(message_char1),len_trim(message_char1)+1)), &
         message_char2(1:min(len(message_char2),len_trim(message_char2)+1)), &
         message_char3(1:min(len(message_char3),len_trim(message_char3)+1)), &
         message_f1
   else
      write(display,form)message_char1(1:min(len(message_char1),len_trim(message_char1)+1)), &
         message_char2(1:min(len(message_char2),len_trim(message_char2)+1)), &
         message_char3(1:min(len(message_char3),len_trim(message_char3)+1)), &
         message_f1
   endif

   call writelog_distribute(destination, display)

   end subroutine writelog_aaaf

   subroutine writelog_aafa(destination,form,message_char1b,message_char2b,message_f1b,message_char3b)
   implicit none
   character(*),intent(in)    ::  form,message_char1b,message_char2b,message_char3b
   character(*),intent(in)       ::  destination
   real*8,intent(in)          ::  message_f1b
   character(slen)            ::  display

   if (form=='') then
      write(display,*)message_char1b(1:min(len(message_char1b),len_trim(message_char1b)+1)), &
         message_char2b(1:min(len(message_char2b),len_trim(message_char2b)+1)), &
         message_f1b,trim(message_char3b)
   else
      write(display,form)message_char1b(1:min(len(message_char1b),len_trim(message_char1b)+1)), &
         message_char2b(1:min(len(message_char2b),len_trim(message_char2b)+1)), &
         message_f1b,trim(message_char3b)
   endif

   call writelog_distribute(destination, display)

   end subroutine writelog_aafa

   subroutine writelog_afaaa(destination,form,mc1a,mfa,mc2a,mc3a,mc4a)
   implicit none
   character(*),intent(in)    ::  form,mc1a,mc2a,mc3a,mc4a
   character(*),intent(in)       ::  destination
   real*8,intent(in)          ::  mfa
   character(slen)            ::  display

   if (form=='') then
      write(display,*)mc1a(1:min(len(mc1a),len_trim(mc1a)+1)), &
         mfa, &
         mc2a(1:min(len(mc2a),len_trim(mc2a)+1)), &
         mc3a(1:min(len(mc3a),len_trim(mc3a)+1)), &
         trim(mc4a)
   else
      write(display,form)mc1a(1:min(len(mc1a),len_trim(mc1a)+1)), &
         mfa, &
         mc2a(1:min(len(mc2a),len_trim(mc2a)+1)), &
         mc3a(1:min(len(mc3a),len_trim(mc3a)+1)), &
         trim(mc4a)
   endif

   call writelog_distribute(destination, display)

   end subroutine writelog_afaaa

   subroutine writelog_aafaf(destination,form,message_char1,message_char2,message_f1,message_char3,message_f2)
   implicit none
   character(*),intent(in)    ::  form,message_char1,message_char2,message_char3
   character(*),intent(in)       ::  destination
   real*8,intent(in)          ::  message_f1,message_f2
   character(slen)            ::  display

   if (form=='') then
      write(display,*)message_char1(1:min(len(message_char1),len_trim(message_char1)+1)), &
         message_char2(1:min(len(message_char2),len_trim(message_char2)+1)), &
         message_f1, &
         message_char3(1:min(len(message_char3),len_trim(message_char3)+1)), &
         message_f2
   else
      write(display,form)message_char1(1:min(len(message_char1),len_trim(message_char1)+1)), &
         message_char2(1:min(len(message_char2),len_trim(message_char2)+1)), &
         message_f1, &
         message_char3(1:min(len(message_char3),len_trim(message_char3)+1)), &
         message_f2
   endif

   call writelog_distribute(destination, display)

   end subroutine writelog_aafaf

   subroutine writelog_aaafaf(destination,form,message_char1,message_char2,message_char3,message_f1,message_char4,message_f2)
   implicit none
   character(*),intent(in)    ::  form,message_char1,message_char2,message_char3,message_char4
   character(*),intent(in)       ::  destination
   real*8,intent(in)          ::  message_f1,message_f2
   character(slen)            ::  display

   if (form=='') then
      write(display,*)message_char1(1:min(len(message_char1),len_trim(message_char1)+1)), &
         message_char2(1:min(len(message_char2),len_trim(message_char2)+1)), &
         message_char3(1:min(len(message_char3),len_trim(message_char3)+1)), &
         message_f1, &
         message_char4(1:min(len(message_char4),len_trim(message_char4)+1)), &
         message_f2
   else
      write(display,form)message_char1(1:min(len(message_char1),len_trim(message_char1)+1)), &
         message_char2(1:min(len(message_char2),len_trim(message_char2)+1)), &
         message_char3(1:min(len(message_char3),len_trim(message_char3)+1)), &
         message_f1, &
         message_char4(1:min(len(message_char4),len_trim(message_char4)+1)), &
         message_f2
   endif

   call writelog_distribute(destination, display)

   end subroutine writelog_aaafaf

   subroutine writelog_afafafaf(destination,form,mc1,mf1,mc2,mf2,mc3,mf3,mc4,mf4)
   implicit none
   character(*),intent(in)    ::  form,mc1,mc2,mc3,mc4
   character(*),intent(in)    ::  destination
   real*8,intent(in)          ::  mf1,mf2,mf3,mf4
   character(slen)            ::  display

   if (form=='') then
      write(display,*)mc1(1:min(len(mc1),len_trim(mc1)+1)), &
         mf1, &
         mc2(1:min(len(mc2),len_trim(mc2)+1)), &
         mf2, &
         mc3(1:min(len(mc3),len_trim(mc3)+1)), &
         mf3, &
         mc4(1:min(len(mc4),len_trim(mc4)+1)), &
         mf4
   else
      write(display,form)mc1(1:min(len(mc1),len_trim(mc1)+1)), &
         mf1, &
         mc2(1:min(len(mc2),len_trim(mc2)+1)), &
         mf2, &
         mc3(1:min(len(mc3),len_trim(mc3)+1)), &
         mf3, &
         mc4(1:min(len(mc4),len_trim(mc4)+1)), &
         mf4
   endif

   call writelog_distribute(destination, display)

   end subroutine writelog_afafafaf

   subroutine writelog_illll(destination,form,mi1,ml1,ml2,ml3,ml4)
   implicit none
   character(*),intent(in)    ::  form
   character(*),intent(in)    ::  destination
   integer*4,intent(in)       ::  mi1
   logical,intent(in)         ::  ml1,ml2,ml3,ml4
   character(slen)            ::  display

   if (form=='') then
      write(display,*)mi1,ml1,ml2,ml3,ml4
   else
      write(display,form)mi1,ml1,ml2,ml3,ml4
   endif

   call writelog_distribute(destination, display)

   end subroutine writelog_illll

   subroutine writelog_fa(destination,form,mf1,mc1)
   implicit none
   character(*),intent(in)    ::  form, mc1
   character(*),intent(in)    ::  destination
   real*8,intent(in)          ::  mf1
   character(slen)            ::  display

   if (form=='') then
      write(display,*)mf1,trim(mc1)
   else
      write(display,form)mf1,trim(mc1)
   endif

   call writelog_distribute(destination, display)

   end subroutine writelog_fa

   subroutine writelog_afaiaaa(destination,form,mc1,mf1,mc2,mi1,mc3,mc4,mc5)
   implicit none
   character(*),intent(in)    ::  form, mc1,mc2,mc3,mc4,mc5
   character(*),intent(in)    ::  destination
   real*8,intent(in)          ::  mf1
   integer,intent(in)         ::  mi1
   character(slen)            ::  display

   if (form=='') then
      write(display,*)mc1(1:min(len(mc1),len_trim(mc1)+1)), &
         mf1, &
         mc2(1:min(len(mc2),len_trim(mc2)+1)), &
         mi1, &
         mc3(1:min(len(mc3),len_trim(mc3)+1)), &
         mc4(1:min(len(mc4),len_trim(mc4)+1)), &
         trim(mc5)
   else
      write(display,form)mc1(1:min(len(mc1),len_trim(mc1)+1)), &
         mf1, &
         mc2(1:min(len(mc2),len_trim(mc2)+1)), &
         mi1, &
         mc3(1:min(len(mc3),len_trim(mc3)+1)), &
         mc4(1:min(len(mc4),len_trim(mc4)+1)), &
         trim(mc5)
   endif

   call writelog_distribute(destination, display)

   end subroutine writelog_afaiaaa

   subroutine assignlogdelegate_internal(fPtr)
   use iso_c_binding
   type(c_funptr), VALUE :: fPtr

   distributelog => null()
   if (c_associated(fPtr)) then
      call c_f_procpointer (fPtr, distributelog )
   endif

   end subroutine assignlogdelegate_internal

   subroutine check_file_exist(filename,exist,forceclose)
   use m_xbeach_errorhandling
   implicit none

   character(*)               :: filename
   logical,intent(out),optional :: exist
   logical,intent(in), optional :: forceclose
   logical                    :: endsim
   integer                    :: error

   if (present(forceclose)) then
      endsim = forceclose
   else
      endsim = .true.
   endif

   error = 0
   call check_file_exist_generic(filename,error)

   if (error==1 .and. endsim) then

      call writelog('sle','','File ''',trim(filename),''' not found. Terminating simulation')

      call xbeach_errorhandler()
   endif

   if (present(exist)) then
      if (error==1) then
         exist = .false.
      else
         exist = .true.
      endif
   endif
   end subroutine check_file_exist



   subroutine check_file_length_1D(fname,d1)
   use m_xbeach_errorhandling


   implicit none
   character(*)                   ::  fname
   integer, intent(in)            ::  d1
   integer                        ::  fid,iost
   integer                        ::  i
   real,dimension(:),allocatable  ::  dat


   allocate(dat(d1))
   open(newunit=fid,file=trim(fname))
   read(fid,*,iostat=iost)(dat(i),i=1,d1)
   if (iost .ne. 0) then
      call writelog('sle','','Error processing file ''',trim(fname),'''. File may be too short or contains invalid values.', &
         ' Terminating simulation' )
      call xbeach_errorhandler()
   endif
   close(fid)
   deallocate(dat)


   end subroutine check_file_length_1D

   subroutine check_file_length_2D(fname,d1,d2)
   use m_xbeach_errorhandling


   implicit none
   character(*)                     :: fname
   integer, intent(in)              :: d1,d2
   integer                          :: fid,iost
   integer                          :: i,j
   real,dimension(:,:),allocatable  :: dat



   allocate(dat(d1,d2))
   open(newunit=fid,file=trim(fname))
   read(fid,*,iostat=iost)((dat(i,j),i=1,d1),j=1,d2)
   if (iost .ne. 0) then
      call writelog('sle','','Error processing file ''',trim(fname),'''. File may be too short or contains invalid values.', &
         ' Terminating simulation')
      call xbeach_errorhandler()
   endif
   close(fid)
   deallocate(dat)

   end subroutine check_file_length_2D

   subroutine check_file_length_3D(fname,d1,d2,d3)
   use m_xbeach_errorhandling


   implicit none
   character(*)                       ::  fname
   integer, intent(in)                ::  d1,d2,d3
   integer                            ::  fid,iost
   integer                            ::  i,j,k
   real,dimension(:,:,:),allocatable  ::  dat



   allocate(dat(d1,d2,d3))
   open(newunit=fid,file=trim(fname))
   read(fid,*,iostat=iost)(((dat(i,j,k),i=1,d1),j=1,d2),k=1,d3)
   if (iost .ne. 0) then
      call writelog('esl','Error processing file ''',trim(fname),'''. File may be too short or contains invalid values.', &
         ' Terminating simulation')
      call xbeach_errorhandler()
   endif
   close(fid)
   deallocate(dat)

   end subroutine check_file_length_3D

   subroutine checkbcfilelength(tstop,instat,filename,nspectrumloc,filetype,nonh)

   use m_xbeach_errorhandling

   IMPLICIT NONE
   type fileinfo
      character(slen)  :: fname
      integer          :: nlines
   end type

   real*8, intent(in)          :: tstop
   character(slen), intent(in) :: instat
   character(slen)             :: filename,dummy
   character(slen)             :: testc
   character(len=1)            :: ch
   integer                     :: i,ier=0,nlines,fid,nlocs,ifid,fid2
   real*8                      :: t,dt,total,d1,d2,d3,d4,d5
   type(fileinfo),dimension(:),allocatable :: bcfiles
   integer, intent(in)         :: nspectrumloc
   integer, intent(inout)      :: filetype
   logical,intent(in),optional :: nonh
   logical                     :: lnonh

   if (present(nonh)) then
      lnonh=nonh
   else
      lnonh = .false.
   endif


   ier = 0
   open(newunit=fid,file=trim(filename))
   i=0
   do while (ier==0)
      read(fid,'(a)',iostat=ier)ch
      if (ier==0)i=i+1
   enddo
   nlines=i
   rewind(fid)

   ! test for multiple locations setting
   read(fid,*,iostat=ier)testc
   if (ier .ne. 0) then
      call report_file_read_error(filename)
   endif
   if (trim(testc)=='LOCLIST') then
      if (nspectrumloc<2) then
         call writelog('sle',' ','Error: LOCLIST found in file, but nspectrumloc<2. Change value in surfbeat input file, or change bc specification.')
         call xbeach_errorhandler()
      endif
      nlocs = nlines-1
      allocate(bcfiles(nlocs))
      do ifid = 1,nlocs
         read(fid,*,iostat=ier)d1,d2,bcfiles(ifid)%fname
         if (ier .ne. 0) then
            call report_file_read_error(filename)
         endif
         call check_file_exist(trim(bcfiles(ifid)%fname))
         open(newunit=fid2,file=trim(bcfiles(ifid)%fname))
         i=0
         ier = 0
         do while (ier==0)
            read(fid2,'(a)',iostat=ier)ch
            if (ier==0)i=i+1
         enddo
         close(fid2)
         bcfiles(ifid)%nlines=i
      enddo
   else
      nlocs = 1
      allocate(bcfiles(1))
      bcfiles(1)%fname = filename
      bcfiles(1)%nlines = nlines
   endif
   close(fid)

   do ifid=1,nlocs
      open(newunit=fid,file=trim(bcfiles(ifid)%fname))
      if (trim(instat)=='jons' .or. trim(instat)=='swan' .or. trim(instat)=='vardens') then
         read(fid,*,iostat=ier)testc
         if (ier .ne. 0) then
            call report_file_read_error(bcfiles(ifid)%fname)
         endif
         if (trim(testc)=='FILELIST') then
            filetype = 1
            bcfiles(ifid)%nlines=bcfiles(ifid)%nlines-1
         else
            filetype = 0
         endif
      elseif (trim(instat)=='stat_table' .or. trim(instat)=='jons_table') then
         filetype = 2
      elseif (trim(instat)=='reuse') then
         filetype = 3
      endif

      total=0.d0
      i=0
      select case (filetype)
      case(0)
         total=2.d0*tstop
      case(1)
         do while (total<tstop .and. i<bcfiles(ifid)%nlines)
            read(fid,*,iostat=ier)t,dt,dummy
            if (ier .ne. 0) then
               call report_file_read_error(bcfiles(ifid)%fname)
            endif
            total=total+t
            i=i+1
            call check_file_exist(trim(dummy))
         enddo
      case(2)
         do while (total<tstop .and. i<bcfiles(ifid)%nlines)
            read(fid,*,iostat=ier)d1,d2,d3,d4,d5,t,dt
            if (ier .ne. 0) then
               call report_file_read_error(bcfiles(ifid)%fname)
            endif
            total=total+t
            i=i+1
         enddo
      case (3)
         do while (total<tstop .and. i<bcfiles(ifid)%nlines)
            if (lnonh) then
               read(fid,*,iostat=ier)d1,total,dummy
            else
               read(fid,*,iostat=ier)total,d2,d3,d4,d5,dummy
            endif
            if (ier .ne. 0) then
               call report_file_read_error(bcfiles(ifid)%fname)
            endif
            call check_file_exist(trim(dummy))
            i=i+1
         enddo
      end select
      close(fid)
      if (total<tstop) then
         call writelog('sle',' ','Error: Wave boundary condition time series too short in ',trim(bcfiles(ifid)%fname))
         call writelog('sle','(a,f0.2,a,f0.2)',' Total wave condition time series is ',total, &
            ' but simulation length is ',tstop)
         call writelog('sle',' ','Stopping calculation')
         call xbeach_errorhandler()
      endif
   enddo ! nlocs
   end subroutine checkbcfilelength

   function get_file_length(filename) result (n)

   implicit none

   character(slen), intent(in)             :: filename
   integer                                 :: n
   integer                                 :: io, error, lunfil
   real*8                                  :: temp

   n   = 0
   io  = 0

   if (filename==' ') then
      n = 0
   else
      call check_file_exist_generic(filename, error)

      if (error == 1) then
         n = 0
      else
         open(newunit=lunfil,file=filename)
         do while (io==0)
            n = n + 1
            read(lunfil,*,IOSTAT=io) temp
         enddo
         close(lunfil)
         n = n - 1
      endif
   endif

   end function get_file_length

   subroutine check_file_exist_generic(filename,error)
   implicit none

   character(*), intent(in)   :: filename
   integer, intent(out)       :: error
   logical                    :: file_exists

   inquire(file=filename,exist=file_exists)

   error = 0

   if (.not. file_exists) then
      error = 1
   endif

   end subroutine check_file_exist_generic


   end module m_xbeach_filefunctions
