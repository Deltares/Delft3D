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

!> Specifies the interface for MessageHandling's callback functionality.
! (A bit awkward, but including it in MessageHandling's module header
! did not make it visible in subprograms when using ifort 11.0.)
module MHCallBack
   abstract interface
      subroutine mh_callbackiface(level, message)
         integer, intent(in) :: level !< The severity level
         character(len=*), intent(in) :: message !< log message
      end subroutine mh_callbackiface
   end interface

   abstract interface
      subroutine c_callbackiface(level, msg) bind(C)
        use iso_c_binding
        use iso_c_utils
        integer(c_int), value, intent(in) :: level !< severity
        character(c_char), intent(in) :: msg(MAXSTRINGLEN) !< c message null terminated
      end subroutine c_callbackiface
   end interface

   abstract interface
      subroutine progress_iface(msg, progress)
        character(len=*), intent(in) :: msg !< c message null terminated
        double precision,  intent(in) :: progress
      end subroutine progress_iface
   end interface


   abstract interface
      subroutine progress_c_iface(msg, progress) bind(C)
        use iso_c_binding
        use iso_c_utils
        character(c_char), intent(in) :: msg(MAXSTRINGLEN) !< c message null terminated
        real(c_double), intent(in) :: progress !< progress in fraction
      end subroutine progress_c_iface
   end interface

!> Shows a message in a GUI dialog.
!! This interface is to be used by utility libraries that want to call back
!! a GUI routine in the parent program to display a message box.
   abstract interface
      subroutine msgbox_callbackiface(msg)
        character(len=*), intent(in) :: msg   !< Message string
      end subroutine msgbox_callbackiface
   end interface

end module MHCallBack

!> Diagnostics output module.
!! Prints and/or logs messages from an application.
!! Three variants:
!! -# write2screen: writes directly to stdout
!! -# useLog: log all messages in a message buffer (a queue), can be read out by any other application.
!! -# lunMessages: writes directly to an already opened specified file pointer.
!!
!! See MessageHandling::SetMessageHandling for more details.
!!
!! Messages have a severity level: LEVEL_(DEBUG|INFO|WARN|ERROR|FATAL).
module MessageHandling
   use MHCallBack
   use iso_c_utils
   implicit none

   procedure(progress_iface), pointer :: progress_callback => null()
   procedure(progress_c_iface), pointer :: progress_c_callback => null()

   !> Callback routine invoked upon any mess/err (i.e. SetMessage)
   procedure(msgbox_callbackiface), pointer :: msgbox_callback => null()


   integer, parameter, public    :: BUFLEN = 1024
   !> The message buffer allows you to write any number of variables in any
   !! order to a character string. Call msg_flush or err_flush to output
   !! the actual message or error.
   character(len=MAXSTRINGLEN), public :: msgbuf
   character(len=MAXSTRINGLEN), public :: errmsg

   public SetMessage
   public GetMessageCount
   public SetMessageHandling
   public FinalizeMessageHandling
   public mess
   public err
   public GetMessage_MH
   public getOldestMessage
   public resetMessageCount_MH
   public getMaxErrorLevel
   public resetMaxerrorLevel
   public set_logger
   public set_progress_c_callback
   public set_mh_callback
   public set_msgbox_callback
   public msgbox
   public msg_flush
   public dbg_flush
   public warn_flush
   public err_flush
   public fatal_flush
   public set_progress_callback
   public progress
   public stringtolevel

   integer,parameter, public     :: LEVEL_ALL   = 0
   integer,parameter, public     :: LEVEL_DEBUG = 1
   integer,parameter, public     :: LEVEL_INFO  = 2
   integer,parameter, public     :: LEVEL_WARN  = 3
   integer,parameter, public     :: LEVEL_ERROR = 4
   integer,parameter, public     :: LEVEL_FATAL = 5
   integer,parameter, public     :: LEVEL_NONE  = 6
   integer,parameter, public     :: Idlen = 256  !< Max string length of Ids. Recommended to use one character less for the actual Id, to allow for a null char at the end, when interfacing with C.
   integer,parameter, public     :: max_level = 5
   character(len=12), dimension(max_level), private    :: level_prefix = (/'** DEBUG  : ',  &
                                                                           '** INFO   : ',  &
                                                                           '** WARNING: ',  &
                                                                           '** ERROR  : ',  &
                                                                           '** FATAL  : '/)
   character(len=12), public              :: space12 = ' '
   integer, dimension(max_level), public  :: mess_level_count


   interface mess
   module procedure message1string
   module procedure message2string
   module procedure message3string
   module procedure message4string
   module procedure message1char1real
   module procedure message1char2real
   module procedure message2char1real
   module procedure message2char2real
   module procedure message1char1int
   module procedure message1char2int
   module procedure message1char3int
   module procedure message1char1double
   module procedure message2double
   module procedure message2int1char
   module procedure message1char1int1double
   module procedure message1double1int1char
   end interface

   interface err
   module procedure error1char
   module procedure error2char
   module procedure error3char
   module procedure error4char
   module procedure error1char1real
   module procedure error1char2real
   module procedure error2char1real
   module procedure error2char2real
   module procedure error1char1double
   module procedure error1char1int
   module procedure error1char2int
   module procedure error1char1int1double
   end interface

private

   integer               , parameter,              private :: maxMessages = 3000
   integer               ,                         private :: messagecount = 0 !< Number of messages currently in message buffer (queue).
   character(len=MAXSTRINGLEN) , dimension(maxMessages), private :: Messages
   integer               , dimension(maxMessages), private :: Levels
   integer               ,                         private :: ibuffertail  = 0 !< Index of newest message in message buffer.

   integer,                                    private :: maxErrorLevel = 0
   !> The threshold levels: do not emit messages that have a level lower than treshold.
   !! Note: each of the three output channels can be configured with its own treshold level.
   integer,                                    public  :: thresholdLvl_stdout = 0 !< Threshold level specific for stdout channel.
   integer,                                    public  :: thresholdLvl_log    = 0 !< Threshold level specific for the logging queue channel.
   integer,                                    public  :: thresholdLvl_file   = 0 !< Threshold level specific for the file output channel.
   integer,                                    public  :: thresholdLvl_callback   = LEVEL_WARN !< Threshold level specific for the c callback.
   character(len=idlen),                       public  :: prefix_callback   = "kernel" !< prefix specific for the c callback.

   !> For the above threshold levels to become active, each channel must be separately enabled:
   integer, save                  :: lunMess             = 0       !< The file pointer to be used for the file output channel.
   logical, save                  :: writeMessage2Screen = .true.  !< Whether or not to use the stdout channel.
   logical, save                  :: useLogging          = .true.  !< Whether or not to use the logging queue channel.
   logical, save                  :: alreadyInCallback=.false.                   !< flag for preventing recursive calls to callback subroutine

   !> Callback routine invoked upon any mess/err (i.e. SetMessage)
   procedure(mh_callbackiface), pointer :: mh_callback => null()
   procedure(c_callbackiface), pointer :: c_logger => null()

   contains

!> Returns the numeric logging level value for a given level name.
!! The returned level can be used for calling SetMessageHandling(.., thresholdLevel=ilevel,...)
function stringtolevel(levelname) result(ilevel)
   character(len=*), intent(in) :: levelname !< Name of the level, 'DEBUG'/'INFO', etc.
   integer                      :: ilevel    !< The numeric value of the given level

   ilevel = LEVEL_NONE

   select case (trim(levelname))
   case ('ALL')
      ilevel = LEVEL_ALL
   case ('DEBUG')
      ilevel = LEVEL_DEBUG
   case ('INFO')
      ilevel = LEVEL_INFO
   case ('WARN')
      ilevel = LEVEL_WARN
   case ('ERROR')
      ilevel = LEVEL_ERROR
   case ('FATAL')
      ilevel = LEVEL_FATAL
   end select

end function stringtolevel

!> Sets up the output channels and filtering of messages.
!!
!! Three output channels are available:
!! * standard out ("screen output")
!! * a logging queue which can be inquired from your application.
!! * a plain text file
!! All three output channels are optional and can be used in any combination.
!!
!! Messages have a severity level, and each output channel can be filtered with
!! its own threshold level. Note that the threshold level is only active if the
!! output channel has been enabled. See the respective input arguments for enabling.
!!
subroutine SetMessageHandling(write2screen, useLog, lunMessages, callback, thresholdLevel, thresholdLevel_stdout, &
            thresholdLevel_log, thresholdLevel_file, reset_counters, thresholdLevel_callback, prefix_logging)
   logical, optional, intent(in)       :: write2screen           !< Enable stdout: print messages to stdout.
   logical, optional, intent(in)       :: useLog                 !< Enable logging queue: store messages in buffer.
   integer, optional, intent(in)       :: lunMessages            !< Enable file output: nonzero file pointer whereto messages can be written.
   integer, optional, intent(in)       :: thresholdLevel         !< Messages with level lower than the thresholdlevel
                                                                 !< will be discarded. Used as default for all three output channels.
   integer, optional, intent(in)       :: thresholdLevel_stdout  !< Threshold level specific for stdout channel.
   integer, optional, intent(in)       :: thresholdLevel_log     !< Threshold level specific for the logging queue channel.
   integer, optional, intent(in)       :: thresholdLevel_file    !< Threshold level specific for the file output channel.
   integer, optional, intent(in)       :: thresholdLevel_callback!< Threshold level specific for the file output channel.
   logical, optional, intent(in)       :: reset_counters         !< If present and True then reset message counters.
                                                                 !< SetMessageHandling is called more than once.
   character(len=*), optional, intent(in) :: prefix_logging      !< prefix for logging

   procedure(mh_callbackiface), optional :: callback

   if (present(write2screen) ) writeMessage2Screen = write2screen
   if (present(lunMessages) )  lunMess             = lunMessages
   if (present(useLog) )       useLogging          = useLog
   if (present(callback) ) then
      call set_mh_callback(callback)
   endif

   ! For backwards compatibility: if non-specific thresholdLevel is passed, use it for all three channels.
   if (present(thresholdLevel) )  then
      thresholdLvl_stdout = thresholdLevel
      thresholdLvl_log    = thresholdLevel
      thresholdLvl_file   = thresholdLevel
      thresholdLvl_callback = thresholdLevel
   end if

   ! .. but override the threshold level per channel, when given.
   if (present(thresholdLevel_stdout) )  then
      thresholdLvl_stdout = thresholdLevel_stdout
   end if
   if (present(thresholdLevel_log) )  then
      thresholdLvl_log = thresholdLevel_log
   end if
   if (present(thresholdLevel_file) )  then
      thresholdLvl_file = thresholdLevel_file
   end if
   if (present(thresholdLevel_callback) )  then
      thresholdLvl_callback = thresholdLevel_callback
   end if
   if (present(prefix_logging))  then
      prefix_callback = prefix_logging
   end if

   if (present(reset_counters)) then
     if (reset_counters) then
        mess_level_count = 0
        messagecount = 0
        ibuffertail  = 0
     end if
   endif

   alreadyInCallback = .false.

end subroutine SetMessageHandling

subroutine FinalizeMessageHandling()
  close (lunMess)
  lunMess = 0
end subroutine FinalizeMessageHandling

subroutine set_mh_callback(callback)
  procedure(mh_callbackiface) :: callback
  mh_callback => callback
end subroutine set_mh_callback



subroutine set_logger(c_callback) bind(C, name="set_logger")
#ifdef _WIN32
  !DEC$ ATTRIBUTES DLLEXPORT::set_logger
#endif

  use iso_c_binding
  implicit none
  type(c_funptr), value :: c_callback

  ! Set a callback that will be cauled with new messages

  call c_f_procpointer(c_callback, c_logger)
end subroutine set_logger


subroutine set_msgbox_callback(callback)
  procedure(msgbox_callbackiface) :: callback
  msgbox_callback => callback
end subroutine set_msgbox_callback


!> Displays a (GUI) message box by calling a subroutine in the parent program,
!! IF a callback subroutine has been registered.
subroutine msgbox(title, msg, level)
   character(len=*), intent(in) :: title !< Title string
   character(len=*), intent(in) :: msg   !< Message string
   integer,          intent(in) :: level !< Severity level (e.g., LEVEL_ERROR).

   ! call the registered msgbox
   if (associated(msgbox_callback)) then
      call msgbox_callback(msg)
   end if

end subroutine msgbox




!> The main message routine. Puts the message string to all output
!! channels previously set up by SetMessageHandling
recursive subroutine SetMessage(level, string)
  use iso_c_binding
  use iso_c_utils


  integer, intent(in)           :: level  !< One of: LEVEL_(DEBUG|INFO|WARN|ERROR|FATAL).
  character(len=*), intent(in)  :: string !< Complete message string.
  character(c_char)             :: c_string(MAXSTRINGLEN)

  integer :: levelact
  character(len=MAXSTRINGLEN)   :: msg !< message.


  levelact = max(1,min(max_level, level))

  if (level >= 0) then
     ! Always *count* messages, independent from any treshold level.
     mess_level_count(levelact) = mess_level_count(levelact) + 1

     if (level >= thresholdLvl_stdout) then
        if (writeMessage2Screen) then
           write (*, '(a)') level_prefix(levelact)//trim(string)
        end if
     endif

     if (lunMess /= 0) then
        if (level >= thresholdLvl_file) then
           write (lunMess, '(a)') level_prefix(levelact)//trim(string)
        end if
     end if

     if (level > maxErrorLevel) then
        maxErrorLevel = level
     endif

     if (useLogging) then
        if (level >= thresholdLvl_log) then
           call pushMessage(levelact, string)
        endif
     endif
  elseif (level < 0) then

     ! If negative level just put string to all output channels without prefix and counting
     if (writeMessage2Screen) then
        write (*, '(a)') trim(string)
     endif

     if (lunMess /= 0) then
        write (lunMess, '(a)') trim(string)
     end if

  endif

  ! Optional callback routine for any user-specified actions (e.g., upon error)
  if (associated(mh_callback).and. .not. alreadyInCallback) then
     alreadyInCallback = .true.
     call mh_callback(level, trim(string)) !In future, possibly also error #ID
     alreadyInCallback = .false.
  end if

  if (associated(c_logger).and. .not. alreadyInCallback) then
     if (level >= thresholdLvl_callback) then
        alreadyInCallback = .true.
        msg = trim(prefix_callback)//': '//trim(string)
        c_string = string_to_char_array(trim(msg))
        call c_logger(level, c_string)
        alreadyInCallback = .false.
     endif

  end if

end subroutine SetMessage


!> Pushes a new message at the tail of the message queue.
subroutine pushMessage(level, string)
   integer,          intent(in)  :: level  !< One of: LEVEL_(DEBUG|INFO|WARN|ERROR|FATAL).
   character(len=*), intent(in)  :: string !< Complete message string.


   ibuffertail = mod(ibuffertail, maxmessages) + 1
   messagecount = min(maxmessages, messagecount+1)

   messages(ibuffertail)  = string
   levels(ibuffertail)    = level
end subroutine pushMessage


!> Pops the oldest message from the head of the message queue.
!! When the message queue is empty, level=LEVEL_NONE is returned.
subroutine getOldestMessage(level, msg)
   integer,          intent(out) :: level !< Set to the level of the newest message.
   character(len=*), intent(out) :: msg   !< Set to the newest message text.

   integer :: ibufferhead, itrimlen

   if (messagecount == 0) then
      level = LEVEL_NONE
      return
   else
      ! ibufferhead: front element in the message queue, is tail minus count, but notice the cyclic list/queue (mod) and +1 for 1-based index.
      ibufferhead = mod(ibuffertail - messagecount + maxmessages, maxmessages) + 1
      msg = ' '
      itrimlen = min(len(msg), len_trim(messages(ibuffertail))) ! Shortest of actual message and the available space in output variable.

      msg   = messages(ibuffertail)(1:itrimlen)
      level = levels(ibufferhead)
      messagecount = messagecount-1
   end if
end subroutine getOldestMessage


!> Returns the number of messages that are still in the message buffer queue.
!! Note: it is advised to use getOldestMessage to pop messages from the queue.
integer function getMessageCount()
   getMessageCount = messagecount
end function


!> Gets a message from the message buffer queue, without any checks on whether it's there.
!! Returns the integer level as the function's return value, and stores the accompanying message in the message dummy argument.
integer function GetMessage_MH(imessage, message)
   integer,            intent(in)  :: imessage  !< Position in the message queue.
   character(len=200), intent(out) :: message   !< The message text.

   message=messages(imessage)(1:200)
   GetMessage_MH = levels(imessage)
end function


subroutine resetMessageCount_MH()

   messageCount = 0
end subroutine

integer function getMaxErrorLevel()
   getMaxErrorLevel = maxErrorLevel
end function

subroutine resetMaxerrorLevel()
   maxErrorLevel = 0
end subroutine

subroutine message1string(level, w1)
  use iso_c_utils
    character(*)    :: w1
    integer         :: level

    integer                        :: l1
    character(MAXSTRINGLEN)                 :: rec

    rec = ' '
    l1 = len_trim(w1)
    if (l1 > 0) then
        write (rec(1:), '(a)') w1(:l1)
    endif

    call setMessage(level, rec)
end subroutine message1string

subroutine message2string(level, w1, w2)
    character(*) :: w1, w2
    integer         :: level

    integer :: l1, l2
    character(MAXSTRINGLEN) :: rec

    rec = ' '
    l1 = len_trim(w1)
    if (l1 > 0) then
        write (rec(1:), '(a)') w1(:l1)
    else
        l1 = 1
    endif
    l2 = len_trim(w2)
    if (l2 > 0) then
        write (rec(2 + l1:), '(a)') w2(:l2)
    endif

    call SetMessage(level, rec)
end subroutine message2string

subroutine message3string(level, w1, w2, w3)
    character(*) :: w1, w2, w3
    integer         :: level

    integer :: l1, l2, l3
    character(MAXSTRINGLEN) :: rec

    rec = ' '
    l1 = len_trim(w1)
    if (l1 > 0) then
        write (rec(1:), '(a)') w1(:l1)
    else
        l1 = 1
    endif
    l2 = len_trim(w2)
    if (l2 > 0) then
        write (rec(2 + l1:), '(a)') w2(:l2)
    else
        l2 = 1
    endif
    l3 = len_trim(w3)
    if (l3 > 0) then
        write (rec(3 + l1 + l2:), '(a)') w3(:l3)
    endif

    call SetMessage(level, rec)
 end subroutine message3string

subroutine message4string(level, w1, w2, w3, w4)
    character(*) :: w1, w2, w3, w4
    integer         :: level

    integer :: l1, l2, l3, l4
    character(MAXSTRINGLEN) :: rec

    rec = ' '
    l1 = len_trim(w1)
    if (l1 > 0) then
        write (rec(1:), '(a)') w1(:l1)
    else
        l1 = 1
    endif
    l2 = len_trim(w2)
    if (l2 > 0) then
        write (rec(2 + l1:), '(a)') w2(:l2)
    else
        l2 = 1
    endif
    l3 = len_trim(w3)
    if (l3 > 0) then
        write (rec(3 + l1 + l2:), '(a)') w3(:l3)
    else
        l3 = 1
    endif
    l4 = len_trim(w4)
    if (l4 > 0) then
        write (rec(4 + l1 + l2 + l3:), '(a)') w4(:l4)
    endif

    call SetMessage(level, rec)
end subroutine message4string

subroutine message2char1real(level, w1, w2, r3)

    real :: r3
    character(*) :: w1, w2
    intent (in) r3
    integer         :: level

    integer :: l1, l2
    character(MAXSTRINGLEN) :: rec

    rec = ' '
    l1 = len_trim(w1)
    if (l1 > 0) then
        write (rec(1:), '(a)') w1(:l1)
    else
        l1 = 1
    endif
    l2 = len_trim(w2)
    if (l2 > 0) then
        write (rec(2 + l1:), '(a)') w2(:l2)
    else
        l2 = 1
    endif
    write (rec(3 + l1 + l2:), '(f14.6)') r3

    call SetMessage(level, rec)
end subroutine message2char1real

subroutine message2char2real(level, w1, w2, r3, r4)
    real :: r3, r4
    character(*) :: w1, w2
    intent (in) r3, r4
    integer         :: level

    integer :: l1, l2
    character(MAXSTRINGLEN) :: rec

    rec = ' '
    l1 = len_trim(w1)
    if (l1 > 0) then
        write (rec(1:), '(a)') w1(:l1)
    else
        l1 = 1
    endif
    l2 = len_trim(w2)
    if (l2 > 0) then
        write (rec(2 + l1:), '(a)') w2(:l2)
    else
        l2 = 1
    endif
    write (rec(3 + l1 + l2:), '(2f14.6)') r3, r4

    call SetMessage(level, rec)
end subroutine message2char2real

subroutine message1char1real(level, w1, r2)
    real :: r2
    character(*) :: w1
    intent (in) r2
    integer         :: level

    integer :: l1
    character(MAXSTRINGLEN) :: rec

    rec = ' '
    l1 = len_trim(w1)
    if (l1 > 0) then
        write (rec(1:), '(a)') w1(:l1)
    else
        l1 = 1
    endif
    write (rec(2 + l1:), '(F14.6)') r2

    call SetMessage(level, rec)
end subroutine message1char1real

subroutine message1char1double(level, w1, d2)
    double precision, intent(in) :: d2
    character(*) :: w1
    integer         :: level

    integer :: l1
    character(MAXSTRINGLEN) :: rec

    rec = ' '
    l1 = len_trim(w1)
    if (l1 > 0) then
        write (rec(1:), '(a)') w1(:l1)
    else
        l1 = 1
    endif
    write (rec(2 + l1:), '(F14.6)') d2

    call SetMessage(level, rec)
end subroutine message1char1double

subroutine message2double(level, d1, d2)
    double precision, intent(in) :: d1, d2
    integer         :: level

    character(MAXSTRINGLEN) :: rec

    rec = ' '
    write (rec,'(2F20.6)') d1,d2
    call SetMessage(level, rec)
end subroutine message2double

subroutine message1char1int(level, w1, i2)
    integer :: i2
    character(*) :: w1
    intent (in) i2
    integer         :: level

    integer :: l1
    character(MAXSTRINGLEN) :: rec

    rec = ' '
    l1 = len_trim(w1)
    if (l1 > 0) then
        write (rec(1:), '(a)') w1(:l1)
    else
        l1 = 1
    endif
    write (rec(2 + l1:), '(I14)') i2

    call SetMessage(level, rec)
end subroutine message1char1int

subroutine message1char2int(level, w1, i2, i3)
    integer :: i2, i3
    character(*) :: w1
    intent (in) i2, i3
    integer         :: level

    integer :: l1
    character(MAXSTRINGLEN) :: rec

    rec = ' '
    l1 = len_trim(w1)
    if (l1 > 0) then
        write (rec(1:), '(a)') w1(:l1)
    else
        l1 = 1
    endif
    write (rec(2 + l1:), '(2I14)') i2, i3

    call SetMessage(level, rec)

end subroutine message1char2int

subroutine message2int1char(level, i1, i2, w3)
    integer :: i1, i2
    character(*) :: w3
    intent (in) i1, i2
    integer         :: level

    integer :: l3
    character(MAXSTRINGLEN) :: rec

    rec = ' '
    write (rec( 1:28), '(2I14)') i1, i2
    l3 = len_trim(w3)
    if (l3 > 0) then
        write (rec(30:), '(a)') w3(:l3)
    endif

    call SetMessage(level, rec)

end subroutine message2int1char

subroutine message1char3int(level, w1, i2, i3, i4)
    integer :: i2, i3, i4
    character(*) :: w1
    intent (in) i2, i3, i4
    integer        :: level
    integer        :: l1
    character(MAXSTRINGLEN) :: rec

    rec = ' '
    l1 = len_trim(w1)
    if (l1 > 0) then
        write (rec(1:), '(a)') w1(:l1)
    else
        l1 = 1
    endif
    write (rec(2 + l1:), '(3I14)') i2, i3, i4

    call SetMessage(level, rec)
end subroutine message1char3int

subroutine message1char2real(level, w1, r2, r3)
    real         :: r2, r3
    character(*) :: w1
    integer      :: level
    intent(in)   :: level, w1, r2, r3

    integer        :: l1
    character(MAXSTRINGLEN) :: rec

    rec = ' '
    l1 = len_trim(w1)
    if (l1 > 0) then
        write (rec(1:), '(a)') w1(:l1)
    else
        l1 = 1
    endif
    write (rec(2 + l1:), '(2F14.6)') r2, r3

    call SetMessage(level, rec)
end subroutine message1char2real

subroutine message1char1int1double(level, w1, i2, d3)
    integer          :: level
    character(*)     :: w1
    integer          :: i2
    double precision :: d3

    integer :: l1
    character(MAXSTRINGLEN) :: rec

    rec = ' '
    l1 = len_trim(w1)
    if (l1 > 0) then
        write (rec(1:), '(a)') w1(:l1)
    else
        l1 = 1
    endif
    write (rec(2 + l1:), '(i14)') i2
    write (rec(16 + l1:), '(F14.6)') d3

    call SetMessage(level, rec)
end subroutine message1char1int1double

subroutine message1double1int1char(level, d1, i2, w3)
    integer          :: level
    character(*)     :: w3
    integer          :: i2
    double precision :: d1

    integer :: l3
    character(MAXSTRINGLEN) :: rec

    rec = ' '
    write (rec(1 :16), '(F16.6)') d1
    write (rec(18:31), '(i14)'  ) i2
    l3 = len_trim(w3)
    if (l3 > 0) then
        write (rec(33:), '(a)') w3(:l3)
    endif

    call SetMessage(level, rec)
end subroutine message1double1int1char
!-- Error interfaces ----------------------------
subroutine error4char(w1, w2, w3, w4)
    character(*) :: w1, w2, w3, w4

    call mess(LEVEL_ERROR, w1, w2, w3, w4)
end subroutine error4char

subroutine error3char(w1, w2, w3)
    character(*) :: w1, w2, w3

    call mess(LEVEL_ERROR, w1, w2, w3)
end subroutine error3char

subroutine error2char(w1, w2)
    character(*) :: w1, w2

    call mess(LEVEL_ERROR, w1, w2)
end subroutine error2char

subroutine error1char(w1)
    character(*) :: w1

    call mess(LEVEL_ERROR, w1)
end subroutine error1char

subroutine error2char1real(w1, w2, r3)
    real :: r3
    character(*) :: w1, w2

    call mess(LEVEL_ERROR, w1, w2, r3)
end subroutine error2char1real

subroutine error2char2real(w1, w2, r3, r4)
    real :: r3, r4
    character(*) :: w1, w2

    call mess(LEVEL_ERROR, w1, w2, r3, r4)
end subroutine error2char2real

subroutine error1char1real(w1, r2)
    real :: r2
    character(*) :: w1

    call mess(LEVEL_ERROR, w1, r2)
end subroutine error1char1real

subroutine error1char1double(w1, d2)
    double precision, intent(in) :: d2
    character(*) :: w1

    call mess(LEVEL_ERROR, w1, d2)
end subroutine error1char1double

subroutine error1char1int(w1, i2)
    integer :: i2
    character(*) :: w1

    call mess(LEVEL_ERROR, w1, i2)
end subroutine error1char1int

subroutine error1char2real(w1, r2, r3)
    real :: r2, r3
    character(*) :: w1

    call mess(LEVEL_ERROR, w1, r2, r3)
end subroutine error1char2real

subroutine error1char2int(w1, i2, i3)
    integer :: i2, i3
    character(*) :: w1

    call mess(LEVEL_ERROR, w1, i2, i3)
end subroutine error1char2int

subroutine error1char1int1double(w1, i2, d3)
    character(*)     :: w1
    integer          :: i2
    double precision :: d3

    call mess(LEVEL_ERROR, w1, i2, d3)
end subroutine error1char1int1double

!> Output the current message buffer as a 'debug' message.
subroutine dbg_flush()
    call mess(LEVEL_DEBUG,  msgbuf)
end subroutine dbg_flush

!!> Output the current message buffer as an 'info' message.
subroutine msg_flush()
    call mess(LEVEL_INFO,  msgbuf)
end subroutine msg_flush

!> Output the current message buffer as a 'warning' message.
subroutine warn_flush()
    call mess(LEVEL_WARN,  msgbuf)
end subroutine warn_flush

!> Output the current message buffer as an 'error' message.
subroutine err_flush()
    call mess(LEVEL_ERROR, msgbuf)
end subroutine err_flush

!> Output the current message buffer as a 'fatal' message.
subroutine fatal_flush()
    call mess(LEVEL_FATAL, msgbuf)
end subroutine fatal_flush

!
!
!!--------------------------------------------------------------------------------------------------
!
!
subroutine progress(message, fraction)
  use iso_c_binding
  use iso_c_utils
  implicit none

  character(len=*), intent(in) :: message
  double precision, intent(in) :: fraction

  ! call the registered progress bar
  if (associated(progress_callback)) then
     call progress_callback(message, fraction)
  end if

  ! call the c callback if registered
  if (associated(progress_c_callback)) then
     call progress_c_callback(string_to_char_array(message), fraction)
  end if

end subroutine progress
!
subroutine set_progress_callback(callback)
  ! set the progress handler
  procedure(progress_iface) :: callback

  ! TODO check if we need cptr2fptr
  progress_callback => callback

end subroutine set_progress_callback

subroutine set_progress_c_callback(c_callback) bind(C, name="set_progress_c_callback")
#ifdef _WIN32
  !DEC$ ATTRIBUTES DLLEXPORT:: set_progress_c_callback
#endif

  use iso_c_binding
  use iso_c_utils
  implicit none
  type(c_funptr) :: c_callback

  ! Set a callback that will be cauled with new messages

  call c_f_procpointer(c_callback, progress_c_callback)
end subroutine set_progress_c_callback




end module MessageHandling
