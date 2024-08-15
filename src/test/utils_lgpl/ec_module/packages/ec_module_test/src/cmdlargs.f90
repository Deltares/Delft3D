!============================================================================
! retrieving flags or parameters from the command-line:
! argint, argreal, arglogical
! R. Leander
!============================================================================
module m_cmdlargs
implicit none

private

public  ::  argint
public  ::  argreal
public  ::  argstring
public  ::  arglogical

    contains

        function argint(prefix, default) result (i)
        implicit none
        integer         :: i
        integer         :: iargc
        integer         :: jarg
        integer         :: default
        character*(*)   :: prefix
        character*50    :: sarg

        i =default
        iargc = command_argument_count()
        do jarg = 1, iargc
           call get_command_argument( jarg, sarg)
           if(sarg(1:index(sarg,' ')-1).eq.prefix) then
              call get_command_argument( jarg + 1, sarg)
              read(sarg,*,end=233,err=233) i
           endif
 233       continue
        enddo
        end function argint

        function argreal(prefix, default) result (r)
        implicit none
        real            :: r
        integer         :: iargc
        integer         :: jarg
        real            :: default
        character*(*)   :: prefix
        character*50    :: sarg

        r = default
        iargc = command_argument_count()
        do jarg = 1, iargc
           call get_command_argument( jarg, sarg)
           if(sarg(1:index(sarg,' ')-1).eq.prefix) then
              call get_command_argument( jarg + 1, sarg)
              read(sarg,*,end=323,err=323) r
           endif
 323       continue
        enddo
        end function argreal

        function arglogical(prefix) result (l)
!       returns .True. if the prefix is found in ARGV[]
        implicit none
        logical         :: l
        integer         :: iargc
        integer         :: jarg
        character*(*)   :: prefix
        character*50    :: sarg

        l = .False.
        iargc = command_argument_count()
        jarg = 1
        do jarg = 1, iargc
           call get_command_argument( jarg, sarg)
           if(sarg(1:index(sarg,' ')-1).eq.prefix) l=.True.
        enddo
        end function arglogical

        function argstring(prefix, default, s) result (success)
        implicit none
        logical         :: success
        integer         :: iargc
        integer         :: jarg
        character*(*)   :: default
        character*(*)   :: s            !MAX 20 characters!
        character*(*)   :: prefix
        character*50    :: sarg

        s = default
        success = .False.
        iargc = command_argument_count()
        do jarg = 1, iargc
           call get_command_argument( jarg, sarg)
           if(sarg(1:index(sarg,' ')-1).eq.prefix) then
              call get_command_argument( jarg + 1, sarg)
!             read(sarg,*,end=233,err=233) s
              s=sarg                                       !2012-02-19
              success=.True.
           endif
 233       continue
        enddo
        end function argstring

end module m_cmdlargs
