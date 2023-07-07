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
        integer         :: jarg
        integer         :: default
        character*(*)   :: prefix
        character*50    :: sarg

        i=default
        do jarg=1,iargc()
           call getarg(jarg,sarg)
           if(sarg(1:index(sarg,' ')-1).eq.prefix) then
              call getarg(jarg+1,sarg)
              read(sarg,*,end=233,err=233) i
           endif
 233       continue
        enddo
        end function argint

        function argreal(prefix, default) result (r)
        implicit none
        real            :: r
        integer         :: jarg
        real            :: default
        character*(*)   :: prefix
        character*50    :: sarg

        r=default
        do jarg=1,iargc()
           call getarg(jarg,sarg)
           if(sarg(1:index(sarg,' ')-1).eq.prefix) then
              call getarg(jarg+1,sarg)
              read(sarg,*,end=323,err=323) r
           endif
 323       continue
        enddo
        end function argreal

        function arglogical(prefix) result (l)
!       returns .True. if the prefix is found in ARGV[]
        implicit none
        logical         :: l
        integer         :: jarg
        character*(*)   :: prefix
        character*50    :: sarg

        l=.False.
        jarg=1
        do jarg=1,iargc()
           call getarg(jarg,sarg)
           if(sarg(1:index(sarg,' ')-1).eq.prefix) l=.True.
        enddo
        end function arglogical

        function argstring(prefix, default, s) result (success)
        implicit none
        logical         :: success
        integer         :: jarg
        character*(*)   :: default
        character*(*)   :: s            !MAX 20 characters!
        character*(*)   :: prefix
        character*50    :: sarg

        s=default
        success=.False.
        do jarg=1,iargc()
           call getarg(jarg,sarg)
           if(sarg(1:index(sarg,' ')-1).eq.prefix) then
              call getarg(jarg+1,sarg)
!             read(sarg,*,end=233,err=233) s
              s=sarg                                       !2012-02-19
              success=.True.
           endif
 233       continue
        enddo
        end function argstring

end module m_cmdlargs
