      module M_control

      implicit none
      
      CHARACTER*256   IODir
      CHARACTER*26    Procs, Deps, IniDeps
      INTEGER   ::    NrDep, NrProc, IniNrDep
      LOGICAL   ::    ProcFirst = .false.
      logical   ::    IniFirst  = .false.    !hk: is this ok?

      end module M_control
      
      SUBROUTINE INITFP (FirstProc, Initmode)

      implicit none

      logical                        :: firstproc
      logical                        :: initmode
      
      RETURN
      END


      SUBROUTINE INITCT (CmdLine, Id, Status)
      
      implicit none
      
      CHARACTER*(*) CmdLine
      INTEGER*4     Id
      INTEGER*2     Status

      Status = 0
      !HVP Temporarily, since file i/o will vanish
      Id = 1

      RETURN
      END

      SUBROUTINE CrashCt (Id, selfcrash)
      implicit none
      INTEGER*4 Id
      LOGICAL   Selfcrash

      RETURN
      END


      SUBROUTINE STEPCT (timold, timnew, Id, Status, InitMode, crashed)
      implicit none
      Double Precision  timold, timnew
      INTEGER*4 Id
      INTEGER*2 Status
      LOGICAL   InitMode, Crashed

      RETURN
      END

      SUBROUTINE ENDCT (Id, Status)
      implicit none
      INTEGER*4 Id
      INTEGER*2 Status

      Status = 0

      RETURN
      END
