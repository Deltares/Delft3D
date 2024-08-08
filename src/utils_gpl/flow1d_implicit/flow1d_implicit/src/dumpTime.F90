

subroutine dumpTimeInit(fileName)

    ! arguments
    character*(*) :: fileName

    ! shared between routines
    integer(4) :: curTimeStep
    integer   :: fh
    common / dumpInfo / curTimeStep, fh

#if (defined(DO_TIMESTEP_LOGGING))
    curTimeStep = 0
    fh = 111
    open (fh, file=filename)
#endif

end subroutine dumpTimeInit


subroutine dumpTimeClose()

    ! shared between routines
    integer(4) :: curTimeStep
    integer   :: fh,ios
    common / dumpInfo / curTimeStep, fh

#if (defined(DO_TIMESTEP_LOGGING))
    close(fh,iostat=ios)
#endif

end subroutine dumpTimeClose


subroutine dumpTimeIncrement()

    ! shared between routines
    integer(4):: curTimeStep
    integer   :: fh
    common / dumpInfo / curTimeStep, fh

#if (defined(DO_TIMESTEP_LOGGING))
    curTimeStep = curTimeStep + 1
#endif

end subroutine dumpTimeIncrement



subroutine dumpTimeStep(text)

    ! arguments
    character*(*) :: text

    ! shared between routines
    integer(4):: curTimeStep
    integer   :: fh
    common / dumpInfo / curTimeStep, fh


    ! locals
    character(Len=10) :: sysTime    ! string with system time

#if (defined(DO_TIMESTEP_LOGGING))
    call date_and_time ( TIME = sysTime )

    write(fh, '('' '',A2,'':'',A2,'':'',A2,''.'',A3, '' - '', A, '' TS = '', I5)') &
            sysTime(1:2), sysTime(3:4), sysTime(5:6), sysTime(8:10), text, curTimeStep
    flush(fh)
#endif

end subroutine dumpTimeStep


