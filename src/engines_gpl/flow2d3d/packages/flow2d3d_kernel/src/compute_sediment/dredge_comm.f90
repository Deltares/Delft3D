module dredge_comm
contains
subroutine dredgecommunicate(a, n, error, msgstr)
    use precision
    use dfparall, only: parll, dfloat, dfsum
    implicit none
    !
    integer               , intent(in)    :: n      ! length of real array
    real(fp), dimension(n), intent(inout) :: a      ! real array to be accumulated
    logical               , intent(out)   :: error  ! error flag
    character(*)          , intent(out)   :: msgstr ! string to pass message
    !
    if (parll) then
        call dfreduce ( a, n, dfloat, dfsum, error, msgstr )
    else
        error = .false.
        msgstr = ' '
        call dd_dredgecommunicate(a,n)
    endif
end subroutine dredgecommunicate
end module dredge_comm
