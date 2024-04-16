module m_delwaq1_const
    integer, parameter :: nlun = 50              !< number of input / output files
    integer, parameter :: noint = 200            !< number of integration options implemented

    integer, parameter :: max_int_size = 2500000        !< default size integer work array
    integer, parameter :: irmax = 10000000       !< default size real work array
    integer, parameter :: max_char_size = 1000000        !< default size character work array

    integer, parameter :: noitm = 11             !< number of items with time-functions
    integer, parameter :: nooutp = 9             !< number of output files
end module m_delwaq1_const
