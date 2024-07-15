program test

logical :: finished
integer :: key

write(*,*)'start'

call iscreenopen(' ', 'GR', 800, 600, 256)
call iscreentitle('G','Click in this window to close it')
call inkeyevent(key)
call iscreenclose

write(*,*)'starting infinite loop'
finished = .false.
do while (.not. finished)
enddo

write(*,*)'finish'
end program test
