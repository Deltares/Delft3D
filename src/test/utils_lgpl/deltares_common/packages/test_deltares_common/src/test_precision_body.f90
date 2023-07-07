! Body for the test of the comparison routines

    integer :: result

! Pass two equal numbers
    result = comparereal( 1.0_wp, 1.0_wp )
    call assert_equal( result, 0, 'Passing two equal numbers gives result 0' )

! Pass two numbers that are clearly different
    result = comparereal( 0.5_wp, 1.0_wp )
    call assert_equal( result, -1, 'The first number lower than the second gives -1' )
    result = comparereal( 1.5_wp, 1.0_wp )
    call assert_equal( result, 1, 'The first number greater than the second gives -1' )

! Pass two numbers that are almost the same
    result = comparereal( 1.0_wp, 1.0_wp - epsilon(1.0_wp) )
    call assert_equal( result, 0, 'Passing two almost equal numbers (within the limit) gives 0 (- epsilon)' )
    result = comparereal( 1.0_wp, 1.0_wp + epsilon(1.0_wp) )
    call assert_equal( result, 0, 'Passing two almost equal numbers (within the limit) gives 0 (+ epsilon)' )

    result = comparereal( 1.0_wp, 1.0_wp - epsilon(1.0_wp), 0.25_wp * epsilon(1.0_wp) )
    call assert_equal( result, 1, 'Passing two almost equal numbers (outside the limit) gives 1 (- epsilon)' )
    result = comparereal( 1.0_wp, 1.0_wp + epsilon(1.0_wp), 0.25_wp * epsilon(1.0_wp) )
    call assert_equal( result, -1, 'Passing two almost equal numbers (outside the limit) gives -1 (+ epsilon)' )

! Pass two numbers that are almost the same - different scale
    result = comparereal( 100.0_wp, 100.0_wp - 100.0_wp * epsilon(1.0_wp) )
    call assert_equal( result, 0, 'Passing two almost equal large numbers (within the limit) gives 0 (- epsilon)' )
    result = comparereal( 100.0_wp, 100.0_wp + 100.0_wp * epsilon(1.0_wp) )
    call assert_equal( result, 0, 'Passing two almost equal large numbers (within the limit) gives 0 (+ epsilon)' )

    result = comparereal( 100.0_wp, 100.0_wp - 100.0_wp * epsilon(1.0_wp), 0.25_wp * epsilon(1.0_wp) )
    call assert_equal( result, 1, 'Passing two almost equal large numbers (outside the limit) gives 1 (- epsilon)' )
    result = comparereal( 100.0_wp, 100.0_wp + 100.0_wp * epsilon(1.0_wp), 0.25_wp * epsilon(1.0_wp) )
    call assert_equal( result, -1, 'Passing two almost equal large numbers (outside the limit) gives -1 (+ epsilon)' )

