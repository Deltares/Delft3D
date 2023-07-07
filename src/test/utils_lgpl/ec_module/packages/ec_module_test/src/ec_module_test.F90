!  ec_module_test.f90
!
!  FUNCTIONS:
!  ec_module_test - Minimal Stand-alone test program for EC-module
!

!****************************************************************************
!
!  PROGRAM: ec_module_test
!
!  PURPOSE:  Minimal Stand-alone test program for EC-module (cli) calling basic routines
!  TODO : cmdl args: use old f77 routines, put them in separate module  (argfloat, argint, etc)
!         read test definition using mdu reader (property tree stuff)
!         message in Teamcity style: ##teamcity[...], to recognize test results
!
!****************************************************************************


! ##################################################### MAIN ################################################################
 program ec_module_test
    use properties
    use m_ec_module_test
    use m_cmdlargs
    use tree_data_types
    use tree_structures
    implicit none
    ! command line arguments [--no-compare -c <config-file-name> --new-reference]
    logical                             :: verbose, jacompare, janewref
    character(len=50)                   :: configfilename    !< File name of configuration
    character(len=50)                   :: configfilepath    !< File path of configuration
    type(tree_data), pointer            :: config_ptr
    character(len=100)                  :: startupDir        !< File path of working directory
    character(len=500)                  :: regel
    character(len=50)                   :: testname
    character(len=50)                   :: testlistname
    integer                             :: readerr
    integer                             :: iostat
    logical                             :: multitest, singletest, alltest
    integer                             :: i
    integer                             :: lun

    logical                             :: ja_custom_data_format
    logical                             :: ja_custom_time_format
    logical                             :: stop_on_differ
    logical                             :: is_internal


    if (arglogical('-h')) then
        write(0,*)
        write(0,'(a)') 'ec_module_test'
        write(0,'(a)') 'Test program for ec-module functionality, TeamCity compliant'
        write(0,'(a)') 'syntax: ec_module_test -c <config_file> [ -t <testname> | -l <listfilename>] [-v] [-s] [--no-compare] [--new-reference]'
        write(0,'(a)') '                                        [--data-format <fortran format string> ] [--time-format <fortran format string>]' 
        write(0,'(a)') 'Aruments:   -h                                    show (this) description'
        write(0,'(a)') '            -v                                    verbose'
        write(0,'(a)') '            -s                                    stop current test on difference beyond tolerance'
        write(0,'(a)') '            -c <configfile>                       config file with defined tests (XML)'
        write(0,'(a)') '                                                  may be "internal" for internal unit tests.'
        write(0,'(a)') '            -t <testname>                         name of a single test in the configuration to be run'
        write(0,'(a)') '            -l <listfilename>                     name of a list file with names of tests to be run'
        write(0,'(a)') '            --data-format <fortran format string> specifier for data (get-values) written to out'
        write(0,'(a)') '            --time-format <fortran format string> specifier for time written to out'
        write(0,'(a)') '            --new-reference                       write the new results to the reference location to accept the run as reference'
        write(0,'(a)') '            --no-compare                          skip the comparison step, just run'
        write(0,'(a)')
        write(0,'(a)') 'N.B. The --new-reference flag implies --no-compare'
        write(0,'(a)')
       stop
    endif
    call getcwd(startupDir)                                                 !< save start-up dir
    verbose = arglogical('-v') .or. arglogical('--verbose')                 !< verbosity flag
    stop_on_differ = arglogical('-s') .or. arglogical('--stop-on-differ')   !< stop tests the moment we find a difference beyond tolerance
    janewref = arglogical('--new-reference')
    jacompare = .not.(janewref .or. arglogical('--no-compare'))
    if (.not.argstring('-c','',configfilename)) then
       call TCMessage('*','No config file specified','testFailed')
       stop ''
    endif

    ! supports a custom format string for the output
    ! (because the output can also be used as a reference and in that case, it needs the precision that matches the tolerance!)
    ja_custom_data_format = argstring('--data-format',DATA_FMT,data_format)
    ja_custom_time_format = argstring('--time-format',TIME_FMT,time_format)

    multitest  = argstring('-l','',testlistname)    ! Run a bunch of tests, specified in the listfile
    singletest = argstring('-t','',testname)        ! Run a single test
    alltest    = .not.(multitest .or. singletest)   ! Run all tests in the given configuration file (default)
    is_internal = (configfilename == 'internal')

    ! Open configuration file
    if (is_internal) then
       call do_test_internal
    else
       call tree_create(trim(configfilename), config_ptr)
       call prop_inifile(trim(configfilename), config_ptr, readerr)                       ! without preprocessing the mdu-file
       if ( readerr /= 0 ) then
          call TCMessage(testname,'Cannot open file '//trim(configfilename),'testFailed')
          stop ''
       endif

       if (multitest) then
          open (newunit=lun, file=testlistname, status='OLD', iostat=iostat)
          if (iostat==0) then
             iostat = 0
             do while(.True.)
                read(lun,'(a)',iostat=iostat) regel
                if (is_iostat_end(iostat)) exit
                if (len_trim(regel)>0 .and. regel(1:1)/='#') then
                   read(regel,*) configfilepath, configfilename, testname
                   call chdir(trim(configfilepath))
                   call do_test(verbose, jacompare, janewref, config_ptr, testname, stop_on_differ)   ! Loop over tests
                   call chdir(trim(startupDir))
                endif
             enddo
             close(lun)
          endif
       endif

       if (singletest) then
          call do_test(verbose, jacompare, janewref, config_ptr, testname, stop_on_differ)            ! Single test
       endif

       if (alltest) then
          do i = 1,size(config_ptr%child_nodes)
             testname = tree_get_name(config_ptr%child_nodes(i)%node_ptr )
             call do_test(verbose, jacompare, janewref, config_ptr, testname, stop_on_differ)         ! All tests in the file
          enddo
       endif

       ! Clean up tree
       call tree_destroy(config_ptr)
    endif

 end program ec_module_test
