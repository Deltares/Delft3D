!  m_ec_module_test.f90
!
!  FUNCTIONS:
!  ec_module_test - Minimal Stand-alone test program for EC-module
!

!****************************************************************************
!
!  MODULE: m_ec_module_test
!
!  PURPOSE:  Minimal Stand-alone test program for EC-module (cli) calling basic routines
!  TODO : cmdl args: use old f77 routines, put them in separate module  (argfloat, argint, etc)
!         read test definition using mdu reader (property tree stuff)
!         message in Teamcity style: ##teamcity[...], to recognize test results
!
!****************************************************************************

 module m_ec_module_test
    use precision
    use m_alloc
    use m_ec_module
    use m_ec_provider
    use properties
    use string_module
    use time_class
    use m_ec_unit_tests
    use tree_data_types
    use tree_structures

    implicit none

    type tEcTestDef
      character(len=100) :: name
      character(len=300):: testpathname = '.'         ! chdir to this directory when started
      character(len=300) :: quantityname
      character(len=300) :: locationname
      character(len=300) :: extFilename = ''          ! if ext-file name is specified, ignore inFilename and method
      character(len=300) :: refFilename
      character(len=300) :: outFilename
      character(len=300) :: inFilename
      character(len=300) :: forcingfile = ''
      integer           :: inFiletype
      integer           :: method
      integer           :: npoint, ntimes
      real(hp), dimension(:), allocatable :: x,y,t
      real(hp), dimension(:), allocatable :: x2,y2
      integer           :: operand
      integer           :: vectormax
      integer           :: tgt_refdate
      real(kind=hp)     :: tgt_tzone
      integer           :: tgt_tunit
      logical           :: jasferic = .False.
      real(kind=hp)     :: missing_value = ec_undef_hp
      integer           :: targetItem = ec_undef_int


      real(kind=hp), dimension(:), allocatable :: resultVector
      real(kind=hp), dimension(:), allocatable :: referenceVector
      real(kind=hp)     :: tolerance
      integer           :: ndxstart = -1
      integer           :: ndxend= -1
      logical           :: useBcBlockApproach = .false. !< First read all bc-headers (like in CF_DLL)?
    end type

!   character(len=*), parameter :: TIME_FMT = '(e12.4$)'
!   character(len=*), parameter :: DATA_FMT = '(e20.10$)'
    character(len=*), parameter :: TIME_FMT = '(f14.8$)'
    character(len=*), parameter :: DATA_FMT = '(f12.5$)'

    character(len=66) :: time_format
    character(len=66) :: data_format

private

   public :: do_test
   public :: do_test_internal
   public :: TIME_FMT
   public :: DATA_FMT
   public :: time_format
   public :: data_format
   public :: TCMessage

   ! file handles
   integer :: logLun = -1
   integer :: treeTstLun = -1
   integer :: treeRefLun = -1
   integer :: testOutLun = -1
   integer :: testRefLun = -1

   logical :: logLun_open     = .False.
   logical :: treeTstLun_open = .False.
   logical :: treeRefLun_open = .False.
   logical :: testOutLun_open = .False.
   logical :: testRefLun_open = .False.

contains

    subroutine do_test(verbose, jacompare, janewref, config_ptr, testname, stop_on_differ)
       implicit none
       logical, intent(in)                 :: verbose, jacompare, janewref
       type(tree_data), pointer            :: config_ptr
       character(len=*), intent(in)        :: testname
       logical, intent(in)                 :: stop_on_differ

       logical                             :: logging
       logical                             :: success
       logical                             :: comparison_failed
       type(tEcInstance), pointer, save    :: ecInstancePtr !< Local EC instance
       integer, dimension(:), allocatable  :: itemIDs       !< Connection available outside to which one can connect target items
       integer                             :: ftree, fext
       integer                             :: it, id, iext, ntgt
       character(len=100)                  :: treeTstFile   !< dumped EC-tree
       character(len=100)                  :: treeRefFile   !< referenced EC-tree
       real(kind=hp), dimension(:),   allocatable :: targetArray
       real(kind=hp), dimension(:,:), allocatable :: targetArray_ref
       type(tEcTestDef)                    :: tst
       integer                             :: iostat, result_size, bc_count
       character(len=666)                  :: regel
       real(kind=hp)                       :: time
       integer                             :: ref_start, ref_end
       real(hp), dimension(:,:), allocatable :: xyen
       character(len=666)                  :: oldcwd
       character(len=666)                  :: tmpcwd
       integer                             :: chdir
       integer                             :: isrc, nsrc
       type(tree_data), pointer            :: ext_ptr, node_ptr
       logical                             :: has_forcingfile
       logical                             :: results_differ
       character(len=50)                   :: timestr
       type(c_time)                        :: ec_time

      character(len=300):: quantityname, inFilename, inFiletype, method, operand, forcingfile
      integer           :: infiletype_nr, method_nr, operand_nr


       call TCMessage(testname,'','testStarted')

       ! Activate EC-logging if desired
       if (verbose) then
          open(newunit=logLun, file=trim(testname)//'.log', status='REPLACE', iostat=iostat)
          if (iostat/=0) then
             call TCMessage(testname,'Cannot open '//trim(testname)//'.log for logging ...','testFailed')
             logging = .False.
          else
             logLun_open = .True.
             logging = .True.
          endif
       else
          logging = .false.
       endif

       ! Create and initialize EC instance
       if (.not. ecCreateInstance(ecInstancePtr)) then
          call TCMessage(testname,'Cannot create EC-Module instance','testFailed')
       end if

       ! Read job description from file
       if (.not.getTestInput(config_ptr, tst, testname)) return

       if (tst%jasferic) then
          ecInstancePtr%coordsystem = EC_COORDS_SFERIC
       else
          ecInstancePtr%coordsystem = EC_COORDS_CARTESIAN
       endif

       call getcwd(oldcwd)
       if (len_trim(oldcwd)>0) then
          iostat = chdir(trim(tst%testpathname))         ! relatively change to designated directory
          if (iostat/=0) then
             call TCMessage(testname,'Could not descend into '//trim(tst%testpathname)//' ...','testFailed')
             return
          endif
       endif
       call getcwd(tmpcwd)
       call TCMessage(testname,'path='//trim(tmpcwd),'testRunning')

       ntgt = 3                                ! find out what the number of target items should be or from config

       if (tst%useBcBlockApproach) then
           ntgt = 2                            ! In this case, always two items: one source item (2) and one target item (1, time interpolated)
           call realloc(itemIDs,ntgt)
           itemIDs = ec_undef_int              ! initialize itemID's, assume we are not using existing targets
           ! parse block headers in bc-file
           bc_count = ecCollectBCBlocks(ecInstancePtr, tst%inFilename, iostat)
           if (bc_count /= 0 .and. iostat /= 0)then ! boundaries/laterals found, but error in reading them
              call TCMessage(testname,'Could not read boundary blocks from BC file '// trim(tst%inFilename),'testFailed')
              return
           endif
           itemIDs(2) = ecFindItemByQuantityLocation(ecInstancePtr, tst%locationname, tst%quantityname)
           if (itemIDs(2) > 0) then
              itemIDs(1)  = ecCreateTimeInterpolatedItem(ecInstancePtr, itemIDs(2))
           else
              call TCMessage(testname,'Could not find source item '// trim(tst%locationname) // &
                             '.' // trim(tst%quantityname) ,'testFailed')
           endif
       else
          ! Create SOURCE, later we can connect to it from the TARGET side
          ! Make an array of external points, associated with the internal points
          ! These are necessary in polytim items (forcing waterlevel boundaries in FM).
          if (allocated(tst%x2) .and. allocated(tst%y2)) then
              if (size(tst%x2)/=tst%npoint) then
                 call TCMessage(testname,'Array x2 (external points) has the wrong size','testFailed')
                 call ec_test_exception()
                 return
              endif
              if (size(tst%y2)/=tst%npoint) then
                 call TCMessage(testname,'Array y2 (external points) has the wrong size','testFailed')
                 call ec_test_exception()
                 return
              endif
              allocate(xyen(2,tst%npoint))
              do id = 1,tst%npoint
                 xyen(1,id) = tst%x2(id)
                 xyen(2,id) = tst%y2(id)
              enddo
          endif

          if (len_trim(tst%extFilename)==0) then
             call realloc(itemIDs,1)
             itemIDs = ec_undef_int              ! initialize itemID's, assume we are not usng existing targets
             if (len_trim(tst%forcingfile)==0) then
                success = ecModuleAddTimeSpaceRelation(ecInstancePtr, &
                                                       tst%quantityname, &
                                                       tst%x, tst%y, &
                                                       tst%vectormax, &
                                                       tst%inFilename, tst%inFiletype, &
                                                       tst%method, tst%operand, &
                                                       tst%tgt_refdate, tst%tgt_tzone, tst%tgt_tunit, &
                                                       tst%jasferic, tst%missing_value, itemIDs(1:1), &
                                                       xyen=xyen)
             else
                success = ecModuleAddTimeSpaceRelation(ecInstancePtr, &
                                                       tst%quantityname, &
                                                       tst%x, tst%y, &
                                                       tst%vectormax, &
                                                       tst%inFilename, tst%inFiletype, &
                                                       tst%method, tst%operand, &
                                                       tst%tgt_refdate, tst%tgt_tzone, tst%tgt_tunit, &
                                                       tst%jasferic, tst%missing_value, itemIDs(1:1), &
                                                       forcingfile=trim(tst%forcingfile), &
                                                       xyen=xyen)
             endif
             if (.not.success) then
                call TCMessage(testname,'AddTimeSpaceRelation failed','testFailed',details=dumpECMessageStack(0,ec_test_callback_msg))
                call ec_test_exception
                return
             endif
          else                                         ! read the EXT-file into a tree
             open(newunit=fext,file=trim(tst%extFilename))
             call tree_create(trim(tst%extFilename), ext_ptr)
             call prop_inifile(trim(tst%extFilename), ext_ptr, iostat)
             close(fext)
             if ( iostat /= 0 ) then
                call TCMessage(testname,'Cannot open file '//trim(tst%extFilename),'testFailed')
                call ec_test_exception
                return
             endif

             nsrc = 0
             do iext = 1,size(ext_ptr%child_nodes)
                if (trim(tree_get_name(node_ptr)) == 'Forcing') then
                   nsrc = nsrc + 1
                endif
             enddo

             call realloc(itemIDs,nsrc)
             itemIDs = ec_undef_int              ! initialize itemID's, assume we are not usng existing targets
             isrc = 0
             do iext = 1,size(ext_ptr%child_nodes)
                if (trim(tree_get_name(node_ptr)) == 'Forcing') then
                   node_ptr => ext_ptr%child_nodes(iext)%node_ptr
                   call prop_get(node_ptr, '', 'Quantity', quantityname, success)
                   call prop_get(node_ptr, '', 'BCFile', forcingfile, success)
                   has_forcingfile=success
                   call prop_get(node_ptr, '', 'Filename', inFilename, success)
                   call prop_get(node_ptr, '', 'Filetype', inFiletype, success)
                   inFiletype_nr = getECFileTypeNumber(inFiletype)
                   call prop_get(node_ptr, '', 'Method', method, success)
                   method_nr = getECFileTypeNumber(method)
                   call prop_get(node_ptr, '', 'Operand', operand, success)
                   operand_nr = getECFileTypeNumber(operand)
                   isrc = isrc + 1

                   !if (config%inFileType == ec_undef_int) then
                   !    return
                   !endif
                   !if (method == ec_undef_int) then
                   !    return
                   !endif
                   !if (operand == ec_undef_int) then
                   !    return
                   !endif

                   ! UNDER CONSTRUCTION:
                   ! Multiple external forcings reading from EXT-file
                   if (has_forcingfile) then
                      success = ecModuleAddTimeSpaceRelation(ecInstancePtr, &
                                                             quantityname, &
                                                             tst%x, tst%y, &
                                                             tst%vectormax, &
                                                             inFilename, inFiletype_nr, &
                                                             method_nr, operand_nr, &
                                                             tst%tgt_refdate, tst%tgt_tzone, tst%tgt_tunit, &
                                                             tst%jasferic, tst%missing_value, itemIDs(isrc:isrc), &
                                                             xyen=xyen, forcingfile=forcingfile)
                   else
                      success = ecModuleAddTimeSpaceRelation(ecInstancePtr, &
                                                             quantityname, &
                                                             tst%x, tst%y, &
                                                             tst%vectormax, &
                                                             inFilename, inFiletype_nr, &
                                                             method_nr, operand_nr, &
                                                             tst%tgt_refdate, tst%tgt_tzone, tst%tgt_tunit, &
                                                             tst%jasferic, tst%missing_value, itemIDs(isrc:isrc), &
                                                             xyen=xyen)
                   endif
                   if (.not.success) then
                      call TCMessage(testname,'AddTimeSpaceRelation failed','testFailed',details=dumpECMessageStack(0,ec_test_callback_msg))
                      call ec_test_exception
                      return
                   endif
                end if
             enddo
          endif
       endif

       ! Get the list of source items of the returned connection into an integer array
       ! TODO: * Create a method of connection, allocating, filling and returning a list of its target/source itemID's, and make this public in the module
            ! The line below only works when the factor_out=1, which is the case for FM !!
       ! call ec

       ! Inspect the 'tree' of items etc... 'printState'-call
       treeTstFile = trim(tst%name)//'.tree'
       treeRefFile = trim(tst%name)//'.tree.ref'
       if (janewref) then
          open(newunit=ftree,file=trim(treeRefFile),iostat=iostat)
          if (iostat/=0) then
             call TCMessage(testname,'Cannot write EC-module reference tree!','testFailed')
             call ec_test_exception
             return
          endif
          write(*,*) 'Writing new reference file '//trim(treeRefFile)
       else
          open(newunit=ftree,file=trim(treeTstFile),iostat=iostat)
          if (iostat/=0) then
             call TCMessage(testname,'Cannot write EC-module tree!','testFailed')
             call ec_test_exception
             return
          endif
       endif
       call ecInstancePrintState(ecInstancePtr,callback_msg,ftree)
       close(ftree)

       ! try to estimate the result size
       result_size = ecEstimateItemresultSize(ecInstancePtr, itemIDs(1))
       if (result_size<=0) then
          call TCMessage(testname,'returned result size is not larger than zero','testFailed')
          return
       endif
       call realloc(targetArray,result_size)
       
       ! TODO: properly initialize the targetArray       
       targetArray = ec_undef_hp
       
       if (tst%ndxstart==-1) tst%ndxstart = 1
       if (tst%ndxend==-1) tst%ndxend = result_size

       ! If in compare mode, read file with reference values
       if (jacompare) then
          ! inquire first
          open(newunit=testRefLun, file=trim(tst%refFilename), status='OLD', iostat=iostat)
          if (iostat/=0) then
             call TCMessage(testname,'reference file cannot be opened','testFailed')
             return
          else
             testRefLun_open = .True.
          endif
          if (tst%useBcBlockApproach) then
             ref_start = 1
             ref_end   = 1
             tst%npoint = 1
          else
             ref_start = tst%ndxstart
             ref_end   = tst%ndxend
          endif
          if (allocated(targetArray_ref)) deallocate (targetArray_ref)
          allocate (targetArray_ref(ref_start:ref_end,tst%ntimes))
          do it = 1, tst%ntimes
             read(testRefLun,'(a)',iostat=iostat) regel
             if (iostat/=0) then
                call TCMessage(testname,'error in reference file','testFailed')
                call ec_test_exception()
                return
             endif
             read(regel,*,iostat=iostat) time, targetArray_ref(ref_start:ref_end,it)
             if (iostat/=0) then
                call TCMessage(testname,'error in reference file','testFailed')
                call ec_test_exception()
                return
             endif
             if (comparereal(time, tst%t(it), 1d-12) /= 0) then
                call TCMessage(testname,'wrong time in reference file','testFailed')
                call ec_test_exception()
                return
             endif
          enddo
          close (testRefLun)
          testRefLun_open = .False.
       endif

       ! Loop: for all requested times and targets untill failure
       if (janewref) then
          open(newunit=testOutLun, file=trim(tst%refFilename), status='REPLACE', iostat=iostat)
          write(*,*) 'Writing new reference file '//trim(tst%refFilename)
       else
          open(newunit=testOutLun, file=trim(testname)//'.out', status='REPLACE', iostat=iostat)
       endif

       comparison_failed = .false.
       do it = 1, tst%ntimes
          if (tst%useBcBlockApproach) then
             call ec_time%set(tst%t(it))
             success = ecItemGetValues(ecInstancePtr, itemIDs(1), ec_time, targetArray)
          else
             success = ec_gettimespacevalue_by_itemID(ecInstancePtr, itemIDs(1), tst%tgt_refdate, tst%tgt_tzone, tst%tgt_tunit, tst%t(it), targetArray)
          endif
          if (.not.success) then
             call TCMessage(testname,'Error getting value for target item','testFailed',details=dumpECMessageStack(0,ec_test_callback_msg))
             call ec_test_exception
             return
             cycle
          endif

          write(testOutLun,time_format) tst%t(it)
          do id=max(tst%ndxstart,1), min(tst%ndxend,result_size)
             write(testOutLun,'(a$)') ' '
             write(testOutLun,data_format) targetArray(id)
          enddo

          write(testOutLun,*)

          ! compare with reference and verify success
          ! TODO: fix !
          if (jacompare) then
             results_differ = .False.
             do id = 1,tst%npoint
                results_differ = results_differ .or. (abs(targetArray(id)-targetArray_ref(id,it))>tst%tolerance)
             enddo
             if (results_differ) then
                write(timestr,time_format) tst%t(it) 
                comparison_failed = .true.
                if (stop_on_differ) then
                   call TCMessage(testname,'Results differ','testFailed')
                   call ec_test_exception()     ! activate these lines if we want tests to be aborted upon different result
                   return
                else
                   call TCMessage(testname,'Results differ, t = '//trim(timestr),'testFailed')
                end if

                ! Report the difference in the log-file, not in the out file
                write(logLun,*)
                write(logLun,'(a$)') 'Difference at time = '
                write(logLun,time_format) tst%t(it)                        ! The time for which we have a difference
                   write(logLun,'(a$)') ', Tolerance = '
                   write(logLun,'(E15.5)') tst%tolerance                    ! The test tolerance
                   write(logLun,*)
                write(logLun,'(a$)') 'TST: '
                do id=max(tst%ndxstart,1), min(tst%ndxend,result_size)     ! The tested values
                   write(logLun,'(a$)') ' '
                   write(logLun,data_format) targetArray(id)
                   write(logLun,*)
                enddo
                write(logLun,'(a$)') 'REF: '
                do id=max(tst%ndxstart,1), min(tst%ndxend,result_size)     ! The reference values
                   write(logLun,'(a$)') ' '
                   write(logLun,data_format) targetArray_ref(id,it)
                   write(logLun,*)
                enddo
                write(logLun,*)
             endif
          endif
       enddo
       close(testOutLun)

       if (logging) then
          close (logLun)
          logLun_open = .False.
       endif
       if (comparison_failed) then
         call TCMessage(testname,'Comparison failed','testFinished')
       else
         call TCMessage(testname,'Comparison passed','testFinished')
       endif
       write(*,*)

       iostat = chdir(trim(oldcwd))
       ! Free EC instance
       success = ecInstanceFree(ecInstancePtr)

       contains
          subroutine callback_msg(lvl,msg)
             implicit none
             integer, intent(in)              :: lvl
             character(len=*), intent(in)     :: msg
             write(lvl,'(A)') trim(msg)       ! intentionally using lvl as a file handle !!
          end subroutine callback_msg

          function getTestInput(config_ptr, config, testname) result (success)
             implicit none
             logical                                   :: success
             type(tEcTestDef), intent(out)             :: config
             type(tree_data), pointer                  :: config_ptr
             character(len=*), intent(in)              :: testname
             character(len=50)                         :: filetype
             character(len=50)                         :: method
             character(len=50)                         :: operand
             character(len=50)                         :: kerneltimeunit
             character(len=666)                        :: liststring
             integer                                   :: jasferic
             integer                                   :: nx, ny, nt, ipt
             integer                                   :: nx2, ny2
             integer                                   :: ix, iy, it, ipos
             logical                                   :: status
             real(hp)                                  :: t_stop, t_start, t_increment
             type(tree_data), pointer  :: thetest => null()

             ! Try to open the test
             success = .false.
             call tree_get_node_by_name(config_ptr, trim(testname), thetest)
             if (.not.associated(thetest)) then
                call TCMessage(testname,'Cannot find test '//trim(testname),'testFailed')
                return
             endif

             call prop_get (config_ptr, trim(testname), 'VectorMax', config%vectormax, status)

             liststring = ''
             nx = 0
             ny = 0
             call prop_get (config_ptr, trim(testname), 'LocationsX', liststring , status)
             if (len_trim(liststring)>0) then                  ! count the comma's, which serve as separators
                nx = 1
                do ipos = 1, len_trim(liststring)
                   if (liststring(ipos:ipos) == ',') nx = nx + 1
                enddo
                allocate(config%x(nx))
                read(liststring,*) (config%x(ix),ix=1,nx)
             endif

             liststring = ''
             call prop_get (config_ptr, trim(testname), 'LocationsX2', liststring , status)
             if (len_trim(liststring)>0) then                  ! count the comma's, which serve as separators
                nx2 = 1
                do ipos = 1, len_trim(liststring)
                   if (liststring(ipos:ipos) == ',') nx2 = nx2 + 1
                enddo
                allocate(config%x2(nx2))
                read(liststring,*) (config%x2(ix),ix=1,nx2)
                nx=min(nx,nx2)
             endif

             liststring = ''
             call prop_get (config_ptr, trim(testname), 'LocationsY', liststring , status)
             if (len_trim(liststring)>0) then                  ! count the comma's, which serve as separators
                ny = 1
                do ipos = 1, len_trim(liststring)
                   if (liststring(ipos:ipos) == ',') ny = ny + 1
                enddo
                allocate(config%y(ny))
                read(liststring,*) (config%y(iy),iy=1,ny)
             endif

             liststring = ''
             call prop_get (config_ptr, trim(testname), 'LocationsY2', liststring , status)
             if (len_trim(liststring)>0) then                  ! count the comma's, which serve as separators
                ny2 = 1
                do ipos = 1, len_trim(liststring)
                   if (liststring(ipos:ipos) == ',') ny2 = ny2 + 1
                enddo
                allocate(config%y2(ny2))
                read(liststring,*) (config%y2(iy),iy=1,ny2)
                ny=min(ny,ny2)
             endif
             config%npoint=min(nx,ny)

             config%ntimes = 0
             liststring = ''
             call prop_get (config_ptr, trim(testname), 'TimeIntervals', liststring , status)
             if (len_trim(liststring)>0) then
                read(liststring,*) t_start, t_stop, t_increment
                nt = 1 + (t_stop - t_start)/t_increment
                allocate(config%t(nt))
                do it = 1, nt
                   config%t(it) = t_start + (it-1)*t_increment
                enddo
                config%ntimes = nt
             endif
             liststring = ''
             call prop_get (config_ptr, trim(testname), 'Times', liststring , status)
             if (len_trim(liststring)>0) then
                nt = 1
                do ipos = 1, len_trim(liststring)
                   if (liststring(ipos:ipos) == ',') nt = nt + 1
                enddo
                allocate(config%t(nt))
                read(liststring,*) (config%t(it),it=1,nt)
                config%ntimes = nt
             endif

             call prop_get (config_ptr, trim(testname),  'TestPathName', config%testpathname , status)
             call prop_get (config_ptr, trim(testname),  'QuantityName', config%quantityname , status)
             call prop_get (config_ptr, trim(testname),  'LocationName', config%locationname , status)
             call prop_get (config_ptr, trim(testname),  'ExtFileName', config%extFilename , status)
             call prop_get (config_ptr, trim(testname),  'InputFileName', config%inFilename , status)
             call prop_get (config_ptr, trim(testname),  'InputFileType', filetype , status)
             if (status) config%inFileType = getECFileTypeNumber(filetype)
             if (config%inFileType == ec_undef_int) return
             call prop_get (config_ptr, trim(testname),  'RefFileName', config%refFilename , status)
             call prop_get (config_ptr, trim(testname),  'BCFileName', config%forcingfile , status)
             call prop_get (config_ptr, trim(testname),  'ConverterMethod', method , status)
             if (status) config%method = getECMethodNumber(method)
             if (config%method == ec_undef_int) return
             call prop_get (config_ptr, trim(testname),  'Operand', operand , status)
             if (status) config%operand = getECOperandNumber(operand)
             if (config%operand == ec_undef_int) return
             call prop_get (config_ptr, trim(testname), 'KernelRefDate', config%tgt_refdate , status)
             call prop_get (config_ptr, trim(testname),  'KernelTimeZone', config%tgt_tzone , status)
             call prop_get (config_ptr, trim(testname),  'KernelTimeUnit', kerneltimeunit , status)
             if (status) config%tgt_tunit = getECTimeUnitNumber(kerneltimeunit)
             if (config%tgt_tunit == ec_undef_int) return
             call prop_get (config_ptr, trim(testname),  'Tolerance', config%tolerance , status)
             call prop_get (config_ptr, trim(testname), 'IndexStart', config%ndxstart , status)
             call prop_get (config_ptr, trim(testname), 'IndexEnd', config%ndxend , status)
             call prop_get (config_ptr, trim(testname), 'UseSpheric', jasferic , status)
             config%jasferic = (jasferic>0)
             call prop_get(config_ptr, trim(testname),  'UseBcBlockApproach', config%useBcBlockApproach, status)

             ! This information should normally be obtained from a config file : configFilename
             config%name = testname
             allocate(targetArray(config%npoint*config%vectormax))
             allocate(targetArray_ref(config%npoint*config%vectormax,config%ntimes))
             config%missing_value = ec_undef_hp
             success = .true.
          end function getTestInput

          function compareTextDump(treeTstFile, treeRefFile) result (success)
             implicit none
             logical                          :: success
             character(len=*)                 :: treeTstFile
             character(len=*)                 :: treeRefFile
             integer                          :: ioRef, ioTst
             character(len=300)               :: lineTst, lineRef

             success = .False.
             open(newunit=treeTstLun,file=treeTstFile,status='OLD',iostat=ioTst)
             open(newunit=treeRefLun,file=treeRefFile,status='OLD',iostat=ioRef)
             if(ioTst/=0 .or. ioRef/=0) then
                if(ioTst/=0) then
                   call TCMessage(testname,'Cannot open file '//trim(treeTstFile),'testFailed')
                else
                   close(treeTstLun)
                endif
                if(ioRef/=0) then
                   call TCMessage(testname,'Cannot open file '//trim(treeRefFile),'testFailed')
                else
                   close(treeTstLun)
                endif
                return
             endif
             do while (ioTst==0 .and. ioRef==0)         ! Read files while not one of then EOFs .......
                read(treeTstLun,'(a)', iostat = ioTst) LineTst
                read(treeRefLun,'(a)', iostat = ioRef) Lineref
                if ((ioRef==0) .and. (ioTst/=0)) return    ! tested dump too short
                if ((ioTst==0) .and. (ioRef/=0)) return    ! reference dump too short
                if (trim(LineTst)/=trim(LineRef)) return   ! mismatching lines
             enddo
             close(treeRefLun)
             close(treeTstLun)
             success = .True.
          end function compareTextDump

          function getECFileTypeNumber(FileTypeName) result (FileTypeNumber)
          implicit none
          integer  ::  FileTypeNumber
          character(len=*), intent(in)     :: FileTypeName
          character(len=30)                :: sFileTypeName
          sFileTypeName = FileTypeName
          call str_lower(sFileTypeName)

          select case (trim(sFileTypeName))
          case ('undefined')
              FileTypeNumber = provFile_undefined
          case ('uniform')
              FileTypeNumber = provFile_uniform
          case ('unimagdir')
              FileTypeNumber = provFile_unimagdir
          case ('swvp')
              FileTypeNumber = provFile_svwp
          case ('svwp_weight')
              FileTypeNumber = provFile_svwp_weight
          case ('arcinfo')
              FileTypeNumber = provFile_arcinfo
          case ('spiderweb')
              FileTypeNumber = provFile_spiderweb
          case ('curvi')
              FileTypeNumber = provFile_curvi
          case ('curvi_weight')
              FileTypeNumber = provFile_curvi_weight
          case ('samples')
              FileTypeNumber = provFile_samples
          case ('triangulationmagdir')
              FileTypeNumber = provFile_triangulationmagdir
          case ('poly_tim')
              FileTypeNumber = provFile_poly_tim
          case ('fourier')
              FileTypeNumber = provFile_fourier
          case ('grib')
              FileTypeNumber = provFile_grib
          case ('netcdf')
              FileTypeNumber = provFile_netcdf
          case ('qhtable')
              FileTypeNumber = provFile_qhtable
          case ('t3d')
              FileTypeNumber = provFile_t3D
          case ('bc')
              FileTypeNumber = provFile_bc
          case ('fourier_cor')
              FileTypeNumber = provFile_fourier_cor
          case default
              FileTypeNumber = ec_undef_int
              call TCMessage(testname,'Unknown filetype '//trim(FileTypeName),'testFailed')
              call ec_test_exception()
          end select
          end function getECFileTypeNumber


          function getECMethodNumber(MethodName) result (MethodNumber)
          implicit none
          integer  ::  MethodNumber
          character(len=*), intent(in)     :: MethodName
          character(len=30)                :: sMethodName
          sMethodName = MethodName
          call str_lower(sMethodName)
          select case (trim(sMethodName))
          case ('unknown')
             MethodNumber = interpolate_unknown
          case ('passthrough')
             MethodNumber = interpolate_passthrough
          case ('timespace')
             MethodNumber = interpolate_timespace
          case ('spacetime')
             MethodNumber = interpolate_spacetime
          case ('time')
             MethodNumber = interpolate_time
          case ('space')
             MethodNumber = interpolate_space
          case ('spacetimesaveweightfactors')
             MethodNumber = interpolate_spacetimeSaveWeightFactors
          case ('time_extrapolation_ok')
             MethodNumber = interpolate_time_extrapolation_ok
          case ('triangle')
             MethodNumber = interpolate_triangle
          case ('averaging')
             MethodNumber = interpolate_averaging
          case ('triangleindex')
             MethodNumber = interpolate_triangleindex
          case ('smoothing')
             MethodNumber = interpolate_smoothing
          case ('intdiffusion')
             MethodNumber = interpolate_intdiffusion
          case ('vertprofile')
             MethodNumber = interpolate_vertprofile
          case default
             MethodNumber = ec_undef_int
             call TCMessage(testname,'Unknown method '//trim(MethodName),'testFailed')
             call ec_test_exception()
          end select
          end function getECMethodNumber

          function getECOperandNumber(OperandName) result (OperandNumber)
          implicit none
          integer  ::  OperandNumber
          character(len=*), intent(in)     :: OperandName
          character(len=30)                :: sOperandName
          sOperandName = OperandName
          call str_lower(sOperandName)
          select case (trim(sOperandName))
          case ('undefined')
             OperandNumber = operand_undefined
          case ('add')
             OperandNumber = operand_add
          case ('replace')
             OperandNumber = operand_replace
          case ('replace_element')
             OperandNumber = operand_replace_element
          case ('add_element')
             OperandNumber = operand_add_element
          case default
             OperandNumber = ec_undef_int
             call TCMessage(testname,'Unknown operand '//trim(OperandName),'testFailed')
             call ec_test_exception()
          end select
       end function getECOperandNumber

       function getECTimeUnitnumber(TimeUnitName) result (TimeUnitNumber)
          implicit none
          integer  ::  TimeUnitNumber
          character(len=*), intent(in)     :: TimeUnitName
          character(len=30)                :: sTimeUnitName
          sTimeUnitName = TimeUnitName
          call str_lower(sTimeUnitName)
          select case (trim(sTimeUnitName))
          case ('second')
             TimeUnitNumber = ec_second
          case ('minute')
             TimeUnitNumber = ec_minute
          case ('hour')
             TimeUnitNumber = ec_hour
          case ('day')
             TimeUnitNumber = ec_day
          case default
             TimeUnitNumber = ec_undef_int
             call TCMessage(testname,'Unknown time unit '//trim(TimeUnitName),'testFailed')
             call ec_test_exception()
          end select
       end function getECTimeUnitnumber

       subroutine ec_test_callback_msg(lvl,msg)
          integer, intent(in)             :: lvl
          character(len=*), intent(in)    :: msg
          write(*,'(A)') trim(msg)
          write(logLun,'(A)') trim(msg)
       end subroutine ec_test_callback_msg

       subroutine ec_test_exception
          implicit none
          logical :: opened
          integer :: iostat
          inquire(testRefLun,opened=opened)
          if (testRefLun_open) then
              close(testRefLun)
              testRefLun_open = .False.
          endif
          if (associated(ecInstancePtr)) then
             if (.not. ecFreeInstance(ecInstancePtr)) then
                write(logLun,*) '* could not free EC-Module instance after this test !!'
             end if
          end if
          if (logLun_open) then
              close(logLun,iostat=iostat)
              logLun_open = .False.
          endif
          ! OK, this looks weird at the end of each failed test, but the testbench does it...
          ! Every test is enclosed within 'testStarted' and 'testFinished'
          call TCMessage(testname,'Comparison failed','testFinished')
          write(*,*)

          ! Everything allocated within do_test should be deallocated here
          if (allocated(xyen)) deallocate(xyen)
          iostat = chdir(trim(oldcwd))

          call tEcTestDef_destroy(tst)
       end subroutine ec_test_exception
    end subroutine do_test

    subroutine do_test_internal
       character(len=100) :: testname
       character(len=300) :: errMessage
       logical            :: success

       testname = 'ec_support_unittests'
       call TCMessage(testname,'','testStarted')
       call TestEcGetTimesteps(success, errMessage)
       if (success) then
         call TCMessage(testname,'Comparison passed','testFinished')
       else
         call TCMessage(testname, errMessage, 'testFailed')
       endif

       testname = 'ec_support_NetCDF_time_string_conversion'
       call TCMessage(testname,'','testStarted')
       call TestTimestringToUnitAndRefdate1(success, errMessage)
       if (success) then
         call TCMessage(testname,'Comparison passed','testFinished')
       else
         call TCMessage(testname, errMessage, 'testFailed')
       endif

       testname = 'ec_support_ArcInfo_time_string_conversion'
       call TCMessage(testname,'','testStarted')
       call TestTimestringToUnitAndRefdate2(success, errMessage)
       if (success) then
         call TCMessage(testname,'Comparison passed','testFinished')
       else
         call TCMessage(testname, errMessage, 'testFailed')
       endif

       testname = 'ec_support_time_string_error_handling'
       call TCMessage(testname,'','testStarted')
       call TestTimestringToUnitAndRefdate3(success, errMessage)
       if (success) then
         call TCMessage(testname,'Comparison passed','testFinished')
       else
         call TCMessage(testname, errMessage, 'testFailed')
       endif

    end subroutine do_test_internal

    subroutine tEcTestDef_destroy(tst)
      implicit none
      type(tEcTestDef)  :: tst
      if (allocated(tst%t)) deallocate(tst%t)
      if (allocated(tst%x)) deallocate(tst%x)
      if (allocated(tst%y)) deallocate(tst%y)
      if (allocated(tst%x2)) deallocate(tst%x2)
      if (allocated(tst%y2)) deallocate(tst%y2)
      if (allocated(tst%resultVector)) deallocate(tst%resultVector)
      if (allocated(tst%referenceVector)) deallocate(tst%referenceVector)
    end subroutine tEcTestDef_destroy

    subroutine TCMessage(testname,msgtext,status,details,dev)
       implicit none
       character(len=*), intent(in)           ::  msgtext, status, testname
       character(len=*), intent(in), optional ::  details
       integer,          intent(in), optional ::  dev
       integer :: devOut
       ! status should one of testFinished, testFailed
       if (present(dev)) then
          devOut = dev
       else
          devOut = 0
       endif
       if (present(details)) then
          write(devOut,'(a)') '##teamcity['//trim(status)//' name='''//trim(testname)//''' message='''//trim(msgtext)//''' details='''//trim(details)//''']'
       else
          write(devOut,'(a)') '##teamcity['//trim(status)//' name='''//trim(testname)//''' message='''//trim(msgtext)//''']'
       endif
    end subroutine TCMessage

 end module m_ec_module_test

