subroutine rdtrt(lundia    ,error     ,lftrto    ,dt        ,mmax      , &
               & nmax      ,nmaxus    ,kmax      ,itimtt    ,gdp       )
!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2024.                                
!                                                                               
!  This program is free software: you can redistribute it and/or modify         
!  it under the terms of the GNU General Public License as published by         
!  the Free Software Foundation version 3.                                      
!                                                                               
!  This program is distributed in the hope that it will be useful,              
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU General Public License for more details.                                 
!                                                                               
!  You should have received a copy of the GNU General Public License            
!  along with this program.  If not, see <http://www.gnu.org/licenses/>.        
!                                                                               
!  contact: delft3d.support@deltares.nl                                         
!  Stichting Deltares                                                           
!  P.O. Box 177                                                                 
!  2600 MH Delft, The Netherlands                                               
!                                                                               
!  All indications and logos of, and references to, "Delft3D" and "Deltares"    
!  are registered trademarks of Stichting Deltares, and remain the property of  
!  Stichting Deltares. All rights reserved.                                     
!                                                                               
!-------------------------------------------------------------------------------
!  
!  
!!--description-----------------------------------------------------------------
!
! Reads the dimensions ntrt, nttaru, nttarv
! Initializes nroupa
!
!!--pseudo code and references--------------------------------------------------
! NONE
!!--declarations----------------------------------------------------------------
    use precision
    use properties
    use globaldata
    use string_module
    use dfparall
    use system_utils, only: exifil
    !
    implicit none
    !
    type(globdat),target :: gdp
    !
    ! The following list of pointer parameters is used to point inside the gdp structure
    !
    real(fp)                   , pointer :: alf_area_ser
    real(fp)                   , pointer :: trtminh
    integer                    , pointer :: iarea_avg
    integer                    , pointer :: nroupa
    integer                    , pointer :: nttaru
    integer                    , pointer :: nttarv
    integer                    , pointer :: ntrt
    integer                    , pointer :: max_cl
    integer                    , pointer :: mfg
    integer                    , pointer :: nfg
    integer , dimension(:,:)   , pointer :: ittaru
    integer , dimension(:,:)   , pointer :: ittarv
    integer , dimension(:,:)   , pointer :: ittdef
    integer , dimension(:)     , pointer :: itrt_list
    real(fp)                   , pointer :: dryflc
    real(fp), dimension(:)     , pointer :: fraccu_list
    real(fp), dimension(:,:)   , pointer :: rgcalu
    real(fp), dimension(:,:)   , pointer :: rgcalv
    real(fp), dimension(:)     , pointer :: rttaru
    real(fp), dimension(:)     , pointer :: rttarv
    real(fp), dimension(:,:)   , pointer :: rttdef
    real(fp), dimension(:,:,:) , pointer :: rttfu
    real(fp), dimension(:,:,:) , pointer :: rttfv
    real(fp), dimension(:,:)   , pointer :: vegh2d
    real(fp), dimension(:,:)   , pointer :: vden2d
    logical                    , pointer :: waqol
    type (gd_trachy)           , pointer :: gdtrachy
!
! Local parameters
!
    integer, parameter :: irough = 300
    integer, parameter :: maxfld = 10
!
! Global variables
!
    integer  :: itimtt
    integer  :: lundia
    integer  :: kmax
    integer  :: mmax
    integer  :: nmax
    integer  :: nmaxus
    logical  :: error
    logical  :: lftrto
    real(fp) :: dt
!
! Local variables
!
    integer                          :: i
    integer                          :: ibeg
    integer                          :: iend
    integer                          :: iocond
    integer                          :: istat
    integer                          :: it
    integer                          :: j
    integer                          :: lcurec
    integer                          :: lfile
    integer                          :: luntmp
    integer                          :: m
    integer                          :: m1
    integer                          :: mll
    integer                          :: mcurec
    integer                          :: mtrt
    integer                          :: n
    integer                          :: n1
    integer                          :: nll
    integer                          :: nrflds
    integer, dimension(irough)       :: nropar
    integer, dimension(maxfld)       :: ifield
    integer, dimension(maxfld)       :: itype
    integer, dimension(maxfld)       :: lenchr
    integer, external                :: newlun
    logical                          :: dtn
    logical                          :: lokay
    logical                          :: lrcode
    real(fp)                         :: rtimtt
    real(fp), dimension(maxfld)      :: rfield
    character(10), dimension(maxfld) :: cfield
    character(11)                    :: fmttmp
    character(132)                   :: rec132
    character(20)                    :: chulp
    character(256)                   :: filtmp
    character(300)                   :: errmsg
    character(6)                     :: keyw
    character(20)                    :: txtput1
!
!! executable statements -------------------------------------------------------
!
    alf_area_ser   => gdp%gdtrachy%alf_area_ser
    trtminh        => gdp%gdtrachy%trtminh
    iarea_avg      => gdp%gdtrachy%iarea_avg
    nroupa         => gdp%gdtrachy%nroupa
    nttaru         => gdp%gdtrachy%nttaru
    nttarv         => gdp%gdtrachy%nttarv
    ntrt           => gdp%gdtrachy%ntrt
    max_cl         => gdp%gdtrachy%max_cl
    dryflc         => gdp%gdnumeco%dryflc
    mfg            => gdp%gdparall%mfg
    nfg            => gdp%gdparall%nfg
    waqol          => gdp%gdwaqpar%waqol
    gdtrachy       => gdp%gdtrachy
    itrt_list      => gdp%gdtrachy%itrt_list
    fraccu_list    => gdp%gdtrachy%fraccu_list
    !
    ! Allocate trachytope arrays that are used in main routines
    !
    if (.not. associated(gdtrachy%rttfu)) then
       !
       istat = 0
       if (waqol) then
          if (istat==0) allocate(gdtrachy%vegh2d(gdp%d%nlb:gdp%d%nub,gdp%d%mlb:gdp%d%mub), stat = istat)
          if (istat==0) allocate(gdtrachy%vden2d(gdp%d%nlb:gdp%d%nub,gdp%d%mlb:gdp%d%mub), stat = istat)
          vegh2d      => gdp%gdtrachy%vegh2d
          vden2d      => gdp%gdtrachy%vden2d
          vegh2d = 0.0_fp
          vden2d = 0.0_fp
       endif
       !
       if (istat==0) allocate(gdtrachy%rttfu(gdp%d%nlb:gdp%d%nub,gdp%d%mlb:gdp%d%mub,kmax), stat = istat)
       if (istat==0) allocate(gdtrachy%rttfv(gdp%d%nlb:gdp%d%nub,gdp%d%mlb:gdp%d%mub,kmax), stat = istat)
       if (istat/=0) then
          call prterr(lundia, 'U021', 'RDTRT: memory alloc error')
          call d3stop(1, gdp)
       endif
       !
       ! update local pointers
       !
       rttfu          => gdp%gdtrachy%rttfu
       rttfv          => gdp%gdtrachy%rttfv
       !
       ! initialize arrays
       !
       rttfu(gdp%d%nlb:gdp%d%nub,gdp%d%mlb:gdp%d%mub,1:kmax) = 0.0_fp
       rttfv(gdp%d%nlb:gdp%d%nub,gdp%d%mlb:gdp%d%mub,1:kmax) = 0.0_fp
    endif
    !
    ! Initialize scalars
    !
    lftrto       = .false.
    itimtt       = 1
    iarea_avg    = 1
    alf_area_ser = 0.6_fp
    !
    ! Read value of Trtrou, default NO
    !
    chulp = 'N'
    call prop_get_string(gdp%mdfile_ptr,'*','Trtrou',chulp)
    !
    ! set LFTRTO to TRUE if CHULP = Y/y
    !
    call small(chulp ,1 )
    if (chulp=='y') lftrto = .true.
    !
    ! if Trtrou turned out to be NO, don't look any further.
    !
    if (.not.lftrto) goto 9999
    !
    ! Allocate trachytope arrays that are only used locally
    !
    istat = 0
    if (.not. associated(gdtrachy%ittaru)) then
                     allocate(gdtrachy%ittaru(nttaru,3)                               , stat = istat)
       if (istat==0) allocate(gdtrachy%ittarv(nttarv,3)                               , stat = istat)
       if (istat==0) allocate(gdtrachy%ittdef(ntrt,2)                                 , stat = istat)
       if (istat==0) allocate(gdtrachy%rgcalu(gdp%d%nlb:gdp%d%nub,gdp%d%mlb:gdp%d%mub), stat = istat)
       if (istat==0) allocate(gdtrachy%rgcalv(gdp%d%nlb:gdp%d%nub,gdp%d%mlb:gdp%d%mub), stat = istat)
       if (istat==0) allocate(gdtrachy%rttaru(nttaru)                                 , stat = istat)
       if (istat==0) allocate(gdtrachy%rttarv(nttarv)                                 , stat = istat)
       if (istat==0) allocate(gdtrachy%rttdef(ntrt,nroupa)                            , stat = istat)
       !
       if (istat/=0) then
          call prterr(lundia, 'U021', 'RDTRT: memory alloc error')
          call d3stop(1, gdp)
       endif
       !
       ! update local pointers
       !
       ittaru         => gdp%gdtrachy%ittaru
       ittarv         => gdp%gdtrachy%ittarv
       ittdef         => gdp%gdtrachy%ittdef
       rgcalu         => gdp%gdtrachy%rgcalu
       rgcalv         => gdp%gdtrachy%rgcalv
       rttaru         => gdp%gdtrachy%rttaru
       rttarv         => gdp%gdtrachy%rttarv
       rttdef         => gdp%gdtrachy%rttdef
       !
    endif
    !
    write (lundia, '(a)') '*** Start  of trachytopes input'
    !
    ! locate 'TrtDt' record for update time step
    !
    rtimtt = dt
    call prop_get(gdp%mdfile_ptr, '*', 'TrtDt', rtimtt)
    !
    ! Check on multiple
    itimtt = nint(rtimtt/dt)
    if (dtn(itimtt, rtimtt, dt)) then
       call prterr(lundia    ,'U044'    ,'Trachytope update time'        )
       error = .true.
       goto 9999
    endif
    !
    txtput1 = 'TrtDt'
    write (lundia, '(a,a,f7.3,a)') txtput1,': ',rtimtt,' min'
    write (lundia, '(a,a,i5,a)') txtput1,': every ',itimtt,' timesteps'
    !
    ! Trtdef: trachytope definition file (must exist, no default)
    !
    filtmp = ' '
    keyw   = 'Trtdef'
    call prop_get_string(gdp%mdfile_ptr,'*',keyw,filtmp)
    !
    txtput1 = keyw
    write (lundia, '(a,a,a)') txtput1,': ',trim(filtmp)
    !
    ! keyword not found ?
    !
    if (filtmp == ' ') then
       call prterr(lundia    ,'V091'    ,keyw      )
       error = .true.
       goto 9999
    endif
    !
    ! test file existence
    !
    lfile = index(filtmp, ' ')
    if (lfile==0) lfile = 13
    lfile = lfile - 1
    call remove_leading_spaces(filtmp    ,lfile     )
    if (.not.exifil(filtmp, lundia)) then
       !
       ! file does not exist !!
       !
       call prterr(lundia    ,'P101'    ,filtmp(1:lfile)      )
       error = .true.
       goto 9999
    endif
    !
    ! open trachytope definition file
    !
    open (newunit=luntmp, file = filtmp(1:lfile), form = 'formatted', iostat = iocond,  &
        & status = 'old')
    if (iocond/=0) then
       call prterr(lundia    ,'U015'    ,filtmp(1:lfile)      )
       error = .true.
       goto 9999
    endif
    !
    ! freeformatted file
    ! read records till end of file
    !
    mtrt   = 0
    mcurec = 0
    !
    ! Create list with possible roughnes descriptions.
    !     NROPAR the number of parameters for the codes
    !
    nropar = -1
    !
    !    1-49: Special treatment
    !
    !     Water free terrain
    nropar(1) = 0
    !
    !     Combinations of area roughnesses
    nropar(2) = 4
    !
    !   51-99: Standard roughness coefficients
    !
    !     Constant White-Colebrook / Nikuradse value
    nropar(51) = 1
    !
    !     Constant Chezy value
    nropar(52) = 1
    !
    !     Constant Manning value
    nropar(53) = 1
    !
    !     Constant z0 value
    nropar(54) = 1
    !
    ! 101-149: Alluvial roughness predictors
    !
    !     Waqua description (Simple v. Rijn)
    nropar(101) = 2
    !
    !     power law
    nropar(102) = 2
    !
    !     Van Rijn
    nropar(103) = 0
    !
    !     Struiksma
    nropar(104) = 5
    !
    !     Quadratic combination of bedform roughness heights
    ! used to be 7, use keywords: BdfRpC, BdfRpR, BdfMrC, BdfMrR, BdfDnC, BdfDnR, BdfD50
    nropar(105) = 0
    !
    !     Linear superposition of bedform roughness heights
    nropar(106) = 0
    !
    ! 151-199: Vegetation roughness predictors (areas)
    !
    !     Waqua vegetation formulation 1
    nropar(151) = 2
    !
    !     Waqua vegetation formulation 2
    nropar(152) = 4
    !
    !     Baptist vegetation formulation
    nropar(153) = 4
    nropar(154) = 4
    !
    ! 201-249: Vegetation roughness predictors (linear)
    !
    !     Waqua lineair elements formulation 1
    nropar(201) = 2
    !
    !     Waqua lineair elements formulation 2
    nropar(202) = 4
    !
    ! 251-299: Vegetation roughness predictors (point)
    !
    !     Waqua tree elements formulation 1
    nropar(251) = 2
    !
    ! -->
    !
    ! read line
    !
  110 continue
    read (luntmp, '(a)', iostat = iocond) rec132
    if (iocond/=0) then
       !
       ! End-of-file ?
       !
       if (iocond<0) goto 199
       !
       ! Reading error
       !
       call prterr(lundia    ,'G007'    ,filtmp(1:lfile)      )
       error = .true.
       goto 199
    else
       mcurec = mcurec + 1
    endif
    !
    ! Interpret line ...
    !
    !
    ! Comment line
    !
    if ((rec132(1:1)=='*') .or. (rec132(1:1)=='#')) goto 110
    !
    ! Scan the record.
    !
    ibeg = 1
    iend = 132
    call scannr(rec132    ,ibeg      ,iend      ,nrflds    ,itype     , &
              & ifield    ,rfield    ,cfield    ,lenchr    ,maxfld    , &
              & .true.    ,.true.    ,.true.    )
    !
    ! When no sub-fields are found, record appears to be empty
    !
    if (nrflds==0) goto 110
    !
    if (nrflds<0) then
       !
       ! Cannot interpret line
       !
       error  = .true.
       rec132 = ' '
       write (rec132, '(i12)') mcurec
       call remove_leading_spaces(rec132    ,lcurec    )
       errmsg = filtmp(1:lfile) // ', Record: ' // rec132(1:lcurec)
       call prterr(lundia    ,'G007'    ,errmsg    )
       goto 199
    endif
    !
    ! Check the contents
    !
    if (itype(1)==1 .and. itype(2)==1) then
       !
       ! Determine roughness description (i)
       !
       lrcode = .false.
       if (ifield(2)<=irough) then
          i = ifield(2)
          if (nropar(i)>=0) then
             lrcode = .true.
          endif
       endif
       if (lrcode) then
          !
          ! Check required parameters
          !
          if (nrflds/=nropar(i) + 2) then
             error  = .true.
             rec132 = ' '
             write (rec132, '(i12)') mcurec
             call remove_leading_spaces(rec132    ,lcurec    )
             errmsg = filtmp(1:lfile) // ', Record: ' // rec132(1:lcurec)
             call prterr(lundia    ,'J014'    ,errmsg    )
             goto 199
          endif
          !
          ! Check the parameters
          !
          lokay = .true.
          do j = 1, nropar(i)
             if (itype(j + 2)/=2 .and. itype(j + 2)/=1) then
                lokay = .false.
             endif
          enddo
          !
          ! Cannot interpret line
          !
          if (.not.lokay) then
             error  = .true.
             rec132 = ' '
             write (rec132, '(i12)') mcurec
             call remove_leading_spaces(rec132    ,lcurec    )
             errmsg = filtmp(1:lfile) // ', Record: ' // rec132(1:lcurec)
             call prterr(lundia    ,'G007'    ,errmsg    )
             goto 199
          endif
          !
          ! Everything seems to be OK.
          !
          ! Increment MTRT for checking array size.
          !
          mtrt = mtrt + 1
          if (mtrt>ntrt) then
             call prterr(lundia    ,'J004'    ,' '       )
             error = .true.
             goto 199
          endif
          !
          ! Store the trachytope number and roughness description in arrays
          !
          ittdef(mtrt, 1) = ifield(1)
          ittdef(mtrt, 2) = ifield(2)
          !
          ! Store parameters data in array
          !
          do j = 1, nropar(i)
             if (itype(j + 2)==2) then
                rttdef(mtrt, j) = rfield(j + 2)
             else
                rttdef(mtrt, j) = real(ifield(j + 2),fp)
             endif
          enddo
       else
          !
          ! Unknown rougness code
          !
          error  = .true.
          rec132 = ' '
          write (rec132, '(i12)') mcurec
          call remove_leading_spaces(rec132    ,lcurec    )
          errmsg = filtmp(1:lfile) // ', Record: ' // rec132(1:lcurec)
          call prterr(lundia    ,'J005'    ,errmsg    )
          goto 199
       endif
    else
       !
       ! Cannot interpret line
       !
       error  = .true.
       rec132 = ' '
       write (rec132, '(i12)') mcurec
       call remove_leading_spaces(rec132    ,lcurec    )
       errmsg = filtmp(1:lfile) // ', Record: ' // rec132(1:lcurec)
       call prterr(lundia    ,'G007'    ,errmsg    )
       goto 199
    endif
    !
    ! Go to the next record.
    !
    goto 110
    ! <--
    !
    ! close file
    !
  199 continue
    close (luntmp)
    if (error) goto 9999
    !
    ! Trtu  : trachytope area file for U-direction
    !             (must exist, no default)
    !
    filtmp = ' '
    keyw   = 'Trtu'
    call prop_get_string(gdp%mdfile_ptr,'*',keyw,filtmp)
    !
    txtput1 = keyw
    write (lundia, '(a,a,a)') txtput1,': ',trim(filtmp)
    !
    ! keyword not found ?
    !
    if (filtmp == ' ' .and. ntrt /= 1) then
       call prterr(lundia    ,'V091'    ,keyw      )
       error = .true.
       goto 9999
    endif
    !
    ! read file
    !
    if (filtmp == ' ') then
       i = 0
       do m = mfg,(mfg+gdp%d%mmax-1)
          do n = nfg,(nfg+gdp%d%nmax-1)
             i = i+1
             ittaru(i,1) = n
             ittaru(i,2) = m
             ittaru(i,3) = ittdef(1,1)
             rttaru(i) = 1.0_fp
          enddo
       enddo
    else
       call rdttar(filtmp    ,lundia    ,error     ,nttaru    ,ittaru    , &
                 & rttaru    ,gdp       )
       if (error) goto 9999
    endif
    !
    ! Trtv  : trachytope area file for V-direction
    !             (must exist, no default)
    !
    filtmp = ' '
    keyw   = 'Trtv'
    call prop_get_string(gdp%mdfile_ptr,'*',keyw,filtmp)
    !
    txtput1 = keyw
    write (lundia, '(a,a,a)') txtput1,': ',trim(filtmp)
    !
    ! keyword not found ?
    !
    if (filtmp == ' ' .and. ntrt /= 1) then
       call prterr(lundia    ,'V091'    ,keyw      )
       error = .true.
       goto 9999
    endif
    !
    ! read file
    !
    if (filtmp == ' ') then
       i = 0
       do m = mfg,(mfg+gdp%d%mmax-1)
          do n = nfg,(nfg+gdp%d%nmax-1)
             i = i+1
             ittarv(i,1) = n
             ittarv(i,2) = m
             ittarv(i,3) = ittdef(1,1)
             rttarv(i) = 1.0_fp
          enddo
       enddo
    else
       call rdttar(filtmp    ,lundia    ,error     ,nttarv    ,ittarv    , &
                 & rttarv    ,gdp       )
       if (error) goto 9999
    endif
    !
    ! TrtClu : Calibration trachytopes in U-direction
    ! If not found fill array with 1.0
    !
    filtmp = ' '
    keyw   = 'TrtClu'
    call prop_get_string(gdp%mdfile_ptr,'*',keyw,filtmp)
    !
    txtput1 = keyw
    write (lundia, '(a,a,a)') txtput1,': ',trim(filtmp)
    !
    ! keyword not found ?
    !
    if (filtmp /= ' ') then
       !
       ! test file existence
       !
       lfile = index(filtmp, ' ')
       if (lfile==0) lfile = 13
       lfile = lfile - 1
       call remove_leading_spaces(filtmp    ,lfile     )
       if (.not.exifil(filtmp, lundia)) then
          !
          ! file does not exist !!
          !
          call prterr(lundia    ,'P101'    ,filtmp(1:lfile)      )
          error = .true.
          goto 9999
       endif
       !
       ! Calibration data has been specified
       ! Use routine that also read the depth file to read the data
       !
       fmttmp = 'FORMATTED'
       call depfil(lundia    ,error     ,filtmp    ,fmttmp    , &
                 & rgcalu    ,1         ,1         ,gdp%griddim)
       if (error) goto 9999
    else
       !
       ! Fill with 1.0
       !
       do m = 1, mmax
          do n = 1, nmaxus
             rgcalu(n, m) = 1.0_fp
          enddo
       enddo
    endif
    !
    ! TrtClv : Calibration trachytopes in V-direction
    ! If not found fill array with 1.0
    !
    filtmp = ' '
    keyw   = 'TrtClv'
    call prop_get_string(gdp%mdfile_ptr,'*',keyw,filtmp)
    !
    txtput1 = keyw
    write (lundia, '(a,a,a)') txtput1,': ',trim(filtmp)
    !
    ! keyword not found ?
    !
    if (filtmp /= ' ') then
       !
       ! test file existence
       !
       lfile = index(filtmp, ' ')
       if (lfile==0) lfile = 13
       lfile = lfile - 1
       call remove_leading_spaces(filtmp    ,lfile     )
       if (.not.exifil(filtmp, lundia)) then
          !
          ! file does not exist !!
          !
          call prterr(lundia    ,'P101'    ,filtmp(1:lfile)      )
          error = .true.
          goto 9999
       endif
       !
       ! Calibration data has been specified
       ! Use routine that also read the depth file to read the data
       !
       fmttmp = 'FORMATTED'
       call depfil(lundia    ,error     ,filtmp    ,fmttmp    , &
                 & rgcalv    ,1         ,1         ,gdp%griddim)
       if (error) then
       endif
    else
       !
       ! Fill with 1.0
       !
       do m = 1, mmax
          do n = 1, nmaxus
             rgcalv(n, m) = 1.0_fp
          enddo
       enddo
    endif
    !
    !  Minimum water depth in roughness computations
    !
    trtminh = dryflc
    keyw    = 'TrtMnH'
    call prop_get(gdp%mdfile_ptr, '*', keyw, trtminh)
    !
    txtput1 = keyw
    write (lundia, '(a,a,f7.3,a)') txtput1,': ',trtminh,' m'
    !
    !  Area averaging method:
    !  1: Nikuradse k based
    !  2: Chezy C based (parallel and serial)
    !
    keyw = 'TrtMth'
    call prop_get(gdp%mdfile_ptr, '*', keyw, iarea_avg)
    !
    txtput1 = keyw
    if (iarea_avg<1 .or. iarea_avg>2) then
       call prterr(lundia    ,'J001'    ,'TrtMth should be 1 or 2')
       error = .true.
       goto 9999
    endif
    write (lundia, '(a,a,i3)') txtput1,': ',iarea_avg
    !
    if (iarea_avg == 2) then
       !
       !  Alfa factor for serial and parallel averaging of area roughnesses
       !
       keyw = 'TrtAsr'
       call prop_get(gdp%mdfile_ptr, '*', keyw, alf_area_ser)
       !
       txtput1 = keyw
       write (lundia, '(a,a,f7.3)') txtput1,': ',alf_area_ser
       !
    endif
    !
    max_cl  = -1
    keyw    = 'TrtMxR'
    txtput1 = keyw
    call prop_get(gdp%mdfile_ptr, '*', keyw, max_cl)
    if (max_cl == -1) then
       ! Not specified, use default value
       !
       max_cl = 8
       write (lundia, '(a,a,i0,a)') txtput1,': ',max_cl, ' (default value)'
    else
       write (lundia, '(a,a,i0)') txtput1,': ',max_cl
    endif
    if (istat==0) allocate(gdtrachy%fraccu_list(max_cl)  , stat = istat)
    if (istat==0) allocate(gdtrachy%itrt_list(max_cl)    , stat = istat)
    !
    if (istat/=0) then
       call prterr(lundia, 'U021', 'RDTRT: memory alloc error (step 2)')
       call d3stop(1, gdp)
    endif
    !
    write (lundia, '(a)') '*** End    of trachytopes input'
    write (lundia, *)
    !
    ! transfer to local subdomain
    !

    mll = gdp%d%mmax
    nll = gdp%d%nmaxus

    if (parll .and. nttaru > 0) then
       do it = 1,nttaru
          n1 = ittaru(it,1) - nfg + 1
          m1 = ittaru(it,2) - mfg + 1
          ! check if observation point is inside or outside subdomain
          if ( m1>=1 .and. n1>=1 .and. m1<=mll .and. n1<=nll ) then
             !
             ! point is inside subdomain
             !
             ittaru(it,1) = n1
             ittaru(it,2) = m1
          else
             ittaru(it,1) = -1
             ittaru(it,2) = -1
             ittaru(it,3) = -1
             rttaru(it) = -1.0
          endif
       enddo
    endif

    if (parll .and. nttarv > 0) then
       do it = 1,nttarv
          n1 = ittarv(it,1) - nfg + 1
          m1 = ittarv(it,2) - mfg + 1
          ! check if observation point is inside or outside subdomain
          if ( m1>=1 .and. n1>=1 .and. m1<=mll .and. n1<=nll ) then
             !
             ! point is inside subdomain
             !
             ittarv(it,1) = n1
             ittarv(it,2) = m1
          else
             ittarv(it,1) = -1
             ittarv(it,2) = -1
             ittarv(it,3) = -1
             rttarv(it) = -1.0
          endif
   
       enddo
    endif
    !
 9999 continue
end subroutine rdtrt
