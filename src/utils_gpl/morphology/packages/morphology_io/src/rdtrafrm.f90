module m_rdtrafrm
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
!-------------------------------------------------------------------------------
use m_depfil_stm
implicit none

private

public initrafrm
public rdtrafrm
public setpardef
public echotrafrm

interface setpardef
   module procedure setpardeflog
   module procedure setpardefint
   module procedure setpardefreal
end interface setpardef   

contains

subroutine initrafrm(lundia    ,error     ,lsedtot   ,trapar    )
!!--description-----------------------------------------------------------------
!
! Reads transport formula and parameters
!
!!--declarations----------------------------------------------------------------
    use precision
    use morphology_data_module, only: trapar_type, MAX_RP, MAX_IP, MAX_SP, WS_MAX_RP, WS_MAX_IP, WS_MAX_SP
    use message_module
    !
! Arguments
    !
    integer                   , intent(in)   :: lundia
    logical                   , intent(out)  :: error
    integer                   , intent(in)   :: lsedtot !  Description and declaration in iidim.f90
    type (trapar_type)        , pointer      :: trapar
!
! Local variables
!
    integer                          , pointer :: max_integers
    integer                          , pointer :: max_reals
    integer                          , pointer :: max_strings
    integer                          , pointer :: max_integers_settle
    integer                          , pointer :: max_reals_settle
    integer                          , pointer :: max_strings_settle
    integer                          , pointer :: npar
    integer                          , pointer :: nouttot
    integer                          , pointer :: noutpar(:)
    integer                          , pointer :: ioutpar(:,:)
    character(256)                   , pointer :: outpar_name(:,:)
    character(256)                   , pointer :: outpar_longname(:,:)
    character(256)                   , pointer :: dll_function_settle(:)
    character(256)                   , pointer :: dll_name_settle(:)
    integer(pntrsize)                , pointer :: dll_handle_settle(:)
    integer                          , pointer :: dll_integers_settle(:)
    real(hp)                         , pointer :: dll_reals_settle(:)
    character(256)                   , pointer :: dll_strings_settle(:)
    character(256)                   , pointer :: dll_usrfil_settle(:)
    integer                          , pointer :: iform_settle(:)
    real(fp)                         , pointer :: par_settle(:,:)
    character(256)                   , pointer :: dll_function(:)
    character(256)                   , pointer :: dll_name(:)
    integer(pntrsize)                , pointer :: dll_handle(:)
    integer                          , pointer :: dll_integers(:)
    real(hp)                         , pointer :: dll_reals(:)
    character(256)                   , pointer :: dll_strings(:)
    character(256)                   , pointer :: dll_usrfil(:)
    character(256)                   , pointer :: flstrn(:)
    integer                          , pointer :: iform(:)
    character(256)                   , pointer :: name(:)
    real(fp)                         , pointer :: par(:,:)
    character(25)                    , pointer :: parname(:,:)
    character(256)                   , pointer :: parfilename(:,:)
    integer                          , pointer :: iparfile(:,:)
!
    integer                        :: istat
    character(256)                 :: errmsg
!
!! executable statements -------------------------------------------------------
!
    max_integers         => trapar%max_integers
    max_reals            => trapar%max_reals
    max_strings          => trapar%max_strings
    max_integers_settle  => trapar%max_integers_settle
    max_reals_settle     => trapar%max_reals_settle
    max_strings_settle   => trapar%max_strings_settle
    npar                 => trapar%npar
    !
    istat = 0
    if (.not. associated(trapar%par)) then
                     allocate (trapar%par        (npar,lsedtot), stat = istat)
       if (istat==0) allocate (trapar%parname    (npar,lsedtot), stat = istat)
       if (istat==0) allocate (trapar%parfilename(npar,lsedtot), stat = istat)
       if (istat==0) allocate (trapar%iparfile   (npar,lsedtot), stat = istat)
       if (istat==0) allocate (trapar%iform           (lsedtot), stat = istat)
       if (istat==0) allocate (trapar%flstrn          (lsedtot), stat = istat)
       if (istat==0) allocate (trapar%name            (lsedtot), stat = istat)
       !
       max_integers = MAX_IP
       max_reals    = MAX_RP
       max_strings  = MAX_SP
       if (istat==0) allocate (trapar%dll_handle  (lsedtot), stat = istat)
       if (istat==0) allocate (trapar%dll_name    (lsedtot), stat = istat)
       if (istat==0) allocate (trapar%dll_function(lsedtot), stat = istat)
       if (istat==0) allocate (trapar%dll_usrfil  (lsedtot), stat = istat)
       if (istat==0) allocate (trapar%dll_integers(max_integers), stat = istat)
       if (istat==0) allocate (trapar%dll_reals   (max_reals   ), stat = istat)
       if (istat==0) allocate (trapar%dll_strings (max_strings ), stat = istat)
       !
       max_integers_settle = WS_MAX_IP
       max_reals_settle    = WS_MAX_RP
       max_strings_settle  = WS_MAX_SP
       if (istat==0) allocate (trapar%dll_handle_settle  (lsedtot), stat = istat)
       if (istat==0) allocate (trapar%dll_name_settle    (lsedtot), stat = istat)
       if (istat==0) allocate (trapar%dll_function_settle(lsedtot), stat = istat)
       if (istat==0) allocate (trapar%dll_usrfil_settle  (lsedtot), stat = istat)
       if (istat==0) allocate (trapar%dll_integers_settle(max_integers_settle), stat = istat)
       if (istat==0) allocate (trapar%dll_reals_settle   (max_reals_settle   ), stat = istat)
       if (istat==0) allocate (trapar%dll_strings_settle (max_strings_settle ), stat = istat)
       if (istat==0) allocate (trapar%iform_settle       (lsedtot), stat = istat)
       if (istat==0) allocate (trapar%par_settle         (npar,lsedtot), stat = istat)
       !
       if (istat==0) allocate (trapar%noutpar            (lsedtot), stat = istat)
       if (istat==0) allocate (trapar%ioutpar            (npar, lsedtot), stat = istat)
       nullify(trapar%outpar)
       if (istat==0) allocate (trapar%outpar_name        (npar, lsedtot), stat = istat)
       if (istat==0) allocate (trapar%outpar_longname    (npar, lsedtot), stat = istat)
       !
       if (istat/=0) then
          errmsg = 'IniTraFrm: memory alloc error'
          call write_error(errmsg, unit=lundia)
          error = .true.
          return
       endif
    endif
    !
    par           => trapar%par
    parname       => trapar%parname
    parfilename   => trapar%parfilename
    iparfile      => trapar%iparfile
    iform         => trapar%iform
    flstrn        => trapar%flstrn
    name          => trapar%name
    !
    iform   = -999
    flstrn  = ' '
    name    = ' '
    par     = 0.0_fp
    parname = ' '
    parfilename = ' '
    iparfile    = 0
    !
    dll_function  => trapar%dll_function
    dll_name      => trapar%dll_name
    dll_handle    => trapar%dll_handle
    dll_usrfil    => trapar%dll_usrfil
    dll_integers  => trapar%dll_integers
    dll_reals     => trapar%dll_reals
    dll_strings   => trapar%dll_strings
    !
    dll_function  = ' '
    dll_name      = ' '
    dll_handle    = 0
    dll_usrfil    = ' '
    dll_integers  = 0
    dll_reals     = 0.0_hp
    dll_strings   = ' '
    !
    dll_function_settle  => trapar%dll_function_settle
    dll_name_settle      => trapar%dll_name_settle
    dll_handle_settle    => trapar%dll_handle_settle
    dll_usrfil_settle    => trapar%dll_usrfil_settle
    dll_integers_settle  => trapar%dll_integers_settle
    dll_reals_settle     => trapar%dll_reals_settle
    dll_strings_settle   => trapar%dll_strings_settle
    iform_settle         => trapar%iform_settle
    par_settle           => trapar%par_settle
    !
    dll_function_settle  = ' '
    dll_name_settle      = ' '
    dll_handle_settle    = 0
    dll_usrfil_settle    = ' '
    dll_integers_settle  = 0
    dll_reals_settle     = 0.0_hp
    dll_strings_settle   = ' '
    iform_settle         = 0
    par_settle           = 0.0_fp
    !
    nouttot              => trapar%nouttot
    noutpar              => trapar%noutpar
    ioutpar              => trapar%ioutpar
    outpar_name          => trapar%outpar_name
    outpar_longname      => trapar%outpar_longname
    !
    nouttot              = 0
    noutpar              = 0
    ioutpar              = 0
    outpar_name          = ''
    outpar_longname      = ''
end subroutine initrafrm


subroutine rdtrafrm(lundia    ,error     ,filtrn    ,lsedtot   , &
                  & ipardef   ,rpardef   ,npardef   ,trapar    , &
                  & sedparout ,sedtyp    ,sedblock  ,dims      , &
                  & julrefday ,max_mud_sedtyp)
!!--description-----------------------------------------------------------------
!
! Reads transport formula and parameters
!
!!--declarations----------------------------------------------------------------
    use precision
    use sediment_basics_module, only: SEDTYP_SAND
    use morphology_data_module
    use properties, only: tree_data
    use grid_dimens_module 
    !
    ! Arguments
    !
    integer                      , intent(in)   :: npardef        !< 
    integer                      , intent(in)   :: lundia         !< unit number of diagnostic output
    logical                      , intent(out)  :: error          !< error flag
    integer                      , intent(in)   :: lsedtot        !< total number of sediment fractions
    type (trapar_type)           , target       :: trapar         !< 
    character(*)                 , intent(in)   :: filtrn         !< filename of default sand transport formula
    integer, dimension(2,npardef), intent(in)   :: ipardef        !< 
    real(fp), dimension(npardef) , intent(in)   :: rpardef        !<
    logical                      , intent(in)   :: sedparout      !< flag whether intermediate output of sediment transport formula should be written
    integer, dimension(:)        , intent(in)   :: sedtyp         !< sediment type
    type(tree_data), dimension(:), intent(in)   :: sedblock       !< 
    type (griddimtype), target   , intent(in)   :: dims           !< grid dimensions
    integer                      , intent(in)   :: julrefday      !< Julian reference date
    integer                      , intent(in)   :: max_mud_sedtyp !< last sediment type counted as "mud"
!
! Local variables
!
    integer                        , pointer :: max_integers
    integer                        , pointer :: max_reals
    integer                        , pointer :: max_strings
    integer                        , pointer :: max_integers_settle
    integer                        , pointer :: max_reals_settle
    integer                        , pointer :: max_strings_settle
    integer                        , pointer :: npar
    integer                        , pointer :: nouttot
    integer       , dimension(:)   , pointer :: noutpar
    integer       , dimension(:,:) , pointer :: ioutpar
    character(256), dimension(:,:) , pointer :: outpar_name 
    character(256), dimension(:,:) , pointer :: outpar_longname 
    integer                        , pointer :: nparfile
    character(256), dimension(:)   , pointer :: dll_name
    character(256), dimension(:)   , pointer :: dll_function
    integer(pntrsize), dimension(:), pointer :: dll_handle
    integer       , dimension(:)   , pointer :: dll_integers
    real(hp)      , dimension(:)   , pointer :: dll_reals
    character(256), dimension(:)   , pointer :: dll_strings
    character(256), dimension(:)   , pointer :: dll_usrfil
    character(256), dimension(:)   , pointer :: flstrn
    integer,        dimension(:)   , pointer :: iform
    character(256), dimension(:)   , pointer :: name
    real(fp),       dimension(:,:) , pointer :: par
    character(25) , dimension(:,:) , pointer :: parname
    character(256), dimension(:,:) , pointer :: parfilename
    integer,        dimension(:,:) , pointer :: iparfile
    !
    integer           :: i
    integer           :: iform1tmp
    integer           :: iformdef
    integer           :: istat
    integer           :: ll
!
!! executable statements -------------------------------------------------------
!
    max_integers         => trapar%max_integers
    max_reals            => trapar%max_reals
    max_strings          => trapar%max_strings
    max_integers_settle  => trapar%max_integers_settle
    max_reals_settle     => trapar%max_reals_settle
    max_strings_settle   => trapar%max_strings_settle
    npar                 => trapar%npar
    nparfile             => trapar%nparfile
    !
    iform         => trapar%iform
    flstrn        => trapar%flstrn
    name          => trapar%name
    par           => trapar%par
    parname       => trapar%parname
    parfilename   => trapar%parfilename
    iparfile      => trapar%iparfile
    !
    dll_name      => trapar%dll_name
    dll_function  => trapar%dll_function
    dll_handle    => trapar%dll_handle
    dll_usrfil    => trapar%dll_usrfil
    dll_integers  => trapar%dll_integers
    dll_reals     => trapar%dll_reals
    dll_strings   => trapar%dll_strings
    !
    nouttot       => trapar%nouttot
    noutpar       => trapar%noutpar
    ioutpar       => trapar%ioutpar
    outpar_name   => trapar%outpar_name
    outpar_longname => trapar%outpar_longname
    !
    error = .false.
    !
    write (lundia, *)
    write (lundia, '(a)') 'Determining transport formulation(s)'
    !
    ! for cohesive sediment fractions formula -3 (Partheniades-Krone) is the default formula
    !
    do ll=1, lsedtot
       if (sedtyp(ll) <= max_mud_sedtyp .and. iform(ll)==-999) iform(ll) = -3
    enddo
    !
    ! backup value of iform(1) because the first index will be used to load
    ! the default sediment transport settings.
    !
    iform1tmp = iform(1)
    if (filtrn == ' ') then
       ! keyword not specified, using Van Rijn (1993) as default
       iform(1) = -1
    else
       write (lundia, '(a,a)') 'Reading: ',trim(filtrn)
    endif
    call rdtrafrm0(lundia    ,error     ,iform     ,npar      ,par       , &
                 & parfilename,iparfile  , &
                 & nparfile  ,0         ,SEDTYP_SAND, max_mud_sedtyp, &
                 & filtrn    ,name      ,dll_handle,dll_name  ,dll_function, &
                 & dll_usrfil,ipardef   ,rpardef   ,npardef   ,sedblock  , &
                 & parname    ,noutpar   ,outpar_name, outpar_longname)
    if (error) return
    iformdef = iform(1)
    do ll=2, lsedtot
       if (iform(ll)==-999 .and. flstrn(ll)==' ') then
          name(ll)              = name(1)
          dll_handle(ll)        = dll_handle(1)
          dll_function(ll)      = dll_function(1)
          dll_usrfil(ll)        = dll_usrfil(1)
          par(:,ll)             = par(:,1)
          parname(:,ll)         = parname(:,1)
          parfilename(:,ll)     = parfilename(:,1)
          iparfile(:,ll)        = iparfile(:,1)
          noutpar(ll)           = noutpar(1)
          outpar_name(:,ll)     = outpar_name(:,1)
          outpar_longname(:,ll) = outpar_longname(:,1)
       endif
    enddo
    !
    ! restore the iform(1) value if it had a specified value
    !
    iform(1) = iform1tmp
    if (iform1tmp/=-999 .or. flstrn(1)/=' ') then
       name(1) = ' '
       dll_handle(1) = 0
       dll_function(1) = ' '
       dll_usrfil(1) = ' '
       par(:,1) = 0.0_fp
       parname(:,1) = ' '
       parfilename(:,1) = ' '
       iparfile(:,1) = 0
       noutpar(1) = 0
    endif
    !
    do ll = 1,lsedtot
       if (flstrn(ll) /= ' ') write (lundia, '(a,a)') 'Reading: ',trim(flstrn(ll))
       if (flstrn(ll) /= ' ' .or. iform(ll)/=-999) then
          call rdtrafrm0(lundia    ,error     ,iform     ,npar      ,par       , &
                       & parfilename,iparfile  , &
                       & nparfile  ,ll        ,sedtyp(ll), max_mud_sedtyp, &
                       & flstrn(ll),name      ,dll_handle,dll_name  ,dll_function, &
                       & dll_usrfil,ipardef   ,rpardef   ,npardef   ,sedblock  , &
                       & parname   ,noutpar   ,outpar_name, outpar_longname)
       else
          iform(ll) = iformdef
       endif
       if (error) return
    enddo
    !
    call rdtraparfiles(lundia    ,error     ,lsedtot   ,trapar    , &
                   & dims      ,julrefday )
    if (error) return
    !
    nouttot = 0
    if (sedparout) then
       do ll = 1, lsedtot
          do i = 1, noutpar(ll)
              nouttot = nouttot+1
              ioutpar(i,ll) = nouttot
          enddo
       enddo
       allocate(trapar%outpar(nouttot,dims%nmlb:dims%nmub), stat=istat)
       if (istat==0) trapar%outpar = 0.0_fp
    endif
    !
end subroutine rdtrafrm


subroutine rdtrafrm0(lundia    ,error     ,iform     ,npar      ,par       , &
                   & parfilename,iparfile  , &
                   & nparfile  ,ifrac     ,sedtyp    ,max_mud_sedtyp, &
                   & flname    ,name      ,dll_handle,dll_name  ,dll_func  , &
                   & dll_usrfil,ipardef   ,rpardef   ,npardef   ,sedblock  , &
                   & parname   ,noutpar   ,outpar_name, outpar_longname)
!!--description-----------------------------------------------------------------
!
! Reads transport formula and parameters
!
!!--declarations----------------------------------------------------------------
    use precision
    use properties
    use string_module
    use message_module
    use system_utils, only: SHARED_LIB_PREFIX, SHARED_LIB_EXTENSION
    use morphology_data_module, only: PARSOURCE_FIELD, PARSOURCE_TIME
!
! Arguments
!
    integer                          , intent(in)    :: npardef
    integer                          , intent(in)    :: lundia           !< unit number of diagnostic output
    logical                          , intent(out)   :: error
    integer, dimension(:)                            :: iform
    integer                          , intent(in)    :: npar
    integer, dimension(:)            , intent(out)   :: noutpar
    character(256)   , dimension(:,:), intent(out)   :: outpar_name
    character(256)   , dimension(:,:), intent(out)   :: outpar_longname
    integer(pntrsize), dimension(:)  , intent(out)   :: dll_handle
    real(fp)    , dimension(:,:)                     :: par
    character(*), dimension(:,:)                     :: parname
    character(*), dimension(:,:)                     :: parfilename
    integer     , dimension(:,:)                     :: iparfile
    integer                                          :: nparfile
    integer                          , intent(in)    :: ifrac
    integer                          , intent(in)    :: sedtyp
    integer                          , intent(in)    :: max_mud_sedtyp
    character(*)                                     :: flname
    character(*), dimension(:)                       :: name
    character(*), dimension(:)                       :: dll_name
    character(*), dimension(:)                       :: dll_func
    character(*), dimension(:)                       :: dll_usrfil
    integer, dimension(2,npardef)    , intent(in)    :: ipardef
    real(fp), dimension(npardef)     , intent(in)    :: rpardef
    type(tree_data), dimension(:)    , intent(in)    :: sedblock
!
! Local variables
!
    integer                                    :: i
    integer                                    :: i10
    integer                                    :: inp
    integer                                    :: iost
    integer                                    :: istat
    integer                                    :: l
    integer                                    :: lfile
    integer                                    :: nparreq
    integer                                    :: nparopt
    integer                                    :: version
    integer                      , external    :: open_shared_library
    logical                                    :: lex
    real(fp)                                   :: nodef
    real(fp)       , dimension(:), allocatable :: pardef
    character(3)                               :: key
    character(10)                              :: versionstring
    character(80)                              :: string
    character(256)                             :: errmsg
    character(256)                             :: rec
    type(tree_data)              , pointer     :: tran_ptr
    type(tree_data)              , pointer     :: sed_ptr
!
!! executable statements -------------------------------------------------------
!
    ! ifrac will be 0 for the default transport formula, put data in 1 index instead
    !
    l = max(ifrac,1)
    allocate(sed_ptr)
    allocate(pardef(npar))
    sed_ptr = sedblock(l)
    !
    if (flname == ' ') then
       ! parameters not in a separate file -- read them from the block in the .sed file
       version = -1
    else
       write (lundia, '(a,a)') '    Input file                   : ',trim(flname)
       call remove_leading_spaces(flname, lfile)
       !
       inquire (file = flname(1:lfile), exist = lex)
       !
       if (.not.lex) then
          errmsg = 'TraFrm file '//trim(flname)//' does not exist'
          call write_error(errmsg, unit=lundia)
          error = .true.
          return
       endif
       !
       ! Create TransportFormula branch in input tree
       !
       call tree_create( 'TransportFormula Input', tran_ptr )
       call tree_put_data( tran_ptr, transfer(trim(flname),node_value), 'STRING' )
       !
       ! Put trafrm-file in input tree
       !
       call prop_file('ini',trim(flname),tran_ptr,istat)
       if (istat /= 0) then
          select case (istat)
          case(1)
             errmsg = FILE_NOT_FOUND // trim(flname)
             call write_error(errmsg, unit=lundia)
          case(3)
             errmsg = PREMATURE_EOF // trim(flname)
             call write_error(errmsg, unit=lundia)
          case default
             errmsg = FILE_READ_ERROR // trim(flname)
             call write_error(errmsg, unit=lundia)
          endselect
          error = .true.
          return
       endif
       !
       ! Check version number of trafrm-file
       !
       version = 0
       versionstring = '00.00'
       call prop_get(tran_ptr,'TransportFormulaFileInformation','FileVersion',versionstring)
       if (trim(versionstring) == '01.00') then
          write (lundia, '(a,a)') '    Version number of input file  : ', trim(versionstring)
          version = 1
          !
          rec  = ' '
          call prop_get(tran_ptr,'TransportFormula','DLL',rec)
          call prop_get(tran_ptr,'TransportFormula','TranspLib',rec)
          dll_name(l) = rec
          if (rec /= ' ') then
             name(l) = ' '
             call prop_get(tran_ptr,'TransportFormula','Name',name(l))
             !
             if (sedtyp <= max_mud_sedtyp) then
                iform(l) = 21 ! user defined version of EROSILT
             else
                iform(l) = 15 ! user defined version of EQTRAN
             endif
             write(rec,'(3a)') SHARED_LIB_PREFIX, trim(dll_name(l)), SHARED_LIB_EXTENSION
             !
             ! Get handle to the shared library (dll/so)
             !
             istat = 0
             istat = open_shared_library(dll_handle(l), rec)
             if (istat /= 0) then
                errmsg = 'Can not open shared library '//trim(rec)
                call write_error(errmsg, unit=lundia)
                error = .true.
                return
             endif
             dll_func(l) = ' '
             call prop_get(tran_ptr,'TransportFormula','function',dll_func(l))
             call prop_get(tran_ptr,'TransportFormula','TranspFunction',dll_func(l))
             !
             dll_usrfil(l) = ' '
             call prop_get(tran_ptr,'TransportFormula','InputFile',dll_usrfil(l))
             call prop_get(tran_ptr,'TransportFormula','TranspInput',dll_usrfil(l))
             !
             write (lundia, '(4a)') '    User defined routine ',trim(dll_func(l)),' from ',trim(dll_name(l))
          else
             iform(l) = -999
             call prop_get(tran_ptr,'TransportFormula','Number',iform(l))
             if (iform(l) == -999) then
                errmsg = 'Unable to find transport formula information in '//trim(flname)
                call write_error(errmsg, unit=lundia)
                error = .true.
                return
             endif
             write (lundia, '(a,i3)') '    Transport formula used =', iform(l)
          endif
       else
          write (lundia, '(a)') '*** WARNING Transport file "'//flname(1:lfile)//'" uses old MOR-format. Please update to keyword-based file.'
          !
          open (newunit = inp, file = flname(1:lfile),status = 'old', iostat = iost)
          if (iost/=0) then
             errmsg = FILE_NOT_FOUND // flname(1:lfile)
             call write_error(errmsg, unit=lundia)
             error = .true.
             return
          endif
          string = ' '
          read (inp, '(a)', iostat = iost) string
          do while (index(string, 'IFORM')==0 .and. iost==0)
             read (inp, '(a)', iostat = iost) string
          enddo
          if (iost==0) then
             backspace (inp)
             read (inp, *, iostat = iost) iform(l)
          endif
          if (iost/=0) then
             errmsg = 'Unable to locate transport formula number IFORM'
             call write_error(errmsg, unit=lundia)
             error = .true.
             return
          endif
          !
          if (iform(l)>=0 .and. iform(l)<10) then
             write (key, '(a,i1)') '#', iform(l)
          else
             write (key, '(a,i2)') '#', iform(l)
          endif
          read (inp, '(a)', iostat = iost) string
          do while (index(string, key)==0 .and. iost==0)
             read (inp, '(a)', iostat = iost) string
          enddo
          if (iost/=0 .and. iform(l)>0) then
             errmsg = 'Transport formula parameters not found'
             call write_error(errmsg, unit=lundia)
             error = .true.
             return
          endif
       endif
    endif
    !
    ! Get transport formula name, parameter names and default values
    !
    if (iform(l) == 15 .or. iform(l) == 21) then
       rec  = ' '
       call prop_get(sed_ptr,'Sediment','DLL',rec)
       call prop_get(sed_ptr,'Sediment','TranspLib',rec)
       dll_name(l) = rec
       if (rec == ' ') then
          ! No Library name found ...
       else
          name(l) = ' '
          call prop_get(sed_ptr,'Sediment','Name',name(l))
          !
          write(rec,'(3a)') SHARED_LIB_PREFIX, trim(dll_name(l)), SHARED_LIB_EXTENSION
          !
          ! Get handle to the shared library (dll/so)
          !
          istat = 0
          istat = open_shared_library(dll_handle(l), rec)
          if (istat /= 0) then
             errmsg = 'Can not open shared library '//trim(rec)
             call write_error(errmsg, unit=lundia)
             error = .true.
             return
          endif
          dll_func(l) = ' '
          call prop_get(sed_ptr,'Sediment','function',dll_func(l))
          call prop_get(sed_ptr,'Sediment','TranspFunction',dll_func(l))
          !
          dll_usrfil(l) = ' '
          call prop_get(sed_ptr,'Sediment','InputFile',dll_usrfil(l))
          call prop_get(sed_ptr,'Sediment','TranspInput',dll_usrfil(l))
          !
          write (lundia, '(4a)') '    User defined routine ',trim(dll_func(l)),' from ',trim(dll_name(l))
       endif
       nparreq = 0
       nparopt = 0
    else
       call traparams(iform(l),name(l),nparreq   ,nparopt   ,parname(:,l)   , &
                    & pardef, nodef, noutpar(l), outpar_name(:,l), outpar_longname(:,l))
       if (name(l) == ' ') then
          error      = .true.
          write(errmsg,'(A,I0,A)') 'Transport formula number ',iform(l),' is not implemented'
          call write_error(errmsg, unit=lundia)
          return
       endif
    endif
    !
    ! Overrule default values with values read from mor file
    !
    call getpardef(ipardef   ,rpardef   ,npardef   ,iform(l),pardef    )
    !
    par(:,l) = 0
    if (version==0) then
       do i = 1, nparreq
          i10 = i+10
          read (inp, *) par(i10,l)
       enddo
       do i = nparreq+1, nparreq+nparopt
          i10 = i+10
          read(inp,*,iostat=iost) par(i10,l)
          if (iost/=0) par(i10,l) = pardef(i)
       enddo
    else
       do i = 1, nparreq+nparopt
          i10 = i+10
          if (version==1) then
             par(i10,l) = pardef(i)
             parfilename(i10,l) = ' '
             !
             call prop_get(tran_ptr,'TransportFormula',parname(i,l),parfilename(i10,l))
             if (parfilename(i10,l) /= ' ') then
                call prop_get(tran_ptr,'TransportFormula',parname(i,l),par(i10,l))
             elseif (associated(sed_ptr%node_name)) then
                ! parameter not in transport file, now check sedblock
                call prop_get(sed_ptr,'Sediment',parname(i,l),parfilename(i10,l))
                call prop_get(sed_ptr,'Sediment',parname(i,l),par(i10,l))
             endif
          else
             par(i10,l) = pardef(i)
             parfilename(i10,l) = ' '
             !
             if (ifrac>0 .and. associated(sed_ptr%node_name)) then
                ! try to locate parameter in sedblock
                call prop_get(sed_ptr,'Sediment',parname(i,l),parfilename(i10,l))
                call prop_get(sed_ptr,'Sediment',parname(i,l),par(i10,l))
             endif
          endif
          !
          if (parfilename(i10,l) /= ' ') then
             if (file_exists(flname, parfilename(i10,l))) then
                nparfile = nparfile + 1
                iparfile(i10,l) = nparfile
             endif
          endif
          !
          if (iparfile(i10,l) == 0 .and. comparereal(par(i10,l),nodef) == 0) then
             error  = .true.
             errmsg = 'No value obtained for parameter '//trim(parname(i,l))//' without default value.'
             call write_error(errmsg, unit=lundia)
          endif
       enddo
    endif
    !
    if (version==0) then
       close(inp)
    endif
    if (version>=0) then
       call tree_destroy( tran_ptr )
    endif
    deallocate(sed_ptr)
    deallocate(pardef)
end subroutine rdtrafrm0


!> Check whether a file exists
function file_exists(flname, parfile) result (exists)
    character(*)                 , intent(in)    :: flname
    character(*)                 , intent(inout) :: parfile

    logical :: exists !< flag indicating whether file exists
    
    call combinepaths(flname, parfile)
    inquire (file = parfile, exist = exists)
end function file_exists


!> Read transport formula parameter files (time- or space-varying)
subroutine rdtraparfiles(lundia    ,error     ,lsedtot   ,trapar    , &
                      & dims      ,julrefday )
    use precision
    use morphology_data_module
    use grid_dimens_module 
    use message_module, only: write_error
    use table_handles, only: readtable, gettable
    !
    ! Arguments
    !
    integer                      , intent(in)   :: lundia    !< unit number of diagnostic output
    logical                      , intent(out)  :: error     !< error flag
    integer                      , intent(in)   :: lsedtot   !< total number of sediment fractions
    type (trapar_type)           , target       :: trapar    !< transport parameters
    type (griddimtype), target   , intent(in)   :: dims      !< grid dimensions
    integer                      , intent(in)   :: julrefday !< Julian reference date
!
! Local variables
!
    integer                        , pointer :: npar
    integer                        , pointer :: nparfile
    character(256)                 , pointer :: parfilename(:,:)
    integer                        , pointer :: iparfile(:,:)
    type(parfile_type)             , pointer :: parfile(:)
    !
    integer           :: i
    integer           :: i10
    integer           :: istat
    integer           :: j
    integer           :: ll
    character(256)    :: filename
    character(256)    :: errmsg
    character(11)     :: fmttmp
    
!
!! executable statements -------------------------------------------------------
!
    npar          => trapar%npar
    nparfile      => trapar%nparfile
    parfilename   => trapar%parfilename
    iparfile      => trapar%iparfile
    !
    error  = .false.
    fmttmp = 'formatted'
    istat  = 0
    !
    allocate(trapar%parfile(nparfile), stat=istat)
    if (istat/=0) then
       error = .true.
       return
    endif
    parfile       => trapar%parfile
    !
    do ll = 1, lsedtot
       do i10 = 11, npar
          j = trapar%iparfile(i10,ll)
          if (j>0) then
             i = i10-10
             filename = trapar%parfilename(i10,ll)
             write (lundia, '(a,a)') 'Reading: ',trim(filename)
             !
             parfile(j)%source = determine_source_type(filename)
             !
             select case (parfile(j)%source)
             case (PARSOURCE_FIELD)
                 allocate(parfile(j)%parfld(dims%nmlb:dims%nmub), stat=istat)
                 if (istat/=0) then
                    error = .true.
                    return
                 endif
                 call depfil_stm(lundia     ,error      ,filename   ,fmttmp    , &
                               & parfile(j)%parfld,1          ,1          ,dims      , errmsg)
                 if (error) then 
                     call write_error(errmsg, unit=lundia)
                     return
                 endif
                 
             case (PARSOURCE_TIME)
                 call readtable(parfile(j)%ts, filename, julrefday, errmsg)
                 if (errmsg /= ' ') then
                     call write_error(errmsg, unit=lundia)
                     error = .true.
                     return
                 endif
                 call gettable(parfile(j)%ts, 'General' ,trim(trapar%parname(i,ll)), parfile(j)%itable_ts, parfile(j)%ipar_ts, parfile(j)%npar_ts, 1, errmsg)
                 if (errmsg /= ' ') then
                     call write_error(errmsg, unit=lundia)
                     error = .true.
                     return
                 endif
                 parfile(j)%irec_ts   = 1 ! start with record 1
                 parfile(j)%refjulday = julrefday ! store reference date
                 parfile(j)%timhr     = -1.0e10_fp
                 parfile(j)%par       = -999.0_fp
                 
             case default
             end select
          endif
       enddo
    enddo
end subroutine rdtraparfiles


!> determine the type of the specified file
function determine_source_type(filename) result (source)
   use morphology_data_module, only: PARSOURCE_UNKNOWN, PARSOURCE_FIELD, PARSOURCE_TIME
   use system_utils, only: split_filename
   use string_module, only: str_lower
   
   character(len=*), intent(in) :: filename !< full name of the file
   integer                      :: source   !< file type
   
   character(len=256) :: path  !< file path
   character(len=256) :: file  !< file name
   character(len=256) :: ext   !< file extension
   
   call split_filename(filename, path, file, ext)
   call str_lower(ext)
   select case (ext)
   case ('.xyz','.dep')
      source = PARSOURCE_FIELD
   case ('.bct')
      source = PARSOURCE_TIME
   case default
      source = PARSOURCE_UNKNOWN
   end select
end function determine_source_type


subroutine setpardeflog(ipardef   ,rpardef   ,npardef   ,iform     , &
                      & ipar      ,val       )
    use precision
!
! Arguments
!
    integer                       , intent(in)    :: npardef
    integer, dimension(2,npardef) , intent(inout) :: ipardef
    integer                       , intent(in)    :: iform
    integer                       , intent(in)    :: ipar
    real(fp), dimension(npardef)  , intent(inout) :: rpardef
    logical                       , intent(in)    :: val
!
! Local variables
!
    real(fp)                                      :: rval
!
!! executable statements -------------------------------------------------------
!
    rval = 0.0_fp
    if (val) rval = 1.0_fp
    call setpardefreal(ipardef   ,rpardef   ,npardef   ,iform     , &
                     & ipar      ,rval      )
end subroutine setpardeflog


subroutine setpardefint(ipardef   ,rpardef   ,npardef   ,iform     , &
                      & ipar      ,val       )
!!--description-----------------------------------------------------------------
!
!
!
!!--declarations----------------------------------------------------------------
    use precision
!
! Arguments
!
    integer                       , intent(in)    :: npardef
    integer, dimension(2,npardef) , intent(inout) :: ipardef
    integer                       , intent(in)    :: iform
    integer                       , intent(in)    :: ipar
    real(fp), dimension(npardef)  , intent(inout) :: rpardef
    integer                       , intent(in)    :: val
!
! Local variables
!
    real(fp)                                      :: rval
!
!! executable statements -------------------------------------------------------
!
    rval = real(val,fp)
    call setpardefreal(ipardef   ,rpardef   ,npardef   ,iform     , &
                     & ipar      ,rval      )
end subroutine setpardefint


subroutine setpardefreal(ipardef   ,rpardef   ,npardef   ,iform     , &
                       & ipar      ,val       )
!!--description-----------------------------------------------------------------
!
!
!
!!--declarations----------------------------------------------------------------
    use precision
!
! Arguments
!
    integer                       , intent(in)    :: npardef
    integer, dimension(2,npardef) , intent(inout) :: ipardef
    integer                       , intent(in)    :: iform
    integer                       , intent(in)    :: ipar
    real(fp), dimension(npardef)  , intent(inout) :: rpardef
    real(fp)                      , intent(in)    :: val
!
! Local variables
!
    integer                                       :: i
!
!! executable statements -------------------------------------------------------
!
    do i = 1,npardef
       if (ipardef(1,i) == 0) then
          ipardef(1,i) = iform
          ipardef(2,i) = ipar
          rpardef(i)   = val
          return
       endif
    enddo
end subroutine setpardefreal


subroutine getpardef(ipardef   ,rpardef   ,npardef   ,iform     ,pardef    )
!!--description-----------------------------------------------------------------
!
!
!
!!--declarations----------------------------------------------------------------
    use precision
!
! Arguments
!
    integer                       , intent(in)    :: npardef
    integer, dimension(2,npardef) , intent(in)    :: ipardef
    integer                       , intent(in)    :: iform
    real(fp), dimension(npardef)  , intent(in)    :: rpardef
    real(fp), dimension(*)        , intent(inout) :: pardef
!
! Local variables
!
    integer                                       :: i
!
!! executable statements -------------------------------------------------------
!
    do i = 1, npardef
       if (ipardef(1,i) == iform) then
          pardef(ipardef(2,i)) = rpardef(i)
       elseif (ipardef(1,i) == 0) then
          return
       endif
    enddo
end subroutine getpardef


subroutine echotrafrm(lundia    ,trapar      ,ifrac     )
!!--description-----------------------------------------------------------------
!
! Report transport formula and associated parameters to diag file
!
!!--declarations----------------------------------------------------------------
    use precision
    use morphology_data_module, only: trapar_type
!
! Arguments
!
    integer                                      :: lundia  !  Description and declaration in inout.igs
    type(trapar_type)              , intent(in)  :: trapar
    integer                        , intent(in)  :: ifrac
!
! Local variables
!
    integer                        :: i
    integer                        :: i10
!
!! executable statements -------------------------------------------------------
!
    write (lundia, '(a,a)') '    Formula name              : ',trim(trapar%name(ifrac))
    if (trapar%iform(ifrac)==15 .or. trapar%iform(ifrac)==21) then
       write (lundia, '(a,a)') '    Dynamic library           : ',trim(trapar%dll_name(ifrac))
       write (lundia, '(a,a)') '    Function in library       : ',trim(trapar%dll_function(ifrac))
       if (trapar%dll_usrfil(ifrac) /= ' ') then
          write (lundia, '(a,a)') '    Input file for function   : ',trim(trapar%dll_usrfil(ifrac))
       endif
    else
       write (lundia, '(a,i3)') '    Transport formula used    :', trapar%iform(ifrac)
       !
       do i = 1, trapar%npar
          if (trapar%parname(i,ifrac) == ' ') then
             exit
          endif
          !
          i10 = 10+i
          if (trapar%iparfile(i10,ifrac) == 0) then
             write (lundia, '(3a,e12.4)') '    ',trapar%parname(i,ifrac),' :',trapar%par(i10,ifrac)
          else
             write (lundia, '(4a)') '    ',trapar%parname(i,ifrac),' : ',trim(trapar%parfilename(i10,ifrac))
          endif
       enddo
    endif
end subroutine echotrafrm


subroutine traparams(iform     ,name      ,nparreq   ,nparopt   ,parkeyw   , &
                   & pardef    ,nodef     ,noutpar   ,outpar_name, outpar_longname)
!!--description-----------------------------------------------------------------
!
! Provides characteristics of built-in transport formulae
!
!!--declarations----------------------------------------------------------------
    use precision
!
! Arguments
!
    integer                       , intent(in)  :: iform
    character(*)                  , intent(out) :: name
    integer                       , intent(out) :: nparreq
    integer                       , intent(out) :: nparopt
    real(fp)                      , intent(out) :: pardef(:)
    character(*)                  , intent(out) :: parkeyw(:)
    real(fp)                      , intent(out) :: nodef
    integer        , optional     , intent(out) :: noutpar
    character(*)   , optional     , intent(out) :: outpar_name(:)
    character(*)   , optional     , intent(out) :: outpar_longname(:)
!
! Local variables
!
!
!! executable statements -------------------------------------------------------
!
    nodef   = -999999.0_fp
    !
    if (iform /= 15) name = ' '
    nparreq = 0
    nparopt = 0
    if (present(noutpar)) then
       noutpar = 0
    endif
    parkeyw = ' '
    pardef  = nodef
    if (iform == -4) then
       name       = 'Van der A et al. (2013): SANTOSS extended Van Rijn (2007)'
       nparopt    = 13
       parkeyw(1) = 'IopSus'
       pardef(1)  = 0.0_fp
       parkeyw(2) = 'Pangle'
       pardef(2)  = 0.0_fp
       parkeyw(3) = 'Fpco'
       pardef(3)  = 1.0_fp
       parkeyw(4) = 'Subiw'
       pardef(4)  = 51.0_fp
       parkeyw(5) = 'EpsPar'
       pardef(5)  = 0.0_fp ! false
       parkeyw(6) = 'GamTcr'
       pardef(6)  = 1.5_fp
       parkeyw(7) = 'SalMax'
       pardef(7)  = 0.0_fp
       parkeyw(8) = 'BetaM'
       pardef(8)  = 3.0_fp
       parkeyw(9) = 'Wform'
       pardef(9)  = 1.0_fp
       ! NOTE UP TO HERE IDENTICAL TO VAN RIJN (2007) FORMULA -2: Numbers/parameters must match!
       parkeyw(10)='SW_effects'
       pardef(10) = 1.0_fp
       parkeyw(11)='AS_effects'
       pardef(11) = 1.0_fp
       parkeyw(12)='PL_effects'
       pardef(12) = 1.0_fp
       parkeyw(13)='SL_effects'
       pardef(13) =  1.0_fp
       if (present(noutpar)) then
          noutpar = 24
          outpar_name( 1)     = 'uwc'
          outpar_longname( 1) = 'orbital velocity at crest' ! m/s
          outpar_name( 2)     = 'uwt'
          outpar_longname( 2) = 'orbital velocity in trough' ! m/s
          outpar_name( 3)     = 'rh'
          outpar_longname( 3) = 'ripple height' ! m
          outpar_name( 4)     = 'ksw'
          outpar_longname( 4) = 'wave roughness height' ! m
          outpar_name( 5)     = 'ksc'
          outpar_longname( 5) = 'current roughness height' ! m
          outpar_name( 6)     = 'ucrepr'
          outpar_longname( 6) = 'representative velocity at crest' ! m/s
          outpar_name( 7)     = 'utrepr'
          outpar_longname( 7) = 'representative velocity in trough' ! m/s
          outpar_name( 8)     = 'fcwc'
          outpar_longname( 8) = 'friction factor at crest' ! -
          outpar_name( 9)     = 'fcwt'
          outpar_longname( 9) = 'friction factor in trough' ! -
          outpar_name(10)     = 'screpr'
          outpar_longname(10) = 'representative shear stress at crest' ! -
          outpar_name(11)     = 'strepr'
          outpar_longname(11) = 'representative shear stress in trough' ! -
          outpar_name(12)     = 'pc'
          outpar_longname(12) = 'phase lag parameter at crest' ! -
          outpar_name(13)     = 'pt'
          outpar_longname(13) = 'phase lag parameter in trough' ! -
          outpar_name(14)     = 'Occ'
          outpar_longname(14) = 'load entrained and transported during crest period' ! -
          outpar_name(15)     = 'Otc'
          outpar_longname(15) = 'load entrained during trough period and transported during crest period' ! -
          outpar_name(16)     = 'Ott'
          outpar_longname(16) = 'load entrained and transported during trough period' ! -
          outpar_name(17)     = 'Oct'
          outpar_longname(17) = 'load entrained during crest period and transported during trough period' ! -
          outpar_name(18)     = 'Tc'
          outpar_longname(18) = 'wave period at crest' ! s
          outpar_name(19)     = 'Tt'
          outpar_longname(19) = 'wave period in trough' ! s
          outpar_name(20)     = 'Phicx'
          outpar_longname(20) = 'dimensionless sediment transport at crest' ! -
          outpar_name(21)     = 'Phitx'
          outpar_longname(21) = 'dimensionless sediment transport in trough' ! -
          outpar_name(22)     = 'Qsu'
          outpar_longname(22) = 'volumetric transport rate' ! m2/s
          outpar_name(23)     = 'as'
          outpar_longname(23) = 'skewness' ! -
          outpar_name(24)     = 'ak'
          outpar_longname(24) = 'asymmetry' ! -
       endif
    elseif (iform == -3) then
       name       = 'Partheniades-Krone'
       nparreq    = 3
       parkeyw(1) = 'EroPar'
       pardef(1)  = 0.0_fp
       parkeyw(2) = 'TcrSed'
       pardef(2)  = 0.0_fp
       parkeyw(3) = 'TcrEro'
       pardef(3)  = 0.0_fp
       nparopt    = 5
       parkeyw(4) = 'TcrFluff'
       pardef(4)  = 0.0_fp
       parkeyw(5) = 'ParFluff0'
       pardef(5)  = 0.0_fp
       parkeyw(6) = 'ParFluff1'
       pardef(6)  = 0.0_fp
       parkeyw(7) = 'DepEff'
       pardef(7)  = -1.0_fp
       parkeyw(8) = 'PowerN'
       pardef(8)  = 1.0_fp
    elseif (iform == -2) then
       name       = 'Van Rijn (2007): TRANSPOR2004'
       nparopt    = 9
       parkeyw(1) = 'IopSus'
       pardef(1)  = 0.0_fp
       parkeyw(2) = 'Pangle'
       pardef(2)  = 0.0_fp
       parkeyw(3) = 'Fpco'
       pardef(3)  = 1.0_fp
       parkeyw(4) = 'Subiw'
       pardef(4)  = 51.0_fp
       parkeyw(5) = 'EpsPar'
       pardef(5)  = 0.0_fp ! false
       parkeyw(6) = 'GamTcr'
       pardef(6)  = 1.5_fp
       parkeyw(7) = 'SalMax'
       pardef(7)  = 0.0_fp
       parkeyw(8) = 'BetaM'
       pardef(8)  = 3.0_fp
       parkeyw(9) = 'Wform'
       pardef(9)  = 1.0_fp
       ! NOTE PARAMETERS ADDED HERE MUST BE COPIED TO SANTOSS FORMULA -4: Numbers/parameters must match!
       if (present(noutpar)) then
          noutpar = 17
          outpar_name( 1)     = 'tauc'
          outpar_longname( 1) = 'bed shear stress due to currents' ! kg/(m s2)
          outpar_name( 2)     = 'tauwav'
          outpar_longname( 2) = 'bed shear stress due to waves' ! kg/(m s2)
          outpar_name( 3)     = 'taubcw'
          outpar_longname( 3) = 'bed shear stress due to currents and waves' ! kg/(m s2)
          outpar_name( 4)     = 'usus'
          outpar_longname( 4) = 'adjusted reference velocity' ! m/s
          outpar_name( 5)     = 'zusus'
          outpar_longname( 5) = 'height above bed for adjusted reference velocity' ! m
          outpar_name( 6)     = 'dss'
          outpar_longname( 6) = 'suspended sediment diameter' ! m
          outpar_name( 7)     = 'caks'
          outpar_longname( 7) = 'reference concentration' ! kg/m3
          outpar_name( 8)     = 'aks'
          outpar_longname( 8) = 'reference height' ! m
          outpar_name( 9)     = 'deltas'
          outpar_longname( 9) = 'deltas' ! m
          outpar_name(10)     = 'epsmxc'
          outpar_longname(10) = 'epsmax due to currents' ! -
          outpar_name(11)     = 'epsmax'
          outpar_longname(11) = 'epsmax due to waves' ! -
          outpar_name(12)     = 'uon'
          outpar_longname(12) = 'onshore velocity' ! m/s
          outpar_name(13)     = 'uoff'
          outpar_longname(13) = 'offshore velocity' ! m/s
          outpar_name(14)     = 'vcr'
          outpar_longname(14) = 'critical flow velocity' ! m/s
          outpar_name(15)     = 'uwb'
          outpar_longname(15) = 'velocity wave boundary layer' ! m/s
          outpar_name(16)     = 'awb'
          outpar_longname(16) = 'horizontal excursion of orbital motion' ! m
          outpar_name(17)     = 'rksrs'
          outpar_longname(17) = 'ripple roughness height' ! m
       endif
    elseif (iform == -1) then
       name       = 'Van Rijn (1993)'
       nparopt    = 8
       parkeyw(1) = 'IopSus'
       pardef(1)  = 0.0_fp
       parkeyw(2) = 'AksFac'
       pardef(2)  = 1.0_fp
       parkeyw(3) = 'RWave'
       pardef(3)  = 2.0_fp
       parkeyw(4) = 'RDC'
       pardef(4)  = 0.01_fp
       parkeyw(5) = 'RDW'
       pardef(5)  = 0.02_fp
       parkeyw(6) = 'IopKCW'
       pardef(6)  = 1.0_fp
       parkeyw(7) = 'EpsPar'
       pardef(7)  = 0.0_fp ! false
       parkeyw(8) = 'BetaM'
       pardef(8)  = 3.0_fp
       if (present(noutpar)) then
          noutpar = 16
          outpar_name( 1)     = 'tauc'
          outpar_longname( 1) = 'bed shear stress due to currents' ! kg/(m s2)
          outpar_name( 2)     = 'tauwav'
          outpar_longname( 2) = 'bed shear stress due to waves' ! kg/(m s2)
          outpar_name( 3)     = 'taubcw'
          outpar_longname( 3) = 'bed shear stress due to currents and waves' ! kg/(m s2)
          outpar_name( 4)     = 'usus'
          outpar_longname( 4) = 'adjusted reference velocity' ! m/s
          outpar_name( 5)     = 'zusus'
          outpar_longname( 5) = 'height above bed for adjusted reference velocity' ! m
          outpar_name( 6)     = 'dss'
          outpar_longname( 6) = 'suspended sediment diameter' ! m
          outpar_name( 7)     = 'caks'
          outpar_longname( 7) = 'reference concentration' ! kg/m3
          outpar_name( 8)     = 'aks'
          outpar_longname( 8) = 'reference height' ! m
          outpar_name( 9)     = 'deltas'
          outpar_longname( 9) = 'deltas' ! m
          outpar_name(10)     = 'epsmxc'
          outpar_longname(10) = 'epsmax due to currents' ! -
          outpar_name(11)     = 'epsmax'
          outpar_longname(11) = 'epsmax due to waves' ! -
          outpar_name(12)     = 'uon'
          outpar_longname(12) = 'onshore velocity' ! m/s
          outpar_name(13)     = 'uoff'
          outpar_longname(13) = 'offshore velocity' ! m/s
          outpar_name(14)     = 'vcr'
          outpar_longname(14) = 'critical flow velocity' ! m/s
          outpar_name(15)     = 'uwb'
          outpar_longname(15) = 'velocity wave boundary layer' ! m/s
          outpar_name(16)     = 'awb'
          outpar_longname(16) = 'horizontal excursion of orbital motion' ! m
       endif
    elseif (iform == 1) then
       name       = 'Engelund-Hansen (1967)'
       nparreq    = 1
       parkeyw(1) = 'ACal'
       nparopt    = 2
       parkeyw(2) = 'RouKs'
       pardef(2)  = 1.0_fp
       parkeyw(3) = 'SusFac'
       pardef(3)  = 0.0_fp
    elseif (iform == 2) then
       name       = 'Meyer-Peter-Mueller (1948)'
       nparreq    = 1
       parkeyw(1) = 'ACal'
    elseif (iform == 3) then
       name       = 'Swanby / Ackers-White'
       nparreq    = 1
       parkeyw(1) = 'ACal'
       nparopt    = 1
       parkeyw(2) = 'RouKs'
       pardef(2)  = 1.0_fp
    elseif (iform == 4) then
       name       = 'General formula'
       nparreq    = 5
       parkeyw(1) = 'ACal'
       parkeyw(2) = 'PowerB'
       parkeyw(3) = 'PowerC'
       parkeyw(4) = 'RipFac'
       parkeyw(5) = 'ThetaC'
       nparopt    = 5
       parkeyw(6) = 'SusACal'
       pardef(6)  = 0.0_fp
       parkeyw(7) = 'SusPowerB'
       pardef(7)  = 3.0_fp
       parkeyw(8) = 'SusPowerC'
       pardef(8)  = 0.0_fp
       parkeyw(9) = 'SusRipFac'
       pardef(9)  = 1.0_fp
       parkeyw(10)= 'SusThetaC'
       pardef(10) = 0.0_fp
    elseif (iform == 5) then
       name       = 'Bijker (1971)'
       nparreq    = 9
       parkeyw(1) = 'CalBs'
       parkeyw(2) = 'CalBd'
       parkeyw(3) = 'CritCs'
       parkeyw(4) = 'CritCd'
       parkeyw(5) = '[dummy]' !don't remove: backward compatibility!
       pardef(5)  = 1.0_fp
       parkeyw(6) = 'RouKs'
       pardef(6)  = -999.0_fp
       parkeyw(7) = 'WSettle'
       pardef(7)  = -999.0_fp
       parkeyw(8) = 'Porosity'
       pardef(8)  = -999.0_fp
       parkeyw(9) = 'TWave'
       pardef(9)  = -999.0_fp
       nparopt    = 3
       parkeyw(10) = 'FacA'
       pardef(10) = 0.0_fp
       parkeyw(11) = 'FacU'
       pardef(11) = 0.0_fp
       parkeyw(12) = 'EpsSL'
       pardef(12) = 0.0_fp
    !elseif (iform == 6) then
    !   name       = 'Bailard'
    !   nparreq    = 10
    elseif (iform == 7) then
       name       = 'Van Rijn (1984)'
       nparreq    = 4
       parkeyw(1) = 'ACal'
       parkeyw(2) = '[dummy]' !don't remove: backward compatibility!
       pardef(2)  = 1.0_fp
       parkeyw(3) = 'Aks'
       parkeyw(4) = 'WSettle'
       nparopt    = 1
       parkeyw(5) = 'BetaM'
       pardef(5)  = 0.0_fp
    !elseif (iform == 8) then
    !   name       = 'Van Rijn / Ribberink (1994)'
    !   nparreq    = 14
    !elseif (iform == 9) then
    !   name       = 'Partheniades / Krone'
    !elseif (iform == 10) then
    !   name       = 'Ashida & Michiue'
    !   nparreq    = 3
    elseif (iform == 11) then
       name       = 'Soulsby / Van Rijn'
       nparreq    = 3
       parkeyw(1) = 'ACal'
       parkeyw(2) = 'RatioD90D50'
       parkeyw(3) = 'RouZ0'
       nparopt    = 1
       parkeyw(4) = 'AlfaUrms'
       pardef(4)  = 1.0_fp
    elseif (iform == 12) then
       name       = 'Soulsby'
       nparreq    = 3
       parkeyw(1) = 'ACal'
       parkeyw(2) = 'ModInd'
       parkeyw(3) = 'RatioD50Z0'
    elseif (iform == 13) then
       name       = 'Wang / Fredsoe'
       nparreq    = 2
       parkeyw(1) = 'VicMol'
       parkeyw(2) = 'ACal' ! Don't move up: historical order!
    elseif (iform == 14) then
       name       = 'Ashida-Michiue (1974)'
       nparreq    = 5   
       parkeyw(1) = 'ACal'
       parkeyw(2) = 'ThetaC'
       parkeyw(3) = 'PowerM'
       parkeyw(4) = 'PowerP'
       parkeyw(5) = 'PowerQ'
    elseif (iform == 15) then
       if (name == ' ') name = 'External subroutine'
       nparreq    = 0
    elseif (iform == 16) then
       name       = 'Wilcock-Crowe (2003)'
       nparopt    = 1
       parkeyw(1) = 'ACal'
       pardef(1)  = 1.0_fp
       if (present(noutpar)) then
          noutpar = 6
          outpar_name( 1)     = 'wistar'
          outpar_longname( 1) = 'dimensionless bedload transport rate' ! -
          outpar_name( 2)     = 'ustar'
          outpar_longname( 2) = 'shear velocity' ! m/s
          outpar_name( 3)     = 'phi'
          outpar_longname( 3) = 'ratio of shear stress over ref shear stress' ! -
          outpar_name( 4)     = 'tauri'
          outpar_longname( 4) = 'ref shear stress at fraction mean diameter' ! kg/(m s2)
          outpar_name( 5)     = 'taurm'
          outpar_longname( 5) = 'ref shear stress at geometric mean diameter' ! kg/(m s2)
          outpar_name( 6)     = 'b'
          outpar_longname( 6) = 'exponent b' ! -
       endif
    elseif (iform == 17) then
       name       = 'Gaeuman et. al. (2009) lab calibration'
       nparreq    = 2
       parkeyw(1) = 'ThetaC0'
       pardef(1)  = 0.021_fp
       parkeyw(2) = 'Alpha0'
       pardef(2)  = 0.33_fp
    elseif (iform == 18) then
       name       = 'Gaeuman et. al. (2009) Trinity River'
       nparreq    = 2
       parkeyw(1) = 'ThetaC0'
       pardef(1)  = 0.03_fp
       parkeyw(2) = 'Alpha0'
       pardef(2)  = 0.3_fp
    elseif (iform == 19) then
       name       = 'Van Thiel / Van Rijn (2008)'
       nparopt    =  13
       parkeyw(1) = 'facua'
       pardef(1)  = 0.1_fp
       parkeyw(2) = 'facAs'
       pardef(2)  = 0.1_fp
       parkeyw(3) = 'facSk'
       pardef(3)  = 0.1_fp
       parkeyw(4) = 'waveform'
       pardef(4)  = 2.0_fp      ! 1=ruessink, 2=van thiel
       parkeyw(5) = 'sws'
       pardef(5)  = 1.0_fp ! true
       parkeyw(6) = 'lws'
       pardef(6)  = 1.0_fp ! true
       parkeyw(7) = 'dilatancy'
       pardef(7)  = 0.0_fp ! false
       parkeyw(8) = 'rheeA'
       pardef(8)  = 0.75_fp
       parkeyw(9) = 'pormax'
       pardef(9)  = 0.5_fp
       parkeyw(10) = 'bedslpini'
       pardef(10)  = 0.0_fp  ! 0=none, 1=total; 2=bedload only
       parkeyw(11) = 'smax'
       pardef(11)  = -1.0_fp     ! [-1; 3]
       parkeyw(12) = 'reposeangle'
       pardef(12)  = 30.0_fp
       parkeyw(13) = 'cmax'
       pardef(13)  = 0.1_fp
       
    elseif (iform == 20) then
       name       = 'Soulsby / Van Rijn, XBeach flavour'
       nparopt    =  14
       parkeyw(1) = 'facua'
       pardef(1)  = 0.1_fp
       parkeyw(2) = 'facAs'
       pardef(2)  = 0.1_fp
       parkeyw(3) = 'facSk'
       pardef(3)  = 0.1_fp
       parkeyw(4) = 'waveform'
       pardef(4)  =  2.0_fp      ! 1=ruessink, 2=van thiel
       parkeyw(5) = 'sws'
       pardef(5)  = 1.0_fp ! true
       parkeyw(6) = 'lws'
       pardef(6)  = 1.0_fp ! true
       parkeyw(7) = 'dilatancy'
       pardef(7)  = 0.0_fp ! false
       parkeyw(8) = 'rheeA'
       pardef(8)  = 0.75_fp
       parkeyw(9) = 'pormax'
       pardef(9)  = 0.5_fp
       parkeyw(10) = 'bedslpini'
       pardef(10)  = 0.0_fp ! 0=none, 1=total; 2=bedload only
       parkeyw(11) = 'smax'
       pardef(11)  = -1.0_fp   ! [-1; 3]
       parkeyw(12) = 'reposeangle'
       pardef(12)  = 30.0_fp
       parkeyw(13) = 'cmax'
       pardef(13)  = 0.1_fp 
       parkeyw(14) = 'z0'
       pardef(14)  = 0.006_fp
    elseif (iform == 21) then
       if (name == ' ') name = 'External subroutine'
       nparreq    = 0
    elseif (iform == 22) then
       name       = 'ASMITA'
       nparreq    = 2
       parkeyw(1) = 'CEqui'
       parkeyw(2) = 'HEqui'
       nparopt    = 3
       parkeyw(3) = 'N'
       pardef(3)  = 3.0_fp
       parkeyw(4) = 'MaxHH'
       pardef(4)  = 10.0_fp
       parkeyw(5) = 'RefLevel'
       pardef(5)  = 0.0_fp
    else
       write (*, '(A,I0,A)') 'The model is trying to use an undefined transport formula ',iform,'.'
       call throwexception()
    endif
end subroutine traparams

end module m_rdtrafrm
