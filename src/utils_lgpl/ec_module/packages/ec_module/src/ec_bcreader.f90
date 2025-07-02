!----- GPL ---------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2011-2025.                                
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
!  
!  


module m_ec_bcreader
  use precision
  use m_ec_parameters
  use m_ec_support
  use m_ec_message
  use m_ec_typedefs
  use m_alloc
  use multi_file_io
  use string_module
  use physicalconsts, only : CtoKelvin
  implicit none

  private

  public   ::  ecBCInit
  public   ::  ecBCReadLine
  public   ::  ecBCBlockCreate
  public   ::  ecBCBlockFree
  public   ::  ecBCBlockFree1dArray
  public   ::  matchblock
  public   ::  matchkeyvalue
  public   ::  matchkeyvaluelist
  public   ::  processhdr
  public   ::  checkhdr
  public   ::  sortndx


contains

  ! =======================================================================

  !> Initialize BC instance
  function ecBCInit (instancePtr, fname, quantityName, plilabel, bc, iostat, funtype) result (success)
    use m_ec_netcdf_timeseries
    implicit none
    !   Open a .bc file with external forcings
    !   Scrolls the headers until it finds quantity, location and .... are matched
    !   Advances the filepointer upto the first line of data, setting all necesary fields of bc-object
    !   Leaves the file open and return the filehandle fhandle (in a bc-object) to caller
    logical                                        :: success
    type(tEcInstance),             pointer         :: instancePtr  !< intent(in)
    character(len=*),              intent(in)      :: fname
    character(len=*),              intent(in)      :: quantityName
    character(len=*),              intent(in)      :: plilabel
    type (tEcBCBlock),             intent(inout)   :: bc
    integer, optional,             intent(out)     :: iostat
    character(len=*), optional,    intent(in)      :: funtype

    integer(kind=8)                                :: fhandle
    success = .false.
    bc%qname = quantityName
    bc%bcname = plilabel
    bc%fname = fname                                  ! store original filename for later reference (?)
    if (associated(bc%quantity)) deallocate(bc%quantity)
    allocate(bc%quantity)
    call str_upper(bc%qname,len(trim(bc%qname)))
    call str_upper(bc%bcname,len(trim(bc%bcname)))
    !
    select case (bc%ftype)
    case (BC_FTYPE_ASCII)
       if (bc%bcFilePtr%fhandle<0) then                   ! check if file already opened in our adminstration
          if (.not.ecSupportOpenExistingFileGnu(fhandle, bc%bcFilePtr%bcfilename)) then
             call setECMessage("Unable to open "//trim(bc%bcFilePtr%bcfilename))
             return
          else
             bc%bcFilePtr%fhandle = fhandle
          end if
       end if
       if (.not.ecBCFilescan(bc, iostat, funtype=funtype)) then     ! parsing the open bc-file
          return                                               ! quantityName-plilabel combination not found
       else
          allocate(bc%columns(bc%numcols))
       endif
    case (BC_FTYPE_NETCDF)
       allocate(bc%ncvarndx(1))
       if (.not.ecNetCDFscan(bc%ncptr, quantityName, plilabel, bc%ncvarndx, bc%nclocndx, &
                              bc%dimvector, vectormax=bc%quantity%vectormax)) then
          return                                               ! quantityName-plilabel combination not found
       endif
       if (bc%numlay<=1) then
          bc%func = BC_FUNC_TSERIES
       else
          bc%func = BC_FUNC_TIM3D
       endif
       ! TODO:
       ! Support specification of the time-interpolation type in the netcdf timeseries variable as an attribute
       bc%timeunit         = bc%ncptr%timeunit
       bc%timeint          = BC_TIMEINT_LIN   
       bc%quantity%name    = quantityName
       bc%quantity%missing = bc%ncptr%fillvalues(bc%ncvarndx(1))
       bc%quantity%factor  = bc%ncptr%scales(bc%ncvarndx(1))
       bc%quantity%offset  = bc%ncptr%offsets(bc%ncvarndx(1))
    case default
       call setECMessage("Forcing file ("//trim(bc%fname)//") should either be of type .nc (netcdf timeseries file) or .bc (ascii BC-file).")
       return
    end select
    success=.true.
  end function ecBCInit


  ! =======================================================================

  !> Find quantity-pli point combination, preparing a BC-block using an ASCII-file as input
  function ecBCFilescan(bc, iostat, funtype) result (success)
    implicit none
    logical                                   :: success
    type (tEcBCBlock),          intent(inout) :: bc
    integer,                    intent(out)   :: iostat
    character(len=*), optional, intent(in)    :: funtype
    character(len=:), allocatable ::  rec
    character(len=:), allocatable ::  keyvaluestr ! all key-value pairs in one header; allocated on assign

    integer                       ::  posfs
    integer                       ::  nfld
    integer                       ::  nq
    logical                       ::  jaheader, jafound
    integer(kind=8)               ::  currentpos

    integer (kind=8)              ::  savepos
    type(tEcBlockList), pointer   ::  blocklistPtr
    type(tEcBCFile), pointer      ::  bcFilePtr
    integer                       ::  reclen
    integer                       ::  blocktype
    integer, parameter            ::  BT_GENERAL = 0
    integer, parameter            ::  BT_FORCING = 1
    character(len=15)             ::  key, val

    success = .false.
    iostat = EC_UNKNOWN_ERROR
    savepos = 0

    ! TODO: Check if we have a matching position in the administration,
    ! If so:
    !    create a new filehandler
    !    set the metadata correctly by process-header 
    ! If not:
    !    find the last read position for this file, that is: the last recorded start position of a data-block
    !    start searching from there 
    
    bcFilePtr => bc%bcFilePtr
    blocklistPtr => bcFilePtr%blocklist
    jafound = .false.
    jaheader = .false.
    currentpos = bcFilePtr%last_position
    blocktype = bcFilePtr%last_blocktype
    do while (associated(blocklistPtr))
       if (matchblock(blocklistPtr%keyvaluestr,bc%bcname,bc%qname,funtype=funtype)) then
          jafound = .true.
          currentpos = blocklistPtr%position
          blocktype = blocklistPtr%blocktype
          exit
       endif 
       blocklistPtr => blocklistPtr%next 
    enddo
    
    call mf_backspace(bcFilePtr%fhandle, currentpos)
    if (.not.jafound) then
       do
          if (mf_eof(bcFilePtr%fhandle)) then                               ! forward to last position we searched in this file
             iostat = EC_EOF
             return
          endif
          call mf_read(bcFilePtr%fhandle, rec, savepos)

          if (rec=="") cycle
          if (index('!#%*',rec(1:1))>0) cycle
          reclen=index(rec,'#')
          if (reclen>0) then
             rec = rec(1:reclen-1)
          endif
 
          if (len_trim(rec)>0) then                                  ! skip empty lines
             if (index(rec,'[')>0 .and. index(rec,']')>0) then 
                if (strcmpi(adjustl(rec),'[general]')) then                    ! new boundary chapter
                   blocktype = BT_GENERAL
                else if (strcmpi(adjustl(rec),'[forcing]') .or. &
                         strcmpi(adjustl(rec),'[boundary]')) then
                   jaheader = .true.                                 ! switching to header mode
                   blocktype = BT_FORCING
                   keyvaluestr = ''
                   nfld = 0                                          ! count the number of fields in this header block
                   nq = 0                                            ! count the (maximum) number of quantities in this block
                else
                   call setECMessage("Unknown block type '"//trim(rec)//           &
                                "' in file "//trim(bc%fname)//", block "//trim(bc%bcname)//".") 
                   return
                endif
             else
                select case (blocktype)
                case (BT_FORCING)
                   if (jaheader) then
                      posfs = index(rec,'=')                         ! key value pair ?
                      if (posfs>0) then
                         call replace_char(rec,9,32)                 ! replace tabs by spaces, header key-value pairs only
                         nfld = nfld + 1                             ! count the number of lines in the header file
                         ! TODO: Replace this key-value string by a linked list-base class for key-value dictionaries
                         call str_upper(rec(1:posfs-1))              ! all keywords uppercase , not case sensitive
                         if (index(rec(1:posfs-1),'QUANTITY')>0) then
                            nq = nq + 1
                         endif
                         keyvaluestr = trim(keyvaluestr)//''''// (trim(adjustl(rec(1:posfs-1))))//''',''' //(trim(adjustl(rec(posfs+1:))))//''','
                      else                                                    ! switch to datamode
                         ! TODO: Store the location information somewhere to be able to return to it later 
                         call str_upper(keyvaluestr, len_trim(keyvaluestr))   ! case insensitive format
      
                         allocate(blocklistPtr)                                   ! Add information for this block to the administration
                         blocklistPtr%position = savepos
                         blocklistPtr%blocktype = blocktype
                         blocklistPtr%keyvaluestr = keyvaluestr
                         blocklistPtr%next => bcFilePtr%blocklist
                         blocklistPtr%nfld = nfld
                         blocklistPtr%nq = nq
                         bcFilePtr%blocklist => blocklistPtr
                         bcFilePtr%last_position = savepos
                         bcFilePtr%last_blocktype = blocktype
      
                         if (matchblock(keyvaluestr,bc%bcname,bc%qname,funtype=funtype)) then
                            if (.not.processhdr(bc,nfld,nq,keyvaluestr)) return   ! dumb translation of bc-object metadata
                            if (.not.checkhdr(bc)) return                         ! check on the contents of the bc-object
                            call mf_backspace(bcFilePtr%fhandle, savepos)                   ! Rewind the first line with data
                            success = .true.
                            iostat = EC_NOERR
                            return
                         else
                            ! location was found, but not all required meta data was present
                            iostat = EC_METADATA_INVALID
                         endif                                    ! Right quantity
                         jaheader = .false.                       ! No, we are NOT reading a header
                      endif                                       ! switch to datamode
                   endif          ! in header mode (data lines are ignored)
                case (BT_GENERAL)
                   posfs = index(rec,'=')
                   if (posfs>0) then
                      call replace_char(rec,9,32)
                      key = adjustl(rec(:posfs-1))
                      val = adjustl(rec(posfs+1:))
                      call str_upper(key)
                      select case (key)
                      case ('FILEVERSION')
                         bcFilePtr%FileVersion = trim(val)
                      case ('FILETYPE') 
                         bcFilePtr%FileType = trim(val)
                      end select
                   endif
                end select
             endif
          endif                ! non-empty string
       enddo                   ! read/scan loop
    else
       if (.not.processhdr(bc,blocklistPtr%nfld,blocklistPtr%nq,blocklistPtr%keyvaluestr)) return
       if (.not.checkhdr(bc)) return
       success = .true.
       iostat = EC_NOERR
    endif                      ! need to search or already in 'database'
  end function ecBCFilescan

  function matchkeyvaluelist(keyvaluestr,key,value) result (jafound)
    implicit none
    logical                          :: jafound
    character(len=*),    intent(in)  :: keyvaluestr
    character(len=*),    intent(in)  :: key
    character(len=*),    intent(in)  :: value
    jafound = (index(keyvaluestr,','''//trim(key)//''','''//trim(value)//':')>0)            ! like 'keyword1','value1 : one,two,three','keyword',..... etc
  end function matchkeyvaluelist

  function matchkeyvalue(keyvaluestr,key,value) result (jafound)
    implicit none
    logical                          :: jafound
    character(len=*),    intent(in)  :: keyvaluestr
    character(len=*),    intent(in)  :: key
    character(len=*),    intent(in)  :: value
!   jafound = (index(keyvaluestr,','''//trim(key)//''','''//trim(value)//''',')>0)            ! like 'keyword1','value1','keyword',.....
    jafound = (index(keyvaluestr,''''//trim(key)//''','''//trim(value)//'''')>0)            ! like 'keyword1','value1','keyword',.....
  end function matchkeyvalue

  function matchblock(keyvaluestr,bcname,qname,funtype) result (jafound)
    logical                                 :: jafound
    character(len=*), intent(in)            :: keyvaluestr
    character(len=*), intent(in)            :: bcname
    character(len=*), intent(in)            :: qname
    character(len=*), intent(in), optional  :: funtype
    jafound = .false.
    if (present(funtype)) then
       if (.not.matchkeyvalue(keyvaluestr,'FUNCTION',trim(funtype))) then
          return
       endif
    endif
    if (matchkeyvalue(keyvaluestr,'NAME',trim(bcname))) then
       if (matchkeyvalue(keyvaluestr,'FUNCTION','ASTRONOMIC').or.matchkeyvalue(keyvaluestr,'FUNCTION','ASTRONOMIC-CORRECTION')) then
          if (matchkeyvalue(keyvaluestr,'QUANTITY',trim(qname)//' '//'AMPLITUDE')) then
             if (matchkeyvalue(keyvaluestr,'QUANTITY',trim(qname)//' '//'PHASE')) then
                jafound = .true.
             endif
          endif
       elseif (matchkeyvalue(keyvaluestr,'FUNCTION','HARMONIC').or.matchkeyvalue(keyvaluestr,'FUNCTION','HARMONIC-CORRECTION')) then
          if (matchkeyvalue(keyvaluestr,'QUANTITY',trim(qname)//' '//'AMPLITUDE')) then
             if (matchkeyvalue(keyvaluestr,'QUANTITY',trim(qname)//' '//'PHASE')) then
                jafound = .true.
             endif
          endif
       elseif (matchkeyvalue(keyvaluestr,'FUNCTION','QHTABLE')) then
          if (matchkeyvalue(keyvaluestr,'QUANTITY',trim(qname)//' '//'WATERLEVEL')) then
             if (matchkeyvalue(keyvaluestr,'QUANTITY',trim(qname)//' '//'DISCHARGE')) then
                jafound = .true.
             endif
          endif
       else
          if (matchkeyvalue(keyvaluestr,'QUANTITY',qname)) then
             jafound = .true.
          elseif (matchkeyvaluelist(keyvaluestr,'VECTOR',qname)) then
             jafound = .true.
          endif
       endif
    endif
  end function matchblock

  !> Given a character string of key-value pairs gathered from a header block,
  !> extract all relevant fields (and find the block of requested quantity)
  !> and fill a bc-object
  function processhdr(bc,nfld,nq,keyvaluestr) result (success)
    implicit none
    logical                                 ::      success
    type (tEcBCBlock),  intent(inout)       ::      bc              !< boundary condition instance
    integer,            intent(in)          ::      nfld            !< number of fields in the header
    integer,            intent(in)          ::      nq              !< number of quantities (quantity block)
    character(len=*),   intent(in)          ::      keyvaluestr     !< string containing header fields as key-value pairs

    integer                          ::     ifld
    integer                          ::     i, jv
    integer                          ::     iostat
    type(VLSType), allocatable       ::     hdrkeys(:)     !< All keys from header
    type(VLSType), allocatable       ::     hdrvals(:)     !< All values from header
    integer, allocatable             ::     iv(:), il(:), perm_vpos(:)

    integer                          ::     ipos, npos, posfs, ipos1, ipos2
    integer                          ::     iq, iq_sel, idim, num_layers_grid
    integer, parameter               ::     MAXDIM = 10    !< max number of vector quantities in one vector
    character(len=maxNameLen)        ::     vectorquantities(MAXDIM)
    character(len=maxNameLen)        ::     vectordefinition, vectorstr
    real(kind=hp), pointer           ::     vp_new(:)

    success = .false.
    if (allocated(hdrkeys)) then
       deallocate(hdrkeys)
    endif
    if (allocated(hdrvals)) then
       deallocate(hdrvals)
    endif

    if (associated(bc%quantity)) then
       if (allocated(bc%quantity%jacolumn)) then
          deallocate(bc%quantity%jacolumn)
       endif
       if (allocated(bc%quantity%col2elm)) then
          deallocate(bc%quantity%col2elm)
       endif
       deallocate(bc%quantity)
    endif
    allocate(bc%quantity)
    allocate(hdrkeys(nfld),hdrvals(nfld))
    allocate(bc%quantity%jacolumn(nq))
    allocate(bc%quantity%col2elm(nq))
    allocate(iv(nq),il(nq))
    iv = -1
    il = -1
    bc%quantity%col2elm = -1
    vectordefinition = ''

    ipos1=1
    do ifld = 1, nfld
       ipos2 = index(keyvaluestr(ipos1:), "',")+1
       hdrkeys(ifld)%s = keyvaluestr(ipos1+1:ipos1+ipos2-3)
       ipos1 = ipos1+ipos2
       ipos2 = index(keyvaluestr(ipos1:), "',")+1
       hdrvals(ifld)%s = keyvaluestr(ipos1+1:ipos1+ipos2-3)
       ipos1 = ipos1+ipos2
    end do

    iq = 0
    iq_sel = 0
    do ifld=1,nfld
       call remove_chars(hdrkeys(ifld)%s,' -_.')         ! filter out all connection characters from the keywords
       select case (adjustl(hdrkeys(ifld)%s))
       case ('QUANTITY')
          iq = iq + 1                                    ! count quantities, corresponds with column numbers [iq]
          if (hdrvals(ifld)%s==bc%qname) then            ! detected quantity of interest
             bc%quantity%name=bc%qname
             bc%quantity%jacolumn(iq)=.true.
             iq_sel = iq_sel + 1
             il(iq) = 1                                  ! layer this column belongs to, default 1
             iv(iq) = 1                                  ! iv is the number of the element in the vector
             bc%quantity%col2elm(iq_sel) = iq
          else if (index(vectordefinition,'|'//trim(hdrvals(ifld)%s)//'|')>0) then         ! quantity is part of requested vector
             posfs = index(vectordefinition,'|'//trim(hdrvals(ifld)%s)//'|')
             bc%quantity%jacolumn(iq)=.true.
             iq_sel = iq_sel + 1
             il(iq) = 1                                  ! layer this column belongs to, default 1
             iv(iq) = 0                                  ! iv is the number of the element in the vector
             do jv=1,posfs
                if (vectordefinition(jv:jv)=='|') iv(iq) = iv(iq) + 1                    ! count number of | preceding the quantitie.
             enddo
          else
             bc%quantity%jacolumn(iq)=.false.
             select case (bc%func)
             case (BC_FUNC_TSERIES, BC_FUNC_TIM3D)
                if (hdrvals(ifld)%s=='TIME') then    ! special check on the time field
                   bc%timecolumn = iq
                endif
                case (BC_FUNC_HARMONIC, BC_FUNC_ASTRO, BC_FUNC_HARMOCORR, BC_FUNC_ASTROCORR, BC_FUNC_CMP3D)
                   if (hdrvals(ifld)%s=='HARMONIC COMPONENT') then          ! harmonic component
                      bc%astro_component_column = iq
                   endif
                   if (hdrvals(ifld)%s=='ASTRONOMIC COMPONENT') then        ! astronomic component label
                      bc%astro_component_column = iq
                   endif
                   if (hdrvals(ifld)%s==trim(bc%qname)//' AMPLITUDE') then  ! amplitude field for astronomic/harmonic components
                      bc%astro_amplitude_column = iq
                   endif
                   if (hdrvals(ifld)%s==trim(bc%qname)//' PHASE') then      ! phase field for astronomic/harmonic components
                      bc%astro_phase_column = iq
                   endif
             case (BC_FUNC_QHTABLE)
                if (hdrvals(ifld)%s==trim(bc%qname)//' WATERLEVEL') then ! waterlevel field for qh-boundary
                   bc%qh_waterlevel_column = iq
                endif
                if (hdrvals(ifld)%s==trim(bc%qname)//' DISCHARGE') then  ! discharge field for qh-boundary
                   bc%qh_discharge_column = iq
                endif
             end select
          endif
       case ('VECTOR')
          vectorstr = trim(hdrvals(ifld)%s)
          posfs = index(vectorstr,':')
          if (posfs>0) then
             if (trim(vectorstr(1:posfs-1))==trim(bc%qname)) then           ! this vector defines the requested 'quantity'
                bc%quantity%name=bc%qname
                vectorquantities = ''
                read (vectorstr(posfs+1:len_trim(vectorstr)),*,iostat=iostat)  (vectorquantities(idim),idim=1,MAXDIM)
                vectordefinition = '|'
                bc%quantity%vectormax=1
                do idim=1,MAXDIM
                   vectordefinition=trim(vectordefinition)//trim(vectorquantities(idim))//'|'
                   if (len_trim(vectorquantities(idim))>0) bc%quantity%vectormax=idim
                enddo
             endif
          endif
       case ('MISSING')
          read(hdrvals(ifld)%s,*) bc%quantity%missing
       case ('UNIT')
          if (bc%quantity%jacolumn(iq)) then
             bc%quantity%unit = trim(hdrvals(ifld)%s)
          endif
          if (iq==bc%timecolumn) then                     ! Is this the unit of time ?
             bc%timeunit = trim(hdrvals(ifld)%s)            ! store timeunit string in this bc instance
          endif
          if (iq == bc%astro_component_column) then
             bc%timeunit = trim(hdrvals(ifld)%s)            ! store period/feq unit in time unit
          endif
       case ('FUNCTION')
          if (iq>0) cycle
          select case (adjustl(hdrvals(ifld)%s))
          case ('TIMESERIES')
             bc%func = BC_FUNC_TSERIES
          case ('CONSTANT')                        ! Constant is a special version of time-series (Sobek3)
             bc%func = BC_FUNC_CONSTANT
             bc%timeint = timeint_bfrom            ! Time interpolation keyword is likely absent, default to a0=1 and a1=0
          case ('HARMONIC')
             bc%func = BC_FUNC_HARMONIC
          case ('ASTRONOMIC')
             bc%func = BC_FUNC_ASTRO
          case ('HARMONIC-CORRECTION')
             bc%func = BC_FUNC_HARMOCORR
          case ('ASTRONOMIC-CORRECTION')
             bc%func = BC_FUNC_ASTROCORR
          case ('T3D')
             bc%func = BC_FUNC_TIM3D
          case ('C3D')
             bc%func = BC_FUNC_CMP3D
          case ('QHTABLE')
             bc%func = BC_FUNC_QHTABLE
          end select
       case ('OFFSET')
          if (iq>0) cycle
          read(hdrvals(ifld)%s,*) bc%quantity%offset
       case ('FACTOR')
          if (iq>0) cycle
          read(hdrvals(ifld)%s,*) bc%quantity%factor
       case ('VERTICALPOSITION','VERTPOSITIONINDEX')
          read(hdrvals(ifld)%s,*) il(iq)
          bc%quantity%vertndx = il(iq)                          ! layer this column belongs to, default 1
       case ('VERTICALPOSITIONSPECIFICATION','VERTPOSITIONS')
          npos=0
          if (len_trim(hdrvals(ifld)%s)>0) then
             npos = count([(verify(hdrvals(ifld)%s(i:i),', ')>0   &
                  .and.verify(hdrvals(ifld)%s(i-1:i-1),', ')==0, i=2,len_trim(hdrvals(ifld)%s))]) + 1
          endif
          allocate(bc%vp(npos))
          bc%numlay = npos
          read(hdrvals(ifld)%s,*) (bc%vp(ipos),ipos=1,npos)       ! globally store ALL vertical positions
          allocate(perm_vpos(npos))
          call sortndx(bc%vp,perm_vpos,npos)                    ! produce the permutation that sorts the vertical positions perm_vpos
       case ('TIMEINTERPOLATION')
          select case (adjustl(hdrvals(ifld)%s))
          case ('LINEAR')
             bc%timeint = BC_TIMEINT_LIN
          case ('LINEAR-EXTRAPOLATE')
             bc%timeint = BC_TIMEINT_LIN_EXTRAPOL
          case ('BLOCK-TO')
             bc%timeint = BC_TIMEINT_BTO
          case ('BLOCK-FROM')
             bc%timeint = BC_TIMEINT_BFROM
          case ('BLOCK', 'BLOCKTO', 'BLOCKFROM')
             call setECMessage("Unknown time interpolation Block in file "//trim(bc%fname)//", block " &
                                //trim(bc%bcname)//". Use Block-To or Block-From.")
             return
          case ('AMOUNTTORATE')
             bc%timeint = BC_TIMEINT_AMOUNT_TO_RATE
          case default
             call setECMessage("Unknown time interpolation '"//trim(adjustl(hdrvals(ifld)%s))//           &
                                "' in file "//trim(bc%fname)//", block "//trim(bc%bcname)//".") 
             return
          end select
       case ('PERIODIC')
          select case (trim(adjustl(hdrvals(ifld)%s)))
          case ('TRUE','T','.T.','1','JA','YES')
             bc%periodic = .True.
          case default
             bc%periodic = .False.
          end select
       case ('VERTICALINTERPOLATION','VERTINTERPOLATION')
          select case (adjustl(hdrvals(ifld)%s))
          case ('LINEAR')
             bc%zInterpolationType = zinterpolate_linear
          case ('LOG')
             bc%zInterpolationType = zinterpolate_log
          case ('BLOCK')
             bc%zInterpolationType = zinterpolate_block
          case ('MEAN')
             bc%zInterpolationType = zinterpolate_mean
          case default
             bc%zInterpolationType = zinterpolate_unknown
             call setECMessage("Unknown vertical interpolation '"//trim(adjustl(hdrvals(ifld)%s))//           &
                                "' in file "//trim(bc%fname)//", block "//trim(bc%bcname)//".") 
             return
          end select
       case ('VERTICALPOSITIONTYPE','VERTPOSITIONTYPE')
          IF (index(hdrvals(ifld)%s,'PERCEN')>0 .and. index(hdrvals(ifld)%s,'BED')>0) then
             hdrvals(ifld)%s = 'PERCBED'
          endif
          select case (adjustl(hdrvals(ifld)%s))
          case ('SINGLE')
             bc%vptyp = BC_VPTYP_SINGLE
          case ('PERCBED')
             bc%vptyp = BC_VPTYP_PERCBED
          case ('ZDATUM')
             bc%vptyp = BC_VPTYP_ZDATUM
          case ('BEDSURF')
             bc%vptyp = BC_VPTYP_BEDSURF
          case ('PERCSURF')
             bc%vptyp = BC_VPTYP_PERCSURF
          case ('ZBED')
             bc%vptyp = BC_VPTYP_ZBED
          case ('ZSURF')
             bc%vptyp = BC_VPTYP_ZSURF
          case default
             call setECMessage("Unknown vertical position type '"//trim(adjustl(hdrvals(ifld)%s))//           &
                                "' in file "//trim(bc%fname)//", block "//trim(bc%bcname)//".") 
             return
          end select
       case default
           ! Give a warning/error if the key that looks like one of the keys
           ! related to vertical positions is used, but is not recoqnized.
           ! Note that key "NAME" is ignored here.
           if (index(hdrkeys(ifld)%s, "VERT") > 0 .and. index(hdrkeys(ifld)%s, "POS") > 0) then
               call setECMessage("Unknown keyword '"//trim(adjustl(hdrkeys(ifld)%s))//           &
                                "' in file "//trim(bc%fname)//", block "//trim(bc%bcname)//".")
               call setECMessage("Use one of: 'VERTICALPOSITION', 'VERTPOSITIONINDEX', " // &
                                              "'VERTICALPOSITIONSPECIFICATION', 'VERTPOSITIONS'")
               return
           end if
       end select
    enddo

    if (bc%quantity%unit == 'K' .or. bc%quantity%unit == 'KELVIN' .or. bc%quantity%unit == 'Kelvin') then
       ! convert Kelvin to degrees Celsius (kernel expects degrees Celsius)
       bc%quantity%offset = bc%quantity%offset - CtoKelvin
    endif

    ! Fill bc%quantity%col2elm(nq) which holds the mapping of columns in the file to vector positions
    bc%quantity%col2elm(iq) = -1
    bc%numcols = iq
    do iq = 1, nq
       if (iv(iq)>0) then
          bc%quantity%col2elm(iq) = (il(iq)-1)*bc%quantity%vectormax + iv(iq)
          if (associated(bc%vp)) then
             bc%quantity%col2elm(iq) = (perm_vpos(il(iq))-1)*bc%quantity%vectormax + iv(iq)
          else
             bc%quantity%col2elm(iq) = iv(iq)
          endif
       else
          bc%quantity%col2elm(iq) = -1
       endif
    enddo

    if (associated(bc%vp)) then
       num_layers_grid = size(bc%vp)
       if (perm_vpos(1) /= 1) then
         allocate(vp_new(num_layers_grid))
         do iq = 1, num_layers_grid
            vp_new(iq) = bc%vp(perm_vpos(iq))
         enddo
         deallocate(bc%vp)
         bc%vp => vp_new
       endif
    endif

    deallocate(hdrkeys)
    deallocate(hdrvals)
    deallocate(iv,il)

    success = .true.
    if (bc%vptyp == BC_VPTYP_PERCBED) then
       success = checkAndFixLayers(bc%vp, bc%quantity%name)
    endif
  end function processhdr

  !> check if all layers are in range 0.0 - 1.0
  !! if in range 0.0 - 100.0 convert percentages into fractions
  function checkAndFixLayers(vp, name) result(success)
     implicit none
     real(kind=hp),    intent(inout) :: vp(:)      !< vertical positions
     character(len=*), intent(in)    :: name       !< quantity name, used in error messages
     logical                         :: success    !< function result

     real(kind=hp)   :: minvp                      !< lowest vertical position
     real(kind=hp)   :: maxvp                      !< higest vertical position
     integer         :: num_layers_grid                       !< number of layers
     integer         :: k                          !< loop counter
     logical, save   :: warningPrinted = .false.   !< flag to avoid printing the same warning many times

     success = .true.

     minvp = minval(vp)
     maxvp = maxval(vp)
     num_layers_grid = size(vp)

     if (minvp >= 0.0d0 .and. maxvp <= 1.0d0) then
        continue ! all layers oke
     else if (minvp < 0.0d0 .or. maxvp > 100.0d0) then
        ! negative or even in percentages too high
        call printErrMessageLayers()
        success = .false.
     else
        ! in range 0.0 - 100.0. probably percentages. extra check, increasing numbers?
        do k = 2, num_layers_grid
           if (vp(k) < vp(k-1)) then
              success = .false.
              exit
           endif
        enddo
        if (success) then
            if (.not. warningPrinted) then
               call setECMessage("converting layer percentages in bc-file to fractions.")
               warningPrinted = .true.
            endif
            vp = vp * 0.01_hp
        else
            call printErrMessageLayers()
        endif
     endif

  contains

  subroutine printErrMessageLayers()
     character(len=8)              :: strMin   !< minvp converted to a string
     character(len=8)              :: strMax   !< maxvp converted to a string
     character(len=:), allocatable :: errorMessage

     write(strMin,'(f8.3)') minvp
     write(strMax,'(f8.3)') maxvp

     call setECMessage("sigma positions must be in range 0.0 - 1.0")
     errorMessage = "range for " // trim(name) // " is " // strMin // " - " // strMax // "."
     call setECMessage(errorMessage)
  end subroutine printErrMessageLayers

  end function checkAndFixLayers


  !> Given a filled bc-object, scrutinize its content for completeness, validity and consistency
  function checkhdr(bc) result (success)
    implicit none
    logical                                     ::      success
    type (tEcBCBlock),         intent(inout)    ::      bc
    !
    success = .false.
    ! If function is tseries (not intended for the 3rd dimension, but numlay>1, indicating a specification of
    ! vertical position, assume that the function label is a mistake and change it into T3D
    if (bc%numlay>1 .and. bc%func==BC_FUNC_TSERIES) then
       bc%func=BC_FUNC_TIM3D
    end if

    success = .true.
  end function checkhdr

  !> Read the next record from a *.bc file.
  function ecBCReadLine(fileReaderPtr, values, time_steps, recout, eof) result(success)
    use m_ec_netcdf_timeseries
    implicit none
    logical                                                 :: success       !< function status
    type(tEcFileReader),    pointer                         :: fileReaderPtr
    real(hp), optional,                       intent(inout) :: time_steps    !< number of time steps of duration: seconds
    real(hp), dimension(:), optional,         intent(inout) :: values        !< vector of values for a BC_FUNC_TSERIES
    character(len=:), optional, allocatable,  intent(out)   :: recout        !< line prepared from input for caller
    logical, optional,                        intent(out)   :: eof           !< reading failed, but only because eof

    !
    type(tEcBCBlock), pointer      :: bcPtr
    integer                        :: n_col      !< number of columns in the file, inferred from the number of quantity blocks in the header
    integer                        :: n_col_time !< position of the dedicated time-column in the line, deduced from the header
    character(len=:), allocatable  :: rec        !< content of a line
    character(len=30)              :: ncolstr
    integer                        :: istat      !< status of read operation
    integer                        :: i, j       !< loop counters
    integer(kind=8)                :: savepos    !< saved position in file, for mf_read to enabled rewinding
    real(kind=hp), dimension(1:1)  :: ec_timesteps ! to read in source time from file block
    real(kind=hp)                  :: amplitude

    bcPtr => fileReaderPtr%bc

    success = .false.

    select case (bcPtr%ftype)
    case (BC_FTYPE_ASCII)
       n_col = bcPtr%numcols             ! Number of quantities = columns specified in the header
       n_col_time = bcPtr%timecolumn     ! Rank number of the column presumably holding the time (not necessarily the first)

       if(present(recout)) then
          recout = ''                       ! initialize return string to empty
       endif
       rec = ''
       if (present(eof)) then
          eof = .false.
       endif
       do while(len_trim(rec)==0)
          if (bcPtr%feof) then
             select case (BCPtr%func)
             case (BC_FUNC_TSERIES, BC_FUNC_TIM3D, BC_FUNC_CONSTANT)
                call setECMessage("   File: "//trim(bcPtr%fname)//", Location: "//trim(bcPtr%fname)//", Quantity: "//trim(bcPtr%qname))
                call setECMessage("Datablock end (eof) has been reached (READING BEYOND FINAL TIME).")
             end select
             if (present(eof)) then
                eof = .true.
             endif
             return
          endif

          if (bcPtr%fposition>0) then
              call mf_backspace(bcPtr%bcFilePtr%fhandle, bcPtr%fposition)           ! set newly opened file to the appropriate position 
              call mf_read(bcPtr%bcFilePtr%fhandle, rec,savepos)
              bcPtr%feof = mf_eof(bcPtr%bcFilePtr%fhandle)
              call mf_getpos(bcPtr%bcFilePtr%fhandle, bcPtr%fposition)
          else        
              call mf_read(bcPtr%bcFilePtr%fhandle, rec,savepos)
              bcPtr%feof = mf_eof(bcPtr%bcFilePtr%fhandle)
              call mf_getpos(bcPtr%bcFilePtr%fhandle, bcPtr%fposition)
          endif

          if (len(rec) == 0) cycle
          if (rec(1:1)=='#') cycle
          if (index(rec,'[')>0 .and. index(rec,']')>0) then ! lines with [ and ] are assumed as block headings
             select case (BCPtr%func)
             case (BC_FUNC_TSERIES, BC_FUNC_TIM3D)
                call setECMessage("   File: "//trim(bcPtr%fname)//", Location: "//trim(bcPtr%bcname)//", Quantity: "//trim(bcPtr%qname))
                call setECMessage("Datablock end (new [forcing] block) has been prematurely reached.")
             end select
             if (present(eof)) then
                eof = .true.
             endif
             return
          endif
       enddo

       BCPtr%columns(1:n_col)=''
       read(rec, *, IOSTAT = istat) BCPtr%columns(1:n_col)
       if (istat /= 0) then
          ! error handling, report column number i, field content columns(i) and record rec  ....
          ! TODO: hookup MessageHandlign and print rec and column stats here directly
          call setECMessage("   File: "//trim(bcPtr%fname)//", Location: "//trim(bcPtr%fname)//", Quantity: "//trim(bcPtr%qname))
          call setECMessage("ec_bcreader::ecBCReadBlock: Read failure.")
          write (ncolstr,'(a,i0,a,i0,a)') '(expecting ',n_col,' columns)'
          call setECMessage("   ''"//trim(rec)//"'' "//trim(ncolstr))
          success = .false.
          return
       endif

       select case (BCPtr%func)
       case (BC_FUNC_TSERIES, BC_FUNC_TIM3D, BC_FUNC_CONSTANT)
          if (n_col_time>0) then
             read (BCPtr%columns(n_col_time), *) ec_timesteps(1)
             ! Convert source time to kernel time:
             time_steps = ecSupportThisTimeToMJD(fileReaderPtr%tframe, ec_timesteps(1))
          endif
          j=0
          if (count(BCPtr%quantity%col2elm>0)==0) then                ! col2elm is not used to map the read colums, added to the vector in reading order
             do i=1,n_col
                if (BCPtr%quantity%jacolumn(i)) then
                   j=j+1
                   read(BCPtr%columns(i),*,iostat=istat) values(j)
                   values(j)=values(j)*BCPtr%quantity%factor+BCPtr%quantity%offset      ! apply using given offset and factor to this quantity
                   if (istat/=0) then
                      ! error handling, report column number i, field content columns(i) and record rec  ....
                      call setECMessage("Read failure in file: "//trim(bcPtr%fname))
                      return
                   endif
                endif
             enddo
          else
             do i=1,n_col                                                                     ! loop over columns
                if (BCPtr%quantity%col2elm(i)>0) then                                         ! if the i-th column was assigned to a vector position
                   read(BCPtr%columns(i),*,iostat=istat) values(BCPtr%quantity%col2elm(i))    ! read the i-th column into the col2vectpos(i)-th element
                   values(BCPtr%quantity%col2elm(i)) = values(BCPtr%quantity%col2elm(i))        &
                        &                                                   * BCPtr%quantity%factor                    &
                        &                                                   + BCPtr%quantity%offset
                   if (istat>0) then
                      call setECMessage("   line = "//trim(rec))
                      call setECMessage("Read failure in file: "//trim(bcPtr%fname))
                      return
                   endif
                endif
             enddo
          endif
       case (BC_FUNC_ASTRO,BC_FUNC_HARMONIC,BC_FUNC_ASTROCORR,BC_FUNC_HARMOCORR)
          ! Produce a record of component, amplitude, phase extracted from rec
          ! Apply the factor to the amplitude column only
          read(BCPtr%columns(BCPtr%astro_amplitude_column),*,iostat=istat) amplitude
          if (istat==0) then
             amplitude = amplitude * BCPtr%quantity%factor
             write(BCPtr%columns(BCPtr%astro_amplitude_column),*) amplitude
          endif

          ! construct a new record
          recout =                     trim(BCPtr%columns(BCPtr%astro_component_column))
          recout = trim(recout)//'  '//trim(BCPtr%columns(BCPtr%astro_amplitude_column))
          recout = trim(recout)//'  '//trim(BCPtr%columns(BCPtr%astro_phase_column))
       case (BC_FUNC_QHTABLE)
          ! Produce a record of waterlevel, discharge extracted from rec
          recout =                     trim(BCPtr%columns(BCPtr%qh_discharge_column))
          recout = trim(recout)//'  '//trim(BCPtr%columns(BCPtr%qh_waterlevel_column))
       end select

    case (BC_FTYPE_NETCDF)
       if (BCPtr%nctimndx>BCPtr%ncptr%dimlen(BCPtr%ncptr%timedimid)) then
          call setECMessage("Datablock end (eof) has been reached in file: "//trim(bcPtr%fname))
          return
       endif
       if (.not.ecNetCDFGetTimeseriesValue (BCPtr%ncptr,BCPtr%ncvarndx,BCPtr%nclocndx,BCPtr%dimvector, &
          BCPtr%nctimndx,ec_timesteps,values, BCPtr%buffer)) then
          call setECMessage("Read failure in file: "//trim(bcPtr%fname))
          return
       else
          BCPtr%nctimndx = BCPtr%nctimndx + 1
          time_steps = ecSupportThisTimeToMJD(fileReaderPtr%tframe, ec_timesteps(1))
       endif
    case default
       call setECMessage("Invalid filetype set for file: "//trim(bcPtr%fname)//' (internal EC-error)')
       return
    end select

    success = .true.
  end function ecBCReadLine


  ! =======================================================================

  !> Construct a new BCBlock instance with the specified id.
  !! Failure is indicated by returning a null pointer.
  function ecBCBlockCreate(bcBlockId) result(bcBlockPtr)
    type(tEcBCBlock), pointer            :: bcBlockPtr !< the new BCBlock, intent(out)
    integer,                  intent(in) :: bcBlockId  !< unique BCBlock id
    !
    integer :: istat   !< allocate() status
    logical :: success !< helper variable
    !
    success = .false.
    !
    ! allocation
    allocate(bcBlockPtr, stat = istat)
    if (istat == 0) then
       ! see what's allocatable in this object and add it here
       ! ......
    end if
    if (istat /= 0) then
       call setECMessage("ec_bcreader::ecBCBlockCreate: Unable to allocate additional memory.")
       bcBlockPtr => null()
       return
    end if
    ! initialization
    bcBlockPtr%id = bcBlockId
  end function ecBCBlockCreate

  ! BCBlock destructor
  function ecBCBlockFree(bcblock) result(success)
    implicit none
    logical                         :: success !< function status
    type(tEcBCBlock), intent(inout) :: bcblock !< intent(inout)
    !
    integer   :: i

    success = .false.

    !----------------------------------------
    success = .true.        ! RL666 : This destructor needs fixing asap, disabled for now, crashes
    return
    !----------------------------------------
    if (associated(bcblock%ncptr)) then
       bcblock%ncptr => null()
       ! TODO: also clean up netcdf instance somewhere (or is this not necessary)
    endif

    if (bcblock%bcFilePtr%fhandle>0) then
       call mf_close(bcblock%bcFilePtr%fhandle)
    endif

    if (associated(bcblock%ncptr)) then
       bcblock%ncptr => null()
    endif
    if (allocated(bcblock%columns)) then
       deallocate(bcblock%columns)
    endif
    if (associated(bcblock%vp)) then
       deallocate(bcblock%vp)
    endif

    do i = 1,size(bcblock%quantities,dim=1)
       if (.not.ecBCQuantityFree(bcblock%quantities(i))) then
          return                                        ! TODO: issue a warning
       endif
    enddo

    success = .true.
  end function ecBCBlockFree

   !> Frees a 1D array of tEcFieldPtrs, after which the fieldPtr is deallocated.
  function ecBCBlockFree1dArray(BCBlockPtr, nBCBlocks) result (success)
    logical                                    :: success    !< function status
    type(tEcBCBlockPtr), dimension(:), pointer :: BCBlockPtr !< intent(inout)
    integer, intent(inout)                     :: nBCBlocks  !< number of Fields
    !
    integer :: i      !< loop counter
    integer :: istat  !< deallocate() status
    !
    success = .true.
    !
    if (.not. associated(BCBlockPtr)) then
       call setECMessage("WARNING: ec_bcreader::ecBCBlockFree1dArray: Dummy argument BCBlockPtr is already disassociated.")
    else
       ! Free and deallocate all tEcFieldPtrs in the 1d array.
       do i=1, nBCBlocks
          if (ecBCBlockFree(BCBlockPtr(i)%Ptr)) then
             deallocate(BCBlockPtr(i)%ptr, stat = istat)
             if (istat /= 0) success = .false.
          else
             success = .false.
          end if
       end do
       ! Finally deallocate the tEcFieldPtr(:) pointer.
       if (success) then
          deallocate(BCBlockPtr, stat = istat)
          if (istat /= 0) success = .false.
       end if
    end if
    nBCBlocks = 0
end function ecBCBlockFree1dArray

      ! =======================================================================

  ! BCQuantity destructor
  function ecBCQuantityFree(bcquantity) result(success)
    implicit none
    logical                            :: success    !< function status
    type(tEcBCQuantity), intent(inout) :: bcquantity !< intent(inout)
    success = .false.
    if (allocated(bcquantity%jacolumn)) then
       deallocate(bcquantity%jacolumn)
    endif
    if (allocated(bcquantity%col2elm)) then
       deallocate(bcquantity%col2elm)
    endif
    if (allocated(bcquantity%astro_component)) then
       deallocate(bcquantity%astro_component)
    endif
    if (allocated(bcquantity%astro_amplitude)) then
       deallocate(bcquantity%astro_amplitude)
    endif
    if (allocated(bcquantity%astro_phase)) then
       deallocate(bcquantity%astro_phase)
    endif
    success = .true.
  end function ecBCQuantityFree


  ! =======================================================================

  subroutine sortndx(x,c,n)
    implicit none
    double precision, intent(in)    :: x(:)
    integer, intent(out)            :: c(:)
    integer, intent(in)             :: n

    integer                         :: a(n), b(n)
    integer                         :: i,j,bs
    double precision                :: as

    a(1:n) = x(1:n)
    ! n = size(a,dim=1)
    do i=1,n
       b(i) = i
       c(i) = i
    enddo
    do i=2,n
       as = a(i)
       bs = b(i)
       do j = i,2,-1
          if (a(j-1)>as) then
             a(j)=a(j-1)
             b(j)=b(j-1)
             c(b(j))=c(b(j))+1
          else
             a(j)=as
             b(j)=bs
             c(i)=j
             exit
          endif
       enddo
       if (j==1) then
          a(j)=as
          b(j)=bs
          c(i)=j
       endif
    enddo
  end subroutine sortndx

end module m_ec_bcreader
