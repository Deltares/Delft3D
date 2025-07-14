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

module m_ec_bccollect
    use m_ec_parameters
    use m_ec_typedefs
    use m_ec_provider
    use m_ec_bcreader
    use m_ec_support
    use m_ec_message
    use m_ec_instance
    use m_alloc 
    use multi_file_io
    use string_module
    use time_module
    implicit none 
          
    private 

    public   ::  ecCollectBCBlocks
    public   ::  collectbc_all

   interface ecCollectBCBlocks
      module procedure collectbc
      module procedure collectbc_all
   end interface ecCollectBCBlocks

   contains 

!============================================================================================================================
! Collectloop: Scan a bc-file, being given QUANTITY and LOCATION.
!              For each match, create a file reader, add it to the list of file readers in the instance
    integer function collectbc(instancePtr, fname, quantity, location, iostat) result (count)
    use m_ec_bcreader, only: checkhdr
    implicit none            
    type (tEcInstance),     pointer,   intent(in)      :: instancePtr     !< EC Instance, overall structure for the EC-module 
    character(len=*),                  intent(in)      :: fname           !< file name (bc-format)
    character(len=*),                  intent(in)      :: quantity        !< quantity name 
    character(len=*),                  intent(in)      :: location        !< location       ! quantity+location = search key 
    integer,                           intent(out)     :: iostat

    integer (kind=8)    ::  fhandle
    character(:), allocatable ::  rec
    integer             ::  reclen 
    integer             ::  commentpos
    character(len=1000) ::  keyvaluestr                                    ! all key-value pairs in one header 
    integer             ::  posfs
    integer             ::  nfld
    integer             ::  nq
    logical             ::  jablock, jaheader
    integer             ::  lineno 
    integer (kind=8)    ::  savepos 
    integer             ::  iostatloc
    type (tEcBCBlock),    pointer :: bcBlockPtr
    type (tEcFileReader), pointer :: fileReaderPtr
    integer             :: bcBlockId, fileReaderId

    iostat = EC_UNKNOWN_ERROR
    lineno = 0 
    savepos = 0

    if (.not.ecSupportOpenExistingFileGnu(fhandle, fname)) then
       iostat = EC_DATA_NOTFOUND
       return
    end if

    count = 0
    do while (.not.mf_eof(fhandle))
       call mf_read(fhandle,rec,savepos)
       iostatloc = 0 ! mf_read always ok?
       if (iostatloc /= 0) then 
          iostat = iostatloc
          return ! beter break?
       endif  
       lineno = lineno + 1 
       if (index('!#%*',rec(1:1))>0) cycle                     ! deal with various begin-of-line delimiters
       reclen = len_trim(rec)                                  ! deal with various comment delimiters 
       commentpos = index(rec,'//')
       if (commentpos>0) reclen = min(reclen,commentpos-1)
       commentpos = index(rec,' %')
       if (commentpos>0) reclen = min(reclen,commentpos-1)
       commentpos = index(rec,' #')
       if (commentpos>0) reclen = min(reclen,commentpos-1)
       commentpos = index(rec,' *')
       if (commentpos>0) reclen = min(reclen,commentpos-1)
       commentpos = index(rec,' !')
       if (commentpos>0) reclen = min(reclen,commentpos-1)

       if (len_trim(rec(1:reclen))>0) then                     ! skip empty lines 
          if (index(rec,'[forcing]'         )>0 .or. &
              index(rec,'[Boundary]'        )>0 .or. &
              index(rec,'[LateralDischarge]')>0) then          ! new boundary chapter       
             jaheader = .true.                                 ! switching to header mode 
             keyvaluestr = ','
             jablock=.false.
             nfld = 0                                          ! count the number of fields in this header block 
             nq = 0                                            ! count the (maximum) number of quantities in this block 
          else
             if (jaheader) then 
                posfs = index(rec(1:reclen),'=')               ! key value pair ?  
                if (posfs>0) then 
                   call replace_char(rec,9,32)                 ! replace tabs by spaces, header key-value pairs only 
                   nfld = nfld + 1                             ! count the number of lines in the header file 
                                                               ! Create a lengthy string of ',key,value,key,value.....' 
                   call str_upper(rec(1:posfs-1))              ! all keywords uppercase , not case sensitive 
                   if (index(rec(1:posfs-1),'QUANTITY')>0) then 
                      nq = nq + 1 
                   endif 

                   keyvaluestr = trim(keyvaluestr)//''''// (trim(adjustl(rec(1:posfs-1))))//''',''' &
                                                    //(trim(adjustl(rec(posfs+1:reclen))))//''',' 
                endif 
             else                                                    ! switch to datamode 
                call str_upper(keyvaluestr,len(trim(keyvaluestr)))   ! case insensitive format 
                if (matchblock(keyvaluestr,location,quantity)) then
                   bcBlockId = ecInstanceCreateBCBlock(InstancePtr)
                   bcBlockPtr=>ecSupportFindBCBlock(instancePtr, bcBlockId)
                   fileReaderId = ecInstanceCreateFileReader(InstancePtr)
                   fileReaderPtr = ecSupportFindFileReader(instancePtr, fileReaderID)
                   fileReaderPtr%bc => bcBlockPtr
                   if (.not.processhdr_all_quantities(bcBlockPtr,nfld,nq,keyvaluestr)) return   ! dumb translation of bc-object metadata  
                   if (.not.checkhdr(bcBlockPtr)) return                                        ! check on the contents of the bc-object 
                   bcBlockPtr%fname = fname 
                   if (ecSupportOpenExistingFileGnu(bcBlockPtr%bcFilePtr%fhandle, fname)) then
                      call mf_backspace(bcBlockPtr%bcFilePtr%fhandle, savepos)           ! set newly opened file to the appropriate position 
                      count = count + 1 
                      iostat = EC_NOERR
                   else 
                      call mf_close(bcBlockPtr%bcFilePtr%fhandle)
                      iostat = EC_DATA_NOTFOUND
                   end if
                endif                                       ! Right label
                jaheader = .false.                          ! No, we are NOT reading a header 
             endif          ! in header mode (data lines are ignored) 
          endif             ! not a new '[forcing]' item  
       endif                ! non-empty string 
    enddo                   ! read/scan loop, ended when we reached end-of-file
    iostat = EC_EOF      
    end function collectbc

!============================================================================================================================
! Collectloop: Scan a bc-file
!              For each bc-block, create a file reader, add it to the list of file readers in the instance
    integer function collectbc_all(instancePtr, fname, iostat, k_refdate, k_timezone, k_timestep_unit, dtnodal) result (count)
    use m_ec_alloc
    implicit none            
    type (tEcInstance),     pointer,   intent(in)      :: instancePtr     !< EC Instance, overall structure for the EC-module 
    character(len=*),                  intent(in)      :: fname           !< file name (bc-format)
    integer,                           intent(out)     :: iostat
    integer,                optional,  intent(in)      :: k_refdate       !< Kernel timeframe refdate
    real(hp),               optional,  intent(in)      :: k_timezone      !< Kernel timeframe timezone
    integer,                optional,  intent(in)      :: k_timestep_unit !< Kernel timeframe timestep unit 
    real(kind=hp),          optional,  intent(in)      :: dtnodal         !< Nodal factors update interval

    integer (kind=8)    ::  fhandle
    character(:), allocatable :: rec
    integer             ::  reclen 
    character(len=1000) ::  keyvaluestr                                    ! all key-value pairs in one header 
    integer             ::  posfs
    integer             ::  nfld
    integer             ::  nq
    logical             ::  jablock, jaheader
    integer             ::  lineno 
    integer (kind=8)    ::  savepos 
    type (tEcBCBlock),    pointer :: bcBlockPtr
    type (tEcBCFile),     pointer :: bcFilePtr
    type (tEcFileReader), pointer :: fileReaderPtr
    integer             :: bcBlockId, fileReaderId
    integer             :: ifr 
    logical             :: isLateral
    real(hp)            :: k_mjd                                          ! kernel reference date as Modified Julian date

    iostat = EC_UNKNOWN_ERROR
    lineno = 0 
    savepos = 0
    jaheader = .false.
    isLateral = .false.

    count = 0
    if (.not.ecSupportOpenExistingFileGnu(fhandle, fname)) then
       iostat = EC_DATA_NOTFOUND
       return
    end if

    if (instancePtr%nBCFiles == size(instancePtr%ecBCFilesPtr)) then
       if (.not. ecArrayIncrease(instancePtr%ecBCFilesPtr, instancePtr%nBCFiles)) then
          return
       end if
    end if
    instancePtr%nBCFiles = instancePtr%nBCFiles + 1

    allocate (bcFilePtr)
    bcFilePtr%bcfilename = fname
    bcFilePtr%fhandle = fhandle
    instancePtr%ecBCFilesPtr(instancePtr%nBCFiles)%Ptr => bcFilePtr

    do while (.not.mf_eof(fhandle))
       call mf_read(fhandle,rec,savepos)
       if (savepos<0) then
          iostat = EC_IO_ERROR
          return
       end if
       lineno = lineno + 1 
       if (index('!#%*',rec)==1) cycle                     ! if commentws out in the first position
       reclen = len_trim(rec)                              ! deal with various comment delimiters 
       if (reclen < 3) then
          cycle
       else
          if (index(rec,'[forcing]'         )>0 .or. &
              index(rec,'[Boundary]'        )>0 .or. &
              index(rec,'[LateralDischarge]')>0) then          ! new boundary chapter       
             jaheader = .true.                                 ! switching to header mode 
             keyvaluestr = ','
             jablock=.false.
             nfld = 0                                          ! count the number of fields in this header block 
             nq = 0                                            ! count the (maximum) number of quantities in this block
             if (index(rec,'[LateralDischarge]')>0) then
                isLateral = .true.
             else
                isLateral = .false.
             endif
          else
             if (jaheader) then
                posfs = index(rec(1:reclen),'=')               ! key value pair ?  
                if (posfs>0) then 
                   call replace_char(rec,9,32)                 ! replace tabs by spaces, header key-value pairs only 
                   nfld = nfld + 1                             ! count the number of lines in the header file 
                                                               ! Create a lengthy string of ',key,value,key,value.....' 
                   call str_upper(rec(1:posfs-1))              ! all keywords uppercase , not case sensitive 
                   if (index(rec(1:posfs-1),'QUANTITY')>0) then 
                      nq = nq + 1 
                   endif 
                   keyvaluestr = trim(keyvaluestr)//''''// (trim(adjustl(rec(1:posfs-1))))//''',''' &
                                                    //(trim(adjustl(rec(posfs+1:reclen))))//''',' 
                else                                                    ! switch to datamode 
                   call str_upper(keyvaluestr,len(trim(keyvaluestr)))   ! case insensitive format 
                   bcBlockId = ecInstanceCreateBCBlock(InstancePtr)
                   bcBlockPtr=>ecSupportFindBCBlock(instancePtr, bcBlockId)
                   fileReaderId = ecInstanceCreateFileReader(InstancePtr)
                   fileReaderPtr => ecSupportFindFileReader(instancePtr, fileReaderID)
                   fileReaderPtr%bc => bcBlockPtr
                   fileReaderPtr%ofType = provFile_bc
                   bcBlockPtr%isLateral = isLateral
                   if (.not.(processhdr_all_quantities(bcBlockPtr,nfld,nq,keyvaluestr))) then
                     return  ! dumb translation of bc-object metadata  
                   endif
!                  if (.not.(checkhdr(bcBlockPtr))) return    ! skip check                       ! check on the contents of the bc-object       
                   bcBlockPtr%ftype=BC_FTYPE_ASCII                                               ! set BC-Block filetype to ASCII

                   if (present(k_refdate) .and. present(k_timezone) .and. present(k_timestep_unit)) then 
                      k_mjd = date2mjd(k_refdate)
                      if (present(dtnodal)) then
                         if (.not.ecProviderInitializeTimeFrame(fileReaderPtr, k_mjd, k_timezone, k_timestep_unit, dtnodal)) return
                      else
                         if (.not.ecProviderInitializeTimeFrame(fileReaderPtr, k_mjd, k_timezone, k_timestep_unit)) return
                      endif
                   else
                      if (.not.ecProviderInitializeTimeFrame(fileReaderPtr, -1.d0, 0.d0, ec_second)) return
                   endif
                   bcBlockPtr%fposition = savepos
                   bcBlockPtr%bcFilePtr => bcFilePtr 
                   count = count + 1
                   jaheader = .false.
                endif 
             endif          ! in header mode (data lines are ignored) 
          endif             ! not a new '[forcing]' item  
       endif                ! non-empty string 
    enddo                   ! read/scan loop, ended when we reached end-of-file
    ! Create the items 
    do ifr = instancePtr%nFileReaders-count+1,instancePtr%nFileReaders                   ! The latest set of filereaders = latest set of bcBlocks       
       if (.not.items_from_bc_quantities(instancePtr,instancePtr%ecFileReadersPtr(ifr)%ptr)) return
    enddo
    iostat = EC_NOERR

    end function collectbc_all
!============================================================================================================================
    
    !> Given a character string of key-value pairs gathered from a header block,
    !> extract all relevant fields (and find the block of requested quantity)
    !> and fill a bc-object

    
    !> Under construction: one bc-quantity instance for each quantity, but with a vector dimensionality of 1 and number of layers 1. 
    !> For now use the existing processhdr, with only 1 quantity that can be a user-defined vector (actually containing multiple quantities)
    function processhdr_all_quantities(bc,nfld,nq,keyvaluestr) result (success) 
    implicit none 
    logical                                 ::      success 
    type (tEcBCBlock),  intent(inout)       ::      bc              !< boundary condition instance   
    integer,            intent(in)          ::      nfld            !< number of fields in the header 
    integer,            intent(in)          ::      nq              !< number of quantities (quantity block) 
    character(len=*),   intent(in)          ::      keyvaluestr     !< string containing header fields as key-value pairs

    integer                          ::     ifld 
    integer                          ::     iostat
    character(len=256),  allocatable  ::     hdrkeys(:)     !< All keys from header
    character(len=256),  allocatable  ::     hdrvals(:)     !< All values from header
    character(len=256)                ::     dumstr

    integer                          ::     iq, iq_sel

    success = .False.
    if (allocated(hdrkeys)) then 
       deallocate(hdrkeys)
    endif 
    if (allocated(hdrvals)) then 
       deallocate(hdrvals)
    endif 
    if (allocated(bc%quantities)) then 
       deallocate(bc%quantities)
    endif 
    allocate(hdrkeys(nfld),hdrvals(nfld))

    hdrvals=''
    hdrkeys=''
    read(keyvaluestr,*,iostat=iostat) dumstr,(hdrkeys(ifld),hdrvals(ifld),ifld=1,nfld)

    allocate(bc%quantities(nq))                    ! individual quantity objects 
    ! RL666: TODO: support vectors and layers, grouping of columns into quantities 
    do iq = 1,nq 
       allocate(bc%quantities(iq)%jacolumn(nq))
       allocate(bc%quantities(iq)%col2elm(nq))     ! RL66 check if this one is still needed
       bc%quantities(iq)%col2elm = -1 
       bc%quantities(iq)%col2elm(iq) = 1 
       bc%quantities(iq)%jacolumn = .false.        
       bc%quantities(iq)%jacolumn(iq) = .true.    
    enddo
    
    iq = 0                              
    iq_sel = 0
    do ifld=1,nfld
       call replace_char(hdrkeys(ifld),ichar('-'),ichar(' '))
       call replace_char(hdrkeys(ifld),ichar('_'),ichar(' '))
       call replace_char(hdrkeys(ifld),ichar('.'),ichar(' '))
       select case (trim(adjustl(hdrkeys(ifld))))
          case ('NAME')
               bc%bcname = trim(hdrvals(ifld))
          case ('QUANTITY')
               iq = iq + 1                                 ! count quantities, corresponds with column numbers [iq]
               bc%quantities(iq)%name = trim(hdrvals(ifld)) 
               select case (bc%func)                       ! some quantities have a distinct meaning, require interpretation later
               case (BC_FUNC_TSERIES, BC_FUNC_TIM3D)
                  if (trim(hdrvals(ifld))=='TIME') then    ! special check on the time field 
                     bc%timecolumn = iq 
                  endif 
               case (BC_FUNC_HARMONIC, BC_FUNC_ASTRO, BC_FUNC_HARMOCORR, BC_FUNC_ASTROCORR, BC_FUNC_CMP3D)
                  if (trim(hdrvals(ifld))=='HARMONIC COMPONENT') then          ! harmonic component
                     bc%astro_component_column = iq
                  endif
                  if (trim(hdrvals(ifld))=='ASTRONOMIC COMPONENT') then        ! astronomic component label
                     bc%astro_component_column = iq
                  endif
                  if (trim(hdrvals(ifld))==trim(bc%qname)//' AMPLITUDE') then  ! amplitude field for astronomic/harmonic components
                     bc%astro_amplitude_column = iq
                  endif
                  if (trim(hdrvals(ifld))==trim(bc%qname)//' PHASE') then      ! phase field for astronomic/harmonic components
                     bc%astro_phase_column = iq
                  endif
               case (BC_FUNC_QHTABLE)
                  if (trim(hdrvals(ifld))==trim(bc%qname)//' WATERLEVEL' .or. & 
                      trim(hdrvals(ifld))=='WATER_LEVEL')                 then ! waterlevel field for qh-
                     bc%qh_waterlevel_column = iq
                  endif
                  if (trim(hdrvals(ifld))==trim(bc%qname)//' DISCHARGE' .or. & 
                      trim(hdrvals(ifld))=='WATER_DISCHARGE')            then  ! discharge field for qh-
                     bc%qh_discharge_column = iq
                  endif
               end select                                          
          case ('UNIT')
               bc%quantities(iq)%unit = trim(hdrvals(ifld))
               if (iq==bc%timecolumn) then                 ! Is this the unit of time ? 
                  bc%timeunit = trim(hdrvals(ifld))        ! store timeunit string in this bc instance 
               endif 
          case ('FUNCTION')
               if (iq>0) cycle 
               select case (trim(adjustl(hdrvals(ifld))))
                  case ('TIMESERIES')
                     bc%func = BC_FUNC_TSERIES
                  case ('CONSTANT')                        ! Constant is a special version of time-series (Sobek3)
                     bc%func = BC_FUNC_CONSTANT
                     bc%timeint = timeint_bfrom            ! Time interpolation keyword is likely absent, default to a0=1 and a1=0
                  case ('T3D')
                     bc%func = BC_FUNC_TIM3D
                     allocate(bc%quantities(1))                     ! joint quantity objects
                  case ('QHTABLE')
                     bc%func = BC_FUNC_QHTABLE
                     if (len_trim(bc%qname)==0) then
                        bc%qname = 'QHBND'
                     endif
                  case ('HARMONIC')
                     bc%func = BC_FUNC_HARMONIC
                  case ('ASTRONOMIC')
                     bc%func = BC_FUNC_ASTRO
                  case default
                     call setECMessage("Unknown function """//trim(hdrvals(ifld))//"""")
                  return
               end select 
          case ('OFFSET')                           
               if (iq>0) then
                  read(hdrvals(ifld),*) bc%quantities(iq)%offset
               endif
          case ('FACTOR')
               if (iq>0) then
                  read(hdrvals(ifld),*) bc%quantities(iq)%factor
               endif
          case ('MISSING VALUE DEFINITION')
               if (iq>0) then
                  read(hdrvals(ifld),*) bc%quantities(iq)%missing
               endif
          case ('TIME INTERPOLATION')
               select case (trim(adjustl(hdrvals(ifld))))
                  case ('LINEAR')
                     bc%timeint = BC_TIMEINT_LIN
                  case ('LINEAR-EXTRAPOLATE')
                     bc%timeint = BC_TIMEINT_LIN_EXTRAPOL
                  case ('BLOCK-TO')
                     bc%timeint = BC_TIMEINT_BTO
                  case ('BLOCK-FROM')
                     bc%timeint = timeint_bfrom
               end select  
          case ('PERIODIC')
               select case (trim(adjustl(hdrvals(ifld))))
               case ('TRUE','T','.T.','1','JA','YES')
                  bc%periodic = .True.
               case default
                  bc%periodic = .False.
               end select  
       end select 
    enddo 
    
    ! Fill bc%quantity%col2elm(nq) which holds the mapping of columns in the file to vector positions
    bc%numcols = iq
    do iq = 1,nq 
       bc%quantities(iq)%col2elm = -1 
       bc%quantities(iq)%col2elm(iq) = 1
    enddo 

    deallocate(hdrkeys)
    deallocate(hdrvals)
    success = .True.
    end function processhdr_all_quantities

end module m_ec_bccollect
