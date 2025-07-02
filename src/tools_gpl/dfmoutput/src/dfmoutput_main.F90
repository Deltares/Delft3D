!----- AGPL --------------------------------------------------------------------
!                                                                               
!  Copyright (C)  Stichting Deltares, 2017-2025.                                
!                                                                               
!  This file is part of Delft3D (D-Flow Flexible Mesh component).               
!                                                                               
!  Delft3D is free software: you can redistribute it and/or modify              
!  it under the terms of the GNU Affero General Public License as               
!  published by the Free Software Foundation version 3.                         
!                                                                               
!  Delft3D  is distributed in the hope that it will be useful,                  
!  but WITHOUT ANY WARRANTY; without even the implied warranty of               
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                
!  GNU Affero General Public License for more details.                          
!                                                                               
!  You should have received a copy of the GNU Affero General Public License     
!  along with Delft3D.  If not, see <http://www.gnu.org/licenses/>.             
!                                                                               
!  contact: delft3d.support@deltares.nl                                         
!  Stichting Deltares                                                           
!  P.O. Box 177                                                                 
!  2600 MH Delft, The Netherlands                                               
!                                                                               
!  All indications and logos of, and references to, "Delft3D",                  
!  "D-Flow Flexible Mesh" and "Deltares" are registered trademarks of Stichting 
!  Deltares, and remain the property of Stichting Deltares. All rights reserved.
!                                                                               
!-------------------------------------------------------------------------------

! 
! 

!> DFMOUTPUT - A postprocessing tool for output files from D-Flow Flexible Mesh.
!! Combines several commands/operations into a single program.
!!
!! Available commands:
!! * mapmerge - Merge multiple _map.nc files into a single one, intended for merging partioned output files.
!! * max25    - Filter for histories.
!! * extract  - (not implemented) Extract time series on certain locations from _his.nc files.
!! * convert  - (not implemented) Convert old format map files into UGRID compliant map files.
!!
!! 
program dfmoutput
use IR_Precision                                                        ! Integers and reals precision definition.
use Data_Type_Command_Line_Interface, only: Type_Command_Line_Interface ! Definition of Type_Command_Line_Interface.
use dfmoutput_version_module
use dfm_params
use dfm_merge
use m_alloc
use precision, only : hp
use dfm_max25_getdata
use dfm_gen_filter
!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
implicit none
type(Type_Command_Line_Interface) :: cli          !< Command Line Interface (CLI).
integer(I4P)                      :: ierr         !< Error trapping flag.
character(len=MAXNAMELEN), allocatable :: infiles(:)  !< Input file name(s)
character(len=MAXNAMELEN)              :: listfile    !< List file containing all input file name(s)
character(len=MAXNAMELEN), allocatable :: outfiles(:) !< Output file name(s)
character(len=32        )              :: filter_length  !< filter lengths given for max25
character(len=32        )              :: var_name       !< variable name given for max25
character(len=MAXNAMELEN)              :: rec
character(len=12) :: coeff(3)
real(kind=hp)     :: rcoeff(3)
integer :: ninfiles
integer :: i, fp
logical :: exist
logical :: force

!-----------------------------------------------------------------------------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------------------------------------
!! initializing Command Line Interface
call cli%init(progname    = base_name,                                            &
              version     = version,                                             &
              description = 'Tool for handling D-Flow FM output files',                    &
              examples    = ["dfmoutput mapmerge         --infile model_0000_map.nc model_0001_map.nc", &
                             "dfmoutput max_running_mean --infile hisfile.nc                         ", &
                             "dfmoutput max25            --infile hisfile.nc                         ", &
                             "dfmoutput genfilter        --infile hisfile.nc  --intval 6             "])
!                             "dfmoutput extract --station='stat A' model_his.nc         "])

! setting Command Line Arguments
call cli%add(switch='--verbose',  switch_ab='-d', help='Print verbose diagnostics output.', required=.false., act='store_true', def='.false.')

! Set up MAPMERGE command
call cli%add_group(group='mapmerge',description='Merge multiple map files from parallel run into one.')

! set Command Line Arguments of mapmerge command
call cli%add(group='mapmerge',switch='--infile',  switch_ab='-i', help='One or more input files.',required=.false.,act='store',def=char(0),valname='FILE',nargs='+')
call cli%add(group='mapmerge',switch='--listfile',switch_ab='-F', help='Pass contents of LISTFILE as input files.',required=.false.,act='store',def='',valname='LISTFILE')
call cli%add(group='mapmerge',switch='--outfile', switch_ab='-o', help='Write output to file DSTFILE. Default: <model>_merged_map.nc',required=.false.,act='store',def='',valname='DSTFILE')
call cli%add(group='mapmerge',switch='--force',   switch_ab='-f', help='Force overwriting of existing output file.',required=.false.,act='store_true',def='.false.')
call cli%add(group='mapmerge',switch='--time',    switch_ab='-t', help='Only select certain time(s) from input files.'//char(10)// &
                                                                       'A time argument can be one of:'//char(10)// &
                                                                       '   NUMBER           time index in file, starting at 1.'//char(10)// &
                                                                       '   ''{'' DATETIME ''}'' datetime (ISO8601, e.g., 2015-07-31T15:37:28)'//char(10)// &
                                                                       '   ''LAST''           last available time in input file.', &
             required=.false.,act='store',def='',valname='TIME[:TIME2]')

!! Set up MAX_RUNNING_MEAN command
call cli%add_group(group='max_running_mean',description='Get max_running_mean value and other derived properties from his file.')
call cli%add(group='max_running_mean',switch='--infile',  switch_ab='-i', help='One input files.',required=.true.,act='store',def=char(0),valname='FILE')
call cli%add(group='max_running_mean',switch='--filterlength', switch_ab='-l', help='Filter length. Default: 13,25',required=.false.,act='store',def='13,25',valname='FILTERLENGTH')
call cli%add(group='max_running_mean',switch='--varname', help='Variable name. Default: waterlevel',required=.false.,act='store',def='waterlevel',valname='WATERLEVEL')
call cli%add(group='max_running_mean',switch='--outfile', switch_ab='-o', help='Write output to file OUTFILE. Default: max_running_mean.out',required=.false.,act='store',def='max_running_mean.out',valname='OUTFILE')

!! Set up MAX25 command
call cli%add_group(group='max25',description='Get max25 value and other derived properties from his file.')
call cli%add(group='max25',switch='--infile',  switch_ab='-i', help='One input files.',required=.true.,act='store',def=char(0),valname='FILE')
call cli%add(group='max25',switch='--filterlength', switch_ab='-l', help='Filter length. Default: 13,25',required=.false.,act='store',def='13,25',valname='FILTERLENGTH')
call cli%add(group='max25',switch='--varname', help='Variable name. Default: waterlevel',required=.false.,act='store',def='waterlevel',valname='WATERLEVEL')
call cli%add(group='max25',switch='--outfile', switch_ab='-o', help='Write output to file OUTFILE. Default: max25.out',required=.false.,act='store',def='max25.out',valname='OUTFILE')


!! Set up gen_filter command
call cli%add_group(group='genfilter',description='Get maximum value based on a generic filter.')
call cli%add(group='genfilter',switch='--infile',  switch_ab='-i', help='One input files.',required=.true.,act='store',def=char(0),valname='FILE')
call cli%add(group='genfilter',switch='--varname', help='Variable name. Default: waterlevel',required=.false.,act='store',def='waterlevel',valname='WATERLEVEL')
call cli%add(group='genfilter',switch='--outfile', switch_ab='-o', help='Write output to file OUTFILE. Default: max.out',required=.false.,act='store',def='max.out',valname='OUTFILE')
call cli%add(group='genfilter',switch='--intval', help='filter period in terms of timesteps. Default: 6',required=.false.,act='store',def='6',valname='INTVAL')
call cli%add(group='genfilter',switch='--coefimpl', help='Filter coefficient; impl. part. Default: 0.3',required=.false.,act='store',def='0.3',valname='COEFIMP')
call cli%add(group='genfilter',switch='--coefexpl', help='Filter coefficient; expl. part. Default: 0.3',required=.false.,act='store',def='0.3',valname='COEFEXPL')

!! Set up EXTRACT command
!call cli%add_group(group='extract',description='Extract time series from a his file.')
!call cli%add(group='extract', switch='--infile',  switch_ab='-i', help='Read input from file SRCFILE.', required=.true., act='store', def='', valname='SRCFILE')
!call cli%add(group='extract', switch='--outfile', switch_ab='-o', help='Write output to file DSTFILE.', required=.true., act='store', def='', valname='DSTFILE')

!! Set up CONVERT command
!call cli%add_group(group='convert',description='Convert file format')
!call cli%add(group='convert', switch='--infile',    switch_ab='-i',  help='Read input from file SRCFILE.', required=.true., act='store', def='', valname='SRCFILE')
!call cli%add(group='convert', switch='--outfile',   switch_ab='-o',  help='Write output to file DSTFILE.', required=.true., act='store', def='', valname='DSTFILE')
!call cli%add(group='convert', switch='--informat',  switch_ab='-if', help='Input format of SRCFILE.',      required=.true., act='store', def='', valname='FILE', choices='CF-OLD,HIS')
!call cli%add(group='convert', switch='--outformat', switch_ab='-of', help='Output format of SRCFILE.',     required=.true., act='store', def='', valname='FILE', choices='UGRID,CSV')

! parsing Command Line Interface
call cli%parse(error=ierr)
if (ierr /= 0) goto 888

! Check general options
call cli%get(switch='-d', val = verbose_mode, error=ierr)
if (verbose_mode) then
   write (*,'(a)') 'VERBOSE MODE'
end if

!-----------------------------------------------------------------------------------------------------------------------------------
!! Start this run:  Check which actual command needs to be done

! MAPMERGE command
if (cli%run_command('mapmerge')) then
   if (verbose_mode) then
      write (*,'(a)') 'MAPMERGE mode'
   end if

   !! -i FILE1 [FILE2 ...]
   call cli%get_varying(group='mapmerge', switch='-i', val = infiles, error=ierr)
   if (ierr /= 0) goto 888
   if (allocated(infiles)) then
      if (infiles(1)(1:1) /= char(0)) then ! Test for missing argument (dummy default was set to char(0))
         ninfiles = size(infiles)
      else
         ninfiles = 0
      end if
   else
      ninfiles = 0
   end if

   !! -F LISTFILE1  (alternative for -i)
   call cli%get(group='mapmerge', switch='-F', val = listfile, error=ierr)
   if (ierr /= 0) goto 888
   if (len_trim(listfile) > 0) then
      inquire(file=trim(listfile),exist=exist)
      if (exist) then
         open (newunit=fp, file = trim(listfile), action='read')
         ninfiles = 0
         do
            read (fp, '(a)', end=100) rec
            if (len_trim(rec) > 0) then
               ninfiles = ninfiles + 1
            end if
         end do
100      continue
         call realloc(infiles, ninfiles)
         rewind(fp)
         i = 0
         do
            read (fp, '(a)', end=101) rec
            if (len_trim(rec) > 0) then
               i = i + 1
               infiles(i) = trim(rec)
            end if
         end do
101      continue
         close (fp)
      else
         write (*,'(a)') 'Warning: dfmoutput: List file '''//trim(listfile)//'''not found.'
      end if
   end if

   !! -o OUTFILE
   allocate(outfiles(1))
   outfiles(1) = ''
   call cli%get(group='mapmerge', switch='-o', val = outfiles(1), error=ierr)
   if (ierr /= 0) goto 888

   call cli%get(group='mapmerge', switch='-f', val = force, error=ierr)
   if (ierr /= 0) goto 888

   ierr = dfm_merge_mapfiles(infiles, ninfiles, outfiles(1), force)

! MAX_RUNNING_MEAN command for water levels
else if (cli%run_command('max_running_mean')) then
   allocate(infiles(1))
   allocate(outfiles(1))
   call cli%get(group='max_running_mean', switch='-i', val = infiles(1), error=ierr)
   if (ierr /= 0) goto 888
   call cli%get(group='max_running_mean', switch='-o', val = outfiles(1), error=ierr)
   if (ierr /= 0) goto 888
   call cli%get(group='max_running_mean', switch='--varname', val = var_name, error=ierr)
   if (ierr /= 0) goto 888
   call cli%get(group='max_running_mean', switch='-l', val = filter_length, error=ierr)
   if (ierr /= 0) goto 888

   if (verbose_mode) then
      write(*,*) 'arguments for max25: ', trim(infiles(1)), ', ', trim(outfiles(1)), ', ', trim(var_name), ', ', trim(filter_length)
   endif

   call fmgetdata_running_mean(trim(infiles(1)), trim(outfiles(1)), trim(var_name), trim(filter_length))

! MAX25 command for water levels
else if (cli%run_command('max25')) then
   allocate(infiles(1))
   allocate(outfiles(1))
   call cli%get(group='max25', switch='-i', val = infiles(1), error=ierr)
   if (ierr /= 0) goto 888
   call cli%get(group='max25', switch='-o', val = outfiles(1), error=ierr)
   if (ierr /= 0) goto 888
   call cli%get(group='max25', switch='--varname', val = var_name, error=ierr)
   if (ierr /= 0) goto 888
   call cli%get(group='max25', switch='-l', val = filter_length, error=ierr)
   if (ierr /= 0) goto 888

   if (verbose_mode) then
      write(*,*) 'arguments for max25: ', trim(infiles(1)), ', ', trim(outfiles(1)), ', ', trim(var_name), ', ', trim(filter_length)
   endif

   call fmgetdata(trim(infiles(1)), trim(outfiles(1)), trim(var_name), trim(filter_length))

! genfilter command for water levels
else if (cli%run_command('genfilter')) then
   allocate(infiles(1))
   allocate(outfiles(1))
   call cli%get(group='genfilter', switch='-i', val = infiles(1), error=ierr)
   if (ierr /= 0) goto 888
   call cli%get(group='genfilter', switch='-o', val = outfiles(1), error=ierr)
   if (ierr /= 0) goto 888
   call cli%get(group='genfilter', switch='--varname', val = var_name, error=ierr)
   if (ierr /= 0) goto 888
   call cli%get(group='genfilter', switch='--intval', val = coeff(1), error=ierr)
   if (ierr /= 0) goto 888
   call cli%get(group='genfilter', switch='--coefimpl', val = coeff(2), error=ierr)
   if (ierr /= 0) goto 888
   call cli%get(group='genfilter', switch='--coefexpl', val = coeff(3), error=ierr)
   if (ierr /= 0) goto 888
   do i=1,3
      read(coeff(i),*) rcoeff(i)
   enddo

   if (verbose_mode) then
      write(*,*) 'arguments for genfilter: ', trim(infiles(1)), ', ', trim(outfiles(1)), ', ', trim(var_name)
      write(*,*) 'coefficients: ', rcoeff
   endif

   call gen_filter(trim(infiles(1)), trim(outfiles(1)), trim(var_name), rcoeff(1), rcoeff(2), rcoeff(3))

! EXTRACT command
else if (cli%run_command('extract')) then
   write (*,'(a)') 'Error: EXTRACT command not implemented yet.'
   goto 999

! CONVERT command
else if (cli%run_command('convert')) then
   write (*,'(a)') 'Error: CONVERT command not implemented yet.'
   goto 999
end if

goto 999

888 continue ! Error handling
   write (*,'(a)') 'Error code: '//trim(str(n=ierr))

999 continue
   if (allocated(infiles))  deallocate(infiles)
   if (allocated(outfiles)) deallocate(outfiles)
!!-----------------------------------------------------------------------------------------------------------------------------------
end program dfmoutput
