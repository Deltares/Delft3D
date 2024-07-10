! ---- LGPL --------------------------------------------------------------------
!
! Copyright (C)  Stichting Deltares, 2011-2024.
!
! This library is free software; you can redistribute it and/or
! modify it under the terms of the GNU Lesser General Public
! License as published by the Free Software Foundation version 2.1.
!
! This library is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! Lesser General Public License for more details.
!
! You should have received a copy of the GNU Lesser General Public
! License along with this library; if not, see <http://www.gnu.org/licenses/>.
!
! contact: delft3d.support@deltares.nl
! Stichting Deltares
! P.O. Box 177
! 2600 MH Delft, The Netherlands
!
! All indications and logos of, and references to, "Delft3D" and "Deltares"
! are registered trademarks of Stichting Deltares, and remain the property of
! Stichting Deltares. All rights reserved.
!
!------------------------------------------------------------------------------
!        $Author: Markus $
!        $Date: 1-04-03 10:52 $
!        $Source: /u/cvsroot/gpp/libsrc/ods/mor_sys.f,v $
!
subroutine ods_mor_nef_com_dim&
&(fname ,itype ,dimtyp, pardep, timdep, locdep,&
&ndim  ,ierror, option                       )
!-----------------------------------------------------------------------
!         Function: dimension selection for maps
!                   MORSYS NEFIS COM files
!
!-----------------------------------------------------------------------
!   Calling routine :              GETPAR
!-----------------------------------------------------------------------
!   Called  routines:              OPNNEF
!                                  CLOSFL
!                                  GETELT (nefis)
!                                  INQGRP (nefis)
!-----------------------------------------------------------------------
!    Parameters:
!    -----------
!
!   Var.      Type Dimensions
!   -------------------------
!
! FNAME      CH*256 3          I   full name including path and ext.
! ITYPE       I*4              I   file type
! DIMTYP     CH*3              I   filter required dimension par,tim,loc
! PARDEP      I*4              I   parameter dependency type
! TIMDEP      I*4              I        time dependency type
! LOCDEP      I*4              I    location dependency type
! NDIM        I*4   4          O   returned dimensions
! IERROR      I*4              O   = 0 no errors, = 1 error detected
! OPTION     CH*256           I/O  option (not used)
!-----------------------------------------------------------------------
!          Constants:
!
! Const.      Type
!
!-----------------------------------------------------------------------
!    Local variables:
!    ----------------
!
!   Var.      Type Dimensions
!   -------------------------
!
! CELDEF     CH*16                 Cell name definition
! ELMNAM     CH*16                 Element name definition
! OKEE        L*4                  Flag for error reqocnition
! GRPDEF     CH*16                 Group name definition
! GRPDMS      I*4  5               Array with GRPNDM dimensions
! GRPNDM      I*4                  Number of dimenmsions of tmap group
! GRPORD      I*4  5               Array which gives order in which data
!                                  must be read
! HDAFDS      I*4  999             Data file descriptor for the COM-DAT
!                                  file
! HDEFDS      I*4  2997            Definition file description for the
!                                  COM-DEF file
! IERROR      I*4                  Error code for NEFIS error
! num_layers_grid        I*4                  Number of layers
! KM          I*4                  Number of layers for chosen parameter
! L           I*4                  Help variable
! LMAX        I*4                  Number of constituents
! NPAR        I*4                  Number of found parameters
! NRCEL       I*4                  Number of cells defined in group 1&3
! UINDEX      I*4  3               Array with indices of the cells to
!                                  be read
! USRORD      I*4                  Sequence in which the cells must be
!                                  read
!-----------------------------------------------------------------------
!
!  DECLARATIONS
!
   include 'ods.inc'
!
   integer         ierror , itype  , ind
   integer                  npar   , ii
   integer         num_columns   , num_rows   , num_layers_grid
   integer         pardep , timdep , locdep
   integer         ndim(4)
!
   character*3     dimtyp
   character       fname(*)*256
   character*256   option
!
   logical         ex     , okee
!-----------------------------------------------------------------------
!-----declaration NEFIS
!-----------------------------------------------------------------------
   integer maxgrp , maxelm
   integer hdefds(2997) , hdafds(999)
   integer ntwav  , ntcur  , ntran  , ntbot , ntr    , ntrm   ,&
   &ntmbot , nthwb  , nthwg  , km
!
!-----------------------------------------------------------------------
!-----Initialisation
!-----------------------------------------------------------------------
   okee   = .true.
!pvb  write(*,*)' call com_dim'
!
   ierror =  0
!
!--------------------------------------------------------------------
!-----Test if com-<case>.dat and com-<case>.def Nefis COM-files
!     exist
!--------------------------------------------------------------------
   call ods_check_nefis( fname  , '.def' , ierror )
   if ( ierror .ne. ieok   ) then
      return
   endif
!--------------------------------------------------------------------
!-----Open com-<case>.dat and com-<case>.def COM-files
!--------------------------------------------------------------------
   call OPNNEF(fname, itype, hdafds, hdefds, ierror)
   if (ierror .ne. 0) then
      ierror = IEFIRO
      return
   endif
!--------------------------------------------------------------------
!-----Read array-dimensions from Nefis COM-files group 2
!--------------------------------------------------------------------
   ntcur=0
   ntwav=0
   nthwb=0
   ntran=0
   ntbot=0
   ntr=0
   ntrm=0
   ntmbot=0
   nthwg=0
   call comdim( hdafds , hdefds , okee   , npar  , num_rows  , num_columns  ,&
   &num_layers_grid   , ntcur  , ntwav  , nthwb , ntran , ntbot ,&
   &ntr    , ntrm   , ntmbot , nthwg , pardep, km    )
!-----------------------------------------------------------------------
!-----return required dimension
!-----------------------------------------------------------------------
!     if ( .NOT. okee) STOP 'Error in comdim'
   if (dimtyp(1:3) .eq. 'par') then
      ndim (1) = 1
      ndim (2) = npar
!        write( *,*) 'dim: nofpar', npar
   else if (dimtyp(1:3) .eq. 'tim') then
      ndim (1) = 1
      ndim (2) = max( ntcur  , ntwav  , nthwb  , ntran  ,  ntbot  ,&
      &ntr    , ntrm   , ntmbot , nthwg            )
   else if (dimtyp(1:3) .eq. 'loc') then
!-----------------------------------------------------------------------
!-----the number of dimensions differs per parameter
!-----pardep determines which parameter, km determines the number of
!-----layers
!-----------------------------------------------------------------------
      ndim (1) = 2
      ndim (2) = num_columns
      ndim (3) = num_rows
      if ( km     .gt. 1 ) then
         ndim (1) = 3
         ndim (4) = km
      endif
   else
      okee = .false.
   endif
!--------------------------------------------------------------------
!-----Close com-<case>.dat and com-<case>.def COM-files
!--------------------------------------------------------------------
   call CLOSFL(fname, ierror)
   okee = okee .and. (ierror .eq. 0)
   okee = .true.
!-----------------------------------------------------------------------
!-----return status to calling routine
!-----------------------------------------------------------------------
   ierror = IEOK
   if (.not. okee) then
      ierror = IEOTHR
   endif
!
   return
!-----------------------------------------------------------------------
end

subroutine ods_mor_nef_com_par&
&( fname , itype , pardef, maxdef, timdep, locdep,&
&maxlst, lang  , parlst, paruni, partyp, parcod,&
&nrlst , ierror, option                        )
!-----------------------------------------------------------------------
!         Function: parameter name selection for maps
!                   MORSYS  NEFIS COM files
!        Method used:
!-----------------------------------------------------------------------
!   Calling routine :              GETPAR
!-----------------------------------------------------------------------
!   Called  routines:              OPNNEF
!                                  CLOSFL
!                                  DATADM
!-----------------------------------------------------------------------
!    Parameters:
!    -----------
!
!   Var.      Type Dimensions
!   -------------------------
!
! FNAME      CH*256 3          I   full name including path and ext.
! ITYPE       I*4              I   file type
! PARDEF     CH*21  maxdef     I   filter for required parameters
! MAXDEF      I*4              I   number of filters in PARDEF
! TIMDEP      I*4              I   dependency of time for the parameters to get
! LOCDEP      I*4              I   dependency of location for the parameters
! MAXLST      I*4              I   maximum number of parameters possible
! LANG        I*4              I   language code
! PARLST     CH*21  maxlst     O   names of parameters
! PARUNI     CH*21  maxlst     O   units of parameters
! PARTYP      I*40  maxlst     O   type of dependency of parameters
! PARCOD      I*40  maxlst     O   access index of parameter
! NRLST       I*4              O   number of parameters to be returned
! IERROR      I*4              O   = 0 no errors, = 1 error detected
!
! OPTION     CH*256 1         I/O  Option (not used )
!-----------------------------------------------------------------------
!          Constants:
!
! Const.      Type
!
! MXNPAR      I*4                  Maximum number of array-elements in
!                                  the local workarrays
!-----------------------------------------------------------------------
!    Local variables:
!    ----------------
!
!   Var.      Type Dimensions
!   -------------------------
!
! CELDEF     CH*16                 Cell name definition
! HDAFDS      I*4  999             Data file descriptor for the COM-DAT
!                                  file
! HDEFDS      I*4  2997            Definition file description for the
!                                  COM-DEF file
! IERROR      I*4                  Error code for NEFIS error
! NOSTAT      I*4                  Number of defined stations
! NTRUV       I*4                  Number of cross-secties in u- and
!                                  v-direction
!-----------------------------------------------------------------------
!
!  DECLARATIONS
!
   include 'ods.inc'
!
   integer         mxnpar
!
   parameter (mxnpar = 60)
!
   integer         lang  ,i
   integer         locdep
   integer         timdep,itype
   integer         maxdef,maxlst,       npar
   integer                              ind
   integer         ierror,nrlst
!
   integer         partyp(maxlst)
   integer         parcod(maxlst)
!
   character       pardef(*)*21
   character       parlst(*)*21
   character       paruni(*)*21
   character       fname(*)*256
   character*256   option
!
   logical         ex           ,okee
!-----------------------------------------------------------------------
!-----declaration NEFIS
!-----------------------------------------------------------------------
   integer         hdefds(2997) , hdafds(999)
!
   integer       TMLCDP
   parameter     ( TMLCDP = IPLMNK + IPTDEP )
!-----------------------------------------------------------------------
!-----translation to more useful names
!-----------------------------------------------------------------------
   integer       nonams
   parameter     ( NONAMS = 10 )
   character*20  NAMES(2,NONAMS)
   data        ( NAMES(1,I)  , NAMES(2,I)  , I = 1,10 ) /&
   &'DP'                   , 'Depth (dynamic)'      ,&
   &'DP0'                  , 'Initial depth d-pts'  ,&
   &'DPF'                  , 'Depth of fixed layer' ,&
   &'DPS'                  , 'Initial depth'        ,&
   &'S1'                   , 'Water level'          ,&
   &'RSAL'                 , 'Salinity'             ,&
   &'RTEM'                 , 'Temperature'          ,&
   &'RSP'                  , 'Spiral flow intensty' ,&
   &'DICWW'                , 'Vertical diffusion'   ,&
   &'TAUMAX'               , 'Bottom shear stress'  /
!
!-----------------------------------------------------------------------
!-----Initialisation
!-----------------------------------------------------------------------
   okee   = .true.
!pvb  write(*,*)' call com_par'
!
   ierror =  0
!
!--------------------------------------------------------------------
!-----Test if com-<case>.dat and com-<case>.def Nefis COM-files
!     exist
!--------------------------------------------------------------------
   call ods_check_nefis( fname  , '.def' , ierror )
   if ( ierror .ne. ieok   ) then
      return
   endif
!--------------------------------------------------------------------
!-----Open com-<case>.dat and com-<case>.def COM-files
!--------------------------------------------------------------------
   call OPNNEF(fname, itype, hdafds, hdefds, ierror)
   if (ierror .ne. 0) then
      ierror = IEFIRO
      return
   endif
!--------------------------------------------------------------------
!-----Read array-dimensions from Nefis COM-files group 2
!--------------------------------------------------------------------
   call compar( hdafds , hdefds , okee   , tmlcdp , iplmnk , npar  ,&
   &partyp , parcod , parlst , paruni                  )
   nrlst  = npar
!--------------------------------------------------------------------
!-----Translate the names (if useful)
!--------------------------------------------------------------------
   do 120 i = 1,npar
      do 110 ind = 1,nonams
         if ( parlst(i) .eq. names(1,ind) ) then
            parlst(i) = names(2,ind)
         endif
110   continue
120 continue
!     WRITE( *,*) nrlst

!--------------------------------------------------------------------
   call CLOSFL(fname, ierror)
   okee   = okee .and. (ierror .eq. 0)
!-----------------------------------------------------------------------
!-----return status to calling routine
!-----------------------------------------------------------------------
   ierror = IEOK
   if (.not. okee) then
      ierror = IEOTHR
   endif
!
   return
!-----------------------------------------------------------------------
end
subroutine ods_mor_nef_com_tme&
&(fname  ,itype  ,timdef, maxdef ,pardep , locdep,&
&maxlst ,        timlst,         timtyp ,&
&nrlst  ,ierror ,option                         )
!-----------------------------------------------------------------------
!           Function: time selection for maps
!                     MORSYS NEFIS COM file
!        Method used:
!-----------------------------------------------------------------------
!   Calling routine :              GETTME
!-----------------------------------------------------------------------
!   Called  routines:              OPNNEF
!                                  CLOSFL
!                                  GETELT (nefis)
!-----------------------------------------------------------------------
!    Parameters:
!    -----------
!
!   Var.      Type Dimensions I/O  description
!   --------------------------------------------------------------------
!
! FNAME      CH*256 3         I    full name including path and ext.
! ITYPE       I*4             I    file type
! TIMDEF      r*8   maxdef*2  I    filter for required times
!                                  julian notation
! MAXDEF      I*4             I    number of locations / filters in TIMDEF
! PARDEP      I*4             I    parameter dependency of the times to get
! LOCDEP      I*4             I    location dependency of the times to get
! MAXLST      I*4             I    maximum number of parameters possible
!
! TIMLST      r*8   maxlst    O    list of times found in julian notation
! TIMTYP      I*4   maxlst    O    list with type of times
! NRLST       I*4             O    number of times found
! OPTION     CH*256           I/O  option (not used)
! IERROR      I*4             O    = 0 no errors, = 1 error detected
!-----------------------------------------------------------------------
!          Constants:
!
! Const.      Type
!
!-----------------------------------------------------------------------
!    Local variables:
!    ----------------
!
!   Var.      Type Dimensions
!   -------------------------
!
! BUFLEN      I*4                  Size in bytes of available buffer
! DT          R*4                  Time step in TUNIT seconds
! ELMNAM     CH*16                 Element name definition
! GRPDEF     CH*16                 Group name definition
! GRPDMS      I*4  5               Array with GRPNDM dimensions
! GRPNDM      I*4                  Number of dimenmsions of this group
! GRPORD      I*4  5               Array which gives order in which data
!                                  must be read
! HDAFDS      I*4  999             Data file descriptor for the COM-DAT
!                                  file
! HDEFDS      I*4  2997            Definition file description for the
!                                  COM-DEF file
! I           I*4                  Help var.
! IDAY        I*4                  Day part of ITDATE (dd)
! IERROR      I*4                  Error code for NEFIS error
! IHULP       I*4  2               Help array.
! IMO         I*4                  Month part of ITDATE (mm)
! IMO1        I*4                  Help var.
! ITDATE      I*4                  Initial simulation start date
! IY          I*4                  Year part of ITDATE (yyyy)
! JULDAY      I*4                  julian day number of ITDATE
! num_layers_grid        I*4                  Number of layers
! L           I*4                  Help var.
! LMAX        I*4                  Number of constituents
! M           I*4                  Help var.
! N           I*4                  Help var.
!                                  file
! OKEE        L*4                  Flag for error checking
! TUNIT       R*4                  Scale unit to define seconds
! UINDEX      I*4  3               Array with indices of the cells to
!                                  be read
! USRORD      I*4                  Sequence in which the cells must be
!                                  read
! ZRHO        L*4                  if .true. then density included
!-----------------------------------------------------------------------
!
!  DECLARATIONS
!
   include  'ods.inc'
!
   integer         ihulp(2)
   integer         itdate,ind, indcap
   integer         ierror,nrlst ,maxdef,maxlst,itype
   integer         julday
   integer         imo1
   integer         iy    ,imo   ,iday
   integer         pardep,locdep
   integer         timtyp(maxlst)
!
   real*8          timlst(maxlst)
   real*8          timdef(maxdef,2)
   real            dt    ,tunit
!
   character       fname(*)*256
   character*256   option
   character       grpdaf*16 , elmnaa*16
!
   logical         ex    ,okee
!-----------------------------------------------------------------------
!-----declaration NEFIS
!-----------------------------------------------------------------------
   integer hdefds(2997) , hdafds(999)
   integer ntwav  , nthwb  , ntcur  , ntran  ,ntbot  ,&
   &num_rows   , num_columns   , num_layers_grid   , npar   ,nthwg
!
   integer uindex(3,5  ),usrord(5),buflen
!
   integer GETELT

!-----------------------------------------------------------------------
!-----Initialisation
!-----------------------------------------------------------------------
   okee   = .true.
!pvb  write(*,*)' call com_tme'
!
   ierror =  0
!
!--------------------------------------------------------------------
!-----Test if com-<case>.dat and com-<case>.def Nefis COM-files
!     exist
!--------------------------------------------------------------------
   call ods_check_nefis( fname  , '.def' , ierror )
   if ( ierror .ne. ieok   ) then
      return
   endif
!--------------------------------------------------------------------
!-----Open com-<case>.dat and com-<case>.def COM-files
!--------------------------------------------------------------------
   call OPNNEF(fname, itype, hdafds, hdefds, ierror )
   if (ierror .ne. 0) then
      ierror = IEFIRO
      return
   endif
!--------------------------------------------------------------------
!-----Read constants from Nefis COM-files group 2
!-----AM:
!-----Probelm: what if the name is given in capitals?
!--------------------------------------------------------------------
   grpdaf    = 'PARAMS'
   ind=index(fname(1),'tram-')
   indcap=index(fname(1),'TRAM-')
   if( ind .ne. 0 .or. indcap .ne. 0 )grpdaf='MAPATRANNTR'
   ind=index(fname(1),'botm-')
   indcap=index(fname(1),'BOTM-')
   if( ind .ne. 0 .or. indcap .ne. 0 )grpdaf='MAPBOT'

   uindex(1,1) = 1
   uindex(2,1) = 1
   uindex(3,1) = 1
   usrord(1) = 1

   buflen    = 2 * 4
   elmnaa    = 'IT01'
   if(grpdaf.eq.'MAPATRANNTR') elmnaa = 'ITO1'
   okee = GETELT(hdefds   ,grpdaf    ,elmnaa    ,&
   &uindex   ,usrord    ,buflen    ,IHULP     )&
   &.eq. 0
   itdate=19000101
   if(okee)itdate    = ihulp (1)
!-----------------------------------------------------------------------
!-----Get timestep (in seconds). Complicated by the use of both DT (min)
!-----and TSCALE (sec). Method:
!-----if TSCALE is zero, or does not exist, rely on DT.
!-----otherwise assume that TSCALE is equal to 60*DT or indeed the
!-----actual timestep. These complications arise from the MORSYS system.
!-----------------------------------------------------------------------
   buflen    = 4
   tunit=1.
   elmnaa    = 'TSCALE'
   if ( ind .ne. 0 .or. indcap .ne. 0 ) ELMNAA='TSCALE'
   okee = GETELT(hdefds   ,grpdaf    ,elmnaa    ,&
   &uindex   ,usrord    ,buflen    ,DT        )&
   &.eq. 0
   if ( .not. okee .or. dt .eq. 0.0 ) then
      elmnaa = 'DT'
      tunit  = 60.0
      ierror = GETELT(hdefds   ,grpdaf    ,elmnaa    ,&
      &uindex   ,usrord    ,buflen    ,DT        )
   endif
!
   ind=index(fname(1),'tram-')
   indcap=index(fname(1),'TRAM-')
   if( ind .ne. 0 .or. indcap .ne. 0 ) dt=dt/tunit
!-----------------------------------------------------------------------
!-----Convert ITDATE to julian day number JULDAY
!-----------------------------------------------------------------------
   iday   = mod (itdate ,   100)
   imo    = mod (itdate , 10000) /  100
   iy     =      itdate / 10000
   imo1   = (imo -14)/12
   julday = iday - 32075 + 1461 * (iy+4800+imo1  )/ 4&
   &+ 367  * (imo  - 2    - imo1  *  12    )/12&
   &- 3    * ((iy  + 4900 + imo1  )/100    )/ 4

!--------------------------------------------------------------------
!-----Read array-timlst from Nefis COM-files
!--------------------------------------------------------------------
   ntcur=0
   ntwav=0
   nthwb=0
   nthwg=0
   ntran=0
   ntbot=0
   call comtme(hdafds , hdefds , okee   , npar   , num_rows   , num_columns   ,&
   &num_layers_grid   , ntcur  , ntwav  , nthwb  , nthwg  , ntran  ,&
   &ntbot  , pardep , timlst , nrlst  ,julday  , dt     ,&
   &tunit                                               )

!-----------------------------------------------------------------------
!-----check required and found number of parameter names
!-----------------------------------------------------------------------
!-----Close com-<case>.dat and com-<case>.def COM-files
!--------------------------------------------------------------------
   call CLOSFL(fname, ierror)
   okee = okee .and. (ierror .eq. 0)
!------------------------------------------------------------------------
!-----return status to calling routine
!-----------------------------------------------------------------------
   ierror = IEOK
   if (.not. okee) then
      ierror = IEOTHR
   endif
!
   return
!-----------------------------------------------------------------------
end

subroutine ods_mor_nef_com_mat&
&(fname ,itype  ,parcod, loc   , tim   ,misval,&
&i3gl  ,maxdim ,xdata , ierror, option,&
&ibuffs,rbuffs                               )
!-----------------------------------------------------------------------
!           Function: select map data out of MORSYS COM NEFIS files
!        Method used:
!
!-----------------------------------------------------------------------
!   Calling routine :              GETMAT
!-----------------------------------------------------------------------
!   Called  routines:              OPNNEF
!                                  CLOSFL
!                                  GETELT (nefis)
!-----------------------------------------------------------------------
!    Parameters:
!    -----------
!
!   Var.      Type Dimensions
!   -------------------------
!
! FNAME      CH*256 3        I     full name including path and ext.
! ITYPE       I*4            I     file type
! PARCOD      I*4            I     parameter to get data of
! LOC         I*4   3*3      I     list with indices of locations
! TIM         R*8   3        I     list with Julian dates
! MISVAL      R*4   1        I     missing value
! I3GL        I*4   1        I     code of data storage :
!                                  1 = fortran
!                                  2 = c
! MAXDIM      I*4            I     lenght of data array
! XDATA       R*4   maxdim   O     array with the data
! IERROR      I*4            O     = 0 no errors, = 1 error detected
! OPTION     CH*256          O     option (not used)
! IBUFFS      I*4   <len>    O/I   integer buffer for reading Nefis file
! RBUFFS      R*4   <len>    O/I   real    buffer for reading Nefis file
!-----------------------------------------------------------------------
!          Constants:
!
! Const.      Type
!
!-----------------------------------------------------------------------
!    Local variables:
!    ----------------
!
!   Var.      Type Dimensions
!   -------------------------
!
! BUFLEN      I*4                  Size in bytes of available buffer
! ELMNAM     CH*16                 Element name definition
! EX          L*4                  flag for exist of file
! GRPDEF     CH*16                 Group name definition
! GRPDMS      I*4  1               Array with GRPNDM dimensions
! GRPNDM      I*4                  Number of dimenmsions of this group
! GRPORD      I*4  1               Array which gives order in which data
!                                  must be read
! HDAFDS      I*4  999             Data file descriptor for the COM-DAT
!                                  file
! HDEFDS      I*4  2997            Definition file description for the
! IERROR      I*4                  Error code for NEFIS error
! LMAXD       I*4                  maximum(1,LMAX)
! N           I*4                  Counter for XDATA
! NOSTAT      I*4                  Number of stations
! NTRUV       I*4                  Number of cross-sections
! OKEE        L*4                  Flag for error checking
! UINDEX      I*4  3               Array with indices of the cells to
!                                  be read
! USRORD      I*4                  Sequence in which the cells must be
!                                  read
!
!--Pointer variables to buffer space
!
!-----------------------------------------------------------------------
!
!  DECLARATIONS
!
   include  'ods.inc'
!
!
   real*8           tim   (3)

   integer         maxdim, itype
   integer         parcod
   integer         loc   (3,3)
   integer         i3gl,ihulp(1),iday,itdate
   integer         ibuffs (*     )

   real            misval,dt,tunit
   real            xdata (maxdim)
   real            rbuffs (*     )

   character       fname (*)*256
   character*256   option
!-----------------------------------------------------------------------
!-----declaration Local variables
!-----------------------------------------------------------------------
!
   integer         ind , indcap, ierror
   integer         imo,iy,imo1,julday
!
   logical         ex    ,okee
!
!-----------------------------------------------------------------------
!-----declaration NEFIS
!-----------------------------------------------------------------------
   character grpdaf*16 , elmnaa*16
   integer   hdefds(2997) , hdafds(999)
   integer   ntwav  , nthwb  , ntcur  , ntran  , ntbot  ,&
   &num_rows   , num_columns   , num_layers_grid   , npar   , nthwg
   integer   ilayf  , ilayl
   integer   uindex(3,5),usrord(5),buflen
   integer   GETELT
!-----------------------------------------------------------------------
!-----Initialisation
!-----------------------------------------------------------------------
!
!pvb  write(*,*)' call com_mat'
   ierror =  0
   okee   = .true.
!--------------------------------------------------------------------
!-----Test if com-<case>.dat and com-<case>.def Nefis COM-files
!     exist
!--------------------------------------------------------------------
   call ods_check_nefis( fname  , '.def' , ierror )
   if ( ierror .ne. ieok   ) then
      return
   endif
!--------------------------------------------------------------------
!-----Open com-<case>.dat and com-<case>.def COM-files
!--------------------------------------------------------------------
   call OPNNEF(fname, itype, hdafds, hdefds, ierror)
   if (ierror .ne. 0) then
      ierror = IEFIRO
      return
   endif
!
!--------------------------------------------------------------------
!-----Read constants from Nefis COM-files group 2
!--------------------------------------------------------------------
   grpdaf    = 'PARAMS'
   ind=index(fname(1),'tram-')
   indcap=index(fname(1),'TRAM-')
   if( ind .ne. 0 .or. indcap .ne. 0 ) grpdaf='MAPATRANNTR'
   ind=index(fname(1),'botm-')
   indcap=index(fname(1),'BOTM-')
   if( ind .ne. 0 .or. indcap .ne. 0 ) grpdaf='MAPBOT'

   uindex(1,1) = 1
   uindex(2,1) = 1
   uindex(3,1) = 1
   usrord(1)   = 1
!
! -------- Some foolish typing error makes it necessary to distinguish
!          between IT01 and ITO1
!
   buflen    = 2 * 4
   elmnaa    = 'IT01'
   if(grpdaf.eq.'MAPATRANNTR') elmnaa = 'ITO1'
   okee = okee .and.&
   &GETELT(hdefds   ,grpdaf    ,elmnaa    ,&
   &uindex   ,usrord    ,buflen    ,IHULP     )&
   &.eq. 0
   itdate = 19000101
   if ( okee ) itdate = ihulp (1)
!
!-----------------------------------------------------------------------
!-----Get timestep (in seconds). Complicated by the use of both DT (min)
!-----and TSCALE (sec). Method:
!-----if TSCALE is zero, or does not exist, rely on DT.
!-----otherwise assume that TSCALE is equal to 60*DT or indeed the
!-----actual timestep. These complications arise from the MORSYS system.
!-----------------------------------------------------------------------
   buflen    = 4
   tunit=1.
   elmnaa    = 'TSCALE'
   if(ind.ne.0 .or. indcap .ne. 0 ) ELMNAA='TSCALE'
   okee = GETELT(hdefds   ,grpdaf    ,elmnaa    ,&
   &uindex   ,usrord    ,buflen    ,DT        )&
   &.eq. 0
   if ( .not. okee .or. dt .eq. 0.0 ) then
      elmnaa = 'DT'
      tunit  = 60.0
      ierror = GETELT(hdefds   ,grpdaf    ,elmnaa    ,&
      &uindex   ,usrord    ,buflen    ,DT        )
   endif
!
   ind=index(fname(1),'tram-')
   indcap=index(fname(1),'TRAM-')
   if( ind .ne. 0 .or. indcap .ne. 0 ) dt=dt/tunit
!
!-----------------------------------------------------------------------
!-----Convert ITDATE to julian day number JULDAY
!-----------------------------------------------------------------------
   iday   = mod (itdate ,   100)
   imo    = mod (itdate , 10000) /  100
   iy     =      itdate / 10000
   imo1   = (imo -14)/12
   julday = iday - 32075 + 1461 * (iy+4800+imo1  )/ 4&
   &+ 367  * (imo  - 2    - imo1  *  12    )/12&
   &- 3    * ((iy  + 4900 + imo1  )/100    )/ 4
!-----------------------------------------------------------------------
   ntcur=0
   ntwav=0
   nthwb=0
   ntran=0
   ntbot=0
   nthwg=0
   ilayf=loc(1,3)
   ilayl=loc(2,3)
   call commat( hdafds , hdefds , okee   , npar   , num_rows   ,&
   &num_columns   , num_layers_grid   , ntcur  , ntwav  , nthwb  ,&
   &nthwg  , ntran  , ntbot  , parcod , ibuffs ,&
   &rbuffs , tim    , xdata  , dt     , julday ,&
   &tunit  , misval , ilayf  , ilayl           )
!
   okee = okee .and. (ierror .eq. 0)
!--------------------------------------------------------------------
!-----Close com-<case>.dat and com-<case>.def COM-files
!--------------------------------------------------------------------
   call CLOSFL(fname, ierror)
   okee = okee .and. (ierror .eq. 0)
!-----------------------------------------------------------------------
!-----return status to calling routine
!-----------------------------------------------------------------------
   ierror = IEOK
   if (.not. okee) then
      ierror = IEOTHR
   endif
!
   return
!-----------------------------------------------------------------------
end
subroutine comdim( datfds  , deffds , okee   , npar   ,&
&num_rows    , num_columns   , num_layers_grid   , ntcur  ,&
&ntwav   , nthwb  , ntran  , ntbot  ,&
&ntr     , ntrm   , ntmbot , nthwg  ,&
&pardep  , km                       )
!
   integer ntcur  , ntwav   , nthwb  , ntran  , ntbot  , ntr   ,&
   &ntrm   , ntmbot  , nthwg  , num_rows   , num_columns   , npar  ,&
   &num_layers_grid   , pardep  , km
!
   integer icodb  , jcodb   , iku    , jku    , ikv    , jkv
   integer datfds(*) , deffds(*)
!
   integer   maxgrp,maxelm
   parameter ( maxgrp = 50 , maxelm = 50 )
   character grpnam(maxgrp)*60        , grpdef(maxgrp)*16,&
   &cel(maxgrp)*16           , elmnam(maxgrp,maxelm)*16,&
   &elmtyp(maxgrp,maxelm)*16 , elmqty(maxgrp,maxelm)*16,&
   &elmunt(maxgrp,maxelm)*16 , elmdes(maxgrp,maxelm)*64
   integer   grpdms(maxgrp) , elmdms(5,maxgrp,maxelm),&
   &elmndm(maxgrp,maxelm)
   integer   nelems(maxgrp) , grpndm(5,maxgrp)
!
   integer   i , j , igrp
   logical   okee

!     LOGICAL initlz /.TRUE./
!     INTEGER rpt
!     SAVE initlz

! BEGIN ===========================================================

!     IF ( initlz ) THEN
!       initlz = .FALSE.
!       OPEN( NEWUNIT = rpt, FILE = 'mor_sys.rpt')
!     ENDIF
!     WRITE(rpt,*) 'Enter: comdim'

   npar=0
   num_rows=0
   num_columns=0
   num_layers_grid=0
   igrp=0
   okee=.true.
   call mapcom( datfds , deffds , okee   , num_rows   , num_columns   , num_layers_grid   ,&
   &igrp   , grpnam , grpdef , cel    , elmnam , elmtyp ,&
   &elmqty , elmunt , elmdes , jcodb  , icodb  , iku    ,&
   &jku    , ikv    , jkv    , nelems , grpndm , grpdms ,&
   &elmdms , elmndm , maxgrp , maxelm , ntcur  , ntwav  ,&
   &nthwb  , nthwg  , ntran  , ntbot  , ntr    , ntrm   ,&
   &ntmbot                                              )
!     WRITE( rpt, *) 'num_rows num_columns', num_rows, num_columns
   if(num_rows.eq.0.and.num_columns.eq.0)then
      do 15 i=1,igrp-1
         do 15 j=1,nelems(i)
            if(elmndm(j,I).ge.2)then
               num_rows=elmdms(1,j,i)
               num_columns=elmdms(2,j,i)
               goto 17
            endif
15    continue
17    continue
   endif
!
! Count the parameters defined on a two- or threedimensional grid
!
   km     = 0
   do 25 i = 1,igrp-1
      do 20 j = 1,nelems(i)
         if ( elmndm(j,i) .ge. 2 ) then
            if ( elmdms(1,j,i) .eq. num_rows .and.&
            &elmdms(2,j,i) .eq. num_columns       )then
               npar   = npar   + 1
               if ( pardep .eq. npar   ) then
                  if ( elmndm(j,i) .eq. 3 ) then
                     km     = elmdms(3,j,i)
                  endif
               endif
            endif
         endif
20    continue
25 continue
   npar=npar+1
!     WRITE( rpt, *) 'npar', npar
!     WRITE(rpt,*) 'Exit : comdim'
end

subroutine compar( datfds , deffds , okee   , tmlcdp , iplmnk ,&
&npar   , partyp , parcod , parlst , paruni )
!
   integer   datfds(*),deffds(*),partyp(*),parcod(*),&
   &tmlcdp,npar,iplmnk
   character parlst(*)*(21),paruni(*)*21
   logical   okee
!
   integer   maxgrp,maxelm
   parameter ( maxgrp = 50 , maxelm = 50 )
   character grpnam(maxgrp)*60        , grpdef(maxgrp)*16,&
   &cel(maxgrp)*16           , elmnam(maxgrp,maxelm)*16,&
   &elmtyp(maxgrp,maxelm)*16 , elmqty(maxgrp,maxelm)*16,&
   &elmunt(maxgrp,maxelm)*16 , elmdes(maxgrp,maxelm)*64
   integer   grpdms(maxgrp) , elmdms(5,maxgrp,maxelm),&
   &elmndm(maxgrp,maxelm)
   integer   nelems(maxgrp) , grpndm(5,maxgrp)
!
   integer   i,j,igrp,jcodb,icodb,ntcur,ntwav,nthwb,ntran,nthwg,&
   &ntbot,ntr,ntrm,ntmbot,iku,jku,ikv,jkv,&
   &num_rows,num_columns,num_layers_grid
!pvb  write(*,*)' call compar'
   npar=0
   num_rows=0
   num_columns=0
   num_layers_grid=0
   igrp=0
   okee=.true.
   call mapcom( datfds , deffds , okee   , num_rows   , num_columns   , num_layers_grid   ,&
   &igrp   , grpnam , grpdef , cel    , elmnam , elmtyp ,&
   &elmqty , elmunt , elmdes , jcodb  , icodb  , iku    ,&
   &jku    , ikv    , jkv    , nelems , grpndm , grpdms ,&
   &elmdms , elmndm , maxgrp , maxelm , ntcur  , ntwav  ,&
   &nthwb  , nthwg  , ntran  , ntbot  , ntr    , ntrm   ,&
   &ntmbot                                              )
!
! TODO: same for num_layers_grid (not all parameters 3D though)
!
   if(num_rows.eq.0 .and. num_columns.eq.0 )then
      do 15 i=1,igrp-1
         do 15 j=1,nelems(i)
            if(elmndm(j,I).ge.2)then
               num_rows=elmdms(1,j,i)
               num_columns=elmdms(2,j,i)
               goto 17
            endif
15    continue
17    continue
   endif
!
! Retain the parameters defined on a two- or threedimensional grid
!
   do 20 i=1,igrp-1
      do 20 j=1,nelems(i)
         if ( elmndm(j,i) .ge. 2 ) then
            if ( elmdms(1,j,i) .eq. num_rows   .and.&
            &elmdms(2,j,i) .eq. num_columns         )then
               npar         = npar   + 1
               parcod(npar) = npar
               parlst(npar) = elmnam(j,i)
               partyp(npar) = IPLMNK
               if ( grpdef(i) .eq. 'WAVTIM'     .or.&
               &grpdef(i) .eq. 'BOTTIM'     .or.&
               &grpdef(i) .eq. 'KENMTIM'    .or.&
               &grpdef(i) .eq. 'CURTIM'     .or.&
               &grpdef(i) .eq. 'DWQTIM'     .or.&
               &grpdef(i) .eq. 'TAUTIM'     .or.&
               &grpdef(i) .eq. 'TRANSTIM'   .or.&
               &grpdef(i) .eq. 'MAPATRAN'   .or.&
               &grpdef(i) .eq. 'MAPTTRAN'   .or.&
               &grpdef(i) .eq. 'MAPBOTTIM'  .or.&
               &grpdef(i) .eq. 'map-series'      )&
               &partyp(npar) = TMLCDP
               paruni(npar) = elmunt(j,i)
            endif
         endif
20 continue
   npar         = npar   + 1
   parcod(npar) = npar
   parlst(npar) = 'UVDAMS'
   partyp(npar) = TMLCDP
   paruni(npar) = '-'
   return
end
subroutine commat( datfds , deffds , okee   , npar   ,&
&num_rows   , num_columns   , num_layers_grid   , ntcur  ,&
&ntwav  , nthwb  , nthwg  , ntran  ,&
&ntbot  , pardep , ibuffs , rbuffs ,&
&tim    , xdata  , dt     , julday ,&
&tunit  , misval , ilayf  , ilayl  )
   integer datfds(*) , deffds(*)
   integer ntcur  , ntwav  , nthwb  , ntran  , ntbot  , ntr    ,&
   &ntrm   , ntmbot , num_rows   , num_columns   , num_layers_grid   , nthwg  ,&
   &npar   , ibuffs(*)
   integer pardep , julday , ilayf  , ilayl
   real    tunit  , rbuffs(*) , xdata(*) , dt     , misval
   real*8  timlst , tim(3)
   logical okee
!
   integer uindex(3,4) , usrord(5) , nindex(3)
   integer l      , n      , iy      , imo     , imo1   , idp    ,&
   &itp    , m      , n1      , n2
   integer ilen   , ii     , jj      , itmodc  , iday   ,&
   &ihou   , imin   , isec
   integer icodb  , jcodb  , iku     , jku     , ikv    , jkv
!
   double precision curtm

   integer maxgrp , maxelm
   parameter ( maxgrp = 50 , maxelm = 50 )
   character grpnam(maxgrp)*60        , grpdef(maxgrp)*16,&
   &cel(maxgrp)*16           , elmnam(maxgrp,maxelm)*16,&
   &elmtyp(maxgrp,maxelm)*16 , elmqty(maxgrp,maxelm)*16,&
   &elmunt(maxgrp,maxelm)*16 , elmdes(maxgrp,maxelm)*64
   integer   grpdms(maxgrp) , elmdms(5,maxgrp,maxelm),&
   &elmndm(maxgrp,maxelm)
   integer   nelems(maxgrp) , grpndm(5,maxgrp)

   logical vector
   integer error  , i      , j      , k      , igrp   , ioff   ,&
   &igrpcu , iitim
   integer ingrp  , ixcor  , ivar   , iycor  , iu1    , iv1    ,&
   &iguu   , igvv   , iu     , iv     , ialfa  , ikcs   ,&
   &ikfu   , ikfv   , ikcu   , ikcv   , nm     , km
   integer GETELT
!pvb  write(*,*)' call commat'
   uindex(1,1)=1
   uindex(2,1)=1
   uindex(3,1)=1
   usrord(1)=1
   vector=.true.
   npar=0
   num_rows=0
   num_columns=0
   num_layers_grid=0
   igrp=0
   ivar=0
   jcodb=0
   icodb=0
   iku=0
   jku=0
   ikv=0
   jkv=0
   ntcur=0
   ntwav=0
   nthwb=0
   nthwg=0
   ntran=0
   ntbot=0
   ntr=0
   ntrm=0
   ntmbot=0
   call mapcom( datfds , deffds , okee   , num_rows   , num_columns   , num_layers_grid   ,&
   &igrp   , grpnam , grpdef , cel    , elmnam , elmtyp ,&
   &elmqty , elmunt , elmdes , jcodb  , icodb  , iku    ,&
   &jku    , ikv    , jkv    , nelems , grpndm , grpdms ,&
   &elmdms , elmndm , maxgrp , maxelm , ntcur  , ntwav  ,&
   &nthwb  , nthwg  , ntran  , ntbot  , ntr    , ntrm   ,&
   &ntmbot                                              )
   okee   = .true.
   if ( num_rows   .eq. 0 .and. num_columns   .eq. 0 ) then
      do 16 i = 1,igrp-1
         do 15 j = 1,nelems(i)
            if ( elmndm(j,i) .ge. 2 ) then
               num_rows   = elmdms(1,j,i)
               num_columns   = elmdms(2,j,i)
               goto 17
            endif
15       continue
16    continue
!
17    continue
   endif
!
   ii=-1000
   do 25 i = 1,igrp-1
!
! For some groups we need to refer to the group CURTIM. So store
! its index
!
      if ( grpdef(i) .eq. 'CURTIM' ) then
         igrpcu = i
      endif
      do 20 j = 1,nelems(i)
         if ( elmndm(j,i) .ge. 2 ) then
            if ( elmdms(1,j,i) .eq. num_rows   .and.&
            &elmdms(2,j,i) .eq. num_columns         ) then
               npar   = npar   + 1
               if ( pardep .eq. npar   ) then
                  ii     = i
                  jj     = j
                  km     = 1
                  if ( elmndm(j,i) .eq. 3 ) then
                     km     = elmdms(3,j,i)
                  endif
               endif
            endif
         endif
20    continue
25 continue
!
   npar   = npar   + 1
   ilen   = 0
   if ( ii     .ne. -1000 ) then
      if ( grpdef(ii) .eq. 'WAVTIM'     ) ilen   = ntwav
      if ( grpdef(ii) .eq. 'CURTIM'     ) ilen   = ntcur
      if ( grpdef(ii) .eq. 'TRANSTIM'   ) ilen   = ntran
      if ( grpdef(ii) .eq. 'BOTTIM'     ) ilen   = ntbot
      if ( grpdef(ii) .eq. 'MAPTTRAN'   ) ilen   = ntrm
      if ( grpdef(ii) .eq. 'MAPATRAN'   ) ilen   = ntr
      if ( grpdef(ii) .eq. 'time-map'   ) ilen   = nthwb
      if ( grpdef(ii) .eq. 'map-series' ) ilen   = nthwg
      if ( grpdef(ii) .eq. 'MAPBOTTIM'  ) ilen   = ntmbot

      iitim  = ii

      if ( grpdef(ii) .eq. 'DWQTIM' .or.&
      &grpdef(ii) .eq. 'TAUTIM'      ) then
         ilen   = ntcur
         iitim  = igrpcu
      endif
      if ( nthwb .ne. 0 ) grpdef(ii) = 'map-info-series'
   endif
   nindex(1) = 1
   nindex(2) = 1
   nindex(3) = 1
   do 100 i = 1,ilen
      uindex(1,1) =  i
      uindex(2,1) =  i
      uindex(3,1) =  1
      itmodc      = -1
      error  = getelt(deffds,grpdef(iitim),&
      &elmnam(1,iitim),uindex,usrord,&
      &4,itmodc)
      call nefout( elmnam(1,iitim) , error  )
      okee   = error .eq.0
      if ( error .ne. 0 .or. itmodc .eq. -1 ) then
!-----------------------------------------------------------------------
!-----------In case an error occured while reading the file
         goto 200
      endif
!-----------------------------------------------------------------------
!--------Calculate current time elapsed in seconds referenced to ITDATE
!        Note (AM):
!        Some files span very long times (more than 60 years). Using
!        only integers presents a problem. Hence the use of double
!        precision reals.
!-----------------------------------------------------------------------
      curtm  = dble( itmodc ) * dble( dt * tunit )
      iday   = int( curtm / 86400.0d00 )
      isec   = int( curtm - 86400.0d00 * dble( iday ) )
      ihou   =   isec   / 3600
      imin   = ( isec   - ihou  * 3600 ) / 60
      isec   =   isec   - ihou  * 3600   - imin * 60
!--------------------------------------------------------------------------
!--------Convert true time from julian day-number
!--------------------------------------------------------------------------
      l      = julday + iday   +  68569
      n      = 4      * l      / 146097
      l      = l - ( 146097 * n + 3 ) / 4
      iy     = 4000 * ( l + 1 ) / 1461001
      l      = l - 1461 * iy / 4 + 31
      imo    = 80 * l / 2447
      iday   = l - 2447 * imo / 80
      l      = imo / 11
      imo    = imo + 2 - 12 * l
      iy     = 100 * ( n - 49 ) + iy + l
!--------------------------------------------------------------------------
!--------Convert to julian notation and store
!--------------------------------------------------------------------------
      imo1      = (imo -14)/12
      idp       = iday - 32075 + 1461 * (iy+4800+imo1  )/ 4&
      &+ 367  * (imo  - 2    - imo1  *  12    )/12&
      &- 3    * ((iy  + 4900 + imo1  )/100    )/ 4
      itp       = ihou * 3600 + imin * 60 + isec - 43200
      timlst    = dble(idp) + dble(itp) / 86400d0
      if ( timlst .le. tim(1)    ) then
         nindex(1) = i
      endif
      if ( timlst .le. tim(2)    ) then
         nindex(2) = i
      endif
100 continue
200 continue
   if ( pardep .ne. npar   ) then
      if(elmnam(jj,ii).eq.'FX'.or.elmnam(jj,ii).eq.'FY')ivar=2
      if(elmnam(jj,ii).eq.'MX'.or.elmnam(jj,ii).eq.'MY')ivar=11
      if(elmnam(jj,ii).eq.'QU'.or.elmnam(jj,ii).eq.'QV')ivar=12
      if(elmnam(jj,ii).eq.'U1'.or.elmnam(jj,ii).eq.'V1')ivar=1
      if(elmnam(jj,ii).eq.'TTXA'.or.elmnam(jj,ii).eq.'TTYA')ivar=6
      if(elmnam(jj,ii).eq.'TTXSA'.or.elmnam(jj,ii).eq.'TTYSA')ivar=8
      if(elmnam(jj,ii).eq.'SX   '.or.elmnam(jj,ii).eq.'SY   ')ivar=3
      if(elmnam(jj,ii).eq.'STX  '.or.elmnam(jj,ii).eq.'STY  ')ivar=4
      if(elmnam(jj,ii).eq.'SXS  '.or.elmnam(jj,ii).eq.'SYS  ')ivar=9
      if(elmnam(jj,ii).eq.'STXS '.or.elmnam(jj,ii).eq.'STYS ')ivar=10
      if(elmnam(jj,ii).eq.'TX'.or.elmnam(jj,ii).eq.'TY')ivar=13
   endif


   if ( pardep .eq. npar   .and. jku    .ne. 0 .and. ivar   .eq. 0&
   &.and. elmnam(jj,ii) .ne. 'XCOR' .and.&
   &elmnam(jj,ii) .ne. 'YCOR'       ) then
!
!  UVDAMS
!
      ikfu   = 1
      ikfv   = ikfu+num_rows*num_columns*km
      ilen   = 4 * num_rows   * num_columns   * km
      ioff   =    -num_rows   * num_columns   * km
!
! --------- Loop over the timesteps
!
      do 390 i = nindex(1),nindex(2),nindex(3)
         ioff  = ioff   + num_rows   * num_columns   * km
         uindex(1,1) = i
         uindex(2,1) = i
         uindex(3,1) = 1
!
         error = getelt(deffds,grpdef(jku),&
         &elmnam(iku,jku),uindex,usrord,&
         &ilen,ibuffs(ikfu))
         error = getelt(deffds,grpdef(jkv),&
         &elmnam(ikv,jkv),uindex,usrord,&
         &ilen,ibuffs(ikfv))

!--------UVDAMS                           parcod = 50
!  TODO: UVDAMS possibly three-dimensional?
!
         nm    = num_rows*num_columns
         do 320 n = 1,num_rows
            do 310 m = 1,num_columns
               n1           =   (n-1) * num_columns +  m
               n2           =   (m-1) * num_rows +  n
               xdata ( ioff+n1 ) =&
               &ibuffs(n2) * 1.0 + ibuffs(n2+nm) * 2.0
310         continue
320      continue
390   continue
!
      goto 1000
   endif
!
   if ( pardep .eq. npar   ) goto 1000
!
   if ( ivar   .ne. 0 ) then
      ioff   =    -num_rows   * num_columns   * km
!
! --------- Vector quantities:
!           Loop over the timesteps
!
      do 490 i = nindex(1),nindex(2),nindex(3)
         ioff  = ioff   + num_rows   * num_columns   * km
         uindex(1,1) = i
         uindex(2,1) = i
         uindex(3,1) = 1
!
         ingrp=i
         ixcor=1
         iycor=ixcor+num_rows*num_columns
         iu1  =iycor+num_rows*num_columns
         iv1  =iu1  +num_rows*num_columns*km
         iguu =iv1  +num_rows*num_columns*km
         igvv =iguu +num_rows*num_columns
         iu   =igvv +num_rows*num_columns
         iv   =iu   +num_rows*num_columns*km
         ialfa=iv   +num_rows*num_columns*km
         ikcs =1
         ikfu =ikcs +num_rows*num_columns
         ikfv =ikfu +num_rows*num_columns
         ikcu =ikfv +num_rows*num_columns
         ikcv =ikcu +num_rows*num_columns
         call travec(ivar,ingrp,deffds,datfds,&
         &rbuffs(ixcor),rbuffs(iycor),rbuffs(iu1),rbuffs(iv1),&
         &rbuffs(iguu),rbuffs(igvv),rbuffs(iu),rbuffs(iv),&
         &rbuffs(ialfa),ibuffs(ikcs),ibuffs(ikfu),ibuffs(ikfv),&
         &ibuffs(ikcu),ibuffs(ikcv),num_rows,num_columns,num_layers_grid,misval)
         if(elmnam(jj,ii).eq.'FX'.or.elmnam(jj,ii).eq.'MX'.or.&
         &elmnam(jj,ii).eq.'QU'.or.elmnam(jj,ii).eq.'U1'.or.&
         &elmnam(jj,ii).eq.'TTXA'.or.elmnam(jj,ii).eq.'TTXSA'.or.&
         &elmnam(jj,ii).eq.'SX  '.or.elmnam(jj,ii).eq.'STX  '.or.&
         &elmnam(jj,ii).eq.'SXS '.or.elmnam(jj,ii).eq.'STXS '.or.&
         &elmnam(jj,ii).eq.'TX ')then
            n1 = 1
            n2 = 1 + num_rows * num_columns * ( ilayf  - 1 )
            do 21 k = ilayf,ilayl
               call tramat( xdata(ioff+n1) , rbuffs(iu+n2-1) ,&
               &num_rows           , num_columns            )
               n1 = n1 + num_rows * num_columns
               n2 = n2 + num_rows * num_columns
21          continue
         else
            n1 = 1
            n2 = 1 + num_rows * num_columns * ( ilayf  - 1 )
            do 22 k = ilayf,ilayl
               call tramat( xdata(ioff+n1) , rbuffs(iv+n2-1) ,&
               &num_rows           , num_columns            )
               n1 = n1 + num_rows * num_columns
               n2 = n2 + num_rows * num_columns
22          continue
         endif
490   continue
   else
!
! -------- Scalar parameters:
!          Loop over the timesteps
!
      ilen   = 4 * num_rows   * num_columns   * km
      ioff   =    -num_rows   * num_columns   * km
!
      do 110 i = nindex(1),nindex(2),nindex(3)
         ioff   = ioff   + num_rows   * num_columns   * km
         uindex(1,1) = i
         uindex(2,1) = i
         uindex(3,1) = 1
!
         if ( elmtyp(jj,ii) .eq. 'REAL' ) then
            error = getelt(deffds,grpdef(ii),&
            &elmnam(jj,ii),uindex,usrord,&
            &ilen,rbuffs)
            if ( elmnam(jj,ii) .eq. 'XCOR' .or.&
            &elmnam(jj,ii) .eq. 'YCOR' .or.&
            &elmnam(jj,ii) .eq. 'XP'   .or.&
            &elmnam(jj,ii) .eq. 'YP'       )then
               uindex(1,1) = 1
               uindex(2,1) = 1
               uindex(3,1) = 1
               error = getelt(deffds,grpdef(jcodb),&
               &elmnam(icodb,jcodb),uindex,usrord,&
               &ilen,ibuffs)
               do 35 n = 1,num_rows
                  do 30 m = 1,num_columns
                     n1           =   (n-1) * num_columns +  m
                     n2           =   (m-1) *num_rows+n
!AM
!                       if ( abs(rbuffs(n2)) .lt. 1.e-8 .or.
!    *                       ibuffs(n2)      .lt. 0         )
!    *                     rbuffs(n2)=misval
                     if ( ibuffs(n2)      .lt. 0         )&
                     &rbuffs(n2)=misval
                     xdata ( ioff+n1 ) = rbuffs(n2)
30                continue
35             continue
            else
!
               n1 = 1
               n2 = 1 + num_rows * num_columns * ( ilayf  - 1 )
               do 45 k = ilayf,ilayl
                  call tramat( xdata(ioff+n1) , rbuffs(n2) ,&
                  &num_rows           , num_columns       )
                  n1 = n1 + num_rows * num_columns
                  n2 = n2 + num_rows * num_columns
45             continue
            endif
         else
            error = getelt(deffds,grpdef(ii),&
            &elmnam(jj,ii),uindex,usrord,&
            &ilen,ibuffs)
            do 55 n = 1,num_rows
               do 50 m = 1,num_columns
                  n1           =   (n-1) * num_columns +  m
                  n2=(m-1)*num_rows+n
                  xdata ( ioff+n1 ) = ibuffs(n2)
50             continue
55          continue
         endif
110   continue
!
      call nefout(elmnam(jj,ii),error)
      okee=error.eq.0
   endif
!
1000 continue
   return
end
subroutine comtme( datfds , deffds , okee   , npar    ,&
&num_rows   , num_columns   , num_layers_grid   , ntcur   ,&
&ntwav  , nthwb  , nthwg  , ntran   ,&
&ntbot  , pardep , timlst , nrlist  ,&
&julday , dt     , tunit            )
   integer datfds(*),deffds(*)
   integer num_rows   , num_columns   , num_layers_grid   , npar   , pardep ,&
   &julday , nrlist , ntcur  , ntwav  , nthwb  ,&
   &ntran  , ntbot  , ntr    , ntrm   , ntmbot ,&
   &nthwg
   real*8  timlst(*)
   real    dt     , tunit
   logical okee
!
   integer   maxgrp,maxelm
   parameter ( maxgrp = 50 , maxelm = 50 )
   character grpnam(maxgrp)*60        , grpdef(maxgrp)*16,&
   &cel(maxgrp)*16           , elmnam(maxgrp,maxelm)*16,&
   &elmtyp(maxgrp,maxelm)*16 , elmqty(maxgrp,maxelm)*16,&
   &elmunt(maxgrp,maxelm)*16 , elmdes(maxgrp,maxelm)*64
   integer   grpdms(maxgrp) , elmdms(5,maxgrp,maxelm),&
   &elmndm(maxgrp,maxelm)
   integer   nelems(maxgrp) , grpndm(5,maxgrp)
!
   integer error,i,j,igrp,igrpcu,iitim,&
   &uindex(3,5),icodb,jcodb,iku,jku,ikv,jkv,&
   &usrord(5)
   integer ilen,ii,itmodc,iday,ihou,imin,isec
   integer l,n,iy,imo,imo1,idp,itp,nhulp
   integer GETELT
   double precision curtm

!pvb  write(*,*)' call comtme'
   uindex(1,1)=1
   uindex(2,1)=1
   uindex(3,1)=1
   usrord(1)=1
   npar=0
   num_rows=0
   num_columns=0
   num_layers_grid=0
   igrp=0
   okee=.true.
   call mapcom( datfds , deffds , okee   , num_rows   , num_columns   , num_layers_grid   ,&
   &igrp   , grpnam , grpdef , cel    , elmnam , elmtyp ,&
   &elmqty , elmunt , elmdes , jcodb  , icodb  , iku    ,&
   &jku    , ikv    , jkv    , nelems , grpndm , grpdms ,&
   &elmdms , elmndm , maxgrp , maxelm , ntcur  , ntwav  ,&
   &nthwb  , nthwg  , ntran  , ntbot  , ntr    , ntrm   ,&
   &ntmbot                                              )
!
   if( num_rows .eq. 0 .and. num_columns .eq. 0 )then
      do 15 i=1,igrp-1
         do 15 j=1,nelems(i)
            if(elmndm(j,I).ge.2)then
               num_rows=elmdms(1,j,i)
               num_columns=elmdms(2,j,i)
               goto 17
            endif
15    continue
17    continue
   endif
!
   igrpcu=0
   ii=-1000
   do 20 i=1,igrp-1
!
! For some groups we need to refer to the group CURTIM. So store
! its index
!
      if ( grpdef(i) .eq. 'CURTIM' ) then
         igrpcu = i
      endif
!
      do 20 j=1,nelems(i)
         if(elmndm(j,i).ge.2)then
            if(elmdms(1,j,i).eq.num_rows.and.elmdms(2,j,i).eq.num_columns)then
               npar=npar+1
               if(pardep.eq.npar)then
                  ii=i
               endif
            endif
         endif
20 continue
   npar=npar+1
   if(pardep.eq.npar)then
      ii=i
   endif
   ilen=0
   if ( ii .ne. -1000 ) then
      if(grpdef(ii).eq.'WAVTIM')ilen=ntwav
      if(grpdef(ii).eq.'CURTIM')ilen=ntcur
      if(grpdef(ii).eq.'TRANSTIM')ilen=ntran
      if(grpdef(ii).eq.'BOTTIM')ilen=ntbot
      if(grpdef(ii).eq.'MAPTTRAN')ilen=ntrm
      if(grpdef(ii).eq.'MAPATRAN')ilen=ntr
      if(grpdef(ii).eq.'MAPBOTTIM')ilen=ntmbot
      if(grpdef(ii).eq.'time-map')ilen=nthwb
      if(grpdef(ii).eq.'map-series')ilen=nthwg
      if(grpdef(ii).eq.'GRID')ilen=1

      iitim = ii

      if ( grpdef(ii) .eq. 'DWQTIM' .or.&
      &grpdef(ii) .eq. 'TAUTIM'      ) then
         ilen=ntcur
         iitim=igrpcu
      endif
   endif
   nhulp=ilen
   do 100 i=1,ilen
      uindex(1,1) = i
      uindex(2,1) = i
      uindex(3,1) = 1
      itmodc=-1
      if(nthwb.ne.0)grpdef(iitim)='map-info-series'
      error=getelt(deffds,grpdef(iitim),&
      &elmnam(1,iitim),uindex,usrord,&
      &4,itmodc)
      call nefout(elmnam(1,iitim),error)
      okee=error.eq.0
      if (error .ne. 0 .or. itmodc .eq. -1) then
!-----------------------------------------------------------------------
!-----------In case an error occured while reading the file then
!           write part which is ok to nhulp = i - 1
!-----------------------------------------------------------------------
         nhulp  = i - 1
         goto 200
      endif
!-----------------------------------------------------------------------
!--------Calculate current time elapsed in seconds referenced to ITDATE
!-----------------------------------------------------------------------
      curtm  = dble( itmodc ) * dble( dt * tunit )
      iday   = int( curtm / 86400.0d00 )
      isec   = int( curtm - 86400.0d00 * dble( iday ) )
      ihou   =   isec   / 3600
      imin   = ( isec   - ihou  * 3600 ) / 60
      isec   =   isec   - ihou  * 3600   - imin * 60
!--------------------------------------------------------------------------
!--------Convert true time from julian day-number
!--------------------------------------------------------------------------
      l      = julday + iday   +  68569
      n      = 4      * l      / 146097
      l      = l - ( 146097 * n + 3 ) / 4
      iy     = 4000 * ( l + 1 ) / 1461001
      l      = l - 1461 * iy / 4 + 31
      imo    = 80 * l / 2447
      iday   = l - 2447 * imo / 80
      l      = imo / 11
      imo    = imo + 2 - 12 * l
      iy     = 100 * ( n - 49 ) + iy + l
!--------------------------------------------------------------------------
!--------Convert to julian notation and store
!--------------------------------------------------------------------------
      imo1      = (imo -14)/12
      idp       = iday - 32075 + 1461 * (iy+4800+imo1  )/ 4&
      &+ 367  * (imo  - 2    - imo1  *  12    )/12&
      &- 3    * ((iy  + 4900 + imo1  )/100    )/ 4
      itp       = ihou * 3600 + imin * 60 + isec - 43200
      timlst(i) = dble(idp) + dble(itp) / 86400d0
100 continue

!-----------------------------------------------------------------------
!-----Exception handling not enough data written to Nefis files
!-----------------------------------------------------------------------
200 continue
!
   nrlist = nhulp
!
   return
end
! =====================================================================
subroutine mapcom&
&( datfds , deffds , okee   , num_rows   , num_columns   , num_layers_grid   ,&
&igrp   , grpnam , grpdef , cel    , elmnam , elmtyp ,&
&elmqty , elmunt , elmdes , jcodb  , icodb  , iku    ,&
&jku    , ikv    , jkv    , nelems , grpndm , grpdms ,&
&elmdms , elmndm , maxgrp , maxelm , ntcur  , ntwav  ,&
&nthwb  , nthwg  , ntran  , ntbot  , ntr    , ntrm   ,&
&ntmbot                                              )

!     Function :

! DATA ----------------------------------------------------- arguments --

   logical&
   &okee

   character&
   &grpnam*60,grpdef*16,grpdaf*16,&
   &cel*16,&
   &elmnam*16,&
   &elmtyp*16,&
   &elmqty*16,&
   &elmunt*16,&
   &elmdes*64

   INTEGER&
   &datfds, deffds,&
   &maxgrp, igrp, grpdms, grpndm,&
   &maxelm, elmdms, elmndm,&
   &jcodb,icodb,iku,jku,ikv,jkv,&
   &ntcur,ntwav,nthwb,nthwg,ntran,ntbot,ntr,ntrm,ntmbot,&
   &num_rows, num_columns, num_layers_grid

   DIMENSION&
   &datfds(*),&
   &deffds(*),&
   &grpnam(*),&
   &grpdef(*),&
   &grpdms(maxgrp),&
   &grpndm(5,maxgrp),&
   &cel(*),&
   &elmnam(maxgrp,*),&
   &elmtyp(maxgrp,*),&
   &elmqty(maxgrp,*),&
   &elmunt(maxgrp,*),&
   &elmdes(maxgrp,*),&
   &elmdms(5,maxgrp,maxelm),&
   &elmndm(maxgrp,*)

! DATA --------------------------------------------------- local --

!     LOGICAL initlz  /.TRUE./

   CHARACTER&
   &celnam*16,&
   &elmnum*16,&
   &elmtup*16,&
   &elmqtu*16,&
   &elmuut*16,&
   &elmdus*64

   INTEGER&
   &uindex,jor,&
   &usrord,grpdm,&
   &INQGRP,INQCEL,INQFST,INQNXT,INQMXI,&
   &inqelm, getelt,&
   &nelems, mxpgrp,mxpelm,&
   &error,i,nbytsg,grpdim

!     INTEGER nofelm, elmnr, n, rpt

   PARAMETER (&
   &mxpgrp = 50,&
   &mxpelm = 50&
   &)

   DIMENSION&
   &nelems( mxpgrp),&
   &uindex(3,5),jor(5),&
   &usrord(5), grpdim(5)

!     SAVE initlz

! BEGIN ==============================================================

!     IF ( initlz ) THEN
!       initlz = .FALSE.
!       OPEN( NEWUNIT = rpt, FILE = 'mapcom.rpt')
!     ENDIF
!     write(rpt,*) 'Enter: mapcom'

   uindex(1,1) = 1
   uindex(2,1) = 1
   uindex(3,1) = 1
   usrord(1)   = 1
   igrp        = 0
!
! -------- (AM) The number of layers is at least 1, the element 'num_layers_grid'
!               may explicitly set this parameter. The other grid
!               parameters (num_rows and num_columns) are always set explicitly.
!
   num_layers_grid        = 1
!
1111 continue
   igrp   = igrp   + 1
!     WRITE( *,*) 'igrp', igrp
   if ( igrp .gt. mxpgrp ) goto 9999
   NELEMS(IGRP) = mxpelm
   if( igrp .eq. 1 )then
      error  = inqfst(datfds,grpnam(igrp),grpdef(igrp))
!       call nefout('inqfst',error)
   else
      error  = inqnxt(datfds,grpnam(igrp),grpdef(igrp))
!       call nefout('inqnxt',error)
   endif
   if( error .ne. 0 ) goto 9999
!
   grpdaf = grpdef(igrp)
   grpdm  = 5
   ERROR=INQGRP(DEFFDS,grpdaf,celnam,GRPDM,&
   &GRPndm(1,IGRP),jOR)
   cel(igrp)      = celnam
   grpdms(igrp)   = grpdm
   GRPndm(1,IGRP) = MAX( GRPndm(1,IGRP) , 1 )
   okee   = error.eq.0
   ERROR  = INQCEL(DEFFDS,CEL(igrp),NELEMS(IGRP),ELMNAM(1,IGRP))
!     call nefout('cell '//cel(igrp),error)
   okee   = okee .and. error.eq.0
   if ( .not. okee ) then
      nelems(igrp) = 0
      goto 9999
   endif
   do 10 i = 1,nelems(igrp)
      elmnum = elmnam(i,igrp)
      elmndm(i,igrp) = 5
      elmtup = ' '
      error  = inqelm(deffds,elmnum,elmtup,&
      &nbytsg,elmqtu,elmuut,&
      &elmdus,elmndm(i,igrp),elmdms(1,i,igrp))
      elmtyp(i,igrp) = elmtup
      elmqty(i,igrp) = elmqtu
      elmunt(i,igrp) = elmuut
      elmdes(i,igrp) = elmdus
!     call nefout('elmnam '//elmnam(i,igrp),error)
      okee   = error.eq.0
!
      if ( elmnam(i,igrp) .eq. 'num_rows' ) then
         error  = getelt(deffds,grpdef(igrp),&
         &elmnam(i,igrp),uindex,usrord,&
         &4,num_rows)
         okee   = okee .and. error.eq.0
      endif
      if ( elmnam(i,igrp) .eq. 'num_columns' ) then
         error  = getelt(deffds,grpdef(igrp),&
         &elmnam(i,igrp),uindex,usrord,&
         &4,num_columns)
         okee   = okee .and. error.eq.0
      endif
      if ( elmnam(i,igrp) .eq. 'num_layers_grid' ) then
         error  = getelt(deffds,grpdef(igrp),&
         &elmnam(i,igrp),uindex,usrord,&
         &4,num_layers_grid)
         if ( num_layers_grid .le. 0 ) num_layers_grid   = 1
         okee   = okee .and. error.eq.0
      endif
      if ( elmnam(i,igrp) .eq. 'NTWAV' ) then
         error  = getelt(deffds,grpdef(igrp),&
         &elmnam(i,igrp),uindex,usrord,&
         &4,ntwav)
         okee   = okee .and. error.eq.0
      endif
      if ( elmnam(i,igrp) .eq. 'time-map' ) then
         error  = inqmxi(deffds,grpdef(igrp),nthwb)
         okee   = okee .and. error.eq.0
      endif
      if ( elmnam(i,igrp) .eq. 'HSIGN' ) then
         error  = inqmxi(deffds,grpdef(igrp),nthwg)
         okee   = okee .and. error.eq.0
      endif
      if ( elmnam(i,igrp) .eq. 'NTCUR' ) then
!AM
!
!           error  = getelt(deffds,datfds,grpdef(igrp),
!    *                      elmnam(i,igrp),uindex,usrord,
!    *                      4,ntcur)
!AM
!           error  = inqgrp(deffds,grpdef(igrp),celnam,grpdm,
!    *                      grpdim,usrord)
         error  = inqmxi(deffds,'CURTIM',ntcur)
      endif
      if ( elmnam(i,igrp) .eq. 'NTTRAN' ) then
         error  = getelt(deffds,grpdef(igrp),&
         &elmnam(i,igrp),uindex,usrord,&
         &4,ntran)
      endif
      if ( elmnam(i,igrp) .eq. 'NTBOT' ) then
         error  = getelt(deffds,grpdef(igrp),&
         &elmnam(i,igrp),uindex,usrord,&
         &4,ntbot)
      endif
      if ( elmnam(i,igrp) .eq. 'NTR  ' ) then
         error  = getelt(deffds,grpdef(igrp),&
         &elmnam(i,igrp),uindex,usrord,&
         &4,ntr  )
      endif
      if ( elmnam(i,igrp) .eq. 'NTRM ' ) then
         error  = getelt(deffds,grpdef(igrp),&
         &elmnam(i,igrp),uindex,usrord,&
         &4,ntrm )
      endif
      if ( elmnam(i,igrp) .eq. 'NTMBOT' ) then
         error  = getelt(deffds,grpdef(igrp),&
         &elmnam(i,igrp),uindex,usrord,&
         &4,ntmbot)
      endif
      if ( elmnam(i,igrp) .eq. 'CODB' ) then
         icodb  = i
         jcodb  = igrp
      endif
      if ( elmnam(i,igrp) .eq. 'CODE' ) then
         icodb  = i
         jcodb  = igrp
      endif
      if ( elmnam(i,igrp) .eq. 'KFU' ) then
         iku    = i
         jku    = igrp
      endif
      if ( elmnam(i,igrp) .eq. 'KFV' ) then
         ikv    = i
         jkv    = igrp
      endif
10 continue
!
   GOTO 1111
!
9999 CONTINUE
!     elmnr = 0
!     DO i = 1, igrp-1
!       WRITE( rpt,*) 'Cell ', i,'  ',cel(i)
!       nofelm = nelems(i)
!       DO n = 1, nofelm
!         elmnr = elmnr + 1
!         WRITE( rpt,*) n, '  ',elmnam(n,i)
!       ENDDO
!     ENDDO
!     write(rpt,*) 'Exit : mapcom'
end
subroutine nefout(name,error)
   character name*(*)
   integer error
!     if(error.ne.0.and.error.ne.-17021)then
!       write(*,*)' nefis error ',error
!     else
!       write(*,'(1x,a)')name
!     endif
   return
end
subroutine tr2wav (u1, v1, uzeta, vzeta, xcor, ycor,&
&guu, gvv, num_rows, num_columns, kcs, kfu, kfv,&
&ivar,alfas,alfa,misval)

!
! parameters:
!
! name     type     lenght      i o   description
! ====     ====     ======      ===   ===========
! guu      real     num_rows*num_columns   *     coeff. arrays g-eta-eta
! gvv      real     num_rows*num_columns   *     coeff. arrays g-ksi-ksi
! kcs      integer  num_rows*num_columns   *     0/1/2 non-active / active /
!                                     boundary water-level point
! kfu      integer  num_rows*num_columns   *     0/1 non-active / active u-point
! kfv      integer  num_rows*num_columns   *     0/1 non-active / active v-point
! num_columns     integer  1           *     m-size of the grid
! num_rows     integer  1           *     n-size of the grid
! u1       real     num_rows*num_columns   *     array with u-velocity in u-point
! uzeta    real     num_rows*num_columns     *   array with x-velocity in zetapoint
! v1       real     num_rows*num_columns   *     array with v-velocity in v-point
! vzeta    real     num_rows*num_columns     *   array with y-velocity in zetapoint
! xcor     real     num_rows*num_columns   *     x-coordinate here used in depth
!                                     point
! ycor     real     num_rows*num_columns   *     y-coordinate here used in depth
!                                     point
!
! important local variables:
!
! name     type     lenght            description
! ====     ====     ======            ===========
! eps      real     1                 small value for real value tests
! guugem   real     1                 g-eta-eta in zeta point
! gvvgem   real     1                 g-ksi-ksi in zeta point
! ugem     real     1                 u- velocity in zeta point in
!                                     transformed plane
! vgem     real     1                 v- velocity in zeta point in
!                                     transformed plane
! xeta     real     1                 x-coordinate for eta line for
!                                     transformation to physical plane
! xksi     real     1                 x-coordinate for ksi line for
!                                     transformation to physical plane
! yeta     real     1                 y-coordinate for eta line for
!                                     transformation to physical plane
! yksi     real     1                 y-coordinate for ksi line for
!                                     transformation to physical plane
!***********************************************************************
!-----------------------------------------------------------------------
!-----specifications and declarations
!-----------------------------------------------------------------------
   integer num_rows,num_columns
   real         u1    (num_rows  ,num_columns  ), alfas (num_rows  ,num_columns  ),&
   &v1    (num_rows  ,num_columns  ), uzeta (num_rows  ,num_columns  ),&
   &vzeta (num_rows  ,num_columns  ),&
   &xcor  (num_rows  ,num_columns  ), ycor  (num_rows  ,num_columns  ),&
   &guu   (num_rows  ,num_columns  ), gvv   (num_rows  ,num_columns  )
   Logical      alfa,lvar
   integer      kcs   (num_rows  ,num_columns  ),&
   &kfu   (num_rows  ,num_columns  ),kfv   (num_rows  ,num_columns  )
   integer ivar,m,n,n1,m1,md,nd
   real eps,xksi,yksi,xeta,yeta,guugem,gvvgem,ugem,vgem,&
   &pi,degrad,um,umd,vn,vnd,misval,countu,countv
!-----------------------------------------------------------------------
!-----initialisation
!-----------------------------------------------------------------------
!
!-----parameters
!
   eps   =  0.
!
!-----arrays
!     For vectorplots the defaultvalue is <eps>.
!
   Do 20 m = 1,num_columns
      Do 10 n = 1,num_rows
         uzeta (n,m) = misval
         vzeta (n,m) = misval
10    Continue
20 Continue
!-----------------------------------------------------------------------
!-----calculate u end v at zeta points
!     note: inside the irocol table guu and gvv are
!           never .le. eps so this check can be skipped.
!-----------------------------------------------------------------------
   If (.NOT.alfa) Then
      do 100 m=1,num_columns
         do 110 n = 1,num_rows
!
!
! -------- This statement means that some cells which are clearly
!          active, are not filled with a meaningful value.
!          (AM)
!         if((kcs(n,m).ne.0.and.kfu(n,m).ne.0.and.kfv(n,m).ne.0))then
!
!          Changed to:
            if ( kcs(n,m).ne.0 ) then
               n1=max(n-1,1)
               m1=max(m-1,1)
               if ( kcs   (n  ,m  ) .eq.   0   .or.&
               &guu   (n  ,m1) .le. eps   .or.&
               &gvv   (n1,m  ) .le. eps ) then
                  uzeta (n,m) = eps
                  vzeta (n,m) = eps
               else
                  xksi   = .5 * (xcor  (n  ,m  ) - xcor  (n  ,m1) +&
                  &xcor  (n1,m  ) - xcor  (n1,m1))
                  yksi   = .5 * (ycor  (n  ,m  ) - ycor  (n  ,m1) +&
                  &ycor  (n1,m  ) - ycor  (n1,m1))
                  xeta   = .5 * (xcor  (n  ,m  ) - xcor  (n1,m  ) +&
                  &xcor  (n  ,m1) - xcor  (n1,m1))
                  yeta   = .5 * (ycor  (n  ,m  ) - ycor  (n1,m  ) +&
                  &ycor  (n  ,m1) - ycor  (n1,m1))
!
                  guugem =  (guu   (n  ,m  ) + guu   (n  ,m1)) / 2.
                  gvvgem =  (gvv   (n  ,m  ) + gvv   (n1,m  )) / 2.
                  if ( ivar .le. 4 .or. ivar .gt. 10 ) then
                     if ( abs(gvvgem) .gt. 0. ) then
                        ugem   =   kcs   (n  ,m  ) * .5*&
                        &(u1    (n  ,m  ) * kfu   (n  ,m  )  +&
                        &u1    (n  ,m1)  * kfu   (n  ,m1)) /&
                        &gvvgem
                     endif
                     if ( abs(guugem) .gt. 0. ) then
                        vgem   =   kcs   (n  ,m  ) * .5*&
                        &(v1    (n  ,m  ) * kfv   (n  ,m  )  +&
                        &v1    (n1,m  )  * kfv   (n1,m  )) /&
                        &guugem
                     endif
                  else
                     if ( abs(gvvgem) .gt. 0. ) then
                        ugem   = kcs(n,m)*u1(n,m)/gvvgem
                     endif
                     if ( abs(guugem) .gt. 0. ) then
                        vgem   = kcs(n,m)*v1(n,m)/guugem
                     endif
                  endif
                  uzeta (n,m) = xksi   * ugem   + xeta  * vgem
                  vzeta (n,m) = yksi   * ugem   + yeta  * vgem
               endif
            endif
110      continue
100   continue
   Else
      pi     = 4.*ATAN (1.0)
      degrad = pi/180.
      Do 210 n=1,num_rows
         Do 200 m=1,num_columns
            md   = MAX (1,m-1)
            nd   = MAX (1,n-1)
!
! -------- This leads to missing vectors near an edge of the grid:
!          (AM)
!                      lvar = ((kcs(n ,m ).EQ.1).AND.(kfu(n,m).EQ.1.OR.
!    *                          kfu(n ,md).EQ.1  .OR. kfv(n,m).EQ.1.OR.
!    *                          kfv(nd,m ).EQ.1))
!                      If (lvar) Then
!                         um   = u1(n ,m )*kfu(n ,m )
!                         umd  = u1(n ,md)*kfu(n ,md)
!                         vn   = v1(n ,m )*kfv(n ,m )
!                         vnd  = v1(nd,m )*kfv(nd,m )
!                         ugem = .5*(um+umd)*kcs(n,m)
!                         vgem = .5*(vn+vnd)*kcs(n,m)
!
!          Instead:
!
            if ( kcs(n,m)    .eq. 1 ) then
               ugem   = 0.0
               vgem   = 0.0
               countu = 0.0
               countv = 0.0
               if ( kfu(n,m)  .eq. 1 ) then
                  ugem   = u1(n,m)
                  countu = 1.0
               endif
               if ( kfu(n,md) .eq. 1 ) then
                  ugem   = ugem   + u1(n,md)
                  countu = countu + 1.0
               endif
               if ( kfv(n,m)  .eq. 1 ) then
                  vgem   = vgem   + v1(n,m)
                  countv = 1.0
               endif
               if ( kfv(nd,m) .eq. 1 ) then
                  vgem   = vgem   + v1(nd,m)
                  countv = countv + 1.0
               endif
!
               if ( countu .gt. 0.0 )&
               &ugem   = ugem   / countu
               if ( countv .gt. 0.0 )&
               &vgem   = vgem   / countv
               uzeta(n,m) = ugem*COS (alfas(n,m)*degrad)&
               &-vgem*SIN (alfas(n,m)*degrad)
               vzeta(n,m) = ugem*SIN (alfas(n,m)*degrad)&
               &+vgem*COS (alfas(n,m)*degrad)
            Endif
200      Continue
210   Continue
   Endif
   return
end
subroutine travec(ivar,ingrp,deffds,datfds,xcor,ycor,u1,v1,&
&guu,gvv,u,v,alfas,kcs,kfu,kfv,&
&kcu,kcv,num_rows,num_columns,num_layers_grid,misval)
   real        XCOR(*),YCOR(*),U1(*),V1(*),misval,&
   &GUU(*),GVV(*),&
   &U(*),V(*),alfas(*)
   INTEGER KCS(*),KFU(*),KFV(*),&
   &kcu(*),kcv(*)
   INTEGER      ERROR
   INTEGER DEFFDS(*),DATFDS(*),ELMDMS(3),&
   &CELIDT(1),USRORD(1),uindex(3,1)
   CHARACTER ELMNAM*16,ELMTYP*10,ELMQTY*16,ELMUNT*16,ELMDES*64,&
   &kor*16
   INTEGER INQELM,GETELt
   Logical      alfa
   integer ivar,ingrp,num_rows,num_columns,num_layers_grid,k,l,i,nvec,nby,idam,nlen,jm,im
   integer koff,nlenv
!
   do 3 k=1,num_rows
      do 3 l=1,num_columns
         i=(l-1)*num_rows+k
         kcs(i)=1
         kfu(i)=1
         kfv(i)=1
         kcu(i)=0
         kcv(i)=0
3  continue
   USRORD(1)=1
   NVEC=0
   IDAM=5
   ELMNAM='XCOR'
   ERROR=INQELM(DEFFDS,ELMNAM,ELMTYP,NBY,ELMQTY,ELMUNT,&
   &ELMDES,IDaM,ELMDMS)
   call nefout(elmnam,error)
   nlen=elmdms(1)*elmdms(2)*NBY
   nlenv=elmdms(1)*elmdms(2)*num_layers_grid*NBY
   KOR='map-const'
   CELIDT(1)=1
   uindex(1,1)=celidt(1)
   uindex(2,1)=celidt(1)
   uindex(3,1)=1
   ERROR=GETELt (DEFFDS,KOR,elmnam,uindex,USRORD,&
   &nlen,xcor    )
   If (error.ne.0) Then
      kor   ='GRID'
      ERROR = GETELt (DEFFDS,KOR,elmnam,uindex,USRORD,&
      &nlen,xcor    )
   Endif
   call nefout(elmnam,error)
   elmnam ='YCOR'
   ERROR  = GETELt (DEFFDS,KOR,elmnam,uindex,USRORD,&
   &nlen,ycor    )
   If (error.ne.0) Then
      kor   ='GRID'
      ERROR = GETELt (DEFFDS,KOR,elmnam,uindex,USRORD,&
      &nlen,ycor    )
   Endif
   call nefout(elmnam,error)
   elmnam ='GUU '
   ERROR  = GETELt (DEFFDS,KOR,elmnam,uindex,USRORD,&
   &nlen,GUU     )
   alfa   = .FALSE.
   If (error.ne.0) Then
      kor   ='GRID'
      ERROR = GETELt (DEFFDS,KOR,elmnam,uindex,USRORD,&
      &nlen,guu     )
      If (error.ne.0) Then
         kor    ='map-const'
         elmnam = 'ALFAS'
         ERROR  = GETELt (DEFFDS,KOR,elmnam,uindex,USRORD,&
         &nlen,alfas   )
         alfa   = .TRUE.
      Endif
   Endif
   call nefout(elmnam,error)
   If (.NOT.alfa) Then
      elmnam ='GVV '
      ERROR  = GETELt (DEFFDS,KOR,elmnam,uindex,USRORD,&
      &nlen,GVV     )
      If (error.ne.0) Then
         kor    ='GRID'
         ERROR  = GETELt (DEFFDS,KOR,elmnam,uindex,USRORD,&
         &nlen,gvv     )
      Endif
      call nefout(elmnam,error)
   Endif
   elmnam ='KCS'
   error  = getelt (deffds,kor,elmnam,uindex,usrord,&
   &nlen,kcs)
   If (error.ne.0) Then
      kor='KENMCNST'
      ERROR=GETELt(DEFFDS,KOR,elmnam,uindex,USRORD,&
      &nlen,kcs     )
   endif
   elmnam='KCU'
   error=getelt(deffds,kor,elmnam,uindex,usrord,&
   &nlen,kcu)
   If (error.ne.0) Then
      kor='KENMCNST'
      ERROR=GETELt(DEFFDS,KOR,elmnam,uindex,USRORD,&
      &nlen,kcu     )
   endif
   elmnam='KCV'
   error=getelt(deffds,kor,elmnam,uindex,usrord,&
   &nlen,kcv)
   If (error.ne.0) Then
      kor='KENMCNST'
      ERROR=GETELt(DEFFDS,KOR,elmnam,uindex,USRORD,&
      &nlen,kcv     )
   endif
   kor='map-series'
   celidt(1)=ingrp
   uindex(1,1)=celidt(1)
   uindex(2,1)=celidt(1)
   uindex(3,1)=1
   kor='map-series'
   nvec=nvec+1
   elmnam='KFU '
   ERROR=GETELt(DEFFDS,KOR,elmnam,uindex,USRORD,&
   &nlen,KFU     )
   If (error.ne.0) Then
      kor='KENMTIM'
      ERROR=GETELt(DEFFDS,KOR,elmnam,uindex,USRORD,&
      &nlen,kfu     )
   endif
   elmnam='KFV '
   ERROR=GETELt(DEFFDS,KOR,elmnam,uindex,USRORD,&
   &nlen,KFV     )
   If (error.ne.0) Then
      kor='KENMTIM'
      ERROR=GETELt(DEFFDS,KOR,elmnam,uindex,USRORD,&
      &nlen,kfv     )
   endif
   elmnam='U1  '
   if(ivar.eq.2)elmnam='FX'
   if(ivar.eq.3)elmnam='SX'
   if(ivar.eq.4)elmnam='STX'
   if(ivar.eq.5)elmnam='TTXI'
   if(ivar.eq.6)elmnam='TTXA'
   if(ivar.eq.7)elmnam='TTXSI'
   if(ivar.eq.8)elmnam='TTXSA'
   if(ivar.eq.9)elmnam='SXS'
   if(ivar.eq.10)elmnam='STXS'
   if(ivar.eq.11)elmnam='MX'
   if(ivar.eq.12)elmnam='QU'
   if(ivar.eq.13)elmnam='TX'
   ERROR=GETELT(DEFFDS,KOR,elmnam,uindex,USRORD,&
   &nlenv,U1      )
   If (error.ne.0) Then
      kor='CURTIM'
      if(ivar.eq.2)kor='WAVTIM'
      if(ivar.gt.2)kor='TRANSTIM'
      if(ivar.eq.11)kor='WAVTIM'
      if(ivar.eq.12)kor='CURTIM'
      if(ivar.eq.13)kor='MAPBOTTIM'
      ERROR=GETELT(DEFFDS,KOR,elmnam,uindex,USRORD,&
      &nlenv,U1      )
      if(ivar.gt.2.and.ivar.le.10)then
         if(error.ne.0)then
            kor='MAPTTRAN'
            ERROR=GETELT(DEFFDS,KOR,ELMNAM,uindex,USRORD,NLENV,U1)
         ENDIF
      ENDIF
      if(ivar.gt.4.and.ivar.le.10)then
         if(error.ne.0)then
            kor='MAPATRAN'
            ERROR=GETELT(DEFFDS,KOR,ELMNAM,uindex,USRORD,NLENV,U1)
         ENDIF
      ENDIF
   endif
   call nefout(elmnam,error)
   elmnam='V1  '
   if(ivar.eq.2)elmnam='FY'
   if(ivar.eq.3)elmnam='SY'
   if(ivar.eq.4)elmnam='STY'
   if(ivar.eq.5)elmnam='TTYI'
   if(ivar.eq.6)elmnam='TTYA'
   if(ivar.eq.7)elmnam='TTYSI'
   if(ivar.eq.8)elmnam='TTYSA'
   if(ivar.eq.9)elmnam='SYS'
   if(ivar.eq.10)elmnam='STYS'
   if(ivar.eq.11)elmnam='MY'
   if(ivar.eq.12)elmnam='QV'
   if(ivar.eq.13)elmnam='TY'
   ERROR=GETELT(DEFFDS,KOR,elmnam,uindex,USRORD,&
   &nlenv,V1      )
   If (error.ne.0) Then
      kor='CURTIM'
      if(ivar.eq.2)kor='WAVTIM'
      if(ivar.GT.2)kor='TRANSTIM'
      if(ivar.eq.11)kor='WAVTIM'
      if(ivar.eq.12)kor='CURTIM'
      if(ivar.eq.13)kor='MAPBOTTIM'
      ERROR=GETELT(DEFFDS,KOR,elmnam,uindex,USRORD,&
      &nlenv,V1      )
      if(ivar.gt.2.and.ivar.le.10)then
         if(error.ne.0)then
            kor='MAPTTRAN'
            ERROR=GETELT(DEFFDS,KOR,ELMNAM,uindex,USRORD,NLENV,V1)
         ENDIF
      ENDIF
      if(ivar.gt.4.and.ivar.le.10)then
         if(error.ne.0)then
            kor='MAPATRAN'
            ERROR=GETELT(DEFFDS,KOR,ELMNAM,uindex,USRORD,NLENV,V1)
         ENDIF
      ENDIF
   endif
   call nefout(elmnam,error)
   jm=num_columns
   im=num_rows
   koff=1
   do 10 k = 1,num_layers_grid
      Call tr2wav (u1(koff), v1(koff), u(koff) , v(koff) , xcor,&
      &ycor,guu, gvv, im  , jm  , kcs, kfu, kfv,ivar,&
      &alfas,alfa,misval)
      koff   = koff   + num_rows   * num_columns
10 continue
   RETURN
END
subroutine tramat( xdata  , rbuffs  , num_rows   , num_columns   )
!
! Subroutine to transpose a matrix stored in the buffer
!
   integer num_rows   , num_columns
   real    xdata(num_columns,num_rows) , rbuffs(num_rows,num_columns)
!
   integer i      , j
!
   do 120 j = 1,num_rows
      do 110 i = 1,num_columns
         xdata(i,j)  = rbuffs(j,i)
110   continue
120 continue
!
   return
end
