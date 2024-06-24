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
!        $Source: /u/cvsroot/gpp/libsrc/ods/phidias.f,v $
!
!#ifdef WINNT
!     INCLUDE '../include/nfsintrf.i'
!
!     INTERFACE TO FUNCTION GETELT_i [ALIAS:'_GETELT']
!    +                             ( VALUE1, VALUE2, VALUE3, VALUE4 ,
!    +                               VALUE5, VALUE6, VALUE7, VALUE8 )
!
!     INTEGER   GETELT_i
!
!     INTEGER   VALUE1
!     INTEGER   VALUE2
!     CHARACTER VALUE3
!     CHARACTER VALUE4
!     INTEGER   VALUE5
!     INTEGER   VALUE6
!     INTEGER   VALUE7
!     CHARACTER VALUE8
!
!     END
!#endif
subroutine phspdim&
!#ifdef WINNT
!    *                 [ALIAS:'_phspdim']
!#endif
&(fname ,itype ,dimtyp, pardep, timdep, locdep,&
&ndim  ,ierror, option                       )
!-----------------------------------------------------------------------
!         Function: dimension selection for maps
!                   PHIDIAS-Spectral  NEFIS  files
!
!-----------------------------------------------------------------------
!   Calling routine :              GETDIM
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
! FILHDA     CH*256                File name NEFIS data file for COM
! FILHDE     CH*256                File name NEFIS definition file for
!                                  COM
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
! L           I*4                  Help variable
! NPAR        I*4                  Number of found parameters
! NRCEL       I*4                  Number of cells defined in group
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
   integer         ierror,itype,ind
   integer                npar
   integer         pardep,timdep,locdep
   integer         ndim   (5    )
!
   character*3     dimtyp
   character       fname(*)*256
   character*256   filhda,filhde
   character*256   option
!
   logical         ex    ,okee
!-----------------------------------------------------------------------
!-----declaration NEFIS
!-----------------------------------------------------------------------
   integer       maxelm
   parameter     ( maxelm = 50 )
   integer       hdefds( 2997),hdafds(  999)
   integer       nrelm, ix, iy, nbytsg, i, elmndm, nloc
   integer       elmdms(5)
   integer       INQCEL, INQELM, INQDAT, INQMXI
   character*8   elmtyp
   character*12  grpnam
   character*16  elmnam(maxelm), elmqty, elmunt, grpdef
   character*64  elmdes
!-----------------------------------------------------------------------
!-----Initialisation
!-----------------------------------------------------------------------
   okee   = .true.
!
   ierror =  0
!
!--------------------------------------------------------------------
!-----Test if .dat and .def Nefis -files
!     exist
!--------------------------------------------------------------------

   ind = index ( fname(1), char(0))
   if ( ind .eq. 0 ) then
      filhda = fname(1)
   else
      filhda = fname(1)(1:ind-1)
   endif
   inquire (file=filhda,exist=ex)
   if (.not.ex) then
      ierror = IENOFI
      go to 900
   endif
!
   ind = index ( fname(2), char(0))
   if ( ind .eq. 0 ) then
      filhde = fname(2)
   else
      filhde = fname(2)(1:ind-1)
   endif
   inquire (file=filhde,exist=ex)
   if (.not.ex) then
      ierror = IENOFI
      go to 900
   endif
!--------------------------------------------------------------------
!-----Open files
!--------------------------------------------------------------------
   call OPNNEF(fname, itype, hdafds, hdefds, ierror)
   if (ierror .ne. 0) then
      ierror = IEFIRO
      go to 900
   endif
!
   if ( dimtyp(1:3) .eq. 'par' ) then
!
!        Read number of parameters
!
      ndim(1) = 1
      npar = 0
!
!        First check the number of elements in the coord group
!
      nrelm = maxelm
      ierror = INQCEL( hdefds, 'spc-coord', nrelm, elmnam )
      if ( ierror .ne. 0 ) then
         ierror = IEINFO
         go to 900
      endif
!
!        Now find out the number of 2-D elements in it
!
      do 100 i = 1,nrelm
         elmndm = 5
         ierror = INQELM( hdefds, elmnam(i), elmtyp, nbytsg,&
         &elmqty, elmunt, elmdes, elmndm,&
         &elmdms )
!
         if ( ierror .ne. 0 ) then
            ierror = IEOTHR
            go to 900
         endif
         if ( elmndm .eq. 2 ) npar = npar + 1
100   continue
!
!        Check the number of spectral variables on the file
!        First check the number of elements in the group spc-series
!
      nrelm = maxelm
      ierror = INQCEL( hdefds, 'spc-step', nrelm, elmnam )
      if ( ierror .ne. 0 ) then
         ierror = IEINFO
         go to 900
      endif
!
!        Now find out the number of 2-D elements in it
!
      do 150 i = 1,nrelm
         elmndm = 5
         ierror = INQELM( hdefds, elmnam(i), elmtyp, nbytsg,&
         &elmqty, elmunt, elmdes, elmndm,&
         &elmdms )
!
         if ( ierror .ne. 0 ) then
            ierror = IEOTHR
            go to 900
         endif
         if ( elmndm .eq. 2 ) npar = npar + 1
150   continue
!
!        Now we found the total number of 2-D parameters
!
      ndim(2) = npar
   endif
!
   if ( dimtyp(1:3) .eq. 'loc' ) then
!
!        Read number of locations
!
      ndim(1) = 4
      nloc = 0
!
!        First find out the number of locations
!
      i = 0
      grpnam = 'spc-coord-'
200   i = i+1
      if ( i .lt. 10 ) then
         grpnam(11:11) = '0'
         write( grpnam(12:12), '(i1)' ) i
      else
         if ( i .lt. 100 ) then
            write( grpnam(11:12), '(i2)' ) i
         else
            write( grpnam(11:12), '(i3)' ) i
         endif
      endif

      ierror = INQDAT( hdafds, grpnam, grpdef )
!
      if ( ierror .eq. IEOK ) then
         nloc = nloc + 1
         go to 200
      endif
!
      ndim(2) = nloc
!
!        Now get the dimensions of the 2-D grid
!
      nrelm = maxelm
      ierror = INQCEL( hdefds, 'spc-coord', nrelm, elmnam )
      if ( ierror .ne. 0 ) then
         ierror = IEINFO
         go to 900
      endif
!
!        Now find out the number of 2-D elements in it
!
      ix = 0
      iy = 0
      do 220 i = 1,nrelm
         elmndm = 5
         ierror = INQELM( hdefds, elmnam(i), elmtyp, nbytsg,&
         &elmqty, elmunt, elmdes, elmndm,&
         &elmdms )
!
         if ( ierror .ne. 0 ) then
            ierror = IEOTHR
            go to 900
         endif
         if ( elmndm .eq. 2 ) then
            ix = max( ix, elmdms(1))
            iy = max( iy, elmdms(2))
         endif
220   continue
!
      ndim(3) = ix+1
      ndim(4) = iy
      ndim(5) = (ix+1)* iy
      ndim(2) = iy
      ndim(3) = ix+1
      ndim(4) = nloc
   endif

   if ( dimtyp(1:3) .eq. 'tim' ) then
!
!        Read number of times
!
      ndim(1) = 1
!
      ierror = INQMXI( hdefds, 'spc-series-01', ndim(2) )

      if ( ierror .ne. 0 ) then
         ierror = IEINFO
         go to 900
      endif
   endif

!--------------------------------------------------------------------
!-----Close files
!--------------------------------------------------------------------
900 call CLOSFL(fname, ierror)
!
   return
!-----------------------------------------------------------------------
end


subroutine phsppar&
!#ifdef WINNT
!    *                 [ALIAS:'_phsppar']
!#endif
&(fname , itype , pardef, maxdef, timdep, locdep,&
&maxlst, lang  , parlst, paruni, partyp, parcod,&
&nrlst , ierror, option                        )
!-----------------------------------------------------------------------
!         Function: parameter name selection for maps
!                   PHIDIAS  NEFIS  files
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
! FILHDA     CH*256                File name NEFIS data file for COM
! FILHDE     CH*256                File name NEFIS definition file for
!                                  COM
! HDAFDS      I*4  999             Data file descriptor for the COM-DAT
!                                  file
! HDEFDS      I*4  2997            Definition file description for the
!                                  COM-DEF file
! IERROR      I*4                  Error code for NEFIS error
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
   integer         lang
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
   character*256   filhda,filhde
   character*256   option
!
   logical         ex
!-----------------------------------------------------------------------
!-----declaration NEFIS
!-----------------------------------------------------------------------
!
   integer       maxelm
   parameter     ( maxelm = 50 )
   integer       hdefds( 2997),hdafds(  999)
   integer       nrelm, nbytsg, i, elmndm
   integer       elmdms(5)
   integer       INQCEL, INQELM
   character*8   elmtyp
   character*16  elmnam(maxelm), elmqty, elmunt
   character*64  elmdes

   integer       TMLCDP,TMLCDH
   parameter     ( TMLCDP = IPLMNK + IPTDEP,TMLCDH=IPLDEP+IPLLST )
!
!-----------------------------------------------------------------------
!-----Initialisation
!-----------------------------------------------------------------------
!
   ierror =  0
!
!--------------------------------------------------------------------
!-----Test if .dat and .def Nefis files
!     exist
!--------------------------------------------------------------------
   ind = index ( fname(1), char(0))
   if ( ind .eq. 0 ) then
      filhda = fname(1)
   else
      filhda = fname(1)(1:ind-1)
   endif
   inquire (file=filhda,exist=ex)
   if (.not.ex) then
      ierror = IENOFI
      return
   endif
!
   ind = index ( fname(2), char(0))
   if ( ind .eq. 0 ) then
      filhde = fname(2)
   else
      filhde = fname(2)(1:ind-1)
   endif
   inquire (file=filhde,exist=ex)
   if (.not.ex) then
      ierror = IENOFI
      return
   endif
!--------------------------------------------------------------------
!-----Open .dat and .def files
!--------------------------------------------------------------------
   call OPNNEF(fname, itype, hdafds, hdefds, ierror)
   if (ierror .ne. 0) then
      ierror = IEFIRO
      return
   endif

   if ( pardef(1)(1:1) .eq. '*' ) then
!
!        Get the parameter names
!
      npar = 0
!
!        First get the element names of the spc-coord group
!
      nrelm = maxelm
      ierror = INQCEL( hdefds, 'spc-coord', nrelm, elmnam )
      if ( ierror .ne. 0 ) then
         ierror = IEINFO
         go to 900
      endif
!
!        Now select those elements which are 2-D
!
      do 100 i = 1,nrelm
         elmndm = 5
         ierror = INQELM( hdefds, elmnam(i), elmtyp, nbytsg,&
         &elmqty, elmunt, elmdes, elmndm,&
         &elmdms )
!
         if ( ierror .ne. 0 ) then
            ierror = IEOTHR
            go to 900
         endif
         if ( elmndm .eq. 2 ) then
            npar = npar + 1
            parlst( npar) = elmnam(i)
            partyp( npar) = IPLMNK
            parcod( npar) = i-1
         endif
100   continue
!
!        Now add the names of the spectral variables
!        First check the number of elements in the group spc-series
!
      nrelm = maxelm
      ierror = INQCEL( hdefds, 'spc-step', nrelm, elmnam )
      if ( ierror .ne. 0 ) then
         ierror = IEINFO
         go to 900
      endif
!
!        Now find out the number of 2-D elements in it
!
      do 150 i = 1,nrelm
         elmndm = 5
         ierror = INQELM( hdefds, elmnam(i), elmtyp, nbytsg,&
         &elmqty, elmunt, elmdes, elmndm,&
         &elmdms )
!
         if ( ierror .ne. 0 ) then
            ierror = IEOTHR
            go to 900
         endif
         if ( elmndm .eq. 2 ) then
            npar = npar + 1
            parlst( npar) = elmnam(i)
            partyp( npar) = TMLCDP
            parcod( npar) = 1000+i-1
         endif
150   continue
   endif
   nrlst = npar
!--------------------------------------------------------------------
!-----Close files
!--------------------------------------------------------------------
900 call CLOSFL(fname, ierror)
!
   return
!-----------------------------------------------------------------------
end

subroutine phsploc&
!#ifdef WINNT
!    *                 [ALIAS:'_phsploc']
!#endif
&(fname , itype , locdef, maxdef, pardep, timdep,&
&loclst, loctyp, locnr , maxlst, nrlst , ierror,&
&option )
!-----------------------------------------------------------------------
!         Function: location name selection for maps
!                   PHIDIAS Spectral NEFIS  files
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
! LOCDEF     CH*21  maxdef     I   filter for required locations
! MAXDEF      I*4              I   number of filters in LOCDEF
! TIMDEP      I*4              I   dependency of time for the locations to get
! PARDEP      I*4              I   dependency of parameter for the locations
!                                  to get
! LOCLST     CH*21             O   location names
! LOCTYP      I*4              O
! LOCNR       I*4              O
! MAXLST      I*4              I   maximum number of locations possible
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
! FILHDA     CH*256                File name NEFIS data file for COM
! FILHDE     CH*256                File name NEFIS definition file for
!                                  COM
! HDAFDS      I*4  999             Data file descriptor for the COM-DAT
!                                  file
! HDEFDS      I*4  2997            Definition file description for the
!                                  COM-DEF file
! IERROR      I*4                  Error code for NEFIS error
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
   integer         pardep
   integer         timdep,itype
   integer         maxdef,maxlst,       nloc
   integer                              ind
   integer         ierror,nrlst
!
   integer         loctyp(maxlst)
   integer         locnr(maxlst)
!
   character       locdef(*)*21
   character       loclst(*)*21
   character       fname(*)*256
   character*256   filhda,filhde
   character*256   option
!
   logical         ex
!-----------------------------------------------------------------------
!-----declaration NEFIS
!-----------------------------------------------------------------------
   integer       hdefds( 2997),hdafds(  999)
   integer       i
   integer       uindex(3), usrord(1)
   integer       GETELT, GETELS, INQDAT
   real          xp, yp
   character*16  grpnam, grpdef

!
!-----------------------------------------------------------------------
!-----Initialisation
!-----------------------------------------------------------------------
!
   ierror =  0
!
!--------------------------------------------------------------------
!-----Test if .dat and .def Nefis files
!     exist
!--------------------------------------------------------------------
   ind = index ( fname(1), char(0))
   if ( ind .eq. 0 ) then
      filhda = fname(1)
   else
      filhda = fname(1)(1:ind-1)
   endif
   inquire (file=filhda,exist=ex)
   if (.not.ex) then
      ierror = IENOFI
      return
   endif
!
   ind = index ( fname(2), char(0))
   if ( ind .eq. 0 ) then
      filhde = fname(2)
   else
      filhde = fname(2)(1:ind-1)
   endif
   inquire (file=filhde,exist=ex)
   if (.not.ex) then
      ierror = IENOFI
      return
   endif
!--------------------------------------------------------------------
!-----Open .dat and .def files
!--------------------------------------------------------------------
   call OPNNEF(fname, itype, hdafds, hdefds, ierror)
   if (ierror .ne. 0) then
      ierror = IEFIRO
      return
   endif

   uindex(1) = 1
   uindex(2) = 1
   uindex(3) = 1
   usrord(1) = 1

   nrlst = 0

   if ( locdef(1)(1:1) .eq. '*' ) then
!
!        Get the location names
!
      nloc = 0
!
      i = 0
      grpnam = 'spc-coord-'
200   i = i+1
      if ( i .lt. 10 ) then
         grpnam(11:11) = '0'
         write( grpnam(12:12), '(i1)' ) i
      else
         write( grpnam(11:12), '(i2)' ) i
      endif
!
      ierror = INQDAT( hdafds, grpnam, grpdef )
!
      if ( ierror .eq. IEOK ) then
         nloc = nloc + 1
!
         ierror = GETELT( hdefds, grpnam, 'XS', uindex,&
         &usrord, 4     , xp    )
         ierror = GETELT( hdefds, grpnam, 'YS', uindex,&
         &usrord, 4     , yp    )

         if ( ierror .ne. 0 ) then
            ierror = IEOTHR
            go to 900
         endif

         if ( maxlst .lt. nloc ) then
            ierror = IEPMNY
            go to 900
         endif
         write( loclst(nloc),'(''X='',F7.0,'' Y='',F7.0)') xp, yp
         loctyp(nloc) = 0
         locnr( nloc) = 0

         go to 200
      endif
   endif
   nrlst = nloc
!
!--------------------------------------------------------------------
!-----Close files
!--------------------------------------------------------------------
900 call CLOSFL(fname, ierror)
!
   return
!-----------------------------------------------------------------------
end
subroutine             phsptme&
!#ifdef WINNT
!    *                 [ALIAS:'_phsptme']
!#endif
&(fname  ,itype  ,timdef, maxdef ,pardep , locdep,&
&maxlst ,        timlst,         timtyp ,&
&nrlst  ,ierror ,option                         )
!-----------------------------------------------------------------------
!           Function: time selection for maps
!                     PHIDIAS Spectral NEFIS  file
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
! FILHDA     CH*256                File name NEFIS data file for COM
! FILHDE     CH*256                File name NEFIS definition file for
!                                  COM
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
! KMAX        I*4                  Number of layers
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
   integer         itdate,ind
   integer         ierror,nrlst ,maxdef,maxlst,itype
   integer         julday
   integer         imo1
   integer         iy    ,imo   ,iday
   integer         pardep,locdep
   integer         timtyp(maxlst)
!
   real*8           timlst(maxlst)
   real*8           timdef(maxdef,2)
   real             dt    ,tunit
!
   character       fname(*)*256
   character*256   filhda,filhde
   character*256   option
!
   logical         ex
!-----------------------------------------------------------------------
!-----declaration NEFIS
!-----------------------------------------------------------------------
   integer       maxelm
   parameter     ( maxelm = 50 )
   integer       hdefds( 2997),hdafds(  999)
   integer       i, idp, itp, n
   integer       itmodc, icurtm, ihou, imin, isec, l
   integer       GETELT, GETELS, INQMXI
!#ifdef WINNT
!     integer       GETELT_i
!#endif
   real          tmodc
   character*12  tnul
   character*16  elmnam(maxelm), grpnam
!
   integer&
   &uindex(3,5  ),usrord(5),buflen
!
!
!-----------------------------------------------------------------------
!-----Initialisation
!-----------------------------------------------------------------------
!
   ierror =  0
!
!--------------------------------------------------------------------
!-----Test if .dat and .def Nefis files
!     exist
!--------------------------------------------------------------------
   ind = index ( fname(1), char(0))
   if ( ind .eq. 0 ) then
      filhda = fname(1)
   else
      filhda = fname(1)(1:ind-1)
   endif
   inquire (file=filhda,exist=ex)
   if (.not.ex) then
      ierror = IENOFI
      return
   endif
!
   ind = index ( fname(2), char(0))
   if ( ind .eq. 0 ) then
      filhde = fname(2)
   else
      filhde = fname(2)(1:ind-1)
   endif
   inquire (file=filhde,exist=ex)
   if (.not.ex) then
      ierror = IENOFI
      return
   endif
!--------------------------------------------------------------------
!-----Open .dat and .def files
!--------------------------------------------------------------------
   call OPNNEF(fname, itype, hdafds, hdefds, ierror )
   if (ierror .ne. 0) then
      ierror = IEFIRO
      return
   endif
!
!     Read number of times available at the NEFIS fils
!
   ierror = INQMXI( hdefds, 'spc-series-01', nrlst )

   if ( ierror .ne. 0 ) then
      ierror = IEINFO
      go to 900
   endif

!--------------------------------------------------------------------
!-----Read constants from Nefis files
!--------------------------------------------------------------------
   grpnam    = 'spc-info-series'
   uindex(1,1) = 1
   uindex(2,1) = 1
   uindex(3,1) = 1
   usrord(1) = 1

   buflen    =12
   elmnam(1) = 'tijd-spcc'
!#ifdef WINNT
!     ierror = GETELT_i
!#else
   ierror = GETELS&
!#endif
   &(hdefds   ,grpnam    ,elmnam(1) ,&
   &uindex   ,usrord    ,buflen    ,tnul      )

   read(tnul(1:6),'(i6)')itdate
   buflen    = 4
   tunit=1.
   dt=1.
!-----------------------------------------------------------------------
!-----Convert ITDATE to julian day number JULDAY
!-----------------------------------------------------------------------
   iday   = mod (itdate ,   100)
   imo    = mod (itdate , 10000) /  100
   iy     = 1900 + itdate / 10000
   imo1   = (imo -14)/12
   julday = iday - 32075 + 1461 * (iy+4800+imo1  )/ 4&
   &+ 367  * (imo  - 2    - imo1  *  12    )/12&
   &- 3    * ((iy  + 4900 + imo1  )/100    )/ 4

!--------------------------------------------------------------------
!-----Read array-timlst from Nefis files
!--------------------------------------------------------------------

   do 200 i = 1,nrlst
      if ( i .gt. maxlst ) then
         ierror = IEPMNY
         go to 900
      endif
      uindex(1,1) = i
      uindex(2,1) = i
      uindex(3,1) = 1
      itmodc=-1
      tmodc=-1.
      grpnam='spc-info-series'
      elmnam(1)='tijd-spcr'
      ierror=GETELT(hdefds,grpnam,&
      &elmnam(1),uindex,usrord, 4,tmodc)
      itmodc=tmodc*3600.
      if (ierror .ne. 0 ) then
!-----------------------------------------------------------------------
!-----------In case an error occured while reading the file then
!           write part which is ok to nhulp = i - 1
!-----------------------------------------------------------------------
         ierror = IEOTHR
         goto 900
      endif
!-----------------------------------------------------------------------
!--------Calculate current time elapsed in seconds referenced to ITDATE
!-----------------------------------------------------------------------
      icurtm = nint  (itmodc * dt * tunit)
      iday   = icurtm / 86400
      icurtm = icurtm - iday  * 86400
      ihou   = icurtm / 3600
      icurtm = icurtm - ihou  * 3600
      imin   = icurtm / 60
      icurtm = icurtm - imin  * 60
      isec   = icurtm
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
200 continue

!
!--------------------------------------------------------------------
!-----Close files
!--------------------------------------------------------------------
900 call CLOSFL(fname, ierror)
!
   return
!-----------------------------------------------------------------------
end

subroutine             phspmat&
!#ifdef WINNT
!    *                 [ALIAS:'_phspmat']
!#endif
&(fname ,itype  ,parcod, loc   , tim   ,misval,&
&i3gl  ,maxdim ,xdata , ierror, option,&
&ibuffs,rbuffs                               )
!-----------------------------------------------------------------------
!           Function: select map data out of PHIDIAS MAP NEFIS files
!        Method used:
!
!-----------------------------------------------------------------------
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
! FILHDA     CH*256                File name NEFIS data file for COM
! FILHDE     CH*256                File name NEFIS definition file for
!                                  COM
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
   integer         i3gl,iday,itdate
   integer         ibuffs (*     )

   real            misval,dt,tunit
   real            xdata (maxdim)
   real            rbuffs (*     )

   character       fname (*)*256,tnul*12
   character*256   option
!-----------------------------------------------------------------------
!-----declaration Local variables
!-----------------------------------------------------------------------
   character*256   filhda,filhde
!
   integer         ind ,ierror
   integer         imo,iy,imo1,julday
!
   logical         ex
!
!-----------------------------------------------------------------------
!-----declaration NEFIS
!-----------------------------------------------------------------------
   integer       maxelm
   parameter     ( maxelm = 50 )
   integer       hdefds( 2997),hdafds(  999)
   integer       nrelm, i, idp, itp, n, m, nmax, mmax
   integer       elmdms(5), nbytsg, elmndm, n1, n2, ipar
   integer       nrlst
   integer       itmodc, icurtm, ihou, imin, isec, l
   integer       GETELT, GETELS, INQMXI, INQCEL, INQELM
!#ifdef WINNT
!     integer       GETELT_i
!#endif
   real          tmodc
   real*8        timlev
   character*8   elmtyp
   character*16  elmnam(maxelm), grpnam, grpdef, elmnaa
   character*16  elmqty, elmunt, elmtim
   character*64  elmdes

   integer       uindex(3,1),usrord(3),buflen
!-----------------------------------------------------------------------
!-----Initialisation
!-----------------------------------------------------------------------
!
   ierror =  0
!--------------------------------------------------------------------
!-----Test if .dat and .def Nefis files
!     exist
!--------------------------------------------------------------------
   ind = index ( fname(1), char(0))
   if ( ind .eq. 0 ) then
      filhda = fname(1)
   else
      filhda = fname(1)(1:ind-1)
   endif
   inquire (file=filhda,exist=ex)
   if (.not.ex) then
      ierror = IENOFI
      return
   endif
!
   ind = index ( fname(2), char(0))
   if ( ind .eq. 0 ) then
      filhde = fname(2)
   else
      filhde = fname(2)(1:ind-1)
   endif
   inquire (file=filhde,exist=ex)
   if (.not.ex) then
      ierror = IENOFI
      return
   endif
!--------------------------------------------------------------------
!-----Open .dat and .def COM-files
!--------------------------------------------------------------------
   call OPNNEF(fname, itype, hdafds, hdefds, ierror)
   if (ierror .ne. 0) then
      ierror = IEFIRO
      return
   endif
!
!--------------------------------------------------------------------
!-----Read constants from Nefis files
!--------------------------------------------------------------------
   grpdef    = 'spc-info-series'
   uindex(1,1) = 1
   uindex(2,1) = 1
   uindex(3,1) = 1
   usrord(1) = 1

   buflen    =12 * 4
   elmnaa    = 'tijd-spcc'
!#ifdef WINNT
!     ierror = GETELT_i
!#else
   ierror = GETELS&
!#endif
   &(hdefds   ,grpdef    ,elmnaa    ,&
   &uindex   ,usrord    ,buflen    ,tnul      )
   read(tnul(1:6),'(i6)')itdate
   buflen    = 4
   tunit=1.
   dt=1.
!-----------------------------------------------------------------------
!-----Convert ITDATE to julian day number JULDAY
!-----------------------------------------------------------------------
   iday   = mod (itdate ,   100)
   imo    = mod (itdate , 10000) /  100
   iy     = 1900 + itdate / 10000
   imo1   = (imo -14)/12
   julday = iday - 32075 + 1461 * (iy+4800+imo1  )/ 4&
   &+ 367  * (imo  - 2    - imo1  *  12    )/12&
   &- 3    * ((iy  + 4900 + imo1  )/100    )/ 4
!-----------------------------------------------------------------------
!
   if ( parcod .lt. 1000 ) then
      nrelm = maxelm
      grpnam = 'spc-coord'
      ierror = INQCEL( hdefds, grpnam, nrelm, elmnam )
      if ( ierror .ne. 0 ) then
         ierror = IEINFO
         go to 900
      endif

      elmndm = 5
      ierror = INQELM( hdefds, elmnam(parcod+1), elmtyp, nbytsg,&
      &elmqty, elmunt, elmdes, elmndm, elmdms )
      if ( ierror .ne. 0 ) then
         ierror = IEOTHR
         go to 900
      endif
      nmax = elmdms(1)
      mmax = elmdms(2)
      buflen = 4 * nmax *mmax
      if ( nmax * mmax .gt. maxdim ) then
         ierror = IEPMNY
         go to 900
      endif
      grpnam = 'spc-coord-01'
      ierror = GETELT( hdefds, grpnam, elmnam(parcod+1),&
      &uindex, usrord, buflen, rbuffs )
      if ( ierror .ne. 0 ) then
         ierror = IEOTHR
         go to 900
      endif
      do 100 n = 1,nmax
         do 100 m = 1,mmax
            n1 = (n-1) * mmax + m
            n2 = (m-1) * nmax + n
            xdata(n1) = rbuffs(n2)
100   continue
!
!        Add values for last directional sector
!
      do 150 m = 1,mmax
         n1 = nmax * mmax + m
         if ( elmnam(parcod+1) .eq. '2D-directions' ) then
            xdata(n1) = 360.0
         else
            xdata(n1) = xdata(m)
         endif
150   continue
   else
      ipar = parcod - 1000 + 1
      nrelm = maxelm
      grpnam = 'spc-step'
      ierror = INQCEL( hdefds, grpnam, nrelm, elmnam )
      if ( ierror .ne. 0 ) then
         ierror = IEINFO
         go to 900
      endif
!
      elmndm = 5
      ierror = INQELM( hdefds, elmnam(ipar), elmtyp, nbytsg,&
      &elmqty, elmunt, elmdes, elmndm, elmdms )
      if ( ierror .ne. 0 ) then
         ierror = IEOTHR
         go to 900
      endif
      nmax = elmdms(1)
      mmax = elmdms(2)
      buflen = 4 * nmax *mmax
      if ( nmax * mmax .gt. maxdim ) then
         ierror = IEPMNY
         go to 900
      endif

!
!        Read number of times available at the NEFIS fils
!
      ierror = INQMXI( hdefds, 'spc-series-01', nrlst )

      if ( ierror .ne. 0 ) then
         ierror = IEINFO
         go to 900
      endif
!--------------------------------------------------------------------
!-----Define correct group w.r.t. location wanted
!--------------------------------------------------------------------
      grpdef = 'spc-series-'
      if ( loc(2,3) .lt. 10 ) then
         grpdef(12:12) = '0'
         write( grpdef(13:13), '(i1)' ) loc(2,3)
      else
         if ( loc(3,1) .lt. 10 ) then
            write( grpdef(12:13), '(i2)' ) loc(2,3)
         else
            write( grpdef(12:14), '(i3)' ) loc(2,3)
         endif
      endif
!--------------------------------------------------------------------
!-----Loop for all available times
!--------------------------------------------------------------------
      ind = 0
      do 300 i = 1,nrlst
         uindex(1,1) = i
         uindex(2,1) = i
         uindex(3,1) = 1
         itmodc=-1
         tmodc=-1.
         grpnam='spc-info-series'
         elmtim='tijd-spcr'
         ierror=GETELT(hdefds,grpnam,&
         &elmtim,uindex,usrord, 4,tmodc)
         itmodc=tmodc*3600.
         if (ierror .ne. 0 ) then
!-----------------------------------------------------------------------
!-----------In case an error occured while reading the file then
!           write part which is ok to nhulp = i - 1
!-----------------------------------------------------------------------
            ierror = IEOTHR
            goto 900
         endif
!-----------------------------------------------------------------------
!--------Calculate current time elapsed in seconds referenced to ITDATE
!-----------------------------------------------------------------------
         icurtm = nint  (itmodc * dt * tunit)
         iday   = icurtm / 86400
         icurtm = icurtm - iday  * 86400
         ihou   = icurtm / 3600
         icurtm = icurtm - ihou  * 3600
         imin   = icurtm / 60
         icurtm = icurtm - imin  * 60
         isec   = icurtm
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
         timlev    = dble(idp) + dble(itp) / 86400d0
!
!--------------------------------------------------------------------------
!--------Check if timelevel is inside asked interval
!--------------------------------------------------------------------------
         if ( timlev .ge. tim(1) .and. timlev .le. tim(2) ) then
            uindex(1,1) = i
            uindex(2,1) = i
            uindex(3,1) = 1

            if ( (ind + nmax * mmax) .gt. maxdim ) then
               ierror = IEPMNY
               go to 900
            endif
!
            ierror = GETELT( hdefds, grpdef, elmnam(ipar),&
            &uindex, usrord, buflen, rbuffs )
            if (ierror .ne. 0 ) then
               ierror = IEOTHR
               goto 900
            endif
            do 200 n = 1,nmax
               do 200 m = 1,mmax
                  n1 = (n-1) * mmax + m
                  n2 = (m-1) * nmax + n
                  xdata(ind+n1) = rbuffs(n2)
                  if ( xdata(ind+n1) .lt. 1.e-3 )&
                  &xdata(ind+n1) = misval
200         continue
!
!              Add values for last directional sector
!
            do 250 m = 1,mmax
               n1 = nmax * mmax + m
               xdata(n1) = xdata(m)
250         continue
            ind = ind + (nmax + 1) * mmax
!
!              For the moment: just read one dataset
!
            go to 900
         endif
300   continue
   endif
!
!--------------------------------------------------------------------
!-----Close files
!--------------------------------------------------------------------
900 call CLOSFL(fname, ierror)
!
   return
!-----------------------------------------------------------------------
end

subroutine phi_dim&
!#ifdef WINNT
!    *                 [ALIAS:'_phi_dim']
!#endif
&(fname ,itype ,dimtyp, pardep, timdep, locdep,&
&ndim  ,ierror, option                       )
!-----------------------------------------------------------------------
!         Function: dimension selection for maps
!                   PHIDIAS NEFIS  files
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
! FILHDA     CH*256                File name NEFIS data file for COM
! FILHDE     CH*256                File name NEFIS definition file for
!                                  COM
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
! L           I*4                  Help variable
! NPAR        I*4                  Number of found parameters
! NRCEL       I*4                  Number of cells defined in group
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
   integer         ierror,itype,ind
   integer                npar
   integer         mmax  ,nmax
   integer         pardep,timdep,locdep
   integer         ndim   (4    )
!
   character*3     dimtyp
   character       fname(*)*256
   character*256   filhda,filhde
   character*256   option
!
   logical         ex    ,okee
!-----------------------------------------------------------------------
!-----declaration NEFIS
!-----------------------------------------------------------------------
   integer maxgrp,maxelm
   parameter(maxgrp=50,maxelm=50)
   character grpnam(maxgrp)*60,grpdef(maxgrp)*16,&
   &cel(maxgrp)*16,elmnam(maxgrp,maxelm)*16,&
   &elmtyp(maxgrp,maxelm)*16,elmqty(maxgrp,maxelm)*16,&
   &elmunt(maxgrp,maxelm)*16,elmdes(maxgrp,maxelm)*64
   integer grpndm(maxgrp),&
   &grpdms(5,maxgrp),elmdms(5,maxgrp,maxelm),&
   &elmndm(maxgrp,maxelm)
   integer       hdefds( 2997),hdafds(  999)
   integer maplast,hislast,nloc
!
!

!pvb  write(*,*)' call phi_dim'
!-----------------------------------------------------------------------
!-----Initialisation
!-----------------------------------------------------------------------
   okee   = .true.
!
   ierror =  0
!
!--------------------------------------------------------------------
!-----Test if .dat and .def Nefis -files
!     exist
!--------------------------------------------------------------------

   ind = index ( fname(1), char(0))
   if ( ind .eq. 0 ) then
      filhda = fname(1)
   else
      filhda = fname(1)(1:ind-1)
   endif
   inquire (file=filhda,exist=ex)
   if (.not.ex) then
      ierror = IENOFI
      return
   endif
!
   ind = index ( fname(2), char(0))
   if ( ind .eq. 0 ) then
      filhde = fname(2)
   else
      filhde = fname(2)(1:ind-1)
   endif
   inquire (file=filhde,exist=ex)
   if (.not.ex) then
      ierror = IENOFI
      return
   endif
!--------------------------------------------------------------------
!-----Open files
!--------------------------------------------------------------------
   call OPNNEF(fname, itype, hdafds, hdefds, ierror)
   if (ierror .ne. 0) then
      ierror = IEFIRO
      return
   endif
!--------------------------------------------------------------------
!-----Read array-dimensions from Nefis files
!--------------------------------------------------------------------
   maplast=0
   nmax=0
   mmax=0
   call phidim(hdafds,hdefds,okee,npar,nmax,mmax,maplast,itype,&
   &nloc,hislast,&
   &grpnam,grpdef,cel,elmnam,elmtyp,elmqty,&
   &elmunt,elmdes,&
   &grpndm,grpdms,elmdms,elmndm,maxgrp,maxelm)
!-----------------------------------------------------------------------
!-----return required dimension
!-----------------------------------------------------------------------
   if(itype.eq.5051)then
!
!  map file
!
      if (dimtyp(1:3) .eq. 'par') then
         ndim (1) = 1
         ndim (2) = npar
      else if (dimtyp(1:3) .eq. 'tim') then
         ndim (1) = 1
         ndim (2) = maplast
      else if (dimtyp(1:3) .eq. 'loc') then
         ndim (1) = 2
         ndim (2) = mmax
         ndim (3) = nmax
      else
         okee = .false.
      endif
   endif
   if(itype.eq.5050)then
!
!  history file
!
      if (dimtyp(1:3) .eq. 'par') then
         ndim (1) = 1
         ndim (2) = npar
      else if (dimtyp(1:3) .eq. 'tim') then
         ndim (1) = 1
         ndim (2) = hislast
      else if (dimtyp(1:3) .eq. 'loc') then
         ndim (1) = 1
         ndim (2) = nloc
      else
         okee = .false.
      endif
   endif
!--------------------------------------------------------------------
!-----Close files
!--------------------------------------------------------------------
   call CLOSFL(fname, ierror)
   okee = okee .and. (ierror .eq. 0)
   okee=.true.
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

subroutine phi_par&
!#ifdef WINNT
!    *                 [ALIAS:'_phi_par']
!#endif
&(fname , itype , pardef, maxdef, timdep, locdep,&
&maxlst, lang  , parlst, paruni, partyp, parcod,&
&nrlst , ierror, option                        )
!-----------------------------------------------------------------------
!         Function: parameter name selection for maps
!                   PHIDIAS  NEFIS  files
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
! FILHDA     CH*256                File name NEFIS data file for COM
! FILHDE     CH*256                File name NEFIS definition file for
!                                  COM
! HDAFDS      I*4  999             Data file descriptor for the COM-DAT
!                                  file
! HDEFDS      I*4  2997            Definition file description for the
!                                  COM-DEF file
! IERROR      I*4                  Error code for NEFIS error
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
   integer         lang
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
   character*256   filhda,filhde
   character*256   option
!
   logical         ex           ,okee
!-----------------------------------------------------------------------
!-----declaration NEFIS
!-----------------------------------------------------------------------
   integer maxgrp,maxelm
   parameter(maxgrp=50,maxelm=50)
   character grpnam(maxgrp)*60,grpdef(maxgrp)*16,&
   &cel(maxgrp)*16,elmnam(maxgrp,maxelm)*16,&
   &elmtyp(maxgrp,maxelm)*16,elmqty(maxgrp,maxelm)*16,&
   &elmunt(maxgrp,maxelm)*16,elmdes(maxgrp,maxelm)*64
   integer grpndm(maxgrp),&
   &grpdms(5,maxgrp),elmdms(5,maxgrp,maxelm),&
   &elmndm(maxgrp,maxelm)
   integer       hdefds( 2997),hdafds(  999)
!
!


!

   integer       TMLCDP,TMLCDH
   parameter     ( TMLCDP = IPLMNK + IPTDEP,TMLCDH=IPLDEP+IPLLST )
!
!-----------------------------------------------------------------------
!-----Initialisation
!-----------------------------------------------------------------------
!pvb  write(*,*)' call phi_par'
   okee   = .true.
!
   ierror =  0
!
!--------------------------------------------------------------------
!-----Test if .dat and .def Nefis files
!     exist
!--------------------------------------------------------------------
   ind = index ( fname(1), char(0))
   if ( ind .eq. 0 ) then
      filhda = fname(1)
   else
      filhda = fname(1)(1:ind-1)
   endif
   inquire (file=filhda,exist=ex)
   if (.not.ex) then
      ierror = IENOFI
      return
   endif
!
   ind = index ( fname(2), char(0))
   if ( ind .eq. 0 ) then
      filhde = fname(2)
   else
      filhde = fname(2)(1:ind-1)
   endif
   inquire (file=filhde,exist=ex)
   if (.not.ex) then
      ierror = IENOFI
      return
   endif
!--------------------------------------------------------------------
!-----Open .dat and .def files
!--------------------------------------------------------------------
   call OPNNEF(fname, itype, hdafds, hdefds, ierror)
   if (ierror .ne. 0) then
      ierror = IEFIRO
      return
   endif
!--------------------------------------------------------------------
!-----Read array-dimensions from Nefis COM-files group 2
!--------------------------------------------------------------------
   call phipar(hdafds,hdefds,okee,tmlcdp,iplmnk,tmlcdh,npar,&
   &grpnam,grpdef,cel,elmnam,elmtyp,elmqty,&
   &elmunt,elmdes,itype,&
   &grpndm,grpdms,elmdms,elmndm,maxgrp,maxelm,&
   &partyp,parcod,parlst,paruni)
   nrlst=npar

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
subroutine phi_loc&
!#ifdef WINNT
!    *                 [ALIAS:'_phi_loc']
!#endif
&(fname  ,itype  ,locdef ,maxdef ,pardep ,timdep ,&
&maxlst ,        loclst ,        loctyp ,nrlst  ,&
&locnr  ,ierror ,zbuffs ,option                 )
!-----------------------------------------------------------------------
!           Function: parameter name selection for time histories
!                     PHIDIAS NEFIS files
!        Method used:
!
!-----------------------------------------------------------------------
!   Calling routine :              GETLOC
!-----------------------------------------------------------------------
!   Called  routines:              OPNNEF
!                                  CLOSFL
!-----------------------------------------------------------------------
!    Parameters:
!    -----------
!
!   Var.      Type Dimensions
!   -------------------------
!
! FNAME      CH*256 3         I    full name including path and ext.
! ITYPE       I*4             I    file type
! LOCDEF     CH*21  maxdef    I    filter for required parameter locs.
! MAXDEF      I*4             I    number of filters in PARDEF
! PARDEP      I*4             I    access index of parameter location
! TIMDEP      I*4             I    access index of parameter time
! MAXLST      I*4             I    maximum number of parameters possible
! LOCLST     CH*21  nrlst     O    names of parameter locations
! LOCTYP      I*4   nrlst     O    type  of parameter locations
! NRLST       I*4             O    number of to be returned
! LOCNR       I*4   nrlst     O    list of index numbers of locations
! IERROR      I*4             O    = 0 no errors, = 1 error detected
! ZBUFFS     CH*20  maxlst    I/O  workspace names of locations
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
! FOUT        L*4                  Flag for further execution program
!                                  fout  = .true.  : stop execution
!                                  fout  = .false. : go on
! FILHDA     CH*256                File name NEFIS data file for HIS
! FILHDE     CH*256                File name NEFIS definition file for
!                                  HIS
! GRPDEF     CH*16                 Group name definition
!                                  must be read
! HDAFDS      I*4  999             Data file descriptor for the HIS-DAT
!                                  file
! HDEFDS      I*4  2997            Definition file description for the
!                                  HIS-DEF file
! I           I*4                  Help variable
! IERROR      I*4                  Error code for NEFIS error
! LNAME      CH*20                 Help var. location name
! NLOC        I*4                  Number of found locations
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
   integer         ierror,nrlst ,nloc  ,ind
   integer         i     ,nostat
   integer         maxlst,itype
   integer         maxdef
   integer         pardep
   integer         timdep
   integer         loctyp(maxlst)
   integer         locnr (maxlst)
!
   character       locdef(*)*21
   character       loclst(*)*21
   character*20    zbuffs(maxlst)
   character       fname(3)*256
   character*256   filhda,filhde
   character*256   option
!
   logical         ex           ,okee,fout
!-----------------------------------------------------------------------
!-----declaration NEFIS
!-----------------------------------------------------------------------
   integer maxgrp,maxelm
   parameter(maxgrp=50,maxelm=50)
   character grpnam(maxgrp)*60,grpdef(maxgrp)*16,&
   &cel(maxgrp)*16,elmnam(maxgrp,maxelm)*16,&
   &elmtyp(maxgrp,maxelm)*16,elmqty(maxgrp,maxelm)*16,&
   &elmunt(maxgrp,maxelm)*16,elmdes(maxgrp,maxelm)*64
   integer grpndm(maxgrp),&
   &grpdms(5,maxgrp),elmdms(5,maxgrp,maxelm),&
   &elmndm(maxgrp,maxelm)
   integer       hdefds( 2997),hdafds(  999)
   integer hislast,npar,maplast

!
!-----------------------------------------------------------------------
!-----Initialisation
!-----------------------------------------------------------------------
   fout   = .false.
!pvb  write(*,*)' call phi_loc'
   ierror =  0
!--------------------------------------------------------------------
!-----Test .dat and .def Nefis HIS-files
!     exist
!--------------------------------------------------------------------
   ind = index ( fname(1), char(0))
   if ( ind .eq. 0 ) then
      filhda = fname(1)
   else
      filhda = fname(1)(1:ind-1)
   endif
   inquire (file=filhda,exist=ex)
   if (.not.ex) then
      ierror = IENOFI
      return
   endif
!
   ind = index ( fname(2), char(0))
   if ( ind .eq. 0 ) then
      filhde = fname(2)
   else
      filhde = fname(2)(1:ind-1)
   endif
   inquire (file=filhde,exist=ex)
   if (.not.ex) then
      ierror = IENOFI
      return
   endif
!--------------------------------------------------------------------
!-----Open .dat and .def HIS-files
!--------------------------------------------------------------------
   call OPNNEF(fname, itype, hdafds, hdefds, ierror )
   if (ierror .ne. 0) then
      ierror = IEFIRO
      return
   endif
!--------------------------------------------------------------------
!-----Read array-dimensions from Nefis HIS-files group 2
!--------------------------------------------------------------------
!--------------------------------------------------------------------
!-----Read array-loclst from Nefis files
!--------------------------------------------------------------------
   call philoc(hdafds,hdefds,okee,npar,nostat,maplast,itype,&
   &hislast,&
   &grpnam,grpdef,cel,elmnam,elmtyp,elmqty,&
   &elmunt,elmdes,zbuffs,&
   &grpndm,grpdms,elmdms,elmndm,maxgrp,maxelm)

!
   nloc=0
   do 10 i = 1, nostat
      nloc = nloc + 1
      loclst (nloc) = zbuffs(i)
      locnr  (nloc) = i
10 continue


   nrlst = nloc

!-----------------------------------------------------------------------
!-----check found number against required number
!     filter not yet used
!-----------------------------------------------------------------------
   if (nloc   .gt. maxlst) then
      fout   = .true.
   endif
!--------------------------------------------------------------------
!-----Close .dat and .def HIS-files
!--------------------------------------------------------------------
   call CLOSFL(fname, ierror)
   if (ierror .ne. 0) then
      fout  = .true.
   endif
!-----------------------------------------------------------------------
!-----return status to calling routine
!-----------------------------------------------------------------------
   ierror = IEOK
   if (fout  ) then
      ierror = IEOTHR
   endif
!
   return
!-----------------------------------------------------------------------
end

subroutine             phi_tme&
!#ifdef WINNT
!    *                 [ALIAS:'_phi_tme']
!#endif
&(fname  ,itype  ,timdef, maxdef ,pardep , locdep,&
&maxlst ,        timlst,         timtyp ,&
&nrlst  ,ierror ,option                         )
!-----------------------------------------------------------------------
!           Function: time selection for maps
!                     PHIDIAS NEFIS  file
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
! FILHDA     CH*256                File name NEFIS data file for COM
! FILHDE     CH*256                File name NEFIS definition file for
!                                  COM
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
! KMAX        I*4                  Number of layers
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
   integer         itdate,ind
   integer         ierror,nrlst ,maxdef,maxlst,itype
   integer         julday
   integer         imo1
   integer         iy    ,imo   ,iday
   integer         pardep,locdep
   integer         timtyp(maxlst)
!
   real*8           timlst(maxlst)
   real*8           timdef(maxdef,2)
   real             dt    ,tunit
!
   character       fname(*)*256
   character*256   filhda,filhde
   character*256   option
!
   logical         ex    ,okee
!-----------------------------------------------------------------------
!-----declaration NEFIS
!-----------------------------------------------------------------------
   integer maxgrp,maxelm
   parameter(maxgrp=50,maxelm=50)
   character grpnam(maxgrp)*60,grpdef(maxgrp)*16,&
   &cel(maxgrp)*16,elmnam(maxgrp,maxelm)*16,&
   &elmtyp(maxgrp,maxelm)*16,elmqty(maxgrp,maxelm)*16,&
   &elmunt(maxgrp,maxelm)*16,elmdes(maxgrp,maxelm)*64,&
   &grpdaf*16,elmnaa*16,tnul*12
   integer grpndm(maxgrp),&
   &grpdms(5,maxgrp),elmdms(5,maxgrp,maxelm),&
   &elmndm(maxgrp,maxelm)
   integer       hdefds( 2997),hdafds(  999)
   integer maplast,nmax,mmax,npar
!
   integer&
   &uindex(3,5  ),usrord(5),buflen
!
!
   integer       GETELT, GETELS
!#ifdef WINNT
!     integer       GETELT_i
!#endif

!-----------------------------------------------------------------------
!-----Initialisation
!-----------------------------------------------------------------------
   okee   = .true.
!pvb  write(*,*)' call phi_tme'
!
   ierror =  0
!
!--------------------------------------------------------------------
!-----Test if .dat and .def Nefis files
!     exist
!--------------------------------------------------------------------
   ind = index ( fname(1), char(0))
   if ( ind .eq. 0 ) then
      filhda = fname(1)
   else
      filhda = fname(1)(1:ind-1)
   endif
   inquire (file=filhda,exist=ex)
   if (.not.ex) then
      ierror = IENOFI
      return
   endif
!
   ind = index ( fname(2), char(0))
   if ( ind .eq. 0 ) then
      filhde = fname(2)
   else
      filhde = fname(2)(1:ind-1)
   endif
   inquire (file=filhde,exist=ex)
   if (.not.ex) then
      ierror = IENOFI
      return
   endif
!--------------------------------------------------------------------
!-----Open .dat and .def files
!--------------------------------------------------------------------
   call OPNNEF(fname, itype, hdafds, hdefds, ierror )
   if (ierror .ne. 0) then
      ierror = IEFIRO
      return
   endif
!--------------------------------------------------------------------
!-----Read constants from Nefis files
!--------------------------------------------------------------------
   grpdaf    = 'map-info-series'
   if(itype.eq.5050)grpdaf='tsr-info-series'
   uindex(1,1) = 2
   uindex(2,1) = 2
   uindex(3,1) = 1
   usrord(1) = 1

   buflen    =12 * 4
   elmnaa    = 'tijd-mapc'
   if(itype.eq.5050)elmnaa='tijd-tsrc'
!#ifdef WINNT
!     ierror = GETELT_i
!#else
   ierror = GETELS&
!#endif
   &(hdefds   ,grpdaf    ,elmnaa    ,&
   &uindex   ,usrord    ,buflen    ,tnul      )
   okee = okee .and. ierror .eq. 0

   read(tnul(1:6),'(i6)')itdate
   buflen    = 4
   tunit=1.
   dt=1.
!-----------------------------------------------------------------------
!-----Convert ITDATE to julian day number JULDAY
!-----------------------------------------------------------------------
   iday   = mod (itdate ,   100)
   imo    = mod (itdate , 10000) /  100
   iy     = 1900 + itdate / 10000
   imo1   = (imo -14)/12
   julday = iday - 32075 + 1461 * (iy+4800+imo1  )/ 4&
   &+ 367  * (imo  - 2    - imo1  *  12    )/12&
   &- 3    * ((iy  + 4900 + imo1  )/100    )/ 4

!--------------------------------------------------------------------
!-----Read array-timlst from Nefis files
!--------------------------------------------------------------------
   maplast=0
   call phitme(hdafds,hdefds,okee,npar,nmax,mmax,itype,&
   &grpnam,grpdef,cel,elmnam,elmtyp,elmqty,&
   &elmunt,elmdes,&
   &grpndm,grpdms,elmdms,elmndm,maxgrp,maxelm,&
   &maplast,&
   &pardep,timlst,nrlst,julday,dt,tunit)

!-----------------------------------------------------------------------
!-----Close .dat and .def files
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

subroutine             phi_mat&
!#ifdef WINNT
!    *                 [ALIAS:'_phi_mat']
!#endif
&(fname ,itype  ,parcod, loc   , tim   ,misval,&
&i3gl  ,maxdim ,xdata , ierror, option,&
&ibuffs,rbuffs                               )
!-----------------------------------------------------------------------
!           Function: select map data out of PHIDIAS MAP NEFIS files
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
! FILHDA     CH*256                File name NEFIS data file for COM
! FILHDE     CH*256                File name NEFIS definition file for
!                                  COM
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
   integer         i3gl,iday,itdate
   integer         ibuffs (*     )

   real            misval,dt,tunit
   real            xdata (maxdim)
   real            rbuffs (*     )

   character       fname (*)*256,tnul*12
   character*256   option
!-----------------------------------------------------------------------
!-----declaration Local variables
!-----------------------------------------------------------------------
   character*256   filhda,filhde
!
   integer         ind ,ierror
   integer         imo,iy,imo1,julday
!
   logical         ex    ,okee
!
!-----------------------------------------------------------------------
!-----declaration NEFIS
!-----------------------------------------------------------------------
   integer maxgrp,maxelm
   parameter(maxgrp=50,maxelm=50)
   character grpnam(maxgrp)*60,grpdef(maxgrp)*16,&
   &cel(maxgrp)*16,elmnam(maxgrp,maxelm)*16,&
   &elmtyp(maxgrp,maxelm)*16,elmqty(maxgrp,maxelm)*16,&
   &elmunt(maxgrp,maxelm)*16,elmdes(maxgrp,maxelm)*64,&
   &grpdaf*16,elmnaa*16
   integer grpndm(maxgrp),&
   &grpdms(5,maxgrp),elmdms(5,maxgrp,maxelm),&
   &elmndm(maxgrp,maxelm)
   integer       hdefds( 2997),hdafds(  999)
   integer maplast,nmax,mmax,npar
   integer       uindex(3,5),usrord(5),buflen
   integer       GETELT, GETELS
!#ifdef WINNT
!     integer       GETELT_i
!#endif
!-----------------------------------------------------------------------
!-----Initialisation
!-----------------------------------------------------------------------
!
!pvb  write(*,*)' call phi_mat'
!pvb  write(*,*)' loc ',loc(1,1),loc(1,2),loc(1,3)
   ierror =  0
   okee   = .true.
!--------------------------------------------------------------------
!-----Test if .dat and .def Nefis files
!     exist
!--------------------------------------------------------------------
   ind = index ( fname(1), char(0))
!pvb  write(*,*)' in phi_mat ind ',ind
   if ( ind .eq. 0 ) then
      filhda = fname(1)
   else
      filhda = fname(1)(1:ind-1)
   endif
   inquire (file=filhda,exist=ex)
   if (.not.ex) then
      ierror = IENOFI
      return
   endif
!
   ind = index ( fname(2), char(0))
!pvb  write(*,*)' in phi_mat ind ',ind
   if ( ind .eq. 0 ) then
      filhde = fname(2)
   else
      filhde = fname(2)(1:ind-1)
   endif
   inquire (file=filhde,exist=ex)
   if (.not.ex) then
      ierror = IENOFI
      return
   endif
!pvb  write(*,*)' in phi_mat voor open ',filhde,filhda
!--------------------------------------------------------------------
!-----Open .dat and .def COM-files
!--------------------------------------------------------------------
   call OPNNEF(fname, itype, hdafds, hdefds, ierror)
   if (ierror .ne. 0) then
      ierror = IEFIRO
      return
   endif
!
!--------------------------------------------------------------------
!-----Read constants from Nefis files
!--------------------------------------------------------------------
   grpdaf    = 'map-info-series'
   if(itype.eq.5050)grpdaf='tsr-info-series'
   uindex(1,1) = 1
   uindex(2,1) = 1
   uindex(3,1) = 1
   usrord(1) = 1

   buflen    =12 * 4
   elmnaa    = 'tijd-mapc'
   if(itype.eq.5050)elmnaa='tijd-tsrc'
!#ifdef WINNT
!     ierror = GETELT_i
!#else
   ierror = GETELS&
!#endif
   &(hdefds   ,grpdaf    ,elmnaa    ,&
   &uindex   ,usrord    ,buflen    ,tnul      )
   okee  =  ierror .eq. 0
   read(tnul(1:6),'(i6)')itdate
   buflen    = 4
   tunit=1.
   dt=1.
!-----------------------------------------------------------------------
!-----Convert ITDATE to julian day number JULDAY
!-----------------------------------------------------------------------
   iday   = mod (itdate ,   100)
   imo    = mod (itdate , 10000) /  100
   iy     = 1900 + itdate / 10000
   imo1   = (imo -14)/12
   julday = iday - 32075 + 1461 * (iy+4800+imo1  )/ 4&
   &+ 367  * (imo  - 2    - imo1  *  12    )/12&
   &- 3    * ((iy  + 4900 + imo1  )/100    )/ 4
!-----------------------------------------------------------------------
   maplast=0
!pvb  write(*,*)' in phi_mat voor phimat'
   call phimat(hdafds,hdefds,okee,npar,nmax,mmax,itype,&
   &grpnam,grpdef,cel,elmnam,elmtyp,elmqty,&
   &elmunt,elmdes,&
   &grpndm,grpdms,elmdms,elmndm,maxgrp,maxelm,&
   &maplast,&
   &parcod,&
   &ibuffs,rbuffs,tim,xdata,&
   &dt,julday,tunit,&
   &misval,loc)
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
subroutine phidim(datfds,deffds,okee,npar,nmax,mmax,maplast,&
&itype,nloc,hislast,&
&grpnam,grpdef,cel,elmnam,elmtyp,elmqty,&
&elmunt,elmdes,&
&grpndm,grpdms,elmdms,elmndm,maxgrp,maxelm)
   integer maxgrp,maxelm
   character grpnam(*)*60,grpdef(*)*16,&
   &cel(*)*16,elmnam(maxgrp,*)*16,&
   &elmtyp(maxgrp,*)*16,elmqty(maxgrp,*)*16,&
   &elmunt(maxgrp,*)*16,elmdes(maxgrp,*)*64
   integer datfds(*),deffds(*),nelems(100),grpdms(maxgrp),&
   &grpndm(5,maxgrp),elmdms(5,maxgrp,maxelm),elmndm(maxgrp,*)
   integer i,j,igrp,itype,&
   &nmax,mmax,&
   &npar,maplast,nloc,hislast
   logical okee
!pvb  write(*,*)' call phidim'
   if(itype.eq.5051)then
!
!  map file
!
      npar=0
      nmax=0
      mmax=0
      igrp=0
      okee=.true.
      call mapphi(datfds,deffds,okee,nmax,mmax,igrp,&
      &grpnam,grpdef,cel,elmnam,elmtyp,elmqty,&
      &elmunt,elmdes,nelems,&
      &grpndm,grpdms,elmdms,elmndm,maxgrp,maxelm,&
      &maplast)
      if(nmax.eq.0.and.mmax.eq.0)then
         do 15 i=1,igrp-1
            do 15 j=1,nelems(i)
               if(elmndm(j,I).eq.2)then
                  nmax=elmdms(1,j,i)
                  mmax=elmdms(2,j,i)
                  goto 17
               endif
15       continue
17       continue
      endif
      do 20 i=1,igrp-1
         do 20 j=1,nelems(i)
            if(elmndm(j,i).eq.2)then
               if(elmdms(1,j,i).eq.nmax.and.elmdms(2,j,i).eq.mmax)then
                  npar=npar+1
               endif
            endif
20    continue
   endif
   if(itype.eq.5050)then
!
!  history file
!
      npar=0
      nloc=0
      igrp=0
      okee=.true.
      call hisphi(datfds,deffds,okee,nloc,igrp,&
      &grpnam,grpdef,cel,elmnam,elmtyp,elmqty,&
      &elmunt,elmdes,nelems,&
      &grpndm,grpdms,elmdms,elmndm,maxgrp,maxelm,&
      &hislast)
      do 30 i=1,igrp-1
         do 30 j=1,nelems(i)
            if(grpnam(i).eq.'tsr-series')then
               npar=npar+1
            endif
30    continue
   endif
   return
end
subroutine phipar(datfds,deffds,okee,tmlcdp,iplmnk,tmlcdh,npar,&
&grpnam,grpdef,cel,elmnam,elmtyp,elmqty,&
&elmunt,elmdes,itype,&
&grpndm,grpdms,elmdms,elmndm,maxgrp,maxelm,&
&partyp,parcod,parlst,paruni)
   integer maxgrp,maxelm
   character grpnam(*)*60,grpdef(*)*16,&
   &cel(*)*16,elmnam(maxgrp,*)*16,&
   &elmtyp(maxgrp,*)*16,elmqty(maxgrp,*)*16,&
   &elmunt(maxgrp,*)*16,elmdes(maxgrp,*)*64,&
   &parlst(*)*(21),paruni(*)*21
   integer datfds(*),deffds(*),nelems(100),grpdms(maxgrp),&
   &grpndm(5,maxgrp),elmdms(5,maxgrp,maxelm),elmndm(maxgrp,*)
   integer nmax,mmax,maplast,i,j,igrp,nloc,hislast,itype,&
   &partyp(*),parcod(*),tmlcdp,npar,iplmnk,tmlcdh
   logical okee
!pvb  write(*,*)' call phipar'
   if(itype.eq.5051)then
!
!   map file
!
      npar=0
      nmax=0
      mmax=0
      igrp=0
      okee=.true.
      call mapphi(datfds,deffds,okee,nmax,mmax,igrp,&
      &grpnam,grpdef,cel,elmnam,elmtyp,elmqty,&
      &elmunt,elmdes,nelems,&
      &grpndm,grpdms,elmdms,elmndm,maxgrp,maxelm,&
      &maplast)
      if(nmax.eq.0.and.mmax.eq.0)then
         do 15 i=1,igrp-1
            do 15 j=1,nelems(i)
               if(elmndm(j,I).eq.2)then
                  nmax=elmdms(1,j,i)
                  mmax=elmdms(2,j,i)
                  goto 17
               endif
15       continue
17       continue
      endif
      do 20 i=1,igrp-1
         do 20 j=1,nelems(i)
            if(elmndm(j,i).eq.2)then
               if(elmdms(1,j,i).eq.nmax.and.elmdms(2,j,i).eq.mmax)then
                  npar=npar+1
                  parcod(npar)=npar
                  parlst(npar)=elmnam(j,i)
                  partyp(npar)=IPLMNK
                  if(grpdef(i).eq.'map-series')&
                  &partyp(npar)=TMLCDP
                  paruni(npar)=elmunt(j,i)
               endif
            endif
20    continue
   endif
   if(itype.eq.5050)then
!
!   history file
!
      npar=0
      nloc=0
      igrp=0
      okee=.true.
      call hisphi(datfds,deffds,okee,nloc,igrp,&
      &grpnam,grpdef,cel,elmnam,elmtyp,elmqty,&
      &elmunt,elmdes,nelems,&
      &grpndm,grpdms,elmdms,elmndm,maxgrp,maxelm,&
      &hislast)
      do 30 i=1,igrp-1
         do 30 j=1,nelems(i)
            if(grpnam(i).eq.'tsr-series')then
               npar=npar+1
               parcod(npar)=npar
               parlst(npar)=elmnam(j,i)
               partyp(npar)=TMLCDH
               paruni(npar)=elmunt(j,i)
            endif
30    continue
   endif
   return
end
subroutine philoc(datfds,deffds,okee,npar,nostat,maplast,&
&itype,hislast,&
&grpnam,grpdef,cel,elmnam,elmtyp,elmqty,&
&elmunt,elmdes,zbuffs,&
&grpndm,grpdms,elmdms,elmndm,maxgrp,maxelm)
   integer maxgrp,maxelm
   character grpnam(*)*60,grpdef(*)*16,zbuffs(*)*(*),&
   &cel(*)*16,elmnam(maxgrp,*)*16,&
   &elmtyp(maxgrp,*)*16,elmqty(maxgrp,*)*16,&
   &elmunt(maxgrp,*)*16,elmdes(maxgrp,*)*64
   integer datfds(*),deffds(*),nelems(100),grpdms(maxgrp),&
   &grpndm(5,maxgrp),elmdms(5,maxgrp,maxelm),elmndm(maxgrp,*)
   integer i,j,igrp,itype,error,getelt,&
   &nmax,mmax,nostat,ilen,&
   &npar,maplast,nloc,hislast,&
   &uindex(3,5),&
   &usrord(5)
   logical okee
   real xt(500),yt(500)
!pvb  write(*,*)' call philoc'
   if(itype.eq.5051)then
!
!  map file
!
      npar=0
      nmax=0
      mmax=0
      igrp=0
      okee=.true.
      call mapphi(datfds,deffds,okee,nmax,mmax,igrp,&
      &grpnam,grpdef,cel,elmnam,elmtyp,elmqty,&
      &elmunt,elmdes,nelems,&
      &grpndm,grpdms,elmdms,elmndm,maxgrp,maxelm,&
      &maplast)
      if(nmax.eq.0.and.mmax.eq.0)then
         do 15 i=1,igrp-1
            do 15 j=1,nelems(i)
               if(elmndm(j,I).eq.2)then
                  nmax=elmdms(1,j,i)
                  mmax=elmdms(2,j,i)
                  goto 17
               endif
15       continue
17       continue
      endif
      do 20 i=1,igrp-1
         do 20 j=1,nelems(i)
            if(elmndm(j,i).eq.2)then
               if(elmdms(1,j,i).eq.nmax.and.elmdms(2,j,i).eq.mmax)then
                  npar=npar+1
               endif
            endif
20    continue
   endif
   if(itype.eq.5050)then
!
!  history file
!
      npar=0
      nloc=0
      igrp=0
      okee=.true.
      call hisphi(datfds,deffds,okee,nloc,igrp,&
      &grpnam,grpdef,cel,elmnam,elmtyp,elmqty,&
      &elmunt,elmdes,nelems,&
      &grpndm,grpdms,elmdms,elmndm,maxgrp,maxelm,&
      &hislast)
      do 30 i=1,igrp-1
         do 30 j=1,nelems(i)
            if(grpnam(i).eq.'tsr-series')then
               npar=npar+1
            endif
30    continue
      nostat=nloc
      ilen=nostat*4
      uindex(1,1)=1
      uindex(2,1)=1
      uindex(3,1)=1
      usrord(1)=1
      error=getelt(deffds,'tsr-coord',&
      &'XT'         ,uindex,usrord,&
      &ilen,xt    )
!pvb  if(error.ne.0)write(*,*)' nefis error xt ',error
      error=getelt(deffds,'tsr-coord',&
      &'YT'         ,uindex,usrord,&
      &ilen,yt    )
!pvb  if(error.ne.0)write(*,*)' nefis error yt ',error
      do 40 i=1,nostat
         zbuffs(i)(1:3)='XT='
         write(zbuffs(i)(4:10),'(f7.2)')xt(i)
         zbuffs(i)(11:13)='YT='
         write(zbuffs(i)(14:20),'(f7.2)')yt(i)
40    continue
   endif
   return
end
subroutine phitme(datfds,deffds,okee,npar,nmax,mmax,itype,&
&grpnam,grpdef,cel,elmnam,elmtyp,elmqty,&
&elmunt,elmdes,&
&grpndm,grpdms,elmdms,elmndm,maxgrp,maxelm,&
&maplast,&
&pardep,timlst,nrlist,julday,dt,tunit)
   integer maxgrp,maxelm
   character grpnam(*)*60,grpdef(*)*16,&
   &cel(*)*16,elmnam(maxgrp,*)*16,&
   &elmtyp(maxgrp,*)*16,elmqty(maxgrp,*)*16,&
   &elmunt(maxgrp,*)*16,elmdes(maxgrp,*)*64
   integer datfds(*),deffds(*),nelems(100),grpdms(maxgrp),&
   &grpndm(5,maxgrp),elmdms(5,maxgrp,maxelm),elmndm(maxgrp,*)
   integer error,i,j,igrp,itype,&
   &uindex(3,5),nmax,mmax,&
   &usrord(5),npar,maplast,hislast,nloc
   integer pardep,julday,ilen,ii,itmodc,icurtm,iday,ihou,imin,isec
   integer l,n,iy,imo,imo1,idp,itp,nhulp,nrlist
   real*8 timlst(*)
   real dt,tunit,tmodc
   logical okee
   INTEGER&
   &getelt
!pvb  write(*,*)' call phitme'
   if(itype.eq.5051)then
!
! map file
!
      uindex(1,1)=1
      uindex(2,1)=1
      uindex(3,1)=1
      usrord(1)=1
      npar=0
      nmax=0
      mmax=0
      igrp=0
      okee=.true.
      call mapphi(datfds,deffds,okee,nmax,mmax,igrp,&
      &grpnam,grpdef,cel,elmnam,elmtyp,elmqty,&
      &elmunt,elmdes,nelems,&
      &grpndm,grpdms,elmdms,elmndm,maxgrp,maxelm,&
      &maplast)
      if(nmax.eq.0.and.mmax.eq.0)then
         do 15 i=1,igrp-1
            do 15 j=1,nelems(i)
               if(elmndm(j,I).eq.2)then
                  nmax=elmdms(1,j,i)
                  mmax=elmdms(2,j,i)
                  goto 17
               endif
15       continue
17       continue
      endif
      ii=-1000
      do 20 i=1,igrp-1
         do 20 j=1,nelems(i)
            if(elmndm(j,i).eq.2)then
               if(elmdms(1,j,i).eq.nmax.and.elmdms(2,j,i).eq.mmax)then
                  npar=npar+1
                  if(pardep.eq.npar)then
                     ii=i
                  endif
               endif
            endif
20    continue
      ilen=0
      if(ii.ne.-1000)then
         if(grpdef(ii).eq.'map-series')ilen=maplast
      endif
      nhulp=ilen
      do 100 i=1,ilen
         uindex(1,1) = i
         uindex(2,1) = i
         uindex(3,1) = 1
         itmodc=-1
         tmodc=-1.
         grpdef(ii)='map-info-series'
         elmnam(1,ii)='tijd-mapr'
         error=getelt(deffds,grpdef(ii),&
         &elmnam(1,ii),uindex,usrord,&
         &4,tmodc)
         call nefout(elmnam(1,ii),error)
         okee=error.eq.0
         if(okee)itmodc=tmodc*3600.
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
         icurtm = nint  (itmodc * dt * tunit)
         iday   = icurtm / 86400
         icurtm = icurtm - iday  * 86400
         ihou   = icurtm / 3600
         icurtm = icurtm - ihou  * 3600
         imin   = icurtm / 60
         icurtm = icurtm - imin  * 60
         isec   = icurtm
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
!pvb     write(*,*)' i tim ',i,timlst(i)
100   continue

!-----------------------------------------------------------------------
!-----Exception handling not enough data written to Nefis files
!-----------------------------------------------------------------------
200   continue
!
      nrlist = nhulp
   endif
   if(itype.eq.5050)then
!
!  history file
!
      uindex(1,1)=1
      uindex(2,1)=1
      uindex(3,1)=1
      usrord(1)=1
      npar=0
      nloc=0
      igrp=0
      okee=.true.
      call hisphi(datfds,deffds,okee,nloc,igrp,&
      &grpnam,grpdef,cel,elmnam,elmtyp,elmqty,&
      &elmunt,elmdes,nelems,&
      &grpndm,grpdms,elmdms,elmndm,maxgrp,maxelm,&
      &hislast)
      ii = -1000
      do 30 i=1,igrp-1
         do 30 j=1,nelems(i)
            if(grpnam(i).eq.'tsr-series')then
               npar=npar+1
               if(pardep.eq.npar) ii=i
            endif
30    continue
      ilen=0
      if(ii.ne.-1000)then
         if(grpdef(ii).eq.'tsr-series')ilen=hislast
      endif
      nhulp=ilen
      do 300 i=1,ilen
         uindex(1,1) = i
         uindex(2,1) = i
         uindex(3,1) = 1
         itmodc=-1
         tmodc=-1.
         grpdef(ii)='tsr-info-series'
         elmnam(1,ii)='tijd-tsrr'
         error=getelt(deffds,grpdef(ii),&
         &elmnam(1,ii),uindex,usrord,&
         &4,tmodc)
         call nefout(elmnam(1,ii),error)
         okee=error.eq.0
         if(okee)itmodc=tmodc*3600.
         if (error .ne. 0 .or. itmodc .eq. -1) then
!-----------------------------------------------------------------------
!-----------In case an error occured while reading the file then
!           write part which is ok to nhulp = i - 1
!-----------------------------------------------------------------------
            nhulp  = i - 1
            goto 400
         endif
!-----------------------------------------------------------------------
!--------Calculate current time elapsed in seconds referenced to ITDATE
!-----------------------------------------------------------------------
         icurtm = nint  (itmodc * dt * tunit)
         iday   = icurtm / 86400
         icurtm = icurtm - iday  * 86400
         ihou   = icurtm / 3600
         icurtm = icurtm - ihou  * 3600
         imin   = icurtm / 60
         icurtm = icurtm - imin  * 60
         isec   = icurtm
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
300   continue

!-----------------------------------------------------------------------
!-----Exception handling not enough data written to Nefis files
!-----------------------------------------------------------------------
400   continue
!
      nrlist = nhulp
   endif
   return
end
subroutine phimat(datfds,deffds,okee,npar,nmax,mmax,itype,&
&grpnam,grpdef,cel,elmnam,elmtyp,elmqty,&
&elmunt,elmdes,&
&grpndm,grpdms,elmdms,elmndm,maxgrp,maxelm,&
&maplast,&
&pardep,&
&ibuffs,rbuffs,tim,xdata,&
&dt,julday,tunit,&
&misval,loc)
   integer maxgrp,maxelm,loc(3,3)
   character grpnam(*)*60,grpdef(*)*16,elmnaa*16,grpdea*16,&
   &cel(*)*16,elmnam(maxgrp,*)*16,&
   &elmtyp(maxgrp,*)*16,elmqty(maxgrp,*)*16,&
   &elmunt(maxgrp,*)*16,elmdes(maxgrp,*)*64
   integer datfds(*),deffds(*),nelems(100),grpdms(maxgrp),&
   &grpndm(5,maxgrp),elmdms(5,maxgrp,maxelm),elmndm(maxgrp,*)
   integer error,i,j,igrp,itype,nhulp,&
   &maplast,hislast,&
   &uindex(3,5),nmax,mmax,nloc,&
   &usrord(5),npar,ibuffs(*),nindex(3)
   integer pardep,julday,ilen,ii,jj,itmodc,icurtm,iday,ihou,imin,isec
   integer l,n,iy,imo,imo1,idp,itp,m,n1,n2
   real    tunit,rbuffs(*),xdata(*),dt,misval,tmodc
   real*8 timlst,tim(3)
   real hm0(100)
   logical okee,vector
   INTEGER&
   &getelt,ibuf
!pvb  write(*,*)' call phimat'
   if(itype.eq.5051)then
!
!  map file
!
      uindex(1,1)=1
      uindex(2,1)=1
      uindex(3,1)=1
      usrord(1)=1
      vector=.true.
      npar=0
      nmax=0
      mmax=0
      igrp=0
      okee=.true.
      call mapphi(datfds,deffds,okee,nmax,mmax,igrp,&
      &grpnam,grpdef,cel,elmnam,elmtyp,elmqty,&
      &elmunt,elmdes,nelems,&
      &grpndm,grpdms,elmdms,elmndm,maxgrp,maxelm,&
      &maplast)
      if(nmax.eq.0.and.mmax.eq.0)then
         do 15 i=1,igrp-1
            do 15 j=1,nelems(i)
               if(elmndm(j,I).eq.2)then
                  nmax=elmdms(1,j,i)
                  mmax=elmdms(2,j,i)
                  goto 17
               endif
15       continue
17       continue
      endif
      ii=-1000
      do 20 i=1,igrp-1
         do 20 j=1,nelems(i)
            if(elmndm(j,i).eq.2)then
               if(elmdms(1,j,i).eq.nmax.and.elmdms(2,j,i).eq.mmax)then
                  npar=npar+1
                  if(pardep.eq.npar)then
                     ii=i
                     jj=j
                  endif
               endif
            endif
20    continue
!pvb  write(*,*)' in phimat ',elmnam(jj,ii),pardep
      ilen=0
      if(ii.ne.-1000)then
         if(grpdef(ii).eq.'map-series')ilen=maplast
      endif
      nindex(1)=1
      nindex(2)=1
      nindex(3)=1
      do 100 i=1,ilen
         uindex(1,1) = i
         uindex(2,1) = i
         uindex(3,1) = 1
         itmodc=-1
         tmodc=-1.
         grpdea='map-info-series'
         elmnaa='tijd-mapr'
         error=getelt(deffds,grpdea,&
         &elmnaa,uindex,usrord,&
         &4,tmodc)
         call nefout(elmnaa,error)
         okee=error.eq.0
         if(okee)itmodc=tmodc*3600.
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
         icurtm = nint  (itmodc * dt * tunit)
         iday   = icurtm / 86400
         icurtm = icurtm - iday  * 86400
         ihou   = icurtm / 3600
         icurtm = icurtm - ihou  * 3600
         imin   = icurtm / 60
         icurtm = icurtm - imin  * 60
         isec   = icurtm
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
         if(timlst.le.tim(1))then
            nindex(1)=i
         endif
         if(timlst.le.tim(2))then
            nindex(2)=i
         endif
100   continue
200   continue
      uindex(1,1) = nindex(1)
      uindex(2,1) = nindex(2)
      uindex(3,1) = nindex(3)
      ilen=4*nmax*mmax
      if(elmtyp(jj,ii).eq.'REAL')then
         error=getelt(deffds,grpdef(ii),&
         &elmnam(jj,ii),uindex,usrord,&
         &ilen,rbuffs)
         do 35 n = 1,nmax
            do 35 m = 1,mmax
               n1           =   (n-1) * mmax +  m
               n2           =   (m-1) *nmax+n
               xdata ( n1 ) = rbuffs(n2)
35       continue
      else
         error=getelt(deffds,grpdef(ii),&
         &elmnam(jj,ii),uindex,usrord,&
         &ilen,ibuffs)
         do 40 n = 1,nmax
            do 40 m = 1,mmax
               n1           =   (n-1) * mmax +  m
               n2=(m-1)*nmax+n
               xdata ( n1 ) = ibuffs(n2)
40       continue
      endif
      call nefout(elmnam(jj,ii),error)
      okee=error.eq.0
   endif
!
   if(itype.eq.5050)then
!
!  history file
!
      uindex(1,1)=1
      uindex(2,1)=1
      uindex(3,1)=1
      usrord(1)=1
      ibuf=400
      npar=0
      nloc=0
      igrp=0
      okee=.true.
      call hisphi(datfds,deffds,okee,nloc,igrp,&
      &grpnam,grpdef,cel,elmnam,elmtyp,elmqty,&
      &elmunt,elmdes,nelems,&
      &grpndm,grpdms,elmdms,elmndm,maxgrp,maxelm,&
      &hislast)
      ii=-1000
      do 30 i=1,igrp-1
         do 30 j=1,nelems(i)
            if(grpnam(i).eq.'tsr-series')then
               npar=npar+1
               if(pardep.eq.npar)then
                  ii=i
                  jj=j
               endif
            endif
30    continue
!pvb  write(*,*)' in phimat ',elmnam(jj,ii),pardep
      ilen=0
      if(ii.ne.-1000)then
         if(grpdef(ii).eq.'tsr-series')ilen=hislast
      endif
      nindex(1)=1
      nindex(2)=1
      nindex(3)=1
      do 300 i=1,ilen
         uindex(1,1) = i
         uindex(2,1) = i
         uindex(3,1) = 1
         error=getelt(deffds,grpdef(ii),&
         &elmnam(jj,ii),uindex,usrord,&
         &ibuf,hm0)
         call nefout(elmnam(jj,ii),error)
         okee=error.eq.0
         if(okee)xdata(i)=hm0(loc(1,1))
300   continue
      okee=error.eq.0
   endif
   return
end
subroutine hisphi(datfds,deffds,okee,nloc,igrp,&
&grpnam,grpdef,cel,elmnam,elmtyp,elmqty,&
&elmunt,elmdes,&
&nelems,&
&grpndm,grpdms,elmdms,elmndm,maxgrp,maxelm,&
&hislast)
   integer maxgrp,maxelm
   character grpnam(*)*60,grpdef(*)*16,grpdaf*16,celnam*16,&
   &cel(*)*16,elmnam(maxgrp,*)*16,elmnum*16,&
   &elmtyp(maxgrp,*)*16,elmqty(maxgrp,*)*16,&
   &elmunt(maxgrp,*)*16,elmdes(maxgrp,*)*64,&
   &elmtup*16,elmqtu*16,elmuut*16,elmdus*16
   integer datfds(*),deffds(*),nelems(100),grpdms(maxgrp),&
   &grpndm(5,maxgrp),elmdms(5,maxgrp,maxelm),elmndm(maxgrp,*)
   integer error,i,igrp,nbytsg,&
   &uindex(3,5),nloc,jor(5),hislast,&
   &usrord(5),grpdm
   INTEGER INQGRP,INQCEL,INQFST,INQNXT,inqelm,&
   &getelt
   logical okee
!pvb  write(*,*)' call hisphi'
   uindex(1,1)=1
   uindex(2,1)=1
   uindex(3,1)=1
   usrord(1)=1
   igrp=0
1111 continue
   igrp=igrp+1
   NELEMS(IGRP)=100
   if(igrp.eq.1)then
      error=inqfst(datfds,grpnam(igrp),grpdef(igrp))
      call nefout('inqfst',error)
   else
      error=inqnxt(datfds,grpnam(igrp),grpdef(igrp))
      call nefout('inqnxt',error)
   endif
   if(error.ne.0)goto 9999
   grpdaf=grpdef(igrp)
   grpdm=5
   ERROR=INQGRP(DEFFDS,grpdaf,celnam,GRPDM,&
   &GRPndm(1,IGRP),jOR)
   cel(igrp)=celnam
   grpdms(igrp)=grpdm
   GRPndm(1,IGRP)=MAX(GRPndm(1,IGRP),1)
   call nefout(grpdef(igrp),error)
   okee=error.eq.0
   ERROR=INQCEL(DEFFDS,CEL(igrp),NELEMS(IGRP),ELMNAM(1,IGRP))
   call nefout(cel(igrp),error)
   okee=error.eq.0
   do 10 i=1,nelems(igrp)
      elmnum=elmnam(i,igrp)
      error=inqelm(deffds,elmnum,elmtup,&
      &nbytsg,elmqtu,elmuut,&
      &elmdus,elmndm(i,igrp),elmdms(1,i,igrp))
      elmtyp(i,igrp)=elmtup
      elmqty(i,igrp)=elmqtu
      elmunt(i,igrp)=elmuut
      elmdes(i,igrp)=elmdus
      call nefout(elmnam(i,igrp),error)
      okee=error.eq.0
      if(elmnam(i,igrp).eq.'his-last')then
         error=getelt(deffds,grpdef(igrp),&
         &elmnam(i,igrp),uindex,usrord,&
         &4,hislast)
         okee=error.eq.0
      endif
      if(elmnam(i,igrp).eq.'XT')then
         nloc=elmdms(1,i,igrp)
      endif
10 continue
   GOTO 1111
9999 CONTINUE
end
subroutine mapphi(datfds,deffds,okee,nmax,mmax,igrp,&
&grpnam,grpdef,cel,elmnam,elmtyp,elmqty,&
&elmunt,elmdes,&
&nelems,&
&grpndm,grpdms,elmdms,elmndm,maxgrp,maxelm,&
&maplast)
   integer maxgrp,maxelm
   character grpnam(*)*60,grpdef(*)*16,grpdaf*16,celnam*16,&
   &cel(*)*16,elmnam(maxgrp,*)*16,elmnum*16,&
   &elmtyp(maxgrp,*)*16,elmqty(maxgrp,*)*16,&
   &elmunt(maxgrp,*)*16,elmdes(maxgrp,*)*64,&
   &elmtup*16,elmqtu*16,elmuut*16,elmdus*16
   integer datfds(*),deffds(*),nelems(100),grpdms(maxgrp),&
   &grpndm(5,maxgrp),elmdms(5,maxgrp,maxelm),elmndm(maxgrp,*)
   integer error,i,igrp,nbytsg,&
   &uindex(3,5),nmax,mmax,jor(5),maplast,&
   &usrord(5),grpdm
   INTEGER INQGRP,INQCEL,INQFST,INQNXT,inqelm,&
   &getelt
   logical okee
!pvb  write(*,*)' call mapadm'
   uindex(1,1)=1
   uindex(2,1)=1
   uindex(3,1)=1
   usrord(1)=1
   igrp=0
1111 continue
   igrp=igrp+1
   NELEMS(IGRP)=100
   if(igrp.eq.1)then
      error=inqfst(datfds,grpnam(igrp),grpdef(igrp))
      call nefout('inqfst',error)
   else
      error=inqnxt(datfds,grpnam(igrp),grpdef(igrp))
      call nefout('inqnxt',error)
   endif
   if(error.ne.0)goto 9999
   grpdaf=grpdef(igrp)
   grpdm=5
   ERROR=INQGRP(DEFFDS,grpdaf,celnam,GRPDM,&
   &GRPndm(1,IGRP),jOR)
   cel(igrp)=celnam
   grpdms(igrp)=grpdm
   GRPndm(1,IGRP)=MAX(GRPndm(1,IGRP),1)
   call nefout(grpdef(igrp),error)
   okee=error.eq.0
   ERROR=INQCEL(DEFFDS,CEL(igrp),NELEMS(IGRP),ELMNAM(1,IGRP))
   call nefout(cel(igrp),error)
   okee=error.eq.0
   do 10 i=1,nelems(igrp)
      elmnum=elmnam(i,igrp)
      error=inqelm(deffds,elmnum,elmtup,&
      &nbytsg,elmqtu,elmuut,&
      &elmdus,elmndm(i,igrp),elmdms(1,i,igrp))
      elmtyp(i,igrp)=elmtup
      elmqty(i,igrp)=elmqtu
      elmunt(i,igrp)=elmuut
      elmdes(i,igrp)=elmdus
      call nefout(elmnam(i,igrp),error)
      okee=error.eq.0
      if(elmnam(i,igrp).eq.'map-last')then
         error=getelt(deffds,grpdef(igrp),&
         &elmnam(i,igrp),uindex,usrord,&
         &4,maplast)
         okee=error.eq.0
      endif
10 continue
   GOTO 1111
9999 CONTINUE
end
