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
c     $Author: Markus $
c     $Date: 6-06-03 10:40 $
c     $Source: /u/cvsroot/gpp/libsrc/ods/tri_map.f,v $
c
c#ifdef WINNT
c$DEFINE GETELT_V8_I
c      INCLUDE '../include/nfsintrf.i'
c$UNDEFINE GETELT_V8_I
c      INTERFACE TO INTEGER FUNCTION GETELT_i [ALIAS:'_GETELT']
c     +                             ( VALUE1, VALUE2, VALUE3, VALUE4 ,
c     +                               VALUE5, VALUE6, VALUE7, VALUE8 )
c
c      INTEGER   VALUE1
c      INTEGER   VALUE2
c      CHARACTER VALUE3
c      CHARACTER VALUE4
c      INTEGER   VALUE5
c      INTEGER   VALUE6
c      INTEGER   VALUE7
c      CHARACTER VALUE8
c
c      END
c#endif

      subroutine ods_tri_nef_map_par
c#ifdef WINNT
c     *          [ALIAS:'_ods_tri_nef_map_par']
c#endif
     *                 (
     *                  fname , itype , pardef, maxdef, timdep, locdep,
     *                  maxlst, lang  , parlst, paruni, partyp, parcod,
     *                  nrlst , ierror, option                        )
c-----------------------------------------------------------------------
c         Function: parameter name selection for maps
c                   TRISULA NEFIS MAP files
c        Method used:
c-----------------------------------------------------------------------
c   Calling routine :              GETPAR
c-----------------------------------------------------------------------
c   Called  routines:              OPNNEF
c                                  CLOSFL
c                                  GETELT (nefis)
c                                  INQELM (nefis)
c-----------------------------------------------------------------------
c    Parameters:
c    -----------
c
c   Var.      Type Dimensions
c   -------------------------
c
c FNAME      CH*256 3          I   full name including path and ext.
c ITYPE       I*4              I   file type
c PARDEF     CH*21  maxdef     I   filter for required parameters
c MAXDEF      I*4              I   number of filters in PARDEF
c TIMDEP      I*4              I   dependency of time for the parameters to get
c LOCDEP      I*4              I   dependency of location for the parameters
c MAXLST      I*4              I   maximum number of parameters possible
c LANG        I*4              I   language code
c PARLST     CH*21  maxlst     O   names of parameters
c PARUNI     CH*21  maxlst     O   units of parameters
c PARTYP      I*40  maxlst     O   type of dependency of parameters
c PARCOD      I*40  maxlst     O   access index of parameter
c NRLST       I*4              O   number of parameters to be returned
c IERROR      I*4              O   = 0 no errors, = 1 error detected
c
c OPTION     CH*256 1         I/O  Option (not used )
c-----------------------------------------------------------------------
c          Constants:
c
c Const.      Type
c
c MXNPAR      I*4                  Maximum number of array-elements in
c                                  the local workarrays
c-----------------------------------------------------------------------
c    Local variables:
c    ----------------
c
c   Var.      Type Dimensions
c   -------------------------
c
c BUFLEN      I*4                  Size in bytes of available buffer
c CELDEF     CH*16                 Cell name definition
c ELMNAM     CH*16                 Element name definition
c FILHDA     CH*256                File name NEFIS data file for MAP
c FILHDE     CH*256                File name NEFIS definition file for
c                                  MAP
c GRPDEF     CH*16                 Group name definition
c HDAFDS      I*4  999             Data file descriptor for the MAP-DAT
c                                  file
c HDEFDS      I*4  2997            Definition file description for the
c                                  MAP-DEF file
c IERROR      I*4                  Error code for NEFIS error
c KMAX        I*4                  Number of layers
c LMAX    I   I*4                  Number of constituents
c                                  for old files LMAX = LSTCI
c                                  for new files LMAX = LSTCI + LTUR
c LSTCI   I   I*4                  Total number of constituents (incl.
c                                  turbulence for old trim files).
c LTUR    I   I*4                  Number of turbulence constituents
c NAMCON     CH*20                 Constituent names
c NOSTAT      I*4                  Number of defined stations
c NTRUV       I*4                  Number of cross-secties in u- and
c                                  v-direction
c MAPIND      I*4  MXNPAR          TRISULA-NEFIS dependant index list
c MAPLST     CH*20 MXNPAR          TRISULA-NEFIS possible parameterlist
c MAPTYP      I*4  MXNPAR          TRISULA-NEFIS possible code list
c MAPUNI     CH*20 MXNPAR          TRISULA-NEFIS possible unitlist
c SELMAP  I  CH*20                 Output flags containing Y or N for
c                                  various output quantities selection
c                                  for map files
c UINDEX      I*4  3               Array with indices of the cells to
c                                  be read
c USRORD      I*4                  Sequence in which the cells must be
c                                  read
c ZRHO        L*4                  if .true. then density included
c-----------------------------------------------------------------------
c
c  DECLARATIONS
c
      include 'ods.inc'
c
      integer         mxnpar
c
      parameter (mxnpar = 60)
c
      integer         lang
      integer         locdep
      integer         timdep,itype
      integer         maxdef,maxlst,i     ,npar  ,mmax  ,nmax
      integer         kmax  ,lmax  ,l     ,ind
      integer         lstci ,ltur  ,irho
      integer         ierror,nrlst
c
      integer         mapind(mxnpar)
      integer         maptyp(mxnpar)
      integer         partyp(maxlst)
      integer         parcod(maxlst)
c
      character       pardef(*)*(*)
      character       parlst(*)*(*)
      character       paruni(*)*(*)
      character*20    namcon(10    )
      character*20    maplst(mxnpar)
      character*20    mapuni(mxnpar)
      character*20    selmap
      character       fname(*)*(*)
      character*256   filhda,filhde
      character*256   option
c
      logical         ex    ,zrho  ,okee
c-----------------------------------------------------------------------
c-----declaration NEFIS
c-----------------------------------------------------------------------
      integer       hdefds( 2997),hdafds(  999)
c
      integer       uindex(    3),usrord,buflen

      character*16  grpdef,elmnam
c
      integer       GETELT,GETELS,INQELM
c#ifdef WINNT
      integer       GETELT_i
c#endif

      integer       TMLCDP
      parameter     ( TMLCDP = IPLMNK + IPTDEP )
c
      data (mapind(i),maplst(i),mapuni(i),maptyp(i),i=1,15,1)/
     *   1,'water level         ','m                   ',    TMLCDP,
     *   2,'total water depth   ','m                   ',    TMLCDP,
     *   3,'dpt. aver. cur. u   ','m/s                 ',    TMLCDP,
     *   4,'dpt. aver. cur. v   ','m/s                 ',    TMLCDP,
     *   5,'dpt. aver. cur. mag.','m/s                 ',    TMLCDP,
     *   6,'dpt. aver. cur. dir.','degrees             ',    TMLCDP,
     *   7,'current u           ','m/s                 ',    TMLCDP,
     *   8,'current v           ','m/s                 ',    TMLCDP,
     *   9,'current mag. (horiz)','m/s                 ',    TMLCDP,
     *  10,'current dir. (horiz)','degrees             ',    TMLCDP,
     *  11,'current w           ','m/s                 ',    TMLCDP,
     *  12,'z-coordinate        ','user defined        ',    TMLCDP,
     *  13,'flow rate u         ','m**3/s              ',    TMLCDP,
     *  14,'flow rate v         ','m**3/s              ',    TMLCDP,
     *  15,'eddy viscosity      ','m**2/s              ',    TMLCDP/
      data (mapind(i),maplst(i),mapuni(i),maptyp(i),i=16,30,1)/
     *  16,'bed stress u        ','N/m**2              ',    TMLCDP,
     *  17,'bed stress v        ','N/m**2              ',    TMLCDP,
     *  18,'bed stress mag.     ','N/m**2              ',    TMLCDP,
     *  19,'bed stress dir.     ','degrees             ',    TMLCDP,
     *  20,'constituent         ','user defined        ',    TMLCDP,
     *  21,'constituent         ','user defined        ',    TMLCDP,
     *  22,'constituent         ','user defined        ',    TMLCDP,
     *  23,'constituent         ','user defined        ',    TMLCDP,
     *  24,'constituent         ','user defined        ',    TMLCDP,
     *  25,'constituent         ','user defined        ',    TMLCDP,
     *  26,'constituent         ','user defined        ',    TMLCDP,
     *  27,'constituent         ','user defined        ',    TMLCDP,
     *  28,'constituent         ','user defined        ',    TMLCDP,
     *  29,'constituent         ','user defined        ',    TMLCDP,
     *  30,'                    ','                    ',    TMLCDP/
      data (mapind(i),maplst(i),mapuni(i),maptyp(i),i=31,45,1)/
     *  31,'density             ','kg/m**3             ',    TMLCDP,
     *  32,'eddy diffusivity    ','m**2/s              ',    TMLCDP,
     *  33,'                    ','                    ',    TMLCDP,
     *  34,'                    ','                    ',    TMLCDP,
     *  35,'active grid         ','-                   ',    TMLCDP,
     *  36,'boundary lines      ','-                   ',    TMLCDP,
     *  37,'temporary dry pnts. ','-                   ',    TMLCDP,
     *  38,'vectors dpth. aver. ','-                   ',    TMLCDP,
     *  39,'vectors bed stress  ','-                   ',    TMLCDP,
     *  40,'vectors velocity uv ','-                   ',    TMLCDP,
     *  41,'vectors velocity uw ','-                   ',    TMLCDP,
     *  42,'vectors velocity vw ','-                   ',    TMLCDP,
     *  43,'                    ','                    ',    TMLCDP,
     *  44,'dpt. at d_points    ','m                   ',    IPLMNK,
     *  45,'dpt. at z_points    ','m                   ',    IPLMNK/
      data (mapind(i),maplst(i),mapuni(i),maptyp(i),i=46,60,1)/
     *  46,'XCOR                ','user defined        ',    IPLMNK,
     *  47,'YCOR                ','user defined        ',    IPLMNK,
     *  48,'XZ                  ','user defined        ',    IPLMNK,
     *  49,'YZ                  ','user defined        ',    IPLMNK,
     *  50,'UVDAMS              ','-                   ',    TMLCDP,
     *  51,'ZCOR step           ','m                   ',    TMLCDP,
     *  52,'ZCOR slope          ','m                   ',    TMLCDP,
     *  53,'                    ','                    ',    TMLCDP,
     *  54,'                    ','                    ',    TMLCDP,
     *  55,'                    ','                    ',    TMLCDP,
     *  56,'                    ','                    ',    TMLCDP,
     *  57,'                    ','                    ',    TMLCDP,
     *  58,'                    ','                    ',    TMLCDP,
     *  59,'                    ','                    ',    IPLMNK,
     *  60,'                    ','                    ',    IPLMNK/

c-----------------------------------------------------------------------
c-----Initialisation
c-----------------------------------------------------------------------
      okee   = .true.
      zrho   = .false.
c
      ierror =  0
c
c--------------------------------------------------------------------
c-----Test if trim-<runid>.dat and trim-<runid>.def Nefis MAP-files
c     exist
c--------------------------------------------------------------------
      call ods_check_nefis( fname  , '.def' , ierror )
      if ( ierror .ne. ieok   ) then
         return
      endif
c--------------------------------------------------------------------
c-----Open trim-<runid>.dat and trim-<runid>.def MAP-files
c--------------------------------------------------------------------
      call OPNNEF(fname, itype, hdafds, hdefds, ierror)
      if (ierror .ne. 0) then
         ierror = IEFIRO
         return
      endif
c--------------------------------------------------------------------
c-----Read array-dimensions from Nefis MAP-files group 2
c--------------------------------------------------------------------
      grpdef = 'map-const'
c
      uindex(1) = 1
      uindex(2) = 1
      uindex(3) = 1
      usrord    = 1
      buflen    = 4
c
      npar      = 0
      mmax      = 0
      nmax      = 0
      lmax      = 0
      lstci     = 0
      ltur      = 0
      irho      = 0
      kmax      = 0
c
      elmnam    = 'MMAX'
      okee = okee .and.
     *       GETELT(hdefds,grpdef    ,elmnam    ,
     *              uindex,usrord    ,buflen    ,MMAX      )
     *       .eq. 0
c
      elmnam    = 'NMAX'
      okee = okee .and.
     *       GETELT(hdefds,grpdef    ,elmnam    ,
     *              uindex,usrord    ,buflen    ,NMAX      )
     *       .eq. 0
c
c-----------------------------------------------------------------------
c--------Read element LMAX; LMAX only defined in old trim files
c        hence if not defined ierror = -25041
c-----------------------------------------------------------------------
         elmnam    = 'LMAX'
         ierror    = GETELT(hdefds,grpdef    ,elmnam    ,
     *                      uindex,usrord    ,buflen    ,LSTCI     )
         if (ierror .ne. 0) then
c-----------------------------------------------------------------------
c-----------In case of a new trih file read LSTCI and LTUR
c-----------------------------------------------------------------------
            elmnam = 'LSTCI'
            ierror = GETELT(hdefds,grpdef    ,elmnam    ,
     *                      uindex,usrord    ,buflen    ,LSTCI     )
            if (ierror .ne. 0) then
               okee   = .false.
               goto 8888
            endif
c
            elmnam = 'LTUR'
            ierror = GETELT(hdefds,grpdef    ,elmnam    ,
     *                      uindex,usrord    ,buflen    ,LTUR      )
            if (ierror .ne. 0) then
               okee   = .false.
               goto 8888
            endif
         endif
c
         lmax = lstci + ltur
c
         elmnam = 'KMAX'
         ierror = GETELT(hdefds,grpdef    ,elmnam    ,
     *                   uindex,usrord    ,buflen    ,KMAX      )
         if (ierror .ne. 0) then
            okee   = .false.
            goto 8888
         endif
c
c-----------------------------------------------------------------------
c-----Element SELMAP selection of output
c-----------------------------------------------------------------------
      buflen    = 20
      elmnam    = 'SELMAP'
c#ifdef WINNT
c     ierror    = GETELT_i
c#else
      ierror    = GETELS
c#endif
     *                  (hdefds   ,grpdef    ,elmnam    ,
     *                   uindex   ,usrord    ,buflen    ,SELMAP    )
      if (ierror .ne. 0) then
c-----------------------------------------------------------------------
c--------In case of a old trim file no SELMAP then
c           re-define SELMAP like definition in subroutine RDPRFL
c-----------------------------------------------------------------------
         selmap = 'YYYYYYYYYYYYYYYYYYYY'
         if (kmax   .eq. 1) selmap( 4: 5) = 'NN'
         if (lstci  .eq. 0) selmap( 6:13) = 'NNNNNNNN'
         if (ltur   .eq. 0) selmap(14:15) = 'NN'
         if (kmax   .eq. 1) selmap(18:19) = 'NN'
         if (lmax   .eq. 0) selmap(19:19) = 'N'
         selmap(20:20) = 'X'
      endif
c-----------------------------------------------------------------------
c-----Element NAMCON constituents and turbulence quantity names
c     only if selmap( 6:15) <> 'NNNNNNNNNN'
c-----------------------------------------------------------------------
      if (index (selmap( 6:15),'Y') .gt. 0) then
         buflen    = 20 * lmax
         elmnam    = 'NAMCON'
c#ifdef WINNT
c        ierror    = GETELT_i
c#else
         ierror    = GETELS
c#endif
     *                     (hdefds,grpdef    ,elmnam    ,
     *                      uindex,usrord    ,buflen    ,NAMCON    )
         if (ierror .ne. 0) then
            okee   = .false.
            goto 8888
         endif
c
         do 110 l = 1,lmax
            if ( selmap(5+l:5+l) .eq. 'Y' ) then
               if (namcon(l)(:11) .eq. 'Salinity   ') irho   = 1
               if (namcon(l)(:11) .eq. 'Temperature') irho   = 1
            endif
  110    continue
      endif
c-----------------------------------------------------------------------
c-----In case of a old trim file re-define SELMAP(20:20)
c-----------------------------------------------------------------------
      if (selmap(20:20) .eq. 'X') then
         selmap(20:20) = 'N'
         if (irho   .eq. 1) selmap(20:20) = 'Y'
      endif
c
      zrho = irho .eq. 1
c
c--------------------------------------------------------------------
c-----Generate parameternames from Nefis MAP-files
c--------------------------------------------------------------------
c-----water level, ZWL
      if ( selmap(1:1) .eq. 'Y' ) then
         npar = npar + 1
         parcod(npar) = mapind( 1)
         parlst(npar) = maplst(parcod(npar))
         partyp(npar) = maptyp(parcod(npar))
         paruni(npar) = mapuni(parcod(npar))
c-----water depth, ZWL + DPS
         npar = npar + 1
         parcod(npar) = mapind( 2)
         parlst(npar) = maplst(parcod(npar))
         partyp(npar) = maptyp(parcod(npar))
         paruni(npar) = mapuni(parcod(npar))
      endif
      if ( selmap(2:3) .eq. 'YY' ) then
         if (kmax .gt. 1) then
c--------dpt. aver. cur. u
            npar = npar + 1
            parcod(npar) = mapind( 3)
            parlst(npar) = maplst(parcod(npar))
            partyp(npar) = maptyp(parcod(npar))
            paruni(npar) = mapuni(parcod(npar))
c--------dpt. aver. cur. v
            npar = npar + 1
            parcod(npar) = mapind( 4)
            parlst(npar) = maplst(parcod(npar))
            partyp(npar) = maptyp(parcod(npar))
            paruni(npar) = mapuni(parcod(npar))
c--------dpt. aver. cur. mag.
            npar = npar + 1
            parcod(npar) = mapind( 5)
            parlst(npar) = maplst(parcod(npar))
            partyp(npar) = maptyp(parcod(npar))
            paruni(npar) = mapuni(parcod(npar))
c--------dpt. aver. cur. dir.
            npar = npar + 1
            parcod(npar) = mapind( 6)
            parlst(npar) = maplst(parcod(npar))
            partyp(npar) = maptyp(parcod(npar))
            paruni(npar) = mapuni(parcod(npar))
         endif

c-----current u (layer)
         npar = npar + 1
         parcod(npar) = mapind( 7)
         parlst(npar) = maplst(parcod(npar))
         partyp(npar) = maptyp(parcod(npar))
         paruni(npar) = mapuni(parcod(npar))
c-----current v (layer)
         npar = npar + 1
         parcod(npar) = mapind( 8)
         parlst(npar) = maplst(parcod(npar))
         partyp(npar) = maptyp(parcod(npar))
         paruni(npar) = mapuni(parcod(npar))
c-----current mag. (layer)
         npar = npar + 1
         parcod(npar) = mapind( 9)
         parlst(npar) = maplst(parcod(npar))
         partyp(npar) = maptyp(parcod(npar))
         paruni(npar) = mapuni(parcod(npar))
c-----current dir. (layer)
         npar = npar + 1
         parcod(npar) = mapind(10)
         parlst(npar) = maplst(parcod(npar))
         partyp(npar) = maptyp(parcod(npar))
         paruni(npar) = mapuni(parcod(npar))
      endif

      if ( selmap(5:5) .eq. 'Y' ) then
         if (kmax .gt. 1) then
c--------current w.   (layer)
            npar = npar + 1
            parcod(npar) = mapind(11)
            parlst(npar) = maplst(parcod(npar))
            partyp(npar) = maptyp(parcod(npar))
            paruni(npar) = mapuni(parcod(npar))
         endif
      endif
c--------z-coordinate (can always be defined: 2D or 3D)
c     if (kmax .gt. 1) then
         npar = npar + 1
         parcod(npar) = mapind(12)
         parlst(npar) = maplst(parcod(npar))
         partyp(npar) = maptyp(parcod(npar))
         paruni(npar) = mapuni(parcod(npar))
c     endif

c-----flow rate u, ZQXK           (not yet implemented)
c     npar = npar + 1
c     parcod(npar) = mapind(13)
c     parlst(npar) = maplst(parcod(npar))
c     partyp(npar) = maptyp(parcod(npar))
c     paruni(npar) = mapuni(parcod(npar))
c-----flow rate v, ZQYK           (not yet implemented)
c     npar = npar + 1
c     parcod(npar) = mapind(14)
c     parlst(npar) = maplst(parcod(npar))
c     partyp(npar) = maptyp(parcod(npar))
c     paruni(npar) = mapuni(parcod(npar))

      if ( selmap(18:18) .eq. 'Y' ) then
         if (kmax .gt. 1) then
c--------eddy viscosity, ZVICWW
            npar = npar + 1
            parcod(npar) = mapind(15)
            parlst(npar) = maplst(parcod(npar))
            partyp(npar) = maptyp(parcod(npar))
            paruni(npar) = mapuni(parcod(npar))
         endif
      endif

      if ( selmap(16:17) .eq. 'YY' ) then
c-----bed stress u, ZTAUKSI
         npar = npar + 1
         parcod(npar) = mapind(16)
         parlst(npar) = maplst(parcod(npar))
         partyp(npar) = maptyp(parcod(npar))
         paruni(npar) = mapuni(parcod(npar))
c-----bed stress v, ZTAUETA
         npar = npar + 1
         parcod(npar) = mapind(17)
         parlst(npar) = maplst(parcod(npar))
         partyp(npar) = maptyp(parcod(npar))
         paruni(npar) = mapuni(parcod(npar))
c-----bed stress mag, ZTAUETA
         npar = npar + 1
         parcod(npar) = mapind(18)
         parlst(npar) = maplst(parcod(npar))
         partyp(npar) = maptyp(parcod(npar))
         paruni(npar) = mapuni(parcod(npar))
c-----bed stress dir, ZTAUETA
         npar = npar + 1
         parcod(npar) = mapind(19)
         parlst(npar) = maplst(parcod(npar))
         partyp(npar) = maptyp(parcod(npar))
         paruni(npar) = mapuni(parcod(npar))
      endif

      if (lmax   .gt. 0) then
c--------constituents, GRO(1:lstci), ZTUR(1:ltur)
         do 10 l = 1, lmax, 1
            if ( selmap(5+l:5+l) .eq. 'Y' ) then
               npar = npar + 1
               parcod(npar) = mapind(19+l)
               parlst(npar) = namcon(l)(1:20)
               partyp(npar) = maptyp(parcod(npar))
               if (parlst(npar) .eq. 'Salinity') then
                  paruni(npar) = 'ppt'
               else if (parlst(npar) .eq. 'Temperature') then
                  paruni(npar) = 'degrees C'
               else
                  paruni(npar) = mapuni(parcod(npar))
               endif
            endif
   10    continue

         if (zrho) then
c-----------density, ZRHO
            npar = npar + 1
            parcod(npar) = mapind(31)
            parlst(npar) = maplst(parcod(npar))
            partyp(npar) = maptyp(parcod(npar))
            paruni(npar) = mapuni(parcod(npar))
         endif

         if ( selmap(19:19) .eq. 'Y' ) then
            if (kmax   .gt. 1) then
c-----------eddy diffusivity, ZDICWW
               npar = npar + 1
               parcod(npar) = mapind(32)
               parlst(npar) = maplst(parcod(npar))
               partyp(npar) = maptyp(parcod(npar))
               paruni(npar) = mapuni(parcod(npar))
            endif
         endif
      endif
c-----the following parameters are not continuous quantities:
c     they require a different approach
c-----active grid
c     npar = npar + 1
c     parcod(npar) = mapind(35)
c     parlst(npar) = maplst(parcod(npar))
c     partyp(npar) = maptyp(parcod(npar))
c     paruni(npar) = mapuni(parcod(npar))
c-----boundary lines
c     npar = npar + 1
c     parcod(npar) = mapind(36)
c     parlst(npar) = maplst(parcod(npar))
c     partyp(npar) = maptyp(parcod(npar))
c     paruni(npar) = mapuni(parcod(npar))
c-----temporary dry points
c     npar = npar + 1
c     parcod(npar) = mapind(37)
c     parlst(npar) = maplst(parcod(npar))
c     partyp(npar) = maptyp(parcod(npar))
c     paruni(npar) = mapuni(parcod(npar))

c     if ( selmap(1:3) .eq. 'YYY' ) then
c        if (kmax .gt. 1) then
c--------vectors depth average velocity
c           npar = npar + 1
c           parcod(npar) = mapind(38)
c           parlst(npar) = maplst(parcod(npar))
c           partyp(npar) = maptyp(parcod(npar))
c        paruni(npar) = mapuni(parcod(npar))
c--------vectors velocity uv           (not useful in current ODS setup!)
c           npar = npar + 1
c           parcod(npar) = mapind(40)
c           parlst(npar) = maplst(parcod(npar))
c           partyp(npar) = maptyp(parcod(npar))
c           paruni(npar) = mapuni(parcod(npar))
c--------vectors velocity uw           (not yet implemented)
c           npar = npar + 1
c           parcod(npar) = mapind(41)
c           parlst(npar) = maplst(parcod(npar))
c           partyp(npar) = maptyp(parcod(npar))
c           paruni(npar) = mapuni(parcod(npar))
c--------vectors velocity vw           (not yet implemented)
c           npar = npar + 1
c           parcod(npar) = mapind(42)
c           parlst(npar) = maplst(parcod(npar))
c           partyp(npar) = maptyp(parcod(npar))
c           paruni(npar) = mapuni(parcod(npar))
c        endif
c     endif
c-----vectors bed stress
c     if ( selmap(16:17) .eq. 'YY' ) then
c        npar = npar + 1
c        parcod(npar) = mapind(39)
c        parlst(npar) = maplst(parcod(npar))
c        partyp(npar) = maptyp(parcod(npar))
c        paruni(npar) = mapuni(parcod(npar))
c     endif

c-----dpt. at d_points
      npar = npar + 1
      parcod(npar) = mapind(44)
      parlst(npar) = maplst(parcod(npar))
      partyp(npar) = maptyp(parcod(npar))
      paruni(npar) = mapuni(parcod(npar))
c-----dpt. at z_points
      npar = npar + 1
      parcod(npar) = mapind(45)
      parlst(npar) = maplst(parcod(npar))
      partyp(npar) = maptyp(parcod(npar))
      paruni(npar) = mapuni(parcod(npar))
c-----x coordinate at depth location, XCOR
      npar = npar + 1
      parcod(npar) = mapind(46)
      parlst(npar) = maplst(parcod(npar))
      partyp(npar) = maptyp(parcod(npar))
      paruni(npar) = mapuni(parcod(npar))
c-----y coordinate at depth location, YCOR
      npar = npar + 1
      parcod(npar) = mapind(47)
      parlst(npar) = maplst(parcod(npar))
      partyp(npar) = maptyp(parcod(npar))
      paruni(npar) = mapuni(parcod(npar))
c-----x coordinate at zeta location, XZ
      npar = npar + 1
      parcod(npar) = mapind(48)
      parlst(npar) = maplst(parcod(npar))
      partyp(npar) = maptyp(parcod(npar))
      paruni(npar) = mapuni(parcod(npar))
c-----y coordinate at zeta location, YZ
      npar = npar + 1
      parcod(npar) = mapind(49)
      parlst(npar) = maplst(parcod(npar))
      partyp(npar) = maptyp(parcod(npar))
      paruni(npar) = mapuni(parcod(npar))
c-----grid attribute, UVDAMS
      npar = npar + 1
      parcod(npar) = mapind(50)
      parlst(npar) = maplst(parcod(npar))
      partyp(npar) = maptyp(parcod(npar))
      paruni(npar) = mapuni(parcod(npar))
c-----z coordinate, ZCOR step     (not yet implemented)
c     npar = npar + 1
c     parcod(npar) = mapind(51)
c     parlst(npar) = maplst(parcod(npar))
c     partyp(npar) = maptyp(parcod(npar))
c     paruni(npar) = mapuni(parcod(npar))
c-----z coordinate, ZCOR slope    (not yet implemented)
c     npar = npar + 1
c     parcod(npar) = mapind(52)
c     parlst(npar) = maplst(parcod(npar))
c     partyp(npar) = maptyp(parcod(npar))
c     paruni(npar) = mapuni(parcod(npar))
c
      nrlst = npar
c
c-----------------------------------------------------------------------
c-----check required and found number of parameter names
c-----------------------------------------------------------------------
      okee = okee .and. (maxlst .eq. npar)
c--------------------------------------------------------------------
c-----Close trim-<runid>.dat and trim-<runid>.def MAP-files
c--------------------------------------------------------------------
 8888 continue
      call CLOSFL(fname, ierror)
      okee = okee .and. (ierror .eq. 0)
c-----------------------------------------------------------------------
c-----return status to calling routine
c-----------------------------------------------------------------------
      ierror = IEOK
      if (.not. okee) then
         ierror = IEOTHR
      endif
c
      return
c-----------------------------------------------------------------------
      end

      subroutine ods_tri_nef_map_dim
c#ifdef WINNT
c    *          [ALIAS:'_ods_tri_nef_map_dim']
c#endif
     *                  (
     *                   fname ,itype ,dimtyp, pardep, timdep, locdep,
     *                   ndim  ,ierror, option                       )
c-----------------------------------------------------------------------
c         Function: dimension selection for maps
c                   TRISULA NEFIS MAP files
c
c-----------------------------------------------------------------------
c   Calling routine :              GETPAR
c-----------------------------------------------------------------------
c   Called  routines:              OPNNEF
c                                  CLOSFL
c                                  GETELT (nefis)
c                                  INQGRP (nefis)
c-----------------------------------------------------------------------
c    Parameters:
c    -----------
c
c   Var.      Type Dimensions
c   -------------------------
c
c FNAME      CH*256 3          I   full name including path and ext.
c ITYPE       I*4              I   file type
c DIMTYP     CH*3              I   filter required dimension par,tim,loc
c PARDEP      I*4              I   parameter dependency type
c TIMDEP      I*4              I        time dependency type
c LOCDEP      I*4              I    location dependency type
c NDIM        I*4   4          O   returned dimensions
c IERROR      I*4              O   = 0 no errors, = 1 error detected
c OPTION     CH*256           I/O  option (not used)
c-----------------------------------------------------------------------
c          Constants:
c
c Const.      Type
c
c-----------------------------------------------------------------------
c    Local variables:
c    ----------------
c
c   Var.      Type Dimensions
c   -------------------------
c
c BUFLEN      I*4                  Size in bytes of available buffer
c CELDEF     CH*16                 Cell name definition
c ELMNAM     CH*16                 Element name definition
c OKEE        L*4                  Flag for error reqocnition
c FILHDA     CH*256                File name NEFIS data file for MAP
c FILHDE     CH*256                File name NEFIS definition file for
c                                  MAP
c GRPDEF     CH*16                 Group name definition
c GRPDMS      I*4  5               Array with GRPNDM dimensions
c GRPNDM      I*4                  Number of dimenmsions of tmap group
c GRPORD      I*4  5               Array which gives order in which data
c                                  must be read
c HDAFDS      I*4  999             Data file descriptor for the MAP-DAT
c                                  file
c HDEFDS      I*4  2997            Definition file description for the
c                                  MAP-DEF file
c IERROR      I*4                  Error code for NEFIS error
c KMAX        I*4                  Number of layers
c L           I*4                  Help variable
c LMAX    I   I*4                  Number of constituents
c                                  for old files LMAX = LSTCI
c                                  for new files LMAX = LSTCI + LTUR
c LSTCI   I   I*4                  Total number of constituents (incl.
c                                  turbulence for old trim files).
c LTUR    I   I*4                  Number of turbulence constituents
c NPAR        I*4                  Number of found parameters
c NRCEL       I*4                  Number of cells defined in group 1&3
c SELMAP  I  CH*20                 Output flags containing Y or N for
c                                  various output quantities selection
c                                  for map files
c UINDEX      I*4  3               Array with indices of the cells to
c                                  be read
c USRORD      I*4                  Sequence in which the cells must be
c                                  read
c ZRHO        L*4                  if .true. then density included
c-----------------------------------------------------------------------
c
c  DECLARATIONS
c
      include 'ods.inc'
c
      integer         ierror,itype
      integer         nrcel ,npar  ,lmax  ,kmax  ,l
      integer         mmax  ,nmax
      integer         pardep,timdep,locdep
      integer         lstci ,ltur  ,irho
      integer         ndim   (4    )
c
      character*3     dimtyp
      character*20    selmap,namcon(10)
      character       fname(*)*(*)
      character*256   filhda,filhde
      character*256   option
c
      logical         ex    ,okee  ,zrho  ,chk
c-----------------------------------------------------------------------
c-----declaration NEFIS
c-----------------------------------------------------------------------
      integer       hdefds( 2997),hdafds(  999)
c
      integer       grpndm,grpdms(    5),grpord(    5),
     *              uindex(    3),usrord,buflen       ,
     *              ind
c
      character*16  grpdef,celnam,elmnam
c
      integer       INQGRP,GETELT,GETELS
      integer       INQMXI
c#ifdef WINNT
      integer       GETELT_i
c#endif

c-----------------------------------------------------------------------
c-----Initialisation
c-----------------------------------------------------------------------
      okee   = .true.
      zrho   = .false.
c
      ierror =  0
c
c--------------------------------------------------------------------
c-----Test if trim-<runid>.dat and trim-<runid>.def Nefis MAP-files
c     exist
c--------------------------------------------------------------------

C     ind = index ( fname(1), char(0))
C     if ( ind .eq. 0 ) then
C        filhda = fname(1)
C     else
C        filhda = fname(1)(1:ind-1)
C     endif
C     inquire (file=filhda,exist=ex)
C     if (.not.ex) then
C        ierror = IENOFI
C        return
C     endif
c
C     ind = index ( fname(2), char(0))
C     if ( ind .eq. 0 ) then
C        filhde = fname(2)
C     else
C        filhde = fname(2)(1:ind-1)
C     endif
C     inquire (file=filhde,exist=ex)
C     if (.not.ex) then
C        ierror = IENOFI
C        return
C     endif
      call ods_check_nefis( fname  , '.def' , ierror )
      if ( ierror .ne. ieok   ) then
         return
      endif
c--------------------------------------------------------------------
c-----Open trim-<runid>.dat and trim-<runid>.def MAP-files
c--------------------------------------------------------------------
      call OPNNEF(fname, itype, hdafds, hdefds, ierror)
      if (ierror .ne. 0) then
         ierror = IEFIRO
         return
      endif
c--------------------------------------------------------------------
c-----Read array-dimension nrcel from Nefis MAP-files group 1
c--------------------------------------------------------------------
      grpdef = 'map-info-series'
      grpndm = 5
      celnam = grpdef
      ierror = INQGRP(hdefds   ,grpdef    ,celnam    ,grpndm    ,
     *                grpdms   ,grpord                          )
      nrcel  = grpdms(1)
c--------------------------------------------------------------------
c-----Test value of nrcel if nrcel = 0 then get nrcel with INQMXI
c--------------------------------------------------------------------
      if (nrcel  .eq. 0) then
         ierror    = INQMXI(hdefds,grpdef    ,nrcel     )
         okee = okee .and. (ierror .eq. 0)
         if (nrcel  .eq. 0) then
            okee = .false.
            goto 8888
         endif
      endif
c--------------------------------------------------------------------
c-----Read array-dimensions from Nefis MAP-files group 2
c--------------------------------------------------------------------
      grpdef = 'map-const'
c
      uindex(1) = 1
      uindex(2) = 1
      uindex(3) = 1
      usrord    = 1
      buflen    = 4
c
      npar      = 0
      mmax      = 0
      nmax      = 0
      lmax      = 0
      lstci     = 0
      ltur      = 0
      irho      = 0
      kmax      = 0
c
      elmnam    = 'MMAX'
      okee = okee .and.
     *       GETELT(hdefds,grpdef    ,elmnam    ,
     *              uindex,usrord    ,buflen    ,MMAX      )
     *       .eq. 0
c
      elmnam    = 'NMAX'
      okee = okee .and.
     *       GETELT(hdefds,grpdef    ,elmnam    ,
     *              uindex,usrord    ,buflen    ,NMAX      )
     *       .eq. 0
c
c--------------------------------------------------------------------
c-----Read element LMAX; LMAX only defined in old trim files
c     hence if not defined ierror = -25041
c--------------------------------------------------------------------
      elmnam    = 'LMAX'
      ierror    = GETELT(hdefds,grpdef    ,elmnam    ,
     *                   uindex,usrord    ,buflen    ,LSTCI     )
      if (ierror .ne. 0) then
c--------------------------------------------------------------------
c--------In case of a new trih file read LSTCI and LTUR
c--------------------------------------------------------------------
         elmnam = 'LSTCI'
         ierror = GETELT(hdefds,grpdef    ,elmnam    ,
     *                   uindex,usrord    ,buflen    ,LSTCI     )
         if (ierror .ne. 0) then
            okee = .false.
            goto 8888
         endif
c
         elmnam = 'LTUR'
         ierror = GETELT(hdefds,grpdef    ,elmnam    ,
     *                   uindex,usrord    ,buflen    ,LTUR      )
         if (ierror .ne. 0) then
            okee = .false.
            goto 8888
         endif
      endif
c
      lmax = lstci + ltur
c
      elmnam = 'KMAX'
      ierror = GETELT(hdefds,grpdef    ,elmnam    ,
     *                uindex,usrord    ,buflen    ,KMAX      )
      if (ierror .ne. 0) then
         okee = .false.
         goto 8888
      endif
c
c-----------------------------------------------------------------------
c-----Element SELMAP selection of output
c-----------------------------------------------------------------------
      buflen    = 20
      elmnam    = 'SELMAP'
c#ifdef WINNT
c     ierror    = GETELT_i
c#else
      ierror    = GETELS
c#endif
     *                  (hdefds   ,grpdef    ,elmnam    ,
     *                   uindex   ,usrord    ,buflen    ,SELMAP    )
      if (ierror .ne. 0) then
c-----------------------------------------------------------------------
c--------In case of a old trim file no SELMAP then
c           re-define SELMAP like definition in subroutine RDPRFL
c-----------------------------------------------------------------------
         selmap = 'YYYYYYYYYYYYYYYYYYYY'
         if (kmax   .eq. 1) selmap( 4: 5) = 'NN'
         if (lstci  .eq. 0) selmap( 6:13) = 'NNNNNNNN'
         if (ltur   .eq. 0) selmap(14:15) = 'NN'
         if (kmax   .eq. 1) selmap(18:19) = 'NN'
         if (lmax   .eq. 0) selmap(19:19) = 'N'
         selmap(20:20) = 'X'
      endif
c-----------------------------------------------------------------------
c-----Element NAMCON constituents and turbulence quantity names
c     only if selmap( 6:15) <> 'NNNNNNNNNN'
c-----------------------------------------------------------------------
      if (index (selmap( 6:15),'Y') .gt. 0) then
         buflen    = 20 * lmax
         elmnam    = 'NAMCON'
c#ifdef WINNT
c        ierror    = GETELT_i
c#else
         ierror    = GETELS
c#endif
     *                     (hdefds,grpdef    ,elmnam    ,
     *                      uindex,usrord    ,buflen    ,NAMCON    )
         if (ierror .ne. 0) then
            okee = .false.
            goto 8888
         endif
c
         do 110 l = 1,lmax
            if ( selmap(5+l:5+l) .eq. 'Y' ) then
               if (namcon(l)(:11) .eq. 'Salinity   ') irho   = 1
               if (namcon(l)(:11) .eq. 'Temperature') irho   = 1
            endif
  110    continue
      endif
c-----------------------------------------------------------------------
c-----In case of a old trim file re-define SELMAP(20:20)
c-----------------------------------------------------------------------
      if (selmap(20:20) .eq. 'X') then
         selmap(20:20) = 'N'
         if (irho   .eq. 1) selmap(20:20) = 'Y'
      endif
c
      zrho = irho .eq. 1
c
c--------------------------------------------------------------------
c-----Generate parameternames from Nefis MAP-files
c--------------------------------------------------------------------
      if ( selmap(1:1) .eq. 'Y' ) then
c-----water level, ZWL                               pardep =  1
         npar = npar + 1
c-----water depth, ZWL + DPS                         pardep =  2
         npar = npar + 1
      endif
      if ( selmap(2:3) .eq. 'YY' ) then
         if (kmax .gt. 1) then
c--------dpt. aver. cur. u                           pardep =  3
            npar = npar + 1
c--------dpt. aver. cur. v                           pardep =  4
            npar = npar + 1
c--------dpt. aver. cur. mag.                        pardep =  5
            npar = npar + 1
c--------dpt. aver. cur. dir.                        pardep =  6
            npar = npar + 1
c--------vectors dpt. aver.                          pardep = 38
c           npar = npar + 1
c--------vectors velocity uv (layer)(not useful)     pardep = 40
c           npar = npar + 1
c--------vectors velocity uw (layer)(not yet implemented)ep = 41
c           npar = npar + 1
c--------vectors velocity vw (layer)(not yet implemented)ep = 42
c           npar = npar + 1
         endif

c-----current u (layer)                              pardep =  7
         npar = npar + 1
c-----current v (layer)                              pardep =  8
         npar = npar + 1
c-----current mag. (layer)                           pardep =  9
         npar = npar + 1
c-----current dir. (layer)                           pardep = 10
         npar = npar + 1
      endif

      if ( selmap(5:5) .eq. 'Y' ) then
         if (kmax .gt. 1) then
c--------current w.   (layer)                        pardep = 11
            npar = npar + 1
         endif
      endif
c--------z-coordinate                                pardep = 12
c     if (kmax .gt. 1) then
         npar = npar + 1
c     endif

c-----flow rate u, ZQXK   (not yet implemented)      pardep = 13
c     npar = npar + 1
c-----flow rate v, ZQYK   (not yet implemented)      pardep = 14
c     npar = npar + 1

      if ( selmap(18:18) .eq. 'Y' ) then
         if (kmax .gt. 1) then
c--------eddy viscosity, ZVICWW  (layer)             pardep = 15
            npar = npar + 1
         endif
      endif

      if ( selmap(16:17) .eq. 'YY' ) then
c-----bed stress u, ZTAUKSI                          pardep = 16
         npar = npar + 1
c-----bed stress v, ZTAUETA                          pardep = 17
         npar = npar + 1
c-----bed stress mag, ZTAUETA                        pardep = 18
         npar = npar + 1
c-----bed stress dir, ZTAUETA                        pardep = 19
         npar = npar + 1
      endif

      if (lmax   .gt. 0) then
c--------constituents, GRO (1:lstci) (layer)         pardep = 20..29
c        constituents, ZTUR(1:ltur ) (layer)         pardep = 20..29
         do 10 l = 1, lmax, 1
            if ( selmap(5+l:5+l) .eq. 'Y' ) then
               npar = npar + 1
            endif
   10    continue

         if (zrho) then
c-----------density, ZRHO  (layer)                   pardep = 31
            npar = npar + 1
         endif

         if ( selmap(19:19) .eq. 'Y' ) then
            if (kmax   .gt. 1) then
c-----------eddy diffusivity, ZDICWW  (layer)        pardep = 32
               npar = npar + 1
            endif
         endif
      endif

c-----the following parameters are not continuous quantities:
c     they require a different approach
c-----active grid                                    pardep = 35
c     npar = npar + 1
c-----boundary lines                                 pardep = 36
c     npar = npar + 1
c-----temporary dry points                           pardep = 37
c     npar = npar + 1
c-----vectors bed stress                             pardep = 39
c     npar = npar + 1

c-----dpt. at d_points                               pardep = 44
      npar = npar + 1
c-----dpt. at z_points                               pardep = 45
      npar = npar + 1
c-----x coordinate at depth location, XCOR           pardep = 46
      npar = npar + 1
c-----y coordinate at depth location, YCOR           pardep = 47
      npar = npar + 1
c-----x coordinate at zeta location, XZ              pardep = 48
      npar = npar + 1
c-----y coordinate at zeta location, YZ              pardep = 49
      npar = npar + 1
c-----grid attribute, UVDAMS                         pardep = 50
      npar = npar + 1
c-----z coordinate, ZCOR step     (not yet implemented)rdep = 51
c     npar = npar + 1
c-----z coordinate, ZCOR slope    (not yet implemented)rdep = 52
c     npar = npar + 1

c-----------------------------------------------------------------------
c-----return required dimension
c-----------------------------------------------------------------------
      if (dimtyp(1:3) .eq. 'par') then
         ndim (1) = 1
         ndim (2) = npar
      else if (dimtyp(1:3) .eq. 'tim') then
         ndim (1) = 1
         ndim (2) = nrcel
      else if (dimtyp(1:3) .eq. 'loc') then
         if ( pardep .ge. 38 .and. pardep .le. 39 ) then
c-----------vectors (u, v componen) with one layer
            ndim (1) = 3
            ndim (2) = mmax
            ndim (3) = nmax
            ndim (4) = 2
         else if ( pardep .ge. 40 .and. pardep .le. 42 ) then
c-----------vectors (u, v componen) with possibly more than one layer
            if ( kmax .gt. 1 ) then
               ndim (1) = 3
               ndim (2) = mmax
               ndim (3) = nmax
               ndim (4) = kmax * 2
            else
               ndim (1) = 3
               ndim (2) = mmax
               ndim (3) = nmax
               ndim (4) = 2
            endif
         endif
         chk = ( pardep .ge.  1 .and. pardep .le.  6 ) .or.
     *         ( pardep .ge. 13 .and. pardep .le. 14 ) .or.
     *         ( pardep .ge. 16 .and. pardep .le. 19 ) .or.
     *         ( pardep .ge. 35 .and. pardep .le. 37 ) .or.
     *         ( pardep .ge. 44 .and. pardep .le. 50 )
c--------chk = .true.  then 2 dimensional (1-6,13-14,16-19,35-37,44-50)
c              .false. then poss. 3 dimensional (7-11,12,15,20-29,31,32)
         if ( chk ) then
            ndim (1) = 2
            ndim (2) = mmax
            ndim (3) = nmax
         else
            if ( pardep .eq. 15 .or. pardep .eq. 32 ) then
c--------------ZVICWW and ZDICWW with layers (0:kmax)
                  ndim (1) = 3
                  ndim (2) = mmax
                  ndim (3) = nmax
                  ndim (4) = kmax + 1
            else if ( pardep .eq. 12 ) then
c--------------z-coordinate
                  ndim (1) = 3
                  ndim (2) = mmax
                  ndim (3) = nmax
                  ndim (4) = kmax + 1
            else if ( pardep .ge. 20 .and. pardep .le. 29 ) then
c--------------constituents or turbulence v245 or later
               if ( pardep-19 .gt. lstci ) then
c-----------------turbulence with layers (0:kmax)
                  ndim (1) = 3
                  ndim (2) = mmax
                  ndim (3) = nmax
                  ndim (4) = kmax + 1
               else
c-----------------constituents or turbulence with layers (1:kmax)
c                 v240 or before
                  if ( kmax .gt. 1 ) then
                     ndim (1) = 3
                     ndim (2) = mmax
                     ndim (3) = nmax
                     ndim (4) = kmax
                  else
                     ndim (1) = 2
                     ndim (2) = mmax
                     ndim (3) = nmax
                  endif
               endif
            else
               if ( kmax .gt. 1 ) then
                  ndim (1) = 3
                  ndim (2) = mmax
                  ndim (3) = nmax
                  ndim (4) = kmax
               else
                  ndim (1) = 2
                  ndim (2) = mmax
                  ndim (3) = nmax
               endif
            endif
         endif
      else
         okee = .false.
      endif
c--------------------------------------------------------------------
c-----Close trim-<runid>.dat and trim-<runid>.def MAP-files
c--------------------------------------------------------------------
 8888 continue
      call CLOSFL(fname, ierror)
      okee = okee .and. (ierror .eq. 0)
c-----------------------------------------------------------------------
c-----return status to calling routine
c-----------------------------------------------------------------------
      ierror = IEOK
      if (.not. okee) then
         ierror = IEOTHR
      endif
c
      return
c-----------------------------------------------------------------------
      end

      subroutine ods_tri_nef_map_tme
c#ifdef WINNT
c    *          [ALIAS:'_ods_tri_nef_map_tme']
c#endif
     *                 (
     *                  fname  ,itype  ,timdef, maxdef ,pardep , locdep,
     *                  maxlst ,        timlst,         timtyp ,
     *                  nrlst  ,ierror ,option                         )
c-----------------------------------------------------------------------
c           Function: time selection for maps
c                     TRISULA NEFIS files
c        Method used:
c-----------------------------------------------------------------------
c   Calling routine :              GETTME
c-----------------------------------------------------------------------
c   Called  routines:              OPNNEF
c                                  CLOSFL
c                                  GETELT (nefis)
c                                  INQGRP (nefis)
c-----------------------------------------------------------------------
c    Parameters:
c    -----------
c
c   Var.      Type Dimensions I/O  description
c   --------------------------------------------------------------------
c
c FNAME      CH*256 3         I    full name including path and ext.
c ITYPE       I*4             I    file type
c TIMDEF      r*8   maxdef*2  I    filter for required times
c                                  julian notation
c MAXDEF      I*4             I    number of locations / filters in TIMDEF
c PARDEP      I*4             I    parameter dependency of the times to get
c LOCDEP      I*4             I    location dependency of the times to get
c MAXLST      I*4             I    maximum number of parameters possible
c
c TIMLST      r*8   maxlst    O    list of times found in julian notation
c TIMTYP      I*4   maxlst    O    list with type of times
c NRLST       I*4             O    number of times found
c OPTION     CH*256           I/O  option (not used)
c IERROR      I*4             O    = 0 no errors, = 1 error detected
c-----------------------------------------------------------------------
c          Constants:
c
c Const.      Type
c
c-----------------------------------------------------------------------
c    Local variables:
c    ----------------
c
c   Var.      Type Dimensions
c   -------------------------
c
c BUFLEN      I*4                  Size in bytes of available buffer
c DT          R*4                  Time step in TUNIT seconds
c ELMNAM     CH*16                 Element name definition
c FILHDA     CH*256                File name NEFIS data file for MAP
c FILHDE     CH*256                File name NEFIS definition file for
c                                  MAP
c GRPDEF     CH*16                 Group name definition
c GRPDMS      I*4  5               Array with GRPNDM dimensions
c GRPNDM      I*4                  Number of dimenmsions of this group
c GRPORD      I*4  5               Array which gives order in which data
c                                  must be read
c HDAFDS      I*4  999             Data file descriptor for the MAP-DAT
c                                  file
c HDEFDS      I*4  2997            Definition file description for the
c                                  MAP-DEF file
c I           I*4                  Help var.
c IDATE       I*4                  integer date yyyy*10000+mm*100+dd
c IDAY        I*4                  Day part of ITDATE (dd)
c IDP         I*4                  Help var. date part julian notation
c IERROR      I*4                  Error code for NEFIS error
c IHULP       I*4  2               Help array.
c IMO         I*4                  Month part of ITDATE (mm)
c IMO1        I*4                  Help var.
c ITDATE      I*4                  Initial simulation start date
c ITIME       I*4                  integer time hh*10000+mm*100+ss
c ITMODC      I*4                  Help var. for time step number
c ITP         I*4                  Help var. time part julian notation
c IY          I*4                  Year part of ITDATE (yyyy)
c JULDAY      I*4                  julian day number of ITDATE
c KMAX        I*4                  Number of layers
c L           I*4                  Help var.
c LMAX        I*4                  Number of constituents
c M           I*4                  Help var.
c N           I*4                  Help var.
c NHULP       I*4                  Number of cells in case error in
c                                  file
c NRCEL       I*4                  Number of cells defined in group 1&3
c OKEE        L*4                  Flag for error checking
c TUNIT       R*4                  Scale unit to define seconds
c UINDEX      I*4  3               Array with indices of the cells to
c                                  be read
c USRORD      I*4                  Sequence in which the cells must be
c                                  read
c ZRHO        L*4                  if .true. then density included
c-----------------------------------------------------------------------
c
c  DECLARATIONS
c
      include         'ods.inc'
c
      integer         ihulp(2)
      integer         nhulp ,i     ,itmodc,itdate,ind
      integer         ierror,nrlst ,maxdef,maxlst,nrcel ,itype
      integer         julday
      integer         icurtm,l     ,n     ,imo1  ,idp   ,itp
      integer         ihou  ,imin  ,isec
      integer         iy    ,imo   ,iday
      integer         itime ,idate
      integer         pardep,locdep
      integer         timtyp(maxlst)
c
      double precision timlst(maxlst)
      double precision timdef(maxdef,2)
      real             dt    ,tunit
c
      character       fname(*)*(*)
      character*256   filhda,filhde
      character*256   option
c
      logical         ex    ,okee
c-----------------------------------------------------------------------
c-----declaration NEFIS
c-----------------------------------------------------------------------
      integer       hdefds( 2997),hdafds(  999)
c
      integer       grpndm,grpdms(    5),grpord(    5),
     *              uindex(    3),usrord,buflen
c
      character*16  grpdef,elmnam,celnam
c
      integer       INQGRP,GETELT,GETELS
      integer       INQMXI
c#ifdef WINNT
      integer       GETELT_i
c#endif

c-----------------------------------------------------------------------
c-----Initialisation
c-----------------------------------------------------------------------
      okee   = .true.
c
      ierror =  0
c
c--------------------------------------------------------------------
c-----Test if trim-<runid>.dat and trim-<runid>.def Nefis MAP-files
c     exist
c--------------------------------------------------------------------
      call ods_check_nefis( fname  , '.def' , ierror )
      if ( ierror .ne. ieok   ) then
         return
      endif
c
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
c--------------------------------------------------------------------
c-----Open trim-<runid>.dat and trim-<runid>.def MAP-files
c--------------------------------------------------------------------
      call OPNNEF(fname, itype, hdafds, hdefds, ierror )
      if (ierror .ne. 0) then
         ierror = IEFIRO
         return
      endif
c--------------------------------------------------------------------
c-----Read array-dimension nrcel from Nefis MAP-files group 1
c--------------------------------------------------------------------
      grpdef = 'map-info-series'
      grpndm = 5
      celnam = grpdef
      ierror = INQGRP(hdefds   ,grpdef    ,celnam    ,grpndm    ,
     *                grpdms   ,grpord                          )
      nrcel  = grpdms(1)
c--------------------------------------------------------------------
c-----Test value of nrcel if nrcel = 0 then get nrcel with INQMXI
c--------------------------------------------------------------------
      if (nrcel  .eq. 0) then
         ierror    = INQMXI(hdefds,grpdef    ,nrcel     )
         okee = okee .and. (ierror .eq. 0)
         if (nrcel  .eq. 0) then
            okee   = .false.
            goto 8888
         endif
      endif
c--------------------------------------------------------------------
c-----Read constants from Nefis MAP-files group 2
c--------------------------------------------------------------------
      grpdef    = 'map-const'
      uindex(1) = 1
      uindex(2) = 1
      uindex(3) = 1
      usrord    = 1
c
      buflen    = 2 * 4
      elmnam    = 'ITDATE'
      okee = okee .and.
     *       GETELT(hdefds   ,grpdef    ,elmnam    ,
     *              uindex   ,usrord    ,buflen    ,IHULP     )
     *       .eq. 0
      itdate    = ihulp (1)
c
      buflen    = 4
      elmnam    = 'TUNIT'
      okee = okee .and.
     *       GETELT(hdefds   ,grpdef    ,elmnam    ,
     *              uindex   ,usrord    ,buflen    ,TUNIT     )
     *       .eq. 0
c
      buflen    = 4
      elmnam    = 'DT'
      okee = okee .and.
     *       GETELT(hdefds   ,grpdef    ,elmnam    ,
     *              uindex   ,usrord    ,buflen    ,DT        )
     *       .eq. 0

c-----------------------------------------------------------------------
c-----Convert ITDATE to julian day number JULDAY
c-----------------------------------------------------------------------
      iday   = mod (itdate ,   100)
      imo    = mod (itdate , 10000) /  100
      iy     =      itdate / 10000
      imo1   = (imo -14)/12
      julday = iday - 32075 + 1461 * (iy+4800+imo1  )/ 4
     *       + 367  * (imo  - 2    - imo1  *  12    )/12
     *       - 3    * ((iy  + 4900 + imo1  )/100    )/ 4

c-----------------------------------------------------------------------
c-----Initialize Nefis variables for group 1
c-----------------------------------------------------------------------
      grpdef    = 'map-info-series'
      elmnam    = 'ITMAPC'
c
      uindex(1) = 1
      uindex(2) = 1
      uindex(3) = 1
      usrord    = 1
      buflen    = 4
c-----------------------------------------------------------------------
c-----Define number of rows default nrcel, if an error
c     occures (ierror <> 0) then re-define
c-----------------------------------------------------------------------
      nhulp  = nrcel
      do 100 i=1,nrcel
c-----------------------------------------------------------------------
c--------Read from group 1 ITMODC
c-----------------------------------------------------------------------
         uindex(1) = i
         uindex(2) = i
         uindex(3) = 1
c
         ierror    = GETELT(hdefds,grpdef    ,elmnam    ,
     *                      uindex,usrord    ,buflen    ,ITMODC    )
         if (ierror .ne. 0 .or. itmodc .eq. -1) then
c-----------------------------------------------------------------------
c-----------In case an error occured while reading the file then
c           write part which is ok to nhulp = i - 1
c-----------------------------------------------------------------------
            nhulp  = i - 1
            goto 200
         endif
c-----------------------------------------------------------------------
c--------Calculate current time elapsed in seconds referenced to ITDATE
c-----------------------------------------------------------------------
         icurtm = nint  (itmodc * dt * tunit)
         iday   = icurtm / 86400
         icurtm = icurtm - iday  * 86400
         ihou   = icurtm / 3600
         icurtm = icurtm - ihou  * 3600
         imin   = icurtm / 60
         icurtm = icurtm - imin  * 60
         isec   = icurtm
c--------------------------------------------------------------------------
c--------Convert true time from julian day-number
c--------------------------------------------------------------------------
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
c--------------------------------------------------------------------------
c--------Define integer values for time and date
c--------------------------------------------------------------------------
         itime  = ihou   * 10000 + imin   * 100 + isec
         idate  = iy     * 10000 + imo    * 100 + iday
c--------------------------------------------------------------------------
c--------Convert to julian notation and store
c--------------------------------------------------------------------------
         imo1      = (imo -14)/12
         idp       = iday - 32075 + 1461 * (iy+4800+imo1  )/ 4
     *             + 367  * (imo  - 2    - imo1  *  12    )/12
     *             - 3    * ((iy  + 4900 + imo1  )/100    )/ 4
         itp       = ihou * 3600 + imin * 60 + isec - 43200
         timlst(i) = dble(idp) + dble(itp) / 86400d0
  100 continue

c-----------------------------------------------------------------------
c-----Exception handling not enough data written to Nefis files
c-----------------------------------------------------------------------
  200 continue
c
      nrlst = nhulp
c
c-----------------------------------------------------------------------
c-----check required and found number of parameter names
c-----------------------------------------------------------------------
       okee = okee .and. (maxlst .le. nrcel)
c--------------------------------------------------------------------
c-----Close trim-<runid>.dat and trim-<runid>.def MAP-files
c--------------------------------------------------------------------
 8888 continue
      call CLOSFL(fname, ierror)
      okee = okee .and. (ierror .eq. 0)
c-----------------------------------------------------------------------
c-----return status to calling routine
c-----------------------------------------------------------------------
      ierror = IEOK
      if (.not. okee) then
         ierror = IEOTHR
      endif
c
      return
c-----------------------------------------------------------------------
      end

      subroutine julind_map
c#ifdef WINNT
c    *          [ALIAS:'_julind_map']
c#endif
     *          (hdefds, hdafds, tim, nindex, ierror)
c-----------------------------------------------------------------------
c           Function: transform julian day to index in time series
c        Method used:
c
c-----------------------------------------------------------------------
c   Calling routine :              ODS_TRI_NEF_MAP_MAT
c-----------------------------------------------------------------------
c   Called  routines:              GETELT (nefis)
c                                  INQGRP (nefis)
c                                  INQMXI (nefis)
c-----------------------------------------------------------------------
c    Parameters:
c    -----------
c
c   Var.      Type Dimensions I/O  description
c   --------------------------------------------------------------------
c
c HDAFDS      I*4  999        I    Data file descriptor for the MAP-DAT
c                                  file
c HDEFDS      I*4  2997       I    Definition file description for the
c                                  MAP-DEF file
c TIM         r*8  3          I    list with julian dates, begin, end, inc
c NINDEX      I*4  3          O    indices of time frame in Nefis file
c IERROR      I*4             O    = 0 no errors, = 1 error detected
c-----------------------------------------------------------------------
c          Constants:
c
c Const.      Type
c
c-----------------------------------------------------------------------
c    Local variables:
c    ----------------
c
c   Var.      Type Dimensions
c   -------------------------
c
c BUFLEN      I*4                  Size in bytes of available buffer
c CELNAM     CH*16                 Cell name definition
c DT          R*4                  Time step in TUNIT seconds
c ELMNAM     CH*16                 Element name definition
c FOUT        L*4                  Flag for further execution program
c                                  fout  = .true.  : stop execution
c                                  fout  = .false. : go on
c GRPDEF     CH*16                 Group name definition
c GRPDMS      I*4  5               Array with GRPNDM dimensions
c GRPNDM      I*4                  Number of dimenmsions of tmap group
c GRPORD      I*4  5               Array which gives order in which data
c                                  must be read
c I           I*4                  Help var.
c IDAY        I*4                  Day part of ITDATE (dd)
c ICURTM      I*4                  Current timeref. to ITDATE
c IDP         I*4                  Help var. date part julian notation
c IERROR      I*4                  Error code for NEFIS error
c IHOU        I*4                  Help var. current step hours
c IHULP       I*4  2               Help array.
c IMO         I*4                  Month part of ITDATE (mm)
c IMO1        I*4                  Help var.
c IMIN        I*4                  Help var. current step minutes
c ISEC        I*4                  Help var. current step seconds
c ITDATE      I*4                  Initial simulation start date
c ITMODC      I*4                  Help var. for time step number
c ITP         I*4                  Help var. time part julian notation
c IY          I*4                  Year part of ITDATE (yyyy)
c JULDAY      I*4                  julian day number of ITDATE
c L           I*4                  Help var.
c LMAX        I*4                  Number of constituents
c N           I*4                  Help var.
c NRCEL       I*4                  Number of cells defined in group 1&3
c NRII        I*4                  Number of index intervals
c NRTI        I*4                  Number of time intervals
c TIMSTP      R*8                  Current julian day for tmap step
c TUNIT       R*4                  Scale unit to define seconds
c UINDEX      I*4  3               Array with indices of the cells to
c                                  be read
c USRORD      I*4                  Sequence in which the cells must be
c                                  read
c-----------------------------------------------------------------------
c
c  DECLARATIONS
c
      include         'ods.inc'
c
      integer         ihulp(2)
      integer         itmodc,itdate
      integer         ierror,nrcel
      integer         julday
      integer         icurtm,l     ,n     ,imo1  ,idp   ,itp
      integer         ihou  ,imin  ,isec
      integer         iy    ,imo   ,iday
      integer         nindex(3)
      integer         nrti  ,nrii  ,i
c
      real             dt    ,tunit
      double precision tim(3)
      double precision timstp
      double precision epstim
c
      logical         fout
c-----------------------------------------------------------------------
c-----declaration NEFIS
c-----------------------------------------------------------------------
      integer       hdefds( 2997),hdafds(  999)
      integer       grpndm,grpdms(    5),grpord(    5),
     *              uindex(    3),usrord,buflen
      character*16  grpdef,elmnam,celnam
      integer       INQGRP,GETELT,GETELS,INQMXI
c#ifdef WINNT
      integer       GETELT_i
c#endif

c-----------------------------------------------------------------------
c-----Initialisation
c-----------------------------------------------------------------------
      fout      = .false.
      ierror    =  0
      nindex(1) =  1
      nindex(2) =  1
      nindex(3) =  1
c
c--------------------------------------------------------------------
c-----Read array-dimension nrcel from Nefis MAP-files group 1
c--------------------------------------------------------------------
      grpdef = 'map-info-series'
      grpndm = 5
      celnam = grpdef
      ierror = INQGRP(hdefds   ,grpdef    ,celnam    ,grpndm    ,
     *                grpdms   ,grpord                          )
      nrcel  = grpdms(1)
c--------------------------------------------------------------------
c-----Test value of nrcel if nrcel = 0 then get nrcel with INQMXI
c--------------------------------------------------------------------
      if (nrcel  .eq. 0) then
         ierror    = INQMXI(hdefds,grpdef    ,nrcel     )
         if (ierror .ne. 0) then
            fout  = .true.
            goto 8888
         endif
      endif
c--------------------------------------------------------------------
c-----Read constants from Nefis MAP-files group 2
c--------------------------------------------------------------------
      grpdef    = 'map-const'
      uindex(1) = 1
      uindex(2) = 1
      uindex(3) = 1
      usrord    = 1
c
      buflen    = 2 * 4
      elmnam    = 'ITDATE'
      ierror    = GETELT(hdefds   ,grpdef    ,elmnam    ,
     *                   uindex   ,usrord    ,buflen    ,IHULP     )
      if (ierror .ne. 0) then
         fout   = .true.
         goto 8888
      endif
      itdate    = ihulp (1)
c
      buflen    = 4
      elmnam    = 'TUNIT'
      ierror    = GETELT(hdefds   ,grpdef    ,elmnam    ,
     *                   uindex   ,usrord    ,buflen    ,TUNIT     )
      if (ierror .ne. 0) then
         fout   = .true.
         goto 8888
      endif
c
      buflen    = 4
      elmnam    = 'DT'
      ierror    = GETELT(hdefds   ,grpdef    ,elmnam    ,
     *                   uindex   ,usrord    ,buflen    ,DT        )
      if (ierror .ne. 0) then
         fout   = .true.
         goto 8888
      endif
c-----------------------------------------------------------------------
c-----Convert ITDATE to julian day number JULDAY
c-----------------------------------------------------------------------
      iday   = mod (itdate ,   100)
      imo    = mod (itdate , 10000) /  100
      iy     =      itdate / 10000
      imo1   = (imo -14)/12
      julday = iday - 32075 + 1461 * (iy+4800+imo1  )/ 4
     *       + 367  * (imo  - 2    - imo1  *  12    )/12
     *       - 3    * ((iy  + 4900 + imo1  )/100    )/ 4
c-----------------------------------------------------------------------
c-----Initialize Nefis variables for group 1
c-----------------------------------------------------------------------
      grpdef    = 'map-info-series'
      elmnam    = 'ITMAPC'
c
      uindex(1) = 1
      uindex(2) = 1
      uindex(3) = 1
      usrord    = 1
      buflen    = 4
c-----------------------------------------------------------------------
c-----Define number of rows default nrcel, if an error
c     occures (ierror <> 0) then re-define
c     Note:
c     It is necessary to use a small margin (1/100th of a second),
c     because otherwise the animation procedure may nd up using the
c     wrong data (all because of the time stepping with double
c     precision reals)
c-----------------------------------------------------------------------

      epstim = 1.0d0 / (86400.0d0 * 100.0d0)

      do 100 i=1,nrcel
c-----------------------------------------------------------------------
c--------Read from group 1 ITMODC
c-----------------------------------------------------------------------
         uindex(1) = i
         uindex(2) = i
         uindex(3) = 1
c
         ierror    = GETELT(hdefds,grpdef    ,elmnam    ,
     *                      uindex,usrord    ,buflen    ,ITMODC    )
c-----------------------------------------------------------------------
c--------Calculate current time elapsed in seconds referenced to ITDATE
c-----------------------------------------------------------------------
         icurtm = nint  (itmodc * dt * tunit)
         iday   = icurtm / 86400
         icurtm = icurtm - iday  * 86400
         ihou   = icurtm / 3600
         icurtm = icurtm - ihou  * 3600
         imin   = icurtm / 60
         icurtm = icurtm - imin  * 60
         isec   = icurtm
c--------------------------------------------------------------------------
c--------Convert true time from julian day-number
c--------------------------------------------------------------------------
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
c--------------------------------------------------------------------------
c--------Convert to julian notation and store
c--------------------------------------------------------------------------
         imo1      = (imo -14)/12
         idp       = iday - 32075 + 1461 * (iy+4800+imo1  )/ 4
     *             + 367  * (imo  - 2    - imo1  *  12    )/12
     *             - 3    * ((iy  + 4900 + imo1  )/100    )/ 4
         itp       = ihou * 3600 + imin * 60 + isec - 43200
         timstp    = dble(idp) + dble(itp) / 86400d0
c-----------------------------------------------------------------------
c--------find the begin and end index
c-----------------------------------------------------------------------
         if ( timstp .le. (tim(1)+epstim) ) then
            nindex(1) = i
         else
            if ( timstp .le. (tim(2)+epstim) ) then
               nindex(2) = i
            endif
         endif
  100 continue
c-----------------------------------------------------------------------
c-----find the increment
c-----------------------------------------------------------------------
      nrti      = int((tim(2) - tim(1) + 0.1 * tim(3)) / tim(3))
      nrii      = nindex(2) - nindex(1)
      nindex(3) = max(1,nrii) / max(1,nrti)
c-----------------------------------------------------------------------
c-----return status to calling routine
c-----------------------------------------------------------------------
 8888 continue
      ierror = IEOK
      if (fout  ) then
         ierror = IEOTHR
      endif
c
      return
c-----------------------------------------------------------------------
      end

      subroutine ods_tri_nef_map_loc
c#ifdef WINNT
c    *          [ALIAS:'_ods_tri_nef_map_loc']
c#endif
     *                 (
     *                  fname  ,itype  ,locdef ,maxdef ,pardep ,timdep ,
     *                  maxlst ,        loclst ,        loctyp ,nrlst  ,
     *                  locnr  ,ierror ,zbuffs ,option                 )
c-----------------------------------------------------------------------
c           Function: parameter location selection for maps
c                     TRISULA NEFIS files
c        Method used:
c
c-----------------------------------------------------------------------
c   Calling routine :              GETLOC
c-----------------------------------------------------------------------
c   Called  routines:              OPNNEF
c                                  CLOSFL
c                                  GETELT (nefis)
c-----------------------------------------------------------------------
c    Parameters:
c    -----------
c
c   Var.      Type Dimensions
c   -------------------------
c
c FNAME      CH*256 3         I    full name including path and ext.
c ITYPE       I*4             I    file type
c LOCDEF     CH*21  maxdef    I    filter for required parameter locs.
c MAXDEF      I*4             I    number of filters in PARDEF
c PARDEP      I*4             I    access index of parameter location
c TIMDEP      I*4             I    access index of parameter time
c MAXLST      I*4             I    maximum number of parameters possible
c LOCLST     CH*21  nrlst     O    names of parameter locations
c LOCTYP      I*4   nrlst     O    type  of parameter locations
c NRLST       I*4             O    number of to be returned
c LOCNR       I*4   nrlst     O    list of index numbers of locations
c IERROR      I*4             O    = 0 no errors, = 1 error detected
c ZBUFFS     CH*21  maxlst    I/O  workspace names of locations
c OPTION     CH*256           I/O  option (not used)
c-----------------------------------------------------------------------
c          Constants:
c
c Const.      Type
c
c-----------------------------------------------------------------------
c    Local variables:
c    ----------------
c
c   Var.      Type Dimensions
c   -------------------------
c
c BUFLEN      I*4                  Size in bytes of available buffer
c CELDEF     CH*16                 Cell name definition
c ELMNAM     CH*16                 Element name definition
c FILHDA     CH*256                File name NEFIS data file for MAP
c FILHDE     CH*256                File name NEFIS definition file for
c                                  MAP
c GRPDEF     CH*16                 Group name definition
c                                  must be read
c HDAFDS      I*4  999             Data file descriptor for the MAP-DAT
c                                  file
c HDEFDS      I*4  2997            Definition file description for the
c                                  MAP-DEF file
c IERROR      I*4                  Error code for NEFIS error
c LMAX    I   I*4                  Number of constituents
c                                  for old files LMAX = LSTCI
c                                  for new files LMAX = LSTCI + LTUR
c LNAME      CH*20                 Help var. location name
c LSTCI   I   I*4                  Total number of constituents (incl.
c                                  turbulence for old trim files).
c LTUR    I   I*4                  Number of turbulence constituents
c NLOC        I*4                  Number of found locations
c NOSTAT      I*4                  Number of defined stations
c NTRUV       I*4                  Number of cross-secties in u- and
c                                  v-direction
c OKEE        L*4                  Flag for error checking
c SELMAP  I  CH*20                 Output flags containing Y or N for
c                                  various output quantities selection
c                                  for map files
c UINDEX      I*4  3               Array with indices of the cells to
c                                  be read
c USRORD      I*4                  Sequence in which the cells must be
c                                  read
c-----------------------------------------------------------------------
c
c  DECLARATIONS
c
      include 'ods.inc'
c
      integer         ierror,nrlst ,ind   ,l
      integer         lstci ,ltur  ,irho
      integer         mmax  ,nmax  ,lmax  ,kmax ,npar
      integer         maxlst,itype
      integer         maxdef
      integer         pardep
      integer         timdep
      integer         loctyp(maxlst)
      integer         locnr (maxlst)
c
      character       locdef(*)*(*)
      character       loclst(*)*(*)
      character*20    selmap,namcon(10)
      character*20    zbuffs(maxlst)
      character       fname(*)*(*)
      character*256   filhda,filhde
      character*256   option
c
      logical         ex    ,okee  ,check ,zrho
c-----------------------------------------------------------------------
c-----declaration NEFIS
c-----------------------------------------------------------------------
      integer       hdefds( 2997),hdafds(  999)
c
      integer       uindex(    3),usrord,buflen
c
      character*16  grpdef,elmnam
c
      integer       GETELT,GETELS
c#ifdef WINNT
      integer       GETELT_i
c#endif
c-----------------------------------------------------------------------
c-----Initialisation
c-----------------------------------------------------------------------
      okee   = .true.
      zrho   = .false.
      ierror =  0
c--------------------------------------------------------------------
c-----Test if trim-<runid>.dat and trim-<runid>.def Nefis MAP-files
c     exist
c--------------------------------------------------------------------
      call ods_check_nefis( fname  , '.def' , ierror )
      if ( ierror .ne. ieok   ) then
         return
      endif
c--------------------------------------------------------------------
c-----Open trim-<runid>.dat and trim-<runid>.def MAP-files
c--------------------------------------------------------------------
      call OPNNEF(fname, itype, hdafds, hdefds, ierror )
      if (ierror .ne. 0) then
         ierror = IEFIRO
         return
      endif
c--------------------------------------------------------------------
c-----Read array-dimensions from Nefis MAP-files group 2
c--------------------------------------------------------------------
      grpdef = 'map-const'
c
      uindex(1) = 1
      uindex(2) = 1
      uindex(3) = 1
      usrord    = 1
      buflen    = 4
c
      npar      = 0
      mmax      = 0
      nmax      = 0
      lmax      = 0
      lstci     = 0
      ltur      = 0
      irho      = 0
      kmax      = 0
c
      elmnam    = 'MMAX'
      okee = okee .and.
     *       GETELT(hdefds,grpdef    ,elmnam    ,
     *              uindex,usrord    ,buflen    ,MMAX      )
     *       .eq. 0
c
      elmnam    = 'NMAX'
      okee = okee .and.
     *       GETELT(hdefds,grpdef    ,elmnam    ,
     *              uindex,usrord    ,buflen    ,NMAX      )
     *       .eq. 0
c
c--------------------------------------------------------------------
c-----Read element LMAX; LMAX only defined in old trim files
c     hence if not defined ierror = -25041
c--------------------------------------------------------------------
      elmnam    = 'LMAX'
      ierror    = GETELT(hdefds,grpdef    ,elmnam    ,
     *                   uindex,usrord    ,buflen    ,LSTCI     )
      if (ierror .ne. 0) then
c--------------------------------------------------------------------
c--------In case of a new trih file read LSTCI and LTUR
c--------------------------------------------------------------------
         elmnam = 'LSTCI'
         ierror = GETELT(hdefds,grpdef    ,elmnam    ,
     *                   uindex,usrord    ,buflen    ,LSTCI     )
         if (ierror .ne. 0) then
            okee = .false.
            goto 8888
         endif

         elmnam = 'LTUR'
         ierror = GETELT(hdefds,grpdef    ,elmnam    ,
     *                   uindex,usrord    ,buflen    ,LTUR      )
         if (ierror .ne. 0) then
            okee = .false.
            goto 8888
         endif
      endif
c
      lmax = lstci + ltur
c
      elmnam = 'KMAX'
      ierror = GETELT(hdefds,grpdef    ,elmnam    ,
     *                uindex,usrord    ,buflen    ,KMAX      )
      if (ierror .ne. 0) then
         okee = .false.
         goto 8888
      endif
c
c-----------------------------------------------------------------------
c-----Element SELMAP selection of output
c-----------------------------------------------------------------------
      buflen    = 20
      elmnam    = 'SELMAP'
c#ifdef WINNT
c     ierror    = GETELT_i
c#else
      ierror    = GETELS
c#endif
     *                  (hdefds   ,grpdef    ,elmnam    ,
     *                   uindex   ,usrord    ,buflen    ,SELMAP    )
      if (ierror .ne. 0) then
c-----------------------------------------------------------------------
c--------In case of a old trim file no SELMAP then
c           re-define SELMAP like definition in subroutine RDPRFL
c-----------------------------------------------------------------------
         selmap = 'YYYYYYYYYYYYYYYYYYYY'
         if (kmax   .eq. 1) selmap( 4: 5) = 'NN'
         if (lstci  .eq. 0) selmap( 6:13) = 'NNNNNNNN'
         if (ltur   .eq. 0) selmap(14:15) = 'NN'
         if (kmax   .eq. 1) selmap(18:19) = 'NN'
         if (lmax   .eq. 0) selmap(19:19) = 'N'
         selmap(20:20) = 'X'
      endif
c-----------------------------------------------------------------------
c-----Element NAMCON constituents and turbulence quantity names
c     only if selmap( 6:15) <> 'NNNNNNNNNN'
c-----------------------------------------------------------------------
      if (index (selmap( 6:15),'Y') .gt. 0) then
         buflen    = 20 * lmax
         elmnam    = 'NAMCON'
c#ifdef WINNT
c        ierror    = GETELT_i
c#else
         ierror    = GETELS
c#endif
     *                     (hdefds,grpdef    ,elmnam    ,
     *                      uindex,usrord    ,buflen    ,NAMCON    )
         if (ierror .ne. 0) then
            okee = .false.
            goto 8888
         endif
c
         do 110 l = 1,lmax
            if (namcon(l)(:11) .eq. 'Salinity   ') irho   = 1
            if (namcon(l)(:11) .eq. 'Temperature') irho   = 1
  110    continue
      endif
c-----------------------------------------------------------------------
c-----In case of a old trim file re-define SELMAP(20:20)
c-----------------------------------------------------------------------
      if (selmap(20:20) .eq. 'X') then
         selmap(20:20) = 'N'
         if (irho   .eq. 1) selmap(20:20) = 'Y'
      endif
c
      zrho = irho .eq. 1
c--------------------------------------------------------------------
c-----Generate location names from Nefis MAP-files
c     select possibe parametercode values
c--------------------------------------------------------------------
      check    = pardep .ge.  1 .and. pardep .le. 29 .or.
     *           pardep .ge. 31 .and. pardep .le. 32 .or.
     *           pardep .ge. 35 .and. pardep .le. 42 .or.
     *           pardep .ge. 44 .and. pardep .le. 45
c
c-----no location names known; noaction

      nrlst = maxlst

c-----------------------------------------------------------------------
c-----check found number against required number
c-----------------------------------------------------------------------
      okee = okee .and. (nrlst .eq. maxlst)
c--------------------------------------------------------------------
c-----Close trim-<runid>.dat and trim-<runid>.def MAP-files
c--------------------------------------------------------------------
 8888 continue
      call CLOSFL(fname, ierror)
      okee = okee .and. (ierror .eq. 0)
c-----------------------------------------------------------------------
c-----return status to calling routine
c-----------------------------------------------------------------------
      ierror = IEOK
      if (.not. okee) then
         ierror = IEOTHR
      endif
c
      return
c-----------------------------------------------------------------------
      end

      subroutine ods_tri_nef_map_mat
c#ifdef WINNT
c    *          [ALIAS:'_ods_tri_nef_map_mat']
c#endif
     *                  (
     *                   fname ,itype  ,parcod, loc   , tim   ,misval,
     *                   i3gl  ,maxdim ,xdata , ierror, option,
     *                   ibuffs,rbuffs                               )
c-----------------------------------------------------------------------
c           Function: select map data out of TRISULA NEFIS files
c        Method used:
c
c-----------------------------------------------------------------------
c   Calling routine :              GETMAT
c-----------------------------------------------------------------------
c   Called  routines:              OPNNEF
c                                  CLOSFL
c                                  GETELT (nefis)
c                                  julind_map
c                                  ods_tri_nef_map_getdata
c-----------------------------------------------------------------------
c    Parameters:
c    -----------
c
c   Var.      Type Dimensions
c   -------------------------
c
c FNAME      CH*256 3        I     full name including path and ext.
c ITYPE       I*4            I     file type
c PARCOD      I*4            I     parameter to get data of
c LOC         I*4   3*3      I     list with indices of locations
c TIM         R*8   3        I     list with Julian dates
c MISVAL      R*4   1        I     missing value
c I3GL        I*4   1        I     code of data storage :
c                                  1 = fortran
c                                  2 = c
c MAXDIM      I*4            I     lenght of data array
c XDATA       R*4   maxdim   O     array with the data
c IERROR      I*4            O     = 0 no errors, = 1 error detected
c OPTION     CH*256          O     option (not used)
c IBUFFS      I*4   <len>    O/I   integer buffer for reading Nefis file
c RBUFFS      R*4   <len>    O/I   real    buffer for reading Nefis file
c-----------------------------------------------------------------------
c          Constants:
c
c Const.      Type
c
c-----------------------------------------------------------------------
c    Local variables:
c    ----------------
c
c   Var.      Type Dimensions
c   -------------------------
c
c BUFLEN      I*4                  Size in bytes of available buffer
c ELMNAM     CH*16                 Element name definition
c EX          L*4                  flag for exist of file
c FILHDA     CH*256                File name NEFIS data file for MAP
c FILHDE     CH*256                File name NEFIS definition file for
c                                  MAP
c GRPDEF     CH*16                 Group name definition
c GRPDMS      I*4  1               Array with GRPNDM dimensions
c GRPNDM      I*4                  Number of dimenmsions of this group
c GRPORD      I*4  1               Array which gives order in which data
c                                  must be read
c HDAFDS      I*4  999             Data file descriptor for the MAP-DAT
c                                  file
c HDEFDS      I*4  2997            Definition file description for the
c IERROR      I*4                  Error code for NEFIS error
c ITIM        I*4                  index of map time
c KMAX        I*4                  Number of layers
c LMAX    I   I*4                  Number of constituents
c                                  for old files LMAX = LSTCI
c                                  for new files LMAX = LSTCI + LTUR
c LMAXD       I*4                  maximum(1,LMAX)
c LSTCI   I   I*4                  Total number of constituents (incl.
c                                  turbulence for old trim files).
c LTUR    I   I*4                  Number of turbulence constituents
c LAY         I*4  3               layer selection actual
c N           I*4                  Counter for XDATA
c NOSTAT      I*4                  Number of stations
c NTRUV       I*4                  Number of cross-sections
c NINDEX      I*4  3               indices of time frame in Nefis file
c OKEE        L*4                  Flag for error checking
c SELMAP  I  CH*20                 Output flags containing Y or N for
c                                  various output quantities selection
c                                  for map files
c UINDEX      I*4  3               Array with indices of the cells to
c                                  be read
c USRORD      I*4                  Sequence in which the cells must be
c                                  read
c
c--Pointer variables to buffer space
c
c DICWW       I*4                  Pointer for array DICWW
c DP          I*4                  Pointer for array DP
c DPS         I*4                  Pointer for array DPS
c IFLAG       I*4                  Pointer for array IFLAG
c IGRID       I*4                  Pointer for array IGRID
c JCONST      I*4                  Pointer variable
c KCS         I*4                  Pointer for array KCS
c KCU         I*4                  Pointer for array KCU
c KCV         I*4                  Pointer for array KCV
c KFU         I*4                  Pointer for array KFU
c KFV         I*4                  Pointer for array KFV
c R1          I*4                  Pointer for array R1
c RHO         I*4                  Pointer for array RHO
c S1          I*4                  Pointer for array S1
c TAUETA      I*4                  Pointer for array TAUETA
c TAUKSI      I*4                  Pointer for array TAUKSI
c THICK       I*4                  Pointer for array THICK
c U1          I*4                  Pointer for array U1
c V1          I*4                  Pointer for array V1
c VICWW       I*4                  Pointer for array VICWW
c WPHY        I*4                  Pointer for array WPHY
c XCOR        I*4                  Pointer for array XCOR
c XZ          I*4                  Pointer for array XZ
c YCOR        I*4                  Pointer for array YCOR
c YZ          I*4                  Pointer for array YZ
c-----------------------------------------------------------------------
c
c  DECLARATIONS
c
      include         'ods.inc'
c
      integer         dicww ,dp    ,dps   ,alfas        ,
     *                iflag ,igrid ,jconst,
     *                kcs   ,kcu   ,kcv   ,kfu   ,kfv   ,
     *                r1    ,rho   ,s1    ,taueta,tauksi,
     *                thick ,u1    ,v1    ,vicww ,wphy  ,
     *                xcor  ,xz    ,ycor  ,yz
c
      double precision tim   (3)

      integer         maxdim, itype
      integer         parcod
      integer         loc   (3,3)
      integer         i3gl
      integer         ibuffs (*     )

      real            misval
      real            xdata (maxdim)
      real            rbuffs (*     )

      character       fname (*)*(*)
      character*256   option
c-----------------------------------------------------------------------
c-----declaration Local variables
c-----------------------------------------------------------------------
      character*256   filhda,filhde
      character*20    selmap,namcon(10)
c
      integer         nindex (3)
      integer         lay(3),ind   ,itim  ,l
      integer         ierror,kmax  ,lmax  ,mmax  ,nmax
      integer         lstci ,ltur  ,irho  ,kmaxon
      integer         noroco,irocol
c
      logical         ex    ,okee  ,zrho
c
c-----------------------------------------------------------------------
c-----declaration NEFIS
c-----------------------------------------------------------------------
      integer       hdefds( 2997),hdafds(  999)
      integer       uindex(    3),usrord,buflen
      integer*4     nbytsg,elmndm,elmdms(    5)
c
      character*8   elmtyp
c
      character*16  grpdef,elmnam,elmqty,elmunt
c
      character*64  elmdes
c
      integer       GETELT,GETELS,INQELM
c#ifdef WINNT
      integer       GETELT_i
c#endif
c-----------------------------------------------------------------------
c-----Initialisation
c-----------------------------------------------------------------------
c
      ierror =  0
      okee   = .true.
      zrho   = .false.
c--------------------------------------------------------------------
c-----Test if trim-<runid>.dat and trim-<runid>.def Nefis MAP-files
c     exist
c--------------------------------------------------------------------
      call ods_check_nefis( fname  , '.def' , ierror )
      if ( ierror .ne. ieok   ) then
         return
      endif
c--------------------------------------------------------------------
c-----Open trim-<runid>.dat and trim-<runid>.def MAP-files
c--------------------------------------------------------------------
      call OPNNEF(fname, itype, hdafds, hdefds, ierror)
      if (ierror .ne. 0) then
         ierror = IEFIRO
         return
      endif
c--------------------------------------------------------------------
c-----Read array-dimensions from Nefis MAP-files group 2
c--------------------------------------------------------------------
      grpdef = 'map-const'
c
      uindex(1) = 1
      uindex(2) = 1
      uindex(3) = 1
      usrord    = 1
      buflen    = 4
c
      mmax      = 0
      nmax      = 0
      lmax      = 0
      lstci     = 0
      ltur      = 0
      irho      = 0
      kmax      = 0
      noroco    = 0
c
      elmnam    = 'MMAX'
      okee = okee .and.
     *       GETELT(hdefds,grpdef    ,elmnam    ,
     *              uindex,usrord    ,buflen    ,MMAX      )
     *       .eq. 0
c
      elmnam    = 'NMAX'
      okee = okee .and.
     *       GETELT(hdefds,grpdef    ,elmnam    ,
     *              uindex,usrord    ,buflen    ,NMAX      )
     *       .eq. 0
c
c--------------------------------------------------------------------
c-----Read element LMAX; LMAX only defined in old trim files
c     hence if not defined ierror = -25041
c--------------------------------------------------------------------
      elmnam    = 'LMAX'
      ierror    = GETELT(hdefds,grpdef    ,elmnam    ,
     *                   uindex,usrord    ,buflen    ,LSTCI     )
      if (ierror .ne. 0) then
c--------------------------------------------------------------------
c--------In case of a new trih file read LSTCI and LTUR
c--------------------------------------------------------------------
         elmnam = 'LSTCI'
         ierror = GETELT(hdefds,grpdef    ,elmnam    ,
     *                   uindex,usrord    ,buflen    ,LSTCI     )
         if (ierror .ne. 0) then
            okee   = .false.
            goto 8888
         endif
c
         elmnam = 'LTUR'
         ierror = GETELT(hdefds,grpdef    ,elmnam    ,
     *                   uindex,usrord    ,buflen    ,LTUR      )
         if (ierror .ne. 0) then
            okee   = .false.
            goto 8888
         endif
      endif
c
      lmax = lstci + ltur
c
      elmnam = 'KMAX'
      ierror = GETELT(hdefds,grpdef    ,elmnam    ,
     *                uindex,usrord    ,buflen    ,KMAX      )
      if (ierror .ne. 0) then
         okee   = .false.
         goto 8888
      endif
c
c-----------------------------------------------------------------------
c-----Element SELMAP selection of output
c-----------------------------------------------------------------------
      buflen    = 20
      elmnam    = 'SELMAP'
c#ifdef WINNT
c     ierror    = GETELT_i
c#else
      ierror    = GETELS
c#endif
     *                  (hdefds   ,grpdef    ,elmnam    ,
     *                   uindex   ,usrord    ,buflen    ,SELMAP    )
      if (ierror .ne. 0) then
c-----------------------------------------------------------------------
c--------In case of a old trim file no SELMAP then
c           re-define SELMAP like definition in subroutine RDPRFL
c-----------------------------------------------------------------------
         selmap = 'YYYYYYYYYYYYYYYYYYYY'
         if (kmax   .eq. 1) selmap( 4: 5) = 'NN'
         if (lstci  .eq. 0) selmap( 6:13) = 'NNNNNNNN'
         if (ltur   .eq. 0) selmap(14:15) = 'NN'
         if (kmax   .eq. 1) selmap(18:19) = 'NN'
         if (lmax   .eq. 0) selmap(19:19) = 'N'
         selmap(20:20) = 'X'
      endif
c-----------------------------------------------------------------------
c-----Element NAMCON constituents and turbulence quantity names
c     only if selmap( 6:15) <> 'NNNNNNNNNN'
c-----------------------------------------------------------------------
      if (index (selmap( 6:15),'Y') .gt. 0) then
         buflen    = 20 * lmax
         elmnam    = 'NAMCON'
c#ifdef WINNT
c        ierror    = GETELT_i
c#else
         ierror    = GETELS
c#endif
     *                     (hdefds,grpdef    ,elmnam    ,
     *                      uindex,usrord    ,buflen    ,NAMCON    )
         if (ierror .ne. 0) then
            okee   = .false.
            goto 8888
         endif
c
         do 110 l = 1,lmax
            if (namcon(l)(:11) .eq. 'Salinity   ') irho   = 1
            if (namcon(l)(:11) .eq. 'Temperature') irho   = 1
  110    continue
      endif
c-----------------------------------------------------------------------
c-----In case of a old trim file re-define SELMAP(20:20)
c-----------------------------------------------------------------------
      if (selmap(20:20) .eq. 'X') then
         selmap(20:20) = 'N'
         if (irho   .eq. 1) selmap(20:20) = 'Y'
      endif
c
      zrho = irho .eq. 1
c
      elmnam    = 'NOROCO'
      okee = okee .and.
     *       GETELT(hdefds,grpdef    ,elmnam    ,
     *              uindex,usrord    ,buflen    ,NOROCO    )
     *       .eq. 0
c-----------------------------------------------------------------------
      elmnam    = 'DICWW'
      elmndm    = 5
      ierror    = INQELM(hdefds,elmnam ,elmtyp ,nbytsg ,elmqty ,
     *                   elmunt,elmdes ,elmndm ,elmdms         )
c
      if ( ierror .ne. 0 ) then
c--------version 2.45 and later
         kmaxon    = 1
      else
         if (elmdms(elmndm) .eq. kmax) then
c--------version 2.03
            kmaxon    = 0
         else
c--------version 2.45 and later
            kmaxon    = 1
         endif
      endif
c
c-----------------------------------------------------------------------
c--------array indices (reals)
c-----------------------------------------------------------------------
         xcor   = 1
         ycor   = xcor   + nmax   * mmax
         xz     = ycor   + nmax   * mmax
         yz     = xz     + nmax   * mmax
         alfas  = yz     + nmax   * mmax
         dp     = alfas  + nmax   * mmax
         dps    = dp     + nmax   * mmax
         thick  = dps    + nmax   * mmax
         jconst = thick  + kmax   + 1
         s1     = jconst
         tauksi = s1     + nmax * mmax
         taueta = tauksi + nmax * mmax
         u1     = jconst
         v1     = u1     + nmax * mmax * kmax
         wphy   = v1     + nmax * mmax * kmax
         r1     = jconst
         vicww  = jconst
         dicww  = vicww  + nmax * mmax * (kmax+kmaxon)
         rho    = dicww  + nmax * mmax * (kmax+kmaxon)
c        next   = jconst + nmax * mmax * (kmax+kmaxon) * max (3,lmax)
c
c-----------------------------------------------------------------------
c--------array indices (integers)
c-----------------------------------------------------------------------
         irocol =  1
         kcs    =  irocol + 5     * noroco
         kcu    =  kcs    + nmax  * mmax
         kcv    =  kcu    + nmax  * mmax
         kfu    =  kcv    + nmax  * mmax
         kfv    =  kfu    + nmax  * mmax
         iflag  =  kfv    + nmax  * mmax
         igrid  =  iflag  + nmax  * mmax
c        next   =  igrid  + nmax  * mmax
c
c-----------------------------------------------------------------------
c-----calculate indices for time frame
c-----------------------------------------------------------------------
      call julind_map (hdefds, hdafds, tim,   nindex, ierror)
      okee = okee .and. (ierror .eq. 0)
      if (.not. okee) then
         goto 8888
      endif
c--------------------------------------------------------------------
c-----set tim           {precon: tim(1  )=tim(2  ) and loc(3  )=1}
c--------------------------------------------------------------------
      itim   = nindex(1)
c--------------------------------------------------------------------
c-----for 2DH must be;-----------------------------------------------
c-----set loc (layer  ) {precon: loc(1,3)<=loc(2,3) and loc(3,3)=1}
c--------------------------------------------------------------------
      lay(1) = max( 1, loc(1,3) )
      lay(2) = max( 1, min( loc(2,3), kmax) )
      lay(3) = max( 1, loc(3,3) )
c
c-----------------------------------------------------------------------
      call ods_tri_nef_map_getdata(
     *               hdefds        ,hdafds        ,misval        ,
     *               nmax          ,mmax          ,kmax          ,
     *               lmax          ,noroco        ,itim          ,
     *               lay           ,parcod        ,ierror        ,
     *               lstci         ,ltur          ,kmaxon        ,
     *               maxdim        ,xdata         ,
     *               ibuffs(irocol),ibuffs(kcs   ),ibuffs(kcu   ),
     *               ibuffs(kcv   ),ibuffs(kfu   ),ibuffs(kfv   ),
     *               ibuffs(iflag ),ibuffs(igrid ),
     *               rbuffs(xcor  ),rbuffs(ycor  ),rbuffs(xz    ),
     *               rbuffs(yz    ),rbuffs(alfas ),
     *               rbuffs(dp    ),rbuffs(dps   ),rbuffs(thick ),
     *               rbuffs(s1    ),rbuffs(tauksi),rbuffs(taueta),
     *               rbuffs(u1    ),rbuffs(v1    ),rbuffs(wphy  ),
     *               rbuffs(r1    ),rbuffs(vicww ),rbuffs(dicww ),
     *               rbuffs(rho   )                              )
c
      okee = okee .and. (ierror .eq. 0)
c--------------------------------------------------------------------
c-----Close trim-<runid>.dat and trim-<runid>.def MAP-files
c--------------------------------------------------------------------
 8888 continue
      call CLOSFL(fname, ierror)
      okee = okee .and. (ierror .eq. 0)
c-----------------------------------------------------------------------
c-----return status to calling routine
c-----------------------------------------------------------------------
      ierror = IEOK
      if (.not. okee) then
         ierror = IEOTHR
      endif
c
      return
c-----------------------------------------------------------------------
      end

      subroutine ods_tri_nef_map_getdata
c#ifdef WINNT
c    *          [ALIAS:'_ods_tri_nef_map_getdata']
c#endif
     *                 (
     *                  hdefds        ,hdafds        ,misval        ,
     *                  nmax          ,mmax          ,kmax          ,
     *                  lmax          ,noroco        ,itim          ,
     *                  lay           ,parcod        ,ierror        ,
     *                  lstci         ,ltur          ,kmaxon        ,
     *                  maxdim        ,xdata         ,
     *                  irocol        ,kcs           ,kcu           ,
     *                  kcv           ,kfu           ,kfv           ,
     *                  iflag         ,igrid         ,
     *                  xcor          ,ycor          ,xz            ,
     *                  yz            ,alfas         ,
     *                  dp            ,dps           ,thick         ,
     *                  s1            ,tauksi        ,taueta        ,
     *                  u1            ,v1            ,wphy          ,
     *                  r1            ,vicww         ,dicww         ,
     *                  rho                                         )
c-----------------------------------------------------------------------
c           Function: get map data out of TRISULA NEFIS files
c        Method used:
c
c-----------------------------------------------------------------------
C   Calling routine :              ODS_TRI_NEF_MAP_MAT
c-----------------------------------------------------------------------
c   Called  routines:              wrhcor
c                                  wrhwat
c                                  wrh3di
c                                  wrhuvi
c                                  wrhtai
c                                  wrhcon
c                                  wrhtur
c                                  wrhgrd
c                                  wrhtd
c                                  wrh3dv
c                                  wrhtav
c                                  wrhuvv
c
c-----------------------------------------------------------------------
c    Parameters:
c    -----------
c
c   Var.      Type Dimensions
c   -------------------------
c
c HDAFDS      I*4  999             Data file descriptor for the MAP-DAT
c                                  file
c HDEFDS      I*4  2997            Definition file description for the
c MISVAL      R*4                  missing value
c ITIM        I*4                  index of map time
c KMAX    I   I*4                  Number of layers
c LAY     I   I*4  3               Specified layers
c LMAX    I   I*4                  Number of constituents
c                                  for old files LMAX = LSTCI
c                                  for new files LMAX = LSTCI + LTUR
c LSTCI   I   I*4                  Total number of constituents (incl.
c                                  turbulence for old trim files).
c LTUR    I   I*4                  Number of turbulence constituents
c MMAX    I   I*4                  Number of gridpoints in the x-dir.
c NMAX    I   I*4                  Number of gridpoints in the y-dir.
c NOROCO  I   I*4                  Number of Computational rows & cols
c MAXDIM      I*4            I     length of data array
c XDATA       R*4   maxdim   O     array with the data
c IERROR      I*4            O     = 0 no errors, = 1 error detected
c PARCOD  I   I*4                  parameter to get data of
c                                   1 = 'wl' water-elevation
c                                   2 = 'wh' water-hight
c                                  16 = 'tu' taubottom u direction
c                                  17 = 'tv' taubottom v direction
c                                  18 = 'tm' taubottom magnitude
c                                  19 = 'ta' taubottom direction
c                                  39 = 'tf' taubottom vector field
c                                   7 = 'u ' velocity u direction
c                                   8 = 'v ' velocity v direction
c                                   9 = 'm ' velocity magnitude
c                                  10 = 'd ' velocity direction
c                                  40 = 'uv' velocity vector field
c  not yet implemented             41 = 'uw' velocity vector field
c  not yet implemented             42 = 'vw' velocity vector field
c                                   3 = 'du' dpt. aver. vel. u
c                                   4 = 'dv' dpt. aver. vel. v
c                                   5 = 'dm' dpt. aver. vel. mag
c                                   6 = 'dd' dpt. aver. vel. dir
c                                  38 = 'df' dpt. aver. vel vector
c                                  11 = 'w ' velocity w direction
c  not yet implemented             13 = 'qx' flow rate u
c  not yet implemented             14 = 'qy' flow rate v
c                                  2. = 'c ' concentration
c                                  15 = 'vw' viscosity
c                                  31 = 'rh' density
c                                  32 = 'dw' diffusity
c                                  35 = 'ag' active grid
c                                  36 = 'bl' boundary lines, including
c                                            permanent dry points
c                                  37 = 'td' time dependent dry points
c                                  44 = 'dl' depth lines in H-points
c                                  45 = 'dz' depth lines in Z-points
c                                  46 =      XCOR x coordinate depths
c                                  47 =      YCOR y coordinate depths
c                                  48 =      XCOR x coordinate zeta
c                                  49 =      YCOR y coordinate zeta
c                                  50 =      UVDAMS grid attribute
c  not yet implemented             51 =      z coordinate step
c  not yet implemented             52 =      z coordinate slope
c                                  12 =      z-coordinate
c DICWW   --  R*4  NMAX,MMAX,KMAX+1Diffusity in zeta points
c DP      --  R*4  NMAX,MMAX       Depth values in depth points
c DPS     --  R*4  NMAX,MMAX       Depth values in zeta points depending
c                                  on dryflp
c ALFAS   --  R*4  NMAX,MMAX       Transformation coefficients (in radians)
c IFLAG   --  I*4  NMAX,MMAX       Array for permanent and tempory dry
c                                  point
c IGRID   --  I*4  NMAX,MMAX       Array with actual grid
c IROCOL  --  I*4  5,NOROCO        Pointer table with bound. coord. and
c                                  bound. types (comp. cols. and rows)
c KCS     --  I*4  NMAX,MMAX       Mask array for the zeta points
c                                  (time independent)
c                                  =0 inactive point
c                                  =1 active   point
c                                  =2 open boundary point
c KCU     --  I*4  NMAX,MMAX       Mask array for the u-velocity point
c                                  (time independent)
c                                  =0 dry      point
c                                  =1 active   point
c KCV     --  I*4  NMAX,MMAX       Mask array for the v-velocity point
c                                  (time independent)
c                                  =0 dry      point
c                                  =1 active   point
c KFU     --  I*4  NMAX,MMAX       Mask array for the u-velocity point
c                                  (time dependent)
c                                  =0 dry      point
c                                  =1 active   point
c KFV     --  I*4  NMAX,MMAX       Mask array for the v-velocity point
c                                  (time dependent)
c                                  =0 dry      point
c                                  =1 active   point
c R1      --  R*4  NMAX,MMAX,KMAX+1,LMAX
c                                  Concentrations in zeta point
c RHO     --  R*4  NMAX,MMAX,KMAX  Density in zeta points
c S1      --  R*4  NMAX,MMAX       Water-level in zeta point
c TAUETA  --  R*4  NMAX,MMAX       Tau bottom in v-velocity point
c TAUKSI  --  R*4  NMAX,MMAX       Tau bottom in u-velocity point
c THICK   --  R*4  KMAX            Relative layer thickness
c U1      --  R*4  NMAX,MMAX,KMAX  U-velocity in u-velocity point
c V1      --  R*4  NMAX,MMAX,KMAX  V-velocity in v-velocity point
c VICWW   --  R*4  NMAX,MMAX,KMAX+1Viscosity in zeta points
c WPHY    --  R*4  NMAX,MMAX,KMAX  W-velocity in zeta point
c XCOR    --  R*4  NMAX,MMAX       X-coordinate in depth point
c XZ      --  R*4  NMAX,MMAX       X-coordinate in zeta point
c YCOR    --  R*4  NMAX,MMAX       Y-coordinate in depth point
c YZ      --  R*4  NMAX,MMAX       Y-coordinate in zeta point
c-----------------------------------------------------------------------
c    Local variables:
c    ----------------
c
c   Var.      Type Dimensions
c   -------------------------
c
c BUFLEN      I*4                  Size in bytes of available buffer
c FACTOR      R*4                  Scaling factor for W-velocities
c GRDANG      R*4                  Vertex between the y-axis and north
c GRPDEF     CH*16                 Group name definition
c IERROR      I*4                  Error code for NEFIS error
c IRHO        I*4                  Parameter to check if var='rh' is
c                                  permitted (irho = 1)
c KMAX        I*4                  Number of layers
c LMAX        I*4                  Number of constituents
c LAY         I*4                  Actual layer number
c K           I*4                  Loop variable
c M           I*4                  Loop variable
c N           I*4                  Loop variable
c N1          I*4                  Help variable
c N2          I*4                  Help variable
c NOROW       I*4                  Number of comp. rows  in IROCOL-array
c OKEE        L*4                  Flag for error checking
c UINDEX      I*4  3               Array with indices of the cells to
c                                  be read
c USRORD      I*4                  Sequence in which the cells must be
c                                  read
c-----------------------------------------------------------------------
c
c  declarations
c
      include         'ods.inc'
c
      integer       n            ,m            ,norow
      integer       nd           ,md           ,k
      integer       n1           ,n2           ,nlay
      integer       kmaxon
c
      integer       nmax         ,mmax         ,kmax         ,
     *              lmax         ,noroco       ,itim         ,
     *              lay(3)       ,parcod       ,ierror       ,
     *              maxdim       ,lstci        ,ltur
c
      logical       usefix
c
      real          misval       ,factor       ,grdang       ,
     *              depfac       ,depth        ,depflm
c
      real          xdata(maxdim)
c
      integer       irocol(5     ,noroco),kcs   (nmax  ,mmax  ),
     *              kcu   (nmax  ,mmax  ),kcv   (nmax  ,mmax  ),
     *              kfu   (nmax  ,mmax  ),kfv   (nmax  ,mmax  ),
     *              iflag (nmax  ,mmax  ),igrid (nmax  ,mmax  )
c
      real          xcor  (nmax  ,mmax  ),ycor  (nmax  ,mmax  ),
     *              xz    (nmax  ,mmax  ),yz    (nmax  ,mmax  ),
     *              alfas (nmax  ,mmax  ),
     *              dp    (nmax  ,mmax  ),dps   (nmax  ,mmax  ),
     *              thick (kmax+1)
      real          s1    (nmax  ,mmax  ),
     *              tauksi(nmax  ,mmax  ),taueta(nmax  ,mmax  ),
     *              u1    (nmax  ,mmax   ,kmax  ),
     *              v1    (nmax  ,mmax   ,kmax  ),
     *              wphy  (nmax  ,mmax   ,kmax  ),
     *              r1    (nmax  ,mmax   ,kmax+kmaxon ,lmax ),
     *              vicww (nmax  ,mmax   ,kmax+kmaxon),
     *              dicww (nmax  ,mmax   ,kmax+kmaxon),
     *              rho   (nmax  ,mmax   ,kmax  )
c
      logical       okee  ,lvar
c
c-----------------------------------------------------------------------
c-----Declarations NEFIS
      integer*4     hdefds( 2997),hdafds(  999)
c
      integer*4     uindex(    3),usrord,buflen
c
      character*16  grpdef

      character*16  elmtyp
      character*16  elmqty
      character*16  elmunt
      character*64  elmdsc
      integer       nbytsg
      integer       elmndm
      integer       elmdms(5)

c
      integer*4     GETELT,GETELS
c#ifdef WINNT
      integer       GETELT_i
c#endif
c
c-----------------------------------------------------------------------
c-----Initalize
      okee      = .true.
c-----default value for vertical velocities w
cAM:  this is unnecessary - the vertical scaling is provided by the
c     plotroutines
c     factor    = 1000.0
cAM
c-----------------------------------------------------------------------
c-----Read initial values from group 2
      grpdef    = 'map-const'
      uindex(1) = 1
      uindex(2) = 1
      uindex(3) = 1
      usrord    = 1
c
      buflen    = 4
      okee      = okee .and.
     *            GETELT(hdefds,grpdef    ,'GRDANG'  ,
     *                   uindex,usrord    ,buflen    ,GRDANG    )
     *            .eq. 0

c-----------------------------------------------------------------------
c-----Get the sigma-layer thicknesses or the fixed layer coordinates
c     If fixed layer coordinates are available, then they are preferred.
c     If neither information is available, we have a problem
c
      buflen    =  4 * (kmax+1)
      ierror    = GETELT(hdefds,grpdef    ,'ZK'      ,
     *                   uindex,usrord    ,buflen    ,THICK     )
      if ( ierror .eq. 0 ) then
         ierror = INQELM(hdefds,'ZK', elmtyp, nbytsg, elmqty, elmunt,
     *               elmdsc, elmdim, elmdms )
         if ( elmdms(1) .eq. 1 ) then
            ierror = -11111
         endif
      endif

      if ( ierror .ne. 0 ) then
         usefix = .false.
         ierror = GETELT(hdefds,grpdef    ,'THICK'   ,
     *                   uindex,usrord    ,buflen    ,THICK     )
         if ( ierror .ne. 0 ) then
            okee = .false.
         endif
      else
         usefix = .true.
         depflm = thick(1)
         do 5 k = 1,kmax
            thick(k) = thick(k+1) - thick(k)
    5    continue
      endif

c-----------------------------------------------------------------------
c-----Read and calculate coordinate arrays
c     s1 will be used as buffer array
      call    wrhcor(okee      ,hdefds    ,hdafds    ,misval    ,
     *               nmax      ,mmax      ,noroco    ,norow     ,
     *               irocol    ,kcs       ,kcu       ,kcv       ,
     *               xcor      ,ycor      ,xz        ,yz        ,
     *               alfas                ,dp        ,dps       ,
     *               s1                                         )
c
c-----for time dependent blocks read kfu and kfv-----------
c     not needed for;
c        active grid                      parcod = 35  'ag'
c        boundary lines                   parcod = 36  'bl'
c        dpt. at d_points                 parcod = 44  'dl'
c        dpt. at z_points                 parcod = 45  'dz'
      lvar   = ((parcod .ne. 35) .and. (parcod .ne. 36) .and.
     *          (parcod .ne. 44) .and. (parcod .ne. 45))
      if (lvar ) then
         grpdef    = 'map-series'
         usrord    = 1
         buflen    = 4 * nmax   * mmax
         uindex(1) = itim
         uindex(2) = itim
         uindex(3) = 1
c
c--------Read from group 3 KFU
         okee      = okee .and.
     *               GETELT(hdefds,grpdef    ,'KFU   '  ,
     *                      uindex,usrord ,buflen    ,KFU       )
     *            .eq. 0
c
c--------Read from group 3 KFV
         okee      = okee .and.
     *               GETELT(hdefds,grpdef    ,'KFV   '  ,
     *                      uindex,usrord ,buflen    ,KFV       )
     *            .eq. 0
      endif
c
c--------------------------------------------------------------------
c-----get data
      if (parcod .eq. 1 .or. parcod .eq.  2 ) then
c--------water level, ZWL                 parcod =  1  'wl'
c--------total water depth, ZWL + DPS     parcod =  2  'wh'
         call wrhwat(okee      ,hdefds    ,hdafds    ,parcod    ,
     *               itim      ,nmax      ,mmax      ,
     *               kcs       ,kfu       ,kfv       ,
     *               dps       ,s1                              )
c--------store results in xdata
         do 10 n = 1,nmax
         do 10 m = 1,mmax
            n1           =   (n-1) * mmax +  m
            xdata ( n1 ) = s1    (n,m)
   10    continue
      else if (parcod .ge.  3 .and. parcod .le. 6 ) then
c--------dpt. aver. cur. u         {precon: parcod= 3} 'du'
c--------dpt. aver. cur. v         {precon: parcod= 4} 'dv'
c--------dpt. aver. cur. mag.      {precon: parcod= 5} 'dm'
c--------dpt. aver. cur. dir.      {precon: parcod= 6} 'dd'
         call wrh3di(okee      ,hdefds    ,hdafds    ,parcod    ,
     *               grdang    ,itim      ,misval    ,
     *               nmax      ,mmax      ,kmax      ,
     *               kcs       ,kfu       ,kfv       ,
     *               xcor      ,ycor      ,alfas     ,
     *               u1        ,v1        ,thick                )
c--------store results in xdata
         if ( parcod .eq.  3 .or. parcod .eq.  5 ) then
            do 30 n = 1,nmax
            do 30 m = 1,mmax
               n1           =   (n-1) * mmax +  m
               xdata ( n1 ) = u1    (n ,m , 1)
   30       continue
         else if ( parcod .eq.  4 .or. parcod .eq.  6 ) then
            do 40 n = 1,nmax
            do 40 m = 1,mmax
               n1           =   (n-1) * mmax +  m
               xdata ( n1 ) = v1    (n ,m , 1)
   40       continue
         endif
      else if (parcod .ge.  7 .and. parcod .le. 10) then
c--------current u (layer)         {precon: parcod= 7} 'u '
c--------current v (layer)         {precon: parcod= 8} 'v '
c--------current mag. (layer)      {precon: parcod= 9} 'm '
c--------current dir. (layer)      {precon: parcod=10} 'd '
         call wrhuvi(okee      ,hdefds    ,hdafds    ,parcod    ,
     *               grdang    ,itim      ,misval    ,
     *               nmax      ,mmax      ,kmax      ,
     *               kcs       ,kfu       ,kfv       ,
     *               xcor      ,ycor      ,
     *               alfas                ,u1        ,v1        )
c--------store results in xdata
         if ( parcod .eq.  7 .or. parcod .eq.  9 ) then
            n1 = 0
            do 70 k = lay(1), lay(2), lay(3)
            do 70 n = 1,nmax
            do 70 m = 1,mmax
               n1           = n1 + 1
               xdata ( n1 ) = u1    (n ,m ,k )
   70       continue
         else if ( parcod .eq.  8 .or. parcod .eq. 10 ) then
            n1 = 0
            do 80 k = lay(1), lay(2), lay(3)
            do 80 n = 1,nmax
            do 80 m = 1,mmax
               n1           = n1 + 1
               xdata ( n1 ) = v1    (n ,m ,k )
   80       continue
         endif
      else if (parcod .eq. 11 ) then
c--------current w.   (layer)                          'w '
         grpdef    = 'map-series'
         usrord    = 1
         buflen    = 4 * nmax   * mmax   * kmax
         uindex(1) = itim
         uindex(2) = itim
         uindex(3) = 1
c-----------------------------------------------------------------------
c--------Read from group 3 WPHY
c-----------------------------------------------------------------------
         okee      = okee .and.
     *               GETELT(hdefds   ,grpdef    ,'WPHY  '  ,
     *                      uindex   ,usrord ,buflen    ,WPHY      )
     *               .eq. 0
c--------Calculate WPHY*FACTOR
c        if kcs = 0 then permanent drypoint; if surrounding kfu and kfv
c        := 0 then temporary drypoint
         n1 = 0
         do 110 k = lay(1), lay(2), lay(3)
         do 110 n = 1,nmax
         do 110 m = 1,mmax
            md    = max (1,m-1)
            nd    = max (1,n-1)
            lvar  = ((kcs   (n ,m ) .eq. 1) .and.
     *               (kfu   (n ,m ) .eq. 1  .or.
     *                kfu   (n ,md) .eq. 1  .or.
     *                kfv   (n ,m ) .eq. 1  .or.
     *                kfv   (nd,m ) .eq. 1))
            n1           = n1 + 1
            if (lvar  ) then
cAM:
c        see remark above
cAM            xdata ( n1 ) = wphy(n ,m ,k ) * factor
               if ( wphy(n,m,k) .ne. -999.0 ) then
                  xdata ( n1 ) = wphy(n ,m ,k )
               else
                  xdata ( n1 ) = misval
               endif
            else
               xdata ( n1 ) = misval
            endif
  110    continue
      else if (parcod .eq. 12 ) then
c--------z-coordinate
c        beware of exceeding the array space:
c        this parameter is used mainly in connection with
c        others, and therefore the array space
c
c        First get the water level for the required time (parcod=1)
c
         n1 = 1
         call wrhwat(okee      ,hdefds    ,hdafds    ,n1        ,
     *               itim      ,nmax      ,mmax      ,
     *               kcs       ,kfu       ,kfv       ,
     *               dps       ,s1                              )
c
         nlay = lay(2) + 1
         if ( maxdim .lt. nmax*mmax*nlay ) nlay = maxdim / nmax / mmax
c
c-------Two cases arise:
c       Sigma-coordinates and fixed layer coordinates
c       Note:
c       Due to consistent but apparently strange aspects of the
c       plots (having to do with quick changes in depth that are
c       averaged or lead to triangular grid cells), we set the
c       depth to be constant over the layers, except near the
c       surface.
         if ( usefix ) then
            n1 = 0
            do 125 k = lay(1), nlay, lay(3)
               if ( k .eq. 1 ) then
                  depth = depflm
               else
                  depth = depth + thick(k-1)
               endif

               do 124 n = 1,nmax
                  do 123 m = 1,mmax
                     nd    = max(1,n-1)
                     md    = max(1,m-1)
                     lvar  =  (kcs   (n ,m ) .eq. 1)
                     n1    = n1 + 1
                     if ( lvar ) then
                        xdata(n1) = depth
                        if ( xdata(n1) .gt. s1(n,m) ) then
                           xdata(n1) = s1(n,m)
                        endif
                     else
                        xdata(n1) = misval
                     endif
  123             continue
  124          continue
  125       continue
         else
            n1 = 0
            depfac = 0.0
            do 129 k = lay(1), nlay, lay(3)

c-------Determine the contribution of the depth
c-------The "if" statement is essential: in case of
c-------2d-variables
               if ( k .eq. 1 ) then
                  depfac = 0.0
               else
                  depfac = depfac + thick(k-1)
                  if ( k .eq. nlay ) depfac = 1.0
               endif

               do 128 n = 1,nmax
                  do 127 m = 1,mmax
                     md    = max (1,m-1)
                     nd    = max (1,n-1)
                     lvar  =  (kcs   (n ,m ) .eq. 1)
c
c--------Be careful: all active cells must have a z-coordinate!
c           lvar  = ((kcs   (n ,m ) .eq. 1) .and.
c    *               (kfu   (n ,m ) .eq. 1  .or.
c    *                kfu   (n ,md) .eq. 1  .or.
c    *                kfv   (n ,m ) .eq. 1  .or.
c    *                kfv   (nd,m ) .eq. 1))
                     n1           = n1 + 1
                     if (lvar  ) then
                        xdata(n1) = s1(n,m) - depfac * (s1(n,m)+dp(n,m))
                     else
                        xdata(n1) = misval
                     endif
  127             continue
  128          continue
  129       continue
         endif

      else if (parcod .eq. 13 ) then
c--------flow rate u, ZQXK                    not yet implemented
      else if (parcod .eq. 14 ) then
c--------flow rate v, ZQYK                    not yet implemented
      else if (parcod .eq. 15 ) then
c--------eddy viscosity, ZVICWW           parcod = 15 'vw' or 'nu'
         grpdef    = 'map-series'
         usrord    = 1
         buflen    = 4 * nmax   * mmax  * (kmax+kmaxon)
         uindex(1) = itim
         uindex(2) = itim
         uindex(3) = 1
c--------Read from group 3 ZVICWW
         okee      = okee .and.
     *               GETELT(hdefds,grpdef    ,'VICWW '  ,
     *                      uindex,usrord ,buflen    ,VICWW     )
     *            .eq. 0
c--------if kcs = 0 then permanent drypoint; if surrounding kfu and kfv
c        := 0 then temporary drypoint
         n1 = 0
         do 150 k = lay(1), lay(2), lay(3)
         do 150 n = 1,nmax
         do 150 m = 1,mmax
            md    = max (1,m-1)
            nd    = max (1,n-1)
            lvar  = ((kcs   (n ,m ) .eq. 1) .and.
     *               (kfu   (n ,m ) .eq. 1  .or.
     *                kfu   (n ,md) .eq. 1  .or.
     *                kfv   (n ,m ) .eq. 1  .or.
     *                kfv   (nd,m ) .eq. 1))
            n1           = n1 + 1
            if (lvar  ) then
               xdata ( n1 ) = vicww (n ,m ,k+kmaxon )
            else
               xdata ( n1 ) = misval
            endif
  150    continue
      else if (parcod .ge. 16 .and. parcod .le.19 ) then
c--------bed stress u, ZTAUKSI   {precon: parcod=16}   'tu'
c--------bed stress v, ZTAUETA   {precon: parcod=17}   'tv'
c--------bed stress mag, ZTAUETA {precon: parcod=18}   'tm'
c--------bed stress dir, ZTAUETA {precon: parcod=19}   'ta'
         call wrhtai(okee      ,hdefds    ,hdafds    ,parcod    ,
     *               misval    ,
     *               grdang    ,itim      ,nmax      ,mmax      ,
     *               kcs       ,kfu       ,kfv       ,
     *               xcor      ,ycor      ,alfas     ,
     *               tauksi    ,taueta                          )
c--------store results in xdata
         if ( parcod .eq. 16 .or. parcod .eq. 18 ) then
            do 160 n = 1,nmax
            do 160 m = 1,mmax
               n1           =   (n-1) * mmax +  m
               xdata ( n1 ) = tauksi(n ,m )
  160       continue
         else if ( parcod .eq. 17 .or. parcod .eq. 19 ) then
            do 170 n = 1,nmax
            do 170 m = 1,mmax
               n1           =   (n-1) * mmax +  m
               xdata ( n1 ) = taueta(n ,m )
  170       continue
         endif
      else if (parcod .ge. 20 .and. parcod .le.29 ) then
         if ( (parcod-19) .le. lstci ) then
c-----------constituents, GRO(1:lstci)
c                          {precon: <con.index>=parcod-19}
            call wrhcon(okee      ,hdefds    ,hdafds    ,itim      ,
     *                  xdata     ,maxdim    ,parcod    ,lay       ,
     *                  misval    ,
     *                  nmax      ,mmax      ,kmax      ,lstci     ,
     *                  kcs       ,kfu       ,kfv       ,r1        )
         else
c-----------constituents, ZTUR(1:ltur)
c                          {precon: <con.index>=parcod-19-lstci}
            call wrhtur(okee      ,hdefds    ,hdafds    ,itim      ,
     *                  misval    ,
     *                  nmax      ,mmax      ,kmax+1    ,ltur      ,
     *                  kcs       ,kfu       ,kfv       ,r1        )
c-----------store results in xdata
            n1 = 0
            do 210 k = lay(1), lay(2), lay(3)
            do 210 n = 1,nmax
            do 210 m = 1,mmax
               n1           = n1 + 1
               xdata ( n1 ) = r1 (n ,m , k+1, parcod-19-lstci)
  210       continue
         endif
      else if (parcod .eq. 31 ) then
c--------density, ZRHO                    parcod = 31 'rh'
         grpdef    = 'map-series'
         usrord    = 1
         buflen    = 4 * nmax   * mmax  * kmax
         uindex(1) = itim
         uindex(2) = itim
         uindex(3) = 1
c--------Read from group 3 RHO
         okee   = okee .and.
     *            GETELT(hdefds   ,grpdef    ,'RHO   '  ,
     *                   uindex   ,usrord ,buflen    ,RHO       )
     *            .eq. 0
c--------if kcs = 0 then permanent drypoint; if surrounding kfu and kfv
c        := 0 then temporary drypoint
         n1 = 0
         do 310 k = lay(1), lay(2), lay(3)
         do 310 n = 1,nmax
         do 310 m = 1,mmax
            md    = max (1,m-1)
            nd    = max (1,n-1)
            lvar  = ((kcs   (n ,m ) .eq. 1) .and.
     *               (kfu   (n ,m ) .eq. 1  .or.
     *                kfu   (n ,md) .eq. 1  .or.
     *                kfv   (n ,m ) .eq. 1  .or.
     *                kfv   (nd,m ) .eq. 1))
            n1           = n1 + 1
            if (lvar  ) then
               xdata ( n1 ) = rho   (n ,m ,k )
            else
               xdata ( n1 ) = misval
            endif
  310    continue
      else if (parcod .eq. 32 ) then
c--------eddy diffusivity, ZDICWW         parcod = 32 'dw' or 'k '
         grpdef    = 'map-series'
         usrord    = 1
         buflen    = 4 * nmax   * mmax  * (kmax+kmaxon)
         uindex(1) = itim
         uindex(2) = itim
         uindex(3) = 1
c--------Read from group 3 DICWW
         okee   = okee .and.
     *            GETELT(hdefds   ,grpdef    ,'DICWW '  ,
     *                   uindex   ,usrord ,buflen    ,DICWW     )
     *            .eq. 0
c--------if kcs = 0 then permanent drypoint; if surrounding kfu and kfv
c        := 0 then temporary drypoint
         n1 = 0
         do 320 k = lay(1), lay(2), lay(3)
         do 320 n = 1,nmax
         do 320 m = 1,mmax
            md    = max (1,m-1)
            nd    = max (1,n-1)
            lvar  = ((kcs   (n ,m ) .eq. 1) .and.
     *               (kfu   (n ,m ) .eq. 1  .or.
     *                kfu   (n ,md) .eq. 1  .or.
     *                kfv   (n ,m ) .eq. 1  .or.
     *                kfv   (nd,m ) .eq. 1))
            n1           = n1 + 1
            if (lvar  ) then
               xdata ( n1 ) = dicww (n ,m ,k+kmaxon )
            else
               xdata ( n1 ) = misval
            endif
  320    continue
      else if (parcod .eq. 35 .or. parcod .eq. 36 ) then
c--------active grid                      parcod = 35  'ag'
c--------boundary lines                   parcod = 36  'bl'
         call wrhgrd(nmax      ,mmax      ,noroco    ,norow     ,
     *               irocol    ,kcu       ,kcv       ,iflag     ,
     *               igrid                                      )
c--------store results in xdata
         if ( parcod .eq. 35 ) then
            do 350 n = 1,nmax
            do 350 m = 1,mmax
               n1           =   (n-1) * mmax +  m
               xdata ( n1 ) = igrid (n ,m )
  350       continue
         else if ( parcod .eq. 36 ) then
            do 360 n = 1,nmax
            do 360 m = 1,mmax
               n1           =   (n-1) * mmax +  m
               xdata ( n1 ) = iflag (n ,m )
  360       continue
         endif
      else if (parcod .eq. 37 ) then
c--------temporary dry pnts.                           'td'
         call wrhtd (nmax      ,mmax      ,noroco    ,norow     ,
     *               irocol    ,kcu       ,kcv       ,
     *               kfu       ,kfv       ,iflag                )
c--------store results in xdata
         do 370 n = 1,nmax
         do 370 m = 1,mmax
            n1           =   (n-1) * mmax +  m
            xdata ( n1 ) = iflag (n ,m )
  370    continue
      else if (parcod .eq. 38 ) then
c--------vectors dpth. aver.                           'df'
         call wrh3dv(okee      ,hdefds    ,hdafds    ,misval    ,
     *               itim      ,nmax      ,mmax      ,kmax      ,
     *               kcs       ,kfu       ,kfv       ,
     *               xcor      ,ycor      ,alfas     ,
     *               u1        ,v1        ,thick                )
c--------store results in xdata
         do 380 n = 1,nmax
         do 380 m = 1,mmax
            n1           =   (n-1) * mmax +  m
            n2           =    nmax * mmax +      n1
            xdata ( n1 ) = u1    (n ,m , 1)
            xdata ( n2 ) = v1    (n ,m , 1)
  380    continue
      else if (parcod .eq. 39 ) then
c--------vectors bed stress                            'tf'
         call wrhtav(okee      ,hdefds    ,hdafds    ,misval    ,
     *               itim      ,nmax      ,mmax      ,
     *               kcs       ,kfu       ,kfv       ,
     *               xcor      ,ycor      ,alfas     ,
     *               tauksi    ,taueta                          )
c--------store results in xdata
         do 390 n = 1,nmax
         do 390 m = 1,mmax
            n1           =   (n-1) * mmax +  m
            n2           =    nmax * mmax +      n1
            xdata ( n1 ) = tauksi(n ,m )
            xdata ( n2 ) = taueta(n ,m )
  390    continue
      else if (parcod .eq. 40 ) then
c--------vectors velocity uv                           'uv'
         call wrhuvv(okee      ,hdefds    ,hdafds    ,misval    ,
     *               itim      ,nmax      ,mmax      ,kmax      ,
     *               kcs       ,kfu       ,kfv       ,
     *               xcor      ,ycor      ,
     *               alfas                ,u1        ,v1        )
c--------store results in xdata
         n1 = 0
         n2 = nmax * mmax * ( ( lay(2) - lay(1) ) / lay(3) + 1 )
         do 400 k = lay(1), lay(2), lay(3)
         do 400 n = 1,nmax
         do 400 m = 1,mmax
            n1           = n1 + 1
            n2           = n2 + 1
            xdata ( n1 ) = u1    (n ,m ,k )
            xdata ( n2 ) = v1    (n ,m ,k )
  400    continue
      else if (parcod .eq. 41 ) then
c--------vectors velocity uw               not yet implemented
      else if (parcod .eq. 42 ) then
c--------vectors velocity vw               not yet implemented
      else if (parcod .eq. 44 ) then
c--------dpt. at d_points                 parcod = 44  'dl'
c-----------------------------------------------------------------------
c--------if kcs = 0 then permanent drypoint
c-----------------------------------------------------------------------
         do 440 n = 1,nmax
         do 440 m = 1,mmax
            lvar  = ((kcs   (n ,m ) .ne. 0))
            n1           =   (n-1) * mmax +  m
            if (lvar  ) then
               xdata ( n1 ) = dp    (n ,m )
            else
               xdata ( n1 ) = misval
            endif
  440    continue
      else if (parcod .eq. 45 ) then
c--------dpt. at z_points                 parcod = 45  'dz'
c--------if kcs = 0 then permanent drypoint
         do 450 n = 1,nmax
         do 450 m = 1,mmax
            lvar  = ((kcs   (n ,m ) .ne. 0))
            n1           =   (n-1) * mmax +  m
            if (lvar  ) then
               xdata ( n1 ) = dps   (n ,m )
            else
               xdata ( n1 ) = misval
            endif
  450    continue
      else if (parcod .eq. 46 ) then
c--------XCOR                             parcod = 46
         do 460 n = 1,nmax
         do 460 m = 1,mmax
            n1           =   (n-1) * mmax +  m
            xdata ( n1 ) = xcor(n,m)
  460    continue
      else if (parcod .eq. 47 ) then
c--------YCOR                             parcod = 47
         do 470 n = 1,nmax
         do 470 m = 1,mmax
            n1           =   (n-1) * mmax +  m
            xdata ( n1 ) = ycor(n,m)
  470    continue
      else if (parcod .eq. 48 ) then
c--------XZ                               parcod = 48
         do 480 n = 1,nmax
         do 480 m = 1,mmax
            n1           =   (n-1) * mmax +  m
            xdata ( n1 ) = xz  (n,m)
  480    continue
      else if (parcod .eq. 49 ) then
c--------XZ                               parcod = 49
         do 490 n = 1,nmax
         do 490 m = 1,mmax
            n1           =   (n-1) * mmax +  m
            xdata ( n1 ) = yz  (n,m)
  490    continue
      else if (parcod .eq. 50 ) then
c--------UVDAMS                           parcod = 50
         do 500 n = 1,nmax
         do 500 m = 1,mmax
            n1           =   (n-1) * mmax +  m
            xdata ( n1 ) = kfu(n,m) * 1.0 + kfv(n,m) * 2.0
  500    continue
      else if (parcod .eq. 51 ) then
c--------z coordinate step                 not yet implemented
      else if (parcod .eq. 52 ) then
c--------z coordinate slope                not yet implemented
      endif
c
c-----------------------------------------------------------------------
      ierror = IEOK
      if (.not. okee) then
         ierror = IEOTHR
      endif
c
      return
      end

      subroutine wrhcor(okee      ,hdefds    ,hdafds    ,misval    ,
     *                  nmax      ,mmax      ,noroco    ,norow     ,
     *                  irocol    ,kcs       ,kcu       ,kcv       ,
     *                  xcor      ,ycor      ,xz        ,yz        ,
     *                  alfas                ,dp        ,dps       ,
     *                  zbuff                                      )
c-----------------------------------------------------------------------
c             Module: SUBROUTINE WRHCOR
c           Function: - read NEFIS data, coordinates from file
c                     - calculate model frame
c        Method used:
c
c-----------------------------------------------------------------------
c   Calling routine :              ods_tri_nef_map_getdata
c-----------------------------------------------------------------------
c   Called  routines:              GETELT
c-----------------------------------------------------------------------
c  Formal parameters:
c  ------------------
c
c   Var. I/O  Type Dimensions
c   -------------------------
c
c DP       O  R*4  NMAX,MMAX       Depth values in depth points
c DPS      O  R*4  NMAX,MMAX       Depth values in zeta points
c OKEE    IO  L*4                  Flag for further execution program
c ALFAS    O  R*4  NMAX,MMAX       Transformation coefficients (in radians)
c HDAFDS  I   I*4  999             Data file descriptor for the MAP-DAT
c                                  file
c HDEFDS  I   I*4  2997            Definition file description for the
c                                  MAP-DEF file
c MISVAL  I   R*4                  missing value
c IROCOL   O  I*4  5,NOROCO        Pointer table with bound. coord. and
c                                  bound. types (comp. cols. and rows)
c KCS      O  I*4  NMAX,MMAX       Mask array for the zeta points
c                                  (time independent)
c                                  =0 inactive point
c                                  =1 active   point
c                                  =2 open boundary point
c KCU      O  I*4  NMAX,MMAX       Mask array for the u-velocity point
c                                  (time independent)
c                                  =0 dry      point
c                                  =1 active   point
c KCV      O  I*4  NMAX,MMAX       Mask array for the v-velocity point
c                                  (time independent)
c                                  =0 dry      point
c                                  =1 active   point
c MMAX    I   I*4                  Number of gridpoints in the x-dir.
c NMAX    I   I*4                  Number of gridpoints in the y-dir.
c NOROCO  I   I*4                  Number of Computational rows & cols
c NOROW    O  I*4                  Number of comp. rows  in IROCOL-array
c XCOR     O  R*4  NMAX,MMAX       X-coordinate in depth point
c XZ       O  R*4  NMAX,MMAX       X-coordinate in zeta point
c YCOR     O  R*4  NMAX,MMAX       Y-coordinate in depth point
c YZ       O  R*4  NMAX,MMAX       Y-coordinate in zeta point
c ZBUFF    O  R*4  NMAX,MMAX       Buffer array to read nefis files
c-----------------------------------------------------------------------
c    Local variables:
c    ----------------
c
c   Var.      Type Dimensions
c   -------------------------
c
c BUFLEN      I*4                  Buffer length
c DRYFLP     CH*4                  Drying and flooding procedure
c GRPDEF     CH*16                 Group name definition
c M           I*4                  Loop variable
c MD          I*4                  Help variable
c N           I*4                  Loop variable
c ND          I*4                  Help variable
c UINDEX      I*4  3               Array containing cell indices with
c                                  has to be read
c USRORD      I*4                  Sequence in which the cells must be
c                                  read
c-----------------------------------------------------------------------
c
c  declarations
c
      integer       nmax  ,mmax  ,norow ,noroco,n     ,m
      integer       nd    ,md
c
      real          misval,pi    ,degrad,dxdksi ,dydksi
c
      integer       irocol(5     ,noroco),kcs   (nmax  ,mmax  ),
     *              kcu   (nmax  ,mmax  ),kcv   (nmax  ,mmax  )
c
      real          xcor  (nmax  ,mmax  ),ycor  (nmax  ,mmax  ),
     *              xz    (nmax  ,mmax  ),yz    (nmax  ,mmax  ),
     *              alfas (nmax  ,mmax  ),
     *              dp    (nmax  ,mmax  ),dps   (nmax  ,mmax  )
      real          zbuff (nmax  ,mmax  )
c
      character*4   dryflp
c
      logical       okee
c-----------------------------------------------------------------------
c-----Declarations NEFIS
c-----------------------------------------------------------------------
      integer*4     hdefds( 2997),hdafds(  999)
c
      integer*4     uindex(    3),usrord,buflen, ierror
c
      character*16  grpdef
c
      integer*4     GETELT,GETELS
c#ifdef WINNT
      integer*4     GETELT_i
c#endif
c
c-----------------------------------------------------------------------
      dryflp = 'NO  '
      pi = 4.0 * atan( 1.0 )
c
      do 10 m = 1,mmax
         do 10 n = 1,nmax
            dps   (n,m) = misval
            xz    (n,m) = misval
            yz    (n,m) = misval
   10 continue
c-----------------------------------------------------------------------
c-----Initialize Nefis variables
c-----------------------------------------------------------------------
      grpdef    = 'map-const'
      uindex(1) = 1
      uindex(2) = 1
      uindex(3) = 1
      usrord    = 1
c-----------------------------------------------------------------------
c-----Read NOROW and IROCOL tabel from group 2
c-----------------------------------------------------------------------
      buflen    = 4
      okee      = okee .and.
     *            GETELT(hdefds,grpdef    ,'NOROW '  ,
     *                   uindex,usrord    ,buflen    ,NOROW     )
     *            .eq. 0
c
      buflen    =  4 * 5 * noroco
      okee      = okee .and.
     *            GETELT(hdefds,grpdef    ,'IROCOL'  ,
     *                   uindex,usrord    ,buflen    ,IROCOL    )
     *            .eq. 0
c-----------------------------------------------------------------------
c-----Read KCS array from group 2
c-----------------------------------------------------------------------
      buflen    =  4 * nmax  * mmax
      okee      = okee .and.
     *            GETELT(hdefds,grpdef    ,'KCS   '  ,
     *                   uindex,usrord    ,buflen    ,KCS       )
     *            .eq. 0
c-----------------------------------------------------------------------
c-----Read KCU array from group 2
c-----------------------------------------------------------------------
      buflen    =  4 * nmax  * mmax
      okee      = okee .and.
     *            GETELT(hdefds,grpdef    ,'KCU   '  ,
     *                   uindex,usrord    ,buflen    ,KCU       )
     *            .eq. 0
c-----------------------------------------------------------------------
c-----Read KCV array from group 2
c-----------------------------------------------------------------------
      buflen    =  4 * nmax  * mmax
      okee      = okee .and.
     *            GETELT(hdefds,grpdef    ,'KCV   '  ,
     *                   uindex,usrord    ,buflen    ,KCV       )
     *            .eq. 0
c-----------------------------------------------------------------------
c-----Read XCOR array from group 2 in buffer and calculate min and max
c-----------------------------------------------------------------------
      buflen    =  4 * nmax  * mmax
      okee      = okee .and.
     *            GETELT(hdefds,grpdef    ,'XCOR  '  ,
     *                   uindex,usrord    ,buflen    ,XCOR      )
     *            .eq. 0
c
c-----------------------------------------------------------------------
c-----Read YCOR array from group 2 in buffer and calculate min and max
c-----------------------------------------------------------------------
      buflen    =  4 * nmax  * mmax
      okee      = okee .and.
     *            GETELT(hdefds,grpdef    ,'YCOR  '  ,
     *                   uindex,usrord    ,buflen    ,YCOR      )
     *            .eq. 0
c
c-----------------------------------------------------------------------
c-----Try to read ALFAS array from group 2 - if not available (old version)
c-----then compute it from XCOR and YCOR
c-----------------------------------------------------------------------
c
      buflen    =  4 * nmax  * mmax
      ierror    =
     *            GETELT(hdefds,grpdef    ,'ALFAS '  ,
     *                   uindex,usrord    ,buflen    ,ALFAS     )

      if ( ierror .ne. 0 ) then
         do 110 m = 1,mmax
           do 110 n = 1,nmax
             md = max( 1,m-1 )
             nd = max( 1,n-1 )
             dxdksi = 0.5 * ( xcor(n,m) - xcor(n,md) +
     *                        xcor(nd,m) - xcor(nd,md) )
             dydksi = 0.5 * ( ycor(n,m) - ycor(n,md) +
     *                        ycor(nd,m) - ycor(nd,md) )
             if ( dxdksi .eq. 0. .and. dydksi .eq. 0. ) then
               alfas(n,m) = 0.0
             else
               alfas(n,m) = atan2( dydksi,dxdksi )
             endif
  110    continue
      else
        degrad = pi / 180.
        do 115 m = 1,mmax
          do 115 n = 1,nmax
            alfas(n,m) = alfas(n,m) * degrad
  115   continue
      endif

c-----------------------------------------------------------------------
c-----NOTE: do not fill in missing value, unless xcor(i,j) is 0
c-----(xcor can be and must be defined for inactive cells - kcs=0)
c-----AM (dd. 4 february 1997):
c-----This correction appears to be unnecessary and awkward anyway
c-----in case the coordinate "0.0" is a useful coordinate.
c-----------------------------------------------------------------------
c     do 120 n = 1,nmax
c        do 120 m = 1,mmax
c           if (abs(xcor (n,m)) .lt. 1.0e-8 ) then
c              xcor  (n,m) = misval
c           endif
c           if (abs(ycor (n,m)) .lt. 1.0e-8 ) then
c              ycor  (n,m) = misval
c           endif
c 120 continue
c
c-----------------------------------------------------------------------
c-----Read XZ array from group 2 in buffer for points with kcs <> 0
c-----------------------------------------------------------------------
      buflen    =  4 * nmax  * mmax
      okee      = okee .and.
     *            GETELT(hdefds,grpdef    ,'XZ    '  ,
     *                   uindex,usrord    ,buflen    ,ZBUFF     )
     *            .eq. 0
c
      do 210 n = 1,nmax
         do 210 m = 1,mmax
            if (kcs  (n,m) .ne. 0) then
               xz    (n,m) = zbuff (n,m)
            endif
  210 continue
c-----------------------------------------------------------------------
c-----Read YZ array from group 2 in buffer for points with kcs <> 0
c-----------------------------------------------------------------------
      buflen    =  4 * nmax  * mmax
      okee      = okee .and.
     *            GETELT(hdefds,grpdef    ,'YZ    '  ,
     *                   uindex,usrord    ,buflen    ,ZBUFF     )
     *            .eq. 0
c
      do 250 n = 1,nmax
         do 250 m = 1,mmax
            if (kcs  (n,m) .ne. 0) then
               yz    (n,m) = zbuff (n,m)
            endif
  250 continue
c-----------------------------------------------------------------------
c-----Read DP0 array from group 2 calculate min and max
c-----------------------------------------------------------------------
      buflen    =  4 * nmax   * mmax
      okee      = okee .and.
     *            GETELT(hdefds,grpdef    ,'DP0   '  ,
     *                   uindex,usrord    ,buflen    ,DP        )
     *            .eq. 0
c-----------------------------------------------------------------------
c-----Read DRYFLP from group 2 and calculate DPS depending on value
c     of dryflp
c-----------------------------------------------------------------------
      buflen    = 4
      okee      = okee .and.
c#ifdef WINNT
c    *            GETELT_i
c#else
     *            GETELS
c#endif
     *                  (hdefds,grpdef    ,'DRYFLP'  ,
     *                   uindex,usrord    ,buflen    ,DRYFLP    )
     *            .eq. 0
c
      if (dryflp .eq. 'MEAN') then
c-----------------------------------------------------------------------
c--------calculate dps depth in zeta point as the mean of 4 surrounding
c        depth points
c-----------------------------------------------------------------------
         do 410 m = 1,mmax
            do 420 n = 1,nmax
               md    = max(1,m-1)
               nd    = max(1,n-1)
               if (kcs  (n,m) .ne. 0) then
                  dps   (n,m) = 0.25 * (dp (n ,m ) + dp (n ,md)  +
     *                                  dp (nd,md) + dp (nd,m ))
               endif
  420       continue
  410    continue
      else if (dryflp .eq. 'MIN ') then
c-----------------------------------------------------------------------
c--------calculate dps depth in zeta point as mean of 2 minima of 2
c        surrounding depth points
c-----------------------------------------------------------------------
         do 430 m = 1,mmax
            do 440 n = 1,nmax
               md    = max(1,m-1)
               nd    = max(1,n-1)
               if (kcs  (n,m) .ne. 0) then
                  dps   (n,m) = 0.5 * (min (dp (n ,m ),dp (nd,md))  +
     *                                 min (dp (n ,md),dp (nd,m )))
               endif
  440       continue
  430    continue
      else
c-----------------------------------------------------------------------
c--------calculate dps depth in zeta point as maximum of 4 depth points
c        dryflp = 'MAX' or dryflp = 'NO'
c-----------------------------------------------------------------------
         do 450 m = 1,mmax
            do 460 n = 1,nmax
               md    = max(1,m-1)
               nd    = max(1,n-1)
               if (kcs  (n,m) .ne. 0) then
                  dps   (n,m) = max (dp (n ,m ),dp (n ,md),
     *                               dp (nd,md),dp (nd,m ))
               endif
  460       continue
  450    continue
      endif
c
      return
      end

      subroutine wrhwat(okee      ,hdefds    ,hdafds    ,parcod    ,
     *                  itim      ,nmax      ,mmax      ,
     *                  kcs       ,kfu       ,kfv       ,
     *                  dps       ,s1                              )
c-----------------------------------------------------------------------
c             Module: SUBROUTINE WRHWAT
c           Function: Get NEFIS data for time itim for
c                     parcod =  1  'wl' or
c                     parcod =  2  'wh'
c        Method used:
c
c-----------------------------------------------------------------------
c   Calling routine :              ods_tri_nef_map_getdata
c-----------------------------------------------------------------------
c   Called  routines:              GETELT (nf)
c-----------------------------------------------------------------------
c  Formal parameters:
c  ------------------
c
c   Var. I/O  Type Dimensions
c   -------------------------
c
c DPS     I   R*4  NMAX,MMAX       Depth in zeta point
c OKEE     O  L*4                  Flag for further execution program
c ITIM    I   I*4                  Specified time
c KCS     I   I*4  NMAX,MMAX       Mask array for the zeta points
c                                  (time independent)
c                                  =0 inactive point
c                                  =1 active   point
c                                  =2 open boundary point
c KFU     I   I*4  NMAX,MMAX       Mask array for the u-velocity point
c                                  (time dependent)
c                                  =0 dry      point
c                                  =1 active   point
c KFV     I   I*4  NMAX,MMAX       Mask array for the v-velocity point
c                                  (time dependent)
c                                  =0 dry      point
c                                  =1 active   point
c HDAFDS  I   I*4  999             Data file descriptor for the MAP-DAT
c                                  file
c HDEFDS  I   I*4  2997            Definition file description for the
c                                  MAP-DEF file
c MMAX    I   I*4                  Number of gridpoints in the x-dir.
c NMAX    I   I*4                  Number of gridpoints in the y-dir.
c S1       O  R*4  NMAX,MMAX       Water-level or waterhight in zeta pnt
c PARCOD  I   I*4                  Code for parameter
c-----------------------------------------------------------------------
c    Local variables:
c    ----------------
c
c   Var.      Type Dimensions
c   -------------------------
c
c BUFLEN      I*4                  Buffer length
c GRPDEF     CH*16                 Group name definition
c LVAR        L*4                  Help var.
c M           I*4                  Help var.
c N           I*4                  Help var.
c UINDEX      I*4  3               Array containing cell indices with
c                                  has to be read
c USRORD      I*4                  Sequence in which the cells must be
c                                  read
c-----------------------------------------------------------------------
c
c  DECLARATIONS
c
      integer       nmax  ,mmax  ,parcod,itim
      integer       n     ,m     ,nd    ,md
c
      integer       kcs   (nmax  ,mmax  ),
     *              kfu   (nmax  ,mmax  ),kfv   (nmax  ,mmax  )
c
      real          dps   (nmax  ,mmax  ),s1    (nmax  ,mmax  )
c
      logical       okee  ,lvar
c-----------------------------------------------------------------------
c-----Declaraties NEFIS
c-----------------------------------------------------------------------
      integer*4     hdefds( 2997),hdafds(  999)
c
      integer*4     uindex(    3),usrord,buflen
c
      character*16  grpdef
c
      integer*4     GETELT,GETELS
c#ifdef WINNT
      integer       GETELT_i
c#endif
c--------------------------------------------------------------------
c-----Initialize Nefis variables
c--------------------------------------------------------------------
      grpdef    = 'map-series'
      usrord    = 1
      buflen    = 4 * nmax   * mmax
      uindex(1) = itim
      uindex(2) = itim
      uindex(3) = 1
c--------------------------------------------------------------------
c-----Read from group 3 S1
c--------------------------------------------------------------------
      okee      = okee .and.
     *            GETELT(hdefds   ,grpdef    ,'S1    '  ,
     *                   uindex   ,usrord ,buflen    ,S1        )
     *            .eq. 0
c--------------------------------------------------------------------
      if ( parcod .eq. 2) then
c-----------------------------------------------------------------------
c--------Calculate WH
c        if kcs = 0 then permanent drypoint
c        else if surrounding kfu and kfv = 0 then temporary drypoint
c-----------------------------------------------------------------------
         do 110 n = 1,nmax
            do 110 m = 1,mmax
               md    = max (1,m-1)
               nd    = max (1,n-1)
c              lvar  = ((kcs   (n ,m ) .eq. 1) .and.
c    *                  (kfu   (n ,m ) .eq. 1  .or.
c    *                   kfu   (n ,md) .eq. 1  .or.
c    *                   kfv   (n ,m ) .eq. 1  .or.
c    *                   kfv   (nd,m ) .eq. 1))
               lvar  = ((kcs   (n ,m ) .eq. 1))
               if (lvar  ) then
                  s1    (n,m) = s1    (n,m) + dps   (n,m)
               else
                  s1    (n,m) = 0.0
               endif
  110    continue
      endif
c
      return
      end

      subroutine wrh3di(okee      ,hdefds    ,hdafds    ,parcod    ,
     *                  grdang    ,itim      ,misval    ,
     *                  nmax      ,mmax      ,kmax      ,
     *                  kcs       ,kfu       ,kfv       ,
     *                  xcor      ,ycor      ,alfas     ,
     *                  u1        ,v1        ,thick                )
c-----------------------------------------------------------------------
c             Module: SUBROUTINE WRH3DI
c           Function: get NEFIS data for time itim for
c                     parcod =  3  'du' or
c                     parcod =  4  'dv' or
c                     parcod =  5  'dm' or
c                     parcod =  6  'dd'
c        Method used:
c
c-----------------------------------------------------------------------
c   Calling routine :              ods_tri_nef_map_getdata
c-----------------------------------------------------------------------
c   Called  routines:              GETELT (nf)
c-----------------------------------------------------------------------
c  Formal parameters:
c  ------------------
c
c   Var. I/O  Type Dimensions
c   -------------------------
c
c GRDANG  I   R*4                  Vertex between the y-axis and north
c GUU     I   R*4  NMAX,MMAX       Mean value for distance coefficients
c GVV     I   R*4  NMAX,MMAX       Mean value for distance coefficients
c OKEE     O  L*4                  Flag for further execution program
c ITIM    I   I*4                  Specified time
c MISVAL  I   R*4                  missing value
c PARCOD  I   I*4                  Parameter code
c KCS     I   I*4  NMAX,MMAX       Mask array for the zeta points
c                                  (time independent)
c                                  =0 inactive point
c                                  =1 active   point
c                                  =2 open boundary point
c KFU     I   I*4  NMAX,MMAX       Mask array for the u-velocity point
c                                  (time dependent)
c                                  =0 dry      point
c                                  =1 active   point
c KFV     I   I*4  NMAX,MMAX       Mask array for the v-velocity point
c                                  (time dependent)
c                                  =0 dry      point
c                                  =1 active   point
c KMAX    I   I*4                  Number of layers
c HDAFDS  I   I*4  999             Data file descriptor for the MAP-DAT
c                                  file
c HDEFDS  I   I*4  2997            Definition file description for the
c                                  MAP-DEF file
c MMAX    I   I*4                  Number of gridpoints in the x-dir.
c NMAX    I   I*4                  Number of gridpoints in the y-dir.
c THICK   I   R*4  KMAX            Relative layer thickness
c U1       O  R*4  NMAX,MMAX,KMAX  U-velocity in u-velocity point
c V1       O  R*4  NMAX,MMAX,KMAX  V-velocity in v-velocity point
c XCOR    I   R*4  NMAX,MMAX       X-distance between 2 grid lines
c                                  around a zeta point in y-direction
c YCOR    I   R*4  NMAX,MMAX       Y-distance between 2 grid lines
c                                  around a zeta point in y-direction
c-----------------------------------------------------------------------
c    Local variables:
c    ----------------
c
c   Var.      Type Dimensions
c   -------------------------
c
c BUFLEN      I*4                  Buffer length
c DUVD        R*4                  Depth mean velocity direction
c DUVM        R*4                  Depth mean velocity magnitude
c DUZ         R*4                  Depth mean u-velocity in a zeta
c                                  point
c DVZ         R*4                  Depth mean v-velocity in a zeta
c                                  point
c EPS         R*4                  Small value to test for backwards
c                                  transformation
c GRPDEF     CH*16                 Group name definition
c GUUGEM      R*4                  Help var. 'mean' GUU value
c GVVGEM      R*4                  Help var. 'mean' GVV value
c KENMU       I*4                  Mask value for two consecutive u-vel.
c                                  points (max 1)
c KENMV       I*4                  Mask value for two consecutive v-vel.
c                                  points (max 1)
c KFS         I*4                  Temporary dry point
c LVAR        L*4                  Help var.
c M           I*4                  Loop var. 1-MMAX
c MD          I*4                  Max (1,M-1)
c MM          I*4                  Max (2,M)
c MMD         I*4                  MM-1
c N           I*4                  Loop var. 1-NMAX
c ND          I*4                  Max (1,N-1)
c NN          I*4                  Max (2,N)
c NND         I*4                  NN-1
c PI          R*4                  Value for pi (3.14 etc.)
c UGEM        R*4                  Help var. 'mean' u-velocity value
c UINDEX      I*4  3               Array containing cell indices with
c                                  has to be read
c UM          R*4                  Help var. for u-vel. in u-point M
c UMD         R*4                  Help var. for u-vel. in u-point MD
c USRORD      I*4                  Sequence in which the cells must be
c                                  read
c VGEM        R*4                  Help var. 'mean' v-velocity value
c VN          R*4                  Help var. for v-vel. in v-point N
c VND         R*4                  Help var. for v-vel. in v-point ND
c                                  outside model
c ZDEF        R*4                  Default value for z value outside
c                                  model
c-----------------------------------------------------------------------
c
c  declarations
c
      integer       nmax  ,mmax  ,kmax  ,parcod,itim
      integer       n     ,m     ,nd    ,md
      integer       k
c
      real          grdang,pi    ,eps   ,misval,duz   ,dvz
      real          duvm  ,duvd  ,hulp
      real          um    ,umd   ,vn    ,vnd
      real          ugem  ,vgem  ,totthk
c
      integer       kcs   (nmax  ,mmax  ),
     *              kfu   (nmax  ,mmax  ),kfv   (nmax  ,mmax  )
c
      real          xcor  (nmax  ,mmax  ),ycor  (nmax  ,mmax  ),
     *              alfas (nmax  ,mmax  )
      real          u1    (nmax  ,mmax   ,kmax  ),
     *              v1    (nmax  ,mmax   ,kmax  ),
     *              thick (kmax  )
c
      logical       okee  ,lvar
c-----------------------------------------------------------------------
c-----Declarations NEFIS
c-----------------------------------------------------------------------
      integer*4     hdefds( 2997),hdafds(  999)
c
      integer*4     uindex(    3),usrord,buflen
c
      character*16  grpdef
c
      integer*4     GETELT,GETELS
c#ifdef WINNT
      integer       GETELT_i
c#endif
c-----------------------------------------------------------------------
c-----General initialisation
c-----------------------------------------------------------------------
      pi     = atan (1.0) * 4.
      eps    = 1.e-12
c--------------------------------------------------------------------
c-----Initialize Nefis variables
c--------------------------------------------------------------------
      grpdef    = 'map-series'
      usrord    = 1
      buflen    = 4 * nmax   * mmax   * kmax
      uindex(1) = itim
      uindex(2) = itim
      uindex(3) = 1
c--------------------------------------------------------------------
c-----Read from group 3 U1
c--------------------------------------------------------------------
      okee      = okee .and.
     *            GETELT(hdefds   ,grpdef    ,'U1    '  ,
     *                   uindex   ,usrord ,buflen    ,U1        )
     *            .eq. 0
c--------------------------------------------------------------------
c-----Read from group 3 V1
c--------------------------------------------------------------------
      okee      = okee .and.
     *            GETELT(hdefds   ,grpdef    ,'V1    '  ,
     *                   uindex   ,usrord ,buflen    ,V1        )
     *            .eq. 0
c--------------------------------------------------------------------
c-----Calculate DUZ, DVZ, DUVM and DUVD
c     if kcs = 0 then permanent drypoint
c     else if surrounding kfu and kfv = 0 then temporary drypoint
c--------------------------------------------------------------------
      do 110 n = nmax, 1, -1
         do 110 m = mmax, 1, -1
            md    = max (1,m-1)
            nd    = max (1,n-1)
            lvar  = ((kcs   (n ,m ) .eq. 1) .and.
     *               (kfu   (n ,m ) .eq. 1  .or.
     *                kfu   (n ,md) .eq. 1  .or.
     *                kfv   (n ,m ) .eq. 1  .or.
     *                kfv   (nd,m ) .eq. 1))
            if (lvar  ) then
c--------------------------------------------------------------------
c--------------Sum U1 and V1, then backwards transformation and
c              calculation of DUZ and DVZ
c--------------------------------------------------------------------
               um     = 0.
               umd    = 0.
               vn     = 0.
               vnd    = 0.
               totthk = 0.
               do 120 k = 1,kmax
                  if ( u1(n,m,k)  .ne. -999.0 .and.
     *                 u1(n,md,k) .ne. -999.0 .and.
     *                 v1(nd,m,k) .ne. -999.0 .and.
     *                 v1(n,m,k)  .ne. -999.0       ) then
                     um     = um     + u1    (n ,m ,k ) * thick (k)
                     umd    = umd    + u1    (n ,md,k ) * thick (k)
                     vn     = vn     + v1    (n ,m ,k ) * thick (k)
                     vnd    = vnd    + v1    (nd,m ,k ) * thick (k)
                     totthk = totthk + thick(k)
                  endif
  120          continue
c
               if ( totthk .ne. 0.0 ) then
                  um     = um     * kfu   (n ,m )
                  umd    = umd    * kfu   (n ,md)
                  vn     = vn     * kfv   (n ,m )
                  vnd    = vnd    * kfv   (nd,m )
                  ugem   = 0.5 * (um   + umd  ) * kcs(n,m) / totthk
                  vgem   = 0.5 * (vn   + vnd  ) * kcs(n,m) / totthk
                  duz    =  ugem * cos( alfas(n,m) )  -
     *                      vgem * sin( alfas(n,m) )
                  dvz    =  ugem * sin( alfas(n,m) )  +
     *                      vgem * cos( alfas(n,m) )
c--------------------------------------------------------------------
c--------------Calculate DUVM and DUVD
c              duvd should be defined between 0. and 360. degrees
c              atan2 <-180,180>, grdang [0,360] => mod (.. + 720)
c
c        Note (AM, dd 31 march 1999)
c        The sign before grdang was -, this has been adjusted, though
c        in most cases grdang is zero anyway.
c--------------------------------------------------------------------
                  duvm   = sqrt ( duz    * duz    + dvz    * dvz   )
                  if (abs (duz   ) .lt. eps) then
                     duz    = eps
                  endif
                  if (abs (dvz   ) .lt. eps) then
                     dvz    = eps
                  endif
                  hulp   =    90. - atan2 (dvz   ,duz   ) * 180. / pi +
     *                     grdang
                  duvd   = mod    ( hulp  +  720.,  360.)
                  if ( parcod .eq. 3 .or. parcod .eq. 4) then
                     u1    (n ,m , 1) = duz
                     v1    (n ,m , 1) = dvz
                  else if ( parcod .eq. 5 .or. parcod .eq. 6) then
                     u1    (n ,m , 1) = duvm
                     v1    (n ,m , 1) = duvd
                  endif
               else
                  u1    (n ,m , 1) = misval
                  v1    (n ,m , 1) = misval
               endif
            else
               u1    (n ,m , 1) = misval
               v1    (n ,m , 1) = misval
            endif
c
  110 continue
      return
      end

      subroutine wrhuvi(okee      ,hdefds    ,hdafds    ,parcod    ,
     *                  grdang    ,itim      ,misval    ,
     *                  nmax      ,mmax      ,kmax      ,
     *                  kcs       ,kfu       ,kfv       ,
     *                  xcor      ,ycor      ,
     *                  alfas                ,u1        ,v1        )
c-----------------------------------------------------------------------
c             Module: SUBROUTINE WRHUVI
c           Function: get NEFIS data for time itim for
c                     parcod =  7  'u ',
c                     parcod =  8  'v ',
c                     parcod =  9  'm ',
c                     parcod = 10  'd ',
c        Method used:
c
c-----------------------------------------------------------------------
c   Calling routine :              ods_tri_nef_map_getdata
c-----------------------------------------------------------------------
c   Called  routines:              GETELT (nf)
c-----------------------------------------------------------------------
c  Formal parameters:
c  ------------------
c
c   Var. I/O  Type Dimensions
c   -------------------------
c
c GRDANG  I   R*4                  Vertex between the y-axis and north
c ALFAS   I   R*4  NMAX,MMAX       Transformation coefficients (in radians)
c GVV     I   R*4  NMAX,MMAX       Mean value for distance coefficients
c OKEE     O  L*4                  Flag for further execution program
c ITIM    I   I*4                  Specified time
c MISVAL  I   R*4                  missing value
c KCS     I   I*4  NMAX,MMAX       Mask array for the zeta points
c                                  (time independent)
c                                  =0 inactive point
c                                  =1 active   point
c                                  =2 open boundary point
c KFU     I   I*4  NMAX,MMAX       Mask array for the u-velocity point
c                                  (time dependent)
c                                  =0 dry      point
c                                  =1 active   point
c KFV     I   I*4  NMAX,MMAX       Mask array for the v-velocity point
c                                  (time dependent)
c                                  =0 dry      point
c                                  =1 active   point
c KMAX    I   I*4                  Number of layers
c HDAFDS  I   I*4  999             Data file descriptor for the MAP-DAT
c                                  file
c HDEFDS  I   I*4  2997            Definition file description for the
c                                  MAP-DEF file
c MMAX    I   I*4                  Number of gridpoints in the x-dir.
c NMAX    I   I*4                  Number of gridpoints in the y-dir.
c PARCOD  I   I*4                  Parameter code
c U1       O  R*4  NMAX,MMAX,KMAX  U-velocity in u-velocity point
c V1       O  R*4  NMAX,MMAX,KMAX  V-velocity in v-velocity point
c XCOR    I   R*4  NMAX,MMAX       X-distance between 2 grid lines
c                                  around a zeta point in y-direction
c YCOR    I   R*4  NMAX,MMAX       Y-distance between 2 grid lines
c                                  around a zeta point in y-direction
c-----------------------------------------------------------------------
c    Local variables:
c    ----------------
c
c   Var.      Type Dimensions
c   -------------------------
c
c BUFLEN      I*4                  Buffer length
c EPS         R*4                  Small value to test for backwards
c                                  transformation
c GRPDEF     CH*16                 Group name definition
c GUUGEM      R*4                  Help var. 'mean' GUU value
c GVVGEM      R*4                  Help var. 'mean' GVV value
c KENMU       I*4                  Mask value for two consecutive u-vel.
c                                  points (max 1)
c KENMV       I*4                  Mask value for two consecutive v-vel.
c                                  points (max 1)
c KFS         I*4                  Temporary dry point
c LVAR        L*4                  Help var.
c M           I*4                  Loop var. 1-MMAX
c MD          I*4                  Max (1,M-1)
c MM          I*4                  Max (2,M)
c MMD         I*4                  MM-1
c N           I*4                  Loop var. 1-NMAX
c ND          I*4                  Max (1,N-1)
c NN          I*4                  Max (2,N)
c NND         I*4                  NN-1
c PI          R*4                  Value for pi (3.14 etc.)
c UGEM        R*4                  Help var. 'mean' u-velocity value
c UINDEX      I*4  3               Array containing cell indices with
c                                  has to be read
c UM          R*4                  Help var. for u-vel. in u-point M
c UMD         R*4                  Help var. for u-vel. in u-point MD
c USRORD      I*4                  Sequence in which the cells must be
c                                  read
c UVD         R*4                  Velocity direction
c UVM         R*4                  Velocity magnitude
c UZ          R*4                  U-velocity in a zeta point
c VGEM        R*4                  Help var. 'mean' v-velocity value
c VN          R*4                  Help var. for v-vel. in v-point N
c VND         R*4                  Help var. for v-vel. in v-point ND
c VZ          R*4                  V-velocity in a zeta point
c XYDEF       R*4                  Default value for x,y coordinate
c                                  outside model
c ZDEF        R*4                  Default value for z value outside
c                                  model
c-----------------------------------------------------------------------
c
c  declarations
c
      integer       nmax  ,mmax  ,kmax  ,parcod,itim
      integer       n     ,m     ,nd    ,md    ,k
c
      real          grdang,pi    ,eps   ,misval,uz    ,vz
      real          uvm   ,uvd   ,hulp
      real          um    ,umd   ,vn    ,vnd
      real          ugem  ,vgem
c
      integer       kcs   (nmax  ,mmax  ),
     *              kfu   (nmax  ,mmax  ),kfv   (nmax  ,mmax  )
c
      real          xcor  (nmax  ,mmax  ),ycor  (nmax  ,mmax  ),
     *              alfas (nmax  ,mmax  )
      real          u1    (nmax  ,mmax   ,kmax  ),
     *              v1    (nmax  ,mmax   ,kmax  )
c
      logical       okee  ,lvar, zactiv
c-----------------------------------------------------------------------
c-----Declarations NEFIS
c-----------------------------------------------------------------------
      integer*4     hdefds( 2997),hdafds(  999)
c
      integer*4     uindex(    3),usrord,buflen
c
      character*16  grpdef
c
      integer*4     GETELT,GETELS
c#ifdef WINNT
      integer       GETELT_i
c#endif
c-----------------------------------------------------------------------
c-----General initialisation
c-----------------------------------------------------------------------
      pi     = atan (1.0) * 4.
      eps    = 1.e-12
c--------------------------------------------------------------------
c-----Initialize Nefis variables
c--------------------------------------------------------------------
      grpdef    = 'map-series'
      usrord    = 1
      buflen    = 4 * nmax   * mmax   * kmax
      uindex(1) = itim
      uindex(2) = itim
      uindex(3) = 1
c-----------------------------------------------------------------------
c--------Read from  group 3 U1
c-----------------------------------------------------------------------
      okee      = okee .and.
     *            GETELT(hdefds   ,grpdef    ,'U1    '  ,
     *                   uindex   ,usrord ,buflen    ,U1        )
     *            .eq. 0
c-----------------------------------------------------------------------
c--------Read from group 3 V1
c-----------------------------------------------------------------------
      okee      = okee .and.
     *            GETELT(hdefds   ,grpdef    ,'V1    '  ,
     *                   uindex   ,usrord ,buflen    ,V1        )
     *            .eq. 0
c--------------------------------------------------------------------
c-----Calculate UZ, VZ, UVM and UVD
c     if kcs = 0 then permanent drypoint; if surrounding kfu and kfv
c     := 0 then temporary drypoint
c--------------------------------------------------------------------
      do 110 n = nmax, 1, -1
         do 110 m = mmax, 1, -1
            md    = max (1,m-1)
            nd    = max (1,n-1)
            lvar  = ((kcs   (n ,m ) .eq. 1) .and.
     *               (kfu   (n ,m ) .eq. 1  .or.
     *                kfu   (n ,md) .eq. 1  .or.
     *                kfv   (n ,m ) .eq. 1  .or.
     *                kfv   (nd,m ) .eq. 1))
            do 110 k = 1, kmax
               zactiv = .false.
               if ( lvar ) then
                  if ( u1(n,m,k)  .eq. -999.0 .or.
     *                 u1(n,md,k) .eq. -999.0 .or.
     *                 v1(nd,m,k) .eq. -999.0 .or.
     *                 v1(n,m,k)  .eq. -999.0      ) then
                     zactiv = .false.
                  else
                     zactiv = .true.
                  endif
               endif
               if ( zactiv ) then
c-----------------------------------------------------------------------
c-----------------Backwards transformation of U1 and V1 and
c                 calculation of UZ and VZ
c-----------------------------------------------------------------------
                     um     = u1    (n ,m ,k )  * kfu   (n ,m )
                     umd    = u1    (n ,md,k )  * kfu   (n ,md)
                     vn     = v1    (n ,m ,k )  * kfv   (n ,m )
                     vnd    = v1    (nd,m ,k )  * kfv   (nd,m )
c
                     ugem   = 0.5 * (um   + umd ) * kcs(n,m)
                     vgem   = 0.5 * (vn   + vnd ) * kcs(n,m)
c
                     uz     = ugem * cos( alfas(n,m) )  -
     *                        vgem * sin( alfas(n,m) )
                     vz     = ugem * sin( alfas(n,m) )  +
     *                        vgem * cos( alfas(n,m) )
c-----------------------------------------------------------------------
c-----------------Calculate UVM and UVD
c                 uvd should be defined between 0. and 360. degrees
c                 atan2 <-180,180>, grdang [0,360] => mod (.. + 720)
c
c        Note (AM, dd 31 march 1999)
c        The sign before grdang was -, this has been adjusted, though
c        in most cases grdang is zero anyway.
c-----------------------------------------------------------------------
                  uvm    = sqrt ( uz     * uz     + vz     * vz    )
                  if (abs (uz    ) .lt. eps) then
                     uz     = eps
                  endif
                  if (abs (vz    ) .lt. eps) then
                     vz     = eps
                  endif
                  hulp   =    90. - atan2 (vz    ,uz    ) * 180. / pi +
     *                     grdang
                  uvd    = mod    ( hulp  +  720.,  360.)
                  if ( parcod .eq. 7 .or. parcod .eq. 8) then
                      u1    (n ,m , k) = uz
                      v1    (n ,m , k) = vz
                  else if ( parcod .eq. 9 .or. parcod .eq. 10) then
                      u1    (n ,m , k) = uvm
                      v1    (n ,m , k) = uvd
                  endif
               else
                  if ( parcod .eq. 7 .or. parcod .eq. 8) then
                      u1    (n ,m , k) = misval
                      v1    (n ,m , k) = misval
                  else if ( parcod .eq. 9 .or. parcod .eq. 10) then
                      u1    (n ,m , k) = misval
                      v1    (n ,m , k) = misval
                  endif
               endif
  110 continue

      return
      end

      subroutine wrh3dv(okee      ,hdefds    ,hdafds    ,misval    ,
     *                  itim      ,nmax      ,mmax      ,kmax      ,
     *                  kcs       ,kfu       ,kfv       ,
     *                  xcor      ,ycor      ,alfas     ,
     *                  u1        ,v1        ,thick                )
c-----------------------------------------------------------------------
c             Module: SUBROUTINE WRH3DV
c           Function: get NEFIS data for time itim for
c                     paramete = 38  'df'
c        Method used:
c
c-----------------------------------------------------------------------
c   Calling routine :              ods_tri_nef_map_getdata
c-----------------------------------------------------------------------
c   Called  routines:              GETELT (nf)
c-----------------------------------------------------------------------
c  Formal parameters:
c  ------------------
c
c   Var. I/O  Type Dimensions
c   -------------------------
c
c ALFAS   I   R*4  NMAX,MMAX       Transformation coefficients (in radians)
c OKEE     O  L*4                  Flag for further execution program
c ITIM    I   I*4                  Specified time
c MISVAL  I   R*4                  missing value
c KCS     I   I*4  NMAX,MMAX       Mask array for the zeta points
c                                  (time independent)
c                                  =0 inactive point
c                                  =1 active   point
c                                  =2 open boundary point
c KFU     I   I*4  NMAX,MMAX       Mask array for the u-velocity point
c                                  (time dependent)
c                                  =0 dry      point
c                                  =1 active   point
c KFV     I   I*4  NMAX,MMAX       Mask array for the v-velocity point
c                                  (time dependent)
c                                  =0 dry      point
c                                  =1 active   point
c KMAX    I   I*4                  Number of layers
c HDAFDS  I   I*4  999             Data file descriptor for the MAP-DAT
c                                  file
c HDEFDS  I   I*4  2997            Definition file description for the
c                                  MAP-DEF file
c MMAX    I   I*4                  Number of gridpoints in the x-dir.
c NMAX    I   I*4                  Number of gridpoints in the y-dir.
c THICK   I   R*4  KMAX            Relative layer thickness
c U1       O  R*4  NMAX,MMAX,KMAX  U-velocity in u-velocity point
c V1       O  R*4  NMAX,MMAX,KMAX  V-velocity in v-velocity point
c XCOR    I   R*4  NMAX,MMAX       X-distance between 2 grid lines
c                                  around a zeta point in y-direction
c YCOR    I   R*4  NMAX,MMAX       Y-distance between 2 grid lines
c                                  around a zeta point in y-direction
c-----------------------------------------------------------------------
c    Local variables:
c    ----------------
c
c   Var.      Type Dimensions
c   -------------------------
c
c BUFLEN      I*4                  Buffer length
c DUZ         R*4                  Depth mean u-velocity in a zeta
c                                  point
c DVZ         R*4                  Depth mean v-velocity in a zeta
c                                  point
c EPS         R*4                  Small value to test for backwards
c                                  transformation
c GRPDEF     CH*16                 Group name definition
c GUUGEM      R*4                  Help var. 'mean' GUU value
c GVVGEM      R*4                  Help var. 'mean' GVV value
c KENMU       I*4                  Mask value for two consecutive u-vel.
c                                  points (max 1)
c KENMV       I*4                  Mask value for two consecutive v-vel.
c                                  points (max 1)
c LVAR        L*4                  Help var.
c M           I*4                  Loop var. 1-MMAX
c MD          I*4                  Max (1,M-1)
c MM          I*4                  Max (2,M)
c MMD         I*4                  MM-1
c N           I*4                  Loop var. 1-NMAX
c ND          I*4                  Max (1,N-1)
c NN          I*4                  Max (2,N)
c NND         I*4                  NN-1
c UGEM        R*4                  Help var. 'mean' u-velocity value
c UINDEX      I*4  3               Array containing cell indices with
c                                  has to be read
c UM          R*4                  Help var. for u-vel. in u-point M
c UMD         R*4                  Help var. for u-vel. in u-point MD
c USRORD      I*4                  Sequence in which the cells must be
c                                  read
c VGEM        R*4                  Help var. 'mean' v-velocity value
c VN          R*4                  Help var. for v-vel. in v-point N
c VND         R*4                  Help var. for v-vel. in v-point ND
c ZDEF        R*4                  Default value for z value outside
c                                  model
c-----------------------------------------------------------------------
c
c  declarations
c
      integer       nmax  ,mmax  ,kmax  ,itim
      integer       n     ,m     ,nd    ,md    ,k
c
      real          eps   ,misval,duz   ,dvz
      real          um    ,umd   ,vn    ,vnd
      real          ugem  ,vgem
c
      integer       kcs   (nmax  ,mmax  ),
     *              kfu   (nmax  ,mmax  ),kfv   (nmax  ,mmax  )
c
      real          xcor  (nmax  ,mmax  ),ycor  (nmax  ,mmax  ),
     *              alfas (nmax  ,mmax  )
      real          u1    (nmax  ,mmax   ,kmax  ),
     *              v1    (nmax  ,mmax   ,kmax  ),
     *              thick (kmax  )
c
      logical       okee  ,lvar
c-----------------------------------------------------------------------
c-----Declarations NEFIS
c-----------------------------------------------------------------------
      integer*4     hdefds( 2997),hdafds(  999)
c
      integer*4     uindex(    3),usrord,buflen
c
      character*16  grpdef
c
      integer*4     GETELT,GETELS
c#ifdef WINNT
      integer       GETELT_i
c#endif
c-----------------------------------------------------------------------
c-----General initialisation
c-----------------------------------------------------------------------
      eps    = 1.e-12
c--------------------------------------------------------------------
c-----Initialize Nefis variables
c--------------------------------------------------------------------
      grpdef    = 'map-series'
      usrord    = 1
      buflen    = 4 * nmax   * mmax   * kmax
      uindex(1) = itim
      uindex(2) = itim
      uindex(3) = 1
c--------------------------------------------------------------------
c-----Read from group 3 U1
c--------------------------------------------------------------------
      okee      = okee .and.
     *            GETELT(hdefds   ,grpdef    ,'U1    '  ,
     *                   uindex   ,usrord ,buflen    ,U1        )
     *            .eq. 0
c--------------------------------------------------------------------
c-----Read from group 3 V1
c--------------------------------------------------------------------
      okee      = okee .and.
     *            GETELT(hdefds   ,grpdef    ,'V1    '  ,
     *                   uindex   ,usrord ,buflen    ,V1        )
     *            .eq. 0
c--------------------------------------------------------------------
c-----Calculate DUZ and DVZ
c     if kcs = 0 then permanent drypoint
c     else if surrounding kfu and kfv = 0 then temporary drypoint
c--------------------------------------------------------------------
      do 110 n = nmax, 1, -1
         do 110 m = mmax, 1, -1
            md    = max (1,m-1)
            nd    = max (1,n-1)
            lvar  = ((kcs   (n ,m ) .ne. 0) .and.
     *               (kfu   (n ,m ) .eq. 1  .or.
     *                kfu   (n ,md) .eq. 1  .or.
     *                kfv   (n ,m ) .eq. 1  .or.
     *                kfv   (nd,m ) .eq. 1))
            if (lvar  ) then
c--------------------------------------------------------------------
c--------------Summon U1 and V1, then backwards transformation and
c              calculation of DUZ and DVZ
c--------------------------------------------------------------------
                  um     = 0.
                  umd    = 0.
                  vn     = 0.
                  vnd    = 0.
                  do 120 k = 1,kmax
                     um  = um     + u1    (n ,m ,k ) * thick (k)
                     umd = umd    + u1    (n ,md,k ) * thick (k)
                     vn  = vn     + v1    (n ,m ,k ) * thick (k)
                     vnd = vnd    + v1    (nd,m ,k ) * thick (k)
  120             continue
c
                  um     = um     * kfu   (n ,m )
                  umd    = umd    * kfu   (n ,md)
                  vn     = vn     * kfv   (n ,m )
                  vnd    = vnd    * kfv   (nd,m )
c
                  ugem   = 0.5 * (um   + umd  ) * kcs(n,m)
                  vgem   = 0.5 * (vn   + vnd  ) * kcs(n,m)
c
                  duz    =  ugem  * cos( alfas(n,m) ) -
     *                      vgem  * sin( alfas(n,m) )
                  dvz    =  ugem  * sin( alfas(n,m) ) +
     *                      vgem  * cos( alfas(n,m) )
c
               u1    (n ,m , 1) = duz
               v1    (n ,m , 1) = dvz
            else
               u1    (n ,m , 1) = misval
               v1    (n ,m , 1) = misval
            endif
  110 continue
c
      return
      end

      subroutine wrhcon(okee      ,hdefds    ,hdafds    ,itim      ,
     *                  xdata     ,maxdim    ,parcod    ,lay       ,
     *                  misval    ,
     *                  nmax      ,mmax      ,kmax      ,lmax      ,
     *                  kcs       ,kfu       ,kfv       ,r1        )
c-----------------------------------------------------------------------
c             Module: SUBROUTINE WRHCON
c           Function: get NEFIS data for time itim for
c                     parameter = 2.  'c '
c        Method used:
c
c-----------------------------------------------------------------------
c   Calling routine :              ods_tri_nef_map_getdata
c-----------------------------------------------------------------------
c   Called  routines:              GETELT (nf)
c-----------------------------------------------------------------------
c  Formal parameters:
c  ------------------
c
c   Var. I/O  Type Dimensions
c   -------------------------
c
c OKEE     O  L*4                  Flag for further execution program
c ITIM    I   I*4                  Specified time
c MISVAL  I   R*4                  missing value
c ISTOF   I   I*4                  Number of constITUENT TO BE PLOTTED
c KCS     I   I*4  NMAX,MMAX       Mask array for the zeta points
c                                  (time independent)
c                                  =0 inactive point
c                                  =1 active   point
c                                  =2 open boundary point
c KFU     I   I*4  NMAX,MMAX       Mask array for the u-velocity point
c                                  (time dependent)
c                                  =0 dry      point
c                                  =1 active   point
c KFV     I   I*4  NMAX,MMAX       Mask array for the v-velocity point
c                                  (time dependent)
c                                  =0 dry      point
c                                  =1 active   point
c KMAX    I   I*4                  Number of layers
c LMAX    I   I*4                  Number of constituents
c HDAFDS  I   I*4  999             Data file descriptor for the MAP-DAT
c                                  file
c HDEFDS  I   I*4  2997            Definition file description for the
c                                  MAP-DEF file
c MMAX    I   I*4                  Number of gridpoints in the x-dir.
c NAMCON  I  CH*20 LMAX            Name of constituent
c NMAX    I   I*4                  Number of gridpoints in the y-dir.
c R1       O  R*4  NMAX,MMAX,KMAX,LMAX
c                                  Concentrations in zeta point
c LAY         I*4                  Actual layer number
c MAXDIM      I*4            I     length of data array
c PARCOD  I   I*4                  parameter to get data of
c XDATA       R*4   maxdim   O     array with the data
c-----------------------------------------------------------------------
c    Local variables:
c    ----------------
c
c   Var.      Type Dimensions
c   -------------------------
c
c BUFLEN      I*4                  Buffer length
c FMT        CH*46                 Help var. for write format
c FMTDEF     CH*46                 Help var. for write format default
c                                  values
c GRPDEF     CH*16                 Group name definition
c KFS         I*4                  Temporary dry point
c LVAR        L*4                  Help var.
c K           I*4                  Help var.
c L           I*4                  Help var.
c M           I*4                  Help var.
c N           I*4                  Help var.
c N1          I*4                  Help variable
c NRBLOK      I*4                  Blocknumber in data file
c OLDID      CH*3                  Old run-identification
c UINDEX      I*4  3               Array containing cell indices with
c                                  has to be read
c USRORD      I*4                  Sequence in which the cells must be
c                                  read
c ZDEF        R*4                  Default value for z value outside
c                                  model
c-----------------------------------------------------------------------
c
c  DECLARATIONS
c
      integer       nmax  ,mmax  ,kmax  ,lmax  ,itim
      integer       n     ,m     ,k     ,l
      integer       nd    ,md    ,n1    ,maxdim,parcod
      integer       lay(3)
c
      real          misval
      real          xdata (maxdim)
c
      integer       kcs   (nmax  ,mmax  ),
     *              kfu   (nmax  ,mmax  ),kfv   (nmax  ,mmax  )
c
      real          r1    (nmax  ,mmax   ,kmax  ,lmax  )
c
      logical       okee  ,lvar
c-----------------------------------------------------------------------
c-----Declarations NEFIS
c-----------------------------------------------------------------------
      integer*4     hdefds( 2997),hdafds(  999)
c
      integer*4     uindex(    3),usrord,buflen
c
      character*16  grpdef
c
      integer*4     GETELT,GETELS
c#ifdef WINNT
      integer       GETELT_i
c#endif
c--------------------------------------------------------------------
c-----Initialize Nefis variables
c--------------------------------------------------------------------
      grpdef    = 'map-series'
      usrord    = 1
      buflen    = 4 * nmax   * mmax   * kmax   * lmax
      uindex(1) = itim
      uindex(2) = itim
      uindex(3) = 1
c--------------------------------------------------------------------
c-----Read from group 3 R1
c--------------------------------------------------------------------
      okee      = okee .and.
     *            GETELT(hdefds   ,grpdef    ,'R1    '  ,
     *                   uindex   ,usrord ,buflen    ,R1        )
     *            .eq. 0
c--------------------------------------------------------------------
c-----Write R1 for all constituents
c     if kcs = 0 then permanent drypoint; if surrounding kfu and kfv
c     := 0 then temporary drypoint
c--------------------------------------------------------------------
      do 110 n = 1,nmax
         do 110 m = 1,mmax
            md    = max (1,m-1)
            nd    = max (1,n-1)
            lvar  = (kcs   (n ,m ) .eq. 1)
c           lvar  = ((kcs   (n ,m ) .eq. 1) .and.
c    *               (kfu   (n ,m ) .eq. 1  .or.
c    *                kfu   (n ,md) .eq. 1  .or.
c    *                kfv   (n ,m ) .eq. 1  .or.
c    *                kfv   (nd,m ) .eq. 1))
            if (.not. lvar  ) then
               do 100 k = 1,kmax
               do 100 l = 1,lmax
                  r1    (n ,m ,k ,l ) = misval
  100          continue
            endif
  110 continue
c
c-----store results in xdata
      n1 = 0
      do 200 k = lay(1), lay(2), lay(3)
      do 200 n = 1,nmax
      do 200 m = 1,mmax
         n1           = n1 + 1
         xdata ( n1 ) = r1 (n ,m , k, parcod-19)
  200 continue
c
      return
      end

      subroutine wrhtur(okee      ,hdefds    ,hdafds    ,itim      ,
     *                  misval    ,
     *                  nmax      ,mmax      ,kmax      ,ltur      ,
     *                  kcs       ,kfu       ,kfv       ,rtur1     )
c-----------------------------------------------------------------------
c             Module: SUBROUTINE WRHTUR
c           Function: get NEFIS data for time itim for
c                     parameter = 2.  'c ' turbulence parameters
c        Method used:
c
c-----------------------------------------------------------------------
c   Calling routine :              ods_tri_nef_map_getdata
c-----------------------------------------------------------------------
c   Called  routines:              GETELT (nf)
c-----------------------------------------------------------------------
c  Formal parameters:
c  ------------------
c
c   Var. I/O  Type Dimensions
c   -------------------------
c
c OKEE     O  L*4                  Flag for further execution program
c ITIM    I   I*4                  Specified time
c MISVAL  I   R*4                  missing value
c ISTOF   I   I*4                  Number of constITUENT TO BE PLOTTED
c KCS     I   I*4  NMAX,MMAX       Mask array for the zeta points
c                                  (time independent)
c                                  =0 inactive point
c                                  =1 active   point
c                                  =2 open boundary point
c KFU     I   I*4  NMAX,MMAX       Mask array for the u-velocity point
c                                  (time dependent)
c                                  =0 dry      point
c                                  =1 active   point
c KFV     I   I*4  NMAX,MMAX       Mask array for the v-velocity point
c                                  (time dependent)
c                                  =0 dry      point
c                                  =1 active   point
c KMAX    I   I*4                  Number of layers
c LTUR    I   I*4                  Number of turbulence parameters
c HDAFDS  I   I*4  999             Data file descriptor for the MAP-DAT
c                                  file
c HDEFDS  I   I*4  2997            Definition file description for the
c                                  MAP-DEF file
c MMAX    I   I*4                  Number of gridpoints in the x-dir.
c NAMCON  I  CH*20 LMAX            Name of constituent
c NMAX    I   I*4                  Number of gridpoints in the y-dir.
c RTUR1    O  R*4  NMAX,MMAX,KMAX,LMAX
c                                  Concentrations in zeta point
c-----------------------------------------------------------------------
c    Local variables:
c    ----------------
c
c   Var.      Type Dimensions
c   -------------------------
c
c BUFLEN      I*4                  Buffer length
c FMT        CH*46                 Help var. for write format
c FMTDEF     CH*46                 Help var. for write format default
c                                  values
c GRPDEF     CH*16                 Group name definition
c KFS         I*4                  Temporary dry point
c LVAR        L*4                  Help var.
c K           I*4                  Help var.
c L           I*4                  Help var.
c M           I*4                  Help var.
c N           I*4                  Help var.
c NRBLOK      I*4                  Blocknumber in data file
c OLDID      CH*3                  Old run-identification
c UINDEX      I*4  3               Array containing cell indices with
c                                  has to be read
c USRORD      I*4                  Sequence in which the cells must be
c                                  read
c ZDEF        R*4                  Default value for z value outside
c                                  model
c-----------------------------------------------------------------------
c
c  DECLARATIONS
c
      integer       nmax  ,mmax  ,kmax  ,ltur  ,itim
      integer       n     ,m     ,k     ,l
      integer       nd    ,md
c
      real          misval
c
      integer       kcs   (nmax  ,mmax  ),
     *              kfu   (nmax  ,mmax  ),kfv   (nmax  ,mmax  )
c
      real          rtur1 (nmax  ,mmax   ,kmax  ,ltur  )
c
      logical       okee  ,lvar
c-----------------------------------------------------------------------
c-----Declarations NEFIS
c-----------------------------------------------------------------------
      integer*4     hdefds( 2997),hdafds(  999)
c
      integer*4     uindex(    3),usrord,buflen
c
      character*16  grpdef
c
      integer*4     GETELT,GETELS
c#ifdef WINNT
      integer       GETELT_i
c#endif
c--------------------------------------------------------------------
c-----Initialize Nefis variables
c--------------------------------------------------------------------
      grpdef    = 'map-series'
      usrord    = 1
      buflen    = 4 * nmax   * mmax   * kmax   * ltur
      uindex(1) = itim
      uindex(2) = itim
      uindex(3) = 1
c--------------------------------------------------------------------
c-----Read from group 3 R1
c--------------------------------------------------------------------
      okee      = okee .and.
     *            GETELT(hdefds   ,grpdef    ,'RTUR1 '  ,
     *                   uindex   ,usrord ,buflen    ,RTUR1     )
     *            .eq. 0
c--------------------------------------------------------------------
c-----Write R1 for all constituents
c     if kcs = 0 then permanent drypoint; if surrounding kfu and kfv
c     := 0 then temporary drypoint
c--------------------------------------------------------------------
      do 110 n = 1,nmax
         do 110 m = 1,mmax
            md    = max (1,m-1)
            nd    = max (1,n-1)
            lvar  = ((kcs   (n ,m ) .eq. 1) .and.
     *               (kfu   (n ,m ) .eq. 1  .or.
     *                kfu   (n ,md) .eq. 1  .or.
     *                kfv   (n ,m ) .eq. 1  .or.
     *                kfv   (nd,m ) .eq. 1))
            if (.not. lvar  ) then
               do 100 k = 1,kmax
               do 100 l = 1,ltur
                  rtur1 (n ,m ,k ,l ) = misval
  100          continue
            endif
  110 continue
c
      return
      end

      subroutine wrhtai(okee      ,hdefds    ,hdafds    ,parcod    ,
     *                  misval    ,
     *                  grdang    ,itim      ,nmax      ,mmax      ,
     *                  kcs       ,kfu       ,kfv       ,
     *                  xcor      ,ycor      ,alfas                ,
     *                  tauksi    ,taueta                          )
c-----------------------------------------------------------------------
c             Module: SUBROUTINE WRHTAI
c           Function: get NEFIS data for time itim for
c                     parameter = 16  'tu'
c                     parameter = 17  'tv'
c                     parameter = 18  'tm'
c                     parameter = 19  'ta'
c        Method used:
c
c-----------------------------------------------------------------------
c   Calling routine :              ods_tri_nef_map_getdata
c-----------------------------------------------------------------------
c   Called  routines:              GETELT (nf)
c-----------------------------------------------------------------------
c  Formal parameters:
c  ------------------
c
c   Var. I/O  Type Dimensions
c   -------------------------
c
c GRDANG  I   R*4                  Vertex between the y-axis and north
c ALFAS   I   R*4  NMAX,MMAX       Transformation coefficients (in radians)
c OKEE     O  L*4                  Flag for further execution program
c ITIM    I   I*4                  Specified time
c MISVAL  I   R*4                  missing value
c PARCOD  I   I*4                  Parameter code
c KCS     I   I*4  NMAX,MMAX       Mask array for the zeta points
c                                  (time independent)
c                                  =0 inactive point
c                                  =1 active   point
c                                  =2 open boundary point
c KFU     I   I*4  NMAX,MMAX       Mask array for the u-velocity point
c                                  (time dependent)
c                                  =0 dry      point
c                                  =1 active   point
c KFV     I   I*4  NMAX,MMAX       Mask array for the v-velocity point
c                                  (time dependent)
c                                  =0 dry      point
c                                  =1 active   point
c HDAFDS  I   I*4  999             Data file descriptor for the MAP-DAT
c                                  file
c HDEFDS  I   I*4  2997            Definition file description for the
c                                  MAP-DEF file
c MMAX    I   I*4                  Number of gridpoints in the x-dir.
c NMAX    I   I*4                  Number of gridpoints in the y-dir.
c TAUETA   O  R*4  NMAX,MMAX       Tau bottom in v-velocity point
c TAUKSI   O  R*4  NMAX,MMAX       Tau bottom in u-velocity point
c XCOR    I   R*4  NMAX,MMAX       X-distance between 2 grid lines
c                                  around a zeta point in y-direction
c YCOR    I   R*4  NMAX,MMAX       Y-distance between 2 grid lines
c                                  around a zeta point in y-direction
c-----------------------------------------------------------------------
c    Local variables:
c    ----------------
c
c   Var.      Type Dimensions
c   -------------------------
c
c BUFLEN      I*4                  Buffer length
c EPS         R*4                  Small value to test for backwards
c                                  transformation
c GRPDEF     CH*16                 Group name definition
c GUUGEM      R*4                  Help var. 'mean' GUU value
c GVVGEM      R*4                  Help var. 'mean' GVV value
c KENMU       I*4                  Mask value for two consecutive u-vel.
c                                  points (max 1)
c KENMV       I*4                  Mask value for two consecutive v-vel.
c                                  points (max 1)
c KFS         I*4                  Temporary dry point
c LVAR        L*4                  Help var.
c M           I*4                  Loop var. 1-MMAX
c MD          I*4                  Max (1,M-1)
c MM          I*4                  Max (2,M)
c MMD         I*4                  MM-1
c N           I*4                  Loop var. 1-NMAX
c ND          I*4                  Max (1,N-1)
c NN          I*4                  Max (2,N)
c NND         I*4                  NN-1
c PI          R*4                  Value for pi (3.14 etc.)
c UINDEX      I*4  3               Array containing cell indices with
c                                  has to be read
c USRORD      I*4                  Sequence in which the cells must be
c                                  read
c TAUD        R*4                  Tau bottom direction
c TAUM        R*4                  Tau bottom magnitue
c TAUZU       R*4                  Tau bottom U in a zeta point
c TAUZV       R*4                  Tau bottom V in a zeta point
c TEGEM       R*4                  Help var. 'mean' taueta value
c TEN         R*4                  Help var. for taueta in v-point N
c TEND        R*4                  Help var. for taueta in v-point ND
c TKGEM       R*4                  Help var. 'mean' tauksi value
c TKM         R*4                  Help var. for tauksi in u-point M
c TKMD        R*4                  Help var. for tauksi in u-point MD
c ZDEF        R*4                  Default value for z value outside
c                                  model
c-----------------------------------------------------------------------
c
c  declarations
c
      integer       nmax  ,mmax  ,itim  ,parcod
      integer       n     ,m     ,nd    ,md
c
      real          eps   ,misval,tauzu ,tauzv ,hulp
      real          taud  ,taum  ,tkm   ,tkmd  ,ten   ,tend
      real          pi
      real          tkgem ,tegem ,grdang
c
      integer       kcs   (nmax  ,mmax  ),
     *              kfu   (nmax  ,mmax  ),kfv   (nmax  ,mmax  )
c
      real          xcor  (nmax  ,mmax  ),ycor  (nmax  ,mmax  ),
     *              alfas (nmax  ,mmax  )
      real          tauksi(nmax  ,mmax  ),taueta(nmax  ,mmax  )
c
      logical       okee  ,lvar
c-----------------------------------------------------------------------
c-----Declaraties NEFIS
c-----------------------------------------------------------------------
      integer*4     hdefds( 2997),hdafds(  999)
c
      integer*4     uindex(    3),usrord,buflen
c
      character*16  grpdef
c
      integer*4     GETELT,GETELS
c#ifdef WINNT
      integer       GETELT_i
c#endif
c-----------------------------------------------------------------------
c-----General initialisation
c-----------------------------------------------------------------------
      pi     = atan (1.0) * 4.
      eps    = 1.e-12
c--------------------------------------------------------------------
c-----Initialize Nefis variables
c--------------------------------------------------------------------
      grpdef    = 'map-series'
      usrord    = 1
      buflen    = 4 * nmax   * mmax
      uindex(1) = itim
      uindex(2) = itim
      uindex(3) = 1
c--------------------------------------------------------------------
c-----Read from group 3 TAUKSI
c--------------------------------------------------------------------
      okee      = okee .and.
     *            GETELT(hdefds   ,grpdef    ,'TAUKSI'  ,
     *                   uindex   ,usrord ,buflen    ,TAUKSI    )
     *            .eq. 0
c--------------------------------------------------------------------
c-----Read from group 3 TAUETA
c--------------------------------------------------------------------
      okee      = okee .and.
     *            GETELT(hdefds   ,grpdef    ,'TAUETA'  ,
     *                   uindex   ,usrord ,buflen    ,TAUETA    )
     *            .eq. 0
c--------------------------------------------------------------------
c-----Calculate TAUZU, TAUZV, TAUM and TAUD
c     if kcs = 0 then permanent drypoint; if surrounding kfu and kfv
c     := 0 then temporary drypoint
c--------------------------------------------------------------------
      do 110 n = nmax, 1, -1
         do 110 m = mmax, 1, -1
            md    = max (1,m-1)
            nd    = max (1,n-1)
            lvar  = ((kcs   (n ,m ) .eq. 1) .and.
     *               (kfu   (n ,m ) .eq. 1  .or.
     *                kfu   (n ,md) .eq. 1  .or.
     *                kfv   (n ,m ) .eq. 1  .or.
     *                kfv   (nd,m ) .eq. 1))
            if (lvar  ) then
c--------------------------------------------------------------------
c--------------Backward transformation of TAUKSI and TAUETA and
c              calculation of TAUZU and TAUZV
c--------------------------------------------------------------------
                  tkm    = tauksi(n ,m )  * kfu   (n ,m )
                  tkmd   = tauksi(n ,md)  * kfu   (n ,md)
                  ten    = taueta(n ,m )  * kfv   (n ,m )
                  tend   = taueta(nd,m )  * kfv   (nd,m )
c
                  tkgem  = 0.5 * (tkm  + tkmd) * kcs(n,m)
                  tegem  = 0.5 * (ten  + tend) * kcs(n,m)
c
                  tauzu  =  tkgem  * cos( alfas(n,m) ) -
     *                      tegem  * sin( alfas(n,m) )
                  tauzv  =  tkgem  * sin( alfas(n,m) ) +
     *                      tegem  * cos( alfas(n,m) )
c--------------------------------------------------------------------
c--------------Calculation of TAUM and TAUD
c              taud should be defined between 0. and 360. degrees
c              atan2 <-180,180>, grdang [0,360] => mod (.. + 720)
c
c        Note (AM, dd 31 march 1999)
c        The sign before grdang was -, this has been adjusted, though
c        in most cases grdang is zero anyway.
c--------------------------------------------------------------------
               taum   = sqrt ( tauzu  * tauzu  + tauzv  * tauzv )
               if (abs (tauzu ) .lt. eps) then
                  tauzu  = eps
               endif
               if (abs (tauzv ) .lt. eps) then
                  tauzv  = eps
               endif
               hulp   =    90. - atan2 (tauzv ,tauzu ) * 180. / pi +
     *                  grdang
               taud   = mod    ( hulp  +  720.,  360.)
c
               if ( parcod .eq. 16 .or. parcod .eq. 17 ) then
                  tauksi(n ,m ) = tauzu
                  taueta(n ,m ) = tauzv
               else if ( parcod .eq. 18 .or. parcod .eq. 19 ) then
                  tauksi(n ,m ) = taum
                  taueta(n ,m ) = taud
               endif
            else
               tauksi(n ,m ) = misval
               taueta(n ,m ) = misval
            endif
  110 continue
c
      return
      end

      subroutine wrhtav(okee      ,hdefds    ,hdafds    ,misval    ,
     *                  itim      ,nmax      ,mmax      ,
     *                  kcs       ,kfu       ,kfv       ,
     *                  xcor      ,ycor      ,alfas     ,
     *                  tauksi    ,taueta                          )
c-----------------------------------------------------------------------
c             Module: SUBROUTINE WRHTAV
c           Function: get NEFIS data for time itim for
c                     parameter = 39  'tf'
c        Method used:
c
c-----------------------------------------------------------------------
c   Calling routine :              ods_tri_nef_map_getdata
c-----------------------------------------------------------------------
c   Called  routines:              GETELT (nf)
c-----------------------------------------------------------------------
c  Formal parameters:
c  ------------------
c
c   Var. I/O  Type Dimensions
c   -------------------------
c
c ALFAS   I   R*4  NMAX,MMAX       Transformation coefficients (in radians)
c OKEE     O  L*4                  Flag for further execution program
c ITIM    I   I*4                  Specified time
c MISVAL  I   R*4                  missing value
c KCS     I   I*4  NMAX,MMAX       Mask array for the zeta points
c                                  (time independent)
c                                  =0 inactive point
c                                  =1 active   point
c                                  =2 open boundary point
c KFU     I   I*4  NMAX,MMAX       Mask array for the u-velocity point
c                                  (time dependent)
c                                  =0 dry      point
c                                  =1 active   point
c KFV     I   I*4  NMAX,MMAX       Mask array for the v-velocity point
c                                  (time dependent)
c                                  =0 dry      point
c                                  =1 active   point
c HDAFDS  I   I*4  999             Data file descriptor for the MAP-DAT
c                                  file
c HDEFDS  I   I*4  2997            Definition file description for the
c                                  MAP-DEF file
c MMAX    I   I*4                  Number of gridpoints in the x-dir.
c NMAX    I   I*4                  Number of gridpoints in the y-dir.
c TAUETA   O  R*4  NMAX,MMAX       Tau bottom in v-velocity point
c TAUKSI   O  R*4  NMAX,MMAX       Tau bottom in u-velocity point
c XCOR    I   R*4  NMAX,MMAX       X-distance between 2 grid lines
c                                  around a zeta point in y-direction
c YCOR    I   R*4  NMAX,MMAX       Y-distance between 2 grid lines
c                                  around a zeta point in y-direction
c-----------------------------------------------------------------------
c    Local variables:
c    ----------------
c
c   Var.      Type Dimensions
c   -------------------------
c
c BUFLEN      I*4                  Buffer length
c EPS         R*4                  Small value to test for backwards
c                                  transformation
c GRPDEF     CH*16                 Group name definition
c GUUGEM      R*4                  Help var. 'mean' GUU value
c GVVGEM      R*4                  Help var. 'mean' GVV value
c KENMU       I*4                  Mask value for two consecutive u-vel.
c                                  points (max 1)
c KENMV       I*4                  Mask value for two consecutive v-vel.
c                                  points (max 1)
c LVAR        L*4                  Help var.
c M           I*4                  Loop var. 1-MMAX
c MD          I*4                  Max (1,M-1)
c MM          I*4                  Max (2,M)
c MMD         I*4                  MM-1
c N           I*4                  Loop var. 1-NMAX
c ND          I*4                  Max (1,N-1)
c NN          I*4                  Max (2,N)
c NND         I*4                  NN-1
c TAUZU       R*4                  Tau bottom U in a zeta point
c TAUZV       R*4                  Tau bottom V in a zeta point
c TEGEM       R*4                  Help var. 'mean' taueta value
c TEN         R*4                  Help var. for taueta in v-point N
c TEND        R*4                  Help var. for taueta in v-point ND
c TKGEM       R*4                  Help var. 'mean' tauksi value
c TKM         R*4                  Help var. for tauksi in u-point M
c TKMD        R*4                  Help var. for tauksi in u-point MD
c UINDEX      I*4  3               Array containing cell indices with
c                                  has to be read
c USRORD      I*4                  Sequence in which the cells must be
c                                  read
c ZDEF        R*4                  Default value for z value outside
c                                  model
c-----------------------------------------------------------------------
c
c  declarations
c
      integer       nmax  ,mmax  ,nd    ,md
      integer       itim
      integer       n     ,m
c
      real          tauzu ,tauzv ,misval,eps
      real          tegem ,ten   ,tend  ,tkgem ,tkm   ,tkmd
c
      integer       kcs   (nmax  ,mmax  ),
     *              kfu   (nmax  ,mmax  ),kfv   (nmax  ,mmax  )
c
      real          xcor  (nmax  ,mmax  ),ycor  (nmax  ,mmax  ),
     *              alfas (nmax  ,mmax  )
      real          tauksi(nmax  ,mmax  ),taueta(nmax  ,mmax  )
c
      logical       okee  ,lvar
c-----------------------------------------------------------------------
c-----Declaraties NEFIS
c-----------------------------------------------------------------------
      integer*4     hdefds( 2997),hdafds(  999)
c
      integer*4     uindex(    3),usrord,buflen
c
      character*16  grpdef
c
      integer*4     GETELT,GETELS
c#ifdef WINNT
      integer       GETELT_i
c#endif
c-----------------------------------------------------------------------
c-----General initialisation
c-----------------------------------------------------------------------
      eps    = 1.e-12
c--------------------------------------------------------------------
c-----Initialize Nefis variables
c--------------------------------------------------------------------
      grpdef    = 'map-series'
      usrord    = 1
      buflen    = 4 * nmax   * mmax
      uindex(1) = itim
      uindex(2) = itim
      uindex(3) = 1
c--------------------------------------------------------------------
c-----Read from group 3 TAUKSI
c--------------------------------------------------------------------
      okee      = okee .and.
     *            GETELT(hdefds   ,grpdef    ,'TAUKSI'  ,
     *                   uindex   ,usrord ,buflen    ,TAUKSI    )
     *            .eq. 0
c--------------------------------------------------------------------
c-----Read from group 3 TAUETA
c--------------------------------------------------------------------
      okee      = okee .and.
     *            GETELT(hdefds   ,grpdef    ,'TAUETA'  ,
     *                   uindex   ,usrord ,buflen    ,TAUETA    )
     *            .eq. 0
c
c-----Calculate TAUZU and TAUZV
c     if kcs = 0 then permanent drypoint; if surrounding kfu and kfv
c     := 0 then temporary drypoint
      do 110 n = nmax, 1, -1
         do 110 m = mmax, 1, -1
            md    = max (1,m-1)
            nd    = max (1,n-1)
            lvar  = ((kcs   (n ,m ) .ne. 0) .and.
     *               (kfu   (n ,m ) .eq. 1  .or.
     *                kfu   (n ,md) .eq. 1  .or.
     *                kfv   (n ,m ) .eq. 1  .or.
     *                kfv   (nd,m ) .eq. 1))
            if (lvar  ) then
c--------------Backward transformation of TAUKSI and TAUETA and
c              calculation of TAUZU and TAUZV
                  tkm    = tauksi(n ,m )  * kfu   (n ,m )
                  tkmd   = tauksi(n ,md)  * kfu   (n ,md)
                  ten    = taueta(n ,m )  * kfv   (n ,m )
                  tend   = taueta(nd,m )  * kfv   (nd,m )
c
                  tkgem  = 0.5 * (tkm  + tkmd) * kcs(n,m)
                  tegem  = 0.5 * (ten  + tend) * kcs(n,m)
c
                  tauzu  =  tkgem * cos( alfas(n,m) ) -
     *                      tegem * sin( alfas(n,m) )
                  tauzv  =  tkgem * sin( alfas(n,m) ) +
     *                      tegem * cos( alfas(n,m) )
c
               tauksi(n ,m ) = tauzu
               taueta(n ,m ) = tauzv
            else
               tauksi(n ,m ) = misval
               taueta(n ,m ) = misval
            endif
  110 continue
c
      return
      end

      subroutine wrhuvv(okee      ,hdefds    ,hdafds    ,misval    ,
     *                  itim      ,nmax      ,mmax      ,kmax      ,
     *                  kcs       ,kfu       ,kfv       ,
     *                  xcor      ,ycor      ,
     *                  alfas                ,u1        ,v1        )
c-----------------------------------------------------------------------
c             Module: SUBROUTINE WRHUVV
c           Function: get NEFIS data for time itim for
c                     parameter = 40  'uv'
c        Method used:
c
c-----------------------------------------------------------------------
c   Calling routine :              ods_tri_nef_map_getdata
c-----------------------------------------------------------------------
c   Called  routines:              GETELT (nf)
c-----------------------------------------------------------------------
c  Formal parameters:
c  ------------------
c
c   Var. I/O  Type Dimensions
c   -------------------------
c
c ALFAS   I   R*4  NMAX,MMAX       Transformation coefficients (in radians)
c OKEE     O  L*4                  Flag for further execution program
c ITIM    I   I*4                  Specified time
c MISVAL  I   R*4                  missing value
c KCS     I   I*4  NMAX,MMAX       Mask array for the zeta points
c                                  (time independent)
c                                  =0 inactive point
c                                  =1 active   point
c                                  =2 open boundary point
c KFU     I   I*4  NMAX,MMAX       Mask array for the u-velocity point
c                                  (time dependent)
c                                  =0 dry      point
c                                  =1 active   point
c KFV     I   I*4  NMAX,MMAX       Mask array for the v-velocity point
c                                  (time dependent)
c                                  =0 dry      point
c                                  =1 active   point
c KMAX    I   I*4                  Number of layers
c HDAFDS  I   I*4  999             Data file descriptor for the MAP-DAT
c                                  file
c HDEFDS  I   I*4  2997            Definition file description for the
c                                  MAP-DEF file
c MMAX    I   I*4                  Number of gridpoints in the x-dir.
c NMAX    I   I*4                  Number of gridpoints in the y-dir.
c U1       O  R*4  NMAX,MMAX,KMAX  U-velocity in u-velocity point
c V1       O  R*4  NMAX,MMAX,KMAX  V-velocity in v-velocity point
c XCOR    I   R*4  NMAX,MMAX       X-distance between 2 grid lines
c                                  around a zeta point in y-direction
c YCOR    I   R*4  NMAX,MMAX       Y-distance between 2 grid lines
c                                  around a zeta point in y-direction
c-----------------------------------------------------------------------
c    Local variables:
c    ----------------
c
c   Var.      Type Dimensions
c   -------------------------
c
c BUFLEN      I*4                  Buffer length
c EPS         R*4                  Small value to test for backwards
c                                  transformation
c GRPDEF     CH*16                 Group name definition
c GUUGEM      R*4                  Help var. 'mean' GUU value
c GVVGEM      R*4                  Help var. 'mean' GVV value
c KENMU       I*4                  Mask value for two consecutive u-vel.
c                                  points (max 1)
c KENMV       I*4                  Mask value for two consecutive v-vel.
c                                  points (max 1)
c LVAR        L*4                  Help var.
c M           I*4                  Loop var. 1-MMAX
c MD          I*4                  Max (1,M-1)
c MM          I*4                  Max (2,M)
c MMD         I*4                  MM-1
c N           I*4                  Loop var. 1-NMAX
c ND          I*4                  Max (1,N-1)
c NN          I*4                  Max (2,N)
c NND         I*4                  NN-1
c UGEM        R*4                  Help var. 'mean' u-velocity value
c UINDEX      I*4  3               Array containing cell indices with
c                                  has to be read
c UM          R*4                  Help var. for u-vel. in u-point M
c UMD         R*4                  Help var. for u-vel. in u-point MD
c USRORD      I*4                  Sequence in which the cells must be
c                                  read
c UVD         R*4                  Velocity direction
c UVM         R*4                  Velocity magnitude
c UZ          R*4                  U-velocity in a zeta point
c VGEM        R*4                  Help var. 'mean' v-velocity value
c VN          R*4                  Help var. for v-vel. in v-point N
c VND         R*4                  Help var. for v-vel. in v-point ND
c VZ          R*4                  V-velocity in a zeta point
c ZDEF        R*4                  Default value for z value outside
c                                  model
c-----------------------------------------------------------------------
c
c  declarations
c
      integer       kmax  ,itim
      integer       nmax  ,mmax  ,nd    ,md
      integer       n     ,m     ,k
c
      real          misval,eps
      real          ugem  ,um    ,umd   ,uz
      real          vgem  ,vn    ,vnd   ,vz
c
      integer       kcs   (nmax  ,mmax  ),
     *              kfu   (nmax  ,mmax  ),kfv   (nmax  ,mmax  )
c
      real          xcor  (nmax  ,mmax  ),ycor  (nmax  ,mmax  ),
     *              alfas (nmax  ,mmax  )
      real          u1    (nmax  ,mmax   ,kmax  ),
     *              v1    (nmax  ,mmax   ,kmax  )
c
      logical       okee  ,lvar
c-----------------------------------------------------------------------
c-----Declarations NEFIS
c-----------------------------------------------------------------------
      integer*4     hdefds( 2997),hdafds(  999)
c
      integer*4     uindex(    3),usrord,buflen
c
      character*16  grpdef
c
      integer*4     GETELT,GETELS
c#ifdef WINNT
      integer       GETELT_i
c#endif
c-----------------------------------------------------------------------
c-----General initialisation
c-----------------------------------------------------------------------
      eps    = 1.e-12
c--------------------------------------------------------------------
c-----Initialize Nefis variables
c--------------------------------------------------------------------
      grpdef    = 'map-series'
      usrord    = 1
      buflen    = 4 * nmax   * mmax   * kmax
      uindex(1) = itim
      uindex(2) = itim
      uindex(3) = 1
c--------------------------------------------------------------------
c-----Read from  group 3 U1
c--------------------------------------------------------------------
      okee      = okee .and.
     *            GETELT(hdefds   ,grpdef    ,'U1    '  ,
     *                   uindex   ,usrord ,buflen    ,U1        )
     *            .eq. 0
c--------------------------------------------------------------------
c-----Read from group 3 V1
c--------------------------------------------------------------------
      okee      = okee .and.
     *            GETELT(hdefds   ,grpdef    ,'V1    '  ,
     *                   uindex   ,usrord ,buflen    ,V1        )
     *            .eq. 0
c-----Calculate UZ and VZ
c     if kcs = 0 then permanent drypoint; if surrounding kfu and kfv
c     := 0 then temporary drypoint
      do 110 n = nmax, 1, -1
         do 110 m = mmax, 1, -1
            md    = max (1,m-1)
            nd    = max (1,n-1)
            lvar  = ((kcs   (n ,m ) .ne. 0) .and.
     *               (kfu   (n ,m ) .eq. 1  .or.
     *                kfu   (n ,md) .eq. 1  .or.
     *                kfv   (n ,m ) .eq. 1  .or.
     *                kfv   (nd,m ) .eq. 1))
            do 110 k = 1, kmax
               if (lvar  ) then
c-----------------------------------------------------------------------
c-----------------Backwards transformation of U1 and V1 and
c                 calculation of UZ and VZ
c-----------------------------------------------------------------------
                     um     = u1    (n ,m ,k )  * kfu   (n ,m )
                     umd    = u1    (n ,md,k )  * kfu   (n ,md)
                     vn     = v1    (n ,m ,k )  * kfv   (n ,m )
                     vnd    = v1    (nd,m ,k )  * kfv   (nd,m )
c
                     ugem   = 0.5 * (um   + umd ) * kcs(n,m)
                     vgem   = 0.5 * (vn   + vnd ) * kcs(n,m)
c
                     uz     =  ugem * cos( alfas(n,m) ) -
     *                         vgem * sin( alfas(n,m) )
                     vz     =  ugem * sin( alfas(n,m) ) +
     *                         vgem * cos( alfas(n,m) )
c
                  u1    (n ,m ,k ) = uz
                  v1    (n ,m ,k ) = vz
               else
                  u1    (n ,m ,k ) = misval
                  v1    (n ,m ,k ) = misval
               endif
  110 continue
c
      return
      end

      subroutine wrhgrd(nmax      ,mmax      ,noroco    ,norow     ,
     *                  irocol    ,kcu       ,kcv       ,iflag     ,
     *                  igrid                                      )
c-----------------------------------------------------------------------
c             Module: SUBROUTINE WRHGRD
c           Function: get NEFIS time independent data for
c                     parameter = 35  'ag'
c                     parameter = 36  'bl'
c        Method used:
c
c-----------------------------------------------------------------------
c   Calling routine :              ods_tri_nef_map_getdata
c-----------------------------------------------------------------------
c   Called  routines:              NONE
c-----------------------------------------------------------------------
c  Formal parameters:
c  ------------------
c
c   Var. I/O  Type Dimensions
c   -------------------------
c
c IFLAG    O  I*4  NMAX,MMAX       Array for permanent and tempory dry
c                                  point
c IGRID    O  I*4  NMAX,MMAX       Array with actual grid
c IROCOL  I   I*4  5,NOROCO        Pointer table with bound. coord. and
c                                  bound. types (comp. cols. and rows)
c KCU     I   I*4  NMAX,MMAX       Mask array for the u-velocity point
c                                  (time independent)
c                                  =0 dry      point
c                                  =1 active   point
c KCV     I   I*4  NMAX,MMAX       Mask array for the v-velocity point
c                                  (time independent)
c                                  =0 dry      point
c                                  =1 active   point
c MMAX    I   I*4                  Number of gridpoints in the x-dir.
c NMAX    I   I*4                  Number of gridpoints in the y-dir.
c NOROCO  I   I*4                  Number of Computational rows & cols
c NOROW       I*4                  Number of comp. rows  in IROCOL-array
c-----------------------------------------------------------------------
c    Local variables:
c    ----------------
c
c   Var.      Type Dimensions
c   -------------------------
c
c I23         I*4                  Value for tekal nabour n+1,m
c I29         I*4                  Value for tekal nabour n-1,m
c I5          I*4                  Value for tekal nabour n,m+1
c I7          I*4                  Value for tekal nabour n,m-1
c M           I*4                  Help var.
c M1          I*4                  Help var.
c M2          I*4                  Help var.
c N           I*4                  Help var.
c N1          I*4                  Help var.
c-----------------------------------------------------------------------
c
c  DECLARATIONS
c
      integer       nmax  ,mmax  ,noroco,norow ,n     ,m
      integer       m1    ,m2    ,i     ,n1
      integer       i23   ,i29   ,i5    ,i7
c
      integer       irocol(5     ,noroco),kcu   (nmax  ,mmax  ),
     *              kcv   (nmax  ,mmax  ),iflag (nmax  ,mmax  )
      integer       igrid (nmax  ,mmax  )
c--------------------------------------------------------------------
c-----Initialize iflag = 0 and igrid = 0
c--------------------------------------------------------------------
      do 110 m = 1,mmax
         do 110 n = 1,nmax
            iflag (n,m) = 0
            igrid (n,m) = 0
  110 continue
c--------------------------------------------------------------------
c-----Define IBUFF values inside irocol table and
c     calculate minima and maxima
c--------------------------------------------------------------------
      do 210 i = 1,norow
         n     = irocol(1,i)
         m1    = irocol(2,i)
         m2    = irocol(3,i)
         do 220 m = m1-1,m2
            igrid (n  ,m) = 1
            igrid (n-1,m) = 1
  220    continue
  210 continue
c-----------------------------------------------------------------------
c--------Define IFLAG values inside irocol table
c  +--------------------------------------------------------------------
c  |  scan <kcu> and <kcv> for 0, to find: boundaries,
c  |                                       small dams and
c  |                                       permanently dry points.
c  |
c  |  "internal codes" of <iflag> are:
c  |                                           1000
c  |                                             |
c  |                                     0001 -- o -- 0010
c  |                                             |
c  |                                           0100
c  +--------------------------------------------------------------------
c-----------------------------------------------------------------------
c        +--------------------------------------------------------------
c        | interior + right border + upper border
c        +--------------------------------------------------------------
c        |
c        |   n ----->  + 10 -  1 +    -    +    -   +
c        |                      100
c        |             |    o    |    o    |    o   |
c        |                     1000
c        |             +    -    +    -    +    -   +
c        |
c        |             |    o    |    o    |    o   |
c        |
c        |             +    -    +    -    +    -   +
c        |                       |
c        |                       |
c        |                       m
c        +--------------------------------------------------------------
c-----------------------------------------------------------------------
      do 310 i = 1,norow
         n     = irocol(1,i)
         m1    = irocol(2,i)
         m2    = irocol(3,i)
         do 320 m = m1,m2
            if (kcu(n,m) .eq. 0) then
               iflag (n  ,m) = iflag (n  ,m) +  100
               iflag (n-1,m) = iflag (n-1,m) + 1000
            endif
            if (kcv(n,m) .eq. 0) then
               iflag (n,m  ) = iflag (n,m  ) +  1
               iflag (n,m-1) = iflag (n,m-1) + 10
            endif
  320    continue
  310 continue
c-----------------------------------------------------------------------
c        +--------------------------------------------------------------
c        | left border
c        +--------------------------------------------------------------
c        |
c        |   n ----->  +    -    +    -    +    -   +
c        |            100
c        |             |    o    |    o    |    o   |
c        |           1000
c        |             +    -    +    -    +    -   +
c        |
c        |             |    o    |    o    |    o   |
c        |
c        |             +    -    +    -    +    -   +
c        |                       |
c        |                       |
c        |                       m1
c        +--------------------------------------------------------------
c-----------------------------------------------------------------------
      do 330 i = 1,norow
         n     = irocol(1,i)
         m1    = irocol(2,i)
         if (kcu(n,m1-1) .eq. 0) then
            iflag (n  ,m1-1) = iflag (n  ,m1-1) +  100
            iflag (n-1,m1-1) = iflag (n-1,m1-1) + 1000
         endif
  330 continue
c-----------------------------------------------------------------------
c        +--------------------------------------------------------------
c        | lower border
c        +--------------------------------------------------------------
c        |
c        |             +    -    +    -    +    -   +
c        |
c        |             |    o    |    o    |    o   |
c        |
c        |   m ----->  +    -    +    -    +    -   +
c        |
c        |             |    o    |    o    |    o   |
c        |
c        |             + 10 -  1 +    -    +    -   +
c        |                       |
c        |                       |
c        |                       n1
c        +--------------------------------------------------------------
c-----------------------------------------------------------------------
      do 340 i = norow+1,noroco
         m     = irocol(1,i)
         n1    = irocol(2,i)
         if (kcv(n1-1,m) .eq. 0) then
            iflag (n1-1,m  ) = iflag (n1-1,m  ) +  1
            iflag (n1-1,m-1) = iflag (n1-1,m-1) + 10
         endif
  340 continue
c-----------------------------------------------------------------------
c        +--------------------------------------------------------------
c        | swap "internal codes" to "external codes" (=TEKAL-codes)
c        +--------------------------------------------------------------
c        |     "internal codes"            "external codes"
c        |
c        |          1000                           23
c        |            |                             |
c        |    0001 -- o -- 0010                7 -- o -- 5
c        |            |                             |
c        |          0100                           29
c        +--------------------------------------------------------------
c-----------------------------------------------------------------------
      do 350 n = 1,nmax
         do 350 m = 1,mmax
            i23 = (iflag (n,m)                       ) / 1000
            i29 = (iflag (n,m)-i23*1000              ) / 100
            i5  = (iflag (n,m)-i23*1000-i29*100      ) / 10
            i7  = (iflag (n,m)-i23*1000-i29*100-i5*10)
            iflag (n,m) = i7*7 + i5*5 + i29*29 + i23*23
  350 continue
c
      return
      end

      subroutine wrhtd (nmax      ,mmax      ,noroco    ,norow     ,
     *                  irocol    ,kcu       ,kcv       ,
     *                  kfu       ,kfv       ,iflag                )
c-----------------------------------------------------------------------
c             Module: SUBROUTINE WRHTD
c           Function: get NEFIS data time dependent for
c                     parameter = 37  'td'
c        Method used:
c
c-----------------------------------------------------------------------
c   Calling routine :              ods_tri_nef_map_getdata
c-----------------------------------------------------------------------
c   Called  routines:              NONE
c-----------------------------------------------------------------------
c  Formal parameters:
c  ------------------
c
c   Var. I/O  Type Dimensions
c   -------------------------
c
c IFLAG    O  I*4  NMAX,MMAX       Array for permanent and tempory dry
c                                  point
c IROCOL  I   I*4  5,NOROCO        Pointer table with bound. coord. and
c                                  bound. types (comp. cols. and rows)
c KCU     I   I*4  NMAX,MMAX       Mask array for the u-velocity point
c                                  (time independent)
c                                  =0 dry      point
c                                  =1 active   point
c KCV     I   I*4  NMAX,MMAX       Mask array for the v-velocity point
c                                  (time independent)
c                                  =0 dry      point
c                                  =1 active   point
c KFU      O  I*4  NMAX,MMAX       Mask array for the u-velocity point
c                                  (time dependent)
c                                  =0 dry      point
c                                  =1 active   point
c KFV      O  I*4  NMAX,MMAX       Mask array for the v-velocity point
c                                  (time dependent)
c                                  =0 dry      point
c                                  =1 active   point
c MMAX    I   I*4                  Number of gridpoints in the x-dir.
c NMAX    I   I*4                  Number of gridpoints in the y-dir.
c NOROCO  I   I*4                  Number of Computational rows & cols
c NOROW   I   I*4                  Number of comp. rows  in IROCOL-array
c-----------------------------------------------------------------------
c    Local variables:
c    ----------------
c
c   Var.      Type Dimensions
c   -------------------------
c
c I23         I*4                  Value for tekal nabour n+1,m
c I29         I*4                  Value for tekal nabour n-1,m
c I5          I*4                  Value for tekal nabour n,m+1
c I7          I*4                  Value for tekal nabour n,m-1
c M           I*4                  Help var.
c M1          I*4                  Help var.
c M2          I*4                  Help var.
c N           I*4                  Help var.
c N1          I*4                  Help var.
c-----------------------------------------------------------------------
c
c  declarations
c
      integer       nmax  ,mmax  ,noroco,norow ,n     ,m
      integer       m1    ,m2    ,i     ,n1
      integer       i23   ,i29   ,i5    ,i7
c
      integer       irocol(5     ,noroco),kcu   (nmax  ,mmax  ),
     *              kcv   (nmax  ,mmax  ),kfu   (nmax  ,mmax  ),
     *              kfv   (nmax  ,mmax  ),iflag (nmax  ,mmax  )
c
c--------------------------------------------------------------------
c-----Initialize iflag = 0
c--------------------------------------------------------------------
      do 110 m = 1,mmax
         do 110 n = 1,nmax
            iflag (n,m) = 0
  110 continue
c-----------------------------------------------------------------------
c--------Define IFLAG values inside the irocol table
c-----------------------------------------------------------------------
c  |  scan <kfu> and <kfv> for 0 and
c  |       <kcu> and <kcv> for <> 0, to find: temporary dry points.
c  |  NOTE: kcu = 0 => perm. dry, kcu = 1 and kfu = 0 temp. dry
c  |        kcu - kfu =  0 then or active or perm. dry
c  |        kcu - kfu = -1 theoretically impossible
c  |        kcu - kfu =  1 temp. dry
c  |
c  |  "internal codes" of <iflag> are:
c  |                                           1000
c  |                                             |
c  |                                     0001 -- o -- 0010
c  |                                             |
c  |                                           0100
c  +--------------------------------------------------------------------
c-----------------------------------------------------------------------
c        +--------------------------------------------------------------
c        | interior + right border + upper border
c        +--------------------------------------------------------------
c        |
c        |   n ----->  + 10 -  1 +    -    +    -   +
c        |                      100
c        |             |    o    |    o    |    o   |
c        |                     1000
c        |             +    -    +    -    +    -   +
c        |
c        |             |    o    |    o    |    o   |
c        |
c        |             +    -    +    -    +    -   +
c        |                       |
c        |                       |
c        |                       m
c        +--------------------------------------------------------------
c-----------------------------------------------------------------------
      do 310 i = 1,norow
         n     = irocol(1,i)
         m1    = irocol(2,i)
         m2    = irocol(3,i)
         do 320 m = m1,m2
            if ((kcu(n,m) - kfu(n,m)) .eq. 1) then
               iflag (n  ,m) = iflag (n  ,m) +  100
               iflag (n-1,m) = iflag (n-1,m) + 1000
            endif
            if ((kcv(n,m) - kfv(n,m)) .eq. 1) then
               iflag (n,m  ) = iflag (n,m  ) +  1
               iflag (n,m-1) = iflag (n,m-1) + 10
            endif
  320    continue
  310 continue
c-----------------------------------------------------------------------
c        +--------------------------------------------------------------
c        | left border
c        +--------------------------------------------------------------
c        |
c        |   n ----->  +    -    +    -    +    -   +
c        |            100
c        |             |    o    |    o    |    o   |
c        |           1000
c        |             +    -    +    -    +    -   +
c        |
c        |             |    o    |    o    |    o   |
c        |
c        |             +    -    +    -    +    -   +
c        |                       |
c        |                       |
c        |                       m1
c        +--------------------------------------------------------------
c-----------------------------------------------------------------------
      do 330 i = 1,norow
         n     = irocol(1,i)
         m1    = irocol(2,i)
         if ((kcu(n,m1-1) - kfu(n,m1-1)) .eq. 1) then
            iflag (n  ,m1-1) = iflag (n  ,m1-1) +  100
            iflag (n-1,m1-1) = iflag (n-1,m1-1) + 1000
         endif
  330 continue
c-----------------------------------------------------------------------
c        +--------------------------------------------------------------
c        | lower border
c        +--------------------------------------------------------------
c        |
c        |             +    -    +    -    +    -   +
c        |
c        |             |    o    |    o    |    o   |
c        |
c        |   m ----->  +    -    +    -    +    -   +
c        |
c        |             |    o    |    o    |    o   |
c        |
c        |             + 10 -  1 +    -    +    -   +
c        |                       |
c        |                       |
c        |                       n1
c        +--------------------------------------------------------------
c-----------------------------------------------------------------------
      do 340 i = norow+1,noroco
         m     = irocol(1,i)
         n1    = irocol(2,i)
         if ((kcv(n1-1,m) - kfv(n1-1,m)) .eq. 1) then
            iflag (n1-1,m  ) = iflag (n1-1,m  ) +  1
            iflag (n1-1,m-1) = iflag (n1-1,m-1) + 10
         endif
  340 continue
c-----------------------------------------------------------------------
c        +--------------------------------------------------------------
c        | swap "internal codes" to "external codes" (=TEKAL-codes)
c        +--------------------------------------------------------------
c        |     "internal codes"            "external codes"
c        |
c        |          1000                           23
c        |            |                             |
c        |    0001 -- o -- 0010                7 -- o -- 5
c        |            |                             |
c        |          0100                           29
c        +--------------------------------------------------------------
c-----------------------------------------------------------------------
c--------Write values to tekal-data file
c-----------------------------------------------------------------------
      do 350 n = 1,nmax
         do 350 m = 1,mmax
            i23 = (iflag (n,m)                       ) / 1000
            i29 = (iflag (n,m)-i23*1000              ) / 100
            i5  = (iflag (n,m)-i23*1000-i29*100      ) / 10
            i7  = (iflag (n,m)-i23*1000-i29*100-i5*10)
            iflag (n,m) = -1 * (i7*7 + i5*5 + i29*29 + i23*23)
  350 continue
c
      return
      end
