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
!     $Author: Markus $
!     $Date: 6-06-03 10:40 $
!     $Source: /u/cvsroot/gpp/libsrc/ods/tri_map.f,v $
!

subroutine ods_tri_nef_map_par&
&(&
&fname , itype , pardef, maxdef, timdep, locdep,&
&maxlst, lang  , parlst, paruni, partyp, parcod,&
&nrlst , ierror, option                        )
!-----------------------------------------------------------------------
!         Function: parameter name selection for maps
!                   TRISULA NEFIS MAP files
!        Method used:
!-----------------------------------------------------------------------
!   Calling routine :              GETPAR
!-----------------------------------------------------------------------
!   Called  routines:              OPNNEF
!                                  CLOSFL
!                                  GETELT (nefis)
!                                  INQELM (nefis)
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
! BUFLEN      I*4                  Size in bytes of available buffer
! CELDEF     CH*16                 Cell name definition
! ELMNAM     CH*16                 Element name definition
! FILHDA     CH*256                File name NEFIS data file for MAP
! FILHDE     CH*256                File name NEFIS definition file for
!                                  MAP
! GRPDEF     CH*16                 Group name definition
! HDAFDS      I*4  999             Data file descriptor for the MAP-DAT
!                                  file
! HDEFDS      I*4  2997            Definition file description for the
!                                  MAP-DEF file
! IERROR      I*4                  Error code for NEFIS error
! num_layers_grid        I*4                  Number of layers
! LMAX    I   I*4                  Number of constituents
!                                  for old files LMAX = LSTCI
!                                  for new files LMAX = LSTCI + LTUR
! LSTCI   I   I*4                  Total number of constituents (incl.
!                                  turbulence for old trim files).
! LTUR    I   I*4                  Number of turbulence constituents
! NAMCON     CH*20                 Constituent names
! NOSTAT      I*4                  Number of defined stations
! NTRUV       I*4                  Number of cross-secties in u- and
!                                  v-direction
! MAPIND      I*4  MXNPAR          TRISULA-NEFIS dependant index list
! MAPLST     CH*20 MXNPAR          TRISULA-NEFIS possible parameterlist
! MAPTYP      I*4  MXNPAR          TRISULA-NEFIS possible code list
! MAPUNI     CH*20 MXNPAR          TRISULA-NEFIS possible unitlist
! SELMAP  I  CH*20                 Output flags containing Y or N for
!                                  various output quantities selection
!                                  for map files
! UINDEX      I*4  3               Array with indices of the cells to
!                                  be read
! USRORD      I*4                  Sequence in which the cells must be
!                                  read
! ZRHO        L*4                  if .true. then density included
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
   integer         maxdef,maxlst,i     ,npar  ,num_columns  ,num_rows
   integer         num_layers_grid  ,lmax  ,l     ,ind
   integer         lstci ,ltur  ,irho
   integer         ierror,nrlst
!
   integer         mapind(mxnpar)
   integer         maptyp(mxnpar)
   integer         partyp(maxlst)
   integer         parcod(maxlst)
!
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
!
   logical         ex    ,zrho  ,okee
!-----------------------------------------------------------------------
!-----declaration NEFIS
!-----------------------------------------------------------------------
   integer       hdefds( 2997),hdafds(  999)
!
   integer       uindex(    3),usrord,buflen

   character*16  grpdef,elmnam
!
   integer       GETELT,GETELS,INQELM

   integer       TMLCDP
   parameter     ( TMLCDP = IPLMNK + IPTDEP )
!
   data (mapind(i),maplst(i),mapuni(i),maptyp(i),i=1,15,1)/&
   &1,'water level         ','m                   ',    TMLCDP,&
   &2,'total water depth   ','m                   ',    TMLCDP,&
   &3,'dpt. aver. cur. u   ','m/s                 ',    TMLCDP,&
   &4,'dpt. aver. cur. v   ','m/s                 ',    TMLCDP,&
   &5,'dpt. aver. cur. mag.','m/s                 ',    TMLCDP,&
   &6,'dpt. aver. cur. dir.','degrees             ',    TMLCDP,&
   &7,'current u           ','m/s                 ',    TMLCDP,&
   &8,'current v           ','m/s                 ',    TMLCDP,&
   &9,'current mag. (horiz)','m/s                 ',    TMLCDP,&
   &10,'current dir. (horiz)','degrees             ',    TMLCDP,&
   &11,'current w           ','m/s                 ',    TMLCDP,&
   &12,'z-coordinate        ','user defined        ',    TMLCDP,&
   &13,'flow rate u         ','m**3/s              ',    TMLCDP,&
   &14,'flow rate v         ','m**3/s              ',    TMLCDP,&
   &15,'eddy viscosity      ','m**2/s              ',    TMLCDP/
   data (mapind(i),maplst(i),mapuni(i),maptyp(i),i=16,30,1)/&
   &16,'bed stress u        ','N/m**2              ',    TMLCDP,&
   &17,'bed stress v        ','N/m**2              ',    TMLCDP,&
   &18,'bed stress mag.     ','N/m**2              ',    TMLCDP,&
   &19,'bed stress dir.     ','degrees             ',    TMLCDP,&
   &20,'constituent         ','user defined        ',    TMLCDP,&
   &21,'constituent         ','user defined        ',    TMLCDP,&
   &22,'constituent         ','user defined        ',    TMLCDP,&
   &23,'constituent         ','user defined        ',    TMLCDP,&
   &24,'constituent         ','user defined        ',    TMLCDP,&
   &25,'constituent         ','user defined        ',    TMLCDP,&
   &26,'constituent         ','user defined        ',    TMLCDP,&
   &27,'constituent         ','user defined        ',    TMLCDP,&
   &28,'constituent         ','user defined        ',    TMLCDP,&
   &29,'constituent         ','user defined        ',    TMLCDP,&
   &30,'                    ','                    ',    TMLCDP/
   data (mapind(i),maplst(i),mapuni(i),maptyp(i),i=31,45,1)/&
   &31,'density             ','kg/m**3             ',    TMLCDP,&
   &32,'eddy diffusivity    ','m**2/s              ',    TMLCDP,&
   &33,'                    ','                    ',    TMLCDP,&
   &34,'                    ','                    ',    TMLCDP,&
   &35,'active grid         ','-                   ',    TMLCDP,&
   &36,'boundary lines      ','-                   ',    TMLCDP,&
   &37,'temporary dry pnts. ','-                   ',    TMLCDP,&
   &38,'vectors dpth. aver. ','-                   ',    TMLCDP,&
   &39,'vectors bed stress  ','-                   ',    TMLCDP,&
   &40,'vectors velocity uv ','-                   ',    TMLCDP,&
   &41,'vectors velocity uw ','-                   ',    TMLCDP,&
   &42,'vectors velocity vw ','-                   ',    TMLCDP,&
   &43,'                    ','                    ',    TMLCDP,&
   &44,'dpt. at d_points    ','m                   ',    IPLMNK,&
   &45,'dpt. at z_points    ','m                   ',    IPLMNK/
   data (mapind(i),maplst(i),mapuni(i),maptyp(i),i=46,60,1)/&
   &46,'XCOR                ','user defined        ',    IPLMNK,&
   &47,'YCOR                ','user defined        ',    IPLMNK,&
   &48,'XZ                  ','user defined        ',    IPLMNK,&
   &49,'YZ                  ','user defined        ',    IPLMNK,&
   &50,'UVDAMS              ','-                   ',    TMLCDP,&
   &51,'ZCOR step           ','m                   ',    TMLCDP,&
   &52,'ZCOR slope          ','m                   ',    TMLCDP,&
   &53,'                    ','                    ',    TMLCDP,&
   &54,'                    ','                    ',    TMLCDP,&
   &55,'                    ','                    ',    TMLCDP,&
   &56,'                    ','                    ',    TMLCDP,&
   &57,'                    ','                    ',    TMLCDP,&
   &58,'                    ','                    ',    TMLCDP,&
   &59,'                    ','                    ',    IPLMNK,&
   &60,'                    ','                    ',    IPLMNK/

!-----------------------------------------------------------------------
!-----Initialisation
!-----------------------------------------------------------------------
   okee   = .true.
   zrho   = .false.
!
   ierror =  0
!
!--------------------------------------------------------------------
!-----Test if trim-<runid>.dat and trim-<runid>.def Nefis MAP-files
!     exist
!--------------------------------------------------------------------
   call ods_check_nefis( fname  , '.def' , ierror )
   if ( ierror .ne. ieok   ) then
      return
   endif
!--------------------------------------------------------------------
!-----Open trim-<runid>.dat and trim-<runid>.def MAP-files
!--------------------------------------------------------------------
   call OPNNEF(fname, itype, hdafds, hdefds, ierror)
   if (ierror .ne. 0) then
      ierror = IEFIRO
      return
   endif
!--------------------------------------------------------------------
!-----Read array-dimensions from Nefis MAP-files group 2
!--------------------------------------------------------------------
   grpdef = 'map-const'
!
   uindex(1) = 1
   uindex(2) = 1
   uindex(3) = 1
   usrord    = 1
   buflen    = 4
!
   npar      = 0
   num_columns      = 0
   num_rows      = 0
   lmax      = 0
   lstci     = 0
   ltur      = 0
   irho      = 0
   num_layers_grid      = 0
!
   elmnam    = 'num_columns'
   okee = okee .and.&
   &GETELT(hdefds,grpdef    ,elmnam    ,&
   &uindex,usrord    ,buflen    ,num_columns      )&
   &.eq. 0
!
   elmnam    = 'num_rows'
   okee = okee .and.&
   &GETELT(hdefds,grpdef    ,elmnam    ,&
   &uindex,usrord    ,buflen    ,num_rows      )&
   &.eq. 0
!
!-----------------------------------------------------------------------
!--------Read element LMAX; LMAX only defined in old trim files
!        hence if not defined ierror = -25041
!-----------------------------------------------------------------------
   elmnam    = 'LMAX'
   ierror    = GETELT(hdefds,grpdef    ,elmnam    ,&
   &uindex,usrord    ,buflen    ,LSTCI     )
   if (ierror .ne. 0) then
!-----------------------------------------------------------------------
!-----------In case of a new trih file read LSTCI and LTUR
!-----------------------------------------------------------------------
      elmnam = 'LSTCI'
      ierror = GETELT(hdefds,grpdef    ,elmnam    ,&
      &uindex,usrord    ,buflen    ,LSTCI     )
      if (ierror .ne. 0) then
         okee   = .false.
         goto 8888
      endif
!
      elmnam = 'LTUR'
      ierror = GETELT(hdefds,grpdef    ,elmnam    ,&
      &uindex,usrord    ,buflen    ,LTUR      )
      if (ierror .ne. 0) then
         okee   = .false.
         goto 8888
      endif
   endif
!
   lmax = lstci + ltur
!
   elmnam = 'num_layers_grid'
   ierror = GETELT(hdefds,grpdef    ,elmnam    ,&
   &uindex,usrord    ,buflen    ,num_layers_grid      )
   if (ierror .ne. 0) then
      okee   = .false.
      goto 8888
   endif
!
!-----------------------------------------------------------------------
!-----Element SELMAP selection of output
!-----------------------------------------------------------------------
   buflen    = 20
   elmnam    = 'SELMAP'
   ierror    = GETELS&
   &(hdefds   ,grpdef    ,elmnam    ,&
   &uindex   ,usrord    ,buflen    ,SELMAP    )
   if (ierror .ne. 0) then
!-----------------------------------------------------------------------
!--------In case of a old trim file no SELMAP then
!           re-define SELMAP like definition in subroutine RDPRFL
!-----------------------------------------------------------------------
      selmap = 'YYYYYYYYYYYYYYYYYYYY'
      if (num_layers_grid   .eq. 1) selmap( 4: 5) = 'NN'
      if (lstci  .eq. 0) selmap( 6:13) = 'NNNNNNNN'
      if (ltur   .eq. 0) selmap(14:15) = 'NN'
      if (num_layers_grid   .eq. 1) selmap(18:19) = 'NN'
      if (lmax   .eq. 0) selmap(19:19) = 'N'
      selmap(20:20) = 'X'
   endif
!-----------------------------------------------------------------------
!-----Element NAMCON constituents and turbulence quantity names
!     only if selmap( 6:15) <> 'NNNNNNNNNN'
!-----------------------------------------------------------------------
   if (index (selmap( 6:15),'Y') .gt. 0) then
      buflen    = 20 * lmax
      elmnam    = 'NAMCON'
      ierror    = GETELS&
      &(hdefds,grpdef    ,elmnam    ,&
      &uindex,usrord    ,buflen    ,NAMCON    )
      if (ierror .ne. 0) then
         okee   = .false.
         goto 8888
      endif
!
      do 110 l = 1,lmax
         if ( selmap(5+l:5+l) .eq. 'Y' ) then
            if (namcon(l)(:11) .eq. 'Salinity   ') irho   = 1
            if (namcon(l)(:11) .eq. 'Temperature') irho   = 1
         endif
110   continue
   endif
!-----------------------------------------------------------------------
!-----In case of a old trim file re-define SELMAP(20:20)
!-----------------------------------------------------------------------
   if (selmap(20:20) .eq. 'X') then
      selmap(20:20) = 'N'
      if (irho   .eq. 1) selmap(20:20) = 'Y'
   endif
!
   zrho = irho .eq. 1
!
!--------------------------------------------------------------------
!-----Generate parameternames from Nefis MAP-files
!--------------------------------------------------------------------
!-----water level, ZWL
   if ( selmap(1:1) .eq. 'Y' ) then
      npar = npar + 1
      parcod(npar) = mapind( 1)
      parlst(npar) = maplst(parcod(npar))
      partyp(npar) = maptyp(parcod(npar))
      paruni(npar) = mapuni(parcod(npar))
!-----water depth, ZWL + DPS
      npar = npar + 1
      parcod(npar) = mapind( 2)
      parlst(npar) = maplst(parcod(npar))
      partyp(npar) = maptyp(parcod(npar))
      paruni(npar) = mapuni(parcod(npar))
   endif
   if ( selmap(2:3) .eq. 'YY' ) then
      if (num_layers_grid .gt. 1) then
!--------dpt. aver. cur. u
         npar = npar + 1
         parcod(npar) = mapind( 3)
         parlst(npar) = maplst(parcod(npar))
         partyp(npar) = maptyp(parcod(npar))
         paruni(npar) = mapuni(parcod(npar))
!--------dpt. aver. cur. v
         npar = npar + 1
         parcod(npar) = mapind( 4)
         parlst(npar) = maplst(parcod(npar))
         partyp(npar) = maptyp(parcod(npar))
         paruni(npar) = mapuni(parcod(npar))
!--------dpt. aver. cur. mag.
         npar = npar + 1
         parcod(npar) = mapind( 5)
         parlst(npar) = maplst(parcod(npar))
         partyp(npar) = maptyp(parcod(npar))
         paruni(npar) = mapuni(parcod(npar))
!--------dpt. aver. cur. dir.
         npar = npar + 1
         parcod(npar) = mapind( 6)
         parlst(npar) = maplst(parcod(npar))
         partyp(npar) = maptyp(parcod(npar))
         paruni(npar) = mapuni(parcod(npar))
      endif

!-----current u (layer)
      npar = npar + 1
      parcod(npar) = mapind( 7)
      parlst(npar) = maplst(parcod(npar))
      partyp(npar) = maptyp(parcod(npar))
      paruni(npar) = mapuni(parcod(npar))
!-----current v (layer)
      npar = npar + 1
      parcod(npar) = mapind( 8)
      parlst(npar) = maplst(parcod(npar))
      partyp(npar) = maptyp(parcod(npar))
      paruni(npar) = mapuni(parcod(npar))
!-----current mag. (layer)
      npar = npar + 1
      parcod(npar) = mapind( 9)
      parlst(npar) = maplst(parcod(npar))
      partyp(npar) = maptyp(parcod(npar))
      paruni(npar) = mapuni(parcod(npar))
!-----current dir. (layer)
      npar = npar + 1
      parcod(npar) = mapind(10)
      parlst(npar) = maplst(parcod(npar))
      partyp(npar) = maptyp(parcod(npar))
      paruni(npar) = mapuni(parcod(npar))
   endif

   if ( selmap(5:5) .eq. 'Y' ) then
      if (num_layers_grid .gt. 1) then
!--------current w.   (layer)
         npar = npar + 1
         parcod(npar) = mapind(11)
         parlst(npar) = maplst(parcod(npar))
         partyp(npar) = maptyp(parcod(npar))
         paruni(npar) = mapuni(parcod(npar))
      endif
   endif
!--------z-coordinate (can always be defined: 2D or 3D)
!     if (num_layers_grid .gt. 1) then
   npar = npar + 1
   parcod(npar) = mapind(12)
   parlst(npar) = maplst(parcod(npar))
   partyp(npar) = maptyp(parcod(npar))
   paruni(npar) = mapuni(parcod(npar))
!     endif

!-----flow rate u, ZQXK           (not yet implemented)
!     npar = npar + 1
!     parcod(npar) = mapind(13)
!     parlst(npar) = maplst(parcod(npar))
!     partyp(npar) = maptyp(parcod(npar))
!     paruni(npar) = mapuni(parcod(npar))
!-----flow rate v, ZQYK           (not yet implemented)
!     npar = npar + 1
!     parcod(npar) = mapind(14)
!     parlst(npar) = maplst(parcod(npar))
!     partyp(npar) = maptyp(parcod(npar))
!     paruni(npar) = mapuni(parcod(npar))

   if ( selmap(18:18) .eq. 'Y' ) then
      if (num_layers_grid .gt. 1) then
!--------eddy viscosity, ZVICWW
         npar = npar + 1
         parcod(npar) = mapind(15)
         parlst(npar) = maplst(parcod(npar))
         partyp(npar) = maptyp(parcod(npar))
         paruni(npar) = mapuni(parcod(npar))
      endif
   endif

   if ( selmap(16:17) .eq. 'YY' ) then
!-----bed stress u, ZTAUKSI
      npar = npar + 1
      parcod(npar) = mapind(16)
      parlst(npar) = maplst(parcod(npar))
      partyp(npar) = maptyp(parcod(npar))
      paruni(npar) = mapuni(parcod(npar))
!-----bed stress v, ZTAUETA
      npar = npar + 1
      parcod(npar) = mapind(17)
      parlst(npar) = maplst(parcod(npar))
      partyp(npar) = maptyp(parcod(npar))
      paruni(npar) = mapuni(parcod(npar))
!-----bed stress mag, ZTAUETA
      npar = npar + 1
      parcod(npar) = mapind(18)
      parlst(npar) = maplst(parcod(npar))
      partyp(npar) = maptyp(parcod(npar))
      paruni(npar) = mapuni(parcod(npar))
!-----bed stress dir, ZTAUETA
      npar = npar + 1
      parcod(npar) = mapind(19)
      parlst(npar) = maplst(parcod(npar))
      partyp(npar) = maptyp(parcod(npar))
      paruni(npar) = mapuni(parcod(npar))
   endif

   if (lmax   .gt. 0) then
!--------constituents, GRO(1:lstci), ZTUR(1:ltur)
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
!-----------density, ZRHO
         npar = npar + 1
         parcod(npar) = mapind(31)
         parlst(npar) = maplst(parcod(npar))
         partyp(npar) = maptyp(parcod(npar))
         paruni(npar) = mapuni(parcod(npar))
      endif

      if ( selmap(19:19) .eq. 'Y' ) then
         if (num_layers_grid   .gt. 1) then
!-----------eddy diffusivity, ZDICWW
            npar = npar + 1
            parcod(npar) = mapind(32)
            parlst(npar) = maplst(parcod(npar))
            partyp(npar) = maptyp(parcod(npar))
            paruni(npar) = mapuni(parcod(npar))
         endif
      endif
   endif
!-----the following parameters are not continuous quantities:
!     they require a different approach
!-----active grid
!     npar = npar + 1
!     parcod(npar) = mapind(35)
!     parlst(npar) = maplst(parcod(npar))
!     partyp(npar) = maptyp(parcod(npar))
!     paruni(npar) = mapuni(parcod(npar))
!-----boundary lines
!     npar = npar + 1
!     parcod(npar) = mapind(36)
!     parlst(npar) = maplst(parcod(npar))
!     partyp(npar) = maptyp(parcod(npar))
!     paruni(npar) = mapuni(parcod(npar))
!-----temporary dry points
!     npar = npar + 1
!     parcod(npar) = mapind(37)
!     parlst(npar) = maplst(parcod(npar))
!     partyp(npar) = maptyp(parcod(npar))
!     paruni(npar) = mapuni(parcod(npar))

!     if ( selmap(1:3) .eq. 'YYY' ) then
!        if (num_layers_grid .gt. 1) then
!--------vectors depth average velocity
!           npar = npar + 1
!           parcod(npar) = mapind(38)
!           parlst(npar) = maplst(parcod(npar))
!           partyp(npar) = maptyp(parcod(npar))
!        paruni(npar) = mapuni(parcod(npar))
!--------vectors velocity uv           (not useful in current ODS setup!)
!           npar = npar + 1
!           parcod(npar) = mapind(40)
!           parlst(npar) = maplst(parcod(npar))
!           partyp(npar) = maptyp(parcod(npar))
!           paruni(npar) = mapuni(parcod(npar))
!--------vectors velocity uw           (not yet implemented)
!           npar = npar + 1
!           parcod(npar) = mapind(41)
!           parlst(npar) = maplst(parcod(npar))
!           partyp(npar) = maptyp(parcod(npar))
!           paruni(npar) = mapuni(parcod(npar))
!--------vectors velocity vw           (not yet implemented)
!           npar = npar + 1
!           parcod(npar) = mapind(42)
!           parlst(npar) = maplst(parcod(npar))
!           partyp(npar) = maptyp(parcod(npar))
!           paruni(npar) = mapuni(parcod(npar))
!        endif
!     endif
!-----vectors bed stress
!     if ( selmap(16:17) .eq. 'YY' ) then
!        npar = npar + 1
!        parcod(npar) = mapind(39)
!        parlst(npar) = maplst(parcod(npar))
!        partyp(npar) = maptyp(parcod(npar))
!        paruni(npar) = mapuni(parcod(npar))
!     endif

!-----dpt. at d_points
   npar = npar + 1
   parcod(npar) = mapind(44)
   parlst(npar) = maplst(parcod(npar))
   partyp(npar) = maptyp(parcod(npar))
   paruni(npar) = mapuni(parcod(npar))
!-----dpt. at z_points
   npar = npar + 1
   parcod(npar) = mapind(45)
   parlst(npar) = maplst(parcod(npar))
   partyp(npar) = maptyp(parcod(npar))
   paruni(npar) = mapuni(parcod(npar))
!-----x coordinate at depth location, XCOR
   npar = npar + 1
   parcod(npar) = mapind(46)
   parlst(npar) = maplst(parcod(npar))
   partyp(npar) = maptyp(parcod(npar))
   paruni(npar) = mapuni(parcod(npar))
!-----y coordinate at depth location, YCOR
   npar = npar + 1
   parcod(npar) = mapind(47)
   parlst(npar) = maplst(parcod(npar))
   partyp(npar) = maptyp(parcod(npar))
   paruni(npar) = mapuni(parcod(npar))
!-----x coordinate at zeta location, XZ
   npar = npar + 1
   parcod(npar) = mapind(48)
   parlst(npar) = maplst(parcod(npar))
   partyp(npar) = maptyp(parcod(npar))
   paruni(npar) = mapuni(parcod(npar))
!-----y coordinate at zeta location, YZ
   npar = npar + 1
   parcod(npar) = mapind(49)
   parlst(npar) = maplst(parcod(npar))
   partyp(npar) = maptyp(parcod(npar))
   paruni(npar) = mapuni(parcod(npar))
!-----grid attribute, UVDAMS
   npar = npar + 1
   parcod(npar) = mapind(50)
   parlst(npar) = maplst(parcod(npar))
   partyp(npar) = maptyp(parcod(npar))
   paruni(npar) = mapuni(parcod(npar))
!-----z coordinate, ZCOR step     (not yet implemented)
!     npar = npar + 1
!     parcod(npar) = mapind(51)
!     parlst(npar) = maplst(parcod(npar))
!     partyp(npar) = maptyp(parcod(npar))
!     paruni(npar) = mapuni(parcod(npar))
!-----z coordinate, ZCOR slope    (not yet implemented)
!     npar = npar + 1
!     parcod(npar) = mapind(52)
!     parlst(npar) = maplst(parcod(npar))
!     partyp(npar) = maptyp(parcod(npar))
!     paruni(npar) = mapuni(parcod(npar))
!
   nrlst = npar
!
!-----------------------------------------------------------------------
!-----check required and found number of parameter names
!-----------------------------------------------------------------------
   okee = okee .and. (maxlst .eq. npar)
!--------------------------------------------------------------------
!-----Close trim-<runid>.dat and trim-<runid>.def MAP-files
!--------------------------------------------------------------------
8888 continue
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

subroutine ods_tri_nef_map_dim&
&(&
&fname ,itype ,dimtyp, pardep, timdep, locdep,&
&ndim  ,ierror, option                       )
!-----------------------------------------------------------------------
!         Function: dimension selection for maps
!                   TRISULA NEFIS MAP files
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
! BUFLEN      I*4                  Size in bytes of available buffer
! CELDEF     CH*16                 Cell name definition
! ELMNAM     CH*16                 Element name definition
! OKEE        L*4                  Flag for error reqocnition
! FILHDA     CH*256                File name NEFIS data file for MAP
! FILHDE     CH*256                File name NEFIS definition file for
!                                  MAP
! GRPDEF     CH*16                 Group name definition
! GRPDMS      I*4  5               Array with GRPNDM dimensions
! GRPNDM      I*4                  Number of dimenmsions of tmap group
! GRPORD      I*4  5               Array which gives order in which data
!                                  must be read
! HDAFDS      I*4  999             Data file descriptor for the MAP-DAT
!                                  file
! HDEFDS      I*4  2997            Definition file description for the
!                                  MAP-DEF file
! IERROR      I*4                  Error code for NEFIS error
! num_layers_grid        I*4                  Number of layers
! L           I*4                  Help variable
! LMAX    I   I*4                  Number of constituents
!                                  for old files LMAX = LSTCI
!                                  for new files LMAX = LSTCI + LTUR
! LSTCI   I   I*4                  Total number of constituents (incl.
!                                  turbulence for old trim files).
! LTUR    I   I*4                  Number of turbulence constituents
! NPAR        I*4                  Number of found parameters
! NRCEL       I*4                  Number of cells defined in group 1&3
! SELMAP  I  CH*20                 Output flags containing Y or N for
!                                  various output quantities selection
!                                  for map files
! UINDEX      I*4  3               Array with indices of the cells to
!                                  be read
! USRORD      I*4                  Sequence in which the cells must be
!                                  read
! ZRHO        L*4                  if .true. then density included
!-----------------------------------------------------------------------
!
!  DECLARATIONS
!
   include 'ods.inc'
!
   integer         ierror,itype
   integer         nrcel ,npar  ,lmax  ,num_layers_grid  ,l
   integer         num_columns  ,num_rows
   integer         pardep,timdep,locdep
   integer         lstci ,ltur  ,irho
   integer         ndim   (4    )
!
   character*3     dimtyp
   character*20    selmap,namcon(10)
   character       fname(*)*(*)
   character*256   filhda,filhde
   character*256   option
!
   logical         ex    ,okee  ,zrho  ,chk
!-----------------------------------------------------------------------
!-----declaration NEFIS
!-----------------------------------------------------------------------
   integer       hdefds( 2997),hdafds(  999)
!
   integer       grpndm,grpdms(    5),grpord(    5),&
   &uindex(    3),usrord,buflen       ,&
   &ind
!
   character*16  grpdef,celnam,elmnam
!
   integer       INQGRP,GETELT,GETELS
   integer       INQMXI

!-----------------------------------------------------------------------
!-----Initialisation
!-----------------------------------------------------------------------
   okee   = .true.
   zrho   = .false.
!
   ierror =  0
!
!--------------------------------------------------------------------
!-----Test if trim-<runid>.dat and trim-<runid>.def Nefis MAP-files
!     exist
!--------------------------------------------------------------------

!     ind = index ( fname(1), char(0))
!     if ( ind .eq. 0 ) then
!        filhda = fname(1)
!     else
!        filhda = fname(1)(1:ind-1)
!     endif
!     inquire (file=filhda,exist=ex)
!     if (.not.ex) then
!        ierror = IENOFI
!        return
!     endif
!
!     ind = index ( fname(2), char(0))
!     if ( ind .eq. 0 ) then
!        filhde = fname(2)
!     else
!        filhde = fname(2)(1:ind-1)
!     endif
!     inquire (file=filhde,exist=ex)
!     if (.not.ex) then
!        ierror = IENOFI
!        return
!     endif
   call ods_check_nefis( fname  , '.def' , ierror )
   if ( ierror .ne. ieok   ) then
      return
   endif
!--------------------------------------------------------------------
!-----Open trim-<runid>.dat and trim-<runid>.def MAP-files
!--------------------------------------------------------------------
   call OPNNEF(fname, itype, hdafds, hdefds, ierror)
   if (ierror .ne. 0) then
      ierror = IEFIRO
      return
   endif
!--------------------------------------------------------------------
!-----Read array-dimension nrcel from Nefis MAP-files group 1
!--------------------------------------------------------------------
   grpdef = 'map-info-series'
   grpndm = 5
   celnam = grpdef
   ierror = INQGRP(hdefds   ,grpdef    ,celnam    ,grpndm    ,&
   &grpdms   ,grpord                          )
   nrcel  = grpdms(1)
!--------------------------------------------------------------------
!-----Test value of nrcel if nrcel = 0 then get nrcel with INQMXI
!--------------------------------------------------------------------
   if (nrcel  .eq. 0) then
      ierror    = INQMXI(hdefds,grpdef    ,nrcel     )
      okee = okee .and. (ierror .eq. 0)
      if (nrcel  .eq. 0) then
         okee = .false.
         goto 8888
      endif
   endif
!--------------------------------------------------------------------
!-----Read array-dimensions from Nefis MAP-files group 2
!--------------------------------------------------------------------
   grpdef = 'map-const'
!
   uindex(1) = 1
   uindex(2) = 1
   uindex(3) = 1
   usrord    = 1
   buflen    = 4
!
   npar      = 0
   num_columns      = 0
   num_rows      = 0
   lmax      = 0
   lstci     = 0
   ltur      = 0
   irho      = 0
   num_layers_grid      = 0
!
   elmnam    = 'num_columns'
   okee = okee .and.&
   &GETELT(hdefds,grpdef    ,elmnam    ,&
   &uindex,usrord    ,buflen    ,num_columns      )&
   &.eq. 0
!
   elmnam    = 'num_rows'
   okee = okee .and.&
   &GETELT(hdefds,grpdef    ,elmnam    ,&
   &uindex,usrord    ,buflen    ,num_rows      )&
   &.eq. 0
!
!--------------------------------------------------------------------
!-----Read element LMAX; LMAX only defined in old trim files
!     hence if not defined ierror = -25041
!--------------------------------------------------------------------
   elmnam    = 'LMAX'
   ierror    = GETELT(hdefds,grpdef    ,elmnam    ,&
   &uindex,usrord    ,buflen    ,LSTCI     )
   if (ierror .ne. 0) then
!--------------------------------------------------------------------
!--------In case of a new trih file read LSTCI and LTUR
!--------------------------------------------------------------------
      elmnam = 'LSTCI'
      ierror = GETELT(hdefds,grpdef    ,elmnam    ,&
      &uindex,usrord    ,buflen    ,LSTCI     )
      if (ierror .ne. 0) then
         okee = .false.
         goto 8888
      endif
!
      elmnam = 'LTUR'
      ierror = GETELT(hdefds,grpdef    ,elmnam    ,&
      &uindex,usrord    ,buflen    ,LTUR      )
      if (ierror .ne. 0) then
         okee = .false.
         goto 8888
      endif
   endif
!
   lmax = lstci + ltur
!
   elmnam = 'num_layers_grid'
   ierror = GETELT(hdefds,grpdef    ,elmnam    ,&
   &uindex,usrord    ,buflen    ,num_layers_grid      )
   if (ierror .ne. 0) then
      okee = .false.
      goto 8888
   endif
!
!-----------------------------------------------------------------------
!-----Element SELMAP selection of output
!-----------------------------------------------------------------------
   buflen    = 20
   elmnam    = 'SELMAP'
   ierror    = GETELS&
   &(hdefds   ,grpdef    ,elmnam    ,&
   &uindex   ,usrord    ,buflen    ,SELMAP    )
   if (ierror .ne. 0) then
!-----------------------------------------------------------------------
!--------In case of a old trim file no SELMAP then
!           re-define SELMAP like definition in subroutine RDPRFL
!-----------------------------------------------------------------------
      selmap = 'YYYYYYYYYYYYYYYYYYYY'
      if (num_layers_grid   .eq. 1) selmap( 4: 5) = 'NN'
      if (lstci  .eq. 0) selmap( 6:13) = 'NNNNNNNN'
      if (ltur   .eq. 0) selmap(14:15) = 'NN'
      if (num_layers_grid   .eq. 1) selmap(18:19) = 'NN'
      if (lmax   .eq. 0) selmap(19:19) = 'N'
      selmap(20:20) = 'X'
   endif
!-----------------------------------------------------------------------
!-----Element NAMCON constituents and turbulence quantity names
!     only if selmap( 6:15) <> 'NNNNNNNNNN'
!-----------------------------------------------------------------------
   if (index (selmap( 6:15),'Y') .gt. 0) then
      buflen    = 20 * lmax
      elmnam    = 'NAMCON'
      ierror    = GETELS&
      &(hdefds,grpdef    ,elmnam    ,&
      &uindex,usrord    ,buflen    ,NAMCON    )
      if (ierror .ne. 0) then
         okee = .false.
         goto 8888
      endif
!
      do 110 l = 1,lmax
         if ( selmap(5+l:5+l) .eq. 'Y' ) then
            if (namcon(l)(:11) .eq. 'Salinity   ') irho   = 1
            if (namcon(l)(:11) .eq. 'Temperature') irho   = 1
         endif
110   continue
   endif
!-----------------------------------------------------------------------
!-----In case of a old trim file re-define SELMAP(20:20)
!-----------------------------------------------------------------------
   if (selmap(20:20) .eq. 'X') then
      selmap(20:20) = 'N'
      if (irho   .eq. 1) selmap(20:20) = 'Y'
   endif
!
   zrho = irho .eq. 1
!
!--------------------------------------------------------------------
!-----Generate parameternames from Nefis MAP-files
!--------------------------------------------------------------------
   if ( selmap(1:1) .eq. 'Y' ) then
!-----water level, ZWL                               pardep =  1
      npar = npar + 1
!-----water depth, ZWL + DPS                         pardep =  2
      npar = npar + 1
   endif
   if ( selmap(2:3) .eq. 'YY' ) then
      if (num_layers_grid .gt. 1) then
!--------dpt. aver. cur. u                           pardep =  3
         npar = npar + 1
!--------dpt. aver. cur. v                           pardep =  4
         npar = npar + 1
!--------dpt. aver. cur. mag.                        pardep =  5
         npar = npar + 1
!--------dpt. aver. cur. dir.                        pardep =  6
         npar = npar + 1
!--------vectors dpt. aver.                          pardep = 38
!           npar = npar + 1
!--------vectors velocity uv (layer)(not useful)     pardep = 40
!           npar = npar + 1
!--------vectors velocity uw (layer)(not yet implemented)ep = 41
!           npar = npar + 1
!--------vectors velocity vw (layer)(not yet implemented)ep = 42
!           npar = npar + 1
      endif

!-----current u (layer)                              pardep =  7
      npar = npar + 1
!-----current v (layer)                              pardep =  8
      npar = npar + 1
!-----current mag. (layer)                           pardep =  9
      npar = npar + 1
!-----current dir. (layer)                           pardep = 10
      npar = npar + 1
   endif

   if ( selmap(5:5) .eq. 'Y' ) then
      if (num_layers_grid .gt. 1) then
!--------current w.   (layer)                        pardep = 11
         npar = npar + 1
      endif
   endif
!--------z-coordinate                                pardep = 12
!     if (num_layers_grid .gt. 1) then
   npar = npar + 1
!     endif

!-----flow rate u, ZQXK   (not yet implemented)      pardep = 13
!     npar = npar + 1
!-----flow rate v, ZQYK   (not yet implemented)      pardep = 14
!     npar = npar + 1

   if ( selmap(18:18) .eq. 'Y' ) then
      if (num_layers_grid .gt. 1) then
!--------eddy viscosity, ZVICWW  (layer)             pardep = 15
         npar = npar + 1
      endif
   endif

   if ( selmap(16:17) .eq. 'YY' ) then
!-----bed stress u, ZTAUKSI                          pardep = 16
      npar = npar + 1
!-----bed stress v, ZTAUETA                          pardep = 17
      npar = npar + 1
!-----bed stress mag, ZTAUETA                        pardep = 18
      npar = npar + 1
!-----bed stress dir, ZTAUETA                        pardep = 19
      npar = npar + 1
   endif

   if (lmax   .gt. 0) then
!--------constituents, GRO (1:lstci) (layer)         pardep = 20..29
!        constituents, ZTUR(1:ltur ) (layer)         pardep = 20..29
      do 10 l = 1, lmax, 1
         if ( selmap(5+l:5+l) .eq. 'Y' ) then
            npar = npar + 1
         endif
10    continue

      if (zrho) then
!-----------density, ZRHO  (layer)                   pardep = 31
         npar = npar + 1
      endif

      if ( selmap(19:19) .eq. 'Y' ) then
         if (num_layers_grid   .gt. 1) then
!-----------eddy diffusivity, ZDICWW  (layer)        pardep = 32
            npar = npar + 1
         endif
      endif
   endif

!-----the following parameters are not continuous quantities:
!     they require a different approach
!-----active grid                                    pardep = 35
!     npar = npar + 1
!-----boundary lines                                 pardep = 36
!     npar = npar + 1
!-----temporary dry points                           pardep = 37
!     npar = npar + 1
!-----vectors bed stress                             pardep = 39
!     npar = npar + 1

!-----dpt. at d_points                               pardep = 44
   npar = npar + 1
!-----dpt. at z_points                               pardep = 45
   npar = npar + 1
!-----x coordinate at depth location, XCOR           pardep = 46
   npar = npar + 1
!-----y coordinate at depth location, YCOR           pardep = 47
   npar = npar + 1
!-----x coordinate at zeta location, XZ              pardep = 48
   npar = npar + 1
!-----y coordinate at zeta location, YZ              pardep = 49
   npar = npar + 1
!-----grid attribute, UVDAMS                         pardep = 50
   npar = npar + 1
!-----z coordinate, ZCOR step     (not yet implemented)rdep = 51
!     npar = npar + 1
!-----z coordinate, ZCOR slope    (not yet implemented)rdep = 52
!     npar = npar + 1

!-----------------------------------------------------------------------
!-----return required dimension
!-----------------------------------------------------------------------
   if (dimtyp(1:3) .eq. 'par') then
      ndim (1) = 1
      ndim (2) = npar
   else if (dimtyp(1:3) .eq. 'tim') then
      ndim (1) = 1
      ndim (2) = nrcel
   else if (dimtyp(1:3) .eq. 'loc') then
      if ( pardep .ge. 38 .and. pardep .le. 39 ) then
!-----------vectors (u, v componen) with one layer
         ndim (1) = 3
         ndim (2) = num_columns
         ndim (3) = num_rows
         ndim (4) = 2
      else if ( pardep .ge. 40 .and. pardep .le. 42 ) then
!-----------vectors (u, v componen) with possibly more than one layer
         if ( num_layers_grid .gt. 1 ) then
            ndim (1) = 3
            ndim (2) = num_columns
            ndim (3) = num_rows
            ndim (4) = num_layers_grid * 2
         else
            ndim (1) = 3
            ndim (2) = num_columns
            ndim (3) = num_rows
            ndim (4) = 2
         endif
      endif
      chk = ( pardep .ge.  1 .and. pardep .le.  6 ) .or.&
      &( pardep .ge. 13 .and. pardep .le. 14 ) .or.&
      &( pardep .ge. 16 .and. pardep .le. 19 ) .or.&
      &( pardep .ge. 35 .and. pardep .le. 37 ) .or.&
      &( pardep .ge. 44 .and. pardep .le. 50 )
!--------chk = .true.  then 2 dimensional (1-6,13-14,16-19,35-37,44-50)
!              .false. then poss. 3 dimensional (7-11,12,15,20-29,31,32)
      if ( chk ) then
         ndim (1) = 2
         ndim (2) = num_columns
         ndim (3) = num_rows
      else
         if ( pardep .eq. 15 .or. pardep .eq. 32 ) then
!--------------ZVICWW and ZDICWW with layers (0:num_layers_grid)
            ndim (1) = 3
            ndim (2) = num_columns
            ndim (3) = num_rows
            ndim (4) = num_layers_grid + 1
         else if ( pardep .eq. 12 ) then
!--------------z-coordinate
            ndim (1) = 3
            ndim (2) = num_columns
            ndim (3) = num_rows
            ndim (4) = num_layers_grid + 1
         else if ( pardep .ge. 20 .and. pardep .le. 29 ) then
!--------------constituents or turbulence v245 or later
            if ( pardep-19 .gt. lstci ) then
!-----------------turbulence with layers (0:num_layers_grid)
               ndim (1) = 3
               ndim (2) = num_columns
               ndim (3) = num_rows
               ndim (4) = num_layers_grid + 1
            else
!-----------------constituents or turbulence with layers (1:num_layers_grid)
!                 v240 or before
               if ( num_layers_grid .gt. 1 ) then
                  ndim (1) = 3
                  ndim (2) = num_columns
                  ndim (3) = num_rows
                  ndim (4) = num_layers_grid
               else
                  ndim (1) = 2
                  ndim (2) = num_columns
                  ndim (3) = num_rows
               endif
            endif
         else
            if ( num_layers_grid .gt. 1 ) then
               ndim (1) = 3
               ndim (2) = num_columns
               ndim (3) = num_rows
               ndim (4) = num_layers_grid
            else
               ndim (1) = 2
               ndim (2) = num_columns
               ndim (3) = num_rows
            endif
         endif
      endif
   else
      okee = .false.
   endif
!--------------------------------------------------------------------
!-----Close trim-<runid>.dat and trim-<runid>.def MAP-files
!--------------------------------------------------------------------
8888 continue
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

subroutine ods_tri_nef_map_tme&
&(&
&fname  ,itype  ,timdef, maxdef ,pardep , locdep,&
&maxlst ,        timlst,         timtyp ,&
&nrlst  ,ierror ,option                         )
!-----------------------------------------------------------------------
!           Function: time selection for maps
!                     TRISULA NEFIS files
!        Method used:
!-----------------------------------------------------------------------
!   Calling routine :              GETTME
!-----------------------------------------------------------------------
!   Called  routines:              OPNNEF
!                                  CLOSFL
!                                  GETELT (nefis)
!                                  INQGRP (nefis)
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
! FILHDA     CH*256                File name NEFIS data file for MAP
! FILHDE     CH*256                File name NEFIS definition file for
!                                  MAP
! GRPDEF     CH*16                 Group name definition
! GRPDMS      I*4  5               Array with GRPNDM dimensions
! GRPNDM      I*4                  Number of dimenmsions of this group
! GRPORD      I*4  5               Array which gives order in which data
!                                  must be read
! HDAFDS      I*4  999             Data file descriptor for the MAP-DAT
!                                  file
! HDEFDS      I*4  2997            Definition file description for the
!                                  MAP-DEF file
! I           I*4                  Help var.
! IDATE       I*4                  integer date yyyy*10000+mm*100+dd
! IDAY        I*4                  Day part of ITDATE (dd)
! IDP         I*4                  Help var. date part julian notation
! IERROR      I*4                  Error code for NEFIS error
! IHULP       I*4  2               Help array.
! IMO         I*4                  Month part of ITDATE (mm)
! IMO1        I*4                  Help var.
! ITDATE      I*4                  Initial simulation start date
! ITIME       I*4                  integer time hh*10000+mm*100+ss
! ITMODC      I*4                  Help var. for time step number
! ITP         I*4                  Help var. time part julian notation
! IY          I*4                  Year part of ITDATE (yyyy)
! JULDAY      I*4                  julian day number of ITDATE
! num_layers_grid        I*4                  Number of layers
! L           I*4                  Help var.
! LMAX        I*4                  Number of constituents
! M           I*4                  Help var.
! N           I*4                  Help var.
! NHULP       I*4                  Number of cells in case error in
!                                  file
! NRCEL       I*4                  Number of cells defined in group 1&3
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
   include         'ods.inc'
!
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
!
   double precision timlst(maxlst)
   double precision timdef(maxdef,2)
   real             dt    ,tunit
!
   character       fname(*)*(*)
   character*256   filhda,filhde
   character*256   option
!
   logical         ex    ,okee
!-----------------------------------------------------------------------
!-----declaration NEFIS
!-----------------------------------------------------------------------
   integer       hdefds( 2997),hdafds(  999)
!
   integer       grpndm,grpdms(    5),grpord(    5),&
   &uindex(    3),usrord,buflen
!
   character*16  grpdef,elmnam,celnam
!
   integer       INQGRP,GETELT,GETELS
   integer       INQMXI

!-----------------------------------------------------------------------
!-----Initialisation
!-----------------------------------------------------------------------
   okee   = .true.
!
   ierror =  0
!
!--------------------------------------------------------------------
!-----Test if trim-<runid>.dat and trim-<runid>.def Nefis MAP-files
!     exist
!--------------------------------------------------------------------
   call ods_check_nefis( fname  , '.def' , ierror )
   if ( ierror .ne. ieok   ) then
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
!-----Open trim-<runid>.dat and trim-<runid>.def MAP-files
!--------------------------------------------------------------------
   call OPNNEF(fname, itype, hdafds, hdefds, ierror )
   if (ierror .ne. 0) then
      ierror = IEFIRO
      return
   endif
!--------------------------------------------------------------------
!-----Read array-dimension nrcel from Nefis MAP-files group 1
!--------------------------------------------------------------------
   grpdef = 'map-info-series'
   grpndm = 5
   celnam = grpdef
   ierror = INQGRP(hdefds   ,grpdef    ,celnam    ,grpndm    ,&
   &grpdms   ,grpord                          )
   nrcel  = grpdms(1)
!--------------------------------------------------------------------
!-----Test value of nrcel if nrcel = 0 then get nrcel with INQMXI
!--------------------------------------------------------------------
   if (nrcel  .eq. 0) then
      ierror    = INQMXI(hdefds,grpdef    ,nrcel     )
      okee = okee .and. (ierror .eq. 0)
      if (nrcel  .eq. 0) then
         okee   = .false.
         goto 8888
      endif
   endif
!--------------------------------------------------------------------
!-----Read constants from Nefis MAP-files group 2
!--------------------------------------------------------------------
   grpdef    = 'map-const'
   uindex(1) = 1
   uindex(2) = 1
   uindex(3) = 1
   usrord    = 1
!
   buflen    = 2 * 4
   elmnam    = 'ITDATE'
   okee = okee .and.&
   &GETELT(hdefds   ,grpdef    ,elmnam    ,&
   &uindex   ,usrord    ,buflen    ,IHULP     )&
   &.eq. 0
   itdate    = ihulp (1)
!
   buflen    = 4
   elmnam    = 'TUNIT'
   okee = okee .and.&
   &GETELT(hdefds   ,grpdef    ,elmnam    ,&
   &uindex   ,usrord    ,buflen    ,TUNIT     )&
   &.eq. 0
!
   buflen    = 4
   elmnam    = 'DT'
   okee = okee .and.&
   &GETELT(hdefds   ,grpdef    ,elmnam    ,&
   &uindex   ,usrord    ,buflen    ,DT        )&
   &.eq. 0

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
!-----Initialize Nefis variables for group 1
!-----------------------------------------------------------------------
   grpdef    = 'map-info-series'
   elmnam    = 'ITMAPC'
!
   uindex(1) = 1
   uindex(2) = 1
   uindex(3) = 1
   usrord    = 1
   buflen    = 4
!-----------------------------------------------------------------------
!-----Define number of rows default nrcel, if an error
!     occures (ierror <> 0) then re-define
!-----------------------------------------------------------------------
   nhulp  = nrcel
   do 100 i=1,nrcel
!-----------------------------------------------------------------------
!--------Read from group 1 ITMODC
!-----------------------------------------------------------------------
      uindex(1) = i
      uindex(2) = i
      uindex(3) = 1
!
      ierror    = GETELT(hdefds,grpdef    ,elmnam    ,&
      &uindex,usrord    ,buflen    ,ITMODC    )
      if (ierror .ne. 0 .or. itmodc .eq. -1) then
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
!--------Define integer values for time and date
!--------------------------------------------------------------------------
      itime  = ihou   * 10000 + imin   * 100 + isec
      idate  = iy     * 10000 + imo    * 100 + iday
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
   nrlst = nhulp
!
!-----------------------------------------------------------------------
!-----check required and found number of parameter names
!-----------------------------------------------------------------------
   okee = okee .and. (maxlst .le. nrcel)
!--------------------------------------------------------------------
!-----Close trim-<runid>.dat and trim-<runid>.def MAP-files
!--------------------------------------------------------------------
8888 continue
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

subroutine julind_map&
&(hdefds, hdafds, tim, nindex, ierror)
!-----------------------------------------------------------------------
!           Function: transform julian day to index in time series
!        Method used:
!
!-----------------------------------------------------------------------
!   Calling routine :              ODS_TRI_NEF_MAP_MAT
!-----------------------------------------------------------------------
!   Called  routines:              GETELT (nefis)
!                                  INQGRP (nefis)
!                                  INQMXI (nefis)
!-----------------------------------------------------------------------
!    Parameters:
!    -----------
!
!   Var.      Type Dimensions I/O  description
!   --------------------------------------------------------------------
!
! HDAFDS      I*4  999        I    Data file descriptor for the MAP-DAT
!                                  file
! HDEFDS      I*4  2997       I    Definition file description for the
!                                  MAP-DEF file
! TIM         r*8  3          I    list with julian dates, begin, end, inc
! NINDEX      I*4  3          O    indices of time frame in Nefis file
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
! CELNAM     CH*16                 Cell name definition
! DT          R*4                  Time step in TUNIT seconds
! ELMNAM     CH*16                 Element name definition
! FOUT        L*4                  Flag for further execution program
!                                  fout  = .true.  : stop execution
!                                  fout  = .false. : go on
! GRPDEF     CH*16                 Group name definition
! GRPDMS      I*4  5               Array with GRPNDM dimensions
! GRPNDM      I*4                  Number of dimenmsions of tmap group
! GRPORD      I*4  5               Array which gives order in which data
!                                  must be read
! I           I*4                  Help var.
! IDAY        I*4                  Day part of ITDATE (dd)
! ICURTM      I*4                  Current timeref. to ITDATE
! IDP         I*4                  Help var. date part julian notation
! IERROR      I*4                  Error code for NEFIS error
! IHOU        I*4                  Help var. current step hours
! IHULP       I*4  2               Help array.
! IMO         I*4                  Month part of ITDATE (mm)
! IMO1        I*4                  Help var.
! IMIN        I*4                  Help var. current step minutes
! ISEC        I*4                  Help var. current step seconds
! ITDATE      I*4                  Initial simulation start date
! ITMODC      I*4                  Help var. for time step number
! ITP         I*4                  Help var. time part julian notation
! IY          I*4                  Year part of ITDATE (yyyy)
! JULDAY      I*4                  julian day number of ITDATE
! L           I*4                  Help var.
! LMAX        I*4                  Number of constituents
! N           I*4                  Help var.
! NRCEL       I*4                  Number of cells defined in group 1&3
! NRII        I*4                  Number of index intervals
! NRTI        I*4                  Number of time intervals
! TIMSTP      R*8                  Current julian day for tmap step
! TUNIT       R*4                  Scale unit to define seconds
! UINDEX      I*4  3               Array with indices of the cells to
!                                  be read
! USRORD      I*4                  Sequence in which the cells must be
!                                  read
!-----------------------------------------------------------------------
!
!  DECLARATIONS
!
   include         'ods.inc'
!
   integer         ihulp(2)
   integer         itmodc,itdate
   integer         ierror,nrcel
   integer         julday
   integer         icurtm,l     ,n     ,imo1  ,idp   ,itp
   integer         ihou  ,imin  ,isec
   integer         iy    ,imo   ,iday
   integer         nindex(3)
   integer         nrti  ,nrii  ,i
!
   real             dt    ,tunit
   double precision tim(3)
   double precision timstp
   double precision epstim
!
   logical         fout
!-----------------------------------------------------------------------
!-----declaration NEFIS
!-----------------------------------------------------------------------
   integer       hdefds( 2997),hdafds(  999)
   integer       grpndm,grpdms(    5),grpord(    5),&
   &uindex(    3),usrord,buflen
   character*16  grpdef,elmnam,celnam
   integer       INQGRP,GETELT,GETELS,INQMXI

!-----------------------------------------------------------------------
!-----Initialisation
!-----------------------------------------------------------------------
   fout      = .false.
   ierror    =  0
   nindex(1) =  1
   nindex(2) =  1
   nindex(3) =  1
!
!--------------------------------------------------------------------
!-----Read array-dimension nrcel from Nefis MAP-files group 1
!--------------------------------------------------------------------
   grpdef = 'map-info-series'
   grpndm = 5
   celnam = grpdef
   ierror = INQGRP(hdefds   ,grpdef    ,celnam    ,grpndm    ,&
   &grpdms   ,grpord                          )
   nrcel  = grpdms(1)
!--------------------------------------------------------------------
!-----Test value of nrcel if nrcel = 0 then get nrcel with INQMXI
!--------------------------------------------------------------------
   if (nrcel  .eq. 0) then
      ierror    = INQMXI(hdefds,grpdef    ,nrcel     )
      if (ierror .ne. 0) then
         fout  = .true.
         goto 8888
      endif
   endif
!--------------------------------------------------------------------
!-----Read constants from Nefis MAP-files group 2
!--------------------------------------------------------------------
   grpdef    = 'map-const'
   uindex(1) = 1
   uindex(2) = 1
   uindex(3) = 1
   usrord    = 1
!
   buflen    = 2 * 4
   elmnam    = 'ITDATE'
   ierror    = GETELT(hdefds   ,grpdef    ,elmnam    ,&
   &uindex   ,usrord    ,buflen    ,IHULP     )
   if (ierror .ne. 0) then
      fout   = .true.
      goto 8888
   endif
   itdate    = ihulp (1)
!
   buflen    = 4
   elmnam    = 'TUNIT'
   ierror    = GETELT(hdefds   ,grpdef    ,elmnam    ,&
   &uindex   ,usrord    ,buflen    ,TUNIT     )
   if (ierror .ne. 0) then
      fout   = .true.
      goto 8888
   endif
!
   buflen    = 4
   elmnam    = 'DT'
   ierror    = GETELT(hdefds   ,grpdef    ,elmnam    ,&
   &uindex   ,usrord    ,buflen    ,DT        )
   if (ierror .ne. 0) then
      fout   = .true.
      goto 8888
   endif
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
!-----Initialize Nefis variables for group 1
!-----------------------------------------------------------------------
   grpdef    = 'map-info-series'
   elmnam    = 'ITMAPC'
!
   uindex(1) = 1
   uindex(2) = 1
   uindex(3) = 1
   usrord    = 1
   buflen    = 4
!-----------------------------------------------------------------------
!-----Define number of rows default nrcel, if an error
!     occures (ierror <> 0) then re-define
!     Note:
!     It is necessary to use a small margin (1/100th of a second),
!     because otherwise the animation procedure may nd up using the
!     wrong data (all because of the time stepping with double
!     precision reals)
!-----------------------------------------------------------------------

   epstim = 1.0d0 / (86400.0d0 * 100.0d0)

   do 100 i=1,nrcel
!-----------------------------------------------------------------------
!--------Read from group 1 ITMODC
!-----------------------------------------------------------------------
      uindex(1) = i
      uindex(2) = i
      uindex(3) = 1
!
      ierror    = GETELT(hdefds,grpdef    ,elmnam    ,&
      &uindex,usrord    ,buflen    ,ITMODC    )
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
      timstp    = dble(idp) + dble(itp) / 86400d0
!-----------------------------------------------------------------------
!--------find the begin and end index
!-----------------------------------------------------------------------
      if ( timstp .le. (tim(1)+epstim) ) then
         nindex(1) = i
      else
         if ( timstp .le. (tim(2)+epstim) ) then
            nindex(2) = i
         endif
      endif
100 continue
!-----------------------------------------------------------------------
!-----find the increment
!-----------------------------------------------------------------------
   nrti      = int((tim(2) - tim(1) + 0.1 * tim(3)) / tim(3))
   nrii      = nindex(2) - nindex(1)
   nindex(3) = max(1,nrii) / max(1,nrti)
!-----------------------------------------------------------------------
!-----return status to calling routine
!-----------------------------------------------------------------------
8888 continue
   ierror = IEOK
   if (fout  ) then
      ierror = IEOTHR
   endif
!
   return
!-----------------------------------------------------------------------
end

subroutine ods_tri_nef_map_loc&
&(&
&fname  ,itype  ,locdef ,maxdef ,pardep ,timdep ,&
&maxlst ,        loclst ,        loctyp ,nrlst  ,&
&locnr  ,ierror ,zbuffs ,option                 )
!-----------------------------------------------------------------------
!           Function: parameter location selection for maps
!                     TRISULA NEFIS files
!        Method used:
!
!-----------------------------------------------------------------------
!   Calling routine :              GETLOC
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
! ZBUFFS     CH*21  maxlst    I/O  workspace names of locations
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
! BUFLEN      I*4                  Size in bytes of available buffer
! CELDEF     CH*16                 Cell name definition
! ELMNAM     CH*16                 Element name definition
! FILHDA     CH*256                File name NEFIS data file for MAP
! FILHDE     CH*256                File name NEFIS definition file for
!                                  MAP
! GRPDEF     CH*16                 Group name definition
!                                  must be read
! HDAFDS      I*4  999             Data file descriptor for the MAP-DAT
!                                  file
! HDEFDS      I*4  2997            Definition file description for the
!                                  MAP-DEF file
! IERROR      I*4                  Error code for NEFIS error
! LMAX    I   I*4                  Number of constituents
!                                  for old files LMAX = LSTCI
!                                  for new files LMAX = LSTCI + LTUR
! LNAME      CH*20                 Help var. location name
! LSTCI   I   I*4                  Total number of constituents (incl.
!                                  turbulence for old trim files).
! LTUR    I   I*4                  Number of turbulence constituents
! NLOC        I*4                  Number of found locations
! NOSTAT      I*4                  Number of defined stations
! NTRUV       I*4                  Number of cross-secties in u- and
!                                  v-direction
! OKEE        L*4                  Flag for error checking
! SELMAP  I  CH*20                 Output flags containing Y or N for
!                                  various output quantities selection
!                                  for map files
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
   integer         ierror,nrlst ,ind   ,l
   integer         lstci ,ltur  ,irho
   integer         num_columns  ,num_rows  ,lmax  ,num_layers_grid ,npar
   integer         maxlst,itype
   integer         maxdef
   integer         pardep
   integer         timdep
   integer         loctyp(maxlst)
   integer         locnr (maxlst)
!
   character       locdef(*)*(*)
   character       loclst(*)*(*)
   character*20    selmap,namcon(10)
   character*20    zbuffs(maxlst)
   character       fname(*)*(*)
   character*256   filhda,filhde
   character*256   option
!
   logical         ex    ,okee  ,check ,zrho
!-----------------------------------------------------------------------
!-----declaration NEFIS
!-----------------------------------------------------------------------
   integer       hdefds( 2997),hdafds(  999)
!
   integer       uindex(    3),usrord,buflen
!
   character*16  grpdef,elmnam
!
   integer       GETELT,GETELS
!-----------------------------------------------------------------------
!-----Initialisation
!-----------------------------------------------------------------------
   okee   = .true.
   zrho   = .false.
   ierror =  0
!--------------------------------------------------------------------
!-----Test if trim-<runid>.dat and trim-<runid>.def Nefis MAP-files
!     exist
!--------------------------------------------------------------------
   call ods_check_nefis( fname  , '.def' , ierror )
   if ( ierror .ne. ieok   ) then
      return
   endif
!--------------------------------------------------------------------
!-----Open trim-<runid>.dat and trim-<runid>.def MAP-files
!--------------------------------------------------------------------
   call OPNNEF(fname, itype, hdafds, hdefds, ierror )
   if (ierror .ne. 0) then
      ierror = IEFIRO
      return
   endif
!--------------------------------------------------------------------
!-----Read array-dimensions from Nefis MAP-files group 2
!--------------------------------------------------------------------
   grpdef = 'map-const'
!
   uindex(1) = 1
   uindex(2) = 1
   uindex(3) = 1
   usrord    = 1
   buflen    = 4
!
   npar      = 0
   num_columns      = 0
   num_rows      = 0
   lmax      = 0
   lstci     = 0
   ltur      = 0
   irho      = 0
   num_layers_grid      = 0
!
   elmnam    = 'num_columns'
   okee = okee .and.&
   &GETELT(hdefds,grpdef    ,elmnam    ,&
   &uindex,usrord    ,buflen    ,num_columns      )&
   &.eq. 0
!
   elmnam    = 'num_rows'
   okee = okee .and.&
   &GETELT(hdefds,grpdef    ,elmnam    ,&
   &uindex,usrord    ,buflen    ,num_rows      )&
   &.eq. 0
!
!--------------------------------------------------------------------
!-----Read element LMAX; LMAX only defined in old trim files
!     hence if not defined ierror = -25041
!--------------------------------------------------------------------
   elmnam    = 'LMAX'
   ierror    = GETELT(hdefds,grpdef    ,elmnam    ,&
   &uindex,usrord    ,buflen    ,LSTCI     )
   if (ierror .ne. 0) then
!--------------------------------------------------------------------
!--------In case of a new trih file read LSTCI and LTUR
!--------------------------------------------------------------------
      elmnam = 'LSTCI'
      ierror = GETELT(hdefds,grpdef    ,elmnam    ,&
      &uindex,usrord    ,buflen    ,LSTCI     )
      if (ierror .ne. 0) then
         okee = .false.
         goto 8888
      endif

      elmnam = 'LTUR'
      ierror = GETELT(hdefds,grpdef    ,elmnam    ,&
      &uindex,usrord    ,buflen    ,LTUR      )
      if (ierror .ne. 0) then
         okee = .false.
         goto 8888
      endif
   endif
!
   lmax = lstci + ltur
!
   elmnam = 'num_layers_grid'
   ierror = GETELT(hdefds,grpdef    ,elmnam    ,&
   &uindex,usrord    ,buflen    ,num_layers_grid      )
   if (ierror .ne. 0) then
      okee = .false.
      goto 8888
   endif
!
!-----------------------------------------------------------------------
!-----Element SELMAP selection of output
!-----------------------------------------------------------------------
   buflen    = 20
   elmnam    = 'SELMAP'
   ierror    = GETELS&
   &(hdefds   ,grpdef    ,elmnam    ,&
   &uindex   ,usrord    ,buflen    ,SELMAP    )
   if (ierror .ne. 0) then
!-----------------------------------------------------------------------
!--------In case of a old trim file no SELMAP then
!           re-define SELMAP like definition in subroutine RDPRFL
!-----------------------------------------------------------------------
      selmap = 'YYYYYYYYYYYYYYYYYYYY'
      if (num_layers_grid   .eq. 1) selmap( 4: 5) = 'NN'
      if (lstci  .eq. 0) selmap( 6:13) = 'NNNNNNNN'
      if (ltur   .eq. 0) selmap(14:15) = 'NN'
      if (num_layers_grid   .eq. 1) selmap(18:19) = 'NN'
      if (lmax   .eq. 0) selmap(19:19) = 'N'
      selmap(20:20) = 'X'
   endif
!-----------------------------------------------------------------------
!-----Element NAMCON constituents and turbulence quantity names
!     only if selmap( 6:15) <> 'NNNNNNNNNN'
!-----------------------------------------------------------------------
   if (index (selmap( 6:15),'Y') .gt. 0) then
      buflen    = 20 * lmax
      elmnam    = 'NAMCON'
      ierror    = GETELS&
      &(hdefds,grpdef    ,elmnam    ,&
      &uindex,usrord    ,buflen    ,NAMCON    )
      if (ierror .ne. 0) then
         okee = .false.
         goto 8888
      endif
!
      do 110 l = 1,lmax
         if (namcon(l)(:11) .eq. 'Salinity   ') irho   = 1
         if (namcon(l)(:11) .eq. 'Temperature') irho   = 1
110   continue
   endif
!-----------------------------------------------------------------------
!-----In case of a old trim file re-define SELMAP(20:20)
!-----------------------------------------------------------------------
   if (selmap(20:20) .eq. 'X') then
      selmap(20:20) = 'N'
      if (irho   .eq. 1) selmap(20:20) = 'Y'
   endif
!
   zrho = irho .eq. 1
!--------------------------------------------------------------------
!-----Generate location names from Nefis MAP-files
!     select possibe parametercode values
!--------------------------------------------------------------------
   check    = pardep .ge.  1 .and. pardep .le. 29 .or.&
   &pardep .ge. 31 .and. pardep .le. 32 .or.&
   &pardep .ge. 35 .and. pardep .le. 42 .or.&
   &pardep .ge. 44 .and. pardep .le. 45
!
!-----no location names known; noaction

   nrlst = maxlst

!-----------------------------------------------------------------------
!-----check found number against required number
!-----------------------------------------------------------------------
   okee = okee .and. (nrlst .eq. maxlst)
!--------------------------------------------------------------------
!-----Close trim-<runid>.dat and trim-<runid>.def MAP-files
!--------------------------------------------------------------------
8888 continue
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

subroutine ods_tri_nef_map_mat&
&(&
&fname ,itype  ,parcod, loc   , tim   ,misval,&
&i3gl  ,maxdim ,xdata , ierror, option,&
&ibuffs,rbuffs                               )
!-----------------------------------------------------------------------
!           Function: select map data out of TRISULA NEFIS files
!        Method used:
!
!-----------------------------------------------------------------------
!   Calling routine :              GETMAT
!-----------------------------------------------------------------------
!   Called  routines:              OPNNEF
!                                  CLOSFL
!                                  GETELT (nefis)
!                                  julind_map
!                                  ods_tri_nef_map_getdata
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
! FILHDA     CH*256                File name NEFIS data file for MAP
! FILHDE     CH*256                File name NEFIS definition file for
!                                  MAP
! GRPDEF     CH*16                 Group name definition
! GRPDMS      I*4  1               Array with GRPNDM dimensions
! GRPNDM      I*4                  Number of dimenmsions of this group
! GRPORD      I*4  1               Array which gives order in which data
!                                  must be read
! HDAFDS      I*4  999             Data file descriptor for the MAP-DAT
!                                  file
! HDEFDS      I*4  2997            Definition file description for the
! IERROR      I*4                  Error code for NEFIS error
! ITIM        I*4                  index of map time
! num_layers_grid        I*4                  Number of layers
! LMAX    I   I*4                  Number of constituents
!                                  for old files LMAX = LSTCI
!                                  for new files LMAX = LSTCI + LTUR
! LMAXD       I*4                  maximum(1,LMAX)
! LSTCI   I   I*4                  Total number of constituents (incl.
!                                  turbulence for old trim files).
! LTUR    I   I*4                  Number of turbulence constituents
! LAY         I*4  3               layer selection actual
! N           I*4                  Counter for XDATA
! NOSTAT      I*4                  Number of stations
! NTRUV       I*4                  Number of cross-sections
! NINDEX      I*4  3               indices of time frame in Nefis file
! OKEE        L*4                  Flag for error checking
! SELMAP  I  CH*20                 Output flags containing Y or N for
!                                  various output quantities selection
!                                  for map files
! UINDEX      I*4  3               Array with indices of the cells to
!                                  be read
! USRORD      I*4                  Sequence in which the cells must be
!                                  read
!
!--Pointer variables to buffer space
!
! DICWW       I*4                  Pointer for array DICWW
! DP          I*4                  Pointer for array DP
! DPS         I*4                  Pointer for array DPS
! IFLAG       I*4                  Pointer for array IFLAG
! IGRID       I*4                  Pointer for array IGRID
! JCONST      I*4                  Pointer variable
! KCS         I*4                  Pointer for array KCS
! KCU         I*4                  Pointer for array KCU
! KCV         I*4                  Pointer for array KCV
! KFU         I*4                  Pointer for array KFU
! KFV         I*4                  Pointer for array KFV
! R1          I*4                  Pointer for array R1
! RHO         I*4                  Pointer for array RHO
! S1          I*4                  Pointer for array S1
! TAUETA      I*4                  Pointer for array TAUETA
! TAUKSI      I*4                  Pointer for array TAUKSI
! THICK       I*4                  Pointer for array THICK
! U1          I*4                  Pointer for array U1
! V1          I*4                  Pointer for array V1
! VICWW       I*4                  Pointer for array VICWW
! WPHY        I*4                  Pointer for array WPHY
! XCOR        I*4                  Pointer for array XCOR
! XZ          I*4                  Pointer for array XZ
! YCOR        I*4                  Pointer for array YCOR
! YZ          I*4                  Pointer for array YZ
!-----------------------------------------------------------------------
!
!  DECLARATIONS
!
   include         'ods.inc'
!
   integer         dicww ,dp    ,dps   ,alfas        ,&
   &iflag ,igrid ,jconst,&
   &kcs   ,kcu   ,kcv   ,kfu   ,kfv   ,&
   &r1    ,rho   ,s1    ,taueta,tauksi,&
   &thick ,u1    ,v1    ,vicww ,wphy  ,&
   &xcor  ,xz    ,ycor  ,yz
!
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
!-----------------------------------------------------------------------
!-----declaration Local variables
!-----------------------------------------------------------------------
   character*256   filhda,filhde
   character*20    selmap,namcon(10)
!
   integer         nindex (3)
   integer         lay(3),ind   ,itim  ,l
   integer         ierror,num_layers_grid  ,lmax  ,num_columns  ,num_rows
   integer         lstci ,ltur  ,irho  ,kmaxon
   integer         noroco,irocol
!
   logical         ex    ,okee  ,zrho
!
!-----------------------------------------------------------------------
!-----declaration NEFIS
!-----------------------------------------------------------------------
   integer       hdefds( 2997),hdafds(  999)
   integer       uindex(    3),usrord,buflen
   integer*4     nbytsg,elmndm,elmdms(    5)
!
   character*8   elmtyp
!
   character*16  grpdef,elmnam,elmqty,elmunt
!
   character*64  elmdes
!
   integer       GETELT,GETELS,INQELM
!-----------------------------------------------------------------------
!-----Initialisation
!-----------------------------------------------------------------------
!
   ierror =  0
   okee   = .true.
   zrho   = .false.
!--------------------------------------------------------------------
!-----Test if trim-<runid>.dat and trim-<runid>.def Nefis MAP-files
!     exist
!--------------------------------------------------------------------
   call ods_check_nefis( fname  , '.def' , ierror )
   if ( ierror .ne. ieok   ) then
      return
   endif
!--------------------------------------------------------------------
!-----Open trim-<runid>.dat and trim-<runid>.def MAP-files
!--------------------------------------------------------------------
   call OPNNEF(fname, itype, hdafds, hdefds, ierror)
   if (ierror .ne. 0) then
      ierror = IEFIRO
      return
   endif
!--------------------------------------------------------------------
!-----Read array-dimensions from Nefis MAP-files group 2
!--------------------------------------------------------------------
   grpdef = 'map-const'
!
   uindex(1) = 1
   uindex(2) = 1
   uindex(3) = 1
   usrord    = 1
   buflen    = 4
!
   num_columns      = 0
   num_rows      = 0
   lmax      = 0
   lstci     = 0
   ltur      = 0
   irho      = 0
   num_layers_grid      = 0
   noroco    = 0
!
   elmnam    = 'num_columns'
   okee = okee .and.&
   &GETELT(hdefds,grpdef    ,elmnam    ,&
   &uindex,usrord    ,buflen    ,num_columns      )&
   &.eq. 0
!
   elmnam    = 'num_rows'
   okee = okee .and.&
   &GETELT(hdefds,grpdef    ,elmnam    ,&
   &uindex,usrord    ,buflen    ,num_rows      )&
   &.eq. 0
!
!--------------------------------------------------------------------
!-----Read element LMAX; LMAX only defined in old trim files
!     hence if not defined ierror = -25041
!--------------------------------------------------------------------
   elmnam    = 'LMAX'
   ierror    = GETELT(hdefds,grpdef    ,elmnam    ,&
   &uindex,usrord    ,buflen    ,LSTCI     )
   if (ierror .ne. 0) then
!--------------------------------------------------------------------
!--------In case of a new trih file read LSTCI and LTUR
!--------------------------------------------------------------------
      elmnam = 'LSTCI'
      ierror = GETELT(hdefds,grpdef    ,elmnam    ,&
      &uindex,usrord    ,buflen    ,LSTCI     )
      if (ierror .ne. 0) then
         okee   = .false.
         goto 8888
      endif
!
      elmnam = 'LTUR'
      ierror = GETELT(hdefds,grpdef    ,elmnam    ,&
      &uindex,usrord    ,buflen    ,LTUR      )
      if (ierror .ne. 0) then
         okee   = .false.
         goto 8888
      endif
   endif
!
   lmax = lstci + ltur
!
   elmnam = 'num_layers_grid'
   ierror = GETELT(hdefds,grpdef    ,elmnam    ,&
   &uindex,usrord    ,buflen    ,num_layers_grid      )
   if (ierror .ne. 0) then
      okee   = .false.
      goto 8888
   endif
!
!-----------------------------------------------------------------------
!-----Element SELMAP selection of output
!-----------------------------------------------------------------------
   buflen    = 20
   elmnam    = 'SELMAP'
   ierror    = GETELS&
   &(hdefds   ,grpdef    ,elmnam    ,&
   &uindex   ,usrord    ,buflen    ,SELMAP    )
   if (ierror .ne. 0) then
!-----------------------------------------------------------------------
!--------In case of a old trim file no SELMAP then
!           re-define SELMAP like definition in subroutine RDPRFL
!-----------------------------------------------------------------------
      selmap = 'YYYYYYYYYYYYYYYYYYYY'
      if (num_layers_grid   .eq. 1) selmap( 4: 5) = 'NN'
      if (lstci  .eq. 0) selmap( 6:13) = 'NNNNNNNN'
      if (ltur   .eq. 0) selmap(14:15) = 'NN'
      if (num_layers_grid   .eq. 1) selmap(18:19) = 'NN'
      if (lmax   .eq. 0) selmap(19:19) = 'N'
      selmap(20:20) = 'X'
   endif
!-----------------------------------------------------------------------
!-----Element NAMCON constituents and turbulence quantity names
!     only if selmap( 6:15) <> 'NNNNNNNNNN'
!-----------------------------------------------------------------------
   if (index (selmap( 6:15),'Y') .gt. 0) then
      buflen    = 20 * lmax
      elmnam    = 'NAMCON'
      ierror    = GETELS&
      &(hdefds,grpdef    ,elmnam    ,&
      &uindex,usrord    ,buflen    ,NAMCON    )
      if (ierror .ne. 0) then
         okee   = .false.
         goto 8888
      endif
!
      do 110 l = 1,lmax
         if (namcon(l)(:11) .eq. 'Salinity   ') irho   = 1
         if (namcon(l)(:11) .eq. 'Temperature') irho   = 1
110   continue
   endif
!-----------------------------------------------------------------------
!-----In case of a old trim file re-define SELMAP(20:20)
!-----------------------------------------------------------------------
   if (selmap(20:20) .eq. 'X') then
      selmap(20:20) = 'N'
      if (irho   .eq. 1) selmap(20:20) = 'Y'
   endif
!
   zrho = irho .eq. 1
!
   elmnam    = 'NOROCO'
   okee = okee .and.&
   &GETELT(hdefds,grpdef    ,elmnam    ,&
   &uindex,usrord    ,buflen    ,NOROCO    )&
   &.eq. 0
!-----------------------------------------------------------------------
   elmnam    = 'DICWW'
   elmndm    = 5
   ierror    = INQELM(hdefds,elmnam ,elmtyp ,nbytsg ,elmqty ,&
   &elmunt,elmdes ,elmndm ,elmdms         )
!
   if ( ierror .ne. 0 ) then
!--------version 2.45 and later
      kmaxon    = 1
   else
      if (elmdms(elmndm) .eq. num_layers_grid) then
!--------version 2.03
         kmaxon    = 0
      else
!--------version 2.45 and later
         kmaxon    = 1
      endif
   endif
!
!-----------------------------------------------------------------------
!--------array indices (reals)
!-----------------------------------------------------------------------
   xcor   = 1
   ycor   = xcor   + num_rows   * num_columns
   xz     = ycor   + num_rows   * num_columns
   yz     = xz     + num_rows   * num_columns
   alfas  = yz     + num_rows   * num_columns
   dp     = alfas  + num_rows   * num_columns
   dps    = dp     + num_rows   * num_columns
   thick  = dps    + num_rows   * num_columns
   jconst = thick  + num_layers_grid   + 1
   s1     = jconst
   tauksi = s1     + num_rows * num_columns
   taueta = tauksi + num_rows * num_columns
   u1     = jconst
   v1     = u1     + num_rows * num_columns * num_layers_grid
   wphy   = v1     + num_rows * num_columns * num_layers_grid
   r1     = jconst
   vicww  = jconst
   dicww  = vicww  + num_rows * num_columns * (num_layers_grid+kmaxon)
   rho    = dicww  + num_rows * num_columns * (num_layers_grid+kmaxon)
!        next   = jconst + num_rows * num_columns * (num_layers_grid+kmaxon) * max (3,lmax)
!
!-----------------------------------------------------------------------
!--------array indices (integers)
!-----------------------------------------------------------------------
   irocol =  1
   kcs    =  irocol + 5     * noroco
   kcu    =  kcs    + num_rows  * num_columns
   kcv    =  kcu    + num_rows  * num_columns
   kfu    =  kcv    + num_rows  * num_columns
   kfv    =  kfu    + num_rows  * num_columns
   iflag  =  kfv    + num_rows  * num_columns
   igrid  =  iflag  + num_rows  * num_columns
!        next   =  igrid  + num_rows  * num_columns
!
!-----------------------------------------------------------------------
!-----calculate indices for time frame
!-----------------------------------------------------------------------
   call julind_map (hdefds, hdafds, tim,   nindex, ierror)
   okee = okee .and. (ierror .eq. 0)
   if (.not. okee) then
      goto 8888
   endif
!--------------------------------------------------------------------
!-----set tim           {precon: tim(1  )=tim(2  ) and loc(3  )=1}
!--------------------------------------------------------------------
   itim   = nindex(1)
!--------------------------------------------------------------------
!-----for 2DH must be;-----------------------------------------------
!-----set loc (layer  ) {precon: loc(1,3)<=loc(2,3) and loc(3,3)=1}
!--------------------------------------------------------------------
   lay(1) = max( 1, loc(1,3) )
   lay(2) = max( 1, min( loc(2,3), num_layers_grid) )
   lay(3) = max( 1, loc(3,3) )
!
!-----------------------------------------------------------------------
   call ods_tri_nef_map_getdata(&
   &hdefds        ,hdafds        ,misval        ,&
   &num_rows          ,num_columns          ,num_layers_grid          ,&
   &lmax          ,noroco        ,itim          ,&
   &lay           ,parcod        ,ierror        ,&
   &lstci         ,ltur          ,kmaxon        ,&
   &maxdim        ,xdata         ,&
   &ibuffs(irocol),ibuffs(kcs   ),ibuffs(kcu   ),&
   &ibuffs(kcv   ),ibuffs(kfu   ),ibuffs(kfv   ),&
   &ibuffs(iflag ),ibuffs(igrid ),&
   &rbuffs(xcor  ),rbuffs(ycor  ),rbuffs(xz    ),&
   &rbuffs(yz    ),rbuffs(alfas ),&
   &rbuffs(dp    ),rbuffs(dps   ),rbuffs(thick ),&
   &rbuffs(s1    ),rbuffs(tauksi),rbuffs(taueta),&
   &rbuffs(u1    ),rbuffs(v1    ),rbuffs(wphy  ),&
   &rbuffs(r1    ),rbuffs(vicww ),rbuffs(dicww ),&
   &rbuffs(rho   )                              )
!
   okee = okee .and. (ierror .eq. 0)
!--------------------------------------------------------------------
!-----Close trim-<runid>.dat and trim-<runid>.def MAP-files
!--------------------------------------------------------------------
8888 continue
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

subroutine ods_tri_nef_map_getdata&
&(&
&hdefds        ,hdafds        ,misval        ,&
&num_rows          ,num_columns          ,num_layers_grid          ,&
&lmax          ,noroco        ,itim          ,&
&lay           ,parcod        ,ierror        ,&
&lstci         ,ltur          ,kmaxon        ,&
&maxdim        ,xdata         ,&
&irocol        ,kcs           ,kcu           ,&
&kcv           ,kfu           ,kfv           ,&
&iflag         ,igrid         ,&
&xcor          ,ycor          ,xz            ,&
&yz            ,alfas         ,&
&dp            ,dps           ,thick         ,&
&s1            ,tauksi        ,taueta        ,&
&u1            ,v1            ,wphy          ,&
&r1            ,vicww         ,dicww         ,&
&rho                                         )
!-----------------------------------------------------------------------
!           Function: get map data out of TRISULA NEFIS files
!        Method used:
!
!-----------------------------------------------------------------------
!   Calling routine :              ODS_TRI_NEF_MAP_MAT
!-----------------------------------------------------------------------
!   Called  routines:              wrhcor
!                                  wrhwat
!                                  wrh3di
!                                  wrhuvi
!                                  wrhtai
!                                  wrhcon
!                                  wrhtur
!                                  wrhgrd
!                                  wrhtd
!                                  wrh3dv
!                                  wrhtav
!                                  wrhuvv
!
!-----------------------------------------------------------------------
!    Parameters:
!    -----------
!
!   Var.      Type Dimensions
!   -------------------------
!
! HDAFDS      I*4  999             Data file descriptor for the MAP-DAT
!                                  file
! HDEFDS      I*4  2997            Definition file description for the
! MISVAL      R*4                  missing value
! ITIM        I*4                  index of map time
! num_layers_grid    I   I*4                  Number of layers
! LAY     I   I*4  3               Specified layers
! LMAX    I   I*4                  Number of constituents
!                                  for old files LMAX = LSTCI
!                                  for new files LMAX = LSTCI + LTUR
! LSTCI   I   I*4                  Total number of constituents (incl.
!                                  turbulence for old trim files).
! LTUR    I   I*4                  Number of turbulence constituents
! num_columns    I   I*4                  Number of gridpoints in the x-dir.
! num_rows    I   I*4                  Number of gridpoints in the y-dir.
! NOROCO  I   I*4                  Number of Computational rows & cols
! MAXDIM      I*4            I     length of data array
! XDATA       R*4   maxdim   O     array with the data
! IERROR      I*4            O     = 0 no errors, = 1 error detected
! PARCOD  I   I*4                  parameter to get data of
!                                   1 = 'wl' water-elevation
!                                   2 = 'wh' water-hight
!                                  16 = 'tu' taubottom u direction
!                                  17 = 'tv' taubottom v direction
!                                  18 = 'tm' taubottom magnitude
!                                  19 = 'ta' taubottom direction
!                                  39 = 'tf' taubottom vector field
!                                   7 = 'u ' velocity u direction
!                                   8 = 'v ' velocity v direction
!                                   9 = 'm ' velocity magnitude
!                                  10 = 'd ' velocity direction
!                                  40 = 'uv' velocity vector field
!  not yet implemented             41 = 'uw' velocity vector field
!  not yet implemented             42 = 'vw' velocity vector field
!                                   3 = 'du' dpt. aver. vel. u
!                                   4 = 'dv' dpt. aver. vel. v
!                                   5 = 'dm' dpt. aver. vel. mag
!                                   6 = 'dd' dpt. aver. vel. dir
!                                  38 = 'df' dpt. aver. vel vector
!                                  11 = 'w ' velocity w direction
!  not yet implemented             13 = 'qx' flow rate u
!  not yet implemented             14 = 'qy' flow rate v
!                                  2. = 'c ' concentration
!                                  15 = 'vw' viscosity
!                                  31 = 'rh' density
!                                  32 = 'dw' diffusity
!                                  35 = 'ag' active grid
!                                  36 = 'bl' boundary lines, including
!                                            permanent dry points
!                                  37 = 'td' time dependent dry points
!                                  44 = 'dl' depth lines in H-points
!                                  45 = 'dz' depth lines in Z-points
!                                  46 =      XCOR x coordinate depths
!                                  47 =      YCOR y coordinate depths
!                                  48 =      XCOR x coordinate zeta
!                                  49 =      YCOR y coordinate zeta
!                                  50 =      UVDAMS grid attribute
!  not yet implemented             51 =      z coordinate step
!  not yet implemented             52 =      z coordinate slope
!                                  12 =      z-coordinate
! DICWW   --  R*4  num_rows,num_columns,num_layers_grid+1Diffusity in zeta points
! DP      --  R*4  num_rows,num_columns       Depth values in depth points
! DPS     --  R*4  num_rows,num_columns       Depth values in zeta points depending
!                                  on dryflp
! ALFAS   --  R*4  num_rows,num_columns       Transformation coefficients (in radians)
! IFLAG   --  I*4  num_rows,num_columns       Array for permanent and tempory dry
!                                  point
! IGRID   --  I*4  num_rows,num_columns       Array with actual grid
! IROCOL  --  I*4  5,NOROCO        Pointer table with bound. coord. and
!                                  bound. types (comp. cols. and rows)
! KCS     --  I*4  num_rows,num_columns       Mask array for the zeta points
!                                  (time independent)
!                                  =0 inactive point
!                                  =1 active   point
!                                  =2 open boundary point
! KCU     --  I*4  num_rows,num_columns       Mask array for the u-velocity point
!                                  (time independent)
!                                  =0 dry      point
!                                  =1 active   point
! KCV     --  I*4  num_rows,num_columns       Mask array for the v-velocity point
!                                  (time independent)
!                                  =0 dry      point
!                                  =1 active   point
! KFU     --  I*4  num_rows,num_columns       Mask array for the u-velocity point
!                                  (time dependent)
!                                  =0 dry      point
!                                  =1 active   point
! KFV     --  I*4  num_rows,num_columns       Mask array for the v-velocity point
!                                  (time dependent)
!                                  =0 dry      point
!                                  =1 active   point
! R1      --  R*4  num_rows,num_columns,num_layers_grid+1,LMAX
!                                  Concentrations in zeta point
! RHO     --  R*4  num_rows,num_columns,num_layers_grid  Density in zeta points
! S1      --  R*4  num_rows,num_columns       Water-level in zeta point
! TAUETA  --  R*4  num_rows,num_columns       Tau bottom in v-velocity point
! TAUKSI  --  R*4  num_rows,num_columns       Tau bottom in u-velocity point
! THICK   --  R*4  num_layers_grid            Relative layer thickness
! U1      --  R*4  num_rows,num_columns,num_layers_grid  U-velocity in u-velocity point
! V1      --  R*4  num_rows,num_columns,num_layers_grid  V-velocity in v-velocity point
! VICWW   --  R*4  num_rows,num_columns,num_layers_grid+1Viscosity in zeta points
! WPHY    --  R*4  num_rows,num_columns,num_layers_grid  W-velocity in zeta point
! XCOR    --  R*4  num_rows,num_columns       X-coordinate in depth point
! XZ      --  R*4  num_rows,num_columns       X-coordinate in zeta point
! YCOR    --  R*4  num_rows,num_columns       Y-coordinate in depth point
! YZ      --  R*4  num_rows,num_columns       Y-coordinate in zeta point
!-----------------------------------------------------------------------
!    Local variables:
!    ----------------
!
!   Var.      Type Dimensions
!   -------------------------
!
! BUFLEN      I*4                  Size in bytes of available buffer
! FACTOR      R*4                  Scaling factor for W-velocities
! GRDANG      R*4                  Vertex between the y-axis and north
! GRPDEF     CH*16                 Group name definition
! IERROR      I*4                  Error code for NEFIS error
! IRHO        I*4                  Parameter to check if var='rh' is
!                                  permitted (irho = 1)
! num_layers_grid        I*4                  Number of layers
! LMAX        I*4                  Number of constituents
! LAY         I*4                  Actual layer number
! K           I*4                  Loop variable
! M           I*4                  Loop variable
! N           I*4                  Loop variable
! N1          I*4                  Help variable
! N2          I*4                  Help variable
! NOROW       I*4                  Number of comp. rows  in IROCOL-array
! OKEE        L*4                  Flag for error checking
! UINDEX      I*4  3               Array with indices of the cells to
!                                  be read
! USRORD      I*4                  Sequence in which the cells must be
!                                  read
!-----------------------------------------------------------------------
!
!  declarations
!
   include         'ods.inc'
!
   integer       n            ,m            ,norow
   integer       nd           ,md           ,k
   integer       n1           ,n2           ,nlay
   integer       kmaxon
!
   integer       num_rows         ,num_columns         ,num_layers_grid         ,&
   &lmax         ,noroco       ,itim         ,&
   &lay(3)       ,parcod       ,ierror       ,&
   &maxdim       ,lstci        ,ltur
!
   logical       usefix
!
   real          misval       ,factor       ,grdang       ,&
   &depfac       ,depth        ,depflm
!
   real          xdata(maxdim)
!
   integer       irocol(5     ,noroco),kcs   (num_rows  ,num_columns  ),&
   &kcu   (num_rows  ,num_columns  ),kcv   (num_rows  ,num_columns  ),&
   &kfu   (num_rows  ,num_columns  ),kfv   (num_rows  ,num_columns  ),&
   &iflag (num_rows  ,num_columns  ),igrid (num_rows  ,num_columns  )
!
   real          xcor  (num_rows  ,num_columns  ),ycor  (num_rows  ,num_columns  ),&
   &xz    (num_rows  ,num_columns  ),yz    (num_rows  ,num_columns  ),&
   &alfas (num_rows  ,num_columns  ),&
   &dp    (num_rows  ,num_columns  ),dps   (num_rows  ,num_columns  ),&
   &thick (num_layers_grid+1)
   real          s1    (num_rows  ,num_columns  ),&
   &tauksi(num_rows  ,num_columns  ),taueta(num_rows  ,num_columns  ),&
   &u1    (num_rows  ,num_columns   ,num_layers_grid  ),&
   &v1    (num_rows  ,num_columns   ,num_layers_grid  ),&
   &wphy  (num_rows  ,num_columns   ,num_layers_grid  ),&
   &r1    (num_rows  ,num_columns   ,num_layers_grid+kmaxon ,lmax ),&
   &vicww (num_rows  ,num_columns   ,num_layers_grid+kmaxon),&
   &dicww (num_rows  ,num_columns   ,num_layers_grid+kmaxon),&
   &rho   (num_rows  ,num_columns   ,num_layers_grid  )
!
   logical       okee  ,lvar
!
!-----------------------------------------------------------------------
!-----Declarations NEFIS
   integer*4     hdefds( 2997),hdafds(  999)
!
   integer*4     uindex(    3),usrord,buflen
!
   character*16  grpdef

   character*16  elmtyp
   character*16  elmqty
   character*16  elmunt
   character*64  elmdsc
   integer       nbytsg
   integer       elmndm
   integer       elmdms(5)

!
   integer*4     GETELT,GETELS
!
!-----------------------------------------------------------------------
!-----Initalize
   okee      = .true.
!-----default value for vertical velocities w
!AM:  this is unnecessary - the vertical scaling is provided by the
!     plotroutines
!     factor    = 1000.0
!AM
!-----------------------------------------------------------------------
!-----Read initial values from group 2
   grpdef    = 'map-const'
   uindex(1) = 1
   uindex(2) = 1
   uindex(3) = 1
   usrord    = 1
!
   buflen    = 4
   okee      = okee .and.&
   &GETELT(hdefds,grpdef    ,'GRDANG'  ,&
   &uindex,usrord    ,buflen    ,GRDANG    )&
   &.eq. 0

!-----------------------------------------------------------------------
!-----Get the sigma-layer thicknesses or the fixed layer coordinates
!     If fixed layer coordinates are available, then they are preferred.
!     If neither information is available, we have a problem
!
   buflen    =  4 * (num_layers_grid+1)
   ierror    = GETELT(hdefds,grpdef    ,'ZK'      ,&
   &uindex,usrord    ,buflen    ,THICK     )
   if ( ierror .eq. 0 ) then
      ierror = INQELM(hdefds,'ZK', elmtyp, nbytsg, elmqty, elmunt,&
      &elmdsc, elmdim, elmdms )
      if ( elmdms(1) .eq. 1 ) then
         ierror = -11111
      endif
   endif

   if ( ierror .ne. 0 ) then
      usefix = .false.
      ierror = GETELT(hdefds,grpdef    ,'THICK'   ,&
      &uindex,usrord    ,buflen    ,THICK     )
      if ( ierror .ne. 0 ) then
         okee = .false.
      endif
   else
      usefix = .true.
      depflm = thick(1)
      do 5 k = 1,num_layers_grid
         thick(k) = thick(k+1) - thick(k)
5     continue
   endif

!-----------------------------------------------------------------------
!-----Read and calculate coordinate arrays
!     s1 will be used as buffer array
   call    wrhcor(okee      ,hdefds    ,hdafds    ,misval    ,&
   &num_rows      ,num_columns      ,noroco    ,norow     ,&
   &irocol    ,kcs       ,kcu       ,kcv       ,&
   &xcor      ,ycor      ,xz        ,yz        ,&
   &alfas                ,dp        ,dps       ,&
   &s1                                         )
!
!-----for time dependent blocks read kfu and kfv-----------
!     not needed for;
!        active grid                      parcod = 35  'ag'
!        boundary lines                   parcod = 36  'bl'
!        dpt. at d_points                 parcod = 44  'dl'
!        dpt. at z_points                 parcod = 45  'dz'
   lvar   = ((parcod .ne. 35) .and. (parcod .ne. 36) .and.&
   &(parcod .ne. 44) .and. (parcod .ne. 45))
   if (lvar ) then
      grpdef    = 'map-series'
      usrord    = 1
      buflen    = 4 * num_rows   * num_columns
      uindex(1) = itim
      uindex(2) = itim
      uindex(3) = 1
!
!--------Read from group 3 KFU
      okee      = okee .and.&
      &GETELT(hdefds,grpdef    ,'KFU   '  ,&
      &uindex,usrord ,buflen    ,KFU       )&
      &.eq. 0
!
!--------Read from group 3 KFV
      okee      = okee .and.&
      &GETELT(hdefds,grpdef    ,'KFV   '  ,&
      &uindex,usrord ,buflen    ,KFV       )&
      &.eq. 0
   endif
!
!--------------------------------------------------------------------
!-----get data
   if (parcod .eq. 1 .or. parcod .eq.  2 ) then
!--------water level, ZWL                 parcod =  1  'wl'
!--------total water depth, ZWL + DPS     parcod =  2  'wh'
      call wrhwat(okee      ,hdefds    ,hdafds    ,parcod    ,&
      &itim      ,num_rows      ,num_columns      ,&
      &kcs       ,kfu       ,kfv       ,&
      &dps       ,s1                              )
!--------store results in xdata
      do 10 n = 1,num_rows
         do 10 m = 1,num_columns
            n1           =   (n-1) * num_columns +  m
            xdata ( n1 ) = s1    (n,m)
10    continue
   else if (parcod .ge.  3 .and. parcod .le. 6 ) then
!--------dpt. aver. cur. u         {precon: parcod= 3} 'du'
!--------dpt. aver. cur. v         {precon: parcod= 4} 'dv'
!--------dpt. aver. cur. mag.      {precon: parcod= 5} 'dm'
!--------dpt. aver. cur. dir.      {precon: parcod= 6} 'dd'
      call wrh3di(okee      ,hdefds    ,hdafds    ,parcod    ,&
      &grdang    ,itim      ,misval    ,&
      &num_rows      ,num_columns      ,num_layers_grid      ,&
      &kcs       ,kfu       ,kfv       ,&
      &xcor      ,ycor      ,alfas     ,&
      &u1        ,v1        ,thick                )
!--------store results in xdata
      if ( parcod .eq.  3 .or. parcod .eq.  5 ) then
         do 30 n = 1,num_rows
            do 30 m = 1,num_columns
               n1           =   (n-1) * num_columns +  m
               xdata ( n1 ) = u1    (n ,m , 1)
30       continue
      else if ( parcod .eq.  4 .or. parcod .eq.  6 ) then
         do 40 n = 1,num_rows
            do 40 m = 1,num_columns
               n1           =   (n-1) * num_columns +  m
               xdata ( n1 ) = v1    (n ,m , 1)
40       continue
      endif
   else if (parcod .ge.  7 .and. parcod .le. 10) then
!--------current u (layer)         {precon: parcod= 7} 'u '
!--------current v (layer)         {precon: parcod= 8} 'v '
!--------current mag. (layer)      {precon: parcod= 9} 'm '
!--------current dir. (layer)      {precon: parcod=10} 'd '
      call wrhuvi(okee      ,hdefds    ,hdafds    ,parcod    ,&
      &grdang    ,itim      ,misval    ,&
      &num_rows      ,num_columns      ,num_layers_grid      ,&
      &kcs       ,kfu       ,kfv       ,&
      &xcor      ,ycor      ,&
      &alfas                ,u1        ,v1        )
!--------store results in xdata
      if ( parcod .eq.  7 .or. parcod .eq.  9 ) then
         n1 = 0
         do 70 k = lay(1), lay(2), lay(3)
            do 70 n = 1,num_rows
               do 70 m = 1,num_columns
                  n1           = n1 + 1
                  xdata ( n1 ) = u1    (n ,m ,k )
70       continue
      else if ( parcod .eq.  8 .or. parcod .eq. 10 ) then
         n1 = 0
         do 80 k = lay(1), lay(2), lay(3)
            do 80 n = 1,num_rows
               do 80 m = 1,num_columns
                  n1           = n1 + 1
                  xdata ( n1 ) = v1    (n ,m ,k )
80       continue
      endif
   else if (parcod .eq. 11 ) then
!--------current w.   (layer)                          'w '
      grpdef    = 'map-series'
      usrord    = 1
      buflen    = 4 * num_rows   * num_columns   * num_layers_grid
      uindex(1) = itim
      uindex(2) = itim
      uindex(3) = 1
!-----------------------------------------------------------------------
!--------Read from group 3 WPHY
!-----------------------------------------------------------------------
      okee      = okee .and.&
      &GETELT(hdefds   ,grpdef    ,'WPHY  '  ,&
      &uindex   ,usrord ,buflen    ,WPHY      )&
      &.eq. 0
!--------Calculate WPHY*FACTOR
!        if kcs = 0 then permanent drypoint; if surrounding kfu and kfv
!        := 0 then temporary drypoint
      n1 = 0
      do 110 k = lay(1), lay(2), lay(3)
         do 110 n = 1,num_rows
            do 110 m = 1,num_columns
               md    = max (1,m-1)
               nd    = max (1,n-1)
               lvar  = ((kcs   (n ,m ) .eq. 1) .and.&
               &(kfu   (n ,m ) .eq. 1  .or.&
               &kfu   (n ,md) .eq. 1  .or.&
               &kfv   (n ,m ) .eq. 1  .or.&
               &kfv   (nd,m ) .eq. 1))
               n1           = n1 + 1
               if (lvar  ) then
!AM:
!        see remark above
!AM            xdata ( n1 ) = wphy(n ,m ,k ) * factor
                  if ( wphy(n,m,k) .ne. -999.0 ) then
                     xdata ( n1 ) = wphy(n ,m ,k )
                  else
                     xdata ( n1 ) = misval
                  endif
               else
                  xdata ( n1 ) = misval
               endif
110   continue
   else if (parcod .eq. 12 ) then
!--------z-coordinate
!        beware of exceeding the array space:
!        this parameter is used mainly in connection with
!        others, and therefore the array space
!
!        First get the water level for the required time (parcod=1)
!
      n1 = 1
      call wrhwat(okee      ,hdefds    ,hdafds    ,n1        ,&
      &itim      ,num_rows      ,num_columns      ,&
      &kcs       ,kfu       ,kfv       ,&
      &dps       ,s1                              )
!
      nlay = lay(2) + 1
      if ( maxdim .lt. num_rows*num_columns*nlay ) nlay = maxdim / num_rows / num_columns
!
!-------Two cases arise:
!       Sigma-coordinates and fixed layer coordinates
!       Note:
!       Due to consistent but apparently strange aspects of the
!       plots (having to do with quick changes in depth that are
!       averaged or lead to triangular grid cells), we set the
!       depth to be constant over the layers, except near the
!       surface.
      if ( usefix ) then
         n1 = 0
         do 125 k = lay(1), nlay, lay(3)
            if ( k .eq. 1 ) then
               depth = depflm
            else
               depth = depth + thick(k-1)
            endif

            do 124 n = 1,num_rows
               do 123 m = 1,num_columns
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
123            continue
124         continue
125      continue
      else
         n1 = 0
         depfac = 0.0
         do 129 k = lay(1), nlay, lay(3)

!-------Determine the contribution of the depth
!-------The "if" statement is essential: in case of
!-------2d-variables
            if ( k .eq. 1 ) then
               depfac = 0.0
            else
               depfac = depfac + thick(k-1)
               if ( k .eq. nlay ) depfac = 1.0
            endif

            do 128 n = 1,num_rows
               do 127 m = 1,num_columns
                  md    = max (1,m-1)
                  nd    = max (1,n-1)
                  lvar  =  (kcs   (n ,m ) .eq. 1)
!
!--------Be careful: all active cells must have a z-coordinate!
!           lvar  = ((kcs   (n ,m ) .eq. 1) .and.
!    *               (kfu   (n ,m ) .eq. 1  .or.
!    *                kfu   (n ,md) .eq. 1  .or.
!    *                kfv   (n ,m ) .eq. 1  .or.
!    *                kfv   (nd,m ) .eq. 1))
                  n1           = n1 + 1
                  if (lvar  ) then
                     xdata(n1) = s1(n,m) - depfac * (s1(n,m)+dp(n,m))
                  else
                     xdata(n1) = misval
                  endif
127            continue
128         continue
129      continue
      endif

   else if (parcod .eq. 13 ) then
!--------flow rate u, ZQXK                    not yet implemented
   else if (parcod .eq. 14 ) then
!--------flow rate v, ZQYK                    not yet implemented
   else if (parcod .eq. 15 ) then
!--------eddy viscosity, ZVICWW           parcod = 15 'vw' or 'nu'
      grpdef    = 'map-series'
      usrord    = 1
      buflen    = 4 * num_rows   * num_columns  * (num_layers_grid+kmaxon)
      uindex(1) = itim
      uindex(2) = itim
      uindex(3) = 1
!--------Read from group 3 ZVICWW
      okee      = okee .and.&
      &GETELT(hdefds,grpdef    ,'VICWW '  ,&
      &uindex,usrord ,buflen    ,VICWW     )&
      &.eq. 0
!--------if kcs = 0 then permanent drypoint; if surrounding kfu and kfv
!        := 0 then temporary drypoint
      n1 = 0
      do 150 k = lay(1), lay(2), lay(3)
         do 150 n = 1,num_rows
            do 150 m = 1,num_columns
               md    = max (1,m-1)
               nd    = max (1,n-1)
               lvar  = ((kcs   (n ,m ) .eq. 1) .and.&
               &(kfu   (n ,m ) .eq. 1  .or.&
               &kfu   (n ,md) .eq. 1  .or.&
               &kfv   (n ,m ) .eq. 1  .or.&
               &kfv   (nd,m ) .eq. 1))
               n1           = n1 + 1
               if (lvar  ) then
                  xdata ( n1 ) = vicww (n ,m ,k+kmaxon )
               else
                  xdata ( n1 ) = misval
               endif
150   continue
   else if (parcod .ge. 16 .and. parcod .le.19 ) then
!--------bed stress u, ZTAUKSI   {precon: parcod=16}   'tu'
!--------bed stress v, ZTAUETA   {precon: parcod=17}   'tv'
!--------bed stress mag, ZTAUETA {precon: parcod=18}   'tm'
!--------bed stress dir, ZTAUETA {precon: parcod=19}   'ta'
      call wrhtai(okee      ,hdefds    ,hdafds    ,parcod    ,&
      &misval    ,&
      &grdang    ,itim      ,num_rows      ,num_columns      ,&
      &kcs       ,kfu       ,kfv       ,&
      &xcor      ,ycor      ,alfas     ,&
      &tauksi    ,taueta                          )
!--------store results in xdata
      if ( parcod .eq. 16 .or. parcod .eq. 18 ) then
         do 160 n = 1,num_rows
            do 160 m = 1,num_columns
               n1           =   (n-1) * num_columns +  m
               xdata ( n1 ) = tauksi(n ,m )
160      continue
      else if ( parcod .eq. 17 .or. parcod .eq. 19 ) then
         do 170 n = 1,num_rows
            do 170 m = 1,num_columns
               n1           =   (n-1) * num_columns +  m
               xdata ( n1 ) = taueta(n ,m )
170      continue
      endif
   else if (parcod .ge. 20 .and. parcod .le.29 ) then
      if ( (parcod-19) .le. lstci ) then
!-----------constituents, GRO(1:lstci)
!                          {precon: <con.index>=parcod-19}
         call wrhcon(okee      ,hdefds    ,hdafds    ,itim      ,&
         &xdata     ,maxdim    ,parcod    ,lay       ,&
         &misval    ,&
         &num_rows      ,num_columns      ,num_layers_grid      ,lstci     ,&
         &kcs       ,kfu       ,kfv       ,r1        )
      else
!-----------constituents, ZTUR(1:ltur)
!                          {precon: <con.index>=parcod-19-lstci}
         call wrhtur(okee      ,hdefds    ,hdafds    ,itim      ,&
         &misval    ,&
         &num_rows      ,num_columns      ,num_layers_grid+1    ,ltur      ,&
         &kcs       ,kfu       ,kfv       ,r1        )
!-----------store results in xdata
         n1 = 0
         do 210 k = lay(1), lay(2), lay(3)
            do 210 n = 1,num_rows
               do 210 m = 1,num_columns
                  n1           = n1 + 1
                  xdata ( n1 ) = r1 (n ,m , k+1, parcod-19-lstci)
210      continue
      endif
   else if (parcod .eq. 31 ) then
!--------density, ZRHO                    parcod = 31 'rh'
      grpdef    = 'map-series'
      usrord    = 1
      buflen    = 4 * num_rows   * num_columns  * num_layers_grid
      uindex(1) = itim
      uindex(2) = itim
      uindex(3) = 1
!--------Read from group 3 RHO
      okee   = okee .and.&
      &GETELT(hdefds   ,grpdef    ,'RHO   '  ,&
      &uindex   ,usrord ,buflen    ,RHO       )&
      &.eq. 0
!--------if kcs = 0 then permanent drypoint; if surrounding kfu and kfv
!        := 0 then temporary drypoint
      n1 = 0
      do 310 k = lay(1), lay(2), lay(3)
         do 310 n = 1,num_rows
            do 310 m = 1,num_columns
               md    = max (1,m-1)
               nd    = max (1,n-1)
               lvar  = ((kcs   (n ,m ) .eq. 1) .and.&
               &(kfu   (n ,m ) .eq. 1  .or.&
               &kfu   (n ,md) .eq. 1  .or.&
               &kfv   (n ,m ) .eq. 1  .or.&
               &kfv   (nd,m ) .eq. 1))
               n1           = n1 + 1
               if (lvar  ) then
                  xdata ( n1 ) = rho   (n ,m ,k )
               else
                  xdata ( n1 ) = misval
               endif
310   continue
   else if (parcod .eq. 32 ) then
!--------eddy diffusivity, ZDICWW         parcod = 32 'dw' or 'k '
      grpdef    = 'map-series'
      usrord    = 1
      buflen    = 4 * num_rows   * num_columns  * (num_layers_grid+kmaxon)
      uindex(1) = itim
      uindex(2) = itim
      uindex(3) = 1
!--------Read from group 3 DICWW
      okee   = okee .and.&
      &GETELT(hdefds   ,grpdef    ,'DICWW '  ,&
      &uindex   ,usrord ,buflen    ,DICWW     )&
      &.eq. 0
!--------if kcs = 0 then permanent drypoint; if surrounding kfu and kfv
!        := 0 then temporary drypoint
      n1 = 0
      do 320 k = lay(1), lay(2), lay(3)
         do 320 n = 1,num_rows
            do 320 m = 1,num_columns
               md    = max (1,m-1)
               nd    = max (1,n-1)
               lvar  = ((kcs   (n ,m ) .eq. 1) .and.&
               &(kfu   (n ,m ) .eq. 1  .or.&
               &kfu   (n ,md) .eq. 1  .or.&
               &kfv   (n ,m ) .eq. 1  .or.&
               &kfv   (nd,m ) .eq. 1))
               n1           = n1 + 1
               if (lvar  ) then
                  xdata ( n1 ) = dicww (n ,m ,k+kmaxon )
               else
                  xdata ( n1 ) = misval
               endif
320   continue
   else if (parcod .eq. 35 .or. parcod .eq. 36 ) then
!--------active grid                      parcod = 35  'ag'
!--------boundary lines                   parcod = 36  'bl'
      call wrhgrd(num_rows      ,num_columns      ,noroco    ,norow     ,&
      &irocol    ,kcu       ,kcv       ,iflag     ,&
      &igrid                                      )
!--------store results in xdata
      if ( parcod .eq. 35 ) then
         do 350 n = 1,num_rows
            do 350 m = 1,num_columns
               n1           =   (n-1) * num_columns +  m
               xdata ( n1 ) = igrid (n ,m )
350      continue
      else if ( parcod .eq. 36 ) then
         do 360 n = 1,num_rows
            do 360 m = 1,num_columns
               n1           =   (n-1) * num_columns +  m
               xdata ( n1 ) = iflag (n ,m )
360      continue
      endif
   else if (parcod .eq. 37 ) then
!--------temporary dry pnts.                           'td'
      call wrhtd (num_rows      ,num_columns      ,noroco    ,norow     ,&
      &irocol    ,kcu       ,kcv       ,&
      &kfu       ,kfv       ,iflag                )
!--------store results in xdata
      do 370 n = 1,num_rows
         do 370 m = 1,num_columns
            n1           =   (n-1) * num_columns +  m
            xdata ( n1 ) = iflag (n ,m )
370   continue
   else if (parcod .eq. 38 ) then
!--------vectors dpth. aver.                           'df'
      call wrh3dv(okee      ,hdefds    ,hdafds    ,misval    ,&
      &itim      ,num_rows      ,num_columns      ,num_layers_grid      ,&
      &kcs       ,kfu       ,kfv       ,&
      &xcor      ,ycor      ,alfas     ,&
      &u1        ,v1        ,thick                )
!--------store results in xdata
      do 380 n = 1,num_rows
         do 380 m = 1,num_columns
            n1           =   (n-1) * num_columns +  m
            n2           =    num_rows * num_columns +      n1
            xdata ( n1 ) = u1    (n ,m , 1)
            xdata ( n2 ) = v1    (n ,m , 1)
380   continue
   else if (parcod .eq. 39 ) then
!--------vectors bed stress                            'tf'
      call wrhtav(okee      ,hdefds    ,hdafds    ,misval    ,&
      &itim      ,num_rows      ,num_columns      ,&
      &kcs       ,kfu       ,kfv       ,&
      &xcor      ,ycor      ,alfas     ,&
      &tauksi    ,taueta                          )
!--------store results in xdata
      do 390 n = 1,num_rows
         do 390 m = 1,num_columns
            n1           =   (n-1) * num_columns +  m
            n2           =    num_rows * num_columns +      n1
            xdata ( n1 ) = tauksi(n ,m )
            xdata ( n2 ) = taueta(n ,m )
390   continue
   else if (parcod .eq. 40 ) then
!--------vectors velocity uv                           'uv'
      call wrhuvv(okee      ,hdefds    ,hdafds    ,misval    ,&
      &itim      ,num_rows      ,num_columns      ,num_layers_grid      ,&
      &kcs       ,kfu       ,kfv       ,&
      &xcor      ,ycor      ,&
      &alfas                ,u1        ,v1        )
!--------store results in xdata
      n1 = 0
      n2 = num_rows * num_columns * ( ( lay(2) - lay(1) ) / lay(3) + 1 )
      do 400 k = lay(1), lay(2), lay(3)
         do 400 n = 1,num_rows
            do 400 m = 1,num_columns
               n1           = n1 + 1
               n2           = n2 + 1
               xdata ( n1 ) = u1    (n ,m ,k )
               xdata ( n2 ) = v1    (n ,m ,k )
400   continue
   else if (parcod .eq. 41 ) then
!--------vectors velocity uw               not yet implemented
   else if (parcod .eq. 42 ) then
!--------vectors velocity vw               not yet implemented
   else if (parcod .eq. 44 ) then
!--------dpt. at d_points                 parcod = 44  'dl'
!-----------------------------------------------------------------------
!--------if kcs = 0 then permanent drypoint
!-----------------------------------------------------------------------
      do 440 n = 1,num_rows
         do 440 m = 1,num_columns
            lvar  = ((kcs   (n ,m ) .ne. 0))
            n1           =   (n-1) * num_columns +  m
            if (lvar  ) then
               xdata ( n1 ) = dp    (n ,m )
            else
               xdata ( n1 ) = misval
            endif
440   continue
   else if (parcod .eq. 45 ) then
!--------dpt. at z_points                 parcod = 45  'dz'
!--------if kcs = 0 then permanent drypoint
      do 450 n = 1,num_rows
         do 450 m = 1,num_columns
            lvar  = ((kcs   (n ,m ) .ne. 0))
            n1           =   (n-1) * num_columns +  m
            if (lvar  ) then
               xdata ( n1 ) = dps   (n ,m )
            else
               xdata ( n1 ) = misval
            endif
450   continue
   else if (parcod .eq. 46 ) then
!--------XCOR                             parcod = 46
      do 460 n = 1,num_rows
         do 460 m = 1,num_columns
            n1           =   (n-1) * num_columns +  m
            xdata ( n1 ) = xcor(n,m)
460   continue
   else if (parcod .eq. 47 ) then
!--------YCOR                             parcod = 47
      do 470 n = 1,num_rows
         do 470 m = 1,num_columns
            n1           =   (n-1) * num_columns +  m
            xdata ( n1 ) = ycor(n,m)
470   continue
   else if (parcod .eq. 48 ) then
!--------XZ                               parcod = 48
      do 480 n = 1,num_rows
         do 480 m = 1,num_columns
            n1           =   (n-1) * num_columns +  m
            xdata ( n1 ) = xz  (n,m)
480   continue
   else if (parcod .eq. 49 ) then
!--------XZ                               parcod = 49
      do 490 n = 1,num_rows
         do 490 m = 1,num_columns
            n1           =   (n-1) * num_columns +  m
            xdata ( n1 ) = yz  (n,m)
490   continue
   else if (parcod .eq. 50 ) then
!--------UVDAMS                           parcod = 50
      do 500 n = 1,num_rows
         do 500 m = 1,num_columns
            n1           =   (n-1) * num_columns +  m
            xdata ( n1 ) = kfu(n,m) * 1.0 + kfv(n,m) * 2.0
500   continue
   else if (parcod .eq. 51 ) then
!--------z coordinate step                 not yet implemented
   else if (parcod .eq. 52 ) then
!--------z coordinate slope                not yet implemented
   endif
!
!-----------------------------------------------------------------------
   ierror = IEOK
   if (.not. okee) then
      ierror = IEOTHR
   endif
!
   return
end

subroutine wrhcor(okee      ,hdefds    ,hdafds    ,misval    ,&
&num_rows      ,num_columns      ,noroco    ,norow     ,&
&irocol    ,kcs       ,kcu       ,kcv       ,&
&xcor      ,ycor      ,xz        ,yz        ,&
&alfas                ,dp        ,dps       ,&
&zbuff                                      )
!-----------------------------------------------------------------------
!             Module: SUBROUTINE WRHCOR
!           Function: - read NEFIS data, coordinates from file
!                     - calculate model frame
!        Method used:
!
!-----------------------------------------------------------------------
!   Calling routine :              ods_tri_nef_map_getdata
!-----------------------------------------------------------------------
!   Called  routines:              GETELT
!-----------------------------------------------------------------------
!  Formal parameters:
!  ------------------
!
!   Var. I/O  Type Dimensions
!   -------------------------
!
! DP       O  R*4  num_rows,num_columns       Depth values in depth points
! DPS      O  R*4  num_rows,num_columns       Depth values in zeta points
! OKEE    IO  L*4                  Flag for further execution program
! ALFAS    O  R*4  num_rows,num_columns       Transformation coefficients (in radians)
! HDAFDS  I   I*4  999             Data file descriptor for the MAP-DAT
!                                  file
! HDEFDS  I   I*4  2997            Definition file description for the
!                                  MAP-DEF file
! MISVAL  I   R*4                  missing value
! IROCOL   O  I*4  5,NOROCO        Pointer table with bound. coord. and
!                                  bound. types (comp. cols. and rows)
! KCS      O  I*4  num_rows,num_columns       Mask array for the zeta points
!                                  (time independent)
!                                  =0 inactive point
!                                  =1 active   point
!                                  =2 open boundary point
! KCU      O  I*4  num_rows,num_columns       Mask array for the u-velocity point
!                                  (time independent)
!                                  =0 dry      point
!                                  =1 active   point
! KCV      O  I*4  num_rows,num_columns       Mask array for the v-velocity point
!                                  (time independent)
!                                  =0 dry      point
!                                  =1 active   point
! num_columns    I   I*4                  Number of gridpoints in the x-dir.
! num_rows    I   I*4                  Number of gridpoints in the y-dir.
! NOROCO  I   I*4                  Number of Computational rows & cols
! NOROW    O  I*4                  Number of comp. rows  in IROCOL-array
! XCOR     O  R*4  num_rows,num_columns       X-coordinate in depth point
! XZ       O  R*4  num_rows,num_columns       X-coordinate in zeta point
! YCOR     O  R*4  num_rows,num_columns       Y-coordinate in depth point
! YZ       O  R*4  num_rows,num_columns       Y-coordinate in zeta point
! ZBUFF    O  R*4  num_rows,num_columns       Buffer array to read nefis files
!-----------------------------------------------------------------------
!    Local variables:
!    ----------------
!
!   Var.      Type Dimensions
!   -------------------------
!
! BUFLEN      I*4                  Buffer length
! DRYFLP     CH*4                  Drying and flooding procedure
! GRPDEF     CH*16                 Group name definition
! M           I*4                  Loop variable
! MD          I*4                  Help variable
! N           I*4                  Loop variable
! ND          I*4                  Help variable
! UINDEX      I*4  3               Array containing cell indices with
!                                  has to be read
! USRORD      I*4                  Sequence in which the cells must be
!                                  read
!-----------------------------------------------------------------------
!
!  declarations
!
   integer       num_rows  ,num_columns  ,norow ,noroco,n     ,m
   integer       nd    ,md
!
   real          misval,pi    ,degrad,dxdksi ,dydksi
!
   integer       irocol(5     ,noroco),kcs   (num_rows  ,num_columns  ),&
   &kcu   (num_rows  ,num_columns  ),kcv   (num_rows  ,num_columns  )
!
   real          xcor  (num_rows  ,num_columns  ),ycor  (num_rows  ,num_columns  ),&
   &xz    (num_rows  ,num_columns  ),yz    (num_rows  ,num_columns  ),&
   &alfas (num_rows  ,num_columns  ),&
   &dp    (num_rows  ,num_columns  ),dps   (num_rows  ,num_columns  )
   real          zbuff (num_rows  ,num_columns  )
!
   character*4   dryflp
!
   logical       okee
!-----------------------------------------------------------------------
!-----Declarations NEFIS
!-----------------------------------------------------------------------
   integer*4     hdefds( 2997),hdafds(  999)
!
   integer*4     uindex(    3),usrord,buflen, ierror
!
   character*16  grpdef
!
   integer*4     GETELT,GETELS
!
!-----------------------------------------------------------------------
   dryflp = 'NO  '
   pi = 4.0 * atan( 1.0 )
!
   do 10 m = 1,num_columns
      do 10 n = 1,num_rows
         dps   (n,m) = misval
         xz    (n,m) = misval
         yz    (n,m) = misval
10 continue
!-----------------------------------------------------------------------
!-----Initialize Nefis variables
!-----------------------------------------------------------------------
   grpdef    = 'map-const'
   uindex(1) = 1
   uindex(2) = 1
   uindex(3) = 1
   usrord    = 1
!-----------------------------------------------------------------------
!-----Read NOROW and IROCOL tabel from group 2
!-----------------------------------------------------------------------
   buflen    = 4
   okee      = okee .and.&
   &GETELT(hdefds,grpdef    ,'NOROW '  ,&
   &uindex,usrord    ,buflen    ,NOROW     )&
   &.eq. 0
!
   buflen    =  4 * 5 * noroco
   okee      = okee .and.&
   &GETELT(hdefds,grpdef    ,'IROCOL'  ,&
   &uindex,usrord    ,buflen    ,IROCOL    )&
   &.eq. 0
!-----------------------------------------------------------------------
!-----Read KCS array from group 2
!-----------------------------------------------------------------------
   buflen    =  4 * num_rows  * num_columns
   okee      = okee .and.&
   &GETELT(hdefds,grpdef    ,'KCS   '  ,&
   &uindex,usrord    ,buflen    ,KCS       )&
   &.eq. 0
!-----------------------------------------------------------------------
!-----Read KCU array from group 2
!-----------------------------------------------------------------------
   buflen    =  4 * num_rows  * num_columns
   okee      = okee .and.&
   &GETELT(hdefds,grpdef    ,'KCU   '  ,&
   &uindex,usrord    ,buflen    ,KCU       )&
   &.eq. 0
!-----------------------------------------------------------------------
!-----Read KCV array from group 2
!-----------------------------------------------------------------------
   buflen    =  4 * num_rows  * num_columns
   okee      = okee .and.&
   &GETELT(hdefds,grpdef    ,'KCV   '  ,&
   &uindex,usrord    ,buflen    ,KCV       )&
   &.eq. 0
!-----------------------------------------------------------------------
!-----Read XCOR array from group 2 in buffer and calculate min and max
!-----------------------------------------------------------------------
   buflen    =  4 * num_rows  * num_columns
   okee      = okee .and.&
   &GETELT(hdefds,grpdef    ,'XCOR  '  ,&
   &uindex,usrord    ,buflen    ,XCOR      )&
   &.eq. 0
!
!-----------------------------------------------------------------------
!-----Read YCOR array from group 2 in buffer and calculate min and max
!-----------------------------------------------------------------------
   buflen    =  4 * num_rows  * num_columns
   okee      = okee .and.&
   &GETELT(hdefds,grpdef    ,'YCOR  '  ,&
   &uindex,usrord    ,buflen    ,YCOR      )&
   &.eq. 0
!
!-----------------------------------------------------------------------
!-----Try to read ALFAS array from group 2 - if not available (old version)
!-----then compute it from XCOR and YCOR
!-----------------------------------------------------------------------
!
   buflen    =  4 * num_rows  * num_columns
   ierror    =&
   &GETELT(hdefds,grpdef    ,'ALFAS '  ,&
   &uindex,usrord    ,buflen    ,ALFAS     )

   if ( ierror .ne. 0 ) then
      do 110 m = 1,num_columns
         do 110 n = 1,num_rows
            md = max( 1,m-1 )
            nd = max( 1,n-1 )
            dxdksi = 0.5 * ( xcor(n,m) - xcor(n,md) +&
            &xcor(nd,m) - xcor(nd,md) )
            dydksi = 0.5 * ( ycor(n,m) - ycor(n,md) +&
            &ycor(nd,m) - ycor(nd,md) )
            if ( dxdksi .eq. 0. .and. dydksi .eq. 0. ) then
               alfas(n,m) = 0.0
            else
               alfas(n,m) = atan2( dydksi,dxdksi )
            endif
110   continue
   else
      degrad = pi / 180.
      do 115 m = 1,num_columns
         do 115 n = 1,num_rows
            alfas(n,m) = alfas(n,m) * degrad
115   continue
   endif

!-----------------------------------------------------------------------
!-----NOTE: do not fill in missing value, unless xcor(i,j) is 0
!-----(xcor can be and must be defined for inactive cells - kcs=0)
!-----AM (dd. 4 february 1997):
!-----This correction appears to be unnecessary and awkward anyway
!-----in case the coordinate "0.0" is a useful coordinate.
!-----------------------------------------------------------------------
!     do 120 n = 1,num_rows
!        do 120 m = 1,num_columns
!           if (abs(xcor (n,m)) .lt. 1.0e-8 ) then
!              xcor  (n,m) = misval
!           endif
!           if (abs(ycor (n,m)) .lt. 1.0e-8 ) then
!              ycor  (n,m) = misval
!           endif
! 120 continue
!
!-----------------------------------------------------------------------
!-----Read XZ array from group 2 in buffer for points with kcs <> 0
!-----------------------------------------------------------------------
   buflen    =  4 * num_rows  * num_columns
   okee      = okee .and.&
   &GETELT(hdefds,grpdef    ,'XZ    '  ,&
   &uindex,usrord    ,buflen    ,ZBUFF     )&
   &.eq. 0
!
   do 210 n = 1,num_rows
      do 210 m = 1,num_columns
         if (kcs  (n,m) .ne. 0) then
            xz    (n,m) = zbuff (n,m)
         endif
210 continue
!-----------------------------------------------------------------------
!-----Read YZ array from group 2 in buffer for points with kcs <> 0
!-----------------------------------------------------------------------
   buflen    =  4 * num_rows  * num_columns
   okee      = okee .and.&
   &GETELT(hdefds,grpdef    ,'YZ    '  ,&
   &uindex,usrord    ,buflen    ,ZBUFF     )&
   &.eq. 0
!
   do 250 n = 1,num_rows
      do 250 m = 1,num_columns
         if (kcs  (n,m) .ne. 0) then
            yz    (n,m) = zbuff (n,m)
         endif
250 continue
!-----------------------------------------------------------------------
!-----Read DP0 array from group 2 calculate min and max
!-----------------------------------------------------------------------
   buflen    =  4 * num_rows   * num_columns
   okee      = okee .and.&
   &GETELT(hdefds,grpdef    ,'DP0   '  ,&
   &uindex,usrord    ,buflen    ,DP        )&
   &.eq. 0
!-----------------------------------------------------------------------
!-----Read DRYFLP from group 2 and calculate DPS depending on value
!     of dryflp
!-----------------------------------------------------------------------
   buflen    = 4
   okee      = okee .and.&
   &GETELS&
   &(hdefds,grpdef    ,'DRYFLP'  ,&
   &uindex,usrord    ,buflen    ,DRYFLP    )&
   &.eq. 0
!
   if (dryflp .eq. 'MEAN') then
!-----------------------------------------------------------------------
!--------calculate dps depth in zeta point as the mean of 4 surrounding
!        depth points
!-----------------------------------------------------------------------
      do 410 m = 1,num_columns
         do 420 n = 1,num_rows
            md    = max(1,m-1)
            nd    = max(1,n-1)
            if (kcs  (n,m) .ne. 0) then
               dps   (n,m) = 0.25 * (dp (n ,m ) + dp (n ,md)  +&
               &dp (nd,md) + dp (nd,m ))
            endif
420      continue
410   continue
   else if (dryflp .eq. 'MIN ') then
!-----------------------------------------------------------------------
!--------calculate dps depth in zeta point as mean of 2 minima of 2
!        surrounding depth points
!-----------------------------------------------------------------------
      do 430 m = 1,num_columns
         do 440 n = 1,num_rows
            md    = max(1,m-1)
            nd    = max(1,n-1)
            if (kcs  (n,m) .ne. 0) then
               dps   (n,m) = 0.5 * (min (dp (n ,m ),dp (nd,md))  +&
               &min (dp (n ,md),dp (nd,m )))
            endif
440      continue
430   continue
   else
!-----------------------------------------------------------------------
!--------calculate dps depth in zeta point as maximum of 4 depth points
!        dryflp = 'MAX' or dryflp = 'NO'
!-----------------------------------------------------------------------
      do 450 m = 1,num_columns
         do 460 n = 1,num_rows
            md    = max(1,m-1)
            nd    = max(1,n-1)
            if (kcs  (n,m) .ne. 0) then
               dps   (n,m) = max (dp (n ,m ),dp (n ,md),&
               &dp (nd,md),dp (nd,m ))
            endif
460      continue
450   continue
   endif
!
   return
end

subroutine wrhwat(okee      ,hdefds    ,hdafds    ,parcod    ,&
&itim      ,num_rows      ,num_columns      ,&
&kcs       ,kfu       ,kfv       ,&
&dps       ,s1                              )
!-----------------------------------------------------------------------
!             Module: SUBROUTINE WRHWAT
!           Function: Get NEFIS data for time itim for
!                     parcod =  1  'wl' or
!                     parcod =  2  'wh'
!        Method used:
!
!-----------------------------------------------------------------------
!   Calling routine :              ods_tri_nef_map_getdata
!-----------------------------------------------------------------------
!   Called  routines:              GETELT (nf)
!-----------------------------------------------------------------------
!  Formal parameters:
!  ------------------
!
!   Var. I/O  Type Dimensions
!   -------------------------
!
! DPS     I   R*4  num_rows,num_columns       Depth in zeta point
! OKEE     O  L*4                  Flag for further execution program
! ITIM    I   I*4                  Specified time
! KCS     I   I*4  num_rows,num_columns       Mask array for the zeta points
!                                  (time independent)
!                                  =0 inactive point
!                                  =1 active   point
!                                  =2 open boundary point
! KFU     I   I*4  num_rows,num_columns       Mask array for the u-velocity point
!                                  (time dependent)
!                                  =0 dry      point
!                                  =1 active   point
! KFV     I   I*4  num_rows,num_columns       Mask array for the v-velocity point
!                                  (time dependent)
!                                  =0 dry      point
!                                  =1 active   point
! HDAFDS  I   I*4  999             Data file descriptor for the MAP-DAT
!                                  file
! HDEFDS  I   I*4  2997            Definition file description for the
!                                  MAP-DEF file
! num_columns    I   I*4                  Number of gridpoints in the x-dir.
! num_rows    I   I*4                  Number of gridpoints in the y-dir.
! S1       O  R*4  num_rows,num_columns       Water-level or waterhight in zeta pnt
! PARCOD  I   I*4                  Code for parameter
!-----------------------------------------------------------------------
!    Local variables:
!    ----------------
!
!   Var.      Type Dimensions
!   -------------------------
!
! BUFLEN      I*4                  Buffer length
! GRPDEF     CH*16                 Group name definition
! LVAR        L*4                  Help var.
! M           I*4                  Help var.
! N           I*4                  Help var.
! UINDEX      I*4  3               Array containing cell indices with
!                                  has to be read
! USRORD      I*4                  Sequence in which the cells must be
!                                  read
!-----------------------------------------------------------------------
!
!  DECLARATIONS
!
   integer       num_rows  ,num_columns  ,parcod,itim
   integer       n     ,m     ,nd    ,md
!
   integer       kcs   (num_rows  ,num_columns  ),&
   &kfu   (num_rows  ,num_columns  ),kfv   (num_rows  ,num_columns  )
!
   real          dps   (num_rows  ,num_columns  ),s1    (num_rows  ,num_columns  )
!
   logical       okee  ,lvar
!-----------------------------------------------------------------------
!-----Declaraties NEFIS
!-----------------------------------------------------------------------
   integer*4     hdefds( 2997),hdafds(  999)
!
   integer*4     uindex(    3),usrord,buflen
!
   character*16  grpdef
!
   integer*4     GETELT,GETELS
!--------------------------------------------------------------------
!-----Initialize Nefis variables
!--------------------------------------------------------------------
   grpdef    = 'map-series'
   usrord    = 1
   buflen    = 4 * num_rows   * num_columns
   uindex(1) = itim
   uindex(2) = itim
   uindex(3) = 1
!--------------------------------------------------------------------
!-----Read from group 3 S1
!--------------------------------------------------------------------
   okee      = okee .and.&
   &GETELT(hdefds   ,grpdef    ,'S1    '  ,&
   &uindex   ,usrord ,buflen    ,S1        )&
   &.eq. 0
!--------------------------------------------------------------------
   if ( parcod .eq. 2) then
!-----------------------------------------------------------------------
!--------Calculate WH
!        if kcs = 0 then permanent drypoint
!        else if surrounding kfu and kfv = 0 then temporary drypoint
!-----------------------------------------------------------------------
      do 110 n = 1,num_rows
         do 110 m = 1,num_columns
            md    = max (1,m-1)
            nd    = max (1,n-1)
!              lvar  = ((kcs   (n ,m ) .eq. 1) .and.
!    *                  (kfu   (n ,m ) .eq. 1  .or.
!    *                   kfu   (n ,md) .eq. 1  .or.
!    *                   kfv   (n ,m ) .eq. 1  .or.
!    *                   kfv   (nd,m ) .eq. 1))
            lvar  = ((kcs   (n ,m ) .eq. 1))
            if (lvar  ) then
               s1    (n,m) = s1    (n,m) + dps   (n,m)
            else
               s1    (n,m) = 0.0
            endif
110   continue
   endif
!
   return
end

subroutine wrh3di(okee      ,hdefds    ,hdafds    ,parcod    ,&
&grdang    ,itim      ,misval    ,&
&num_rows      ,num_columns      ,num_layers_grid      ,&
&kcs       ,kfu       ,kfv       ,&
&xcor      ,ycor      ,alfas     ,&
&u1        ,v1        ,thick                )
!-----------------------------------------------------------------------
!             Module: SUBROUTINE WRH3DI
!           Function: get NEFIS data for time itim for
!                     parcod =  3  'du' or
!                     parcod =  4  'dv' or
!                     parcod =  5  'dm' or
!                     parcod =  6  'dd'
!        Method used:
!
!-----------------------------------------------------------------------
!   Calling routine :              ods_tri_nef_map_getdata
!-----------------------------------------------------------------------
!   Called  routines:              GETELT (nf)
!-----------------------------------------------------------------------
!  Formal parameters:
!  ------------------
!
!   Var. I/O  Type Dimensions
!   -------------------------
!
! GRDANG  I   R*4                  Vertex between the y-axis and north
! GUU     I   R*4  num_rows,num_columns       Mean value for distance coefficients
! GVV     I   R*4  num_rows,num_columns       Mean value for distance coefficients
! OKEE     O  L*4                  Flag for further execution program
! ITIM    I   I*4                  Specified time
! MISVAL  I   R*4                  missing value
! PARCOD  I   I*4                  Parameter code
! KCS     I   I*4  num_rows,num_columns       Mask array for the zeta points
!                                  (time independent)
!                                  =0 inactive point
!                                  =1 active   point
!                                  =2 open boundary point
! KFU     I   I*4  num_rows,num_columns       Mask array for the u-velocity point
!                                  (time dependent)
!                                  =0 dry      point
!                                  =1 active   point
! KFV     I   I*4  num_rows,num_columns       Mask array for the v-velocity point
!                                  (time dependent)
!                                  =0 dry      point
!                                  =1 active   point
! num_layers_grid    I   I*4                  Number of layers
! HDAFDS  I   I*4  999             Data file descriptor for the MAP-DAT
!                                  file
! HDEFDS  I   I*4  2997            Definition file description for the
!                                  MAP-DEF file
! num_columns    I   I*4                  Number of gridpoints in the x-dir.
! num_rows    I   I*4                  Number of gridpoints in the y-dir.
! THICK   I   R*4  num_layers_grid            Relative layer thickness
! U1       O  R*4  num_rows,num_columns,num_layers_grid  U-velocity in u-velocity point
! V1       O  R*4  num_rows,num_columns,num_layers_grid  V-velocity in v-velocity point
! XCOR    I   R*4  num_rows,num_columns       X-distance between 2 grid lines
!                                  around a zeta point in y-direction
! YCOR    I   R*4  num_rows,num_columns       Y-distance between 2 grid lines
!                                  around a zeta point in y-direction
!-----------------------------------------------------------------------
!    Local variables:
!    ----------------
!
!   Var.      Type Dimensions
!   -------------------------
!
! BUFLEN      I*4                  Buffer length
! DUVD        R*4                  Depth mean velocity direction
! DUVM        R*4                  Depth mean velocity magnitude
! DUZ         R*4                  Depth mean u-velocity in a zeta
!                                  point
! DVZ         R*4                  Depth mean v-velocity in a zeta
!                                  point
! EPS         R*4                  Small value to test for backwards
!                                  transformation
! GRPDEF     CH*16                 Group name definition
! GUUGEM      R*4                  Help var. 'mean' GUU value
! GVVGEM      R*4                  Help var. 'mean' GVV value
! KENMU       I*4                  Mask value for two consecutive u-vel.
!                                  points (max 1)
! KENMV       I*4                  Mask value for two consecutive v-vel.
!                                  points (max 1)
! KFS         I*4                  Temporary dry point
! LVAR        L*4                  Help var.
! M           I*4                  Loop var. 1-num_columns
! MD          I*4                  Max (1,M-1)
! MM          I*4                  Max (2,M)
! MMD         I*4                  MM-1
! N           I*4                  Loop var. 1-num_rows
! ND          I*4                  Max (1,N-1)
! NN          I*4                  Max (2,N)
! NND         I*4                  NN-1
! PI          R*4                  Value for pi (3.14 etc.)
! UGEM        R*4                  Help var. 'mean' u-velocity value
! UINDEX      I*4  3               Array containing cell indices with
!                                  has to be read
! UM          R*4                  Help var. for u-vel. in u-point M
! UMD         R*4                  Help var. for u-vel. in u-point MD
! USRORD      I*4                  Sequence in which the cells must be
!                                  read
! VGEM        R*4                  Help var. 'mean' v-velocity value
! VN          R*4                  Help var. for v-vel. in v-point N
! VND         R*4                  Help var. for v-vel. in v-point ND
!                                  outside model
! ZDEF        R*4                  Default value for z value outside
!                                  model
!-----------------------------------------------------------------------
!
!  declarations
!
   integer       num_rows  ,num_columns  ,num_layers_grid  ,parcod,itim
   integer       n     ,m     ,nd    ,md
   integer       k
!
   real          grdang,pi    ,eps   ,misval,duz   ,dvz
   real          duvm  ,duvd  ,hulp
   real          um    ,umd   ,vn    ,vnd
   real          ugem  ,vgem  ,totthk
!
   integer       kcs   (num_rows  ,num_columns  ),&
   &kfu   (num_rows  ,num_columns  ),kfv   (num_rows  ,num_columns  )
!
   real          xcor  (num_rows  ,num_columns  ),ycor  (num_rows  ,num_columns  ),&
   &alfas (num_rows  ,num_columns  )
   real          u1    (num_rows  ,num_columns   ,num_layers_grid  ),&
   &v1    (num_rows  ,num_columns   ,num_layers_grid  ),&
   &thick (num_layers_grid  )
!
   logical       okee  ,lvar
!-----------------------------------------------------------------------
!-----Declarations NEFIS
!-----------------------------------------------------------------------
   integer*4     hdefds( 2997),hdafds(  999)
!
   integer*4     uindex(    3),usrord,buflen
!
   character*16  grpdef
!
   integer*4     GETELT,GETELS
!-----------------------------------------------------------------------
!-----General initialisation
!-----------------------------------------------------------------------
   pi     = atan (1.0) * 4.
   eps    = 1.e-12
!--------------------------------------------------------------------
!-----Initialize Nefis variables
!--------------------------------------------------------------------
   grpdef    = 'map-series'
   usrord    = 1
   buflen    = 4 * num_rows   * num_columns   * num_layers_grid
   uindex(1) = itim
   uindex(2) = itim
   uindex(3) = 1
!--------------------------------------------------------------------
!-----Read from group 3 U1
!--------------------------------------------------------------------
   okee      = okee .and.&
   &GETELT(hdefds   ,grpdef    ,'U1    '  ,&
   &uindex   ,usrord ,buflen    ,U1        )&
   &.eq. 0
!--------------------------------------------------------------------
!-----Read from group 3 V1
!--------------------------------------------------------------------
   okee      = okee .and.&
   &GETELT(hdefds   ,grpdef    ,'V1    '  ,&
   &uindex   ,usrord ,buflen    ,V1        )&
   &.eq. 0
!--------------------------------------------------------------------
!-----Calculate DUZ, DVZ, DUVM and DUVD
!     if kcs = 0 then permanent drypoint
!     else if surrounding kfu and kfv = 0 then temporary drypoint
!--------------------------------------------------------------------
   do 110 n = num_rows, 1, -1
      do 110 m = num_columns, 1, -1
         md    = max (1,m-1)
         nd    = max (1,n-1)
         lvar  = ((kcs   (n ,m ) .eq. 1) .and.&
         &(kfu   (n ,m ) .eq. 1  .or.&
         &kfu   (n ,md) .eq. 1  .or.&
         &kfv   (n ,m ) .eq. 1  .or.&
         &kfv   (nd,m ) .eq. 1))
         if (lvar  ) then
!--------------------------------------------------------------------
!--------------Sum U1 and V1, then backwards transformation and
!              calculation of DUZ and DVZ
!--------------------------------------------------------------------
            um     = 0.
            umd    = 0.
            vn     = 0.
            vnd    = 0.
            totthk = 0.
            do 120 k = 1,num_layers_grid
               if ( u1(n,m,k)  .ne. -999.0 .and.&
               &u1(n,md,k) .ne. -999.0 .and.&
               &v1(nd,m,k) .ne. -999.0 .and.&
               &v1(n,m,k)  .ne. -999.0       ) then
                  um     = um     + u1    (n ,m ,k ) * thick (k)
                  umd    = umd    + u1    (n ,md,k ) * thick (k)
                  vn     = vn     + v1    (n ,m ,k ) * thick (k)
                  vnd    = vnd    + v1    (nd,m ,k ) * thick (k)
                  totthk = totthk + thick(k)
               endif
120         continue
!
            if ( totthk .ne. 0.0 ) then
               um     = um     * kfu   (n ,m )
               umd    = umd    * kfu   (n ,md)
               vn     = vn     * kfv   (n ,m )
               vnd    = vnd    * kfv   (nd,m )
               ugem   = 0.5 * (um   + umd  ) * kcs(n,m) / totthk
               vgem   = 0.5 * (vn   + vnd  ) * kcs(n,m) / totthk
               duz    =  ugem * cos( alfas(n,m) )  -&
               &vgem * sin( alfas(n,m) )
               dvz    =  ugem * sin( alfas(n,m) )  +&
               &vgem * cos( alfas(n,m) )
!--------------------------------------------------------------------
!--------------Calculate DUVM and DUVD
!              duvd should be defined between 0. and 360. degrees
!              atan2 <-180,180>, grdang [0,360] => mod (.. + 720)
!
!        Note (AM, dd 31 march 1999)
!        The sign before grdang was -, this has been adjusted, though
!        in most cases grdang is zero anyway.
!--------------------------------------------------------------------
               duvm   = sqrt ( duz    * duz    + dvz    * dvz   )
               if (abs (duz   ) .lt. eps) then
                  duz    = eps
               endif
               if (abs (dvz   ) .lt. eps) then
                  dvz    = eps
               endif
               hulp   =    90. - atan2 (dvz   ,duz   ) * 180. / pi +&
               &grdang
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
!
110 continue
   return
end

subroutine wrhuvi(okee      ,hdefds    ,hdafds    ,parcod    ,&
&grdang    ,itim      ,misval    ,&
&num_rows      ,num_columns      ,num_layers_grid      ,&
&kcs       ,kfu       ,kfv       ,&
&xcor      ,ycor      ,&
&alfas                ,u1        ,v1        )
!-----------------------------------------------------------------------
!             Module: SUBROUTINE WRHUVI
!           Function: get NEFIS data for time itim for
!                     parcod =  7  'u ',
!                     parcod =  8  'v ',
!                     parcod =  9  'm ',
!                     parcod = 10  'd ',
!        Method used:
!
!-----------------------------------------------------------------------
!   Calling routine :              ods_tri_nef_map_getdata
!-----------------------------------------------------------------------
!   Called  routines:              GETELT (nf)
!-----------------------------------------------------------------------
!  Formal parameters:
!  ------------------
!
!   Var. I/O  Type Dimensions
!   -------------------------
!
! GRDANG  I   R*4                  Vertex between the y-axis and north
! ALFAS   I   R*4  num_rows,num_columns       Transformation coefficients (in radians)
! GVV     I   R*4  num_rows,num_columns       Mean value for distance coefficients
! OKEE     O  L*4                  Flag for further execution program
! ITIM    I   I*4                  Specified time
! MISVAL  I   R*4                  missing value
! KCS     I   I*4  num_rows,num_columns       Mask array for the zeta points
!                                  (time independent)
!                                  =0 inactive point
!                                  =1 active   point
!                                  =2 open boundary point
! KFU     I   I*4  num_rows,num_columns       Mask array for the u-velocity point
!                                  (time dependent)
!                                  =0 dry      point
!                                  =1 active   point
! KFV     I   I*4  num_rows,num_columns       Mask array for the v-velocity point
!                                  (time dependent)
!                                  =0 dry      point
!                                  =1 active   point
! num_layers_grid    I   I*4                  Number of layers
! HDAFDS  I   I*4  999             Data file descriptor for the MAP-DAT
!                                  file
! HDEFDS  I   I*4  2997            Definition file description for the
!                                  MAP-DEF file
! num_columns    I   I*4                  Number of gridpoints in the x-dir.
! num_rows    I   I*4                  Number of gridpoints in the y-dir.
! PARCOD  I   I*4                  Parameter code
! U1       O  R*4  num_rows,num_columns,num_layers_grid  U-velocity in u-velocity point
! V1       O  R*4  num_rows,num_columns,num_layers_grid  V-velocity in v-velocity point
! XCOR    I   R*4  num_rows,num_columns       X-distance between 2 grid lines
!                                  around a zeta point in y-direction
! YCOR    I   R*4  num_rows,num_columns       Y-distance between 2 grid lines
!                                  around a zeta point in y-direction
!-----------------------------------------------------------------------
!    Local variables:
!    ----------------
!
!   Var.      Type Dimensions
!   -------------------------
!
! BUFLEN      I*4                  Buffer length
! EPS         R*4                  Small value to test for backwards
!                                  transformation
! GRPDEF     CH*16                 Group name definition
! GUUGEM      R*4                  Help var. 'mean' GUU value
! GVVGEM      R*4                  Help var. 'mean' GVV value
! KENMU       I*4                  Mask value for two consecutive u-vel.
!                                  points (max 1)
! KENMV       I*4                  Mask value for two consecutive v-vel.
!                                  points (max 1)
! KFS         I*4                  Temporary dry point
! LVAR        L*4                  Help var.
! M           I*4                  Loop var. 1-num_columns
! MD          I*4                  Max (1,M-1)
! MM          I*4                  Max (2,M)
! MMD         I*4                  MM-1
! N           I*4                  Loop var. 1-num_rows
! ND          I*4                  Max (1,N-1)
! NN          I*4                  Max (2,N)
! NND         I*4                  NN-1
! PI          R*4                  Value for pi (3.14 etc.)
! UGEM        R*4                  Help var. 'mean' u-velocity value
! UINDEX      I*4  3               Array containing cell indices with
!                                  has to be read
! UM          R*4                  Help var. for u-vel. in u-point M
! UMD         R*4                  Help var. for u-vel. in u-point MD
! USRORD      I*4                  Sequence in which the cells must be
!                                  read
! UVD         R*4                  Velocity direction
! UVM         R*4                  Velocity magnitude
! UZ          R*4                  U-velocity in a zeta point
! VGEM        R*4                  Help var. 'mean' v-velocity value
! VN          R*4                  Help var. for v-vel. in v-point N
! VND         R*4                  Help var. for v-vel. in v-point ND
! VZ          R*4                  V-velocity in a zeta point
! XYDEF       R*4                  Default value for x,y coordinate
!                                  outside model
! ZDEF        R*4                  Default value for z value outside
!                                  model
!-----------------------------------------------------------------------
!
!  declarations
!
   integer       num_rows  ,num_columns  ,num_layers_grid  ,parcod,itim
   integer       n     ,m     ,nd    ,md    ,k
!
   real          grdang,pi    ,eps   ,misval,uz    ,vz
   real          uvm   ,uvd   ,hulp
   real          um    ,umd   ,vn    ,vnd
   real          ugem  ,vgem
!
   integer       kcs   (num_rows  ,num_columns  ),&
   &kfu   (num_rows  ,num_columns  ),kfv   (num_rows  ,num_columns  )
!
   real          xcor  (num_rows  ,num_columns  ),ycor  (num_rows  ,num_columns  ),&
   &alfas (num_rows  ,num_columns  )
   real          u1    (num_rows  ,num_columns   ,num_layers_grid  ),&
   &v1    (num_rows  ,num_columns   ,num_layers_grid  )
!
   logical       okee  ,lvar, zactiv
!-----------------------------------------------------------------------
!-----Declarations NEFIS
!-----------------------------------------------------------------------
   integer*4     hdefds( 2997),hdafds(  999)
!
   integer*4     uindex(    3),usrord,buflen
!
   character*16  grpdef
!
   integer*4     GETELT,GETELS
!-----------------------------------------------------------------------
!-----General initialisation
!-----------------------------------------------------------------------
   pi     = atan (1.0) * 4.
   eps    = 1.e-12
!--------------------------------------------------------------------
!-----Initialize Nefis variables
!--------------------------------------------------------------------
   grpdef    = 'map-series'
   usrord    = 1
   buflen    = 4 * num_rows   * num_columns   * num_layers_grid
   uindex(1) = itim
   uindex(2) = itim
   uindex(3) = 1
!-----------------------------------------------------------------------
!--------Read from  group 3 U1
!-----------------------------------------------------------------------
   okee      = okee .and.&
   &GETELT(hdefds   ,grpdef    ,'U1    '  ,&
   &uindex   ,usrord ,buflen    ,U1        )&
   &.eq. 0
!-----------------------------------------------------------------------
!--------Read from group 3 V1
!-----------------------------------------------------------------------
   okee      = okee .and.&
   &GETELT(hdefds   ,grpdef    ,'V1    '  ,&
   &uindex   ,usrord ,buflen    ,V1        )&
   &.eq. 0
!--------------------------------------------------------------------
!-----Calculate UZ, VZ, UVM and UVD
!     if kcs = 0 then permanent drypoint; if surrounding kfu and kfv
!     := 0 then temporary drypoint
!--------------------------------------------------------------------
   do 110 n = num_rows, 1, -1
      do 110 m = num_columns, 1, -1
         md    = max (1,m-1)
         nd    = max (1,n-1)
         lvar  = ((kcs   (n ,m ) .eq. 1) .and.&
         &(kfu   (n ,m ) .eq. 1  .or.&
         &kfu   (n ,md) .eq. 1  .or.&
         &kfv   (n ,m ) .eq. 1  .or.&
         &kfv   (nd,m ) .eq. 1))
         do 110 k = 1, num_layers_grid
            zactiv = .false.
            if ( lvar ) then
               if ( u1(n,m,k)  .eq. -999.0 .or.&
               &u1(n,md,k) .eq. -999.0 .or.&
               &v1(nd,m,k) .eq. -999.0 .or.&
               &v1(n,m,k)  .eq. -999.0      ) then
                  zactiv = .false.
               else
                  zactiv = .true.
               endif
            endif
            if ( zactiv ) then
!-----------------------------------------------------------------------
!-----------------Backwards transformation of U1 and V1 and
!                 calculation of UZ and VZ
!-----------------------------------------------------------------------
               um     = u1    (n ,m ,k )  * kfu   (n ,m )
               umd    = u1    (n ,md,k )  * kfu   (n ,md)
               vn     = v1    (n ,m ,k )  * kfv   (n ,m )
               vnd    = v1    (nd,m ,k )  * kfv   (nd,m )
!
               ugem   = 0.5 * (um   + umd ) * kcs(n,m)
               vgem   = 0.5 * (vn   + vnd ) * kcs(n,m)
!
               uz     = ugem * cos( alfas(n,m) )  -&
               &vgem * sin( alfas(n,m) )
               vz     = ugem * sin( alfas(n,m) )  +&
               &vgem * cos( alfas(n,m) )
!-----------------------------------------------------------------------
!-----------------Calculate UVM and UVD
!                 uvd should be defined between 0. and 360. degrees
!                 atan2 <-180,180>, grdang [0,360] => mod (.. + 720)
!
!        Note (AM, dd 31 march 1999)
!        The sign before grdang was -, this has been adjusted, though
!        in most cases grdang is zero anyway.
!-----------------------------------------------------------------------
               uvm    = sqrt ( uz     * uz     + vz     * vz    )
               if (abs (uz    ) .lt. eps) then
                  uz     = eps
               endif
               if (abs (vz    ) .lt. eps) then
                  vz     = eps
               endif
               hulp   =    90. - atan2 (vz    ,uz    ) * 180. / pi +&
               &grdang
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

subroutine wrh3dv(okee      ,hdefds    ,hdafds    ,misval    ,&
&itim      ,num_rows      ,num_columns      ,num_layers_grid      ,&
&kcs       ,kfu       ,kfv       ,&
&xcor      ,ycor      ,alfas     ,&
&u1        ,v1        ,thick                )
!-----------------------------------------------------------------------
!             Module: SUBROUTINE WRH3DV
!           Function: get NEFIS data for time itim for
!                     paramete = 38  'df'
!        Method used:
!
!-----------------------------------------------------------------------
!   Calling routine :              ods_tri_nef_map_getdata
!-----------------------------------------------------------------------
!   Called  routines:              GETELT (nf)
!-----------------------------------------------------------------------
!  Formal parameters:
!  ------------------
!
!   Var. I/O  Type Dimensions
!   -------------------------
!
! ALFAS   I   R*4  num_rows,num_columns       Transformation coefficients (in radians)
! OKEE     O  L*4                  Flag for further execution program
! ITIM    I   I*4                  Specified time
! MISVAL  I   R*4                  missing value
! KCS     I   I*4  num_rows,num_columns       Mask array for the zeta points
!                                  (time independent)
!                                  =0 inactive point
!                                  =1 active   point
!                                  =2 open boundary point
! KFU     I   I*4  num_rows,num_columns       Mask array for the u-velocity point
!                                  (time dependent)
!                                  =0 dry      point
!                                  =1 active   point
! KFV     I   I*4  num_rows,num_columns       Mask array for the v-velocity point
!                                  (time dependent)
!                                  =0 dry      point
!                                  =1 active   point
! num_layers_grid    I   I*4                  Number of layers
! HDAFDS  I   I*4  999             Data file descriptor for the MAP-DAT
!                                  file
! HDEFDS  I   I*4  2997            Definition file description for the
!                                  MAP-DEF file
! num_columns    I   I*4                  Number of gridpoints in the x-dir.
! num_rows    I   I*4                  Number of gridpoints in the y-dir.
! THICK   I   R*4  num_layers_grid            Relative layer thickness
! U1       O  R*4  num_rows,num_columns,num_layers_grid  U-velocity in u-velocity point
! V1       O  R*4  num_rows,num_columns,num_layers_grid  V-velocity in v-velocity point
! XCOR    I   R*4  num_rows,num_columns       X-distance between 2 grid lines
!                                  around a zeta point in y-direction
! YCOR    I   R*4  num_rows,num_columns       Y-distance between 2 grid lines
!                                  around a zeta point in y-direction
!-----------------------------------------------------------------------
!    Local variables:
!    ----------------
!
!   Var.      Type Dimensions
!   -------------------------
!
! BUFLEN      I*4                  Buffer length
! DUZ         R*4                  Depth mean u-velocity in a zeta
!                                  point
! DVZ         R*4                  Depth mean v-velocity in a zeta
!                                  point
! EPS         R*4                  Small value to test for backwards
!                                  transformation
! GRPDEF     CH*16                 Group name definition
! GUUGEM      R*4                  Help var. 'mean' GUU value
! GVVGEM      R*4                  Help var. 'mean' GVV value
! KENMU       I*4                  Mask value for two consecutive u-vel.
!                                  points (max 1)
! KENMV       I*4                  Mask value for two consecutive v-vel.
!                                  points (max 1)
! LVAR        L*4                  Help var.
! M           I*4                  Loop var. 1-num_columns
! MD          I*4                  Max (1,M-1)
! MM          I*4                  Max (2,M)
! MMD         I*4                  MM-1
! N           I*4                  Loop var. 1-num_rows
! ND          I*4                  Max (1,N-1)
! NN          I*4                  Max (2,N)
! NND         I*4                  NN-1
! UGEM        R*4                  Help var. 'mean' u-velocity value
! UINDEX      I*4  3               Array containing cell indices with
!                                  has to be read
! UM          R*4                  Help var. for u-vel. in u-point M
! UMD         R*4                  Help var. for u-vel. in u-point MD
! USRORD      I*4                  Sequence in which the cells must be
!                                  read
! VGEM        R*4                  Help var. 'mean' v-velocity value
! VN          R*4                  Help var. for v-vel. in v-point N
! VND         R*4                  Help var. for v-vel. in v-point ND
! ZDEF        R*4                  Default value for z value outside
!                                  model
!-----------------------------------------------------------------------
!
!  declarations
!
   integer       num_rows  ,num_columns  ,num_layers_grid  ,itim
   integer       n     ,m     ,nd    ,md    ,k
!
   real          eps   ,misval,duz   ,dvz
   real          um    ,umd   ,vn    ,vnd
   real          ugem  ,vgem
!
   integer       kcs   (num_rows  ,num_columns  ),&
   &kfu   (num_rows  ,num_columns  ),kfv   (num_rows  ,num_columns  )
!
   real          xcor  (num_rows  ,num_columns  ),ycor  (num_rows  ,num_columns  ),&
   &alfas (num_rows  ,num_columns  )
   real          u1    (num_rows  ,num_columns   ,num_layers_grid  ),&
   &v1    (num_rows  ,num_columns   ,num_layers_grid  ),&
   &thick (num_layers_grid  )
!
   logical       okee  ,lvar
!-----------------------------------------------------------------------
!-----Declarations NEFIS
!-----------------------------------------------------------------------
   integer*4     hdefds( 2997),hdafds(  999)
!
   integer*4     uindex(    3),usrord,buflen
!
   character*16  grpdef
!
   integer*4     GETELT,GETELS
!-----------------------------------------------------------------------
!-----General initialisation
!-----------------------------------------------------------------------
   eps    = 1.e-12
!--------------------------------------------------------------------
!-----Initialize Nefis variables
!--------------------------------------------------------------------
   grpdef    = 'map-series'
   usrord    = 1
   buflen    = 4 * num_rows   * num_columns   * num_layers_grid
   uindex(1) = itim
   uindex(2) = itim
   uindex(3) = 1
!--------------------------------------------------------------------
!-----Read from group 3 U1
!--------------------------------------------------------------------
   okee      = okee .and.&
   &GETELT(hdefds   ,grpdef    ,'U1    '  ,&
   &uindex   ,usrord ,buflen    ,U1        )&
   &.eq. 0
!--------------------------------------------------------------------
!-----Read from group 3 V1
!--------------------------------------------------------------------
   okee      = okee .and.&
   &GETELT(hdefds   ,grpdef    ,'V1    '  ,&
   &uindex   ,usrord ,buflen    ,V1        )&
   &.eq. 0
!--------------------------------------------------------------------
!-----Calculate DUZ and DVZ
!     if kcs = 0 then permanent drypoint
!     else if surrounding kfu and kfv = 0 then temporary drypoint
!--------------------------------------------------------------------
   do 110 n = num_rows, 1, -1
      do 110 m = num_columns, 1, -1
         md    = max (1,m-1)
         nd    = max (1,n-1)
         lvar  = ((kcs   (n ,m ) .ne. 0) .and.&
         &(kfu   (n ,m ) .eq. 1  .or.&
         &kfu   (n ,md) .eq. 1  .or.&
         &kfv   (n ,m ) .eq. 1  .or.&
         &kfv   (nd,m ) .eq. 1))
         if (lvar  ) then
!--------------------------------------------------------------------
!--------------Summon U1 and V1, then backwards transformation and
!              calculation of DUZ and DVZ
!--------------------------------------------------------------------
            um     = 0.
            umd    = 0.
            vn     = 0.
            vnd    = 0.
            do 120 k = 1,num_layers_grid
               um  = um     + u1    (n ,m ,k ) * thick (k)
               umd = umd    + u1    (n ,md,k ) * thick (k)
               vn  = vn     + v1    (n ,m ,k ) * thick (k)
               vnd = vnd    + v1    (nd,m ,k ) * thick (k)
120         continue
!
            um     = um     * kfu   (n ,m )
            umd    = umd    * kfu   (n ,md)
            vn     = vn     * kfv   (n ,m )
            vnd    = vnd    * kfv   (nd,m )
!
            ugem   = 0.5 * (um   + umd  ) * kcs(n,m)
            vgem   = 0.5 * (vn   + vnd  ) * kcs(n,m)
!
            duz    =  ugem  * cos( alfas(n,m) ) -&
            &vgem  * sin( alfas(n,m) )
            dvz    =  ugem  * sin( alfas(n,m) ) +&
            &vgem  * cos( alfas(n,m) )
!
            u1    (n ,m , 1) = duz
            v1    (n ,m , 1) = dvz
         else
            u1    (n ,m , 1) = misval
            v1    (n ,m , 1) = misval
         endif
110 continue
!
   return
end

subroutine wrhcon(okee      ,hdefds    ,hdafds    ,itim      ,&
&xdata     ,maxdim    ,parcod    ,lay       ,&
&misval    ,&
&num_rows      ,num_columns      ,num_layers_grid      ,lmax      ,&
&kcs       ,kfu       ,kfv       ,r1        )
!-----------------------------------------------------------------------
!             Module: SUBROUTINE WRHCON
!           Function: get NEFIS data for time itim for
!                     parameter = 2.  'c '
!        Method used:
!
!-----------------------------------------------------------------------
!   Calling routine :              ods_tri_nef_map_getdata
!-----------------------------------------------------------------------
!   Called  routines:              GETELT (nf)
!-----------------------------------------------------------------------
!  Formal parameters:
!  ------------------
!
!   Var. I/O  Type Dimensions
!   -------------------------
!
! OKEE     O  L*4                  Flag for further execution program
! ITIM    I   I*4                  Specified time
! MISVAL  I   R*4                  missing value
! ISTOF   I   I*4                  Number of constITUENT TO BE PLOTTED
! KCS     I   I*4  num_rows,num_columns       Mask array for the zeta points
!                                  (time independent)
!                                  =0 inactive point
!                                  =1 active   point
!                                  =2 open boundary point
! KFU     I   I*4  num_rows,num_columns       Mask array for the u-velocity point
!                                  (time dependent)
!                                  =0 dry      point
!                                  =1 active   point
! KFV     I   I*4  num_rows,num_columns       Mask array for the v-velocity point
!                                  (time dependent)
!                                  =0 dry      point
!                                  =1 active   point
! num_layers_grid    I   I*4                  Number of layers
! LMAX    I   I*4                  Number of constituents
! HDAFDS  I   I*4  999             Data file descriptor for the MAP-DAT
!                                  file
! HDEFDS  I   I*4  2997            Definition file description for the
!                                  MAP-DEF file
! num_columns    I   I*4                  Number of gridpoints in the x-dir.
! NAMCON  I  CH*20 LMAX            Name of constituent
! num_rows    I   I*4                  Number of gridpoints in the y-dir.
! R1       O  R*4  num_rows,num_columns,num_layers_grid,LMAX
!                                  Concentrations in zeta point
! LAY         I*4                  Actual layer number
! MAXDIM      I*4            I     length of data array
! PARCOD  I   I*4                  parameter to get data of
! XDATA       R*4   maxdim   O     array with the data
!-----------------------------------------------------------------------
!    Local variables:
!    ----------------
!
!   Var.      Type Dimensions
!   -------------------------
!
! BUFLEN      I*4                  Buffer length
! FMT        CH*46                 Help var. for write format
! FMTDEF     CH*46                 Help var. for write format default
!                                  values
! GRPDEF     CH*16                 Group name definition
! KFS         I*4                  Temporary dry point
! LVAR        L*4                  Help var.
! K           I*4                  Help var.
! L           I*4                  Help var.
! M           I*4                  Help var.
! N           I*4                  Help var.
! N1          I*4                  Help variable
! NRBLOK      I*4                  Blocknumber in data file
! OLDID      CH*3                  Old run-identification
! UINDEX      I*4  3               Array containing cell indices with
!                                  has to be read
! USRORD      I*4                  Sequence in which the cells must be
!                                  read
! ZDEF        R*4                  Default value for z value outside
!                                  model
!-----------------------------------------------------------------------
!
!  DECLARATIONS
!
   integer       num_rows  ,num_columns  ,num_layers_grid  ,lmax  ,itim
   integer       n     ,m     ,k     ,l
   integer       nd    ,md    ,n1    ,maxdim,parcod
   integer       lay(3)
!
   real          misval
   real          xdata (maxdim)
!
   integer       kcs   (num_rows  ,num_columns  ),&
   &kfu   (num_rows  ,num_columns  ),kfv   (num_rows  ,num_columns  )
!
   real          r1    (num_rows  ,num_columns   ,num_layers_grid  ,lmax  )
!
   logical       okee  ,lvar
!-----------------------------------------------------------------------
!-----Declarations NEFIS
!-----------------------------------------------------------------------
   integer*4     hdefds( 2997),hdafds(  999)
!
   integer*4     uindex(    3),usrord,buflen
!
   character*16  grpdef
!
   integer*4     GETELT,GETELS
!--------------------------------------------------------------------
!-----Initialize Nefis variables
!--------------------------------------------------------------------
   grpdef    = 'map-series'
   usrord    = 1
   buflen    = 4 * num_rows   * num_columns   * num_layers_grid   * lmax
   uindex(1) = itim
   uindex(2) = itim
   uindex(3) = 1
!--------------------------------------------------------------------
!-----Read from group 3 R1
!--------------------------------------------------------------------
   okee      = okee .and.&
   &GETELT(hdefds   ,grpdef    ,'R1    '  ,&
   &uindex   ,usrord ,buflen    ,R1        )&
   &.eq. 0
!--------------------------------------------------------------------
!-----Write R1 for all constituents
!     if kcs = 0 then permanent drypoint; if surrounding kfu and kfv
!     := 0 then temporary drypoint
!--------------------------------------------------------------------
   do 110 n = 1,num_rows
      do 110 m = 1,num_columns
         md    = max (1,m-1)
         nd    = max (1,n-1)
         lvar  = (kcs   (n ,m ) .eq. 1)
!           lvar  = ((kcs   (n ,m ) .eq. 1) .and.
!    *               (kfu   (n ,m ) .eq. 1  .or.
!    *                kfu   (n ,md) .eq. 1  .or.
!    *                kfv   (n ,m ) .eq. 1  .or.
!    *                kfv   (nd,m ) .eq. 1))
         if (.not. lvar  ) then
            do 100 k = 1,num_layers_grid
               do 100 l = 1,lmax
                  r1    (n ,m ,k ,l ) = misval
100         continue
         endif
110 continue
!
!-----store results in xdata
   n1 = 0
   do 200 k = lay(1), lay(2), lay(3)
      do 200 n = 1,num_rows
         do 200 m = 1,num_columns
            n1           = n1 + 1
            xdata ( n1 ) = r1 (n ,m , k, parcod-19)
200 continue
!
   return
end

subroutine wrhtur(okee      ,hdefds    ,hdafds    ,itim      ,&
&misval    ,&
&num_rows      ,num_columns      ,num_layers_grid      ,ltur      ,&
&kcs       ,kfu       ,kfv       ,rtur1     )
!-----------------------------------------------------------------------
!             Module: SUBROUTINE WRHTUR
!           Function: get NEFIS data for time itim for
!                     parameter = 2.  'c ' turbulence parameters
!        Method used:
!
!-----------------------------------------------------------------------
!   Calling routine :              ods_tri_nef_map_getdata
!-----------------------------------------------------------------------
!   Called  routines:              GETELT (nf)
!-----------------------------------------------------------------------
!  Formal parameters:
!  ------------------
!
!   Var. I/O  Type Dimensions
!   -------------------------
!
! OKEE     O  L*4                  Flag for further execution program
! ITIM    I   I*4                  Specified time
! MISVAL  I   R*4                  missing value
! ISTOF   I   I*4                  Number of constITUENT TO BE PLOTTED
! KCS     I   I*4  num_rows,num_columns       Mask array for the zeta points
!                                  (time independent)
!                                  =0 inactive point
!                                  =1 active   point
!                                  =2 open boundary point
! KFU     I   I*4  num_rows,num_columns       Mask array for the u-velocity point
!                                  (time dependent)
!                                  =0 dry      point
!                                  =1 active   point
! KFV     I   I*4  num_rows,num_columns       Mask array for the v-velocity point
!                                  (time dependent)
!                                  =0 dry      point
!                                  =1 active   point
! num_layers_grid    I   I*4                  Number of layers
! LTUR    I   I*4                  Number of turbulence parameters
! HDAFDS  I   I*4  999             Data file descriptor for the MAP-DAT
!                                  file
! HDEFDS  I   I*4  2997            Definition file description for the
!                                  MAP-DEF file
! num_columns    I   I*4                  Number of gridpoints in the x-dir.
! NAMCON  I  CH*20 LMAX            Name of constituent
! num_rows    I   I*4                  Number of gridpoints in the y-dir.
! RTUR1    O  R*4  num_rows,num_columns,num_layers_grid,LMAX
!                                  Concentrations in zeta point
!-----------------------------------------------------------------------
!    Local variables:
!    ----------------
!
!   Var.      Type Dimensions
!   -------------------------
!
! BUFLEN      I*4                  Buffer length
! FMT        CH*46                 Help var. for write format
! FMTDEF     CH*46                 Help var. for write format default
!                                  values
! GRPDEF     CH*16                 Group name definition
! KFS         I*4                  Temporary dry point
! LVAR        L*4                  Help var.
! K           I*4                  Help var.
! L           I*4                  Help var.
! M           I*4                  Help var.
! N           I*4                  Help var.
! NRBLOK      I*4                  Blocknumber in data file
! OLDID      CH*3                  Old run-identification
! UINDEX      I*4  3               Array containing cell indices with
!                                  has to be read
! USRORD      I*4                  Sequence in which the cells must be
!                                  read
! ZDEF        R*4                  Default value for z value outside
!                                  model
!-----------------------------------------------------------------------
!
!  DECLARATIONS
!
   integer       num_rows  ,num_columns  ,num_layers_grid  ,ltur  ,itim
   integer       n     ,m     ,k     ,l
   integer       nd    ,md
!
   real          misval
!
   integer       kcs   (num_rows  ,num_columns  ),&
   &kfu   (num_rows  ,num_columns  ),kfv   (num_rows  ,num_columns  )
!
   real          rtur1 (num_rows  ,num_columns   ,num_layers_grid  ,ltur  )
!
   logical       okee  ,lvar
!-----------------------------------------------------------------------
!-----Declarations NEFIS
!-----------------------------------------------------------------------
   integer*4     hdefds( 2997),hdafds(  999)
!
   integer*4     uindex(    3),usrord,buflen
!
   character*16  grpdef
!
   integer*4     GETELT,GETELS
!--------------------------------------------------------------------
!-----Initialize Nefis variables
!--------------------------------------------------------------------
   grpdef    = 'map-series'
   usrord    = 1
   buflen    = 4 * num_rows   * num_columns   * num_layers_grid   * ltur
   uindex(1) = itim
   uindex(2) = itim
   uindex(3) = 1
!--------------------------------------------------------------------
!-----Read from group 3 R1
!--------------------------------------------------------------------
   okee      = okee .and.&
   &GETELT(hdefds   ,grpdef    ,'RTUR1 '  ,&
   &uindex   ,usrord ,buflen    ,RTUR1     )&
   &.eq. 0
!--------------------------------------------------------------------
!-----Write R1 for all constituents
!     if kcs = 0 then permanent drypoint; if surrounding kfu and kfv
!     := 0 then temporary drypoint
!--------------------------------------------------------------------
   do 110 n = 1,num_rows
      do 110 m = 1,num_columns
         md    = max (1,m-1)
         nd    = max (1,n-1)
         lvar  = ((kcs   (n ,m ) .eq. 1) .and.&
         &(kfu   (n ,m ) .eq. 1  .or.&
         &kfu   (n ,md) .eq. 1  .or.&
         &kfv   (n ,m ) .eq. 1  .or.&
         &kfv   (nd,m ) .eq. 1))
         if (.not. lvar  ) then
            do 100 k = 1,num_layers_grid
               do 100 l = 1,ltur
                  rtur1 (n ,m ,k ,l ) = misval
100         continue
         endif
110 continue
!
   return
end

subroutine wrhtai(okee      ,hdefds    ,hdafds    ,parcod    ,&
&misval    ,&
&grdang    ,itim      ,num_rows      ,num_columns      ,&
&kcs       ,kfu       ,kfv       ,&
&xcor      ,ycor      ,alfas                ,&
&tauksi    ,taueta                          )
!-----------------------------------------------------------------------
!             Module: SUBROUTINE WRHTAI
!           Function: get NEFIS data for time itim for
!                     parameter = 16  'tu'
!                     parameter = 17  'tv'
!                     parameter = 18  'tm'
!                     parameter = 19  'ta'
!        Method used:
!
!-----------------------------------------------------------------------
!   Calling routine :              ods_tri_nef_map_getdata
!-----------------------------------------------------------------------
!   Called  routines:              GETELT (nf)
!-----------------------------------------------------------------------
!  Formal parameters:
!  ------------------
!
!   Var. I/O  Type Dimensions
!   -------------------------
!
! GRDANG  I   R*4                  Vertex between the y-axis and north
! ALFAS   I   R*4  num_rows,num_columns       Transformation coefficients (in radians)
! OKEE     O  L*4                  Flag for further execution program
! ITIM    I   I*4                  Specified time
! MISVAL  I   R*4                  missing value
! PARCOD  I   I*4                  Parameter code
! KCS     I   I*4  num_rows,num_columns       Mask array for the zeta points
!                                  (time independent)
!                                  =0 inactive point
!                                  =1 active   point
!                                  =2 open boundary point
! KFU     I   I*4  num_rows,num_columns       Mask array for the u-velocity point
!                                  (time dependent)
!                                  =0 dry      point
!                                  =1 active   point
! KFV     I   I*4  num_rows,num_columns       Mask array for the v-velocity point
!                                  (time dependent)
!                                  =0 dry      point
!                                  =1 active   point
! HDAFDS  I   I*4  999             Data file descriptor for the MAP-DAT
!                                  file
! HDEFDS  I   I*4  2997            Definition file description for the
!                                  MAP-DEF file
! num_columns    I   I*4                  Number of gridpoints in the x-dir.
! num_rows    I   I*4                  Number of gridpoints in the y-dir.
! TAUETA   O  R*4  num_rows,num_columns       Tau bottom in v-velocity point
! TAUKSI   O  R*4  num_rows,num_columns       Tau bottom in u-velocity point
! XCOR    I   R*4  num_rows,num_columns       X-distance between 2 grid lines
!                                  around a zeta point in y-direction
! YCOR    I   R*4  num_rows,num_columns       Y-distance between 2 grid lines
!                                  around a zeta point in y-direction
!-----------------------------------------------------------------------
!    Local variables:
!    ----------------
!
!   Var.      Type Dimensions
!   -------------------------
!
! BUFLEN      I*4                  Buffer length
! EPS         R*4                  Small value to test for backwards
!                                  transformation
! GRPDEF     CH*16                 Group name definition
! GUUGEM      R*4                  Help var. 'mean' GUU value
! GVVGEM      R*4                  Help var. 'mean' GVV value
! KENMU       I*4                  Mask value for two consecutive u-vel.
!                                  points (max 1)
! KENMV       I*4                  Mask value for two consecutive v-vel.
!                                  points (max 1)
! KFS         I*4                  Temporary dry point
! LVAR        L*4                  Help var.
! M           I*4                  Loop var. 1-num_columns
! MD          I*4                  Max (1,M-1)
! MM          I*4                  Max (2,M)
! MMD         I*4                  MM-1
! N           I*4                  Loop var. 1-num_rows
! ND          I*4                  Max (1,N-1)
! NN          I*4                  Max (2,N)
! NND         I*4                  NN-1
! PI          R*4                  Value for pi (3.14 etc.)
! UINDEX      I*4  3               Array containing cell indices with
!                                  has to be read
! USRORD      I*4                  Sequence in which the cells must be
!                                  read
! TAUD        R*4                  Tau bottom direction
! TAUM        R*4                  Tau bottom magnitue
! TAUZU       R*4                  Tau bottom U in a zeta point
! TAUZV       R*4                  Tau bottom V in a zeta point
! TEGEM       R*4                  Help var. 'mean' taueta value
! TEN         R*4                  Help var. for taueta in v-point N
! TEND        R*4                  Help var. for taueta in v-point ND
! TKGEM       R*4                  Help var. 'mean' tauksi value
! TKM         R*4                  Help var. for tauksi in u-point M
! TKMD        R*4                  Help var. for tauksi in u-point MD
! ZDEF        R*4                  Default value for z value outside
!                                  model
!-----------------------------------------------------------------------
!
!  declarations
!
   integer       num_rows  ,num_columns  ,itim  ,parcod
   integer       n     ,m     ,nd    ,md
!
   real          eps   ,misval,tauzu ,tauzv ,hulp
   real          taud  ,taum  ,tkm   ,tkmd  ,ten   ,tend
   real          pi
   real          tkgem ,tegem ,grdang
!
   integer       kcs   (num_rows  ,num_columns  ),&
   &kfu   (num_rows  ,num_columns  ),kfv   (num_rows  ,num_columns  )
!
   real          xcor  (num_rows  ,num_columns  ),ycor  (num_rows  ,num_columns  ),&
   &alfas (num_rows  ,num_columns  )
   real          tauksi(num_rows  ,num_columns  ),taueta(num_rows  ,num_columns  )
!
   logical       okee  ,lvar
!-----------------------------------------------------------------------
!-----Declaraties NEFIS
!-----------------------------------------------------------------------
   integer*4     hdefds( 2997),hdafds(  999)
!
   integer*4     uindex(    3),usrord,buflen
!
   character*16  grpdef
!
   integer*4     GETELT,GETELS
!-----------------------------------------------------------------------
!-----General initialisation
!-----------------------------------------------------------------------
   pi     = atan (1.0) * 4.
   eps    = 1.e-12
!--------------------------------------------------------------------
!-----Initialize Nefis variables
!--------------------------------------------------------------------
   grpdef    = 'map-series'
   usrord    = 1
   buflen    = 4 * num_rows   * num_columns
   uindex(1) = itim
   uindex(2) = itim
   uindex(3) = 1
!--------------------------------------------------------------------
!-----Read from group 3 TAUKSI
!--------------------------------------------------------------------
   okee      = okee .and.&
   &GETELT(hdefds   ,grpdef    ,'TAUKSI'  ,&
   &uindex   ,usrord ,buflen    ,TAUKSI    )&
   &.eq. 0
!--------------------------------------------------------------------
!-----Read from group 3 TAUETA
!--------------------------------------------------------------------
   okee      = okee .and.&
   &GETELT(hdefds   ,grpdef    ,'TAUETA'  ,&
   &uindex   ,usrord ,buflen    ,TAUETA    )&
   &.eq. 0
!--------------------------------------------------------------------
!-----Calculate TAUZU, TAUZV, TAUM and TAUD
!     if kcs = 0 then permanent drypoint; if surrounding kfu and kfv
!     := 0 then temporary drypoint
!--------------------------------------------------------------------
   do 110 n = num_rows, 1, -1
      do 110 m = num_columns, 1, -1
         md    = max (1,m-1)
         nd    = max (1,n-1)
         lvar  = ((kcs   (n ,m ) .eq. 1) .and.&
         &(kfu   (n ,m ) .eq. 1  .or.&
         &kfu   (n ,md) .eq. 1  .or.&
         &kfv   (n ,m ) .eq. 1  .or.&
         &kfv   (nd,m ) .eq. 1))
         if (lvar  ) then
!--------------------------------------------------------------------
!--------------Backward transformation of TAUKSI and TAUETA and
!              calculation of TAUZU and TAUZV
!--------------------------------------------------------------------
            tkm    = tauksi(n ,m )  * kfu   (n ,m )
            tkmd   = tauksi(n ,md)  * kfu   (n ,md)
            ten    = taueta(n ,m )  * kfv   (n ,m )
            tend   = taueta(nd,m )  * kfv   (nd,m )
!
            tkgem  = 0.5 * (tkm  + tkmd) * kcs(n,m)
            tegem  = 0.5 * (ten  + tend) * kcs(n,m)
!
            tauzu  =  tkgem  * cos( alfas(n,m) ) -&
            &tegem  * sin( alfas(n,m) )
            tauzv  =  tkgem  * sin( alfas(n,m) ) +&
            &tegem  * cos( alfas(n,m) )
!--------------------------------------------------------------------
!--------------Calculation of TAUM and TAUD
!              taud should be defined between 0. and 360. degrees
!              atan2 <-180,180>, grdang [0,360] => mod (.. + 720)
!
!        Note (AM, dd 31 march 1999)
!        The sign before grdang was -, this has been adjusted, though
!        in most cases grdang is zero anyway.
!--------------------------------------------------------------------
            taum   = sqrt ( tauzu  * tauzu  + tauzv  * tauzv )
            if (abs (tauzu ) .lt. eps) then
               tauzu  = eps
            endif
            if (abs (tauzv ) .lt. eps) then
               tauzv  = eps
            endif
            hulp   =    90. - atan2 (tauzv ,tauzu ) * 180. / pi +&
            &grdang
            taud   = mod    ( hulp  +  720.,  360.)
!
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
!
   return
end

subroutine wrhtav(okee      ,hdefds    ,hdafds    ,misval    ,&
&itim      ,num_rows      ,num_columns      ,&
&kcs       ,kfu       ,kfv       ,&
&xcor      ,ycor      ,alfas     ,&
&tauksi    ,taueta                          )
!-----------------------------------------------------------------------
!             Module: SUBROUTINE WRHTAV
!           Function: get NEFIS data for time itim for
!                     parameter = 39  'tf'
!        Method used:
!
!-----------------------------------------------------------------------
!   Calling routine :              ods_tri_nef_map_getdata
!-----------------------------------------------------------------------
!   Called  routines:              GETELT (nf)
!-----------------------------------------------------------------------
!  Formal parameters:
!  ------------------
!
!   Var. I/O  Type Dimensions
!   -------------------------
!
! ALFAS   I   R*4  num_rows,num_columns       Transformation coefficients (in radians)
! OKEE     O  L*4                  Flag for further execution program
! ITIM    I   I*4                  Specified time
! MISVAL  I   R*4                  missing value
! KCS     I   I*4  num_rows,num_columns       Mask array for the zeta points
!                                  (time independent)
!                                  =0 inactive point
!                                  =1 active   point
!                                  =2 open boundary point
! KFU     I   I*4  num_rows,num_columns       Mask array for the u-velocity point
!                                  (time dependent)
!                                  =0 dry      point
!                                  =1 active   point
! KFV     I   I*4  num_rows,num_columns       Mask array for the v-velocity point
!                                  (time dependent)
!                                  =0 dry      point
!                                  =1 active   point
! HDAFDS  I   I*4  999             Data file descriptor for the MAP-DAT
!                                  file
! HDEFDS  I   I*4  2997            Definition file description for the
!                                  MAP-DEF file
! num_columns    I   I*4                  Number of gridpoints in the x-dir.
! num_rows    I   I*4                  Number of gridpoints in the y-dir.
! TAUETA   O  R*4  num_rows,num_columns       Tau bottom in v-velocity point
! TAUKSI   O  R*4  num_rows,num_columns       Tau bottom in u-velocity point
! XCOR    I   R*4  num_rows,num_columns       X-distance between 2 grid lines
!                                  around a zeta point in y-direction
! YCOR    I   R*4  num_rows,num_columns       Y-distance between 2 grid lines
!                                  around a zeta point in y-direction
!-----------------------------------------------------------------------
!    Local variables:
!    ----------------
!
!   Var.      Type Dimensions
!   -------------------------
!
! BUFLEN      I*4                  Buffer length
! EPS         R*4                  Small value to test for backwards
!                                  transformation
! GRPDEF     CH*16                 Group name definition
! GUUGEM      R*4                  Help var. 'mean' GUU value
! GVVGEM      R*4                  Help var. 'mean' GVV value
! KENMU       I*4                  Mask value for two consecutive u-vel.
!                                  points (max 1)
! KENMV       I*4                  Mask value for two consecutive v-vel.
!                                  points (max 1)
! LVAR        L*4                  Help var.
! M           I*4                  Loop var. 1-num_columns
! MD          I*4                  Max (1,M-1)
! MM          I*4                  Max (2,M)
! MMD         I*4                  MM-1
! N           I*4                  Loop var. 1-num_rows
! ND          I*4                  Max (1,N-1)
! NN          I*4                  Max (2,N)
! NND         I*4                  NN-1
! TAUZU       R*4                  Tau bottom U in a zeta point
! TAUZV       R*4                  Tau bottom V in a zeta point
! TEGEM       R*4                  Help var. 'mean' taueta value
! TEN         R*4                  Help var. for taueta in v-point N
! TEND        R*4                  Help var. for taueta in v-point ND
! TKGEM       R*4                  Help var. 'mean' tauksi value
! TKM         R*4                  Help var. for tauksi in u-point M
! TKMD        R*4                  Help var. for tauksi in u-point MD
! UINDEX      I*4  3               Array containing cell indices with
!                                  has to be read
! USRORD      I*4                  Sequence in which the cells must be
!                                  read
! ZDEF        R*4                  Default value for z value outside
!                                  model
!-----------------------------------------------------------------------
!
!  declarations
!
   integer       num_rows  ,num_columns  ,nd    ,md
   integer       itim
   integer       n     ,m
!
   real          tauzu ,tauzv ,misval,eps
   real          tegem ,ten   ,tend  ,tkgem ,tkm   ,tkmd
!
   integer       kcs   (num_rows  ,num_columns  ),&
   &kfu   (num_rows  ,num_columns  ),kfv   (num_rows  ,num_columns  )
!
   real          xcor  (num_rows  ,num_columns  ),ycor  (num_rows  ,num_columns  ),&
   &alfas (num_rows  ,num_columns  )
   real          tauksi(num_rows  ,num_columns  ),taueta(num_rows  ,num_columns  )
!
   logical       okee  ,lvar
!-----------------------------------------------------------------------
!-----Declaraties NEFIS
!-----------------------------------------------------------------------
   integer*4     hdefds( 2997),hdafds(  999)
!
   integer*4     uindex(    3),usrord,buflen
!
   character*16  grpdef
!
   integer*4     GETELT,GETELS
!-----------------------------------------------------------------------
!-----General initialisation
!-----------------------------------------------------------------------
   eps    = 1.e-12
!--------------------------------------------------------------------
!-----Initialize Nefis variables
!--------------------------------------------------------------------
   grpdef    = 'map-series'
   usrord    = 1
   buflen    = 4 * num_rows   * num_columns
   uindex(1) = itim
   uindex(2) = itim
   uindex(3) = 1
!--------------------------------------------------------------------
!-----Read from group 3 TAUKSI
!--------------------------------------------------------------------
   okee      = okee .and.&
   &GETELT(hdefds   ,grpdef    ,'TAUKSI'  ,&
   &uindex   ,usrord ,buflen    ,TAUKSI    )&
   &.eq. 0
!--------------------------------------------------------------------
!-----Read from group 3 TAUETA
!--------------------------------------------------------------------
   okee      = okee .and.&
   &GETELT(hdefds   ,grpdef    ,'TAUETA'  ,&
   &uindex   ,usrord ,buflen    ,TAUETA    )&
   &.eq. 0
!
!-----Calculate TAUZU and TAUZV
!     if kcs = 0 then permanent drypoint; if surrounding kfu and kfv
!     := 0 then temporary drypoint
   do 110 n = num_rows, 1, -1
      do 110 m = num_columns, 1, -1
         md    = max (1,m-1)
         nd    = max (1,n-1)
         lvar  = ((kcs   (n ,m ) .ne. 0) .and.&
         &(kfu   (n ,m ) .eq. 1  .or.&
         &kfu   (n ,md) .eq. 1  .or.&
         &kfv   (n ,m ) .eq. 1  .or.&
         &kfv   (nd,m ) .eq. 1))
         if (lvar  ) then
!--------------Backward transformation of TAUKSI and TAUETA and
!              calculation of TAUZU and TAUZV
            tkm    = tauksi(n ,m )  * kfu   (n ,m )
            tkmd   = tauksi(n ,md)  * kfu   (n ,md)
            ten    = taueta(n ,m )  * kfv   (n ,m )
            tend   = taueta(nd,m )  * kfv   (nd,m )
!
            tkgem  = 0.5 * (tkm  + tkmd) * kcs(n,m)
            tegem  = 0.5 * (ten  + tend) * kcs(n,m)
!
            tauzu  =  tkgem * cos( alfas(n,m) ) -&
            &tegem * sin( alfas(n,m) )
            tauzv  =  tkgem * sin( alfas(n,m) ) +&
            &tegem * cos( alfas(n,m) )
!
            tauksi(n ,m ) = tauzu
            taueta(n ,m ) = tauzv
         else
            tauksi(n ,m ) = misval
            taueta(n ,m ) = misval
         endif
110 continue
!
   return
end

subroutine wrhuvv(okee      ,hdefds    ,hdafds    ,misval    ,&
&itim      ,num_rows      ,num_columns      ,num_layers_grid      ,&
&kcs       ,kfu       ,kfv       ,&
&xcor      ,ycor      ,&
&alfas                ,u1        ,v1        )
!-----------------------------------------------------------------------
!             Module: SUBROUTINE WRHUVV
!           Function: get NEFIS data for time itim for
!                     parameter = 40  'uv'
!        Method used:
!
!-----------------------------------------------------------------------
!   Calling routine :              ods_tri_nef_map_getdata
!-----------------------------------------------------------------------
!   Called  routines:              GETELT (nf)
!-----------------------------------------------------------------------
!  Formal parameters:
!  ------------------
!
!   Var. I/O  Type Dimensions
!   -------------------------
!
! ALFAS   I   R*4  num_rows,num_columns       Transformation coefficients (in radians)
! OKEE     O  L*4                  Flag for further execution program
! ITIM    I   I*4                  Specified time
! MISVAL  I   R*4                  missing value
! KCS     I   I*4  num_rows,num_columns       Mask array for the zeta points
!                                  (time independent)
!                                  =0 inactive point
!                                  =1 active   point
!                                  =2 open boundary point
! KFU     I   I*4  num_rows,num_columns       Mask array for the u-velocity point
!                                  (time dependent)
!                                  =0 dry      point
!                                  =1 active   point
! KFV     I   I*4  num_rows,num_columns       Mask array for the v-velocity point
!                                  (time dependent)
!                                  =0 dry      point
!                                  =1 active   point
! num_layers_grid    I   I*4                  Number of layers
! HDAFDS  I   I*4  999             Data file descriptor for the MAP-DAT
!                                  file
! HDEFDS  I   I*4  2997            Definition file description for the
!                                  MAP-DEF file
! num_columns    I   I*4                  Number of gridpoints in the x-dir.
! num_rows    I   I*4                  Number of gridpoints in the y-dir.
! U1       O  R*4  num_rows,num_columns,num_layers_grid  U-velocity in u-velocity point
! V1       O  R*4  num_rows,num_columns,num_layers_grid  V-velocity in v-velocity point
! XCOR    I   R*4  num_rows,num_columns       X-distance between 2 grid lines
!                                  around a zeta point in y-direction
! YCOR    I   R*4  num_rows,num_columns       Y-distance between 2 grid lines
!                                  around a zeta point in y-direction
!-----------------------------------------------------------------------
!    Local variables:
!    ----------------
!
!   Var.      Type Dimensions
!   -------------------------
!
! BUFLEN      I*4                  Buffer length
! EPS         R*4                  Small value to test for backwards
!                                  transformation
! GRPDEF     CH*16                 Group name definition
! GUUGEM      R*4                  Help var. 'mean' GUU value
! GVVGEM      R*4                  Help var. 'mean' GVV value
! KENMU       I*4                  Mask value for two consecutive u-vel.
!                                  points (max 1)
! KENMV       I*4                  Mask value for two consecutive v-vel.
!                                  points (max 1)
! LVAR        L*4                  Help var.
! M           I*4                  Loop var. 1-num_columns
! MD          I*4                  Max (1,M-1)
! MM          I*4                  Max (2,M)
! MMD         I*4                  MM-1
! N           I*4                  Loop var. 1-num_rows
! ND          I*4                  Max (1,N-1)
! NN          I*4                  Max (2,N)
! NND         I*4                  NN-1
! UGEM        R*4                  Help var. 'mean' u-velocity value
! UINDEX      I*4  3               Array containing cell indices with
!                                  has to be read
! UM          R*4                  Help var. for u-vel. in u-point M
! UMD         R*4                  Help var. for u-vel. in u-point MD
! USRORD      I*4                  Sequence in which the cells must be
!                                  read
! UVD         R*4                  Velocity direction
! UVM         R*4                  Velocity magnitude
! UZ          R*4                  U-velocity in a zeta point
! VGEM        R*4                  Help var. 'mean' v-velocity value
! VN          R*4                  Help var. for v-vel. in v-point N
! VND         R*4                  Help var. for v-vel. in v-point ND
! VZ          R*4                  V-velocity in a zeta point
! ZDEF        R*4                  Default value for z value outside
!                                  model
!-----------------------------------------------------------------------
!
!  declarations
!
   integer       num_layers_grid  ,itim
   integer       num_rows  ,num_columns  ,nd    ,md
   integer       n     ,m     ,k
!
   real          misval,eps
   real          ugem  ,um    ,umd   ,uz
   real          vgem  ,vn    ,vnd   ,vz
!
   integer       kcs   (num_rows  ,num_columns  ),&
   &kfu   (num_rows  ,num_columns  ),kfv   (num_rows  ,num_columns  )
!
   real          xcor  (num_rows  ,num_columns  ),ycor  (num_rows  ,num_columns  ),&
   &alfas (num_rows  ,num_columns  )
   real          u1    (num_rows  ,num_columns   ,num_layers_grid  ),&
   &v1    (num_rows  ,num_columns   ,num_layers_grid  )
!
   logical       okee  ,lvar
!-----------------------------------------------------------------------
!-----Declarations NEFIS
!-----------------------------------------------------------------------
   integer*4     hdefds( 2997),hdafds(  999)
!
   integer*4     uindex(    3),usrord,buflen
!
   character*16  grpdef
!
   integer*4     GETELT,GETELS
!-----------------------------------------------------------------------
!-----General initialisation
!-----------------------------------------------------------------------
   eps    = 1.e-12
!--------------------------------------------------------------------
!-----Initialize Nefis variables
!--------------------------------------------------------------------
   grpdef    = 'map-series'
   usrord    = 1
   buflen    = 4 * num_rows   * num_columns   * num_layers_grid
   uindex(1) = itim
   uindex(2) = itim
   uindex(3) = 1
!--------------------------------------------------------------------
!-----Read from  group 3 U1
!--------------------------------------------------------------------
   okee      = okee .and.&
   &GETELT(hdefds   ,grpdef    ,'U1    '  ,&
   &uindex   ,usrord ,buflen    ,U1        )&
   &.eq. 0
!--------------------------------------------------------------------
!-----Read from group 3 V1
!--------------------------------------------------------------------
   okee      = okee .and.&
   &GETELT(hdefds   ,grpdef    ,'V1    '  ,&
   &uindex   ,usrord ,buflen    ,V1        )&
   &.eq. 0
!-----Calculate UZ and VZ
!     if kcs = 0 then permanent drypoint; if surrounding kfu and kfv
!     := 0 then temporary drypoint
   do 110 n = num_rows, 1, -1
      do 110 m = num_columns, 1, -1
         md    = max (1,m-1)
         nd    = max (1,n-1)
         lvar  = ((kcs   (n ,m ) .ne. 0) .and.&
         &(kfu   (n ,m ) .eq. 1  .or.&
         &kfu   (n ,md) .eq. 1  .or.&
         &kfv   (n ,m ) .eq. 1  .or.&
         &kfv   (nd,m ) .eq. 1))
         do 110 k = 1, num_layers_grid
            if (lvar  ) then
!-----------------------------------------------------------------------
!-----------------Backwards transformation of U1 and V1 and
!                 calculation of UZ and VZ
!-----------------------------------------------------------------------
               um     = u1    (n ,m ,k )  * kfu   (n ,m )
               umd    = u1    (n ,md,k )  * kfu   (n ,md)
               vn     = v1    (n ,m ,k )  * kfv   (n ,m )
               vnd    = v1    (nd,m ,k )  * kfv   (nd,m )
!
               ugem   = 0.5 * (um   + umd ) * kcs(n,m)
               vgem   = 0.5 * (vn   + vnd ) * kcs(n,m)
!
               uz     =  ugem * cos( alfas(n,m) ) -&
               &vgem * sin( alfas(n,m) )
               vz     =  ugem * sin( alfas(n,m) ) +&
               &vgem * cos( alfas(n,m) )
!
               u1    (n ,m ,k ) = uz
               v1    (n ,m ,k ) = vz
            else
               u1    (n ,m ,k ) = misval
               v1    (n ,m ,k ) = misval
            endif
110 continue
!
   return
end

subroutine wrhgrd(num_rows      ,num_columns      ,noroco    ,norow     ,&
&irocol    ,kcu       ,kcv       ,iflag     ,&
&igrid                                      )
!-----------------------------------------------------------------------
!             Module: SUBROUTINE WRHGRD
!           Function: get NEFIS time independent data for
!                     parameter = 35  'ag'
!                     parameter = 36  'bl'
!        Method used:
!
!-----------------------------------------------------------------------
!   Calling routine :              ods_tri_nef_map_getdata
!-----------------------------------------------------------------------
!   Called  routines:              NONE
!-----------------------------------------------------------------------
!  Formal parameters:
!  ------------------
!
!   Var. I/O  Type Dimensions
!   -------------------------
!
! IFLAG    O  I*4  num_rows,num_columns       Array for permanent and tempory dry
!                                  point
! IGRID    O  I*4  num_rows,num_columns       Array with actual grid
! IROCOL  I   I*4  5,NOROCO        Pointer table with bound. coord. and
!                                  bound. types (comp. cols. and rows)
! KCU     I   I*4  num_rows,num_columns       Mask array for the u-velocity point
!                                  (time independent)
!                                  =0 dry      point
!                                  =1 active   point
! KCV     I   I*4  num_rows,num_columns       Mask array for the v-velocity point
!                                  (time independent)
!                                  =0 dry      point
!                                  =1 active   point
! num_columns    I   I*4                  Number of gridpoints in the x-dir.
! num_rows    I   I*4                  Number of gridpoints in the y-dir.
! NOROCO  I   I*4                  Number of Computational rows & cols
! NOROW       I*4                  Number of comp. rows  in IROCOL-array
!-----------------------------------------------------------------------
!    Local variables:
!    ----------------
!
!   Var.      Type Dimensions
!   -------------------------
!
! I23         I*4                  Value for tekal nabour n+1,m
! I29         I*4                  Value for tekal nabour n-1,m
! I5          I*4                  Value for tekal nabour n,m+1
! I7          I*4                  Value for tekal nabour n,m-1
! M           I*4                  Help var.
! M1          I*4                  Help var.
! M2          I*4                  Help var.
! N           I*4                  Help var.
! N1          I*4                  Help var.
!-----------------------------------------------------------------------
!
!  DECLARATIONS
!
   integer       num_rows  ,num_columns  ,noroco,norow ,n     ,m
   integer       m1    ,m2    ,i     ,n1
   integer       i23   ,i29   ,i5    ,i7
!
   integer       irocol(5     ,noroco),kcu   (num_rows  ,num_columns  ),&
   &kcv   (num_rows  ,num_columns  ),iflag (num_rows  ,num_columns  )
   integer       igrid (num_rows  ,num_columns  )
!--------------------------------------------------------------------
!-----Initialize iflag = 0 and igrid = 0
!--------------------------------------------------------------------
   do 110 m = 1,num_columns
      do 110 n = 1,num_rows
         iflag (n,m) = 0
         igrid (n,m) = 0
110 continue
!--------------------------------------------------------------------
!-----Define IBUFF values inside irocol table and
!     calculate minima and maxima
!--------------------------------------------------------------------
   do 210 i = 1,norow
      n     = irocol(1,i)
      m1    = irocol(2,i)
      m2    = irocol(3,i)
      do 220 m = m1-1,m2
         igrid (n  ,m) = 1
         igrid (n-1,m) = 1
220   continue
210 continue
!-----------------------------------------------------------------------
!--------Define IFLAG values inside irocol table
!  +--------------------------------------------------------------------
!  |  scan <kcu> and <kcv> for 0, to find: boundaries,
!  |                                       small dams and
!  |                                       permanently dry points.
!  |
!  |  "internal codes" of <iflag> are:
!  |                                           1000
!  |                                             |
!  |                                     0001 -- o -- 0010
!  |                                             |
!  |                                           0100
!  +--------------------------------------------------------------------
!-----------------------------------------------------------------------
!        +--------------------------------------------------------------
!        | interior + right border + upper border
!        +--------------------------------------------------------------
!        |
!        |   n ----->  + 10 -  1 +    -    +    -   +
!        |                      100
!        |             |    o    |    o    |    o   |
!        |                     1000
!        |             +    -    +    -    +    -   +
!        |
!        |             |    o    |    o    |    o   |
!        |
!        |             +    -    +    -    +    -   +
!        |                       |
!        |                       |
!        |                       m
!        +--------------------------------------------------------------
!-----------------------------------------------------------------------
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
320   continue
310 continue
!-----------------------------------------------------------------------
!        +--------------------------------------------------------------
!        | left border
!        +--------------------------------------------------------------
!        |
!        |   n ----->  +    -    +    -    +    -   +
!        |            100
!        |             |    o    |    o    |    o   |
!        |           1000
!        |             +    -    +    -    +    -   +
!        |
!        |             |    o    |    o    |    o   |
!        |
!        |             +    -    +    -    +    -   +
!        |                       |
!        |                       |
!        |                       m1
!        +--------------------------------------------------------------
!-----------------------------------------------------------------------
   do 330 i = 1,norow
      n     = irocol(1,i)
      m1    = irocol(2,i)
      if (kcu(n,m1-1) .eq. 0) then
         iflag (n  ,m1-1) = iflag (n  ,m1-1) +  100
         iflag (n-1,m1-1) = iflag (n-1,m1-1) + 1000
      endif
330 continue
!-----------------------------------------------------------------------
!        +--------------------------------------------------------------
!        | lower border
!        +--------------------------------------------------------------
!        |
!        |             +    -    +    -    +    -   +
!        |
!        |             |    o    |    o    |    o   |
!        |
!        |   m ----->  +    -    +    -    +    -   +
!        |
!        |             |    o    |    o    |    o   |
!        |
!        |             + 10 -  1 +    -    +    -   +
!        |                       |
!        |                       |
!        |                       n1
!        +--------------------------------------------------------------
!-----------------------------------------------------------------------
   do 340 i = norow+1,noroco
      m     = irocol(1,i)
      n1    = irocol(2,i)
      if (kcv(n1-1,m) .eq. 0) then
         iflag (n1-1,m  ) = iflag (n1-1,m  ) +  1
         iflag (n1-1,m-1) = iflag (n1-1,m-1) + 10
      endif
340 continue
!-----------------------------------------------------------------------
!        +--------------------------------------------------------------
!        | swap "internal codes" to "external codes" (=TEKAL-codes)
!        +--------------------------------------------------------------
!        |     "internal codes"            "external codes"
!        |
!        |          1000                           23
!        |            |                             |
!        |    0001 -- o -- 0010                7 -- o -- 5
!        |            |                             |
!        |          0100                           29
!        +--------------------------------------------------------------
!-----------------------------------------------------------------------
   do 350 n = 1,num_rows
      do 350 m = 1,num_columns
         i23 = (iflag (n,m)                       ) / 1000
         i29 = (iflag (n,m)-i23*1000              ) / 100
         i5  = (iflag (n,m)-i23*1000-i29*100      ) / 10
         i7  = (iflag (n,m)-i23*1000-i29*100-i5*10)
         iflag (n,m) = i7*7 + i5*5 + i29*29 + i23*23
350 continue
!
   return
end

subroutine wrhtd (num_rows      ,num_columns      ,noroco    ,norow     ,&
&irocol    ,kcu       ,kcv       ,&
&kfu       ,kfv       ,iflag                )
!-----------------------------------------------------------------------
!             Module: SUBROUTINE WRHTD
!           Function: get NEFIS data time dependent for
!                     parameter = 37  'td'
!        Method used:
!
!-----------------------------------------------------------------------
!   Calling routine :              ods_tri_nef_map_getdata
!-----------------------------------------------------------------------
!   Called  routines:              NONE
!-----------------------------------------------------------------------
!  Formal parameters:
!  ------------------
!
!   Var. I/O  Type Dimensions
!   -------------------------
!
! IFLAG    O  I*4  num_rows,num_columns       Array for permanent and tempory dry
!                                  point
! IROCOL  I   I*4  5,NOROCO        Pointer table with bound. coord. and
!                                  bound. types (comp. cols. and rows)
! KCU     I   I*4  num_rows,num_columns       Mask array for the u-velocity point
!                                  (time independent)
!                                  =0 dry      point
!                                  =1 active   point
! KCV     I   I*4  num_rows,num_columns       Mask array for the v-velocity point
!                                  (time independent)
!                                  =0 dry      point
!                                  =1 active   point
! KFU      O  I*4  num_rows,num_columns       Mask array for the u-velocity point
!                                  (time dependent)
!                                  =0 dry      point
!                                  =1 active   point
! KFV      O  I*4  num_rows,num_columns       Mask array for the v-velocity point
!                                  (time dependent)
!                                  =0 dry      point
!                                  =1 active   point
! num_columns    I   I*4                  Number of gridpoints in the x-dir.
! num_rows    I   I*4                  Number of gridpoints in the y-dir.
! NOROCO  I   I*4                  Number of Computational rows & cols
! NOROW   I   I*4                  Number of comp. rows  in IROCOL-array
!-----------------------------------------------------------------------
!    Local variables:
!    ----------------
!
!   Var.      Type Dimensions
!   -------------------------
!
! I23         I*4                  Value for tekal nabour n+1,m
! I29         I*4                  Value for tekal nabour n-1,m
! I5          I*4                  Value for tekal nabour n,m+1
! I7          I*4                  Value for tekal nabour n,m-1
! M           I*4                  Help var.
! M1          I*4                  Help var.
! M2          I*4                  Help var.
! N           I*4                  Help var.
! N1          I*4                  Help var.
!-----------------------------------------------------------------------
!
!  declarations
!
   integer       num_rows  ,num_columns  ,noroco,norow ,n     ,m
   integer       m1    ,m2    ,i     ,n1
   integer       i23   ,i29   ,i5    ,i7
!
   integer       irocol(5     ,noroco),kcu   (num_rows  ,num_columns  ),&
   &kcv   (num_rows  ,num_columns  ),kfu   (num_rows  ,num_columns  ),&
   &kfv   (num_rows  ,num_columns  ),iflag (num_rows  ,num_columns  )
!
!--------------------------------------------------------------------
!-----Initialize iflag = 0
!--------------------------------------------------------------------
   do 110 m = 1,num_columns
      do 110 n = 1,num_rows
         iflag (n,m) = 0
110 continue
!-----------------------------------------------------------------------
!--------Define IFLAG values inside the irocol table
!-----------------------------------------------------------------------
!  |  scan <kfu> and <kfv> for 0 and
!  |       <kcu> and <kcv> for <> 0, to find: temporary dry points.
!  |  NOTE: kcu = 0 => perm. dry, kcu = 1 and kfu = 0 temp. dry
!  |        kcu - kfu =  0 then or active or perm. dry
!  |        kcu - kfu = -1 theoretically impossible
!  |        kcu - kfu =  1 temp. dry
!  |
!  |  "internal codes" of <iflag> are:
!  |                                           1000
!  |                                             |
!  |                                     0001 -- o -- 0010
!  |                                             |
!  |                                           0100
!  +--------------------------------------------------------------------
!-----------------------------------------------------------------------
!        +--------------------------------------------------------------
!        | interior + right border + upper border
!        +--------------------------------------------------------------
!        |
!        |   n ----->  + 10 -  1 +    -    +    -   +
!        |                      100
!        |             |    o    |    o    |    o   |
!        |                     1000
!        |             +    -    +    -    +    -   +
!        |
!        |             |    o    |    o    |    o   |
!        |
!        |             +    -    +    -    +    -   +
!        |                       |
!        |                       |
!        |                       m
!        +--------------------------------------------------------------
!-----------------------------------------------------------------------
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
320   continue
310 continue
!-----------------------------------------------------------------------
!        +--------------------------------------------------------------
!        | left border
!        +--------------------------------------------------------------
!        |
!        |   n ----->  +    -    +    -    +    -   +
!        |            100
!        |             |    o    |    o    |    o   |
!        |           1000
!        |             +    -    +    -    +    -   +
!        |
!        |             |    o    |    o    |    o   |
!        |
!        |             +    -    +    -    +    -   +
!        |                       |
!        |                       |
!        |                       m1
!        +--------------------------------------------------------------
!-----------------------------------------------------------------------
   do 330 i = 1,norow
      n     = irocol(1,i)
      m1    = irocol(2,i)
      if ((kcu(n,m1-1) - kfu(n,m1-1)) .eq. 1) then
         iflag (n  ,m1-1) = iflag (n  ,m1-1) +  100
         iflag (n-1,m1-1) = iflag (n-1,m1-1) + 1000
      endif
330 continue
!-----------------------------------------------------------------------
!        +--------------------------------------------------------------
!        | lower border
!        +--------------------------------------------------------------
!        |
!        |             +    -    +    -    +    -   +
!        |
!        |             |    o    |    o    |    o   |
!        |
!        |   m ----->  +    -    +    -    +    -   +
!        |
!        |             |    o    |    o    |    o   |
!        |
!        |             + 10 -  1 +    -    +    -   +
!        |                       |
!        |                       |
!        |                       n1
!        +--------------------------------------------------------------
!-----------------------------------------------------------------------
   do 340 i = norow+1,noroco
      m     = irocol(1,i)
      n1    = irocol(2,i)
      if ((kcv(n1-1,m) - kfv(n1-1,m)) .eq. 1) then
         iflag (n1-1,m  ) = iflag (n1-1,m  ) +  1
         iflag (n1-1,m-1) = iflag (n1-1,m-1) + 10
      endif
340 continue
!-----------------------------------------------------------------------
!        +--------------------------------------------------------------
!        | swap "internal codes" to "external codes" (=TEKAL-codes)
!        +--------------------------------------------------------------
!        |     "internal codes"            "external codes"
!        |
!        |          1000                           23
!        |            |                             |
!        |    0001 -- o -- 0010                7 -- o -- 5
!        |            |                             |
!        |          0100                           29
!        +--------------------------------------------------------------
!-----------------------------------------------------------------------
!--------Write values to tekal-data file
!-----------------------------------------------------------------------
   do 350 n = 1,num_rows
      do 350 m = 1,num_columns
         i23 = (iflag (n,m)                       ) / 1000
         i29 = (iflag (n,m)-i23*1000              ) / 100
         i5  = (iflag (n,m)-i23*1000-i29*100      ) / 10
         i7  = (iflag (n,m)-i23*1000-i29*100-i5*10)
         iflag (n,m) = -1 * (i7*7 + i5*5 + i29*29 + i23*23)
350 continue
!
   return
end
