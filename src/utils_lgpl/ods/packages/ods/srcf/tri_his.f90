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
!        $Source: /u/cvsroot/gpp/libsrc/ods/tri_his.f,v $
!

subroutine hispar&
&(fname , itype , pardef, maxdef, timdep, locdep,&
&maxlst, lang  , parlst, paruni, partyp, parcod,&
&nrlst , ierror, option                        )
!-----------------------------------------------------------------------
!           Function: parameter name selection for time histories
!                     TRISULA NEFIS files
!        Method used:
!
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
! FOUT        L*4                  Flag for further execution program
!                                  fout  = .true.  : stop execution
!                                  fout  = .false. : go on
! FILHDA     CH*256                File name NEFIS data file for HIS
! FILHDE     CH*256                File name NEFIS definition file for
!                                  HIS
! GRPDEF     CH*16                 Group name definition
! HDAFDS      I*4  999             Data file descriptor for the HIS-DAT
!                                  file
! HDEFDS      I*4  2997            Definition file description for the
!                                  HIS-DEF file
! IERROR      I*4                  Error code for NEFIS error
! num_layers_grid        I*4                  Number of layers
! LMAX        I*4                  Total number of constituents
!                                  for old files LMAX = LSTCI
!                                  for new files LMAX = LSTCI + LTUR
! LSTCI       I*4                  Total number of constituents (incl.
!                                  turbulence for old trim files).
! LTUR        I*4                  Number of turbulence constituents
! NAMCON     CH*20                 Constituent names
! NOSTAT      I*4                  Number of defined stations
! NTRUV       I*4                  Number of cross-secties in u- and
!                                  v-direction
! HISIND      I*4  MXNPAR          TRISULA-NEFIS dependant index list
! HISLST     CH*20 MXNPAR          TRISULA-NEFIS possible parameterlist
! HISTYP      I*4  MXNPAR          TRISULA-NEFIS possible code list
! HISUNI     CH*20 MXNPAR          TRISULA-NEFIS possible unitlist
! SELHIS     CH*23                 Output flags containing Y or N for
!                                  various output quantities selection
!                                  for his files
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
!-----see TRISULA user Manual, release 2.45, version 0.1 May 1995
!     Appendix A: limitations number of constituents <= 5
!                             + Salinity + Temperature + Turbulence
   parameter (mxnpar = 75)
!
   integer         lang
   integer         locdep
   integer         timdep,itype
   integer         maxdef,maxlst,i     ,npar  ,nostat,ntruv
   integer         num_layers_grid  ,lmax  ,l     ,ind   ,lstci ,ltur
   integer         ierror,nrlst ,irho
!
   integer         hisind(mxnpar)
   integer         histyp(mxnpar)
   integer         partyp(maxlst)
   integer         parcod(maxlst)
!
   character       pardef(*)*(*)
   character       parlst(*)*(*)
   character       paruni(*)*(*)
   character*20    namcon(10    )
   character*20    hislst(mxnpar)
   character*20    hisuni(mxnpar)
   character*23    selhis
   character       fname(*)*(*)
   character*256   filhda,filhde
   character*256   option
!
   logical         ex    ,fout  ,zrho
!-----------------------------------------------------------------------
!-----declaration NEFIS
!-----------------------------------------------------------------------
   integer       hdefds( 2997),hdafds(  999)
!
   integer       uindex(    3),usrord,buflen       ,&
   &nbytsg,elmndm(    5),elmdms(     5)
!
   character*8   elmtyp

   character*16  grpdef,elmnam,elmqty,elmunt

   character*64  elmdes
!
   integer       GETELT,GETELS,INQELM
   integer       TMLCDP
   parameter     ( TMLCDP = IPLDEP + IPLLST )
!
   data (hisind(i),hislst(i),hisuni(i),histyp(i),i=1,15,1)/&
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
   &12,'flow rate u         ','m**3/s              ',    TMLCDP,&
   &13,'flow rate v         ','m**3/s              ',    TMLCDP,&
   &14,'                    ','                    ',    TMLCDP,&
   &15,'eddy viscosity      ','m**2/s              ',    TMLCDP/
   data (hisind(i),hislst(i),hisuni(i),histyp(i),i=16,30,1)/&
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
!  (bedstress) u and v are not correctly defined in u-points -> removed
!    *  38,'current u at u-point','m/s                 ',    TMLCDP,
!    *  39,'current v at v-point','m/s                 ',    TMLCDP,
!    *  40,'bed stress at u-pnt.','N/m**2              ',    TMLCDP,
!    *  41,'bed stress at v-pnt.','N/m**2              ',    TMLCDP,
   data (hisind(i),hislst(i),hisuni(i),histyp(i),i=31,45,1)/&
   &31,'density             ','kg/m**3             ',    TMLCDP,&
   &32,'eddy diffusivity    ','m**2/s              ',    TMLCDP,&
   &33,'accumulated flow    ','m**3                ',    TMLCDP,&
   &34,'momentary flow      ','m**3/s              ',    TMLCDP,&
   &35,'                    ','                    ',    TMLCDP,&
   &36,'                    ','                    ',    TMLCDP,&
   &37,'                    ','                    ',    TMLCDP,&
   &38,'                    ','                    ',    TMLCDP,&
   &39,'                    ','                    ',    TMLCDP,&
   &40,'                    ','                    ',    TMLCDP,&
   &41,'                    ','                    ',    TMLCDP,&
   &42,'accumulated flow u  ','m**3                ',    TMLCDP,&
   &43,'accumulated flow v  ','m**3                ',    TMLCDP,&
   &44,'                    ','                    ',    TMLCDP,&
   &45,'                    ','                    ',    TMLCDP/
   data (hisind(i),hislst(i),hisuni(i),histyp(i),i=46,55,1)/&
   &46,'adv. flux           ','user defined        ',    TMLCDP,&
   &47,'adv. flux           ','user defined        ',    TMLCDP,&
   &48,'adv. flux           ','user defined        ',    TMLCDP,&
   &49,'adv. flux           ','user defined        ',    TMLCDP,&
   &50,'adv. flux           ','user defined        ',    TMLCDP,&
   &51,'adv. flux           ','user defined        ',    TMLCDP,&
   &52,'adv. flux           ','user defined        ',    TMLCDP,&
   &53,'adv. flux           ','user defined        ',    TMLCDP,&
   &54,'adv. flux           ','user defined        ',    TMLCDP,&
   &55,'adv. flux           ','user defined        ',    TMLCDP/
   data (hisind(i),hislst(i),hisuni(i),histyp(i),i=56,65,1)/&
   &56,'dis. flux           ','user defined        ',    TMLCDP,&
   &57,'dis. flux           ','user defined        ',    TMLCDP,&
   &58,'dis. flux           ','user defined        ',    TMLCDP,&
   &59,'dis. flux           ','user defined        ',    TMLCDP,&
   &60,'dis. flux           ','user defined        ',    TMLCDP,&
   &61,'dis. flux           ','user defined        ',    TMLCDP,&
   &62,'dis. flux           ','user defined        ',    TMLCDP,&
   &63,'dis. flux           ','user defined        ',    TMLCDP,&
   &64,'dis. flux           ','user defined        ',    TMLCDP,&
   &65,'dis. flux           ','user defined        ',    TMLCDP/
   data (hisind(i),hislst(i),hisuni(i),histyp(i),i=66,75,1)/&
   &66,'tot. flux           ','user defined        ',    TMLCDP,&
   &67,'tot. flux           ','user defined        ',    TMLCDP,&
   &68,'tot. flux           ','user defined        ',    TMLCDP,&
   &69,'tot. flux           ','user defined        ',    TMLCDP,&
   &70,'tot. flux           ','user defined        ',    TMLCDP,&
   &71,'tot. flux           ','user defined        ',    TMLCDP,&
   &72,'tot. flux           ','user defined        ',    TMLCDP,&
   &73,'tot. flux           ','user defined        ',    TMLCDP,&
   &74,'tot. flux           ','user defined        ',    TMLCDP,&
   &75,'tot. flux           ','user defined        ',    TMLCDP/

!-----------------------------------------------------------------------
!-----Initialisation
!-----------------------------------------------------------------------
   fout   = .false.
   zrho   = .false.
   ierror =  0
!
!--------------------------------------------------------------------
!-----Test if trih-<runid>.dat and trih-<runid>.def Nefis HIS-files
!     exist
!--------------------------------------------------------------------
   call ods_check_nefis( fname  , '.def' , ierror )
   if ( ierror .ne. ieok   ) then
      return
   endif
!--------------------------------------------------------------------
!-----Open trih-<runid>.dat and trih-<runid>.def HIS-files
!--------------------------------------------------------------------
   call OPNNEF(fname, itype, hdafds, hdefds, ierror)
   if (ierror .ne. 0) then
      ierror = IEFIRO
      return
   endif
!--------------------------------------------------------------------
!-----Read array-dimensions from Nefis HIS-files group 2
!--------------------------------------------------------------------
   grpdef = 'his-const'
!
   uindex(1) = 1
   uindex(2) = 1
   uindex(3) = 1
   usrord    = 1
   buflen    = 4
!
   npar      = 0
   nostat    = 0
   ntruv     = 0
   lmax      = 0
   lstci     = 0
   ltur      = 0
   num_layers_grid      = 0
!
   elmnam    = 'NOSTAT'
   ierror    = GETELT(hdefds,grpdef    ,elmnam    ,&
   &uindex,usrord    ,buflen    ,NOSTAT    )
   if (ierror .ne. 0) then
      fout   = .true.
      goto 8888
   endif
!
   elmnam    = 'NTRUV'
   ierror    = GETELT(hdefds,grpdef    ,elmnam    ,&
   &uindex,usrord    ,buflen    ,NTRUV     )
   if (ierror .ne. 0) then
      fout   = .true.
      goto 8888
   endif
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
         fout   = .true.
         goto 8888
      endif
!
      elmnam = 'LTUR'
      ierror = GETELT(hdefds,grpdef    ,elmnam    ,&
      &uindex,usrord    ,buflen    ,LTUR      )
      if (ierror .ne. 0) then
         fout   = .true.
         goto 8888
      endif
   endif
!
   if (nostat .gt. 0) then
      elmnam = 'num_layers_grid'
      ierror = GETELT(hdefds,grpdef    ,elmnam    ,&
      &uindex,usrord    ,buflen    ,num_layers_grid      )
      if (ierror .ne. 0) then
         fout   = .true.
         goto 8888
      endif
   endif
!
   lmax = lstci + ltur
!
!-----------------------------------------------------------------------
!-----Element SELHIS selection of output
!-----------------------------------------------------------------------
   buflen    = 23
   elmnam    = 'SELHIS'
   ierror    = GETELS&
   &(hdefds   ,grpdef    ,elmnam    ,&
   &uindex   ,usrord    ,buflen    ,SELHIS    )
   if (ierror .ne. 0) then
!-----------------------------------------------------------------------
!--------In case of a old trih file no SELHIS then
!           re-define SELHIS
!-----------------------------------------------------------------------
      selhis = 'YYYYYYYYYYYYYYYYYYYYYYY'
      selhis(19:19) = 'X'
      if (nostat .eq. 0) selhis( 1:19) = 'NNNNNNNNNNNNNNNNNNN'
      if (num_layers_grid   .eq. 1) selhis( 4: 4) = 'N'
      if (lstci  .eq. 0) selhis( 5:12) = 'NNNNNNNN'
      if (ltur   .eq. 0) selhis(13:14) = 'NN'
      if (num_layers_grid   .eq. 1) selhis(17:18) = 'NN'
      if (lmax   .eq. 0) selhis(18:18) = 'N'
      if (ntruv  .eq. 0) selhis(20:23) = 'NNNN'
      if (lstci  .eq. 0) selhis(22:23) = 'NN'
   endif
!-----------------------------------------------------------------------
!-----Element NAMCON constituents and turbulence quantity names
!     only if selhis( 5:14) <> 'NNNNNNNNNN'
!-----------------------------------------------------------------------
   irho   = 0
   if (index (selhis( 5:14),'Y') .gt. 0) then
      buflen    = 20 * lmax
      elmnam    = 'NAMCON'
      ierror = GETELS&
      &(hdefds,grpdef    ,elmnam    ,&
      &uindex,usrord    ,buflen    ,NAMCON    )
      if (ierror .ne. 0) then
         fout   = .true.
         goto 8888
      endif
!
      do 5 l = 1,lmax
         if (selhis(4+l:4+l) .eq. 'Y' ) then
            if (namcon(l)(:11) .eq. 'Salinity   ') irho   = 1
            if (namcon(l)(:11) .eq. 'Temperature') irho   = 1
         endif
5     continue
   endif
!-----------------------------------------------------------------------
!-----In case of a old trih file re-define SELHIS(19:19)
!-----------------------------------------------------------------------
   if (selhis(19:19) .eq. 'X') then
      selhis(19:19) = 'N'
      if (irho   .eq. 1) selhis(19:19) = 'Y'
   endif
   zrho   = (irho .eq. 1)
!
!--------------------------------------------------------------------
!-----Generate parameternames from Nefis HIS-files
!--------------------------------------------------------------------
   if (nostat .gt. 0) then
      if ( selhis(1:1) .eq. 'Y' ) then
!--------water level, ZWL
         npar = npar + 1
         parcod(npar) = hisind( 1)
         parlst(npar) = hislst(parcod(npar))
         partyp(npar) = histyp(parcod(npar))
         paruni(npar) = hisuni(parcod(npar))
!--------water depth, ZWL + DPS
         npar = npar + 1
         parcod(npar) = hisind( 2)
         parlst(npar) = hislst(parcod(npar))
         partyp(npar) = histyp(parcod(npar))
         paruni(npar) = hisuni(parcod(npar))
      endif
      if ( selhis(2:3) .eq. 'YY' ) then
         if (num_layers_grid .gt. 1) then
!-----------dpt. aver. cur. u
            npar = npar + 1
            parcod(npar) = hisind( 3)
            parlst(npar) = hislst(parcod(npar))
            partyp(npar) = histyp(parcod(npar))
            paruni(npar) = hisuni(parcod(npar))
!-----------dpt. aver. cur. v
            npar = npar + 1
            parcod(npar) = hisind( 4)
            parlst(npar) = hislst(parcod(npar))
            partyp(npar) = histyp(parcod(npar))
            paruni(npar) = hisuni(parcod(npar))
!-----------dpt. aver. cur. mag.
            npar = npar + 1
            parcod(npar) = hisind( 5)
            parlst(npar) = hislst(parcod(npar))
            partyp(npar) = histyp(parcod(npar))
            paruni(npar) = hisuni(parcod(npar))
!-----------dpt. aver. cur. dir.
            npar = npar + 1
            parcod(npar) = hisind( 6)
            parlst(npar) = hislst(parcod(npar))
            partyp(npar) = histyp(parcod(npar))
            paruni(npar) = hisuni(parcod(npar))
         endif

!--------current u (layer)
         npar = npar + 1
         parcod(npar) = hisind( 7)
         parlst(npar) = hislst(parcod(npar))
         partyp(npar) = histyp(parcod(npar))
         paruni(npar) = hisuni(parcod(npar))
!--------current v (layer)
         npar = npar + 1
         parcod(npar) = hisind( 8)
         parlst(npar) = hislst(parcod(npar))
         partyp(npar) = histyp(parcod(npar))
         paruni(npar) = hisuni(parcod(npar))
!--------current mag. (layer)
         npar = npar + 1
         parcod(npar) = hisind( 9)
         parlst(npar) = hislst(parcod(npar))
         partyp(npar) = histyp(parcod(npar))
         paruni(npar) = hisuni(parcod(npar))
!--------current dir. (layer)
         npar = npar + 1
         parcod(npar) = hisind(10)
         parlst(npar) = hislst(parcod(npar))
         partyp(npar) = histyp(parcod(npar))
         paruni(npar) = hisuni(parcod(npar))
      endif

      if ( selhis(4:4) .eq. 'Y' ) then
         if (num_layers_grid .gt. 1) then
!-----------current w.   (layer)
            npar = npar + 1
            parcod(npar) = hisind(11)
            parlst(npar) = hislst(parcod(npar))
            partyp(npar) = histyp(parcod(npar))
            paruni(npar) = hisuni(parcod(npar))
         endif
      endif

      if ( selhis(20:20) .eq. 'Y' ) then
!--------flow rate u, ZQXK
         npar = npar + 1
         parcod(npar) = hisind(12)
         parlst(npar) = hislst(parcod(npar))
         partyp(npar) = histyp(parcod(npar))
         paruni(npar) = hisuni(parcod(npar))
!--------flow rate v, ZQYK
         npar = npar + 1
         parcod(npar) = hisind(13)
         parlst(npar) = hislst(parcod(npar))
         partyp(npar) = histyp(parcod(npar))
         paruni(npar) = hisuni(parcod(npar))
      endif

      if ( selhis(17:17) .eq. 'Y' ) then
         if (num_layers_grid .gt. 1) then
!-----------viscosity, ZVICWW
            npar = npar + 1
            parcod(npar) = hisind(15)
            parlst(npar) = hislst(parcod(npar))
            partyp(npar) = histyp(parcod(npar))
            paruni(npar) = hisuni(parcod(npar))
         endif
      endif

      if ( selhis(15:16) .eq. 'YY' ) then
!--------bed stress u, ZTAUKSI
         npar = npar + 1
         parcod(npar) = hisind(16)
         parlst(npar) = hislst(parcod(npar))
         partyp(npar) = histyp(parcod(npar))
         paruni(npar) = hisuni(parcod(npar))
!--------bed stress v, ZTAUETA
         npar = npar + 1
         parcod(npar) = hisind(17)
         parlst(npar) = hislst(parcod(npar))
         partyp(npar) = histyp(parcod(npar))
         paruni(npar) = hisuni(parcod(npar))
!--------bed stress mag, ZTAUETA
         npar = npar + 1
         parcod(npar) = hisind(18)
         parlst(npar) = hislst(parcod(npar))
         partyp(npar) = histyp(parcod(npar))
         paruni(npar) = hisuni(parcod(npar))
!--------bed stress dir, ZTAUETA
         npar = npar + 1
         parcod(npar) = hisind(19)
         parlst(npar) = hislst(parcod(npar))
         partyp(npar) = histyp(parcod(npar))
         paruni(npar) = hisuni(parcod(npar))
      endif

      if (lmax   .gt. 0 ) then
!-----------constituents, GRO(1:lstci), ZTUR(1:ltur)
         do 10 l = 1, lmax, 1
            if ( selhis(4+l:4+l) .eq. 'Y' ) then
               npar = npar + 1
               parcod(npar) = hisind(19+l)
               parlst(npar) = namcon(l)(1:20)
               partyp(npar) = histyp(parcod(npar))
               if (parlst(npar) .eq. 'Salinity') then
                  paruni(npar) = 'ppt'
               else if (parlst(npar) .eq. 'Temperature') then
                  paruni(npar) = 'degrees C'
               else
                  paruni(npar) = hisuni(parcod(npar))
               endif
            endif
10       continue

         if (zrho) then
!--------------density, ZRHO
            npar = npar + 1
            parcod(npar) = hisind(31)
            parlst(npar) = hislst(parcod(npar))
            partyp(npar) = histyp(parcod(npar))
            paruni(npar) = hisuni(parcod(npar))
         endif
      endif

      if ( selhis(17:17) .eq. 'Y' ) then
         if (num_layers_grid   .gt. 1) then
!--------------diffusivity, ZDICWW
            npar = npar + 1
            parcod(npar) = hisind(32)
            parlst(npar) = hislst(parcod(npar))
            partyp(npar) = histyp(parcod(npar))
            paruni(npar) = hisuni(parcod(npar))
         endif
      endif
   endif

   if (nostat .gt. 0) then

!--------bed stress at u-point - removed ( not available )
!--------bed stress at v-point - removed ( not available )

      if ( selhis(20:20) .eq. 'Y' ) then
!--------accumulated flow u
         npar = npar + 1
         parcod(npar) = hisind(42)
         parlst(npar) = hislst(parcod(npar))
         partyp(npar) = histyp(parcod(npar))
         paruni(npar) = hisuni(parcod(npar))
!--------accumulated flow v
         npar = npar + 1
         parcod(npar) = hisind(43)
         parlst(npar) = hislst(parcod(npar))
         partyp(npar) = histyp(parcod(npar))
         paruni(npar) = hisuni(parcod(npar))
      endif
   endif
!
   if (ntruv  .gt. 0) then
      if ( selhis(20:20) .eq. 'Y' ) then
!--------momentary flow , FLTR
         npar = npar + 1
         parcod(npar) = hisind(33)
         parlst(npar) = hislst(parcod(npar))
         partyp(npar) = histyp(parcod(npar))
         paruni(npar) = hisuni(parcod(npar))
!--------accumulated flow,CTR
         npar = npar + 1
         parcod(npar) = hisind(34)
         parlst(npar) = hislst(parcod(npar))
         partyp(npar) = histyp(parcod(npar))
         paruni(npar) = hisuni(parcod(npar))
      endif

      if (lstci  .gt. 0) then
         do 20 l = 1, lstci, 1
            if ( selhis(22:22) .eq. 'Y' ) then
!--------------advective flux, ATR(1:lstci)
               npar = npar + 1
               parcod(npar) = hisind(46+l)
               parlst(npar) = hislst(parcod(npar))(1:10) //&
               &namcon(l)(1:10)
               partyp(npar) = histyp(parcod(npar))
               paruni(npar) = hisuni(parcod(npar))
            endif
20       continue

         do 30 l = 1, lstci, 1
            if ( selhis(22:22) .eq. 'Y' ) then
!--------------dispersive flux, DTR(1:lstci)
               npar = npar + 1
               parcod(npar) = hisind(56+l)
               parlst(npar) = hislst(parcod(npar))(1:10) //&
               &namcon(l)(1:10)
               partyp(npar) = histyp(parcod(npar))
               paruni(npar) = hisuni(parcod(npar))
            endif
30       continue
!
         do 40 l = 1, lstci, 1
            if ( selhis(22:22) .eq. 'Y' ) then
!--------------total flux, (ATR + DTR)(1:lstci)
               npar = npar + 1
               parcod(npar) = hisind(66+l)
               parlst(npar) = hislst(parcod(npar))(1:10) //&
               &namcon(l)(1:10)
               partyp(npar) = histyp(parcod(npar))
               paruni(npar) = hisuni(parcod(npar))
            endif
40       continue
      endif
   endif

   nrlst = npar
!
!-----------------------------------------------------------------------
!-----check; filter not yet used so found number should be less or equal
!     the required number
!-----------------------------------------------------------------------
   if (npar .gt. maxlst) then
      fout  = .true.
   endif
!--------------------------------------------------------------------
!-----Close trih-<runid>.dat and trih-<runid>.def HIS-files
!--------------------------------------------------------------------
8888 continue
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

subroutine hisdim&
&(fname ,itype ,dimtyp, pardep, timdep,&
&locdep,ndim  ,ierror, option        )
!-----------------------------------------------------------------------
!           Function: dimension selection for time histories
!                     TRISULA NEFIS files
!        Method used:
!
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
! FOUT        L*4                  Flag for further execution program
!                                  fout  = .true.  : stop execution
!                                  fout  = .false. : go on
! FILHDA     CH*256                File name NEFIS data file for HIS
! FILHDE     CH*256                File name NEFIS definition file for
!                                  HIS
! GRPDEF     CH*16                 Group name definition
! GRPDMS      I*4  5               Array with GRPNDM dimensions
! GRPNDM      I*4                  Number of dimenmsions of this group
! GRPORD      I*4  5               Array which gives order in which data
!                                  must be read
! HDAFDS      I*4  999             Data file descriptor for the HIS-DAT
!                                  file
! HDEFDS      I*4  2997            Definition file description for the
!                                  HIS-DEF file
! IERROR      I*4                  Error code for NEFIS error
! num_layers_grid        I*4                  Number of layers
! L           I*4                  Help variable
! LMAX        I*4                  Total number of constituents
!                                  for old files LMAX = LSTCI
!                                  for new files LMAX = LSTCI + LTUR
! LSTCI       I*4                  Total number of constituents (incl.
!                                  turbulence for old trim files).
! LTUR        I*4                  Number of turbulence constituents
! NOSTAT      I*4                  Number of defined stations
! NPAR        I*4                  Number of found parameters
! NRCEL       I*4                  Number of cells defined in group 1&3
! NTRUV       I*4                  Number of cross-secties in u- and
!                                  v-direction
! SELHIS     CH*23                 Output flags containing Y or N for
!                                  various output quantities selection
!                                  for his files
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
   integer         ierror,itype ,lstci ,ltur  ,irho
   integer         nrcel ,npar  ,nostat,ntruv ,lmax  ,num_layers_grid  ,l
   integer         pardep,timdep,locdep
   integer         ndim   (4    )
!
   character*3     dimtyp
   character       fname(*)*(*)
   character*20    namcon(10)
   character*23    selhis
   character*256   filhda,filhde
   character*256   option
!
   logical         ex    ,fout  ,zrho  ,check
!-----------------------------------------------------------------------
!-----declaration NEFIS
!-----------------------------------------------------------------------
   integer       hdefds( 2997),hdafds(  999)
!
   integer       grpndm,grpdms(    5),grpord(    5),&
   &uindex(    3),usrord,buflen       ,&
   &nbytsg,elmndm(    5),elmdms(     5),&
   &ind
!
   character*8   elmtyp
!
   character*16  grpdef,elmnam,celnam,elmqty,elmunt
!
   character*64  elmdes
!
   integer       INQGRP,GETELT,GETELS,INQELM
   integer       INQMXI

!-----------------------------------------------------------------------
!-----Initialisation
!-----------------------------------------------------------------------
   fout   = .false.
   zrho   = .false.
!
   ierror =  0
!
!--------------------------------------------------------------------
!-----Test if trih-<runid>.dat and trih-<runid>.def Nefis HIS-files
!     exist
!--------------------------------------------------------------------
   call ods_check_nefis( fname  , '.def' , ierror )
   if ( ierror .ne. ieok   ) then
      return
   endif
!--------------------------------------------------------------------
!-----Open trih-<runid>.dat and trih-<runid>.def HIS-files
!--------------------------------------------------------------------
   call OPNNEF(fname, itype, hdafds, hdefds, ierror)
   if (ierror .ne. 0) then
      ierror = IEFIRO
      return
   endif
!--------------------------------------------------------------------
!-----Read array-dimension nrcel from Nefis HIS-files group 1
!--------------------------------------------------------------------
   grpdef = 'his-info-series'
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
      if (nrcel  .eq. 0) then
         fout   = .true.
         goto 8888
      endif
   endif
!--------------------------------------------------------------------
!-----Read array-dimensions from Nefis HIS-files group 2
!--------------------------------------------------------------------
   grpdef = 'his-const'
!
   uindex(1) = 1
   uindex(2) = 1
   uindex(3) = 1
   usrord    = 1
   buflen    = 4
!
   npar      = 0
   nostat    = 0
   ntruv     = 0
   lmax      = 0
   lstci     = 0
   ltur      = 0
   num_layers_grid      = 0
!
   elmnam    = 'NOSTAT'
   ierror    = GETELT(hdefds,grpdef    ,elmnam    ,&
   &uindex,usrord    ,buflen    ,NOSTAT    )
   if (ierror .ne. 0) then
      fout   = .true.
      goto 8888
   endif
!
   elmnam    = 'NTRUV'
   ierror    = GETELT(hdefds,grpdef    ,elmnam    ,&
   &uindex,usrord    ,buflen    ,NTRUV     )
   if (ierror .ne. 0) then
      fout   = .true.
      goto 8888
   endif
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
         fout   = .true.
         goto 8888
      endif

      elmnam = 'LTUR'
      ierror = GETELT(hdefds,grpdef    ,elmnam    ,&
      &uindex,usrord    ,buflen    ,LTUR      )
      if (ierror .ne. 0) then
         fout   = .true.
         goto 8888
      endif
   endif
!
   if (nostat .gt. 0) then
      elmnam = 'num_layers_grid'
      ierror = GETELT(hdefds,grpdef    ,elmnam    ,&
      &uindex,usrord    ,buflen    ,num_layers_grid      )
      if (ierror .ne. 0) then
         fout   = .true.
         goto 8888
      endif
   endif
!
   lmax = lstci + ltur
!
!-----------------------------------------------------------------------
!-----Element SELHIS selection of output
!-----------------------------------------------------------------------
   buflen    = 23
   elmnam    = 'SELHIS'
   ierror    = GETELS&
   &(hdefds   ,grpdef    ,elmnam    ,&
   &uindex   ,usrord    ,buflen    ,SELHIS    )
   if (ierror .ne. 0) then
!-----------------------------------------------------------------------
!--------In case of a old trih file no SELHIS then
!           re-define SELHIS
!-----------------------------------------------------------------------
      selhis = 'YYYYYYYYYYYYYYYYYYYYYYY'
      selhis(19:19) = 'X'
      if (nostat .eq. 0) selhis( 1:19) = 'NNNNNNNNNNNNNNNNNNN'
      if (num_layers_grid   .eq. 1) selhis( 4: 4) = 'N'
      if (lstci  .eq. 0) selhis( 5:12) = 'NNNNNNNN'
      if (ltur   .eq. 0) selhis(13:14) = 'NN'
      if (num_layers_grid   .eq. 1) selhis(17:18) = 'NN'
      if (lmax   .eq. 0) selhis(18:18) = 'N'
      if (ntruv  .eq. 0) selhis(20:23) = 'NNNN'
      if (lstci  .eq. 0) selhis(22:23) = 'NN'
   endif
!-----------------------------------------------------------------------
!-----Element NAMCON constituents and turbulence quantity names
!     only if selhis( 5:14) <> 'NNNNNNNNNN'
!-----------------------------------------------------------------------
   irho   = 0
   if (index (selhis( 5:14),'Y') .gt. 0) then
      buflen    = 20 * lmax
      elmnam    = 'NAMCON'
      ierror    = GETELS&
      &(hdefds,grpdef    ,elmnam    ,&
      &uindex,usrord    ,buflen    ,NAMCON    )
      if (ierror .ne. 0) then
         fout   = .true.
         goto 8888
      endif
!
      do 5 l = 1,lmax
         if (selhis(4+l:4+l) .eq. 'Y' ) then
            if (namcon(l)(:11) .eq. 'Salinity   ') irho   = 1
            if (namcon(l)(:11) .eq. 'Temperature') irho   = 1
         endif
5     continue
   endif
!-----------------------------------------------------------------------
!-----In case of a old trih file re-define SELHIS(19:19)
!-----------------------------------------------------------------------
   if (selhis(19:19) .eq. 'X') then
      selhis(19:19) = 'N'
      if (irho   .eq. 1) selhis(19:19) = 'Y'
   endif
   zrho   = (irho .eq. 1)
!
!--------------------------------------------------------------------
!-----Generate parameternames from Nefis HIS-files
!--------------------------------------------------------------------
   if (nostat .gt. 0) then
      if ( selhis(1:1) .eq. 'Y' ) then
!--------water level, ZWL
         npar = npar + 1
!--------water depth, ZWL + DPS
         npar = npar + 1
      endif
      if ( selhis(2:3) .eq. 'YY' ) then
         if (num_layers_grid .gt. 1) then
!-----------dpt. aver. cur. u
            npar = npar + 1
!-----------dpt. aver. cur. v
            npar = npar + 1
!-----------dpt. aver. cur. mag.
            npar = npar + 1
!-----------dpt. aver. cur. dir.
            npar = npar + 1
         endif

!--------current u (layer)
         npar = npar + 1
!--------current v (layer)
         npar = npar + 1
!--------current mag. (layer)
         npar = npar + 1
!--------current dir. (layer)
         npar = npar + 1
      endif

      if ( selhis(4:4) .eq. 'Y' ) then
         if (num_layers_grid .gt. 1) then
!-----------current w.   (layer)
            npar = npar + 1
         endif
      endif

      if ( selhis(20:20) .eq. 'Y' ) then
!--------flow rate u, ZQXK
         npar = npar + 1
!--------flow rate v, ZQYK
         npar = npar + 1
      endif

      if ( selhis(17:17) .eq. 'Y' ) then
         if (num_layers_grid .gt. 1) then
!-----------viscosity, ZVICWW
            npar = npar + 1
         endif
      endif

      if ( selhis(15:16) .eq. 'YY' ) then
!--------bottomstress u, ZTAUKSI
         npar = npar + 1
!--------bottomstress v, ZTAUETA
         npar = npar + 1
!--------bottomstress mag, ZTAUETA
         npar = npar + 1
!--------bottomstress dir, ZTAUETA
         npar = npar + 1
      endif

      if (lmax   .gt. 0) then
!-----------constituents, GRO(1:lstci), ZTUR(1:ltur)
         do 10 l = 1, lmax, 1
            if ( selhis(4+l:4+l) .eq. 'Y' ) then
               npar = npar + 1
            endif
10       continue

         if (zrho) then
!--------------density, ZRHO
            npar = npar + 1
         endif
      endif

      if ( selhis(17:17) .eq. 'Y' ) then
         if (num_layers_grid   .gt. 1) then
!--------------diffusivity, ZDICWW
            npar = npar + 1
         endif
      endif
   endif


   if (nostat .gt. 0) then

!--------bottom stress at u-point
! removed         npar = npar + 1
!--------bottom stress at v-point
! removed         npar = npar + 1

      if ( selhis(20:20) .eq. 'Y' ) then
!--------accumulated flow u
         npar = npar + 1
!--------accumulated flow v
         npar = npar + 1
      endif
   endif

   if (ntruv  .gt. 0) then
      if ( selhis(20:20) .eq. 'Y' ) then
!--------momentary flow , FLTR
         npar = npar + 1
!--------accumulated flow,CTR
         npar = npar + 1
      endif

      if (lstci   .gt. 0) then
         do 20 l = 1, lstci, 1
            if ( selhis(22:22) .eq. 'Y' ) then
!--------------advective flux, ATR(1:lstci)
               npar = npar + 1
            endif
20       continue

         do 30 l = 1, lstci, 1
            if ( selhis(22:22) .eq. 'Y' ) then
!--------------dispersive flux, DTR(1:lstci)
               npar = npar + 1
            endif
30       continue

         do 40 l = 1, lstci, 1
            if ( selhis(22:22) .eq. 'Y' ) then
!--------------total flux, (ATR + DTR)(1:lstci)
               npar = npar + 1
            endif
40       continue
      endif
   endif
!
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
      ndim (1) = 1
      ndim (2) = 1
      ndim (3) = 1
      ndim (4) = 1
!--------select parameters with more then one layer
      check    =(pardep .ge.  7 .and. pardep .le. 13).or.&
      &pardep .eq. 15 .or.&
      &(pardep .ge. 20 .and. pardep .le. 29).or.&
      &pardep .eq. 31 .or.  pardep .eq. 32 .or.&
      &pardep .eq. 42 .or.  pardep .eq. 43
      if (num_layers_grid .gt. 1 .and. check) then
!-----------parameter with more then one layer
         ndim (1) = 3
         if (pardep .eq. 15 .or.  pardep .eq. 32 ) then
!--------------ZVICWW or ZDICWW with 0:num_layers_grid
            ndim (3) = num_layers_grid + 1
         else
!--------------all others 1:num_layers_grid
            ndim (3) = num_layers_grid
         endif
      else
!-----------parameter with one layer
         ndim (1) = 1
      endif
!--------pardep in { 1...32, 38...43} is station
!        pardep in {33...34, 46...75} is cross section
      check    = (pardep .ge.  1 .and. pardep .le. 32) .or.&
      &(pardep .ge. 38 .and. pardep .le. 43)
      if (check) then
         ndim (2) = nostat
      else
         ndim (2) = ntruv
      endif
   else
      fout  = .true.
   endif
!
!--------------------------------------------------------------------
!-----Close trih-<runid>.dat and trih-<runid>.def HIS-files
!--------------------------------------------------------------------
8888 continue
   call CLOSFL(fname, ierror)
   if (ierror .ne. 0) then
      fout  = .true.
   endif
!-----------------------------------------------------------------------
!-----return status to calling routine
!-----------------------------------------------------------------------
   ierror = IEOK
   if (fout) then
      ierror = IEOTHR
   endif
!
   return
!-----------------------------------------------------------------------
end

subroutine histme&
&(fname  ,itype  ,timdef, maxdef ,pardep , locdep,&
&maxlst ,        timlst,         timtyp ,&
&nrlst  ,ierror ,option                         )
!-----------------------------------------------------------------------
!           Function: time selection for time histories
!                     TRISULA NEFIS files
!        Method used:
!
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
! FOUT        L*4                  Flag for further execution program
!                                  fout  = .true.  : stop execution
!                                  fout  = .false. : go on
! FILHDA     CH*256                File name NEFIS data file for HIS
! FILHDE     CH*256                File name NEFIS definition file for
!                                  HIS
! GRPDEF     CH*16                 Group name definition
! GRPDMS      I*4  5               Array with GRPNDM dimensions
! GRPNDM      I*4                  Number of dimenmsions of this group
! GRPORD      I*4  5               Array which gives order in which data
!                                  must be read
! HDAFDS      I*4  999             Data file descriptor for the HIS-DAT
!                                  file
! HDEFDS      I*4  2997            Definition file description for the
!                                  HIS-DEF file
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
   real*8          timlst(maxlst)
   real*8          timdef(maxdef,2)
   real            dt    ,tunit
!
   character       fname(*)*(*)
   character*256   filhda,filhde
   character*256   option
!
   logical         ex    ,fout
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
   fout   = .false.
   ierror =  0
!
!--------------------------------------------------------------------
!-----Test if trih-<runid>.dat and trih-<runid>.def Nefis HIS-files
!     exist
!--------------------------------------------------------------------
   call ods_check_nefis( fname  , '.def' , ierror )
   if ( ierror .ne. ieok   ) then
      return
   endif
!--------------------------------------------------------------------
!-----Open trih-<runid>.dat and trih-<runid>.def HIS-files
!--------------------------------------------------------------------
   call OPNNEF(fname, itype, hdafds, hdefds, ierror )
   if (ierror .ne. 0) then
      ierror = IEFIRO
      return
   endif
!--------------------------------------------------------------------
!-----Read array-dimension nrcel from Nefis HIS-files group 1
!--------------------------------------------------------------------
   grpdef = 'his-info-series'
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
      if (nrcel  .eq. 0) then
         fout   = .true.
         goto 8888
      endif
   endif
!--------------------------------------------------------------------
!-----Read constants from Nefis HIS-files group 2
!--------------------------------------------------------------------
   grpdef    = 'his-const'
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
   grpdef    = 'his-info-series'
   elmnam    = 'ITHISC'
!
   uindex(1) = 1
   uindex(2) = 1
   uindex(3) = 1
   usrord    = 1
   buflen    = 4
!-----------------------------------------------------------------------
!-----Define number of rows default nrcel, if an error
!     occurres (ierror <> 0) then re-define
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
!-----check; found number should be less or equal required number
!-----------------------------------------------------------------------
   if (nrcel .gt. maxlst) then
      fout  = .true.
   endif
!--------------------------------------------------------------------
!-----Close trih-<runid>.dat and trih-<runid>.def HIS-files
!--------------------------------------------------------------------
8888 continue
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

subroutine julind(hdefds, hdafds, tim, nindex, ierror)
!-----------------------------------------------------------------------
!           Function: transform julian day to index in time series
!        Method used:
!
!-----------------------------------------------------------------------
!     Tree structure:
!
!         JULIND
!                INQGRP (nf)
!                INQMXI (nf)
!                GETELT (nf)
!
!     Explanation   :   md     - machine depended routine
!                       nf     - nefis functions
!-----------------------------------------------------------------------
!   Calling routine :              HISMAT
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
! HDAFDS      I*4  999        I    Data file descriptor for the HIS-DAT
!                                  file
! HDEFDS      I*4  2997       I    Definition file description for the
!                                  HIS-DEF file
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
! GRPNDM      I*4                  Number of dimenmsions of this group
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
! TIMSTP      R*8                  Current julian day for this step
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
   real            dt    ,tunit
   real*8          tim(3)
   real*8          timstp
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
!-----Read array-dimension nrcel from Nefis HIS-files group 1
!--------------------------------------------------------------------
   grpdef = 'his-info-series'
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
!-----Read constants from Nefis HIS-files group 2
!--------------------------------------------------------------------
   grpdef    = 'his-const'
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
   grpdef    = 'his-info-series'
   elmnam    = 'ITHISC'
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
      if ( timstp .le. tim(1) ) then
         nindex(1) = i
      else
         if ( timstp .le. tim(2) ) then
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

subroutine hisloc&
&(fname  ,itype  ,locdef ,maxdef ,pardep ,timdep ,&
&maxlst ,        loclst ,        loctyp ,nrlst  ,&
&locnr  ,ierror ,zbuffs ,option                 )
!-----------------------------------------------------------------------
!           Function: parameter name selection for time histories
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
! BUFLEN      I*4                  Size in bytes of available buffer
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
! NOSTAT      I*4                  Number of defined stations
! NTRUV       I*4                  Number of cross-secties in u- and
!                                  v-direction
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
   integer         i     ,nostat,ntruv
   integer         maxlst,itype
   integer         maxdef
   integer         pardep
   integer         timdep
   integer         loctyp(maxlst)
   integer         locnr (maxlst)
!
   character       locdef(*)*(*)
   character       loclst(*)*(*)
   character*20    zbuffs(maxlst)
   character       fname(*)*(*)
   character*256   filhda,filhde
   character*256   option
!
   logical         ex    ,fout  ,check
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
   fout   = .false.
   ierror =  0
!--------------------------------------------------------------------
!-----Test if trih-<runid>.dat and trih-<runid>.def Nefis HIS-files
!     exist
!--------------------------------------------------------------------
   call ods_check_nefis( fname  , '.def' , ierror )
   if ( ierror .ne. ieok   ) then
      return
   endif
!--------------------------------------------------------------------
!-----Open trih-<runid>.dat and trih-<runid>.def HIS-files
!--------------------------------------------------------------------
   call OPNNEF(fname, itype, hdafds, hdefds, ierror )
   if (ierror .ne. 0) then
      ierror = IEFIRO
      return
   endif
!--------------------------------------------------------------------
!-----Read array-dimensions from Nefis HIS-files group 2
!--------------------------------------------------------------------
   grpdef = 'his-const'
!
   uindex(1) = 1
   uindex(2) = 1
   uindex(3) = 1
   usrord    = 1
   buflen    = 4
!
   nloc      = 0
   nostat    = 0
   ntruv     = 0
!
   elmnam    = 'NOSTAT'
   ierror    = GETELT(hdefds,grpdef    ,elmnam    ,&
   &uindex,usrord    ,buflen    ,NOSTAT    )
   if (ierror .ne. 0) then
      fout   = .true.
      goto 8888
   endif
!
   elmnam    = 'NTRUV'
   ierror    = GETELT(hdefds,grpdef    ,elmnam    ,&
   &uindex,usrord    ,buflen    ,NTRUV     )
   if (ierror .ne. 0) then
      fout   = .true.
      goto 8888
   endif
!--------------------------------------------------------------------
!-----Generate location names from Nefis HIS-files
!-----pardep in { 1...32, 38...43} is station
!     pardep in {33...34, 46...75} is cross section
!--------------------------------------------------------------------
   check    = pardep .ge.  1 .and. pardep .le. 32 .or.&
   &pardep .ge. 38 .and. pardep .le. 43
!
   if (check) then
!--------get station locations
      grpdef = 'his-const'
      uindex(1) = 1
      uindex(2) = 1
      uindex(3) = 1
      usrord    = 1
      buflen    = 20 * nostat
      elmnam    = 'NAMST'
      ierror    = GETELS&
      &(hdefds,grpdef    ,elmnam    ,&
      &uindex,usrord    ,buflen    ,zbuffs     )
      if (ierror .ne. 0) then
         fout   = .true.
         goto 8888
      endif
!
      do 10 i = 1, nostat
         nloc = nloc + 1
         loclst (nloc) = zbuffs(i)
         locnr  (nloc) = i
10    continue

   else
!--------get cross section locations
      grpdef = 'his-const'
      uindex(1) = 1
      uindex(2) = 1
      uindex(3) = 1
      usrord    = 1
      buflen    = 20 * ntruv
      elmnam    = 'NAMTRA'
      ierror    = GETELS&
      &(hdefds,grpdef    ,elmnam    ,&
      &uindex,usrord    ,buflen    ,zbuffs     )
      if (ierror .ne. 0) then
         fout   = .true.
         goto 8888
      endif
!
      do 20 i = 1, ntruv
         nloc = nloc + 1
         loclst (nloc) = zbuffs(i)
         locnr  (nloc) = i
20    continue
!
   endif

   nrlst = nloc

!-----------------------------------------------------------------------
!-----check found number against required number
!     filter not yet used
!-----------------------------------------------------------------------
   if (nloc   .gt. maxlst) then
      fout   = .true.
   endif
!--------------------------------------------------------------------
!-----Close trih-<runid>.dat and trih-<runid>.def HIS-files
!--------------------------------------------------------------------
8888 continue
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

subroutine hismat&
&(fname ,itype  ,parcod, loc   , tim   ,misval,&
&i3gl  ,maxdim ,xdata , ierror, option,zbuffs)
!-----------------------------------------------------------------------
!           Function: select history data out of
!                     TRISULA NEFIS files
!        Method used:
!
!-----------------------------------------------------------------------
!   Calling routine :              GETMAT
!-----------------------------------------------------------------------
!   Called  routines:              OPNNEF
!                                  CLOSFL
!                                  GETELT (nefis)
!                                  M3HWAT
!                                  M3HCDA
!                                  M3HCUR
!                                  M3HCRS
!                                  M3H2D
!                                  M3H15
!                                  M3H20
!                                  M3H29
!                                  M3H31
!                                  M3H32
!                                  M3HFUV
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
! MAXDIM      I*4            I     length of data array
! XDATA       R*4   maxdim   O     array with the data
! IERROR      I*4            O     = 0 no errors, = 1 error detected
! OPTION     CH*256          O     option (not used)
! ZBUFFS      R*4   <len>    O/I   buffer for reading Nefis file:
!                                  if ( 1<=parcod<=32 .or.
!                                      38<=parcod<=43    ) then
!                                       <len> = nostat * lmaxd * num_layers_grid
!                                  else
!                                       <len> = ntruv  * lmaxd
!                                  endif
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
! FILHDA     CH*256                File name NEFIS data file for HIS
! FILHDE     CH*256                File name NEFIS definition file for
!                                  HIS
! FOUT        L*4                  Flag for further execution program
!                                  fout  = .true.  : stop execution
!                                  fout  = .false. : go on
! GRPDEF     CH*16                 Group name definition
! GRPDMS      I*4  1               Array with GRPNDM dimensions
! GRPNDM      I*4                  Number of dimenmsions of this group
! GRPORD      I*4  1               Array which gives order in which data
!                                  must be read
! HDAFDS      I*4  999             Data file descriptor for the HIS-DAT
!                                  file
! HDEFDS      I*4  2997            Definition file description for the
! IERROR      I*4                  Error code for NEFIS error
! ISTAT       I*4                  index of station
! num_layers_grid        I*4                  Number of layers
! LMAX        I*4                  Total number of constituents
!                                  for old files LMAX = LSTCI
!                                  for new files LMAX = LSTCI + LTUR
! LSTCI       I*4                  Total number of constituents (incl.
!                                  turbulence for old trim files).
! LTUR        I*4                  Number of turbulence constituents
! LMAXD       I*4                  maximum(1,LMAX)
! LAY         I*4                  Actual layer number
! N           I*4                  Counter for XDATA
! NOSTAT      I*4                  Number of stations
! NTRUV       I*4                  Number of cross-sections
! NINDEX      I*4  3               indices of time frame in Nefis file
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
   real*8          tim   (3)

   integer         maxdim, itype
   integer         parcod
   integer         zturid
   integer         loc   (3,3)
   integer         i3gl

   real            misval
   real            xdata (maxdim)
   real            zbuffs (*     )

   character       fname (*)*(*)
   character*256   option
!-----------------------------------------------------------------------
!-----declaration Local variables
!-----------------------------------------------------------------------
   character*256   filhda,filhde
!
   integer         nindex (3)
   integer         istat ,lay   ,ind   ,lstci ,ltur
   integer         ierror,num_layers_grid  ,lmax  ,nostat,ntruv
!
   logical         ex    ,fout
!
!-----------------------------------------------------------------------
!-----declaration NEFIS
!-----------------------------------------------------------------------
   integer       hdefds( 2997),hdafds(  999)
   integer       uindex(    3),usrord,buflen
   character*16  grpdef,elmnam
   integer       GETELT,GETELS
!-----------------------------------------------------------------------
!-----Initialisation
!-----------------------------------------------------------------------
!
   ierror =  0
   fout   = .false.
!--------------------------------------------------------------------
!-----Test if trih-<runid>.dat and trih-<runid>.def Nefis HIS-files
!     exist
!--------------------------------------------------------------------
   call ods_check_nefis( fname  , '.def' , ierror )
   if ( ierror .ne. ieok   ) then
      return
   endif
!--------------------------------------------------------------------
!-----Open trih-<runid>.dat and trih-<runid>.def HIS-files
!--------------------------------------------------------------------
   call OPNNEF(fname, itype, hdafds, hdefds, ierror)
   if (ierror .ne. 0) then
      ierror = IEFIRO
      return
   endif
!-----------------------------------------------------------------------
!-----Get array dimensions form NEFIS-HIS files group 2
!-----------------------------------------------------------------------
   grpdef = 'his-const'
!
   uindex(1) = 1
   uindex(2) = 1
   uindex(3) = 1
   usrord    = 1
   buflen    = 4
!
   nostat    = 0
   ntruv     = 0
   num_layers_grid      = 0
   lmax      = 0
   lstci     = 0
   ltur      = 0
!
   elmnam    = 'NOSTAT'
   ierror    = GETELT(hdefds,grpdef    ,elmnam    ,&
   &uindex,usrord    ,buflen    ,NOSTAT    )
   if (ierror .ne. 0) then
      fout   = .true.
      goto 8888
   endif
!
   elmnam    = 'NTRUV'
   ierror    = GETELT(hdefds,grpdef    ,elmnam    ,&
   &uindex,usrord    ,buflen    ,NTRUV     )
   if (ierror .ne. 0) then
      fout   = .true.
      goto 8888
   endif
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
         fout   = .true.
         goto 8888
      endif

      elmnam = 'LTUR'
      ierror = GETELT(hdefds,grpdef    ,elmnam    ,&
      &uindex,usrord    ,buflen    ,LTUR      )
      if (ierror .ne. 0) then
         fout   = .true.
         goto 8888
      endif
   endif
!
   if (nostat .gt. 0) then
      elmnam = 'num_layers_grid'
      ierror = GETELT(hdefds,grpdef    ,elmnam    ,&
      &uindex,usrord    ,buflen    ,num_layers_grid      )
      if (ierror .ne. 0) then
         fout   = .true.
         goto 8888
      endif
   endif
!
   lmax = lstci + ltur
!
!-----------------------------------------------------------------------
!-----calculate indices for time frame
!-----------------------------------------------------------------------
   call julind (hdefds, hdafds, tim,   nindex, ierror)
   if (ierror .ne. 0) then
      fout  = .true.
      goto 8888
   endif
!-----------------------------------------------------------------------
!-----set loc (station) {precon: loc(1,1)=loc(2,1) and loc(3,1)=1}
!     plottable timeseries presumed
!     HACK:
!     insure a reasonable value for index
!     Note:
!     Thanks to the location codes, the index for the horizontal
!     direction does not require a correction. This is not true
!     for the index in the vertical. So, this is corrected by 1.
!     (Blame it on somebody!)
!-----------------------------------------------------------------------
   istat  = max( 1, loc(1,1) )
!--------------------------------------------------------------------
!-----set loc (layer  ) {precon: loc(1,3)=loc(2,3) and loc(3,3)=1}
!     plottable timeseries presumed
!--------------------------------------------------------------------
   lay    = 1 + max( 0, loc(1,3) )
!--------------------------------------------------------------------
!--------------------------------------------------------------------
!-----get data
!--------------------------------------------------------------------
   if (parcod .eq. 1 ) then
!--------water level, ZWL
      call m3hwat (hdefds, hdafds, nindex, maxdim, istat , nostat,&
      &parcod, xdata , ierror, zbuffs)
   else if (parcod .eq.  2 ) then
!--------water depth, ZWL + DPS
      call m3hwat (hdefds, hdafds, nindex, maxdim, istat , nostat,&
      &parcod, xdata , ierror, zbuffs)
   else if (parcod .ge.  3 .and. parcod .le. 6 ) then
!--------dpt. aver. cur. u         {precon: parcod= 3}
!--------dpt. aver. cur. v         {precon: parcod= 4}
!--------dpt. aver. cur. mag.      {precon: parcod= 5}
!--------dpt. aver. cur. dir.      {precon: parcod= 6}
      call m3hcda (hdefds, hdafds, nindex, maxdim, istat , nostat,&
      &num_layers_grid  ,         parcod, xdata , ierror, zbuffs)
   else if (parcod .ge.  7 .and. parcod .le. 10) then
!--------current u (layer)         {precon: parcod= 7}
!--------current v (layer)         {precon: parcod= 8}
!--------current mag. (layer)      {precon: parcod= 9}
!--------current dir. (layer)      {precon: parcod=10}
      call m3hcur (hdefds, hdafds, nindex, maxdim, istat , nostat,&
      &num_layers_grid  , lay   , parcod, xdata , ierror, zbuffs)
   else if (parcod .eq. 11 ) then
!--------current w.   (layer)
      call m3h2d  (hdefds, hdafds, nindex, maxdim, istat , nostat,&
      &num_layers_grid  , lay   ,'ZCURW ',xdata , ierror, zbuffs)
   else if (parcod .eq. 12 ) then
!--------flow rate u, ZQXK
      call m3h2d  (hdefds, hdafds, nindex, maxdim, istat , nostat,&
      &num_layers_grid  , lay   ,'ZQXK  ',xdata , ierror, zbuffs)
   else if (parcod .eq. 13 ) then
!--------flow rate v, ZQYK
      call m3h2d  (hdefds, hdafds, nindex, maxdim, istat , nostat,&
      &num_layers_grid  , lay   ,'ZQYK  ',xdata , ierror, zbuffs)
   else if (parcod .eq. 15 ) then
!--------viscosity, ZVICWW
      call m3h15  (hdefds, hdafds, nindex, maxdim, istat , nostat,&
      &num_layers_grid  , lay   ,         xdata , ierror, zbuffs)
   else if (parcod .ge. 16 .and. parcod .le.19 ) then
!--------bottomstress u, ZTAUKSI   {precon: parcod=16}
!--------bottomstress v, ZTAUETA   {precon: parcod=17}
!--------bottomstress mag, ZTAUETA {precon: parcod=18}
!--------bottomstress dir, ZTAUETA {precon: parcod=19}
      call m3hwat (hdefds, hdafds, nindex, maxdim, istat , nostat,&
      &parcod, xdata , ierror, zbuffs)
   else if (parcod .ge. 20 .and. parcod .le.29 ) then
      if (parcod-19 .le. lstci) then
!-----------constituents, GRO (1:lstci) {precon: <con.index>=parcod-19}
         call m3h20(hdefds, hdafds, nindex, maxdim, istat , nostat,&
         &num_layers_grid  , lay   , lmax  , parcod, xdata , ierror,&
         &zbuffs)
      else
!-----------constituents, ZTUR(1:ltur )
!                        {precon: <con.index>=parcod-19-lstci}
         zturid = parcod - 19 - lstci
         call m3h29(hdefds, hdafds, nindex, maxdim, istat , nostat,&
         &num_layers_grid  , lay   , ltur  , zturid, xdata , ierror,&
         &zbuffs)
      endif
   else if (parcod .eq. 31 ) then
!--------density, ZRHO
      call m3h31  (hdefds, hdafds, nindex, maxdim, istat , nostat,&
      &num_layers_grid  , lay   ,         xdata , ierror, zbuffs)
   else if (parcod .eq. 32 ) then
!--------diffusivity, ZDICWW
      call m3h32  (hdefds, hdafds, nindex, maxdim, istat , nostat,&
      &num_layers_grid  , lay   ,         xdata , ierror, zbuffs)
   else if (parcod .eq. 33 ) then
!--------accumulated flow,CTR
      call m3hcrs (hdefds, hdafds, nindex, maxdim, istat , ntruv ,&
      &1     , lstci , parcod, xdata , ierror,&
      &zbuffs)
   else if (parcod .eq. 34 ) then
!--------momentary flow , FLTR
      call m3hcrs (hdefds, hdafds, nindex, maxdim, istat , ntruv ,&
      &1     , lstci , parcod, xdata , ierror,&
      &zbuffs)
   else if (parcod .eq. 42 ) then
!--------accumulated flow u
      call m3hfuv (hdefds, hdafds, nindex, maxdim, istat , nostat,&
      &num_layers_grid  , lay   ,'ZQXK  ',xdata , ierror, zbuffs)
   else if (parcod .eq. 43 ) then
!--------accumulated flow v
      call m3hfuv (hdefds, hdafds, nindex, maxdim, istat , nostat,&
      &num_layers_grid  , lay   ,'ZQYK  ',xdata , ierror, zbuffs)
   else if (parcod .ge. 47 .and. parcod .le. 55 ) then
!--------advective flux, ATR(1:lstci)
      call m3hcrs (hdefds, hdafds, nindex, maxdim, istat , ntruv ,&
      &parcod-46, lstci , parcod, xdata , ierror,&
      &zbuffs)
   else if (parcod .ge. 57 .and. parcod .le. 65 ) then
!--------dispersive flux, DTR(1:lstci)
      call m3hcrs (hdefds, hdafds, nindex, maxdim, istat , ntruv ,&
      &parcod-56, lstci , parcod, xdata , ierror,&
      &zbuffs)
   else if (parcod .ge. 67 .and. parcod .le. 75 ) then
!--------total flux, (ATR + DTR)(1:lstci)
      call m3hcrs (hdefds, hdafds, nindex, maxdim, istat , ntruv ,&
      &parcod-66, lstci , parcod, xdata , ierror,&
      &zbuffs)
   endif
!
   if (ierror .ne. 0) then
      fout  = .true.
   endif
!--------------------------------------------------------------------
!-----Close trih-<runid>.dat and trih-<runid>.def HIS-files
!--------------------------------------------------------------------
8888 continue
   call CLOSFL(fname, ierror)
   if (ierror .ne. 0) then
      fout  = .true.
   endif
!-----------------------------------------------------------------------
!-----return status to calling routine
!-----------------------------------------------------------------------
   ierror = IEOK
   if (fout) then
      ierror = IEOTHR
   endif
!
   return
!-----------------------------------------------------------------------
end

subroutine m3h2d  (hdefds, hdafds, nindex, maxdim, istat , nostat,&
&num_layers_grid  , lay   , pardef, xdata , ierror, zbuffs)
!-----------------------------------------------------------------------
!           Function: select from TRISULA NEFIS-HIS:
!                     pardef
!
!        Method used:
!
!-----------------------------------------------------------------------
!   Calling routine :              HISMAT
!-----------------------------------------------------------------------
!   Called  routines:              GETELT (nefis)
!-----------------------------------------------------------------------
!    Parameters:
!    -----------
!
!   Var.      Type Dimensions
!   -------------------------
!
! HDAFDS      I*4  999       I     Data file descriptor for the HIS-DAT
!                                  file
! HDEFDS      I*4  2997      I     Definition file description for the
! NINDEX      I*4  3         I     indices of time frame in Nefis file
! MAXDIM      I*4            I     length of data array
! ISTAT       I*4            I     Selected station number
! NOSTAT      I*4            I     Number of stations
! num_layers_grid        I*4            I     Number of layers
! LAY         I*4            I     Selected layer number
! PARDEF      CH*6           I     Selected parameter name
! XDATA       R*4   maxdim   O     array with the data
! IERROR      I*4            O     error indicator
! ZBUFFS      R*4   nostat   I/O   buffer for reading Nefis file
!-----------------------------------------------------------------------
!    Local variables:
!    ----------------
!
!   Var.      Type Dimensions
!   -------------------------
!
! BUFLEN      I*4                  Size in bytes of available buffer
! GRPDEF     CH*16                 Group name definition
! I           I*4                  Help variable, loop index
! N           I*4                  Counter for data array
! UINDEX      I*4  3               Indices of the cells to be read
! USRORD      I*4                  Readsequence for the cells
!-----------------------------------------------------------------------
!
!  DECLARATIONS
!
   include         'ods.inc'
!
   integer         hdefds( 2997),hdafds(  999)
   integer         uindex(    3),usrord,buflen
   character*16    grpdef
   integer         GETELT,GETELS
!
   integer         istat ,nostat,num_layers_grid  ,lay   ,ierror,n     ,i
   integer         maxdim
   integer         nindex (3)
   real            zbuffs (nostat,num_layers_grid)
   real            xdata  (maxdim)
   character*6     pardef
!--------------------------------------------------------------------
!
   grpdef    = 'his-series'
   buflen    =  4 * nostat * num_layers_grid
   ierror    = 0
   usrord    = 1
   n         = 0

   do 100 i=nindex(1),nindex(2),nindex(3)
!--------------------------------------------------------------------
!--------Initialize uindex
!--------------------------------------------------------------------
      uindex(1) = i
      uindex(2) = i
      uindex(3) = 1
!--------------------------------------------------------------------
!--------Read from group 3 PARDEF
!--------------------------------------------------------------------
      ierror    = GETELT(hdefds,grpdef    ,PARDEF    ,&
      &uindex,usrord ,buflen    ,ZBUFFS    )
      if (ierror .ne. 0) then
         ierror = IEOTHR
         goto 8888
      endif
!
      n        = n + 1
      xdata(n) = zbuffs(istat ,lay )
100 continue
!-----------------------------------------------------------------------
!-----return status to calling routine
!-----------------------------------------------------------------------
8888 return
end

subroutine m3h15  (hdefds, hdafds, nindex, maxdim, istat , nostat,&
&num_layers_grid  , lay   ,         xdata , ierror, zbuffs)
!-----------------------------------------------------------------------
!           Function: select from TRISULA NEFIS-HIS:
!                     15 viscosity, ZVICWW
!
!        Method used:
!
!-----------------------------------------------------------------------
!   Calling routine :              HISMAT
!-----------------------------------------------------------------------
!   Called  routines:              GETELT (nefis)
!-----------------------------------------------------------------------
!    Parameters:
!    -----------
!
!   Var.      Type Dimensions
!   -------------------------
!
! HDAFDS      I*4  999       I     Data file descriptor for the HIS-DAT
!                                  file
! HDEFDS      I*4  2997      I     Definition file description for the
! NINDEX      I*4  3         I     indices of time frame in Nefis file
! MAXDIM      I*4            I     length of data array
! ISTAT       I*4            I     Selected station number
! NOSTAT      I*4            I     Number of stations
! num_layers_grid        I*4            I     Number of layers
! LAY         I*4            I     Selected layer number
! XDATA       R*4   maxdim   O     array with the data
! IERROR      I*4            O     error indicator
! ZBUFFS      R*4   nostat   I/O   buffer for reading Nefis file
!-----------------------------------------------------------------------
!    Local variables:
!    ----------------
!
!   Var.      Type Dimensions
!   -------------------------
!
! BUFLEN      I*4                  Size in bytes of available buffer
! EPS         R*4                  small value
! GRPDEF     CH*16                 Group name definition
! I           I*4                  Help variable, loop index
! IERROR      I*4                  Help variable, error indicator
! N           I*4                  Counter for data array
! UINDEX      I*4  3               Array with indices of the cells to
!                                  be read
! USRORD      I*4                  Sequence in which the cells must be
!                                  read
!-----------------------------------------------------------------------
!
!  DECLARATIONS
!
   include       'ods.inc'
!
!-----------------------------------------------------------------------
!-----declaration NEFIS
!-----------------------------------------------------------------------
   integer       hdefds( 2997),hdafds(  999)
!
   integer       uindex(    3),usrord,buflen       ,&
   &nbytsg,elmndm,elmdms(     5)
!
   character*8   elmtyp

   character*16  grpdef,elmnam,elmqty,elmunt

   character*64  elmdes
!
   integer       GETELT,GETELS,INQELM
!-----------------------------------------------------------------------
!
   integer         istat ,nostat,num_layers_grid  ,lay   ,ierror,n     ,i
   integer         maxdim,kmaxon
   integer         nindex (3)
   real            zbuffs (nostat,num_layers_grid+1)
   real            xdata  (maxdim)
!--------------------------------------------------------------------
!
   elmnam    = 'ZVICWW'
   elmndm    = 5
   ierror    = INQELM(hdefds,elmnam ,elmtyp ,nbytsg ,elmqty ,&
   &elmunt,elmdes ,elmndm ,elmdms         )
   if (elmdms(elmndm) .eq. num_layers_grid) then
!--------version 2.03
      kmaxon    = 0
   else
!--------version 2.45 and later
      kmaxon    = 1
   endif
!
   grpdef    = 'his-series'
   buflen    =  4 * nostat * (num_layers_grid + kmaxon)
   ierror    = 0
   usrord    = 1
   n         = 0

   do 100 i=nindex(1),nindex(2),nindex(3)
!--------------------------------------------------------------------
!--------Initialize uindex
!--------------------------------------------------------------------
      uindex(1) = i
      uindex(2) = i
      uindex(3) = 1
!--------------------------------------------------------------------
!--------Read from group 3 ZVICWW
!--------------------------------------------------------------------
      ierror    = GETELT(hdefds,grpdef    ,'ZVICWW'  ,&
      &uindex,usrord ,buflen    ,ZBUFFS    )
      if (ierror .ne. 0) then
         ierror = IEOTHR
         goto 8888
      endif

      n        = n + 1
      xdata(n) = zbuffs(istat ,lay + kmaxon)
!
100 continue

!-----------------------------------------------------------------------
!-----return status to calling routine
!-----------------------------------------------------------------------
8888 return
end

subroutine m3h20  (hdefds, hdafds, nindex, maxdim, istat , nostat,&
&num_layers_grid  , lay   , lmax  , parcod, xdata , ierror,&
&zbuffs)
!-----------------------------------------------------------------------
!           Function: select from TRISULA NEFIS-HIS:
!                     20 constituent ,<parcod-19>
!
!        Method used:
!
!-----------------------------------------------------------------------
!   Calling routine :              HISMAT
!-----------------------------------------------------------------------
!   Called  routines:              GETELT (nefis)
!-----------------------------------------------------------------------
!    Parameters:
!    -----------
!
!   Var.      Type Dimensions
!   -------------------------
!
! HDAFDS      I*4  999       I     Data file descriptor for the HIS-DAT
!                                  file
! HDEFDS      I*4  2997      I     Definition file description for the
! NINDEX      I*4  3         I     indices of time frame in Nefis file
! MAXDIM      I*4            I     length of data array
! ISTAT       I*4            I     Selected station number
! NOSTAT      I*4            I     Number of stations
! num_layers_grid        I*4            I     Number of layers
! LAY         I*4            I     Selected layer number
! LMAX        I*4            I     Number of constituents
! PARCOD      I*4            I     parameter to get data of
! XDATA       R*4   maxdim   O     array with the data
! IERROR      I*4            O     error indicator
! ZBUFFS      R*4   nostat   I/O   buffer for reading Nefis file
!-----------------------------------------------------------------------
!    Local variables:
!    ----------------
!
!   Var.      Type Dimensions
!   -------------------------
!
! BUFLEN      I*4                  Size in bytes of available buffer
! GRPDEF     CH*16                 Group name definition
! I           I*4                  Help variable, loop index
! N           I*4                  Counter for data array
! UINDEX      I*4  3               Array with indices of the cells to
!                                  be read
! USRORD      I*4                  Sequence in which the cells must be
!                                  read
!-----------------------------------------------------------------------
!
!  DECLARATIONS
!
   include       'ods.inc'
!
   integer       hdefds( 2997),hdafds(  999)
   integer       uindex(    3),usrord,buflen
   character*16  grpdef
   integer       GETELT,GETELS
!
   integer         maxdim
   integer         istat ,nostat,num_layers_grid  ,lay   ,ierror,n     ,i
   integer         lmax
   integer         parcod
   integer         nindex (3)
!
   real            zbuffs (nostat,num_layers_grid,lmax)
   real            xdata  (maxdim)
!--------------------------------------------------------------------
!
   ierror    = 0
   grpdef    = 'his-series'
   buflen    =  4 * nostat * num_layers_grid * lmax
   usrord    = 1
   n         = 0

   do 100 i=nindex(1),nindex(2),nindex(3)
!--------------------------------------------------------------------
!--------Initialize uindex
!--------------------------------------------------------------------
      uindex(1) = i
      uindex(2) = i
      uindex(3) = 1
!--------------------------------------------------------------------
!--------Read from group 20 GRO
!--------------------------------------------------------------------
      ierror    = GETELT(hdefds,grpdef    ,'GRO'     ,&
      &uindex,usrord ,buflen    ,ZBUFFS    )
      if (ierror .ne. 0) then
         ierror = IEOTHR
         goto 8888
      endif

      n        = n + 1
      xdata(n) = zbuffs(istat,lay,parcod-19)
!
100 continue

!-----------------------------------------------------------------------
!-----return status to calling routine
!-----------------------------------------------------------------------
8888 return
end

subroutine m3h29  (hdefds, hdafds, nindex, maxdim, istat , nostat,&
&num_layers_grid  , lay   , ltur  , zturid, xdata , ierror,&
&zbuffs)
!-----------------------------------------------------------------------
!           Function: select from TRISULA NEFIS-HIS:
!                     20 turbulence constituent ,<zturid>
!
!        Method used:
!
!-----------------------------------------------------------------------
!   Calling routine :              HISMAT
!-----------------------------------------------------------------------
!   Called  routines:              GETELT (nefis)
!-----------------------------------------------------------------------
!    Parameters:
!    -----------
!
!   Var.      Type Dimensions
!   -------------------------
!
! HDAFDS      I*4  999       I     Data file descriptor for the HIS-DAT
!                                  file
! HDEFDS      I*4  2997      I     Definition file description for the
! NINDEX      I*4  3         I     indices of time frame in Nefis file
! MAXDIM      I*4            I     length of data array
! ISTAT       I*4            I     Selected station number
! NOSTAT      I*4            I     Number of stations
! num_layers_grid        I*4            I     Number of layers
! LAY         I*4            I     Selected layer number
! LTUR        I*4            I     Number of turbulence constituents
! ZTURID      I*4            I     parameter to get data of
! XDATA       R*4   maxdim   O     array with the data
! IERROR      I*4            O     error indicator
! ZBUFFS      R*4   nostat   I/O   buffer for reading Nefis file
!-----------------------------------------------------------------------
!    Local variables:
!    ----------------
!
!   Var.      Type Dimensions
!   -------------------------
!
! BUFLEN      I*4                  Size in bytes of available buffer
! GRPDEF     CH*16                 Group name definition
! I           I*4                  Help variable, loop index
! N           I*4                  Counter for data array
! UINDEX      I*4  3               Array with indices of the cells to
!                                  be read
! USRORD      I*4                  Sequence in which the cells must be
!                                  read
!-----------------------------------------------------------------------
!
!  DECLARATIONS
!
   include       'ods.inc'
!
   integer       hdefds( 2997),hdafds(  999)
   integer       uindex(    3),usrord,buflen
   character*16  grpdef
   integer       GETELT,GETELS
!
   integer         maxdim
   integer         istat ,nostat,num_layers_grid  ,lay   ,ierror,n     ,i
   integer         ltur
   integer         zturid
   integer         nindex (3)
!
   real            zbuffs (nostat,num_layers_grid+1,ltur)
   real            xdata  (maxdim)
!--------------------------------------------------------------------
!
   ierror    = 0
   grpdef    = 'his-series'
   buflen    =  4 * nostat * (num_layers_grid+1) * ltur
   usrord    = 1
   n         = 0

   do 100 i=nindex(1),nindex(2),nindex(3)
!--------------------------------------------------------------------
!--------Initialize uindex
!--------------------------------------------------------------------
      uindex(1) = i
      uindex(2) = i
      uindex(3) = 1
!--------------------------------------------------------------------
!--------Read from group 3 ZTUR
!--------------------------------------------------------------------
      ierror    = GETELT(hdefds,grpdef    ,'ZTUR'    ,&
      &uindex,usrord ,buflen    ,ZBUFFS    )
      if (ierror .ne. 0) then
         ierror = IEOTHR
         goto 8888
      endif

      n        = n + 1
      xdata(n) = zbuffs(istat,lay+1,zturid)
!
100 continue

!-----------------------------------------------------------------------
!-----return status to calling routine
!-----------------------------------------------------------------------
8888 return
end

subroutine m3h31  (hdefds, hdafds, nindex, maxdim, istat , nostat,&
&num_layers_grid  , lay   ,         xdata , ierror, zbuffs)
!-----------------------------------------------------------------------
!           Function: select from TRISULA NEFIS-HIS:
!                     31 density, ZRHO
!
!        Method used:
!
!-----------------------------------------------------------------------
!   Calling routine :              HISMAT
!-----------------------------------------------------------------------
!   Called  routines:              GETELT (nefis)
!-----------------------------------------------------------------------
!    Parameters:
!    -----------
!
!   Var.      Type Dimensions
!   -------------------------
!
! HDAFDS      I*4  999       I     Data file descriptor for the HIS-DAT
!                                  file
! HDEFDS      I*4  2997      I     Definition file description for the
! NINDEX      I*4  3         I     indices of time frame in Nefis file
! MAXDIM      I*4            I     length of data array
! ISTAT       I*4            I     Selected station number
! NOSTAT      I*4            I     Number of stations
! num_layers_grid        I*4            I     Number of layers
! LAY         I*4            I     Selected layer number
! XDATA       R*4   maxdim   O     array with the data
! IERROR      I*4            O     error indicator
! ZBUFFS      R*4   nostat   I/O   buffer for reading Nefis file
!                   *num_layers_grid
!-----------------------------------------------------------------------
!    Local variables:
!    ----------------
!
!   Var.      Type Dimensions
!   -------------------------
!
! BUFLEN      I*4                  Size in bytes of available buffer
! GRPDEF     CH*16                 Group name definition
! I           I*4                  Help variable, loop index
! N           I*4                  Counter for data array
! UINDEX      I*4  3               Array with indices of the cells to
!                                  be read
! USRORD      I*4                  Sequence in which the cells must be
!                                  read
!-----------------------------------------------------------------------
!
!  DECLARATIONS
!
   include       'ods.inc'
!
   integer       hdefds( 2997),hdafds(  999)
   integer       uindex(    3),usrord,buflen
   character*16  grpdef
   integer       GETELT,GETELS
!
   integer         maxdim
   integer         istat ,nostat,num_layers_grid  ,lay   ,ierror,n     ,i
   integer         nindex (3)
   real            zbuffs (nostat,num_layers_grid)
   real            xdata  (maxdim)
!--------------------------------------------------------------------
!
   ierror    = 0
   grpdef    = 'his-series'
   buflen    =  4 * nostat * num_layers_grid
   usrord    = 1
   n         = 0

   do 100 i=nindex(1),nindex(2),nindex(3)
!--------------------------------------------------------------------
!--------Initialize uindex
!--------------------------------------------------------------------
      uindex(1) = i
      uindex(2) = i
      uindex(3) = 1
!--------------------------------------------------------------------
!--------Read from group 31 ZRHO
!--------------------------------------------------------------------
      ierror    = GETELT(hdefds,grpdef    ,'ZRHO'    ,&
      &uindex,usrord ,buflen    ,ZBUFFS    )
      if (ierror .ne. 0) then
         ierror = IEOTHR
         goto 8888
      endif

      n        = n + 1
      xdata(n) = zbuffs(istat,lay)
!
100 continue

!-----------------------------------------------------------------------
!-----return status to calling routine
!-----------------------------------------------------------------------
8888 return
end

subroutine m3h32  (hdefds, hdafds, nindex, maxdim, istat , nostat,&
&num_layers_grid  , lay   ,         xdata , ierror, zbuffs)
!-----------------------------------------------------------------------
!           Function: select from TRISULA NEFIS-HIS:
!                     32 diffusivity, ZDICWW
!
!        Method used:
!
!-----------------------------------------------------------------------
!   Calling routine :              HISMAT
!-----------------------------------------------------------------------
!   Called  routines:              GETELT (nefis)
!-----------------------------------------------------------------------
!    Parameters:
!    -----------
!
!   Var.      Type Dimensions
!   -------------------------
!
! HDAFDS      I*4  999       I     Data file descriptor for the HIS-DAT
!                                  file
! HDEFDS      I*4  2997      I     Definition file description for the
! NINDEX      I*4  3         I     indices of time frame in Nefis file
! MAXDIM      I*4            I     length of data array
! ISTAT       I*4            I     Selected station number
! NOSTAT      I*4            I     Number of stations
! num_layers_grid        I*4            I     Number of layers
! LAY         I*4            I     Selected layer number
! XDATA       R*4   maxdim   O     array with the data
! IERROR      I*4            O     error indicator
! ZBUFFS      R*4   nostat   I/O   buffer for reading Nefis file
!-----------------------------------------------------------------------
!    Local variables:
!    ----------------
!
!   Var.      Type Dimensions
!   -------------------------
!
! BUFLEN      I*4                  Size in bytes of available buffer
! EPS         R*4                  small value
! GRPDEF     CH*16                 Group name definition
! I           I*4                  Help variable, loop index
! IERROR      I*4                  Help variable, error indicator
! N           I*4                  Counter for data array
! UINDEX      I*4  3               Array with indices of the cells to
!                                  be read
! USRORD      I*4                  Sequence in which the cells must be
!                                  read
!-----------------------------------------------------------------------
!
!  DECLARATIONS
!
   include       'ods.inc'
!
!-----------------------------------------------------------------------
!-----declaration NEFIS
!-----------------------------------------------------------------------
   integer       hdefds( 2997),hdafds(  999)
!
   integer       uindex(    3),usrord,buflen       ,&
   &nbytsg,elmndm,elmdms(     5)
!
   character*8   elmtyp

   character*16  grpdef,elmnam,elmqty,elmunt

   character*64  elmdes
!
   integer       GETELT,GETELS,INQELM
!-----------------------------------------------------------------------
!
   integer         istat ,nostat,num_layers_grid  ,lay   ,ierror,n     ,i
   integer         maxdim,kmaxon
   integer         nindex (3)
   real            zbuffs (nostat,num_layers_grid+1)
   real            xdata  (maxdim)
!--------------------------------------------------------------------
!
   elmnam    = 'ZDICWW'
   elmndm    = 5
   ierror    = INQELM(hdefds,elmnam ,elmtyp ,nbytsg ,elmqty ,&
   &elmunt,elmdes ,elmndm ,elmdms         )
   if (ierror .ne. 0) then
!--------version 2.45 and later
      kmaxon    = 1
   else if (elmdms(elmndm) .eq. num_layers_grid) then
!--------version 2.03
      kmaxon    = 0
   else
!--------version 2.45 and later
      kmaxon    = 1
   endif
!
   grpdef    = 'his-series'
   buflen    =  4 * nostat * (num_layers_grid + kmaxon)
   ierror    = 0
   usrord    = 1
   n         = 0

   do 100 i=nindex(1),nindex(2),nindex(3)
!--------------------------------------------------------------------
!--------Initialize uindex
!--------------------------------------------------------------------
      uindex(1) = i
      uindex(2) = i
      uindex(3) = 1
!--------------------------------------------------------------------
!--------Read from group 3 ZDICWW
!--------------------------------------------------------------------
      ierror    = GETELT(hdefds,grpdef    ,'ZDICWW'  ,&
      &uindex,usrord ,buflen    ,ZBUFFS    )
      if (ierror .ne. 0) then
         ierror = IEOTHR
         goto 8888
      endif

      n        = n + 1
      xdata(n) = zbuffs(istat ,lay + kmaxon)
!
100 continue

!-----------------------------------------------------------------------
!-----return status to calling routine
!-----------------------------------------------------------------------
8888 return
end

subroutine m3hfuv (hdefds, hdafds, nindex, maxdim, istat , nostat,&
&num_layers_grid  , lay   , pardef, xdata , ierror, zbuffs)
!-----------------------------------------------------------------------
!           Function: select from TRISULA NEFIS-HIS:
!                     PARDEF
!
!        Method used:
!
!-----------------------------------------------------------------------
!   Calling routine :              HISMAT
!-----------------------------------------------------------------------
!   Called  routines:              GETELT (nefis)
!-----------------------------------------------------------------------
!    Parameters:
!    -----------
!
!   Var.      Type Dimensions
!   -------------------------
!
! HDAFDS      I*4  999       I     Data file descriptor for the HIS-DAT
!                                  file
! HDEFDS      I*4  2997      I     Definition file description for the
! NINDEX      I*4  3         I     indices of time frame in Nefis file
! MAXDIM      I*4            I     length of data array
! ISTAT       I*4            I     Selected station number
! NOSTAT      I*4            I     Number of stations
! num_layers_grid        I*4            I     Number of layers
! LAY         I*4            I     Selected layer number
! PARDEF      CH*6           I     Selected element name
! XDATA       R*4   maxdim   O     array with the data
! IERROR      I*4            O     error indicator
! ZBUFFS      R*4   nostat   I/O   buffer for reading Nefis file
!-----------------------------------------------------------------------
!    Local variables:
!    ----------------
!
!   Var.      Type Dimensions
!   -------------------------
!
! BUFLEN      I*4                  Size in bytes of available buffer
! CELNAM     CH*16                 Celname definition
! DT          R*4                  Time step interval
! DTHIS       R*4                  Time step interval for history file
! OKEE        L*4                  Flag for NEFIS file readings
! GRPDEF     CH*16                 Group name definition
! I           I*4                  Help variable, loop index
! IT1         I*4                  Help variable, first history step
! IT2         I*4                  Help variable, second history step
! N           I*4                  Counter for data array
! UINDEX      I*4  3               Array with indices of the cells to
!                                  be read
! USRORD      I*4                  Sequence in which the cells must be
!                                  read
! ZVXK        R*4                  help variable; accumulated flow u
!-----------------------------------------------------------------------
!
!  DECLARATIONS
!
   include         'ods.inc'
!
   integer         grpndm,grpdms(    5),grpord(    5)
   integer         hdefds( 2997),hdafds(  999)
   integer         uindex(    3),usrord,buflen
   character*16    grpdef,celnam
   character*6     pardef
   integer         GETELT,GETELS,INQGRP,INQMXI
!
   integer         istat ,nostat,ierror,n     ,i     ,num_layers_grid  ,lay
   integer         nrcel ,it1   ,it2
   integer         maxdim
   integer         nindex (3)
!
   real            zv
   real            dt    ,dthis ,tunit
!
   real            xdata  (maxdim)
   real            zbuffs (nostat,num_layers_grid)
!
   logical         okee

!
!-----Read array-dimension nrcel from Nefis HIS-files group 1
   grpdef = 'his-info-series'
   grpndm = 5
   celnam = grpdef
   okee   =&
   &INQGRP(hdefds   ,grpdef    ,celnam    ,grpndm    ,&
   &grpdms   ,grpord                          )&
   &.eq. 0
   nrcel  = grpdms(1)
!
!-----Test value of nrcel if nrcel = 0 then get nrcel with INQMXI
   if (nrcel  .eq. 0) then
      okee      = okee .and.&
      &INQMXI(hdefds,grpdef    ,nrcel     )&
      &.eq. 0
   endif

   buflen    = 4
   uindex(1) = 1
   uindex(2) = 1
   uindex(3) = 1
   usrord    = 1
   it1       = 0
   it2       = 0
!
!-----Get values for IT1 and IT2 from group 1
   okee      = okee .and.&
   &GETELT(hdefds,grpdef    ,'ITHISC'  ,&
   &uindex,usrord    ,buflen    ,IT1       )&
   &.eq. 0
   if (nrcel  .gt. 1) then
      uindex(1) = 2
      uindex(2) = 2
      okee      = okee .and.&
      &GETELT(hdefds,grpdef    ,'ITHISC'  ,&
      &uindex,usrord    ,buflen    ,IT2       )&
      &.eq. 0
   else
      it2 = it1 + 1
   endif

   grpdef    = 'his-const'
   uindex(1) = 1
   uindex(2) = 1
   uindex(3) = 1
   buflen    = 4
   okee      =&
   &GETELT(hdefds,grpdef    ,'TUNIT'   ,&
   &uindex,usrord    ,buflen    ,TUNIT     )&
   &.eq. 0

   okee      = okee .and.&
   &GETELT(hdefds,grpdef    ,'DT'      ,&
   &uindex,usrord    ,buflen    ,DT        )&
   &.eq. 0
!
!-----set history time interval
   dthis     = real( it2 - it1 ) * tunit * dt
!
   grpdef    = 'his-series'
   buflen    =  4 * nostat * num_layers_grid
   n         = 0
   zv        = 0.0

   do 100 i=nindex(1),nindex(2),nindex(3)
!--------------------------------------------------------------------
!--------Initialize uindex
!--------------------------------------------------------------------
      uindex(1) = i
      uindex(2) = i
      uindex(3) = 1
!--------------------------------------------------------------------
!--------Read from group 3 PARDEF
!--------------------------------------------------------------------
      okee      = okee .and.&
      &GETELT(hdefds,grpdef    ,PARDEF    ,&
      &uindex,usrord    ,buflen    ,zbuffs    )&
      &.eq. 0

      n         = n + 1
      zv        = zv + dthis * zbuffs(istat ,lay )
      xdata(n)  = zv
100 continue

!-----------------------------------------------------------------------
!-----return status to calling routine
!-----------------------------------------------------------------------
   ierror = IEOK
   if (.not. okee) then
      ierror = IEOTHR
   endif
!
   return
end

subroutine m3hcda (hdefds, hdafds, nindex, maxdim, istat , nostat,&
&num_layers_grid  ,         parcod, xdata , ierror, zbuffs)
!-----------------------------------------------------------------------
!           Function: select from TRISULA NEFIS-HIS:
!                      3 dpt. aver. cur. u
!                      4 dpt. aver. cur. v
!                      5 dpt. aver. cur. mag.
!                      6 dpt. aver. cur. dir.
!
!        Method used:
!
!-----------------------------------------------------------------------
!   Calling routine :              HISMAT
!-----------------------------------------------------------------------
!   Called  routines:              GETELT (nefis)
!-----------------------------------------------------------------------
!    Parameters:
!    -----------
!
!   Var.      Type Dimensions
!   -------------------------
!
! HDAFDS      I*4  999       I     Data file descriptor for the HIS-DAT
!                                  file
! HDEFDS      I*4  2997      I     Definition file description for the
! NINDEX      I*4  3         I     indices of time frame in Nefis file
! MAXDIM      I*4            I     length of data array
! ISTAT       I*4            I     Selected station number
! NOSTAT      I*4            I     Number of stations
! num_layers_grid        I*4            I     Number of layers
! PARCOD      I*4            I     parameter to get data of
! XDATA       R*4   maxdim   O     array with the data
! IERROR      I*4            O     error indicator
! ZBUFFS      R*4   nostat   I/O   buffer for reading Nefis file
!-----------------------------------------------------------------------
!    Local variables:
!    ----------------
!
!   Var.      Type Dimensions
!   -------------------------
!
! BUFLEN      I*4                  Size in bytes of available buffer
! DPS         R*4                  Depth at station location
! ELM        CH*16                 Element name definition
! ELMNAM     CH*16 3               Element name definition
! EPS         R*4                  small value
! OKEE        L*4                  Flag for NEFIS file readings
! GRDANG      R*4                  Vertex between y-axis and true North
! GRPDEF     CH*16                 Group name definition
! GUUGEM      R*4                  Mean value for distance coefficients
! GVVGEM      R*4                  Mean value for distance coefficients
! HULP        R*4                  Help variable
! I           I*4                  Help variable, loop index
! N           I*4                  Counter for data array
! PI          R*4                  Constant pi
! U           R*4                  bottom stress u-direction
! UINDEX      I*4  3               Array with indices of the cells to
!                                  be read
! USRORD      I*4                  Sequence in which the cells must be
!                                  read
! V           R*4                  bottom stress v-direction
! XETA        R*4                  X-distance between 2 grid lines
!                                  around zeta point in y-direction
! XKSI        R*4                  X-distance between 2 grid lines
!                                  around zeta point in x-direction
! YETA        R*4                  Y-distance between 2 grid lines
!                                  around zeta point in y-direction
! YKSI        R*4                  Y-distance between 2 grid lines
!                                  around zeta point in x-direction
! ZTAUD       R*4                  bottom stress dir.
! ZTAUET      R*4                  bottom stress y-direction
! ZTAVKS      R*4                  bottom stress x-direction
! ZTAUM       R*4                  bottom stress mag.
! ZTAUT       R*4                  bottom stress u-direction at station
! ZTAVT       R*4                  bottom stress v-direction at station
! ZWH         R*4                  total water depth
! ZWL         R*4                  water level
!-----------------------------------------------------------------------
!
!  DECLARATIONS
!
   include         'ods.inc'
!
   integer         hdefds( 2997),hdafds(  999)
   integer         uindex(    3),usrord,buflen
   character*16    grpdef
   integer         GETELT,GETELS
!
   integer         istat ,nostat,num_layers_grid  ,ierror,n     ,i     ,k
   integer         maxdim
   integer         parcod
   integer         nindex (3)
!
   real            pi    ,eps   ,zcuru ,zcurv ,zcurdu,zcurdv
   real            zcurdm,hulp  ,zcurdd
!
   real            zbuffs (nostat,num_layers_grid)
   real            xdata  (maxdim)
!
   logical         okee
!
   real            grdang
   real            alfas
!-----see TRISULA user Manual, release 2.03, version 0.1 May 1993
!     Appendix A: limitations 1<= zmax <= 42
   real            thick (42)
!

   pi        = atan (1.0) * 4.
   eps       = 1.e-12
   grpdef    = 'his-const'
   uindex(1) = 1
   uindex(2) = 1
   uindex(3) = 1
   usrord    = 1

   buflen    = 4 * num_layers_grid
   okee      =&
   &GETELT(hdefds,grpdef    ,'THICK'   ,&
   &uindex,usrord    ,buflen    ,THICK     )&
   &.eq. 0

   buflen    = 4
   okee      = okee .and.&
   &GETELT(hdefds,grpdef    ,'GRDANG'  ,&
   &uindex,usrord    ,buflen    ,GRDANG    )&
   &.eq. 0
!
   buflen    =  4 * nostat
!     get element alfas (or compute it from xksi and yksi)
!
   call gtalfs( hdefds, hdafds, istat, nostat, alfas,&
   &zbuffs, ierror )
   okee      = okee .and. ierror .eq. 0
!
   grpdef    = 'his-series'
   n         = 0
   buflen    =  4 * nostat * num_layers_grid

   do 100 i=nindex(1),nindex(2),nindex(3)
!--------------------------------------------------------------------
!--------Initialize uindex
!--------------------------------------------------------------------
      uindex(1) = i
      uindex(2) = i
      uindex(3) = 1
!--------------------------------------------------------------------
!--------Read from group 3 ZCURU
!--------------------------------------------------------------------
      okee      = okee .and.&
      &GETELT(hdefds,grpdef    ,'ZCURU'   ,&
      &uindex,usrord    ,buflen    ,zbuffs    )&
      &.eq. 0
      zcuru     = 0.0
      do 110 k=1,num_layers_grid
         zcuru  = zcuru + zbuffs(istat ,k) * thick(k)
110   continue

!--------------------------------------------------------------------
!--------Read from group 3 ZCURV
!--------------------------------------------------------------------
      okee      = okee .and.&
      &GETELT(hdefds,grpdef    ,'ZCURV'   ,&
      &uindex,usrord    ,buflen    ,zbuffs    )&
      &.eq. 0
      zcurv     = 0
      do 120 k=1,num_layers_grid
         zcurv  = zcurv + zbuffs(istat ,k) * thick(k)
120   continue
!--------------------------------------------------------------------
!--------Backwards transformation of ZCURU and ZCURV
!--------------------------------------------------------------------
!c       if (guugem .lt. eps .or.
!c   *       gvvgem .lt. eps) then
!c          zcurdu = 0.0
!c          zcurdv = 0.0
!c       else
!c          u      =  zcuru  / gvvgem
!c          v      =  zcurv  / guugem
!c          zcurdu =  xksi * u  + xeta * v
!c          zcurdv =  yksi * u  + yeta * v
!c       endif
      zcurdu = zcuru * cos( alfas ) - zcurv * sin( alfas )
      zcurdv = zcuru * sin( alfas ) + zcurv * cos( alfas )
!--------------------------------------------------------------------
!--------Calculate ZCURDM and ZCURDD
!        zcurdd should be defined between 0. and 360. degrees
!        atan2 <-180,180>, grdang [0,360] => mod (.. + 720)
!
!        Note (AM, dd 31 march 1999)
!        The sign before grdang was -, this has been adjusted, though
!        in most cases grdang is zero anyway.
!--------------------------------------------------------------------
      zcurdm  = sqrt (zcurdu * zcurdu + zcurdv * zcurdv)
      if (abs (zcurdu) .lt. eps) then
         zcurdu = eps
      endif
      if (abs (zcurdv) .lt. eps) then
         zcurdv = eps
      endif
      hulp   = 90. - atan2 (zcurdv,zcurdu) * 180 / pi + grdang
      zcurdd = amod (hulp  + 720., 360.)

      n      = n + 1
      if (parcod .eq. 3) then
         xdata(n) = zcurdu
      else if (parcod .eq.  4) then
         xdata(n) = zcurdv
      else if (parcod .eq.  5) then
         xdata(n) = zcurdm
      else if (parcod .eq.  6) then
         xdata(n) = zcurdd
      endif
!
100 continue

!-----------------------------------------------------------------------
!-----return status to calling routine
!-----------------------------------------------------------------------
   ierror = IEOK
   if (.not. okee) then
      ierror = IEOTHR
   endif
!
   return
end

subroutine m3hcrs (hdefds, hdafds, nindex, maxdim, istat , ntruv ,&
&lconst, lstci , parcod, xdata , ierror,&
&zbuffs)
!-----------------------------------------------------------------------
!           Function: select from TRISULA NEFIS-HIS:
!                     33 Momentary flow, FLTR
!                     34 Accumulated flow, CTR
!                47 - 55 advective flux, ATR
!                57 - 65 dispersive flux, DTR
!                67 - 75 total flux, ATR + DTR
!
!        Method used:
!
!-----------------------------------------------------------------------
!   Calling routine :              HISMAT
!-----------------------------------------------------------------------
!   Called  routines:              GETELT (nefis)
!-----------------------------------------------------------------------
!    Parameters:
!    -----------
!
!   Var.      Type Dimensions
!   -------------------------
!
! HDAFDS      I*4  999       I     Data file descriptor for the HIS-DAT
!                                  file
! HDEFDS      I*4  2997      I     Definition file description for the
! NINDEX      I*4  3         I     indices of time frame in Nefis file
! MAXDIM      I*4            I     length of data array
! ISTAT       I*4            I     Selected station number
! NTRUV       I*4            I     Number of cross-sections
! LCONST      I*4            I     Number of selected constituent
! LSTCI       I*4            I     Number of constituents
! PARCOD      I*4            I     parameter to get data of
! XDATA       R*4   maxdim   O     array with the data
! IERROR      I*4            O     error indicator
! ZBUFFS      R*4   nostat   I/O   buffer for reading Nefis file
!-----------------------------------------------------------------------
!    Local variables:
!    ----------------
!
!   Var.      Type Dimensions
!   -------------------------
!
! BUFLEN      I*4                  Size in bytes of available buffer
! GRPDEF     CH*16                 Group name definition
! I           I*4                  Help variable, loop index
! N           I*4                  Counter for data array
! UINDEX      I*4  3               Array with indices of the cells to
!                                  be read
! USRORD      I*4                  Sequence in which the cells must be
!                                  read
!-----------------------------------------------------------------------
!
!  DECLARATIONS
!
   include       'ods.inc'
!
   integer       hdefds( 2997),hdafds(  999)
   integer       uindex(    3),usrord,buflen
   character*16  grpdef
   integer       GETELT,GETELS
!
   integer         maxdim
   integer         istat ,ntruv ,ierror,n     ,i
   integer         lstci ,lconst
   integer         parcod
   integer         nindex (3)
!
   real            atr   ,dtr   ,fltr  ,ctr
!
   real            zbuffs (ntruv ,*   )
   real            xdata  (maxdim)
!
   logical         okee
!--------------------------------------------------------------------
!
   okee      = .TRUE.
   grpdef    = 'his-series'
   usrord    = 1
   n         = 0

   do 100 i=nindex(1),nindex(2),nindex(3)
!--------------------------------------------------------------------
!--------Initialize uindex
!--------------------------------------------------------------------
      uindex(1) = i
      uindex(2) = i
      uindex(3) = 1

      buflen    =  4 * ntruv

!--------Read from group 3 FLTR
      okee      = okee .and.&
      &GETELT(hdefds,grpdef    ,'FLTR'    ,&
      &uindex,usrord ,buflen    ,ZBUFFS    )&
      &.eq. 0
      fltr     = zbuffs(istat,1     )

!--------Read from group 3 CTR
      okee      = okee .and.&
      &GETELT(hdefds,grpdef    ,'CTR'     ,&
      &uindex,usrord ,buflen    ,ZBUFFS    )&
      &.eq. 0
      ctr      = zbuffs(istat,1     )

      if ( lstci .gt. 0 ) then
         buflen    =  4 * ntruv * lstci
!-----------Read from group 3 ATR
         okee      = okee .and.&
         &GETELT(hdefds,grpdef    ,'ATR'     ,&
         &uindex,usrord ,buflen    ,ZBUFFS    )&
         &.eq. 0
         atr      = zbuffs(istat,lconst)

!-----------Read from group 3 DTR
         okee      = okee .and.&
         &GETELT(hdefds,grpdef    ,'DTR'     ,&
         &uindex,usrord ,buflen    ,ZBUFFS    )&
         &.eq. 0
         dtr      = zbuffs(istat,lconst)
      endif

      n        = n + 1
      if ( parcod .eq. 33 ) then
         xdata(n) = fltr
      else if ( parcod .eq. 34 ) then
         xdata(n) = ctr
      else if ( parcod .ge. 47 .and. parcod .le. 55 ) then
         xdata(n) = atr
      else if ( parcod .ge. 57 .and. parcod .le. 65 ) then
         xdata(n) = dtr
      else if ( parcod .ge. 67 .and. parcod .le. 75 ) then
         xdata(n) = atr + dtr
      endif
!
100 continue

!-----------------------------------------------------------------------
!-----return status to calling routine
!-----------------------------------------------------------------------
   ierror    = IEOK
   if ( .not. okee ) then
      ierror    = IEOTHR
   endif

   return
end

subroutine m3hcur (hdefds, hdafds, nindex, maxdim, istat , nostat,&
&num_layers_grid  , lay   , parcod, xdata , ierror, zbuffs)
!-----------------------------------------------------------------------
!           Function: select from TRISULA NEFIS-HIS:
!                      7 current u            zeta_position
!                      8 current v            zeta_position
!                      9 current mag. (horiz) zeta_position
!                     10 current dir. (horiz) zeta_position
!
!        Method used:
!
!-----------------------------------------------------------------------
!   Calling routine :              HISMAT
!-----------------------------------------------------------------------
!   Called  routines:              GETELT (nefis)
!-----------------------------------------------------------------------
!    Parameters:
!    -----------
!
!   Var.      Type Dimensions
!   -------------------------
!
! HDAFDS      I*4  999       I     Data file descriptor for the HIS-DAT
!                                  file
! HDEFDS      I*4  2997      I     Definition file description for the
! NINDEX      I*4  3         I     indices of time frame in Nefis file
! MAXDIM      I*4            I     length of data array
! ISTAT       I*4            I     Selected station number
! NOSTAT      I*4            I     Number of stations
! num_layers_grid        I*4            I     Number of layers
! LAY         I*4            I     Selected layer number
! PARCOD      I*4            I     parameter to get data of
! XDATA       R*4   maxdim   O     array with the data
! IERROR      I*4            O     error indicator
! ZBUFFS      R*4   nostat   I/O   buffer for reading Nefis file
!-----------------------------------------------------------------------
!    Local variables:
!    ----------------
!
!   Var.      Type Dimensions
!   -------------------------
!
! BUFLEN      I*4                  Size in bytes of available buffer
! ELM        CH*16                 Element name definition
! ELMNAM     CH*16 3               Element name definition
! EPS         R*4                  small value
! OKEE        L*4                  Flag for NEFIS file readings
! GRDANG      R*4                  Vertex between y-axis and true North
! GRPDEF     CH*16                 Group name definition
! GUUGEM      R*4                  Mean value for distance coefficients
! GVVGEM      R*4                  Mean value for distance coefficients
! HULP        R*4                  Help variable
! I           I*4                  Help variable, loop index
! N           I*4                  Counter for data array
! PI          R*4                  Constant pi
! U           R*4                  velocity u-direction
! UINDEX      I*4  3               Array with indices of the cells to
!                                  be read
! USRORD      I*4                  Sequence in which the cells must be
!                                  read
! V           R*4                  velocity v-direction
! XETA        R*4                  X-distance between 2 grid lines
!                                  around zeta point in y-direction
! XKSI        R*4                  X-distance between 2 grid lines
!                                  around zeta point in x-direction
! YETA        R*4                  Y-distance between 2 grid lines
!                                  around zeta point in y-direction
! YKSI        R*4                  Y-distance between 2 grid lines
!                                  around zeta point in x-direction
! ZCURD       R*4                  velocity direction in selected station
! ZCURM       R*4                  velocity magnitude in selected station
! ZCURU       R*4                  u-velocity component at u-point
! ZCURV       R*4                  v-velocity component at v-point
! ZCURUT      R*4                  u-velocity component at station
! ZCURVT      R*4                  v-velocity component at station
! ZWH         R*4                  total water depth
! ZWL         R*4                  water level
!-----------------------------------------------------------------------
!
!  DECLARATIONS
!
   include       'ods.inc'
!
   integer       hdefds( 2997),hdafds(  999)
   integer       uindex(    3),usrord,buflen
   character*16  grpdef
   integer       GETELT,GETELS
!
   integer         istat ,nostat,num_layers_grid  ,ierror,n     ,i     ,lay
   integer         maxdim
   integer         parcod
   integer         nindex (3)
!
   real            pi    ,eps   ,zcuru ,zcurv ,zcurvt,zcurut
   real            zcurm ,hulp  ,zcurd
!
   real            zbuffs (nostat,num_layers_grid)
   real            xdata  (maxdim)
!
   logical         okee
!
   real            grdang
   real            alfas
!
   pi        = atan (1.0) * 4.
   eps       = 1.e-12
   grpdef    = 'his-const'
   uindex(1) = 1
   uindex(2) = 1
   uindex(3) = 1
   usrord    = 1

   buflen    = 4
   okee      =&
   &GETELT(hdefds,grpdef    ,'GRDANG'  ,&
   &uindex,usrord    ,buflen    ,GRDANG    )&
   &.eq. 0
!
   buflen    =  4 * nostat
!
!     get element alfas (or compute it from xksi and yksi)
!
   call gtalfs( hdefds, hdafds, istat, nostat, alfas,&
   &zbuffs, ierror )
   okee      = okee .and. ierror .eq. 0
!
   grpdef    = 'his-series'
   n         = 0
   buflen    =  4 * nostat * num_layers_grid

   do 100 i=nindex(1),nindex(2),nindex(3)
!--------------------------------------------------------------------
!--------Initialize uindex
!--------------------------------------------------------------------
      uindex(1) = i
      uindex(2) = i
      uindex(3) = 1
!--------------------------------------------------------------------
!--------Read from group 3 ZCURU
!--------------------------------------------------------------------
      okee   = okee .and.&
      &GETELT(hdefds,grpdef    ,'ZCURU'   ,&
      &uindex,usrord ,buflen    ,ZBUFFS    )&
      &.eq. 0
      zcuru  = zbuffs(istat ,lay   )
!--------------------------------------------------------------------
!--------Read from group 3 ZCURV
!--------------------------------------------------------------------
      okee   = okee .and.&
      &GETELT(hdefds,grpdef    ,'ZCURV'   ,&
      &uindex,usrord ,buflen    ,ZBUFFS    )&
      &.eq. 0
      zcurv  = zbuffs(istat ,lay   )
!--------------------------------------------------------------------
!--------Backwards transformation of ZCURU and ZCURV
!--------------------------------------------------------------------
!c       if (guugem .lt. eps .or.
!c   *       gvvgem .lt. eps) then
!c          zcurut = 0.0
!c          zcurvt = 0.0
!c       else
!c          u      =  zcuru  / gvvgem
!c          v      =  zcurv  / guugem
!c          zcurut =  xksi * u  + xeta * v
!c          zcurvt =  yksi * u  + yeta * v
!c       endif
      zcurut = zcuru * cos( alfas ) - zcurv * sin( alfas )
      zcurvt = zcuru * sin( alfas ) + zcurv * cos( alfas )
!--------------------------------------------------------------------
!--------Calculate ZCURM and ZCURD
!        zcurd should be defined between 0. and 360. degrees
!        atan2 <-180,180>, grdang [0,360] => mod (.. + 720)
!
!        Note (AM, dd 31 march 1999)
!        The sign before grdang was -, this has been adjusted, though
!        in most cases grdang is zero anyway.
!--------------------------------------------------------------------
      zcurm  = sqrt (zcurut * zcurut + zcurvt * zcurvt)
      if (abs (zcurut) .lt. eps) then
         zcurut = eps
      endif
      if (abs (zcurvt) .lt. eps) then
         zcurvt = eps
      endif
      hulp   = 90. - atan2 (zcurvt,zcurut) * 180 / pi + grdang
      zcurd  = amod (hulp  + 720., 360.)

      n      = n + 1
      if (parcod .eq. 7) then
         xdata(n) = zcurut
      else if (parcod .eq.  8) then
         xdata(n) = zcurvt
      else if (parcod .eq.  9) then
         xdata(n) = zcurm
      else if (parcod .eq. 10) then
         xdata(n) = zcurd
      endif
!
100 continue

!-----------------------------------------------------------------------
!-----return status to calling routine
!-----------------------------------------------------------------------
   ierror = IEOK
   if (.not. okee) then
      ierror = IEOTHR
   endif
!
   return
end

subroutine m3hwat (hdefds, hdafds, nindex, maxdim, istat , nostat,&
&parcod, xdata , ierror, zbuffs)
!-----------------------------------------------------------------------
!           Function: select from TRISULA NEFIS-HIS:
!                      1 water level
!                      2 total water depth
!                     16 bottom stress u    zeta_position
!                     17 bottom stress v    zeta_position
!                     18 bottom stress mag. zeta_position
!                     19 bottom stress dir. zeta_position
!                     40 bottom stress u    u_position
!                     41 bottom stress v    v_position
!
!        Method used:
!
!-----------------------------------------------------------------------
!   Calling routine :              HISMAT
!-----------------------------------------------------------------------
!   Called  routines:              GETELT (nefis)
!-----------------------------------------------------------------------
!    Parameters:
!    -----------
!
!   Var.      Type Dimensions
!   -------------------------
!
! HDAFDS      I*4  999       I     Data file descriptor for the HIS-DAT
!                                  file
! HDEFDS      I*4  2997      I     Definition file description for the
! NINDEX      I*4  3         I     indices of time frame in Nefis file
! MAXDIM      I*4            I     length of data array
! ISTAT       I*4            I     Selected station number
! NOSTAT      I*4            I     Number of stations
! PARCOD      I*4            I     parameter to get data of
! XDATA       R*4   maxdim   O     array with the data
! IERROR      I*4            O     error indicator
! ZBUFFS      R*4   nostat   I/O   buffer for reading Nefis file
!-----------------------------------------------------------------------
!    Local variables:
!    ----------------
!
!   Var.      Type Dimensions
!   -------------------------
!
! BUFLEN      I*4                  Size in bytes of available buffer
! DPS         R*4                  Depth at station location
! EPS         R*4                  small value
! OKEE        L*4                  Flag for NEFIS file readings
! GRDANG      R*4                  Vertex between y-axis and true North
! GRPNAM     CH*16                 Group name definition
! GUUGEM      R*4                  Mean value for distance coefficients
! GVVGEM      R*4                  Mean value for distance coefficients
! HULP        R*4                  Help variable
! I           I*4                  Help variable, loop index
! IERROR      I*4                  Help variable, error indicator
! N           I*4                  Counter for data array
! PI          R*4                  Constant pi
! U           R*4                  bottom stress u-direction
! UINDEX      I*4  3               Array with indices of the cells to
!                                  be read
! USRORD      I*4                  Sequence in which the cells must be
!                                  read
! V           R*4                  bottom stress v-direction
! XETA        R*4                  X-distance between 2 grid lines
!                                  around zeta point in y-direction
! XKSI        R*4                  X-distance between 2 grid lines
!                                  around zeta point in x-direction
! YETA        R*4                  Y-distance between 2 grid lines
!                                  around zeta point in y-direction
! YKSI        R*4                  Y-distance between 2 grid lines
!                                  around zeta point in x-direction
! ZTAUD       R*4                  bottom stress dir.
! ZTAUET      R*4                  bottom stress y-direction
! ZTAVKS      R*4                  bottom stress x-direction
! ZTAUM       R*4                  bottom stress mag.
! ZTAUT       R*4                  bottom stress u-direction at station
! ZTAVT       R*4                  bottom stress v-direction at station
! ZWH         R*4                  total water depth
! ZWL         R*4                  water level
!-----------------------------------------------------------------------
!
!  DECLARATIONS
!
   include       'ods.inc'
!
   integer       hdefds( 2997),hdafds(  999)
   integer       uindex(    3),usrord,buflen
   character*16  grpnam
   integer       GETELT,GETELS
!
   integer         maxdim
   integer         parcod
   integer         nindex (3)
   integer         istat ,nostat,ierror,n     ,i
!
   real            ztauut,ztauvt,ztaum ,hulp  ,ztaud ,pi    ,eps
   real            zwl   ,zwh   ,ztauet,ztauks
!
   real            zbuffs (nostat)
   real            xdata  (maxdim)
!
   logical         okee
!
   real            grdang
   real            dps
   real            alfas
!
   pi        = atan (1.0) * 4.
   eps       = 1.e-12
   grpnam    = 'his-const'
   uindex(1) = 1
   uindex(2) = 1
   uindex(3) = 1
   usrord    = 1
!
   buflen    = 4
   okee      =&
   &GETELT(hdefds,grpnam    ,'GRDANG'  ,&
   &uindex,usrord    ,buflen    ,GRDANG    )&
   &.eq. 0
!
   buflen    =  4 * nostat

!     get element alfas (or compute it from xksi and yksi)
!
   call gtalfs( hdefds, hdafds, istat, nostat, alfas,&
   &zbuffs, ierror )
   okee      = okee .and. ierror .eq. 0
!
   okee      = okee .and.&
   &GETELT(hdefds,grpnam    ,'DPS'     ,&
   &uindex,usrord    ,buflen    ,zbuffs    )&
   &.eq. 0
   dps       = zbuffs(istat )

!
   grpnam    = 'his-series'
   n         = 0
   buflen    =  4 * nostat

   do 100 i=nindex(1),nindex(2),nindex(3)
!--------------------------------------------------------------------
!--------Initialize uindex
!--------------------------------------------------------------------
      uindex(1) = i
      uindex(2) = i
      uindex(3) = 1
!--------------------------------------------------------------------
!--------Read from group 3 ZWL
!--------Calculate ZWH
!--------------------------------------------------------------------
      if ( parcod .eq. 1 .or. parcod .eq. 2 ) then
         okee      = okee .and.&
         &GETELT(hdefds,grpnam    ,'ZWL'     ,&
         &uindex,usrord ,buflen    ,ZBUFFS    )&
         &.eq. 0
         zwl    = zbuffs(istat)
         zwh       = zwl + dps
      else
!--------------------------------------------------------------------
!--------Read from group 3 ZTAUKS
!--------------------------------------------------------------------
         okee      = okee .and.&
         &GETELT(hdefds,grpnam    ,'ZTAUKS'  ,&
         &uindex,usrord ,buflen    ,ZBUFFS    )&
         &.eq. 0
         ztauks    = zbuffs(istat)
!--------------------------------------------------------------------
!--------Read from group 3 ZTAUET
!--------------------------------------------------------------------
         okee      = okee .and.&
         &GETELT(hdefds,grpnam    ,'ZTAUET'  ,&
         &uindex,usrord ,buflen    ,ZBUFFS    )&
         &.eq. 0
         ztauet    = zbuffs(istat)
!--------------------------------------------------------------------
!--------Backwards transformation of ZTAUKS and ZTAUET
!--------------------------------------------------------------------
!c       if (guugem .lt. eps .or.
!c   *       gvvgem .lt. eps) then
!c          ztauut = 0.0
!c          ztauvt = 0.0
!c       else
!c          u      =  ztauks / gvvgem
!c          v      =  ztauet / guugem
!c          ztauut =  xksi * u  + xeta * v
!c          ztauvt =  yksi * u  + yeta * v
!c       endif
         ztauut = ztauks * cos( alfas ) - ztauet * sin( alfas )
         ztauvt = ztauks * sin( alfas ) + ztauet * cos( alfas )
!--------------------------------------------------------------------
!--------Calculate ZTAUM and ZTAUD
!        ztaud should be defined between 0. en  360. degrees
!        atan2 <-180,180>, grdang [0,360] => mod (.. + 720)
!
!        Note (AM, dd 31 march 1999)
!        The sign before grdang was -, this has been adjusted, though
!        in most cases grdang is zero anyway.
!--------------------------------------------------------------------
         ztaum  = sqrt (ztauut * ztauut + ztauvt * ztauvt)
         if (abs (ztauut) .lt. eps) then
            ztauut = eps
         endif
         if (abs (ztauvt) .lt. eps) then
            ztauvt = eps
         endif
         hulp   = 90. - atan2 (ztauvt,ztauut) * 180 / pi + grdang
         ztaud  = amod (hulp  + 720., 360.)
      endif

      n = n + 1
      if (parcod .eq. 1) then
         xdata(n) = zwl
      else if (parcod .eq.  2) then
         xdata(n) = zwh
      else if (parcod .eq. 16) then
         xdata(n) = ztauut
      else if (parcod .eq. 17) then
         xdata(n) = ztauvt
      else if (parcod .eq. 18) then
         xdata(n) = ztaum
      else if (parcod .eq. 19) then
         xdata(n) = ztaud
      endif
100 continue

!-----------------------------------------------------------------------
!-----return status to calling routine
!-----------------------------------------------------------------------
   ierror = IEOK
   if ( .not. okee ) then
      ierror = IEOTHR
   endif
!
end
subroutine gtalfs( hdefds, hdafds, istat, nostat, alfas,&
&buff, ierror )
!
!     purpose:
!
!              Get alfas (for backward transformation) from NEFIS file
!              If element is not on the file (old TRISULA version)
!              then alfas is computed from xksi and yksi
!              alfas is transformed to radians
!
!
   integer hdefds(*), hdafds(*)
   integer istat, nostat, ierror
   real    buff(nostat), alfas

   integer      uindex(3), usrord, buflen
   real         xksi, yksi, raddeg, degrad, pi, eps
   character*16 grpdef,elmnam

   integer*4    GETELT,GETELS

!
!     General intialisation
!
   pi = 4. * atan(1.0)
   raddeg = 180./pi
   degrad = pi/180.
   eps    = 1.E-10
!
   buflen = 4 * nostat
   grpdef = 'his-const'
   elmnam = 'ALFAS'
   uindex(1) = 1
   uindex(2) = 1
   uindex(3) = 1
   usrord    = 1
!
!     Try to read the element ALFAS
!
   ierror = GETELT( hdefds, grpdef, elmnam, uindex,&
   &usrord, buflen, buff  )
   if ( ierror .ne. 0) then
!
!       In case of an old trih file read XKSI and YKSI and compute
!       ALFAS
!
      buflen = 4 * nostat
      elmnam = 'XKSI'
      ierror = GETELT( hdefds, grpdef, elmnam, uindex,&
      &usrord, buflen, buff  )
      if ( ierror .ne. 0 ) go to 900

      xksi   = buff(istat)
      elmnam = 'YKSI'
      ierror = GETELT( hdefds, grpdef, elmnam, uindex,&
      &usrord, buflen, buff  )
      if ( ierror .ne. 0 ) go to 900

      yksi = buff(istat)
      if ( abs(xksi) .le. eps .and. abs(yksi) .le. eps ) then
         alfas = 0.0
      else
         alfas = atan2( yksi, xksi)
      endif
   else
      alfas = buff(istat) * degrad
   endif

900 return
end




