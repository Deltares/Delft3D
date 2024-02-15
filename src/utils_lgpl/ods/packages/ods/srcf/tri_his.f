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
c        $Author: Markus $
c        $Date: 1-04-03 10:52 $
c        $Source: /u/cvsroot/gpp/libsrc/ods/tri_his.f,v $
c
c#ifdef WINNT
c      INCLUDE '../include/nfsintrf.i'
c
c      INTERFACE TO FUNCTION GETELT_i [ALIAS:'_GETELT']
c     +                             ( VALUE1, VALUE2, VALUE3, VALUE4 ,
c     +                               VALUE5, VALUE6, VALUE7, VALUE8 )
c
c      INTEGER   GETELT_i
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
c
c      INTERFACE TO FUNCTION GETELT_j [ALIAS:'_GETELT']
c     +                             ( VALUE1, VALUE2, VALUE3, VALUE4 ,
c     +                               VALUE5, VALUE6, VALUE7, VALUE8 )
c
c      INTEGER   GETELT_j
c
c      INTEGER   VALUE1
c      INTEGER   VALUE2
c      CHARACTER VALUE3
c      CHARACTER VALUE4
c      INTEGER   VALUE5
c      INTEGER   VALUE6
c      INTEGER   VALUE7
c      INTEGER   VALUE8
c
c      END

c#endif

      subroutine hispar
c#ifdef WINNT
c     *                 [ALIAS:'_hispar']
c#endif
     *                 (fname , itype , pardef, maxdef, timdep, locdep,
     *                  maxlst, lang  , parlst, paruni, partyp, parcod,
     *                  nrlst , ierror, option                        )
c-----------------------------------------------------------------------
c           Function: parameter name selection for time histories
c                     TRISULA NEFIS files
c        Method used:
c
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
c FOUT        L*4                  Flag for further execution program
c                                  fout  = .true.  : stop execution
c                                  fout  = .false. : go on
c FILHDA     CH*256                File name NEFIS data file for HIS
c FILHDE     CH*256                File name NEFIS definition file for
c                                  HIS
c GRPDEF     CH*16                 Group name definition
c HDAFDS      I*4  999             Data file descriptor for the HIS-DAT
c                                  file
c HDEFDS      I*4  2997            Definition file description for the
c                                  HIS-DEF file
c IERROR      I*4                  Error code for NEFIS error
c KMAX        I*4                  Number of layers
c LMAX        I*4                  Total number of constituents
c                                  for old files LMAX = LSTCI
c                                  for new files LMAX = LSTCI + LTUR
c LSTCI       I*4                  Total number of constituents (incl.
c                                  turbulence for old trim files).
c LTUR        I*4                  Number of turbulence constituents
c NAMCON     CH*20                 Constituent names
c NOSTAT      I*4                  Number of defined stations
c NTRUV       I*4                  Number of cross-secties in u- and
c                                  v-direction
c HISIND      I*4  MXNPAR          TRISULA-NEFIS dependant index list
c HISLST     CH*20 MXNPAR          TRISULA-NEFIS possible parameterlist
c HISTYP      I*4  MXNPAR          TRISULA-NEFIS possible code list
c HISUNI     CH*20 MXNPAR          TRISULA-NEFIS possible unitlist
c SELHIS     CH*23                 Output flags containing Y or N for
c                                  various output quantities selection
c                                  for his files
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
c-----see TRISULA user Manual, release 2.45, version 0.1 May 1995
c     Appendix A: limitations number of constituents <= 5
c                             + Salinity + Temperature + Turbulence
      parameter (mxnpar = 75)
c
      integer         lang
      integer         locdep
      integer         timdep,itype
      integer         maxdef,maxlst,i     ,npar  ,nostat,ntruv
      integer         kmax  ,lmax  ,l     ,ind   ,lstci ,ltur
      integer         ierror,nrlst ,irho
c
      integer         hisind(mxnpar)
      integer         histyp(mxnpar)
      integer         partyp(maxlst)
      integer         parcod(maxlst)
c
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
c
      logical         ex    ,fout  ,zrho
c-----------------------------------------------------------------------
c-----declaration NEFIS
c-----------------------------------------------------------------------
      integer       hdefds( 2997),hdafds(  999)
c
      integer       uindex(    3),usrord,buflen       ,
     *              nbytsg,elmndm(    5),elmdms(     5)
c
      character*8   elmtyp

      character*16  grpdef,elmnam,elmqty,elmunt

      character*64  elmdes
c
      integer       GETELT,GETELS,INQELM

c#ifdef WINNT
c     integer       GETELT_i
c#endif

      integer       TMLCDP
      parameter     ( TMLCDP = IPLDEP + IPLLST )
c
      data (hisind(i),hislst(i),hisuni(i),histyp(i),i=1,15,1)/
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
     *  12,'flow rate u         ','m**3/s              ',    TMLCDP,
     *  13,'flow rate v         ','m**3/s              ',    TMLCDP,
     *  14,'                    ','                    ',    TMLCDP,
     *  15,'eddy viscosity      ','m**2/s              ',    TMLCDP/
      data (hisind(i),hislst(i),hisuni(i),histyp(i),i=16,30,1)/
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
      data (hisind(i),hislst(i),hisuni(i),histyp(i),i=31,45,1)/
     *  31,'density             ','kg/m**3             ',    TMLCDP,
     *  32,'eddy diffusivity    ','m**2/s              ',    TMLCDP,
     *  33,'accumulated flow    ','m**3                ',    TMLCDP,
     *  34,'momentary flow      ','m**3/s              ',    TMLCDP,
     *  35,'                    ','                    ',    TMLCDP,
     *  36,'                    ','                    ',    TMLCDP,
     *  37,'                    ','                    ',    TMLCDP,
c  u and v are not correctly defined in u-points -> removed
c    *  38,'current u at u-point','m/s                 ',    TMLCDP,
c    *  39,'current v at v-point','m/s                 ',    TMLCDP,
     *  38,'                    ','                    ',    TMLCDP,
     *  39,'                    ','                    ',    TMLCDP,
c  bedstress u and v are not correctly defined in u-points -> removed
c    *  40,'bed stress at u-pnt.','N/m**2              ',    TMLCDP,
c    *  41,'bed stress at v-pnt.','N/m**2              ',    TMLCDP,
     *  40,'                    ','                    ',    TMLCDP,
     *  41,'                    ','                    ',    TMLCDP,
     *  42,'accumulated flow u  ','m**3                ',    TMLCDP,
     *  43,'accumulated flow v  ','m**3                ',    TMLCDP,
     *  44,'                    ','                    ',    TMLCDP,
     *  45,'                    ','                    ',    TMLCDP/
      data (hisind(i),hislst(i),hisuni(i),histyp(i),i=46,55,1)/
     *  46,'adv. flux           ','user defined        ',    TMLCDP,
     *  47,'adv. flux           ','user defined        ',    TMLCDP,
     *  48,'adv. flux           ','user defined        ',    TMLCDP,
     *  49,'adv. flux           ','user defined        ',    TMLCDP,
     *  50,'adv. flux           ','user defined        ',    TMLCDP,
     *  51,'adv. flux           ','user defined        ',    TMLCDP,
     *  52,'adv. flux           ','user defined        ',    TMLCDP,
     *  53,'adv. flux           ','user defined        ',    TMLCDP,
     *  54,'adv. flux           ','user defined        ',    TMLCDP,
     *  55,'adv. flux           ','user defined        ',    TMLCDP/
      data (hisind(i),hislst(i),hisuni(i),histyp(i),i=56,65,1)/
     *  56,'dis. flux           ','user defined        ',    TMLCDP,
     *  57,'dis. flux           ','user defined        ',    TMLCDP,
     *  58,'dis. flux           ','user defined        ',    TMLCDP,
     *  59,'dis. flux           ','user defined        ',    TMLCDP,
     *  60,'dis. flux           ','user defined        ',    TMLCDP,
     *  61,'dis. flux           ','user defined        ',    TMLCDP,
     *  62,'dis. flux           ','user defined        ',    TMLCDP,
     *  63,'dis. flux           ','user defined        ',    TMLCDP,
     *  64,'dis. flux           ','user defined        ',    TMLCDP,
     *  65,'dis. flux           ','user defined        ',    TMLCDP/
      data (hisind(i),hislst(i),hisuni(i),histyp(i),i=66,75,1)/
     *  66,'tot. flux           ','user defined        ',    TMLCDP,
     *  67,'tot. flux           ','user defined        ',    TMLCDP,
     *  68,'tot. flux           ','user defined        ',    TMLCDP,
     *  69,'tot. flux           ','user defined        ',    TMLCDP,
     *  70,'tot. flux           ','user defined        ',    TMLCDP,
     *  71,'tot. flux           ','user defined        ',    TMLCDP,
     *  72,'tot. flux           ','user defined        ',    TMLCDP,
     *  73,'tot. flux           ','user defined        ',    TMLCDP,
     *  74,'tot. flux           ','user defined        ',    TMLCDP,
     *  75,'tot. flux           ','user defined        ',    TMLCDP/

c-----------------------------------------------------------------------
c-----Initialisation
c-----------------------------------------------------------------------
      fout   = .false.
      zrho   = .false.
      ierror =  0
c
c--------------------------------------------------------------------
c-----Test if trih-<runid>.dat and trih-<runid>.def Nefis HIS-files
c     exist
c--------------------------------------------------------------------
      call ods_check_nefis( fname  , '.def' , ierror )
      if ( ierror .ne. ieok   ) then
         return
      endif
c--------------------------------------------------------------------
c-----Open trih-<runid>.dat and trih-<runid>.def HIS-files
c--------------------------------------------------------------------
      call OPNNEF(fname, itype, hdafds, hdefds, ierror)
      if (ierror .ne. 0) then
         ierror = IEFIRO
         return
      endif
c--------------------------------------------------------------------
c-----Read array-dimensions from Nefis HIS-files group 2
c--------------------------------------------------------------------
      grpdef = 'his-const'
c
      uindex(1) = 1
      uindex(2) = 1
      uindex(3) = 1
      usrord    = 1
      buflen    = 4
c
      npar      = 0
      nostat    = 0
      ntruv     = 0
      lmax      = 0
      lstci     = 0
      ltur      = 0
      kmax      = 0
c
      elmnam    = 'NOSTAT'
      ierror    = GETELT(hdefds,grpdef    ,elmnam    ,
     *                   uindex,usrord    ,buflen    ,NOSTAT    )
      if (ierror .ne. 0) then
         fout   = .true.
         goto 8888
      endif
c
      elmnam    = 'NTRUV'
      ierror    = GETELT(hdefds,grpdef    ,elmnam    ,
     *                   uindex,usrord    ,buflen    ,NTRUV     )
      if (ierror .ne. 0) then
         fout   = .true.
         goto 8888
      endif
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
            fout   = .true.
            goto 8888
         endif
c
         elmnam = 'LTUR'
         ierror = GETELT(hdefds,grpdef    ,elmnam    ,
     *                   uindex,usrord    ,buflen    ,LTUR      )
         if (ierror .ne. 0) then
            fout   = .true.
            goto 8888
         endif
      endif
c
      if (nostat .gt. 0) then
         elmnam = 'KMAX'
         ierror = GETELT(hdefds,grpdef    ,elmnam    ,
     *                   uindex,usrord    ,buflen    ,KMAX      )
         if (ierror .ne. 0) then
            fout   = .true.
            goto 8888
         endif
      endif
c
      lmax = lstci + ltur
c
c-----------------------------------------------------------------------
c-----Element SELHIS selection of output
c-----------------------------------------------------------------------
      buflen    = 23
      elmnam    = 'SELHIS'
c#ifdef WINNT
c     ierror    = GETELT_i
c#else
      ierror    = GETELS
c#endif
     *                  (hdefds   ,grpdef    ,elmnam    ,
     *                   uindex   ,usrord    ,buflen    ,SELHIS    )
      if (ierror .ne. 0) then
c-----------------------------------------------------------------------
c--------In case of a old trih file no SELHIS then
c           re-define SELHIS
c-----------------------------------------------------------------------
         selhis = 'YYYYYYYYYYYYYYYYYYYYYYY'
         selhis(19:19) = 'X'
         if (nostat .eq. 0) selhis( 1:19) = 'NNNNNNNNNNNNNNNNNNN'
         if (kmax   .eq. 1) selhis( 4: 4) = 'N'
         if (lstci  .eq. 0) selhis( 5:12) = 'NNNNNNNN'
         if (ltur   .eq. 0) selhis(13:14) = 'NN'
         if (kmax   .eq. 1) selhis(17:18) = 'NN'
         if (lmax   .eq. 0) selhis(18:18) = 'N'
         if (ntruv  .eq. 0) selhis(20:23) = 'NNNN'
         if (lstci  .eq. 0) selhis(22:23) = 'NN'
      endif
c-----------------------------------------------------------------------
c-----Element NAMCON constituents and turbulence quantity names
c     only if selhis( 5:14) <> 'NNNNNNNNNN'
c-----------------------------------------------------------------------
      irho   = 0
      if (index (selhis( 5:14),'Y') .gt. 0) then
         buflen    = 20 * lmax
         elmnam    = 'NAMCON'
c#ifdef WINNT
c        ierror = GETELT_i
c#else
         ierror = GETELS
c#endif
     *                  (hdefds,grpdef    ,elmnam    ,
     *                   uindex,usrord    ,buflen    ,NAMCON    )
         if (ierror .ne. 0) then
            fout   = .true.
            goto 8888
         endif
c
         do 5 l = 1,lmax
            if (selhis(4+l:4+l) .eq. 'Y' ) then
               if (namcon(l)(:11) .eq. 'Salinity   ') irho   = 1
               if (namcon(l)(:11) .eq. 'Temperature') irho   = 1
            endif
    5    continue
      endif
c-----------------------------------------------------------------------
c-----In case of a old trih file re-define SELHIS(19:19)
c-----------------------------------------------------------------------
      if (selhis(19:19) .eq. 'X') then
         selhis(19:19) = 'N'
         if (irho   .eq. 1) selhis(19:19) = 'Y'
      endif
      zrho   = (irho .eq. 1)
c
c--------------------------------------------------------------------
c-----Generate parameternames from Nefis HIS-files
c--------------------------------------------------------------------
      if (nostat .gt. 0) then
         if ( selhis(1:1) .eq. 'Y' ) then
c--------water level, ZWL
            npar = npar + 1
            parcod(npar) = hisind( 1)
            parlst(npar) = hislst(parcod(npar))
            partyp(npar) = histyp(parcod(npar))
            paruni(npar) = hisuni(parcod(npar))
c--------water depth, ZWL + DPS
            npar = npar + 1
            parcod(npar) = hisind( 2)
            parlst(npar) = hislst(parcod(npar))
            partyp(npar) = histyp(parcod(npar))
            paruni(npar) = hisuni(parcod(npar))
         endif
         if ( selhis(2:3) .eq. 'YY' ) then
            if (kmax .gt. 1) then
c-----------dpt. aver. cur. u
               npar = npar + 1
               parcod(npar) = hisind( 3)
               parlst(npar) = hislst(parcod(npar))
               partyp(npar) = histyp(parcod(npar))
               paruni(npar) = hisuni(parcod(npar))
c-----------dpt. aver. cur. v
               npar = npar + 1
               parcod(npar) = hisind( 4)
               parlst(npar) = hislst(parcod(npar))
               partyp(npar) = histyp(parcod(npar))
               paruni(npar) = hisuni(parcod(npar))
c-----------dpt. aver. cur. mag.
               npar = npar + 1
               parcod(npar) = hisind( 5)
               parlst(npar) = hislst(parcod(npar))
               partyp(npar) = histyp(parcod(npar))
               paruni(npar) = hisuni(parcod(npar))
c-----------dpt. aver. cur. dir.
               npar = npar + 1
               parcod(npar) = hisind( 6)
               parlst(npar) = hislst(parcod(npar))
               partyp(npar) = histyp(parcod(npar))
               paruni(npar) = hisuni(parcod(npar))
            endif

c--------current u (layer)
            npar = npar + 1
            parcod(npar) = hisind( 7)
            parlst(npar) = hislst(parcod(npar))
            partyp(npar) = histyp(parcod(npar))
            paruni(npar) = hisuni(parcod(npar))
c--------current v (layer)
            npar = npar + 1
            parcod(npar) = hisind( 8)
            parlst(npar) = hislst(parcod(npar))
            partyp(npar) = histyp(parcod(npar))
            paruni(npar) = hisuni(parcod(npar))
c--------current mag. (layer)
            npar = npar + 1
            parcod(npar) = hisind( 9)
            parlst(npar) = hislst(parcod(npar))
            partyp(npar) = histyp(parcod(npar))
            paruni(npar) = hisuni(parcod(npar))
c--------current dir. (layer)
            npar = npar + 1
            parcod(npar) = hisind(10)
            parlst(npar) = hislst(parcod(npar))
            partyp(npar) = histyp(parcod(npar))
            paruni(npar) = hisuni(parcod(npar))
         endif

         if ( selhis(4:4) .eq. 'Y' ) then
            if (kmax .gt. 1) then
c-----------current w.   (layer)
               npar = npar + 1
               parcod(npar) = hisind(11)
               parlst(npar) = hislst(parcod(npar))
               partyp(npar) = histyp(parcod(npar))
               paruni(npar) = hisuni(parcod(npar))
            endif
         endif

         if ( selhis(20:20) .eq. 'Y' ) then
c--------flow rate u, ZQXK
            npar = npar + 1
            parcod(npar) = hisind(12)
            parlst(npar) = hislst(parcod(npar))
            partyp(npar) = histyp(parcod(npar))
            paruni(npar) = hisuni(parcod(npar))
c--------flow rate v, ZQYK
            npar = npar + 1
            parcod(npar) = hisind(13)
            parlst(npar) = hislst(parcod(npar))
            partyp(npar) = histyp(parcod(npar))
            paruni(npar) = hisuni(parcod(npar))
         endif

         if ( selhis(17:17) .eq. 'Y' ) then
            if (kmax .gt. 1) then
c-----------viscosity, ZVICWW
               npar = npar + 1
               parcod(npar) = hisind(15)
               parlst(npar) = hislst(parcod(npar))
               partyp(npar) = histyp(parcod(npar))
               paruni(npar) = hisuni(parcod(npar))
            endif
         endif

         if ( selhis(15:16) .eq. 'YY' ) then
c--------bed stress u, ZTAUKSI
            npar = npar + 1
            parcod(npar) = hisind(16)
            parlst(npar) = hislst(parcod(npar))
            partyp(npar) = histyp(parcod(npar))
            paruni(npar) = hisuni(parcod(npar))
c--------bed stress v, ZTAUETA
            npar = npar + 1
            parcod(npar) = hisind(17)
            parlst(npar) = hislst(parcod(npar))
            partyp(npar) = histyp(parcod(npar))
            paruni(npar) = hisuni(parcod(npar))
c--------bed stress mag, ZTAUETA
            npar = npar + 1
            parcod(npar) = hisind(18)
            parlst(npar) = hislst(parcod(npar))
            partyp(npar) = histyp(parcod(npar))
            paruni(npar) = hisuni(parcod(npar))
c--------bed stress dir, ZTAUETA
            npar = npar + 1
            parcod(npar) = hisind(19)
            parlst(npar) = hislst(parcod(npar))
            partyp(npar) = histyp(parcod(npar))
            paruni(npar) = hisuni(parcod(npar))
         endif

         if (lmax   .gt. 0 ) then
c-----------constituents, GRO(1:lstci), ZTUR(1:ltur)
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
c--------------density, ZRHO
               npar = npar + 1
               parcod(npar) = hisind(31)
               parlst(npar) = hislst(parcod(npar))
               partyp(npar) = histyp(parcod(npar))
               paruni(npar) = hisuni(parcod(npar))
            endif
         endif

         if ( selhis(17:17) .eq. 'Y' ) then
            if (kmax   .gt. 1) then
c--------------diffusivity, ZDICWW
               npar = npar + 1
               parcod(npar) = hisind(32)
               parlst(npar) = hislst(parcod(npar))
               partyp(npar) = histyp(parcod(npar))
               paruni(npar) = hisuni(parcod(npar))
            endif
         endif
      endif

      if (nostat .gt. 0) then

c--------bed stress at u-point - removed ( not available )
c--------bed stress at v-point - removed ( not available )

         if ( selhis(20:20) .eq. 'Y' ) then
c--------accumulated flow u
            npar = npar + 1
            parcod(npar) = hisind(42)
            parlst(npar) = hislst(parcod(npar))
            partyp(npar) = histyp(parcod(npar))
            paruni(npar) = hisuni(parcod(npar))
c--------accumulated flow v
            npar = npar + 1
            parcod(npar) = hisind(43)
            parlst(npar) = hislst(parcod(npar))
            partyp(npar) = histyp(parcod(npar))
            paruni(npar) = hisuni(parcod(npar))
         endif
      endif
c
      if (ntruv  .gt. 0) then
         if ( selhis(20:20) .eq. 'Y' ) then
c--------momentary flow , FLTR
            npar = npar + 1
            parcod(npar) = hisind(33)
            parlst(npar) = hislst(parcod(npar))
            partyp(npar) = histyp(parcod(npar))
            paruni(npar) = hisuni(parcod(npar))
c--------accumulated flow,CTR
            npar = npar + 1
            parcod(npar) = hisind(34)
            parlst(npar) = hislst(parcod(npar))
            partyp(npar) = histyp(parcod(npar))
            paruni(npar) = hisuni(parcod(npar))
         endif

         if (lstci  .gt. 0) then
            do 20 l = 1, lstci, 1
               if ( selhis(22:22) .eq. 'Y' ) then
c--------------advective flux, ATR(1:lstci)
                  npar = npar + 1
                  parcod(npar) = hisind(46+l)
                  parlst(npar) = hislst(parcod(npar))(1:10) //
     *                           namcon(l)(1:10)
                  partyp(npar) = histyp(parcod(npar))
                  paruni(npar) = hisuni(parcod(npar))
               endif
   20       continue

            do 30 l = 1, lstci, 1
               if ( selhis(22:22) .eq. 'Y' ) then
c--------------dispersive flux, DTR(1:lstci)
                  npar = npar + 1
                  parcod(npar) = hisind(56+l)
                  parlst(npar) = hislst(parcod(npar))(1:10) //
     *                           namcon(l)(1:10)
                  partyp(npar) = histyp(parcod(npar))
                  paruni(npar) = hisuni(parcod(npar))
               endif
   30       continue
c
            do 40 l = 1, lstci, 1
               if ( selhis(22:22) .eq. 'Y' ) then
c--------------total flux, (ATR + DTR)(1:lstci)
                  npar = npar + 1
                  parcod(npar) = hisind(66+l)
                  parlst(npar) = hislst(parcod(npar))(1:10) //
     *                           namcon(l)(1:10)
                  partyp(npar) = histyp(parcod(npar))
                  paruni(npar) = hisuni(parcod(npar))
               endif
   40       continue
         endif
      endif

      nrlst = npar
c
c-----------------------------------------------------------------------
c-----check; filter not yet used so found number should be less or equal
c     the required number
c-----------------------------------------------------------------------
       if (npar .gt. maxlst) then
         fout  = .true.
      endif
c--------------------------------------------------------------------
c-----Close trih-<runid>.dat and trih-<runid>.def HIS-files
c--------------------------------------------------------------------
 8888 continue
      call CLOSFL(fname, ierror)
      if (ierror .ne. 0) then
         fout  = .true.
      endif
c-----------------------------------------------------------------------
c-----return status to calling routine
c-----------------------------------------------------------------------
      ierror = IEOK
      if (fout  ) then
         ierror = IEOTHR
      endif
c
      return
c-----------------------------------------------------------------------
      end

      subroutine hisdim
c#ifdef WINNT
c    *          [ALIAS:'_hisdim']
c#endif
     *                  (fname ,itype ,dimtyp, pardep, timdep,
     *                   locdep,ndim  ,ierror, option        )
c-----------------------------------------------------------------------
c           Function: dimension selection for time histories
c                     TRISULA NEFIS files
c        Method used:
c
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
c FOUT        L*4                  Flag for further execution program
c                                  fout  = .true.  : stop execution
c                                  fout  = .false. : go on
c FILHDA     CH*256                File name NEFIS data file for HIS
c FILHDE     CH*256                File name NEFIS definition file for
c                                  HIS
c GRPDEF     CH*16                 Group name definition
c GRPDMS      I*4  5               Array with GRPNDM dimensions
c GRPNDM      I*4                  Number of dimenmsions of this group
c GRPORD      I*4  5               Array which gives order in which data
c                                  must be read
c HDAFDS      I*4  999             Data file descriptor for the HIS-DAT
c                                  file
c HDEFDS      I*4  2997            Definition file description for the
c                                  HIS-DEF file
c IERROR      I*4                  Error code for NEFIS error
c KMAX        I*4                  Number of layers
c L           I*4                  Help variable
c LMAX        I*4                  Total number of constituents
c                                  for old files LMAX = LSTCI
c                                  for new files LMAX = LSTCI + LTUR
c LSTCI       I*4                  Total number of constituents (incl.
c                                  turbulence for old trim files).
c LTUR        I*4                  Number of turbulence constituents
c NOSTAT      I*4                  Number of defined stations
c NPAR        I*4                  Number of found parameters
c NRCEL       I*4                  Number of cells defined in group 1&3
c NTRUV       I*4                  Number of cross-secties in u- and
c                                  v-direction
c SELHIS     CH*23                 Output flags containing Y or N for
c                                  various output quantities selection
c                                  for his files
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
      integer         ierror,itype ,lstci ,ltur  ,irho
      integer         nrcel ,npar  ,nostat,ntruv ,lmax  ,kmax  ,l
      integer         pardep,timdep,locdep
      integer         ndim   (4    )
c
      character*3     dimtyp
      character       fname(*)*(*)
      character*20    namcon(10)
      character*23    selhis
      character*256   filhda,filhde
      character*256   option
c
      logical         ex    ,fout  ,zrho  ,check
c-----------------------------------------------------------------------
c-----declaration NEFIS
c-----------------------------------------------------------------------
      integer       hdefds( 2997),hdafds(  999)
c
      integer       grpndm,grpdms(    5),grpord(    5),
     *              uindex(    3),usrord,buflen       ,
     *              nbytsg,elmndm(    5),elmdms(     5),
     *              ind
c
      character*8   elmtyp
c
      character*16  grpdef,elmnam,celnam,elmqty,elmunt
c
      character*64  elmdes
c
      integer       INQGRP,GETELT,GETELS,INQELM
      integer       INQMXI
c#ifdef WINNT
c     integer       GETELT_i
c#endif

c-----------------------------------------------------------------------
c-----Initialisation
c-----------------------------------------------------------------------
      fout   = .false.
      zrho   = .false.
c
      ierror =  0
c
c--------------------------------------------------------------------
c-----Test if trih-<runid>.dat and trih-<runid>.def Nefis HIS-files
c     exist
c--------------------------------------------------------------------
      call ods_check_nefis( fname  , '.def' , ierror )
      if ( ierror .ne. ieok   ) then
         return
      endif
c--------------------------------------------------------------------
c-----Open trih-<runid>.dat and trih-<runid>.def HIS-files
c--------------------------------------------------------------------
      call OPNNEF(fname, itype, hdafds, hdefds, ierror)
      if (ierror .ne. 0) then
         ierror = IEFIRO
         return
      endif
c--------------------------------------------------------------------
c-----Read array-dimension nrcel from Nefis HIS-files group 1
c--------------------------------------------------------------------
      grpdef = 'his-info-series'
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
         if (nrcel  .eq. 0) then
            fout   = .true.
            goto 8888
         endif
      endif
c--------------------------------------------------------------------
c-----Read array-dimensions from Nefis HIS-files group 2
c--------------------------------------------------------------------
      grpdef = 'his-const'
c
      uindex(1) = 1
      uindex(2) = 1
      uindex(3) = 1
      usrord    = 1
      buflen    = 4
c
      npar      = 0
      nostat    = 0
      ntruv     = 0
      lmax      = 0
      lstci     = 0
      ltur      = 0
      kmax      = 0
c
      elmnam    = 'NOSTAT'
      ierror    = GETELT(hdefds,grpdef    ,elmnam    ,
     *                   uindex,usrord    ,buflen    ,NOSTAT    )
      if (ierror .ne. 0) then
         fout   = .true.
         goto 8888
      endif
c
      elmnam    = 'NTRUV'
      ierror    = GETELT(hdefds,grpdef    ,elmnam    ,
     *                   uindex,usrord    ,buflen    ,NTRUV     )
      if (ierror .ne. 0) then
         fout   = .true.
         goto 8888
      endif
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
            fout   = .true.
            goto 8888
         endif

         elmnam = 'LTUR'
         ierror = GETELT(hdefds,grpdef    ,elmnam    ,
     *                   uindex,usrord    ,buflen    ,LTUR      )
         if (ierror .ne. 0) then
            fout   = .true.
            goto 8888
         endif
      endif
c
      if (nostat .gt. 0) then
         elmnam = 'KMAX'
         ierror = GETELT(hdefds,grpdef    ,elmnam    ,
     *                   uindex,usrord    ,buflen    ,KMAX      )
         if (ierror .ne. 0) then
            fout   = .true.
            goto 8888
         endif
      endif
c
      lmax = lstci + ltur
c
c-----------------------------------------------------------------------
c-----Element SELHIS selection of output
c-----------------------------------------------------------------------
      buflen    = 23
      elmnam    = 'SELHIS'
c#ifdef WINNT
c     ierror    = GETELT_i
c#else
      ierror    = GETELS
c#endif
     *                  (hdefds   ,grpdef    ,elmnam    ,
     *                   uindex   ,usrord    ,buflen    ,SELHIS    )
      if (ierror .ne. 0) then
c-----------------------------------------------------------------------
c--------In case of a old trih file no SELHIS then
c           re-define SELHIS
c-----------------------------------------------------------------------
         selhis = 'YYYYYYYYYYYYYYYYYYYYYYY'
         selhis(19:19) = 'X'
         if (nostat .eq. 0) selhis( 1:19) = 'NNNNNNNNNNNNNNNNNNN'
         if (kmax   .eq. 1) selhis( 4: 4) = 'N'
         if (lstci  .eq. 0) selhis( 5:12) = 'NNNNNNNN'
         if (ltur   .eq. 0) selhis(13:14) = 'NN'
         if (kmax   .eq. 1) selhis(17:18) = 'NN'
         if (lmax   .eq. 0) selhis(18:18) = 'N'
         if (ntruv  .eq. 0) selhis(20:23) = 'NNNN'
         if (lstci  .eq. 0) selhis(22:23) = 'NN'
      endif
c-----------------------------------------------------------------------
c-----Element NAMCON constituents and turbulence quantity names
c     only if selhis( 5:14) <> 'NNNNNNNNNN'
c-----------------------------------------------------------------------
      irho   = 0
      if (index (selhis( 5:14),'Y') .gt. 0) then
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
            fout   = .true.
            goto 8888
         endif
c
         do 5 l = 1,lmax
            if (selhis(4+l:4+l) .eq. 'Y' ) then
               if (namcon(l)(:11) .eq. 'Salinity   ') irho   = 1
               if (namcon(l)(:11) .eq. 'Temperature') irho   = 1
            endif
    5    continue
      endif
c-----------------------------------------------------------------------
c-----In case of a old trih file re-define SELHIS(19:19)
c-----------------------------------------------------------------------
      if (selhis(19:19) .eq. 'X') then
         selhis(19:19) = 'N'
         if (irho   .eq. 1) selhis(19:19) = 'Y'
      endif
      zrho   = (irho .eq. 1)
c
c--------------------------------------------------------------------
c-----Generate parameternames from Nefis HIS-files
c--------------------------------------------------------------------
      if (nostat .gt. 0) then
         if ( selhis(1:1) .eq. 'Y' ) then
c--------water level, ZWL
            npar = npar + 1
c--------water depth, ZWL + DPS
            npar = npar + 1
         endif
         if ( selhis(2:3) .eq. 'YY' ) then
            if (kmax .gt. 1) then
c-----------dpt. aver. cur. u
               npar = npar + 1
c-----------dpt. aver. cur. v
               npar = npar + 1
c-----------dpt. aver. cur. mag.
               npar = npar + 1
c-----------dpt. aver. cur. dir.
               npar = npar + 1
            endif

c--------current u (layer)
            npar = npar + 1
c--------current v (layer)
            npar = npar + 1
c--------current mag. (layer)
            npar = npar + 1
c--------current dir. (layer)
            npar = npar + 1
         endif

         if ( selhis(4:4) .eq. 'Y' ) then
            if (kmax .gt. 1) then
c-----------current w.   (layer)
               npar = npar + 1
            endif
         endif

         if ( selhis(20:20) .eq. 'Y' ) then
c--------flow rate u, ZQXK
            npar = npar + 1
c--------flow rate v, ZQYK
            npar = npar + 1
         endif

         if ( selhis(17:17) .eq. 'Y' ) then
            if (kmax .gt. 1) then
c-----------viscosity, ZVICWW
               npar = npar + 1
            endif
         endif

         if ( selhis(15:16) .eq. 'YY' ) then
c--------bottomstress u, ZTAUKSI
            npar = npar + 1
c--------bottomstress v, ZTAUETA
            npar = npar + 1
c--------bottomstress mag, ZTAUETA
            npar = npar + 1
c--------bottomstress dir, ZTAUETA
            npar = npar + 1
         endif

         if (lmax   .gt. 0) then
c-----------constituents, GRO(1:lstci), ZTUR(1:ltur)
            do 10 l = 1, lmax, 1
               if ( selhis(4+l:4+l) .eq. 'Y' ) then
                  npar = npar + 1
               endif
   10       continue

            if (zrho) then
c--------------density, ZRHO
               npar = npar + 1
            endif
         endif

         if ( selhis(17:17) .eq. 'Y' ) then
            if (kmax   .gt. 1) then
c--------------diffusivity, ZDICWW
               npar = npar + 1
            endif
         endif
      endif


      if (nostat .gt. 0) then

c--------bottom stress at u-point
c removed         npar = npar + 1
c--------bottom stress at v-point
c removed         npar = npar + 1

         if ( selhis(20:20) .eq. 'Y' ) then
c--------accumulated flow u
            npar = npar + 1
c--------accumulated flow v
            npar = npar + 1
         endif
      endif

      if (ntruv  .gt. 0) then
         if ( selhis(20:20) .eq. 'Y' ) then
c--------momentary flow , FLTR
            npar = npar + 1
c--------accumulated flow,CTR
            npar = npar + 1
         endif

         if (lstci   .gt. 0) then
            do 20 l = 1, lstci, 1
               if ( selhis(22:22) .eq. 'Y' ) then
c--------------advective flux, ATR(1:lstci)
                  npar = npar + 1
               endif
   20       continue

            do 30 l = 1, lstci, 1
               if ( selhis(22:22) .eq. 'Y' ) then
c--------------dispersive flux, DTR(1:lstci)
                  npar = npar + 1
               endif
   30       continue

            do 40 l = 1, lstci, 1
               if ( selhis(22:22) .eq. 'Y' ) then
c--------------total flux, (ATR + DTR)(1:lstci)
                  npar = npar + 1
               endif
   40       continue
         endif
      endif
c
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
         ndim (1) = 1
         ndim (2) = 1
         ndim (3) = 1
         ndim (4) = 1
c--------select parameters with more then one layer
         check    =(pardep .ge.  7 .and. pardep .le. 13).or.
     *              pardep .eq. 15 .or.
     *             (pardep .ge. 20 .and. pardep .le. 29).or.
     *              pardep .eq. 31 .or.  pardep .eq. 32 .or.
     *              pardep .eq. 42 .or.  pardep .eq. 43
         if (kmax .gt. 1 .and. check) then
c-----------parameter with more then one layer
            ndim (1) = 3
            if (pardep .eq. 15 .or.  pardep .eq. 32 ) then
c--------------ZVICWW or ZDICWW with 0:kmax
               ndim (3) = kmax + 1
            else
c--------------all others 1:kmax
               ndim (3) = kmax
            endif
         else
c-----------parameter with one layer
            ndim (1) = 1
         endif
c--------pardep in { 1...32, 38...43} is station
c        pardep in {33...34, 46...75} is cross section
         check    = (pardep .ge.  1 .and. pardep .le. 32) .or.
     *              (pardep .ge. 38 .and. pardep .le. 43)
         if (check) then
            ndim (2) = nostat
         else
            ndim (2) = ntruv
         endif
      else
         fout  = .true.
      endif
c
c--------------------------------------------------------------------
c-----Close trih-<runid>.dat and trih-<runid>.def HIS-files
c--------------------------------------------------------------------
 8888 continue
      call CLOSFL(fname, ierror)
      if (ierror .ne. 0) then
         fout  = .true.
      endif
c-----------------------------------------------------------------------
c-----return status to calling routine
c-----------------------------------------------------------------------
      ierror = IEOK
      if (fout) then
         ierror = IEOTHR
      endif
c
      return
c-----------------------------------------------------------------------
      end

      subroutine histme
c#ifdef WINNT
c    *                 [ALIAS:'_histme']
c#endif
     *                 (fname  ,itype  ,timdef, maxdef ,pardep , locdep,
     *                  maxlst ,        timlst,         timtyp ,
     *                  nrlst  ,ierror ,option                         )
c-----------------------------------------------------------------------
c           Function: time selection for time histories
c                     TRISULA NEFIS files
c        Method used:
c
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
c FOUT        L*4                  Flag for further execution program
c                                  fout  = .true.  : stop execution
c                                  fout  = .false. : go on
c FILHDA     CH*256                File name NEFIS data file for HIS
c FILHDE     CH*256                File name NEFIS definition file for
c                                  HIS
c GRPDEF     CH*16                 Group name definition
c GRPDMS      I*4  5               Array with GRPNDM dimensions
c GRPNDM      I*4                  Number of dimenmsions of this group
c GRPORD      I*4  5               Array which gives order in which data
c                                  must be read
c HDAFDS      I*4  999             Data file descriptor for the HIS-DAT
c                                  file
c HDEFDS      I*4  2997            Definition file description for the
c                                  HIS-DEF file
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
      real*8          timlst(maxlst)
      real*8          timdef(maxdef,2)
      real            dt    ,tunit
c
      character       fname(*)*(*)
      character*256   filhda,filhde
      character*256   option
c
      logical         ex    ,fout
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

c-----------------------------------------------------------------------
c-----Initialisation
c-----------------------------------------------------------------------
      fout   = .false.
      ierror =  0
c
c--------------------------------------------------------------------
c-----Test if trih-<runid>.dat and trih-<runid>.def Nefis HIS-files
c     exist
c--------------------------------------------------------------------
      call ods_check_nefis( fname  , '.def' , ierror )
      if ( ierror .ne. ieok   ) then
         return
      endif
c--------------------------------------------------------------------
c-----Open trih-<runid>.dat and trih-<runid>.def HIS-files
c--------------------------------------------------------------------
      call OPNNEF(fname, itype, hdafds, hdefds, ierror )
      if (ierror .ne. 0) then
         ierror = IEFIRO
         return
      endif
c--------------------------------------------------------------------
c-----Read array-dimension nrcel from Nefis HIS-files group 1
c--------------------------------------------------------------------
      grpdef = 'his-info-series'
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
         if (nrcel  .eq. 0) then
            fout   = .true.
            goto 8888
         endif
      endif
c--------------------------------------------------------------------
c-----Read constants from Nefis HIS-files group 2
c--------------------------------------------------------------------
      grpdef    = 'his-const'
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
      grpdef    = 'his-info-series'
      elmnam    = 'ITHISC'
c
      uindex(1) = 1
      uindex(2) = 1
      uindex(3) = 1
      usrord    = 1
      buflen    = 4
c-----------------------------------------------------------------------
c-----Define number of rows default nrcel, if an error
c     occurres (ierror <> 0) then re-define
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
c-----check; found number should be less or equal required number
c-----------------------------------------------------------------------
      if (nrcel .gt. maxlst) then
         fout  = .true.
      endif
c--------------------------------------------------------------------
c-----Close trih-<runid>.dat and trih-<runid>.def HIS-files
c--------------------------------------------------------------------
 8888 continue
      call CLOSFL(fname, ierror)
      if (ierror .ne. 0) then
         fout  = .true.
      endif
c-----------------------------------------------------------------------
c-----return status to calling routine
c-----------------------------------------------------------------------
      ierror = IEOK
      if (fout  ) then
         ierror = IEOTHR
      endif
c
      return
c-----------------------------------------------------------------------
      end

      subroutine julind(hdefds, hdafds, tim, nindex, ierror)
c-----------------------------------------------------------------------
c           Function: transform julian day to index in time series
c        Method used:
c
c-----------------------------------------------------------------------
c     Tree structure:
c
c         JULIND
c                INQGRP (nf)
c                INQMXI (nf)
c                GETELT (nf)
c
c     Explanation   :   md     - machine depended routine
c                       nf     - nefis functions
c-----------------------------------------------------------------------
c   Calling routine :              HISMAT
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
c HDAFDS      I*4  999        I    Data file descriptor for the HIS-DAT
c                                  file
c HDEFDS      I*4  2997       I    Definition file description for the
c                                  HIS-DEF file
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
c GRPNDM      I*4                  Number of dimenmsions of this group
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
c TIMSTP      R*8                  Current julian day for this step
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
      real            dt    ,tunit
      real*8          tim(3)
      real*8          timstp
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
c-----Read array-dimension nrcel from Nefis HIS-files group 1
c--------------------------------------------------------------------
      grpdef = 'his-info-series'
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
c-----Read constants from Nefis HIS-files group 2
c--------------------------------------------------------------------
      grpdef    = 'his-const'
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
      grpdef    = 'his-info-series'
      elmnam    = 'ITHISC'
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
         if ( timstp .le. tim(1) ) then
            nindex(1) = i
         else
            if ( timstp .le. tim(2) ) then
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

      subroutine hisloc
c#ifdef WINNT
c    *               [ALIAS:'_hisloc']
c#endif
     *                 (fname  ,itype  ,locdef ,maxdef ,pardep ,timdep ,
     *                  maxlst ,        loclst ,        loctyp ,nrlst  ,
     *                  locnr  ,ierror ,zbuffs ,option                 )
c-----------------------------------------------------------------------
c           Function: parameter name selection for time histories
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
c ZBUFFS     CH*20  maxlst    I/O  workspace names of locations
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
c FOUT        L*4                  Flag for further execution program
c                                  fout  = .true.  : stop execution
c                                  fout  = .false. : go on
c FILHDA     CH*256                File name NEFIS data file for HIS
c FILHDE     CH*256                File name NEFIS definition file for
c                                  HIS
c GRPDEF     CH*16                 Group name definition
c                                  must be read
c HDAFDS      I*4  999             Data file descriptor for the HIS-DAT
c                                  file
c HDEFDS      I*4  2997            Definition file description for the
c                                  HIS-DEF file
c I           I*4                  Help variable
c IERROR      I*4                  Error code for NEFIS error
c LNAME      CH*20                 Help var. location name
c NLOC        I*4                  Number of found locations
c NOSTAT      I*4                  Number of defined stations
c NTRUV       I*4                  Number of cross-secties in u- and
c                                  v-direction
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
      integer         ierror,nrlst ,nloc  ,ind
      integer         i     ,nostat,ntruv
      integer         maxlst,itype
      integer         maxdef
      integer         pardep
      integer         timdep
      integer         loctyp(maxlst)
      integer         locnr (maxlst)
c
      character       locdef(*)*(*)
      character       loclst(*)*(*)
      character*20    zbuffs(maxlst)
      character       fname(*)*(*)
      character*256   filhda,filhde
      character*256   option
c
      logical         ex    ,fout  ,check
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
c     integer       GETELT_i
c#endif
c-----------------------------------------------------------------------
c-----Initialisation
c-----------------------------------------------------------------------
      fout   = .false.
      ierror =  0
c--------------------------------------------------------------------
c-----Test if trih-<runid>.dat and trih-<runid>.def Nefis HIS-files
c     exist
c--------------------------------------------------------------------
      call ods_check_nefis( fname  , '.def' , ierror )
      if ( ierror .ne. ieok   ) then
         return
      endif
c--------------------------------------------------------------------
c-----Open trih-<runid>.dat and trih-<runid>.def HIS-files
c--------------------------------------------------------------------
      call OPNNEF(fname, itype, hdafds, hdefds, ierror )
      if (ierror .ne. 0) then
         ierror = IEFIRO
         return
      endif
c--------------------------------------------------------------------
c-----Read array-dimensions from Nefis HIS-files group 2
c--------------------------------------------------------------------
      grpdef = 'his-const'
c
      uindex(1) = 1
      uindex(2) = 1
      uindex(3) = 1
      usrord    = 1
      buflen    = 4
c
      nloc      = 0
      nostat    = 0
      ntruv     = 0
c
      elmnam    = 'NOSTAT'
      ierror    = GETELT(hdefds,grpdef    ,elmnam    ,
     *                   uindex,usrord    ,buflen    ,NOSTAT    )
      if (ierror .ne. 0) then
         fout   = .true.
         goto 8888
      endif
c
      elmnam    = 'NTRUV'
      ierror    = GETELT(hdefds,grpdef    ,elmnam    ,
     *                   uindex,usrord    ,buflen    ,NTRUV     )
      if (ierror .ne. 0) then
         fout   = .true.
         goto 8888
      endif
c--------------------------------------------------------------------
c-----Generate location names from Nefis HIS-files
c-----pardep in { 1...32, 38...43} is station
c     pardep in {33...34, 46...75} is cross section
c--------------------------------------------------------------------
      check    = pardep .ge.  1 .and. pardep .le. 32 .or.
     *           pardep .ge. 38 .and. pardep .le. 43
c
      if (check) then
c--------get station locations
         grpdef = 'his-const'
         uindex(1) = 1
         uindex(2) = 1
         uindex(3) = 1
         usrord    = 1
         buflen    = 20 * nostat
         elmnam    = 'NAMST'
c#ifdef WINNT
c        ierror    = GETELT_i
c#else
         ierror    = GETELS
c#endif
     *                     (hdefds,grpdef    ,elmnam    ,
     *                      uindex,usrord    ,buflen    ,zbuffs     )
         if (ierror .ne. 0) then
            fout   = .true.
            goto 8888
         endif
c
         do 10 i = 1, nostat
            nloc = nloc + 1
            loclst (nloc) = zbuffs(i)
            locnr  (nloc) = i
   10    continue

      else
c--------get cross section locations
         grpdef = 'his-const'
         uindex(1) = 1
         uindex(2) = 1
         uindex(3) = 1
         usrord    = 1
         buflen    = 20 * ntruv
         elmnam    = 'NAMTRA'
c#ifdef WINNT
c        ierror    = GETELT_i
c#else
         ierror    = GETELS
c#endif
     *                     (hdefds,grpdef    ,elmnam    ,
     *                      uindex,usrord    ,buflen    ,zbuffs     )
         if (ierror .ne. 0) then
            fout   = .true.
            goto 8888
         endif
c
         do 20 i = 1, ntruv
            nloc = nloc + 1
            loclst (nloc) = zbuffs(i)
            locnr  (nloc) = i
   20    continue
c
      endif

      nrlst = nloc

c-----------------------------------------------------------------------
c-----check found number against required number
c     filter not yet used
c-----------------------------------------------------------------------
      if (nloc   .gt. maxlst) then
         fout   = .true.
      endif
c--------------------------------------------------------------------
c-----Close trih-<runid>.dat and trih-<runid>.def HIS-files
c--------------------------------------------------------------------
 8888 continue
      call CLOSFL(fname, ierror)
      if (ierror .ne. 0) then
         fout  = .true.
      endif
c-----------------------------------------------------------------------
c-----return status to calling routine
c-----------------------------------------------------------------------
      ierror = IEOK
      if (fout  ) then
         ierror = IEOTHR
      endif
c
      return
c-----------------------------------------------------------------------
      end

      subroutine hismat
c#ifdef WINNT
c    *                [ALIAS:'_hismat']
c#endif
     *                  (fname ,itype  ,parcod, loc   , tim   ,misval,
     *                   i3gl  ,maxdim ,xdata , ierror, option,zbuffs)
c-----------------------------------------------------------------------
c           Function: select history data out of
c                     TRISULA NEFIS files
c        Method used:
c
c-----------------------------------------------------------------------
c   Calling routine :              GETMAT
c-----------------------------------------------------------------------
c   Called  routines:              OPNNEF
c                                  CLOSFL
c                                  GETELT (nefis)
c                                  M3HWAT
c                                  M3HCDA
c                                  M3HCUR
c                                  M3HCRS
c                                  M3H2D
c                                  M3H15
c                                  M3H20
c                                  M3H29
c                                  M3H31
c                                  M3H32
c                                  M3HFUV
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
c MAXDIM      I*4            I     length of data array
c XDATA       R*4   maxdim   O     array with the data
c IERROR      I*4            O     = 0 no errors, = 1 error detected
c OPTION     CH*256          O     option (not used)
c ZBUFFS      R*4   <len>    O/I   buffer for reading Nefis file:
c                                  if ( 1<=parcod<=32 .or.
c                                      38<=parcod<=43    ) then
c                                       <len> = nostat * lmaxd * kmax
c                                  else
c                                       <len> = ntruv  * lmaxd
c                                  endif
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
c FILHDA     CH*256                File name NEFIS data file for HIS
c FILHDE     CH*256                File name NEFIS definition file for
c                                  HIS
c FOUT        L*4                  Flag for further execution program
c                                  fout  = .true.  : stop execution
c                                  fout  = .false. : go on
c GRPDEF     CH*16                 Group name definition
c GRPDMS      I*4  1               Array with GRPNDM dimensions
c GRPNDM      I*4                  Number of dimenmsions of this group
c GRPORD      I*4  1               Array which gives order in which data
c                                  must be read
c HDAFDS      I*4  999             Data file descriptor for the HIS-DAT
c                                  file
c HDEFDS      I*4  2997            Definition file description for the
c IERROR      I*4                  Error code for NEFIS error
c ISTAT       I*4                  index of station
c KMAX        I*4                  Number of layers
c LMAX        I*4                  Total number of constituents
c                                  for old files LMAX = LSTCI
c                                  for new files LMAX = LSTCI + LTUR
c LSTCI       I*4                  Total number of constituents (incl.
c                                  turbulence for old trim files).
c LTUR        I*4                  Number of turbulence constituents
c LMAXD       I*4                  maximum(1,LMAX)
c LAY         I*4                  Actual layer number
c N           I*4                  Counter for XDATA
c NOSTAT      I*4                  Number of stations
c NTRUV       I*4                  Number of cross-sections
c NINDEX      I*4  3               indices of time frame in Nefis file
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
c-----------------------------------------------------------------------
c-----declaration Local variables
c-----------------------------------------------------------------------
      character*256   filhda,filhde
c
      integer         nindex (3)
      integer         istat ,lay   ,ind   ,lstci ,ltur
      integer         ierror,kmax  ,lmax  ,nostat,ntruv
c
      logical         ex    ,fout
c
c-----------------------------------------------------------------------
c-----declaration NEFIS
c-----------------------------------------------------------------------
      integer       hdefds( 2997),hdafds(  999)
      integer       uindex(    3),usrord,buflen
      character*16  grpdef,elmnam
      integer       GETELT,GETELS
c-----------------------------------------------------------------------
c-----Initialisation
c-----------------------------------------------------------------------
c
      ierror =  0
      fout   = .false.
c--------------------------------------------------------------------
c-----Test if trih-<runid>.dat and trih-<runid>.def Nefis HIS-files
c     exist
c--------------------------------------------------------------------
      call ods_check_nefis( fname  , '.def' , ierror )
      if ( ierror .ne. ieok   ) then
         return
      endif
c--------------------------------------------------------------------
c-----Open trih-<runid>.dat and trih-<runid>.def HIS-files
c--------------------------------------------------------------------
      call OPNNEF(fname, itype, hdafds, hdefds, ierror)
      if (ierror .ne. 0) then
         ierror = IEFIRO
         return
      endif
c-----------------------------------------------------------------------
c-----Get array dimensions form NEFIS-HIS files group 2
c-----------------------------------------------------------------------
      grpdef = 'his-const'
c
      uindex(1) = 1
      uindex(2) = 1
      uindex(3) = 1
      usrord    = 1
      buflen    = 4
c
      nostat    = 0
      ntruv     = 0
      kmax      = 0
      lmax      = 0
      lstci     = 0
      ltur      = 0
c
      elmnam    = 'NOSTAT'
      ierror    = GETELT(hdefds,grpdef    ,elmnam    ,
     *                   uindex,usrord    ,buflen    ,NOSTAT    )
      if (ierror .ne. 0) then
         fout   = .true.
         goto 8888
      endif
c
      elmnam    = 'NTRUV'
      ierror    = GETELT(hdefds,grpdef    ,elmnam    ,
     *                   uindex,usrord    ,buflen    ,NTRUV     )
      if (ierror .ne. 0) then
         fout   = .true.
         goto 8888
      endif
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
            fout   = .true.
            goto 8888
         endif

         elmnam = 'LTUR'
         ierror = GETELT(hdefds,grpdef    ,elmnam    ,
     *                   uindex,usrord    ,buflen    ,LTUR      )
         if (ierror .ne. 0) then
            fout   = .true.
            goto 8888
         endif
      endif
c
      if (nostat .gt. 0) then
         elmnam = 'KMAX'
         ierror = GETELT(hdefds,grpdef    ,elmnam    ,
     *                   uindex,usrord    ,buflen    ,KMAX      )
         if (ierror .ne. 0) then
            fout   = .true.
            goto 8888
         endif
      endif
c
      lmax = lstci + ltur
c
c-----------------------------------------------------------------------
c-----calculate indices for time frame
c-----------------------------------------------------------------------
      call julind (hdefds, hdafds, tim,   nindex, ierror)
      if (ierror .ne. 0) then
         fout  = .true.
         goto 8888
      endif
c-----------------------------------------------------------------------
c-----set loc (station) {precon: loc(1,1)=loc(2,1) and loc(3,1)=1}
c     plottable timeseries presumed
c     HACK:
c     insure a reasonable value for index
c     Note:
c     Thanks to the location codes, the index for the horizontal
c     direction does not require a correction. This is not true
c     for the index in the vertical. So, this is corrected by 1.
c     (Blame it on somebody!)
c-----------------------------------------------------------------------
      istat  = max( 1, loc(1,1) )
c--------------------------------------------------------------------
c-----set loc (layer  ) {precon: loc(1,3)=loc(2,3) and loc(3,3)=1}
c     plottable timeseries presumed
c--------------------------------------------------------------------
      lay    = 1 + max( 0, loc(1,3) )
c--------------------------------------------------------------------
c--------------------------------------------------------------------
c-----get data
c--------------------------------------------------------------------
      if (parcod .eq. 1 ) then
c--------water level, ZWL
         call m3hwat (hdefds, hdafds, nindex, maxdim, istat , nostat,
     *                                parcod, xdata , ierror, zbuffs)
      else if (parcod .eq.  2 ) then
c--------water depth, ZWL + DPS
         call m3hwat (hdefds, hdafds, nindex, maxdim, istat , nostat,
     *                                parcod, xdata , ierror, zbuffs)
      else if (parcod .ge.  3 .and. parcod .le. 6 ) then
c--------dpt. aver. cur. u         {precon: parcod= 3}
c--------dpt. aver. cur. v         {precon: parcod= 4}
c--------dpt. aver. cur. mag.      {precon: parcod= 5}
c--------dpt. aver. cur. dir.      {precon: parcod= 6}
         call m3hcda (hdefds, hdafds, nindex, maxdim, istat , nostat,
     *                kmax  ,         parcod, xdata , ierror, zbuffs)
      else if (parcod .ge.  7 .and. parcod .le. 10) then
c--------current u (layer)         {precon: parcod= 7}
c--------current v (layer)         {precon: parcod= 8}
c--------current mag. (layer)      {precon: parcod= 9}
c--------current dir. (layer)      {precon: parcod=10}
         call m3hcur (hdefds, hdafds, nindex, maxdim, istat , nostat,
     *                kmax  , lay   , parcod, xdata , ierror, zbuffs)
      else if (parcod .eq. 11 ) then
c--------current w.   (layer)
         call m3h2d  (hdefds, hdafds, nindex, maxdim, istat , nostat,
     *                kmax  , lay   ,'ZCURW ',xdata , ierror, zbuffs)
      else if (parcod .eq. 12 ) then
c--------flow rate u, ZQXK
         call m3h2d  (hdefds, hdafds, nindex, maxdim, istat , nostat,
     *                kmax  , lay   ,'ZQXK  ',xdata , ierror, zbuffs)
      else if (parcod .eq. 13 ) then
c--------flow rate v, ZQYK
         call m3h2d  (hdefds, hdafds, nindex, maxdim, istat , nostat,
     *                kmax  , lay   ,'ZQYK  ',xdata , ierror, zbuffs)
      else if (parcod .eq. 15 ) then
c--------viscosity, ZVICWW
         call m3h15  (hdefds, hdafds, nindex, maxdim, istat , nostat,
     *                kmax  , lay   ,         xdata , ierror, zbuffs)
      else if (parcod .ge. 16 .and. parcod .le.19 ) then
c--------bottomstress u, ZTAUKSI   {precon: parcod=16}
c--------bottomstress v, ZTAUETA   {precon: parcod=17}
c--------bottomstress mag, ZTAUETA {precon: parcod=18}
c--------bottomstress dir, ZTAUETA {precon: parcod=19}
         call m3hwat (hdefds, hdafds, nindex, maxdim, istat , nostat,
     *                                parcod, xdata , ierror, zbuffs)
      else if (parcod .ge. 20 .and. parcod .le.29 ) then
         if (parcod-19 .le. lstci) then
c-----------constituents, GRO (1:lstci) {precon: <con.index>=parcod-19}
           call m3h20(hdefds, hdafds, nindex, maxdim, istat , nostat,
     *                kmax  , lay   , lmax  , parcod, xdata , ierror,
     *                zbuffs)
         else
c-----------constituents, ZTUR(1:ltur )
c                        {precon: <con.index>=parcod-19-lstci}
           zturid = parcod - 19 - lstci
           call m3h29(hdefds, hdafds, nindex, maxdim, istat , nostat,
     *                kmax  , lay   , ltur  , zturid, xdata , ierror,
     *                zbuffs)
         endif
      else if (parcod .eq. 31 ) then
c--------density, ZRHO
         call m3h31  (hdefds, hdafds, nindex, maxdim, istat , nostat,
     *                kmax  , lay   ,         xdata , ierror, zbuffs)
      else if (parcod .eq. 32 ) then
c--------diffusivity, ZDICWW
         call m3h32  (hdefds, hdafds, nindex, maxdim, istat , nostat,
     *                kmax  , lay   ,         xdata , ierror, zbuffs)
      else if (parcod .eq. 33 ) then
c--------accumulated flow,CTR
         call m3hcrs (hdefds, hdafds, nindex, maxdim, istat , ntruv ,
     *                        1     , lstci , parcod, xdata , ierror,
     *                zbuffs)
      else if (parcod .eq. 34 ) then
c--------momentary flow , FLTR
         call m3hcrs (hdefds, hdafds, nindex, maxdim, istat , ntruv ,
     *                        1     , lstci , parcod, xdata , ierror,
     *                zbuffs)
      else if (parcod .eq. 42 ) then
c--------accumulated flow u
         call m3hfuv (hdefds, hdafds, nindex, maxdim, istat , nostat,
     *                kmax  , lay   ,'ZQXK  ',xdata , ierror, zbuffs)
      else if (parcod .eq. 43 ) then
c--------accumulated flow v
         call m3hfuv (hdefds, hdafds, nindex, maxdim, istat , nostat,
     *                kmax  , lay   ,'ZQYK  ',xdata , ierror, zbuffs)
      else if (parcod .ge. 47 .and. parcod .le. 55 ) then
c--------advective flux, ATR(1:lstci)
         call m3hcrs (hdefds, hdafds, nindex, maxdim, istat , ntruv ,
     *                     parcod-46, lstci , parcod, xdata , ierror,
     *                zbuffs)
      else if (parcod .ge. 57 .and. parcod .le. 65 ) then
c--------dispersive flux, DTR(1:lstci)
         call m3hcrs (hdefds, hdafds, nindex, maxdim, istat , ntruv ,
     *                     parcod-56, lstci , parcod, xdata , ierror,
     *                zbuffs)
      else if (parcod .ge. 67 .and. parcod .le. 75 ) then
c--------total flux, (ATR + DTR)(1:lstci)
         call m3hcrs (hdefds, hdafds, nindex, maxdim, istat , ntruv ,
     *                     parcod-66, lstci , parcod, xdata , ierror,
     *                zbuffs)
      endif
c
      if (ierror .ne. 0) then
         fout  = .true.
      endif
c--------------------------------------------------------------------
c-----Close trih-<runid>.dat and trih-<runid>.def HIS-files
c--------------------------------------------------------------------
 8888 continue
      call CLOSFL(fname, ierror)
      if (ierror .ne. 0) then
         fout  = .true.
      endif
c-----------------------------------------------------------------------
c-----return status to calling routine
c-----------------------------------------------------------------------
      ierror = IEOK
      if (fout) then
         ierror = IEOTHR
      endif
c
      return
c-----------------------------------------------------------------------
      end

      subroutine m3h2d  (hdefds, hdafds, nindex, maxdim, istat , nostat,
     *                   kmax  , lay   , pardef, xdata , ierror, zbuffs)
c-----------------------------------------------------------------------
c           Function: select from TRISULA NEFIS-HIS:
c                     pardef
c
c        Method used:
c
c-----------------------------------------------------------------------
c   Calling routine :              HISMAT
c-----------------------------------------------------------------------
c   Called  routines:              GETELT (nefis)
c-----------------------------------------------------------------------
c    Parameters:
c    -----------
c
c   Var.      Type Dimensions
c   -------------------------
c
c HDAFDS      I*4  999       I     Data file descriptor for the HIS-DAT
c                                  file
c HDEFDS      I*4  2997      I     Definition file description for the
c NINDEX      I*4  3         I     indices of time frame in Nefis file
c MAXDIM      I*4            I     length of data array
c ISTAT       I*4            I     Selected station number
c NOSTAT      I*4            I     Number of stations
c KMAX        I*4            I     Number of layers
c LAY         I*4            I     Selected layer number
c PARDEF      CH*6           I     Selected parameter name
c XDATA       R*4   maxdim   O     array with the data
c IERROR      I*4            O     error indicator
c ZBUFFS      R*4   nostat   I/O   buffer for reading Nefis file
c-----------------------------------------------------------------------
c    Local variables:
c    ----------------
c
c   Var.      Type Dimensions
c   -------------------------
c
c BUFLEN      I*4                  Size in bytes of available buffer
c GRPDEF     CH*16                 Group name definition
c I           I*4                  Help variable, loop index
c N           I*4                  Counter for data array
c UINDEX      I*4  3               Indices of the cells to be read
c USRORD      I*4                  Readsequence for the cells
c-----------------------------------------------------------------------
c
c  DECLARATIONS
c
      include         'ods.inc'
c
      integer         hdefds( 2997),hdafds(  999)
      integer         uindex(    3),usrord,buflen
      character*16    grpdef
      integer         GETELT,GETELS
c
      integer         istat ,nostat,kmax  ,lay   ,ierror,n     ,i
      integer         maxdim
      integer         nindex (3)
      real            zbuffs (nostat,kmax)
      real            xdata  (maxdim)
      character*6     pardef
c--------------------------------------------------------------------
c
      grpdef    = 'his-series'
      buflen    =  4 * nostat * kmax
      ierror    = 0
      usrord    = 1
      n         = 0

      do 100 i=nindex(1),nindex(2),nindex(3)
c--------------------------------------------------------------------
c--------Initialize uindex
c--------------------------------------------------------------------
         uindex(1) = i
         uindex(2) = i
         uindex(3) = 1
c--------------------------------------------------------------------
c--------Read from group 3 PARDEF
c--------------------------------------------------------------------
         ierror    = GETELT(hdefds,grpdef    ,PARDEF    ,
     *                      uindex,usrord ,buflen    ,ZBUFFS    )
         if (ierror .ne. 0) then
            ierror = IEOTHR
            goto 8888
         endif
c
         n        = n + 1
         xdata(n) = zbuffs(istat ,lay )
  100 continue
c-----------------------------------------------------------------------
c-----return status to calling routine
c-----------------------------------------------------------------------
 8888 return
      end

      subroutine m3h15  (hdefds, hdafds, nindex, maxdim, istat , nostat,
     *                   kmax  , lay   ,         xdata , ierror, zbuffs)
c-----------------------------------------------------------------------
c           Function: select from TRISULA NEFIS-HIS:
c                     15 viscosity, ZVICWW
c
c        Method used:
c
c-----------------------------------------------------------------------
c   Calling routine :              HISMAT
c-----------------------------------------------------------------------
c   Called  routines:              GETELT (nefis)
c-----------------------------------------------------------------------
c    Parameters:
c    -----------
c
c   Var.      Type Dimensions
c   -------------------------
c
c HDAFDS      I*4  999       I     Data file descriptor for the HIS-DAT
c                                  file
c HDEFDS      I*4  2997      I     Definition file description for the
c NINDEX      I*4  3         I     indices of time frame in Nefis file
c MAXDIM      I*4            I     length of data array
c ISTAT       I*4            I     Selected station number
c NOSTAT      I*4            I     Number of stations
c KMAX        I*4            I     Number of layers
c LAY         I*4            I     Selected layer number
c XDATA       R*4   maxdim   O     array with the data
c IERROR      I*4            O     error indicator
c ZBUFFS      R*4   nostat   I/O   buffer for reading Nefis file
c-----------------------------------------------------------------------
c    Local variables:
c    ----------------
c
c   Var.      Type Dimensions
c   -------------------------
c
c BUFLEN      I*4                  Size in bytes of available buffer
c EPS         R*4                  small value
c GRPDEF     CH*16                 Group name definition
c I           I*4                  Help variable, loop index
c IERROR      I*4                  Help variable, error indicator
c N           I*4                  Counter for data array
c UINDEX      I*4  3               Array with indices of the cells to
c                                  be read
c USRORD      I*4                  Sequence in which the cells must be
c                                  read
c-----------------------------------------------------------------------
c
c  DECLARATIONS
c
      include       'ods.inc'
c
c-----------------------------------------------------------------------
c-----declaration NEFIS
c-----------------------------------------------------------------------
      integer       hdefds( 2997),hdafds(  999)
c
      integer       uindex(    3),usrord,buflen       ,
     *              nbytsg,elmndm,elmdms(     5)
c
      character*8   elmtyp

      character*16  grpdef,elmnam,elmqty,elmunt

      character*64  elmdes
c
      integer       GETELT,GETELS,INQELM
c-----------------------------------------------------------------------
c
      integer         istat ,nostat,kmax  ,lay   ,ierror,n     ,i
      integer         maxdim,kmaxon
      integer         nindex (3)
      real            zbuffs (nostat,kmax+1)
      real            xdata  (maxdim)
c--------------------------------------------------------------------
c
      elmnam    = 'ZVICWW'
      elmndm    = 5
      ierror    = INQELM(hdefds,elmnam ,elmtyp ,nbytsg ,elmqty ,
     *                   elmunt,elmdes ,elmndm ,elmdms         )
      if (elmdms(elmndm) .eq. kmax) then
c--------version 2.03
         kmaxon    = 0
      else
c--------version 2.45 and later
         kmaxon    = 1
      endif
c
      grpdef    = 'his-series'
      buflen    =  4 * nostat * (kmax + kmaxon)
      ierror    = 0
      usrord    = 1
      n         = 0

      do 100 i=nindex(1),nindex(2),nindex(3)
c--------------------------------------------------------------------
c--------Initialize uindex
c--------------------------------------------------------------------
         uindex(1) = i
         uindex(2) = i
         uindex(3) = 1
c--------------------------------------------------------------------
c--------Read from group 3 ZVICWW
c--------------------------------------------------------------------
         ierror    = GETELT(hdefds,grpdef    ,'ZVICWW'  ,
     *                      uindex,usrord ,buflen    ,ZBUFFS    )
         if (ierror .ne. 0) then
            ierror = IEOTHR
            goto 8888
         endif

         n        = n + 1
         xdata(n) = zbuffs(istat ,lay + kmaxon)
c
  100 continue

c-----------------------------------------------------------------------
c-----return status to calling routine
c-----------------------------------------------------------------------
 8888 return
      end

      subroutine m3h20  (hdefds, hdafds, nindex, maxdim, istat , nostat,
     *                   kmax  , lay   , lmax  , parcod, xdata , ierror,
     *                   zbuffs)
c-----------------------------------------------------------------------
c           Function: select from TRISULA NEFIS-HIS:
c                     20 constituent ,<parcod-19>
c
c        Method used:
c
c-----------------------------------------------------------------------
c   Calling routine :              HISMAT
c-----------------------------------------------------------------------
c   Called  routines:              GETELT (nefis)
c-----------------------------------------------------------------------
c    Parameters:
c    -----------
c
c   Var.      Type Dimensions
c   -------------------------
c
c HDAFDS      I*4  999       I     Data file descriptor for the HIS-DAT
c                                  file
c HDEFDS      I*4  2997      I     Definition file description for the
c NINDEX      I*4  3         I     indices of time frame in Nefis file
c MAXDIM      I*4            I     length of data array
c ISTAT       I*4            I     Selected station number
c NOSTAT      I*4            I     Number of stations
c KMAX        I*4            I     Number of layers
c LAY         I*4            I     Selected layer number
c LMAX        I*4            I     Number of constituents
c PARCOD      I*4            I     parameter to get data of
c XDATA       R*4   maxdim   O     array with the data
c IERROR      I*4            O     error indicator
c ZBUFFS      R*4   nostat   I/O   buffer for reading Nefis file
c-----------------------------------------------------------------------
c    Local variables:
c    ----------------
c
c   Var.      Type Dimensions
c   -------------------------
c
c BUFLEN      I*4                  Size in bytes of available buffer
c GRPDEF     CH*16                 Group name definition
c I           I*4                  Help variable, loop index
c N           I*4                  Counter for data array
c UINDEX      I*4  3               Array with indices of the cells to
c                                  be read
c USRORD      I*4                  Sequence in which the cells must be
c                                  read
c-----------------------------------------------------------------------
c
c  DECLARATIONS
c
      include       'ods.inc'
c
      integer       hdefds( 2997),hdafds(  999)
      integer       uindex(    3),usrord,buflen
      character*16  grpdef
      integer       GETELT,GETELS
c
      integer         maxdim
      integer         istat ,nostat,kmax  ,lay   ,ierror,n     ,i
      integer         lmax
      integer         parcod
      integer         nindex (3)
c
      real            zbuffs (nostat,kmax,lmax)
      real            xdata  (maxdim)
c--------------------------------------------------------------------
c
      ierror    = 0
      grpdef    = 'his-series'
      buflen    =  4 * nostat * kmax * lmax
      usrord    = 1
      n         = 0

      do 100 i=nindex(1),nindex(2),nindex(3)
c--------------------------------------------------------------------
c--------Initialize uindex
c--------------------------------------------------------------------
         uindex(1) = i
         uindex(2) = i
         uindex(3) = 1
c--------------------------------------------------------------------
c--------Read from group 20 GRO
c--------------------------------------------------------------------
         ierror    = GETELT(hdefds,grpdef    ,'GRO'     ,
     *                      uindex,usrord ,buflen    ,ZBUFFS    )
         if (ierror .ne. 0) then
            ierror = IEOTHR
            goto 8888
         endif

         n        = n + 1
         xdata(n) = zbuffs(istat,lay,parcod-19)
c
  100 continue

c-----------------------------------------------------------------------
c-----return status to calling routine
c-----------------------------------------------------------------------
 8888 return
      end

      subroutine m3h29  (hdefds, hdafds, nindex, maxdim, istat , nostat,
     *                   kmax  , lay   , ltur  , zturid, xdata , ierror,
     *                   zbuffs)
c-----------------------------------------------------------------------
c           Function: select from TRISULA NEFIS-HIS:
c                     20 turbulence constituent ,<zturid>
c
c        Method used:
c
c-----------------------------------------------------------------------
c   Calling routine :              HISMAT
c-----------------------------------------------------------------------
c   Called  routines:              GETELT (nefis)
c-----------------------------------------------------------------------
c    Parameters:
c    -----------
c
c   Var.      Type Dimensions
c   -------------------------
c
c HDAFDS      I*4  999       I     Data file descriptor for the HIS-DAT
c                                  file
c HDEFDS      I*4  2997      I     Definition file description for the
c NINDEX      I*4  3         I     indices of time frame in Nefis file
c MAXDIM      I*4            I     length of data array
c ISTAT       I*4            I     Selected station number
c NOSTAT      I*4            I     Number of stations
c KMAX        I*4            I     Number of layers
c LAY         I*4            I     Selected layer number
c LTUR        I*4            I     Number of turbulence constituents
c ZTURID      I*4            I     parameter to get data of
c XDATA       R*4   maxdim   O     array with the data
c IERROR      I*4            O     error indicator
c ZBUFFS      R*4   nostat   I/O   buffer for reading Nefis file
c-----------------------------------------------------------------------
c    Local variables:
c    ----------------
c
c   Var.      Type Dimensions
c   -------------------------
c
c BUFLEN      I*4                  Size in bytes of available buffer
c GRPDEF     CH*16                 Group name definition
c I           I*4                  Help variable, loop index
c N           I*4                  Counter for data array
c UINDEX      I*4  3               Array with indices of the cells to
c                                  be read
c USRORD      I*4                  Sequence in which the cells must be
c                                  read
c-----------------------------------------------------------------------
c
c  DECLARATIONS
c
      include       'ods.inc'
c
      integer       hdefds( 2997),hdafds(  999)
      integer       uindex(    3),usrord,buflen
      character*16  grpdef
      integer       GETELT,GETELS
c
      integer         maxdim
      integer         istat ,nostat,kmax  ,lay   ,ierror,n     ,i
      integer         ltur
      integer         zturid
      integer         nindex (3)
c
      real            zbuffs (nostat,kmax+1,ltur)
      real            xdata  (maxdim)
c--------------------------------------------------------------------
c
      ierror    = 0
      grpdef    = 'his-series'
      buflen    =  4 * nostat * (kmax+1) * ltur
      usrord    = 1
      n         = 0

      do 100 i=nindex(1),nindex(2),nindex(3)
c--------------------------------------------------------------------
c--------Initialize uindex
c--------------------------------------------------------------------
         uindex(1) = i
         uindex(2) = i
         uindex(3) = 1
c--------------------------------------------------------------------
c--------Read from group 3 ZTUR
c--------------------------------------------------------------------
         ierror    = GETELT(hdefds,grpdef    ,'ZTUR'    ,
     *                      uindex,usrord ,buflen    ,ZBUFFS    )
         if (ierror .ne. 0) then
            ierror = IEOTHR
            goto 8888
         endif

         n        = n + 1
         xdata(n) = zbuffs(istat,lay+1,zturid)
c
  100 continue

c-----------------------------------------------------------------------
c-----return status to calling routine
c-----------------------------------------------------------------------
 8888 return
      end

      subroutine m3h31  (hdefds, hdafds, nindex, maxdim, istat , nostat,
     *                   kmax  , lay   ,         xdata , ierror, zbuffs)
c-----------------------------------------------------------------------
c           Function: select from TRISULA NEFIS-HIS:
c                     31 density, ZRHO
c
c        Method used:
c
c-----------------------------------------------------------------------
c   Calling routine :              HISMAT
c-----------------------------------------------------------------------
c   Called  routines:              GETELT (nefis)
c-----------------------------------------------------------------------
c    Parameters:
c    -----------
c
c   Var.      Type Dimensions
c   -------------------------
c
c HDAFDS      I*4  999       I     Data file descriptor for the HIS-DAT
c                                  file
c HDEFDS      I*4  2997      I     Definition file description for the
c NINDEX      I*4  3         I     indices of time frame in Nefis file
c MAXDIM      I*4            I     length of data array
c ISTAT       I*4            I     Selected station number
c NOSTAT      I*4            I     Number of stations
c KMAX        I*4            I     Number of layers
c LAY         I*4            I     Selected layer number
c XDATA       R*4   maxdim   O     array with the data
c IERROR      I*4            O     error indicator
c ZBUFFS      R*4   nostat   I/O   buffer for reading Nefis file
c                   *kmax
c-----------------------------------------------------------------------
c    Local variables:
c    ----------------
c
c   Var.      Type Dimensions
c   -------------------------
c
c BUFLEN      I*4                  Size in bytes of available buffer
c GRPDEF     CH*16                 Group name definition
c I           I*4                  Help variable, loop index
c N           I*4                  Counter for data array
c UINDEX      I*4  3               Array with indices of the cells to
c                                  be read
c USRORD      I*4                  Sequence in which the cells must be
c                                  read
c-----------------------------------------------------------------------
c
c  DECLARATIONS
c
      include       'ods.inc'
c
      integer       hdefds( 2997),hdafds(  999)
      integer       uindex(    3),usrord,buflen
      character*16  grpdef
      integer       GETELT,GETELS
c
      integer         maxdim
      integer         istat ,nostat,kmax  ,lay   ,ierror,n     ,i
      integer         nindex (3)
      real            zbuffs (nostat,kmax)
      real            xdata  (maxdim)
c--------------------------------------------------------------------
c
      ierror    = 0
      grpdef    = 'his-series'
      buflen    =  4 * nostat * kmax
      usrord    = 1
      n         = 0

      do 100 i=nindex(1),nindex(2),nindex(3)
c--------------------------------------------------------------------
c--------Initialize uindex
c--------------------------------------------------------------------
         uindex(1) = i
         uindex(2) = i
         uindex(3) = 1
c--------------------------------------------------------------------
c--------Read from group 31 ZRHO
c--------------------------------------------------------------------
         ierror    = GETELT(hdefds,grpdef    ,'ZRHO'    ,
     *                      uindex,usrord ,buflen    ,ZBUFFS    )
         if (ierror .ne. 0) then
            ierror = IEOTHR
            goto 8888
         endif

         n        = n + 1
         xdata(n) = zbuffs(istat,lay)
c
  100 continue

c-----------------------------------------------------------------------
c-----return status to calling routine
c-----------------------------------------------------------------------
 8888 return
      end

      subroutine m3h32  (hdefds, hdafds, nindex, maxdim, istat , nostat,
     *                   kmax  , lay   ,         xdata , ierror, zbuffs)
c-----------------------------------------------------------------------
c           Function: select from TRISULA NEFIS-HIS:
c                     32 diffusivity, ZDICWW
c
c        Method used:
c
c-----------------------------------------------------------------------
c   Calling routine :              HISMAT
c-----------------------------------------------------------------------
c   Called  routines:              GETELT (nefis)
c-----------------------------------------------------------------------
c    Parameters:
c    -----------
c
c   Var.      Type Dimensions
c   -------------------------
c
c HDAFDS      I*4  999       I     Data file descriptor for the HIS-DAT
c                                  file
c HDEFDS      I*4  2997      I     Definition file description for the
c NINDEX      I*4  3         I     indices of time frame in Nefis file
c MAXDIM      I*4            I     length of data array
c ISTAT       I*4            I     Selected station number
c NOSTAT      I*4            I     Number of stations
c KMAX        I*4            I     Number of layers
c LAY         I*4            I     Selected layer number
c XDATA       R*4   maxdim   O     array with the data
c IERROR      I*4            O     error indicator
c ZBUFFS      R*4   nostat   I/O   buffer for reading Nefis file
c-----------------------------------------------------------------------
c    Local variables:
c    ----------------
c
c   Var.      Type Dimensions
c   -------------------------
c
c BUFLEN      I*4                  Size in bytes of available buffer
c EPS         R*4                  small value
c GRPDEF     CH*16                 Group name definition
c I           I*4                  Help variable, loop index
c IERROR      I*4                  Help variable, error indicator
c N           I*4                  Counter for data array
c UINDEX      I*4  3               Array with indices of the cells to
c                                  be read
c USRORD      I*4                  Sequence in which the cells must be
c                                  read
c-----------------------------------------------------------------------
c
c  DECLARATIONS
c
      include       'ods.inc'
c
c-----------------------------------------------------------------------
c-----declaration NEFIS
c-----------------------------------------------------------------------
      integer       hdefds( 2997),hdafds(  999)
c
      integer       uindex(    3),usrord,buflen       ,
     *              nbytsg,elmndm,elmdms(     5)
c
      character*8   elmtyp

      character*16  grpdef,elmnam,elmqty,elmunt

      character*64  elmdes
c
      integer       GETELT,GETELS,INQELM
c-----------------------------------------------------------------------
c
      integer         istat ,nostat,kmax  ,lay   ,ierror,n     ,i
      integer         maxdim,kmaxon
      integer         nindex (3)
      real            zbuffs (nostat,kmax+1)
      real            xdata  (maxdim)
c--------------------------------------------------------------------
c
      elmnam    = 'ZDICWW'
      elmndm    = 5
      ierror    = INQELM(hdefds,elmnam ,elmtyp ,nbytsg ,elmqty ,
     *                   elmunt,elmdes ,elmndm ,elmdms         )
      if (ierror .ne. 0) then
c--------version 2.45 and later
         kmaxon    = 1
      else if (elmdms(elmndm) .eq. kmax) then
c--------version 2.03
         kmaxon    = 0
      else
c--------version 2.45 and later
         kmaxon    = 1
      endif
c
      grpdef    = 'his-series'
      buflen    =  4 * nostat * (kmax + kmaxon)
      ierror    = 0
      usrord    = 1
      n         = 0

      do 100 i=nindex(1),nindex(2),nindex(3)
c--------------------------------------------------------------------
c--------Initialize uindex
c--------------------------------------------------------------------
         uindex(1) = i
         uindex(2) = i
         uindex(3) = 1
c--------------------------------------------------------------------
c--------Read from group 3 ZDICWW
c--------------------------------------------------------------------
         ierror    = GETELT(hdefds,grpdef    ,'ZDICWW'  ,
     *                      uindex,usrord ,buflen    ,ZBUFFS    )
         if (ierror .ne. 0) then
            ierror = IEOTHR
            goto 8888
         endif

         n        = n + 1
         xdata(n) = zbuffs(istat ,lay + kmaxon)
c
  100 continue

c-----------------------------------------------------------------------
c-----return status to calling routine
c-----------------------------------------------------------------------
 8888 return
      end

      subroutine m3hfuv (hdefds, hdafds, nindex, maxdim, istat , nostat,
     *                   kmax  , lay   , pardef, xdata , ierror, zbuffs)
c-----------------------------------------------------------------------
c           Function: select from TRISULA NEFIS-HIS:
c                     PARDEF
c
c        Method used:
c
c-----------------------------------------------------------------------
c   Calling routine :              HISMAT
c-----------------------------------------------------------------------
c   Called  routines:              GETELT (nefis)
c-----------------------------------------------------------------------
c    Parameters:
c    -----------
c
c   Var.      Type Dimensions
c   -------------------------
c
c HDAFDS      I*4  999       I     Data file descriptor for the HIS-DAT
c                                  file
c HDEFDS      I*4  2997      I     Definition file description for the
c NINDEX      I*4  3         I     indices of time frame in Nefis file
c MAXDIM      I*4            I     length of data array
c ISTAT       I*4            I     Selected station number
c NOSTAT      I*4            I     Number of stations
c KMAX        I*4            I     Number of layers
c LAY         I*4            I     Selected layer number
c PARDEF      CH*6           I     Selected element name
c XDATA       R*4   maxdim   O     array with the data
c IERROR      I*4            O     error indicator
c ZBUFFS      R*4   nostat   I/O   buffer for reading Nefis file
c-----------------------------------------------------------------------
c    Local variables:
c    ----------------
c
c   Var.      Type Dimensions
c   -------------------------
c
c BUFLEN      I*4                  Size in bytes of available buffer
c CELNAM     CH*16                 Celname definition
c DT          R*4                  Time step interval
c DTHIS       R*4                  Time step interval for history file
c OKEE        L*4                  Flag for NEFIS file readings
c GRPDEF     CH*16                 Group name definition
c I           I*4                  Help variable, loop index
c IT1         I*4                  Help variable, first history step
c IT2         I*4                  Help variable, second history step
c N           I*4                  Counter for data array
c UINDEX      I*4  3               Array with indices of the cells to
c                                  be read
c USRORD      I*4                  Sequence in which the cells must be
c                                  read
c ZVXK        R*4                  help variable; accumulated flow u
c-----------------------------------------------------------------------
c
c  DECLARATIONS
c
      include         'ods.inc'
c
      integer         grpndm,grpdms(    5),grpord(    5)
      integer         hdefds( 2997),hdafds(  999)
      integer         uindex(    3),usrord,buflen
      character*16    grpdef,celnam
      character*6     pardef
      integer         GETELT,GETELS,INQGRP,INQMXI
c
      integer         istat ,nostat,ierror,n     ,i     ,kmax  ,lay
      integer         nrcel ,it1   ,it2
      integer         maxdim
      integer         nindex (3)
c
      real            zv
      real            dt    ,dthis ,tunit
c
      real            xdata  (maxdim)
      real            zbuffs (nostat,kmax)
c
      logical         okee

c
c-----Read array-dimension nrcel from Nefis HIS-files group 1
      grpdef = 'his-info-series'
      grpndm = 5
      celnam = grpdef
      okee   =
     *         INQGRP(hdefds   ,grpdef    ,celnam    ,grpndm    ,
     *                grpdms   ,grpord                          )
     *         .eq. 0
      nrcel  = grpdms(1)
c
c-----Test value of nrcel if nrcel = 0 then get nrcel with INQMXI
      if (nrcel  .eq. 0) then
         okee      = okee .and.
     *               INQMXI(hdefds,grpdef    ,nrcel     )
     *               .eq. 0
      endif

      buflen    = 4
      uindex(1) = 1
      uindex(2) = 1
      uindex(3) = 1
      usrord    = 1
      it1       = 0
      it2       = 0
c
c-----Get values for IT1 and IT2 from group 1
      okee      = okee .and.
     *            GETELT(hdefds,grpdef    ,'ITHISC'  ,
     *                   uindex,usrord    ,buflen    ,IT1       )
     *            .eq. 0
      if (nrcel  .gt. 1) then
         uindex(1) = 2
         uindex(2) = 2
         okee      = okee .and.
     *            GETELT(hdefds,grpdef    ,'ITHISC'  ,
     *                   uindex,usrord    ,buflen    ,IT2       )
     *            .eq. 0
      else
         it2 = it1 + 1
      endif

      grpdef    = 'his-const'
      uindex(1) = 1
      uindex(2) = 1
      uindex(3) = 1
      buflen    = 4
      okee      =
     *            GETELT(hdefds,grpdef    ,'TUNIT'   ,
     *                   uindex,usrord    ,buflen    ,TUNIT     )
     *            .eq. 0

      okee      = okee .and.
     *            GETELT(hdefds,grpdef    ,'DT'      ,
     *                   uindex,usrord    ,buflen    ,DT        )
     *            .eq. 0
c
c-----set history time interval
      dthis     = real( it2 - it1 ) * tunit * dt
c
      grpdef    = 'his-series'
      buflen    =  4 * nostat * kmax
      n         = 0
      zv        = 0.0

      do 100 i=nindex(1),nindex(2),nindex(3)
c--------------------------------------------------------------------
c--------Initialize uindex
c--------------------------------------------------------------------
         uindex(1) = i
         uindex(2) = i
         uindex(3) = 1
c--------------------------------------------------------------------
c--------Read from group 3 PARDEF
c--------------------------------------------------------------------
         okee      = okee .and.
     *               GETELT(hdefds,grpdef    ,PARDEF    ,
     *                      uindex,usrord    ,buflen    ,zbuffs    )
     *               .eq. 0

         n         = n + 1
         zv        = zv + dthis * zbuffs(istat ,lay )
         xdata(n)  = zv
  100 continue

c-----------------------------------------------------------------------
c-----return status to calling routine
c-----------------------------------------------------------------------
      ierror = IEOK
      if (.not. okee) then
         ierror = IEOTHR
      endif
c
      return
      end

      subroutine m3hcda (hdefds, hdafds, nindex, maxdim, istat , nostat,
     *                   kmax  ,         parcod, xdata , ierror, zbuffs)
c-----------------------------------------------------------------------
c           Function: select from TRISULA NEFIS-HIS:
c                      3 dpt. aver. cur. u
c                      4 dpt. aver. cur. v
c                      5 dpt. aver. cur. mag.
c                      6 dpt. aver. cur. dir.
c
c        Method used:
c
c-----------------------------------------------------------------------
c   Calling routine :              HISMAT
c-----------------------------------------------------------------------
c   Called  routines:              GETELT (nefis)
c-----------------------------------------------------------------------
c    Parameters:
c    -----------
c
c   Var.      Type Dimensions
c   -------------------------
c
c HDAFDS      I*4  999       I     Data file descriptor for the HIS-DAT
c                                  file
c HDEFDS      I*4  2997      I     Definition file description for the
c NINDEX      I*4  3         I     indices of time frame in Nefis file
c MAXDIM      I*4            I     length of data array
c ISTAT       I*4            I     Selected station number
c NOSTAT      I*4            I     Number of stations
c KMAX        I*4            I     Number of layers
c PARCOD      I*4            I     parameter to get data of
c XDATA       R*4   maxdim   O     array with the data
c IERROR      I*4            O     error indicator
c ZBUFFS      R*4   nostat   I/O   buffer for reading Nefis file
c-----------------------------------------------------------------------
c    Local variables:
c    ----------------
c
c   Var.      Type Dimensions
c   -------------------------
c
c BUFLEN      I*4                  Size in bytes of available buffer
c DPS         R*4                  Depth at station location
c ELM        CH*16                 Element name definition
c ELMNAM     CH*16 3               Element name definition
c EPS         R*4                  small value
c OKEE        L*4                  Flag for NEFIS file readings
c GRDANG      R*4                  Vertex between y-axis and true North
c GRPDEF     CH*16                 Group name definition
c GUUGEM      R*4                  Mean value for distance coefficients
c GVVGEM      R*4                  Mean value for distance coefficients
c HULP        R*4                  Help variable
c I           I*4                  Help variable, loop index
c N           I*4                  Counter for data array
c PI          R*4                  Constant pi
c U           R*4                  bottom stress u-direction
c UINDEX      I*4  3               Array with indices of the cells to
c                                  be read
c USRORD      I*4                  Sequence in which the cells must be
c                                  read
c V           R*4                  bottom stress v-direction
c XETA        R*4                  X-distance between 2 grid lines
c                                  around zeta point in y-direction
c XKSI        R*4                  X-distance between 2 grid lines
c                                  around zeta point in x-direction
c YETA        R*4                  Y-distance between 2 grid lines
c                                  around zeta point in y-direction
c YKSI        R*4                  Y-distance between 2 grid lines
c                                  around zeta point in x-direction
c ZTAUD       R*4                  bottom stress dir.
c ZTAUET      R*4                  bottom stress y-direction
c ZTAVKS      R*4                  bottom stress x-direction
c ZTAUM       R*4                  bottom stress mag.
c ZTAUT       R*4                  bottom stress u-direction at station
c ZTAVT       R*4                  bottom stress v-direction at station
c ZWH         R*4                  total water depth
c ZWL         R*4                  water level
c-----------------------------------------------------------------------
c
c  DECLARATIONS
c
      include         'ods.inc'
c
      integer         hdefds( 2997),hdafds(  999)
      integer         uindex(    3),usrord,buflen
      character*16    grpdef
      integer         GETELT,GETELS
c
      integer         istat ,nostat,kmax  ,ierror,n     ,i     ,k
      integer         maxdim
      integer         parcod
      integer         nindex (3)
c
      real            pi    ,eps   ,zcuru ,zcurv ,zcurdu,zcurdv
      real            zcurdm,hulp  ,zcurdd
c
      real            zbuffs (nostat,kmax)
      real            xdata  (maxdim)
c
      logical         okee
c
      real            grdang
      real            alfas
c-----see TRISULA user Manual, release 2.03, version 0.1 May 1993
c     Appendix A: limitations 1<= zmax <= 42
      real            thick (42)
c

      pi        = atan (1.0) * 4.
      eps       = 1.e-12
      grpdef    = 'his-const'
      uindex(1) = 1
      uindex(2) = 1
      uindex(3) = 1
      usrord    = 1

      buflen    = 4 * kmax
      okee      =
     *            GETELT(hdefds,grpdef    ,'THICK'   ,
     *                   uindex,usrord    ,buflen    ,THICK     )
     *            .eq. 0

      buflen    = 4
      okee      = okee .and.
     *            GETELT(hdefds,grpdef    ,'GRDANG'  ,
     *                   uindex,usrord    ,buflen    ,GRDANG    )
     *            .eq. 0
c
      buflen    =  4 * nostat
c     get element alfas (or compute it from xksi and yksi)
c
      call gtalfs( hdefds, hdafds, istat, nostat, alfas,
     *             zbuffs, ierror )
      okee      = okee .and. ierror .eq. 0
c
      grpdef    = 'his-series'
      n         = 0
      buflen    =  4 * nostat * kmax

      do 100 i=nindex(1),nindex(2),nindex(3)
c--------------------------------------------------------------------
c--------Initialize uindex
c--------------------------------------------------------------------
         uindex(1) = i
         uindex(2) = i
         uindex(3) = 1
c--------------------------------------------------------------------
c--------Read from group 3 ZCURU
c--------------------------------------------------------------------
         okee      = okee .and.
     *               GETELT(hdefds,grpdef    ,'ZCURU'   ,
     *                      uindex,usrord    ,buflen    ,zbuffs    )
     *               .eq. 0
         zcuru     = 0.0
         do 110 k=1,kmax
            zcuru  = zcuru + zbuffs(istat ,k) * thick(k)
  110    continue

c--------------------------------------------------------------------
c--------Read from group 3 ZCURV
c--------------------------------------------------------------------
         okee      = okee .and.
     *               GETELT(hdefds,grpdef    ,'ZCURV'   ,
     *                      uindex,usrord    ,buflen    ,zbuffs    )
     *               .eq. 0
         zcurv     = 0
         do 120 k=1,kmax
            zcurv  = zcurv + zbuffs(istat ,k) * thick(k)
  120    continue
c--------------------------------------------------------------------
c--------Backwards transformation of ZCURU and ZCURV
c--------------------------------------------------------------------
cc       if (guugem .lt. eps .or.
cc   *       gvvgem .lt. eps) then
cc          zcurdu = 0.0
cc          zcurdv = 0.0
cc       else
cc          u      =  zcuru  / gvvgem
cc          v      =  zcurv  / guugem
cc          zcurdu =  xksi * u  + xeta * v
cc          zcurdv =  yksi * u  + yeta * v
cc       endif
         zcurdu = zcuru * cos( alfas ) - zcurv * sin( alfas )
         zcurdv = zcuru * sin( alfas ) + zcurv * cos( alfas )
c--------------------------------------------------------------------
c--------Calculate ZCURDM and ZCURDD
c        zcurdd should be defined between 0. and 360. degrees
c        atan2 <-180,180>, grdang [0,360] => mod (.. + 720)
c
c        Note (AM, dd 31 march 1999)
c        The sign before grdang was -, this has been adjusted, though
c        in most cases grdang is zero anyway.
c--------------------------------------------------------------------
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
c
  100 continue

c-----------------------------------------------------------------------
c-----return status to calling routine
c-----------------------------------------------------------------------
      ierror = IEOK
      if (.not. okee) then
         ierror = IEOTHR
      endif
c
      return
      end

      subroutine m3hcrs (hdefds, hdafds, nindex, maxdim, istat , ntruv ,
     *                           lconst, lstci , parcod, xdata , ierror,
     *                   zbuffs)
c-----------------------------------------------------------------------
c           Function: select from TRISULA NEFIS-HIS:
c                     33 Momentary flow, FLTR
c                     34 Accumulated flow, CTR
c                47 - 55 advective flux, ATR
c                57 - 65 dispersive flux, DTR
c                67 - 75 total flux, ATR + DTR
c
c        Method used:
c
c-----------------------------------------------------------------------
c   Calling routine :              HISMAT
c-----------------------------------------------------------------------
c   Called  routines:              GETELT (nefis)
c-----------------------------------------------------------------------
c    Parameters:
c    -----------
c
c   Var.      Type Dimensions
c   -------------------------
c
c HDAFDS      I*4  999       I     Data file descriptor for the HIS-DAT
c                                  file
c HDEFDS      I*4  2997      I     Definition file description for the
c NINDEX      I*4  3         I     indices of time frame in Nefis file
c MAXDIM      I*4            I     length of data array
c ISTAT       I*4            I     Selected station number
c NTRUV       I*4            I     Number of cross-sections
c LCONST      I*4            I     Number of selected constituent
c LSTCI       I*4            I     Number of constituents
c PARCOD      I*4            I     parameter to get data of
c XDATA       R*4   maxdim   O     array with the data
c IERROR      I*4            O     error indicator
c ZBUFFS      R*4   nostat   I/O   buffer for reading Nefis file
c-----------------------------------------------------------------------
c    Local variables:
c    ----------------
c
c   Var.      Type Dimensions
c   -------------------------
c
c BUFLEN      I*4                  Size in bytes of available buffer
c GRPDEF     CH*16                 Group name definition
c I           I*4                  Help variable, loop index
c N           I*4                  Counter for data array
c UINDEX      I*4  3               Array with indices of the cells to
c                                  be read
c USRORD      I*4                  Sequence in which the cells must be
c                                  read
c-----------------------------------------------------------------------
c
c  DECLARATIONS
c
      include       'ods.inc'
c
      integer       hdefds( 2997),hdafds(  999)
      integer       uindex(    3),usrord,buflen
      character*16  grpdef
      integer       GETELT,GETELS
c
      integer         maxdim
      integer         istat ,ntruv ,ierror,n     ,i
      integer         lstci ,lconst
      integer         parcod
      integer         nindex (3)
c
      real            atr   ,dtr   ,fltr  ,ctr
c
      real            zbuffs (ntruv ,*   )
      real            xdata  (maxdim)
c
      logical         okee
c--------------------------------------------------------------------
c
      okee      = .TRUE.
      grpdef    = 'his-series'
      usrord    = 1
      n         = 0

      do 100 i=nindex(1),nindex(2),nindex(3)
c--------------------------------------------------------------------
c--------Initialize uindex
c--------------------------------------------------------------------
         uindex(1) = i
         uindex(2) = i
         uindex(3) = 1

         buflen    =  4 * ntruv

c--------Read from group 3 FLTR
         okee      = okee .and.
     *               GETELT(hdefds,grpdef    ,'FLTR'    ,
     *                      uindex,usrord ,buflen    ,ZBUFFS    )
     *              .eq. 0
         fltr     = zbuffs(istat,1     )

c--------Read from group 3 CTR
         okee      = okee .and.
     *               GETELT(hdefds,grpdef    ,'CTR'     ,
     *                      uindex,usrord ,buflen    ,ZBUFFS    )
     *              .eq. 0
         ctr      = zbuffs(istat,1     )

         if ( lstci .gt. 0 ) then
            buflen    =  4 * ntruv * lstci
c-----------Read from group 3 ATR
            okee      = okee .and.
     *                  GETELT(hdefds,grpdef    ,'ATR'     ,
     *                         uindex,usrord ,buflen    ,ZBUFFS    )
     *                 .eq. 0
            atr      = zbuffs(istat,lconst)

c-----------Read from group 3 DTR
            okee      = okee .and.
     *                  GETELT(hdefds,grpdef    ,'DTR'     ,
     *                         uindex,usrord ,buflen    ,ZBUFFS    )
     *                 .eq. 0
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
c
  100 continue

c-----------------------------------------------------------------------
c-----return status to calling routine
c-----------------------------------------------------------------------
      ierror    = IEOK
      if ( .not. okee ) then
         ierror    = IEOTHR
      endif

      return
      end

      subroutine m3hcur (hdefds, hdafds, nindex, maxdim, istat , nostat,
     *                   kmax  , lay   , parcod, xdata , ierror, zbuffs)
c-----------------------------------------------------------------------
c           Function: select from TRISULA NEFIS-HIS:
c                      7 current u            zeta_position
c                      8 current v            zeta_position
c                      9 current mag. (horiz) zeta_position
c                     10 current dir. (horiz) zeta_position
c
c        Method used:
c
c-----------------------------------------------------------------------
c   Calling routine :              HISMAT
c-----------------------------------------------------------------------
c   Called  routines:              GETELT (nefis)
c-----------------------------------------------------------------------
c    Parameters:
c    -----------
c
c   Var.      Type Dimensions
c   -------------------------
c
c HDAFDS      I*4  999       I     Data file descriptor for the HIS-DAT
c                                  file
c HDEFDS      I*4  2997      I     Definition file description for the
c NINDEX      I*4  3         I     indices of time frame in Nefis file
c MAXDIM      I*4            I     length of data array
c ISTAT       I*4            I     Selected station number
c NOSTAT      I*4            I     Number of stations
c KMAX        I*4            I     Number of layers
c LAY         I*4            I     Selected layer number
c PARCOD      I*4            I     parameter to get data of
c XDATA       R*4   maxdim   O     array with the data
c IERROR      I*4            O     error indicator
c ZBUFFS      R*4   nostat   I/O   buffer for reading Nefis file
c-----------------------------------------------------------------------
c    Local variables:
c    ----------------
c
c   Var.      Type Dimensions
c   -------------------------
c
c BUFLEN      I*4                  Size in bytes of available buffer
c ELM        CH*16                 Element name definition
c ELMNAM     CH*16 3               Element name definition
c EPS         R*4                  small value
c OKEE        L*4                  Flag for NEFIS file readings
c GRDANG      R*4                  Vertex between y-axis and true North
c GRPDEF     CH*16                 Group name definition
c GUUGEM      R*4                  Mean value for distance coefficients
c GVVGEM      R*4                  Mean value for distance coefficients
c HULP        R*4                  Help variable
c I           I*4                  Help variable, loop index
c N           I*4                  Counter for data array
c PI          R*4                  Constant pi
c U           R*4                  velocity u-direction
c UINDEX      I*4  3               Array with indices of the cells to
c                                  be read
c USRORD      I*4                  Sequence in which the cells must be
c                                  read
c V           R*4                  velocity v-direction
c XETA        R*4                  X-distance between 2 grid lines
c                                  around zeta point in y-direction
c XKSI        R*4                  X-distance between 2 grid lines
c                                  around zeta point in x-direction
c YETA        R*4                  Y-distance between 2 grid lines
c                                  around zeta point in y-direction
c YKSI        R*4                  Y-distance between 2 grid lines
c                                  around zeta point in x-direction
c ZCURD       R*4                  velocity direction in selected station
c ZCURM       R*4                  velocity magnitude in selected station
c ZCURU       R*4                  u-velocity component at u-point
c ZCURV       R*4                  v-velocity component at v-point
c ZCURUT      R*4                  u-velocity component at station
c ZCURVT      R*4                  v-velocity component at station
c ZWH         R*4                  total water depth
c ZWL         R*4                  water level
c-----------------------------------------------------------------------
c
c  DECLARATIONS
c
      include       'ods.inc'
c
      integer       hdefds( 2997),hdafds(  999)
      integer       uindex(    3),usrord,buflen
      character*16  grpdef
      integer       GETELT,GETELS
c
      integer         istat ,nostat,kmax  ,ierror,n     ,i     ,lay
      integer         maxdim
      integer         parcod
      integer         nindex (3)
c
      real            pi    ,eps   ,zcuru ,zcurv ,zcurvt,zcurut
      real            zcurm ,hulp  ,zcurd
c
      real            zbuffs (nostat,kmax)
      real            xdata  (maxdim)
c
      logical         okee
c
      real            grdang
      real            alfas
c
      pi        = atan (1.0) * 4.
      eps       = 1.e-12
      grpdef    = 'his-const'
      uindex(1) = 1
      uindex(2) = 1
      uindex(3) = 1
      usrord    = 1

      buflen    = 4
      okee      =
     *            GETELT(hdefds,grpdef    ,'GRDANG'  ,
     *                   uindex,usrord    ,buflen    ,GRDANG    )
     *            .eq. 0
c
      buflen    =  4 * nostat
c
c     get element alfas (or compute it from xksi and yksi)
c
      call gtalfs( hdefds, hdafds, istat, nostat, alfas,
     *             zbuffs, ierror )
      okee      = okee .and. ierror .eq. 0
c
      grpdef    = 'his-series'
      n         = 0
      buflen    =  4 * nostat * kmax

      do 100 i=nindex(1),nindex(2),nindex(3)
c--------------------------------------------------------------------
c--------Initialize uindex
c--------------------------------------------------------------------
         uindex(1) = i
         uindex(2) = i
         uindex(3) = 1
c--------------------------------------------------------------------
c--------Read from group 3 ZCURU
c--------------------------------------------------------------------
         okee   = okee .and.
     *            GETELT(hdefds,grpdef    ,'ZCURU'   ,
     *                   uindex,usrord ,buflen    ,ZBUFFS    )
     *               .eq. 0
         zcuru  = zbuffs(istat ,lay   )
c--------------------------------------------------------------------
c--------Read from group 3 ZCURV
c--------------------------------------------------------------------
         okee   = okee .and.
     *            GETELT(hdefds,grpdef    ,'ZCURV'   ,
     *                   uindex,usrord ,buflen    ,ZBUFFS    )
     *               .eq. 0
         zcurv  = zbuffs(istat ,lay   )
c--------------------------------------------------------------------
c--------Backwards transformation of ZCURU and ZCURV
c--------------------------------------------------------------------
cc       if (guugem .lt. eps .or.
cc   *       gvvgem .lt. eps) then
cc          zcurut = 0.0
cc          zcurvt = 0.0
cc       else
cc          u      =  zcuru  / gvvgem
cc          v      =  zcurv  / guugem
cc          zcurut =  xksi * u  + xeta * v
cc          zcurvt =  yksi * u  + yeta * v
cc       endif
         zcurut = zcuru * cos( alfas ) - zcurv * sin( alfas )
         zcurvt = zcuru * sin( alfas ) + zcurv * cos( alfas )
c--------------------------------------------------------------------
c--------Calculate ZCURM and ZCURD
c        zcurd should be defined between 0. and 360. degrees
c        atan2 <-180,180>, grdang [0,360] => mod (.. + 720)
c
c        Note (AM, dd 31 march 1999)
c        The sign before grdang was -, this has been adjusted, though
c        in most cases grdang is zero anyway.
c--------------------------------------------------------------------
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
c
  100 continue

c-----------------------------------------------------------------------
c-----return status to calling routine
c-----------------------------------------------------------------------
      ierror = IEOK
      if (.not. okee) then
         ierror = IEOTHR
      endif
c
      return
      end

      subroutine m3hwat (hdefds, hdafds, nindex, maxdim, istat , nostat,
     *                                   parcod, xdata , ierror, zbuffs)
c-----------------------------------------------------------------------
c           Function: select from TRISULA NEFIS-HIS:
c                      1 water level
c                      2 total water depth
c                     16 bottom stress u    zeta_position
c                     17 bottom stress v    zeta_position
c                     18 bottom stress mag. zeta_position
c                     19 bottom stress dir. zeta_position
c                     40 bottom stress u    u_position
c                     41 bottom stress v    v_position
c
c        Method used:
c
c-----------------------------------------------------------------------
c   Calling routine :              HISMAT
c-----------------------------------------------------------------------
c   Called  routines:              GETELT (nefis)
c-----------------------------------------------------------------------
c    Parameters:
c    -----------
c
c   Var.      Type Dimensions
c   -------------------------
c
c HDAFDS      I*4  999       I     Data file descriptor for the HIS-DAT
c                                  file
c HDEFDS      I*4  2997      I     Definition file description for the
c NINDEX      I*4  3         I     indices of time frame in Nefis file
c MAXDIM      I*4            I     length of data array
c ISTAT       I*4            I     Selected station number
c NOSTAT      I*4            I     Number of stations
c PARCOD      I*4            I     parameter to get data of
c XDATA       R*4   maxdim   O     array with the data
c IERROR      I*4            O     error indicator
c ZBUFFS      R*4   nostat   I/O   buffer for reading Nefis file
c-----------------------------------------------------------------------
c    Local variables:
c    ----------------
c
c   Var.      Type Dimensions
c   -------------------------
c
c BUFLEN      I*4                  Size in bytes of available buffer
c DPS         R*4                  Depth at station location
c EPS         R*4                  small value
c OKEE        L*4                  Flag for NEFIS file readings
c GRDANG      R*4                  Vertex between y-axis and true North
c GRPNAM     CH*16                 Group name definition
c GUUGEM      R*4                  Mean value for distance coefficients
c GVVGEM      R*4                  Mean value for distance coefficients
c HULP        R*4                  Help variable
c I           I*4                  Help variable, loop index
c IERROR      I*4                  Help variable, error indicator
c N           I*4                  Counter for data array
c PI          R*4                  Constant pi
c U           R*4                  bottom stress u-direction
c UINDEX      I*4  3               Array with indices of the cells to
c                                  be read
c USRORD      I*4                  Sequence in which the cells must be
c                                  read
c V           R*4                  bottom stress v-direction
c XETA        R*4                  X-distance between 2 grid lines
c                                  around zeta point in y-direction
c XKSI        R*4                  X-distance between 2 grid lines
c                                  around zeta point in x-direction
c YETA        R*4                  Y-distance between 2 grid lines
c                                  around zeta point in y-direction
c YKSI        R*4                  Y-distance between 2 grid lines
c                                  around zeta point in x-direction
c ZTAUD       R*4                  bottom stress dir.
c ZTAUET      R*4                  bottom stress y-direction
c ZTAVKS      R*4                  bottom stress x-direction
c ZTAUM       R*4                  bottom stress mag.
c ZTAUT       R*4                  bottom stress u-direction at station
c ZTAVT       R*4                  bottom stress v-direction at station
c ZWH         R*4                  total water depth
c ZWL         R*4                  water level
c-----------------------------------------------------------------------
c
c  DECLARATIONS
c
      include       'ods.inc'
c
      integer       hdefds( 2997),hdafds(  999)
      integer       uindex(    3),usrord,buflen
      character*16  grpnam
      integer       GETELT,GETELS
c
      integer         maxdim
      integer         parcod
      integer         nindex (3)
      integer         istat ,nostat,ierror,n     ,i
c
      real            ztauut,ztauvt,ztaum ,hulp  ,ztaud ,pi    ,eps
      real            zwl   ,zwh   ,ztauet,ztauks
c
      real            zbuffs (nostat)
      real            xdata  (maxdim)
c
      logical         okee
c
      real            grdang
      real            dps
      real            alfas
c
      pi        = atan (1.0) * 4.
      eps       = 1.e-12
      grpnam    = 'his-const'
      uindex(1) = 1
      uindex(2) = 1
      uindex(3) = 1
      usrord    = 1
c
      buflen    = 4
      okee      =
     *            GETELT(hdefds,grpnam    ,'GRDANG'  ,
     *                   uindex,usrord    ,buflen    ,GRDANG    )
     *            .eq. 0
c
      buflen    =  4 * nostat

c     get element alfas (or compute it from xksi and yksi)
c
      call gtalfs( hdefds, hdafds, istat, nostat, alfas,
     *             zbuffs, ierror )
      okee      = okee .and. ierror .eq. 0
c
      okee      = okee .and.
     *            GETELT(hdefds,grpnam    ,'DPS'     ,
     *                   uindex,usrord    ,buflen    ,zbuffs    )
     *            .eq. 0
      dps       = zbuffs(istat )

c
      grpnam    = 'his-series'
      n         = 0
      buflen    =  4 * nostat

      do 100 i=nindex(1),nindex(2),nindex(3)
c--------------------------------------------------------------------
c--------Initialize uindex
c--------------------------------------------------------------------
         uindex(1) = i
         uindex(2) = i
         uindex(3) = 1
c--------------------------------------------------------------------
c--------Read from group 3 ZWL
c--------Calculate ZWH
c--------------------------------------------------------------------
         if ( parcod .eq. 1 .or. parcod .eq. 2 ) then
            okee      = okee .and.
     *                  GETELT(hdefds,grpnam    ,'ZWL'     ,
     *                         uindex,usrord ,buflen    ,ZBUFFS    )
     *                  .eq. 0
            zwl    = zbuffs(istat)
            zwh       = zwl + dps
         else
c--------------------------------------------------------------------
c--------Read from group 3 ZTAUKS
c--------------------------------------------------------------------
            okee      = okee .and.
     *                  GETELT(hdefds,grpnam    ,'ZTAUKS'  ,
     *                         uindex,usrord ,buflen    ,ZBUFFS    )
     *                  .eq. 0
            ztauks    = zbuffs(istat)
c--------------------------------------------------------------------
c--------Read from group 3 ZTAUET
c--------------------------------------------------------------------
            okee      = okee .and.
     *                  GETELT(hdefds,grpnam    ,'ZTAUET'  ,
     *                         uindex,usrord ,buflen    ,ZBUFFS    )
     *                  .eq. 0
            ztauet    = zbuffs(istat)
c--------------------------------------------------------------------
c--------Backwards transformation of ZTAUKS and ZTAUET
c--------------------------------------------------------------------
cc       if (guugem .lt. eps .or.
cc   *       gvvgem .lt. eps) then
cc          ztauut = 0.0
cc          ztauvt = 0.0
cc       else
cc          u      =  ztauks / gvvgem
cc          v      =  ztauet / guugem
cc          ztauut =  xksi * u  + xeta * v
cc          ztauvt =  yksi * u  + yeta * v
cc       endif
            ztauut = ztauks * cos( alfas ) - ztauet * sin( alfas )
            ztauvt = ztauks * sin( alfas ) + ztauet * cos( alfas )
c--------------------------------------------------------------------
c--------Calculate ZTAUM and ZTAUD
c        ztaud should be defined between 0. en  360. degrees
c        atan2 <-180,180>, grdang [0,360] => mod (.. + 720)
c
c        Note (AM, dd 31 march 1999)
c        The sign before grdang was -, this has been adjusted, though
c        in most cases grdang is zero anyway.
c--------------------------------------------------------------------
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

c-----------------------------------------------------------------------
c-----return status to calling routine
c-----------------------------------------------------------------------
      ierror = IEOK
      if ( .not. okee ) then
         ierror = IEOTHR
      endif
c
      end
      subroutine gtalfs( hdefds, hdafds, istat, nostat, alfas,
     *                   buff, ierror )
c
c     purpose:
c
c              Get alfas (for backward transformation) from NEFIS file
c              If element is not on the file (old TRISULA version)
c              then alfas is computed from xksi and yksi
c              alfas is transformed to radians
c
c
      integer hdefds(*), hdafds(*)
      integer istat, nostat, ierror
      real    buff(nostat), alfas

      integer      uindex(3), usrord, buflen
      real         xksi, yksi, raddeg, degrad, pi, eps
      character*16 grpdef,elmnam

      integer*4    GETELT,GETELS

c
c     General intialisation
c
      pi = 4. * atan(1.0)
      raddeg = 180./pi
      degrad = pi/180.
      eps    = 1.E-10
c
      buflen = 4 * nostat
      grpdef = 'his-const'
      elmnam = 'ALFAS'
      uindex(1) = 1
      uindex(2) = 1
      uindex(3) = 1
      usrord    = 1
c
c     Try to read the element ALFAS
c
      ierror = GETELT( hdefds, grpdef, elmnam, uindex,
     *                 usrord, buflen, buff  )
      if ( ierror .ne. 0) then
c
c       In case of an old trih file read XKSI and YKSI and compute
c       ALFAS
c
         buflen = 4 * nostat
         elmnam = 'XKSI'
         ierror = GETELT( hdefds, grpdef, elmnam, uindex,
     *                    usrord, buflen, buff  )
         if ( ierror .ne. 0 ) go to 900

         xksi   = buff(istat)
         elmnam = 'YKSI'
         ierror = GETELT( hdefds, grpdef, elmnam, uindex,
     *                    usrord, buflen, buff  )
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




