subroutine mafina (filnam)
!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Main module
!
! Programmer:         J.Kuipers
!
! Module:             Make FIle NAmes
!
! Module description: Reads file containing names of files.
!                     If this file (list is not present construct
!                     file names .
!
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! INPASC  Input of group from "ini-type" Ascii file
!=======================================================================
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: mafina.pf,v $
! Revision 1.7  1999/06/15  15:11:19  kuipe_j
! Default file name = fnm name
!
! Revision 1.6  1999/06/01  13:42:43  kuipe_j
! names in messages substituted + message template
!
! Revision 1.5  1998/11/13  09:01:52  kuipe_j
! aggregationfile in CMT
!
! Revision 1.4  1998/06/24  13:41:19  kuipe_j
! Dot in directory
!
! Revision 1.3  1998/06/11  11:47:32  kuipe_j
! Estuary special integrated
!
! Revision 1.2  1998/06/08  13:15:31  kuipe_j
! time lag hydr controller
!
! Revision 1.1  1998/02/13  13:23:42  kuipe_j
! Adapt to CMT
!
!
!***********************************************************************

   character*256 filnam, fnmnam

   include '..\include\filsim.i'

!     Local variables

   integer            grplen, ngroup, lutemp, l, lt ,i ,idir,&
   &i1    , i2    , ios
   parameter         (ngroup = 14)
   parameter         (grplen = 40)
   character*(grplen) grpnam(ngroup)
   character*1        dirsep
   logical            lstfil
!
!
   data grpnam /&
   &'Flow Module Output                      ',&
   &'Kalman Module Output                    ',&
   &'Salt Module Output                      ',&
   &'Sedimen Module Output                   ',&
   &'Morphology Module Output                ',&
   &'Morphology Trajects                     ',&
   &'Water Quality Output                    ',&
   &'Restart Files                           ',&
   &'Log and status file                     ',&
   &'Auxilliary files                        ',&
   &'Simulation Input                        ',&
   &'Graded sediment Output                  ',&
   &'Graded sediment Auxilliary              ',&
   &'Tidal Analyses                          '/
!
!     Open file with file names
!
   errtem = ' '
   lutemp = 31
   inquire ( file = filnam, exist = lstfil )
   if ( lstfil ) then
      open ( lutemp , file = filnam )

!        The first parameter on the command line is
!        the name of an existing file.
!        Check if it is an .ini-file or a .fnm-file

      call gkwini ( lutemp , 'System' , 'SOBEKSIM' , errtem )
      if (errtem .ne. ' ' ) then
!           It is an .ini-file
!           Build name of file with templates of error messages

!           Determine directory separator

         if (index(errtem,'/').ne.0) then
            dirsep = '/'
         else
            dirsep = '\'
         endif
         i2 = len  (errtem)
         i1 = index(errtem,' ')
         errtem(i1:)      = dirsep//'sim_err.txt'
         errtem(i1+12:i2) = ' '

!           Get .fnm file name

         call gkwini ( lutemp , 'Model' , 'model' , fnmnam )
         close (lutemp)
         if (fnmnam .ne. ' ' ) then
            open ( lutemp , file = fnmnam ,iostat = ios )
            if (ios.ne.0) lstfil = .false.
            filnam = fnmnam
         else
            lstfil = .false.
         endif
      endif
   endif
   if (lstfil) then
!        History format files of module flow
      call gkwini ( lutemp , grpnam(1) , 'map' , flwmap )
      if ( flwmap .eq. ' ') lstfil = .false.
      call gkwini ( lutemp , grpnam(1) , 'his' , flwhis )
      if ( flwhis .eq. ' ') lstfil = .false.
      call gkwini ( lutemp , grpnam(1) , 'str' , fstrhs )
      if ( fstrhs .eq. ' ') lstfil = .false.
      call gkwini ( lutemp , grpnam(1) , 'qlt' , fqlths )
      if ( fqlths .eq. ' ') lstfil = .false.
      call gkwini ( lutemp , grpnam(1) , 'mix' , minmax )
      if ( minmax .eq. ' ') lstfil = .false.
!        History format files of module Kalman filter
      call gkwini ( lutemp , grpnam(2) , 'fhm' , fihmap )
      if ( fihmap .eq. ' ') lstfil = .false.
      call gkwini ( lutemp , grpnam(2) , 'fhh' , fihhis )
      if ( fihhis .eq. ' ') lstfil = .false.
      call gkwini ( lutemp , grpnam(2) , 'fpm' , fipmap )
      if ( fipmap .eq. ' ') lstfil = .false.
      call gkwini ( lutemp , grpnam(2) , 'fph' , fiphis )
      if ( fiphis .eq. ' ') lstfil = .false.
      call gkwini ( lutemp , grpnam(2) , 'frm' , firmap )
      if ( firmap .eq. ' ') lstfil = .false.
      call gkwini ( lutemp , grpnam(2) , 'frh' , firhis )
      if ( firhis .eq. ' ') lstfil = .false.
      call gkwini ( lutemp , grpnam(2) , 'phm' , prhmap )
      if ( prhmap .eq. ' ') lstfil = .false.
      call gkwini ( lutemp , grpnam(2) , 'phh' , prhhis )
      if ( prhhis .eq. ' ') lstfil = .false.
      call gkwini ( lutemp , grpnam(2) , 'ppm' , prpmap )
      if ( prpmap .eq. ' ') lstfil = .false.
      call gkwini ( lutemp , grpnam(2) , 'pph' , prphis )
      if ( prphis .eq. ' ') lstfil = .false.
!        History format files of module salt
      call gkwini ( lutemp , grpnam(3) , 'map' , sltmap )
      if ( sltmap .eq. ' ') lstfil = .false.
      call gkwini ( lutemp , grpnam(3) , 'his' , slthis )
      if ( slthis .eq. ' ') lstfil = .false.
!        History format files of module sediment
      call gkwini ( lutemp , grpnam(4) , 'map' , sdtmap )
      if ( sdtmap .eq. ' ') lstfil = .false.
      call gkwini ( lutemp , grpnam(4) , 'his' , sdthis )
      if ( sdthis .eq. ' ') lstfil = .false.
!        History format files of module morphology
      call gkwini ( lutemp , grpnam(5) , 'map' , mrpmap )
      if ( mrpmap .eq. ' ') lstfil = .false.
      call gkwini ( lutemp , grpnam(5) , 'his' , mrphis )
      if ( mrphis .eq. ' ') lstfil = .false.
!        Morphology trajects
      call gkwini ( lutemp , grpnam(6) , 'tra' , trainp )
      if ( trainp .eq. ' ') lstfil = .false.
      call gkwini ( lutemp , grpnam(6) , 'tro' , traout )
      if ( traout .eq. ' ') lstfil = .false.
      call gkwini ( lutemp , grpnam(6) , 'gro' , griout )
      if ( griout .eq. ' ') lstfil = .false.
!        History format files of graded sediment module
      call gkwini ( lutemp , grpnam(12) , 'map' , gsedmap )
      if ( gsedmap .eq. ' ') lstfil = .false.
      call gkwini ( lutemp , grpnam(12) , 'his' , gsedhis )
      if ( gsedhis .eq. ' ') lstfil = .false.
      call gkwini ( lutemp , grpnam(12) , 'fmp' , gfrcmap )
!TEMP    if ( gfrcmap .eq. ' ') lstfil = .false.
      if ( gfrcmap .eq. ' ') gfrcmap = 'gfrcmap.his'
      call gkwini ( lutemp , grpnam(12) , 'fhs' , gfrchis )
!TEMP    if ( gfrchis .eq. ' ') lstfil = .false.
      if ( gfrchis .eq. ' ') gfrchis = 'gfrchis.his'
!        Other files of graded sediment module
      call gkwini ( lutemp , grpnam(13) , 'gut' , graout )
      if ( graout .eq. ' ') lstfil = .false.
      call gkwini ( lutemp , grpnam(13) , 'glg' , gralog )
      if ( gralog .eq. ' ') lstfil = .false.
!        Water quality files
      call gkwini ( lutemp , grpnam(7) , 'sgf' , fsgfun )
      if ( fsgfun .eq. ' ') lstfil = .false.
      call gkwini ( lutemp , grpnam(7) , 'exa' , fexare )
      if ( fexare .eq. ' ') lstfil = .false.
      call gkwini ( lutemp , grpnam(7) , 'exf' , fexflo )
      if ( fexflo .eq. ' ') lstfil = .false.
      call gkwini ( lutemp , grpnam(7) , 'len' , flenth )
      if ( flenth .eq. ' ') lstfil = .false.
      call gkwini ( lutemp , grpnam(7) , 'vol' , fvolum )
      if ( fvolum .eq. ' ') lstfil = .false.
      call gkwini ( lutemp , grpnam(7) , 'poi' , fpoint )
      if ( fpoint .eq. ' ') lstfil = .false.
      call gkwini ( lutemp , grpnam(7) , 'ino' , fwqino )
      if ( fwqino .eq. ' ') lstfil = .false.
      call gkwini ( lutemp , grpnam(7) , 'inp' , fwqinp )
      if ( fwqinp .eq. ' ') lstfil = .false.
      call gkwini ( lutemp , grpnam(7) , 'wda' , nefwda )
      if ( nefwda .eq. ' ') lstfil = .false.
      call gkwini ( lutemp , grpnam(7) , 'wdf' , nefwdf )
      if ( nefwdf .eq. ' ') lstfil = .false.
!        Tidal analyses
      call gkwini ( lutemp , grpnam(14) , 'gap' , gaprinam )
      if ( gaprinam .eq. ' ')lstfil = .false.
      call gkwini ( lutemp , grpnam(14) , 'gaw' , gawlev )
      if ( gawlev .eq. ' ')  lstfil = .false.
      call gkwini ( lutemp , grpnam(14) , 'gad' , gadisch )
      if ( gadisch .eq. ' ') lstfil = .false.
      call gkwini ( lutemp , grpnam(14) , 'gav' , gaveloc )
      if ( gaveloc .eq. ' ') lstfil = .false.
      call gkwini ( lutemp , grpnam(14) , 'gac' , gaconcen )
      if ( gaconcen .eq. ' ')lstfil = .false.
!        Restart files.
      call gkwini ( lutemp , grpnam(8) , 'rda' , nefrda )
      if ( nefrda .eq. ' ') lstfil = .false.
      call gkwini ( lutemp , grpnam(8) , 'rdf' , nefrdf )
      if ( nefrdf .eq. ' ') lstfil = .false.
      call gkwini ( lutemp , grpnam(8) , 'nda' , nefnda )
      if ( nefnda .eq. ' ') lstfil = .false.
      call gkwini ( lutemp , grpnam(8) , 'ndf' , nefndf )
      if ( nefndf .eq. ' ') lstfil = .false.
!        Log and status file
      call gkwini ( lutemp , grpnam(9) , 'log' , logfil )
      if ( logfil .eq. ' ') lstfil = .false.
      call gkwini ( lutemp , grpnam(9) , 'sta' , statfl )
      if ( statfl .eq. ' ') lstfil = .false.
      call gkwini ( lutemp , grpnam(9) , 'ReturncodeFile',&
      &rtncod )
      if ( rtncod .eq. ' ') lstfil = .false.
!        Auxilliary files
      call gkwini ( lutemp , grpnam(10) , 'res' , fresid )
      if ( fresid .eq. ' ') lstfil = .false.
      call gkwini ( lutemp , grpnam(10) , 'fro' , ffroud )
      if ( ffroud .eq. ' ') lstfil = .false.
      call gkwini ( lutemp , grpnam(10) , 'drs' , fdmprs )
      if ( fdmprs .eq. ' ') lstfil = .false.
      call gkwini ( lutemp , grpnam(10) , 'dst' , fdmpst )
      if ( fdmpst .eq. ' ') lstfil = .false.
      call gkwini ( lutemp , grpnam(10) , 'dsl' , fdmpsl )
      if ( fdmpsl .eq. ' ') lstfil = .false.
!        Simulation input
      call gkwini ( lutemp , grpnam(11) , 'mda' , nefmda )
      if ( nefmda .eq. ' ') lstfil = .false.
      call gkwini ( lutemp , grpnam(11) , 'mdf' , nefmdf )
      if ( nefmdf .eq. ' ') lstfil = .false.
      close (lutemp)
   endif
!
!     Remove extension
!
   l = index (filnam, ' ')
   idir = 0
   do 35 i=1,l
      if (filnam(i:i).eq.'\' .or. filnam(i:i).eq.'/') then
         idir = i
      endif
35 continue
   lt = index (filnam(idir+1:l), '.')
   if (lt.gt.0) then
      lt = idir+lt
      filnam(lt:l)=' '
   endif
   if (.not.lstfil) then
!        History format files of module flow
      flwmap = 'flowmap.his'
      flwhis = 'flowhis.his'
      fstrhs = 'struchis.his'
      fqlths = 'qlathis.his'
      minmax = 'minmax.his'
!        History format files of module Kalman filter
      fihmap = 'kafhmap.his'
      fihhis = 'kafhhis.his'
      fipmap = 'kafpmap.his'
      fiphis = 'kafphis.his'
      firmap = 'kafrmap.his'
      firhis = 'kafrhis.his'
      prhmap = 'kaphmap.his'
      prhhis = 'kaphhis.his'
      prpmap = 'kappmap.his'
      prphis = 'kapphis.his'
!        History format files of module salt
      sltmap = 'saltmap.his'
      slthis = 'salthis.his'
!        History format files of module sediment
      sdtmap = 'sedtmap.his'
      sdthis = 'sedthis.his'
!        History format files of module morphology
      mrpmap = 'morpmap.his'
      mrphis = 'morphis.his'
!        Morphology trajects
      trainp = ' '
      traout = ' '
      griout = ' '
!        History format files of graded sediment module
      gsedmap = 'gsedmap.his'
      gsedhis = 'gsedhis.his'
      gfrcmap = 'gfrcmap.his'
      gfrchis = 'gfrchis.his'
!        Other files of graded sediment module
      call socnam (filnam, graout ,'.gut' )
      call socnam (filnam, gralog ,'.glg' )
!        Tidal analyses
      gaprinam = 'gareport.txt'
      gawlev   = 'gawatlev.fix'
      gadisch  = 'gadisch.fix'
      gaveloc  = 'gavelocity.fix'
      gaconcen = 'gaconcen.fix'
!        Water quality files
      call socnam (filnam, fsgfun ,'.sgf' )
      call socnam (filnam, fexare ,'.exa' )
      call socnam (filnam, fexflo ,'.exf' )
      call socnam (filnam, flenth ,'.len' )
      call socnam (filnam, fvolum ,'.vol' )
      call socnam (filnam, fpoint ,'.poi' )
      call socnam (filnam, fwqino ,'.ino' )
      call socnam (filnam, fwqinp ,'.inp' )
      call socnam (filnam, nefwda ,'.wda' )
      call socnam (filnam, nefwdf ,'.wdf' )

!        Restart files.
      call socnam (filnam, nefrda ,'.rda' )
      call socnam (filnam, nefrdf ,'.rdf' )
      call socnam (filnam, nefnda ,'.nda' )
      call socnam (filnam, nefndf ,'.ndf' )
!        Log and status file
      call socnam (filnam, logfil ,'.log' )
      call socnam (filnam, statfl ,'.sta' )
      rtncod = 'hydrcomp.rtn'
!        Auxilliary files
      fresid = 'residu'
      ffroud = 'froude'
      fdmprs = 'dumpres'
      fdmpst = 'dumpstr'
      fdmpsl = 'dumpsol'
!        Simulation input
      call socnam (filnam, nefmda ,'.mda' )
      call socnam (filnam, nefmdf ,'.mdf' )
!
   endif
   if (errtem.eq. ' ' ) errtem = 'sim_err.txt'
   return
end
