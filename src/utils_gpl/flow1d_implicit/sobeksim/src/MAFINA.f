      subroutine mafina (filnam)
c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Main module
c
c Programmer:         J.Kuipers
c
c Module:             Make FIle NAmes
c
c Module description: Reads file containing names of files.
c                     If this file (list is not present construct
c                     file names .
c
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c INPASC  Input of group from "ini-type" Ascii file
c=======================================================================
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: mafina.pf,v $
c Revision 1.7  1999/06/15  15:11:19  kuipe_j
c Default file name = fnm name
c
c Revision 1.6  1999/06/01  13:42:43  kuipe_j
c names in messages substituted + message template
c
c Revision 1.5  1998/11/13  09:01:52  kuipe_j
c aggregationfile in CMT
c
c Revision 1.4  1998/06/24  13:41:19  kuipe_j
c Dot in directory
c
c Revision 1.3  1998/06/11  11:47:32  kuipe_j
c Estuary special integrated
c
c Revision 1.2  1998/06/08  13:15:31  kuipe_j
c time lag hydr controller
c
c Revision 1.1  1998/02/13  13:23:42  kuipe_j
c Adapt to CMT
c
c
c***********************************************************************

      character*256 filnam, fnmnam

      include '..\include\filsim.i'

C     Local variables

      integer            grplen, ngroup, lutemp, l, lt ,i ,idir,
     +                   i1    , i2    , ios
      parameter         (ngroup = 14)
      parameter         (grplen = 40)
      character*(grplen) grpnam(ngroup)
      character*1        dirsep
      logical            lstfil
c
C
      data grpnam /
     +            'Flow Module Output                      ',
     +            'Kalman Module Output                    ',
     +            'Salt Module Output                      ',
     +            'Sedimen Module Output                   ',
     +            'Morphology Module Output                ',
     +            'Morphology Trajects                     ',
     +            'Water Quality Output                    ',
     +            'Restart Files                           ',
     +            'Log and status file                     ',
     +            'Auxilliary files                        ',
     +            'Simulation Input                        ',
     +            'Graded sediment Output                  ',
     +            'Graded sediment Auxilliary              ',
     +            'Tidal Analyses                          '/ 
c
c     Open file with file names
c
      errtem = ' '
      lutemp = 31
      inquire ( file = filnam, exist = lstfil )
      if ( lstfil ) then
         open ( lutemp , file = filnam )

c        The first parameter on the command line is
c        the name of an existing file.
c        Check if it is an .ini-file or a .fnm-file

         call gkwini ( lutemp , 'System' , 'SOBEKSIM' , errtem )
         if (errtem .ne. ' ' ) then
c           It is an .ini-file
c           Build name of file with templates of error messages

c           Determine directory separator

            if (index(errtem,'/').ne.0) then
               dirsep = '/'
            else
               dirsep = '\'
            endif
            i2 = len  (errtem)
            i1 = index(errtem,' ')
            errtem(i1:)      = dirsep//'sim_err.txt'
            errtem(i1+12:i2) = ' '

c           Get .fnm file name

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
c        History format files of module flow
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
c        History format files of module Kalman filter
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
c        History format files of module salt
         call gkwini ( lutemp , grpnam(3) , 'map' , sltmap )
         if ( sltmap .eq. ' ') lstfil = .false.
         call gkwini ( lutemp , grpnam(3) , 'his' , slthis )
         if ( slthis .eq. ' ') lstfil = .false.
c        History format files of module sediment
         call gkwini ( lutemp , grpnam(4) , 'map' , sdtmap )
         if ( sdtmap .eq. ' ') lstfil = .false.
         call gkwini ( lutemp , grpnam(4) , 'his' , sdthis )
         if ( sdthis .eq. ' ') lstfil = .false.
c        History format files of module morphology
         call gkwini ( lutemp , grpnam(5) , 'map' , mrpmap )
         if ( mrpmap .eq. ' ') lstfil = .false.
         call gkwini ( lutemp , grpnam(5) , 'his' , mrphis )
         if ( mrphis .eq. ' ') lstfil = .false.
c        Morphology trajects
         call gkwini ( lutemp , grpnam(6) , 'tra' , trainp )
         if ( trainp .eq. ' ') lstfil = .false.
         call gkwini ( lutemp , grpnam(6) , 'tro' , traout )
         if ( traout .eq. ' ') lstfil = .false.
         call gkwini ( lutemp , grpnam(6) , 'gro' , griout )
         if ( griout .eq. ' ') lstfil = .false.
c        History format files of graded sediment module
         call gkwini ( lutemp , grpnam(12) , 'map' , gsedmap )
         if ( gsedmap .eq. ' ') lstfil = .false.
         call gkwini ( lutemp , grpnam(12) , 'his' , gsedhis )
         if ( gsedhis .eq. ' ') lstfil = .false.
         call gkwini ( lutemp , grpnam(12) , 'fmp' , gfrcmap )
CTEMP    if ( gfrcmap .eq. ' ') lstfil = .false.
         if ( gfrcmap .eq. ' ') gfrcmap = 'gfrcmap.his'
         call gkwini ( lutemp , grpnam(12) , 'fhs' , gfrchis )
CTEMP    if ( gfrchis .eq. ' ') lstfil = .false.
         if ( gfrchis .eq. ' ') gfrchis = 'gfrchis.his'
c        Other files of graded sediment module         
         call gkwini ( lutemp , grpnam(13) , 'gut' , graout )
         if ( graout .eq. ' ') lstfil = .false.
         call gkwini ( lutemp , grpnam(13) , 'glg' , gralog )
         if ( gralog .eq. ' ') lstfil = .false.
c        Water quality files
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
c        Tidal analyses
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
c        Restart files.
         call gkwini ( lutemp , grpnam(8) , 'rda' , nefrda )
         if ( nefrda .eq. ' ') lstfil = .false.
         call gkwini ( lutemp , grpnam(8) , 'rdf' , nefrdf )
         if ( nefrdf .eq. ' ') lstfil = .false.
         call gkwini ( lutemp , grpnam(8) , 'nda' , nefnda )
         if ( nefnda .eq. ' ') lstfil = .false.
         call gkwini ( lutemp , grpnam(8) , 'ndf' , nefndf )
         if ( nefndf .eq. ' ') lstfil = .false.
c        Log and status file
         call gkwini ( lutemp , grpnam(9) , 'log' , logfil )
         if ( logfil .eq. ' ') lstfil = .false.
         call gkwini ( lutemp , grpnam(9) , 'sta' , statfl )
         if ( statfl .eq. ' ') lstfil = .false.
         call gkwini ( lutemp , grpnam(9) , 'ReturncodeFile',
     &                 rtncod )
         if ( rtncod .eq. ' ') lstfil = .false.
c        Auxilliary files
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
c        Simulation input
         call gkwini ( lutemp , grpnam(11) , 'mda' , nefmda )
         if ( nefmda .eq. ' ') lstfil = .false.
         call gkwini ( lutemp , grpnam(11) , 'mdf' , nefmdf )
         if ( nefmdf .eq. ' ') lstfil = .false.
         close (lutemp)
      endif
c
c     Remove extension
c
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
c        History format files of module flow
         flwmap = 'flowmap.his'
         flwhis = 'flowhis.his'
         fstrhs = 'struchis.his'
         fqlths = 'qlathis.his'
         minmax = 'minmax.his'
c        History format files of module Kalman filter
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
c        History format files of module salt
         sltmap = 'saltmap.his'
         slthis = 'salthis.his'
c        History format files of module sediment
         sdtmap = 'sedtmap.his'
         sdthis = 'sedthis.his'
c        History format files of module morphology
         mrpmap = 'morpmap.his'
         mrphis = 'morphis.his'
c        Morphology trajects
         trainp = ' '
         traout = ' '
         griout = ' '
c        History format files of graded sediment module
         gsedmap = 'gsedmap.his'
         gsedhis = 'gsedhis.his'
         gfrcmap = 'gfrcmap.his'
         gfrchis = 'gfrchis.his'
c        Other files of graded sediment module         
         call socnam (filnam, graout ,'.gut' )
         call socnam (filnam, gralog ,'.glg' )
c        Tidal analyses
         gaprinam = 'gareport.txt'
         gawlev   = 'gawatlev.fix'
         gadisch  = 'gadisch.fix' 
         gaveloc  = 'gavelocity.fix'   
         gaconcen = 'gaconcen.fix'        
c        Water quality files
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

c        Restart files.
         call socnam (filnam, nefrda ,'.rda' )
         call socnam (filnam, nefrdf ,'.rdf' )
         call socnam (filnam, nefnda ,'.nda' )
         call socnam (filnam, nefndf ,'.ndf' )
c        Log and status file
         call socnam (filnam, logfil ,'.log' )
         call socnam (filnam, statfl ,'.sta' )
         rtncod = 'hydrcomp.rtn'
c        Auxilliary files
         fresid = 'residu'
         ffroud = 'froude'
         fdmprs = 'dumpres'
         fdmpst = 'dumpstr'
         fdmpsl = 'dumpsol'
c        Simulation input
         call socnam (filnam, nefmda ,'.mda' )
         call socnam (filnam, nefmdf ,'.mdf' )
c                 
      endif
      if (errtem.eq. ' ' ) errtem = 'sim_err.txt'
      return
      end
