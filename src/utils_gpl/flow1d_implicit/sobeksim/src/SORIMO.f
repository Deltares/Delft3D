      subroutine SORIMO ( istep  ,time   ,itim   ,dtf    ,steady ,
     +                    juer   ,juresi ,jufrou ,juresd ,justrd ,
     +                    ker    ,inocon ,jusold ,lfrou  ,itstat ,
     +                    frobuf ,lrest)

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Main module
c
c Programmer:         S.L. van der Woude
c
c Module:             SORIMO (SObek RIver MOrphology)
c
c Module description: This routine iterates one time step for rivers
c                     including the morphology module.
c
c                     When this module is called an initial flow must be
c                     available. This is the true in the following ca-
c                     ses:
c
c                     o      re-start has been executed;
c                     o      auto-start has been executed;
c                     o      steady flow step has been executed.
c
c                     The first step is the calculation of the courant
c                     number. The calculated celereties are used to
c                     determine the courant number (MOCOUR). From this
c                     routine an integer M (M >= 1) is returned which is
c                     used to reduce the current time step.
c
c                     The calculated sediment transports are used in the
c                     morphology module (MORPH) to calculate the changes
c                     of the cross sections. The next step is a call to
c                     CSTABL to calculate the derived cross sectional
c                     variables. After this a new flow step is calcula-
c                     ted using the new determined time level.
c
c                     The loop continues until the new end time has been
c                     reached or a maximum number of iterations has been
c                     reached.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  4 dtf               I  Time step flow module
c 12 inocon            P  -
c  1 istep             P  -
c  3 itim(2)           IO Actual time level tn+1 expressed in date and
c                         time. Format (integer):
c                         itim(1) = YYYYMMDD (year,month,day)
c                         itim(2) = HHMMSSHH (hour,minute,second,
c                                   hundredth of a second)
c 15 itstat            P  -
c  6 juer              P  -
c  8 jufrou            P  -
c  9 juresd            P  -
c  7 juresi            P  -
c 13 jusold            P  -
c 10 justrd            P  -
c 11 ker               IO Error code:
c                         ok     (0) : No error
c                         info   (1) : Informative message
c                         warnng (2) : Warning
c                         fatal  (3) : Fatal error (processing stops)
c 14 lfrou             P  -
c  5 steady            P  -
c  2 time              IO Actual time level tn+1. in sec.
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c cstabl  Cross Sectional TABLe module
c error   write an ERROR to the error file.
c gtipnt  GeT Integer PoiNTer
c gtrpnt  GeT Real PoiNTer
c mocour  MOrphology COURant number
c morph   MORPHology module
c sedim   SEDIMent module
c soflow  SObek FLOW main routine
c soipar  SObek Integer PARameter
c sorpar  SObek Real PARameter
c sosdir  SObek Sediment flow DIRection
c sotime  SObek TIME
c=======================================================================
c
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: sorimo.pf,v $
c Revision 1.24  1999/03/15  15:05:19  kuipe_j
c Improve Froude file
c
c Revision 1.23  1998/06/11  11:47:48  kuipe_j
c Estuary special integrated
c
c Revision 1.22  1997/05/26  07:37:03  kuipe_j
c statistic of iteration improved
c
c Revision 1.21  1997/02/17  10:09:40  kuipe_j
c Lateral Q in m3/s in cont. equation now
c
c Revision 1.20  1997/01/23  08:30:16  kuipe_j
c Make flow module robust
c
c Revision 1.19  1996/12/04  11:59:31  kuipe_j
c declarations / undefined vars
c
c Revision 1.18  1996/12/02  10:03:49  kuipe_j
c avoid negative pointers
c
c Revision 1.17  1996/05/28  13:29:27  kuipe_j
c Error message courant nr added
c
c Revision 1.16  1996/04/12  13:06:10  kuipe_j
c headers, minor changes
c
c Revision 1.15  1996/04/11  08:16:37  kuipe_j
c Kalman module added
c
c Revision 1.14  1996/03/08  09:40:50  kuipe_j
c Headers
c
c Revision 1.13  1996/03/07  10:44:31  kuipe_j
c Bottom scema acc. to Lax Wendroff with flux limitter
c
c Revision 1.12  1996/02/09  15:13:38  kuipe_j
c a.o. Restart improvements
c
c Revision 1.11  1995/11/21  11:09:14  kuipe_j
c Added features are: Special morphology output for IVR; Improvement of
c     auxilliary output; Automatic speudo time stepping; general structure
c     improvement (Q-dependent lin, relax. of Q only, changed weir Q-H
c     relation); removal of grid points in messages; etc.
c
c Revision 1.10  1995/10/18  10:51:30  hoeks_a
c Some small changes
c
c Revision 1.9  1995/10/18  09:01:06  kuipe_j
c Changes concerning aux. ouput and IVR adjustments
c
c Revision 1.8  1995/10/11  12:24:14  kuipe_j
c Remove aux output temp
c
c Revision 1.7  1995/09/29  10:36:43  kuipe_j
c Improvement of autostart and simple weir
c
c Revision 1.6  1995/09/22  10:04:28  kuipe_j
c variable dimensions, new headers
c
c Revision 1.5  1995/09/12  08:11:34  overmar
c - Option "zomerkaden" added
c - Better linearization
c - Pseudo time
c - Iterative matrix solution
c
c Revision 1.4  1995/08/30  12:37:37  kuipe_j
c Triggers + controllers for BOS
c
c Revision 1.3  1995/05/30  09:57:03  hoeks_a
c Minor changes
c
c Revision 1.2  1995/05/30  07:09:57  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:12:23  hoeks_a
c Initial check-in
c
c Revision 1.5  1994/12/02  13:33:31  kuipe_j
c Improvement of message handling.
c
c Revision 1.4  1994/11/28  08:28:43  kuipe_j
c Time and timestep in double precision.
c
c Revision 1.3  1993/12/13  15:49:56  kuipe_j
c Improved declaration of DSTEP.
c Pointers for Salt module are only set if module is present.
c
c Revision 1.2  1993/11/26  15:10:04  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:39:45  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Parameters
c
      integer  istep ,itim(2), ker   ,juer, juresi ,jufrou ,juresd
      integer  justrd,inocon ,jusold ,itstat(4)
      double   precision       time  ,dtf
      logical  steady, lfrou, lrest
      real     frobuf (8)
c
c     Local variables
c
      integer aft   ,afwfqs,alfab ,att   ,bfricp,bfrict,bgout ,
     +        branch,brnode,celer ,cpack ,dissed,e     ,engpar,
     +        forcon,flwdir,grid  ,grsize,hpack ,hlev  ,izwft ,
     +        maxlev,maxtab,mbdpar,mltpar,morcon,nboun ,nbran ,
     +        nbrnod,nellvl,ngrid ,nlev  ,nmlat ,nnelvl,nnode ,
     +        node  ,nonall,nqlat ,nsedrd,ntab  ,ntabm ,ntmpgr,
     +        nucoef,of    ,prslot,psltvr,qpack ,qlat  ,qltpar,
     +        rc    ,rpack ,sdrdbf,sectc ,sectv ,seddb ,sedinf,
     +        sedpar,sedtr ,slat  ,table ,tmpgr ,trform,sumda ,
     +        typcr ,uscoef,waoft ,wft   ,ws    ,wtt   ,x     ,
     +        morpar
      integer h2
c
c     Run parameters flow module
c
      real     g
      integer  filstp,cpredn
c
c     Run parameters morphology module
c
      real     alphac
      integer  moitmx
c
c     and other variables
c
      integer  ixpar  ,timnp1(2)
c
      double   precision       tnp1   ,ldtf
      integer  cnt    ,m    , igpmc
      logical  lastts ,lsalt, lboun, lkalm,
c     mozart   variables 
     +         lmozad, lgrwt
      integer  nstepd
c
      character*4      txt
      character*18     txt1
c
c     External functions
c
      integer  gtipnt, gtrpnt, gtdpnt, soipar
      real     sorpar
      external gtipnt, gtrpnt, gtdpnt, soipar, sorpar
c
c     Include memory pool
c
      include '..\include\mempool.i'
      include '..\include\errcod.i'
c
c     Sediment module should process morphodynamic boundary conditions
c
      lboun  = .true.
c
c     Extract parameters from flwpar
c
      ixpar = gtrpnt ( 'FLWPAR' )
c
      g      = sorpar ( rp(ixpar), 1 )
c
c     Extract parameters from morpar
c
      ixpar  = gtrpnt ( 'MORPAR' )
c
      moitmx = soipar ( rp(ixpar), 2 )
      alphac = sorpar ( rp(ixpar), 3 )
c
c     Find starting addresses of working arrays
c
c     Single variables are read from the memory pool to simplify the
c     call for the Microsoft Fortran compiler v5
c
      aft    =     gtrpnt ( 'AFT'   )
      afwfqs =     gtrpnt ( 'AFWFQS')
      alfab  =     gtrpnt ( 'ALFAB' )
      att    =     gtrpnt ( 'ATT'   )
      bfricp =     gtrpnt ( 'BFRICP')
      bfrict =     gtipnt ( 'BFRICT')
      bgout  =     gtipnt ( 'BGOUT' )
      branch =     gtipnt ( 'BRANCH')
      brnode =     gtipnt ( 'BRNODE')
      celer  =     gtrpnt ( 'CELER' )
      cpack  =     gtrpnt ( 'CPACK' )
      dissed =     gtrpnt ( 'DISSED')
      e      =     gtrpnt ( 'E'     )
      engpar =     gtrpnt ( 'ENGPAR')
      flwdir =     gtipnt ( 'FLWDIR')
      forcon =     gtrpnt ( 'FORCON')
      grid   =     gtipnt ( 'GRID'  )
      grsize =     gtrpnt ( 'GRSIZE')
      hlev   =     gtdpnt ( 'HLEV'  )
      hpack  =     gtdpnt ( 'HPACK')
      izwft  =     gtrpnt ( 'IZWFT' )
      maxlev = ip (gtipnt ( 'MAXLEV'))
      maxtab = ip (gtipnt ( 'MAXTAB'))
      mbdpar =     gtipnt ( 'MBDPAR')
      mltpar =     gtrpnt ( 'MLTPAR')
      morcon =     gtrpnt ( 'MORCON')
      morpar =     gtrpnt ( 'MORPAR')
      nboun  = ip (gtipnt ( 'NBOUN' ))
      nbran  = ip (gtipnt ( 'NBRAN' ))
      nbrnod = ip (gtipnt ( 'NBRNOD'))
      nellvl =     gtrpnt ( 'NELLVL')
      ngrid  = ip (gtipnt ( 'NGRID' ))
      nlev   =     gtipnt ( 'NLEV'  )
      nmlat  = ip (gtipnt ( 'NMLAT' ))
      nnelvl = ip (gtipnt ( 'NNELVL'))
      nnode  = ip (gtipnt ( 'NNODE' ))
      node   =     gtipnt ( 'NODE'  )
      nonall =     gtipnt ( 'NONALL')
      nqlat  = ip (gtipnt ( 'NQLAT' ))
      nsedrd = ip (gtipnt ( 'NSEDRD'))
      ntab   =     gtipnt ( 'NTAB'  )
      ntabm  = ip (gtipnt ( 'NTABM' ))
      ntmpgr = ip (gtipnt ( 'NTMPGR'))
      nucoef = ip (gtipnt ( 'NUCOEF'))
      of     =     gtrpnt ( 'OF'    )
      prslot =     gtrpnt ( 'PRSLOT')
      psltvr =     gtrpnt ( 'PSLTVR')
      qlat   =     gtrpnt ( 'QLAT'  )
      qltpar =     gtrpnt ( 'QLTPAR')
      qpack  =     gtdpnt ( 'QPACK')
      rc     =     gtrpnt ( 'RC'    )
      rpack  =     gtrpnt ( 'RPACK' )
      sdrdbf =     gtipnt ( 'SDRDBF')
      sectc  =     gtrpnt ( 'SECTC' )
      sectv  =     gtrpnt ( 'SECTV' )
      seddb  =     gtipnt ( 'SEDDB' )
      sedinf =     gtipnt ( 'SEDINF')
      sedpar =     gtrpnt ( 'SEDPAR')
      sedtr  =     gtrpnt ( 'SEDTR' )
      slat   =     gtrpnt ( 'SLAT'  )
      sumda  =     gtrpnt ( 'SUMDA' )
      table  =     gtrpnt ( 'TABLE' )
      tmpgr  =     gtrpnt ( 'TMPGR' )
      trform =     gtrpnt ( 'TRFORM')
      typcr  =     gtipnt ( 'TYPCR' )
      uscoef =     gtrpnt ( 'USCOEF')
      waoft  =     gtrpnt ( 'WAOFT' )
      wft    =     gtrpnt ( 'WFT'   )
      ws     =     gtrpnt ( 'WS'    )
      wtt    =     gtrpnt ( 'WTT'   )
      x      =     gtrpnt ( 'X'     )
c     h2 = hp(,3)
      h2 = hpack + ngrid * 2
c
c     Determine time N+1
c
      tnp1 = time + dtf
      timnp1(1) = itim(1)
      timnp1(2) = itim(2)
      call sotime ( timnp1, dtf )
      cnt    = 0
c
c     Repeat
c
 100  continue
c
c        Calculate current delta time step
c
         ldtf = tnp1 - time
c
c        Determine courant number and max step morphology module
c
         call mocour (    nbran  ,   ngrid  ,
     +                 ip(branch),ip(typcr) ,
     +                 ip(grid)  ,rp(celer) ,rp(x)     ,
     +                    alphac ,sngl(ldtf),
     +                    m      ,igpmc     )
c
c        Check if time step should be reduced
c
         if (m .gt. 1) then
c
c           Reduce time step
c
           ldtf    = ldtf / m
           time    = time + ldtf
           call sotime ( itim, ldtf )
           cnt     = cnt + 1
           lastts  = .false.
         else
           time    = tnp1
           itim(1) = timnp1(1)
           itim(2) = timnp1(2)
           lastts  = .true.
         endif
c
c        Determine flow direction at each grid point
c
         call SOSDIR( dp(qpack) , ngrid , ip(flwdir) )
c
c        Adapt bottoms
c
         call morph (    ngrid  ,   nbran  ,   nboun  ,   nnode  ,
     +                   nbrnod ,   ntmpgr ,   time   ,   ldtf   ,
     +                rp(morpar),ip(flwdir),rp(sumda) ,
     +                ip(grid)  ,ip(branch),ip(node)  ,ip(typcr) ,
     +                ip(brnode),ip(bgout) ,rp(tmpgr) ,
     +                   maxtab ,   ntabm  ,ip(ntab)  ,rp(table) ,
     +                ip(mbdpar),rp(x)     ,
     +                   maxlev ,ip(nlev)  ,dp(hlev)  ,dp(h2)    ,
     +                rp(waoft) ,rp(wft)   ,rp(ws)    ,
     +                rp(sectc) ,rp(afwfqs),
     +                rp(celer) ,rp(sedtr) ,rp(dissed),rp(slat)  ,
     +                   itim   ,   juer   ,   ker    ,rp(prslot))
c
c        Test for fault
c
         if (ker .eq. fatal) then
            goto 900
         endif
c
c        Calculate derived variables
c
         lsalt = .false.
         call cstabl (    ngrid  ,   maxlev ,ip(nlev)  ,dp(hlev)  ,
     +                 rp(wft)   ,rp(aft)   ,rp(wtt)   ,rp(att)   ,
     +                 rp(of)    ,   lsalt  ,rp(izwft) ,   nbran  ,
     +                 ip(branch),ip(typcr) ,rp(sectc) ,rp(sectv) ,
     +                 rp(prslot),rp(psltvr),ip(bfrict),rp(bfricp),
     +                 rp(engpar),rp(grsize),   maxtab ,   ntabm  ,
     +                 ip(ntab)  ,rp(table) ,rp(ws)    ,   .true. , 
     +                    juer   ,   ker    )
c
c        Test for fault
c
         if (ker .eq. fatal) then
            goto 900
         endif
c
c        Kalman filter is switched off
c
         lkalm  = .false.
         filstp = 0
         cpredn = 0
c        mozart parameters
         lmozad = .false.
         lgrwt  = .false.
         nstepd = 0
c
         call  SOFLOW ( istep  ,time   ,itim   ,ldtf   ,filstp ,
     +                  cpredn ,steady ,lsalt  ,lkalm  ,
c                       mozart parameters
     +                  lmozad ,lgrwt  ,lrest  ,nstepd ,
     +                  juresi ,
     +                  jufrou ,juresd ,justrd ,juer   ,ker    ,
     +                  inocon ,jusold ,lfrou  ,itstat ,frobuf )
c
c        Test for fault
c
         if (ker .eq. fatal) then
            goto 900
         endif
c
c        Sediment step
c
         call sedim (    nbran  ,   nnode  ,   nbrnod ,   nboun  ,
     +                   maxlev ,   nsedrd ,   nnelvl ,
     +                   nqlat  ,   nmlat  ,   ngrid  ,   maxtab ,
     +                   ntabm  ,   juer   ,   itim   ,   time   ,
     +                   ldtf   ,   g      ,   lboun  ,   ntmpgr ,
     +                rp(tmpgr) ,ip(branch),ip(sedinf),ip(bfrict),
     +                ip(nonall),ip(node  ),ip(seddb ),ip(brnode),
     +                ip(bgout ),ip(sdrdbf),ip(mbdpar),
     +                ip(ntab  ),rp(afwfqs),rp(waoft ),rp(ws    ),
     +                dp(hlev  ),ip(nlev  ),rp(wft   ),rp(psltvr),
     +                rp(sectv ),rp(alfab ),rp(rpack ),rp(x     ),
     +                rp(cpack ),dp(qpack ),dp(hpack ),rp(grsize),
     +                rp(forcon),rp(slat  ),rp(sedtr ),rp(celer ),
     +                rp(trform),rp(prslot),rp(dissed),rp(nellvl),
     +                rp(rc    ),rp(e     ),   nucoef ,rp(uscoef),
     +                rp(engpar),rp(sedpar),rp(morcon),rp(mltpar),
     +                rp(qltpar),rp(qlat  ),rp(table ),  .true.  ,
     +                   ker    )
c
c        Test for fault
c
         if (ker .eq. fatal) then
            goto 900
         endif
c
c
c     If last time step not reached, try again
c
      istep = istep + 1
      if ((cnt .lt. moitmx) .and. (.not. lastts) ) then
         goto 100
      else if (cnt .eq. moitmx) then
         ker = fatal
         call error (juer,'SORIMO Morp iteration failed',emrncv,ker)
         istep = istep - 1
      else if (cnt .ge. 1) then
c
c        Inform the user
c
         write (txt,'(i4)') cnt
         call error ( juer,'SORIMO @'//txt//
     +               '@ reductions of morphological time step',
     +                erdmst, info )
         write (txt1,'(2(1x,i8))') itim
         call error (juer,'SORIMO timestep@'//txt1//'@',emomes,info)

      endif
c
 900  continue
c
      end
