      subroutine SOESMO ( time   ,itim   ,steps  ,fp     ,
     +                    dtm    ,lastts ,lsalt  ,lkalm  ,
     +                    nreduc ,juer   ,ker
     +                  )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Main module
c
c Programmer:         S.L. van der Woude
c
c Module:             SOESMO (SObek EStuary MOrphology)
c
c Module description: This routine performs the morphodynamic time step
c                     for an estuary. It is possible that the time step
c                     defined by the user is too large. In that case the
c                     time step will be adapted.
c
c                     First the aggregated sediment transports and cele-
c                     rities are avaraged by routine SOSCAV. The calcu-
c                     lated results is used to determine the courant
c                     number (MOCOUR). This routine delivers an integer
c                     M which is used to determine the maximum time step
c                     to be made by the morphodynamic process. If the
c                     initial time step is too large the reduction will
c                     be as follows:
c
c                     o   division of time step by courant number;
c                     o   rounding down to a whole number of flow steps;
c
c                     The reason for this way of adapting the time step
c                     is the possibility to calculate new tidal periods.
c
c                     After the morphodynamic step (MORPH) has been made
c                     routine CSTABL is called to calculate the derived
c                     cross sectional information.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  5 dtm               IO Morphology time step.
c  4 fp                I  Flow period [s]
c  2 itim(2)           I  Actual time level tn+1 expressed in date and
c                         time. Format (integer):
c                         itim(1) = YYYYMMDD (year,month,day)
c                         itim(2) = HHMMSSHH (hour,minute,second,
c                                   hundredth of a second)
c  9 juer              P  -
c 10 ker               IO Error code:
c                         ok     (0) : No error
c                         info   (1) : Informative message
c                         warnng (2) : Warning
c                         fatal  (3) : Fatal error (processing stops)
c  6 lastts            IO True if last part of a reduced traject has
c                         been calculated
c  7 lsalt             P  -
c  8 nreduc            I  Number of time step reductions
c  3 steps             P  -
c  1 time              IO Actual time level tn+1. in sec.
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c cstabl  Cross Sectional TABLe module
c error   write an ERROR to the error file.
c gtipnt  GeT Integer PoiNTer
c gtrpnt  GeT Real PoiNTer
c mocour  MOrphology COURant number
c morph   MORPHology module
c sorpar  SObek Real PARameter
c soscav  SObek Sediment Celerity AVarage
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
c $Log: soesmo.pf,v $
c Revision 1.18  1999/06/01  13:42:47  kuipe_j
c names in messages substituted + message template
c
c Revision 1.17  1999/03/15  15:19:45  kuipe_j
c tabs removed
c
c Revision 1.16  1998/06/11  11:47:38  kuipe_j
c Estuary special integrated
c
c Revision 1.15  1997/10/28  09:13:28  kuipe_j
c Improve reductions handling estuary morph.
c
c Revision 1.14  1997/02/17  10:09:38  kuipe_j
c Lateral Q in m3/s in cont. equation now
c
c Revision 1.13  1997/01/23  08:30:09  kuipe_j
c Make flow module robust
c
c Revision 1.12  1996/12/02  10:03:47  kuipe_j
c avoid negative pointers
c
c Revision 1.11  1996/09/03  14:33:41  kuipe_j
c frequency time hist, run in time est. morp
c
c Revision 1.10  1996/05/28  13:29:26  kuipe_j
c Error message courant nr added
c
c Revision 1.9  1996/04/12  13:06:02  kuipe_j
c headers, minor changes
c
c Revision 1.8  1996/04/11  08:16:29  kuipe_j
c Kalman module added
c
c Revision 1.7  1996/03/07  10:44:29  kuipe_j
c Bottom scema acc. to Lax Wendroff with flux limitter
c
c Revision 1.6  1995/11/21  11:09:09  kuipe_j
c Added features are: Special morphology output for IVR; Improvement of
c     auxilliary output; Automatic speudo time stepping; general structure
c     improvement (Q-dependent lin, relax. of Q only, changed weir Q-H
c     relation); removal of grid points in messages; etc.
c
c Revision 1.5  1995/10/18  09:00:59  kuipe_j
c Changes concerning aux. ouput and IVR adjustments
c
c Revision 1.4  1995/09/22  10:04:07  kuipe_j
c variable dimensions, new headers
c
c Revision 1.3  1995/05/30  09:56:52  hoeks_a
c Minor changes
c
c Revision 1.2  1995/05/30  07:09:45  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:12:10  hoeks_a
c Initial check-in
c
c Revision 1.4  1994/12/02  13:33:27  kuipe_j
c Improvement of message handling.
c
c Revision 1.3  1994/11/28  08:28:34  kuipe_j
c Time and timestep in double precision.
c
c Revision 1.2  1993/11/26  15:09:45  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:39:42  kuipe_j
c Initial version
c
c
c***********************************************************************
c

c
c     Parameters
c
      double   precision       time   ,fp    ,dtm
      integer  steps ,itim(2) ,nreduc ,juer  ,ker
      logical  lastts,lsalt,lkalm
c
c     Variables
c
      real     alphac,  xc , overlp, g
      double   precision       dtma   , deltat
      integer  ixpar  ,m   ,ibr, igpmc, moitmx, lbrnam
      character*4      txt
      character*18     txt1
      character*11     xtxt
      character*40    branam
c
c     Pointers to memory pool and single variables
c
      integer  adissd ,aft    ,afwfqs ,asedtr ,aslat  ,att    ,
     +         bfrict ,bfricp ,bgout  ,branch ,brnode ,celer  ,dissed ,
     +         engpar ,grid   ,grsize ,hlev   ,izwft  ,maxlev ,maxtab ,
     +         mbdpar ,nboun  ,nbran  ,nbrnod ,ngrid  ,nlev   ,nnode  ,
     +         node   ,ntab   ,ntabm  ,ntmpgr ,nucoef ,of     ,prslot ,
     +         psltvr ,sectc  ,sectv  ,sedtr  ,slat   ,table  ,tmpgr  ,
     +         typcr  ,waoft  ,wft    ,ws     ,wtt    ,x      ,morpar ,
     +         flwdir ,alfab  ,uscoef ,trform ,forcon ,sedpar ,ahpack ,
     +         cpack  ,rpack  ,scifri ,pfa    ,arex   ,arexcn ,sumda  ,
     +         arexop ,nnf    ,hqav
c
c     External functions
c
      integer  gtipnt, gtrpnt, gtdpnt, soipar
      real     sorpar
      external gtipnt, gtrpnt, gtdpnt, sorpar, soipar
c
c     Include memory pool
c
      include '..\include\mempool.i'
      include '..\include\errcod.i'
c
c     Read parameters for morphology module
c
      ixpar  = gtrpnt ( 'MORPAR')
      moitmx = soipar ( rp(ixpar), 2 )
      alphac = sorpar ( rp(ixpar), 3 )
c
      ixpar  = gtrpnt ( 'FLWPAR' )
      g      = sorpar ( rp(ixpar), 1 )
      overlp = sorpar ( rp(ixpar), 16)
c
c     Determine start addresses of arrays in memory pools
c
      adissd =     gtrpnt ( 'ADISSD')
      aft    =     gtrpnt ( 'AFT'   )
      afwfqs =     gtrpnt ( 'AFWFQS')
      asedtr =     gtrpnt ( 'ASEDTR')
      aslat  =     gtrpnt ( 'ASLAT' )
      att    =     gtrpnt ( 'ATT'   )
      bfrict =     gtipnt ( 'BFRICT')
      bfricp =     gtrpnt ( 'BFRICP')
      bgout  =     gtipnt ( 'BGOUT' )
      branch =     gtipnt ( 'BRANCH')
      brnode =     gtipnt ( 'BRNODE')
      celer  =     gtrpnt ( 'CELER' )
      dissed =     gtrpnt ( 'DISSED')
      engpar =     gtrpnt ( 'ENGPAR')
      flwdir =     gtipnt ( 'FLWDIR')
      grid   =     gtipnt ( 'GRID'  )
      grsize =     gtrpnt ( 'GRSIZE')
      hlev   =     gtdpnt ( 'HLEV'  )
      izwft  =     gtrpnt ( 'IZWFT' )
      maxlev = ip (gtipnt ( 'MAXLEV'))
      maxtab = ip (gtipnt ( 'MAXTAB'))
      mbdpar =     gtipnt ( 'MBDPAR')
      morpar =     gtrpnt ( 'MORPAR')
      nboun  = ip (gtipnt ( 'NBOUN' ))
      nbran  = ip (gtipnt ( 'NBRAN' ))
      nbrnod = ip (gtipnt ( 'NBRNOD'))
      ngrid  = ip (gtipnt ( 'NGRID' ))
      nlev   =     gtipnt ( 'NLEV'  )
      nnode  = ip (gtipnt ( 'NNODE' ))
      node   =     gtipnt ( 'NODE'  )
      ntab   =     gtipnt ( 'NTAB'  )
      ntabm  = ip (gtipnt ( 'NTABM' ))
      ntmpgr = ip (gtipnt ( 'NTMPGR'))
      nucoef = ip (gtipnt ( 'NUCOEF'))
      of     =     gtrpnt ( 'OF'    )
      prslot =     gtrpnt ( 'PRSLOT')
      psltvr =     gtrpnt ( 'PSLTVR')
      sectc  =     gtrpnt ( 'SECTC' )
      sectv  =     gtrpnt ( 'SECTV' )
      sedtr  =     gtrpnt ( 'SEDTR' )
      slat   =     gtrpnt ( 'SLAT'  )
      sumda  =     gtrpnt ( 'SUMDA' )
      table  =     gtrpnt ( 'TABLE' )
      tmpgr  =     gtrpnt ( 'TMPGR' )
      typcr  =     gtipnt ( 'TYPCR' )
      waoft  =     gtrpnt ( 'WAOFT' )
      wft    =     gtrpnt ( 'WFT'   )
      ws     =     gtrpnt ( 'WS'    )
      wtt    =     gtrpnt ( 'WTT'   )
      x      =     gtrpnt ( 'X'     )
c
      alfab  =     gtrpnt ( 'ALFAB' )
      sedpar =     gtrpnt ( 'SEDPAR')
      uscoef =     gtrpnt ( 'USCOEF')
      trform =     gtrpnt ( 'TRFORM')
      forcon =     gtrpnt ( 'FORCON')
      ahpack =     gtrpnt ( 'AHPACK')
      cpack  =     gtrpnt ( 'CPACK' )
      rpack  =     gtrpnt ( 'RPACK' )
      scifri =     gtipnt ( 'SCIFRI')
      nnf    =  ip(gtipnt ( 'NNF')  )
      pfa    =     gtrpnt ( 'PFA'   )
      arex   =     gtrpnt ( 'AREX'  )
      arexcn =     gtipnt ( 'AREXCN')
      arexop =     gtipnt ( 'AREXOP')
      hqav   =     gtdpnt ( 'HQAV'  )

c     Average water levels and related hydraulic parameters
c     and calculate bed celerities.
c
      call soscav (    steps   ,   nbran   ,   ngrid   ,ip(flwdir),
     +              rp(celer)  ,rp(sedtr)  ,rp(dissed) ,rp(slat)  ,
     +              rp(asedtr) ,rp(adissd) ,rp(aslat)  ,   juer   ,
     +                 maxlev  ,ip(nlev)   ,dp(hlev)   ,rp(wft)   ,
     +              rp(ws)     ,rp(alfab)  ,rp(prslot) ,ip(branch),
     +              rp(sedpar) ,   g       ,rp(grsize) ,   nucoef ,
     +              rp(uscoef) ,rp(trform) ,rp(forcon) ,rp(engpar),
     +              rp(sectv)  ,rp(ahpack) ,rp(cpack)  ,
     +              rp(rpack)  ,rp(waoft)  ,rp(afwfqs) ,   lkalm  ,
     +              ip(typcr)  ,ip(bfrict) ,rp(bfricp) ,rp(aft)   ,
     +              overlp     ,rp(arex)   ,ip(arexcn) ,ip(arexop),
     +              rp(of)     ,   maxtab  ,   ntabm   ,ip(ntab)  ,
     +              rp(table)  ,rp(psltvr) ,ip(scifri) ,   nnf    ,
     +              rp(pfa)    ,dp(hqav )  ,rp(sectc)  ,rp(wtt)   ,
     +              rp(att)    ,   ker     )
c
      if (ker .eq. fatal) go to 900
c
c     Determine maximum courant number
c
      call mocour (    nbran  ,   ngrid  ,
     +              ip(branch),ip(typcr) ,
     +              ip(grid)  ,rp(celer) ,rp(x)     ,
     +                 alphac ,sngl(dtm) ,
     +                 m      ,igpmc     )
c
c     Check if time step should be reduced
c
      if (m .gt. 1) then
c
c        Adapt morphological time step
c
         dtma = dtm / m
c
c        Round down to a multiple of flow period
c
         if (int(dtma / fp) .eq. 0) then
c
            ker = info
            call getloc (igpmc,ibr,xc)
            write (xtxt,'(f10.2)') xc
            call getbrn (ibr,branam,lbrnam)
            call ERROR (juer,'SOESMO Maximum Courant number at @'
     +                        //branam(:lbrnam)//'@ @'
     +                        //xtxt//'@' ,
     +                        emcour , ker )
            ker = fatal
            call error ( juer, 'SOESMO Dt Morph < 1 * Fp', emtsfp, ker )
            write (txt1,'(2(1x,i8))') itim
            call error (juer,'SOESMO timestep@'//txt1//'@',emomes,info)
            goto 900
         else
            dtma = int (dtma / fp) * fp
         endif
c
c        This is not the last morphological time step
c
         lastts = .false.
      else
c
c        This is the last morphological time step, back on original
c        calculation scheme.
c
         dtma   = dtm
         lastts = .true.
      endif
c
c     Adapt bottoms
c
      call morph (    ngrid  ,   nbran  ,   nboun  ,   nnode  ,
     +                nbrnod ,   ntmpgr ,   time   ,   dtma   ,
     +             rp(morpar),ip(flwdir),rp(sumda) ,
     +             ip(grid)  ,ip(branch),ip(node)  ,ip(typcr) ,
     +             ip(brnode),ip(bgout) ,rp(tmpgr) ,
     +                maxtab ,   ntabm  ,ip(ntab)  ,rp(table) ,
     +             ip(mbdpar),rp(x)     ,
     +                maxlev ,ip(nlev)  ,dp(hlev)  ,dp(hqav)  ,
     +             rp(waoft) ,rp(wft)   ,rp(ws)    ,
     +             rp(sectc) ,rp(afwfqs),
     +             rp(celer) ,rp(sedtr) ,rp(dissed),rp(slat)  ,
     +                itim   ,   juer   ,   ker    ,rp(prslot))
c
c     Calculate derived variables (cstabl)
c
      if (ker .eq. fatal) then
         goto 900
      endif
c
      call cstabl (    ngrid  ,   maxlev ,ip(nlev)  ,dp(hlev)  ,
     +              rp(wft)   ,rp(aft)   ,rp(wtt)   ,rp(att)   ,
     +              rp(of)    ,   lsalt  ,rp(izwft) ,   nbran  ,
     +              ip(branch),ip(typcr) ,rp(sectc) ,rp(sectv) ,
     +              rp(prslot),rp(psltvr),ip(bfrict),rp(bfricp),
     +              rp(engpar),rp(grsize),   maxtab ,   ntabm  ,
     +              ip(ntab)  ,rp(table) ,rp(ws)    ,   .true. ,
     +                 juer   ,   ker    )
c
c     Update time
c
      deltat = dtma - fp
      time   = time + deltat
      call sotime ( itim, deltat )
c      WRITE (*,*) 'Morp step, Reduc=',nreduc
c
      if (nreduc .ge. 1 .and. lastts ) then
         ker = max ( ker, info )
         write (txt,'(i4)') nreduc
         call error ( juer,'SOESMO @'//txt//
     +               '@ reductions of morphological time step',
     +                erdmst, ker )
         write (txt1,'(2(1x,i8))') itim
         call error (juer,'SOESMO timestep@'//txt1//'@',emomes,info)
      else if (nreduc .ge. moitmx) then
         ker = fatal
         call error ( juer,'SOESMO Morp iteration failed',
     +                emrncv, ker )
         write (txt1,'(2(1x,i8))') itim
         call error (juer,'SOESMO timestep@'//txt1//'@',emomes,info)
      endif
c
c     Calculate new morphological time step
c
      dtm = dtm - dtma
c
 900  continue
c
      return
      end
