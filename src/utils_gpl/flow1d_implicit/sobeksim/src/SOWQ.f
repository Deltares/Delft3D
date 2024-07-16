      subroutine SOWQ ( laux  ,juer   ,ker )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Main module
c
c Programmer:         S.L. van der Woude
c
c Module:             SOWQ (SObek Water Quality)
c
c Module description: If the application includes water quality this
c                     routine will be called.
c
c                     First the water quantity information is converted
c                     to delwaq format. After this the water quality
c                     module is called by starting delwaq.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  3 juer              P  -
c  4 ker               P  -
c  2 laux              P  -
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c gtcpnt  GeT Character PoiNTer
c gtipnt  GeT Integer PoiNTer
c gtrpnt  GeT Real PoiNTer
c wqint   Water Quality INTerface
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
c $Log: sowq.pf,v $
c Revision 1.5  1998/02/13  13:23:49  kuipe_j
c Adapt to CMT
c
c Revision 1.4  1995/09/22  10:04:38  kuipe_j
c variable dimensions, new headers
c
c Revision 1.3  1995/05/30  09:57:09  hoeks_a
c Minor changes
c
c Revision 1.2  1995/05/30  07:10:04  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:12:29  hoeks_a
c Initial check-in
c
c Revision 1.2  1993/11/26  15:10:10  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:39:46  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c
c     Parameters
c
      logical       laux
      integer       juer   ,ker
c
c     Variables
c
      integer afwfqs ,agrnam ,branch ,brnode ,cpack, fd_nefis_waq,
     +        exdef  ,tofrom ,nbran  ,nbrnod ,nexdef ,
     +        ngrid  ,nnode  ,npntr  ,nposeg ,nqlat  ,nsegmt ,
     +        nsegtb ,pntr   ,qaggr  ,qlaggr ,qltpar ,segfun ,
     +        segmnt ,segtab ,sexar  ,sexfl  ,slen   ,svol   ,
     +        waoft  ,x
      character*120  agrname
      integer i
c
c     External functions
c
      integer       gtcpnt, gtipnt, gtrpnt
      external      gtcpnt, gtipnt, gtrpnt
c
c     Include memory pool
c
      include '..\include\mempool.i'
c
c     Assign addresses
c
      agrnam  =     gtcpnt ( 'AGRNAM' )
      afwfqs  =     gtrpnt ( 'AFWFQS' )
      branch  =     gtipnt ( 'BRANCH' )
      brnode  =     gtipnt ( 'BRNODE' )
      cpack   =     gtrpnt ( 'CPACK'  )
      exdef   =     gtrpnt ( 'EXDEF'  )
      tofrom  =     gtrpnt ( 'TOFROM' )
      nbran   = ip (gtipnt ( 'NBRAN'  ))
      nbrnod  = ip (gtipnt ( 'NBRNOD' ))
      nexdef  = ip (gtipnt ( 'NEXDEF' ))
      ngrid   = ip (gtipnt ( 'NGRID'  ))
      nnode   = ip (gtipnt ( 'NNODE'  ))
      npntr   = ip (gtipnt ( 'NPNTR'  ))
      nposeg  = ip (gtipnt ( 'NPOSEG' ))
      nqlat   = ip (gtipnt ( 'NQLAT'  ))
      nsegmt  = ip (gtipnt ( 'NSEGMT' ))
      nsegtb  = ip (gtipnt ( 'NSEGTB' ))
      pntr    =     gtipnt ( 'PNTR'   )
      qaggr   =     gtrpnt ( 'QAGGR'  )
      qlaggr  =     gtrpnt ( 'QLAGGR' )
      qltpar  =     gtrpnt ( 'QLTPAR' )
      segfun  =     gtrpnt ( 'SEGFUN' )
      segmnt  =     gtipnt ( 'SEGMNT' )
      segtab  =     gtrpnt ( 'SEGTAB' )
      sexar   =     gtrpnt ( 'SEXAR'  )
      sexfl   =     gtrpnt ( 'SEXFL'  )
      slen    =     gtrpnt ( 'SLEN'   )
      svol    =     gtrpnt ( 'SVOL'   )
      waoft   =     gtrpnt ( 'WAOFT'  )
      x       =     gtrpnt ( 'X'      )
c
c     Read pointers of file descriptors, do not open the files
c
      fd_nefis_waq  =     gtipnt ( 'FD_NEFIS_WAQ' )
c
c     Call the water quality interface module
c
      do i = 1, 120
         write(agrname(i:i), '(a)') cp(agrnam-1+i)
      enddo

      call wqint ( agrname, ip(fd_nefis_waq),
     +                laux   ,   nsegmt ,ip(segmnt),   nsegtb ,
     +             rp(segtab),   npntr  ,   nposeg ,ip(pntr)  ,
     +                nexdef ,rp(exdef) ,   nbran  ,ip(branch),
     +                nbrnod ,   nnode  ,ip(brnode),   ngrid  ,
     +                nqlat  ,rp(qltpar),rp(waoft) ,rp(afwfqs),
     +             rp(cpack) ,rp(qaggr) ,rp(qlaggr),rp(x)     ,
     +             rp(tofrom),rp(segfun),rp(sexar) ,rp(sexfl) ,
     +             rp(slen)  ,rp(svol)  ,   juer   ,   ker    )
c
      return
      end
