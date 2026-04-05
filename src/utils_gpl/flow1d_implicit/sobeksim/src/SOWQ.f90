subroutine SOWQ ( laux  ,juer   ,ker )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Main module
!
! Programmer:         S.L. van der Woude
!
! Module:             SOWQ (SObek Water Quality)
!
! Module description: If the application includes water quality this
!                     routine will be called.
!
!                     First the water quantity information is converted
!                     to delwaq format. After this the water quality
!                     module is called by starting delwaq.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  3 juer              P  -
!  4 ker               P  -
!  2 laux              P  -
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! gtcpnt  GeT Character PoiNTer
! gtipnt  GeT Integer PoiNTer
! gtrpnt  GeT Real PoiNTer
! wqint   Water Quality INTerface
!=======================================================================
!
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: sowq.pf,v $
! Revision 1.5  1998/02/13  13:23:49  kuipe_j
! Adapt to CMT
!
! Revision 1.4  1995/09/22  10:04:38  kuipe_j
! variable dimensions, new headers
!
! Revision 1.3  1995/05/30  09:57:09  hoeks_a
! Minor changes
!
! Revision 1.2  1995/05/30  07:10:04  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:12:29  hoeks_a
! Initial check-in
!
! Revision 1.2  1993/11/26  15:10:10  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:39:46  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!
!     Parameters
!
   logical       laux
   integer       juer   ,ker
!
!     Variables
!
   integer afwfqs ,agrnam ,branch ,brnode ,cpack, fd_nefis_waq,&
   &exdef  ,tofrom ,nbran  ,nbrnod ,nexdef ,&
   &ngrid  ,nnode  ,npntr  ,nposeg ,nqlat  ,nsegmt ,&
   &nsegtb ,pntr   ,qaggr  ,qlaggr ,qltpar ,segfun ,&
   &segmnt ,segtab ,sexar  ,sexfl  ,slen   ,svol   ,&
   &waoft  ,x
   character*120  agrname
   integer i
!
!     External functions
!
   integer       gtcpnt, gtipnt, gtrpnt
   external      gtcpnt, gtipnt, gtrpnt
!
!     Include memory pool
!
   include '..\include\mempool.i'
!
!     Assign addresses
!
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
!
!     Read pointers of file descriptors, do not open the files
!
   fd_nefis_waq  =     gtipnt ( 'FD_NEFIS_WAQ' )
!
!     Call the water quality interface module
!
   do i = 1, 120
      write(agrname(i:i), '(a)') cp(agrnam-1+i)
   enddo

   call wqint ( agrname, ip(fd_nefis_waq),&
   &laux   ,   nsegmt ,ip(segmnt),   nsegtb ,&
   &rp(segtab),   npntr  ,   nposeg ,ip(pntr)  ,&
   &nexdef ,rp(exdef) ,   nbran  ,ip(branch),&
   &nbrnod ,   nnode  ,ip(brnode),   ngrid  ,&
   &nqlat  ,rp(qltpar),rp(waoft) ,rp(afwfqs),&
   &rp(cpack) ,rp(qaggr) ,rp(qlaggr),rp(x)     ,&
   &rp(tofrom),rp(segfun),rp(sexar) ,rp(sexfl) ,&
   &rp(slen)  ,rp(svol)  ,   juer   ,   ker    )
!
   return
end
