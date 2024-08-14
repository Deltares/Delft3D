      subroutine satbpa (nmouth ,nbran ,juer, bramrl ,mouqpu ,thcsum ,
     &                   ker    )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Salt Transport Module
c
c Programmer:         J.Kuipers
c
c Module:             SATBPA (SAlt Thatcher harleman Branch PArameters)
c
c Module description: This subroutine is called when the end of a tidal
c                     period in one or more mouths has been noticed. In
c                     that case for all branches the Thatcher Harleman
c                     sum will be calculated. The next quantities, which
c                     are defined on the last 2 tides, are used:
c                     -   Fresh water discharge in branch per mouth;
c                     -   Flood volume in branch per mouth;
c                     -   Maximum velocity in branch per mouth.
c
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  4 bramrl(nmouth+1,  I  Branch-Mouth relation table. The first index
c        ,nbran)          contains the number of related mouths (index
c                         1). The second index contains the first rela-
c                         ted mouth number etc.
c  3 juer              P  -
c  7 ker               O  Error code:
c                         ok     (0) : No error
c                         info   (1) : Informative message
c                         warnng (2) : Warning
c                         fatal  (3) : Fatal error (processing stops)
c  5 mouqpu(3,0:2,     I  Contains auxilliary data for the Thatcher
c        nmouth)          Harleman or ZWENDL dispersion formulation.
c                         - First index:
c                         (1,,) = Fresh water discharge.
c                         (2,,) = Flood volume.
c                         (3,,) = Maximum flood velocity.
c                         - Second index:
c                         (,0,) = For current tide. Mouqpu(i,0,j) con-
c                                 tains the actual sum or maximum on the
c                                 current time.
c                         (,1,) = For the last tide.
c                         (,2,) = For the tide before the last tide.
c                         - Third index:
c                         (,,i) = Mouth number.
c  2 nbran             I  Number of branches.
c  1 nmouth            I  Maximum number of mouths in the network.
c  6 thcsum(2,nbran)   IO Contains Thatcher-Harleman sum per branch:
c                         (1,i) = Constant part of Thatcher-Harleman
c                                 sum.
c                         (2,i) = Part of Thatcher-Harleman sum that is
c                                 constant in one tidal period.
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c error   write an ERROR to the error file.
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
c $Log: satbpa.pf,v $
c Revision 1.4  1999/06/01  13:42:37  kuipe_j
c names in messages substituted + message template
c
c Revision 1.3  1995/05/30  09:56:17  hoeks_a
c Minor changes
c
c Revision 1.2  1995/05/30  07:06:18  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:09:59  hoeks_a
c Initial check-in
c
c Revision 1.2  1993/11/26  15:34:10  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:44:16  kuipe_j
c Initial version
c
c
c***********************************************************************
c
c     Declaration of parameters
c
      integer nmouth         ,nbran ,juer  ,ker
      integer bramrl(nmouth+1,nbran)
      real    mouqpu(3,0:2,*),thcsum(2,nbran)
c
c     Declaration of local variables
c
      integer   ibr  ,nmj  ,im  ,i   ,lbrnam
      real      q    ,p    ,u   ,d
      character*40    branam
c
c     Include sobek error code file
c
      include '..\include\errcod.i'
c
      do 20 ibr = 1,nbran
         q   = 0.
         p   = 0.
         u   = 0.
         nmj = bramrl(1,ibr)
         do 10 i = 2,nmj+1
            im = bramrl(i,ibr)
            q = q + mouqpu(1,1,im) + mouqpu(1,2,im)
            p = p + mouqpu(2,1,im) + mouqpu(2,2,im)
            u = u + max(mouqpu(3,1,im),mouqpu(3,2,im))
   10    continue
         u = u / real(nmj)
         d = p * u * u
c
c        Check on values of q,p and u.
c
         if (q .le. 0. .or. d .lt. 1.e-10) then
            ker = fatal
            call getbrn (ibr,branam,lbrnam)
            call error (juer,'SATBPA branch @'//branam(:lbrnam)//
     &                  '@',esafvv,ker)
            goto 1000
         endif
         thcsum(2,ibr) = thcsum(1,ibr) * ( q / d) **.25
   20 continue
c
 1000 continue
c
      end
