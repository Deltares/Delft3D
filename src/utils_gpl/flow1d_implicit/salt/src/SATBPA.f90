subroutine satbpa (nmouth ,nbran ,juer, bramrl ,mouqpu ,thcsum ,&
&ker    )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Salt Transport Module
!
! Programmer:         J.Kuipers
!
! Module:             SATBPA (SAlt Thatcher harleman Branch PArameters)
!
! Module description: This subroutine is called when the end of a tidal
!                     period in one or more mouths has been noticed. In
!                     that case for all branches the Thatcher Harleman
!                     sum will be calculated. The next quantities, which
!                     are defined on the last 2 tides, are used:
!                     -   Fresh water discharge in branch per mouth;
!                     -   Flood volume in branch per mouth;
!                     -   Maximum velocity in branch per mouth.
!
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  4 bramrl(nmouth+1,  I  Branch-Mouth relation table. The first index
!        ,nbran)          contains the number of related mouths (index
!                         1). The second index contains the first rela-
!                         ted mouth number etc.
!  3 juer              P  -
!  7 ker               O  Error code:
!                         ok     (0) : No error
!                         info   (1) : Informative message
!                         warnng (2) : Warning
!                         fatal  (3) : Fatal error (processing stops)
!  5 mouqpu(3,0:2,     I  Contains auxilliary data for the Thatcher
!        nmouth)          Harleman or ZWENDL dispersion formulation.
!                         - First index:
!                         (1,,) = Fresh water discharge.
!                         (2,,) = Flood volume.
!                         (3,,) = Maximum flood velocity.
!                         - Second index:
!                         (,0,) = For current tide. Mouqpu(i,0,j) con-
!                                 tains the actual sum or maximum on the
!                                 current time.
!                         (,1,) = For the last tide.
!                         (,2,) = For the tide before the last tide.
!                         - Third index:
!                         (,,i) = Mouth number.
!  2 nbran             I  Number of branches.
!  1 nmouth            I  Maximum number of mouths in the network.
!  6 thcsum(2,nbran)   IO Contains Thatcher-Harleman sum per branch:
!                         (1,i) = Constant part of Thatcher-Harleman
!                                 sum.
!                         (2,i) = Part of Thatcher-Harleman sum that is
!                                 constant in one tidal period.
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! error   write an ERROR to the error file.
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
! $Log: satbpa.pf,v $
! Revision 1.4  1999/06/01  13:42:37  kuipe_j
! names in messages substituted + message template
!
! Revision 1.3  1995/05/30  09:56:17  hoeks_a
! Minor changes
!
! Revision 1.2  1995/05/30  07:06:18  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:09:59  hoeks_a
! Initial check-in
!
! Revision 1.2  1993/11/26  15:34:10  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:44:16  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Declaration of parameters
!
   integer nmouth         ,nbran ,juer  ,ker
   integer bramrl(nmouth+1,nbran)
   real    mouqpu(3,0:2,*),thcsum(2,nbran)
!
!     Declaration of local variables
!
   integer   ibr  ,nmj  ,im  ,i   ,lbrnam
   real      q    ,p    ,u   ,d
   character*40    branam
!
!     Include sobek error code file
!
   include '..\include\errcod.i'
!
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
!
!        Check on values of q,p and u.
!
      if (q .le. 0. .or. d .lt. 1.e-10) then
         ker = fatal
         call getbrn (ibr,branam,lbrnam)
         call error (juer,'SATBPA branch @'//branam(:lbrnam)//&
         &'@',esafvv,ker)
         goto 1000
      endif
      thcsum(2,ibr) = thcsum(1,ibr) * ( q / d) **.25
20 continue
!
1000 continue
!
end
