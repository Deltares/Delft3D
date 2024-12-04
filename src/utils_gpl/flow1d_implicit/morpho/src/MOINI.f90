subroutine moini (ngrid  ,nbran  ,nmlat  ,maxlev ,itim   ,&
&branch ,mltpar ,hlev   ,hlev0  ,grid   ,&
&newres  ,fd_nefis_rst, fd_nefis_new, juer,&
&ncelmo ,ker    ,morini ,sumda  ,lgrad  )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Morphology module
!
! Programmer:         S.L. van der Woude
!
! Module:             MOINI (MOrphology INItialisation)
!
! Module description: Initialise datastructures for the sediment trans-
!                     port module.
!
!                     Before the morphology routine can be activated
!                     first the datastructures have to be initialised.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  6 branch            P  -
! 14 dafdrn            P  -
! 12 dafdst            P  -
! 13 defdrn            P  -
! 11 defdst            P  -
!  8 hlev              P  -
!  5 itim              P  -
! 15 juer              P  -
! 17 ker               O  Error code:
!                         ok     (0) : No error
!                         info   (1) : Informative message
!                         warnng (2) : Warning
!                         fatal  (3) : Fatal error (processing stops)
!  4 maxlev            I  Maximum+1 over nlev(1:ngrid); needed for
!                         declarations
!  2 nbran             I  Number of branches.
! 16 ncelmo            P  -
! 10 newres            P  -
!  1 ngrid             I  Number of grid points in network.
!  3 ntmpgr            I  Number of scratch arrays with length ngrid
!                         that are packed in tmpgr.
!  9 tmpgr             P  -
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! morsta  MOrphology read or write ReSTart information
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: moini.pf,v $
! Revision 1.8  1997/06/17  11:26:57  kuipe_j
! output in history format
!
! Revision 1.7  1997/02/17  10:23:15  kuipe_j
! Lateral Q in m3/s in cont. equation now
!
! Revision 1.6  1997/01/23  08:29:48  kuipe_j
! Make flow module robust
!
! Revision 1.5  1996/01/17  13:18:21  kuipe_j
! header update
!
! Revision 1.4  1996/01/16  15:01:37  kuipe_j
! Restart improvements
!
! Revision 1.3  1995/05/30  09:55:52  hoeks_a
! Minor changes
!
! Revision 1.2  1995/05/30  07:04:48  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:09:16  hoeks_a
! Initial check-in
!
! Revision 1.2  1993/11/26  15:32:44  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:44:08  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Parameters
!
   integer   maxlev, ngrid, nbran, nmlat, itim(2), juer, ker
   integer   ncelmo(3)

   integer   fd_nefis_rst, fd_nefis_new,&
   &branch (4,nbran),&
   &morini(*)       ,&
   &grid(ngrid)

   real      mltpar(9,nmlat),&
   &sumda  (ngrid)        ,hlev0 (ngrid)

   double precision hlev   (ngrid,maxlev)

   logical   inires, first, newres, lgrad
!
   integer   ifil,i
!
!     Include sobek error file
!
   include '..\include\errcod.i'
!
!     Check and update administration of lateral stations
!
   call CHLATA (ngrid  ,nmlat ,nbran, mltpar ,grid  ,branch)
!
   ker   = ok
   first = .true.
!
!     Set initial flags for HIS files
!
   do ifil=1,2
      morini(ifil)=0
   enddo
   do i=1,ngrid
      sumda(i) = 0.0
      hlev0(i) = min(hlev(i,1), hlev(i,2))
   enddo
!
   call morsta ( ngrid , maxlev, itim  ,&
   &hlev  , newres, fd_nefis_rst, fd_nefis_new,&
   &juer  , first , ncelmo(3)     ,&
   &inires, ker   , lgrad )

   return
end

