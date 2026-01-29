subroutine KAREST (fd_nefis_rst, grnamf ,nentri ,np     ,nnf   ,&
&nnmu   ,ncelst ,nameel ,p1     ,pfa    ,pmua  ,&
&pw     ,ker    )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Kalman module
!
! Programmer:         J.Brouwer/J.Kuipers
!
! Module:             KAREST (KAlman REad reSTart information)
!
! Module description: Read the Kalman restart information from the restart
!                     file.
!
!                     When the user restarts a simulation run from a
!                     specific point of time this routine will read the
!                     saved restart information from the previous run.
!                     The restart information contains the actual
!                     correction parameters, covariances and noises at
!                     the time level the information was saved.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  2 dafdst            P  -
!  1 defdst            P  -
!  3 grnamf            P  -
! 14 ker               O  Error code:
!                         ok     (0) : No error
!                         info   (1) : Informative message
!                         warnng (2) : Warning
!                         fatal  (3) : Fatal error (processing stops)
!  9 nameel            P  -
!  8 ncelst            I  Actual cell number of a restart block of the
!                         restart file.
!  4 nentri            I  Number of entries (possible report paramaters)
!                         in the tables of a block (i.e.nameel).
!  6 nnf               I  Number of uncertain bed friction parameters.
!  7 nnmu              I  Number of uncertain energy loss parameters in
!                         case of free gate flow.
!  5 np                I  Size of covariance matrix (2*ngrid+nnf+nnmu+1)
! 10 p1                P  -
! 11 pfa               P  -
! 12 pmua              P  -
! 13 pw                I  Uncertain wind stress parameter.
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! getrel  GET Real ELement from a nefis file
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: karest.pf,v $
! Revision 1.3  1999/03/15  15:52:09  kuipe_j
! tabs removed
!
! Revision 1.2  1996/04/12  13:05:19  kuipe_j
! headers, minor changes
!
! Revision 1.1  1996/04/11  08:24:51  kuipe_j
! Kalman module added
!
!
!***********************************************************************
!
!     Declaration of parameters
!
   integer       nentri ,np ,nnf ,nnmu ,ncelst ,ker
   integer       fd_nefis_rst
   real          p1(np,np)      ,pfa(nnf)       ,pmua(nnmu) ,pw
   character*(*) grnamf
   character*(*) nameel(nentri)
!
!     Declaration of local variables
!
   integer       error     ,buflen
   integer       uindex(3) ,usrord(1)
   real          pwa(1)
!
!     Declaration of external functions
!
   integer       getrel
   external      getrel
!
   data          usrord    /1/
!
   uindex(1) = ncelst
   uindex(2) = ncelst
   uindex(3) = 1
!
   buflen = np*np*4
   error = getrel (fd_nefis_rst, grnamf ,nameel(2) ,&
   &uindex  ,usrord ,buflen ,p1     )
   if (error.ne.0) goto 1000
!
   buflen = nnf*4
   error = getrel (fd_nefis_rst, grnamf ,nameel(3) ,&
   &uindex  ,usrord ,buflen ,pfa    )
   if (error.ne.0) goto 1000
!
   if (nnmu .gt. 0) then
!
      buflen = nnmu*4
      error = getrel (fd_nefis_rst, grnamf ,nameel(4) ,&
      &uindex  ,usrord ,buflen ,pmua    )
      if (error.ne.0) goto 1000
   endif
!
   buflen = 4
   pwa(1) = pw
   error = getrel (fd_nefis_rst, grnamf ,nameel(5) ,&
   &uindex  ,usrord ,buflen ,pwa     )
   if (error.ne.0) goto 1000
!
   goto 1010
!
1000 continue
   ker = error
1010 continue
end
