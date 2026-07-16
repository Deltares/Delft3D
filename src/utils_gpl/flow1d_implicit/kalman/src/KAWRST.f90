subroutine KAWRST (fd_nefis_rst, grnamf ,nentri ,np    ,nnf   ,&
&nnmu   ,ncelst ,itim   ,nameel ,pmat  ,pfa   ,&
&pmua   ,pw     ,neferr )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Kalman module
!
! Programmer:         J.Brouwer/J.Kuipers
!
! Module:             KAWRST (KAlman WRite reSTart information)
!
! Module description: Write the Kalman restart information to the restart
!                     file.
!
!                     The user can restart a simulation run from a
!                     specific point of time. This routine writes the
!                     restart data at a user defined frequency to the
!                     restart file. The restart information contains the
!                     actual correction parameters, covariances and noise
!                     s at the time level the information was saved.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  2 dafdst            P  -
!  1 defdst            P  -
!  3 grnamf            P  -
!  9 itim              P  -
! 10 nameel            P  -
!  8 ncelst            I  Actual cell number of a restart block of the
!                         restart file.
! 15 neferr            O  Nefis error code:
!                         0   = No error
!                         <0  = Error:  odd  = fatal
!                                       even = non fatal
!  4 nentri            I  Number of entries (possible report paramaters)
!                         in the tables of a block (i.e.nameel).
!  6 nnf               I  Number of uncertain bed friction parameters.
!  7 nnmu              I  Number of uncertain energy loss parameters in
!                         case of free gate flow.
!  5 np                I  Size of covariance matrix (2*ngrid+nnf+nnmu+1)
! 12 pfa               P  -
! 11 pmat              P  -
! 13 pmua              P  -
! 14 pw                I  Uncertain wind stress parameter.
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! flsdat  FLuSh buffers of DATa file
! putiel  PUT Integer ELement to nefis file
! putrel  PUT Real ELement to a nefis file
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: kawrst.pf,v $
! Revision 1.2  1996/04/12  13:05:38  kuipe_j
! headers, minor changes
!
! Revision 1.1  1996/04/11  08:25:12  kuipe_j
! Kalman module added
!
!
!***********************************************************************
!
!     Declaration of parameters
!
   integer       nentri ,np ,nnf ,nnmu ,ncelst ,neferr
   integer       fd_nefis_rst, itim(2)
   real          pmat (np,np)   ,pfa(nnf)       ,pmua(nnmu) ,pw
   character*(*) grnamf
   character*(*) nameel(nentri)
!
!     Declaration of local variables
!
   integer       error
   integer       uindex(3) ,usrord(1)
   real          pwa(1)
!
!     Declaration of external functions
!
   integer       flsdat    ,putrel    ,putiel
   external      flsdat    ,putrel    ,putiel
!
   data          usrord    /1/
!
   uindex(1) = ncelst
   uindex(2) = ncelst
   uindex(3) = 1
!
   error   = putiel (fd_nefis_rst, grnamf ,nameel(1) ,&
   &uindex  ,usrord ,itim   )
   if (error.ne.0) goto 1000
!
   error = putrel (fd_nefis_rst, grnamf ,nameel(2) ,&
   &uindex  ,usrord ,pmat   )
   if (error.ne.0) goto 1000
!
   error = putrel (fd_nefis_rst, grnamf ,nameel(3) ,&
   &uindex  ,usrord ,pfa    )
   if (error.ne.0) goto 1000
!
   if (nnmu .gt. 0) then
!
      error = putrel (fd_nefis_rst, grnamf ,nameel(4) ,&
      &uindex  ,usrord ,pmua   )
      if (error.ne.0) goto 1000
   endif
!
   pwa(1) = pw
   error = putrel (fd_nefis_rst, grnamf ,nameel(5) ,&
   &uindex  ,usrord ,pwa    )
   if (error.ne.0) goto 1000
!
   error = flsdat(fd_nefis_rst)
!
1000 continue
   neferr = error
end
