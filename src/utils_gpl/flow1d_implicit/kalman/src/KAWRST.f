      subroutine KAWRST (fd_nefis_rst, grnamf ,nentri ,np    ,nnf   ,
     &                   nnmu   ,ncelst ,itim   ,nameel ,pmat  ,pfa   ,
     &                   pmua   ,pw     ,neferr )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Kalman module
c
c Programmer:         J.Brouwer/J.Kuipers
c
c Module:             KAWRST (KAlman WRite reSTart information)
c
c Module description: Write the Kalman restart information to the restart
c                     file.
c
c                     The user can restart a simulation run from a
c                     specific point of time. This routine writes the
c                     restart data at a user defined frequency to the
c                     restart file. The restart information contains the
c                     actual correction parameters, covariances and noise
c                     s at the time level the information was saved.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c  2 dafdst            P  -
c  1 defdst            P  -
c  3 grnamf            P  -
c  9 itim              P  -
c 10 nameel            P  -
c  8 ncelst            I  Actual cell number of a restart block of the
c                         restart file.
c 15 neferr            O  Nefis error code:
c                         0   = No error
c                         <0  = Error:  odd  = fatal
c                                       even = non fatal
c  4 nentri            I  Number of entries (possible report paramaters)
c                         in the tables of a block (i.e.nameel).
c  6 nnf               I  Number of uncertain bed friction parameters.
c  7 nnmu              I  Number of uncertain energy loss parameters in
c                         case of free gate flow.
c  5 np                I  Size of covariance matrix (2*ngrid+nnf+nnmu+1)
c 12 pfa               P  -
c 11 pmat              P  -
c 13 pmua              P  -
c 14 pw                I  Uncertain wind stress parameter.
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c flsdat  FLuSh buffers of DATa file
c putiel  PUT Integer ELement to nefis file
c putrel  PUT Real ELement to a nefis file
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: kawrst.pf,v $
c Revision 1.2  1996/04/12  13:05:38  kuipe_j
c headers, minor changes
c
c Revision 1.1  1996/04/11  08:25:12  kuipe_j
c Kalman module added
c
c
c***********************************************************************
c
c     Declaration of parameters
c
      integer       nentri ,np ,nnf ,nnmu ,ncelst ,neferr
      integer       fd_nefis_rst, itim(2)
      real          pmat (np,np)   ,pfa(nnf)       ,pmua(nnmu) ,pw
      character*(*) grnamf
      character*(*) nameel(nentri)
c
c     Declaration of local variables
c
      integer       error
      integer       uindex(3) ,usrord(1)
      real          pwa(1)
c
c     Declaration of external functions
c
      integer       flsdat    ,putrel    ,putiel
      external      flsdat    ,putrel    ,putiel
c
      data          usrord    /1/
c
      uindex(1) = ncelst
      uindex(2) = ncelst
      uindex(3) = 1
c
      error   = putiel (fd_nefis_rst, grnamf ,nameel(1) ,
     &                  uindex  ,usrord ,itim   )
      if (error.ne.0) goto 1000
c
      error = putrel (fd_nefis_rst, grnamf ,nameel(2) ,
     &                uindex  ,usrord ,pmat   )
      if (error.ne.0) goto 1000
c
      error = putrel (fd_nefis_rst, grnamf ,nameel(3) ,
     &                uindex  ,usrord ,pfa    )
      if (error.ne.0) goto 1000
c
      if (nnmu .gt. 0) then
c
         error = putrel (fd_nefis_rst, grnamf ,nameel(4) ,
     &                   uindex  ,usrord ,pmua   )
         if (error.ne.0) goto 1000
      endif
c
      pwa(1) = pw
      error = putrel (fd_nefis_rst, grnamf ,nameel(5) ,
     &                uindex  ,usrord ,pwa    )
      if (error.ne.0) goto 1000
c
      error = flsdat(fd_nefis_rst)
c
 1000 continue
      neferr = error
      end
