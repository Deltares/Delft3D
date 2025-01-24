subroutine morsta ( ngrid , maxlev, itim  ,&
&hlev  , newres, fd_nefis_rst, fd_nefis_new,&
&juer   , first , ncelst, inires,&
&ker   ,lgrad  )

!=======================================================================
!            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
!                One Dimensional Modelling System
!                           S O B E K
!-----------------------------------------------------------------------
! Subsystem:          Morphology module
!
! Programmer:         S.L. van der Woude
!
! Module:             MORSTA (MOrphology read or write ReSTart information)
!
! Module description: Read or write the restart information from or to
!                     the restart file.
!
!                     When the user restarts a simulation run from a
!                     specific point of time this routine will read the
!                     saved restart information from the previous run.
!                     The restart information contains the actual adap-
!                     ted cross sections on the time level the informa-
!                     tion was saved.
!
!-----------------------------------------------------------------------
! Parameters:
! NR NAME              IO DESCRIPTION
!  8 branch            P  -
! 14 dafdrn            P  -
! 12 dafdst            P  -
! 13 defdrn            P  -
! 11 defdst            P  -
! 16 first             I  True in case of first call.
!  6 hlev              P  -
! 18 inires            I  True when no restart info of this module has
!                         been written before.
!  5 itim              P  -
! 15 juer              P  -
! 19 ker               O  Error code:
!                         ok     (0) : No error
!                         info   (1) : Informative message
!                         warnng (2) : Warning
!                         fatal  (3) : Fatal error (processing stops)
!  2 maxlev            I  Maximum+1 over nlev(1:ngrid); needed for
!                         declarations
!  3 nbran             I  Number of branches.
! 17 ncelst            IO Actual cell number of a restart block of the
!                         restart file.
! 10 newres            I  true, if a new restart file will be made
!  1 ngrid             I  Number of grid points in network.
!  7 nlev              P  -
!  4 ntmpgr            I  Number of scratch arrays with length ngrid
!                         that are packed in tmpgr.
!  9 tmpgr             P  -
!-----------------------------------------------------------------------
! Subprogram calls:
! NAME    DESCRIPTION
! error   write an ERROR to the error file.
! modfst  MOrphology DeFine group for reSTart information
! morest  MOrphology REading of reSTart information
! mowrst  MOrphology WRiting of reSTart information
! statim  Search resTArt TIMe point
!=======================================================================
!
!
!***********************************************************************
! CVS log information:
!
! $Id$
!
! History:
! $Log: morsta.pf,v $
! Revision 1.11  1999/07/23  15:10:29  kuipe_j
! improve restart
!
! Revision 1.10  1999/03/15  15:53:05  kuipe_j
! tabs removed
!
! Revision 1.9  1997/02/17  10:23:18  kuipe_j
! Lateral Q in m3/s in cont. equation now
!
! Revision 1.8  1997/01/23  08:29:51  kuipe_j
! Make flow module robust
!
! Revision 1.7  1996/12/03  08:29:25  kuipe_j
! dimension of element names improved
!
! Revision 1.6  1996/01/17  13:18:23  kuipe_j
! header update
!
! Revision 1.5  1996/01/16  15:01:38  kuipe_j
! Restart improvements
!
! Revision 1.4  1995/10/18  09:00:05  kuipe_j
! Changes concerning aux. ouput and IVR adjustments
!
! Revision 1.3  1995/05/30  09:55:57  hoeks_a
! Minor changes
!
! Revision 1.2  1995/05/30  07:05:00  hoeks_a
! file changed from dos to ux
!
! Revision 1.1  1995/04/13  07:09:26  hoeks_a
! Initial check-in
!
! Revision 1.2  1993/11/26  15:33:00  kuipe_j
! Update after finishing Sobeksel.
!
! Revision 1.1.1.1  1993/07/21  14:44:09  kuipe_j
! Initial version
!
!
!***********************************************************************
!
!     Parameters
!
   integer       ngrid, maxlev, itim(2),&
   &juer, ncelst, ker

   integer       fd_nefis_rst, fd_nefis_new

   double precision hlev (ngrid,maxlev)

   logical       first, inires ,newres ,lgrad
!
!     Local variables
!
   integer       nentri
   parameter    (nentri=2)
   integer       errr, nrerr, i
   integer       ndim (nentri)
   character*16  grnams
   character*16  nameel(nentri), quanel(nentri), unitel(nentri),&
   &nameac(nentri+1)
   character*64  descel(nentri)
   character*8   txt
   logical       inidum, lees
!
!     Include sobek error code file
!
   include '..\include\errcod.i'
!
!     Definition of elements
!
   data (ndim(i),   descel(i),  nameel(i),  quanel(i),&
   &unitel(i), i=1,nentri) /&
!
   &1, 'Restart time step'   ,'ISTEP'   ,'t'   ,'-' ,&
   &2, 'H-levels'            ,'HLEV'    ,'H'   ,'m' /
!
   data grnams /&
   &'MORP-RES-GROUP' /
!
   errr = 0
!
   if (first) then
!
!        Determine if is an initial run of a restart run for this
!        module. In the latter case restart information must exist
!        already.
!
      nrerr = emobor
      call modfst (fd_nefis_rst, grnams, nentri, ngrid , maxlev,&
      &ndim  , nameel, quanel, unitel, descel,&
      &.not.newres   , inires, nameac, errr  )
      if (errr .ne. 0) goto 1000

      if (inires) then
         if (newres) then
!
!              Initialize a new restart file
!
            nrerr = emobor
            call modfst (fd_nefis_new, grnams, nentri, ngrid ,&
            &maxlev, ndim  , nameel, quanel, unitel,&
            &descel, .true., inidum, nameac, errr  )
            if (errr .ne. 0) goto 1000
         endif
         ncelst = 1
      else
!
!           Read from restart file. Select proper time step first
!

         nrerr = emorrd
         call statim (fd_nefis_rst, grnams, nameel(1), itim,&
         &ncelst, errr  )
         lees = .true.
         if (errr .ne. 0) then
            if (lgrad) then
               call error (juer, 'MORSTA', egrest, info )
               errr = 0
               lees = .false.
            else
               goto 1000
            endif
         endif
!
         if (lees)&
         &call morest (fd_nefis_rst, grnams, nentri,&
         &ngrid , maxlev, hlev  ,&
         &ncelst, nameel,&
         &errr&
         &)
!
         if (errr .ne. 0) goto 1000
!
         if (newres) then
!
!              Initialize a new restart file
!
            nrerr = emobor
            call modfst (fd_nefis_rst, grnams, nentri, ngrid ,&
            &maxlev, ndim  , nameel, quanel, unitel,&
            &descel, .true., inidum, nameac, errr  )
            if (errr .ne. 0) goto 1000
            ncelst = 1
         else
!
!              Continue with existing restart file
!
            ncelst = ncelst + 1
         endif
      endif
   else
!
!        Write to restart file
!
      nrerr = emowrd
      call mowrst (fd_nefis_rst, grnams, nentri,&
      &ngrid , maxlev, hlev  , ncelst, itim  ,&
      &nameel, errr&
      &)
      if (errr .ne. 0) goto 1000
      ncelst = ncelst + 1
   endif

   return

1000 continue
!
   if (errr .gt. 0) then
!
!        Could not find restart time step
!
      ker = fatal
      call error (juer, 'MORSTA', emortt, ker )
   else
!
!        Nefis error (<0)
!
      ker = fatal
      write(txt,'(i8)') errr
      call error (juer, 'MORSTA @'//txt//'@', nrerr, ker)
   endif

   return
end
