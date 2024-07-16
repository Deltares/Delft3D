      subroutine momor (fd_nefis_res, nmoman, nmomap, nmotim, ngrid ,
     +                   nbran , itim  , istep , nstep , first , writim,
     +                   juer  , mormap, mortim, maxlev, hlev  , sedtr ,
     +                   dissed, branch, typcr , ncelm , ncelh , mocpre,
     +                   sectc , sectv , wft   , sumda , hlev0 , buf   ,
     +                   ker   )

c=======================================================================
c            Rijkswaterstaat/RIZA and DELFT HYDRAULICS
c                One Dimensional Modelling System
c                           S O B E K
c-----------------------------------------------------------------------
c Subsystem:          Morphology module
c
c Programmer:         S.L. van der Woude
c
c Module:             MOMOR (MOrphology MOrphology Results)
c
c Module description: Subroutine MOMOR writes the user selected morholo-
c                     gy results to the result file. The result file is
c                     processed by the User Interface.
c
c                     In subroutine MOMOR the user selected salt results
c                     at user supplied locations and time levels (System
c                     Specifications for 1D Modelling System Front End)
c                     will be stored on the result file. The stored data
c                     can be processed further by the User Interface.
c                     The user can select functions of place and of
c                     time.
c
c-----------------------------------------------------------------------
c Parameters:
c NR NAME              IO DESCRIPTION
c 20 branch(4,nbran)   I  Branch information:
c                         (1,i) = Node number n1 at begin of branch i.
c                         (2,i) = Node number n2 at end of branch i.
c                         (3,i) = Grid point i1 at begin of branch i.
c                         (4,i) = Grid point i2 at end of branch i.
c 24 buf(ngrid)        O  Buffer for results to be written to NEFIS.
c  2 dafdrs            P  -
c  1 defdrs            P  -
c 19 dissed(4,nbran)   I  Redistributed sediment transport at begin and
c                         end of branches. At the outflow side of the
c                         branch the calculated transports are stored.
c                         At the inflow side the redistributed trans-
c                         ports are stored.
c                         (1,i)   Transport at section 1 (main or left
c                                 channel)  at begin of branch.
c                         (2,i)   Transport at section 2 (right channel)
c                                 at begin of branch.
c                         (3,i)   Transport at section 1 (main or left
c                                 channel)  at end of branch.
c                         (4,i)   Transport at section 2 (right channel)
c                                 at end of branch.
c 11 first             I  True in case of first call.
c 17 hlev(ngrid,       I  (i,j) = H at level j in cross section i.
c       maxlev)           - For a circle cross section:
c                         (i,1) = Reference level.
c                         - For a sedredge cross section:
c                         (i,1) = Bed level of main section (i.e. left
c                                 channel).
c                         (i,2) = Bed level of sub section 1 (i.e. right
c                                 channel).
c  9 istep             I  Current time step number (t(n+1)).
c  8 itim              P  -
c 13 juer              P  -
c 25 ker               O  Error code:
c                         ok     (0) : No error
c                         info   (1) : Informative message
c                         warnng (2) : Warning
c                         fatal  (3) : Fatal error (processing stops)
c 16 maxlev            I  Maximum+1 over nlev(1:ngrid); needed for
c                         declarations
c 23 mocpre            I  Reference table for main codes off output
c                         system
c 14 mormap(nmomap)    I  parameter list for MAP block with morphology
c                         results:
c                         (1)      = Report begin time
c                         (2)      = Report end time
c                         (3)      = Report time step
c                         (4)      = Report parameter 1 main code
c                         (5)      = Report parameter 1 sub code
c                         (i)      = Report parameter 1 main code
c                         (i+1)    = Report parameter 1 sub code
c                         (nmomap) = Report parameter n sub code
c 15 mortim(nmotim)    I  Parameter list for HIST block with morphology
c                         results:
c                         (1)      = Number of places
c                         (2)      = Report grid point 1
c                         (3)      = Report grid point 2
c                         (i)      = Report grid point n
c                         (i+1)    = Report parameter code 1
c                         (i+2)    = Report parameter sub code 1
c                         (nmotim) = Report parameter n sub code
c  7 nbran             I  Number of branches.
c 22 ncelh             I  Actual cell number of a history block of re-
c                         sult file.
c 21 ncelm             I  Actual cell number of a map block of result
c                         file.
c  6 ngrid             I  Number of grid points in network.
c  3 nmoman            I  Number of main codes morphology module
c  4 nmomap            I  Number of entries in sedmap.
c  5 nmotim            I  Number of entries in sedtim.
c 10 nstep             I  Last time step number in simulation.
c 18 sedtr(ngrid,*)    I  Sediment transport results for each gridpoint.
c               1|2       (At first transports per unit width, finaly
c                         total transports)
c                         - Normal branches:
c                         (i,1) = Sediment transport in gridpoint i of
c                                 main section.
c                         - Sedredge branches:
c                         (i,1) = Sediment transport in gridpoint i,
c                                 left channel.
c                         (i,2) = Sediment transport in gridpoint i,
c                                 right channel.
c                         (i,3) = Sediment exchange in gridpoint i.
c 12 writim            P  -
c-----------------------------------------------------------------------
c Subprogram calls:
c NAME    DESCRIPTION
c error   write an ERROR to the error file.
c flsdat  FLuSh buffers of DATa file
c mofdbr  MOrphology FinD BRanch number for a gridpoint
c putrel  PUT Real ELement to a nefis file
c resadm  RESults; make ADMinistration for element selection
c resdes  RESults; DEScription group is defined
c resini  RESults; writing is INItialized
c restim  RESults; writing of current TIMe
c yesmap  YES MAP results are written
c=======================================================================
c
c
c***********************************************************************
c CVS log information:
c
c $Id$
c
c History:
c $Log: momor.pf,v $
c Revision 1.8  1999/03/15  15:52:58  kuipe_j
c tabs removed
c
c Revision 1.7  1997/06/17  11:26:58  kuipe_j
c output in history format
c
c Revision 1.6  1997/02/17  10:23:16  kuipe_j
c Lateral Q in m3/s in cont. equation now
c
c Revision 1.5  1997/01/23  08:29:49  kuipe_j
c Make flow module robust
c
c Revision 1.4  1996/09/03  14:48:49  kuipe_j
c frequency time hist,Improved sed distribution at nodes
c
c Revision 1.3  1995/05/30  09:55:55  hoeks_a
c Minor changes
c
c Revision 1.2  1995/05/30  07:04:53  hoeks_a
c file changed from dos to ux
c
c Revision 1.1  1995/04/13  07:09:21  hoeks_a
c Initial check-in
c
c Revision 1.3  1995/03/08  09:21:01  kuipe_j
c Improvement message Momor
c Improvement shock celerity
c
c Revision 1.2  1993/11/26  15:32:52  kuipe_j
c Update after finishing Sobeksel.
c
c Revision 1.1.1.1  1993/07/21  14:44:08  kuipe_j
c Initial version
c
c
c***********************************************************************
c
      include '..\include\sobcon.i'
      include '..\include\sobdim.i'
c
c     Parameters
c
      integer       nmoman, nmomap, nmotim, nbran, ngrid, istep ,
     +              nstep , maxlev, juer  , ncelm , ncelh, ker
      logical       first , writim
      integer       fd_nefis_res,
     +              itim   (2),
     +              mormap (nmomap),
     +              mortim (nmotim),
     +              mocpre (nmoman),
     +              branch (4,nbran) , typcr (ngrid)

      real          wft(ngrid,maxlev),
     +              sedtr  (ngrid,*),
     +              dissed (4,nbran),
     +              buf    (ngrid)  ,sumda (ngrid) ,hlev0(ngrid),
     +              sectc  (ngrid,3),sectv (ngrid,dmsecv)

      double precision hlev   (ngrid,maxlev)

c
c     Local variables
c
      integer       nentri
      parameter    (nentri=6)
      integer       neferr, i, j, k, l, ie, ie1, ie2, nrerr, nsk, nlc,
     &              lastcod, igrid 
      integer       usrord(1), uindex(3)
      character*16  grnamm, grnamh, grnamd, name, tmpnam
      character*16  nameel(nentri), quanel(nentri), unitel(nentri),
c
c     For this module nameac must be as large as possible because
c     HIS_Z0 will be extended to 'maxlev' plus HIS_Zb
c
     +              nameac(100)
      character*64  descel(nentri)
      character*2   txt
      character*8   txt2
      logical       llog, new   ,newuit
      integer       ibr
c
c     Include sobek error code file
c
      include '..\include\errcod.i'
c
c     External functions
c
      integer       putrel, flsdat
      logical       yesmap
      external      putrel, flsdat, yesmap
c
      data          usrord /1/
c
c     Definition of elements
c
      data  (descel(i),  nameel(i),  quanel(i),
     +      unitel(i), i=1,nentri) /
c
c      1
     +    'Bed level'                       ,'HIS_Zb' ,'Zb' ,'m'   ,
c      2
     +    'Mean Bed level of Main channel'  ,'HIS_Za' ,'Za' ,'m'   ,
c      3
     +    'Increase of Cross-sectional Area','HIS_dA' ,'dA' ,'m2'  ,
c      4
     +    'Increase of Bed level           ','HIS_dZ' ,'dZ' ,'m'   ,
c      5
     +    'Integrated S'                    ,'HIS_Si' ,'Si' ,'m2/s',
c      6
     +    'H-levels'                        ,'HIS_Z0' ,'XS' ,'m'   /
c
      data grnamm, grnamh, grnamd /
     +     'MORP-MAP-GROUP' ,
     +     'MORP-HIS-GROUP' ,
     +     'MORP-DES-GROUP' /
c
      lastcod = 1000000
c
c     Initialise
c
      if (first) then
         nrerr = emoboo
c
         call resini ( fd_nefis_res, grnamd, grnamm , grnamh, 'MORP',
     +                 nentri, nmomap, nmotim, ngrid  , itim  , nameel,
     +                 quanel, unitel, descel, mocpre , mormap, mortim,
     +                 ncelm , ncelh , nameac, lastcod, neferr)
         if (neferr .ne. 0) goto 1000
      endif
c
c     Write MAP results
c
      if (nmomap .gt. 3) then
         if (yesmap(mormap(1), mormap(2), mormap(3), istep )) then
            nrerr = emomap
c
            call restim ( fd_nefis_res, grnamm, 1, itim, ncelm,
     +                    nameac, neferr )
            if (neferr .ne. 0) goto 1000
c
            call resdes ( 'MORP', fd_nefis_res, grnamd, 1, writim,
     +                    mortim, ncelm , ncelh , neferr )
            if (neferr .ne. 0) goto 1000
c
            uindex (1) = ncelm
            uindex (2) = ncelm
            uindex (3) = 1
c
            do 70 i = 4, nmomap, 2
               ie = mocpre(mormap(i)) + mormap(i+1)
c
               if (ie .eq. 1) then
c
c                 Write bottoms (= min of two first levels)
c
                  do 20 ibr = 1, nbran
                     do 10 j = branch(3,ibr), branch(4,ibr)
c
c                       Bottom is min of two lowest levels
c
                        buf(j) = min(hlev(j,1), hlev(j,2))
 10                  continue
 20               continue
c
                  name = 'MAP' // nameel(ie)(4:)
c
                  neferr = putrel ( fd_nefis_res, grnamm, name,
     +                              uindex, usrord, buf   )
c
                  if (neferr .ne. 0) goto 1000
c
               else if (ie .eq. 2) then
c
c                 Write Mean Bed level
c
                  do ibr = 1,nbran
                     if (typcr(ibr).ne.ccrsed) then
                        do igrid = branch(3,ibr),branch(4,ibr)
                           buf(igrid) = sectv(igrid,2)-sectv(igrid,4) /         
     +                                  sectc(igrid,2)
                        enddo
                     else 
                        do igrid = branch(3,ibr),branch(4,ibr)
                           buf(igrid) = hlev(igrid,2) - wft(igrid,1) *
     +                                 (hlev(igrid,2) - hlev(igrid,1)) /
     +                                 (wft(igrid,1) + wft(igrid,2))                
                        enddo 
                     endif
                  enddo     

                  name = 'MAP' // nameel(ie)(4:)
c
                  neferr = putrel ( fd_nefis_res, grnamm, name,
     +                              uindex, usrord, buf   )
c
                  if (neferr .ne. 0) goto 1000

               else if (ie .eq. 3) then
c
c                 Increase in Area
c
                  name = 'MAP' // nameel(ie)(4:)
c
                  neferr = putrel ( fd_nefis_res, grnamm, name,
     +                              uindex, usrord, sumda )
c
                  if (neferr .ne. 0) goto 1000

               else if (ie .eq. 4) then
c
c                 Increase of bed level
c
                  do j=1,ngrid
                     buf(j) = min(hlev(j,1),hlev(j,2)) - hlev0(j)
                  enddo                             
c
                  name = 'MAP' // nameel(ie)(4:)
c
                  neferr = putrel ( fd_nefis_res, grnamm, name,
     +                              uindex, usrord, buf   )
c
               else if (ie .eq. 5) then
c
c                 Write integrated sediment transports
c
                  do 40 ibr = 1, nbran
c
c                    S_int on first and last point from dissed
c
                     buf(branch(3,ibr)) = dissed(1,ibr)
                     buf(branch(4,ibr)) = dissed(3,ibr)
c
                     do 30 j = branch(3,ibr)+1, branch(4,ibr)-1
                        buf(j) = sedtr(j,1)
 30                  continue
 40               continue
c
                  name = 'MAP' // nameel(ie)(4:)
c
                  neferr = putrel ( fd_nefis_res, grnamm, name,
     +                              uindex, usrord, buf   )
c
                  if (neferr .ne. 0) goto 1000
c
               else if (ie .ge. 6) then
c
c                 Write cross sections
c
                  k = mormap(i+1) + 1
                  do 60 ibr = 1, nbran
                     do 50 j = branch(3,ibr), branch(4,ibr)
                        buf(j) = hlev(j,k)
 50                  continue
 60               continue
c
c                 Create name for this cross section level
c
                  ie1 = min(ie,nentri)
                  ie2 = ie-nentri
c
                  if (ie2 .gt. 0) then
                     if (ie2 .gt. 9) then
                        write(txt,'(i2)') ie2
                     else
                        write(txt,'(i1,a)') ie2, ' '
                     endif
                     l      = index (nameel(nentri),' ') - 2
                     tmpnam = 'MAP' // nameel(nentri)(4:l)
                     l      = index (tmpnam,' ') - 1
                     name   = tmpnam (1:l) // txt
                  else
                     name = 'MAP' // nameel(ie1)(4:)
                  endif
c
                  neferr = putrel ( fd_nefis_res, grnamm, name,
     +                              uindex, usrord, buf   )
c
                  if (neferr .ne. 0) goto 1000
c
               endif
 70         continue
         endif
      endif
c
c     Write HIStory results
c
      nlc = mortim(1)
      new = mod(nmotim-nlc,2) .eq. 0

      if ( new ) then
         nsk = 3
      else
         nsk = 0
      endif
c
      if (nmotim .gt. 1+nsk) then
         if (new) then
            newuit = yesmap(mortim(nlc+2),mortim(nlc+3),mortim(nlc+4),
     &                      istep)
         else
            newuit = .false.
         endif
         if ( (mortim(1).gt.0 .and. .not. new) .or. newuit ) then
            nrerr = emohis
c
            call restim ( fd_nefis_res, grnamh, 1, itim, ncelh,
     +                    nameac, neferr )
            if (neferr .ne. 0) goto 1000
c
            call resdes ( 'MORP', fd_nefis_res, grnamd, 2, writim,
     +                    mortim, ncelm , ncelh , neferr )
            if (neferr .ne. 0) goto 1000
c
            uindex (1) = ncelh
            uindex (2) = ncelh
            uindex (3) = 1
c
            do 110 i = mortim(1)+2+nsk, nmotim, 2
               ie = mocpre(mortim(i))+mortim(i+1)
               if (ie .eq. 1) then
c
c                 Bottom for selected grid points
c
                  do 80 j = 1, mortim(1)
c
                     buf(j) = min ( hlev(mortim(j+1),1),
     +                              hlev(mortim(j+1),2)
     +                            )
c
 80               continue
c
                  neferr = putrel ( fd_nefis_res, grnamh, nameel(ie),
     +                              uindex, usrord, buf  )
c
                  if (neferr .ne. 0) goto 1000

               else if (ie .eq. 2) then
c
c                 Write Mean Bed level
c
                  do j=1,mortim(1)
                     igrid = mortim(j+1)
                     call mofdbr ( nbran ,branch ,igrid ,ibr)
                     if (typcr(ibr).ne.ccrsed) then
                        buf(j) = sectv(igrid,2)-sectv(igrid,4) /               
     +                               sectc(igrid,2)
                     else 
                        buf(j) = hlev(igrid,2) - wft(igrid,1) *
     +                              (hlev(igrid,2) - hlev(igrid,1)) /
     +                              (wft(igrid,1) + wft(igrid,2))          
                     endif
                  enddo     
c
                  neferr = putrel ( fd_nefis_res, grnamh, nameel(ie),
     +                              uindex, usrord, buf   )
c
                  if (neferr .ne. 0) goto 1000

               else if (ie .eq. 3) then
c
c                 Increase in Area
c
                  do 82 j = 1, mortim(1)
                     buf(j) = sumda(mortim(j+1))
   82             continue

                  neferr = putrel ( fd_nefis_res, grnamh, nameel(ie),
     +                              uindex, usrord, buf   )
c
                  if (neferr .ne. 0) goto 1000
c
               elseif (ie .eq. 4) then
c
c                 Increase of bed level
c
                  do j = 1, mortim(1)
                      buf(j) = min (hlev(mortim(j+1),1),
     +                              hlev(mortim(j+1),2)) - 
     +                              hlev0(mortim(j+1))
                  enddo
c
                  neferr = putrel ( fd_nefis_res, grnamh, nameel(ie),
     +                              uindex, usrord, buf  )
c
                  if (neferr .ne. 0) goto 1000
c                 
               elseif (ie .eq. 5) then
c
c                 S_int for selected grid points
c
                  do 90 j = 1, mortim(1)
c
c                    Find branch number of this gridpoint
c
                     call mofdbr ( nbran, branch, mortim(j+1), ibr )
c
c                    Test for first or last point
c
                     if (mortim(j+1) .eq. branch(3,ibr)) then
                        buf(j) = dissed(1,ibr)
                     else if (mortim(j+1) .eq. branch(4,ibr)) then
                        buf(j) = dissed(3,ibr)
                     else
                        buf(j) = sedtr(mortim(j+1),1)
                     endif
c
 90               continue
c
                  neferr = putrel ( fd_nefis_res, grnamh, nameel(ie),
     +                              uindex, usrord, buf  )
c
                  if (neferr .ne. 0) goto 1000
c
               elseif (ie .ge. 6) then
c
c                 Cross sections for selected grid points
c
                  k = mortim(i+1) + 1
                  do 100 j = 1, mortim(1)
c
c                    Find branch number of this gridpoint
c
                     call mofdbr ( nbran, branch, mortim(j+1), ibr )
c
                     buf(j) = hlev(mortim(j+1),k)
 100              continue
c
c                 Create name for this cross section level
c
                  ie1 = min(ie,nentri)
                  ie2 = ie-nentri
                  if (ie2 .gt. 0) then
                     if (ie2 .gt. 9) then
                        write(txt,'(i2)') ie2
                     else
                        write(txt,'(i1,a)') ie2, ' '
                     endif
                     l    = index (nameel(nentri),' ') - 2
                     name = nameel(nentri)(1:l)//txt
                  else
                     name = nameel(ie1)
                  endif
c
                  neferr = putrel ( fd_nefis_res, grnamh, name,
     +                              uindex, usrord, buf  )
c

                  if (neferr .ne. 0) goto 1000
               endif
 110        continue
         endif
      endif
c
c     Be sure that at the last time step the number of cells has been
c     written correctly
c
      if (istep .ge. nstep) then
         nrerr = emoeoo
         llog  = .true.
c
         call resdes ( 'MORP', fd_nefis_res, grnamd, 3, llog ,
     +                 mortim, ncelm , ncelh , neferr )
         if (neferr .ne. 0) goto 1000
c
         neferr = flsdat (fd_nefis_res)
         if (neferr .ne. 0) goto 1000
      endif
c
      return
c
 1000 continue
c
      ker = fatal
      write(txt2,'(i8)') neferr
      call error (juer, 'MOMOR @'//txt2//'@', nrerr, ker)
c
      end
