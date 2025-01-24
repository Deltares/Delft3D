Integer function MozSobNodId (cNodeId, nqlat, qltpar, qlatid)
!
!           **********************************************
!           * Find Sobek lateral node id                 *
!           **********************************************
   integer      nqlat
   real         qltpar(9,*)
   character*40 qlatid(*), cNodeId

   integer      istat ,iopt
!
   do istat = 1, nqlat
!
      iopt = int(qltpar(2,istat))
      if (iopt .eq. 8) then
         if ( qlatid (istat) .eq. cNodeId) then
            MozSobNodId = istat
            return
         endif
      endif
   end do
   MozSobNodId = 0
!
end
