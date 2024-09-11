!----- AGPL --------------------------------------------------------------------
!
!  Copyright (C)  Stichting Deltares, 2017-2024.
!
!  This file is part of Delft3D (D-Flow Flexible Mesh component).
!
!  Delft3D is free software: you can redistribute it and/or modify
!  it under the terms of the GNU Affero General Public License as
!  published by the Free Software Foundation version 3.
!
!  Delft3D  is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!  GNU Affero General Public License for more details.
!
!  You should have received a copy of the GNU Affero General Public License
!  along with Delft3D.  If not, see <http://www.gnu.org/licenses/>.
!
!  contact: delft3d.support@deltares.nl
!  Stichting Deltares
!  P.O. Box 177
!  2600 MH Delft, The Netherlands
!
!  All indications and logos of, and references to, "Delft3D",
!  "D-Flow Flexible Mesh" and "Deltares" are registered trademarks of Stichting
!  Deltares, and remain the property of Stichting Deltares. All rights reserved.
!
!-------------------------------------------------------------------------------

!
!

!> write the network domains to file
!>    it is assumed that the domain coloring "idomain" is available
   subroutine partition_write_domains(netfilename, icgsolver, jacells, japolygon, japartugrid)

      use m_partitioninfo
      use unstruc_netcdf, only: unc_write_net, UNC_CONV_UGRID, UNC_CONV_CFOLD
      use unstruc_model, only: md_ident
      use m_polygon, only: NPL
      use dfm_error
      use network_data, only: lne
      use m_flowparameters, only: japartdomain
      use gridoperations
      use system_utils, only: find_last_slash
      use m_qnerror
      use m_wripol

      implicit none

      character(len=*), intent(in) :: netfilename !< filename of whole network
      integer, intent(in) :: icgsolver !< intended solver
      integer, intent(in) :: jacells !< write cell and subdomain numbers to file
      integer, intent(in) :: japolygon !< write partitioning polygon
      integer, intent(in) :: japartugrid !< write partitioning in ugrid format (1) or not (0)

      integer, parameter :: numlen = 4 ! number of digits in domain number string/filename
      character(len=numlen) :: sdmn_loc ! domain number string

      character(len=:), allocatable :: filename
      character(len=:), allocatable :: partfilename
      integer :: idmn ! domain number
      integer :: len_basename, mdep, i1, i2, iconv
      integer :: ierror

      ierror = 1

!     save network
      call savenet()
!     save netcell, lne, lnn, idomain, xz, yz, xzw, yzw, ba
      call savecells()

      iconv = merge(UNC_CONV_UGRID, UNC_CONV_CFOLD, japartugrid > 0)

!     get file basename
      len_basename = index(netfilename, '_net') - 1
      if (len_basename < 1) then
         call qnerror('write domains: net filename error', ' ', ' ')
         goto 1234
      end if

!     write partitioning polygon
      if (japolygon == 1) then
         if (NPL > 0) then
!        use existing polygon
         else
            call generate_partition_pol_from_idomain(ierror)
         end if
         filename = trim(netfilename(1:len_basename)//'_part.pol')
         call newfil(mdep, filename)
         call wripol(mdep)
      end if
!     Write a partition domain netfile with idomain
      if (japartdomain == 1) then
         i1 = find_last_slash(netfilename)
         if (i1 == 0) then
            partfilename = "DFM_interpreted_idomain_"//trim(netfilename)
         else
            i2 = len_trim(netfilename)
            partfilename = netfilename(1:i1)//"DFM_interpreted_idomain_ "//netfilename(i1 + 1:i2)
         end if
         call unc_write_net(partfilename, janetcell=1, janetbnd=1, jaidomain=1, iconventions=iconv, md_ident=md_ident)
      end if

!     set ghostlevel parameters
      call partition_setghost_params(icgsolver)

!     loop over all domains
      do idmn = 0, ndomains - 1
!        make the domain number string
         write (sdmn_loc, '(I4.4)') idmn
         filename = netfilename(1:len_basename)//'_'//sdmn_loc//'_net.nc'

!        make the domain by deleting other parts of the net, and s
         call partition_make_domain(idmn, numlay_cellbased, numlay_nodebased, jacells, ierror)
         if (ierror /= DFM_NOERR) goto 1234

!        write partitioning net files, including cell info. and idomain
         call unc_write_net(filename, janetcell=1, janetbnd=1, jaidomain=jacells, &
                            jaiglobal_s=jacells, iconventions=iconv, md_ident=md_ident) ! Save net bnds to prevent unnecessary open bnds

!        restore network
         call restore()
         call restorecells() ! restore netcell, lne, lnn and idomain,xz, yz, xzw, yzw, ba
      end do
      call restore_1dugrid_state()

      ierror = 0
1234  continue

      return
   end subroutine partition_write_domains
