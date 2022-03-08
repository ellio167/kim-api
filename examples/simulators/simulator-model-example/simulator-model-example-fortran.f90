!
! KIM-API: An API for interatomic models
! Copyright (c) 2013--2021, Regents of the University of Minnesota.
! All rights reserved.
!
! Contributors:
!    Ryan S. Elliott
!
! SPDX-License-Identifier: LGPL-2.1-or-later
!
! This library is free software; you can redistribute it and/or
! modify it under the terms of the GNU Lesser General Public
! License as published by the Free Software Foundation; either
! version 2.1 of the License, or (at your option) any later version.
!
! This library is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! Lesser General Public License for more details.
!
! You should have received a copy of the GNU Lesser General Public License
! along with this library; if not, write to the Free Software Foundation,
! Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
!

module error
  use, intrinsic :: iso_c_binding
  implicit none

  public

contains
  recursive subroutine my_error(message)
    implicit none
    character(len=*, kind=c_char), intent(in) :: message

    print *, "* Error : ", trim(message)
    stop 1
  end subroutine my_error

  recursive subroutine my_warning(message)
    implicit none
    character(len=*, kind=c_char), intent(in) :: message

    print *, "* Warning : ", trim(message)
  end subroutine my_warning
end module error

!-------------------------------------------------------------------------------
!
! Main program
!
!-------------------------------------------------------------------------------
program collections_example_fortran
  use, intrinsic :: iso_c_binding
  use error
  use kim_simulator_headers_module
  implicit none
  interface
    integer(c_int) function c_system(cmd) bind(c, name="system")
      use, intrinsic :: iso_c_binding
      character(c_char), intent(in) :: cmd(*)
    end function c_system
  end interface

  integer(c_int) :: ierr
  integer(c_int) :: extent
  integer(c_int) :: no_fields
  integer(c_int) :: i
  integer(c_int) :: j
  type(kim_simulator_model_handle_type) :: sm

  character(len=2048, kind=c_char) s_name
  character(len=2048, kind=c_char) s_ver
  character(len=2048, kind=c_char) species
  character(len=2048, kind=c_char) field_name
  character(len=2048, kind=c_char) line
  character(len=2048, kind=c_char) dir_name
  character(len=2048, kind=c_char) spec_name
  character(len=2048, kind=c_char) param_basename

  call kim_simulator_model_create( &
    "Sim_LAMMPS_LJcut_AkersonElliott_Alchemy_PbAu", sm, ierr)

  if (ierr /= 0) then
    call my_error("Can't create SM.")
  end if

  call kim_get_simulator_name_and_version(sm, s_name, s_ver)
  print *, "Simulator name    : ", trim(s_name)
  print *, "Simulator version : ", trim(s_ver)
  print *, ""

  call kim_get_number_of_supported_species(sm, extent)
  print *, "SM supports", extent, " species:"
  do i = 1, extent
    call kim_get_supported_species(sm, i, species, ierr)
    if (ierr /= 0) then
      call my_error("Unable to get species.")
    else
      print '(A,I2," ",A)', achar(9), i, trim(species)
    end if
  end do
  print *, ""

  call kim_add_template_map(sm, "atom-type-sym-list", "Pb Pb Au Pb", ierr)
  if (ierr /= 0) then
    call my_error("Unable to add template map.")
  end if
  call kim_close_template_map(sm)
  call kim_get_number_of_simulator_fields(sm, no_fields)
  print '("SM has ",I2," fields :")', no_fields
  do i = 1, no_fields
    call kim_get_simulator_field_metadata(sm, i, extent, field_name, ierr)
    print '("  Field",I2," is ",A," and has ",I2," lines:")', &
      i, trim(field_name), extent

    do j = 1, extent
      call kim_get_simulator_field_line(sm, i, j, line, ierr)
      if (ierr /= 0) then
        call my_error("Unable to get field line.")
      else
        print '(A,A)', achar(9), trim(line)
      end if
    end do
  end do
  print *, ""

  call kim_get_parameter_file_directory_name(sm, dir_name)
  print '("SM param dir name is ",A)', trim(dir_name)

  call kim_get_specification_file_name(sm, spec_name)
  print '("SM spec file name is ",A)', trim(spec_name)
  ierr = c_system("cat "//trim(dir_name)//"/"//trim(spec_name)//c_null_char)

  call kim_get_number_of_parameter_files(sm, extent)
  print '("SM has ",I1," parameter files:")', extent
  do i = 1, extent
    call kim_get_parameter_file_basename(sm, i, param_basename, ierr)
    if (ierr /= 0) then
      call my_error("Unable to get parameter file basename.")
    else
      print '("Parameter file ",I2," has basename ",A)', i, trim(param_basename)
      ierr = c_system( &
             "cat "//trim(dir_name)//"/"//trim(param_basename)//c_null_char)
      print *, ""
    end if
  end do

  call kim_simulator_model_destroy(sm)

end program collections_example_fortran
