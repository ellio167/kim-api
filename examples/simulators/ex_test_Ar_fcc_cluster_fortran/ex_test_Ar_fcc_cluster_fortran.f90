!
! KIM-API: An API for interatomic models
! Copyright (c) 2013--2022, Regents of the University of Minnesota.
! All rights reserved.
!
! Contributors:
!    Ellad B. Tadmor
!    Ryan S. Elliott
!    Stephen M. Whalen
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
! module mod_neighborlist :
!
!    Module contains type and routines related to neighbor list calculation
!
!-------------------------------------------------------------------------------

module mod_neighborlist

  use, intrinsic :: iso_c_binding

  public get_neigh

  type, bind(c) :: neighObject_type
    real(c_double) :: cutoff
    integer(c_int) :: number_of_particles
    type(c_ptr) :: neighbor_list_pointer
  end type neighObject_type
contains

  !-----------------------------------------------------------------------------
  !
  ! get_neigh neighbor list access function
  !
  ! This function implements Locator and Iterator mode
  !
  !-----------------------------------------------------------------------------
  recursive subroutine get_neigh(data_object, number_of_neighbor_lists, &
                                 cutoffs, neighbor_list_index, request, &
                                 numnei, pnei1part, ierr) bind(c)
    use error
    implicit none

    !-- Transferred variables
    type(c_ptr), value, intent(in) :: data_object
    integer(c_int), value, intent(in) :: number_of_neighbor_lists
    real(c_double), intent(in) :: cutoffs(*)
    integer(c_int), value, intent(in) :: neighbor_list_index
    integer(c_int), value, intent(in)  :: request
    integer(c_int), intent(out) :: numnei
    type(c_ptr), intent(out) :: pnei1part
    integer(c_int), intent(out) :: ierr

    !-- Local variables
    integer(c_int) numberOfParticles
    type(neighObject_type), pointer :: neighObject
    integer(c_int), pointer :: neighborList(:, :)

    call c_f_pointer(data_object, neighObject)
    numberOfParticles = neighObject%number_of_particles
    call c_f_pointer(neighObject%neighbor_list_pointer, neighborList, &
                     [numberOfParticles + 1, numberOfParticles])

    if (number_of_neighbor_lists /= 1) then
      call my_warning("invalid number of cutoffs")
      ierr = 1
      return
    end if

    if (cutoffs(1) > neighObject%cutoff) then
      call my_warning("neighbor list cutoff too small for model cutoff")
      ierr = 1
      return
    end if

    if (neighbor_list_index /= 1) then
      call my_warning("wrong list index")
      ierr = 1
      return
    end if

    if ((request > numberOfParticles) .or. (request < 1)) then
      print *, request
      call my_warning("Invalid part ID in get_neigh")
      ierr = 1
      return
    end if

    ! set the returned number of neighbors for the returned part
    numnei = neighborList(1, request)

    ! set the location for the returned neighbor list
    pnei1part = c_loc(neighborList(2, request))

    ierr = 0
    return
  end subroutine get_neigh

end module mod_neighborlist

module mod_utility
  implicit none
  public

contains

  !-----------------------------------------------------------------------------
  !
  ! NEIGH_PURE_cluster_neighborlist
  !
  !-----------------------------------------------------------------------------
  recursive subroutine NEIGH_PURE_cluster_neighborlist( &
    half, numberOfParticles, coords, cutoff, neighObject)
    use, intrinsic :: iso_c_binding
    use mod_neighborlist
    implicit none

    !-- Transferred variables
    logical, intent(in)            :: half
    integer(c_int), intent(in)            :: numberOfParticles
    real(c_double), dimension(3, numberOfParticles), &
      intent(in)            :: coords
    real(c_double), intent(in)            :: cutoff
    type(neighObject_type), intent(inout) :: neighObject

    !-- Local variables
    integer(c_int) i, j, a
    real(c_double) dx(3)
    real(c_double) r2
    real(c_double) cutoff2
    integer(c_int), pointer :: neighborList(:, :)

    call c_f_pointer(neighObject%neighbor_list_pointer, neighborList, &
                     [numberOfParticles + 1, numberOfParticles])

    neighObject%cutoff = cutoff

    cutoff2 = cutoff**2

    do i = 1, numberOfParticles
      a = 1
      do j = 1, numberOfParticles
        dx(:) = coords(:, j) - coords(:, i)
        r2 = dot_product(dx, dx)
        if (r2 <= cutoff2) then
          ! part j is a neighbor of part i
          if ((j > i) .OR. ((.not. half) .AND. (i /= j))) then
            a = a + 1
            neighborList(a, i) = j
          end if
        end if
      end do
      ! part i has a-1 neighbors
      neighborList(1, i) = a - 1
    end do

    return

  end subroutine NEIGH_PURE_cluster_neighborlist

  !-----------------------------------------------------------------------------
  !
  ! create_FCC_configuration subroutine
  !
  !  creates a cubic configuration of FCC particles with lattice spacing
  !  `FCCspacing' and `nCellsPerSide' cells along each direction.
  !
  !  With periodic==.true. this will result in a total number of particles equal
  !  to 4*(nCellsPerSide)**3 + 6*(nCellsPerSide)**2 + 3*(nCellsPerSide) + 1
  !
  !  With periodic==.false. this will result in a total number of particles equal
  !  to 4*(nCellsPerSide)**3
  !
  !  Returns the Id of the particle situated in the middle of the configuration
  !  (this particle will have the most neighbors.)
  !
  !-----------------------------------------------------------------------------
  recursive subroutine create_FCC_configuration(FCCspacing, nCellsPerSide, &
                                                periodic, coords, MiddlePartId)
    use, intrinsic :: iso_c_binding
    implicit none
    integer(c_int), parameter :: cd = c_double ! used for literal constants

    !-- Transferred variables
    real(c_double), intent(in)  :: FCCspacing
    integer(c_int), intent(in)  :: nCellsPerSide
    logical, intent(in)  :: periodic
    real(c_double), intent(out) :: coords(3, *)
    integer(c_int), intent(out) :: MiddlePartId
    !
    ! cluster setup variables
    !
    real(c_double) FCCshifts(3, 4)
    real(c_double) latVec(3)
    integer(c_int) a, i, j, k, m

    ! Create a cubic FCC cluster
    !
    FCCshifts(1, 1) = 0.0_cd
    FCCshifts(2, 1) = 0.0_cd
    FCCshifts(3, 1) = 0.0_cd
    FCCshifts(1, 2) = 0.5_cd * FCCspacing
    FCCshifts(2, 2) = 0.5_cd * FCCspacing
    FCCshifts(3, 2) = 0.0_cd
    FCCshifts(1, 3) = 0.5_cd * FCCspacing
    FCCshifts(2, 3) = 0.0_cd
    FCCshifts(3, 3) = 0.5_cd * FCCspacing
    FCCshifts(1, 4) = 0.0_cd
    FCCshifts(2, 4) = 0.5_cd * FCCspacing
    FCCshifts(3, 4) = 0.5_cd * FCCspacing

    MiddlePartID = 1 ! Always put middle particle as #1
    a = 1            ! leave space for middle particle as particle #1
    do i = 1, nCellsPerSide
      latVec(1) = (i - 1) * FCCspacing
      do j = 1, nCellsPerSide
        latVec(2) = (j - 1) * FCCspacing
        do k = 1, nCellsPerSide
          latVec(3) = (k - 1) * FCCspacing
          do m = 1, 4
            a = a + 1
            coords(:, a) = latVec + FCCshifts(:, m)
            if ((i == nCellsPerside / 2 + 1) &
                .and. (j == nCellsPerSide / 2 + 1) &
                .and. (k == nCellsPerSide / 2 + 1) &
                .and. (m == 1)) &
              then
              ! put middle particle as #1
              coords(:, 1) = latVec + FCCshifts(:, m)
              a = a - 1
            end if
          end do
        end do
        if (.not. periodic) then
          ! Add in the remaining three faces
          ! pos-x face
          latVec(1) = nCellsPerSide * FCCspacing
          latVec(2) = (i - 1) * FCCspacing
          latVec(3) = (j - 1) * FCCspacing
          a = a + 1; coords(:, a) = latVec
          a = a + 1; coords(:, a) = latVec + FCCshifts(:, 4)
          ! pos-y face
          latVec(1) = (i - 1) * FCCspacing
          latVec(2) = nCellsPerSide * FCCspacing
          latVec(3) = (j - 1) * FCCspacing
          a = a + 1; coords(:, a) = latVec
          a = a + 1; coords(:, a) = latVec + FCCshifts(:, 3)
          ! pos-z face
          latVec(1) = (i - 1) * FCCspacing
          latVec(2) = (j - 1) * FCCspacing
          latVec(3) = nCellsPerSide * FCCspacing
          a = a + 1; coords(:, a) = latVec
          a = a + 1; coords(:, a) = latVec + FCCshifts(:, 2)
        end if
      end do
      if (.not. periodic) then
        ! Add in the remaining three edges
        latVec(1) = (i - 1) * FCCspacing
        latVec(2) = nCellsPerSide * FCCspacing
        latVec(3) = nCellsPerSide * FCCspacing
        a = a + 1; coords(:, a) = latVec
        latVec(1) = nCellsPerSide * FCCspacing
        latVec(2) = (i - 1) * FCCspacing
        latVec(3) = nCellsPerSide * FCCspacing
        a = a + 1; coords(:, a) = latVec
        latVec(1) = nCellsPerSide * FCCspacing
        latVec(2) = nCellsPerSide * FCCspacing
        latVec(3) = (i - 1) * FCCspacing
        a = a + 1; coords(:, a) = latVec
      end if
    end do
    if (.not. periodic) then
      ! Add in the remaining corner
      a = a + 1; coords(:, a) = nCellsPerSide * FCCspacing
    end if

    return

  end subroutine create_FCC_configuration

end module mod_utility

!*******************************************************************************
!**
!**  PROGRAM vc_forces_numer_deriv
!**
!**  KIM compliant program to perform numerical derivative check on a model
!**
!*******************************************************************************

!-------------------------------------------------------------------------------
!
! Main program
!
!-------------------------------------------------------------------------------
program ex_test_ar_fcc_cluster_fortran
  use, intrinsic :: iso_c_binding
  use error
  use kim_simulator_headers_module
  use kim_supported_extensions_module
  use mod_neighborlist
  use mod_utility
  implicit none
  integer(c_int), parameter :: cd = c_double ! used for literal constants

  integer(c_int), parameter :: nCellsPerSide = 2
  integer(c_int), parameter :: DIM = 3

  real(c_double), parameter :: cutpad = 0.75_cd
  real(c_double), parameter :: FCCspacing = 5.260_cd
  real(c_double), parameter :: min_spacing = 0.8 * FCCspacing
  real(c_double), parameter :: max_spacing = 1.2 * FCCspacing
  real(c_double), parameter :: spacing_incr = 0.025 * FCCspacing
  real(c_double) :: current_spacing
  real(c_double) :: force_norm

  character(len=256, kind=c_char) :: modelname

  integer(c_int), parameter :: N = 4 * (nCellsPerSide)**3 &
                               + 6 * (nCellsPerSide)**2 &
                               + 3 * (nCellsPerSide) + 1

  type(neighObject_type), target :: neighObject
  integer(c_int), allocatable, target :: neighborList(:, :)

  type(kim_model_handle_type) :: model_handle
  type(kim_compute_arguments_handle_type) :: compute_arguments_handle
  real(c_double) :: influence_distance
  integer(c_int) :: number_of_neighbor_lists
  real(c_double) :: cutoff
  real(c_double) :: cutoffs(1)
  integer(c_int) :: &
    model_will_not_request_neighbors_of_noncontributing_particles(1)
  integer(c_int), target :: particle_species_codes(N)
  integer(c_int), target :: particle_contributing(N)
  real(c_double), target :: energy
  real(c_double), target :: coords(DIM, N)
  real(c_double), target :: forces(DIM, N)
  integer(c_int) i, j, ierr, ierr2

  integer(c_int) species_is_supported
  integer(c_int) species_code
  integer(c_int) requested_units_accepted
  integer(c_int) number_of_model_routine_names
  type(kim_model_routine_name_type) model_routine_name
  integer(c_int) present
  integer(c_int) required
  type(kim_supported_extensions_type), target :: supported_extensions
  character(len=KIM_MAX_EXTENSION_ID_LENGTH, kind=c_char) :: id_string

  integer :: middledum

  ! Initialize
  ierr = 0
  supported_extensions%number_of_supported_extensions = 0

  ! Get KIM Model name to use
  print '("Please enter a valid KIM model name: ")'
  read (*, *) modelname

  ! Create empty KIM object
  !
  call kim_model_create(KIM_NUMBERING_ONE_BASED, &
                        KIM_LENGTH_UNIT_A, &
                        KIM_ENERGY_UNIT_EV, &
                        KIM_CHARGE_UNIT_E, &
                        KIM_TEMPERATURE_UNIT_K, &
                        KIM_TIME_UNIT_PS, &
                        trim(modelname), &
                        requested_units_accepted, &
                        model_handle, ierr)
  if (ierr /= 0) then
    call my_error("kim_api_create")
  end if

  ! check that we are compatible
  if (requested_units_accepted == 0) then
    call my_error("Must adapt to model units")
  end if

  ! check that we know about all required routines
  call kim_get_number_of_model_routine_names(number_of_model_routine_names)
  do i = 1, number_of_model_routine_names
    call kim_get_model_routine_name(i, model_routine_name, ierr)
    if (ierr /= 0) then
      call my_error("kim_get_model_routine_name")
    end if
    call kim_is_routine_present(model_handle, model_routine_name, present, &
                                required, ierr)
    if (ierr /= 0) then
      call my_error("kim_is_routine_present")
    end if

    if ((present == 1) .and. (required == 1)) then
      if (.not. ((model_routine_name == KIM_MODEL_ROUTINE_NAME_CREATE) &
                 .or. (model_routine_name == &
                       KIM_MODEL_ROUTINE_NAME_COMPUTE_ARGUMENTS_CREATE) &
                 .or. (model_routine_name == KIM_MODEL_ROUTINE_NAME_COMPUTE) &
                 .or. (model_routine_name == KIM_MODEL_ROUTINE_NAME_REFRESH) &
                 .or. (model_routine_name == &
                       KIM_MODEL_ROUTINE_NAME_COMPUTE_ARGUMENTS_DESTROY) &
                 .or. (model_routine_name == KIM_MODEL_ROUTINE_NAME_DESTROY))) &
        then
        call my_error("Unknown required ModelRoutineName found.")
      end if
    end if
  end do

  ! check that model supports Ar
  !
  call kim_get_species_support_and_code( &
    model_handle, KIM_SPECIES_NAME_AR, species_is_supported, species_code, ierr)
  if ((ierr /= 0) .or. (species_is_supported /= 1)) then
    call my_error("Model does not support Ar")
  end if

  ! Check supported extensions, if any
  call kim_is_routine_present( &
    model_handle, KIM_MODEL_ROUTINE_NAME_EXTENSION, present, required, ierr)
  if (ierr /= 0) then
    call my_error("Unable to get Extension present/required.")
  end if
  if (present /= 0) then
    call kim_extension(model_handle, KIM_SUPPORTED_EXTENSIONS_ID, &
                       c_loc(supported_extensions), ierr)
    if (ierr /= 0) then
      call my_error("Error returned from kim_model_extension().")
    end if
    write (*, '(A,I2,A)') "Model Supports ", &
      supported_extensions%number_of_supported_extensions, &
      " Extensions:"
    do i = 1, supported_extensions%number_of_supported_extensions
      call kim_c_char_array_to_string( &
        supported_extensions%supported_extension_id(:, i), id_string)
      write (*, '(A,I2,A,A,A,A,I2)') " supportedExtensionID[", i, '] = "', &
        trim(id_string), '" ', &
        "which has required = ", &
        supported_extensions%supported_extension_required(i)
    end do
  end if

  ! Best-practice is to check that the model is compatible
  ! but we will skip it here

  ! create compute_arguments object
  call kim_compute_arguments_create( &
    model_handle, compute_arguments_handle, ierr)
  if (ierr /= 0) then
    call my_error("kim_model_compute_arguments_create")
  end if

  ! register memory with the KIM system
  ierr = 0
  call kim_set_argument_pointer( &
    compute_arguments_handle, KIM_COMPUTE_ARGUMENT_NAME_NUMBER_OF_PARTICLES, &
    n, ierr2)
  ierr = ierr + ierr2
  call kim_set_argument_pointer( &
    compute_arguments_handle, &
    KIM_COMPUTE_ARGUMENT_NAME_PARTICLE_SPECIES_CODES, &
    particle_species_codes, ierr2)
  ierr = ierr + ierr2
  call kim_set_argument_pointer( &
    compute_arguments_handle, &
    KIM_COMPUTE_ARGUMENT_NAME_PARTICLE_CONTRIBUTING, particle_contributing, &
    ierr2)
  ierr = ierr + ierr2
  call kim_set_argument_pointer( &
    compute_arguments_handle, KIM_COMPUTE_ARGUMENT_NAME_COORDINATES, coords, &
    ierr2)
  ierr = ierr + ierr2
  call kim_set_argument_pointer( &
    compute_arguments_handle, KIM_COMPUTE_ARGUMENT_NAME_PARTIAL_ENERGY, &
    energy, ierr2)
  ierr = ierr + ierr2
  call kim_set_argument_pointer( &
    compute_arguments_handle, KIM_COMPUTE_ARGUMENT_NAME_PARTIAL_FORCES, &
    forces, ierr2)
  ierr = ierr + ierr2
  if (ierr /= 0) then
    call my_error("set_argument_pointer")
  end if

  ! Allocate storage for neighbor lists
  !
  allocate (neighborList(N + 1, N))
  neighObject%neighbor_list_pointer = c_loc(neighborList)
  neighObject%number_of_particles = N

  ! Set pointer in KIM object to neighbor list routine and object
  !
  call kim_set_callback_pointer( &
    compute_arguments_handle, KIM_COMPUTE_CALLBACK_NAME_GET_NEIGHBOR_LIST, &
    KIM_LANGUAGE_NAME_FORTRAN, c_funloc(get_neigh), c_loc(neighobject), ierr)
  if (ierr /= 0) then
    call my_error("set_callback_pointer")
  end if

  call kim_get_influence_distance(model_handle, influence_distance)
  call kim_get_number_of_neighbor_lists(model_handle, &
                                        number_of_neighbor_lists)
  if (number_of_neighbor_lists /= 1) then
    call my_error("too many neighbor lists")
  end if
  call kim_get_neighbor_list_values( &
    model_handle, cutoffs, &
    model_will_not_request_neighbors_of_noncontributing_particles, ierr)
  if (ierr /= 0) then
    call my_error("get_neighbor_list_values")
  end if
  cutoff = cutoffs(1)

  ! Setup cluster
  !
  do i = 1, N
    particle_species_codes(i) = species_code
  end do

  ! setup contributing particles
  do i = 1, N
    particle_contributing(i) = 1  ! every particle contributes
  end do

  ! Print output header
  !
  print *
  print *, 'This is Test : ex_test_Ar_fcc_cluster_fortran'
  print *
  print '(80(''-''))'
  print '("Results for KIM Model : ",A)', trim(modelname)

  ! print header
  print '(3A20)', "Energy", "Force Norm", "Lattice Spacing"
  ! do the computations
  current_spacing = min_spacing
  do while (current_spacing < max_spacing)

    call create_FCC_configuration(current_spacing, nCellsPerSide, .false., &
                                  coords, middleDum)
    ! Compute neighbor lists
    call NEIGH_PURE_cluster_neighborlist(.false., N, coords, &
                                         (cutoff + cutpad), neighObject)

    ! Call model compute
    call kim_compute(model_handle, compute_arguments_handle, ierr)
    if (ierr /= 0) then
      call my_error("kim_api_model_compute")
    end if

    ! compue force_norm
    force_norm = 0.0
    do i = 1, N
      do j = 1, DIM
        force_norm = force_norm + forces(j, i) * forces(j, i)
      end do
    end do
    force_norm = sqrt(force_norm)

    ! Print results to screen
    !
    print '(3ES20.10)', energy, force_norm, current_spacing

    current_spacing = current_spacing + spacing_incr
  end do

  ! Deallocate neighbor list object
  deallocate (neighborList)

  call kim_compute_arguments_destroy( &
    model_handle, compute_arguments_handle, ierr)
  if (ierr /= 0) then
    call my_error("compute_arguments_destroy")
  end if
  call kim_model_destroy(model_handle)

end program ex_test_ar_fcc_cluster_fortran
