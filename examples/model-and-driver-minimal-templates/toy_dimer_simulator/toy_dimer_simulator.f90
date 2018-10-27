module error
  use, intrinsic :: iso_c_binding
  implicit none
  public

contains
  subroutine my_error(message)
    implicit none
    character(len=*, kind=c_char), intent(in) :: message

    print *,"* Error : ", trim(message)
    stop
  end subroutine my_error

  subroutine my_warning(message)
    implicit none
    character(len=*, kind=c_char), intent(in) :: message

    print *,"* Error : ", trim(message)
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

!-------------------------------------------------------------------------------
!
! get_neigh neighbor list access function
!
! This function implements Locator and Iterator mode
!
!-------------------------------------------------------------------------------
subroutine get_neigh(data_object, number_of_neighbor_lists, cutoffs, &
  neighbor_list_index, request, numnei, pnei1part, ierr) bind(c)
  use error
  implicit none

  !-- Transferred variables
  type(c_ptr),    value, intent(in) :: data_object
  integer(c_int), value, intent(in) :: number_of_neighbor_lists
  real(c_double),        intent(in) :: cutoffs(number_of_neighbor_lists)
  integer(c_int), value, intent(in) :: neighbor_list_index
  integer(c_int), value, intent(in)  :: request
  integer(c_int),        intent(out) :: numnei
  type(c_ptr),           intent(out) :: pnei1part
  integer(c_int), intent(out) :: ierr

  !-- Local variables
  integer(c_int) numberOfParticles
  type(neighObject_type), pointer :: neighObject
  integer(c_int), pointer :: neighborList(:,:)

  call c_f_pointer(data_object, neighObject)
  numberOfParticles = neighObject%number_of_particles
  call c_f_pointer(neighObject%neighbor_list_pointer, neighborList, &
    [numberOfParticles+1, numberOfParticles])

  if (number_of_neighbor_lists /= 1) then
    call my_warning("invalid number of cutoffs")
    ierr = 1
    return
  endif

  if (cutoffs(1) > neighObject%cutoff) then
    call my_warning("neighbor list cutoff too small for model cutoff")
    ierr = 1
    return
  endif

  if (neighbor_list_index /= 1) then
    call my_warning("wrong list index")
    ierr = 1
    return
  endif

  if ( (request.gt.numberOfParticles) .or. (request.lt.1)) then
    print *, request
    call my_warning("Invalid part ID in get_neigh")
    ierr = 1
    return
  endif

  ! set the returned number of neighbors for the returned part
  numnei = neighborList(1,request)

  ! set the location for the returned neighbor list
  pnei1part = c_loc(neighborList(2,request))

  ierr = 0
  return
end subroutine get_neigh

end module mod_neighborlist


module mod_utility
  implicit none
  public

contains

!-------------------------------------------------------------------------------
!
! NEIGH_PURE_cluster_neighborlist
!
!-------------------------------------------------------------------------------
subroutine NEIGH_PURE_cluster_neighborlist(half, numberOfParticles, coords, &
                                           cutoff, neighObject)
  use, intrinsic :: iso_c_binding
  use mod_neighborlist
  implicit none

  !-- Transferred variables
  logical,        intent(in)            :: half
  integer(c_int), intent(in)            :: numberOfParticles
  real(c_double), dimension(3,numberOfParticles), &
                  intent(in)            :: coords
  real(c_double), intent(in)            :: cutoff
  type(neighObject_type), intent(inout) :: neighObject

  !-- Local variables
  integer(c_int) i, j, a
  real(c_double) dx(3)
  real(c_double) r2
  real(c_double) cutoff2
  integer(c_int), pointer :: neighborList(:,:)

  call c_f_pointer(neighObject%neighbor_list_pointer, neighborList, &
    [numberOfParticles+1, numberOfParticles])

  neighObject%cutoff = cutoff

  cutoff2 = cutoff**2

  do i=1,numberOfParticles
     a = 1
     do j=1,numberOfParticles
        dx(:) = coords(:, j) - coords(:, i)
        r2 = dot_product(dx, dx)
        if (r2.le.cutoff2) then
           ! part j is a neighbor of part i
           if ( (j .gt. i) .OR. ((.not. half) .AND. (i.ne.j)) ) then
               a = a+1
               neighborList(a,i) = j
           endif
        endif
     enddo
     ! part i has a-1 neighbors
     neighborList(1,i) = a-1
  enddo

  return

end subroutine NEIGH_PURE_cluster_neighborlist

end module mod_utility

!-------------------------------------------------------------------------------
!
! Main program
!
!-------------------------------------------------------------------------------
program toy_dimer_simulator
  use, intrinsic :: iso_c_binding
  use error
  use kim_simulator_headers_module
  use mod_neighborlist
  use mod_utility
  implicit none
  integer(c_int), parameter :: cd = c_double ! used for literal constants

  integer(c_int), parameter :: N    = 2
  integer(c_int), parameter :: DIM  = 3

  real(c_double), parameter :: cutpad         = 0.75_cd
  real(c_double), parameter :: min_spacing    = 1.0_cd
  real(c_double), parameter :: max_spacing    = 12.0_cd
  real(c_double), parameter :: spacing_incr   = 1.0_cd
  real(c_double) :: current_spacing

  character(len=256, kind=c_char) :: modelname

  type(neighObject_type), target :: neighObject
  integer(c_int), allocatable, target :: neighborList(:,:)

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
  integer(c_int) i, ierr, ierr2

  integer(c_int) species_is_supported
  integer(c_int) species_code
  integer(c_int) requested_units_accepted

  ! Initialize error flag
  ierr = 0

  ! Get KIM Model name to use
  print '("Please enter a valid KIM model name: ")'
  read(*,*) modelname

  ! Print output header
  !
  print *
  print *,'This is Test : toy_dimer_simulator'
  print *
  print '(80(''-''))'
  print '("Results for KIM Model : ",A)', trim(modelname)

  ! Create empty KIM object
  !
  call kim_model_create(kim_numbering_one_based, &
    kim_length_unit_a, &
    kim_energy_unit_ev, &
    kim_charge_unit_e, &
    kim_temperature_unit_k, &
    kim_time_unit_ps, &
    trim(modelname), &
    requested_units_accepted, &
    model_handle, ierr)
  if (ierr /= 0) then
    call my_error("kim_api_create")
  endif

  ! check that we are compatible
  if (requested_units_accepted == 0) then
    call my_error("Must adapt to model units")
  end if

  ! check that model supports Ar
  !
  call kim_model_get_species_support_and_code(model_handle, &
    kim_species_name_ar, species_is_supported, species_code, ierr)
  if ((ierr /= 0) .or. (species_is_supported /= 1)) then
    call my_error("Model does not support Ar")
  endif

  ! Best-practice is to check that the model is compatible
  ! but we will skip it here

  ! create compute_arguments object
  call kim_model_compute_arguments_create( &
    model_handle, compute_arguments_handle, ierr)
  if (ierr /= 0) then
    call my_error("kim_model_compute_arguments_create")
  endif

  ! register memory with the KIM system
  ierr = 0
  call kim_compute_arguments_set_argument_pointer(compute_arguments_handle, &
    kim_compute_argument_name_number_of_particles, N, ierr2)
  ierr = ierr + ierr2
  call kim_compute_arguments_set_argument_pointer(compute_arguments_handle, &
    kim_compute_argument_name_particle_species_codes, particle_species_codes, &
    ierr2)
  ierr = ierr + ierr2
  call kim_compute_arguments_set_argument_pointer(compute_arguments_handle, &
    kim_compute_argument_name_particle_contributing, particle_contributing, &
    ierr2)
  ierr = ierr + ierr2
  call kim_compute_arguments_set_argument_pointer(compute_arguments_handle, &
    kim_compute_argument_name_coordinates, coords, ierr2)
  ierr = ierr + ierr2
  call kim_compute_arguments_set_argument_pointer(compute_arguments_handle, &
    kim_compute_argument_name_partial_energy, energy, ierr2)
  ierr = ierr + ierr2
  if (ierr /= 0) then
     call my_error("set_argument_pointer")
  endif

  ! Allocate storage for neighbor lists
  !
  allocate(neighborList(N+1,N))
  neighObject%neighbor_list_pointer = c_loc(neighborList)
  neighObject%number_of_particles = N

  ! Set pointer in KIM object to neighbor list routine and object
  !
  call kim_compute_arguments_set_callback_pointer(compute_arguments_handle, &
    kim_compute_callback_name_get_neighbor_list, kim_language_name_fortran, &
    c_funloc(get_neigh), c_loc(neighobject), ierr)
  if (ierr /= 0) then
    call my_error("set_callback_pointer")
  end if

  call kim_model_get_influence_distance(model_handle, influence_distance)
  call kim_model_get_number_of_neighbor_lists(model_handle, &
    number_of_neighbor_lists)
  if (number_of_neighbor_lists /= 1) then
    call my_error("too many neighbor lists")
  endif
  call kim_model_get_neighbor_list_values(model_handle, cutoffs, &
    model_will_not_request_neighbors_of_noncontributing_particles, ierr)
  if (ierr /= 0) then
    call my_error("get_neighbor_list_values")
  end if
  cutoff = cutoffs(1)

  ! Setup cluster
  !
  do i=1,N
    particle_species_codes(i) = species_code
  enddo

  ! setup contributing particles
  do i=1,N
    particle_contributing(i) = 1  ! every particle contributes
  enddo

  ! print header
  print '(2A20)', "Energy", "Dimer Spacing"
  ! do the computations
  current_spacing = min_spacing
  do while (current_spacing < max_spacing)

    coords(:,:) = 0.0_cd
    coords(1,2) = current_spacing
    ! Compute neighbor lists
    call NEIGH_PURE_cluster_neighborlist(.false., N, coords, (cutoff+cutpad), &
                                         neighObject)

    ! Call model compute
    call kim_model_compute(model_handle, compute_arguments_handle, ierr)
    if (ierr /= 0) then
      call my_error("kim_api_model_compute")
    endif

    ! Print results to screen
    !
    print '(3ES20.10)', energy, current_spacing

    current_spacing = current_spacing + spacing_incr
  enddo

  ! Deallocate neighbor list object
  deallocate( neighborList )

  call kim_model_compute_arguments_destroy(&
    model_handle, compute_arguments_handle, ierr)
  if (ierr /= 0) then
    call my_error("compute_arguments_destroy")
  endif
  call kim_model_destroy(model_handle)

end program toy_dimer_simulator
