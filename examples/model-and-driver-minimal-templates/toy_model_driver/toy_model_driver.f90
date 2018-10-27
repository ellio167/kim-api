module toy_model_driver

use, intrinsic :: iso_c_binding
use kim_model_driver_headers_module
implicit none

private
public model_compute, &
       model_refresh, &
       model_destroy, &
       model_compute_arguments_create, &
       model_compute_arguments_destroy, &
       model_buffer_type, &
       cd, &
       species_code

integer(c_int), parameter :: cd = c_double ! used for literal constants
integer(c_int), parameter :: species_code = 1

type, bind(c) :: model_buffer_type
  real(c_double) :: influence_distance
  real(c_double) :: cutoff(1)
  integer(c_int) :: &
    model_will_not_request_neighbors_of_noncontributing_particles(1)
  real(c_double) :: hyperparameter
end type model_buffer_type

contains

!-------------------------------------------------------------------------------
subroutine model_compute(model_compute_handle, &
  model_compute_arguments_handle, ierr) bind(c)
  implicit none

  type(kim_model_compute_handle_type), intent(in) :: model_compute_handle
  type(kim_model_compute_arguments_handle_type), intent(in) :: &
    model_compute_arguments_handle
  integer(c_int), intent(out) :: ierr

  real(c_double) :: rsq,cutsq
  integer(c_int) :: i,j,jj,number_of_neighbors
  integer(c_int) :: ierr2
  type(model_buffer_type), pointer :: buf; type(c_ptr) :: pbuf
  real(c_double) :: rij(3)
  integer(c_int), pointer :: n
  real(c_double), pointer :: energy
  real(c_double), pointer :: coor(:,:)
  integer(c_int), pointer :: neighbors_of_particle(:)
  integer(c_int), pointer :: particle_species_codes(:)
  integer(c_int), pointer :: particle_contributing(:)

  ! get model buffer from model_compute object
  call kim_model_compute_get_model_buffer_pointer(model_compute_handle, pbuf)
  call c_f_pointer(pbuf, buf)

  cutsq = (buf%cutoff(1))**2

  ! Unpack data from model_compute_arguments object
  ierr = 0
  call kim_model_compute_arguments_get_argument_pointer( &
    model_compute_arguments_handle, &
    kim_compute_argument_name_number_of_particles, n, ierr2)
  ierr = ierr + ierr2
  call kim_model_compute_arguments_get_argument_pointer( &
    model_compute_arguments_handle, &
    kim_compute_argument_name_particle_species_codes, &
    n, particle_species_codes, ierr2)
  ierr = ierr + ierr2
  call kim_model_compute_arguments_get_argument_pointer( &
    model_compute_arguments_handle, &
    kim_compute_argument_name_particle_contributing, n, particle_contributing, &
    ierr2)
  ierr = ierr + ierr2
  call kim_model_compute_arguments_get_argument_pointer( &
    model_compute_arguments_handle, &
    kim_compute_argument_name_coordinates, 3, n, coor, ierr2)
  ierr = ierr + ierr2
  call kim_model_compute_arguments_get_argument_pointer( &
    model_compute_arguments_handle, &
    kim_compute_argument_name_partial_energy, energy, ierr2)
  ierr = ierr + ierr2
  if (ierr /= 0) then
    call kim_model_compute_arguments_log_entry(model_compute_arguments_handle, &
      kim_log_verbosity_error, &
      "Unable to get all argument pointers")
    return
  endif

  ! Check to be sure that the species are correct
  ierr = 1 ! assume an error
  do i = 1,n
    if (particle_species_codes(i).ne.species_code) then
      call kim_model_compute_log_entry(model_compute_handle, &
        kim_log_verbosity_error, &
        "Unexpected species code detected")
      return
    endif
  enddo
  ierr = 0 ! everything is good

  ! Initialize energy
  energy = 0.0_cd

  !  Loop over particles and compute energy
  do i = 1, n
    if (particle_contributing(i) == 1) then
      ! Set up neighbor list for next particle
      call kim_model_compute_arguments_get_neighbor_list( &
        model_compute_arguments_handle, 1, i, number_of_neighbors, &
        neighbors_of_particle, ierr)
      if (ierr /= 0) then
        call kim_model_compute_log_entry(model_compute_handle, &
          kim_log_verbosity_error, &
          "Unable to get neighbor list for particle")
        ierr = 1
        return
      endif

      ! Loop over the neighbors of particle i
      do jj = 1, number_of_neighbors
        j = neighbors_of_particle(jj)
        if (i < j) then  ! short-circuit half-list
          rij(:) = coor(:,j) - coor(:,i)
          rsq = dot_product(rij,rij)
          if ( rsq .lt. cutsq ) then
            ! Add energy contributions
            energy = energy + buf%hyperparameter
            if (particle_contributing(j) == 1) then
              energy = energy + buf%hyperparameter
            endif
          endif
        endif
     enddo  ! loop on jj
   endif  ! if particleContributing
 enddo ! do i

  ierr = 0  ! Everything is great
end subroutine model_compute

!-------------------------------------------------------------------------------

! No need to make any changes to the model_destroy() routine
subroutine model_destroy(model_destroy_handle, ierr) bind(c)
  implicit none

  type(kim_model_destroy_handle_type), intent(inout) :: model_destroy_handle
  integer(c_int), intent(out) :: ierr

  type(model_buffer_type), pointer :: buf; type(c_ptr) :: pbuf

  call kim_model_destroy_get_model_buffer_pointer(model_destroy_handle, pbuf)
  call c_f_pointer(pbuf, buf)
  call kim_model_destroy_log_entry(model_destroy_handle, &
    kim_log_verbosity_information, &
    "deallocating model buffer")
  deallocate(buf)

  ierr = 0  ! everything is good
end subroutine model_destroy

!-------------------------------------------------------------------------------
subroutine model_refresh(model_refresh_handle, ierr) bind(c)
  implicit none

  type(kim_model_refresh_handle_type), intent(inout) :: model_refresh_handle
  integer(c_int), intent(out) :: ierr

  type(model_buffer_type), pointer :: buf; type(c_ptr) :: pbuf

  call kim_model_refresh_get_model_buffer_pointer(model_refresh_handle, pbuf)
  call c_f_pointer(pbuf, buf)

  ! Nothing to be done.  This model driver does not publish any parameters

  call kim_model_refresh_set_influence_distance_pointer(model_refresh_handle, &
    buf%influence_distance)
  call kim_model_refresh_set_neighbor_list_pointers(model_refresh_handle, &
    1, buf%cutoff, &
    buf%model_will_not_request_neighbors_of_noncontributing_particles)

  ierr = 0  ! everything is good
end subroutine model_refresh

!-------------------------------------------------------------------------------
subroutine model_compute_arguments_create(model_compute_handle, &
  model_compute_arguments_create_handle, ierr) bind(c)
  use kim_model_compute_arguments_create_module, &
    log_entry => kim_model_compute_arguments_create_log_entry
  implicit none

  type(kim_model_compute_handle_type), intent(in) :: model_compute_handle
  type(kim_model_compute_arguments_create_handle_type), intent(inout) :: &
    model_compute_arguments_create_handle
  integer(c_int), intent(out) :: ierr

  ! avoid unused dummy argument warnings
  if (model_compute_handle .eq. kim_model_compute_null_handle) continue

  ! register arguments
  call kim_model_compute_arguments_create_set_argument_support_status( &
    model_compute_arguments_create_handle, &
    kim_compute_argument_name_partial_energy, &
    kim_support_status_required, ierr)
  if (ierr /= 0) then
    call kim_model_compute_arguments_create_log_entry( &
      model_compute_arguments_create_handle, &
      kim_log_verbosity_error, &
      "Unable to register arguments support_statuses")
    return
  end if

  ierr = 0  ! everything is good
end subroutine model_compute_arguments_create

!-------------------------------------------------------------------------------
subroutine model_compute_arguments_destroy(model_compute_handle, &
  model_compute_arguments_destroy_handle, ierr) bind(c)
  use kim_model_compute_arguments_destroy_module, &
    log_entry => kim_model_compute_arguments_destroy_log_entry
  implicit none

  type(kim_model_compute_handle_type), intent(in) :: model_compute_handle
  type(kim_model_compute_arguments_destroy_handle_type), intent(inout) :: &
    model_compute_arguments_destroy_handle
  integer(c_int), intent(out) :: ierr

  ! avoid unused dummy argument warnings
  if (model_compute_handle .eq. kim_model_compute_null_handle) continue
  if (model_compute_arguments_destroy_handle &
    .eq. kim_model_compute_arguments_destroy_null_handle) continue

  ! nothing to be done

  ierr = 0  ! everything is good
end subroutine model_compute_arguments_destroy

end module toy_model_driver

!-------------------------------------------------------------------------------
subroutine model_driver_create(model_driver_create_handle, &
  requested_length_unit, requested_energy_unit, requested_charge_unit, &
  requested_temperature_unit, requested_time_unit, ierr) bind(c)
use, intrinsic :: iso_c_binding
use toy_model_driver
use kim_model_driver_headers_module
implicit none

type(kim_model_driver_create_handle_type), intent(inout) :: &
  model_driver_create_handle
type(kim_length_unit_type), intent(in), value :: requested_length_unit
type(kim_energy_unit_type), intent(in), value :: requested_energy_unit
type(kim_charge_unit_type), intent(in), value :: requested_charge_unit
type(kim_temperature_unit_type), intent(in), value :: requested_temperature_unit
type(kim_time_unit_type), intent(in), value :: requested_time_unit
integer(c_int), intent(out) :: ierr

integer(c_int) :: ierr2
integer(c_int) :: number_of_parameter_files
character(len=1024, kind=c_char) :: parameter_file_name
character(len=8, kind=c_char) :: species_name_string
type(kim_species_name_type) :: species_name
real(c_double) :: unit_conversion_factor
type(model_buffer_type), pointer :: buffer

! use requested units (we'll convert parameters as needed below)
! We only make use of length and energy, other are unused
call kim_model_driver_create_set_units( &
  model_driver_create_handle, &
  requested_length_unit, &
  requested_energy_unit, &
  kim_charge_unit_unused, &
  kim_temperature_unit_unused, &
  kim_time_unit_unused, ierr)
if (ierr /= 0) then
  call kim_model_driver_create_log_entry(model_driver_create_handle, &
    kim_log_verbosity_error, &
    "Unable to set units")
  return
end if

! we'll use one-based numbering
call kim_model_driver_create_set_model_numbering( &
  model_driver_create_handle, kim_numbering_one_based, ierr)
if (ierr /= 0) then
  call kim_model_driver_create_log_entry(model_driver_create_handle, &
    kim_log_verbosity_error, &
    "Unable to set numbering")
  return
end if

! store callback pointers in KIM object
call kim_model_driver_create_set_compute_pointer( &
  model_driver_create_handle, kim_language_name_fortran, &
  c_funloc(model_compute), ierr)
call kim_model_driver_create_set_compute_arguments_create_pointer( &
  model_driver_create_handle, kim_language_name_fortran, &
  c_funloc(model_compute_arguments_create), ierr2)
ierr = ierr + ierr2
call kim_model_driver_create_set_compute_arguments_destroy_pointer( &
  model_driver_create_handle, kim_language_name_fortran, &
  c_funloc(model_compute_arguments_destroy), ierr2)
ierr = ierr + ierr2
call kim_model_driver_create_set_refresh_pointer( &
  model_driver_create_handle, kim_language_name_fortran, &
  c_funloc(model_refresh), ierr2)
ierr = ierr + ierr2
call kim_model_driver_create_set_destroy_pointer( &
  model_driver_create_handle, kim_language_name_fortran, &
  c_funloc(model_destroy), ierr2)
ierr = ierr + ierr2
if (ierr /= 0) then
  call kim_model_driver_create_log_entry(model_driver_create_handle, &
    kim_log_verbosity_error, &
    "Unable to store callback pointers")
  return
end if

! process parameter file
call kim_model_driver_create_get_number_of_parameter_files( &
  model_driver_create_handle, number_of_parameter_files)
if (number_of_parameter_files .ne. 1) then
  call kim_model_driver_create_log_entry(model_driver_create_handle, &
    kim_log_verbosity_error, &
    "Wrong number of parameter files")
  ierr = 1
  return
end if

! allocate model_buffer object and register it in the model_drier_create object
allocate( buffer )
call kim_model_driver_create_set_model_buffer_pointer( &
  model_driver_create_handle, c_loc(buffer))

! Read in model parameters from parameter file
!
call kim_model_driver_create_get_parameter_file_name( &
  model_driver_create_handle, 1, parameter_file_name, ierr)
if (ierr /= 0) then
  call kim_model_driver_create_log_entry(model_driver_create_handle, &
    kim_log_verbosity_error, &
    "Unable to get parameter file name")
  return
end if
open(10,file=parameter_file_name,status="old")
read(10,*,iostat=ierr,err=100) species_name_string
read(10,*,iostat=ierr,err=100) buffer%cutoff(1)  ! in A
read(10,*,iostat=ierr,err=100) buffer%hyperparameter  ! in eV
close(10)
goto 200
100 continue
! reading parameters failed
call kim_model_driver_create_log_entry(model_driver_create_handle, &
  kim_log_verbosity_error, &
  "Unable to read parameters")
ierr = 1
return
200 continue

! register species
call kim_species_name_from_string(trim(species_name_string), species_name)
call kim_model_driver_create_set_species_code( &
  model_driver_create_handle, species_name, species_code, ierr)
if (ierr /= 0) then
  call kim_model_driver_create_log_entry(model_driver_create_handle, &
    kim_log_verbosity_error, &
    "Unable to set species code")
  return
end if

! convert units of parameters
call kim_model_driver_create_convert_unit( &
  model_driver_create_handle, &
  kim_length_unit_a, &
  kim_energy_unit_unused, &
  kim_charge_unit_unused, &
  kim_temperature_unit_unused, &
  kim_time_unit_unused, &
  requested_length_unit, &
  requested_energy_unit, &
  requested_charge_unit, &
  requested_temperature_unit, &
  requested_time_unit, &
  1.0_cd, 0.0_cd, 0.0_cd, 0.0_cd, 0.0_cd, unit_conversion_factor, ierr)
if (ierr /= 0) then
  call kim_model_driver_create_log_entry(model_driver_create_handle, &
    kim_log_verbosity_error, &
    "Unable to convert length unit")
  return
endif
buffer%cutoff(1) = unit_conversion_factor * buffer%cutoff(1)

call kim_model_driver_create_convert_unit( &
  model_driver_create_handle, &
  kim_length_unit_unused, &
  kim_energy_unit_ev, &
  kim_charge_unit_unused, &
  kim_temperature_unit_unused, &
  kim_time_unit_unused, &
  requested_length_unit, &
  requested_energy_unit, &
  requested_charge_unit, &
  requested_temperature_unit, &
  requested_time_unit, &
  0.0_cd, 1.0_cd, 0.0_cd, 0.0_cd, 0.0_cd, unit_conversion_factor, ierr)
if (ierr /= 0) then
  call kim_model_driver_create_log_entry(model_driver_create_handle, &
    kim_log_verbosity_error, &
    "Unable to convert energy unit")
  return
endif
buffer%hyperparameter = unit_conversion_factor * buffer%hyperparameter

! Set remainder of parameters
buffer%influence_distance = buffer%cutoff(1)
buffer%model_will_not_request_neighbors_of_noncontributing_particles(1) = 1

! register influence distance
call kim_model_driver_create_set_influence_distance_pointer( &
  model_driver_create_handle, buffer%influence_distance)

! register cutoff
call kim_model_driver_create_set_neighbor_list_pointers( &
  model_driver_create_handle, 1, buffer%cutoff, &
  buffer%model_will_not_request_neighbors_of_noncontributing_particles)

ierr = 0  ! everything is good
end subroutine model_driver_create
