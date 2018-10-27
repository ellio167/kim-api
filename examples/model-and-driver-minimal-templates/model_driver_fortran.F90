!@@ Replace MODEL_DRIVER_NAME with your preferred identifier


module MODEL_DRIVER_NAME

use, intrinsic :: iso_c_binding
use kim_model_driver_headers_module
implicit none

private
public model_compute, &
       model_refresh, &
       model_destroy, &
       model_compute_arguments_create, &
       model_compute_arguments_destroy, &
       model_buffer_type
!@@ Add identifiers here as needed

!@@ Add declarations here as needed

type, bind(c) :: model_buffer_type
  real(c_double) :: influence_distance
  !@@ Add declarations here as needed
end type model_buffer_type

contains

!@@ Add routines here as needed

!-------------------------------------------------------------------------------
#include "kim_model_compute_log_macros.fd"
subroutine model_compute(model_compute_handle, &
  model_compute_arguments_handle, ierr) bind(c)
  implicit none

  type(kim_model_compute_handle_type), intent(in) :: model_compute_handle
  type(kim_model_compute_arguments_handle_type), intent(in) :: &
    model_compute_arguments_handle
  integer(c_int), intent(out) :: ierr


  !@@ Add declarations here as needed

  kim_log_file = __FILE__

  !@@ Add code here as needed

  ierr = 0  ! Everything is great
end subroutine model_compute

!-------------------------------------------------------------------------------

! No need to make any changes to the model_destroy() routine
#include "kim_model_destroy_log_macros.fd"
subroutine model_destroy(model_destroy_handle, ierr) bind(c)
  implicit none

  type(kim_model_destroy_handle_type), intent(inout) :: model_destroy_handle
  integer(c_int), intent(out) :: ierr

  type(model_buffer_type), pointer :: buf; type(c_ptr) :: pbuf

  kim_log_file = __FILE__

  call kim_model_destroy_get_model_buffer_pointer(model_destroy_handle, pbuf)
  call c_f_pointer(pbuf, buf)
  kim_log_message = "deallocating model buffer"
  LOG_INFORMATION()
  deallocate(buf)

  ierr = 0  ! everything is good
end subroutine model_destroy

!-------------------------------------------------------------------------------
#include "kim_model_refresh_log_macros.fd"
subroutine model_refresh(model_refresh_handle, ierr) bind(c)
  implicit none

  type(kim_model_refresh_handle_type), intent(inout) :: model_refresh_handle
  integer(c_int), intent(out) :: ierr

  type(model_buffer_type), pointer :: buf; type(c_ptr) :: pbuf

  kim_log_file = __FILE__

  call kim_model_refresh_get_model_buffer_pointer(model_refresh_handle, pbuf)
  call c_f_pointer(pbuf, buf)

  !@@ Add code here as necessary

  ierr = 0  ! everything is good
end subroutine model_refresh

!-------------------------------------------------------------------------------
#include "kim_model_compute_arguments_create_log_macros.fd"
subroutine model_compute_arguments_create(model_compute_handle, &
  model_compute_arguments_create_handle, ierr) bind(c)
  use kim_model_compute_arguments_create_module, &
    log_entry => kim_model_compute_arguments_create_log_entry
  implicit none

  type(kim_model_compute_handle_type), intent(in) :: model_compute_handle
  type(kim_model_compute_arguments_create_handle_type), intent(inout) :: &
    model_compute_arguments_create_handle
  integer(c_int), intent(out) :: ierr

  kim_log_file = __FILE__

  !@@ Add code here as necessary

  ierr = 0  ! everything is good
end subroutine model_compute_arguments_create

!-------------------------------------------------------------------------------
#include "kim_model_compute_arguments_destroy_log_macros.fd"
subroutine model_compute_arguments_destroy(model_compute_handle, &
  model_compute_arguments_destroy_handle, ierr) bind(c)
  use kim_model_compute_arguments_destroy_module, &
    log_entry => kim_model_compute_arguments_destroy_log_entry
  implicit none

  type(kim_model_compute_handle_type), intent(in) :: model_compute_handle
  type(kim_model_compute_arguments_destroy_handle_type), intent(inout) :: &
    model_compute_arguments_destroy_handle
  integer(c_int), intent(out) :: ierr

  kim_log_file = __FILE__

  !@@ Add code here as necessary

  ierr = 0  ! everything is good
end subroutine model_compute_arguments_destroy

end module MODEL_DRIVER_NAME

!-------------------------------------------------------------------------------
#include "kim_model_driver_create_log_macros.fd"
subroutine model_driver_create(model_driver_create_handle, &
  requested_length_unit, requested_energy_unit, requested_charge_unit, &
  requested_temperature_unit, requested_time_unit, ierr) bind(c)
use, intrinsic :: iso_c_binding
use MODEL_DRIVER_NAME
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

!@@ Add declarations here as necessary

kim_log_file = __FILE__

!@@ Add code here as necessary

ierr = 0  ! everything is good
end subroutine model_driver_create
