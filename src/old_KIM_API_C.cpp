//
// CDDL HEADER START
//
// The contents of this file are subject to the terms of the Common Development
// and Distribution License Version 1.0 (the "License").
//
// You can obtain a copy of the license at
// http://www.opensource.org/licenses/CDDL-1.0.  See the License for the
// specific language governing permissions and limitations under the License.
//
// When distributing Covered Code, include this CDDL HEADER in each file and
// include the License file in a prominent location with the name LICENSE.CDDL.
// If applicable, add the following below this CDDL HEADER, with the fields
// enclosed by brackets "[]" replaced with your own identifying information:
//
// Portions Copyright (c) [yyyy] [name of copyright owner]. All rights reserved.
//
// CDDL HEADER END
//

//
// Copyright (c) 2013--2017, Regents of the University of Minnesota.
// All rights reserved.
//
// Contributors:
//    Valeriu Smirichinski
//    Ryan S. Elliott
//    Ellad B. Tadmor
//

//
// Release: This file is part of the kim-api.git repository.
//


#include <iostream>
#include <cstring>
#include <stdarg.h>
#include <stdint.h>

#include "old_KIM_API_C.h"
#include "old_KIM_API.h"
#include "old_KIM_API_status.h"

namespace OLD_KIM
{

//global methods
int old_KIM_API_get_version(const char** const version)
{
  return KIM_API_model::get_version(version);
}

int old_KIM_API_get_version_major(int* const major)
{
  return KIM_API_model::get_version_major(major);
}

int old_KIM_API_get_version_minor(int* const minor)
{
  return KIM_API_model::get_version_minor(minor);
}

int old_KIM_API_get_version_patch(int* const patch)
{
  return KIM_API_model::get_version_patch(patch);
}

int old_KIM_API_get_version_prerelease(const char** const prerelease)
{
  return KIM_API_model::get_version_prerelease(prerelease);
}

int old_KIM_API_get_version_build_metadata(const char** const build_metadata)
{
  return KIM_API_model::get_version_build_metadata(build_metadata);
}

int old_KIM_API_version_newer(const char* const versionA,
                          const char* const versionB,
                          int* const result)
{
  return KIM_API_model::version_newer(versionA, versionB, result);
}

int old_KIM_API_get_version_model_major(void* kimmdl, int* const major)
{
  KIM_API_model* mdl = (KIM_API_model*) kimmdl;
  return mdl->get_version_model_major(major);
}

int old_KIM_API_get_version_model_minor(void* kimmdl, int* const minor)
{
  KIM_API_model* mdl = (KIM_API_model*) kimmdl;
  return mdl->get_version_model_minor(minor);
}
int old_KIM_API_get_version_simulator_major(void* kimmdl, int* const major)
{
  KIM_API_model* mdl = (KIM_API_model*) kimmdl;
  return mdl->get_version_simulator_major(major);
}

int old_KIM_API_get_version_simulator_minor(void* kimmdl, int* const minor)
{
  KIM_API_model* mdl = (KIM_API_model*) kimmdl;
  return mdl->get_version_simulator_minor(minor);
}

int old_KIM_API_string_init(void * kimmdl, const char *siminputstring, const char * mdlname){
     KIM_API_model * mdl;
    mdl = new KIM_API_model[1];
    int error = mdl->string_init(siminputstring,mdlname);
    if(error == KIM_STATUS_OK) {
        *(KIM_API_model **)kimmdl = mdl;
        return error;
    }
    else
    {
      *(KIM_API_model **)kimmdl=NULL;
      delete [] mdl;
      return error;
    }
 }

void old_KIM_API_free(void *kimmdl,int * error){
    KIM_API_model * mdl=*(KIM_API_model **) kimmdl;
    *error=KIM_STATUS_OK;
    if (mdl==NULL) return;
    mdl->free(error);
    delete [] mdl;
    *(KIM_API_model **) kimmdl=NULL;

}
void old_KIM_API_print(void *kimmdl,int * error){
    KIM_API_model * mdl=(KIM_API_model *) kimmdl;
    *error =KIM_STATUS_FAIL;
    if (mdl==NULL) return;
    std::cout<<(*mdl);
    *error=KIM_STATUS_OK;
}

int old_KIM_API_model_init(void * kimmdl){
    KIM_API_model * mdl=(KIM_API_model *) kimmdl;
    if(mdl->model_init()) return KIM_STATUS_OK;
    return KIM_STATUS_FAIL;
}

int old_KIM_API_get_model_kim_str_len(const char * modelname, int* const kimStringLen)
{
    return KIM_API_model::get_model_kim_str_len(modelname, kimStringLen);
}

int old_KIM_API_get_model_kim_str(const char * modelname, const char** const kimString)
{
    return KIM_API_model::get_model_kim_str(modelname, kimString);
}

int old_KIM_API_model_compute(void *kimmdl){

    KIM_API_model * mdl=(KIM_API_model *) kimmdl;

    return mdl->model_compute();

}
int old_KIM_API_model_reinit(void * kimmdl){
    KIM_API_model * mdl=(KIM_API_model *) kimmdl;
    if (mdl->model_reinit()) return KIM_STATUS_OK;
    return KIM_STATUS_FAIL;
}
int old_KIM_API_model_destroy(void * kimmdl){
    KIM_API_model * mdl=(KIM_API_model *) kimmdl;
    return mdl->model_destroy();
}

int old_KIM_API_get_num_model_species(void* kimmdl, int* numberSpecies,
                                  int* maxStringLength)
{
  KIM_API_model * mdl=(KIM_API_model *) kimmdl;
  return mdl->get_num_model_species(numberSpecies, maxStringLength);
}

int old_KIM_API_get_model_species(void* kimmdl, const int index,
                              const char** const speciesString)
{
  KIM_API_model * mdl=(KIM_API_model *) kimmdl;
  return mdl->get_model_species(index, speciesString);
}

int old_KIM_API_get_num_sim_species(void* kimmdl, int* numberSpecies,
                                int* maxStringLength)
{
  KIM_API_model * mdl=(KIM_API_model *) kimmdl;
  return mdl->get_num_sim_species(numberSpecies, maxStringLength);
}

int old_KIM_API_get_sim_species(void* kimmdl, const int index,
                            const char** const speciesString)
{
  KIM_API_model * mdl=(KIM_API_model *) kimmdl;
  return mdl->get_sim_species(index, speciesString);
}

int old_KIM_API_get_num_params(void* kimmdl, int* numberParameters,
                           int* maxStringLength)
{
  KIM_API_model * mdl=(KIM_API_model *) kimmdl;
  return mdl->get_num_params(numberParameters, maxStringLength);
}

int old_KIM_API_get_parameter(void* kimmdl, const int index,
                          const char** const parameterString)
{
  KIM_API_model * mdl=(KIM_API_model *) kimmdl;
  return mdl->get_parameter(index, parameterString);
}

int old_KIM_API_get_species_code(void * kimmdl, const char* species, int * error){
     KIM_API_model * mdl=(KIM_API_model *) kimmdl;
     return mdl->get_species_code(species,error);
}
void old_KIM_API_set_species_code(void * kimmdl, const char* species, int code, int * error){
     KIM_API_model * mdl=(KIM_API_model *) kimmdl;
     return mdl->set_species_code(species, code, error);
}



int old_KIM_API_get_neigh(void *kimmdl, int neighborListIndex, int request, int *numnei, int **nei1part){
    KIM_API_model * mdl=(KIM_API_model *) kimmdl;
    return mdl->get_neigh(neighborListIndex, request,numnei,nei1part);
}

int old_KIM_API_process_dEdr(void **ppkim, double * dE, double * dr, double **dx, int *i, int *j){
   return KIM_API_model::process_dEdr((KIM_API_model **)ppkim,dE,dr,dx,i,j);
}
int old_KIM_API_process_d2Edr2(void **ppkim, double * dE, double ** dr, double **dx,int **i, int **j){
   return KIM_API_model::process_d2Edr2((KIM_API_model **)ppkim,dE,dr,dx,i,j);
 }

int old_KIM_API_get_status_msg(const int status_code, const char** const status_msg)
{
  return KIM_API_model::get_status_msg(status_code, status_msg);
}

int old_KIM_API_report_error(int ln, const char *fl,const char *usermsg,int ier){
    return KIM_API_model::report_error(ln,fl,usermsg,ier);
}

void old_KIM_API_set_model_buffer(void* kimmdl,void *ob,int * ier){
    KIM_API_model * mdl=(KIM_API_model *) kimmdl;
    mdl->set_model_buffer(ob,ier);
}
void * old_KIM_API_get_model_buffer(void* kimmdl, int* ier){
    KIM_API_model * mdl=(KIM_API_model *) kimmdl;
    return mdl->get_model_buffer(ier);
}

void old_KIM_API_set_sim_buffer(void* kimmdl,void *ob,int * ier){
    KIM_API_model * mdl=(KIM_API_model *) kimmdl;
    mdl->set_sim_buffer(ob,ier);
}
void * old_KIM_API_get_sim_buffer(void* kimmdl, int* ier){
    KIM_API_model * mdl=(KIM_API_model *) kimmdl;
    return mdl->get_sim_buffer(ier);
}

//element access methods by name
int  old_KIM_API_set_data(void *kimmdl,const char *nm, intptr_t size, void *dt){
    KIM_API_model * mdl=(KIM_API_model *) kimmdl;
    if(mdl->set_data(nm,size,dt)) return KIM_STATUS_OK;
    return KIM_STATUS_FAIL;
}
int  old_KIM_API_set_method(void *kimmdl,const char *nm, intptr_t size, int const language, func_ptr dt){
    KIM_API_model * mdl=(KIM_API_model *) kimmdl;
    if(mdl->set_method(nm,size,language,dt)) return KIM_STATUS_OK;
    return KIM_STATUS_FAIL;
}
void * old_KIM_API_get_data(void *kimmdl,const char *nm,int *error){
    KIM_API_model * mdl=(KIM_API_model *) kimmdl;
    return mdl->get_data(nm,error);
}
func_ptr old_KIM_API_get_method(void *kimmdl,const char *nm,int * const language, int *error){
    KIM_API_model * mdl=(KIM_API_model *) kimmdl;
    return mdl->get_method(nm,language, error);
}

intptr_t old_KIM_API_get_size(void *kimmdl,const char *nm,int *error){
    KIM_API_model * mdl=(KIM_API_model *) kimmdl;
    return mdl->get_size(nm,error);
}

void old_KIM_API_set_compute(void *kimmdl,const char *nm, int flag, int *error){
    KIM_API_model * mdl=(KIM_API_model *) kimmdl;
    mdl->set_compute(nm, flag, error);
}

int old_KIM_API_get_compute(void *kimmdl,const char *nm, int * error){
    KIM_API_model * mdl=(KIM_API_model *) kimmdl;
    *error = KIM_STATUS_FAIL;
    int comp =mdl->get_compute(nm, error);
    return comp;
}


//multiple data set/get methods
//
void old_KIM_API_setm_data(void *kimmdl, int *err, int numargs, ... ){
    KIM_API_model *pkim = (KIM_API_model *) kimmdl;
    *err=KIM_STATUS_FAIL;
    va_list listPointer;
    va_start(listPointer,numargs);
    if(numargs % 4 != 0) {
        std::cout<<"setm_data: numargs must be multiple of 4"<<std::endl;
        *err=KIM_STATUS_NUMARGS_NOT_DIVISIBLE_BY_4;
        va_end(listPointer);
        return;
    }

    for (int i=0; i<numargs/4; i++){
        char *nm      = va_arg(listPointer, char *);
        intptr_t size = va_arg(listPointer, intptr_t);
        void *dt      = va_arg(listPointer, void *);

        int key       =va_arg(listPointer, int);
        if (key != 1 && key != 0 ){
            *err= KIM_STATUS_WRONG_GROUP_ARGUMENT_KEY;
            va_end(listPointer);
            return;
        }else if(key ==0) continue;

        if(dt==NULL) std::cout<<"setm_data: WARNING: for "<<nm<<" data is NULL\n";
        if(!pkim->set_data(nm,size,dt)){
            std::cout<<"setm_data: set data for "<<nm<<" failed\n";
            va_end(listPointer);
            return;
        }
    }

    *err=KIM_STATUS_OK;
    va_end(listPointer);
}
void old_KIM_API_setm_method(void *kimmdl, int *err, int numargs, ... ){
    KIM_API_model *pkim = (KIM_API_model *) kimmdl;
    *err=KIM_STATUS_FAIL;
    va_list listPointer;
    va_start(listPointer,numargs);
    if(numargs % 5 != 0) {
        std::cout<<"setm_method: numargs must be multiple of 5"<<std::endl;
        *err=KIM_STATUS_NUMARGS_NOT_DIVISIBLE_BY_4;
        va_end(listPointer);
        return;
    }

    for (int i=0; i<numargs/4; i++){
        char *nm      = va_arg(listPointer, char *);
        intptr_t size = va_arg(listPointer, intptr_t);
        int lang = va_arg(listPointer, int);
        func_ptr dt      = va_arg(listPointer, func_ptr);

        int key       =va_arg(listPointer, int);
        if (key != 1 && key != 0 ){
            *err= KIM_STATUS_WRONG_GROUP_ARGUMENT_KEY;
            va_end(listPointer);
            return;
        }else if(key ==0) continue;

        if(dt==NULL) std::cout<<"setm_method: WARNING: for "<<nm<<" data is NULL\n";
        if(!pkim->set_method(nm,size,lang,dt)){
            std::cout<<"setm_method: set data for "<<nm<<" failed\n";
            va_end(listPointer);
            return;
        }
    }

    *err=KIM_STATUS_OK;
    va_end(listPointer);
}

void old_KIM_API_getm_data(void *kimmdl, int *err,int numargs, ...){
    KIM_API_model *pkim = (KIM_API_model *) kimmdl;
    *err=KIM_STATUS_FAIL;
    va_list listPointer;
    va_start(listPointer,numargs);
    if(numargs % 3 != 0) {
        std::cout<<"getm_data: numargs must be multiple of 3"<<std::endl;
        *err=KIM_STATUS_NUMARGS_NOT_DIVISIBLE_BY_3;
        va_end(listPointer);
        return;
    }

    for (int i=0; i<numargs/3; i++){
        char *nm      = va_arg(listPointer, char *);
        void **dt      = va_arg(listPointer, void **);

        int key       =va_arg(listPointer, int);
        if (key != 1 && key != 0 ){
            *err= KIM_STATUS_WRONG_GROUP_ARGUMENT_KEY;
            va_end(listPointer);
            return;
        }else if(key ==0) continue;

        *dt = pkim->get_data(nm,err);
        if(*err != KIM_STATUS_OK){
            std::cout<<"getm_data: get data for "<<nm<<" failed\n";
            va_end(listPointer);
            return;
        }
    }

    *err=KIM_STATUS_OK;
    va_end(listPointer);
}
void old_KIM_API_getm_method(void *kimmdl, int *err,int numargs, ...){
    KIM_API_model *pkim = (KIM_API_model *) kimmdl;
    *err=KIM_STATUS_FAIL;
    va_list listPointer;
    va_start(listPointer,numargs);
    if(numargs % 4 != 0) {
        std::cout<<"getm_method: numargs must be multiple of 4"<<std::endl;
        *err=KIM_STATUS_NUMARGS_NOT_DIVISIBLE_BY_3;
        va_end(listPointer);
        return;
    }

    for (int i=0; i<numargs/3; i++){
        char *nm      = va_arg(listPointer, char *);
        int *lang = va_arg(listPointer, int*);
        func_ptr *dt      = va_arg(listPointer, func_ptr *);

        int key       =va_arg(listPointer, int);
        if (key != 1 && key != 0 ){
            *err= KIM_STATUS_WRONG_GROUP_ARGUMENT_KEY;
            va_end(listPointer);
            return;
        }else if(key ==0) continue;

        *dt = pkim->get_method(nm,lang,err);
        if(*err != KIM_STATUS_OK){
            std::cout<<"getm_method: get data for "<<nm<<" failed\n";
            va_end(listPointer);
            return;
        }
    }

    *err=KIM_STATUS_OK;
    va_end(listPointer);
}

void old_KIM_API_setm_compute(void *kimmdl, int *err, int numargs, ...){
    KIM_API_model *pkim = (KIM_API_model *) kimmdl;
    *err=KIM_STATUS_FAIL;
    va_list listPointer;
    va_start(listPointer,numargs);
    if(numargs % 3 != 0) {
        std::cout<<"setm_compute: numargs must be multiple of 3"<<std::endl;
        *err=KIM_STATUS_NUMARGS_NOT_DIVISIBLE_BY_3;
        va_end(listPointer);
        return;
    }

    for (int i=0; i<numargs/3; i++){
        char *nm      = va_arg(listPointer, char *);
        int compute_flag = va_arg(listPointer, int);

        int key       =va_arg(listPointer, int);
        if (key != 1 && key != 0 ){
            *err= KIM_STATUS_WRONG_GROUP_ARGUMENT_KEY;
            va_end(listPointer);
            return;
        }else if(key ==0) continue;

        if ((compute_flag ==1) || (compute_flag ==0)){
          pkim->set_compute(nm, compute_flag, err);
          if (*err != KIM_STATUS_OK)
          {
            std::cout<<"setm_comupte: unable to set compute for " << nm << "\n";
            va_end(listPointer);
            return;
          }
         }else{
            std::cout<<"setm_compute:  for "<<nm<<" failed: compute_flag must be 0 or 1\n";
            va_end(listPointer);
            return;
        }
    }

    *err=KIM_STATUS_OK;
    va_end(listPointer);
}

void old_KIM_API_getm_compute(void *kimmdl, int *err,int numargs, ...){
    KIM_API_model *pkim = (KIM_API_model *) kimmdl;
    *err=KIM_STATUS_FAIL;
    va_list listPointer;
    va_start(listPointer,numargs);
    if(numargs % 3 != 0) {
        std::cout<<"getm_compute: numargs must be multiple of 3"<<std::endl;
        *err=KIM_STATUS_NUMARGS_NOT_DIVISIBLE_BY_3;
        va_end(listPointer);
        return;
    }

    for (int i=0; i<numargs/3; i++){
        char *nm      = va_arg(listPointer, char *);
        int *compute_flag = va_arg(listPointer, int*);

        int key       =va_arg(listPointer, int);
        if (key != 1 && key != 0 ){
            *err= KIM_STATUS_WRONG_GROUP_ARGUMENT_KEY;
            va_end(listPointer);
            return;
        }else if(key ==0) continue;

        *compute_flag = pkim->get_compute(nm,err);
        if (*err != KIM_STATUS_OK){
           std::cout<<"getm_compute:  name "<<nm<<" not in KIM object\n";
           va_end(listPointer);
           return;
        }
    }

    *err=KIM_STATUS_OK;
    va_end(listPointer);

}

//related to Unit_Handling
double old_KIM_API_get_scale_conversion(const char *u_from,const char *u_to, int *error){
    return KIM_API_model::get_scale_conversion(u_from,u_to,error);
}
int    old_KIM_API_get_unit_handling(void *kimmdl, int *error){
    return ((KIM_API_model *)kimmdl)->get_unit_handling(error);
}
char const * const old_KIM_API_get_unit_length(void *kimmdl, int *error){
    return ((KIM_API_model *)kimmdl)->get_unit_length(error);
}
char const * const old_KIM_API_get_unit_energy(void *kimmdl, int *error){
    return ((KIM_API_model *)kimmdl)->get_unit_energy(error);
}
char const * const old_KIM_API_get_unit_charge(void *kimmdl, int *error){
    return ((KIM_API_model *)kimmdl)->get_unit_charge(error);
}
char const * const old_KIM_API_get_unit_temperature(void *kimmdl, int *error){
    return ((KIM_API_model *)kimmdl)->get_unit_temperature(error);
}
char const * const old_KIM_API_get_unit_time(void *kimmdl, int *error){
    return ((KIM_API_model *)kimmdl)->get_unit_time(error);
}
double old_KIM_API_convert_to_act_unit(void * kimmdl,
                                const char *length,
                                const char *energy,
                                const char *charge,
                                const char *temperature,
                                const char *time,
                                double length_exponent,
                                double energy_exponent,
                                double charge_exponent,
                                double temperature_exponent,
                                double time_exponent,
                                int *kimerror){
    return ((KIM_API_model *)kimmdl)->convert_to_act_unit(length,energy,charge,temperature,time,
            length_exponent,energy_exponent,charge_exponent,temperature_exponent,time_exponent, kimerror);
}

}