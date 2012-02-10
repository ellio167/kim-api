
#include <stdlib.h>
#include <iostream>
#include <fstream>
#include <string.h>
#include <sstream>
#include <math.h>
#include "Unit_Handling.h"

using namespace std;

char * Unit_Handling::derived_list[]={
    "dipole", "density", "dynamic_viscosity", "electric_field",
    "force", "mass","pressure", "stress", "torque", "velocity"
};
int Unit_Handling::nderived_list=10;


char * Unit_Handling::base_list[]={
    "length",  "energy", "charge", "temperature", "time"
};
int Unit_Handling::nbase_list=5;

 // list of supported base units for Unit_length
char * Unit_Handling::length_list[]={
     "A" ,               "Bohr" ,   "cm" ,  "m" , "nm"
};
double Unit_Handling::length_scale[]={
  1.0e-10, 5.291772109217171e-11, 1.0e-2 ,  1.0 , 1.0e-9
};
int Unit_Handling::nlength_list=5;

// list of supported base units for Unit_energy
char * Unit_Handling::energy_list[]={
  "amu*A^2/(ps)^2",  "erg",   "eV" ,          "Hartree" ,  "J", "kcal/mol" , " kJ/mol"
};
double Unit_Handling::energy_scale[]={
    1.66053886e-23,  1.0e7, 1.60217646e-19,  4.3597439422e-18, 1.0,   6.9477e-21, 1.66054e-21
};
int Unit_Handling::nenergy_list=7;

// list of supported base units for Unit_charge
char * Unit_Handling::charge_list[]={
     "C" ,        "e",   "statC"
};
double Unit_Handling::charge_scale[]={
     1.0,    1.602e-19,     2.99792458e-9 //
};
int Unit_Handling::ncharge_list=3;

// list of supported base units for Unit_temperature
char * Unit_Handling::temperature_list[]={
     "K"
};
double Unit_Handling::temperature_scale[]={
     1.0
};
int Unit_Handling::ntemperature_list=1;

// list of supported base units for Unit_time
char * Unit_Handling::time_list[]={
   "fs" ,      "ps" ,  "ns",  "s"
};
double Unit_Handling::time_scale[]={
   1.0e-15,    1.0e-12,  1.0e-9,  1.0
};
int Unit_Handling::ntime_list=4;




Unit_Handling::Unit_Handling(){
    char tmp[]="none";
    strcpy(Unit_length,tmp);
    strcpy(Unit_energy,tmp);
    strcpy(Unit_charge,tmp);
    strcpy(Unit_temperature,tmp);
    strcpy(Unit_time,tmp);
    flexible_handling=false;
}

Unit_Handling:: ~Unit_Handling(){
     
}

void Unit_Handling::check_base_set_flexible(IOline* lines, int nlines, int* error){
    *error =KIM_STATUS_FAIL;
  
    for (int i=0;i<nlines;i++){
        if(strcmp(lines[i].name,"Unit_Handling")==0){
            if(strcmp(lines[i].value,"flexible")==0) {
                flexible_handling=true;
            }else if(strcmp(lines[i].value,"fixed")==0) {
                flexible_handling=false;
            }else{
                *error = KIM_STATUS_WRONG_UNIT_HANDLING;
                return;
            }
        }else if(strcmp(lines[i].name,"Unit_length")==0){
            strcpy(Unit_length,lines[i].value);
        }else if(strcmp(lines[i].name,"Unit_energy")==0){
            strcpy(Unit_energy,lines[i].value);
        }else if(strcmp(lines[i].name,"Unit_charge")==0){
            strcpy(Unit_charge,lines[i].value);
        }else if(strcmp(lines[i].name,"Unit_temperature")==0){
            strcpy(Unit_temperature,lines[i].value);
        }else if(strcmp(lines[i].name,"Unit_time")==0){
            strcpy(Unit_time,lines[i].value);
        }
    }
    if(     ! is_Unit_length_supported()) {
        *error = KIM_STATUS_UNSUPPORTED_UNIT_LENGTH;
        return;
    }else if(! is_Unit_energy_supported()){
        *error = KIM_STATUS_UNSUPPORTED_UNIT_ENERGY;
        return;
    }else if(! is_Unit_charge_supported()){
        *error = KIM_STATUS_UNSUPPORTED_UNIT_CHARGE;
        return;
    }else if(! is_Unit_temperature_supported()){
        *error = KIM_STATUS_UNSUPPORTED_UNIT_TEMPERATURE;
        return;
    }else if(! is_Unit_time_supported()){
        *error = KIM_STATUS_UNSUPPORTED_UNIT_TIME;
        return;
    }
    *error =KIM_STATUS_OK;
}
bool Unit_Handling::is_Unit_length_supported(){
    bool result = false;
    for(int i=0;i<nlength_list;i++){
        if (strcmp(Unit_length,length_list[i])==0) result = true;
    }
    return result;
}
bool Unit_Handling::is_Unit_energy_supported(){
    bool result = false;
    for(int i=0;i<nenergy_list;i++){
        if (strcmp(Unit_energy,energy_list[i])==0) result = true;
    }
    return result;
}
bool Unit_Handling::is_Unit_charge_supported(){
    bool result = false;
    for(int i=0;i<ncharge_list;i++){
        if (strcmp(Unit_charge,charge_list[i])==0) result = true;
    }
    return result;
}
bool Unit_Handling::is_Unit_temperature_supported(){
    bool result = false;
    for(int i=0;i<ntemperature_list;i++){
        if (strcmp(Unit_temperature,temperature_list[i])==0) result = true;
    }
    return result;
}
bool Unit_Handling::is_Unit_time_supported(){
    bool result = false;
    for(int i=0;i<ntime_list;i++){
        if (strcmp(Unit_time,time_list[i])==0) result = true;
    }
    return result;
}

bool Unit_Handling::is_it_Unit_length(char* unit,int *index){
     bool result = false;
    for(int i=0;i<nlength_list;i++){
        if (strcmp(unit,length_list[i])==0) {
            result = true;
            *index = i;
            break;
        }
    }
    return result;
}
bool Unit_Handling::is_it_Unit_energy(char* unit,int *index){
    bool result = false;
    for(int i=0;i<nenergy_list;i++){
        if (strcmp(unit,energy_list[i])==0) {
            result = true;
            *index = i;
            break;
        }
    }
    return result;
}
bool Unit_Handling::is_it_Unit_charge(char* unit, int *index){
    bool result = false;
    for(int i=0;i<ncharge_list;i++){
        if (strcmp(unit,charge_list[i])==0){
            result = true;
            *index = i;
            break;
        }
    }
    return result;
}
bool Unit_Handling::is_it_Unit_temperature(char* unit, int *index){
    bool result = false;
    for(int i=0;i<ntemperature_list;i++){
        if (strcmp(unit,temperature_list[i])==0){
            result = true;
            *index = i;
            break;
        }
    }
    return result;
}
bool Unit_Handling::is_it_Unit_time(char* unit,int *index){
    bool result = false;
    for(int i=0;i<ntime_list;i++){
        if (strcmp(unit,time_list[i])==0){
            result = true;
            *index = i;
            break;
        }
    }
    return result;
}

void Unit_Handling::init_str(char* inputstr, int* error){
    *error = KIM_STATUS_FAIL;
    IOline * IOlines=NULL;
    int nlines = IOline::readlines_str(inputstr,&IOlines);
    this->check_base_set_flexible(IOlines,nlines,error);
    if (IOlines!= NULL) delete [] IOlines;
    return;
}
void Unit_Handling::init(char* inputstr, int* error){
    *error = KIM_STATUS_FAIL;
    IOline * IOlines=NULL;
    int nlines = IOline::readlines(inputstr,&IOlines);
    this->check_base_set_flexible(IOlines,nlines,error);
    if (IOlines!= NULL) delete [] IOlines;
    return;
}
bool Unit_Handling::is_it_base(char* unit){
    bool result = false;
    for(int i=0;i<nbase_list;i++){
        if (strcmp(unit,base_list[i])==0) result = true;
    }
    return result;
}
bool Unit_Handling::is_it_derived(char* unit){
    bool result = false;
    for(int i=0;i<nderived_list;i++){
        if (strcmp(unit,derived_list[i])==0) result = true;
    }
    return result;
}
double Unit_Handling::get_scale_conversion(char *u_from,char *u_to, int *error){
    *error=KIM_STATUS_FAIL;
     int ind_from =-1, ind_to=-1;
     double convert;

    // compute multiplicative conversion factor for units (i.e., covariant transformation)
    if       (is_it_Unit_length(u_from,&ind_from) && is_it_Unit_length(u_to,&ind_to)){
       *error=KIM_STATUS_OK;
       convert = length_scale[ind_to]/length_scale[ind_from];
    }else if (is_it_Unit_energy(u_from,&ind_from) && is_it_Unit_energy(u_to,&ind_to)){
       *error=KIM_STATUS_OK;
       convert = energy_scale[ind_to]/energy_scale[ind_from];
    }else if (is_it_Unit_charge(u_from,&ind_from) && is_it_Unit_charge(u_to,&ind_to)){
       *error=KIM_STATUS_OK;
       convert = charge_scale[ind_to]/charge_scale[ind_from];
    }else if (is_it_Unit_temperature(u_from,&ind_from) && is_it_Unit_temperature(u_to,&ind_to)){
       *error=KIM_STATUS_OK;
       convert = temperature_scale[ind_to]/temperature_scale[ind_from];
    }else if (is_it_Unit_time(u_from,&ind_from) && is_it_Unit_time(u_to,&ind_to)){
       *error=KIM_STATUS_OK;
       convert = time_scale[ind_to]/time_scale[ind_from];
    }else{
        *error=KIM_STATUS_INCONSISTENT_BASE_UNIT;
        convert = -1.0;
    }

    // return multiplicative conversion factor for components (i.e., contravariant transforamtion)
    return 1.0/convert;
}
bool Unit_Handling::do_unit_match(Unit_Handling &test, Unit_Handling  &model){
    if(model.flexible_handling){
        model = test;
        model.flexible_handling=true;
        return true;
    }
    if(strcmp(test.Unit_length,model.Unit_length)==0)
        if(strcmp(test.Unit_energy,model.Unit_energy)==0)
            if(strcmp(test.Unit_charge,model.Unit_charge)==0)
                if(strcmp(test.Unit_temperature,model.Unit_temperature)==0)
                    if(strcmp(test.Unit_time,model.Unit_time)==0) return true;
    return false;
}
void Unit_Handling::print(){
    cout<<"Unit_length : "<<Unit_length<<endl;
    cout<<"Unit_energy : "<<Unit_energy<<endl;
    cout<<"Unit_charge : "<<Unit_charge<<endl;
    cout<<"Unit_temperature : "<<Unit_temperature<<endl;
    cout<<"Unit_time : "<<Unit_time<<endl;
    cout<<"flexible_handling :"<< flexible_handling<<endl;
}
void Unit_Handling::print(ostream& stream){
    stream<<"Unit_length : "<<Unit_length<<endl;
    stream<<"Unit_energy : "<<Unit_energy<<endl;
    stream<<"Unit_charge : "<<Unit_charge<<endl;
    stream<<"Unit_temperature : "<<Unit_temperature<<endl;
    stream<<"Unit_time : "<<Unit_time<<endl;
    stream<<"flexible_handling :"<< flexible_handling<<endl;
}
int Unit_Handling::get_unit_handling(int *error){
    if(flexible_handling) return 1;
    *error = KIM_STATUS_OK;
    return 0;
}
char * Unit_Handling::get_unit_length(int *error){
    *error = KIM_STATUS_FAIL;
    char *tmp = (char *)malloc(KIM_KEY_STRING_LENGTH);
    for(int i=0;i<KIM_KEY_STRING_LENGTH;i++) tmp[i]='\0';
    strcpy(tmp,Unit_length);
    *error = KIM_STATUS_OK;
    return tmp;
}
char * Unit_Handling::get_unit_energy(int *error){
    *error = KIM_STATUS_FAIL;
    char *tmp = (char *)malloc(KIM_KEY_STRING_LENGTH);
    for(int i=0;i<KIM_KEY_STRING_LENGTH;i++) tmp[i]='\0';
    strcpy(tmp,Unit_energy);
    *error = KIM_STATUS_OK;
    return tmp;
}
char * Unit_Handling::get_unit_charge(int *error){
    *error = KIM_STATUS_FAIL;
    char *tmp = (char *)malloc(KIM_KEY_STRING_LENGTH);
    for(int i=0;i<KIM_KEY_STRING_LENGTH;i++) tmp[i]='\0';
    strcpy(tmp,Unit_charge);
    *error = KIM_STATUS_OK;
    return tmp;
}
char * Unit_Handling::get_unit_temperature(int *error){
    *error = KIM_STATUS_FAIL;
    char *tmp = (char *)malloc(KIM_KEY_STRING_LENGTH);
    for(int i=0;i<KIM_KEY_STRING_LENGTH;i++) tmp[i]='\0';
    strcpy(tmp,Unit_temperature);
    *error = KIM_STATUS_OK;
    return tmp;
}
char * Unit_Handling::get_unit_time(int *error){
    *error = KIM_STATUS_FAIL;
    char *tmp = (char *)malloc(KIM_KEY_STRING_LENGTH);
    for(int i=0;i<KIM_KEY_STRING_LENGTH;i++) tmp[i]='\0';
    strcpy(tmp,Unit_time);
    *error = KIM_STATUS_OK;
    return tmp;
}
ostream &operator<<(ostream &stream, Unit_Handling &a){
    a.print(stream);
}

double Unit_Handling::convert_to_act_unit(void *kim,
      char* length, char* energy, char* charge, char* temperature, char* time,
      double length_exponent, double energy_exponent, double charge_exponent,
      double temperature_exponent, double time_exponent, int* kimerror){
      KIM_API_model *kimmdl = (KIM_API_model *)kim;
      char *to_length = kimmdl->get_unit_length(kimerror);
      char *to_energy =kimmdl->get_unit_energy(kimerror);
      char *to_charge =kimmdl->get_unit_charge(kimerror);
      char *to_temperature = kimmdl->get_unit_temperature(kimerror);
      char *to_time = kimmdl->get_unit_time(kimerror);

      int error_length,error_energy,error_charge,error_temperature,error_time;
      double scale_length = kimmdl->get_scale_conversion(length,to_length,&error_length);
      double scale_energy = kimmdl->get_scale_conversion(energy,to_energy,&error_energy);
      double scale_charge = kimmdl->get_scale_conversion(charge,to_charge,&error_charge);
      double scale_temperature = kimmdl->get_scale_conversion(temperature,to_temperature,&error_temperature);
      double scale_time = kimmdl->get_scale_conversion(time,to_time,&error_time);

      free(to_length);free(to_energy);free(to_charge);free(to_temperature);free(to_time);

      if      (error_length < KIM_STATUS_OK){
              *kimerror = error_length;
              return -1.0;
      }else if(error_energy < KIM_STATUS_OK){
              *kimerror = error_energy;
              return -1.0;
      }else if(error_charge < KIM_STATUS_OK){
              *kimerror = error_charge;
              return -1.0;
      }else if(error_temperature < KIM_STATUS_OK){
              *kimerror = error_temperature;
              return -1.0;
      }else if(error_time < KIM_STATUS_OK){
              *kimerror = error_time;
              return -1.0;
      }

      return pow(scale_length,length_exponent)*pow(scale_energy,energy_exponent)*
             pow(scale_charge,charge_exponent)*pow(scale_temperature,temperature_exponent)*
             pow(scale_time, time_exponent);
      
}