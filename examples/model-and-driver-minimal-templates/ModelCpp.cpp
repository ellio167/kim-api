//@@ Replace MODEL_NAME with your preferred identifier


#include "KIM_ModelHeaders.hpp"

namespace
{
class MODEL_NAME
{
 public:
  //****************************************************************************
  MODEL_NAME(KIM::ModelCreate * const modelCreate,
             KIM::LengthUnit const requestedLengthUnit,
             KIM::EnergyUnit const requestedEnergyUnit,
             KIM::ChargeUnit const requestedChargeUnit,
             KIM::TemperatureUnit const requestedTemperatureUnit,
             KIM::TimeUnit const requestedTimeUnit,
             int * const error)
  {
    //@@ Add code here as needed

    // everything is good
    *error = false;
    return;
  };

  //****************************************************************************
  ~MODEL_NAME() {};

  //****************************************************************************
  // no need to make these "extern" since KIM will only access them
  // via function pointers.  "static" is required so that there is not
  // an implicit this pointer added to the prototype by the C++ compiler

  // No need to make any changes to the Destroy() routine
  static int Destroy(KIM::ModelDestroy * const modelDestroy)
  {
    MODEL_NAME * model;
    modelDestroy->GetModelBufferPointer(reinterpret_cast<void **>(&model));

    if (model != NULL)
    {
      // delete object itself
      delete model;
      model = NULL;
    }

    // everything is good
    return false;
  }

  //****************************************************************************
  static int Refresh(KIM::ModelRefresh * const modelRefresh)
  {
    MODEL_NAME * model;
    modelRefresh->GetModelBufferPointer(reinterpret_cast<void **>(&model));

    //@@ Add code here as needed

    // everything is good
    return false;
  };

    //****************************************************************************
#include "KIM_ModelComputeLogMacros.hpp"
  static int
  Compute(KIM::ModelCompute const * const modelCompute,
          KIM::ModelComputeArguments const * const modelComputeArguments)
  {
    //@@ Add code here as needed

    // everything is good
    return false;
  };

  //****************************************************************************
  static int ComputeArgumentsCreate(
      KIM::ModelCompute const * const modelCompute,
      KIM::ModelComputeArgumentsCreate * const modelComputeArgumentsCreate)
  {
    //@@ Add code here as needed

    // everything is good
    return false;
  }
  //****************************************************************************
  static int ComputeArgumentsDestroy(
      KIM::ModelCompute const * const modelCompute,
      KIM::ModelComputeArgumentsDestroy * const modelComputeArgumentsDestroy)
  {
    //@@ Add code here as needed

    // everything is good
    return false;
  };

 private:
  //****************************************************************************
  // Member variables

  //@@ Add variables here as needed

  //****************************************************************************
  // Member routines

  //@@ Add routines here as needed
};

}  // namespace

extern "C" {
//******************************************************************************
//
// No need to make any changes to the ModelCreate() routine
int ModelCreate(KIM::ModelCreate * const modelCreate,
                KIM::LengthUnit const requestedLengthUnit,
                KIM::EnergyUnit const requestedEnergyUnit,
                KIM::ChargeUnit const requestedChargeUnit,
                KIM::TemperatureUnit const requestedTemperatureUnit,
                KIM::TimeUnit const requestedTimeUnit)
{
  int error;

  MODEL_NAME * const model = new MODEL_NAME(modelCreate,
                                            requestedLengthUnit,
                                            requestedEnergyUnit,
                                            requestedChargeUnit,
                                            requestedTemperatureUnit,
                                            requestedTimeUnit,
                                            &error);
  if (error)
  {
    // constructor already reported the error
    delete model;
    return error;
  }

  // register pointer to MODEL_NAME object in moedelCreate object
  modelCreate->SetModelBufferPointer(reinterpret_cast<void *>(model));

  // everything is good
  return false;
}
}  // extern "C"
