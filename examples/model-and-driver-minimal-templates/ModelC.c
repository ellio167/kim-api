#include "KIM_ModelHeaders.h"
#include "stdlib.h"

#define TRUE 1
#define FALSE 0

struct ModelBuffer
{
  double influenceDistance;
  /*@@ Add declarations here as needed */
};
typedef struct ModelBuffer ModelBuffer;

/*----------------------------------------------------------------------------*/
int ModelCreate(KIM_ModelCreate * const modelCreate,
                KIM_LengthUnit const requestedLengthUnit,
                KIM_EnergyUnit const requestedEnergyUnit,
                KIM_ChargeUnit const requestedChargeUnit,
                KIM_TemperatureUnit const requestedTemperatureUnit,
                KIM_TimeUnit const requestedTimeUnit);

/*----------------------------------------------------------------------------*/
static int
ModelCompute(KIM_ModelCompute const * const modelCompute,
             KIM_ModelComputeArguments const * const modelComputeArguments);

/*----------------------------------------------------------------------------*/
static int ModelComputeArgumentsCreate(
    KIM_ModelCompute const * const modelCompute,
    KIM_ModelComputeArgumentsCreate * const modelComputeArgumentsCreate);

/*----------------------------------------------------------------------------*/
static int ModelComputeArgumentsDestroy(
    KIM_ModelCompute const * const modelCompute,
    KIM_ModelComputeArgumentsDestroy * const modelComputeArgumentsDestroy);

/*----------------------------------------------------------------------------*/
static int ModelRefresh(KIM_ModelRefresh * const modelRefresh);

/*----------------------------------------------------------------------------*/
static int ModelDestroy(KIM_ModelDestroy * const modelDestroy);

/*----------------------------------------------------------------------------*/
/*----------------------------------------------------------------------------*/

#include "KIM_ModelComputeLogMacros.h"
static int
ModelCompute(KIM_ModelCompute const * const modelCompute,
             KIM_ModelComputeArguments const * const modelComputeArguments)
{
  /*@@ Add code here as necessary */

  /* everything is great */
  return FALSE;
}

/*----------------------------------------------------------------------------*/
#include "KIM_ModelCreateLogMacros.h"
int ModelCreate(KIM_ModelCreate * const modelCreate,
                KIM_LengthUnit const requestedLengthUnit,
                KIM_EnergyUnit const requestedEnergyUnit,
                KIM_ChargeUnit const requestedChargeUnit,
                KIM_TemperatureUnit const requestedTemperatureUnit,
                KIM_TimeUnit const requestedTimeUnit)
{
  ModelBuffer * modelBufferPointer;

  /*@@ Add code here as necessary */

  modelBufferPointer = (ModelBuffer *) malloc(sizeof(ModelBuffer));

  /* store model buffer in KIM object */
  LOG_INFORMATION("Set influence distance and cutoffs");
  KIM_ModelCreate_SetModelBufferPointer(modelCreate, modelBufferPointer);

  /* set buffer values */
  modelBufferPointer->influenceDistance = 0.0; /*@@ update value */

  /*@@ Add code here as necessary */

  /* everything is good */
  return FALSE;
}

/*----------------------------------------------------------------------------*/
#include "KIM_ModelRefreshLogMacros.h"
static int ModelRefresh(KIM_ModelRefresh * const modelRefresh)
{
  ModelBuffer * modelBufferPointer;

  KIM_ModelRefresh_GetModelBufferPointer(modelRefresh,
                                         (void **) &modelBufferPointer);

  /*@@ Add code here as necessary */

  /* everything is good */
  return FALSE;
}

/*----------------------------------------------------------------------------*/

/* No need to make any changes to ModelDestroy() routine */
#include "KIM_ModelDestroyLogMacros.h"
int ModelDestroy(KIM_ModelDestroy * const modelDestroy)
{
  ModelBuffer * modelBufferPointer;

  KIM_ModelDestroy_GetModelBufferPointer(modelDestroy,
                                         (void **) &modelBufferPointer);
  LOG_INFORMATION("Freeing model memory");
  free(modelBufferPointer);

  /* everything is good */
  return FALSE;
}

/*----------------------------------------------------------------------------*/
#include "KIM_ModelComputeArgumentsCreateLogMacros.h"
static int ModelComputeArgumentsCreate(
    KIM_ModelCompute const * const modelCompute,
    KIM_ModelComputeArgumentsCreate * const modelComputeArgumentsCreate)
{
  /*@@ Add code here as necessary */

  /* everything is good */
  return FALSE;
}

/*----------------------------------------------------------------------------*/
#include "KIM_ModelComputeArgumentsDestroyLogMacros.h"
static int ModelComputeArgumentsDestroy(
    KIM_ModelCompute const * const modelCompute,
    KIM_ModelComputeArgumentsDestroy * const modelComputeArgumentsDestroy)
{
  /*@@ Add code here as necessary */

  /* everything is good */
  return FALSE;
}
