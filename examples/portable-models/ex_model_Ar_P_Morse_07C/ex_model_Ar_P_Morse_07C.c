/*                                                                            */
/* KIM-API: An API for interatomic models                                     */
/* Copyright (c) 2013--2021, Regents of the University of Minnesota.          */
/* All rights reserved.                                                       */
/*                                                                            */
/* Contributors:                                                              */
/*    Ryan S. Elliott                                                         */
/*    Ellad B. Tadmor                                                         */
/*    Stephen M. Whalen                                                       */
/*                                                                            */
/* SPDX-License-Identifier: LGPL-2.1-or-later                                 */
/*                                                                            */
/* This library is free software; you can redistribute it and/or              */
/* modify it under the terms of the GNU Lesser General Public                 */
/* License as published by the Free Software Foundation; either               */
/* version 2.1 of the License, or (at your option) any later version.         */
/*                                                                            */
/* This library is distributed in the hope that it will be useful,            */
/* but WITHOUT ANY WARRANTY; without even the implied warranty of             */
/* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU          */
/* Lesser General Public License for more details.                            */
/*                                                                            */
/* You should have received a copy of the GNU Lesser General Public License   */
/* along with this library; if not, write to the Free Software Foundation,    */
/* Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA         */
/*                                                                            */

/******************************************************************************/
/*                                                                            */
/* ex_model_Ar_P_Morse_07C  pair potential KIM Model                          */
/* shifted to have zero energy at the cutoff radius                           */
/*                                                                            */
/* Language: C                                                                */
/*                                                                            */
/******************************************************************************/


#include "KIM_LogMacros.h"
#include "KIM_ModelHeaders.h"
#include <math.h>
#include <stdio.h>
#include <stdlib.h>

#define TRUE 1
#define FALSE 0

/******************************************************************************/
/* Below are the definitions and values of all Model parameters               */
/******************************************************************************/
#define DIM 3 /* dimensionality of space */
#define SPECCODE 1 /* internal species code */
#define CUTOFF 8.15 /* Angstroms */
#define EPSILON -0.0134783698072604 /* eV */
#define PARAM_C 1.545 /* 1/Angstroms */
#define RZERO 3.786 /* Angstroms */

/* Model buffer definition */
struct buffer
{
  double influenceDistance;
  double cutoff;
  int modelWillNotRequestNeighborsOfNoncontributingParticles;
};
typedef struct buffer buffer;

/* Define prototype for Model create */
int model_create(KIM_ModelCreate * const modelCreate,
                 KIM_LengthUnit const requestedLengthUnit,
                 KIM_EnergyUnit const requestedEnergyUnit,
                 KIM_ChargeUnit const requestedChargeUnit,
                 KIM_TemperatureUnit const requestedTemperatureUnit,
                 KIM_TimeUnit const requestedTimeUnit);

/* Define prototype for other routines */

static int compute_arguments_create(
    KIM_ModelCompute const * const modelCompute,
    KIM_ModelComputeArgumentsCreate * const modelComputeArgumentsCreate);
static int
model_compute(KIM_ModelCompute const * const modelCompute,
              KIM_ModelComputeArguments const * const modelComputeArguments);
static int compute_arguments_destroy(
    KIM_ModelCompute const * const modelCompute,
    KIM_ModelComputeArgumentsDestroy * const modelComputeArgumentsDestroy);
static int model_destroy(KIM_ModelDestroy * const modelDestroy);

/* Define prototypes for pair potential calculations */
static void calc_phi(double * epsilon,
                     double * C,
                     double * Rzero,
                     double * shift,
                     double * cutoff,
                     double r,
                     double * phi);

static void calc_phi_dphi(double * epsilon,
                          double * C,
                          double * Rzero,
                          double * shift,
                          double * cutoff,
                          double r,
                          double * phi,
                          double * dphi);

static void calc_phi_d2phi(double * epsilon,
                           double * C,
                           double * Rzero,
                           double * shift,
                           double * cutoff,
                           double r,
                           double * phi,
                           double * dphi,
                           double * d2phi);

/* Calculate pair potential phi(r) */
static void calc_phi(double * epsilon,
                     double * C,
                     double * Rzero,
                     double * shift,
                     double * cutoff,
                     double r,
                     double * phi)
{
  /* local variables */
  double ep;
  double ep2;

  ep = exp(-(*C) * (r - *Rzero));
  ep2 = ep * ep;

  if (r > *cutoff)
  {
    /* Argument exceeds cutoff radius */
    *phi = 0.0;
  }
  else
  {
    *phi = (*epsilon) * (-ep2 + 2.0 * ep) + *shift;
  }

  return;
}

/* Calculate pair potential phi(r) and its derivative dphi(r) */
static void calc_phi_dphi(double * epsilon,
                          double * C,
                          double * Rzero,
                          double * shift,
                          double * cutoff,
                          double r,
                          double * phi,
                          double * dphi)
{
  /* local variables */
  double ep;
  double ep2;

  ep = exp(-(*C) * (r - *Rzero));
  ep2 = ep * ep;

  if (r > *cutoff)
  {
    /* Argument exceeds cutoff radius */
    *phi = 0.0;
    *dphi = 0.0;
  }
  else
  {
    *phi = (*epsilon) * (-ep2 + 2.0 * ep) + *shift;
    *dphi = 2.0 * (*epsilon) * (*C) * (-ep + ep2);
  }

  return;
}

/* Calculate pair potential phi(r) and its 1st & 2nd derivatives dphi(r), */
/* d2phi(r) */
static void calc_phi_d2phi(double * epsilon,
                           double * C,
                           double * Rzero,
                           double * shift,
                           double * cutoff,
                           double r,
                           double * phi,
                           double * dphi,
                           double * d2phi)
{
  /* local variables */
  double ep;
  double ep2;

  ep = exp(-(*C) * (r - *Rzero));
  ep2 = ep * ep;

  if (r > *cutoff)
  {
    /* Argument exceeds cutoff radius */
    *phi = 0.0;
    *dphi = 0.0;
    *d2phi = 0.0;
  }
  else
  {
    *phi = (*epsilon) * (-ep2 + 2.0 * ep) + *shift;
    *dphi = 2.0 * (*epsilon) * (*C) * (-ep + ep2);
    *d2phi = 2.0 * (*epsilon) * (*C) * (*C) * (ep - 2.0 * ep2);
  }

  return;
}

/* compute function */
#undef KIM_LOGGER_FUNCTION_NAME
#define KIM_LOGGER_FUNCTION_NAME KIM_ModelCompute_LogEntry
#undef KIM_LOGGER_OBJECT_NAME
#define KIM_LOGGER_OBJECT_NAME modelCompute
/**/
static int
model_compute(KIM_ModelCompute const * const modelCompute,
              KIM_ModelComputeArguments const * const modelComputeArguments)
{
  /* local variables */
  double R;
  double R_pairs[2];
  double * pR_pairs = &(R_pairs[0]);
  double Rsqij;
  double phi;
  double dphi;
  double d2phi;
  double dEidr = 0.0;
  double d2Eidr = 0.0;
  double Rij[DIM];
  double * pRij = &(Rij[0]);
  double Rij_pairs[2][3];
  double const * pRij_pairs = &(Rij_pairs[0][0]);
  int ier;
  int i;
  int i_pairs[2];
  int * pi_pairs = &(i_pairs[0]);
  int j;
  int j_pairs[2];
  int * pj_pairs = &(j_pairs[0]);
  int jj;
  int k;
  int const * neighListOfCurrentPart;
  int comp_energy;
  int comp_force;
  int comp_particleEnergy;
  int comp_process_dEdr;
  int comp_process_d2Edr2;

  int * nParts;
  int * particleSpeciesCodes;
  int * particleContributing;
  buffer * bufferPointer;
  double * cutoff;
  double cutsq;
  double epsilon;
  double C;
  double Rzero;
  double shift;
  double * coords;
  double * energy;
  double * force;
  double * particleEnergy;
  int numOfPartNeigh;
  double dummy;

  /* check to see if we have been asked to compute the forces, */
  /* particleEnergy, and d1Edr */
  LOG_INFORMATION("Checking if call backs are present.");
  KIM_ModelComputeArguments_IsCallbackPresent(
      modelComputeArguments,
      KIM_COMPUTE_CALLBACK_NAME_ProcessDEDrTerm,
      &comp_process_dEdr);
  KIM_ModelComputeArguments_IsCallbackPresent(
      modelComputeArguments,
      KIM_COMPUTE_CALLBACK_NAME_ProcessD2EDr2Term,
      &comp_process_d2Edr2);

  LOG_INFORMATION("Getting data pointers");
  ier = KIM_ModelComputeArguments_GetArgumentPointerInteger(
            modelComputeArguments,
            KIM_COMPUTE_ARGUMENT_NAME_numberOfParticles,
            &nParts)
        || KIM_ModelComputeArguments_GetArgumentPointerInteger(
            modelComputeArguments,
            KIM_COMPUTE_ARGUMENT_NAME_particleSpeciesCodes,
            &particleSpeciesCodes)
        || KIM_ModelComputeArguments_GetArgumentPointerInteger(
            modelComputeArguments,
            KIM_COMPUTE_ARGUMENT_NAME_particleContributing,
            &particleContributing)
        || KIM_ModelComputeArguments_GetArgumentPointerDouble(
            modelComputeArguments,
            KIM_COMPUTE_ARGUMENT_NAME_coordinates,
            &coords)
        || KIM_ModelComputeArguments_GetArgumentPointerDouble(
            modelComputeArguments,
            KIM_COMPUTE_ARGUMENT_NAME_partialEnergy,
            &energy)
        || KIM_ModelComputeArguments_GetArgumentPointerDouble(
            modelComputeArguments,
            KIM_COMPUTE_ARGUMENT_NAME_partialForces,
            &force)
        || KIM_ModelComputeArguments_GetArgumentPointerDouble(
            modelComputeArguments,
            KIM_COMPUTE_ARGUMENT_NAME_partialParticleEnergy,
            &particleEnergy);
  if (ier)
  {
    LOG_ERROR("get data pointers failed");
    return ier;
  }

  comp_energy = (energy != NULL);
  comp_force = (force != NULL);
  comp_particleEnergy = (particleEnergy != NULL);

  /* set value of parameters */
  KIM_ModelCompute_GetModelBufferPointer(modelCompute,
                                         (void **) &bufferPointer);
  cutoff = &(bufferPointer->cutoff);
  cutsq = (*cutoff) * (*cutoff);
  epsilon = EPSILON;
  C = PARAM_C;
  Rzero = RZERO;
  /* set value of parameter shift */
  dummy = 0.0;
  /* call calc_phi with r=cutoff and shift=0.0 */
  calc_phi(&epsilon, &C, &Rzero, &dummy, cutoff, *cutoff, &shift);
  /* set shift to -shift */
  shift = -(shift);

  /* Check to be sure that the species are correct */
  /**/
  ier = TRUE; /* assume an error */
  for (i = 0; i < *nParts; ++i)
  {
    if (SPECCODE != particleSpeciesCodes[i])
    {
      LOG_ERROR("Unexpected species code detected");
      return ier;
    }
  }
  ier = FALSE; /* everything is ok */

  /* initialize potential energies, forces, and virial term */
  LOG_INFORMATION("Initializing data");
  if (comp_particleEnergy)
  {
    for (i = 0; i < *nParts; ++i) { particleEnergy[i] = 0.0; }
  }
  if (comp_energy) { *energy = 0.0; }

  if (comp_force)
  {
    for (i = 0; i < *nParts; ++i)
    {
      for (k = 0; k < DIM; ++k) { force[i * DIM + k] = 0.0; }
    }
  }

  /* Compute energy and forces */

  /* loop over particles and compute enregy and forces */
  LOG_INFORMATION("Starting main compute loop");
  for (i = 0; i < *nParts; ++i)
  {
    if (particleContributing[i])
    {
      ier = KIM_ModelComputeArguments_GetNeighborList(modelComputeArguments,
                                                      0,
                                                      i,
                                                      &numOfPartNeigh,
                                                      &neighListOfCurrentPart);
      if (ier)
      {
        /* some sort of problem, exit */
        LOG_ERROR("GetNeighborList failed");
        ier = TRUE;
        return ier;
      }

      /* loop over the neighbors of particle i */
      for (jj = 0; jj < numOfPartNeigh; ++jj)
      {
        j = neighListOfCurrentPart[jj]; /* get neighbor ID */

        if (!(particleContributing[j] && (j < i)))
        {
          /* short-circuit half-list */

          /* compute relative position vector and squared distance */
          Rsqij = 0.0;
          for (k = 0; k < DIM; ++k)
          {
            Rij[k] = coords[j * DIM + k] - coords[i * DIM + k];

            /* compute squared distance */
            Rsqij += Rij[k] * Rij[k];
          }

          /* compute energy and force */
          if (Rsqij < cutsq)
          {
            /* particles are interacting ? */
            R = sqrt(Rsqij);
            if (comp_process_d2Edr2)
            {
              /* compute pair potential and its derivatives */
              calc_phi_d2phi(
                  &epsilon, &C, &Rzero, &shift, cutoff, R, &phi, &dphi, &d2phi);

              /* compute dEidr */
              if (particleContributing[j])
              {
                dEidr = dphi;
                d2Eidr = d2phi;
              }
              else
              {
                dEidr = 0.5 * dphi;
                d2Eidr = 0.5 * d2phi;
              }
            }
            else if (comp_force || comp_process_dEdr)
            {
              /* compute pair potential and its derivative */
              calc_phi_dphi(
                  &epsilon, &C, &Rzero, &shift, cutoff, R, &phi, &dphi);

              /* compute dEidr */
              if (particleContributing[j]) { dEidr = dphi; }
              else
              {
                dEidr = 0.5 * dphi;
              }
            }
            else
            {
              /* compute just pair potential */
              calc_phi(&epsilon, &C, &Rzero, &shift, cutoff, R, &phi);
            }

            /* contribution to energy */
            if (comp_particleEnergy)
            {
              particleEnergy[i] += 0.5 * phi;
              if (particleContributing[j]) { particleEnergy[j] += 0.5 * phi; }
            }
            if (comp_energy)
            {
              if (particleContributing[j]) { *energy += phi; }
              else
              {
                *energy += 0.5 * phi;
              }
            }

            /* contribution to process_dEdr */
            if (comp_process_dEdr)
            {
              ier = KIM_ModelComputeArguments_ProcessDEDrTerm(
                  modelComputeArguments, dEidr, R, pRij, i, j);
              if (ier)
              {
                LOG_ERROR("ProcessDEDrTerm callback error");
                ier = TRUE;
                return ier;
              }
            }

            /* contribution to process_d2Edr2 */
            if (comp_process_d2Edr2)
            {
              R_pairs[0] = R_pairs[1] = R;
              Rij_pairs[0][0] = Rij_pairs[1][0] = Rij[0];
              Rij_pairs[0][1] = Rij_pairs[1][1] = Rij[1];
              Rij_pairs[0][2] = Rij_pairs[1][2] = Rij[2];
              i_pairs[0] = i_pairs[1] = i;
              j_pairs[0] = j_pairs[1] = j;

              ier = KIM_ModelComputeArguments_ProcessD2EDr2Term(
                  modelComputeArguments,
                  d2Eidr,
                  pR_pairs,
                  pRij_pairs,
                  pi_pairs,
                  pj_pairs);
              if (ier)
              {
                LOG_ERROR("ProcessDEDrTerm callback error");
                ier = TRUE;
                return ier;
              }
            }

            /* contribution to forces */
            if (comp_force)
            {
              for (k = 0; k < DIM; ++k)
              {
                force[i * DIM + k]
                    += dEidr * Rij[k] / R; /* accumulate force on i */
                force[j * DIM + k]
                    -= dEidr * Rij[k] / R; /* accumulate force on j */
              }
            }
          }
        } /* if (i < j) */
      } /* loop on jj */
    } /* if contributing */
  } /* loop on i */
  LOG_INFORMATION("Finished compute loop");

  /* everything is great */
  ier = FALSE;

  return ier;
}

/* Create function */
#undef KIM_LOGGER_FUNCTION_NAME
#define KIM_LOGGER_FUNCTION_NAME KIM_ModelCreate_LogEntry
#undef KIM_LOGGER_OBJECT_NAME
#define KIM_LOGGER_OBJECT_NAME modelCreate
/**/
int model_create(KIM_ModelCreate * const modelCreate,
                 KIM_LengthUnit const requestedLengthUnit,
                 KIM_EnergyUnit const requestedEnergyUnit,
                 KIM_ChargeUnit const requestedChargeUnit,
                 KIM_TemperatureUnit const requestedTemperatureUnit,
                 KIM_TimeUnit const requestedTimeUnit)
{
  buffer * bufferPointer;
  int error;

  /* use function pointer definitions to verify prototypes */
  KIM_ModelCreateFunction * create = model_create;
  KIM_ModelComputeArgumentsCreateFunction * CACreate = compute_arguments_create;
  KIM_ModelComputeFunction * compute = model_compute;
  KIM_ModelComputeArgumentsDestroyFunction * CADestroy
      = compute_arguments_destroy;
  KIM_ModelDestroyFunction * destroy = model_destroy;

  (void) create; /* avoid unused parameter warnings */
  (void) requestedLengthUnit;
  (void) requestedEnergyUnit;
  (void) requestedChargeUnit;
  (void) requestedTemperatureUnit;
  (void) requestedTimeUnit;

  /* set units */
  LOG_INFORMATION("Set model units");
  error = KIM_ModelCreate_SetUnits(modelCreate, /* ignoring requested units */
                                   KIM_LENGTH_UNIT_A,
                                   KIM_ENERGY_UNIT_eV,
                                   KIM_CHARGE_UNIT_unused,
                                   KIM_TEMPERATURE_UNIT_unused,
                                   KIM_TIME_UNIT_unused);

  /* register species */
  LOG_INFORMATION("Setting species code");
  error = error
          || KIM_ModelCreate_SetSpeciesCode(
              modelCreate, KIM_SPECIES_NAME_Ar, SPECCODE);

  /* register numbering */
  LOG_INFORMATION("Setting model numbering");
  error = error
          || KIM_ModelCreate_SetModelNumbering(modelCreate,
                                               KIM_NUMBERING_zeroBased);

  /* register function pointers */
  LOG_INFORMATION("Register model function pointers");
  error = error
          || KIM_ModelCreate_SetRoutinePointer(
              modelCreate,
              KIM_MODEL_ROUTINE_NAME_ComputeArgumentsCreate,
              KIM_LANGUAGE_NAME_c,
              TRUE,
              (KIM_Function *) CACreate)
          || KIM_ModelCreate_SetRoutinePointer(modelCreate,
                                               KIM_MODEL_ROUTINE_NAME_Compute,
                                               KIM_LANGUAGE_NAME_c,
                                               TRUE,
                                               (KIM_Function *) compute)
          || KIM_ModelCreate_SetRoutinePointer(
              modelCreate,
              KIM_MODEL_ROUTINE_NAME_ComputeArgumentsDestroy,
              KIM_LANGUAGE_NAME_c,
              TRUE,
              (KIM_Function *) CADestroy)
          || KIM_ModelCreate_SetRoutinePointer(modelCreate,
                                               KIM_MODEL_ROUTINE_NAME_Destroy,
                                               KIM_LANGUAGE_NAME_c,
                                               TRUE,
                                               (KIM_Function *) destroy);

  /* allocate buffer */
  bufferPointer = (buffer *) malloc(sizeof(buffer));

  /* store model buffer in KIM object */
  LOG_INFORMATION("Set influence distance and cutoffs");
  KIM_ModelCreate_SetModelBufferPointer(modelCreate, bufferPointer);

  /* set buffer values */
  bufferPointer->influenceDistance = CUTOFF;
  bufferPointer->cutoff = CUTOFF;
  bufferPointer->modelWillNotRequestNeighborsOfNoncontributingParticles = 1;

  /* register influence distance */
  KIM_ModelCreate_SetInfluenceDistancePointer(
      modelCreate, &(bufferPointer->influenceDistance));

  /* register cutoff */
  KIM_ModelCreate_SetNeighborListPointers(
      modelCreate,
      1,
      &(bufferPointer->cutoff),
      &(bufferPointer->modelWillNotRequestNeighborsOfNoncontributingParticles));

  if (error)
  {
    free(bufferPointer);
    LOG_ERROR("Unable to successfully initialize model");
    return TRUE;
  }
  else
    return FALSE;
}

/* Destroy function */
#undef KIM_LOGGER_FUNCTION_NAME
#define KIM_LOGGER_FUNCTION_NAME KIM_ModelDestroy_LogEntry
#undef KIM_LOGGER_OBJECT_NAME
#define KIM_LOGGER_OBJECT_NAME modelDestroy
/**/
int model_destroy(KIM_ModelDestroy * const modelDestroy)
{
  buffer * bufferPointer;

  LOG_INFORMATION("Getting buffer");
  KIM_ModelDestroy_GetModelBufferPointer(modelDestroy,
                                         (void **) &bufferPointer);
  LOG_INFORMATION("Freeing model memory");
  free(bufferPointer);

  return FALSE;
}

/* compute arguments create routine */
#undef KIM_LOGGER_FUNCTION_NAME
#define KIM_LOGGER_FUNCTION_NAME KIM_ModelCompute_LogEntry
#undef KIM_LOGGER_OBJECT_NAME
#define KIM_LOGGER_OBJECT_NAME modelCompute
/**/
static int compute_arguments_create(
    KIM_ModelCompute const * const modelCompute,
    KIM_ModelComputeArgumentsCreate * const modelComputeArgumentsCreate)
{
  int error;

  (void) modelCompute; /* avoid unused parameter warning */

  /* register arguments */
  LOG_INFORMATION("Register argument supportStatus");
  error = KIM_ModelComputeArgumentsCreate_SetArgumentSupportStatus(
      modelComputeArgumentsCreate,
      KIM_COMPUTE_ARGUMENT_NAME_partialEnergy,
      KIM_SUPPORT_STATUS_optional);
  error = error
          || KIM_ModelComputeArgumentsCreate_SetArgumentSupportStatus(
              modelComputeArgumentsCreate,
              KIM_COMPUTE_ARGUMENT_NAME_partialForces,
              KIM_SUPPORT_STATUS_optional);
  error = error
          || KIM_ModelComputeArgumentsCreate_SetArgumentSupportStatus(
              modelComputeArgumentsCreate,
              KIM_COMPUTE_ARGUMENT_NAME_partialParticleEnergy,
              KIM_SUPPORT_STATUS_optional);

  /* register call backs */
  LOG_INFORMATION("Register call back supportStatus");
  error = error
          || KIM_ModelComputeArgumentsCreate_SetCallbackSupportStatus(
              modelComputeArgumentsCreate,
              KIM_COMPUTE_CALLBACK_NAME_ProcessDEDrTerm,
              KIM_SUPPORT_STATUS_optional);
  error = error
          || KIM_ModelComputeArgumentsCreate_SetCallbackSupportStatus(
              modelComputeArgumentsCreate,
              KIM_COMPUTE_CALLBACK_NAME_ProcessD2EDr2Term,
              KIM_SUPPORT_STATUS_optional);

  if (error)
  {
    LOG_ERROR("Unable to successfully initialize compute arguments");
    return TRUE;
  }
  else
    return FALSE;
}

/* compute arguments destroy routine */
static int compute_arguments_destroy(
    KIM_ModelCompute const * const modelCompute,
    KIM_ModelComputeArgumentsDestroy * const modelComputeArgumentsDestroy)
{
  (void) modelCompute; /* avoid unused parameter warning */
  (void) modelComputeArgumentsDestroy; /* avoid unused parameter warning */

  /* Nothing further to do */

  return FALSE;
}
