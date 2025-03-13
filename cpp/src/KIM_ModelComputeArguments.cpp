//
// KIM-API: An API for interatomic models
// Copyright (c) 2013--2022, Regents of the University of Minnesota.
// All rights reserved.
//
// Contributors:
//    Ryan S. Elliott
//
// SPDX-License-Identifier: LGPL-2.1-or-later
//
// This library is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation; either
// version 2.1 of the License, or (at your option) any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public License
// along with this library; if not, write to the Free Software Foundation,
// Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA
//

//
// Release: This file is part of the kim-api.git repository.
//


#include <cstddef>

#ifndef KIM_LOG_VERBOSITY_HPP_
#include "KIM_LogVerbosity.hpp"
#endif

#ifndef KIM_COMPUTE_ARGUMENT_NAME_HPP_
#include "KIM_ComputeArgumentName.hpp"
#endif

#ifndef KIM_COMPUTE_CALLBACK_NAME_HPP_
#include "KIM_ComputeCallbackName.hpp"
#endif

#ifndef KIM_MODEL_COMPUTE_ARGUMENTS_HPP_
#include "KIM_ModelComputeArguments.hpp"
#endif

#ifndef KIM_COMPUTE_ARGUMENTS_IMPLEMENTATION_HPP_
#include "KIM_ComputeArgumentsImplementation.hpp"
#endif

#define CONVERT_POINTER                  \
  ComputeArgumentsImplementation * pImpl \
      = reinterpret_cast<ComputeArgumentsImplementation *>(pimpl)

namespace KIM
{
int ModelComputeArguments::GetNeighborList(
    int const neighborListIndex,
    int const particleNumber,
    int * const numberOfNeighbors,
    int const ** const neighborsOfParticle) const
{
  CONVERT_POINTER;

  return pImpl->GetNeighborList(neighborListIndex,
                                particleNumber,
                                numberOfNeighbors,
                                neighborsOfParticle);
}

int ModelComputeArguments::ProcessDEDrTerm(double const de,
                                           double const r,
                                           double const * const dx,
                                           int const i,
                                           int const j) const
{
  CONVERT_POINTER;

  return pImpl->ProcessDEDrTerm(de, r, dx, i, j);
}

int ModelComputeArguments::ProcessD2EDr2Term(double const de,
                                             double const * const r,
                                             double const * const dx,
                                             int const * const i,
                                             int const * const j) const
{
  CONVERT_POINTER;

  return pImpl->ProcessD2EDr2Term(de, r, dx, i, j);
}

int ModelComputeArguments::GetArgumentPointer(
    ComputeArgumentName const computeArgumentName, int const ** const ptr) const
{
  CONVERT_POINTER;

  return pImpl->GetArgumentPointer(computeArgumentName, ptr);
}

int ModelComputeArguments::GetArgumentPointer(
    ComputeArgumentName const computeArgumentName, int ** const ptr) const
{
  CONVERT_POINTER;

  return pImpl->GetArgumentPointer(computeArgumentName, ptr);
}

int ModelComputeArguments::GetArgumentPointer(
    ComputeArgumentName const computeArgumentName, double ** const ptr) const
{
  CONVERT_POINTER;

  return pImpl->GetArgumentPointer(computeArgumentName, ptr);
}

int ModelComputeArguments::GetArgumentPointer(
    ComputeArgumentName const computeArgumentName,
    double const ** const ptr) const
{
  CONVERT_POINTER;

  return pImpl->GetArgumentPointer(computeArgumentName, ptr);
}

int ModelComputeArguments::IsCallbackPresent(
    ComputeCallbackName const computeCallbackName, int * const present) const
{
  CONVERT_POINTER;

  return pImpl->IsCallbackPresent(computeCallbackName, present);
}

void ModelComputeArguments::SetModelBufferPointer(void * const ptr)
{
  CONVERT_POINTER;

  pImpl->SetModelBufferPointer(ptr);
}

void ModelComputeArguments::GetModelBufferPointer(void ** const ptr) const
{
  CONVERT_POINTER;

  pImpl->GetModelBufferPointer(ptr);
}

void ModelComputeArguments::LogEntry(LogVerbosity const logVerbosity,
                                     std::string const & message,
                                     int const lineNumber,
                                     std::string const & fileName) const
{
  CONVERT_POINTER;

  pImpl->LogEntry(logVerbosity, message, lineNumber, fileName);
}

void ModelComputeArguments::LogEntry(LogVerbosity const logVerbosity,
                                     std::stringstream const & message,
                                     int const lineNumber,
                                     std::string const & fileName) const
{
  CONVERT_POINTER;

  pImpl->LogEntry(logVerbosity, message, lineNumber, fileName);
}

std::string const & ModelComputeArguments::ToString() const
{
  CONVERT_POINTER;

  return pImpl->ToString();
}

ModelComputeArguments::ModelComputeArguments() : pimpl(NULL) {}

ModelComputeArguments::~ModelComputeArguments() {}

}  // namespace KIM
