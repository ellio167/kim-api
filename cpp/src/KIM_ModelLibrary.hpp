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
// Copyright (c) 2016--2018, Regents of the University of Minnesota.
// All rights reserved.
//
// Contributors:
//    Ryan S. Elliott
//

//
// Release: This file is part of the kim-api.git repository.
//


#ifndef KIM_MODEL_LIBRARY_HPP_
#define KIM_MODEL_LIBRARY_HPP_

#include <string>

#ifndef KIM_FUNC_HPP_
#include "KIM_func.hpp"
#endif

#ifndef KIM_LOG_HPP_
#include "KIM_Log.hpp"
#endif

namespace KIM
{
// Forward declarations
class LanguageName;


class ModelLibrary
{
 public:
  ModelLibrary(Log * const log);
  ~ModelLibrary();

  enum ITEM_TYPE {STAND_ALONE_MODEL, PARAMETERIZED_MODEL, SIMULATOR_MODEL,
                  MODEL_DRIVER};

  int Open(bool const typeIsModel, std::string const & modelName);
  int Close();
  int GetModelType(ITEM_TYPE * const modelType) const;
  int GetModelCreateFunctionPointer(LanguageName * const languageName,
                                    func ** const functionPointer)
      const;
  int GetNumberOfParameterFiles(int * const numberOfParameterFiles) const;
  int GetParameterFileString(
      int const index,
      unsigned int * const parameterFileStringLength,
      unsigned char const ** const parameterFileString) const;
  int GetModelDriverName(std::string * const modelDriverName) const;
  int GetModelCompiledWithVersion(std::string * const versionString) const;

  void LogEntry(LogVerbosity const logVerbosity, std::string const & message,
                int const lineNumber, std::string const & fileName) const;

 private:
  std::string modelName_;
  std::string libraryPath_;
  void * libraryHandle_;
  Log * log_;
};
}  // namespace KIM
#endif  // KIM_MODEL_LIBRARY_HPP_