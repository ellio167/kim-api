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
// Copyright (c) 2016--2020, Regents of the University of Minnesota.
// All rights reserved.
//
// Contributors:
//    Ryan S. Elliott
//

//
// Release: This file is part of the kim-api-2.2.0 package.
//


#ifndef KIM_COLLECTION_HPP_
#define KIM_COLLECTION_HPP_

#include <string>

namespace KIM
{
/// \brief An \ref extensible_enumeration "Extensible Enumeration" for the
/// Collection's supported by the %KIM API.
///
/// The enumeration constants are contained in the COLLECTION namespace.
///
/// \sa KIM_Collection, kim_collection_module::kim_collection_type
///
/// \since 2.1
class Collection
{
 public:
  /// \brief Integer identifying the specific Collection represented.
  ///
  /// \note This should not be directly accessed and is only public for
  /// cross-language reasons.
  ///
  /// \sa KIM_Collection::collectionID,
  /// kim_collection_module::kim_collection_type::collection_id
  ///
  /// \since 2.1
  int collectionID;

  /// \brief Create an uninitialized Collection object.
  ///
  /// \since 2.1
  Collection();

  /// \brief Create a Collection object with the specified id.
  ///
  /// \note This should not be used directly.
  ///
  /// \since 2.1
  Collection(int const id);

  /// \brief Create a Collection object corresponding to the provided
  /// string. If the string does not match one of the values defined by the
  /// %KIM API, then an "unknown" object is generated.
  ///
  /// \sa KIM_Collection_FromString, kim_collection_module::kim_from_string
  ///
  /// \since 2.1
  Collection(std::string const & str);

  /// \brief Determines if the object is a quantity known to the %KIM API.
  ///
  /// Collection's known to the %KIM API are found in the COLLECTION namespace.
  ///
  /// \sa KIM_Collection_Known, kim_collection_module::kim_known
  ///
  /// \since 2.1
  bool Known() const;

  /// \brief Compares Collection objects for equality.
  ///
  /// \note Not all "unknown" objects are equal.
  ///
  /// \sa KIM_Collection_Equal, kim_collection_module::operator(.eq.)
  ///
  /// \since 2.1
  bool operator==(Collection const & rhs) const;

  /// \brief Compares Collection objects for inequality.
  ///
  /// \note It is possible for two "unknown" objects to be not equal.
  ///
  /// \sa KIM_Collection_NotEqual,
  /// kim_collection_module::operator(.ne.)
  ///
  /// \since 2.1
  bool operator!=(Collection const & rhs) const;

  /// \brief Converts the object to a string.
  ///
  /// \return A string object representing the Collection object.
  ///
  /// \note If the Collection object does not correspond to a value
  /// defined by the %KIM API, then the string "unknown" is returned.
  ///
  /// \sa KIM_Collection_ToString, kim_collection_module::kim_to_string
  ///
  /// \since 2.1
  std::string const & ToString() const;
};  // class Collection

/// \brief Contains the enumeration constants and the discovery routines for
/// the Collection \ref extensible_enumeration "Extensible Enumeration".
namespace COLLECTION
{
/// \brief The standard \c system Collection.
///
/// The system wide collection.
///
/// \sa KIM_COLLECTION_system, kim_collection_module::kim_collection_system
///
/// \since 2.1
extern Collection const system;

/// \brief The standard \c user Collection.
///
/// The user collection.
///
/// \sa KIM_COLLECTION_user,
/// kim_collection_module::kim_collection_user
///
/// \since 2.1
extern Collection const user;

/// \brief The standard \c environmentVariable Collection.
///
/// The environment variable collection.
///
/// \sa KIM_COLLECTION_environmentVariable,
/// kim_collection_module::kim_collection_environment_variable
///
/// \since 2.1
extern Collection const environmentVariable;

/// \brief The standard \c currentWorkingDirectory Collection.
///
/// The current working directory collection.
///
/// \sa KIM_COLLECTION_currentWorkingDirectory,
/// kim_collection_module::kim_collection_currentWorkingDirectory
///
/// \since 2.1
extern Collection const currentWorkingDirectory;


/// \brief Get the number of standard Collection's defined by the %KIM API.
///
/// \param[out] numberOfCollections The number of standard Collection's defined
///             by the %KIM API.
///
/// \sa KIM_COLLECTION_GetNumberOfCollections,
/// kim_collection_module::kim_get_number_of_collections
///
/// \since 2.1
void GetNumberOfCollections(int * const numberOfCollections);

/// \brief Get the identity of each defined standard Collection.
///
/// \param[in]  index Zero-based index uniquely labeling each defined standard
///             Collection.  This index ordering is only guaranteed to be
///             stable during the lifetime of the current process.
/// \param[out] collection The Collection object associated with \c index.
///
/// \return \c true if `index < 0` or `index >= numberOfCollections`.
/// \return \c false otherwise.
///
/// \sa KIM_COLLECTION_GetCollection, kim_collection_module::kim_get_collection
///
/// \since 2.1
int GetCollection(int const index, Collection * const collection);

/// \brief Structure provided for use with std::map.
///
/// \since 2.1
struct Comparator
{
  /// \brief Provides an (logically unmeaningful) ordering for
  /// Collection objects so that they can be stored in a std::map.
  ///
  /// \since 2.1
  bool operator()(Collection const & a, Collection const & b) const
  {
    return a.collectionID < b.collectionID;
  }
};  // struct Comparator
}  // namespace COLLECTION
}  // namespace KIM

#endif  // KIM_COLLECTION_HPP_
