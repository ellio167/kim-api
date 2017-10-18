/*                                                                            */
/* CDDL HEADER START                                                          */
/*                                                                            */
/* The contents of this file are subject to the terms of the Common           */
/* Development and Distribution License Version 1.0 (the "License").          */
/*                                                                            */
/* You can obtain a copy of the license at                                    */
/* http://www.opensource.org/licenses/CDDL-1.0.  See the License for the      */
/* specific language governing permissions and limitations under the License. */
/*                                                                            */
/* When distributing Covered Code, include this CDDL HEADER in each file and  */
/* include the License file in a prominent location with the name             */
/* LICENSE.CDDL.                                                              */
/* If applicable, add the following below this CDDL HEADER, with the fields   */
/* enclosed by brackets "[]" replaced with your own identifying information:  */
/*                                                                            */
/* Portions Copyright (c) [yyyy] [name of copyright owner].                   */
/* All rights reserved.                                                       */
/*                                                                            */
/* CDDL HEADER END                                                            */
/*                                                                            */

/*                                                                            */
/* Copyright (c) 2016--2017, Regents of the University of Minnesota.          */
/* All rights reserved.                                                       */
/*                                                                            */
/* Contributors:                                                              */
/*    Ryan S. Elliott                                                         */
/*                                                                            */

/*                                                                            */
/* Release: This file is part of the kim-api.git repository.                  */
/*                                                                            */


#ifndef KIM_TIME_UNIT_H_
#define KIM_TIME_UNIT_H_

struct KIM_TimeUnit
{
  int timeUnitID;
};
#ifndef KIM_TIME_UNIT_DEFINED_
#define KIM_TIME_UNIT_DEFINED_
typedef struct KIM_TimeUnit KIM_TimeUnit;
#endif

KIM_TimeUnit KIM_TimeUnitFromString(char const * const str);

int KIM_TimeUnitEqual(KIM_TimeUnit const left, KIM_TimeUnit right);
int KIM_TimeUnitNotEqual(KIM_TimeUnit const left, KIM_TimeUnit right);
char const * const KIM_TimeUnitString(KIM_TimeUnit const timeUnit);

extern KIM_TimeUnit const KIM_TIME_UNIT_any;
extern KIM_TimeUnit const KIM_TIME_UNIT_fs;
extern KIM_TimeUnit const KIM_TIME_UNIT_ps;
extern KIM_TimeUnit const KIM_TIME_UNIT_ns;
extern KIM_TimeUnit const KIM_TIME_UNIT_s;

#endif  /* KIM_TIME_UNIT_H_ */