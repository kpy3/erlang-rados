// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

#ifndef _RADOS_NIF_H_
#define _RADOS_NIF_H_

#include "erl_nif.h"
#include "rados/librados.h"

extern ERL_NIF_TERM atom_ok;
extern ERL_NIF_TERM atom_error;

extern ErlNifResourceType *rados_connection;

typedef struct {
  ErlNifMutex *mutex; /* used to protect the entire structure from concurrent access */
  rados_t cluster;    /* RADOS handler */
} connection_t;

ERL_NIF_TERM erlang_rados_connect(ErlNifEnv *env, int argc,
                                  const ERL_NIF_TERM argv[]);

ERL_NIF_TERM erlang_rados_shutdown(ErlNifEnv *env, int argc,
                                 const ERL_NIF_TERM argv[]);

ERL_NIF_TERM erlang_rados_version(ErlNifEnv *env, int argc,
                                  const ERL_NIF_TERM argv[]);

#endif
