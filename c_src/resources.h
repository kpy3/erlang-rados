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

#ifndef _RADOS_NIF_RESOURCES_H_
#define _RADOS_NIF_RESOURCES_H_

#include "erl_nif.h"
#include "rados/librados.h"
#include "stdbool.h"

extern ErlNifResourceType *connection_res;

typedef struct {
  bool is_async;
  rados_t *cluster;
  //    rados_ioctx_t io;
  //    rados_completion_t comp;
} connection_t;

int open_resources(ErlNifEnv *env);
void close_resources(ErlNifEnv *env);

void connection_dtor(ErlNifEnv *env, void *obj);

#endif
