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

#include "resources.h"

ErlNifResourceType *connection_res;

static ErlNifResourceType *open_resource(ErlNifEnv *env, const char *name,
                                         ErlNifResourceDtor *dtor) {
  ErlNifResourceType *res;
  res = enif_open_resource_type(env, NULL, name, dtor,
                                ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER, NULL);

  if (!res) {
    enif_fprintf(stderr, "cannot open resource '%s'", name);
    return NULL;
  }

  return res;
}

void connection_dtor(ErlNifEnv *env, void *obj) {
  connection_t *conn_res = (connection_t *)obj;
  //   TODO Release resources here
  //      if(f->is_async)
  //      {
  //           /* Wait for the operation to complete */
  //           rados_aio_wait_for_complete(f->comp);
  //           /* Release the asynchronous I/O complete handle to avoid memory
  //           leaks. */ rados_aio_release(f->comp);
  //      }
  //      rados_ioctx_destroy(f->io);

  /* conn_res->cluster can be shutdown at this point by other process */
  if (conn_res->cluster) {
    rados_shutdown(*(conn_res->cluster));
    free(conn_res->cluster);
    conn_res->cluster = NULL;
  }
}

int open_resources(ErlNifEnv *env) {
  connection_res = open_resource(env, "rados_connection", connection_dtor);
  if (!connection_res) {
    return -1;
  }
  return 0;
}

void close_resources(ErlNifEnv *env) {
  // TODO Do we need it?
}
