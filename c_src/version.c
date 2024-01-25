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

#include "rados_nif.h"

ERL_NIF_TERM
erlang_rados_version(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
  int major, minor, extra;

  rados_version(&major, &minor, &extra);

  ERL_NIF_TERM term =
      enif_make_tuple3(env, enif_make_int(env, major),
                       enif_make_int(env, minor), enif_make_int(env, extra));

  return term;
}
