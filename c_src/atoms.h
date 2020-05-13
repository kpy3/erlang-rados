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

#ifndef _RADOS_NIF_ATOMS_H_
#define _RADOS_NIF_ATOMS_H_

#include "erl_nif.h"

extern ERL_NIF_TERM atom_true;
extern ERL_NIF_TERM atom_false;
extern ERL_NIF_TERM atom_ok;
extern ERL_NIF_TERM atom_error;

void init_atoms(ErlNifEnv *env);
ERL_NIF_TERM mk_atom(ErlNifEnv *env, const char *atom);
ERL_NIF_TERM mk_error(ErlNifEnv *env, const char *mesg);

#endif
