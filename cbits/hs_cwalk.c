/*
 * Copyright (c) 2020 Dong Han
 *
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. Neither the names of the authors or the names of any contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

#include <hs_cwalk.h>
#include <stdlib.h>

HsInt hs_cwk_path_get_basename(const char *path, HsInt *base_off){
    size_t length;
    const char *basename = NULL;
    cwk_path_get_basename(path, &basename, &length);
    if (basename == NULL) {
        *base_off = 0;
        return 0;
    } else {
        *base_off = (HsInt)(basename - path);
        return (HsInt)length;
    }
}

HsInt hs_cwk_path_get_dirname(const char *path){
    size_t length;
    cwk_path_get_dirname(path, &length);
    return (HsInt)length;
}

HsInt hs_cwk_path_get_root(const char *path){
    size_t length;
    cwk_path_get_root(path, &length);
    return (HsInt)length;
}

#if __GLASGOW_HASKELL__ < 810
HsInt hs_cwk_path_join_multiple(const StgMutArrPtrs *paths_arr, HsInt path_n, char *buffer, size_t buffer_size){
    StgArrBytes **paths = (StgArrBytes**)paths_arr->payload;
#else
HsInt hs_cwk_path_join_multiple(const StgArrBytes **paths, HsInt path_n, char *buffer, size_t buffer_size){
#endif
    HsInt r;
    const char **path_list = (const char**)malloc(sizeof(char*)*(path_n+1));
    if (path_list == NULL) {
        *buffer = 0;
        return 0;
    }
    path_list[path_n--] = NULL;
    while(path_n >= 0){
        path_list[path_n] = (char*)paths[path_n]->payload;
        path_n--;
    } 
    r = (HsInt)cwk_path_join_multiple(path_list, buffer, buffer_size);
    free(path_list);
    return r;
}

HsInt hs_cwk_path_get_extension(const char *path, size_t *length){
    const char *extension;
    if (cwk_path_get_extension(path, &extension, length)){
        return (extension - path);
    } else {
        return -1;
    }
}
