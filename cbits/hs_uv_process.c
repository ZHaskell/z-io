/*
 * Copyright (c) 2017-2019 Dong Han
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

#include <hs_uv.h>
#include <stdlib.h>

////////////////////////////////////////////////////////////////////////////////
//
// process


void hs_uv_exit_cb(uv_process_t* handle, int64_t exit_status, int term_signal){
    HsInt slot = (HsInt)handle->data;
    uv_loop_t* loop = handle->loop;
    hs_loop_data* loop_data = loop->data;
    loop_data->buffer_size_table[slot] = (HsInt)exit_status;     
    loop_data->event_queue[loop_data->event_counter] = slot;   // push the slot to event queue
    loop_data->event_counter += 1;
    free_slot(loop_data, slot);  // free the uv_process_t
}

HsInt hs_uv_spawn(uv_loop_t* loop
                 , uv_process_options_t* options
                 , const char* file
                 , const char* all_args
                 , const size_t args_len
                 , const char* all_env
                 , const ssize_t env_len
                 , const char* cwd
                 , uv_stdio_container_t* stdio){
    int r, i;
    char* p;
    char **args = NULL, **env = NULL;

    hs_loop_data* loop_data = loop->data;
    HsInt slot = alloc_slot(loop_data);
    if (slot < 0) return UV_ENOMEM;
    uv_process_t* handle = 
        (uv_process_t*)fetch_uv_struct(loop_data, slot);
    handle->data = (void*)slot;

    options->exit_cb = hs_uv_exit_cb;
    options->file = file;

    i = 0;
    args = (char**)malloc(sizeof(char*)*(args_len + 2));
    if (args == NULL) return UV_ENOMEM;
    args[i++] = (char*)file;
    if (args_len > 0) args[i++] = (char*)all_args;
    for(p = (char*)all_args; i <= args_len; p++){
        if (*p == 0) {
            args[i++] = p+1;
        }
    }
    args[i] = NULL;
    options->args = args;

    i = 0;
    if (env_len >= 0){
        env = (char**)malloc(sizeof(char*)*(env_len+1));
        if (env == NULL) return UV_ENOMEM;
        if (env_len > 0) env[i++] = (char*)all_env;
        for(p = (char*)all_env; i < env_len; p++){
            if (*p == 0) {
                env[i++] = p+1;
            }
        }
        env[i] = NULL;
    }
    options->env = env;

    options->stdio_count = 3;
    options->stdio = stdio;
    options->cwd = cwd;

    r = uv_spawn(loop, handle, options);

    free(args);
    if (env != NULL) free(env);

    if (r < 0) {
        free_slot(loop_data, slot);  // free the uv_process_t, the callback won't fired
        return (HsInt)r;
    } else { 
        loop_data->buffer_size_table[slot] = (HsInt)handle->pid;     
        return slot;
    }
}
