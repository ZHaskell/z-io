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

#include <hs_uv.h>
#include <stdlib.h>

void hs_fs_event_cb(uv_fs_event_t* handle, const char* filename, int events, int status){
    if (status < 0) return;
    HsInt slot = (HsInt)handle->data;
    hs_loop_data* loop_data = handle->loop->data;

    size_t l = strlen(filename) + 2; // including the \NUL and event byte
    // we simply ignore more events if buffer can't hold it
    // libuv use a buffer size 4096, so on haskell side anything > 4096 should work
    if (loop_data->buffer_size_table[slot] >= l){
        loop_data->buffer_size_table[slot] -= l;
        char* buf = loop_data->buffer_table[slot] + loop_data->buffer_size_table[slot];
        *buf = (uint8_t)events;
        memcpy(buf+1, filename, l-1);
    }
}

int hs_uv_fs_event_start(uv_fs_event_t* handle, const char* path, unsigned int flags){
    return uv_fs_event_start(handle, hs_fs_event_cb, path, flags);
}

// Check if the fs event buffer is filled with messages, if so, unlock the fs event thread
//
void hs_fs_event_check_cb(uv_check_t* check){
    uv_fs_event_t* f =(uv_fs_event_t*)check->data;
    HsInt slot = (HsInt)f->data;
    hs_loop_data* loop_data = f->loop->data;
    size_t buffer_index = loop_data->buffer_size_table[slot];
    // This relys on GHC ByteArray# memory layout, ByteArray# length is recorded before content.
    HsInt* buffer_ptr = (HsInt*)loop_data->buffer_table[slot];
    HsInt buffer_total_len = *(buffer_ptr-1);
    if (buffer_index < buffer_total_len ) {
        loop_data->event_queue[loop_data->event_counter] = slot; // push the slot to event queue
        loop_data->event_counter += 1;
    }
}

int hs_uv_fs_event_check_start(uv_check_t* check){
    return uv_check_start(check, hs_fs_event_check_cb);
}
