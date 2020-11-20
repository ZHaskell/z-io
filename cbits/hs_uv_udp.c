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

////////////////////////////////////////////////////////////////////////////////
//
// udp

// We do batch read per uv_run, the buffer index keep decreasing until hit zero
void hs_udp_alloc_cb(uv_handle_t* handle, size_t suggested_size, uv_buf_t* buf){
    HsInt slot = (HsInt)handle->data;
    hs_loop_data* loop_data = handle->loop->data;
    // fetch buffer_table from buffer_table table
    // the first 12 + 128 bytes is reserved for sockaddr and flag
    char** buffer_array = (char**)loop_data->buffer_table[slot];
    ssize_t buffer_index = loop_data->buffer_size_table[slot];
    if (buffer_index < 0) {
        //will be called in hs_udp_check_cb
        //uv_udp_recv_stop((uv_udp_t*)handle);
        buf->base = NULL;
        buf->len  = 0;
    } else {
        buf->base = (char*)buffer_array[buffer_index] + 140;
        buf->len  = *((int32_t*)buffer_array[buffer_index]);
    }
}

void hs_udp_recv_cb (uv_udp_t* udp, ssize_t nread, const uv_buf_t* _buf
    , const struct sockaddr* addr, unsigned flags){
    if (nread ==0 && addr == NULL) return;
    HsInt slot = (HsInt)udp->data;
    hs_loop_data* loop_data = udp->loop->data;

    // no bufs are available
    if (_buf->base == NULL) return;
    // EAGAIN, EWOULDBLOCK
    if (addr == NULL && nread == 0) return;
    
    // move buffer_index
    (loop_data->buffer_size_table[slot])--;

    char* buf = (char*)(_buf->base)-140;  
    struct sockaddr* addr_buf =  (struct sockaddr*)(buf+12);
    // result
    *(int32_t*)buf = (int32_t)nread;
    // flag
    *(int32_t*)(buf+4) = (int32_t)flags;

    if (addr == NULL) {
        // set sockaddr flag
        *(int32_t*)(buf+8) = 0;
    } else {
        // set sockaddr flag
        *(int32_t*)(buf+8) = 1;
        // copy sockaddr
        if (addr->sa_family == AF_INET){
            memcpy(addr_buf, addr, sizeof(struct sockaddr_in));
        } else if (addr->sa_family == AF_INET6){
            memcpy(addr_buf, addr, sizeof(struct sockaddr_in6));
        } else {
            memcpy(addr_buf, addr, sizeof(struct sockaddr));
        }
    }
}

int hs_uv_udp_recv_start(uv_udp_t* handle){
    return uv_udp_recv_start(handle, hs_udp_alloc_cb, hs_udp_recv_cb);
}

// Check if the socket's udp buffer is filled with messages, if so, unlock the udp thread
//
void hs_udp_check_cb(uv_check_t* check){
    uv_udp_t* server=(uv_udp_t*)check->data;
    HsInt slot = (HsInt)server->data;
    hs_loop_data* loop_data = server->loop->data;
    ssize_t buffer_index = loop_data->buffer_size_table[slot];
    // This relys on GHC ByteArray# memory layout, ByteArray# length is recorded before content.
    HsInt* buffer_ptr = (HsInt*)loop_data->buffer_table[slot];
    HsInt buffer_total_len = *(buffer_ptr-1)/(sizeof(void*));
    if (buffer_index < buffer_total_len -1 ) {
        // we stopped if we got some messages, will resume from haskell
        uv_udp_recv_stop(server);
        loop_data->event_queue[loop_data->event_counter] = slot; // push the slot to event queue
        loop_data->event_counter += 1;
    }
}

int hs_uv_udp_check_start(uv_check_t* check){
    return uv_check_start(check, hs_udp_check_cb);
}

void hs_uv_udp_send_cb(uv_udp_send_t* req, int status){
    HsInt slot = (HsInt)req->data;
    uv_loop_t* loop = req->handle->loop;
    hs_loop_data* loop_data = loop->data;
    loop_data->buffer_size_table[slot] = (HsInt)status;      // 0 in case of success, < 0 otherwise.
    loop_data->event_queue[loop_data->event_counter] = slot;   // push the slot to event queue
    loop_data->event_counter += 1;
    free_slot(loop_data, slot);  // free the uv_req_t
}

HsInt hs_uv_udp_send(uv_udp_t* handle, const struct sockaddr* addr, char* buf, HsInt buf_siz){
    uv_loop_t* loop = handle->loop;
    hs_loop_data* loop_data = loop->data;
    HsInt slot = alloc_slot(loop_data);
    if (slot < 0) return UV_ENOMEM;
    uv_udp_send_t* req = 
        (uv_udp_send_t*)fetch_uv_struct(loop_data, slot);
    req->data = (void*)slot;

    // on windows this struct is captured by WSASend
    // on unix this struct is copied by libuv's uv_udp_send
    // so it's safe to allocate it on stack
    uv_buf_t buf_t = { .base = buf, .len = (size_t)buf_siz };
    
    int r = uv_udp_send(req, handle, &buf_t, 1, addr, hs_uv_udp_send_cb);
                                                        // we never use writev: we do our own
                                                        // user-space buffering in haskell.
    if (r < 0) {
        free_slot(loop_data, slot);  // free the uv_req_t, the callback won't fired
        return (HsInt)r;
    } else return slot;

}

HsInt hs_uv_udp_send_connected(uv_udp_t* handle, char* buf, HsInt buf_siz){
    return hs_uv_udp_send(handle, NULL, buf, buf_siz);
}

