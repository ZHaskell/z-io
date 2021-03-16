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

#include <zlib.h>
#include <stdlib.h>
#include <HsFFI.h>

z_stream* create_z_stream(z_stream** streamp){
    z_stream* stream = malloc(sizeof(z_stream));
    *streamp = stream;
    if (stream) {
        stream->zalloc = Z_NULL;
        stream->zfree = Z_NULL;
        stream->opaque = Z_NULL;
        stream->next_in = NULL;
        stream->avail_in = 0;
        stream->next_out = NULL;
        stream->avail_out = 0;
    }
    return stream;
}

int deflate_init2(z_stream *stream, int level, int methodBits, int memlevel, int strategy)
{
    return deflateInit2(stream, level, Z_DEFLATED, methodBits, memlevel, strategy);
}

void free_z_stream_deflate (z_stream *stream)
{
	deflateEnd(stream);
	free(stream);
}

int inflate_init2(z_stream *stream, int methodBits)
{
	return inflateInit2(stream, methodBits);
}

void free_z_stream_inflate (z_stream *stream)
{
	inflateEnd(stream);
	free(stream);
}

unsigned int deflate_set_dictionary (z_stream* stream, const unsigned char* dict, HsInt off, HsInt len){
    return deflateSetDictionary(stream, dict+off, (unsigned int)len);
}

unsigned int inflate_set_dictionary (z_stream* stream, const unsigned char* dict, HsInt off, HsInt len){
    return inflateSetDictionary(stream, dict+off, (unsigned int)len);
}
