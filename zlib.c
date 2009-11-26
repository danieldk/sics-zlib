/*
 * A simple zlib module for Sicstus Prolog
 *
 * Copyright (c) 2009 Daniel de Kok
 *
 * This software is provided 'as-is', without any express or implied
 * warranty.  In no event will the authors be held liable for any damages
 * arising from the use of this software.
 *
 * Permission is granted to anyone to use this software for any purpose,
 * including commercial applications, and to alter it and redistribute it
 * freely, subject to the following restrictions:
 *
 * 1. The origin of this software must not be misrepresented; you must not
 *    claim that you wrote the original software. If you use this software
 *    in a product, an acknowledgment in the product documentation would be
 *    appreciated but is not required.
 * 2. Altered source versions must be plainly marked as such, and must not be
 *    misrepresented as being the original software.
 * 3. This notice may not be removed or altered from any source distribution.
 *
 */

#include <math.h>
#include <stdlib.h>
#include <string.h>
#include <zlib.h>

#include <sicstus/sicstus.h>

int list_length(SP_term_ref list, size_t *len)
{
  if (!SP_is_list(list))
      return 0;

  SP_term_ref head = SP_new_term_ref();
  SP_term_ref tail = SP_new_term_ref();
  SP_term_ref tmp = list;

  *len = 0;
  while (SP_get_list(tmp,head,tail) == SP_SUCCESS) {
    ++(*len);
    tmp = tail;
  }

  return 1;
}

// Put the first n bytes of a Sicstus list in a buffer. Returns a
// pointer to the array if the conversion succeeded, otherwise returns
// NULL.
char *list_to_buf(SP_term_ref data, size_t n)
{
  char *cData = malloc(n);
  size_t w;
  SP_term_ref tail = SP_new_term_ref();
  SP_get_list_n_bytes(data, tail, n, &w, cData);

  if (w != n) {
    free(cData);
    cData = NULL;
  }

  return cData;
}

void raise_exception(char const *message)
{
  SP_term_ref m = SP_new_term_ref();
  SP_put_string(m, message);
  SP_raise_exception(m);
}

SP_term_ref zlib_compress(SP_term_ref data)
{
  SP_term_ref zData = SP_new_term_ref();

  size_t dataLen;
  int r = list_length(data, &dataLen);
  if (!r) {
    raise_exception("Error in zlib_compress: argument is not a valid list!");
    return zData;
  }

  char *cData = list_to_buf(data, dataLen);
  if (cData == NULL) {
    raise_exception("Error in zlib_compress: could not convert list!");
    return zData;
  }

  /* Prepare destination buffer */
  size_t zSize = ceill((dataLen + 1) * 1.001) + 12;
  char *zcData = (char *) malloc(zSize);

  /* Compress */
  r = compress(zcData, &zSize, cData, dataLen);
  free(cData);
  if (r != Z_OK) {
    switch (r) {
    case Z_MEM_ERROR:
      raise_exception("Error in zlib_compress: not enough memory!");
      break;
    case Z_BUF_ERROR:
      raise_exception("Error in zlib_compress: output buffer is too small!");
      break;
    }

    free(zcData);

    return zData;
  }

  SP_term_ref tail = SP_new_term_ref();
  r = SP_put_list_n_bytes(zData, tail, zSize, zcData);
  if (!r)
    raise_exception("Error in zlib_compress: could not convert buffer!");

  free(zcData);

  return zData;
}

SP_term_ref zlib_uncompress(SP_term_ref zData, long dataLen)
{
  SP_term_ref data = SP_new_term_ref();

  size_t zDataLen;
  int r = list_length(zData, &zDataLen);
  if (!r) {
    raise_exception("Error in zlib_uncompress: argument is not a valid list!");
    return data;
  }

  char *zcData = list_to_buf(zData, zDataLen);
  if (zcData == NULL) {
    raise_exception("Error in zlib_uncompress: could not convert list!");
    return data;
  }

  /* Output buffer */
  char *cData = (char *) malloc(dataLen);

  /* Uncompress */
  r = uncompress(cData, &dataLen, zcData, zDataLen);
  free(zcData);
  if (r != Z_OK) {
    switch(r) {
    case Z_MEM_ERROR:
      raise_exception("Error in zlib_uncompress: not enough memory!");
      break;
    case Z_BUF_ERROR:
      raise_exception("Error in zlib_uncompress: output buffer is too small!");
      break;
    case Z_DATA_ERROR:
      raise_exception("Error in zlib_uncompress: corrupted data!");
      break;
    default:
      raise_exception("Error in zlib_uncompress: unknown error!");
    }

    free(cData);

    return data;
  }

  SP_term_ref tail = SP_new_term_ref();
  r = SP_put_list_n_bytes(data, tail, dataLen, cData);
  if (!r)
    raise_exception("Error in zlib_uncompress: could not convert buffer!");

  free(cData);

  return data;
}
