#pragma once

#include <iosfwd>
#include <stdbool.h>
#include <stddef.h>
#include "eckit/io/MemoryHandle.h"
#include "eckit/io/FileDescHandle.h"
#include "eckit/message/Message.h"
#include "eckit/runtime/Main.h"
#include "eckit/io/DataHandle.h"


namespace eckit
{
    class DataHandle;
}

extern "C" {


/** \defgroup Error Handling */
/** @{ */

/** Return codes */
enum FdbErrorValues {
    FDB_SUCCESS                  = 0,
    FDB_ERROR_GENERAL_EXCEPTION  = 1,
    FDB_ERROR_UNKNOWN_EXCEPTION  = 2,
    FDB_ITERATION_COMPLETE       = 3
};

/** Returns a human-readable error message for the last error given an error code
 * \param err Error code (#FdbErrorValues)
 * \returns Error message
 */
const char* fdb_error_string(int err);

/** Error handler callback function signature
 * \param context Error handler context
 * \param error_code Error code (#FdbErrorValues)
 */
typedef void (*fdb_failure_handler_t)(void* context, int error_code);


struct fdb_datareader_t;
/** Opaque type for the DataReader object. Provides access to the binary data returned by a FDB retrieval. */
typedef struct fdb_datareader_t fdb_datareader_t;

int dr_openf(fdb_datareader_t* dh, bool delete_on_close, FILE** file);
int dr_openfm(fdb_datareader_t* dh, const char* mode, bool delete_on_close, FILE** file);

// int close_fn(fdb_datareader_t* dh)
}