#include <functional>
#include "eckit/io/MemoryHandle.h"
#include "eckit/io/FileDescHandle.h"
#include "eckit/message/Message.h"
#include "eckit/runtime/Main.h"
#include "eckit/exception/Exceptions.h"
#include "eckit/io/DataHandle.h"

#include "message/OpenFile_C.h"

using namespace eckit;


extern "C" {
/* Error handling */

static std::string g_current_error_str;
static fdb_failure_handler_t g_failure_handler = nullptr;
static void* g_failure_handler_context = nullptr;

const char* fdb_error_string(int err) {
    switch (err) {
    case FDB_SUCCESS:
        return "Success";
    case FDB_ERROR_GENERAL_EXCEPTION:
    case FDB_ERROR_UNKNOWN_EXCEPTION:
        return g_current_error_str.c_str();
    case FDB_ITERATION_COMPLETE:
        return "Iteration complete";
    default:
        return "<unknown>";
    };
}

} // extern "C"


namespace {

// Template magic to provide a consistent error-handling approach
int innerWrapFn(std::function<int()> f) {
    return f();
}

int innerWrapFn(std::function<void()> f) {
    f();
    return FDB_SUCCESS;
}

template <typename FN>
int wrapApiFunction(FN f) {

    try {
        return innerWrapFn(f);
    } catch (Exception& e) {
        Log::error() << "Caught exception on C-C++ API boundary: " << e.what() << std::endl;
        g_current_error_str = e.what();
        if (g_failure_handler) {
            g_failure_handler(g_failure_handler_context, FDB_ERROR_GENERAL_EXCEPTION);
        }
        return FDB_ERROR_GENERAL_EXCEPTION;
    } catch (std::exception& e) {
        Log::error() << "Caught exception on C-C++ API boundary: " << e.what() << std::endl;
        g_current_error_str = e.what();
        if (g_failure_handler) {
            g_failure_handler(g_failure_handler_context, FDB_ERROR_GENERAL_EXCEPTION);
        }
        return FDB_ERROR_GENERAL_EXCEPTION;
    } catch (...) {
        Log::error() << "Caught unknown on C-C++ API boundary" << std::endl;
        g_current_error_str = "Unrecognised and unknown exception";
        if (g_failure_handler) {
            g_failure_handler(g_failure_handler_context, FDB_ERROR_UNKNOWN_EXCEPTION);
        }
        return FDB_ERROR_UNKNOWN_EXCEPTION;
    }

    ASSERT(false);
}

}

extern "C" {

// struct data_handle_t : public DataHandle {
//     using DataHandle::DataHandle;
// };

struct fdb_datareader_t {
public:
    long open() {
        ASSERT(dh_);
        return dh_->openForRead();
    }
    FILE* openf(bool delete_on_close) {
        ASSERT(dh_);
        return dh_->openf(delete_on_close);
    }
    FILE* openfm(const char* mode, bool delete_on_close) {
        ASSERT(dh_);
        return dh_->openf(mode, delete_on_close);
    }
    void close() {
        ASSERT(dh_);
        dh_->close();
    }
    long tell() {
        ASSERT(dh_);
        return dh_->position();
    }
    long seek(long pos) {
        ASSERT(dh_);
        return dh_->seek(pos);
    }
    void skip(long pos) {
        ASSERT(dh_);
        dh_->skip(pos);
    }
    long read(void* buf, long length) {
        ASSERT(dh_);
        return dh_->read(buf, length);
    }
    void set(DataHandle* dh) {
        if (dh_)
            delete dh_;
        dh_ = dh;
    }
    
private:
    DataHandle* dh_;
};

int dr_openf(fdb_datareader_t* dh, bool delete_on_close, FILE** file) {
    return wrapApiFunction([dh, delete_on_close, file] {
        ASSERT(dh);
        ASSERT(delete_on_close);
        *file = dh->openf(delete_on_close);
    });
}

int dr_openfm(fdb_datareader_t* dh, const char* mode, bool delete_on_close, FILE** file) {
    return wrapApiFunction([dh, mode, delete_on_close, file] {
        ASSERT(dh);
        ASSERT(mode);
        ASSERT(delete_on_close);
        *file = dh->openfm(mode, delete_on_close);
    });
}


// int close_fn(fdb_datareader_t* dh) {
//     return wrapApiFunction([dh] {
//         ASSERT(dh);
//         closefn(dh);
//     });
// }

}