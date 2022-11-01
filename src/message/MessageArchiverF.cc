/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include <vector>

#include "eccodes.h"
#include "eckit/log/Timer.h"
#include "eckit/log/Plural.h"
#include "eckit/log/Bytes.h"
#include "eckit/log/Seconds.h"
#include "eckit/log/Progress.h"

#include "eckit/utils/Tokenizer.h"

#include "eckit/message/Reader.h"
#include "eckit/serialisation/MemoryStream.h"

#include "metkit/mars/MarsParser.h"
#include "metkit/mars/MarsExpension.h"
#include "metkit/mars/MarsRequest.h"

#include "fdb5/LibFdb5.h"
#include "message/MessageArchiverF.h"
#include "fdb5/api/FDB.h"

using namespace fdb5;
namespace
{
    class HandleDeleter
    {
        codes_handle *h_;

    public:
        HandleDeleter(codes_handle *h) : h_(h) {}
        ~HandleDeleter()
        {
            if (h_)
            {
                codes_handle_delete(h_);
            }
        }

        codes_handle *get() { return h_; }
    };

    class KeySetter : public eckit::message::MetadataGatherer
    {

        void setValue(const std::string &key, const std::string &value) override
        {
            key_.set(key, value);
        }

        void setValue(const std::string &key, long value) override
        {
            if (key_.find(key) == key_.end())
            {
                key_.set(key, std::to_string(value));
            }
        }

        void setValue(const std::string &key, double value) override
        {
            if (key_.find(key) == key_.end())
            {
                key_.set(key, std::to_string(value));
            }
        }

    protected:
        Key &key_;

    public:
        KeySetter(Key &key) : key_(key)
        {
            ASSERT(key_.empty());
        }
    };

}
namespace fdbf
{

    using eckit::Log;

    //----------------------------------------------------------------------------------------------------------------------

    MessageArchiverF::MessageArchiverF(const std::string &keys, const Config &config) : MessageDecoder(),
                                                                                        fdb_(config),
                                                                                        keys_(keys)
    {
    }

    eckit::Channel &MessageArchiverF::logVerbose() const
    {
        return verbose_ ? Log::info() : Log::debug<LibFdb5>();
    }

    void MessageArchiverF::getMetadata(const eckit::message::Message &msg,
                                       eckit::message::MetadataGatherer &gather,
                                       eckit::StringList sel_keys) const
    {
        codes_handle *h = codes_handle_new_from_message(nullptr, msg.data(), msg.length());
        ASSERT(h);
        HandleDeleter d(h);

        grib_keys_iterator *ks = grib_keys_iterator_new(h,
                                                        GRIB_KEYS_ITERATOR_ALL_KEYS, NULL);

        ASSERT(ks);

        while (grib_keys_iterator_next(ks))
        {
            const char *name = grib_keys_iterator_get_name(ks);

            bool found = false;

            std::set<std::string> keys_found;
            for (std::vector<std::string>::const_iterator it = sel_keys.begin(); it != sel_keys.end(); it++)
            {
                // std::cout << "checking key" << name << std::endl;
                if (strcmp((*it).c_str(), name) == 0)
                {
                    found = true;
                    sel_keys.erase(it);
                    break;
                }
            }
            if (!found)
                continue;

            if (name[0] == '_')
                continue; // skip silly underscores in GRIB

            char val[1024];
            size_t len = sizeof(val);
            double d;
            long l;

            ASSERT(grib_keys_iterator_get_string(ks, val, &len) == 0);

            if (*val)
            {
                gather.setValue(name, val);
            }

            len = 1;
            if (grib_keys_iterator_get_double(ks, &d, &len) == 0)
            {
                gather.setValue(name, d);
            }
            len = 1;
            if (grib_keys_iterator_get_long(ks, &l, &len) == 0)
            {
                gather.setValue(name, l);
            }
        }

        grib_keys_iterator_delete(ks);

        // Look for request embbeded in GRIB message
        long local;
        size_t size;
        if (grib_get_long(h, "localDefinitionNumber", &local) == 0 && local == 191)
        {
            /* TODO: Not grib2 compatible, but speed-up process */
            if (grib_get_size(h, "freeFormData", &size) == 0 && size != 0)
            {
                unsigned char buffer[size];
                ASSERT(grib_get_bytes(h, "freeFormData", buffer, &size) == 0);

                eckit::MemoryStream s(buffer, size);

                int count;
                s >> count; // Number of requests
                ASSERT(count == 1);
                std::string tmp;
                s >> tmp; // verb
                s >> count;
                for (int i = 0; i < count; i++)
                {
                    std::string keyword, value;
                    int n;
                    s >> keyword;
                    std::transform(keyword.begin(), keyword.end(), keyword.begin(), tolower);
                    s >> n; // Number of values
                    ASSERT(n == 1);
                    s >> value;
                    std::transform(value.begin(), value.end(), value.begin(), tolower);
                    gather.setValue(keyword, value);
                }
            }
        }
    }

    eckit::Length MessageArchiverF::archive(eckit::DataHandle &source)
    {

        eckit::Timer timer("fdb::service::archive");

        eckit::message::Reader reader(source);

        size_t count = 0;
        size_t total_size = 0;

        eckit::Progress progress("FDB archive", 0, source.estimate());

        eckit::Tokenizer parse1(",");
        eckit::StringList v;

        parse1(keys_, v);

        try
        {
            eckit::message::Message msg;

            while ((msg = reader.next()))
            {

                Key key;
                KeySetter setter(key);
                getMetadata(msg, setter, v);

                LOG_DEBUG_LIB(LibFdb5) << "Archiving message "
                                       << " key: " << key << " data: " << msg.data() << " length:" << msg.length()
                                       << std::endl;

                logVerbose() << "Archiving " << key << std::endl;

                fdb_.archive(key, msg.data(), msg.length());

                total_size += msg.length();
                count++;
                progress(total_size);

            }
            flush();
        }
        catch (...)
        {
            throw;
        }

        eckit::Log::userInfo() << "Archived " << eckit::Plural(count, "message") << std::endl;

        eckit::Log::info() << "FDB archive " << eckit::Plural(count, "message") << ","
                           << " size " << eckit::Bytes(total_size) << ","
                           << " in " << eckit::Seconds(timer.elapsed()) << " (" << eckit::Bytes(total_size, timer) << ")"
                           << std::endl;

        return total_size;
    }

    void MessageArchiverF::flush()
    {
        fdb_.flush();
    }

    //----------------------------------------------------------------------------------------------------------------------

} // namespace fdb5
