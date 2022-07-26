/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

/// @file   MessageArchiver.h
/// @author Baudouin Raoult
/// @author Tiago Quintino
/// @date   Mar 2016

#pragma once

#include <iosfwd>

#include "eckit/io/Length.h"
#include "eckit/message/Message.h"

#include "metkit/mars/MarsRequest.h"

#include "fdb5/database/Archiver.h"
#include "fdb5/config/Config.h"
#include "fdb5/message/MessageDecoder.h"
#include "fdb5/database/Key.h"
#include "fdb5/api/FDB.h"

namespace eckit
{
    class DataHandle;
}

namespace fdbf
{

    //----------------------------------------------------------------------------------------------------------------------

    class MessageArchiverF : public fdb5::MessageDecoder
    {

    public: // methods
        MessageArchiverF(const std::string &keys,
                         const fdb5::Config &config = fdb5::Config());

        eckit::Length archive(eckit::DataHandle &source);

        void flush();

    private: // protected
        eckit::Channel &logVerbose() const;
        void getMetadata(const eckit::message::Message &msg,
                         eckit::message::MetadataGatherer &gather, eckit::StringList sel_keys) const;

    private: // members
        fdb5::FDB fdb_;

        const std::string &keys_ = "kk";

        std::vector<metkit::mars::MarsRequest> include_;
        std::vector<metkit::mars::MarsRequest> exclude_;

        eckit::StringDict modifiers_;
        bool verbose_ = true;
    };

    //----------------------------------------------------------------------------------------------------------------------

} // namespace fdbf
