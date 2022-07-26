/*
 * (C) Copyright 1996- ECMWF.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 * In applying this licence, ECMWF does not waive the privileges and immunities
 * granted to it by virtue of its status as an intergovernmental organisation nor
 * does it submit to any jurisdiction.
 */

#include <memory>

#include "eckit/config/LocalConfiguration.h"
#include "eckit/io/DataHandle.h"
#include "eckit/option/CmdArgs.h"
#include "eckit/option/SimpleOption.h"
#include "eckit/option/VectorOption.h"
#include "eckit/log/Log.h"

#include "message/MessageArchiverF.h"
#include "fdb5/tools/FDBTool.h"
#include "fdb5/api/FDB.h"

class FDBWrite : public fdb5::FDBTool
{

    virtual void usage(const std::string &tool) const;

    virtual void init(const eckit::option::CmdArgs &args);

    virtual int minimumPositionalArguments() const { return 1; }

    virtual void execute(const eckit::option::CmdArgs &args);

public:
    FDBWrite(int argc, char **argv) : fdb5::FDBTool(argc, argv),
                                      verbose_(false)
    {

        options_.push_back(
            new eckit::option::SimpleOption<std::string>("keys",
                                                         "List of comma separated key-values of what to include from the input data, e.g --include-filter=stream=enfo,date=10102017"));

        options_.push_back(new eckit::option::SimpleOption<bool>("verbose", "Print verbose output"));
    }

    std::string keys_;
    bool verbose_;
};

void FDBWrite::usage(const std::string &tool) const
{
    eckit::Log::info() << std::endl
                       << "Usage: " << tool << " [--filter-include=...] [--filter-exclude=...] <path1> [path2] ..." << std::endl;
    fdb5::FDBTool::usage(tool);
}

void FDBWrite::init(const eckit::option::CmdArgs &args)
{
    FDBTool::init(args);
    args.get("keys", keys_);
    verbose_ = args.getBool("verbose", false);
}

void FDBWrite::execute(const eckit::option::CmdArgs &args)
{

    eckit::Log::info() << "DDD" << keys_ << std::endl;

    fdbf::MessageArchiverF archiver(keys_, config(args));

    for (size_t i = 0; i < args.count(); i++)
    {
        eckit::PathName path(args(i));
        eckit::Log::info() << "Processing " << path << std::endl;
        std::unique_ptr<eckit::DataHandle> dh(path.fileHandle());
        archiver.archive(*dh);
    }
}

//----------------------------------------------------------------------------------------------------------------------

int main(int argc, char **argv)
{
    FDBWrite app(argc, argv);
    return app.start();
}
