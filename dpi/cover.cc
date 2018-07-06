// A backend for coverage collection over DPI
//
// As a first version, this doesn't do merging of coverage data - we
// assume that each test runs in a separate directory so we don't need
// that.

#include <fstream>
#include <iostream>
#include <map>
#include <set>
#include <string>
#include <stdint.h>

#include <svdpi.h>

namespace {
    struct recorder_t
    {
        void record (const char *scope,
                     const char *name,
                     uint64_t    value);
        void flush () const;

    private:
        typedef std::string                   scope_t;
        typedef std::string                   recname_t;
        typedef std::set<uint64_t>            values_t;

        typedef std::map<recname_t, values_t>      map_for_scope_t;
        typedef std::map<scope_t, map_for_scope_t> rec_map_t;

        rec_map_t data_;
    };
}

void recorder_t::record (const char *scope,
                         const char *name,
                         uint64_t    value)
{
    if (! scope) scope = "<NO SCOPE>";
    if (! name) name = "<NO NAME>";

    data_ [scope][name].insert (value);
}

void recorder_t::flush () const
{
    std::ofstream ofile ("acov.log");
    if (! ofile.good ()) {
        std::cerr << "Failed to open acov.log to flush coverage data.\n";
        return;
    }

    for (rec_map_t::const_iterator it0 = data_.begin ();
         it0 != data_.end ();
         ++ it0) {
        ofile << "SCOPE: " << it0->first << "\n";
        for (map_for_scope_t::const_iterator it1 = it0->second.begin ();
             it1 != it0->second.end ();
             ++ it1) {
            // Write recname
            ofile << it1->first << " : {";

            bool first = true;
            for (values_t::const_iterator it2 = it1->second.begin ();
                 it2 != it1->second.end ();
                 ++ it2) {
                // Write value
                if (! first) {
                    ofile << ", ";
                }
                ofile << * it2;
                first = false;
            }
            ofile << "}\n";
        }
    }
}

static recorder_t *recorder;

static recorder_t *get_recorder () throw()
{
    if (recorder)
        return recorder;

    recorder = new recorder_t;
    return recorder;
}

static void close () throw ()
{
    if (recorder) {
        try {
            recorder->flush ();
        }
        catch (const std::runtime_error &err) {
            std::cerr << "Error when flushing coverage recorder: " << err.what () << "\n";
        }
        delete recorder;
    }
}

extern "C" {
    void acov_record (const char *name, long long value) {
        const char *scope = svGetNameFromScope (svGetScope ());
        get_recorder ()->record (scope, name, (uint64_t) value);
    }

    void acov_close () {
        close ();
    }
}
