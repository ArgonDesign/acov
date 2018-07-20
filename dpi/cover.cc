// A backend for coverage collection over DPI
//
// As a first version, this doesn't do merging of coverage data - we
// assume that each test runs in a separate directory so we don't need
// that.

#include <fstream>
#include <iomanip>
#include <iostream>
#include <map>
#include <set>
#include <string>
#include <stdexcept>

#include <stdint.h>
#include <assert.h>

#include <svdpi.h>

namespace {
    struct recorder_t
    {
        void record (const char *scope,
                     const char *name,
                     const std::string &value);
        void flush () const;

    private:
        typedef std::string           scope_t;
        typedef std::string           recname_t;
        typedef std::set<std::string> values_t;

        typedef std::map<recname_t, values_t>      map_for_scope_t;
        typedef std::map<scope_t, map_for_scope_t> rec_map_t;

        rec_map_t data_;
    };
}

void recorder_t::record (const char        *scope,
                         const char        *name,
                         const std::string &value)
{
    if (! scope) scope = "<NO SCOPE>";
    if (! name) name = "<NO NAME>";

    data_ [scope][name].insert (value);
}

static void
write_value (std::ofstream &ofile, const std::string &bytes)
{
    assert ((bytes.size () & 7) == 0);
    unsigned nwords = bytes.size () / 8;
    const uint64_t *words =
        reinterpret_cast <const uint64_t *> (bytes.c_str ());

    // The words array is stored MSB first, so we can write it out as
    // a hex number by concatenation. We have to pad internal words
    // with zeros if necessary but, to shrink the output slightly, we
    // can avoid padding the first word.
    //
    // Since I suspect most recorded groups will be less than 64 bits
    // in size, this should make a significant difference.
    ofile << std::setfill('0');
    for (unsigned word = 0; word < nwords; ++ word) {
        if (word) {
            ofile << std::setw (16);
        }
        ofile << words [word];
    }
}

void recorder_t::flush () const
{
    std::ofstream ofile ("acov.log");
    if (! ofile.good ()) {
        std::cerr << "Failed to open acov.log to flush coverage data.\n";
        return;
    }

    ofile << std::hex;

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
                write_value (ofile, *it2);
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
        catch (const std::exception &err) {
            std::cerr << "Error when flushing coverage recorder: "
                      << err.what () << "\n";
        }
        delete recorder;
        recorder = NULL;
    }
}

static std::string ll_to_str (long long value)
{
    return std::string (reinterpret_cast<const char *> (& value), 8);
}

extern "C" {
    void acov_record1 (const char *name, long long value) {
        const char *scope = svGetNameFromScope (svGetScope ());
        get_recorder ()->record (scope, name, ll_to_str (value));
    }

    void acov_record2 (const char *name,
                       long long value1, long long value0) {
        const char *scope = svGetNameFromScope (svGetScope ());
        get_recorder ()->record (scope, name,
                                 ll_to_str (value1) + ll_to_str (value0));
    }

    void acov_record3 (const char *name,
                       long long value2, long long value1, long long value0) {
        const char *scope = svGetNameFromScope (svGetScope ());
        get_recorder ()->record (scope, name,
                                 ll_to_str (value2) + ll_to_str (value1) +
                                 ll_to_str (value0));
    }

    void acov_record4 (const char *name,
                       long long value3, long long value2,
                       long long value1, long long value0) {
        const char *scope = svGetNameFromScope (svGetScope ());
        get_recorder ()->record (scope, name,
                                 ll_to_str (value3) + ll_to_str (value2) +
                                 ll_to_str (value1) + ll_to_str (value0));
    }

    void acov_close () {
        close ();
    }
}
