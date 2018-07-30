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
        void record (const char        *scope,
                     uint64_t           modname,
                     uint64_t           grp,
                     const std::string &value);
        void flush () const;

    private:
        typedef uint64_t              modname_t;
        typedef std::string           scope_t;
        typedef std::set<std::string> values_t;

        // We store module -> scope -> grp -> values
        typedef std::map<uint64_t, values_t>      scope_map_t;
        typedef std::map<scope_t, scope_map_t>    module_map_t;
        typedef std::map<modname_t, module_map_t> map_t;

        map_t data_;
    };
}

void recorder_t::record (const char        *scope,
                         uint64_t           modname,
                         uint64_t           grp,
                         const std::string &value)
{
    if (! scope) scope = "<NO SCOPE>";

    data_ [modname][scope][grp].insert (value);
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

static bool        hash_valid;
static long long   saved_hash;
static recorder_t *recorder;

void recorder_t::flush () const
{
    std::ofstream ofile ("acov.log");
    if (! ofile.good ()) {
        std::cerr << "Failed to open acov.log to flush coverage data.\n";
        return;
    }

    ofile << std::hex;
    ofile << saved_hash << "\n";

    for (map_t::const_iterator it0 = data_.begin ();
         it0 != data_.end ();
         ++ it0) {
        ofile << "MODULE: " << it0->first << "\n";

        for (module_map_t::const_iterator it1 = it0->second.begin ();
             it1 != it0->second.end ();
             ++ it1) {
            ofile << "SCOPE: " << it1 -> first << "\n";

            for (scope_map_t::const_iterator it2 = it1->second.begin ();
                 it2 != it1->second.end ();
                 ++ it2) {
                ofile << it2->first << ": {";
                bool first = true;
                for (values_t::const_iterator it3 = it2->second.begin ();
                     it3 != it2->second.end ();
                     ++ it3) {
                    if (! first) ofile << ", ";
                    first = false;
                    write_value (ofile, * it3);
                }
                ofile << "}\n";
            }
        }
    }
}

static recorder_t *get_recorder () throw()
{
    if (recorder)
        return recorder;

    if (! hash_valid) {
        std::cerr << "Error: Instantiating recorder before setting hash.\n";
    }

    recorder = new recorder_t;
    return recorder;
}

static void open (long long hash) throw()
{
    if (hash_valid && saved_hash != hash) {
        std::ios::fmtflags flags = std::cerr.flags ();
        std::cerr << "Error: Overriding existing hash (0x"
                  << std::hex << saved_hash
                  << ") with 0x"
                  << hash << ".\n";
        std::cerr.flags (flags);
    }

    hash_valid = true;
    saved_hash = hash;
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
    void acov_record1 (long long modname, long long grp, long long value) {
        const char *scope = svGetNameFromScope (svGetScope ());
        get_recorder ()->record (scope, modname, grp, ll_to_str (value));
    }

    void acov_record2 (long long modname, long long grp,
                       long long value1, long long value0) {
        const char *scope = svGetNameFromScope (svGetScope ());
        get_recorder ()->record (scope, modname, grp,
                                 ll_to_str (value1) + ll_to_str (value0));
    }

    void acov_record3 (long long modname, long long grp,
                       long long value2, long long value1, long long value0) {
        const char *scope = svGetNameFromScope (svGetScope ());
        get_recorder ()->record (scope, modname, grp,
                                 ll_to_str (value2) + ll_to_str (value1) +
                                 ll_to_str (value0));
    }

    void acov_record4 (long long modname, long long grp,
                       long long value3, long long value2,
                       long long value1, long long value0) {
        const char *scope = svGetNameFromScope (svGetScope ());
        get_recorder ()->record (scope, modname, grp,
                                 ll_to_str (value3) + ll_to_str (value2) +
                                 ll_to_str (value1) + ll_to_str (value0));
    }

    void acov_open (long long hash) {
        open (hash);
    }

    void acov_close () {
        close ();
    }
}
