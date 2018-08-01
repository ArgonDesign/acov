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
    // Normal coverage points get stored as a set of values. For n-bit
    // (n > 64) values, we store a set of std::string's. These are
    // rounded up in length to the next multiple of 64 bits. For
    // 64-bit values or smaller, we store a set of uint64_t's.
    //
    // When storing "cover bits" groups, there's no need to store all
    // the values: we only care about which bits get set and cleared.
    // The values stored are the set of bits that we've seen clear and
    // bits we've seen set. All cover bits groups are stored in the
    // values1_t map (because the indices will never get larger than
    // 2^63)
    typedef std::set<uint64_t>    values1_t;
    typedef std::set<std::string> valuesn_t;

    // We store maps from group index to value. For normal records,
    // group i gets stored at index i (we will never have 2^63
    // groups!). For cover bits records, the set of bits that we've
    // seen high for group i is stored at index i and the set of bits
    // that we've seen low for group i is stored at index -(i+1). (You
    // need the +1 in case group zero is cover bits).
    typedef std::map<int64_t, values1_t> map1_t;
    typedef std::map<int64_t, valuesn_t> mapn_t;

    // The map for a given scope, from group index to values seen.
    struct scope_map_t
    {
        // Used for records of <= 64 bits and cover bits records.
        map1_t map1_;
        // Used for records of > 64 bits
        mapn_t mapn_;
    };

    struct recorder_t
    {
        void record1 (const char        *scope,
                      uint64_t           modname,
                      bool               cover_bits,
                      uint64_t           grp,
                      uint64_t           value);

        void recordn (const char        *scope,
                      uint64_t           modname,
                      bool               cover_bits,
                      uint64_t           grp,
                      const std::string &value);

        void clear ();
        void flush () const;

        void open (long long hash);

    private:
        typedef uint64_t              modname_t;
        typedef std::string           scope_t;
        typedef std::set<std::string> values_t;

        // We store module -> scope -> grp -> values
        typedef std::map<scope_t, scope_map_t>    module_map_t;
        typedef std::map<modname_t, module_map_t> map_t;

        bool      hash_valid;
        long long saved_hash;

        map_t data_;
    };
}

void recorder_t::record1 (const char        *scope,
                          uint64_t           modname,
                          bool               cover_bits,
                          uint64_t           grp,
                          uint64_t           value)
{
    if (! scope) scope = "<NO SCOPE>";

    if (cover_bits) {
        values1_t &ones  = data_ [modname][scope].map1_ [grp];
        values1_t &zeros = data_ [modname][scope].map1_ [- (grp + 1)];

        for (unsigned i = 0; i < 64; ++ i) {
            if (value & 1) {
                ones.insert (i);
            } else {
                zeros.insert (i);
            }
            value >>= 1;
        }
    } else {
        data_ [modname][scope].map1_ [grp].insert (value);
    }
}

void recorder_t::recordn (const char        *scope,
                          uint64_t           modname,
                          bool               cover_bits,
                          uint64_t           grp,
                          const std::string &value)
{
    if (! scope) scope = "<NO SCOPE>";

    if (cover_bits) {
        values1_t &ones  = data_ [modname][scope].map1_ [grp];
        values1_t &zeros = data_ [modname][scope].map1_ [- (grp + 1)];

        // Iterate over the value from LSB to MSB. Values are stored
        // in strings little-endian.
        for (unsigned idx = 0; idx < value.size (); ++ idx) {
            uint8_t byte = static_cast<uint8_t> (value [idx]);
            for (unsigned i = 0; i < 8; ++ i) {
                if (byte & 1) {
                    ones.insert (8 * idx + i);
                } else {
                    zeros.insert (8 * idx + i);
                }
                byte >>= 1;
            }
        }
    } else {
        data_ [modname][scope].mapn_ [grp].insert (value);
    }
}

static void
write_value (std::ofstream &ofile, const std::string &bytes)
{
    assert ((bytes.size () & 7) == 0);
    unsigned nwords = bytes.size () / 8;
    const uint64_t *words =
        reinterpret_cast <const uint64_t *> (bytes.c_str ());

    // The string is stored little-endian. To write the value out
    // MSB-first, we iterate backwards over the words array. We have
    // to pad internal words with zeros if necessary but, to shrink
    // the output slightly, we can avoid padding the first word.
    ofile << std::setfill('0');
    for (unsigned word = 0; word < nwords; ++ word) {
        if (word) {
            ofile << std::setw (16);
        }
        ofile << words [nwords - 1 - word];
    }
}

void recorder_t::clear ()
{
    data_.clear ();
}

void recorder_t::flush () const
{
    std::ofstream ofile ("acov.log");
    if (! ofile.good ()) {
        std::cerr << "Failed to open acov.log to flush coverage data.\n";
        return;
    }

    if (! hash_valid) {
        std::cerr << "Cannot write acov.log because the hash was never set.\n";
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

            // <= 64-bit records and cover bits records
            //
            // The cover bits records will come out first, with
            // negative indices. This is fine: we'll make sense of the
            // craziness in acov-report.
            for (map1_t::const_iterator it2 = it1->second.map1_.begin ();
                 it2 != it1->second.map1_.end ();
                 ++ it2) {
                if (it2->first < 0) {
                    ofile << "-" << - it2->first;
                } else {
                    ofile << it2->first;
                }
                ofile << ": {";
                bool first = true;
                for (values1_t::const_iterator it3 = it2->second.begin ();
                     it3 != it2->second.end ();
                     ++ it3) {
                    if (! first) ofile << ", ";
                    first = false;
                    ofile << * it3;
                }
                ofile << "}\n";
            }

            // > 64-bit records
            for (mapn_t::const_iterator it2 = it1->second.mapn_.begin ();
                 it2 != it1->second.mapn_.end ();
                 ++ it2) {
                ofile << it2->first << ": {";
                bool first = true;
                for (valuesn_t::const_iterator it3 = it2->second.begin ();
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

void recorder_t::open (long long hash)
{
    if (hash_valid && saved_hash != hash) {
        std::ios::fmtflags flags = std::cerr.flags ();
        std::cerr << "Error: Overriding existing hash (0x"
                  << std::hex << saved_hash
                  << ") with 0x"
                  << hash << ".\n";
        std::cerr.flags (flags);
    } else {
        hash_valid = true;
        saved_hash = hash;
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

static void record1 (long long modname, const char *scope,
                     bool cover_bits, long long grp, long long value) throw ()
{
    get_recorder ()->record1 (scope, modname, cover_bits, grp, value);
}

static void recordn (long long modname, const char *scope, bool cover_bits,
                     long long grp, const std::string &value) throw ()
{
    get_recorder ()->recordn (scope, modname, cover_bits, grp, value);
}

static void open (long long hash) throw()
{
    get_recorder ()->open (hash);
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

static void clear () throw ()
{
    if (recorder)
        recorder->clear ();
}

static std::string ll_to_str (long long value)
{
    return std::string (reinterpret_cast<const char *> (& value), 8);
}

extern "C" {
    void acov_record1 (long long modname, char cover_bits,
                       long long grp, long long value) {
        const char *scope = svGetNameFromScope (svGetScope ());
        bool is_cb = cover_bits != '\0';
        record1 (modname, scope, is_cb, grp, value);
    }

    void acov_record2 (long long modname, char cover_bits,
                       long long grp, long long value1, long long value0) {
        const char *scope = svGetNameFromScope (svGetScope ());
        bool is_cb = cover_bits != '\0';
        recordn (modname, scope, is_cb, grp,
                 ll_to_str (value0) + ll_to_str (value1));
    }

    void acov_record3 (long long modname,
                       char cover_bits, long long grp,
                       long long value2, long long value1, long long value0) {
        const char *scope = svGetNameFromScope (svGetScope ());
        bool is_cb = cover_bits != '\0';
        recordn (modname, scope, is_cb, grp,
                 ll_to_str (value0) +
                 ll_to_str (value1) + ll_to_str (value2));
    }

    void acov_record4 (long long modname,
                       char cover_bits, long long grp,
                       long long value3, long long value2,
                       long long value1, long long value0) {
        const char *scope = svGetNameFromScope (svGetScope ());
        bool is_cb = cover_bits != '\0';
        recordn (modname, scope, is_cb, grp,
                 ll_to_str (value0) + ll_to_str (value1) +
                 ll_to_str (value2) + ll_to_str (value3));
    }

    void acov_open (long long hash) {
        open (hash);
    }

    void acov_close () {
        close ();
    }

    void acov_clear () {
        clear ();
    }
}
