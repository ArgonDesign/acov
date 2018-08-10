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
#include <vector>

#include <assert.h>
#include <stdint.h>
#include <stdlib.h>

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

    // This structure holds information the scopes we've seen for a
    // module. This is stored as a map from scope name to integer.
    // This integer indexes into the big vector of contexts where we
    // allocated an entry by calling open().
    struct module_scopes_t
    {
        std::map<std::string, size_t> scope_posns_;
    };

    struct recorder_t
    {
        recorder_t ();

        void record1 (uint64_t           ctxt_idx,
                      bool               cover_bits,
                      uint64_t           grp,
                      uint64_t           value);

        void recordn (uint64_t           ctxt_idx,
                      bool               cover_bits,
                      uint64_t           grp,
                      const std::string &value);

        // Clear all recorded coverage. Leave saved_hash_ and modules_
        // unmolested.
        void clear ();

        // Write recorded coverage to 'acov.log'.
        void flush () const;

        // Open a context, returning a ctxt_idx handle to pass into record*.
        size_t open (const std::string &scope,
                     uint64_t           hash,
                     uint64_t           mod_idx);

    private:
        // Track whether the hash has been initialised yet.
        bool                                hash_valid_;
        uint64_t                            saved_hash_;

        // We store a map from module index to data about scopes of
        // that module.
        std::map<uint64_t, module_scopes_t> modules_;

        // For each module scope, we have a scope_map_t, which records
        // the values we've seen. This is indexed by the "ctxt" index
        // returned by acov_open.
        std::vector<scope_map_t>            coverage_;
    };
}

recorder_t::recorder_t ()
    : hash_valid_ (false)
{}

void recorder_t::record1 (uint64_t           ctxt_idx,
                          bool               cover_bits,
                          uint64_t           grp,
                          uint64_t           value)
{
    map1_t &map1 = coverage_.at (ctxt_idx).map1_;

    if (cover_bits) {
        values1_t &ones  = map1 [grp];
        values1_t &zeros = map1 [- (grp + 1)];

        for (unsigned i = 0; i < 64; ++ i) {
            if (value & 1) {
                ones.insert (i);
            } else {
                zeros.insert (i);
            }
            value >>= 1;
        }
    } else {
        map1 [grp].insert (value);
    }
}

void recorder_t::recordn (uint64_t           ctxt_idx,
                          bool               cover_bits,
                          uint64_t           grp,
                          const std::string &value)
{
    scope_map_t &scope_map = coverage_.at (ctxt_idx);

    if (cover_bits) {
        values1_t &ones  = scope_map.map1_ [grp];
        values1_t &zeros = scope_map.map1_ [- (grp + 1)];

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
        scope_map.mapn_ [grp].insert (value);
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
    std::vector<scope_map_t>::iterator it;
    for (it = coverage_.begin (); it != coverage_.end (); ++ it) {
        it->map1_.clear ();
        it->mapn_.clear ();
    }
}

void recorder_t::flush () const
{
    std::ofstream ofile ("acov.log");
    if (! ofile.good ()) {
        std::cerr << "Failed to open acov.log to flush coverage data.\n";
        return;
    }

    if (! hash_valid_) {
        std::cerr << "Cannot write acov.log because the hash was never set.\n";
        return;
    }

    ofile << std::hex;
    ofile << saved_hash_ << "\n";

    std::map<uint64_t, module_scopes_t>::const_iterator it0;
    for (it0 = modules_.begin (); it0 != modules_.end (); ++ it0) {
        ofile << "MODULE: " << it0->first << "\n";

        std::map<std::string, size_t>::const_iterator it1;
        for (it1 = it0->second.scope_posns_.begin ();
             it1 != it0->second.scope_posns_.end ();
             ++ it1) {

            ofile << "SCOPE: " << it1->first << "\n";
            const scope_map_t &scope_map = coverage_.at (it1->second);

            // <= 64-bit records and cover bits records
            //
            // The cover bits records will come out first, with
            // negative indices. This is fine: we'll make sense of the
            // craziness in acov-report.
            for (map1_t::const_iterator it2 = scope_map.map1_.begin ();
                 it2 != scope_map.map1_.end ();
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
            for (mapn_t::const_iterator it2 = scope_map.mapn_.begin ();
                 it2 != scope_map.mapn_.end ();
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

size_t recorder_t::open (const std::string &scope,
                         uint64_t           hash,
                         uint64_t           mod_idx)
{
    // First, deal with the hash. This should be a global which is the
    // same every time open is called.
    if (hash_valid_ && saved_hash_ != hash) {
        std::ios::fmtflags flags = std::cerr.flags ();
        std::cerr << "Error: Overriding existing hash (0x"
                  << std::hex << saved_hash_
                  << ") with 0x"
                  << hash << ".\n";
        std::cerr.flags (flags);
    } else if (! hash_valid_) {
        hash_valid_ = true;
        saved_hash_ = hash;
    }

    // Now we deal with the scope. We need to allocate a new position
    // for it in coverage.
    size_t ctxt_idx = coverage_.size ();
    coverage_.push_back (scope_map_t ());
    scope_map_t &cmap = coverage_.back ();

    // At this point, cmap is a shiny new map where we want to store
    // coverage data. Its index in coverage_ is stored in ctxt_idx.
    module_scopes_t &mscopes = modules_ [mod_idx];

    typedef std::map<std::string, size_t> map_t;

    std::pair<map_t::iterator, bool> pr =
        mscopes.scope_posns_.insert (map_t::value_type (scope, ctxt_idx));

    // It is an error to call open() twice with the same scope, so
    // let's check that didn't happen.
    assert (pr.second);

    // All is well. Return the index
    return ctxt_idx;
}

static recorder_t *recorder;

static recorder_t *get_recorder () throw()
{
    if (recorder)
        return recorder;

    recorder = new recorder_t;
    return recorder;
}

static void record1 (uint64_t ctxt_idx, bool cover_bits,
                     long long grp, long long value) throw ()
{
    get_recorder ()->record1 (ctxt_idx, cover_bits, grp, value);
}

static void recordn (uint64_t ctxt_idx, bool cover_bits,
                     long long grp, const std::string &value) throw ()
{
    get_recorder ()->recordn (ctxt_idx, cover_bits, grp, value);
}

static long long open (const std::string &scope,
                       uint64_t           hash,
                       uint64_t           mod_idx) throw ()
{
    return get_recorder ()->open (scope, hash, mod_idx);
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
    void acov_record1 (long long ctxt_idx, char cover_bits,
                       long long grp, long long value) {
        assert (ctxt_idx >= 0);
        bool is_cb = cover_bits != '\0';
        record1 (ctxt_idx, is_cb, grp, value);
    }

    void acov_record2 (long long ctxt_idx, char cover_bits,
                       long long grp, long long value1, long long value0) {
        assert (ctxt_idx >= 0);
        bool is_cb = cover_bits != '\0';
        recordn (ctxt_idx, is_cb, grp,
                 ll_to_str (value0) + ll_to_str (value1));
    }

    void acov_record3 (long long ctxt_idx,
                       char cover_bits, long long grp,
                       long long value2, long long value1, long long value0) {
        assert (ctxt_idx >= 0);
        bool is_cb = cover_bits != '\0';
        recordn (ctxt_idx, is_cb, grp,
                 ll_to_str (value0) +
                 ll_to_str (value1) + ll_to_str (value2));
    }

    void acov_record4 (long long ctxt_idx,
                       char cover_bits, long long grp,
                       long long value3, long long value2,
                       long long value1, long long value0) {
        assert (ctxt_idx >= 0);
        bool is_cb = cover_bits != '\0';
        recordn (ctxt_idx, is_cb, grp,
                 ll_to_str (value0) + ll_to_str (value1) +
                 ll_to_str (value2) + ll_to_str (value3));
    }

    long long acov_open (long long hash, long long mod_idx) {
        const char *scope = svGetNameFromScope (svGetScope ());
        if (! scope) {
            std::cerr << "Error: acov_open called with no scope.\n";
            abort ();
        }

        assert (hash >= 0);
        assert (mod_idx >= 0);
        return open (scope, hash, mod_idx);
    }

    void acov_close () {
        close ();
    }

    void acov_clear () {
        clear ();
    }
}
