# ACov: A functional coverage generator

When designing hardware, one coverage metric that matters is
*functional coverage*. This is defined by hand and says things like
"in my testing, I want this FIFO to have been full at least once" or
"I want to test every size of input image that I support" or similar.

SystemVerilog has support for functional coverage in two flavours (Of
course there are two flavours, because why have just one? You'll find
them just behind the kitchen sink...). The flavour that ACov targets
or emulates is based on the `coverpoint` and `covergroup` keywords.
Unfortunately, fully supporting SystemVerilog-style functional
coverage is very complicated and simulators like Verilator can't do
this. If you want to collect functional coverage using Verilator
(going wide across some compute cluster to run loads of tests at
once), you need another approach. For this, ACov has your back

The ultra-condensed description of how to use it is:

  1. Run `acov` on a script describing your coverage points and crosses.
  2. Bind the generated modules to your design.
  3. Run some simulations
  4. Collect the results with `acov-report`.

In slightly more detail, the initial script is written in a sort of
pseudo-Verilog. This defines several modules, each of which will be
turned into an actual Verilog module by running `acov`. The code in these
auto-generated modules contains DPI calls to a library (also included
with ACov) called `libacovdpi.so`. These calls basically say "Hey,
I've just seen the following coverage group with the following value".
When the simulation finishes, the results are written out to a text
file.

Once you've run all your simulations, you run `acov-report`. This
reads the text files that the DPI library generated and assembles them
into an HTML coverage report.

## Example input file

An ACov input file might look like this:

    // A module to collect xxx coverage
    module xxx (foo [10:0], bar [19:0], baz [1:0], qux) {
       when (foo [0]) {
         group {
           record foo cover {0..12, 2047};
           record foo + bar[10:0] as foobar cover {0, 1};
         }
       }

       record baz;
       record qux as qxx;
       record bar cover bits;
    }

Where the inputs `foo`, `bar`, etc. correspond to internal or external 
signals of the module `xxx` that you want to cover.

This will create a Verilog module called `xxx_coverage`, of the
following form:

    module xxx_coverage (input wire        clk,
                         input wire        rst_n,
                         input wire [10:0] foo,
                         input wire [19:0] bar,
                         input wire [1:0]  baz,
                         input wire        qux);

        // Contents go here

    endmodule

Notice that there is a clock and a negative reset line. ACov currently
only supports synchronous modules (with a single clock) that use
asynchronous negative reset.

The `when` block means that we will only record the stuff inside when
its guard is true. Record lines tell the simulation to record their
contents. The full syntax appears in the second record line:

    record foo + bar[10:0] as foobar cover {0, 1};

This tells the simulation to record the expression `foo + bar[10:0]`
and call it `foobar`. It also says that we'd like to see it take
values zero and one.

The first, third and fifth record lines show that you don't need to
specify a record name if the expression being recorded is just a
symbol. You do need to specify it otherwise (because ACov doesn't know
what to call it in this case).

The `cover` keyword tells ACov what we expect to see. If it is
omitted, that means that we would like to see every value allowed by
the width of the expression being recorded. If `cover` is specified,
there are two options for its value. The first is of this form:

    cover {0..12, 2047}

This says that we'd like to see all values from 0 to 12 (inclusive)
and also 2047. The second option is

    cover bits

This means something a bit analogous to toggle coverage. We'd like to
see each bit of the expression equal to zero and equal to one.

The `group` block is analogous to a SystemVerilog `covergroup`. It
tells the simulation to record all the record statements inside it at
the same time. It also implies that we want to cross the records
inside. The group in the example above defines a cross with 14 * 2 =
28 entries.

## Running ACov

Suppose you have written your coverage points in a file called
`input.acov`. Now run ACov with a command line like

    acov input.acov cover_dir

The `cover_dir` directory will be created if necessary and will
contain one file for each module described in `input.acov`. If you had
a module of the form

    module xxx (input [7:0]) {
      ...
    }

then ACov will create a file called `xxx_coverage.sv`. This file
contains SystemVerilog code that defines a module called
`xxx_coverage` which will collect coverage data.

## Binding coverage modules into your design

In practice, you will use the generated modules through the
SystemVerilog `bind` keyword. If you have modules in your design
called `xxx`, `yyy` and `zzz` and you want to bind modules that had
the same name in your `input.acov`, you can write a module of this
form:

    module coverage_bindings;
      bind xxx xxx_coverage xxx_cov_i (.*);
      bind yyy yyy_coverage yyy_cov_i (.*);
      bind zzz zzz_coverage zzz_cov_i (.*);
    endmodule

You need to write this by hand, because ACov has no way to know
exactly how you want to bind things in your design. In practice, this
is not much typing.

If there are two instances of the `xxx` module in the design, the bind
statement above will instantiate two copies of `xxx_coverage`: one
bound to each `xxx` instance. These can be disambiguated by their
scope (which might look something like
"`Top.tb_i.read_i.xxx_i.xxx_cov_i`" and
"`Top.tb_i.write_i.xxx_i.xxx_cov_i`"). The coverage report shows
coverage for each record in the module for each instance.

There is at least one other change you need to make: You need to
instantiate the coverage bindings in your system testbench. You'll
probably want to surround this with a preprocessor ``ifdef` to allow
you to turn them on and off:

    `ifdef FUNC_COV
      coverage_bindings bindings_i ();
    `endif

If your testbench resets after the start of time and you want to
ignore all coverage that happens before then, you'll need a call to
`acov_clear`. For example, if you have a signal `rst_everything_n`
that goes low at the first full reset then you'll need something like
this:

    `ifdef FUNC_COV
    `ifndef ACOV_SV
      import "DPI-C" context function void acov_clear ();
      always @(negedge rst_everything_n) begin
        acov_clear ();
      end
    `endif
    `endif

The inner ``ifndef` means that we only run this in "DPI mode": when
using the DPI library for coverage, rather than the native
SystemVerilog backend.

## Running the simulation

This should be very simple; the only real configuration is telling the
simulator where to find the `libacov.so` library. This is installed at
`$prefix/lib/libacovdpi.so` (or `$prefix/lib32/libacovdpi.so` if you
want the 32-bit version).

If you followed the advice in the previous section, you'll also need
to pass the preprocessor flag `FUNC_COV` to switch coverage on.

If you want to use the SystemVerilog backend because the simulator
supports it, you'll also need to pass the preprocessor flag `ACOV_SV`.

If you run your testbench multiple times with different inputs, you
need to make sure they run in separate directories because the DPI
code writes to `./acov.log` each time.

## Collecting results

To collect the results from the simulation, there is a program called
`acov-report`. This reads each `acov.log`, together with the original
input file, and generates an HTML report. Example usage:

    find test_runs -name 'acov.log' | acov-report input.acov html_dir

This generates `html_dir/index.html` with a coverage report, ready for
viewing.

To catch silly mistakes where you edit `input.acov` and then run
`acov-report` before re-running the tests, each log contains a hash
code that depends on the contents of `input.acov`. If the hash code
doesn't match, you'll get a warning message and `acov-report` will
skip that file.

## Input language reference

The ACov input language fundamentally lists modules (corresponding to
Verilog modules that you want to instrument). Each module contains
groups, which correspond to crosses in the coverage. Each group
contains record statements, which tell ACov what signal to record, and
what values of the signal you expect to see.

Single-line comments are introduced with `//` (like C++).

### Modules

A module is declared with the following syntax:

    module foo (a [10:0], b, c [1:0]) {
      // module contents here
      ...
    }

This declares a module called `foo` with three inputs: `a`, `b` and
`c`. The generated SystemVerilog module will have five input ports:
`clk`, `rst_n` and the three specified in the `module` form. Multi-bit
ports are specified as shown above: `a` is 11 bits wide, `b` is a
single bit and `c` is two bits wide.

### Records

To record a single signal, use a `record` statement. It has several
optional keywords. The full form looks like this:

    record a + b as sum cover {1,2,3};

The only required argument for `record` is the expression to be
recorded. This is expressed in standard Verilog syntax, but the width
checking is slightly stricter than Verilog: binary operators expect
both arguments to be the same width. As such, `2'd3 + 3'd3` is not
equal to `3'd6` (as it would be in Verilog), but gives an error. To
avoid such errors, pad the signals: `{1'b0, 2'd3} + 3'd3` is equal to
`3'd6` as you'd expect.

The first optional argument is the name under which to record the
expression. If the recorded expression is a plain symbol, you don't
have to supply a name; if the recorded expression is more complicated,
you need one.

    record my_signal;                    // OK. Recorded as "my_signal".
    record 10'd1 + my_signal;            // ERROR: What should I call this?
    record 10'd1 + my_signal as cabbage; // OK. Recorded as "cabbage"

The next optional argument is the set of values you want to see. This
is called a *cover list*. If this is not specified, ACov assumes you
want to see every value the signal can take (`2 ** W` where W is the
width of the signal in bits). There's a sanity check that will moan at
you if you don't specify a cover list for a signal wider than 16 bits,
since you're clearly never going to hit every possible value.

The following cover lists all define the same set of numbers (zero to
ten, inclusive):

    cover {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10}
    cover {0..10}
    cover {0..9, 10}
    cover {10, 0..9}
    cover {0..4, 1..9, 4..10}

Instead of giving a cover list, you can specify `cover bits`. This
means something akin to toggle coverage: we want to see each bit of
the signal equal to zero and equal to one. For a signal of width W,
specifying `cover bits` gives `2 * W` bins.

Further `record` examples:

    record c == 4'd10 as c_ten; // where c is 4-bit
    record d[1:0] as d_LSBs cover {0..2};
    record e[8:6] > 3'd5 && |f as e_thresh cover {1};

### Groups

To specify a cross of multiple signals, use the `group` keyword. For
example:

    group {
      record foo cover {0, 1};
      record bar cover {0, 1};
    }

This specifies a 2 by 2 cross with 4 bins in total.

Record statements in groups are slightly constrained: they cannot use
`cover bits` to specify the expected coverage. This is because you can
hit multiple bins with one record when using `cover bits` and it's a
little unclear what a cross between that and another record would
mean.

### When

Often, you'll only want to record a signal when a particular event
happens. The signal is always recorded on a posedge of `clk` when
`rst_n` is true (which is less flexible than SystemVerilog's
coverpoints), but you can further restrict when the record happens
with the `when` keyword:

    when (start_job) {
      record job_size;
      when (job_size > 4'd10) {
        record tiredness;
      }
    }

This example would only record `job_size` (some configuration
parameter) when the job is started, rather than every cycle. Maybe
there is also a `tiredness` parameter, which only matters when the
`job_size` is bigger than 10. Note that the `when` blocks can be
nested.

### In

If you have two different instances of a module in your design and
bind to them as suggested earlier in the document, you may have a
problem because you expect different coverage points for the different
instances.

To avoid this problem, use `in` blocks:

    in "foo" {
      record a;
      in "bar" {
        record b;
      }
    }

This means that we will only require coverage for the signal `a` when
the instance scope has `"foo"` as a substring. We'll only require
coverage for the signal `b` when the instance scope also has `"bar"`
as a substring.
