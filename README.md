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
coverage is very complicated and simulators like Verilator don't do
this. If you want to collect functional coverage using Verilator
(going wide across some compute cluster to run 100 tests at once), you
need another approach. For this, ACov has your back

The ultra-condensed description of how to use it is:

  1. Run `acov` on a script describing your coverage points and crosses.
  2. Bind the generated modules to your design.
  3. Run some simulations
  4. Collect the results with `acov-report`.

In slightly more detail, the initial script is written in a sort of
pseudo-Verilog (described later in this document). This defines
several modules, each of which will be turned into an actual Verilog
module running `acov`. The code in these auto-generated modules
contains DPI calls to a library (also included with ACov) called
`libacovdpi.so`. These calls basically say "Hey, I've just seen the
following coverage group with the following value." and when the
simulation finishes the results are written out to a text file.

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

This will create a verilog module called `xxx_coverage`, of the
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
