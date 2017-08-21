LISP Interpreter in Ada 2012

As I was working on various aspects of the Ada 2012 language, I realized that small syntactic changes, typically brushed away by purists as "syntactic sugar" can significantly change the flavor of a programming language. One of the main gripes with the Ada programming language has been its verbosity, rooted in its Pascal heritage. Every function will need, in addition to its declaration, an implementation with `begin` and `end` keywords as well as a `return` statement. A simple function to compute the maximum of two arguments would look like:

    function Max (X, Y : Float) return Float is
    begin
       if Y >= X then
           return Y;
       else
           return X;
       end if;
    end Add;

With _function expressions_ this turns into:

    function Max (X, Y : Float) return Float is (if Y >= X then Y else X);

While this seems like trivial syntactic sugar, for me it amounts to a fundamental change in how I use the language. The low syntactic overhead of introducing functional abstractions greatly encourages its use, and before long you write a dozen one-line function expressions rather than a 100 line procedure with a number of `while` loops and `if` statements. The language now promotes a _functional_ style of programming: no local or global variables, just arguments and return values.

When looking back at the history of functional programming (term used loosely here, you purists), I came across the [LISP 1.5 Programmers Manual](http://www.softwarepreservation.org/projects/LISP/book/LISP%201.5%20Programmers%20Manual.pdf) and couldn't resist using Ada 2012 to implement it. The functional aspects of the language allow the Ada code to be very close to the _M_-expressions in the manual, while the automatic checking allows for very simple error handling. The few hundred lines of code here are just meant as a sketch, not an actual programming environment.

There is no garbage collection or number handling: the goal has been to stay close to the LISP 1.5 definition and keep things simple. Still, I'll welcome additions that fix bugs or implement more LISP 1.5 functionality and do not add too many lines of code. We might want to add support for numbers (`double`, maybe `int64_t` and `decimal128`), as well as `UTF-8` strings as atoms. We'd probably also want to memoize function calls for efficiency.

Ideally, this could serve as (the base of) a simple scripting engine for domain specific languages such as the MongoDB aggregation expressions: users would be able to define new functions for their aggregation expressions, while we'd be able to limit memory usage and execution time, as well as avoid undefined execution.
