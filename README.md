![Project Status: Abandoned – Initial development has started, but there
has not yet been a stable, usable release; the project has been
abandoned and the author(s) do not intend on continuing
development.](http://www.repostatus.org/badges/latest/abandoned.svg) This library is [**abandonned**](https://tpapp.github.io/post/orphaned-lisp-libraries/).

Latex-table is a simple library for writing vectors and matrices into
LaTeX tables.  Just use labeled-vector-(horizontal|vertical) or
labeled-matrix to write out vectors, and labeled-matrix for matrices.
You can specify the number of significant digits (either as a single
value for all columns, or for each column separately) and the
horizontal/vertical lines.  For the latter, you can use position-type
pairs, eg

:hlines '(0 2 -1 1))

will put a double line on the left hand side of the table, and a
single one on the right - negative numbers count from the right.

The macro with-table puts a table environment around your table.

This version of the package uses David Carlisle's dcolumn LaTeX
package for alignment on the decimal dot.

You can use raw-tabular to access the underlying function that
actually generates all the tables, but then you have to preprocess
your output yourself (read the docstring, or look at the examples).

Examples are in the examples/ directory.

Tamas Papp, tkpapp@gmail.com
