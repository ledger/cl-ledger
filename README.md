# Ledger in Common Lisp

This is a very brief README, since the project is abandoned.

This document assumes the use of SBCL.

## Installation

The first thing to do is install the prerequisites.  You may do this quite
easily using asdf-install (note that if Hunchentoot gives you compilation
error on the file port-sbcl, just select the restart for ACCEPT):

    $ sbcl
    * (require 'asdf-install)
    * (asdf-install:install :cl-ppcre)
    * (asdf-install:install :local-time)
    * (asdf-install:install :series)
    * (quit)

Now you need to pull all of the projects relating to Ledger:

    $ git submodule init
    $ git submodule update

The next step is to configure SBCL so it knows where to load ledger from.
Create a file called ~/.sbclrc and add these contents:

    ;; -*- lisp -*-
    
    (mapc 'require '(asdf asdf-install))
    
    (push "/path/to/cl-ledger/" asdf:*central-registry*)

    (dolist (project '("red-black/" "cambl/" "periods/"))
      (push (merge-pathnames project "/path/to/cl-ledger/")
            asdf:*central-registry*))

Make sure you change /path/to/cl-ledger/ to be the directory where CL-Ledger
lives.  Also, be sure this pathname ends with a slash!  In CL, directory names
always end with/

Now you can run Ledger at the REPL like this:

    $ sbcl
    * (require 'asdf)
    * (asdf:oos 'asdf:load-op :cl-ledger)

This compiles and loads the Ledger core, and also the textual parser package,
for parsing standard Ledger text files (other parser packages soon to come).


## Basic commands

Once in the REPL, try out this command:

    * (ledger:register-report "/path/to/cl-ledger/doc/sample.dat")

You should see a register printed representing the contents of sample.dat.
You can constrain this report using keyword modifiers:

    * (ledger:register-report "/path/to/cl-ledger/doc/sample.dat"
                              :account "books")

Ledger only reads the file past on the first run, and if it changes, so feel
free to repeat the same command several times even for large journal filse.

The following reports are supported:

    * (ledger:register-report "/path/to/cl-ledger/doc/sample.dat" [OPTIONS])
    * (ledger:balance-report "/path/to/cl-ledger/doc/sample.dat" [OPTIONS])
    * (ledger:print-report "/path/to/cl-ledger/doc/sample.dat" [OPTIONS])
    * (ledger:equity-report "/path/to/cl-ledger/doc/sample.dat" [OPTIONS])

    * (ledger:derive-entry "/path/to/cl-ledger/doc/sample.dat" [OPTIONS])

As for OPTIONS, any of the following keyword pairs is allowed.  There are some
extra options allowed for `derive-entry', for which please see below.

    :account "REGEXP"  :not-account "REGEXP"
    :payee "REGEXP"    :not-payee "REGEXP"
    :note "REGEXP"     :not-note "REGEXP"
  
    :begin "YYYY/MM/DD"
    :end "YYYY/MM/DD"
  
    :range "RANGE EXPRESSION"    ; a range expression, like "this month"
    :period "PERIOD EXPRESSION"  ; like "every month this year"
  
    :expr "VALUE-EXPR"           ; most Ledger 2.x value exprs allowed
    :limit "VALUE-EXPR"          ; the same as 2.x's --limit or -l
                                 ; this is a convenience alias for :expr
    :only "VALUE-EXPR"           ; the same as 2.x's --only
    :display "VALUE-EXPR"        ; the same as 2.x's -d or --display

    :status KEYWORD              ; only report transactions whose status
                                 ; is :CLEARED, :PENDING or :UNCLEARED

    :sort "VALUE-EXPR"           ; sort based on VALUE-EXPR calculation
  
    :no-total BOOL               ; don't show totals
    :collapse BOOL               ; collapse multiline entries
    :subtotal BOOL               ; group all transactions by account
    :invert BOOL                 ; negate all transaction values
                                 ; (same as saying :amount "-a")
    :related BOOL                ; show "other" transactions of each entry

    :lots BOOL                   ; show all commodity lot information
    :lot-prices BOOL             ; show commodity lot prices
    :lot-dates BOOL              ; show commodity lot dates
    :lot-tags BOOL               ; show commodity lot tags

    :amount "VALUE-EXPR"         ; use EXPR to display transaction amounts
    :total "VALUE-EXPR"          ; use EXPR to display the running total

    :set-amount "VALUE-EXPR"     ; instead of :amount, actually represent
                                 ; the amount using EXPR (this is rarely
                                 ; something you want to do)
    :set-total "VALUE-EXPR"      ; same for the running total

    :bridge-totals BOOL          ; if the running totals are not contiguous
                                 ; create revaluation entries to fill gaps

    :show OUTPUT-MODE            ; show amounts and totals using the given mode
    :show :market                ; .. in terms of their market value
    :show :basis                 ; .. in terms of their basis cost

Here's a quick table for translating Ledger 2.6.1 options into their
corresponding CL-Ledger keywords:

    Short option    Long option        CL-Ledger keyword
    ------------ -------------------- ---------------------
     -b ARG       --begin ARG          :begin ARG
     -e ARG       --end ARG            :end ARG
     -p ARG       --period ARG         :period ARG
     -l ARG       --limit ARG          :limit ARG
                  --only ARG           :only ARG
     -d ARG       --display ARG        :display ARG
     -n                                :no-total t     <-- for balances
     -n           --collapse           :collapse t
     -r           --related            :related t
     -s           --subtotal           :subtotal t
     -S EXPR      --sort ARG           :sort ARG
                  --sort-entries ARG   :sort-entries ARG
     -t EXPR

### Options to `derive-entry'

The reporting command `derive-entry' takes some special options.  Here is the
docstring for derive-entry:

The DERIVE-ENTRY report uses Ledger to intelligently create a new entry for
you.  The possible keywords arguments are:

    :DATE             <DATE-STRING>
    :PAYEE            <REGEXP>
    :ACCOUNT          <REGEXP>
    :BALANCE-ACCOUNT  <REGEXP>
    :AMOUNT           <VALUE-STRING>
    :APPEND           <BOOLEAN>

Except for :PAYEE, all of these keyword arguments are optional.  Here is what
they mean:

  :PAYEE REGEXP
    Find the most recent entry whose payee matches REGEXP, and base the new
    entry derivation on its details.  If no matching entry can be found, the
    payee of the newly created entry will exactly match REGEXP.

  :DATE DATE-STRING
    The date of the new entry will be DATE-STRING, otherwise it is today.

  :ACCOUNT REGEXP
    Set the first account line in the new entry to be the most recently used
    account which matches REGEXP.  If no such account can be found, an account
    named REGEXP is used.  If no account is specified, the account
    "Expenses:Unknown" is used.

  :BALANCE-ACCOUNT REGEXP
    Like :ACCOUNT, except this refers to the account used for the second
    transaction in the newly derived entry.  If not specified, a calculated
    "balance account" is looked for in the matching entry; if this does not
    apply, the journal's default account is used; if this does not apply, the
    account "Asets:Unknown" is used.

  :AMOUNT VALUE-STRING
    The amount of the first transaction.  If it has no commodity, the
    correlated commodity from the discovered entry is used.

  :APPEND BOOLEAN
    If non-NIL, the new entry is written to the same journal where the
    matching entry was found (for a binder that references many journals, this
    is whichever file the discovered entry was in).

  Here are a few examples, using sample.dat as a reference:

  (ledger:derive-entry "doc/sample.dat" :payee "book")
    =>
    2007/12/04 Book Store
        Expenses:Books                            $20.00
        Liabilities:MasterCard

  (ledger:derive-entry :payee "book" :amount "$125")
    =>
    2007/12/04 Book Store
        Expenses:Books                           $125.00
        Liabilities:MasterCard

  (ledger:derive-entry :payee "Hello World")
    =>
    2007/12/04 Hello World
        Expenses:Unknown
        Assets:Unknown

  (ledger:derive-entry :date "2004/01/01" :payee "Hello World")
    =>
    2004/01/01 Hello World
        Expenses:Unknown
        Assets:Unknown

  (ledger:derive-entry :payee "book" :account "equ")
    =>
    2007/12/04 Book Store
        Equity:Opening Balances                   $20.00
        Liabilities:MasterCard

  (ledger:derive-entry :payee "book" :account "Who Knows")
    =>
    2007/12/04 Book Store
        Who Knows                                 $20.00
        Liabilities:MasterCard

  (ledger:derive-entry :payee "book" :balance-account "bank")
    =>
    2007/12/04 Book Store
        Expenses:Books                            $20.00
        Assets:Bank:Checking

  (ledger:derive-entry :payee "book" :account "liab" 
                       :balance-account "bank")
    =>
    2007/12/04 Book Store
        Liabilities:MasterCard                   $-20.00
        Assets:Bank:Checking

  (ledger:derive-entry :payee "book" :account "bank" :amount 50)
    =>
    2007/12/04 Book Store
        Assets:Bank:Checking                      $50.00
        Liabilities:MasterCard

  (ledger:derive-entry :payee "book" :account "bank" :amount "$125")
    =>
    2007/12/04 Book Store
        Assets:Bank:Checking                     $125.00
        Liabilities:MasterCard

### Binder caching

After the call to `read-binder', the variable `*last-binder*' contains the
contents of what was read.  From that point forward, if no binder or string is
passed to the reporting function, they will assume you wish to report on the
contents of `*last-binder*'.


## Implementations status

Here is how Ledger stands up against current Lisp implementations, in order of
how frequently I use them:

  SBCL         1.0.12.17       WORKS
  LispWorks    5.02 Personal   WORKS
  Allegro CL   8.1 Express     Fails to compile CL-FAD (where is :OSI?)
  Clozure CL   2007-12-07      Fails to compile LOCAL-TIME
  OpenMCL      2007-07-22      Fails to compile LOCAL-TIME
  ECL          2007-12-07      Fails to compile SERIES
  ABCL         0.0.10          Fails to compile SERIES
  CLISP        2.43            Fails to compile RED-BLACK
  CMUCL        19d (2007-11)   Fails to compile PERIODS
  GCL          2.6.7           <unable to build so far>


## In closing

That's it for now, expect more in this file as things progress!

John Wiegley
2007/11/17

p.s. For fans of the SERIES library, you can apply `scan-transactions' or
     `scan-entries' to a binder/account/journal/entry in order to produce
     a SERIES of the corresponding type.  Example:

       (collect (ledger:scan-transactions
       		 (ledger:read-journal "doc/sample.dat")))
	  => [a list of all transactions, in sequence, within sample.dat]
