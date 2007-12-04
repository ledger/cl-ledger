;; entry.lisp

(declaim (optimize (safety 3) (debug 3) (speed 1) (space 0)))

(in-package :ledger)

(defun derive-entry (&rest args)
  "The entry command requires at least one argument, so Ledger can intelligently
create a new entry for you.  The possible arguments are:

  DATE  PAYEE  [ACCOUNT] [AMOUNT] [DRAW ACCOUNT]

  Some things to note:

  - The ACCOUNT is optional; if no account is given, the last account affected
    by PAYEE is used.  If no payee can be found, the generic account
    'Expenses' is used.

  - The AMOUNT is optional; if not specified, the same amount is used as the
    last time PAYEE was seen, or 0 if not applicable.

  - The AMOUNT does not require a commodity; if none is given, the commodity
    currently contained within ACCOUNT is used, or no commodity at all if
    either: the ACCOUNT was not found, or it contains more than one commodity.

  - Lastly, the DRAW ACCOUNT is optional; if not present, the last account
    drawn from by PAYEE is used, or the 'basket' account (specified with 'A
    ACCOUNT' in your Ledger file) if that does not apply, or the generic
    account 'Equity' is used.

  Here are a few examples, all of which may be equivalent depending on your
Ledger data:

  (ledger:derive-entry \"3/25\" \"chevron\")
  (ledger:derive-entry \"3/25\" \"chevron\" \"20\")
  (ledger:derive-entry \"3/25\" \"chevron\" \"$20\")
  (ledger:derive-entry \"3/25\" \"chevron\" \"gas\" \"20\")
  (ledger:derive-entry \"3/25\" \"chevron\" \"gas\" \"$20\" \"checking\")

  A final note: Ledger never modifies your data!  You are responsible for
appending the output of this command to your Ledger file if you choose.")

(provide 'entry)

;; entry.lisp ends here
