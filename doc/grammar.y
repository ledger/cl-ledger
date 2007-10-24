journal: directives_and_entries ;

directives_and_entries:
    directive_or_entry |
    directives_and_entries directive_or_entry ;
directive_or_entry:
    'A' account_name |
    /* ... */
    ;

entry: plain_entry |
       periodic_entry |
       automated_entry ;

plain_entry:
    bol date status_opt code_opt payee note_opt newline transactions ;

periodic_entry:
    bol '~' period_string note_opt newline transactions ;

automated_entry:
    bol '=' expression note_opt newline transactions ;

transactions: transaction | transactions transaction ;
transaction:
    space status_opt account spacer amount price_opt note_opt ;

amount: neg_opt commodity quantity annotation_opt |
        commodity quantity annotation_opt |
        quantity commodity annotation_opt ;

neg_opt: neg | /* epsilon */ ;
neg: '-' ;

annotation_opt: annotation | /* epsilon */ ;
annotation: price_opt date_opt note_opt ;

price_opt: price | /* epsilon */ ;
price: '{' amount '}' ;

date_opt: date | /* epsilon */ ;
date: '[' date ']' ;

note_opt: note | /* epsilon */ ;
note: '(' string ')' ;

amount: neg_opt quantity ;

quantity: BIGNUM ;

spacer: SPACE SPACE | TAB | SPACE TAB ;

account:
    account_name |
    '(' account_name ')' |
    '[' account_name ']' ;
