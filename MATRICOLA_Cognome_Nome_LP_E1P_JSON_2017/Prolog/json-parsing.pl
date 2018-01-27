%%%% -*- Mode: Prolog -*-

%% Progetto LP E1P: JSON Parsing.


%% - Parse a string into a prolog term
%% ---

%% Predicate which transforms a string into a term
json_parse(Json, Content) :-
    nonvar(Json),
    !,
    atom_chars(Json, JsonChars),
    phrase(parse_json(Content), JsonChars).

%% Predicate which transfb
json_parse(Json, Content) :-
    nonvar(Content),
    !,
    json_reverse(Content, Json).

%% O is an object {members} or array [elements]
parse_json(Content) -->
    parse_object(Content),!;
    parse_array(Content).

%% Object with members: '{' Members '}'
parse_object(json_obj(Object)) -->
    whitespace,
    ['{'],
    parse_object_content(Object),
    !,
    ['}'],
    whitespace.

%% Empty object: '{}'
parse_object(json_obj([])) -->
    whitespace,
    ['{'],
    whitespace,
    ['}'],
    whitespace.

%% Array with elements: '[' Elements ']'
parse_array(json_array(Array)) -->
    whitespace,
    ['['],
    whitespace,
    parse_values(Array),
    !,
    whitespace,
    [']'],
    whitespace.

%% Empty array: '[]'
parse_array(json_array([])) -->
    whitespace,
    ['['],
    whitespace,
    [']'],
    whitespace.

%% Parse all values in array
parse_values([Value|Values]) -->
    parse_value(Value),
    whitespace,
    [','],
    !,
    whitespace,
    parse_values(Values).

%% Parse last value
parse_values([Value]) -->
    parse_value(Value).

%% Parse all members in object
parse_object_content(Members) -->
    parse_members(Members).

parse_object_content([]) -->
    !.

%% Members are a list of pair: Pair ',' Members
parse_members([Pair|OtherPairs]) -->
    parse_keyvalue(Pair),
    [','],
    !,
    parse_members(OtherPairs).

%% Parse a pair
parse_members([Pair]) -->
    parse_keyvalue(Pair).

%% Parse a pair "Key": Value to (Key, Value)
parse_keyvalue((Key,Value)) -->
    whitespace,
    parse_key(Key),
    whitespace,
    [':'],
    whitespace,
    parse_value(Value),
    whitespace.

%% Key is a parsed string
parse_key(Key) -->
    parse_string(Key).

%% Value can be a string or a number or a JSON term (Object or Array)

%% Value is a string
parse_value(Value) -->
    parse_string(Value),
    !.

%% Value is a number
parse_value(Value) -->
    parse_number(Value),
    !.

%% Value is a number
parse_value(Value) -->
    parse_boolean(Value),
    !.

%% Value is an object
parse_value(Value) -->
    parse_object(Value),
    !.

%% Value is an array
parse_value(Value) -->
    parse_array(Value),
    !.

%% Value is an atom
parse_number(Value) -->
    parse_number_atom(Value),
    !.

%% Parse number
parse_number_atom(Value) -->
    parse_digits(Digits),
    {
        atomic_list_concat(Digits, Atom),
        atom_number(Atom, Value)
    }.

%% Pars all digits in a number
parse_digits([Digit|Digits]) -->
    parse_digit(Digit),
    !,
    parse_digits(Digits).

parse_digits([]) --> [].

%% A number is made by valid digits checked using valid_digit
parse_digit(Digit) -->
    [Digit],
    { valid_digit(Digit) }.

%% A valid digit is made by 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
valid_digit(Digit) :-
    member(Digit, ['0','1','2','3','4','5','6','7','8','9','0','.','+','-']).

%% Parse a boolean
parse_boolean(true) -->
    ['t'],
    ['r'],
    ['u'],
    ['e'].

parse_boolean(false) -->
    ['f'],
    ['a'],
    ['l'],
    ['s'],
    ['e'].

valid_boolean(Boolean) :-
    string_lower(Boolean, "true");
    string_lower(Boolean, "false").

%% Parse an empty string contained in ""
parse_string("") -->
    ['"'],
    ['"'],
    !.

%% Parse a string contained in ""
parse_string(Chars) -->
    ['"'],
    parse_chars_atom(Chars),
    ['"'],
    !.

%% Parse an empty string contained in ''
parse_string("") -->
    ['\''],
    ['\''],
    !.

%% Pare a string contained in ''
parse_string(Chars) -->
    ['\''],
    parse_chars_atom(Chars),
    ['\''],
    !.

%% A string is made by valid chars checked using valid_char
parse_chars_atom(String) -->
    parse_chars(Chars),
    {
        atom_to_chars(Atom, Chars),
        atom_string(Atom,String)
    }.

%% Parses all characters
parse_chars([Char|Chars]) -->
    parse_char(Char),
    parse_chars(Chars).

%% Last character
parse_chars([Char]) -->
    parse_char(Char).

%% Check valid character
parse_char(Char) -->
    [Char],
    {atom(Char)},
    {valid_char(Char)}.

%% A valid char is different from " ' " or ' " '
valid_char(Char) :-
    not(Char = '\''),
    not(Char = '"').

%% Used for ignore whitespaces, tab and newline
whitespace -->
    whitespace_char,
    !,
    whitespace.

%% No whitespaces
whitespace --> [].

%% Check if a char is a space
whitespace_char -->
    [Char],
    { char_type(Char, space) }.

%% Check if a char is a tab
whitespace_char -->
    [Char],
    { member(Char, "\t") }.

%% Check if a char is a newline
whitespace_char -->
    [Char],
    { member(Char, "\n") }.

%% ---


%% - Read from file and write to file
%% ---

%% Load a file from file, returns a JSON term
json_load(Path, JSON) :-
    exists_file(Path),
    open(Path, read, Stream),
    read_file(Stream, Output),
    close(Stream),
    atomic_list_concat(Output, Mystring),
    json_parse(Mystring, JSON).

%% Writes a JSON term to a file in Path
json_write(JSON, Path) :-
    open(Path, write, Stream),
    json_parse(Mystring, JSON),
    write(Stream, Mystring),
    nl(Stream),
    close(Stream).

read_file(Stream, []) :-
    at_end_of_stream(Stream).

read_file(Stream,[X|L]) :-
    \+ at_end_of_stream(Stream),
    read_line_to_codes(Stream, Codes),
    atom_chars(X, Codes),
    read_file(Stream, L),
    !.

%% ---


%% - Reverse from prolog term to string
%% ---

%% Reverts an empty object
json_reverse(json_obj([]), "{}") :-
    !.

%% Reverts an object
json_reverse(json_obj([Y | Ys]), Result) :-
    !,
    json_reverse_object([Y | Ys], "", Phase1),
    concat("{", Phase1, Phase2),
    concat(Phase2, "}", Result).

%% Reverts an empty array
json_reverse(json_array([]), "[]") :-
    !.

%% Reverts an array
json_reverse(json_array([Y | Ys]), Result) :-
    !,
    json_reverse_array([Y | Ys], "", Phase1),
    concat("[", Phase1, Phase2),
    concat(Phase2, "]", Result).

%% Reverts a JSON object into a string
json_reverse_object([X| Xs], JSONString, Result) :-
    json_reverse_element(X, Result1),
    string_concat(JSONString, Result1, Phase1),
    string_concat(Phase1, ",", Phase2),
    json_reverse_object(Xs, Phase2, Result).

json_reverse_object([], JSONString, Result) :-
    string_concat(Result, ",", JSONString),
    !.

%% Revers a JSON array into a string
json_reverse_array([], JSONString, Result) :-
    !,
    string_concat(Result, ",", JSONString).

json_reverse_array([X| Xs], JSONString, Result) :-
    json_reverse_element(X, Result1),
    string_concat(JSONString, Result1, Phase1),
    string_concat(Phase1, ",", Phase2),
    json_reverse_array(Xs, Phase2, Result).

%% Revers a number into a string
json_reverse_element(X, X) :-
    number(X),
    !.

%% Reverse boolean into a string
json_reverse_element(true, "true") :- !.

json_reverse_element(false, "false") :- !.

%% Reverts nested objects
json_reverse_element(X, Result) :-
    json_reverse(X, Result),
    !.

%% Reverts a string
json_reverse_element(X, Result) :-
    atom(X),
    !,
    atom_string(X, Result).

%% Revers a string, adding "" to be JSON compliant
json_reverse_element(X, Result) :-
    string(X),
    !,
    string_concat("\"", X, Phase1),
    string_concat(Phase1, "\"", Result).

%% Reverts a JSON object pair into a string
json_reverse_element((X,Y), Result) :-
    string(X),
    !,
    string_concat("\"", X, Phase1),
    string_concat(Phase1, "\"", Phase2),
    string_concat(Phase2, ":", Phase3),
    json_reverse_element(Y, Pair),
    string_concat(Phase3, Pair, Result).

%% ---


%% - Get a string, number, object or array from
%% - a json_obj or json_array
%% ---

%% Search in array with multiple terms
json_get(json_array([X | Xs]), [Q | Qs], Result) :-
    !,
    json_get(json_array([X | Xs]), Q, Stage),
    json_get(Stage, Qs, Result).

%% Search in object with multiple terms
json_get(json_obj([X | Xs]), [Q | Qs], Result) :-
    !,
    json_get(json_obj([X | Xs]), Q, Stage),
    json_get(Stage, Qs, Result).

%% If no queries remaining, return element
json_get(X, [], X) :- !.

%% If object is empty, no query can be found
json_get(json_obj([]), _, _) :-
    fail,
    !.

%% Get element in a JSON object
json_get(json_obj([X | Xs]), Query, Result) :-
    !,
    json_get_object([X | Xs], Query, Result).

%% If array is empty, no query can be found
json_get(json_array([]), _, _) :-
    fail,
    !.

%% Get element in a JSON array
json_get(json_array([X | Xs]), Query, Result) :-
    json_get_array([X | Xs], Query, Result),
    !.

%% When Query is Key predicate is true
json_get_object([(Key,Result) | _], Key, Result) :-
    string(Key),
    !.

%% When Query is Key predicate is true
json_get_object([(Key,Result) | _], Query, Result) :-
    atom(Query),
    atom_string(Query, Key),
    !.

%% Continue search Query with next pair
json_get_object([_ | Xs], Query, Result) :-
    json_get_object(Xs, Query, Result),
    !.

%% If object is empty, we can't get object
json_get_object([], _, _) :-
    fail.

%% If index is 0, Result is current element
json_get_array([X | _], 0, X) :- !.

%% Get element from array with an index
%% Prolog manages fails when index
%% is too big or negative
json_get_array([_ | Xs], Index, Result) :-
    number(Index),
    !,
    Next is Index - 1,
    json_get_array(Xs, Next, Result).

%% ---
