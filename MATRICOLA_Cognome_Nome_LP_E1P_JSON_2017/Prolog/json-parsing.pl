%%%% -*- Mode: Prolog -*-
%
% Progetto LP E1P: JSON Parsing.


% - Parse a string into a prolog term
% ---

% Predicate which transforms a string into a term
json_parse(Json, Content) :-
    nonvar(Json),
    !,
    atom_chars(Json, JsonChars),
    phrase(parse_json(Content), JsonChars).

% Predicate which transfb
json_parse(Json, Content) :-
    nonvar(Content),
    !,
    json_reverse(Content, Json).

% O is an object {members} or array [elements]
parse_json(Content) -->
    parse_object(Content),!;
    parse_array(Content).

% Object with members: '{' Members '}'
parse_object(json_obj(Object)) -->
    whitespace,
    ['{'],
    parse_object_content(Object),
    !,
    ['}'],
    whitespace.

% Empty object: '{}'
parse_object(json_obj([])) -->
    whitespace,
    ['{'],
    whitespace,
    ['}'],
    whitespace.

% Array with elements: '[' Elements ']'
parse_array(json_array(Array)) -->
    whitespace,
    ['['],
    whitespace,
    parse_values(Array),
    !,
    whitespace,
    [']'],
    whitespace.

% Empty array: '[]'
parse_array(json_array([])) -->
    whitespace,
    ['['],
    whitespace,
    [']'],
    whitespace.

% Parse all values in array
parse_values([Value|Values]) -->
    parse_value(Value),
    whitespace,
    [','],
    !,
    whitespace,
    parse_values(Values).

parse_values([Value]) -->
    parse_value(Value).

% Parse all members in object
parse_object_content(Members) -->
    parse_members(Members).

parse_object_content([]) -->
    !.

% Members are a list of pair: Pair ',' Members
parse_members([Pair|OtherPairs]) -->
    parse_keyvalue(Pair),
    [','],
    !,
    parse_members(OtherPairs).

parse_members([Pair]) -->
    parse_keyvalue(Pair).

parse_keyvalue((Key,Value)) -->
    whitespace,
    parse_key(Key),
    whitespace,
    [':'],
    whitespace,
    parse_value(Value),
    whitespace.
% Key is a parsed string
parse_key(Key) -->
    parse_string(Key).

% Value can be a string or a number or a JSON term (Object or Array)
parse_value(Value) -->
    parse_string(Value),
    !.

parse_value(Value) -->
    parse_number(Value),
    !.

parse_value(Value) -->
    parse_object(Value),
    !.

parse_value(Value) -->
    parse_array(Value),
    !.

parse_number(Value) -->
    parse_number_atom(Value),
    !.

% Parse number
parse_number_atom(Value) -->
    parse_digits(Digits),
    {
        atomic_list_concat(Digits, Atom),
        atom_number(Atom, Value)
    }.

parse_digits([Digit|Digits]) -->
    parse_digit(Digit),
    !,
    parse_digits(Digits).

parse_digits([]) --> [].

% A number is made by valid digits checked using valid_digit
parse_digit(Digit) -->
    [Digit],
    { valid_digit(Digit) }.

% A valid digit is made by 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
valid_digit(Digit) :-
    member(Digit, ['0','1','2','3','4','5','6','7','8','9','0','.']).

% Parse a string
parse_string("") -->
    ['"'],
    ['"'],
    !.

parse_string(Chars) -->
    ['"'],
    parse_chars_atom(Chars),
    ['"'],
    !.

parse_string("") -->
    ['\''],
    ['\''],
    !.

parse_string(Chars) -->
    ['\''],
    parse_chars_atom(Chars),
    ['\''],
    !.

% A string is made by valid chars checked using valid_char
parse_chars_atom(String) -->
    parse_chars(Chars),
    {
        atom_to_chars(Atom, Chars),
        atom_string(Atom,String)
    }.

parse_chars([Char|Chars]) -->
    parse_char(Char),
    parse_chars(Chars).

parse_chars([Char]) -->
    parse_char(Char).

parse_char(Char) -->
    [Char],
    {atom(Char)},
    {valid_char(Char)}.

% A valid char is different from " ' " or ' " '
valid_char(Char) :-
    not(Char = '\''),
    not(Char = '"').

% Used for ignore whitespaces, tab and newline
whitespace -->
    whitespace_char,
    !,
    whitespace.

whitespace --> [].

% Check if a char is a space
whitespace_char -->
    [Char],
    { char_type(Char, space) }.

% Check if a char is a tab
whitespace_char -->
    [Char],
    { member(Char, "\t") }.

% Check if a char is a newline
whitespace_char -->
    [Char],
    { member(Char, "\n") }.

% ---


% - Read from file and write to file
% ---

json_load(Path, JSON) :-
    exists_file(Path),
    open(Path, read, Stream),
    read_file(Stream, Output),
    close(Stream),
    atomic_list_concat(Output, Mystring),
    json_parse(Mystring, JSON).

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

% ---


% - Reverse from prolog term to string
% ---

json_reverse(json_obj([]), "{}") :-
    !.

json_reverse(json_obj([Y | Ys]), Result) :-
    !,
    json_reverse_object([Y | Ys], "", Phase1),
    concat("{", Phase1, Phase2),
    concat(Phase2, "}", Result).

json_reverse(json_array([]), "[]") :-
    !.

json_reverse(json_array([Y | Ys]), Result) :-
    !,
    json_reverse_array([Y | Ys], "", Phase1),
    concat("[", Phase1, Phase2),
    concat(Phase2, "]", Result).

json_reverse_object([X| Xs], JSONString, Result) :-
    json_reverse_element(X, Result1),
    string_concat(JSONString, Result1, Phase1),
    string_concat(Phase1, ",", Phase2),
    json_reverse_object(Xs, Phase2, Result).

json_reverse_object([], JSONString, Result) :-
    string_concat(Result, ",", JSONString),
    !.

json_reverse_array([], JSONString, Result) :-
    !,
    string_concat(Result, ",", JSONString).

json_reverse_array([X| Xs], JSONString, Result) :-
    json_reverse_element(X, Result1),
    string_concat(JSONString, Result1, Phase1),
    string_concat(Phase1, ",", Phase2),
    json_reverse_array(Xs, Phase2, Result).

json_reverse_element(X, X) :-
    number(X),
    !.

json_reverse_element(X, Result) :-
    json_reverse(X, Result),
    !.

json_reverse_element(X, Result) :-
    atom(X),
    !,
    atom_string(X, Result).

json_reverse_element(X, Result) :-
    string(X),
    !,
    string_concat("\"", X, Phase1),
    string_concat(Phase1, "\"", Result).

json_reverse_element((X,Y), Result) :-
    string(X),
    !,
    string_concat("\"", X, Phase1),
    string_concat(Phase1, "\"", Phase2),
    string_concat(Phase2, ":", Phase3),
    json_reverse_element(Y, Pair),
    string_concat(Phase3, Pair, Result).

% ---


% - Get a string, number, object or array from
% - a json_obj or json_array
% ---

json_get(json_array([X | Xs]), [Q | Qs], Result) :-
    !,
    json_get(json_array([X | Xs]), Q, Stage),
    json_get(Stage, Qs, Result).

json_get(json_obj([X | Xs]), [Q | Qs], Result) :-
    !,
    json_get(json_obj([X | Xs]), Q, Stage),
    json_get(Stage, Qs, Result).

json_get(X, [], X) :- !.

json_get(json_obj([]), _, _) :- !.

json_get(json_obj([X | Xs]), Query, Result) :-
    !,
    json_get_object([X | Xs], Query, Result).

json_get(json_array([]), _, _) :- !.

json_get(json_array([X | Xs]), Query, Result) :-
    json_get_array([X | Xs], Query, Result),
    !.

json_get_object([X | _], Query, Result) :-
    string(Query),
    X = (Key,Data),
    Query=Key,
    !,
    Result = Data.

json_get_object([X | _], Query, Result) :-
    atom(Query),
    atom_string(Query, String),
    X = (Key,Data),
    String=Key,
    !,
    Result = Data.

json_get_object([_ | Xs], Query, Result) :-
    json_get_object(Xs, Query, Result),
    !.

json_get_object([], _, _) :-
    fail.

json_get_array([X | _], 0, X).

json_get_array([_ | Xs], Index, Result) :-
    number(Index),
    !,
    Next is Index - 1,
    json_get_array(Xs, Next, Result).

% ---
