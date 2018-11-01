%token <string> ID
%token COLON
%token COMMA
%token WAIT
%token GO
%token EOF

%start <Labeled_dfa.t> dfa
%%

dfa:
  | s = states; EOF { s }
  ;

states:
  | st = rev_states { List.rev st }
  ;

rev_states:
  | (* empty *) { [] }
  | st = rev_states; name = ID; COLON; action = action; COMMA; go = ID; COMMA; wait = ID
    { Labeled_dfa.State.create ~name ~action ~go ~wait :: st }
  ;

action:
  | WAIT { `Wait }
  | GO   { `Go   }
  ;
