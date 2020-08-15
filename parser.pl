:- use_module(library(dcgs)).

/* from Bratko chapter 17 page 455.
   This comes from Pereira and Warren paper, AI journal, 1980 */

/* this is to define infix operators  and their argument precedence 
   x represents an argument whose precedence is strictly lower than that
   of the operator. y represents an argument whose precedence is lower 
   or equal than that of the operator. */
:- op(100, xfy, and).
:- op(150, xfx, '=>').
:- op(160, xfx, is_a).
/* when using sentence we need to pass 3 arguments, 
   the first will match S in the head of the DGC clause
   the second is the list containing the words in the sentence
   the third is the empty list.
   Example:
     sentence(Meaning, [every,man,that,paints,likes,monet],[]) */

sentence(Meaning) --> 
	noun_phrase(X, SubjState, Assn, Meaning),
    verb_phrase(X, SubjState, Assn).

% sentence(Meaning1 and Meaning2) -->
%     sentence(Meaning1), conjunction, sentence(Meaning2).

% conjunction --> [and] | [but].

% Example state.
% State = state(person(3rd), number(singular))

noun_phrase(X, SubjState, Assn, Meaning) --> 
	determiner(X, SubjState, Prop12, Assn, Meaning),
    noun(X, SubjState, Prop1),
    rel_clause(X, SubjState, Prop1, Prop12).
noun_phrase(X, plural, Assn, exists(X, Prop and plural(X) and Assn)) -->
    noun(X, plural, Prop).
noun_phrase(X, _, Assn, Assn) --> proper_noun(X).
noun_phrase(X, SubjState, Assn, Meaning) -->
    indefinite_pronoun(X, SubjState, Assn, Prop1),
    rel_clause(X, SubjState, Prop1, Meaning).
noun_phrase(X, _, Assn, Meaning) --> pronoun(X, Assn, Meaning).

verb_phrase(X, SubjState, Assn) --> trans_verb(X, SubjState, Y, Assn1), noun_phrase(Y, _SubjState2, Assn1, Assn).
verb_phrase(X, SubjState, Assn) --> intrans_verb(X, SubjState, Assn).

connective --> [that] | [who] | [which].
rel_clause(X, SubjState, Prop1, Prop1 and Prop2) --> connective, verb_phrase(X, SubjState, Prop2).
rel_clause(_, _SubjState, Prop1, Prop1) --> [].

% DETERMINERS
determiner(X, SubjState, Prop, Assn, forall(X, (Prop => Assn))) --> [every],
    { state_has_number(SubjState, singular) }.
determiner(X, SubjState, Prop, Assn, forall(X, (Prop => Assn))) --> [all],
    { state_has_number(SubjState, plural) }.

determiner(X, SubjState, Prop, Assn, exists(X, Prop and Assn)) --> [a],
    { state_has_number(SubjState, singular) }.
determiner(X, SubjState, Prop, Assn, exists(X, Prop and Assn)) --> [some],
    { state_has_number(SubjState, plural) }.

determiner(X, _SubjState, Prop, Assn, locally_unique(X, Prop) and Assn) --> [the].

% INDEFINITE PRONOUNS
indefinite_pronoun(X, SubjState, Assn, exists(X, X is_a person and Assn)) --> { s_verb_state(SubjState) }, ([somebody] | [someone]).
indefinite_pronoun(X, SubjState, Assn, forall(X, ((X is_a person) => Assn))) --> { s_verb_state(SubjState) }, ([everybody] | [everyone]).

% NOUNS
noun_singular(person).
noun_singular(thing).
noun_singular(woman).
noun_singular(dog).
noun_singular(man).
noun_singular(book).

pluralization(person, people).
pluralization(woman, women).
pluralization(man, men).
pluralization(Class, Noun) :- noun_singular(Class), atom_concat(Class, s, Noun).

state_has_number(state(_, number(Number)), Number).
state_has_person(state(person(Person), _), Person).

noun(X, State, X is_a Noun) --> [Noun],
    { state_has_number(State, singular), noun_singular(Noun) }.
noun(X, State, X is_a Class) --> [Plural],
    { state_has_number(State, plural), pluralization(Singular, Plural), Class = Singular }.

% PRONOUNS
pronoun(X, Prop, (Prop, speaker(X))) --> [i] | [me].
pronoun(X, Prop, (Prop, speakee(X))) --> [you].
pronoun(X, Prop, (Prop, locally_unique(X, gender(X, nonbinary), singular(X)))) --> [them] | [they].
pronoun(X, Prop, locally_unique(X, multiple(X)) and Prop) --> [them] | [they].
pronoun(X, Prop, locally_unique(X, gender(X, feminine)) and Prop) --> [she] | [her].
pronoun(X, Prop, locally_unique(X, gender(X, masculine)) and Prop) --> [he] | [him].
pronoun(X, Prop, locally_unique(X, X is_a object) and Prop) --> [it].

proper_noun(john) --> [john].
proper_noun(annie) --> [annie].
proper_noun(monet) --> [monet].
proper_noun(new_york) --> [new, york].

trans_verb(Subj, SubjState, Obj, likes(Subj, Obj)) --> [likes],
    { s_verb_state(SubjState) }.
trans_verb(Subj, SubjState, Obj, likes(Obj, Subj)) --> [is, liked, by],
    { s_verb_state(SubjState) }.
trans_verb(Subj, SubjState, Obj, likes(Subj, Obj)) --> [like],
    { \+ s_verb_state(SubjState) }.
trans_verb(Subj, SubjState, Obj, likes(Obj, Subj)) --> [are, liked, by],
    { \+ s_verb_state(SubjState) }.
trans_verb(Subj, SubjState, Obj, not(likes(Subj, Obj))) --> [does, not, like],
    { s_verb_state(SubjState) }.
trans_verb(Subj, SubjState, Obj, not(likes(Subj, Obj))) --> [do, not, like],
    { \+ s_verb_state(SubjState) }.

trans_verb(Subj, SubjState, Obj, admires(Subj, Obj)) --> [admires],
    { s_verb_state(SubjState) }.
trans_verb(Subj, SubjState, Obj, admires(Subj, Obj)) --> [admire],
    { \+ s_verb_state(SubjState) }.

intrans_verb(Subj, singular, paints(Subj)) --> [paints].
intrans_verb(Subj, plural, paints(Subj)) --> [paint].
intrans_verb(Subj, singular, sleeps(Subj)) --> [sleeps].
intrans_verb(Subj, plural, sleeps(Subj)) --> [sleep].

% SPEC(I, you, he/she/it/the dog, we, yall, they/the dogs).
% SPEC('', '', s,                 '', '',   '').
% verb1(3rd-singular, non-3rd-singular).
verb1(paints, paint).
verb1(sleeps, sleep).
verb1(eats, eat).

s_verb_state(state(person(3), number(singular))).

intrans_verb(Subj, SubjState, Term) --> [Word],
    {
        s_verb_state(SubjState),
        verb1(SVerb, _NonSVerb),
        Word = SVerb,
        Head = SVerb,
        Term =.. [Head, Subj]
    }.

intrans_verb(Subj, SubjState, Term) --> [Word],
    {
        \+ s_verb_state(SubjState),
        verb1(SVerb, NonSVerb),
        Word = NonSVerb,
        Head = SVerb,
        Term =.. [Head, Subj]
    }.
    

/* examples */
/*
?- sentence(S,[every,man,that,paints,likes,monet],[]).
?- sentence(S,[a,woman,that,admires,john,paints],[]).
?- sentence(S,[every,woman,that,likes,a,man,that,admires,monet,paints],[]).
?- sentence(S,[john,likes,annie],[]).
?- sentence(S,[annie,likes,a,man,that,admires,monet],[]).
*/
