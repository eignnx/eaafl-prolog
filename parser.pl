:- use_module(library(dcgs)).

/* from Bratko chapter 17 page 455.
   This comes from Pereira and Warren paper, AI journal, 1980 */

/* this is to define infix operators  and their argument precedence 
   x represents an argument whose precedence is strictly lower than that
   of the operator. y represents an argument whose precedence is lower 
   or equal than that of the operator. */
:- op(100, xfy, and).
:- op(150, xfx, '=>').
/* when using sentence we need to pass 3 arguments, 
   the first will match S in the head of the DGC clause
   the second is the list containing the words in the sentence
   the third is the empty list.
   Example:
     sentence(Meaning, [every,man,that,paints,likes,monet],[]) */

sentence(Meaning) --> 
	noun_phrase(X, Plurality, Assn, Meaning), verb_phrase(X, Plurality, Assn).

% sentence(Meaning1 and Meaning2) -->
%     sentence(Meaning1), conjunction, sentence(Meaning2).

% conjunction --> [and] | [but].

noun_phrase(X, Plurality, Assn, Meaning) --> 
	determiner(X, Plurality, Prop12, Assn, Meaning),
    noun(X, Plurality, Prop1),
    rel_clause(X, Prop1, Prop12).
noun_phrase(X, plural, Assn, exists(X, Prop and plural(X) and Assn)) -->
    noun(X, plural, Prop).
noun_phrase(X, _, Assn, Assn) --> proper_noun(X).
noun_phrase(X, Plurality, Assn, Meaning) -->
    indefinite_pronoun(X, Plurality, Assn, Prop1),
    rel_clause(X, Prop1, Meaning).
noun_phrase(X, _, Assn, Meaning) --> pronoun(X, Assn, Meaning).

verb_phrase(X, _Plurality, Assn) --> trans_verb(X, Y, Assn1), noun_phrase(Y, _Plurality2, Assn1, Assn).
verb_phrase(X, singular, Assn) --> intrans_verb(X, sing_3rd, Assn).
verb_phrase(X, plural, Assn) --> intrans_verb(X, non_sing_3rd, Assn).

connective --> [that] | [who] | [which].
rel_clause(X, Prop1, Prop1 and Prop2) --> connective, verb_phrase(X, _Plurality, Prop2).
rel_clause(_, Prop1, Prop1) --> [].

% DETERMINERS
determiner(X, singular, Prop, Assn, forall(X, (Prop => Assn))) --> [every].
determiner(X, plural, Prop, Assn, forall(X, (Prop => Assn))) --> [all].

determiner(X, singular, Prop, Assn, exists(X, Prop and Assn)) --> [a].
determiner(X, plural, Prop, Assn, exists(X, Prop and Assn)) --> [some].

determiner(X, _Plurality, Prop, Assn, locally_unique(X, Prop) and Assn) --> [the].

% INDEFINITE PRONOUNS
indefinite_pronoun(X, singular, Assn, exists(X, is_a(X, person) and Assn)) --> [somebody] | [someone].
indefinite_pronoun(X, plural, Assn, forall(X, (is_a(X, person) => Assn))) --> [everybody] | [everyone].

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

noun(X, singular, is_a(X, Noun)) --> { noun_singular(Noun) }, [Noun].
noun(X, plural, is_a(X, Class)) --> { pluralization(Class, Noun) }, [Noun].

% PRONOUNS
pronoun(X, Prop, (Prop, speaker(X))) --> [i] | [me].
pronoun(X, Prop, (Prop, speakee(X))) --> [you].
pronoun(X, Prop, (Prop, locally_unique(X, gender(X, nonbinary), singular(X)))) --> [them] | [they].
pronoun(X, Prop, locally_unique(X, multiple(X)) and Prop) --> [them] | [they].
pronoun(X, Prop, locally_unique(X, gender(X, feminine)) and Prop) --> [she] | [her].
pronoun(X, Prop, locally_unique(X, gender(X, masculine)) and Prop) --> [he] | [him].
pronoun(X, Prop, locally_unique(X, is_a(X, object)) and Prop) --> [it].

proper_noun(john) --> [john].
proper_noun(annie) --> [annie].
proper_noun(monet) --> [monet].
proper_noun(new_york) --> [new, york].

trans_verb(Subj, Obj, likes(Subj, Obj)) --> [likes].
trans_verb(Subj, Obj, likes(Obj, Subj)) --> [is, liked, by].
trans_verb(Subj, Obj, not(likes(Subj, Obj))) --> [does, not, like].
trans_verb(Subj, Obj, admires(Subj, Obj)) --> [admires].

intrans_verb(Subj, singular, paints(Subj)) --> [paints].
intrans_verb(Subj, plural, paints(Subj)) --> [paint].
intrans_verb(Subj, singular, sleeps(Subj)) --> [sleeps].
intrans_verb(Subj, plural, sleeps(Subj)) --> [sleep].

% SPEC(I, you, he/she/it/the dog, we, yall, they/the dogs).
% SPEC('', '', s,                 '', '',   '').
% verb1(3rd-singular, non-3rd-singular).
verb1(paints, paint).
verb1(sleeps, sleep).
verb1(eats, sleep).

intrans_verb(Subj, sing_3rd, Term) --> [Word],
    { verb1(Sing3rd, _NonSing3rd), Word = Sing3rd, Head = Sing3rd, Term =.. [Head, Subj] }.

intrans_verb(Subj, non_sing_3rd, Term) --> [Word],
    { verb1(Sing3rd, NonSing3rd), Word = NonSing3rd, Head = Sing3rd, Term =.. [Head, Subj] }.

/* examples */
/*
?- sentence(S,[every,man,that,paints,likes,monet],[]).
?- sentence(S,[a,woman,that,admires,john,paints],[]).
?- sentence(S,[every,woman,that,likes,a,man,that,admires,monet,paints],[]).
?- sentence(S,[john,likes,annie],[]).
?- sentence(S,[annie,likes,a,man,that,admires,monet],[]).
*/
