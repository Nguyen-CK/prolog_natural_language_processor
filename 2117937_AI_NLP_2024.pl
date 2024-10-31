%%%%%%%%%%%% Grammar %%%%%%%%%%%%%%
s --> np(Number, Person, nom, Ani), vp(Number, Person, Ani).
s(s(NP,VP)) --> np(Number, Person, nom, Ani, NP), vp(Number, Person, Ani, VP).

%%%%%%%%%%%% NOUN PHRASES %%%%%%%%%%%%
%% PRONOUNS %%
np(Number, Person, nom, Ani) --> pro(Number, Person, nom, Ani).
np(Number, Person, nom, Ani, np(Pro)) --> pro(Number, Person, nom, Ani, Pro).

np(Number, Person, acc, Ani) --> pro(Number, Person, acc, Ani).
np(Number, Person, acc, Ani, np(Pro)) --> pro(Number, Person, acc, Ani, Pro).

%% COMMON NOUNS PHRASES %%
np(Number, _, _, Ani) --> det(Number), nbar(Number, Ani).
np(Number, _, _, Ani, np(Det,Nbar)) --> det(Number, Det), nbar(Number, Ani, Nbar).

np(Number, _, _, Ani) --> det(Number), nbar(Number, Ani), pp.
np(Number, _, _, Ani, np(Det,Nbar,PP)) --> det(Number, Det), nbar(Number, Ani, Nbar), pp(PP).

nbar(Number, Ani) --> n(Number, Ani).
nbar(Number, Ani, nbar(N)) --> n(Number, Ani, N).

nbar(Number, Ani) --> jp(Number, Ani).
nbar(Number, Ani, nbar(JP)) --> jp(Number, Ani, JP).

%%%%%%%%%%%% PREPOSITIONS PHRASES %%%%%%%%%%%%
pp --> prep, np(_, _, _, _).
pp(pp(Prep,NP)) --> prep(Prep), np(_, _, _, _, NP).

byPrepP(Person) --> byPrep, np(_, Person, acc, ani).
byPrepP(Person,byPrepP(ByPrep, NP)) --> byPrep(ByPrep), np(_, Person, acc, ani, NP).


%%%%%%%%%%%% ADJECTIVE PHRASES %%%%%%%%%%%%
jp(Number, Ani) --> adj, jp(Number, Ani).
jp(Number, Ani, jp(ADJ,JP)) --> adj(ADJ), jp(Number, Ani, JP).

jp(Number, Ani) --> adj, n(Number, Ani).
jp(Number, Ani, jp(ADJ,N)) --> adj(ADJ), n(Number, Ani, N).

%%%%%%%%%%%% VERB PHRASES %%%%%%%%%%%%
%% INTRANSITIVE %%
vp(Number, Person, Ani) --> iv(Number, Person, Ani).
vp(Number, Person, Ani, vp(V)) --> iv(Number, Person, Ani, V).

%% TRANSITIVE %%
vp(Number, Person, Ani) --> tv(Number, Person, Ani), np(_, Person, acc, _).
vp(Number, Person, Ani, vp(V,NP)) --> tv(Number, Person, Ani, V), np(_, Person, acc, _, NP).

%% PASSIVE %%
vp(Number, Person, Ani) --> aux(Number, Person), pastV(Number, Person).
vp(Number, Person, Ani, vp(AUX,PastV)) --> aux(Number, Person, AUX), pastV(Number, Person, PastV).

vp(Number, Person, _) --> aux(Number, Person), pastV(Number, Person), byPrepP(Person).
vp(Number, Person, _, vp(AUX,PastV,ByPrepP)) --> aux(Number, Person, AUX), pastV(Number, Person, PastV), byPrepP(Person, ByPrepP).

%% LEXICON TO WORDS %%
pro(Number, Person, Role, Ani) --> [Word], {lex(Word, pro, Number, Person, Role, Ani)}.
pro(Number, Person, Role, Ani, pro(Word)) --> [Word], {lex(Word, pro, Number, Person, Role, Ani)}.

n(Number, Ani) --> [Word], {lex(Word, n, Number, _, _, Ani)}.
n(Number, Ani, n(Word)) --> [Word], {lex(Word, n, Number, _, _, Ani)}.

tv(Number, Person, Ani) --> [Word], {lex(Word, tv, Number, Person, Ani)}.
tv(Number, Person, Ani, v(Word)) --> [Word], {lex(Word, tv, Number, Person, Ani)}.

pastV(Number, Person) --> [Word], {lex(Word, pastPart, Number, Person, _)}.
pastV(Number, Person, v(Word)) --> [Word], {lex(Word, pastPart, Number, Person, _)}.

iv(Number, Person, Ani) --> [Word], {lex(Word, iv, Number, Person, Ani)}.
iv(Number, Person, Ani, v(Word)) --> [Word], {lex(Word, iv, Number, Person, Ani)}.

aux(Number, Person) --> [Word], {lex(Word, aux, Number, Person)}.
aux(Number, Person, aux(Word)) --> [Word], {lex(Word, aux, Number, Person)}.

det(Number) --> [Word], {lex(Word, det, Number)}.
det(Number, det(Word)) --> [Word], {lex(Word, det, Number)}.

prep --> [Word], {lex(Word, prep)}.
prep(prep(Word)) --> [Word], {lex(Word, prep)}.

byPrep --> [Word], {lex(Word, byPrep)}.
byPrep(byPrep(Word)) --> [Word], {lex(Word, byPrep)}.

adj --> [Word], {lex(Word, adj)}.
adj(adj(Word)) --> [Word], {lex(Word, adj)}.
%%%%%%%%%%%% Lexicon %%%%%%%%%%%%%%
% The lexicon should include all the following words that appear, where the components of each lexical
% entry are as given. For clarity, the lexicon is given in the form that your code would require.
% The grammar must treat the features in the lexicon.
% Note that there are some lexical forms that your grammar would require, but are missing in the listing below.

%%% Pronouns %%%
% For pronouns, the information appears in the following order: word, grammatical category (pronoun),
% number (singular/plural), grammatical person (1st, 2nd, or 3rd), and grammatical role (subject or object)
lex(i,pro,singular,1,nom,ani).
lex(you,pro,singular,2,nom,ani).
lex(he,pro,singular,3,nom,ani).
lex(she,pro,singular,3,nom,ani).
lex(it,pro,singular,3,nom,ani).

lex(we,pro,plural,1,nom,ani).
lex(you,pro,plural,2,nom,ani).
lex(they,pro,plural,3,nom,ani).

lex(me,pro,singular,1,acc,ani).
lex(you,pro,singular,2,acc,ani).
lex(him,pro,singular,3,acc,ani).
lex(her,pro,singular,3,acc,ani).
lex(it,pro,singular,3,acc,ani).

lex(us,pro,plural,1,acc,ani).
lex(you,pro,plural,2,acc,ani).
lex(them,pro,plural,3,acc,ani).

%%% Common Nouns %%%
% For common nouns, the information appears in the following order: word, grammatical category (noun), number
lex(man,n,singular,_,_,ani).
lex(woman,n,singular,_,_,ani).
lex(dog,n,singular,_,_,ani).
lex(apple,n,singular,_,_,nani).
lex(chair,n,singular,_,_,nani).
lex(room,n,singular,_,_,nani).

lex(men,n,plural,_,_,ani).
lex(women,n,plural,_,_,ani).
lex(dogs,n,singular,_,_,ani).
lex(apples,n,plural,_,_,nani).
lex(chairs,n,plural,_,_,nani).
lex(rooms,n,plural,_,_,nani).

%%% Verbs %%%
% For verbs, the information appears in the following order: word, gramma5cal category (verb), number (singular/plural),
% grammatical person (1st, 2nd, 3rd)

%%% Transitive Verbs %%%
% Note that we do not have in this example Lexicon past participles for every transi5ve verb:
% know, see, hire
lex(know,tv,singular,1,ani).
lex(know,tv,singular,2,ani).
lex(knows,tv,singular,3,ani).
lex(know,tv,plural,_,ani).

lex(see,tv,singular,1,ani).
lex(see,tv,singular,2,ani).
lex(sees,tv,singular,3,ani).
lex(see,tv,plural,_,ani).

lex(hire,tv,singular,1,ani).
lex(hire,tv,singular,2,ani).
lex(hires,tv,singular,3,ani).
lex(hire,tv,plural,_,ani).

lex(break,tv,singular,1,ani).
lex(break,tv,singular,2,ani).
lex(breaks,tv,singular,3,ani).
lex(break,tv,plural,_,ani).

lex(bite,tv,singular,1,ani).
lex(bite,tv,singular,2,ani).
lex(bites,tv,singular,3,ani).
lex(bite,tv,plural,_,ani).

%%% Past Participle %%%
% These verb forms are used in the passive.
% Only the tv verbs can be passivized. We don't provide them all in this listing,
% but as part of the coursework, you should add the pastPart of other tv verbs to the lexicon
% so as to recognise some of the sentences above and others.
% Note that verbs in the passive to not require animate subjects, though this is required of
% the transitive forms.
%
% You will need to add to the lexicon past participle forms for all those verbs that are transitive above.
lex(known,pastPart,singular,1,_).
lex(known,pastPart,singular,2,_).
lex(known,pastPart,singular,3,_).
lex(known,pastPart,plural,_,_).

lex(seen,pastPart,singular,1,_).
lex(seen,pastPart,singular,2,_).
lex(seen,pastPart,singular,3,_).
lex(seen,pastPart,plural,_,_).

lex(hired,pastPart,singular,1,_).
lex(hired,pastPart,singular,2,_).
lex(hired,pastPart,singular,3,_).
lex(hired,pastPart,plural,_,_).

lex(broken,pastPart,singular,1,_).
lex(broken,pastPart,singular,2,_).
lex(broken,pastPart,singular,3,_).
lex(broken,pastPart,plural,_,_).

lex(bitten,pastPart,singular, 1, _).
lex(bitten,pastPart,singular, 2, _).
lex(bitten,pastPart,singular, 3, _).
lex(bitten,pastPart,plural, _, _).

%%% Intransitive verbs %%%
% These cannot go into the passive.
lex(fall,iv,singular,1,_).
lex(fall,iv,singular,2,_).
lex(falls,iv,singular,3,_).
lex(fall,iv,plural,_,_).

lex(sleep,iv,singular,1,ani).
lex(sleep,iv,singular,2,ani).
lex(sleeps,iv,singular,3,ani).
lex(sleep,iv,plural,_,ani).

%%% Auxiliary verbs (aux) for the passive %%%
% In this version of the lexicon, animacy of auxilary verbs is not necessary,
% though if the grammar is done in a different way, it might be.
lex(am,aux,singular,1).
lex(are,aux,singular,2).
lex(is,aux,singular,3).
lex(are,aux,plural,_).

%%% Determiners %%%
% For determiners, the information appears in the following order: word, grammatical category
% (determiner), number
lex(the,det,_).
lex(a,det,singular).
lex(two,det,plural).

%%% Prepositions %%%
% For prepositions, the information appears in the following order: word, grammatical category (preposition)
lex(on,prep).
lex(in,prep).
lex(under,prep).

% We have a desigated category for the preposition for the passive by-phrase.
% When we connect to the grammar and used to recognised sentences, it will require
% an animate np. Consult the examples and discussion above.
lex(by,byPrep).

%%% Adjectives %%%
% For adjectives, the information appears in the following order: word, grammatical category (adjec5ve)
lex(old,adj).
lex(young,adj).
lex(red,adj).
lex(short,adj).
lex(tall,adj).