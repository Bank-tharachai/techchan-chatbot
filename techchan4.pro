% 16B09737 Nattanon Tharachai 2018/02/13
%
% TechChan Question Answering System
% (Template code for an exercise of Logic and Reasoning)
% Takahiro Shinozaki, 2018/2/1
% Ver1.1
%
% Ver. 1.1 fixed the position of cut(!) at techchan(Query, Answer)
% clause 2018/2/2
% Ver. 1.0 Initial version 2018/2/1
%
% Examples
% ?- techchan([suzukakedai, kara, oookayama, made], Answer).
% ?- techchan([yokohama, kara, tokyo, made, shinkansen, wo, tsukawazuni], Answer).
% ?- techchan([suzukakedai, kara, oookayama, made, yokohamasen, wo, tsukawazuni], Answer).
% ?- techchan([yokohama, kara, tokyo, made, shinkansen, de], Answer).
% ?- techchan([suzukakedai, kara, oookayama, made, yokohamasen, keiyu], Answer).
% ?- techchan([oookayama, shuuhen, ramen], Answer).
% ?- techchan([oookayama, shuuhen, chuuka], Answer).
% ?- techchan([oookayama, shuuhen, teishoku], Answer).
% ?- techchan([tokodai, ha, nani], Answer).
% ?- techchan([tokodai, ha, nanda], Answer).
% ?- techchan([tokodai, ha, doko], Answer).
% ?- techchan([tokyokougyoudaigaku, toha], Answer).
% ?- techchan([koudaisai, itsu], Answer).
% ?- techchan([anata, ha, dare, desu, ka], Answer).
%
% TechChan is a mascot for Kodai-sai.
% https://koudaisai.jp/mascot/
% When you extend this program, follow the rules of TechChan.
% https://koudaisai.jp/mascot/agreement/
%
%
%

techchan(Query, Answer):-
  recognize(Query, Slots, Flags),!,think(Slots, Flags, Answer),writeln(Answer).
techchan(_, Answer):-
  Answer=[konnichiwa, boku, tokodai, no, techchan, desu], writeln(Answer).


% Basic concepts
station(chuorinkan).
station(suzukakedai).
station(nagatsuta).
station(futakotamagawa).
station(kikuna).
station(jiyugaoka).
station(oookayama).
station(yokohama). % consider yokohama and shinyokohama as same station
station(shibuya).
station(daikanyama).
station(yutenji).
station(gakugeidaigaku).
station(toritsudaigaku).
station(denentoshi).
station(tamagawa).
station(shinmaruko).
station(musashikosugi).
station(motosumiyoshi).
station(hiyoshi).
station(ooimachi).
station(tokyo).
train(denentoshisen).
train(yokohamasen).
train(toyokosen).
train(ooimachisen).
train(shinkansen).
train(jr_toukaidouhonsen).

restaurant(kokoro).
restaurant(hirugao).
restaurant(shisen).
restaurant(yabu).
restaurant(ryoga).
restaurant(nagahamanambawan).
restaurant(tanya).
restaurant(kohikan).
restaurant(gyukaku).
restaurant(coconutSugar).
foodtype(ramen).
foodtype(chuuka).
foodtype(teishoku).
foodtype(cafe).
foodtype(yakiniku).
foodtype(thairyori).

tokodaiTopic(tokodai).
tokodaiTopic(tokyokougyoudaigaku).
tokodaiTopic(koudaisai).
tokodaiTopic(tsubame).
tokodaiTopic(mishima).
tokodaiQ(nani).
tokodaiQ(nanda).
tokodaiQ(doko).
tokodaiQ(dare).
tokodaiQ(itsu).


% knowledge about language to understand the query(automaton)
%% delta(fromState, arcSymbol, toState, slot, flag)
%%% flag [1st, 2nd, 3rd, 4th, 5th] 4 = train line name, 5th = use only, or not use.
delta(0, W, 1, [transport, W, _, _, _],[1,1,0,0,0]):-station(W).
delta(1, kara, 2, _, _).
delta(2, W, 3, [transport, _,W, _, _], [1,0,1,0,0]):-station(W).
delta(2, W, 5, [transport, _,W, _, _], [1,0,1,0,0]):-station(W).
delta(3, made, 4, _, _).
delta(3, made, 5, [transport, _, _, _, noCondition], [1,0,0,1,1]).
delta(3, made, 8, _, _).
delta(4, ikitai, 5, [transport, _, _, _, noCondition], [1,0,0,1,1]).
delta(4, ikitai, 8, _, _).
delta(8, L, 9, [transport, _, _, L, _], [1,0,0,1,0]):-train(L).
delta(9, de, 5, [transport, _, _, _, useTrain], [1,0,0,0,1]).
delta(9, keiyu, 5, [transport, _, _, _, useTrain], [1,0,0,0,1]).
delta(9, wo, 10, _, _).
delta(10, tsukawazuni, 5, [transport, _, _, _, avoidTrain], [1,0,0,0,1]).

delta(0, oookayama, 6, _, _).
delta(6, shuuhen, 7, _, _).
delta(7, W, 5, [food, W, _,_,_], [1,1,0,0,0]):-foodtype(W).

delta(0, T, 11, [tokodai, T, _, _, _], [1,1,0,0,0]):-tokodaiTopic(T).
delta(11, Q, 5, [tokodai, _, Q,_,_], [1,0,1,0,0]):-tokodaiQ(Q).
delta(11, toha, 5, [tokodai, _, nani,_,_], [1,0,1,0,0]).
delta(11, ha, 12, _,_).
delta(12, Q, 5, [tokodai, _, Q,_,_], [1,0,1,0,0]):-tokodaiQ(Q).

goal(5).


% knowledge about transportation to answer the question (directed graph)
%% link(transportation, fromStation, toStation)
link(denentoshisen, chuorinkan, suzukakedai).
link(denentoshisen, suzukakedai, nagatsuta).
link(denentoshisen, suzukakedai, futakotamagawa).
link(denentoshisen, shibuya, futakotamagawa).
link(yokohamasen, nagatsuta, kikuna).
link(yokohamasen, yokohama, kikuna).
link(toyokosen, kikuna, jiyugaoka).
link(toyokosen, yokohama, kikuna).
link(toyokosen, shibuya, daikanyama).
link(toyokosen, nakameguro, daikanyama).
link(toyokosen, nakameguro, yutenji).
link(toyokosen, yutenji, gakugeidaigaku).
link(toyokosen, gakugeidaigaku, toritsudaigaku).
link(toyokosen, toritsudaigaku, jiyugaoka).
link(toyokosen, jiyugaoka, denentoshi).
link(toyokosen, denentoshi, tamagawa).
link(toyokosen, tamagawa, shinmaruko).
link(toyokosen, shinmaruko, musashikosugi).
link(toyokosen, musashikosugi, motosumiyoshi).
link(toyokosen, motosumiyoshi, hiyoshi).
link(ooimachisen, futakotamagawa, jiyugaoka).
link(ooimachisen, jiyugaoka, oookayama).
link(ooimachisen, ooimachi, oookayama).
link(shinkansen, yokohama, tokyo).
link(jr_toukaidouhonsen, yokohama, tokyo).

% Assume the links are bi-directional (make it undirected)
bilink(X, Y, Z):-link(X, Y, Z).
bilink(X, Y, Z):-link(X, Z, Y).

% knowledge about food and restaurant (directed graph)
%% foodlink(foodtype, restaurant)
foodlink(ramen, kokoro).
foodlink(ramen, hirugao).
foodlink(ramen, ryoga).
foodlink(ramen, nagahamanambawan).
foodlink(ramen, tanya).
foodlink(chuuka, shisen_yatai).
foodlink(teishoku, yabu).
foodlink(cafe, kohikan).
foodlink(yakiniku, gyukaku).
foodlink(thairyori, coconutSugar).

% knowledge about tokodai (directed graph)
%% qlink(query, answer)
qlink(tokodai, nani, [tokodai, to, ha, rikoukei, no, kokuritsu, daigaku, dayo]).
qlink(tokyokougyoudaigaku, nani, [tokyokougyoudaigaku, to, ha, rikoukei, no, kokuritsu, daigaku, dayo]).
qlink(tokodai, nanda, [tokodai, to, ha, rikoukei, no, kokuritsu, daigaku, dayo]).
qlink(tokyokougyoudaigaku, nanda, [tokyokougyoudaigaku, to, ha, rikoukei, no, kokuritsu, daigaku, dayo]).
qlink(tokodai, doko, [tokodai, no, campus, ha, oookayama, suzukakedai, tamachi, ni, aru, yo]).
qlink(tokyokougyoudaigaku, doko, [tokyokougyoudaigaku, no, campus, ha, oookayama, suzukakedai, tamachi, ni, aru, yo]).
qlink(koudaisai, nani, [koudaisai, to, ha, toukoudai, no, daigakusai, dayo]).
qlink(koudaisai, nanda, [koudaisai, to, ha, toukoudai, no, daigakusai, dayo]).
qlink(koudaisai, doko, [koudaisai, ha, oookayama, campus, de, kaisai, sareru, yo]).
qlink(koudaisai, itsu, [koudaisai, ha, maitoshi, jyu_gatsu, ni, kaisai, sareru, yo]).
qlink(tsubame, nani, [tsubame, to, ha, toukoudai, no, supercomputer, dayo]).
qlink(tsubame, nanda, [tsubame, to, ha, toukoudai, no, supercomputer, dayo]).
qlink(mishima, dare, [mishima, san, ha, toukoudai, no, gakucho, dayo]).


% recognize (Slot filling based on automaton)
recognize(Ss, Slots, Flags):-goal(G),path(0, Ss, G, Slots, [0,0,0,0,0], Flags).
path(X,[S],Y, Slots, Flags, NewFlags):-
  delta(X, S, Y, Slots, Flags1), listOr(Flags, Flags1, NewFlags).
path(X,[S|Ss],Y, Slots, Flags, NewFlags):-
  delta(X, S, Z, Slots, Flags1), listOr(Flags, Flags1, NewFlags1),
  path(Z, Ss, Y, Slots, NewFlags1, NewFlags).

% think to answer
think([transport, From, To, Train, TrainCondition], [1,1,1,1,1], Answer):-
  reachBy(From, To, _, Transports, Train, TrainCondition),
  transports2utterance(Transports, Answer).
think([food, Type, _,_,_], [1,1,0,0,0], Answer):-
  foodlink(Type, Restaurant), restaurant2utterance(Restaurant, Answer).
think([tokodai, T,Q,_,_], [1,1,1,0,0], Answer):-
  qlink(T, Q, Answer).

% think engine about transportation (graph search)
% search how to move
% rule for no specific train line, then for specific train line

reachBy(From, To, Stations, Transports, Train, TrainCondition):-
  reachBySub(From, To, [], Stations, [], Transports, Train, TrainCondition).

% noCondition
reachBySub(P, P, Ss, Stations, Y, Transports, _, TrainCondition):-
  TrainCondition == noCondition,
  not(myMember(P, Ss)),
  myReverse([P|Ss], Stations), myReverse([[P, you_arrived]|Y], Transports).
% avoidTrain
reachBySub(P, P, Ss, Stations, Y, Transports, Train, TrainCondition):-
  TrainCondition == avoidTrain,
  not(myMember(P, Ss)),
  myReverse([P|Ss], Stations), myReverse([[P, you_arrived]|Y], Transports),
  not(inTransport(Train, Transports)).
% useTrain
reachBySub(P, P, Ss, Stations, Y, Transports, Train, TrainCondition):-
  TrainCondition == useTrain,
  not(myMember(P, Ss)),
  myReverse([P|Ss], Stations), myReverse([[P, you_arrived]|Y], Transports),
  inTransport(Train, Transports).

reachBySub(From, To, [], Stations, [], Transports, Train, TrainCondition):-
  bilink(Tr, From, Y),
  reachBySub(Y, To, [From], Stations, [[From,Tr]], Transports, Train, TrainCondition).

reachBySub(From, To, Ss, Stations, [[Fr,Tr]|FrTrs], Transports, Train, TrainCondition):-
  not(myMember(From, Ss)),bilink(Tr, From, Y),
  reachBySub(Y, To, [From|Ss], Stations, [[Fr,Tr]|FrTrs], Transports, Train, TrainCondition).
reachBySub(From, To, Ss, Stations, [[Fr,Tr]|FrTrs], Transports, Train, TrainCondition):-
  not(myMember(From, Ss)),bilink(Tr1, From, Y),not(Tr==Tr1),
  reachBySub(Y, To, [From|Ss], Stations, [[From,Tr1],[Fr,Tr]|FrTrs], Transports, Train, TrainCondition).

% form answer utterance
transports2utterance([[X, you_arrived]], [X, ni, tsukeruyo]).
transports2utterance([[From, By]|X], [From, kara, By, ninotte|Utterance]):-
  transports2utterance(X, Utterance).
restaurant2utterance(Restaurant, Answer):- Answer = [Restaurant, ga, ii, kamo].

% some list tools
myAppend([], X, X).
myAppend([X | L], Y, [X | Z]) :- myAppend(L, Y, Z).

myReverse([], []).
myReverse([X | L], Y):-myReverse(L, Z), myAppend(Z, [X], Y).

myMember(X, [X | _]).
myMember(X, [_ | L]):-myMember(X, L).

listOr([],[],[]).
listOr([0|X], [0|Y], [0|Z]):-!,listOr(X, Y, Z).
listOr([_|X], [_|Y], [1|Z]):-listOr(X, Y, Z).

inTransport(X, [ H | _ ]):-myMember(X, H).
inTransport(X, [_ | T]):-inTransport(X, T).
