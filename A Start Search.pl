/* Chen Yu & 220006342 */
/* yu996@my.yorku.ca */
/* ========= test cmd in q3 report========= */
add_edge(X1,Y1,X2,Y2,Cost) :-
    assertz(e(X1,Y1,X2,Y2,Cost)),
    assertz(e(X2,Y2,X1,Y1,Cost)).
remove_edge(X1,Y1,X2,Y2) :-
    retractall(e(X1,Y1,X2,Y2,_)),
    retractall(e(X2,Y2,X1,Y1,_)).
update_cost(X1,Y1,X2,Y2,NewCost) :-
    remove_edge(X1,Y1,X2,Y2),
    add_edge(X1,Y1,X2,Y2,NewCost). 
% ========= heuristic function =========
heuristic((X1,Y1), (X2,Y2), H) :-
    H is abs(X1 - X2) + abs(Y1 - Y2).

% ========= Counter =========
:- dynamic counter/1.
counter(0).
next_counter(N) :-
    retract(counter(C)), N is C + 1, asserta(counter(N)).
reset_counter :- retractall(counter(_)), asserta(counter(0)).

% ========= Extended Sequence Annotation =========
label_expansion_order([]).
label_expansion_order([Node|Rest]) :-
    Node = (X,Y),
    hex_center(X, Y, PX, PY),
    next_counter(N),
    format(atom(T), '~w', [N]),
    txg(PX+3, PY+4, T, red, none, 12),
    label_expansion_order(Rest).

% ========= Path Painter =========
draw_hex_path([]).
draw_hex_path([_]).
draw_hex_path([(X1,Y1), (X2,Y2) | Rest]) :-
    hex_center(X1, Y1, PX1, PY1),
    hex_center(X2, Y2, PX2, PY2),
    lng(PX1, PY1, PX2, PY2, red, 3),
    draw_hex_path([(X2,Y2)|Rest]).
draw_rec_path([]).
draw_rec_path([_]).
draw_rec_path([(X1,Y1), (X2,Y2)|Rest]) :-
    cell_size(S), offset(O),
    PX1 is X1*S + O, PY1 is Y1*S + O,
    PX2 is X2*S + O, PY2 is Y2*S + O,
    lng(PX1, PY1, PX2, PY2, red, 3),
    draw_rec_path([(X2,Y2)|Rest]).
draw_tri_path([]).
draw_tri_path([_]).
draw_tri_path([(X1,Y1), (X2,Y2) | Rest]) :-
    px(X1, PX1), py(Y1, PY1),
    px(X2, PX2), py(Y2, PY2),
    lng(PX1, PY1, PX2, PY2, red, 3),
    draw_tri_path([(X2,Y2)|Rest]).


draw_explored_edges([]).
draw_explored_edges([_]).
draw_explored_edges([(X1,Y1), (X2,Y2) | Rest]) :-
    hex_center(X1, Y1, PX1, PY1),
    hex_center(X2, Y2, PX2, PY2),
    lng(PX1, PY1, PX2, PY2, orange, 1),
    draw_explored_edges([(X2,Y2)|Rest]).

% ========= Algorithm =========
bfs([[Node|Path] | _], Node, FinalPath, Explored) :-
    reverse([Node|Path], FinalPath),
    reverse([[Node|Path]], Explored).
bfs([[Node|Path] | Rest], Goal, FinalPath, Explored) :-
    findall([Next,Node|Path],
        (
            e(X1,Y1,X2,Y2,_),
            (Node = (X1,Y1), Next = (X2,Y2); Node = (X2,Y2), Next = (X1,Y1)),
            \+ member(Next, [Node|Path])
        ),
        NextPaths),
    append(Rest, NextPaths, NewQueue),
    bfs(NewQueue, Goal, FinalPath, Explored).

ucs([[Node|Path]-_ | _], Node, FinalPath, Explored) :-
    reverse([Node|Path], FinalPath),
    reverse([[Node|Path]], Explored).

ucs([[Node|Path]-Cost | Rest], Goal, FinalPath, Explored) :-
    findall([Next,Node|Path]-NewCost,
        (
            e(X1,Y1,X2,Y2,Ecost),
            (Node = (X1,Y1), Next = (X2,Y2); Node = (X2,Y2), Next = (X1,Y1)),
            \+ member(Next, [Node|Path]),
            NewCost is Cost + Ecost
        ),
        NextPaths),

    append(Rest, NextPaths, Combined),        
    sort(2, @=<, Combined, NewQueue),         

    ucs(NewQueue, Goal, FinalPath, Explored).


gbf([[Node|Path]-_ | _], Node, FinalPath, Explored) :-
    reverse([Node|Path], FinalPath),
    reverse([[Node|Path]], Explored).
gbf([[Node|Path]-_ | Rest], Goal, FinalPath, Explored) :-
    findall([Next,Node|Path]-H,
        (
            e(X1,Y1,X2,Y2,_),
            (Node = (X1,Y1), Next = (X2,Y2); Node = (X2,Y2), Next = (X1,Y1)),
            \+ member(Next, [Node|Path]),
            heuristic(Next, Goal, H)
        ),
        NextPaths),
    sort(2, @=<, NextPaths, Sorted),
    append(Sorted, Rest, NewQueue),
    gbf(NewQueue, Goal, FinalPath, Explored).

astar([[Node|Path]-_ | _], Node, FinalPath, Explored) :-
    reverse([Node|Path], FinalPath),
    reverse([[Node|Path]], Explored).
astar([[Node|Path]-Cost | Rest], Goal, FinalPath, Explored) :-
    findall([Next,Node|Path]-F,
        (
            e(X1,Y1,X2,Y2,Ecost),
            (Node = (X1,Y1), Next = (X2,Y2); Node = (X2,Y2), Next = (X1,Y1)),
            \+ member(Next, [Node|Path]),
            G is Cost + Ecost,
            heuristic(Next, Goal, H),
            F is G + H
        ),
        NextPaths),
    sort(2, @=<, NextPaths, Sorted),
    append(Sorted, Rest, NewQueue),
    astar(NewQueue, Goal, FinalPath, Explored).

% ========= main run command =========
run_bfs_hex(Start, Goal) :-
    reset_counter,
    bfs([[Start]], Goal, Path, Explored),
    gvi, draw_edges_hex, label_nodes_hex,
    draw_explored_edges(Explored),
    draw_hex_path(Path),
    label_expansion_order(Path),
    gvf.

run_ucs_hex(Start, Goal) :-
    reset_counter,
    ucs([[Start]-0], Goal, Path, Explored),
    gvi, draw_edges_hex, label_nodes_hex,
    draw_explored_edges(Explored),
    draw_hex_path(Path),
    label_expansion_order(Path),
    gvf.

run_gbf_hex(Start, Goal) :-
    heuristic(Start, Goal, H),
    reset_counter,
    gbf([[Start]-H], Goal, Path, Explored),
    gvi, draw_edges_hex, label_nodes_hex,
    draw_explored_edges(Explored),
    draw_hex_path(Path),
    label_expansion_order(Path),
    gvf.

run_ast_hex(Start, Goal) :-
    heuristic(Start, Goal, H),
    reset_counter,
    astar([[Start]-H], Goal, Path, Explored),
    gvi, draw_edges_hex, label_nodes_hex,
    draw_explored_edges(Explored),
    draw_hex_path(Path),
    label_expansion_order(Path),
    gvf.

run_bfs_rect(Start, Goal, MaxX, MaxY) :-
    reset_counter,
    bfs([[Start]], Goal, Path, Explored),
    gvi,
    draw_edges_rect,
    label_nodes_rect(MaxX, MaxY),
    draw_explored_edges(Explored),
    draw_rec_path(Path),
    
    gvf.

run_ucs_rect(Start, Goal, MaxX, MaxY) :-
    reset_counter,
    ucs([[Start]-0], Goal, Path, Explored),
    gvi,
    draw_edges_rect,
    label_nodes_rect(MaxX, MaxY),
    draw_explored_edges(Explored),
    draw_rec_path(Path),
    
    gvf.

run_gbf_rect(Start, Goal, MaxX, MaxY) :-
    heuristic(Start, Goal, H),
    reset_counter,
    gbf([[Start]-H], Goal, Path, Explored),
    gvi,
    draw_edges_rect,
    label_nodes_rect(MaxX, MaxY),
    draw_explored_edges(Explored),
    draw_rec_path(Path),
    
    gvf.

run_ast_rect(Start, Goal, MaxX, MaxY) :-
    heuristic(Start, Goal, H),
    reset_counter,
    astar([[Start]-H], Goal, Path, Explored),
    gvi,
    draw_edges_rect,
    label_nodes_rect(MaxX, MaxY),
    draw_explored_edges(Explored),
    draw_rec_path(Path),
    
    gvf.


run_bfs_tri(Start, Goal, MaxX, MaxY) :-
    reset_counter,
    connect_odd_row(MaxX, MaxY),
    bfs([[Start]], Goal, Path, Explored),
    gvi,
    draw_edges,
    label_nodes,
    draw_explored_edges(Explored),
    draw_tri_path(Path),
    label_expansion_order(Path),
    gvf.

run_ucs_tri(Start, Goal, MaxX, MaxY) :-
    reset_counter,
    connect_odd_row(MaxX, MaxY),
    ucs([[Start]-0], Goal, Path, Explored),
    gvi,
    draw_edges,
    label_nodes,
    draw_explored_edges(Explored),
    draw_tri_path(Path),
    label_expansion_order(Path),
    gvf.

run_gbf_tri(Start, Goal, MaxX, MaxY) :-
    heuristic(Start, Goal, H),
    reset_counter,
    connect_odd_row(MaxX, MaxY),
    gbf([[Start]-H], Goal, Path, Explored),
    gvi,
    draw_edges,
    label_nodes,
    draw_explored_edges(Explored),
    draw_tri_path(Path),
    label_expansion_order(Path),
    gvf.

run_ast_tri(Start, Goal, MaxX, MaxY) :-
    heuristic(Start, Goal, H),
    reset_counter,
    connect_odd_row(MaxX, MaxY),
    astar([[Start]-H], Goal, Path, Explored),
    gvi,
    draw_edges,
    label_nodes,
    draw_explored_edges(Explored),
    draw_tri_path(Path),
    label_expansion_order(Path),
    gvf.



/* ========= Hex Grid ========= */
cell_size_hex(20).
offset_hex(50).

hex_deltas([(-1,-2), (1,-2), (2,0), (1,2), (-1,2), (-2,0)]).

hex_vertices(CX, CY, V) :-
    hex_deltas(D), findall((X,Y), (member((DX,DY), D), X is CX+DX, Y is CY+DY), V).

hex_center(X, Y, Cx, Cy) :-
    cell_size_hex(S), offset_hex(O),
    Cx is O + X * S * 1.5,
    RowOffset is (Y mod 2) * (S * sqrt(3) / 2),
    Cy is O + Y * S * sqrt(3) / 2 + RowOffset.

within_bounds((X,Y), MaxX, MaxY) :- X >= 0, X =< MaxX, Y >= 0, Y =< MaxY.
within_bounds_all([], _, _).
within_bounds_all([H|T], MaxX, MaxY) :- within_bounds(H, MaxX, MaxY), within_bounds_all(T, MaxX, MaxY).

connect_safe_loop(_, [], _, _).
connect_safe_loop(P1, [P2|Rest], MaxX, MaxY) :-
    P1 = (X1,Y1), P2 = (X2,Y2),
    within_bounds((X1,Y1), MaxX, MaxY),
    within_bounds((X2,Y2), MaxX, MaxY),
    assertz(e(X1,Y1,X2,Y2,1)), assertz(e(X2,Y2,X1,Y1,1)),
    connect_safe_loop(P2, Rest, MaxX, MaxY).
connect_safe_loop(_, [P2|Rest], MaxX, MaxY) :- connect_safe_loop(P2, Rest, MaxX, MaxY).

valid_center(CX, CY, MaxX, MaxY) :-
    between(0, MaxX, CX),
    between(0, MaxY, CY),
    CX mod 3 =:= 2,
    ((CX // 3) mod 2 =:= 0, CY mod 4 =:= 2;
     (CX // 3) mod 2 =:= 1, CY mod 4 =:= 0).

gen_hex_grid_manual(MaxX, MaxY) :-
    retractall(e(_,_,_,_,_)),
    forall(
        valid_center(CX, CY, MaxX, MaxY),
        (
            hex_vertices(CX, CY, V),
            V = [First|_], append(V, [First], Loop),
            connect_safe_loop(First, Loop, MaxX, MaxY)
        )
    ).

draw_edges_hex :- forall(e(X1,Y1,X2,Y2,Cost),
    (cost_color(Cost,Color), cost_width(Cost,Width),
     hex_center(X1,Y1,X1s,Y1s), hex_center(X2,Y2,X2s,Y2s),
     lng(X1s,Y1s,X2s,Y2s,Color,Width))).

label_nodes_hex :- forall(e(X,Y,_,_,_),
    (hex_center(X,Y,Cx,Cy), Xs is Cx+5, Ys is Cy-5,
     format(atom(Label), "(~w,~w)", [X,Y]), txg(Xs,Ys,Label))).

main_hex_manual :-
    gvi,
    gen_hex_grid_manual(15, 14),
    draw_edges_hex,
    label_nodes_hex,
    gvf.
/* ========= Rectangular Grid ========= */
cell_size(80).
offset(50).

clear_edges :- retractall(e(_,_,_,_,_)).

gen_rec_edges(MaxX, MaxY) :-
    clear_edges,
    forall(between(0, MaxX, X),
        forall(between(0, MaxY, Y),
            (
                (X < MaxX -> X1 is X+1, assertz(e(X,Y,X1,Y,1)), assertz(e(X1,Y,X,Y,1)) ; true),
                (Y < MaxY -> Y1 is Y+1, assertz(e(X,Y,X,Y1,1)), assertz(e(X,Y1,X,Y,1)) ; true)
            )
        )
    ).

cost_color(1, black). cost_color(2, red). cost_color(3, green). cost_color(4, blue). cost_color(_, magenta).
cost_width(1, 1). cost_width(2, 2). cost_width(3, 3). cost_width(4, 4). cost_width(_, 1).

draw_edges_rect :- cell_size(S), offset(O),
    forall(e(X1,Y1,X2,Y2,Cost),
        (cost_color(Cost,C), cost_width(Cost,W),
         X1s is X1*S+O, Y1s is Y1*S+O, X2s is X2*S+O, Y2s is Y2*S+O,
         lng(X1s,Y1s,X2s,Y2s,C,W))
    ).

label_nodes_rect(MaxX, MaxY) :- cell_size(S), offset(O),
    forall(between(0, MaxX, X),
        forall(between(0, MaxY, Y),
            (SX is X*S+O+5, SY is Y*S+O-5,
             format(atom(Label), "(~w,~w)", [X,Y]),
             txg(SX,SY,Label,none,black,15))
        )
    ).

main_rec(MaxX, MaxY) :-
    filename('rec', MaxX, MaxY, File),
    gvi,
    gen_rec_edges(MaxX, MaxY),
    draw_edges_rect,
    label_nodes_rect(MaxX, MaxY),
    gvf.

filename(GridType, MaxX, MaxY, FileName) :- format(atom(FileName), 'q1~w~wx~w.html', [GridType, MaxX, MaxY]).
/* ========= Triangle Grid ========= */
:- dynamic e/5.

edge_px(60).       
h_margin(50).
v_margin(50).
max_xy(15,14).

main :- main_tri_grid.

main_tri_grid :-
    gvi,
    gen_tri_grid(15,14),
    connect_odd_row(MaxX, MaxY),
    draw_edges,
    label_nodes,
    gvf.

gen_tri_grid(MaxX, MaxY) :-
    retractall(e(_,_,_,_,_)),
    findall(p(X,Y),
        (between(0,MaxY,Y), 0 is Y mod 2,
         (Y mod 4 =:= 0 -> X0 = 0 ; X0 = 1),
         between(X0,MaxX,X), 0 is (X-X0) mod 2),
        Points),
    forall(member(P1, Points), (
        tri_neighbours(P1, Points, Neighs),
        forall(member(P2, Neighs), (
            P1 = p(X1,Y1), P2 = p(X2,Y2),
            assertz(e(X1,Y1,X2,Y2,1)),
            assertz(e(X2,Y2,X1,Y1,1))
        ))
    )).


tri_neighbours(p(X,Y), Points, Neighs) :-
    findall(p(X2,Y2),
        (member((DX,DY), [(2,0),(1,2),(-1,2),(1,-2),(-1,-2)]),
         (DX = 2 -> 0 is Y mod 4 ; true),
         X2 is X+DX, Y2 is Y+DY,
         member(p(X2,Y2), Points)),
        Neighs).

connect_odd_row(MaxX, MaxY) :-
    forall((between(0, MaxY, Y), Y mod 4 =:= 2),
        forall((between(1, MaxX, X), X mod 2 =:= 1, X1 is X + 2, X1 =< MaxX),
            (
                assertz(e(X,Y,X1,Y,1)),
                assertz(e(X1,Y,X,Y,1))
            )
        )
    ).
px(X, PX) :- edge_px(A), h_margin(H), PX is H + X*A/2.
py(Y, PY) :- edge_px(A), v_margin(V),
             RowH is A*sqrt(3)/2,
             Step is Y//2, PY is V + Step*RowH.


draw_edges :-
    forall(e(X1,Y1,X2,Y2,Cost),
        (
            cost_color(Cost,Color),
            cost_width(Cost,Width),
            px(X1,PX1), py(Y1,PY1),
            px(X2,PX2), py(Y2,PY2),
            lng(PX1, PY1, PX2, PY2, Color, Width)
        )).


cost_color(1, "#00CED1").
cost_color(_, black).

cost_width(1, 2).
cost_width(_, 1).


label_nodes :-
    setof((X,Y), Z^W^e(X,Y,Z,W,_), Nodes),
    forall(member((X,Y), Nodes), (
        px(X,PX), py(Y,PY),
        TY is PY - 6,
        format(atom(L), "(~w,~w)", [X,Y]),
        txg(PX, TY, L)
    )).

main :- main_rec(5, 5).  
%================
%global variables
%>>>>>use any global variable
gvr0(Gvar,Y):- not(nb_current(Gvar,Y)) -> gvw(Gvar,''); true. %init to '' if necessary)
gvr(Gvar,Y):- gvr0(Gvar,Y), nb_getval(Gvar,Y). %global var read (init to '' if necessary)
gvw(Gvar,Y):- nb_setval(Gvar,Y). %global var write; to reset set to '' with gvw(Gvar,'') 
gva(Gvar,Y):- gvr(Gvar,G), atomic_list_concat([G,Y],G1), gvw(Gvar,G1). %global var append 
%gvr(buf1,X).
%gva(buf1,5).
%>>>>>use buf as a default global variable
gvi :- gvw(buf,''). %global var initialize
gvr(X) :- gvr(buf,X). %global var read
gvw(X) :- gvw(buf,X). %glabal var write
gva(X) :- gva(buf,X). %global var append
gvf(G) :- gvr(G,X), wf(X). %global var to file
gvf :- gvr(X), wf(X). %default global var (buf) to file
%=================
%write SVG to a HTML file
%>>>>> writing a HTML file
wf(L,File) :- tell(File), h1(X), h2(Y), write(X), nl, write(L), nl, write(Y), told. %write an atom to a file
wf(L) :- wf(L,'test.html'). %use the default filename
%lnv(100,200,300,400,green,2,X), txv(100,200,text,blue,Y), atomic_list_concat([X,Y],Res), wf(Res),!.
%=============
%SVG templates
h1('<!DOCTYPE html>
<html>
<head><meta http-equiv="refresh" content="3" /></head>
<body><h2>Prolog Scalable Vector Graphics Library (PSVGL) by Kamen Kanev</h2>
<svg height="1000" width="1000">
').
h2('Sorry, your browser does not support inline SVG.
</svg>
</body>
</html>
').
/*--------------
PSVGL predicates
----------------*/
%======== 
%SVG line
%>>>>>line (segment) with color and width to a variable
%<line x1="1" y1="2" x2="3" y2="4" stroke="red" stroke-width="3" />
lnv(X1A,Y1A,X2A,Y2A,Color,WidthA,Res) :- maplist(round(),[X1A,Y1A,X2A,Y2A,WidthA],[X1,Y1,X2,Y2,Width]),
	atomic_list_concat(['<line x1="',X1,'" y1="',Y1,'" x2="',X2,'" y2="',Y2,'" stroke="',Color,'" stroke-width="',Width,'"/>', '\n'],Res).
lnv(X1,Y1,X2,Y2,Res) :- lnv(X1,Y1,X2,Y2,black,1,Res). %default color (black) and width (1) <*******
%lnv(100,200,300,400,red,2,Res).
%lnv(100,200,300,400,Res)
%>>>>>line (segment) with color and width shown with write
ln(X1,Y1,X2,Y2,Color,Width) :- lnv(X1,Y1,X2,Y2,Color,Width,Res), write(Res). 
ln(X1,Y1,X2,Y2) :- lnv(X1,Y1,X2,Y2,Res), write(Res).
%ln(100,200,300,400,red,2).
%ln(100,200,300,400).
%>>>>>line (segment) with color and width to a global variable (buffer)
lng(X1,Y1,X2,Y2,Color,Width,Gvar) :- lnv(X1,Y1,X2,Y2,Color,Width,Res), gva(Gvar,Res). 
lng(X1,Y1,X2,Y2,Gvar) :- lnv(X1,Y1,X2,Y2,Res), gva(Gvar,Res). %default color and width
lng(X1,Y1,X2,Y2,Color,Width) :- lnv(X1,Y1,X2,Y2,Color,Width,Res), gva(Res). 
lng(X1,Y1,X2,Y2) :- lnv(X1,Y1,X2,Y2,Res), gva(Res). %default color and width
%gvw(buf1,''),lng(100,200,300,400,red,2,buf1),gvr(buf1,X),gvf(buf1).
%gvw(buf1,''),lng(100,200,300,400,buf1),gvr(buf1,X),gvf(buf1).
%gvi,lng(200,100,300,400,red,2),gvr(X),gvf.
%gvi,lng(100,200,300,400),gvr(X),gvf.
%========
%SVG text
%>>>>> with color, fill, and size to a variable
%<text x="1" y="2" stroke="red" fill="blue" font-size="35">text</text>
txv(XA,YA,T,Color,Fill,Size,Res) :-  maplist(round(),[XA,YA],[X,Y]),
	atomic_list_concat(['<text x="', X, '" y="', Y,'" stroke="',Color, '" fill="', Fill, '" font-size="', Size, '">', T, '</text>', '\n'], Res).
txv(X,Y,T,Res) :- txv(X,Y,T,none,black,20,Res). %default color, fill, and size (20) <*******
%txv(100,200,text,red,none,30,Res).
%txv(100,200,text,Res).
%>>>>> with color, fill, and size shown with write
tx(X,Y,T,Color,Fill,Size) :- txv(X,Y,T,Color,Fill,Size,Res), write(Res).
tx(X,Y,T) :- txv(X,Y,T,Res), write(Res). %default color, fill, and size
%tx(100,200,text,red,green,12).
%tx(100,200,text).
%>>>>> with color, fill, and size to a global variable (buffer)
txg(X,Y,T,Color,Fill,Size,Gvar) :- txv(X,Y,T,Color,Fill,Size,Res), gva(Gvar,Res).
txg(X,Y,T,Gvar) :- txv(X,Y,T,Res), gva(Gvar,Res). %default color, fill, and size
txg(X,Y,T,Color,Fill,Size) :- txv(X,Y,T,Color,Fill,Size,Res), gva(Res).
txg(X,Y,T) :- txv(X,Y,T,Res), gva(Res). %default color, fill, and size
%gvw(buf1,''),txg(100,200,'Text',none,blue,30,buf1),gvr(buf1,X),gvf(buf1).
%gvw(buf1,''),txg(100,200,'Text',buf1),gvr(buf1,X),gvf(buf1).
%gvi,txg(100,200,text,red,black,100),gvr(X),gvf.
%gvi,txg(100,200,text),gvr(X),gvf.
/*PSVGL END=========================================================================*/