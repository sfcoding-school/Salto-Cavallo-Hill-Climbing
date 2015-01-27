(* Definisco un eccezione da sollevare nel caso una possibile mossa del cavallo vada fuori dalla scacchiera. *)
exception Over_board;;

(* La funzione successori calcola i vicini di un nodo del grafo, cioè una posizione raggiungibile dal cavallo sulla schacciera seguendo le regole
del gioco. Le possibili mosse sono messe in una lista di funzioni, che viene poi scorsa tramite ricorsione e controllata di volta in volta tramite
pattern matching, e ogni funzione viene applicata all'attuale nodo del grafo. Se essa genera una posizione non valida, cioè fuori dalla scacchiera,
viene sollevata un eccezione, altrimenti la nuova mossa viene aggiunta alla lista dei possibili successori. *)
let successori n v =
	let mossa1 n (x,y) =
		if x+1<=n && y+2<=n then (x+1,y+2)
		else raise Over_board
	in let mossa2 n (x,y) =
		if x-1>0 && y+2<=n then (x-1,y+2)
		else raise Over_board
	in let mossa3 n (x,y) =
		if x+1<=n && y-2>0 then (x+1,y-2)
		else raise Over_board
	in let mossa4 n (x,y) =
		if x-1>0 && y-2>0 then (x-1,y-2)
		else raise Over_board
	in let mossa5 n (x,y) =
		if x+2<=n && y+1<=n then (x+2,y+1)
		else raise Over_board
	in let mossa6 n (x,y) =
		if x-2>0 && y+1<=n then (x-2,y+1)
		else raise Over_board
	in let mossa7 n (x,y) =
		if x+2<=n && y-1>0 then (x+2,y-1)
		else raise Over_board
	in let mossa8 n (x,y) =
		if x-2>0 && y-1>0 then (x-2,y-1)
		else raise Over_board
	in let rec aux = function
		[] -> []
		| t::c -> try (t v)::(aux c)
			with Over_board -> aux c
	in aux [mossa1 n; mossa2 n; mossa3 n; mossa4 n; mossa5 n; mossa6 n; mossa7 n; mossa8 n];;

(* La funzione salto_cavallo è la funzione principale, che prende in input la posizione di partenza del cavallo e la dimensione della scacchiera.
Al suo interno è definita la funzione estendi, che calcola i possibili passi seguenti di un cammino e ritorna una lista di cammini che seguono da
quello di partenza e tali che non presentino cicli. La funzione corretto invece controlla se un cammino è quello che rappresenta la soluzione,
ovvero un cammino hamiltoniano nel grafo indotto dalle mosse del cavallo: esso deve toccare tutte le n^2 caselle della scacchiera. Infine la funzione
aux, partendo dalla posizione iniziale, verifica tramite ricorsione se sono ancora presenti mosse possibili: se non ce ne sono solleva un eccezione,
altrimenti controlla se la mossa attualmente considerata è una soluzione del gioco, altrimenti espande la soluzione e prosegue la visita in profondità.
Inoltre viene usato un approccio hill climbing, con un euristica detta di Warnsdorff, che suggerisce di privilegiare i cammini con il minor numero di
successori. Questo rende il problema, che di per se è NP-completo, risolvibile spesso in tempo lineare. L'ordinamento, che è quindi fatto solo sulle
nuove soluzioni parziali generate, si basa su un confronto tra il numero di soluzioni parziali che possono essere generate a partire da una data e
privilegia quelle con meno possibilità. Ciò consente di mantenere piccola la lista delle soluzioni parziali da controllare e privilegia percorsi che
comprono prima il bordo della scacchiera e poi la parte interna, così da non lasciare zone inaccessibili verso la fine del cammino. *)
let salto_cavallo inizio n =
	let confronta p1 p2 =
		let c1 = List.length (List.filter (function x -> not(List.mem x p1)) (successori n (List.hd p1)))
		in let c2 = List.length (List.filter (function x -> not(List.mem x p2)) (successori n (List.hd p2)))
		in if c1<c2 then -1
		else if c1=c2 then 0
		else 1
	in let estendi cammino =
		List.map (function x -> x::cammino) (List.filter (function x -> not(List.mem x cammino)) (successori n (List.hd cammino)))
	in let corretto cammino = (List.length cammino)=(n*n)
	in let rec aux = function
		[] -> raise Not_found
		| t::c -> if corretto t then List.rev t
			else aux ((List.sort confronta (estendi t))@c)
	in aux [[inizio]];;

(* Il primo esempio mostra il classico caso 8x8, in cui una soluzione esiste. L'euristica utilizzata consente di trovare la soluzione in tempo lineare,
anche se il problema è in generale NP-completo. *)
salto_cavallo (1,1) 8;;

(* Il secondo esempio mostra come l'euristica renda trattabile il problema anche per casi ben più grandi. *)
salto_cavallo (1,1) 24;;

(* Infine il terzo esempio mostrato è quello di un caso senza soluzione. *)
salto_cavallo (1,1) 4;;