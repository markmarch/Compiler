### Optimizations
Date : 11/30/2011

#### Lecture Topics

1. Introductions to optimizations
2. Constant folding
3. Data flow analyzing
4. Obstacles to optimizations

#### Introductions

Higher level IR <--> Portable optimizations   
|   
Code generator   
|   
Low level IR <--> Machine dependent optimizations  
|   
Printer   
|   
Target assembly


#### Typical optimizations

1. [Constant Folding](http://en.wikipedia.org/wiki/Constant_folding):

		Before:
		h = 60 * 60

		After:
		h = 3600
2. [Copy Propagation](http://en.wikipedia.org/wiki/Copy_propagation):
	
		Before:
		t = h
		d = 24 * t
		
		After:
		t = h
		d = 24 * h
3. [Dead code elimination]:
	
		Before:
		t = h
		d = 24 * h
		return d;

		After:
		d = 24 * h
		return d
4. [Common Sub-expression elimination]:
		
		Before:
		h = 60 * 60
		t = 24 * 60 * 60

		After:
		h = 60 * 60
		t = 24 * h
5. [Algebraic simplifications]:
		
		Before:
		while( i <= n - 1)

		After:
		while( i < n )
6. [Strength reduction]:
	
		Before:
		x = y * 2
		x = y / 2

		After:
		x = y + y
		x = y >> 1
7. [Loop Inversion]:

		Before
		while … { x * 2 + 1 …} // assume x * 2 + 1 doesn't change in the loop

		After:
		t = x * 2 + 1
		while … { t }

#### Origins of optimization opportunities
* user writes sub-optimal code(e.g. for readability)
* previous compiler pass (extra temporeries)
* inherent redundancy in some languages( null reference in Java)


#### Constant Folding

	foo = fun(i : int) -> int {
		n = 3 * 5;
		r = 0;
		while i != 0 {
			t = 7;
			i := i / 2;
			if i != 0 {
				t =: = t + n;
			}
			r := r + t;
		}
		-> r;
	}

	After constant folding:
	foo = fun(i : int) -> int {
		n = 15; // dead code now
		r = 0;
		while i != 0 {
			t = 7;
			i := i / 2;
			if i != 0 {
				t := 22;
			}
			r := r + t;
		}
		-> r;
	}

##### Basic blocks
* straight line instruction sequence
* no branch out of block in the middle
* no branch into the middle of block
* vertex in flow graph

### [Control Flow Graph](http://en.wikipedia.org/wiki/Control_flow_graph)

### [Data Flow Analysis](http://en.wikipedia.org/wiki/Data-flow_analysis)

##### Constant Folding specification:
	
	Direction		: forward
	Info	 		: for each variable , NC/?/constant
	Initializations	: parameters(NC), rest(?) 
	Transfer functions:	
	Merge			: if Info is the same in all 
					  then have Info
					  else NC
   
##### Forward data flow analysis algorithm:
	
	initialize ENTER.out
	workList.put(ENTER)
	while workList not empty {
		B = workList.pop
		B.in = merge B.out for all predecessors
		execute transfer function for all instructions in B one by one to compute B.out
		if B.out changed {
			put all successors of B.out in workList
		}

##### Liveness analysis specification

	Direction	: Backward
	Info 		: for each variable dead/live
	Initialize 	: all variables dead 
	Transfer 
	functions	: case return x; x.info = live
				  case x = y op z; y.info = live, z.info = live, x.info = dead
				  case x = y; y.info = live, x.info = dead
				  if x op y goto L; x.info = y.info = live
	Merge		: if live in any successor B.in then live in B.out




















