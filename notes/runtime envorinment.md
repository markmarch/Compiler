### Runtime Envorinment
***
Date : 11/09/2011

#### Lecture topics
1. IR generator: jumping code
2. Runtime system overview
3. Stack
4. Heap
5. IR generator : memory access
***

#### IR generator : jumping code
2 ways to translate boolean expressions:   

* Jumping code
	* Context : 
		* control statment (`if`, `while`, `while`)
		* boolean expression (`&&`, `||`, 	`!`)
	* Attributes:
		* `true`, `false` (inherited labels)
		* *synthesized*
* Value code
	* Context :
		* everywhere else
	* Attributes:
		* `addr`
		* *synthesized*

Example:
	
	Tack:
	if a < 0 || b {
		y := 1;
	} else {
		y := 2;
	}
	z = y;

	IR:
	if a<0 goto L1;
	goto L0;
	L0:
	if b goto L1;
	goto L2
	L1:
		y = 1;
		goto L3;
	L2: 
		y = 2;
	L3:
		z = y;

##### Syntax Directed Definition:
	
	E --> E1 || E2		  	E1.true = E.true
								E1.false = newLabel() 
								E2.true = E.true
								E2.false = E.false
								E.code = E.code || E1.code || E2.code

	| E1 && E2					E1.true = newLabel()
								E1.false = E.false
								E2.true = E1.true
								E2.false = E.false
								E.code = E.code || E1.code || E2.code

	| E1 relop E2				E.code = E1.code
								|| E2.code 
								|| if E1.addr relop E2.addr
										goto E.true
							 	|| goto E.false
	
	| !E						E1.true = E.false
								E1.false = E.true
								E.code = E1.code
	
	| true						goto E.true
	
	| false					goto E.false
	
	
Sometimes, need to convert:

* from `value code` to `jumping code` : `if a[1] { … }`
* from `jumping code` to `value code` : `z := x && y`

##### Syntax Directed Definition(converting value code to jumping code):

	E --> … 					vcode = getVaueCode(E)
								E.code = vcode
								|| if E.addr goto E.true
								|| toto E.false


##### Syntax Directed Definition(convering jumping code to value code)

	E --> … 					E.true = newLabel()
								E.false = newLabel
								jcode = new getJumpingCode(E)
								E.addr = new TempAddr()
								E.code = E.addr = true
								|| jcode
								|| if E.addr 


#### Runtime System

_compiler_ --> assembly code --> _assembler_ --> relocatable machine code(along with library code`runtime.o`) --> _Linker/Loader_ --> Executable code

##### Runtime Library:
* IO(`print`)
* memory managment(`newArray`, `newRecord`, `append`)
* anything "beyond" the language itself(e.g. `jit compiler`, `thread`, `reflection`)

##### Storage orgnazation:

<table border="1">
	<tr><td> Stack </td></tr>
	<tr><td> Frame memory </td></tr>
	<tr><td> Heap </td></tr>
	<tr><td> Static data</td></tr>
	<tr><td> Code </td></tr>
</table>

`Stack` and `Heap` are memories managed at runtime


#### Stack
***
	main{f(); g();}
	f { if … f() else g(); }
	g {…} 
	
	
	Actuals 			= parameter values
	Return address 		= address of instruction after call in caller
	Caller %rbp 		= base pointer of caller
			 	 		= addressof stack frames of caller
				 		= control link


Call Sequence:  

* put actuals
* make call(side effect : push `return address`)
* push (caller's)`%rbp`
* move `%rbp`, `%rsp`
* sub `%rsp`, frameSize

Return Sequence:

* set `%rax` to be return value
* move `%rsp` <- `%rbp`
* pop `%rbp`
* ret (side effect: pop return addr)
* pop actuals

Example:
	
	Tack:
	
	x = f(E1, …, En-1)			E0.code
									E1.code
									… 
									param[0:n] = E0.addr
									… 
									param[n-1:n] = En-1.addr
									call f:n
	-> E;							E.code
									return E.addr

#### Heap
***
	
	Java		C		C++		Tack
allocation:
	new 		malloc()	  new		recordLit and arrayLit
deallocation:
	garbage collector		free()		delete		unspecified


	





















