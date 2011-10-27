Date : 10/26/2011
Name & Type analysis
====================
####Lecture Topics:
* #####Scopes and definitions
* #####Scope analyzer
* #####Types and their relations
* #####Type analyzer

example:
	AST --> Scope Analyzer --> error message
					|
					|
				Symbol table

#### Scopes and definitions
***
	Definition		Example code in Tack		Symbol
	VarDef 			x = "hello" 				variable x
					x := "hey"					
	FunDef			x = fun() -> void			function x
						 { … }				
	FieldLit		r = (x = 1, y = "two")		field y
					print(r.y)	
	FieldType		r = (x= 1, y = "two")		field y
					:(x : int, y : string)
	FieldType 		add = fun(x:int, y:int)		variable (parameter y) 
					-> int { -> x + y }		
	ForStmt			for i in [1, 2, 3]			variable (iterator i)
					{ print(x:string) }

##### Lexical scoping 
	{
		x = 1;
		{ x = 2;  x := x + 1}
		printf(x : string); // prints 1
	}

	f = fun() -> void { x = 2 }
	main = () -> int {
		x = 1;
		f();
		print(x : string); // print 1 in Tack, since Tack uses lexical scoping
		// will print out 2 in Lisp, which uses dynamic scoping
		-> 0;
	}

***
	
	Scope Owners					Tack Example
	BlockStmt						{ x= "1"; print(x); }
	
	FunDef							f = fun(x:int) -> int {
										y = x + 1; -> x + y; 
									}
	// a block statment doesn't have its own scope if it's a direct child
	// of an other scope owner

	ForStmt
	
	RecordLit

	RecordType

	Program


#####Scope Analyzer
***
Symbol Table: `push(Scope)` `pop` `lookup(String)`   

	current: point to current scope   
	toplevel:
`lookup` will continue search in outter scope if not found in current scope

   
Scope: `contains(String)`, `get(String)`, `def(Symbol)`

	parent: pointer to parent
	children: points of scopes inside this scope 
	symbols: symbols defined in this scope

`contains` is used to discover duplication

Symbol`Map<String, Symbol>`: `name()` `type()`,
	
	scope: the scope in which this symbol is defined

ASTNode

	symbol:
	type:

#####Translation Schemes for Scope Analyzer
(See dragon book Figure 2.38)
***
	
	Rules 				Translation
	B --> {				B.heldScope = new Scope(B, concurrent) 
		  L				push(B.heldScope)
		  }				pop(B.heldScope)

	L --> LS | e		
	S --> B | D | U		D.symbol = new VarSym(D, current)
						current.def(D.symbol) // this might print multiple definition error message
						
	D --> D = E;		
	U --> ID; 


#### Types and their relations
***

				  type
			 / 	   | 		\
	primitive	function	collections
	int						/ 		\
	string				array		record
	bool							(fields of supertype = the prefix of fields of subtype)
	void							   |
									  null (subtype of all fields type)

examples:   
`a = (x = 1, y = "two")` : `(x : int, y : string)`   
`b = (x = 3)` : `(x : int)`   
`b := a` (implicit converstion) a's type is a subtype of b's type   
`a := b` requires a explicit cast : `a := b : (x : int, y : string)`

In <strong>Tack</strong>®:   
`int`, `string`, `bool` can cast to each other   
`print("3");`   
`print(3 : string);` // cast `int` to `string`
`print("" + 3);`  // `+` operator will convert `3` to `string`

#### Type Analyzer
***
##### Translation scheme for type analyzer

	Rules 				Translation
	B --> {				push(B.heldScope)
		  L				
		  }				pop(B.heldScope)

	L --> LS | e		
	S --> B | D | U		
						
						
	D --> D = E;		
	U --> ID; 			U.symbol = lookup(U.name) // error if symbol is not defined



























