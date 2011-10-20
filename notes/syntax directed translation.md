Syntax directed translation
===========================
* Translation Schemes
* Abstract Syntax Tree
* Tree Traversals
* Tree Normalization

***

###Translation Schemes


SDD = Syntax Directed defination

→ grammar + rules for computing attributes

Example :
<pre> 
S → E1 := E2;		    S.a = new AssignStmt(E1.a, E2.a)
  | if E B1 else B2	  S.a = new IfStmt(E.a, B1.a, B2.a)
  | if E B		      S.a = new IfStmt(E.a, B.a)
</pre>
<script src="https://gist.github.com/1299867.js?file=gistfile1.java"></script>

[Rats! example Gist](https://gist.github.com/1299867)

2 kinds of attributes : <strong>inherited</strong> attributes & <strong>synthesized</strong> attributes

<table border ="1px" style="dotted">
<th>
	<td>Synthesized Attributes</td>
	<td>Inherited Attributes</td>
</th>
<tr>
	<td> Computed From </td>
	<td> Children </td>
	<td> Parent & Siblings </td>
</tr>
<tr>
	<td> Implemented where?</td>
	<td> In Parser or In Tree traverals </td>
	<td> Typically in Tree traversals </td>
</tr>
</table>


###Abstract Syntax Tree
<table border="1">
<th><td>Parse Tree</td><td>AST</td></th>
<tr><td>Kind</td><td>Concrete</td><td> abstract</td>
<tr>
	<td> Intermidea Layer</td>
	<td> </td>
	<td> </td>
</tr>
<tr>
	<td> Implemented </td>
	<td> Implicit</td>
	<td> allocated object in memory </td>
</tr>
</table>

### Tree Travelsals
In compiler, usually depth first, left to right

* tree normalizer
* tree printer
* type analyzer
* IR generator

(implemented as subclass of Vistor[[Vistor Pattern](http://en.wikipedia.org/wiki/Visitor_pattern)]

S-attributed  SDD

= Translation scheme using only synthesized attributes

L-attributed SDD

= translation scheme using synthesized attributes and inherited attributes but only form left sibling s or parent.

How can Vistors pattern implment attributes?
<table border="1">
<tr>
	<td> Vistor method returns value</td>
	<td> e.g. Tree Normalizer </td>
	<td> synthisized </td>
</tr>
<tr> 
	<td> Fields of the AstNode subclass</td>
	<td> e.g. Type Analyzer </td>
	<td> syntheisized , inherited </td>
</tr>
<tr>
	<td> Field of vistor </td>
<tr>
<tr>
	<td> Output of Input</td>
	<td> e.g. Pretty printer </td>
	<td> Synthesized / Inherited</td>
</tr>
</table>

### Tree Normalization
Example SDD, construct "raw" AST

<pre>
P → L				| P.r = new Program(L.r)
L → FT			| L.r = new FunDefListHead(F.r, T.r)
T → FT1			| T.r = new FunDefListTail(F.r, T1.r)
  | e			  | T.r = new FunDefListTail()
F → ID = fun		| F.r = new FunDef(ID.value) 
</pre>

Example program:
 
f = fun

g = fun

Example raw AST :

			Program
			   |
			FundDefListHead
			/        \
		FunDef		FunDefListTail
						/   \
					FunDef  FunDefListTail

Desired AST:

			Program
				|
			List<FunDef>
			/		 \
		FunDef		FunDef
	  name = f	    name = g

Normalizer SDD:

--------------
Exmaple : (L.D : desired AST for L, L.I : inherited attribute)

	P → L 	   P.D = new Program(L.D)
	L → FT 	   T.I = [F.d]; L.D = T.D
	T → FT2      T1.I = T.I :: [F.D]; T.D = T1.D 
	   | e       T.D = T.I
	F → ID = fun F.D = new FundDef(ID.name)

Example raw AST to desired AST :

			Program
			   |
			FundDefListHead
			/        	\
		FunDef Df		FunDefListTail D = [Df, Dg]
		name = f		/   		\
					FunDef Dg  	FunDefListTail D = [Df, Dg]
					name = g		I = [Df, Dg]
					I = [Df]
 		
	






















