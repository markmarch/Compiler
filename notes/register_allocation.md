### Register Allocation
Date : 11/23/2011


#### Lecture Topics:
* register allocation

Sample program :
	
	multi = fun(a : int, b : int)
	{
		r = 0;
		i = a;
		while i > 0 {
			r = r + b
			i = i - 1;
		}
		-> r
	}

`virtual register` x64

`calling conventions` : 

	%rdi		%rsi
	a			b
	return address : %rax
	
	multi:
		push 	%rbp
		mov		%rbp, %rsp
		mov 	a, %rdi
		mov 	b, %rsi
		mov 	r, 0
		mov 	i, a
	L1:
		cmp 	i, 0
		jg 		L2
		jmp 	L3
	L2:
		add		r, b
		sub		i, 1
		jmp		L1
	L3:
		mov 	%rax, r
		mov		%rsp, %rbp
		pop		%rbp
		ret

`spill-all` `x64`: keep all variables on the stack
	
[copy propagation (Wiki)](http://en.wikipedia.org/wiki/Copy_propagation)   

Stack Layout
	
	-----------------
	|caller %rbp	|
	|a				|-8
	|b				|-16
	|r				|-24
	|i				|-32
	-----------------
	multi:
		push 	%rbp
		mov 	%rbp, %rsp
		mov  	[%rbp - 8], %rdi
		mov 	[%rbp - 16], %rsi
		mov		[%rbp - 24], 0
		mov		%rax, %[rbp - 8] "choose an arbitry register
		mov 	%[rbp - 32], %rax " we can do copy propagation here
	L1:
		cmp 	[%rbp - 32], 0
		jg 		L2
		jmp 	L3
	L2:
		mov 	%rax, [%rbp - 16]
		add		[%rbp - 24], %rax
		sub 	[%rbp - 32], 1
		jmp 	L1
	L3:
		mov		%rax, [%rbp - 24]
		pop		%rbp
		mov		%rsp, %rbp
		ret
		
`3 register` `x64` : `%rax`, `%rsi`, `%rdi`
[Register Allocation using Interference Graph](http://lambda.uta.edu/cse5317/fall02/notes/node39.html)

[Live Variable Analysis](http://en.wikipedia.org/wiki/Live_variable_analysis)

Graph coloring:

	a:%rdi
	b:%rsi
	r:%rax
	i:%rdi
	
	multi:
		mov %rax,0
	L1:
		cmp %rdi, 0
		jg  L2
		jmp L3
	L2:
		add %rax, %rsi
		sub %rdi, 1
		jmp L1
	L3:
		ret


`2 register` `x64` : `%rdi` `rax`  
calling convention:
	
	%rdi	[%rbp + 16]
	a		b
We can not color the graph with 2 colors, pick 1 or more variable to spill onto the stack and try again.  
 
Which one to pick:

* usually not variables used in loops
* the one with shorter life span
* how often a variable is used(the least used on)
	
spill `a` onto the stack(other 3 variables are in the loop), still can't color the graph

spill `b` onto stack

We get the stack layout and variable allocation:

	-----------------
	| b				| + 16
	| return add  	| + 8
	| % rbp			|
	| a				| - 8
	-----------------

`r` : `%rax,` `i` : `%rdi`

	multi:
		push %rbp
		mov  %rbp, %rsp
		mov %rax, 0
		mov %rdi,	[%rbp -8]
	L1:
		cmp %rdi, 0
		jg	 L2
		jmp L3
	L2:
		add %rax, [%rbp + 16]
		sub %rdi, 1
		jmp L1
	L3:
		mov %rsp, %rbp
		pop %rbp
		ret


















