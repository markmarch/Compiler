Date : 11/16/2011

### Code Generation
***
#### Lecture Topics:

* Introduction to code-gen
* Target language `x64`
* Code-gen without register allocation

***
#### Introduction to code generation

									symbol table
										||
	Intermediate representation => 	code generator
										||
									Target assembly code
										||
					Library   => Linker loader => target binary code

Tack IR: `3 address `

Target assembly code : `x64 assembly`, `CISC`

Linker loader : `gcc -m64 -masm=interl main.s`

					IR					assembly       binary          
	Instrucitons	name/operators		 Name	        number		

Different kinds of assembly:

* CISC:
	1. `complex instruction computer`
	2. `x64`
	3. `Many powerful instruction`
	4. `2 addresses`
	5. `fewer, non-uniformed registers`
	6. `memory access: most instruction can access memory`
* RISC:
	1. `reduced instruction computer`
	2. `ARM`
	3. `Simple fewer instructions`
	4. `3 addresses`
	5. `many uniformed registers`
	6. `few instruction to access memory`
* Stack based:
	1. `typically virual machine`
	2. `Java virtual machine`
	3. `Fewer, simpler instructions`
	4. `0`
	5. `no register`
	6. xxx

##### Stack vs Registers:

Stack:

* `grow unbounded`

Register:

* `fixed set`(e.g. _16_ on x64)

##### Addressing modes:

* immediate
* register
* memory

##### Code generator has two tasks : 

* Register allocation
* Instruction selection/ordering

Trade off:

* simple, fast compile => slow target code (`drag book, section 8.6`)
* comple, slow compile => fast target code










