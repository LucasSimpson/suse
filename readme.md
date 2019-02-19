# SUSE
### Shoulda Used Something Else

SUSE is an interpreter I wrote in Haskell. I made it to:
 - Get better with Haskell
 - Explore the idea of combination parsers
 - Explore writting my own programming language

run.hs will read, parse, and run the contents of the code in input.txt. The syntax is heavily
based on lambda calculus. The language itself is a functional and dynamically typed. 

Syntax example:


```
|fact(x) {
	if (< (x) (2)) then (
		1
	) else (
		* (x) (fact (- (x) (1)))
	)
} (10)
```

The language itself is powerful enough for decently complex patterns. See 'example.suse' for an example where a basic parser combinators is implemented, including foldl, fmap, and bind implementations.



## Syntax Specifics

#### Math statement, using polish notation

```+ (4) (10)```

> 16

#### declare a function with name func bound variable x
```|func(x) { + (x) (10) }```
Function(+ x 10)

#### invoke a function inline
```|func(x) { + (x) (10) } (4)```
16

#### functions are first-class and can be passed as arguments
```|f(func) { func (10) } ( |addFour(x) { + (4) (x) } )```
14

#### Functions can be nested to create multi-argument functions
```
|add(x) { 
	|add_(y) {
		+ (x) (y)
	}
} (-100) (35)
```
-65

#### Functions can then be partiall applied. Also, let statements
```
let
	add = |a(x) { 
		|a_(y) {
			+ (x) (y)
		}
	};
	addTwo = add (2);
in addTwo (20)
```
22

#### Supports Integers, Chars, Booleans, Lists, and provides String literals as a shortcut for a list of chars
```
let
	myList = [1, 2, 3];
	myName = "Lucas";
in + (myList!!(2)) (len(myName))
```
8

#### Basic IO
```
let 
	echo = |f(text) { print (text) };
	fName = "input2.txt";
in echo (readFile (fName))
```
"Wow it worked!"


## More Examples

#### fmap
```
let
	fmap = |f(mapFunc) {
		|l(list) {
			if (== (0) (len (list))) then (
				[]
			) else (
				concat ([mapFunc (list!!(0))]) (f (mapFunc) (
					|t(xs) { slice (1) (len (xs)) (xs) } (list))
				)
			)
		}	
	};
in fmap (+ (2)) ([1, 2, 3])
```
[3, 4, 5]


#### foldl
```
let
	foldl = |f(combFunc) {
		|s(start) {
			|l(list) {
				if (== (len(list)) (0)) then (
					start
				) else (
						f 
							(combFunc) 
							(combFunc (start) (list!!(0))) 
							(|t(xs) { slice (1) (len (xs)) (xs) } (list))
				)
			}
		}
	};
in foldl (+) (0) ([1, 2, 3])
```
6


