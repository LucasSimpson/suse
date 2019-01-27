# SUSE
#### Shoulda Used Something Else

SUSE is an interpreter I wrote in Haskell. I made it to:
 - Get better with Haskell
 - Explore the idea of combination parsers
 - Explore writting my own programming language

run.hs will read, parse, and run the contents of the code in input.txt. The syntax is heavily
based on lambda calculus. Quirks in the syntax are because the parsing code I wrote is not context-free, and is more akin to a regular expression parser (e.g. we use polish notation). Syntax example:


```
|a.(
	|b.(
		+ (!a (20) (10)) (!b (3))
	)
)(
	|x.(
		|y.(
			- (x) (y)
		)
	)
)(
	|x.(
		|y.(
			* (x) (y)
		)
	) (4)
)
```


## Syntax Specifics

#### Math statement, using polish notation

> `+ (4) (10)`
> 16

#### declare a function with bound variable x
> `|x.( + (x) (10) )`
> Function(+ x 10)

#### invoke a function inline
> `|x.( + (x) (10) )(4)`
> 16

#### invoke a passed function argument
> `|x.( !x(10) ) ( |y.( + 4 10 ) )`
> 16

Functions can be nested to create multi-argument functions
> `|x.( 
	|y.(
		+ x y
	)
) (10) (4)`
> 16

