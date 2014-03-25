# A not so thin Veneer over C++
Veneer is a programming language which transpiles to C++.

- A language which is translated into C++, similarly to how coffeescript is translated into javascript
- Has powerful metaprogramming support.  
- Veneer --- Rewrite --> IR ----> C++

## Intermediate Representation
The intermediate representation is based on clojure.  It should be similiar enough to clojure to allow me to:

- Compile (pure, non java-importing) Clojure to C++
- In Clojure, use these IRs as normal functions.

I anticipate the latter of these might be a lot harder than the former.  It would entail a lot of macro magicery, but it is also of less importance.

# Why?
C++ is a very popular language, many tools and libraries are written in it and many teams require it.
It has many problems: it has an excess of features which add to real value, and a dearth of features which would substantially improve it.
Alternatives for other lacking languages have been developed, e.g. Scala over Java, Julia over MATLAB, yet C++ has no replacement.

# Challenges.
- (How) can I support a limited subset of C++ and maintain full interopability with existing code


- What language to write compiler in?
Main choices
1) Haskell
Cons: I don't know Haskell
	  Is neither the source, intermediate nor target language
Pros: I know functional programming and it shouldn't take
	  too much to learn haskell
	  I have always wanted to learn Haskell
	  I suspect its syntax may heavily influence the standard syntax
	  Functional, good and fast, recommended choice by many
2) Clojure
Cons: Already know Clojure, won't learn too much new.
	  No type system, cause for concern.
	  People will need JVM to use.

Pros: Same language as IR, which means once veneer is working, bootstrapping will be almost instantaneous
	  Already know pretty well.
	  No need to parse text and worry about grammars

3) C++
	  Don't even bother.