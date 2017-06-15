How SDM Got Algebra 
====================

Bottom up to top down 
---------------------

hsdm started out as a set of bindings to the ```libsdm``` shared
library which itself offers a named
[sparse distributed vector database](#distributed-sparse-memory).

Wanting to rigourously follow
up [previous results and theory](#references) regarding abstract and
concrete algebraic structures relevant to machine learning; a platform
for studying these (and other) representations was envisaged.

A domain specific language (DSL) was defined and implemented in
Haskell to interpret queries as transactions on the backend database
to retrieve vector data.

E.g. the level set resulting from the expression below can be computed directly
by evaluating the expression against a backend:
```
    (Topo "words" 0.5 0.5 10 
        (Or (State "words" "Sherlock") 
            (State "words" "Watson")))
```
The vector operations were localised (re-implemented) in the client.
Whilst these may not be as fast or space-efficient as the current
backend native implementations, in future this may change. 

The requirement to implement and test a wide range of operations not
currently implemented or "dreamed of in your geometry" and consider
the pragmatics of sparse vs. dense representations overruled any
performance considerations in evaluating the algebraic operations of
the AST.


Main Objectives
---------------

- Expose one vectorspace model/API (```Database.SDM```) - Done
- Unify sparse and dense linear algebra - mainly done but need to provide:
   ```toSparse :: Vec -> Vec```
- Foster experiments in vectorspace operators up to geometric
   algebra - in progress 
- Provide visualisations of measurements etc. - in progress
- Integrate with other algebraic approaches e.g. [HLearn](https://github.com/mikeizbicki/HLearn)
- Easy to use and explain


Side Effects
------------

- Alternative APIs realised (serialized vs. marshalled)
- Reduction of vectorspace backend API to load and store by
   re-implementing (so far query algebra) in client code
- Type safe implementation


To Do
-----

- Expand AST to include optinal function for state and elemental/basis
  vectors with defaulting mechanism.
- Re-factor generic Algebra modules into own library/module space
- Alternative (traditional ```R^n```) vector space backend
- Front end query ```parser :: String -> Either SyntaxError AST``` -
  PoC in place for toy REPL
- Benchmarking.
- Accelerate/REPA/hMatrix for dense implementation.
- Compile rather than Eval.


Appendices
----------

### Distributed Sparse Memory

Pentti Kanerva's seminal work on
[Distributed Sparse Memory](http://en.wikipedia.org/wiki/Sparse_distributed_memory) introduced
the idea that hyper-dimensional learning is a model of memory
[@kanerva1988sparse]. The mathematical model he describes:
hyper-dimensional sparse space, has surprising and unintuitive
properties which are also developed by Sandin and Emuruli in
[@DBLP:journals/corr/abs-1103-3585].

Cohen, Widdows and Schvaneveldt have demonstrated the expressiveness
and performance of the approach when applied to random indexing
methods [@cohen2010reflective]; in particular these authors extended
the vector space model into quantum logics [@cohen2010logical]. This
resonates with Keith van Rijksbergen's influential work on quantum
information retrieval [@Van-Rijsbergen:2004]. Dominic Widdows also
introduced these ideas in his book [@widdows2004geometry].

### Unsupervised Contextural Learning and Semantic Vectors

The primary application of random indexing techniques is to provide
high-performance contextural clustering of events and entities. This
has been widely applied to text mining by the authors above and models
for encoding richer relationships other than co-occurrence have also
been proposed. [@plate1995holographic] and [@cohen2010logical]
illustrate how these might be achieved. The models go beyond the
already useful ability to effectively perform large
scale [SVD](http://en.wikipedia.org/wiki/Singular_value_decomposition)
based dimensional reduction problems, such
as [PCA](http://en.wikipedia.org/wiki/Principal_component_analysis)
and [LSI](http://en.wikipedia.org/wiki/Latent_semantic_indexing) and
point the way for making these techniques more expressive and capable
of generalised knowledge representation more powerful than initially
envisaged.


### Algebra and "The Modelling Problem"

One reason that we have turned to Haskell are its strong connections
with category theory and abstract algebra which we feel are a unifying
framework for mathematics, computing and machine learning in
particular.

One example the successful application of such thinking may be found
e.g. [HLearn](https://github.com/mikeizbicki/HLearn) and in
[@izbickialgebraic], [@izbicki2013two].

Haskell has a mature ecosystem of libraries and researchers working in
our main areas of interest. The community of less mature but promising
dependantly typed language [Idris](https://www.idris-lang.org) largely
overlaps with that of Haskell.

See also:

- [Idris](https://www.idris-lang.org)
- [dataHaskell](http://www.datahaskell.org/docs/community/current-environment.html)
- [Geometric algebra](https://en.wikipedia.org/wiki/Geometric_algebra)

\newpage
References
----------

