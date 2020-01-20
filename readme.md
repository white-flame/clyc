# Clyc: A port of Cyc to Common Lisp

Project Intent
--------------

The primary purpose of Clyc is to explore the internals of Cyc, a uniquely mature and scalable commercial inference engine, from its only known open source release. Cycorp provided source code portions of a [circa 2009 Java version of Cyc](https://sourceforge.net/p/larkc/code/HEAD/tree/trunk/platform/src/main/java/com/cyc/cycjava/cycl/) to [LarKC](https://larkc.org), released under the permissive Apache License 2.0.  Random documentation publicly found on cyc.com (live or through archive.org) is also consulted.

This native Common Lisp version will be refactored, documented, and modernized yielding a much smaller and easier to modify system. It should also run inferences faster than the layered and semi-interpreted Java version, which emulates a Lisp-like environment (SubL/CycL).

100% compatibility with Cyc is not sought, and this will not be a drop-in replacement for any of Cycorp's offerings, but rather a reimplementation of open source raw inference tools. The Cyc "common sense" knowledge base is also not open source, nor available to or through this project, beyond the minimal ["cyc-tiny"](https://sourceforge.net/p/larkc/code/HEAD/tree/trunk/platform/src/main/resources/cyc-tiny/) subset included in LarKC. However, OpenCyc's larger KB subset is also Apache licensed and will be integrated into Clyc at some point.

"Clyc" is pronounced as if "clock" rhymed with "like".


License
---
Clyc is licensed under the GNU Affero General Public License v3. Some files derive from LarKC's Apache v2 licensed files, with the modifications licensed under the AGPL v3.

The AGPL is the most forced-open major license of which we're aware. If there was a standardized "no commercial use allowed" open source software license[*], we would use it here to respect Cycorp's commercial interests. The Clyc developers have no affiliation with Cycorp, its employees, or its customers.

[*]: Creative Commons is [not recommended](https://creativecommons.org/faq/#can-i-apply-a-creative-commons-license-to-software) for software, and does not mandate making source code available.



Current Status
-------

Still in Step 1 of the plan below, with most SubL stdlib datastructure and utility code converted but untested. File-by-file conversion from Java is ongoing. Many function and variable references are still unmet.

The original LarKC distribution has most of its Cyc source code in a flat directory, which is reflected here in `larkc-cycl`. This will be reorganized after things are up & running.


Rough Plan
----------

1. Convert code as-is to working-ish Common Lisp
   - leave as-is dependency ordering errors, repetitive complexity, orphaned vars/funs, etc
   - implement basic stdlib file, thread, networking, etc functions from SubL and/or Java layers
   - sparingly prune code that references missing-larkc features to untangle some problematic dependencies and references
   - hopefully get cyc-tiny .cfasl files loading and assertions & queries working at this stage, even if manual intervention is required
2. Resolve load order dependencies
   - finish & use cross-referencing tools
   - hoist necessary declarations and move some functions around
   - organize into subdirectories
   - maybe eliminate degenerately small functions to make source code more orthogonally readable
   - hopefully load & operate without any warnings at this stage
3. Refactor
   - utilize lambdas and closures instead of toplevel `-INT` DEFUNs, symbolic function names, and dynamic bindings
   - reevaluate what macros should be written and where they're needed
   - separate intended-public APIs from internal functions based on the Java declarations
   - apply deprecations, eliminate redundancies, and pare down protocols
   - create more technical documentation
4. Profile and optimize particularly egregious sections
   - major refactoring of algorithms and infrastructure is allowable
5. Add new code, including based on commented-out function/macro names


Requirements
------------

Clyc currently loads only on SBCL, as a small number of its extensions and low-level implementation details are used. In the future these dependencies could be refactored out as the codebase matures past the specifics of the initial direct port, or else portability shims for other Common Lisp implementations will be added.

Compilation warnings can be made visible by evaluating the following before quickloading:
```
(setf quicklisp-client:*quickload-verbose* t)
```

[The modern Common Lisp + Quicklisp modus operandi is to add a symlink in `~/quicklisp/local-projects/` to projects like Clyc, and run `(ql:quickload "clyc")` from the REPL to load a project.]



Data Structures
--------------------
*These are listed by their* `.java`/`.lisp` *filenames.*

`tries` - Character-based trie used to intern and prefix-complete Constant names from strings.

`id-index` - Key/value storage with incrementally allocated integer keys.  Backed by a vector, which can be grown, with a hashtable storing entries whose keys are out of range.

`set` - Wrapped set-contents for some reason, maybe for its cfasl interface? Replaced with a key->key hashtable implementation.

`cache` - A hashtable with LRU discarding to keep a fixed size. (LRU discarding might be missing-larkc)

`queue` - A FIFO queue (cons-backed), and a priority FIFO queue (b-tree backed).

`binary-tree` - A standard binary tree, and an AVL tree which is missing-larkc.

`stacks` - A LIFO stack which also maintains a count of elements.

`deck` - A push/pop interface manually dispatched to either a queue or stack.

`fvector` - file-vector, an indexed on-disk array of arbitrary-length elements.

### Deprecated

`dictionary` - Key/value storage backed by an a-list when small, and a hashtable when large. Elided in preference to standard hashtables.

`keyhash` - A set, stored in a manually-implemented hash table so as not to store a value. Elided in preference to standard hashtables.

`set-contents` - A set, stored in a list when small, keyhash when large.  Converted to use only a hashtable backend, though should be deprecated in favor of `set`.

`fraction-utilities` - Elided, since CL already natively supports rational numbers.

### File Exists But the Implementation is missing-larkc

`bijection` - A key/value mapping that also supports reversed value→key lookups.  A-list for small maps, pair of hashtables for large maps.  No given implementation, but can be easily recreated.  
`shelfs` - Some data container that supports "finalize", "rearrange", "bsearch", etc.  
`glob` - Some dual-indexed data container.  
`bag` - A multi-set. Possible to recreate.  
`accumulation` - A data accumulation interface that can append its values to various different concrete datastructures.  Probably reconstructable.  
`red-*` - Some form of generic on-disk data repository, maybe similar to the Windows registry?  
`file-hash-table` - On-disk key/value store. Huge function list.  
`sparse-matrix` `sparse-vector` `heap` - Self explanatory.  

Utilities
---------
`structure resourcing` - Object pooling for reusing structure instances. Generally missing-larkc, but took a while to figure out what the term meant.

`cfasl` - Serialization & deserialization tools.

`memoization-state.lisp` - Memoizes function calls.
 
`special-variable-state.lisp` - Snapshots a list of CL special variables.
 
`misc-utilities.lisp` - Startup code.
 



Glossary
--------
**Cyc:**  
`Term` = a constant, NAT, variable, others.  
`Constant` = atomic vocabulary word, in a flat global namespace.  Prefixed with `#$`.  
`Predicate` = relationship between constants, itself named via constant starting with a lowercase letter.  
`Sentence` = cyc s-expression, including logical connectives and predicates.  
`Assertion` = KB storage item comprising a sentence, microtheory, truth value, direction, support.  
`Logical Connective` = `#$and`, `#$or`, `#$not`, `#$implies`, etc.  
`Rule` or `Conditional` = an `#$implies` sentence.  
`Microtheory` = a partition of a KB that can be independently scoped in & out of inferential visibility.  
`Arity` = the number of terms in a predicate not including the first (the operator).  
`Sequence` = a cons list.  
`Sequence term` = a term holding the remainder of a sequence, as in a dotted list. Also `sequence variable` if the term is a variable.  
`Shell` = an empty structure, possibly for filling in structure resourcing pools.

**Clyc:**  
`missing-larkc` = specific term for things in Cyc that were not provided to the LarKC project, distinguished from unimplemented or unfinished things in Clyc.  


Acronyms & Abbreviations
--------
`MT` = MicroTheory.  
`GAF` = Ground Atomic Formula, a sentence that contains no variables or logical connectives.  
`NAT` = Non-Atomic Term, a parameterized function representing a term. `(#$FruitFn #$AppleTree)` is the collection of fruit from apple trees, as opposed to the atomic term `#$Apples` or something.  
`NAUT` = Non-Atomic Unreified Term. A function NAT, before reification, having only the Fn and args.  
`NART` = Non-Atomic Reified Term. Internal identifier that a NAUT resolved to.  
`WFF` = Well-Formed Formula.  
`EL` = Epistemological Level, expressive human-editable form.  
`HL` = Heuristic Level, efficient low-level form.  
`FOL` = First-Order Logic, the full sentence style of EL.  
`CNF` = Conjunctive Normal Form, the style of HL. `(#$and (#$or ?term+)+)`, where terms may also be negated.  
`FORT` = First-Order Reified Term, which is a constant or a NART.  
`SBHL` = Subsumption Based HL, meta predicates like `#$isa`, `#$genls`, `#$genlAttributes`.  
`PSC` = Problem Solving Context, related to which microtheories are in view.  
`GUID` = Globally Unique ID, external identifier.  
`SUID` = (System?) Unique ID, internal identifier.  
`TOU` = Term Of Unit, predicate that maps a NART to a NAUT.  
`TV` = Truth value. Default or monotonically true or false, unknown truth, etc.  
`CZER` = Canonicalizer.  
`AT` = the `arg-type` mechanisms.  
