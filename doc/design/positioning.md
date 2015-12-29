Nobody's going to adopt something unless it provides something new and interesting -- some value proposition that isn't served otherwise.

Type safety and soundness (and their trappings: generics, traits, sums, and pattern matches) are not by themselves compelling enough to convince someone to switch from JavaScript or TypeScript or Python or whatever, especially in the presence of arguments like "We have unit tests" or "Type errors are not a problem in my codebase." or "TypeScript already has types".

That is, Crux must make programming easier and more expressive in some meaningful way for it to be adopted.

So what are Crux's meaningful expressiveness advantages?

1) type-indexed values (e.g. traits e.g. tuples as map keys).  a very common need in javascript.  also gives inference-indexed values, like "def" or aeson's decoder functions
2) higher performance than the equivalent naive javascript (most functions are statically-looked up)
3) tight generated JS code and native backend too
4) statically typed without type annotations (bidirectional type inference)
5) generalized concurrency patterns
6) zero-overhead abstractions (newtypes, traits).  also zero-overhead nominal types like CustomerID
7) generalized iteration: for x in <iterable>

We probably shouldn't use the word "generics".  People who know what they are and want them will see that Crux supports them.  Sadly the Go community has politicized the word into a them-and-us issue.  And Go does support some generics, just not user-defined generics.

Andy raises the good point that we may want to post in more specialized limited communities like LtU to attract the kinds of people who might be interested in helping or providing advice.  That said, we shouldn't announce to HN or anything like that until there's actually a reason someone might try it.
