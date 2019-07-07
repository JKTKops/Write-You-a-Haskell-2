# Contributing

Since a compiler is such a large project, it's virtually guaranteed that I will make mistakes and bad design decisions. I may also write some inefficient code, or code which could be clearer.

Also, I'm still just a college student, and as such it's very likely that I may need to spend a lot of time on my school work and not have as much time to work on ProtoHaskell as I'd like.

So maybe you'd like to help by fixing a mistake I've made. Maybe you just had an improvement on some code that I wrote. You might have some big-picture ideas for the design or some smaller, detailed ideas for a particular section. You might even want to help with major contributions! 

If you belong to the latter (that is, you want to be a significant contributor to this project) then your best bet to contact me is probably via Reddit at u/JKTKops. I'm also responsive to emails, but I'll see a Reddit PM faster (this probably isn't a good thing - oh well).

Otherwise, please open an issue (or pull request, if you've already fixed it!) describing
1) What issue you've found (if any) and/or the idea you have (if any)
2) Propose a fix for the issue if you have one (but please open an issue anyway if you don't!)
3) If the above affects existing code, please try and identify which files (including markdown writeups containing copies of the source code) will be affected. You don't have to fix (or even find) these, but tackling this as issues arise will help make sure that the writeup and the code stay synced. It will also help ensure that the code stays synced _across chapters_.
4) If you haven't done (3), then expect some time to pass while I work on the issue or PR to identify everything that needs to be updated as a result of the changes.

If you fix a subtle issue, or otherwise have code in a PR that looks funky or confusing, _please_ comment it. Since this is a tutorial, it's very likely that some relatively newer Haskellers may look to this repository and project for examples, and comments will be critical. If the comment is long, or the same comment appears in multiple places, follow GHC's example by making a block comment somewhere in a relevant file that begins with `Note: [<describe the note>]` and then a newline. Reference this note from the relevant places with `-- See Note: [<description>]`. If you are referencing a Note which is in a different file, then reference the note with `See Note: [<description>] in path/to/file/from/src`. For example, 
```
See Note: [Deriving MonadState [s]] in Control/Monad/Supply.hs
``` 
This makes it easy to find the correct note by using search functionality to look for the bracketed description, which shouldn't appear anywhere in the actual source. 

That's it! I hope that this will be able to accommodate those who have simply noticed an error that I should fix, as well as those wish to help fix such errors and/or make more significant contributions.
