# irrlisp

Under construction

Glues together Embeddable Common Lisp (ECL) and the Irrlicht game library.

Currently, you can connect to the running program irrlisp-tester with SLIME and run a command like:
``` common-lisp
(irrlisp::upp-irr-scene-ianimatedmeshscenenode-setmd2animation *node* 5)
```
Which will change the animation of the character.
