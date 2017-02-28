# Term-rewriting project

By Cristian Schuszter.

This project's idea is to implement a sort of goal-proving mechanism. Main points:

* The user can define his or her language (function symbols and constants). Information 
about the functions is expressed in the form of "f/n/{infix,postfix,prefix}" where f is the function symbol that will be used,
n is the number of arguments that the function can receive and the last parameter specifies the place in which the symbol can be placed.

* The program can then accept an arbitrary command line writen by the user and parse it, saying true or false and in case of false
giving the reason for it. (ex: (-(x+y) + (2 * x) + 0)


## Future steps

* The next steps will be to add something which allows us to use rules. The 
rule engine will operate on these rules and transform the term that we give it

## Reference books

* Term Rewriting and All That, Franz Baader and Tobias Nipkow

