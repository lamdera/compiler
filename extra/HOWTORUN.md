# How to run

We have to specify ELM_HOME as a subdirectory in the project to get stack.yaml generation to work properly. This could be delt with properly, but that seems quite hard to do.

Development: ```ELM_HOME=elm-home stack ghci```   
Production: tbd, maybe symlink `~/.elm` into this folder to cache across projects?
