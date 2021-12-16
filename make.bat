@echo off
tasm main.asm 
tlink main.obj 
del main.obj
del main.map