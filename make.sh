#!/bin/bash
~/dasm/dasm game.asm -f3 -v5 -ldwarf.lst -odwarf.a26 -sdwarf.sym
stella dwarf.a26
