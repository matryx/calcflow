#x.awk - script to parce clisp lis files for function information
#Copyright (C) 2002 Dan Stanger.  Copyright is assigned to the 
#FSF.

#This library is free software; you can redistribute it and/or
#modify it under the terms of the GNU Lesser General Public
#License as published by the Free Software Foundation; either
#version 2.1 of the License, or (at your option) any later version.

#This library is distributed in the hope that it will be useful,
#but WITHOUT ANY WARRANTY; without even the implied warranty of
#MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
#Lesser General Public License for more details.

#You should have received a copy of the GNU Lesser General Public
#License along with this library; if not, write to the Free Software
#Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

BEGIN { FS="[( ]" ; f = 0 }
f == 0 && $0 ~ /\#\:\|/ && ($5 ~ /DEFUN/ || $5 ~ /DEFMFUN/){ft=$5; 
fn=$6; f=1}
f == 1 && $0 !~ /\#\:\|/ && $0 ~ /Disassembly of function/ {f=2;}
f == 2 && $0 ~ /reads special/ { gsub(/\r/,""); split($0, a, /:/); r = a[2]}
f == 2 && $0 ~ /writes special/ { gsub(/\r/,""); split($0, a, /:/); w = a[2]}
f == 2 && $2 ~ /byte-code/ && $3 ~ /instructions/{printf("%s %s %d reads %s writes %s\n",ft,fn,$1,r,w); f=0}
