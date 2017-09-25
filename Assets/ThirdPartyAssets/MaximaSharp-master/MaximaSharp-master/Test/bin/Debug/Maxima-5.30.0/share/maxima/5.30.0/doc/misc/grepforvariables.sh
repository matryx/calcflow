#!/bin/bash

#grepforvariables.sh - script to parce clisp lis files for 
#function information
#Copyright (C) 2002 Cliff Yapp.  Copyright is assigned to the 
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

Filename=workingvariables.txt

read variable < $Filename

while [ "$variable" != ENDOFVARIABLESFILE ] 
do
    grep -r -i $variable" " doc/info/* >> results;
    grep -r -i $variable"," doc/info/* >> results;
    read resulta < results;
    if [ "$resulta" = "" ] 
    then 
    	echo $variable" - NOTFound";
    fi
    rm results;
    read variable;
done <$Filename

exit 0
