#!/bin/csh -f                                                                  

set rs=1
while ($rs < 201)

echo '1600' >> teste.ANI
echo '    ' >> teste.ANI
tail -1600 coord$rs.xyz >> teste.ANI
@ rs = $rs + 1
end
