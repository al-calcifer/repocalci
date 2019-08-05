#!/bin/csh -f                                                                  

set rs=1
while ($rs < 358)

more coord$rs.xyz >> ani
#echo '' >> ani

@ rs = $rs + 1
end

mv ani video.ANI
