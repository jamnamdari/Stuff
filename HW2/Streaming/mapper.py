__author__ = 'jamnamdari'

#!/usr/bin/env python

import sys
import math

for line in sys.stdin:
    line = line.strip()
    x, y = line.split()
    x = float(x) * 10
    y = float(y) * 10
    x_ceiling = math.ceil(x) / 10
    y_ceiling = math.ceil(y) / 10
    if x == math.ceil(x):
        x_floor = (x_ceiling - 1 ) / 10
    else:
        x_floor = math.floor(x) /10
    if y == math.ceil(y):
        y_floor = (y_ceiling - 1 ) / 10
    else:
        y_floor = math.floor(y) / 10

    print '%s,%s,%s,%s\t%s' % (x_floor, x_ceiling, y_floor, y_ceiling, 1)


