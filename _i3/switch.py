#!/usr/bin/python2.7

import i3
outputs = i3.get_outputs()

# set current workspace to output 0
i3.workspace(outputs[2]['current_workspace'])

# ..and move it to the other output.
# outputs wrap, so the right of the right is left ;)
i3.command('move', 'workspace to output right')

# rinse and repeat
i3.workspace(outputs[5]['current_workspace'])
i3.command('move', 'workspace to output right')
