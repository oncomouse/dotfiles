#!/bin/bash

layout_ptr=$(dwm-msg get_layouts | jq .[$1].address)
dwm-msg run_command setlayoutsafe $layout_ptr
