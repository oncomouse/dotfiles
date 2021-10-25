#!/usr/bin/env bash

riverctl send-layout-cmd stacktile "all_primary false"
riverctl send-layout-cmd stacktile "primary_sublayout full"
riverctl send-layout-cmd stacktile "primary_position right"
riverctl send-layout-cmd stacktile "primary_count 1"
riverctl send-layout-cmd stacktile "primary_ratio 0.55"
riverctl send-layout-cmd stacktile "outer_padding 0"
riverctl send-layout-cmd stacktile "inner_padding 0"
riverctl send-layout-cmd stacktile "secondary_sublayout rows"
riverctl send-layout-cmd stacktile "secondary_count 0"
riverctl send-layout-cmd stacktile "secondary_ratio 0.5"
riverctl send-layout-cmd stacktile "remainder_sublayout rows"
