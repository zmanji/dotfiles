#!/usr/bin/env python3

def main():
    pass


def handle_result(args, result, target_window_id, boss):
    if args[1] == "next":
        boss.active_tab_manager.next_tab(1)
    elif args[1] == "previous":
        boss.active_tab_manager.next_tab(-1)

    boss.active_tab.neighboring_window(args[1])

handle_result.no_ui = True
